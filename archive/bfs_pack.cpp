/*  BFSPack
    Copyright(C) 2022 Lukas Cone

    This program is free software : you can redistribute it and / or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.If not, see <https://www.gnu.org/licenses/>.
*/

#include "bfs.hpp"
#include "datas/app_context.hpp"
#include "datas/binreader.hpp"
#include "datas/binwritter.hpp"
#include "datas/directory_scanner.hpp"
#include "datas/master_printer.hpp"
#include "datas/reflector.hpp"
#include "datas/stat.hpp"
#include "project.h"
#include "zlib.h"
#include <algorithm>
#include <deque>
#include <iomanip>
#include <memory>
#include <mutex>
#include <thread>
#include <vector>

MAKE_ENUM(ENUMSCOPE(class Title, Title), EMEMBER(FlatOut1), EMEMBER(FlatOut2),
          EMEMBER(FlatOutUltimateCarnage));

struct BFSPack : ReflectorBase<BFSPack> {
  Title title = Title::FlatOut1;
  uint32 compressThreshold = 90;
  uint32 minFileSize = 0x80;
  bool verbose = false;
} settings;

REFLECT(
    CLASS(BFSPack),
    MEMBER(title, "t", ReflDesc{"Set title for correct archive handling."}),
    MEMBERNAME(compressThreshold, "compress-threshold", "c",
               ReflDesc{
                   "Writes compressed data only when compression ratio is less "
                   "than specified threshold [0 - 100]%"}),
    MEMBERNAME(
        minFileSize, "min-file-size", "m",
        ReflDesc{
            "Files that are smaller than specified size won't be compressed."}),
    MEMBER(verbose, "v", ReflDesc{"Prints more information."}));

AppInfo_s appInfo{
    AppInfo_s::CONTEXT_VERSION,
    AppMode_e::PACK,
    ArchiveLoadType::ALL,
    BFSPack_DESC " v" BFSPack_VERSION ", " BFSPack_COPYRIGHT "Lukas Cone",
    reinterpret_cast<ReflectorFriend *>(&settings),
};

const AppInfo_s *AppInitModule() {
  RegisterReflectedType<Title>();
  return &appInfo;
}

uint32 GetLookupIndex(es::string_view path) {
  uint32 v1 = path.size();
  int32 v2 = (v1 >> 5) + 1;
  for (int32 i = v1; i >= v2; i -= v2) {
    auto v4 = (v1 >> 2) + 32 * v1 + uint8(path[i - 1]);
    v1 ^= v4;
  }
  return v1 % Header::NUM_HASH_INDICES;
}

struct AFile {
  std::string path;
  size_t compSize;
  size_t uncompSize;
  size_t streamOffset;
  uint32 crc32;
  uint32 index;
  mutable uint32 selfOffset;

  bool operator<(const AFile &other) const { return path < other.path; }
};

struct HuffmanLeaf;
using HuffmanLeafPtr = std::shared_ptr<HuffmanLeaf>;

struct HuffmanLeaf {
  HuffmanLeafPtr left;
  HuffmanLeafPtr right;
  HuffmanLeaf *parent = nullptr;
  size_t weight;
  bool isRight = false;
  uint8 value = 0;
};

struct HuffmanCode {
  uint8 code;
  uint8 codeLen;
};

auto BuildHuffmanTree(const std::vector<char> &items) {
  std::map<char, HuffmanCode> codes;
  std::map<char, HuffmanLeafPtr> leaves;
  std::deque<HuffmanLeafPtr> firstQueue, secondQueue;
  size_t curWeight = 4;

  for (auto data : items) {
    auto lData = std::make_shared<HuffmanLeaf>();
    lData->value = data;
    lData->weight = curWeight++;
    firstQueue.emplace_back(lData);
    leaves.emplace(data, lData);
  }

  auto getMin = [&] {
    auto get = [&](auto &what) {
      auto retVal = what.front();
      what.pop_front();
      return retVal;
    };

    if (firstQueue.empty()) {
      return get(secondQueue);
    }

    if (secondQueue.empty()) {
      return get(firstQueue);
    }

    if (firstQueue.front()->weight < secondQueue.front()->weight) {
      return get(firstQueue);
    }

    return get(secondQueue);
  };

  while (!(firstQueue.empty() && secondQueue.size() == 1)) {
    auto left = getMin();
    auto right = getMin();
    auto top = std::make_shared<HuffmanLeaf>();
    top->weight = left->weight + right->weight;
    top->right = right;
    top->left = left;
    right->parent = top.get();
    right->isRight = true;
    left->parent = top.get();
    left->isRight = false;
    secondQueue.emplace_back(top);
  }

  for (auto [c, leaf] : leaves) {
    HuffmanCode code{};
    HuffmanLeaf *curLeaf = leaf.get();

    while (true) {
      if (!curLeaf->parent) {
        break;
      }
      code.code |= curLeaf->isRight;
      curLeaf = curLeaf->parent;

      if (curLeaf->parent) {
        code.code <<= 1;
      }

      code.codeLen++;
    }

    codes.emplace(c, code);
  }

  return std::make_pair(codes, secondQueue.front());
}

uint8 CalculateHuffmanLeftOffsets(HuffmanLeafPtr item, uint8 curOffset = 0) {
  curOffset++;

  if (item->right || item->left) {
    curOffset = CalculateHuffmanLeftOffsets(item->right, curOffset);
    item->value = curOffset;
    curOffset = CalculateHuffmanLeftOffsets(item->left, curOffset);
  }

  return curOffset;
}

void WriteHuffmanTree(HuffmanLeafPtr item, BinWritterRef wr) {
  wr.Write(item->value);

  if (item->right || item->left) {
    wr.Write<uint8>(0);
    WriteHuffmanTree(item->right, wr);
    WriteHuffmanTree(item->left, wr);
  } else {
    wr.Write<uint8>(0x80);
  }
};

struct Stream {
  std::string streamPath;
  BinWritter streamStore;
  std::vector<AFile> files;

  Stream(std::string &&path)
      : streamPath(std::move(path)), streamStore(streamPath) {}
};

struct BfsMakeContext : AppPackContext {
  std::string outBfs;
  PathFilter dontCompress;
  std::map<std::thread::id, Stream> streams;

  Stream &NewStream() {
    static std::mutex streamsMutex;
    auto thisId = std::this_thread::get_id();
    uint32 threadId = reinterpret_cast<uint32 &>(thisId);
    std::string path = outBfs + std::to_string(threadId) + ".data";

    {
      std::lock_guard<std::mutex> lg(streamsMutex);
      streams.emplace(thisId, std::move(path));
      return streams.at(thisId);
    }
  }

  BfsMakeContext() = default;
  BfsMakeContext(const std::string &path) : outBfs(path) {
    // Some menu stuff cannot be compressed, causes crashes
    dontCompress.AddFilter(es::string_view("data/menu/cars/"));
    // Might be only _bg_ or _backgroud_
    dontCompress.AddFilter(es::string_view("data/menu/*.tga$"));
    dontCompress.AddFilter(es::string_view(".avi$"));
  }
  BfsMakeContext &operator=(BfsMakeContext &&) = default;

  void SendFile(es::string_view path, std::istream &stream) override {
    stream.seekg(0, std::ios::end);
    const size_t streamSize = stream.tellg();
    stream.seekg(0);

    std::string buffer;
    std::string outBuffer;

    auto CompressData = [&](auto &&buffer) {
      outBuffer.resize(std::max(buffer.size() + 0x1000, size_t(0x8000)));
      uLongf availOut = outBuffer.size();
      int state =
          compress(reinterpret_cast<Bytef *>(&outBuffer[0]), &availOut,
                   reinterpret_cast<Bytef *>(&buffer[0]), buffer.size());

      if (state != Z_OK) {
        throw std::runtime_error("zlib error: " + std::to_string(state));
      }

      return availOut;
    };

    size_t compressedSize = streamSize;
    auto found = streams.find(std::this_thread::get_id());
    Stream *tStream = nullptr;

    if (es::IsEnd(streams, found)) {
      tStream = &NewStream();
    } else {
      tStream = &found->second;
    }

    auto &streamStore = tStream->streamStore;
    AFile curFile;
    curFile.streamOffset = streamStore.Tell();

    buffer.resize(streamSize);
    stream.read(&buffer[0], streamSize);
    curFile.crc32 =
        ~crc32(0, reinterpret_cast<const Bytef *>(buffer.data()), streamSize);
    curFile.uncompSize = streamSize;
    bool writeOutBuffer =
        streamSize > settings.minFileSize && !dontCompress.IsFiltered(path);

    if (writeOutBuffer) {
      compressedSize = CompressData(buffer);

      uint32 ratio = ((float)compressedSize / (float)streamSize) * 100;
      writeOutBuffer = ratio <= settings.compressThreshold;

      if (!writeOutBuffer && settings.verbose) {
        printline("Ratio fail " << ratio << "%% for " << path);
      }
    }

    if (writeOutBuffer) {
      curFile.compSize = compressedSize;
      streamStore.WriteBuffer(outBuffer.data(), compressedSize);
    } else {
      curFile.compSize = -1;
      streamStore.WriteBuffer(buffer.data(), streamSize);
    }

    auto &fle = tStream->files.emplace_back(curFile);
    fle.path = path;
    std::transform(fle.path.begin(), fle.path.end(), fle.path.begin(),
                   [](auto c) { return std::tolower(c); });
    fle.index = GetLookupIndex(fle.path);
  }

  void FinishFO1(BinWritterRef wr, const std::vector<AFile> &files) {
    std::map<uint32, std::set<const AFile *>> pickedItems;
    size_t filesBufferSize = files.size() * (sizeof(FileFO1Base) + 2);

    for (auto &f : files) {
      filesBufferSize += f.path.size();
      auto found = pickedItems.find(f.index);

      if (es::IsEnd(pickedItems, found)) {
        pickedItems.emplace(f.index, std::set<const AFile *>{&f});
      } else {
        found->second.emplace(&f);
      }
    }

    wr.Push();
    Header hdr;
    hdr.signature = hdr.SIGNATURE;
    hdr.numFiles = files.size();
    wr.Seek(sizeof(Header) - 4);
    wr.Skip(hdr.numFiles * 4);
    wr.Write(hdr.NUM_HASH_INDICES);

    size_t curFileIndex = 0;

    for (size_t i = 0; i < hdr.NUM_HASH_INDICES; i++) {
      auto found = pickedItems.find(i);
      if (es::IsEnd(pickedItems, found)) {
        wr.Write(uint32(0));
      } else {
        auto &[id, files] = *found;
        HashIndexFO1 hida;
        hida.startFileIndex = curFileIndex;
        hida.numFiles = files.size();
        curFileIndex += files.size();
        wr.Write(hida);
      }
    }

    const size_t pcDataBegin = filesBufferSize + wr.Tell();

    for (auto &[_, files] : pickedItems) {
      for (auto &f : files) {
        FileFO1Base cFile{};

        if (f->compSize != -1) {
          cFile.compressedSize = f->compSize;
          cFile.compressed = 1;
        } else {
          cFile.compressedSize = f->uncompSize;
          cFile.compressed = 0;
        }

        cFile.crc32 = f->crc32;
        cFile.dataOffset = pcDataBegin + f->streamOffset;
        cFile.uncompressedSize = f->uncompSize;
        f->selfOffset = wr.Tell();
        wr.Write(cFile);
        wr.WriteContainerWCount<uint16>(f->path);
      }
    }

    hdr.headerSize = wr.Tell();
    wr.Pop();
    wr.Write(hdr);
    wr.Seek(sizeof(Header) - 4);

    for (auto &[_, files] : pickedItems) {
      for (auto &f : files) {
        wr.Write(f->selfOffset);
      }
    }
    wr.Seek(hdr.headerSize);
  }

  auto WriteCompressedFiles(BinWritterRef wr,
                            const std::map<char, HuffmanCode> &codes,
                            HuffmanLeafPtr root,
                            const std::vector<AFile> &files) {
    wr.Push();
    wr.SetRelativeOrigin(wr.Tell(), false);
    StringsHeader shdr;
    wr.Write(shdr);
    shdr.huffmanDataOffsets = wr.Tell();

    std::stringstream stringStr;
    BinWritterRef wrs(stringStr);
    std::vector<uint32> sOffsets;
    std::vector<uint16> sLens;
    std::map<es::string_view, uint16> indices;

    auto AddString = [&](es::string_view what) {
      sOffsets.push_back(wrs.Tell());
      sLens.push_back(what.size());
      uint8 cReg = 0;
      uint8 cPos = 0;

      for (auto c : what) {
        auto code = codes.at(c);
        cReg |= code.code << cPos;
        cPos += code.codeLen;

        if (cPos > 7) {
          wrs.Write(cReg);
          cPos -= 8;
          cReg = code.code >> (code.codeLen - cPos);
        }
      }

      if (cPos) {
        wrs.Write(cReg);
      }
    };

    for (auto &f : files) {
      size_t folderPart = f.path.find_last_of('/');
      es::string_view folder(f.path.data(), folderPart);
      es::string_view fileName(f.path.data() + folderPart + 1);

      if (!indices.count(folder)) {
        AddString(folder);
        indices.emplace(folder, indices.size());
      }

      if (!indices.count(fileName)) {
        AddString(fileName);
        indices.emplace(fileName, indices.size());
      }
    }

    wr.WriteContainer(sOffsets);
    shdr.stringSizes = wr.Tell();
    wr.WriteContainer(sLens);
    shdr.huffmanTree = wr.Tell();
    CalculateHuffmanLeftOffsets(root);
    WriteHuffmanTree(root, wr);
    shdr.huffmanData = wr.Tell();
    auto hData = stringStr.str();
    wr.WriteContainer(hData);
    shdr.size = wr.Tell();
    wr.ResetRelativeOrigin(false);
    wr.Push(wr.StackIndex1);
    wr.Pop();
    wr.Write(shdr);
    wr.Pop(wr.StackIndex1);

    return indices;
  }

  void FinishFO2(BinWritterRef wr, const std::vector<AFile> &files,
                 bool asFOUC = false) {
    std::map<uint32, std::set<const AFile *>> pickedItems;

    auto [codes, root] = [&] {
      size_t chars[0x100]{};

      for (auto &f : files) {
        auto found = pickedItems.find(f.index);

        if (es::IsEnd(pickedItems, found)) {
          pickedItems.emplace(f.index, std::set<const AFile *>{&f});
        } else {
          found->second.emplace(&f);
        }

        for (auto c : f.path) {
          chars[uint8(c)]++;
        }
      }

      uint8 curChar = 0;
      std::map<size_t, std::vector<char>> weightedChars;

      for (auto c : chars) {
        if (c) {
          weightedChars[c].push_back(curChar);
        }
        curChar++;
      }

      std::vector<char> sortedItems;

      for (auto [_, cv] : weightedChars) {
        for (auto c : cv) {
          sortedItems.push_back(c);
        }
      }

      return BuildHuffmanTree(sortedItems);
    }();

    wr.Push();
    Header hdr;
    hdr.signature = hdr.SIGNATURE;
    hdr.numFiles = files.size();
    wr.Write(hdr);
    wr.Skip(sizeof(HashIndex) * hdr.NUM_HASH_INDICES);

    auto indices = WriteCompressedFiles(wr, codes, root, files);

    const size_t pcDataBegin = wr.Tell() + sizeof(BFile) * hdr.numFiles;

    for (auto &[_, files] : pickedItems) {
      for (auto &f : files) {
        BFile cFile{};

        if (f->compSize != -1) {
          cFile.compressedSize = f->compSize;
          if (asFOUC) {
            cFile.fouc.storeType = 5;
          } else {
            cFile.fo2.compressed = 5;
          }
        } else {
          cFile.compressedSize = f->uncompSize;
          if (asFOUC) {
            cFile.fouc.storeType = 4;
          } else {
            cFile.fo2.compressed = 4;
          }
        }

        cFile.crc32 = f->crc32;
        cFile.dataOffset = pcDataBegin + f->streamOffset;
        cFile.uncompressedSize = f->uncompSize;
        size_t folderPart = f->path.find_last_of('/');
        es::string_view folder(f->path.data(), folderPart);
        es::string_view fileName(f->path.data() + folderPart + 1);
        cFile.folderId = indices.at(folder);
        cFile.fileId = indices.at(fileName);
        f->selfOffset = wr.Tell();
        wr.Write(cFile);
      }
    }

    hdr.headerSize = wr.Tell();
    wr.Pop();
    wr.Write(hdr);

    for (size_t i = 0; i < hdr.NUM_HASH_INDICES; i++) {
      auto found = pickedItems.find(i);
      if (es::IsEnd(pickedItems, found)) {
        wr.Write(HashIndex{});
      } else {
        auto &[id, files] = *found;
        HashIndex hida;
        hida.filesOffset = (*files.cbegin())->selfOffset;
        hida.numFiles = files.size();
        wr.Write(hida);
      }
    }

    wr.Seek(hdr.headerSize);
  }

  void Finish() override {
    std::vector<AFile> files;
    size_t curOffset = 0;

    for (auto &[_, stream] : streams) {
      auto &tFiles = stream.files;
      std::transform(tFiles.begin(), tFiles.end(), std::back_inserter(files),
                     [&](auto &&item) {
                       item.streamOffset += curOffset;
                       return std::move(item);
                     });
      curOffset = tFiles.back().compSize + tFiles.back().streamOffset;
    }

    BinWritter wr(outBfs);

    switch (settings.title) {
    case Title::FlatOut1:
      FinishFO1(wr, files);
      break;
    case Title::FlatOut2:
      FinishFO2(wr, files);
      break;
    case Title::FlatOutUltimateCarnage:
      FinishFO2(wr, files, true);
      break;
    }

    for (auto &[_, stream] : streams) {
      es::Dispose(stream.streamStore);
      BinReader rd(stream.streamPath);
      const size_t fSize = rd.GetSize();
      char buffer[0x80000];
      const size_t numBlocks = fSize / sizeof(buffer);
      const size_t restBytes = fSize % sizeof(buffer);

      for (size_t b = 0; b < numBlocks; b++) {
        rd.Read(buffer);
        wr.Write(buffer);
      }

      if (restBytes) {
        rd.ReadBuffer(buffer, restBytes);
        wr.WriteBuffer(buffer, restBytes);
      }

      es::RemoveFile(stream.streamPath);
    }
  }
};

static thread_local BfsMakeContext archive;

AppPackContext *AppNewArchive(const std::string &folder, const AppPackStats &) {
  auto file = folder;
  while (file.back() == '/') {
    file.pop_back();
  }

  file += ".bfs";
  return &(archive = BfsMakeContext(file));
}
