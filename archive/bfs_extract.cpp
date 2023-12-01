/*  BFSExtract
    Copyright(C) 2022-2023 Lukas Cone

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
#include "project.h"
#include "spike/app_context.hpp"
#include "spike/except.hpp"
#include "spike/io/binreader_stream.hpp"
#include "spike/master_printer.hpp"
#include "zlib.h"
#include <spanstream>
#include <vector>

std::string_view filters[]{
    ".bfs$",
    ".BFS$",
};

static AppInfo_s appInfo{
    .filteredLoad = true,
    .header = BFSExtract_DESC " v" BFSExtract_VERSION ", " BFSExtract_COPYRIGHT
                              "Lukas Cone",
    .filters = filters,
};

AppInfo_s *AppInitModule() { return &appInfo; }

void ExtractAsFO(BinReaderRef rd, size_t numFiles, AppContext *ctx) {
  rd.Skip(numFiles * 4);
  uint32 numHashIndices;
  rd.Read(numHashIndices);
  rd.Skip(numHashIndices * 4);
  std::string fileName;
  std::string inBuffer;
  std::string outBuffer;
  auto ectx = ctx->ExtractContext();

  for (size_t f = 0; f < numFiles; f++) {
    FileFO1Base cFile;
    rd.Read(cFile);
    rd.ReadContainer<uint16>(fileName);

    if (cFile.numDupes) {
      rd.Skip(cFile.numDupes * 4);
    }

    ectx->NewFile(fileName);
    rd.Push();
    rd.Seek(cFile.dataOffset);
    rd.ReadContainer(inBuffer, cFile.compressedSize);
    outBuffer.resize(cFile.uncompressedSize);

    if (cFile.Compressed()) {
      z_stream infstream;
      infstream.zalloc = Z_NULL;
      infstream.zfree = Z_NULL;
      infstream.opaque = Z_NULL;
      infstream.avail_in = inBuffer.size();
      infstream.next_in = reinterpret_cast<Bytef *>(&inBuffer[0]);
      infstream.avail_out = outBuffer.size();
      infstream.next_out = reinterpret_cast<Bytef *>(&outBuffer[0]);
      inflateInit(&infstream);
      int state = inflate(&infstream, Z_FINISH);
      inflateEnd(&infstream);

      if (state < 0) {
        if (infstream.msg) {
          throw std::runtime_error(infstream.msg);
        } else {
          throw std::runtime_error("zlib error: " + std::to_string(state));
        }
      }

      ectx->SendData(outBuffer);
    } else {
      ectx->SendData(inBuffer);
    }

    rd.Pop();
  }
}

struct HuffmanTreeBufferIter {
  std::vector<uint16> *main;
  size_t pos = 0;

  char Get(bool rightWay) {
    if (rightWay) {
      pos++;
    } else {
      pos = main->at(pos);
    }

    auto item = main->at(pos);

    if (item & 0x8000) {
      return item;
    }

    return 0;
  }
};

uint32 DecodeMicro(const uint32 *keys, size_t index, uint32 next,
                   uint32 keyMod) {
  return (keys[(index ^ (keyMod >> 2)) & 3] + keyMod) ^
         (next + ((next << 4) ^ (next >> 5)));
}

void DecryptBlock(uint32 *output) {
  const size_t lastItem = 0x1fff;
  static const uint32 key[]{0x486A3449, 0x53014f82, 0x9e37dc4d, 0x4f9d4c9d};

  auto Decode = [](size_t index, uint32 next) {
    return DecodeMicro(key, index, next, 0x9E3779B9);
  };

  for (size_t i = 0; i < lastItem; i++) {
    output[i] -= Decode(i, output[i + 1]);
  }

  output[lastItem] -= Decode(lastItem, output[0]);
}

void DecryptTOC(uint32 *buffer, size_t iSize) {
  uint32 lastBlock = iSize >> 2;

  if (!lastBlock) {
    return;
  }

  lastBlock--;

  static const uint32 keys[]{0x37e8b81b, 0x107a4fb5, 0xa150a27a, 0x7ddaf997};

  uint32 curKeyMod = 0xB54CDA56;

  for (size_t i = 0; i < 6; i++, curKeyMod -= 0x9E3779B9) {
    for (int32 v = lastBlock; v > 0; v--) {
      buffer[v] -= DecodeMicro(keys, v, buffer[v - 1], curKeyMod);
    }

    buffer[0] -= DecodeMicro(keys, 0, buffer[lastBlock], curKeyMod);
  }
}

void DecryptSpan(uint32 *output, size_t keyOffset, size_t numBlocks) {
  static const uint32 key[]{0x486A3449, 0x53014f82, 0x9e37dc4d, 0x4f9d4c9d};

  auto Decode = [](size_t index, uint32 next) {
    return DecodeMicro(key, index, next, 0x9E3779B9);
  };

  for (size_t i = 0; i < numBlocks; i++) {
    output[i] -= Decode(i + keyOffset, output[i + 1]);
  }
}

template <class C>
void DecryptItem(C &wat, size_t keyOffset, size_t numBlocks) {
  DecryptSpan(reinterpret_cast<uint32 *>(&wat), keyOffset, numBlocks);
}

std::vector<std::string> LoadStrings(BinReaderRef rd) {
  std::vector<std::string> strings;

  StringsHeader strHdr;
  rd.Read(strHdr);
  const size_t numStrings =
      (strHdr.stringSizes - strHdr.huffmanDataOffsets) / 4;
  strings.resize(numStrings);

  {
    std::vector<uint32> huffOffs;
    std::vector<uint16> strLens;
    std::vector<uint16> huffmanBuffer;

    rd.ReadContainer(huffOffs, numStrings);
    rd.ReadContainer(strLens, numStrings);
    rd.ReadContainer(huffmanBuffer,
                     (strHdr.huffmanData - strHdr.huffmanTree) >> 1);

    std::string huffData;
    rd.ReadContainer(huffData, strHdr.size - strHdr.huffmanData);

    for (size_t i = 0; i < numStrings; i++) {
      auto &out = strings.at(i);
      out.resize(strLens.at(i));
      auto *dataBegin = huffData.data() + huffOffs.at(i);
      size_t bit = 0;

      for (size_t t = 0; t < out.size(); t++) {
        HuffmanTreeBufferIter iter;
        iter.main = &huffmanBuffer;

        while (true) {
          const size_t mod = bit % 8;
          const size_t at = bit++ / 8;
          bool right = (dataBegin[at] >> mod) & 1;
          auto huffChar = iter.Get(right);

          if (huffChar) {
            out[t] = huffChar;
            break;
          }
        }
      }
    }
  }

  return strings;
}

void ExtractAsRCU(BinReaderRef rd, AppContext *ctx) {
  HeaderRCUDec hdr;
  rd.Read(hdr);
  DecryptItem(hdr, 0, 5);

  if (hdr.id != Header::ID) {
    uint32 id;
    rd.Seek(0);
    rd.Read(id);
    throw es::InvalidHeaderError(id);
  }

  if (hdr.signature != hdr.SIGNATURE) {
    throw std::runtime_error("BFS archive has invalid signature " +
                             std::to_string(hdr.signature));
  }

  const size_t headerSize = hdr.HeaderSize();
  const size_t numTocBlocks =
      (headerSize / 0x8000) + (headerSize % 0x8000 ? 1 : 0);
  std::string buff = ctx->GetBuffer(numTocBlocks * 0x8000);

  for (size_t b = 0; b < numTocBlocks; b++) {
    DecryptBlock(reinterpret_cast<uint32 *>(buff.data() + b * 0x8000));
  }

  DecryptTOC(reinterpret_cast<uint32 *>(buff.data() + sizeof(HeaderRCU)),
             headerSize - sizeof(HeaderRCU));

  std::spanstream buffStream(buff);
  BinReaderRef rb(buffStream);
  rb.Skip(sizeof(HeaderRCU));
  uint32 numHases;
  rb.Read(numHases);

  if (numHases != Header::NUM_HASH_INDICES) {
    throw std::runtime_error("Invalid archive stream");
  }

  rb.Skip(sizeof(HashIndex) * numHases);

  auto strings = LoadStrings(rb);

  std::string inBuffer;
  std::string outBuffer;
  auto ectx = ctx->ExtractContext();

  if (ectx->RequiresFolders()) {
    rb.Push();

    for (size_t f = 0; f < hdr.numFiles; f++) {
      BFile cFile;
      rb.Read(cFile);
      rb.Skip(cFile.fouc.numDupes * 4);
      ectx->AddFolderPath(strings.at(cFile.folderId));
    }

    ectx->GenerateFolders();
    rb.Pop();
  }

  for (size_t f = 0; f < hdr.numFiles; f++) {
    BFile cFile;
    rb.Read(cFile);

    // Following are offsets to duplicate data streams
    // Data streams are identical
    // Purpose is unknown
    // Possible usage is backup data in case of media corruption
    rb.Skip(cFile.fouc.numDupes * 4);
    auto fileName = strings.at(cFile.folderId) + "/" + strings.at(cFile.fileId);
    ectx->NewFile(fileName);
    const size_t startBlock = cFile.dataOffset / 0x8000;
    const size_t headOffset = cFile.dataOffset % 0x8000;
    rd.Seek(startBlock * 0x8000);
    inBuffer.resize(cFile.compressedSize);
    uint32 block[0x2000];
    rd.Read(block);
    DecryptBlock(block);
    size_t blockTail =
        std::min(0x8000 - headOffset, size_t(cFile.compressedSize));
    size_t availBytes = cFile.compressedSize - blockTail;
    memcpy(inBuffer.data(), reinterpret_cast<char *>(block) + headOffset,
           blockTail);

    while (availBytes) {
      rd.Read(block);
      DecryptBlock(block);
      size_t macroSize = std::min(availBytes, size_t(0x8000));

      memcpy(inBuffer.data() + (cFile.compressedSize - availBytes), block,
             macroSize);
      availBytes -= macroSize;
    }

    if (cFile.Compressed()) {
      outBuffer.resize(cFile.uncompressedSize);
      z_stream infstream;
      infstream.zalloc = Z_NULL;
      infstream.zfree = Z_NULL;
      infstream.opaque = Z_NULL;
      infstream.avail_in = cFile.compressedSize;
      infstream.next_in = reinterpret_cast<Bytef *>(inBuffer.data());
      infstream.avail_out = outBuffer.size();
      infstream.next_out = reinterpret_cast<Bytef *>(outBuffer.data());
      inflateInit(&infstream);
      int state = inflate(&infstream, Z_FINISH);
      inflateEnd(&infstream);

      if (state < 0) {
        if (infstream.msg) {
          throw std::runtime_error(infstream.msg);
        } else {
          throw std::runtime_error("zlib error: " + std::to_string(state));
        }
      }
      ectx->SendData(outBuffer);
    } else {
      ectx->SendData({inBuffer.data() + headOffset, cFile.compressedSize});
    }
  }
}

void AppProcessFile(AppContext *ctx) {
  BinReaderRef rd(ctx->GetStream());
  Header hdr;
  rd.Read(hdr);

  if (hdr.id != hdr.ID) {
    rd.Seek(0);
    ExtractAsRCU(rd, ctx);
    return;
  }

  if (hdr.signature && hdr.signature != hdr.SIGNATURE &&
      hdr.signature != hdr.FOUCSIG) {
    throw std::runtime_error("BFS archive has invalid signature " +
                             std::to_string(hdr.signature));
  }

  if (hdr.numHashIndices != 997) {
    // Consider as FO1 format
    rd.Seek(sizeof(Header) - 4);
    ExtractAsFO(rd, hdr.numFiles, ctx);
    return;
  }

  rd.Skip(hdr.numHashIndices * sizeof(HashIndex));

  auto strings = LoadStrings(rd);

  std::string inBuffer;
  std::string outBuffer;
  auto ectx = ctx->ExtractContext();

  if (ectx->RequiresFolders()) {
    rd.Push();

    for (size_t f = 0; f < hdr.numFiles; f++) {
      BFile cFile;
      rd.Read(cFile);
      rd.Skip((cFile.fo2.numDupes | cFile.fouc.numDupes) * 4);
      ectx->AddFolderPath(strings.at(cFile.folderId));
    }

    ectx->GenerateFolders();
    rd.Pop();
  }

  for (size_t f = 0; f < hdr.numFiles; f++) {
    BFile cFile;
    rd.Read(cFile);

    // Following are offsets to duplicate data streams
    // Data streams are identical
    // Purpose is unknown
    // Possible usage is backup data in case of media corruption
    uint32 numDupes = cFile.fo2.numDupes | cFile.fouc.numDupes;
    rd.Skip(numDupes * 4);
    auto fileName = strings.at(cFile.folderId) + "/" + strings.at(cFile.fileId);
    ectx->NewFile(fileName);
    rd.Push();
    rd.Seek(cFile.dataOffset);
    rd.ReadContainer(inBuffer, cFile.compressedSize);
    outBuffer.resize(cFile.uncompressedSize);

    if (cFile.Compressed()) {
      z_stream infstream;
      infstream.zalloc = Z_NULL;
      infstream.zfree = Z_NULL;
      infstream.opaque = Z_NULL;
      infstream.avail_in = inBuffer.size();
      infstream.next_in = reinterpret_cast<Bytef *>(&inBuffer[0]);
      infstream.avail_out = outBuffer.size();
      infstream.next_out = reinterpret_cast<Bytef *>(&outBuffer[0]);
      inflateInit(&infstream);
      int state = inflate(&infstream, Z_FINISH);
      inflateEnd(&infstream);

      if (state < 0) {
        if (infstream.msg) {
          throw std::runtime_error(infstream.msg);
        } else {
          throw std::runtime_error("zlib error: " + std::to_string(state));
        }
      }
      ectx->SendData(outBuffer);
    } else {
      ectx->SendData(inBuffer);
    }

    rd.Pop();
  }
}

size_t AppExtractStat(request_chunk requester) {
  auto data = requester(0, sizeof(Header));
  auto hdr = reinterpret_cast<Header *>(data.data());

  if (hdr->id != hdr->ID) {
    DecryptItem(*hdr, 0, 4);
    if (hdr->id != hdr->ID) {
      return 0;
    }

    return hdr->numFiles;
  } else {
    return hdr->numFiles;
  }
}
