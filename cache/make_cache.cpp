/*  FlatoutCache
    Copyright(C) 2023 Lukas Cone

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

#include "bfcache_internal.hpp"
#include "project.h"
#include "spike/app_context.hpp"
#include "spike/crypto/crc32.hpp"
#include "spike/crypto/jenkinshash3.hpp"
#include "spike/io/binreader.hpp"
#include "spike/io/binwritter.hpp"
#include "spike/io/stat.hpp"
#include <map>
#include <mutex>
#include <set>
#include <zstd.h>

static AppInfo_s appInfo{
    .header = FlatoutCache_DESC " v" FlatoutCache_VERSION
                                ", " FlatoutCache_COPYRIGHT "Lukas Cone",
};

AppInfo_s *AppInitModule() { return &appInfo; }

struct SubStream {
  size_t offset;
  uint32 size;
};

struct SubStreamCrc : SubStream {
  uint32 crc;
};

struct Stream {
  std::string streamPath;
  BinWritter_t<BinCoreOpenMode::NoBuffer> streamStore;
  size_t blobalOffset = 0;

  Stream(std::string &&path)
      : streamPath(std::move(path)), streamStore(streamPath) {}

  SubStreamCrc SendStream(std::istream &str, bool addNewline = false) {
    const size_t inputSize = BinReaderRef(str).GetSize();
    char buffer[0x10000];
    const size_t numChunks = inputSize / sizeof(buffer);
    const size_t restBytes = inputSize % sizeof(buffer);
    SubStreamCrc retVal;
    retVal.offset = streamStore.Tell();
    uint32 crc = 0;

    for (size_t i = 0; i < numChunks; i++) {
      str.read(buffer, sizeof(buffer));
      streamStore.WriteBuffer(buffer, sizeof(buffer));
      crc = crc32b(crc, buffer, sizeof(buffer));
    }

    if (restBytes) {
      str.read(buffer, restBytes);
      streamStore.WriteBuffer(buffer, restBytes);
      crc = crc32b(crc, buffer, restBytes);
    }

    if (addNewline) {
      streamStore.Write('\n');
    }

    retVal.size = streamStore.Tell() - retVal.offset;
    retVal.crc = crc;

    return retVal;
  }
};

using namespace bfcache;

struct IFile {
  uint32 nameHash;
  CacheFileType type;
  SubStream location;

  bool operator<(const IFile &o) const {
    if (nameHash == o.nameHash) {
      return uint32(type) < uint32(o.type);
    }

    return nameHash < o.nameHash;
  }
};

using FileGroup = std::map<uint32, Stream>;

struct MakeContext : AppPackContext {
  std::string baseFile;
  std::map<CacheFileType, Stream> streams;
  std::mutex mtx;
  std::vector<IFile> files;
  std::set<std::string> fileNames;
  //<crc, fileIndex>
  std::map<uint32, uint32> fileCrcs;

  MakeContext(std::string baseFile_, const AppPackStats &)
      : baseFile(std::move(baseFile_)) {}

  void SendFile(std::string_view path_, std::istream &stream) override {
    std::string strPath(path_);
    std::transform(strPath.begin(), strPath.end(), strPath.begin(),
                   [](char c) { return std::tolower(c); });
    std::string_view path(strPath);

    auto dot = path.find_last_of('.');

    if (dot == path.npos) {
      throw std::runtime_error("File not supported.");
    }

    auto slash = path.find_last_of('/');
    auto fileName = path.substr(slash + 1);
    CacheFileType fileType = FromFileName(fileName);
    std::string_view filePath([&] {
      if (ExcludeFileName(fileType)) {
        return path.substr(0, slash);
      }

      if (IncludeExtension(fileType)) {
        return path;
      }

      return path.substr(0, dot);
    }());

    std::lock_guard lg(mtx);

    if (!streams.contains(fileType)) {
      streams.emplace(fileType, baseFile + std::string(fileName));
    }

    auto &curStream = streams.at(fileType);

    SubStreamCrc streamRange = curStream.SendStream(stream);

    if (auto found = fileCrcs.find(streamRange.crc); found != fileCrcs.end()) {
      IFile curFile = files.at(found->second);
      curFile.nameHash = JenkinsHash3_(filePath);
      files.emplace_back(curFile);
      curStream.streamStore.Seek(streamRange.offset);
    } else {
      IFile curFile{
          .nameHash = JenkinsHash3_(filePath),
          .type = fileType,
          .location = streamRange,
      };

      fileCrcs.emplace(streamRange.crc, uint32(files.size()));
      files.emplace_back(curFile);
    }

    fileNames.emplace(filePath);
  }

  void Finish() override {

    {
      BinWritter outRefs(baseFile + ".refs");
      outRefs.WriteContainer("Number of duplicates: " +
                             std::to_string(files.size() - fileCrcs.size()));
      outRefs.Write('\n');

      for (auto &f : fileNames) {
        outRefs.WriteContainer(std::to_string(JenkinsHash3_(f)));
        outRefs.Write('\t');
        outRefs.WriteContainer(f);
        outRefs.Write('\n');
      }
    }

    BinWritter outIdx(baseFile + ".bfmod");
    BinWritter_t<BinCoreOpenMode::NoBuffer> outData(baseFile + ".dat");
    std::sort(files.begin(), files.end());

    Cache outCache;
    outCache.files.numItems = files.size();
    outIdx.Write(outCache);
    outIdx.ApplyPadding(alignof(CacheBlock));
    outCache.blocks.pointer = outIdx.Tell() - offsetof(Cache, blocks.pointer);

    constexpr size_t BLOCK_SIZE = 0x40000;
    char block[BLOCK_SIZE]{};
    size_t blockAvail = BLOCK_SIZE;

    ZSTD_CCtx *zctx = ZSTD_createCCtx();
    std::string zBuffer;
    zBuffer.resize(ZSTD_compressBound(BLOCK_SIZE));

    auto WriteCompressedBlock = [&] {
      outCache.blocks.numItems++;
      const size_t dataSize = BLOCK_SIZE - blockAvail;

      const uint32 cSize = ZSTD_compressCCtx(
          zctx, zBuffer.data(), zBuffer.size(), block, dataSize, ZSTD_btultra);

      CacheBlock nBlock{
          .offset = outData.Tell(),
          .size = cSize,
          .crc = crc32b(0, zBuffer.data(), cSize),
      };

      outIdx.Write(nBlock);
      outData.WriteBuffer(zBuffer.data(), cSize);
      blockAvail = BLOCK_SIZE;
    };

    auto WriteUncompressedBlock = [&] {
      outCache.blocks.numItems++;
      const uint32 dataSize = BLOCK_SIZE - blockAvail;

      CacheBlock nBlock{
          .offset = outData.Tell(),
          .size = dataSize,
          .crc = crc32b(0, block, dataSize),
      };

      outIdx.Write(nBlock);
      outData.WriteBuffer(block, dataSize);
      blockAvail = BLOCK_SIZE;
    };

    auto WriteBlock = [&] {
      if (outCache.streamingBlocksBegin != -1) {
        WriteUncompressedBlock();
      } else {
        WriteCompressedBlock();
      }
    };

    size_t currentOffset = 0;

    auto StreamBlocks = [&](auto &stream) {
      es::Dispose(stream.streamStore);
      stream.blobalOffset = currentOffset;
      BinReader rd(stream.streamPath);
      size_t streamSize = rd.GetSize();
      currentOffset += streamSize;

      while (streamSize) {
        size_t blockFill = std::min(blockAvail, streamSize);
        streamSize -= blockFill;
        rd.ReadBuffer(block + (BLOCK_SIZE - blockAvail), blockFill);
        blockAvail -= blockFill;

        if (blockAvail == 0) {
          WriteBlock();
        }
      }

      es::Dispose(rd);
      es::RemoveFile(stream.streamPath);
    };

    for (auto &[fileType, stream] : streams) {
      if (outCache.uncachedBlocksBegin == 0 &&
          fileType >= CacheFileType::UnknownUncachedBinaryResource) {
        outCache.uncachedBlocksBegin = outCache.blocks.numItems;
      } else if (outCache.streamingBlocksBegin == 0 &&
                 fileType >= CacheFileType::Ogg) {
        if (blockAvail > 0) {
          currentOffset += blockAvail;
          WriteBlock();
        }
        outCache.streamingBlocksBegin = outCache.blocks.numItems;
      }

      StreamBlocks(stream);
    }

    if (blockAvail != BLOCK_SIZE) {
      WriteBlock();
    }

    outIdx.ApplyPadding(alignof(CacheFile));

    outCache.files.pointer = outIdx.Tell() - offsetof(Cache, files.pointer);

    for (auto &f : files) {
      const size_t globOffset = streams.at(f.type).blobalOffset;

      uint32 microOffset = (globOffset + f.location.offset) % BLOCK_SIZE;
      uint32 tailSize = f.location.size;
      uint32 numBlocks = [&]() -> uint32 {
        if (f.location.size <= BLOCK_SIZE - microOffset) {
          return 0;
        }

        uint32 restSize = f.location.size - (BLOCK_SIZE - microOffset);
        tailSize = restSize % BLOCK_SIZE;

        return (restSize / BLOCK_SIZE) + 1;
      }();

      CacheFile file{
          .nameHash = f.nameHash,
          .fileType = f.type,
          .streamId = 0,
          .microOffset = microOffset,
          .numBlocks = numBlocks,
          .tailSize = tailSize,
          .blockOffset = uint32((globOffset + f.location.offset) / BLOCK_SIZE),
      };
      outIdx.Write(file);
    }

    outIdx.Seek(0);
    outIdx.Write(outCache);
  }
};

AppPackContext *AppNewArchive(const std::string &folder,
                              const AppPackStats &stats) {
  auto file = folder;
  while (file.back() == '/') {
    file.pop_back();
  }

  return new MakeContext(std::move(file), stats);
}
