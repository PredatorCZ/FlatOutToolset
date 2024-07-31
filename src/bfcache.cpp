/*  LibSwarm
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

#include "spike/crypto/jenkinshash3.hpp"

#include <algorithm>
#include <chrono>
#include <cstring>
#include <map>
#include <sstream>
#include <stdexcept>
#include <vector>
#include <zstd.h>

namespace bfcache {
constexpr uint32_t BLOCK_SIZE = 0x40000;
constexpr uint32_t NUM_CACHED_BLOCKS = 3;

struct {
  char block[BLOCK_SIZE];
  uint32_t id = -1;
  uint32_t streamId = -1;
} BLOCK_CACHE[NUM_CACHED_BLOCKS + 1]{};

struct {
  std::vector<Cache *> caches;
  std::vector<std::istream *> streams;
} CACHE_MANGER;

struct CacheFileRequest {
  uint32_t nameHash;
  CacheFileType fileType;
};

size_t FileSize(const CacheFile &file) {
  return file.numBlocks ? (file.numBlocks - 1) * BLOCK_SIZE + file.tailSize +
                              (BLOCK_SIZE - file.microOffset)
                        : file.tailSize;
}

CacheFileType FromFileName(std::string_view file) {
  static const std::map<std::string_view, CacheFileType> EXT_REGISTRY{
      /**/ //
      {"dds", CacheFileType::Dds},
      {"tga", CacheFileType::Tga},
      {"wav", CacheFileType::Wav},
      {"ogg", CacheFileType::Ogg},
      {"txt", CacheFileType::UnknownTextResource},
      {"cfg", CacheFileType::UnknownTextResource},
      {"dat", CacheFileType::UnknownBinaryResource},
      {"rep", CacheFileType::UnknownBinaryResource},
      {"avi", CacheFileType::Avi},
      {"ini", CacheFileType::Ini},
      {"bed", CacheFileType::Bed},
      {"sha", CacheFileType::Shader},
      {"bgm", CacheFileType::Bgm},
      {"bsa", CacheFileType::Bsa},
  };

  static const std::map<std::string_view, CacheFileType> FILE_REGISTRY{
      /**/ //
      {"effectmap.4b", CacheFileType::EffectMap},
      {"resetmap.4b", CacheFileType::ResetMap},
      {"splines.ai", CacheFileType::Splines},
      {"trackai.bin", CacheFileType::TrackAi},
      {"plant_vdb.gen", CacheFileType::PlantsTrie},
      {"track_bvh.gen", CacheFileType::TrackTrie},
      {"track_cdb2.gen", CacheFileType::TrackCollision},
      {"track_spvs.gen", CacheFileType::TrackSpvs},
      {"plant_geom.w32", CacheFileType::Plants},
      {"track_geom.w32", CacheFileType::Track},
      {"crash.dat", CacheFileType::Crash},
  };

  if (auto found = FILE_REGISTRY.find(file); found != FILE_REGISTRY.end()) {
    return found->second;
  }

  std::string_view ext = file.substr(file.find_last_of('.') + 1);

  if (auto found = EXT_REGISTRY.find(ext); found != EXT_REGISTRY.end()) {
    return found->second;
  }

  return CacheFileType::UnknownUncachedBinaryResource;
}

CacheFile *FindFile(CacheFileRequest &fileRequest, Cache *cache) {
  auto found =
      std::lower_bound(cache->files.begin(), cache->files.end(), fileRequest,
                       [](const CacheFile &t, const CacheFileRequest &o) {
                         if (t.nameHash == o.nameHash) {
                           return uint32_t(t.fileType) < uint32_t(o.fileType);
                         }

                         return t.nameHash < o.nameHash;
                       });

  if (found != cache->files.end() && found->nameHash == fileRequest.nameHash &&
      found->fileType == fileRequest.fileType) {
    return found;
  }

  return nullptr;
}

CacheFile *FindFile(CacheFileType fileType, std::string_view filePath,
                    Cache *cache) {
  CacheFileRequest fileRequest{
      .nameHash = JenkinsHash3_(filePath),
      .fileType = fileType,
  };

  return FindFile(fileRequest, cache);
}

const CacheFile *GetFile(std::string_view path) {
  auto slash = path.find_last_of('/');
  auto dot = path.find_last_of('.');

  if (dot == path.npos) {
    throw std::runtime_error("File not supported.");
  }

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

  CacheFileRequest fileRequest{
      .nameHash = JenkinsHash3_(filePath),
      .fileType = fileType,
  };

  for (auto cache : CACHE_MANGER.caches) {
    if (auto found = FindFile(fileRequest, cache); found) {
      return found;
    }
  }

  return nullptr;
}

char *LoadBlock(uint32_t streamIndex, uint32_t blockIndex, CacheBlock block) {
  char imBuffer[BLOCK_SIZE + 0x20]{};
  if (block.size > sizeof(imBuffer)) {
    throw std::runtime_error(std::to_string(block.size) + ">" +
                             std::to_string(sizeof(imBuffer)) + " block " +
                             std::to_string(blockIndex) + " stream " +
                             std::to_string(streamIndex));
  }
  auto cStream = CACHE_MANGER.streams.at(streamIndex);
  cStream->clear();
  cStream->seekg(block.offset);
  cStream->read(imBuffer, block.size);

  if (cStream->fail()) {
    throw std::runtime_error("stream fail " + std::to_string(block.offset) +
                             " " + std::to_string(block.size));
  }

  auto state = ZSTD_decompress(BLOCK_CACHE[blockIndex].block, BLOCK_SIZE,
                               imBuffer, block.size);

  if (ZSTD_isError(state)) {
    throw std::runtime_error("Error decompressing block");
  }

  return BLOCK_CACHE[blockIndex].block;
}

char *GetBlockData(uint32_t blockId, uint32_t streamIndex) {
  const Cache *cacheHeader = CACHE_MANGER.caches.at(streamIndex);

  if (blockId >= cacheHeader->uncachedBlocksBegin) {
    auto &block = BLOCK_CACHE[NUM_CACHED_BLOCKS];
    if (block.id == blockId && streamIndex == block.streamId) {
      return block.block;
    }

    block.id = blockId;
    block.streamId = streamIndex;

    return LoadBlock(streamIndex, NUM_CACHED_BLOCKS,
                     cacheHeader->blocks[blockId]);
  }

  static uint32_t useScore[NUM_CACHED_BLOCKS]{};

  for (uint32_t i = 0; i < NUM_CACHED_BLOCKS; i++) {
    auto &block = BLOCK_CACHE[i];

    if (block.id == blockId && streamIndex == block.streamId) {
      useScore[i]++;
      return block.block;
    }
  }

  uint32_t lowestScore = -1;
  uint32_t lowestScoreIndex = 0;

  for (uint32_t i = 0; i < NUM_CACHED_BLOCKS; i++) {
    if (useScore[i] < lowestScore) {
      lowestScore = useScore[i];
      lowestScoreIndex = i;
    }
  }

  useScore[lowestScoreIndex] = 0;
  BLOCK_CACHE[lowestScoreIndex].id = blockId;
  BLOCK_CACHE[lowestScoreIndex].streamId = streamIndex;

  return LoadBlock(streamIndex, lowestScoreIndex, cacheHeader->blocks[blockId]);
}

void GetFileData(const CacheFile &file, char *outBuffer, uint32_t position,
                 uint32_t dataToBeRead) {
  const Cache *cacheHeader = CACHE_MANGER.caches.at(file.streamId);

  if (file.blockOffset >= cacheHeader->streamingBlocksBegin) {
    auto cStream = CACHE_MANGER.streams.at(file.streamId);
    cStream->seekg(cacheHeader->blocks[file.blockOffset].offset +
                   file.microOffset + position);
    cStream->read(outBuffer, dataToBeRead);
    return;
  }

  uint32_t startBlock =
      file.blockOffset + ((file.microOffset + position) / BLOCK_SIZE);
  const uint32_t microOffset = (file.microOffset + position) % BLOCK_SIZE;

  if (startBlock >= cacheHeader->blocks.numItems) {
    throw std::runtime_error(
        "Block out of range " + std::to_string(file.nameHash) + " " +
        std::to_string((uint32_t)file.fileType) + " " +
        std::to_string(file.blockOffset) + " " + std::to_string(file.streamId));
  }

  const char *blockData = GetBlockData(startBlock++, file.streamId);
  const uint32_t dataToRead = std::min(dataToBeRead, BLOCK_SIZE - microOffset);

  memcpy(outBuffer, blockData + microOffset, dataToRead);
  outBuffer += dataToRead;
  dataToBeRead -= dataToRead;

  while (dataToBeRead) {
    blockData = GetBlockData(startBlock++, file.streamId);
    const uint32_t dataToRead = std::min(dataToBeRead, BLOCK_SIZE);
    memcpy(outBuffer, blockData, dataToRead);
    outBuffer += dataToRead;
    dataToBeRead -= dataToRead;
  }
}

void AddCachePair(Cache *c, std::istream *s) {
  CACHE_MANGER.caches.emplace_back(c);
  CACHE_MANGER.streams.emplace_back(s);
}

void PatchCache() {
  if (CACHE_MANGER.caches.size() < 2) {
    return;
  }

  Cache *baseCache = nullptr;
  uint32_t currentCacheIndex = 1;

  for (auto c : CACHE_MANGER.caches) {
    if (!baseCache) {
      baseCache = c;
      continue;
    }

    for (auto &file : c->files) {
      CacheFileRequest req{
          .nameHash = file.nameHash,
          .fileType = file.fileType,
      };

      file.streamId = currentCacheIndex;

      auto foundFile = FindFile(req, baseCache);

      if (foundFile) {
        *foundFile = file;
      } else if (file.fileType == CacheFileType::Dds) {
        req.fileType = CacheFileType::Tga;
        auto foundFile = FindFile(req, baseCache);

        if (foundFile) {
          *foundFile = file;
        }
      } else {
        // TODO walk hierarchy
        printf("cannot patch %u\n", file.nameHash);
      }
    }

    currentCacheIndex++;
  }
}

} // namespace bfcache
