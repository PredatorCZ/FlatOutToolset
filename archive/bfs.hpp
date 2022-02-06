/*  BFS common structures
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

#pragma once
#include "datas/supercore.hpp"

struct Header {
  static constexpr uint32 SIGNATURE = 0x20040505;
  static constexpr uint32 FOUCSIG = 0x20070310;
  static constexpr uint32 NUM_HASH_INDICES = 997;
  static constexpr uint32 ID = CompileFourCC("bfs1");
  uint32 id = ID;
  uint32 signature; // (asserted)
  uint32 headerSize;
  uint32 numFiles;
  uint32 numHashIndices = NUM_HASH_INDICES; // FO2 and later (asserted)
};

struct HashIndexFO1 {
  uint16 startFileIndex;
  uint16 numFiles;
};

struct HashIndex {
  uint32 filesOffset;
  uint32 numFiles;
};

struct FileFOBase {
  uint8 compressed; // FO1 PS2: 5 zlib, 4 store, other bool
  uint8 numDupes;
};

struct FileFOUCBase {
  uint16 storeType; // 5 zlib, 4 store
  uint16 numDupes;
};

struct BFile {
  union {
    FileFOBase fo2;
    FileFOUCBase fouc;
  };

  bool Compressed() const { return fouc.storeType & 1; }

  uint32 dataOffset;
  uint32 uncompressedSize;
  uint32 compressedSize;
  uint32 crc32; // UC only
  uint16 folderId;
  uint16 fileId;
};

struct FileFO1Base : FileFOBase {
  uint32 dataOffset;
  uint32 uncompressedSize;
  uint32 compressedSize;
  uint32 crc32;

  bool Compressed() const { return compressed & 1; }
};

struct StringsHeader {
  uint32 size;
  uint32 huffmanDataOffsets;
  uint32 stringSizes;
  uint32 huffmanTree;
  uint32 huffmanData;
};
