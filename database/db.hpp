/*  DB common classes
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
#include "datas/reflector.hpp"
#include "datas/string_view.hpp"

struct DB {
  static constexpr uint32 ID = CompileFourCC("PDB\x1a");
  uint32 id = ID;
  uint32 version = 0x200;
  uint32 numNodes;
};

MAKE_ENUM(ENUMSCOPE(class DBValueType
                    : uint8, DBValueType),
          EMEMBER(None), EMEMBER(UINT8), EMEMBER(STRING), EMEMBERVAL(BOOL32, 5),
          EMEMBER(INT32), EMEMBER(FLOAT), EMEMBER(COLOR), EMEMBER(VECTOR2),
          EMEMBER(VECTOR3), EMEMBER(VECTOR4), EMEMBER(REF));

MAKE_ENUM(ENUMSCOPE(class DBValueSubType
                    : uint8, DBValueSubType),
          EMEMBER(Simple), EMEMBER(Array), EMEMBER(CString));

struct DBValue {
  uint32 nameOffset;
  DBValueType type;
  uint8 dataSize;
  uint8 dataSize2; // dataSize cont
  DBValueSubType subType;
  uint32 tag; // 0xA data, 0x165c7a0 ref

  uint16 DataSize() const { return dataSize | (uint16(dataSize2) << 8); }

  template <class C> const C *Data() const {
    return reinterpret_cast<const C *>(this + 1);
  }

  es::string_view Name() const {
    return reinterpret_cast<const char *>(this) + nameOffset;
  }

  const DBValue *Next() const {
    return reinterpret_cast<const DBValue *>(Data<char>() + DataSize());
  }
};

struct DBNode {
  uint32 null;
  int16 parentNodeSkipIndex; // relative to this node
  uint16 numChildren;
  int16 prevEntrySkipIndex; // previous entry on the same level, relative to
                            // this node
  uint16 numValues;
  uint32 nodeNameOffset;
  uint32 dataOffset;

  es::string_view Name() const {
    return reinterpret_cast<const char *>(this) + nodeNameOffset;
  }

  const DBValue *Data() const {
    return reinterpret_cast<const DBValue *>(
        reinterpret_cast<const char *>(this) + dataOffset);
  }

  const DBNode *Parent() const { return this + parentNodeSkipIndex; }
};
