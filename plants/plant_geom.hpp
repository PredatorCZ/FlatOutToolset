/*  plant_geom common classes
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
#include "spike/io/bincore_fwd.hpp"
#include "spike/type/vectors_simd.hpp"
#include "spike/util/supercore.hpp"

struct PlantGeom {
  static constexpr uint32 ID = CompileFourCC("psdb");
  static constexpr uint32 VERSION = 1;
  uint32 id = ID;
  uint32 version = VERSION;
  Vector2 plantScaleOffset[4];
  Vector2 plantScaleScale[4];
  Vector4A16 translationScale;
  Vector4A16 translationOffset;

  void Read(BinReaderRef rd);
};

struct PlantVertex {
  struct {
    uint32 x : 12;
    uint32 z : 12;
    uint8 plantId : 8;
  } pack;
  uint16 y;
  uint8 rotation;
  uint8 scale;
};

struct GeomCluster {
  uint32 numPlants;
  uint32 beginPlant;
};

struct BvhBbox {
  float minMax[6];
};

struct BvhCluster {
  float bbox[6];
  uint32 unk;
  uint32 clusterIndex;
};

enum class BvhType : uint32 {
  CUT_X,
  CUT_Y,
  CUT_Z,
  CLUSTER,
};

union BvhItem {
  struct {
    BvhType cutType : 2;
    uint32 reserved : 3;
    uint32 clusterIndex : 27;
  };
  float cutPosition;
};
