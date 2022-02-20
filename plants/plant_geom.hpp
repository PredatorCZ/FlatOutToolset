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
#include "datas/bincore_fwd.hpp"
#include "datas/vectors_simd.hpp"

struct PlantGeom {
  static constexpr uint32 ID = CompileFourCC("psdb");
  static constexpr uint32 VERSION = 1;
  uint32 id = ID;
  uint32 version = VERSION;
  struct {
    float width;
    float height;
  } plantDims[8];
  Vector4A16 translationScale;
  Vector4A16 translationOffset;

  void Read(BinReaderRef rd);
};

struct PlantVertex {
  USVector pos;
  uint16 rot;

  //void Read(BinReaderRef rd);
};
