/*  Plants2GLTF
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

#include "datas/app_context.hpp"
#include "datas/binreader_stream.hpp"
#include "datas/binwritter.hpp"
#include "datas/except.hpp"
#include "datas/fileinfo.hpp"
#include "datas/master_printer.hpp"
#include "gltf.hpp"
#include "plant_geom.hpp"
#include "project.h"
#include "uni/format.hpp"

std::string_view filters[]{
    "^plant_geom",
};

static AppInfo_s appInfo{
    .filteredLoad = true,
    .header = Plants2GLTF_DESC " v" Plants2GLTF_VERSION
                               ", " Plants2GLTF_COPYRIGHT "Lukas Cone",
    .filters = filters,
};

AppInfo_s *AppInitModule() { return &appInfo; }

void PlantGeom::Read(BinReaderRef rd) {
  rd.Read(id);

  if (id != ID) {
    throw es::InvalidHeaderError(id);
  }

  rd.Read(version);

  if (version != VERSION) {
    throw es::InvalidVersionError(version);
  }

  rd.Read(plantDims);
  struct {
    float xmin, xmax, ymin, ymax, zmin, zmax;
  } bbox;

  rd.Read(bbox);

  // v0 + (v1 - v0) * t

  translationOffset = Vector4A16(bbox.xmin, bbox.ymin, bbox.zmin, 0);
  translationScale =
      Vector4A16(bbox.xmax, bbox.ymax, bbox.zmax, 0) - translationOffset;
}

/*
void PlantVertex::Read(BinReaderRef rd) {
  rd.Read(pos.X);
  rd.Read(pos.Y);
  rd.Read(pos.Z);
  rd.Read(rot);
}*/

void AppProcessFile(std::istream &stream, AppContext *ctx) {
  BinReaderRef rd(stream);
  PlantGeom hdr;
  rd.Read(hdr);
  GLTF main;

  std::vector<PlantVertex> plants;
  rd.ReadContainer(plants);

  const size_t numVertices = 4 * plants.size();
  const size_t numFaces = 6 * plants.size();
  size_t vertsSlot = 0;

  {
    auto &verts = main.NewStream("vertices", 12);
    verts.target = gltf::BufferView::TargetType::ArrayBuffer;
    vertsSlot = verts.slot;
  }

  auto &faces = main.NewStream("indices");
  faces.target = gltf::BufferView::TargetType::ElementArrayBuffer;

  main.scenes.front().nodes.push_back(main.nodes.size());
  auto &node = main.nodes.emplace_back();
  node.mesh = main.meshes.size();
  auto &mesh = main.meshes.emplace_back();
  auto &prim = mesh.primitives.emplace_back();

  {
    auto [acc, slot] = main.NewAccessor(main.Stream(vertsSlot), 4);
    acc.componentType = gltf::Accessor::ComponentType::Float;
    acc.count = numVertices;
    acc.type = gltf::Accessor::Type::Vec3;
    prim.attributes["POSITION"] = slot;
  }
  {
    auto [acc, slot] = main.NewAccessor(main.Stream(faces.slot), 4);
    if (numVertices > 0xffff) {
      acc.componentType = gltf::Accessor::ComponentType::UnsignedInt;
    } else {
      acc.componentType = gltf::Accessor::ComponentType::UnsignedShort;
    }

    acc.count = numFaces;
    acc.type = gltf::Accessor::Type::Scalar;
    prim.indices = slot;
  }

  uni::FormatDescr desc;
  desc.compType = uni::DataType::R16G16B16;
  desc.outType = uni::FormatType::UNORM;

  uni::FormatCodec &codec = uni::FormatCodec::Get(desc);
  std::vector<Vector4A16> outData;
  codec.Sample(outData, reinterpret_cast<const char *>(plants.data()),
               plants.size(), sizeof(PlantVertex));
  auto &verts = main.Stream(vertsSlot);

  for (size_t p = 0; p < plants.size(); p++) {
    uint8 plantId = plants[p].pos.Y >> 8;
    // auto &dims = hdr.plantDims[plantId];
    struct {
      float width = 1;
      float height = 1;
    } dims;
    auto pos = hdr.translationScale * outData[p] + hdr.translationOffset;

    verts.wr.Write<Vector>(Vector4A16(pos + Vector4A16(-dims.width, 0, 0, 0)));
    verts.wr.Write<Vector>(Vector4A16(pos + Vector4A16(dims.width, 0, 0, 0)));
    verts.wr.Write<Vector>(
        Vector4A16(pos + Vector4A16(dims.width, dims.height, 0, 0)));
    verts.wr.Write<Vector>(
        Vector4A16(pos + Vector4A16(-dims.width, dims.height, 0, 0)));

    if (numVertices > 0xffff) {
      UIVector4A16 indices(0, 1, 2, 2);
      faces.wr.Write(indices + UIVector4A16(p * 4));
      UIVector2 indices2(3, 0);
      faces.wr.Write(indices2 + UIVector2(p * 4));
    } else {
      USVector4 indices(0, 1, 2, 2);
      faces.wr.Write(indices + USVector4(p * 4));
      USVector2 indices2(3, 0);
      faces.wr.Write(indices2 + USVector2(p * 4));
    }
  }

  {
    BinWritterRef wr(ctx->NewFile(ctx->workingFile.ChangeExtension(".glb")));
    main.FinishAndSave(wr, std::string(ctx->workingFile.GetFolder()));
  }

  BinWritterRef wr(ctx->NewFile(ctx->workingFile.ChangeExtension(".w32")));

  struct {
    float xmin = -100, xmax = 100, ymin = 0, ymax = 2, zmin = -100, zmax = 100;
  } bbox;

  hdr.plantDims[0] = {10, 10};
  hdr.plantDims[1] = {10, 10};
  hdr.plantDims[2] = {10, 10};
  hdr.plantDims[3] = {10, 10};
  hdr.plantDims[4] = {10, 10};
  hdr.plantDims[5] = {10, 10};
  hdr.plantDims[6] = {10, 10};
  hdr.plantDims[7] = {10, 10};

  wr.Write(hdr.id);
  wr.Write(hdr.version);
  wr.Write(hdr.plantDims);
  wr.Write(bbox);
  wr.Write<uint32>(plants.size());

  for (auto &p : plants) {
    // wr.Write(p.pos.X);
    //  wr.Write(p.pos.y);
    wr.Write(uint16(0));
    wr.Write(uint16(0));
    wr.Write(uint16(0));
    wr.Write(uint16(0));
    // wr.Write(p.pos.z);
    // wr.Write(p.rot);
  }

  struct Cluster {
    uint32 numPlants;
    uint32 startPlant;
  };

  std::vector<Cluster> clusters;
  rd.ReadContainer(clusters);
  wr.WriteContainerWCount(clusters);
}
