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

#include "nlohmann/json.hpp"
#include "plant_geom.hpp"
#include "project.h"
#include "spike/app_context.hpp"
#include "spike/except.hpp"
#include "spike/gltf.hpp"
#include "spike/io/binreader_stream.hpp"
#include "spike/io/binwritter.hpp"
#include "spike/io/fileinfo.hpp"
#include "spike/master_printer.hpp"
#include "spike/reflect/reflector.hpp"
#include "spike/uni/format.hpp"

std::string_view filters[]{
    "^plant_geom",
};

struct Plants2GLTF : ReflectorBase<Plants2GLTF> {
  bool splitToClusters = false;
  bool visualizeBVH = false;
} settings;

REFLECT(CLASS(Plants2GLTF),
        MEMBER(splitToClusters, "C", ReflDesc{"Split plants into clusters."}),
        MEMBER(visualizeBVH, "B",
               ReflDesc{"Visualize BVH and plant cluster bounds. This will "
                        "also splits plants into clusters."}));

static AppInfo_s appInfo{
    .header = Plants2GLTF_DESC " v" Plants2GLTF_VERSION
                               ", " Plants2GLTF_COPYRIGHT "Lukas Cone",
    .settings = reinterpret_cast<ReflectorFriend *>(&settings),
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

  rd.Read(plantScaleOffset);
  rd.Read(plantScaleScale);
  for (size_t p = 0; p < 4; p++) {
    plantScaleScale[p] -= plantScaleOffset[p];
  }
  struct {
    float xmin, xmax, ymin, ymax, zmin, zmax;
  } bbox;

  rd.Read(bbox);

  // v0 + (v1 - v0) * t

  translationOffset = Vector4A16(bbox.xmin, bbox.ymin, bbox.zmin, 0);
  translationScale =
      Vector4A16(bbox.xmax, bbox.ymax, bbox.zmax, 0) - translationOffset;
}

struct GLTFMain : GLTF {
  PlantGeom hdr;
  std::vector<PlantVertex> plants;
  std::vector<GeomCluster> clusters;
  std::map<uint32, std::vector<uint32>> colorGroups;
  std::vector<BvhCluster> bvhClusters;
  BvhBbox mainBBOX;
  std::vector<BvhItem> bvhTree;

  size_t vertsSlot;
  size_t uvSlot;
  size_t indexSlot;

  GLTFMain() {
    {
      auto &verts = NewStream("vertices", 12);
      verts.target = gltf::BufferView::TargetType::ArrayBuffer;
      vertsSlot = verts.slot;
    }
    {
      auto &verts = NewStream("vertices_uvs", 4);
      verts.target = gltf::BufferView::TargetType::ArrayBuffer;
      uvSlot = verts.slot;
    }

    auto &faces = NewStream("indices");
    faces.target = gltf::BufferView::TargetType::ElementArrayBuffer;
    indexSlot = faces.slot;
  }
};

struct BvhCtx {
  GLTFMain &main;
  GLTFStream &verts;
  uint32 indices8Index;
  float depthFrac;
  GLTFStream &uvs;
  gltf::Accessor plantIndexTempl{};
};

void NewBox(BvhCtx &ctx, BvhBbox &bbox, uint32 depth) {
  auto &node = ctx.main.nodes.emplace_back();
  node.mesh = ctx.main.meshes.size();
  auto &mesh = ctx.main.meshes.emplace_back();
  auto &prim = mesh.primitives.emplace_back();
  prim.mode = gltf::Primitive::Mode::LineStrip;
  prim.indices = ctx.indices8Index;

  auto &verts = ctx.verts;

  {
    auto [acc, slot] = ctx.main.NewAccessor(verts, 4);
    acc.componentType = gltf::Accessor::ComponentType::Float;
    acc.count = 8;
    acc.type = gltf::Accessor::Type::Vec3;
    acc.min.resize(3);
    acc.max.resize(3);
    memcpy(acc.min.data(), bbox.minMax, 12);
    memcpy(acc.max.data(), bbox.minMax + 3, 12);
    prim.attributes["POSITION"] = slot;
  }
  {
    auto [acc, slot] = ctx.main.NewAccessor(ctx.uvs, 4);
    acc.componentType = gltf::Accessor::ComponentType::UnsignedByte;
    acc.count = 8;
    acc.type = gltf::Accessor::Type::Vec4;
    acc.normalized = true;
    prim.attributes["COLOR_0"] = slot;
  }

  verts.wr.Write(Vector(bbox.minMax[0], bbox.minMax[4], bbox.minMax[2]));
  verts.wr.Write(Vector(bbox.minMax[0], bbox.minMax[1], bbox.minMax[2]));
  verts.wr.Write(Vector(bbox.minMax[3], bbox.minMax[1], bbox.minMax[2]));
  verts.wr.Write(Vector(bbox.minMax[3], bbox.minMax[4], bbox.minMax[2]));
  verts.wr.Write(Vector(bbox.minMax[0], bbox.minMax[1], bbox.minMax[5]));
  verts.wr.Write(Vector(bbox.minMax[3], bbox.minMax[1], bbox.minMax[5]));
  verts.wr.Write(Vector(bbox.minMax[3], bbox.minMax[4], bbox.minMax[5]));
  verts.wr.Write(Vector(bbox.minMax[0], bbox.minMax[4], bbox.minMax[5]));

  uint32 color = 0xff | (uint32(ctx.depthFrac * depth) * 255);

  for (auto v = 0; v < 8; v++) {
    ctx.uvs.wr.Write(color);
  }
}

void NewClusterBox(BvhCtx &ctx, uint32 clusterIndex) {
  auto &c = ctx.main.bvhClusters.at(clusterIndex);
  auto &node = ctx.main.nodes.emplace_back();
  node.mesh = ctx.main.meshes.size();
  auto &mesh = ctx.main.meshes.emplace_back();
  auto &prim = mesh.primitives.emplace_back();
  prim.mode = gltf::Primitive::Mode::LineStrip;
  prim.indices = ctx.indices8Index;

  float bbox[]{
      c.bbox[0] + c.bbox[3], c.bbox[1] + c.bbox[4], c.bbox[2] + c.bbox[5],
      c.bbox[0] - c.bbox[3], c.bbox[1] - c.bbox[4], c.bbox[2] - c.bbox[5],
  };

  {
    auto [acc, slot] = ctx.main.NewAccessor(ctx.verts, 4);
    acc.componentType = gltf::Accessor::ComponentType::Float;
    acc.count = 8;
    acc.type = gltf::Accessor::Type::Vec3;
    acc.min.resize(3);
    acc.max.resize(3);
    memcpy(acc.min.data(), bbox + 3, 12);
    memcpy(acc.max.data(), bbox, 12);
    prim.attributes["POSITION"] = slot;
  }

  {
    auto [acc, slot] = ctx.main.NewAccessor(ctx.uvs, 4);
    acc.componentType = gltf::Accessor::ComponentType::UnsignedByte;
    acc.count = 8;
    acc.type = gltf::Accessor::Type::Vec4;
    acc.normalized = true;
    prim.attributes["COLOR_0"] = slot;
  }

  ctx.verts.wr.Write(Vector(bbox[0], bbox[4], bbox[2]));
  ctx.verts.wr.Write(Vector(bbox[0], bbox[1], bbox[2]));
  ctx.verts.wr.Write(Vector(bbox[3], bbox[1], bbox[2]));
  ctx.verts.wr.Write(Vector(bbox[3], bbox[4], bbox[2]));
  ctx.verts.wr.Write(Vector(bbox[0], bbox[1], bbox[5]));
  ctx.verts.wr.Write(Vector(bbox[3], bbox[1], bbox[5]));
  ctx.verts.wr.Write(Vector(bbox[3], bbox[4], bbox[5]));
  ctx.verts.wr.Write(Vector(bbox[0], bbox[4], bbox[5]));

  for (auto v = 0; v < 8; v++) {
    ctx.uvs.wr.Write(0xff00);
  }
}

void DoPlants(GLTFMain &main, uint32 beginPlant, uint32 numPlants,
              gltf::Primitive &prim) {
  GLTFStream &uvs = main.Stream(main.uvSlot);
  GLTFStream &verts = main.Stream(main.vertsSlot);

  {
    auto [acc, slot] = main.NewAccessor(uvs, 4);
    acc.componentType = gltf::Accessor::ComponentType::UnsignedShort;
    acc.normalized = true;
    acc.count = numPlants * 4;
    acc.type = gltf::Accessor::Type::Vec2;
    prim.attributes["TEXCOORD_0"] = slot;
  }

  auto [accPos, slotPos] = main.NewAccessor(verts, 4);
  accPos.componentType = gltf::Accessor::ComponentType::Float;
  accPos.count = numPlants * 4;
  accPos.type = gltf::Accessor::Type::Vec3;
  prim.attributes["POSITION"] = slotPos;

  Vector4A16 posMax(-FLT_MAX);
  Vector4A16 posMin(FLT_MAX);

  auto &plant = main.hdr;

  for (size_t p = 0; p < numPlants; p++) {
    auto &plantVert = main.plants.at(beginPlant + p);
    Vector4A16 ps(plantVert.pack.x, plantVert.y, plantVert.pack.z,
                  plantVert.scale);
    ps *= Vector4A16(1) / (Vector4A16(1 << 12, 1 << 16, 1 << 12, 1 << 8) - 1);
    auto plantScale =
        (plant.plantScaleOffset[plantVert.pack.plantId / 4] +
         plant.plantScaleScale[plantVert.pack.plantId / 4] * ps.w) *
        plant.translationScale.y;
    float nrot = (plantVert.rotation * (1 / 255.f) - 0.5f) * 3.14159265f * 2;
    float s = sin(nrot);
    float c = cos(nrot);
    float fac = c - s;
    float yc = s + c;

    auto pos = plant.translationScale * ps + plant.translationOffset;

    Vector4A16 vts[]{
        Vector4A16(-plantScale.x * fac, 0, -plantScale.x * yc, 0),
        Vector4A16(plantScale.x * fac, 0, plantScale.x * yc, 0),
        Vector4A16(plantScale.x * fac, plantScale.y, plantScale.x * yc, 0),
        Vector4A16(-plantScale.x * fac, plantScale.y, -plantScale.x * yc, 0),
    };

    for (auto v : vts) {
      v += pos;
      verts.wr.Write<Vector>(v);
      posMax._data = _mm_max_ps(posMax._data, v._data);
      posMin._data = _mm_min_ps(posMin._data, v._data);
    }

    const float uvFrac = (1.f / 4.f) * 0xffff;
    const float uOffset = uvFrac * (plantVert.pack.plantId % 4);
    const float vOffset = uvFrac * (plantVert.pack.plantId / 4);

    uvs.wr.Write(USVector2(uOffset, vOffset + uvFrac));
    uvs.wr.Write(USVector2(uOffset + uvFrac, vOffset + uvFrac));
    uvs.wr.Write(USVector2(uOffset + uvFrac, vOffset));
    uvs.wr.Write(USVector2(uOffset, vOffset));
  }

  accPos.min.resize(3);
  accPos.max.resize(3);
  memcpy(accPos.min.data(), &posMin, 12);
  memcpy(accPos.max.data(), &posMax, 12);

  size_t curColorIndex = 0;
  for (auto [_, colors] : main.colorGroups) {
    {
      auto [acc, slot] = main.NewAccessor(uvs, 4);
      acc.componentType = gltf::Accessor::ComponentType::UnsignedByte;
      acc.count = numPlants * 4;
      acc.type = gltf::Accessor::Type::Vec4;
      acc.normalized = true;
      prim.attributes["COLOR_" + std::to_string(curColorIndex++)] = slot;
    }

    for (size_t p = 0; p < numPlants; p++) {
      uint32 color = colors.at(beginPlant + p);

      uvs.wr.Write(color);
      uvs.wr.Write(color);
      uvs.wr.Write(color);
      uvs.wr.Write(color);
    }
  }
}

void SavePlants(GLTFMain &main) {
  const size_t numVertices = 4 * main.plants.size();
  const size_t numFaces = 6 * main.plants.size();

  main.scenes.front().nodes.push_back(main.nodes.size());
  auto &node = main.nodes.emplace_back();
  node.mesh = main.meshes.size();
  auto &mesh = main.meshes.emplace_back();
  auto &prim = mesh.primitives.emplace_back();

  {
    auto &faces = main.Stream(main.indexSlot);
    auto [acc, slot] = main.NewAccessor(faces, 4);
    if (numVertices > 0xffff) {
      acc.componentType = gltf::Accessor::ComponentType::UnsignedInt;
      for (size_t p = 0; p < main.plants.size(); p++) {
        UIVector4 indices(0, 1, 2, 2);
        faces.wr.Write(indices + UIVector4(p * 4));
        UIVector2 indices2(3, 0);
        faces.wr.Write(indices2 + UIVector2(p * 4));
      }

    } else {
      acc.componentType = gltf::Accessor::ComponentType::UnsignedShort;
      for (size_t p = 0; p < main.plants.size(); p++) {
        USVector4 indices(0, 1, 2, 2);
        faces.wr.Write(indices + USVector4(p * 4));
        USVector2 indices2(3, 0);
        faces.wr.Write(indices2 + USVector2(p * 4));
      }
    }

    acc.count = numFaces;
    acc.type = gltf::Accessor::Type::Scalar;
    prim.indices = slot;
  }

  DoPlants(main, 0, main.plants.size(), prim);
}

void NewCluster(BvhCtx &ctx, uint32 clusterIndex) {
  auto &cg = ctx.main.clusters.at(clusterIndex);

  auto &node = ctx.main.nodes.emplace_back();
  node.mesh = ctx.main.meshes.size();
  auto &mesh = ctx.main.meshes.emplace_back();
  auto &prim = mesh.primitives.emplace_back();
  prim.indices = ctx.main.accessors.size();
  ctx.main.accessors.emplace_back(ctx.plantIndexTempl).count = cg.numPlants * 6;

  DoPlants(ctx.main, cg.beginPlant, cg.numPlants, prim);
}

void ProcessBvh(BvhCtx &ctx, BvhBbox bbox, uint32 bvhItem, uint32 parentNode,
                uint32 depth) {
  uint32 thisNodeIndex = ctx.main.nodes.size();
  ctx.main.nodes.at(parentNode).children.emplace_back(thisNodeIndex);
  NewBox(ctx, bbox, depth);
  BvhItem curItem = ctx.main.bvhTree.at(bvhItem - 1);

  if (curItem.cutPosition == 0) {
    return;
  }

  BvhBbox half0 = bbox;
  BvhBbox half1 = bbox;

  switch (curItem.cutType) {
  case BvhType::CLUSTER:
    ctx.main.nodes.at(thisNodeIndex)
        .children.emplace_back(ctx.main.nodes.size());
    NewClusterBox(ctx, curItem.clusterIndex);
    ctx.main.nodes.at(ctx.main.nodes.size() - 1)
        .children.emplace_back(ctx.main.nodes.size());
    NewCluster(ctx, ctx.main.bvhClusters.at(curItem.clusterIndex).clusterIndex);
    return;
  case BvhType::CUT_X:
    half0.minMax[0] = curItem.cutPosition;
    half1.minMax[3] = curItem.cutPosition;
    break;
  case BvhType::CUT_Y:
    half0.minMax[1] = curItem.cutPosition;
    half1.minMax[4] = curItem.cutPosition;
    break;
  case BvhType::CUT_Z:
    half0.minMax[2] = curItem.cutPosition;
    half1.minMax[5] = curItem.cutPosition;
    break;
  default:
    return;
  }

  ProcessBvh(ctx, half1, bvhItem * 2, thisNodeIndex, depth + 1);
  ProcessBvh(ctx, half0, bvhItem * 2 + 1, thisNodeIndex, depth + 1);
}

gltf::Accessor GenerateClusterIndices(GLTFMain &main, GLTFStream &faces) {
  uint32 maxPlants = 0;

  for (auto &c : main.clusters) {
    maxPlants = std::max(maxPlants, c.numPlants);
  }

  gltf::Accessor plantIndexTempl{};
  plantIndexTempl.type = gltf::Accessor::Type::Scalar;
  plantIndexTempl.bufferView = faces.index;
  plantIndexTempl.componentType = gltf::Accessor::ComponentType::UnsignedByte;

  if ((maxPlants * 4) > 0xff) {
    plantIndexTempl.componentType =
        gltf::Accessor::ComponentType::UnsignedShort;
    faces.wr.ApplyPadding(2);
  }

  plantIndexTempl.byteOffset = faces.wr.Tell();

  if ((maxPlants * 4) > 0xff) {
    for (size_t p = 0; p < maxPlants; p++) {
      USVector4 indices(0, 1, 2, 2);
      faces.wr.Write(indices + USVector4(p * 4));
      USVector2 indices2(3, 0);
      faces.wr.Write(indices2 + USVector2(p * 4));
    }
  } else {
    for (size_t p = 0; p < maxPlants; p++) {
      UCVector4 indices(0, 1, 2, 2);
      faces.wr.Write(indices + UCVector4(p * 4));
      UCVector2 indices2(3, 0);
      faces.wr.Write(indices2 + UCVector2(p * 4));
    }
  }

  return plantIndexTempl;
}

void SaveBVHClusters(GLTFMain &main) {
  uint32 indices8Index = 0;
  auto &faces = main.Stream(main.indexSlot);

  {
    auto [acc, slot] = main.NewAccessor(faces, 1);
    acc.componentType = gltf::Accessor::ComponentType::UnsignedByte;
    acc.count = 16;
    acc.type = gltf::Accessor::Type::Scalar;
    indices8Index = slot;
    const uint8 INDICES[]{
        /**/ //
            1 - 1,
        2 - 1,
        5 - 1,
        8 - 1,
        7 - 1,
        6 - 1,
        3 - 1,
        4 - 1,
        1 - 1,
        8 - 1,
        7 - 1,
        4 - 1,
        3 - 1,
        2 - 1,
        5 - 1,
        6 - 1,
    };
    faces.wr.Write(INDICES);
  }

  BvhCtx ctx{
      .main = main,
      .verts = main.Stream(main.vertsSlot),
      .indices8Index = indices8Index,
      .depthFrac = 255.f / log2f(main.bvhTree.size() + 1),
      .uvs = main.Stream(main.uvSlot),
      .plantIndexTempl = GenerateClusterIndices(main, faces),
  };

  uint32 rootIndex = main.nodes.size();
  main.scenes.front().nodes.push_back(rootIndex);
  ctx.main.nodes.emplace_back().name = "BVHRoot";
  ProcessBvh(ctx, main.mainBBOX, 1, rootIndex, 0);
}

void SaveClusters(GLTFMain &main) {
  auto &faces = main.Stream(main.indexSlot);

  BvhCtx ctx{
      .main = main,
      .verts = main.Stream(main.vertsSlot),
      .indices8Index = 0,
      .depthFrac = 255.f / log2f(main.bvhTree.size() + 1),
      .uvs = main.Stream(main.uvSlot),
      .plantIndexTempl = GenerateClusterIndices(main, faces),
  };

  for (uint32 i = 0; i < main.clusters.size(); i++) {
    main.scenes.front().nodes.push_back(main.nodes.size());
    NewCluster(ctx, i);
  }
}

void AppProcessFile(AppContext *ctx) {
  BinReaderRef rd(ctx->GetStream());
  GLTFMain main;
  rd.Read(main.hdr);
  rd.ReadContainer(main.plants);

  auto thisFolder = ctx->workingFile.GetFolder();
  thisFolder.remove_suffix(1);
  std::string colorPath(AFileInfo(thisFolder).GetFolder());
  colorPath.append("lighting/plantcolors_w");

  for (size_t i = 0; i < 10; i++) {
    try {
      auto clr = ctx->RequestFile(colorPath + std::to_string(i + 1) + ".w32");
      BinReaderRef clrRd(*clr.Get());
      std::vector<uint32> colors;
      clrRd.ReadContainer(colors, main.plants.size());
      main.colorGroups.emplace(i, std::move(colors));
    } catch (const es::FileNotFoundError &) {
    }
  }

  if (settings.visualizeBVH) {
    rd.ReadContainer(main.clusters);

    auto vdb = ctx->RequestFile(std::string(ctx->workingFile.GetFolder()) +
                                "plant_vdb.gen");
    BinReaderRef rdb(*vdb.Get());
    uint32 bvhid;
    uint32 bvhVersion;
    rdb.Read(bvhid);

    if (bvhid != PlantGeom::ID) {
      throw es::InvalidHeaderError(bvhid);
    }

    rdb.Read(bvhVersion);

    if (bvhVersion != PlantGeom::VERSION) {
      throw es::InvalidVersionError(bvhVersion);
    }
    rdb.Skip(4);
    rdb.ReadContainer(main.bvhClusters);
    rdb.Skip(4);
    rdb.Read(main.mainBBOX);
    rdb.ReadContainer(main.bvhTree);

    SaveBVHClusters(main);
  } else if (settings.splitToClusters) {
    rd.ReadContainer(main.clusters);
    SaveClusters(main);
  } else {
    SavePlants(main);
  }

  BinWritterRef wr(ctx->NewFile(ctx->workingFile.ChangeExtension(".glb")).str);
  main.FinishAndSave(wr, std::string(ctx->workingFile.GetFolder()));
}
