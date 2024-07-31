/*  CDB2GLTF
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
    "^track_cdb2.gen$",
};

struct CDB2GLTF : ReflectorBase<CDB2GLTF> {
  bool splitToClusters = false;
  bool visualizeBVH = false;
} settings;

REFLECT(CLASS(CDB2GLTF),
        MEMBER(splitToClusters, "C", ReflDesc{"Split plants into clusters."}),
        MEMBER(visualizeBVH, "B",
               ReflDesc{"Visualize BVH and plant cluster bounds. This will "
                        "also splits plants into clusters."}));

static AppInfo_s appInfo{
    .header = CDB2GLTF_DESC " v" CDB2GLTF_VERSION ", " CDB2GLTF_COPYRIGHT
                            "Lukas Cone",
    .settings = reinterpret_cast<ReflectorFriend *>(&settings),
    .filters = filters,
};

AppInfo_s *AppInitModule() { return &appInfo; }

enum class BvhType : uint32 {
  CUT_X,
  CUT_Y,
  CUT_Z,
  LEAF,
};

struct BvhBbox {
  float minMax[6];
};

union BvhItem {
  BvhType cutType : 2;
  struct {
    BvhType cutType : 2;
    uint32 flags : 6;
    uint32 childOffset : 24;
    int16 cutHigh;
    int16 cutLow;
  } node;
  struct {
    BvhType cutType : 2;
    uint32 other : 4;
    uint32 leafType : 3;
    uint32 collisionItemsOffset : 23;
    uint32 numCollisionItems : 7;
    uint32 unkCount : 6;
    uint32 startVertex : 19;
  } leaf;
};

struct CollisionBvh {
  uint32 id;
  uint32 null0;
  BvhBbox mainBbox;
  Vector4A16 multiply;
  Vector4A16 add;
  std::vector<BvhItem> bvhTree;
  std::string bvhBuffer;
  std::vector<int16> vertices;

  void Read(BinReaderRef rd) {
    rd.Read(id);
    rd.Read(null0);
    IVector bbox[2];
    rd.Read(bbox);
    Vector fbbox[]{bbox[0].Convert<float>(), bbox[1].Convert<float>()};
    Vector pValues;
    rd.Read(pValues);
    multiply = pValues;
    rd.Read(pValues);
    add = pValues;
    const uint32 bufferSIze = rd.GetSize();
    memcpy(mainBbox.minMax, fbbox, 24);
    uint32 bvhBufferOffset;
    rd.Read(bvhBufferOffset);
    uint32 vtxBufferOffset;
    rd.Read(vtxBufferOffset);
    const uint32 beginBuffers = rd.Tell();
    rd.ReadContainer(bvhTree, bvhBufferOffset / 8);
    const uint32 bvhBufferSize = vtxBufferOffset - bvhBufferOffset;
    rd.ReadContainer(bvhBuffer, bvhBufferSize);
    const uint32 vtxBufferSize = bufferSIze - beginBuffers - vtxBufferOffset;
    rd.ReadContainer(vertices, vtxBufferSize / 2);
  }
};

struct GLTFMain : GLTF {
  size_t vertsSlot;
  size_t indexSlot;
  size_t uvSlot;
  CollisionBvh hdr;

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
    auto [acc, slot] = ctx.main.NewAccessor(ctx.uvs, 4);
    acc.componentType = gltf::Accessor::ComponentType::UnsignedByte;
    acc.count = 8;
    acc.type = gltf::Accessor::Type::Vec4;
    acc.normalized = true;
    prim.attributes["COLOR_0"] = slot;
  }

  auto [accPos, slot] = ctx.main.NewAccessor(verts, 4);
  accPos.componentType = gltf::Accessor::ComponentType::Float;
  accPos.count = 8;
  accPos.type = gltf::Accessor::Type::Vec3;
  prim.attributes["POSITION"] = slot;

  Vector4A16 vts[]{
      {bbox.minMax[0], bbox.minMax[4], bbox.minMax[2], 0},
      {bbox.minMax[0], bbox.minMax[1], bbox.minMax[2], 0},
      {bbox.minMax[3], bbox.minMax[1], bbox.minMax[2], 0},
      {bbox.minMax[3], bbox.minMax[4], bbox.minMax[2], 0},
      {bbox.minMax[0], bbox.minMax[1], bbox.minMax[5], 0},
      {bbox.minMax[3], bbox.minMax[1], bbox.minMax[5], 0},
      {bbox.minMax[3], bbox.minMax[4], bbox.minMax[5], 0},
      {bbox.minMax[0], bbox.minMax[4], bbox.minMax[5], 0},
  };

  Vector4A16 posMax(-FLT_MAX);
  Vector4A16 posMin(FLT_MAX);

  for (auto &v : vts) {
    v = v * ctx.main.hdr.multiply + ctx.main.hdr.add;
    posMax._data = _mm_max_ps(posMax._data, v._data);
    posMin._data = _mm_min_ps(posMin._data, v._data);
    verts.wr.Write<Vector>(v);
  }

  accPos.min.resize(3);
  accPos.max.resize(3);
  memcpy(accPos.min.data(), &posMin, 12);
  memcpy(accPos.max.data(), &posMax, 12);

  uint32 color = 0xff | (uint32(ctx.depthFrac * depth) * 255);

  for (auto v = 0; v < 8; v++) {
    ctx.uvs.wr.Write(color);
  }
}

// Offsets are GLOBAL
struct CollisionItem0First {
  uint64 unk0 : 6;
  uint64 driveable : 2;
  uint64 start1 : 19;
  uint64 start2 : 21;
  static const uint32 size = 6;
};

// Offsets are GLOBAL
struct CollisionItem0Next {
  uint64 unk0 : 6;
  uint64 driveable : 2;
  uint64 unkCount : 6; // used instead of LeafBf::unkCount
  uint64 padding : 1;
  uint64 start0 : 19;
  uint64 start1 : 19;
  // uint64 start2 : 19;
  uint64 start20 : 11;
  uint64 start21 : 8;
  static const uint32 size = 9;
};

// Offsets are GLOBAL
// Same as CollisionItem0First
struct CollisionItem1First {
  uint64 unk0 : 6;
  uint64 driveable : 2;
  uint64 start1 : 19;
  uint64 start2 : 21;
  static const uint32 size = 6;
};

// Offsets are GLOBAL
struct CollisionItem1Next {
  uint64 unk0 : 6;
  uint64 driveable : 1;
  uint64 start0 : 19;
  uint64 start1 : 19;
  uint64 start2 : 19;
  static const uint32 size = 8;
};

struct CollisionItem2 {
  uint64 unk0 : 6;
  uint64 driveable : 2;
  uint64 unkCount : 6; // used instead of LeafBf::unkCount
  uint64 padding : 2;
  uint64 start0 : 8;
  uint64 start1 : 8;
  uint64 start2 : 8;
  static const uint32 size = 5;
};

struct CollisionItem3 {
  uint32 unk0 : 6;
  uint32 driveable : 2;
  uint32 start0 : 8;
  uint32 start1 : 8;
  uint32 start2 : 8;
  static const uint32 size = 4;
};

struct CollisionItem4 {
  uint64 unk0 : 6;
  uint64 driveable : 1;
  uint64 start0 : 11;
  uint64 start1 : 11;
  uint64 start2 : 11;
  static const uint32 size = 5;
};

struct CollisionItem5 {
  uint32 unk0 : 6;
  uint32 driveable : 2;
  uint32 start0 : 5; // unkDataStart + start0
  uint32 start1 : 5; // unkDataStart + start1
  uint32 start2 : 6; // unkDataStart + start2
  static const uint32 size = 3;
};

struct CollisionData {
  uint8 unk0; // Seems like an index, possibly surface type?
  uint8 unk1;
  uint8 driveable;
  // uint8 unk3;
  const int16 *data0;
  const int16 *data1;
  const int16 *data2;
};

using Unpacker = std::vector<CollisionData> (*)(CollisionBvh &, BvhItem &);
Unpacker unpackers[]{
    [](CollisionBvh &hdr, BvhItem &item) {
      std::vector<CollisionData> retval;
      auto &leaf = item.leaf;
      const char *curBuffer = hdr.bvhBuffer.data() + leaf.collisionItemsOffset;
      const int16 *vtxs = hdr.vertices.data();
      CollisionItem0First first;
      memcpy(&first, curBuffer, first.size);
      curBuffer += first.size;

      retval.emplace_back(CollisionData{
          .unk0 = uint8(leaf.unkCount),
          .unk1 = uint8(first.unk0),
          .driveable = uint8(first.driveable),
          .data0 = vtxs + leaf.startVertex,
          .data1 = vtxs + first.start1,
          .data2 = vtxs + first.start2,
      });

      for (size_t i = 1; i < leaf.numCollisionItems; i++) {
        CollisionItem0Next next;
        memcpy(&next, curBuffer, next.size);
        curBuffer += next.size;

        retval.emplace_back(CollisionData{
            .unk0 = uint8(next.unkCount),
            .unk1 = uint8(next.unk0),
            .driveable = uint8(next.driveable),
            .data0 = vtxs + next.start0,
            .data1 = vtxs + next.start1,
            .data2 = vtxs + (next.start20 | (next.start21 << 11)),
        });
      }

      return retval;
    },
    [](CollisionBvh &hdr, BvhItem &item) {
      std::vector<CollisionData> retval;
      auto &leaf = item.leaf;
      const char *curBuffer = hdr.bvhBuffer.data() + leaf.collisionItemsOffset;
      const int16 *vtxs = hdr.vertices.data();
      CollisionItem1First first;
      memcpy(&first, curBuffer, first.size);
      curBuffer += first.size;

      retval.emplace_back(CollisionData{
          .unk0 = uint8(leaf.unkCount),
          .unk1 = uint8(first.unk0),
          .driveable = uint8(first.driveable),
          .data0 = vtxs + leaf.startVertex,
          .data1 = vtxs + first.start1,
          .data2 = vtxs + first.start2,
      });

      for (size_t i = 1; i < leaf.numCollisionItems; i++) {
        CollisionItem1Next next;
        memcpy(&next, curBuffer, next.size);
        curBuffer += next.size;

        retval.emplace_back(CollisionData{
            .unk0 = uint8(leaf.unkCount),
            .unk1 = uint8(next.unk0),
            .driveable = uint8(next.driveable),
            .data0 = vtxs + next.start0,
            .data1 = vtxs + next.start1,
            .data2 = vtxs + next.start2,
        });
      }

      return retval;
    },
    [](CollisionBvh &hdr, BvhItem &item) {
      std::vector<CollisionData> retval;
      auto &leaf = item.leaf;
      const char *curBuffer = hdr.bvhBuffer.data() + leaf.collisionItemsOffset;
      const int16 *vtxs = hdr.vertices.data() + leaf.startVertex;

      for (size_t i = 0; i < leaf.numCollisionItems; i++) {
        CollisionItem2 next;
        memcpy(&next, curBuffer, next.size);
        curBuffer += next.size;

        retval.emplace_back(CollisionData{
            .unk0 = uint8(next.unkCount),
            .unk1 = uint8(next.unk0),
            .driveable = uint8(next.driveable),
            .data0 = vtxs + next.start0,
            .data1 = vtxs + next.start1,
            .data2 = vtxs + next.start2,
        });
      }

      return retval;
    },
    [](CollisionBvh &hdr, BvhItem &item) {
      std::vector<CollisionData> retval;
      auto &leaf = item.leaf;
      const char *curBuffer = hdr.bvhBuffer.data() + leaf.collisionItemsOffset;
      const int16 *vtxs = hdr.vertices.data() + leaf.startVertex;

      for (size_t i = 0; i < leaf.numCollisionItems; i++) {
        CollisionItem3 next;
        memcpy(&next, curBuffer, next.size);
        curBuffer += next.size;

        retval.emplace_back(CollisionData{
            .unk0 = uint8(leaf.unkCount),
            .unk1 = uint8(next.unk0),
            .driveable = uint8(next.driveable),
            .data0 = vtxs + next.start0,
            .data1 = vtxs + next.start1,
            .data2 = vtxs + next.start2,
        });
      }

      return retval;
    },
    [](CollisionBvh &hdr, BvhItem &item) {
      std::vector<CollisionData> retval;
      auto &leaf = item.leaf;
      const char *curBuffer = hdr.bvhBuffer.data() + leaf.collisionItemsOffset;
      const int16 *vtxs = hdr.vertices.data() + leaf.startVertex;

      for (size_t i = 0; i < leaf.numCollisionItems; i++) {
        CollisionItem4 next;
        memcpy(&next, curBuffer, next.size);
        curBuffer += next.size;

        retval.emplace_back(CollisionData{
            .unk0 = uint8(leaf.unkCount),
            .unk1 = uint8(next.unk0),
            .driveable = uint8(next.driveable),
            .data0 = vtxs + next.start0,
            .data1 = vtxs + next.start1,
            .data2 = vtxs + next.start2,
        });
      }

      return retval;
    },
    [](CollisionBvh &hdr, BvhItem &item) {
      std::vector<CollisionData> retval;
      auto &leaf = item.leaf;
      const char *curBuffer = hdr.bvhBuffer.data() + leaf.collisionItemsOffset;
      const int16 *vtxs = hdr.vertices.data() + leaf.startVertex;

      for (size_t i = 0; i < leaf.numCollisionItems; i++) {
        CollisionItem5 next;
        memcpy(&next, curBuffer, next.size);
        curBuffer += next.size;

        retval.emplace_back(CollisionData{
            .unk0 = uint8(leaf.unkCount),
            .unk1 = uint8(next.unk0),
            .driveable = uint8(next.driveable),
            .data0 = vtxs + next.start0,
            .data1 = vtxs + next.start1,
            .data2 = vtxs + next.start2,
        });
      }

      return retval;
    },
};

void NewLeaf(GLTFMain &main, BvhItem &item, uint32 parentNode) {
  auto data = unpackers[item.leaf.leafType](main.hdr, item);
  GLTFStream &verts = main.Stream(main.vertsSlot);
  main.nodes.at(parentNode).children.emplace_back(main.nodes.size());
  auto &node = main.nodes.emplace_back();
  node.mesh = main.meshes.size();
  auto &mesh = main.meshes.emplace_back();
  auto &prim = mesh.primitives.emplace_back();
  prim.mode = gltf::Primitive::Mode::Triangles;

  auto [accPos, slotPos] = main.NewAccessor(verts, 4);
  accPos.componentType = gltf::Accessor::ComponentType::Float;
  accPos.count = data.size() * 3;
  accPos.type = gltf::Accessor::Type::Vec3;
  prim.attributes["POSITION"] = slotPos;
  Vector4A16 posMax(-FLT_MAX);
  Vector4A16 posMin(FLT_MAX);

  for (auto &t : data) {
    Vector4A16 vts[]{
        reinterpret_cast<const SVector *>(t.data0)->Convert<float>(),
        reinterpret_cast<const SVector *>(t.data1)->Convert<float>(),
        reinterpret_cast<const SVector *>(t.data2)->Convert<float>(),
    };

    for (auto &v : vts) {
      v = v * main.hdr.multiply + main.hdr.add;
      posMax._data = _mm_max_ps(posMax._data, v._data);
      posMin._data = _mm_min_ps(posMin._data, v._data);
      verts.wr.Write<Vector>(v);
    }
  }

  accPos.min.resize(3);
  accPos.max.resize(3);
  memcpy(accPos.min.data(), &posMin, 12);
  memcpy(accPos.max.data(), &posMax, 12);
}

void ProcessBvh(BvhCtx &ctx, BvhBbox bbox, uint32 bvhItem, uint32 parentNode,
                uint32 depth) {
  uint32 thisNodeIndex = parentNode;
  /*thisNodeIndex = ctx.main.nodes.size();
  ctx.main.nodes.at(parentNode).children.emplace_back(thisNodeIndex);
  NewBox(ctx, bbox, depth);*/

  BvhItem curItem = ctx.main.hdr.bvhTree.at(bvhItem);

  if (curItem.node.childOffset == 0) {
    return;
  }

  BvhBbox half0 = bbox;
  BvhBbox half1 = bbox;

  switch (curItem.cutType) {
  case BvhType::LEAF:
    NewLeaf(ctx.main, curItem, thisNodeIndex);
    return;
  case BvhType::CUT_X:
    half0.minMax[0] = curItem.node.cutLow;
    half1.minMax[3] = curItem.node.cutHigh;
    break;
  case BvhType::CUT_Y:
    half0.minMax[1] = curItem.node.cutLow;
    half1.minMax[4] = curItem.node.cutHigh;
    break;
  case BvhType::CUT_Z:
    half0.minMax[2] = curItem.node.cutLow;
    half1.minMax[5] = curItem.node.cutHigh;
    break;
  default:
    return;
  }

  ProcessBvh(ctx, half0, curItem.node.childOffset / 8 + 1, thisNodeIndex,
             depth + 1);
  ProcessBvh(ctx, half1, curItem.node.childOffset / 8, thisNodeIndex,
             depth + 1);
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
      .depthFrac = 255.f / log2f(main.hdr.bvhTree.size() + 1),
      .uvs = main.Stream(main.uvSlot),
  };

  uint32 rootIndex = main.nodes.size();
  main.scenes.front().nodes.push_back(rootIndex);
  ctx.main.nodes.emplace_back().name = "BVHRoot";
  ProcessBvh(ctx, main.hdr.mainBbox, 0, rootIndex, 0);
}

void AppProcessFile(AppContext *ctx) {
  BinReaderRef rd(ctx->GetStream());
  GLTFMain main;
  rd.Read(main.hdr);
  SaveBVHClusters(main);

  BinWritterRef wr(ctx->NewFile(ctx->workingFile.ChangeExtension(".glb")).str);
  main.FinishAndSave(wr, std::string(ctx->workingFile.GetFolder()));
}
