/*  BGM2GLTF
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
#include "datas/flags.hpp"
#include "datas/master_printer.hpp"
#include "datas/matrix44.hpp"
#include "datas/reflector.hpp"
#include "gltf.hpp"
#include "nlohmann/json.hpp"
#include "project.h"
#include <vector>

using nlohmann::json;

es::string_view filters[]{
    ".bgm$",
    "^track_geom",
    {},
};

struct BGM2GLTF : ReflectorBase<BGM2GLTF> {
} settings;

REFLECT(CLASS(BGM2GLTF));

AppInfo_s appInfo{
    AppInfo_s::CONTEXT_VERSION,
    AppMode_e::CONVERT,
    ArchiveLoadType::ALL,
    BGM2GLTF_DESC " v" BGM2GLTF_VERSION ", " BGM2GLTF_COPYRIGHT "Lukas Cone",
    reinterpret_cast<ReflectorFriend *>(&settings),
    filters,
};

const AppInfo_s *AppInitModule() { return &appInfo; }

struct Material {
  static constexpr uint32 ID = CompileFourCC("MATC");
  std::string name;
  std::string texture0;
  std::string texture1;
  std::string texture2;
  struct {
    uint32 transparency;
    uint32 null1;
    uint32 usedTextures;
    uint32 unk3;
    uint32 null0[25];
  } data;

  void Read(BinReaderRef rd) {
    uint32 id;
    rd.Read(id);
    if (id != ID) {
      throw es::InvalidHeaderError(id);
    }

    rd.ReadString(name);
    rd.Read(data);
    rd.ReadString(texture0);
    rd.ReadString(texture1);
    rd.ReadString(texture2);
  }
};

enum class BufferType { None, Vertex, Index, Sprite };

enum class VertexBufferFlags {
  Position = 1,
  Normal = 4,
  ColorOrBoneWeights = 6,
  UV1 = 8,
  UV2 = 9,
};

static constexpr uint32 vertexBufferFlagsMask =
    uint32(es::Flags<VertexBufferFlags>(
        VertexBufferFlags::Position, VertexBufferFlags::Normal,
        VertexBufferFlags::ColorOrBoneWeights, VertexBufferFlags::UV1,
        VertexBufferFlags::UV2));

struct Buffer {
  BufferType type;
  uint32 null;
  uint32 numItems;
};

struct PrimitiveBase {
  uint32 flags; // 1 = sprite
  uint32 materialIndex;
  uint32 numVertices;
  es::Flags<VertexBufferFlags> formatFlags;
  uint32 numPolys; // num sprites
  uint32 indexType;
  uint32 numIndices;
};

struct PrimitiveGeneric : PrimitiveBase {
  uint32 numStreams;
  uint32 vertexStreamIndex;
  uint32 vertexStreamOffset;
  uint32 indexStreamIndex;
  uint32 indexStreamOffset;

  void Read(BinReaderRef rd) {
    rd.Read<PrimitiveBase>(*this);
    rd.Read(numStreams);
    rd.Read(vertexStreamIndex);
    rd.Read(vertexStreamOffset);

    if (numStreams > 2) {
      printwarning("PrimitiveGeneric field not > 2 " << numStreams);
    }

    if (flags > 1) {
      printwarning("PrimitiveGeneric field not [0,1] " << flags);
    }

    if (!flags) {
      rd.Read(indexStreamIndex);
      rd.Read(indexStreamOffset);
    }
  }
};

struct PrimitiveOverlay : PrimitiveGeneric {
  float unk0[6]; // bbox?

  void Read(BinReaderRef rd) {
    rd.Read<PrimitiveBase>(*this);
    rd.Read(unk0);
    rd.Read(numStreams);
    rd.Read(vertexStreamIndex);
    rd.Read(vertexStreamOffset);

    if (!flags) {
      rd.Read(indexStreamIndex);
      rd.Read(indexStreamOffset);
    }
  }
};
;

struct BMOD {
  static constexpr uint32 ID = CompileFourCC("BMOD");
  std::string name;

  float bboxCenter[3];
  float bboxMax[3];
  float boundingRadius;

  std::vector<uint32> primitiveIndices;

  void Read(BinReaderRef rd) {
    uint32 id;
    rd.Read(id);

    if (id != ID) {
      throw es::InvalidHeaderError(id);
    }

    if (rd.Read(id); id) {
      printwarning("BMOD field not null " << id);
    }

    rd.ReadString(name);
    rd.Read(bboxCenter);
    rd.Read(bboxMax);
    rd.Read(boundingRadius);
    rd.ReadContainer(primitiveIndices);
  }
};

struct MESH {
  static constexpr uint32 ID = CompileFourCC("MESH");
  std::string name;
  std::string dynamicObject;
  uint32 flag;
  int32 groupId;
  float tm[16];
  std::vector<uint32> meshes;

  void Read(BinReaderRef rd) {
    uint32 id;
    rd.Read(id);

    if (id != ID) {
      throw es::InvalidHeaderError(id);
    }

    rd.ReadString(name);
    rd.ReadString(dynamicObject);
    rd.Read(flag);
    rd.Read(groupId);
    rd.Read(tm);
    rd.ReadContainer(meshes);
  }
};

struct DeformVertex {
  Vector pos;
  Vector deformPos;
  Vector normal;
  Vector deformNormal;
};

struct TrackModel {
  uint32 primitiveIndex;
  uint32 unk1; // lod index?
  uint32 bvhItemId;
  float unk3[6];
};

struct SpriteVertex {
  float pos[3];
  float uv[2];
  uint32 unk[2];
};

struct Foliage {
  uint32 unk0[2];
  uint32 primitiveIndex;
  uint32 bvhItemId;
  Vector4 leavesSpriteTM[4];
  Vector leavesSpriteScale;
  int32 trunkPrimitiveIndex;
  int32 branchesPrimitiveIndex;
  int32 leavesSpritePrimitiveIndex;
  uint32 leavesSpriteIndices;
  uint32 lodSpriteIndex;
  uint32 lodMaterialIndex;
};

struct OBJC {
  static constexpr uint32 ID = CompileFourCC("OBJC");
  std::string name;
  std::string null;
  uint32 flag;
  float tm[16];

  void Read(BinReaderRef rd) {
    uint32 id;
    rd.Read(id);

    if (id != ID) {
      throw es::InvalidHeaderError(id);
    }

    rd.ReadString(name);
    rd.ReadString(null);
    rd.Read(flag);
    rd.Read(tm);
  }
};

struct Dynamic {
  std::vector<uint32> bmodIndices; // lods?
  float bbox[6];

  void Read(BinReaderRef rd) {
    rd.ReadContainer(bmodIndices);
    rd.Read(bbox);
  }
};

struct DynamicName {
  std::string name;
  int32 id0;
  int32 id1;

  void Read(BinReaderRef rd) {
    rd.ReadString(name);
    rd.Read(id0);
    rd.Read(id1);
  }
};

struct MainGLTF : GLTF {
  std::vector<gltf::Primitive> primitives;
  std::map<size_t, gltf::Attributes> attrs;
  size_t bmodsBegin = 0;
  size_t spriteIndicesBuffView = 0;

  void GetMaterials(BinReaderRef rd) {
    std::vector<Material> mats;
    rd.ReadContainer(mats);

    for (auto &m : mats) {
      auto &mat = materials.emplace_back();
      mat.name = std::move(m.name);
      /*mat.pbrMetallicRoughness.baseColorTexture.index = textures.size();
      textures.emplace_back().source = images.size();
      images.emplace_back().uri = m.texture0;*/
    }
  }

  void GetBuffers(BinReaderRef rd) {
    uint32 numBuffers;
    rd.Read(numBuffers);

    auto restream = [&](auto &nStream) {
      char buffer[0x80000];
      const size_t numBlocks = nStream.byteLength / sizeof(buffer);
      const size_t restBytes = nStream.byteLength % sizeof(buffer);

      for (size_t b = 0; b < numBlocks; b++) {
        rd.ReadBuffer(buffer, sizeof(buffer));
        nStream.wr.WriteBuffer(buffer, sizeof(buffer));
      }

      if (restBytes) {
        rd.ReadBuffer(buffer, restBytes);
        nStream.wr.WriteBuffer(buffer, restBytes);
      }
    };

    size_t maxNumSprites = 0;

    for (size_t i = 0; i < numBuffers; i++) {
      Buffer hdr;
      rd.Read(hdr);

      if (hdr.null) {
        printwarning("Buffer field not null.");
      }

      switch (hdr.type) {
      case BufferType::Vertex: {
        uint32 stride;
        es::Flags<VertexBufferFlags> flags;
        rd.Read(stride);
        rd.Read(flags);

        if ((uint32(flags) & vertexBufferFlagsMask) ^ uint32(flags)) {
          throw std::runtime_error(
              "Missing flags implementations for vertex buffer!");
        }

        auto &nStream = NewStream(std::to_string(i) + "vertices", stride);
        nStream.byteLength = stride * hdr.numItems;
        nStream.target = gltf::BufferView::TargetType::ArrayBuffer;
        restream(nStream);

        size_t curOffset = 0;
        gltf::Accessor baseAcc;
        baseAcc.bufferView = nStream.slot;
        baseAcc.count = hdr.numItems;
        baseAcc.componentType = gltf::Accessor::ComponentType::Float;
        auto &attributes = attrs[nStream.slot];

        if (flags[VertexBufferFlags::Position]) {
          attributes["POSITION"] = accessors.size();
          auto &acc = accessors.emplace_back(baseAcc);
          acc.type = gltf::Accessor::Type::Vec3;
          curOffset += 12;
        }

        if (flags[VertexBufferFlags::Normal]) {
          attributes["NORMAL"] = accessors.size();
          auto &acc = accessors.emplace_back(baseAcc);
          acc.type = gltf::Accessor::Type::Vec3;
          acc.byteOffset += curOffset;
          curOffset += 12;
        }

        if (flags[VertexBufferFlags::ColorOrBoneWeights]) {
          attributes["COLOR_0"] = accessors.size();
          auto &acc = accessors.emplace_back(baseAcc);
          acc.componentType = gltf::Accessor::ComponentType::UnsignedByte;
          acc.normalized = true;
          acc.type = gltf::Accessor::Type::Vec4;
          acc.byteOffset += curOffset;
          curOffset += 4;
        }

        if (flags[VertexBufferFlags::UV1] || flags[VertexBufferFlags::UV2]) {
          attributes["TEXCOORD_0"] = accessors.size();
          auto &acc = accessors.emplace_back(baseAcc);
          acc.type = gltf::Accessor::Type::Vec2;
          acc.byteOffset += curOffset;
          curOffset += 8;
        }

        if (flags[VertexBufferFlags::UV2]) {
          attributes["TEXCOORD_1"] = accessors.size();
          auto &acc = accessors.emplace_back(baseAcc);
          acc.type = gltf::Accessor::Type::Vec2;
          acc.byteOffset += curOffset;
          curOffset += 8;
        }

        break;
      }

      case BufferType::Index: {
        auto &nStream = NewStream(std::to_string(i) + "indices");
        nStream.byteLength = hdr.numItems * 2;
        nStream.target = gltf::BufferView::TargetType::ElementArrayBuffer;
        restream(nStream);

        break;
      }

      case BufferType::Sprite: {
        uint32 stride;
        rd.Read(stride);
        auto &nStream = NewStream(std::to_string(i) + "sprites", stride);
        nStream.byteLength = hdr.numItems * stride;
        nStream.target = gltf::BufferView::TargetType::ArrayBuffer;
        restream(nStream);

        if (stride != 28) {
          printwarning("Invalid sprite buffer stride " << stride);
        }

        size_t curOffset = 0;
        gltf::Accessor baseAcc;
        baseAcc.bufferView = nStream.slot;
        baseAcc.count = hdr.numItems;
        maxNumSprites = std::max(size_t(hdr.numItems), maxNumSprites);
        baseAcc.componentType = gltf::Accessor::ComponentType::Float;
        auto &attributes = attrs[nStream.slot];

        {
          attributes["POSITION"] = accessors.size();
          auto &acc = accessors.emplace_back(baseAcc);
          acc.type = gltf::Accessor::Type::Vec3;
          curOffset += 12;
        }

        {
          attributes["TEXCOORD_0"] = accessors.size();
          auto &acc = accessors.emplace_back(baseAcc);
          acc.type = gltf::Accessor::Type::Vec2;
          acc.byteOffset += curOffset;
          curOffset += 8;
        }

        break;
      }

      default:
        throw std::runtime_error("Invalid buffer type " +
                                 std::to_string(uint32(hdr.type)));
      }
    }

    if (maxNumSprites) {
      auto &nStream = NewStream("indices-sprites");
      spriteIndicesBuffView = nStream.slot;
      nStream.byteLength = maxNumSprites * 2;
      nStream.target = gltf::BufferView::TargetType::ElementArrayBuffer;

      for (uint16 i = 0; i < maxNumSprites; i++) {
        nStream.wr.Write(i);
      }
    }
  }

  void GetPrimitivesGeneric(BinReaderRef rd, bool noMeta = false) {
    std::vector<PrimitiveGeneric> prims;
    rd.ReadContainer(prims);

    for (auto &p : prims) {
      gltf::Primitive &prim = primitives.emplace_back();

      if (!noMeta) {
        auto &meta = prim.GetExtensionsAndExtras()["bgm_meta"];
        meta["num_vertices"] = p.numVertices;
        meta["vertex_offset"] =
            p.vertexStreamOffset / Stream(p.vertexStreamIndex).byteStride;
      }
      prim.mode = gltf::Primitive::Mode(p.indexType);
      prim.material = p.materialIndex;
      prim.indices = accessors.size();
      auto &acc = accessors.emplace_back();
      acc.componentType = gltf::Accessor::ComponentType::UnsignedShort;
      acc.type = gltf::Accessor::Type::Scalar;

      if (p.numIndices > 0) {
        acc.count = p.numIndices;
        acc.bufferView = Stream(p.indexStreamIndex).slot;
        acc.byteOffset = p.indexStreamOffset;
      } else {
        acc.count = p.numPolys;
        acc.bufferView = spriteIndicesBuffView;
        acc.byteOffset = (p.vertexStreamOffset / sizeof(SpriteVertex)) * 2;
      }

      prim.attributes = attrs.at(p.vertexStreamIndex);
    }
  }

  void GetPrimitivesOverlay(BinReaderRef rd) {
    std::vector<PrimitiveOverlay> prims;
    rd.ReadContainer(prims);

    for (auto &p : prims) {
      gltf::Primitive &prim = primitives.emplace_back();
      prim.indices = accessors.size();
      prim.mode = gltf::Primitive::Mode(p.indexType);
      prim.material = p.materialIndex;
      {
        auto &acc = accessors.emplace_back();
        acc.componentType = gltf::Accessor::ComponentType::UnsignedShort;
        acc.count = p.numIndices;
        acc.type = gltf::Accessor::Type::Scalar;
        acc.bufferView = Stream(p.indexStreamIndex).slot;
        acc.byteOffset = p.indexStreamOffset;
      }

      prim.attributes = attrs.at(p.vertexStreamIndex);
    }
  }

  void GetBMODs(BinReaderRef rd) {
    std::vector<BMOD> mods;
    rd.ReadContainer(mods);
    bmodsBegin = meshes.size();

    for (auto &m : mods) {
      auto &mesh = meshes.emplace_back();
      mesh.name = std::move(m.name);

      for (auto p : m.primitiveIndices) {
        mesh.primitives.emplace_back(primitives.at(p));
      }
    }
  }

  void GetMESHes(BinReaderRef rd) {
    std::vector<MESH> meshes;
    rd.ReadContainer(meshes);

    for (auto &m : meshes) {
      const size_t modelIndex = nodes.size();
      scenes.front().nodes.push_back(modelIndex);
      auto &model = nodes.emplace_back();
      model.name = std::move(m.name);

      if (!m.dynamicObject.empty()) {
        model.name.push_back('[');
        model.name.append(m.dynamicObject);
        model.name.push_back(']');
      }

      model.mesh = m.meshes.front();
      memcpy(model.matrix.data(), m.tm, 64);

      for (size_t p = 1; p < m.meshes.size(); p++) {
        nodes.at(modelIndex).children.push_back(nodes.size());
        auto &sModel = nodes.emplace_back();
        sModel.mesh = m.meshes[p];
      }
    }
  }

  void GetObjects(BinReaderRef rd) {
    std::vector<OBJC> objs;
    rd.ReadContainer(objs);
    const size_t objNodeIndex = nodes.size();
    scenes.back().nodes.push_back(objNodeIndex);
    nodes.emplace_back().name = "Attachments";

    for (auto &m : objs) {
      nodes.at(objNodeIndex).children.push_back(nodes.size());
      auto &node = nodes.emplace_back();
      node.name = m.name;
      memcpy(node.matrix.data(), m.tm, 64);
    }
  }

  void LoadCrashData(BinReaderRef rd) {
    uint32 numMeshes;
    rd.Read(numMeshes);
    auto &dStream = NewStream("deform-positions");
    auto &nStream = NewStream("deform-normals");
    auto &iStream = NewStream("deform-indices");

    for (size_t i = 0; i < numMeshes; i++) {
      std::string meshName;
      uint32 numPrims;
      rd.ReadString(meshName);
      rd.Read(numPrims);
      bool skip = false;

      auto found = std::find_if(meshes.begin(), meshes.end(), [&](auto &item) {
        return es::string_view(meshName).begins_with(item.name);
      });

      if (es::IsEnd(meshes, found)) {
        printerror("Mesh " << meshName << " not found. Skipping deform.");
        skip = true;
      }

      if (!skip && found->primitives.size() != numPrims) {
        printerror("Mesh primitive count mismatch "
                   << numPrims << " != " << found->primitives.size()
                   << ". Skipping deform.");
        skip = true;
      }

      for (size_t p = 0; p < numPrims; p++) {
        uint32 numVerts;
        uint32 bufferSize;
        rd.Read(numVerts);
        rd.Read(bufferSize);
        rd.Skip(bufferSize);

        if (skip) {
          rd.Skip(numVerts * sizeof(DeformVertex));
        } else {
          auto &prim = found->primitives.at(p);
          gltf::Accessor::Sparse sparseCommon;
          sparseCommon.count = numVerts;
          sparseCommon.indices.bufferView = iStream.slot;
          sparseCommon.indices.componentType =
              gltf::Accessor::ComponentType::UnsignedShort;
          sparseCommon.values.bufferView = dStream.slot;

          {
            auto &meta = prim.GetExtensionsAndExtras().at("bgm_meta");

            if (!meta.contains("indices_offset")) {
              meta["indices_offset"] = sparseCommon.indices.byteOffset =
                  iStream.wr.Tell();
              size_t numVertices = meta.at("num_vertices");
              size_t vertexOffset = meta.at("vertex_offset");

              for (size_t v = 0; v < numVertices; v++) {
                iStream.wr.Write<uint16>(v + vertexOffset);
              }
            } else {
              sparseCommon.indices.byteOffset = meta.at("indices_offset");
            }
          }

          std::vector<DeformVertex> verts;
          rd.ReadContainer(verts, numVerts);
          sparseCommon.values.byteOffset = dStream.wr.Tell();

          gltf::Attributes deformAttrs;
          deformAttrs["POSITION"] = accessors.size();
          deformAttrs["NORMAL"] = accessors.size() + 1;
          prim.targets.emplace_back(std::move(deformAttrs));

          auto posAcc = accessors.at(prim.attributes["POSITION"]);
          posAcc.sparse = sparseCommon;
          accessors.emplace_back(std::move(posAcc));
          sparseCommon.values.bufferView = nStream.slot;
          auto norAcc = accessors.at(prim.attributes["NORMAL"]);
          norAcc.sparse = sparseCommon;
          accessors.emplace_back(std::move(norAcc));

          for (auto &v : verts) {
            dStream.wr.Write(v.deformPos - v.pos);
            nStream.wr.Write(v.deformNormal - v.normal);
          }
        }
      }
    }

    auto &defStream = NewStream("deform-shape");
    auto &cAnim = animations.emplace_back();
    cAnim.name = "deform_shape";
    auto &defSampler = cAnim.samplers.emplace_back();
    auto [keyAcc, keyIndex] = NewAccessor(defStream, 4);
    keyAcc.componentType = gltf::Accessor::ComponentType::Float;
    keyAcc.count = 2;
    keyAcc.type = gltf::Accessor::Type::Scalar;
    defStream.wr.Write(0.0f);
    defStream.wr.Write(1.0f);
    defSampler.input = keyIndex;
    auto [valAcc, valIndex] = NewAccessor(defStream, 4);
    valAcc.componentType = gltf::Accessor::ComponentType::UnsignedByte;
    valAcc.count = 2;
    valAcc.normalized = true;
    valAcc.type = gltf::Accessor::Type::Scalar;
    defStream.wr.Write(uint8(0));
    defStream.wr.Write(uint8(0xff));
    defSampler.output = valIndex;

    size_t curNodeIndex = 0;

    for (auto &n : nodes) {
      if (n.mesh > -1) {
        if (auto &m = meshes.at(n.mesh);
            !m.primitives.front().targets.empty()) {
          auto &channel = cAnim.channels.emplace_back();
          channel.sampler = 0;
          channel.target.node = curNodeIndex;
          channel.target.path = "weights";
        }
      }

      curNodeIndex++;
    }
  }

  void GetDynamics(BinReaderRef rd) {
    std::vector<Dynamic> dynamics;
    rd.ReadContainer(dynamics);
    std::vector<DynamicName> dynNames;
    rd.ReadContainer(dynNames);
    uint32 numGroups;
    rd.Read(numGroups);
    std::vector<MESH> meshes;
    rd.ReadContainer(meshes);
    const size_t dynNodeIndex = nodes.size();
    scenes.back().nodes.push_back(dynNodeIndex);
    nodes.emplace_back().name = "Dynamics";

    // When 1 or more nodes within group are hit, activate the rest
    for (size_t g = 0; g < numGroups; g++) {
      const size_t nodeIndex = nodes.size();
      nodes.at(dynNodeIndex).children.push_back(nodeIndex);
      nodes.emplace_back().name = "Group" + std::to_string(g);
    }

    for (auto &m : meshes) {
      const size_t modelIndex = nodes.size();
      nodes.at(dynNodeIndex + m.groupId + 1).children.push_back(modelIndex);
      auto &model = nodes.emplace_back();
      model.name = std::move(m.name);

      if (!m.dynamicObject.empty()) {
        model.name.push_back('[');
        model.name.append(m.dynamicObject);
        model.name.push_back(']');
      }

      memcpy(model.matrix.data(), m.tm, 64);

      for (size_t p = 0; p < m.meshes.size(); p++) {
        auto &dynWrap = dynNames[m.meshes[p]];
        auto &dynItem0 = dynamics[dynWrap.id0];

        for (auto d : dynItem0.bmodIndices) {
          nodes.at(modelIndex).children.push_back(nodes.size());
          auto &sModel = nodes.emplace_back();
          sModel.mesh = d + bmodsBegin;
        }

        if (dynWrap.id1 > -1) {
          auto &dynItem1 = dynamics[dynWrap.id1];

          for (auto d : dynItem1.bmodIndices) {
            nodes.at(modelIndex).children.push_back(nodes.size());
            auto &sModel = nodes.emplace_back();
            sModel.mesh = d + bmodsBegin;
          }
        }
      }
    }
  }

  void GetMapData(BinReaderRef rd) {
    const size_t mapNodeIndex = nodes.size();
    scenes.back().nodes.push_back(mapNodeIndex);
    nodes.emplace_back().name = "Map";

    std::vector<TrackModel> terrain;
    rd.ReadContainer(terrain);

    for (auto &t : terrain) {
      nodes.at(mapNodeIndex).children.push_back(nodes.size());
      auto &model = nodes.emplace_back();
      model.mesh = meshes.size();

      auto &mesh = meshes.emplace_back();
      mesh.primitives.emplace_back(primitives.at(t.primitiveIndex));
    }
  }

  void GetFoliages(BinReaderRef rd) {
    uint32 unk;
    rd.Read(unk);
    // per poly indices 24b index (to what?), 8b sprite clamp?
    rd.Skip(4 * unk);
    // LOD sprites
    rd.Read(unk);
    rd.Skip(sizeof(SpriteVertex) * unk);

    std::vector<Foliage> foliages;
    rd.ReadContainer(foliages);
    size_t fid = 0;
    const size_t folNodeIndex = nodes.size();
    scenes.back().nodes.push_back(folNodeIndex);
    nodes.emplace_back().name = "Foliages";

    for (auto &t : foliages) {
      const size_t nodeIndex = nodes.size();
      nodes.at(folNodeIndex).children.push_back(nodeIndex);
      auto &model = nodes.emplace_back();
      model.name = "Foliage" + std::to_string(fid++);

      if (t.trunkPrimitiveIndex >= 0) {
        nodes.at(nodeIndex).children.push_back(nodes.size());
        auto &model = nodes.emplace_back();
        model.mesh = meshes.size();
        auto &mesh = meshes.emplace_back();
        mesh.primitives.emplace_back(primitives.at(t.trunkPrimitiveIndex));
      }

      if (t.branchesPrimitiveIndex >= 0) {
        nodes.at(nodeIndex).children.push_back(nodes.size());
        auto &model = nodes.emplace_back();
        model.mesh = meshes.size();
        auto &mesh = meshes.emplace_back();
        mesh.primitives.emplace_back(primitives.at(t.branchesPrimitiveIndex));
      }

      if (t.leavesSpritePrimitiveIndex >= 0) {
        nodes.at(nodeIndex).children.push_back(nodes.size());
        auto &model = nodes.emplace_back();
        model.mesh = meshes.size();
        es::Matrix44 nTM(t.leavesSpriteTM[0], t.leavesSpriteTM[1],
                         t.leavesSpriteTM[2], t.leavesSpriteTM[3]);
        nTM *= es::Matrix44({t.leavesSpriteScale.X, 0, 0, 0},
                            {0, t.leavesSpriteScale.Y, 0, 0},
                            {0, 0, t.leavesSpriteScale.Z, 0});

        memcpy(model.matrix.data(), &nTM, 64);
        auto &mesh = meshes.emplace_back();
        mesh.primitives.emplace_back(
            primitives.at(t.leavesSpritePrimitiveIndex));
      }
    }
  }

  void CleanMeta() {
    for (auto &m : meshes) {
      for (auto &p : m.primitives) {
        p.GetExtensionsAndExtras() = {};
      }
    }
  }
};

void AppProcessFile(std::istream &stream, AppContext *ctx) {
  BinReaderRef rd(stream);
  uint32 meshType;
  rd.Read(meshType);
  MainGLTF main;
  AFileInfo finf(ctx->workingFile);

  // NOTE: FO2 PC only
  // TODO:  Generate sprite polys
  //        Foliage LODs, indices
  //        Might be overkill but bvh grouping?

  if (meshType == 0x20000) {
    main.GetMaterials(rd);
    main.GetBuffers(rd);
    main.GetPrimitivesGeneric(rd);
    main.GetBMODs(rd);
    main.GetMESHes(rd);
    main.GetObjects(rd);

    if (finf.GetFilename() == "body") {
      try {
        AppContextStream str =
            ctx->RequestFile(finf.GetFolder().to_string() + "crash.dat");
        main.LoadCrashData(*str.Get());
      } catch (const std::exception &e) {
        printerror(e.what());
      }
    }
    main.CleanMeta();
  } else if (meshType == 0x10004) {
    main.GetMaterials(rd);
    main.GetBuffers(rd);
    main.GetPrimitivesOverlay(rd);
    main.GetBMODs(rd);
    main.GetMESHes(rd);
  } else if (meshType == 0x20001) {
    uint32 unk;
    rd.Read(unk);

    if (unk != 1) {
      printwarning("Invalid unk value " << unk);
    }

    main.GetMaterials(rd);
    main.GetBuffers(rd);
    main.GetPrimitivesGeneric(rd, true);
    main.GetMapData(rd);
    main.GetFoliages(rd);
    rd.Skip(64);
    main.GetBMODs(rd);
    main.GetObjects(rd);
    main.GetDynamics(rd);
  } else {
    throw std::runtime_error("Invalid mesh type: " + std::to_string(meshType));
  }

  AFileInfo outPath(ctx->outFile);
  BinWritter wr(outPath.GetFullPathNoExt().to_string() + ".glb");

  main.FinishAndSave(wr, outPath.GetFolder());
}
