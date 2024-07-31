/*  Ridge2GLTF
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

#include "project.h"
#include "spike/app_context.hpp"
#include "spike/except.hpp"
#include "spike/gltf.hpp"
#include "spike/io/binreader_stream.hpp"
#include "spike/io/binwritter.hpp"
#include "spike/io/fileinfo.hpp"
#include "spike/master_printer.hpp"
#include "spike/reflect/reflector.hpp"
#include "spike/type/flags.hpp"
#include "spike/type/matrix44.hpp"
#include <vector>

std::string_view filters[]{
    ".vhcl$",
    ".scne$",
};

static AppInfo_s appInfo{
    .multithreaded = false,
    .header = Ridge2GLTF_DESC " v" Ridge2GLTF_VERSION ", " Ridge2GLTF_COPYRIGHT
                              "Lukas Cone",
    .filters = filters,
};

AppInfo_s *AppInitModule() { return &appInfo; }

struct BBOX {
  Vector min;
  Vector max;

  void Read(BinReaderRef_e rd) {
    rd.Read(min);
    rd.Read(max);
  }
};

struct Chunk {
  uint32 id = 0;
  uint32 unk;
  uint32 numItems = 0;

  void Read(BinReaderRef_e rd) {
    rd.Read(id);
    rd.Read(unk);
    rd.Read(numItems);
  }
};

template <class C> void ValidateChunk(C *item, BinReaderRef_e rd) {
  static_cast<Chunk *>(item)->Read(rd);
  if (item->id != C::ID) {
    throw std::runtime_error(
        "Invalid chunk " +
        es::InvalidHeaderError::DecompileFourCC(item->id, 4) + " at offset " +
        std::to_string(rd.Tell() - 12));
  }
}

struct Resource {
  uint32 type;
  std::string path;

  void Read(BinReaderRef_e rd) {
    rd.Read(type);
    rd.ReadContainer(path);
  }
};

struct Material : Chunk {
  static constexpr uint32 ID = CompileFourCC("lrtm");
  std::string name;
  uint32 unk[6];

  void Read(BinReaderRef_e rd) {
    ValidateChunk(this, rd);
    if (numItems != 1) {
      throw std::runtime_error("Unexpected mtrl count at offset " +
                               std::to_string(rd.Tell() - 4));
    }
    rd.ReadContainer(name);
    rd.Read(unk);
  }
};

struct Textures : Chunk {
  static constexpr uint32 ID = CompileFourCC("rtxt");
  struct Item {
    uint32 slot;
    Resource res;

    void Read(BinReaderRef_e rd) {
      rd.Read(slot);
      rd.Read(res);
    }
  };

  std::vector<Item> textures;

  void Read(BinReaderRef_e rd) {
    ValidateChunk(this, rd);
    rd.ReadContainer(textures, numItems);
  }
};

struct Vertex {
  SVector pos;
  uint16 deform;
  UCVector4 polar;
  SVector2 texCoord;
};

static_assert(sizeof(Vertex) == 16);

template <> void FByteswapper(Vertex &item, bool) {
  FByteswapper(item.pos);
  FByteswapper(item.deform);
  FByteswapper(item.texCoord);
}

struct Vertices : Chunk {
  static constexpr uint32 ID = CompileFourCC("trev");

  std::vector<Vertex> vertices;

  void Read(BinReaderRef_e rd) {
    ValidateChunk(this, rd);
    rd.ReadContainer(vertices, numItems);
  }
};

struct Triangles : Chunk {
  static constexpr uint32 ID = CompileFourCC("airt");
  std::vector<USVector> tris;

  void Read(BinReaderRef_e rd) {
    ValidateChunk(this, rd);
    rd.ReadContainer(tris, numItems);
  }
};

struct Edges : Chunk {
  static constexpr uint32 ID = CompileFourCC("mgde");

  void Read(BinReaderRef_e rd) {
    ValidateChunk(this, rd);
    if (numItems != 0) {
      throw std::runtime_error("Unexpected edgm count at offset " +
                               std::to_string(rd.Tell() - 4));
    }
  }
};

struct Batch : Chunk {
  static constexpr uint32 ID = CompileFourCC("hctb");

  struct Item {
    Vector4A16 meshOffset;
    float meshScale;
    BBOX bbox;
    Material material;
    Textures textures;
    Vertices vertices;
    Triangles triangles;
    Edges edges;

    void Read(BinReaderRef_e rd) {
      Vector meshOffset_;
      rd.Read(meshOffset_);
      meshOffset = meshOffset_;
      rd.Read(meshScale);
      rd.Read(bbox);

      for (size_t i = 0; i < 5; i++) {
        rd.Push();
        Chunk item;
        rd.Read(item);
        rd.Pop();

        switch (item.id) {
        case Material::ID:
          rd.Read(material);
          break;
        case Textures::ID:
          rd.Read(textures);
          break;
        case Vertices::ID:
          rd.Read(vertices);
          break;
        case Triangles::ID:
          rd.Read(triangles);
          break;
        case Edges::ID:
          rd.Read(edges);
          break;
        default:
          throw std::runtime_error("Unexpected extra btch subclass at " +
                                   std::to_string(rd.Tell()));
        }
      }
    }
  };

  std::vector<Item> items;

  void Read(BinReaderRef_e rd) {
    ValidateChunk(this, rd);
    rd.ReadContainer(items, numItems);
  }
};

struct Mesh : Chunk {
  static constexpr uint32 ID = CompileFourCC("hsem");

  struct Lod {
    std::string name;
    Batch batch;

    void Read(BinReaderRef_e rd) {
      rd.ReadContainer(name);
      rd.Read(batch);
    }
  };

  std::vector<Lod> lods;

  void Read(BinReaderRef_e rd) {
    ValidateChunk(this, rd);
    rd.ReadContainer(lods, numItems);
  }
};

struct PMesh : Chunk {
  static constexpr uint32 ID = CompileFourCC("hsmp");
  BBOX bbox;
  Mesh mesh;

  void Read(BinReaderRef_e rd) {
    ValidateChunk(this, rd);

    if (numItems > 0) {
      if (numItems > 1) {
        throw std::runtime_error("Unexpected mesh count at offset " +
                                 std::to_string(rd.Tell() - 4));
      }

      rd.Read(bbox);
      rd.Read(mesh);
    }
  }
};

struct Shape : Chunk {
  static constexpr uint32 ID = CompileFourCC("ephs");
  struct Item {
    uint32 unk;
    std::vector<uint8> shapeData;

    void Read(BinReaderRef_e rd) {
      rd.Read(unk);
      rd.ReadContainer(shapeData);
    }
  };

  std::vector<Item> items;

  void Read(BinReaderRef_e rd) {
    ValidateChunk(this, rd);
    rd.ReadContainer(items, numItems);
  }
};

struct Keyframes : Chunk {
  static constexpr uint32 ID = CompileFourCC("arfk");

  void Read(BinReaderRef_e rd) {
    ValidateChunk(this, rd);
    if (numItems) {
      throw std::runtime_error("Unexpected kfra count at offset " +
                               std::to_string(rd.Tell() - 4));
    }
  }
};

struct Anim : Chunk {
  static constexpr uint32 ID = CompileFourCC("mina");
  Keyframes frames;

  void Read(BinReaderRef_e rd) {
    ValidateChunk(this, rd);
    if (numItems != 1) {
      throw std::runtime_error("Unexpected anim count at offset " +
                               std::to_string(rd.Tell() - 4));
    }

    rd.Read(frames);
  }
};

struct Dummy : Chunk {
  static constexpr uint32 ID = CompileFourCC("ymmd");

  struct Item {
    es::Matrix44 mtx;
    std::string name;

    void Read(BinReaderRef_e rd) {
      rd.Read(mtx);
      rd.ReadContainer(name);
    }
  };

  std::vector<Item> items;

  void Read(BinReaderRef_e rd) {
    ValidateChunk(this, rd);

    rd.ReadContainer(items, numItems);
  };
};

struct Model : Chunk {
  static constexpr uint32 ID = CompileFourCC("ldom");
  struct Item {
    std::string name0;
    std::string name1;
    es::Matrix44 mtx;
    BBOX bbox;
    PMesh pmesh0;
    PMesh pmesh1;
    Shape shape;
    Anim anim;
    Dummy dummy;
    uint32 unk[3];

    void Read(BinReaderRef_e rd) {
      rd.ReadContainer(name0);
      rd.ReadContainer(name1);
      rd.Read(mtx);
      rd.Read(bbox);

      for (size_t i = 0; i < 5; i++) {
        rd.Push();
        Chunk item;
        rd.Read(item);
        rd.Pop();

        switch (item.id) {
        case PMesh::ID:
          if (!pmesh0.id) {
            rd.Read(pmesh0);
            break;
          } else if (!pmesh1.id) {
            rd.Read(pmesh1);
            break;
          } else {
            throw std::runtime_error("Unexpected extra pmesh at " +
                                     std::to_string(rd.Tell()));
          }
        case Shape::ID:
          rd.Read(shape);
          break;
        case Anim::ID:
          rd.Read(anim);
          break;
        case Dummy::ID:
          rd.Read(dummy);
          break;
        default:
          throw std::runtime_error("Unexpected extra modl subclass at " +
                                   std::to_string(rd.Tell()));
        }
      }

      rd.Read(unk);
    }
  };

  std::vector<Item> items;

  void Read(BinReaderRef_e rd) {
    ValidateChunk(this, rd);
    rd.ReadContainer(items, numItems);
  }
};

struct VehiclePhysicsShape : Chunk {
  static constexpr uint32 ID = CompileFourCC("hpsv");
};

struct Vehicle {
  static constexpr uint32 ID = CompileFourCC("lchv");
  static constexpr uint32 VERSION = 2;
  Dummy dummy;
  VehiclePhysicsShape physShape;

  void Read(BinReaderRef_e rd) { rd.Read(dummy); }
};

struct Scene {
  static constexpr uint32 ID = CompileFourCC("encs");
  static constexpr uint32 VERSION = 6;
};

struct Header {
  uint32 type;
  uint32 version;
  Model model;

  void Read(BinReaderRef_e rd) {
    rd.SwapEndian(true);
    rd.Read(type);
    rd.Read(version);

    if (type == Vehicle::ID) {
      if (version != Vehicle::VERSION) {
        throw es::InvalidVersionError(version);
      }
      rd.Read(model);
    } else if (type == Scene::ID) {
      if (version != Scene::VERSION) {
        throw es::InvalidVersionError(version);
      }
      rd.Read(model);
    } else {
      throw es::InvalidHeaderError(type);
    }
  }
};

struct GLTFRidge : GLTF {
  GLTFStream &GetIndexStream() {
    if (indexStream < 0) {
      auto &str = NewStream("indices");
      str.target = gltf::BufferView::TargetType::ElementArrayBuffer;
      indexStream = str.slot;
      return str;
    }

    return Stream(indexStream);
  }

  GLTFStream &GetVts() {
    if (vtsStream < 0) {
      auto &str = NewStream("vertices", 16);
      str.target = gltf::BufferView::TargetType::ArrayBuffer;
      vtsStream = str.slot;
      return str;
    }

    return Stream(vtsStream);
  }

private:
  int32 indexStream = -1;
  int32 vtsStream = -1;
};

#include <glm/ext.hpp>
#include <glm/glm.hpp>

void AppProcessFile(AppContext *ctx) {
  Header hdr;
  hdr.Read(ctx->GetStream());
  GLTFModel main;
  main.extensionsRequired.emplace_back("KHR_mesh_quantization");
  main.extensionsUsed.emplace_back("KHR_mesh_quantization");

  for (auto &m : hdr.model.items) {
    for (auto &l : m.pmesh0.mesh.lods) {
      size_t bid = 0;
      for (auto &b : l.batch.items) {
        gltf::Primitive prim;
        {
          auto &psStream = main.GetVt8();
          auto [posAcc, posId] = main.NewAccessor(psStream, 4);
          prim.attributes["POSITION"] = posId;
          posAcc.componentType = gltf::Accessor::ComponentType::Short;
          posAcc.count = b.vertices.numItems;
          posAcc.type = gltf::Accessor::Type::Vec3;

          for (auto &v : b.vertices.vertices) {
            USVector4 pos(v.pos);
            psStream.wr.Write(pos);
          }
        }

        if (0) {
          auto &nrStream = main.GetVt12();
          auto [posAcc, posId] = main.NewAccessor(nrStream, 4);
          prim.attributes["NORMAL"] = posId;
          posAcc.componentType = gltf::Accessor::ComponentType::Float;
          posAcc.count = b.vertices.numItems;
          posAcc.type = gltf::Accessor::Type::Vec3;

          for (auto &v : b.vertices.vertices) {
            Vector4 r1, r2, r3, r4;
            /*v.polar = UCVector4{0xf8, 0x40, 0x1f, 00};

            Vector4 tmp0, tmp1, tmp2, tmp3;
            // clang-format off
            //  tmp0.xy = vec4(0.125, 0.015625, 0.0078125, -1) * in1.wwww
            tmp0.x = 0.125 * v.polar.w;
            tmp0.y = 0.015625 * v.polar.w;
            //  tmp0.xy = fract(tmp0.xyzw)
            tmp0 -= tmp0.Convert<int32>().Convert<float>();
            //  tmp0.z = vec4(0.00390625, 0.00390625, 0.00390625, 0.00390625) *
            in1.wwww tmp0.z = 0.00390625 * v.polar.w;
            //  tmp0.xyz = tmp0.xyzw + in1.xyzw
            tmp0 += v.polar.Convert<float>();
            //  tmp0.xyz = tmp0.xyzw * vec4(0.0078125, 0.0078125, 0.0078125,
            0.0078125) + vec4(-1, -1, -1, -1) tmp0 = tmp0 * 0.0078125 - 1;
            //  tmp0.w = abs(tmp0.zzzz) + vec4(-0.5, -0.5, -0.5, -0.5)
            tmp0.w = abs(tmp0.z) - 0.5;
            /*tmp0 *= 3.14159;

            float x = tmp0.z * sin(tmp0.y) * cos(tmp0.x);
            float y = tmp0.z * sin(tmp0.y) * sin(tmp0.x);
            float z = tmp0.z * cos(tmp0.y);

            glm::mat4 mtx(glm::quat(glm::vec3(x,y ,z)));
            nrStream.wr.Write(mtx[0][0]);
            nrStream.wr.Write(mtx[0][1]);
            nrStream.wr.Write(mtx[0][2]);*/

            /*//  tmp1.xyz = tmp0.xyww * vec4(0.5, 0.25, 1, 1) + vec4(0.25,
            0.25, 0.25, 0.25) tmp1.x = tmp0.x * 0.5 + 0.25; tmp1.y = tmp0.y *
            0.25 + 0.25; tmp1.z = tmp0.w * 1 + 0.25;
            //  tmp0.xyz = tmp0.xyww * vec4(0.5, 0.25, 1, 1) + vec4(0.5, 0.5,
            0.5, 0.5) tmp0.x = tmp0.x * 0.5 + 0.5; tmp0.y = tmp0.y * 0.25 + 0.5;
            tmp0.z = tmp0.w * 1 + 0.5;
            //  tmp0.xyz = fract(tmp0.xyzw)
            tmp0 -= tmp0.Convert<int32>().Convert<float>();
            //  tmp0.xyz = tmp0.xyzw * vec4(6.28319, 6.28319, 6.28319, 6.28319)
            + vec4(-3.14159, -3.14159, -3.14159, -3.14159) tmp0 = tmp0 * 6.28319
            + -3.14159;
            //  tmp0.xyz = tmp0.xyzw * tmp0.xyzw
            tmp0 *= tmp0;
            //  tmp1.xyz = fract(tmp1.xyzw)
            tmp1 -= tmp1.Convert<int32>().Convert<float>();
            //  tmp1.xyz = tmp1.xyzw * vec4(6.28319, 6.28319, 6.28319, 6.28319)
            + vec4(-3.14159, -3.14159, -3.14159, -3.14159) tmp1 = tmp1 * 6.28319
            + -3.14159;
            //  tmp1.xyz = tmp1.xyzw * tmp1.xyzw
            tmp1 *= tmp1;
            //  tmp2.xyz = tmp1.xyzw * vec4(-2.52399e-07, -2.52399e-07,
            -2.52399e-07, -2.52399e-07) +
            vec4(2.47609e-05, 2.47609e-05, 2.47609e-05, 2.47609e-05) tmp2 = tmp1
            * -2.52399e-07 + 2.47609e-05;
            //  tmp2.xyz = tmp1.xyzw * tmp2.xyzw + vec4(-0.00138884,
            -0.00138884, -0.00138884, -0.00138884) tmp2 = tmp1 * tmp2 -
            0.00138884;
            //  tmp2.xyz = tmp1.xyzw * tmp2.xyzw + vec4(0.0416666, 0.0416666,
            0.0416666, 0.0416666) tmp2 = tmp1 * tmp2 + 0.0416666;
            //  tmp2.xyz = tmp1.xyzw * tmp2.xyzw + vec4(-0.5, -0.5, -0.5, -0.5)
            tmp2 = tmp1 * tmp2 - 0.5;
            //  tmp1.xy_w = tmp1.xzzy * tmp2.xzzy + -vec4(-1, -1, -1, -1)
            tmp1.x = tmp1.x * tmp2.x + 1;
            tmp1.y = tmp1.z * tmp2.z + 1;
            tmp1.w = tmp1.y * tmp2.y + 1;
            //  tmp2.xyz = tmp0.xyzw * vec4(-2.52399e-07, -2.52399e-07,
            -2.52399e-07, -2.52399e-07) +
            vec4(2.47609e-05, 2.47609e-05, 2.47609e-05, 2.47609e-05) tmp2 = tmp0
            * -2.52399e-07 + 2.47609e-05;
            //  tmp2.xyz = tmp0.xyzw * tmp2.xyzw + vec4(-0.00138884,
            -0.00138884, -0.00138884, -0.00138884) tmp2 = tmp0 * tmp2 -
            0.00138884;
            //  tmp2.xyz = tmp0.xyzw * tmp2.xyzw + vec4(0.0416666, 0.0416666,
            0.0416666, 0.0416666) tmp2 = tmp0 * tmp2 + 0.0416666;
            //  tmp2.xyz = tmp0.xyzw * tmp2.xyzw + vec4(-0.5, -0.5, -0.5, -0.5)
            tmp2 = tmp0 * tmp2 - 0.5;
            //  tmp0.xyz = tmp0.xyzw * tmp2.xyzw + -vec4(-1, -1, -1, -1)
            tmp0 = tmp0 * tmp2 + 1;
            //  tmp2.xy = tmp1.wyzw * tmp0.xxxx
            tmp2.x = tmp1.w * tmp0.x;
            tmp2.y = tmp1.y * tmp0.x;
            //  tmp2.__zw = tmp1.xyyw * tmp1.xxxx
            tmp2.z = tmp1.y * tmp1.x;
            tmp2.w = tmp1.w * tmp1.x;
            //  tmp3.x = tmp2.xxxx * -tmp0.zzzz + tmp2.zzzz
            tmp3.x = tmp2.x * -tmp0.z + tmp2.z;
            //  tmp3.z = tmp2.wwww * tmp0.zzzz + tmp2.yyyy
            tmp3.z = tmp2.w * tmp0.z + tmp2.y;
            //  tmp1._yz_ = tmp0.xyzw * tmp0.xxyw
            tmp1.y = tmp0.y * tmp0.x;
            tmp1.z = tmp0.z * tmp0.y;
            //  tmp1.x = tmp0.yyyy * -tmp1.xxxx
            tmp1.x = tmp0.y * -tmp1.x;
            //  tmp3.y = tmp1.zzzz
            tmp3.y = tmp1.z;

            nrStream.wr.Write(Vector(tmp3));

            //  tmp2.xyz = tmp3.yzxw * tmp1.xyww
            tmp2.x = tmp3.y * tmp1.x;
            tmp2.y = tmp3.z * tmp1.y;
            tmp2.z = tmp3.x * tmp1.w;
            //  tmp2.xyz = tmp1.wxyw * tmp3.zxyw + -tmp2.xyzw
            tmp2.x = tmp1.w * tmp3.z - tmp2.x;
            tmp2.y = tmp1.x * tmp3.x - tmp2.y;
            tmp2.z = tmp1.y * tmp3.y - tmp2.z;
            //  SLT tmp0.z in1.zzzz vec4(0, 0, 0, 0)
            tmp0.z = v.polar.z < 0 ? 1 : 0;
            //  tmp3.xyz = tmp0.zzzz * tmp2.xyzw
            tmp3 = tmp2 * tmp0.z;
            //  tmp2.xyz = tmp3.xyzw * vec4(-2, -2, -2, -2) + tmp2.xyzw
            tmp2 = (tmp3 * -2) + tmp2;
            // clang-format on

            nrStream.wr.Write(Vector(tmp2));

            tmp1 = Vector4{};
            tmp3 = Vector4{};
            Vector4 tmp4;*/

            /*// mul r1.xyz, c4.yzww, v1.w
            // r1.xyz = vec4(0.125, 0.015625, 0.00390625, 0) * v1.w
            r1 = Vector4{0.125, 0.015625, 0.00390625, 0} * v.polar.w;

            // frc r1.xy, r1
            // r1.xy = fract(r1)
            r1 -= r1.Convert<int32>().Convert<float>();

            // add r1.xyz, r1, v1
            // r1.xyz = r1 + v1
            r1 = r1 + v.polar.Convert<float>();

            // mad r1.xyz, r1, c5.x, c5.y
            // r1.xyz = r1 * 0.0078125 - 1
            r1 = (r1 * 0.0078125) - 1;

            // add r1.w, r1_abs.z, c5.z
            // r1.w = abs(r1.z) - 0.5
            r1.w = abs(r1.z) - 0.5;

            // mad r2.xyz, r1.xyww, -c5.zwyw, -c5.w
            // r2.xyz = r1.xyw * -vec4(-0.5, -0.25, -1, 0) + 0.25
            auto r1t = r1;
            r1t.z = r1.w;
            r2 = r1t * -Vector4(-0.5, -0.25, -1, 0) + 0.25;

            // mad r1.xyz, r1.xyww, -c5.zwyw, -c5.z
            // r1.xyz = r1.xyw * -vec4(-0.5, -0.25, -1, 0) + 0.5
            r1 = r1t * -Vector4(-0.5, -0.25, -1, 0) + 0.5;

            // frc r1.xyz, r1
            // r1.xyz = fract(r1)
            r1 -= r1.Convert<int32>().Convert<float>();

            // mad r1.xyz, r1, c6.x, c6.y
            // r1.xyz = r1 * 6.28318548 -3.14159274
            r1 = r1 * 6.28318548 -3.14159274;

            // mul r1.xyz, r1, r1
            // r1.xyz = r1 * r1
            r1 *= r1;

            // frc r2.xyz, r2
            // r2.xyz = fract(r2)
            r2 -= r2.Convert<int32>().Convert<float>();

            // mad r2.xyz, r2, c6.x, c6.y
            // r2.xyz = r2 * 6.28318548 -3.14159274
            r2 = r2 * 6.28318548 -3.14159274;

            // mul r2.xyz, r2, r2
            // r2.xyz = r2 * r2
            r2 *= r2;

            // mad r3.xyz, r2, c6.z, c6.w
            // r3.xyz = r2 * -2.52398507e-007 + 2.47609005e-005
            r3 = r2 * -2.52398507e-007 + 2.47609005e-005;

            // mad r3.xyz, r2, r3, c7.x
            // r3.xyz = r2 * r3 - 0.00138883968
            r3 = r2 * r3 - 0.00138883968;

            // mad r3.xyz, r2, r3, c7.y
            // r3.xyz = r2 * r3 + 0.0416666418
            r3 = r2 * r3 + 0.0416666418;

            // mad r3.xyz, r2, r3, c5.z
            // r3.xyz = r2 * r3 - 0.5
            r3 = r2 * r3 - 0.5;

            // mad r2.xyz, r2, r3, c4.x
            // r2.xyz = r2 * r3 + 1
            r2 = r2 * r3 + 1;

            // mad r3.xyz, r1, c6.z, c6.w
            // r3.xyz = r1 * -2.52398507e-007 + 2.47609005e-005
            r3 = r1 * -2.52398507e-007 + 2.47609005e-005;

            // mad r3.xyz, r1, r3, c7.x
            // r3.xyz = r1 * r3 - 0.00138883968
            r3 = r1 * r3 - 0.00138883968;

            // mad r3.xyz, r1, r3, c7.y
            // r3.xyz = r1 * r3 + 0.0416666418
            r3 = r1 * r3 + 0.0416666418;

            // mad r3.xyz, r1, r3, c5.z
            // r3.xyz = r1 * r3 - 0.5
            r3 = r1 * r3 - 0.5;

            // mad r1.xyz, r1, r3, c4.x
            // r1.xyz = r1 * r3 + 1
            r1 = r1 * r3 + 1;

            // mul r3.x, r1.y, -r2.x
            // r3.x = r1.y * -r2.x
            r3.x = r1.y * -r2.x;

            // mov r3.w, r2.y
            // r3.w = r2.y
            r3.w = r2.y;

            // mul r3._yz, r1, r1.xxyw
            // r3._yz = r1 * r1.xxyw
            r3.y = r1.x * r1.x;
            r3.z = r1.y * r1.x;

            /*nrStream.wr.Write(r3.x);
            nrStream.wr.Write(r3.y);
            nrStream.wr.Write(r3.w);*/

            /*// mul r1.xy, r2.yzzw, r1.x
            // r1.xy = r2.yz * r1.x
            r1.x = r2.y * r1.x;
            r1.y = r2.z * r1.x;

            // mul r2.xy, r2.zyzw, r2.x
            r2.x = r2.z * r2.x;
            r2.y = r2.y * r2.x;

            // mad r4.x, r1.x, -r1.z, r2.x
            r4.x = r1.x * -r1.z + r2.x;

            // mad r4.z, r2.y, r1.z, r1.y
            r4.z = r2.y * r1.z + r1.y;

            // mov r4.y, r3.z
            r4.y = r3.z;
            */

            /*nrStream.wr.Write(r4.x);
            nrStream.wr.Write(r4.y);
            nrStream.wr.Write(r4.z);*/

            /*// mul r1.xyz, r3.xyww, r4.yzxw
            // r1 = r3.xyw * r4.yzx
            auto r3xyw = r3;
            r3xyw.z = r3.w;
            Vector4 r4yzx(r4.y, r4.z, r4.x, r4.w);
            r1 = r3xyw * r4yzx;

            // mad r1.xyz, r3.wxyw, r4.zxyw, -r1
            // r1 = r3.wxy * r4.zxy - r1;
            Vector4 r3wxy(r3.w, r3.x, r3.y, r3.w);
            Vector4 r4zxy(r4.z, r4.x, r4.y, r4.w);
            r1 = r3wxy * r4zxy - r1;*/

            // slt r1.w, v1.z, c7.z
            // r1.w = v1.z < 0 ? 1 : 0

            // mul r2.xyz, r1.w, r1
            // r2 = r1.w * r1

            // mad r1.xyz, r2, c7.w, r1
            // r1 = r0 * -2 +

            // mul r1, c7.xyxy, v1.w
            // r1 = vec4(0.125, 0.015625, 0.125, 0.015625) * v1.w
            // r1 = Vector4(0.125, 0.015625, 0.125, 0.015625) * v.polar.w;

            // frc r1, r1
            // r1 = fract(r1)
            // r1 -= r1.Convert<int32>().Convert<float>();

            // add r1, r1, v1.xyxy
            // r1 = r1 + v1.xyxy
            float v1x = v.polar.x;
            float v1y = v.polar.y;
            r1 = r1 + Vector4(v1x, v1y, v1x, v1y);

            // mad r1, r1, c7.z, c7.w
            // r1 = r1 * 0.0078125 -1
            r1 = (r1 * 0.0078125 - 1) * 3.14159274;

            // mad r1, r1, c8.xyxy, c8.xxyy
            // r1 = r1 * vec4(0.5, 0.25, 0.5, 0.25) + vec4(0.5, 0.5, 0.25, 0.25)
            r1 = r1 * Vector4(0.5, 0.25, 0.5, 0.25) +
                 Vector4(0.5, 0.5, 0.25, 0.25);

            // frc r1, r1
            // r1 = fract(r1)
            r1 -= r1.Convert<int32>().Convert<float>();

            // mad r1, r1, c8.z, c8.w
            // r1 = r1 * 6.28318548, -3.14159274
            r1 = r1 * 6.28318548 - 3.14159274;

            auto r1_ = r1;

            Vector4 r1s(sin(r1_.x), sin(r1_.y), sin(r1_.z), sin(r1_.w));
            Vector4 r1c(cos(r1_.x), cos(r1_.y), cos(r1_.z), cos(r1_.w));

            // mul r1, r1, r1
            // r1 = r1 * r1
            r1 = r1 * r1;
            // mad r2, r1, c10.x, c10.y
            // r2 = r1 * -2.52398507e-007 + 2.47609005e-005
            r2 = r1 * -2.52398507e-007 + 2.47609005e-005;
            // mad r2, r1, r2, c10.z
            // r2 = r1 * r2 - 0.00138883968
            r2 = r1 * r2 - 0.00138883968;
            // mad r2, r1, r2, c10.w
            // r2 = r1 * r2 + 0.0416666418
            r2 = r1 * r2 + 0.0416666418;
            // mad r2, r1, r2, -c8.x
            // r2 = r1 * r2 - 0.5
            r2 = r1 * r2 - 0.5;
            // mad r1, r1.xwyz, r2.xwyz, -c7.w
            // r1 = r1.xwyz * r2.xwyz + 1
            Vector4 r1_xwyz(r1.x, r1.w, r1.y, r1.z);
            Vector4 r2_xwyz(r2.x, r2.w, r2.y, r2.z);
            r1 = r1_xwyz * r2_xwyz + 1;
            // mul r1.x, r1.z, r1.x
            // r1.x = r1.z * r1.x
            r1.x = r1.z * r1.x;
            // mul r1.z, r1.z, -r1.w
            // r1.z = r1.z * -r1.w
            r1.z = r1.z * -r1.w;

            nrStream.wr.Write(r1.x);
            nrStream.wr.Write(r1.y);
            nrStream.wr.Write(r1.z);

            // mov r1.xy, c11
            // r1.xy =  vec4(0.0500000007, 1.25, 0.00048828125, 0)

            // mul r0.y, r1.x, c128.y

            // mad r0.y, c48.x, r1.y, r0.y
            // mad o1.w, v2.y, c11.z, r0.y
            // mov o0.w, r0.x
            // mov o2.w, r0.x
            // mul o1.xyz, c11.z, v2.xyxw
          }
        }

        auto &idStream = main.GetIndexStream();
        auto [idAcc, idId] = main.NewAccessor(idStream, 2);
        prim.indices = idId;

        idAcc.componentType = gltf::Accessor::ComponentType::UnsignedShort;
        idAcc.count = b.triangles.numItems * 3;
        idAcc.type = gltf::Accessor::Type::Scalar;

        idStream.wr.WriteContainer(b.triangles.tris);

        gltf::Mesh mesh;
        mesh.primitives.emplace_back(std::move(prim));
        gltf::Node node;
        node.name = l.name + std::to_string(bid++);
        auto ntx = m.mtx;
        ntx.r1() *= b.meshScale;
        ntx.r2() *= b.meshScale;
        ntx.r3() *= b.meshScale;
        ntx.r4() += b.meshOffset;
        memcpy(node.matrix.data(), &ntx, sizeof(ntx));
        node.mesh = main.meshes.size();
        main.meshes.emplace_back(std::move(mesh));
        main.scenes.front().nodes.emplace_back(main.nodes.size());
        main.nodes.emplace_back(std::move(node));
      }
    }
  };

  BinWritterRef wr(ctx->NewFile(ctx->workingFile.ChangeExtension2("glb")).str);
  main.FinishAndSave(wr, std::string(ctx->workingFile.GetFolder()));
}
