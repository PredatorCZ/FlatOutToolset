/*  DB2JSON
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
#include "db.hpp"
#include "nlohmann/json.hpp"
#include "project.h"
#include <vector>

using nlohmann::json;

es::string_view filters[]{
    ".db$",
    {},
};

struct DB2JSON : ReflectorBase<DB2JSON> {
} settings;

REFLECT(CLASS(DB2JSON));

AppInfo_s appInfo{
    AppInfo_s::CONTEXT_VERSION,
    AppMode_e::CONVERT,
    ArchiveLoadType::FILTERED,
    DB2JSON_DESC " v" DB2JSON_VERSION ", " DB2JSON_COPYRIGHT "Lukas Cone",
    reinterpret_cast<ReflectorFriend *>(&settings),
    filters,
};

const AppInfo_s *AppInitModule() { return &appInfo; }

void AppProcessFile(std::istream &stream, AppContext *ctx) {
  BinReaderRef rd(stream);
  DB hdr;
  rd.Read(hdr);

  if (hdr.id != hdr.ID) {
    throw es::InvalidHeaderError(hdr.id);
  }

  if (hdr.version != 0x200) {
    throw es::InvalidVersionError(hdr.version);
  }

  std::string buffer;
  rd.ReadContainer(buffer, rd.GetSize() - sizeof(DB));

  auto nodesBegin = reinterpret_cast<DBNode *>(buffer.data());
  auto nodesEnd = nodesBegin + hdr.numNodes;
  json doc;
  json docSchema;
  std::vector<es::string_view> parentNameStack;

  for (auto n = nodesBegin; n < nodesEnd; n++) {
    if (n->Parent() == n) {
      continue;
    }

    const DBNode *cn = n;

    while (cn->Parent() != cn) {
      parentNameStack.emplace_back(cn->Name());
      cn = cn->Parent();
    }

    const size_t numLevels = parentNameStack.size();
    auto cDoc = &doc;
    auto cDocSchema = &docSchema;

    for (size_t p = 0; p < numLevels; p++) {
      auto prevDoc = cDoc;
      auto prevDocSchema = cDocSchema;
      cDoc = &(*cDoc)[parentNameStack.back()];
      cDocSchema = &(*cDocSchema)[parentNameStack.back()];

      if (!cDoc->is_null() && !cDoc->is_object()) {
        cDoc = &(*prevDoc)["@" + parentNameStack.back().to_string()];
      }
      if (!cDocSchema->is_null() && !cDocSchema->is_object()) {
        cDocSchema =
            &(*prevDocSchema)["@" + parentNameStack.back().to_string()];
      }

      parentNameStack.pop_back();
    }

    parentNameStack.clear();

    if (n->numValues) {
      auto value = n->Data();

      auto GenNodePath = [&parentNameStack](const auto *cn) {
        while (cn->Parent() != cn) {
          parentNameStack.emplace_back(cn->Name());
          cn = cn->Parent();
        }

        std::string builtPath;
        const size_t numLevels = parentNameStack.size();

        if (!numLevels) {
          return std::string{};
        }

        for (size_t p = 0; p < numLevels; p++) {
          builtPath.push_back('/');
          builtPath.append(parentNameStack.back());
          parentNameStack.pop_back();
        }

        parentNameStack.clear();

        return builtPath;
      };

      auto InsertSimpleValue = [&](const DBValue *val) {
        auto name = val->Name();
        auto &jVal = (*cDoc)[name];
        switch (val->type) {
        case DBValueType::FLOAT:
          jVal = *val->Data<float>();
          break;
        case DBValueType::BOOL32:
          jVal = *val->Data<uint32>() == 1;
          break;
        case DBValueType::INT32:
          jVal = *val->Data<int32>();
          break;
        case DBValueType::UINT8:
          jVal = *val->Data<uint8>();
          break;
        case DBValueType::VECTOR2:
          jVal = *val->Data<std::array<float, 2>>();
          break;
        case DBValueType::VECTOR3:
          jVal = *val->Data<std::array<float, 3>>();
          break;
        case DBValueType::VECTOR4:
          jVal = *val->Data<std::array<float, 4>>();
          break;
        case DBValueType::REF: {
          auto data = GenNodePath(nodesBegin + *val->Data<uint16>());

          if (data.empty()) {
            break;
          }

          jVal = data;
          break;
        }

        case DBValueType::COLOR:
          jVal = *val->Data<std::array<uint8, 4>>();
          break;

        default:
          throw std::runtime_error("Invalid simple value type: " +
                                   std::to_string(uint8(val->type)));
        }
      };

      auto InsertArrayValues = [&](const DBValue *val) {
        auto &jVal = (*cDoc)[val->Name()];
        switch (val->type) {
        case DBValueType::FLOAT: {
          std::vector<float> data(val->Data<float>(),
                                  val->Data<float>() + (val->DataSize() / 4));
          jVal = data;
          break;
        }
        case DBValueType::INT32: {
          std::vector<int32> data(val->Data<int32>(),
                                  val->Data<int32>() + (val->DataSize() / 4));
          jVal = data;
          break;
        }
        case DBValueType::UINT8: {
          std::vector<uint8> data(val->Data<uint8>(),
                                  val->Data<uint8>() + val->DataSize());
          jVal = data;
          break;
        }
        case DBValueType::VECTOR2: {
          std::vector<std::array<float, 2>> data(
              val->Data<std::array<float, 2>>(),
              val->Data<std::array<float, 2>>() + (val->DataSize() / 8));
          jVal = data;
          break;
        }
        case DBValueType::VECTOR3: {
          std::vector<std::array<float, 3>> data(
              val->Data<std::array<float, 3>>(),
              val->Data<std::array<float, 3>>() + (val->DataSize() / 12));
          jVal = data;
          break;
        }
        case DBValueType::STRING:
          jVal = val->Data<char>();
          break;

        case DBValueType::REF: {
          const size_t numDatas = val->DataSize() / 2;
          auto nodes = val->Data<uint16>();
          std::vector<std::string> data;
          data.resize(numDatas);

          for (size_t d = 0; d < numDatas; d++, nodes++) {
            auto data_ = GenNodePath(nodesBegin + *nodes);

            if (data_.empty()) {
              continue;
            }

            data[d] = data_;
          }
          jVal = data;

          break;
        }

        default:
          throw std::runtime_error("Invalid array value type: " +
                                   std::to_string(uint8(val->type)));
        }
      };

      for (size_t v = 0; v < n->numValues; v++) {
        auto refl = GetReflectedEnum<DBValueType>();
        auto found = std::find(refl->values, refl->values + refl->numMembers,
                               uint64(value->type));
        auto name = refl->names[std::distance(refl->values, found)];

        switch (value->subType) {
        case DBValueSubType::Simple:
          InsertSimpleValue(value);
          (*cDocSchema)[value->Name()] = name;
          break;

        case DBValueSubType::CString:
          (*cDoc)[value->Name()] = value->Data<char>();
          (*cDocSchema)[value->Name()] = "CString";
          break;

        case DBValueSubType::Array: {
          InsertArrayValues(value);
          (*cDocSchema)[value->Name()] = {
              {"type", "Array"},
              {"subtype", name},
          };

          if (value->type == DBValueType::STRING) {
            (*cDocSchema)[value->Name()]["size"] = value->DataSize();
          }

          break;
        }

        default:
          throw std::runtime_error("Invalid value subtype: " +
                                   std::to_string(uint8(value->subType)));
        }

        value = value->Next();
      }
    }
  }

  AFileInfo finf(ctx->outFile);
  BinWritter_t<BinCoreOpenMode::Text> wr(finf.GetFullPathNoExt().to_string() +
                                       ".json");
  wr.BaseStream() << std::setw(4) << doc;

  BinWritter_t<BinCoreOpenMode::Text> wr2(finf.GetFullPathNoExt().to_string() +
                                        ".schema.json");
  wr2.BaseStream() << docSchema;
}
