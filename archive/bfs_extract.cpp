/*  BFSExtract
    Copyright(C) 2022-2023 Lukas Cone

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

#include "bfs.hpp"
#include "datas/app_context.hpp"
#include "datas/binreader_stream.hpp"
#include "datas/except.hpp"
#include "datas/master_printer.hpp"
#include "project.h"
#include "zlib.h"
#include <vector>

std::string_view filters[]{
    ".bfs$",
    ".BFS$",
};

static AppInfo_s appInfo{
    .filteredLoad = true,
    .header = BFSExtract_DESC " v" BFSExtract_VERSION ", " BFSExtract_COPYRIGHT
                              "Lukas Cone",
    .filters = filters,
};

AppInfo_s *AppInitModule() { return &appInfo; }

void ExtractAsFO(BinReaderRef rd, size_t numFiles, AppContext *ctx) {
  rd.Skip(numFiles * 4);
  uint32 numHashIndices;
  rd.Read(numHashIndices);
  rd.Skip(numHashIndices * 4);
  std::string fileName;
  std::string inBuffer;
  std::string outBuffer;
  auto ectx = ctx->ExtractContext();

  for (size_t f = 0; f < numFiles; f++) {
    FileFO1Base cFile;
    rd.Read(cFile);
    rd.ReadContainer<uint16>(fileName);

    if (cFile.numDupes) {
      rd.Skip(cFile.numDupes * 4);
    }

    ectx->NewFile(fileName);
    rd.Push();
    rd.Seek(cFile.dataOffset);
    rd.ReadContainer(inBuffer, cFile.compressedSize);
    outBuffer.resize(cFile.uncompressedSize);

    if (cFile.Compressed()) {
      z_stream infstream;
      infstream.zalloc = Z_NULL;
      infstream.zfree = Z_NULL;
      infstream.opaque = Z_NULL;
      infstream.avail_in = inBuffer.size();
      infstream.next_in = reinterpret_cast<Bytef *>(&inBuffer[0]);
      infstream.avail_out = outBuffer.size();
      infstream.next_out = reinterpret_cast<Bytef *>(&outBuffer[0]);
      inflateInit(&infstream);
      int state = inflate(&infstream, Z_FINISH);
      inflateEnd(&infstream);

      if (state < 0) {
        if (infstream.msg) {
          throw std::runtime_error(infstream.msg);
        } else {
          throw std::runtime_error("zlib error: " + std::to_string(state));
        }
      }

      ectx->SendData(outBuffer);
    } else {
      ectx->SendData(inBuffer);
    }

    rd.Pop();
  }
}

struct HuffmanTreeBufferIter {
  std::vector<uint16> *main;
  size_t pos = 0;

  char Get(bool rightWay) {
    if (rightWay) {
      pos++;
    } else {
      pos = main->at(pos);
    }

    auto item = main->at(pos);

    if (item & 0x8000) {
      return item;
    }

    return 0;
  }
};

void AppProcessFile(AppContext *ctx) {
  BinReaderRef rd(ctx->GetStream());
  Header hdr;
  rd.Read(hdr);

  if (hdr.id != hdr.ID) {
    throw es::InvalidHeaderError(hdr.id);
  }

  if (hdr.signature && hdr.signature != hdr.SIGNATURE &&
      hdr.signature != hdr.FOUCSIG) {
    throw std::runtime_error("BFS archive has invalid signature " +
                             std::to_string(hdr.signature));
  }

  if (hdr.numHashIndices != 997) {
    // Consider as FO1 format
    rd.Seek(sizeof(Header) - 4);
    ExtractAsFO(rd, hdr.numFiles, ctx);
    return;
  }

  rd.Skip(hdr.numHashIndices * sizeof(HashIndex));

  std::vector<std::string> strings;

  StringsHeader strHdr;
  rd.Read(strHdr);
  const size_t numStrings =
      (strHdr.stringSizes - strHdr.huffmanDataOffsets) / 4;
  strings.resize(numStrings);

  {
    std::vector<uint32> huffOffs;
    std::vector<uint16> strLens;
    std::vector<uint16> huffmanBuffer;

    rd.ReadContainer(huffOffs, numStrings);
    rd.ReadContainer(strLens, numStrings);
    rd.ReadContainer(huffmanBuffer,
                     (strHdr.huffmanData - strHdr.huffmanTree) >> 1);

    std::string huffData;
    rd.ReadContainer(huffData, strHdr.size - strHdr.huffmanData);

    for (size_t i = 0; i < numStrings; i++) {
      auto &out = strings.at(i);
      out.resize(strLens.at(i));
      auto *dataBegin = huffData.data() + huffOffs.at(i);
      size_t bit = 0;

      for (size_t t = 0; t < out.size(); t++) {
        HuffmanTreeBufferIter iter;
        iter.main = &huffmanBuffer;

        while (true) {
          const size_t mod = bit % 8;
          const size_t at = bit++ / 8;
          bool right = (dataBegin[at] >> mod) & 1;
          auto huffChar = iter.Get(right);

          if (huffChar) {
            out[t] = huffChar;
            break;
          }
        }
      }
    }
  }

  std::string inBuffer;
  std::string outBuffer;
  auto ectx = ctx->ExtractContext();

  if (ectx->RequiresFolders()) {
    rd.Push();

    for (size_t f = 0; f < hdr.numFiles; f++) {
      BFile cFile;
      rd.Read(cFile);
      rd.Skip((cFile.fo2.numDupes | cFile.fouc.numDupes) * 4);
      ectx->AddFolderPath(strings.at(cFile.folderId));
    }

    ectx->GenerateFolders();
    rd.Pop();
  }

  for (size_t f = 0; f < hdr.numFiles; f++) {
    BFile cFile;
    rd.Read(cFile);

    // Following are offsets to duplicate data streams
    // Data streams are identical
    // Purpose is unknown
    // Possible usage is backup data in case of media corruption
    uint32 numDupes = cFile.fo2.numDupes | cFile.fouc.numDupes;
    rd.Skip(numDupes * 4);
    auto fileName = strings.at(cFile.folderId) + "/" + strings.at(cFile.fileId);
    ectx->NewFile(fileName);
    rd.Push();
    rd.Seek(cFile.dataOffset);
    rd.ReadContainer(inBuffer, cFile.compressedSize);
    outBuffer.resize(cFile.uncompressedSize);

    if (cFile.Compressed()) {
      z_stream infstream;
      infstream.zalloc = Z_NULL;
      infstream.zfree = Z_NULL;
      infstream.opaque = Z_NULL;
      infstream.avail_in = inBuffer.size();
      infstream.next_in = reinterpret_cast<Bytef *>(&inBuffer[0]);
      infstream.avail_out = outBuffer.size();
      infstream.next_out = reinterpret_cast<Bytef *>(&outBuffer[0]);
      inflateInit(&infstream);
      int state = inflate(&infstream, Z_FINISH);
      inflateEnd(&infstream);

      if (state < 0) {
        if (infstream.msg) {
          throw std::runtime_error(infstream.msg);
        } else {
          throw std::runtime_error("zlib error: " + std::to_string(state));
        }
      }
      ectx->SendData(outBuffer);
    } else {
      ectx->SendData(inBuffer);
    }

    rd.Pop();
  }
}

size_t AppExtractStat(request_chunk requester) {
  auto data = requester(0, sizeof(Header));
  auto hdr = reinterpret_cast<const Header *>(data.data());

  if (hdr->id != hdr->ID) {
    return 0;
  } else {
    return hdr->numFiles;
  }
}
