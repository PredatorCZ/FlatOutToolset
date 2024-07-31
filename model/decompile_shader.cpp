/*  DecompileShader
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
#include "spike/io/binreader_stream.hpp"
#include "spike/io/binwritter.hpp"
#include "spike/io/fileinfo.hpp"
#include "spike/master_printer.hpp"
#include "spike/reflect/reflector.hpp"
#include <variant>
#include <vector>

static AppInfo_s appInfo{
    .multithreaded = false,
    .header = DecompileShader_DESC " v" DecompileShader_VERSION
                                   ", " DecompileShader_COPYRIGHT "Lukas Cone",
};

AppInfo_s *AppInitModule() { return &appInfo; }

struct VersionToken {
  static constexpr uint16 PS = 0xFFFF;
  static constexpr uint16 VS = 0xFFFE;
  uint8 minorVersion;
  uint8 majorVersion;
  uint16 shaderType;
};

struct CommentToken {
  static constexpr uint16 ID = 0xFFFE;
  uint16 id;
  uint16 numTokens;
};

// clang-format off
MAKE_ENUM(ENUMSCOPE(class OpCode : uint16, OpCode),
  EMEMBER(NOP),
  EMEMBER(MOV),
  EMEMBER(ADD),
  EMEMBER(SUB),
  EMEMBER(MAD),
  EMEMBER(MUL),
  EMEMBER(RCP),
  EMEMBER(RSQ),
  EMEMBER(DP3),
  EMEMBER(DP4),
  EMEMBER(MIN),
  EMEMBER(MAX),
  EMEMBER(SLT),
  EMEMBER(SGE),
  EMEMBER(EXP),
  EMEMBER(LOG),
  EMEMBER(LIT),
  EMEMBER(DST),
  EMEMBER(LRP),
  EMEMBER(FRC),
  EMEMBER(M4x4),
  EMEMBER(M4x3),
  EMEMBER(M3x4),
  EMEMBER(M3x3),
  EMEMBER(M3x2),
  EMEMBER(CALL),
  EMEMBER(CALLNZ),
  EMEMBER(LOOP),
  EMEMBER(RET),
  EMEMBER(ENDLOOP),
  EMEMBER(LABEL),
  EMEMBER(DCL),
  EMEMBER(POW),
  EMEMBER(CRS),
  EMEMBER(SGN),
  EMEMBER(ABS),
  EMEMBER(NRM),
  EMEMBER(SINCOS),
  EMEMBER(REP),
  EMEMBER(ENDREP),
  EMEMBER(IF),
  EMEMBER(IFC),
  EMEMBER(ELSE),
  EMEMBER(ENDIF),
  EMEMBER(BREAK),
  EMEMBER(BREAKC),
  EMEMBER(MOVA),
  EMEMBER(DEFB),
  EMEMBER(DEFI),
  EMEMBERVAL(TEXCOORD, 64),
  EMEMBER(TEXKILL),
  EMEMBER(TEX),
  EMEMBER(TEXBEM),
  EMEMBER(TEXBEML),
  EMEMBER(TEXREG2AR),
  EMEMBER(TEXREG2GB),
  EMEMBER(TEXM3x2PAD),
  EMEMBER(TEXM3x2TEX),
  EMEMBER(TEXM3x3PAD),
  EMEMBER(TEXM3x3TEX),
  EMEMBER(RESERVED0),
  EMEMBER(TEXM3x3SPEC),
  EMEMBER(TEXM3x3VSPEC),
  EMEMBER(EXPP),
  EMEMBER(LOGP),
  EMEMBER(CND),
  EMEMBER(DEF),
  EMEMBER(TEXREG2RGB),
  EMEMBER(TEXDP3TEX),
  EMEMBER(TEXM3x2DEPTH),
  EMEMBER(TEXDP3),
  EMEMBER(TEXM3x3),
  EMEMBER(TEXDEPTH),
  EMEMBER(CMP),
  EMEMBER(BEM),
  EMEMBER(DP2ADD),
  EMEMBER(DSX),
  EMEMBER(DSY),
  EMEMBER(TEXLDD),
  EMEMBER(SETP),
  EMEMBER(TEXLDL),
  EMEMBER(BREAKP),
  EMEMBERVAL(PHASE, 0xFFFD),
  EMEMBERVAL(COMMENT, 0xFFFE),
  EMEMBERVAL(END, 0xFFFF)
);

// clang-format on

static constexpr int8 opcodeLen[]{
    0,  // NOP
    2,  // MOV
    3,  // ADD
    3,  // SUB
    4,  // MAD
    3,  // MUL
    2,  // RCP
    2,  // RSQ
    3,  // DP3
    3,  // DP4
    3,  // MIN
    3,  // MAX
    3,  // SLT
    3,  // SGE
    2,  // EXP
    2,  // LOG
    2,  // LIT
    3,  // DST
    4,  // LRP
    2,  // FRC
    3,  // M4x4
    3,  // M4x3
    3,  // M3x4
    3,  // M3x3
    3,  // M3x2
    1,  // CALL
    2,  // CALLNZ
    1,  // LOOP
    0,  // RET
    0,  // ENDLOOP
    1,  // LABEL
    -1, // DCL
    3,  // POW
    3,  // CRS
    4,  // SGN
    2,  // ABS
    2,  // NRM
    -1, // SINCOS
    1,  // REP
    0,  // ENDREP
    1,  // IF
    2,  // IFC
    0,  // ELSE
    0,  // ENDIF
    0,  // BREAK
    2,  // BREAKC
    2,  // MOVA
    2,  // DEFB
    5,  // DEFI
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    2, // TEXCOORD
    1, // TEXKILL
    3, // TEX
    2, // TEXBEM
    2, // TEXBEML
    2, // TEXREG2AR
    2, // TEXREG2GB
    2, // TEXM3x2PAD
    2, // TEXM3x2TEX
    2, // TEXM3x3PAD
    2, // TEXM3x3TEX
    0, // RESERVED0
    3, // TEXM3x3SPEC
    2, // TEXM3x3VSPEC
    2, // EXPP
    2, // LOGP
    4, // CND
    5, // DEF
    2, // TEXREG2RGB
    2, // TEXDP3TEX
    2, // TEXM3x2DEPTH
    2, // TEXDP3
    2, // TEXM3x3
    1, // TEXDEPTH
    4, // CMP
    3, // BEM
    4, // DP2ADD
    2, // DSX
    2, // DSY
    5, // TEXLDD
    3, // SETP
    3, // TEXLDL
    1, // BREAKP
};

static_assert(opcodeLen[uint32(OpCode::TEXCOORD)] == 2);

union RegisterKey {
  uint32 raw_;
  struct {
    uint16 regType;
    uint16 regIndex;
  };

  constexpr bool operator<(RegisterKey o) const { return raw_ < o.raw_; }
};

struct ConstFloat {
  float v[4];
};

struct ConstInt {
  int v[4];
};

using ConstReg = std::variant<ConstFloat, ConstInt, bool>;
using ConstRegs = std::map<RegisterKey, ConstReg>;

struct InstructionToken {
  OpCode opCode;
  uint8 controlBits;
  uint8 instrSize : 4;
  bool predicated : 1;
  bool reserved : 1;
  bool execWithPrev : 1;
  bool reserved0 : 1;
};

static_assert(sizeof(InstructionToken) == 4);

MAKE_ENUM(ENUMSCOPE(class DCLUsage
                    : uint16, DCLUsage),
          EMEMBER(Position), EMEMBER(BlendWeight), EMEMBER(BlendIndices),
          EMEMBER(Normal), EMEMBER(PSize), EMEMBER(TexCoord), EMEMBER(Tangent),
          EMEMBER(BiNormal), EMEMBER(TessFactor), EMEMBER(PositionT),
          EMEMBER(Color), EMEMBER(Fog), EMEMBER(Depth), EMEMBER(Sample));

static const char *const samplerType[]{
    "", "", "dcl_2d", "dcl_cube", "dcl_volume",
};

struct DCL_3 {
  DCLUsage usage;
  uint16 index : 4;
  uint16 reserved : 7;
  uint16 samplerType : 4;
  uint16 flag : 1;
};

static_assert(sizeof(DCL_3) == 4);

static const char *const registerType[]{
    /**/ //
    "tmp",
    "in",
    "const0",
    "texture",
    "rasterizer",
    "attrib",
    "out",
    "constInt",
    "colorOut",
    "depthOut",
    "sampler",
    "const1",
    "const2",
    "const3",
    "constBool",
    "loop",
    "tmpFloat16",
    "misc",
    "label",
    "predicate",
};

struct DestinationToken {
  uint16 registerIndex : 11;
  uint16 registerTypeEnd : 2;
  uint16 relativeAddress : 1;
  uint16 reserved0 : 2;
  uint16 writeMask : 4;
  uint16 modifier : 4;
  uint16 reserved1 : 4;
  uint16 registerType : 3;
  uint16 flag : 1;

  uint16 RegisterType() const { return registerType | (registerTypeEnd << 3); }
};

std::ostream &operator<<(std::ostream &str, const DestinationToken &token) {
  if (!token.flag) {
    throw std::runtime_error("Invalid token");
  }

  if (token.relativeAddress) {
    throw std::runtime_error("Relative address not supported");
  }

  auto Mask = [](bool cond, auto wat) {
    if (cond & 1) {
      return wat;
    }

    return "";
  };

  auto Modifier = [](bool cond, auto wat) {
    if (cond & 1) {
      return wat;
    }

    return "";
  };

  return str << Modifier(token.modifier & 1, "saturate(")
             << Modifier(token.modifier & 2, "partialPrecision(")
             << Modifier(token.modifier & 4, "centroid(")
             << registerType[token.RegisterType()] << token.registerIndex << "."
             << Mask(token.writeMask & 1, "x") << Mask(token.writeMask & 2, "y")
             << Mask(token.writeMask & 4, "z") << Mask(token.writeMask & 8, "w")
             << Modifier(token.modifier & 1, ")")
             << Modifier(token.modifier & 2, ")")
             << Modifier(token.modifier & 4, ")");
}

static_assert(sizeof(DestinationToken) == 4);

struct SourceToken {
  uint16 registerIndex : 11;
  uint16 registerTypeEnd : 2;
  uint16 relativeAddress : 1;
  uint16 reserved0 : 2;
  uint16 swizzleX : 2;
  uint16 swizzleY : 2;
  uint16 swizzleZ : 2;
  uint16 swizzleW : 2;
  uint16 modifier : 4;
  uint16 registerType : 3;
  uint16 flag : 1;

  uint16 RegisterType() const { return registerType | (registerTypeEnd << 3); }
};

static const char *const modifier[]{
    /**/ //
    "",
    "-",
    "bias(",
    "-bias(",
    "signExtend(",
    "-signExtend(",
    "complement(",
    "x2(",
    "-x2(",
    "dz(",
    "dw(",
    "abs(",
    "-abs(",
    "!",
};

std::ostream &ToStr(std::ostream &str, const SourceToken &token,
                    const DestinationToken &dest, const ConstRegs &regs) {
  if (!token.flag) {
    throw std::runtime_error("Invalid token");
  }

  if (token.relativeAddress) {
    throw std::runtime_error("Relative address not supported");
  }

  auto Swizzle = [](uint16 cond) { return "x\0y\0z\0w" + cond * 2; };

  RegisterKey key;
  key.regIndex = token.registerIndex;
  key.regType = token.RegisterType();

  str << modifier[token.modifier];

  if (regs.contains(key)) {
    std::visit(
        [&](auto &cType) {
          if constexpr (std::is_same_v<std::decay_t<decltype(cType)>, bool>) {
            str << (cType ? "true" : "false");
          } else {
            uint32 numMasks =
                bool(dest.writeMask & 1) + bool(dest.writeMask & 2) +
                bool(dest.writeMask & 4) + bool(dest.writeMask & 8);
            if (numMasks == 1) {
              str << "float(";
            } else {
              str << "vec" << numMasks << "(";
            }

            if (dest.writeMask & 1) {
              str << cType.v[token.swizzleX] << ", ";
            }
            if (dest.writeMask & 2) {
              str << cType.v[token.swizzleY] << ", ";
            }
            if (dest.writeMask & 4) {
              str << cType.v[token.swizzleZ] << ", ";
            }
            if (dest.writeMask & 8) {
              str << cType.v[token.swizzleW] << ", ";
            }
            str.seekp(str.tellp() - 2);
            str << ")";
          }
        },
        regs.at(key));
  } else {
    str << registerType[token.RegisterType()] << token.registerIndex << ".";
    if (dest.writeMask & 1) {
      str << Swizzle(token.swizzleX);
    }
    if (dest.writeMask & 2) {
      str << Swizzle(token.swizzleY);
    }
    if (dest.writeMask & 4) {
      str << Swizzle(token.swizzleZ);
    }
    if (dest.writeMask & 8) {
      str << Swizzle(token.swizzleW);
    }
  }

  if (std::string_view(modifier[token.modifier]).ends_with('(')) {
    str << ")";
  }

  return str;
}

static_assert(sizeof(SourceToken) == 4);

static const char *const controls[]{"",    "_GT", "_EQ", "_GE",
                                    "_LT", "_NE", "_LE"};

void AppProcessFile(AppContext *ctx) {
  BinReaderRef rd(ctx->GetStream());

  VersionToken version;
  rd.Read(version);

  if (version.shaderType < VersionToken::VS) {
    throw std::runtime_error("Not a shader data");
  }

  ConstRegs constRegs;
  DestinationToken dest;

  auto SaveDest = [&] {
    rd.Read(dest);
    es::print::Get() << dest << " = ";
  };

  auto SaveSource = [&] {
    SourceToken source;
    rd.Read(source);
    ToStr(es::print::Get(), source, dest, constRegs);
  };

  auto SaveOp = [&](auto opName) {
    SaveDest();
    SaveSource();
    es::print::Get() << opName;
    SaveSource();
    printline("");
  };

  while (!rd.IsEOF()) {
    InstructionToken op;
    rd.Read(op);

    auto OpName = [&op] {
      auto refl = GetReflectedEnum<OpCode>();
      for (size_t v = 0; v < refl->numMembers; v++) {
        if (refl->values[v] == uint64(op.opCode)) {
          return refl->names[v];
        }
      }

      throw std::runtime_error("Unsupported opcode: " +
                               std::to_string(uint64(op.opCode)));
    };

    switch (op.opCode) {
    case OpCode::COMMENT: {
      CommentToken comment;
      memcpy(&comment, &op, 4);
      rd.Skip(comment.numTokens * 4);
      break;
    }

    case OpCode::SINCOS:
      throw std::runtime_error("Unsupported operation");

    case OpCode::PHASE:
      throw std::runtime_error("Unsupported operation for ps_1_4");

    case OpCode::END:
      return;

    case OpCode::LOOP:
    case OpCode::LABEL:
    case OpCode::REP:
    case OpCode::IF:
    case OpCode::IFC:
    case OpCode::BREAKC: {
      es::print::Get(es::print::MPType::MSG)
          << OpName() << controls[op.controlBits];

      for (size_t i = 0; i < opcodeLen[uint64(op.opCode)]; i++) {
        SourceToken source;
        rd.Read(source);
        es::print::Get() << " ";
        ToStr(es::print::Get(), source, dest, constRegs);
      }

      es::print::Get() << "\n";
      es::print::FlushAll();
      break;
    }

    case OpCode::DEF: {
      DestinationToken dest;
      rd.Read(dest);
      ConstFloat val;
      rd.Read(val);
      RegisterKey key;
      key.regIndex = dest.registerIndex;
      key.regType = dest.RegisterType();
      constRegs.emplace(key, val);
      break;
    }

    case OpCode::DEFB: {
      DestinationToken dest;
      rd.Read(dest);
      uint32 boolVal;
      rd.Read(boolVal);
      RegisterKey key;
      key.regIndex = dest.registerIndex;
      key.regType = dest.RegisterType();
      constRegs.emplace(key, bool(boolVal));
      break;
    }

    case OpCode::DEFI: {
      DestinationToken dest;
      rd.Read(dest);
      ConstInt val;
      rd.Read(val);
      RegisterKey key;
      key.regIndex = dest.registerIndex;
      key.regType = dest.RegisterType();
      constRegs.emplace(key, val);
      break;
    }

    case OpCode::DCL: {
      DCL_3 def;
      rd.Read(def);
      DestinationToken dest;
      rd.Read(dest);

      if (dest.RegisterType() == 10) {
        printline(samplerType[def.samplerType] << " " << dest);
      } else {
        printline("dcl_" <<
                  [&def] {
                    auto refl = GetReflectedEnum<DCLUsage>();
                    for (size_t v = 0; v < refl->numMembers; v++) {
                      if (refl->values[v] == uint64(def.usage)) {
                        return refl->names[v];
                      }
                    }

                    throw std::runtime_error("Unsupported opcode: " +
                                             std::to_string(uint64(def.usage)));
                  }() << std::to_string(def.index)
                         << " " << dest);
      }

      break;
    }

    case OpCode::ABS:
      SaveDest();
      es::print::Get() << "abs(";
      SaveSource();
      printline(")");
      break;

    case OpCode::FRC:
      SaveDest();
      es::print::Get() << "fract(";
      SaveSource();
      printline(")");
      break;

    case OpCode::LOG:
      SaveDest();
      es::print::Get() << "log2(";
      SaveSource();
      printline(")");
      break;

    case OpCode::RCP:
      SaveDest();
      es::print::Get() << "reciprocal(";
      SaveSource();
      printline(")");
      break;

    case OpCode::RSQ:
      SaveDest();
      es::print::Get() << "reciprocalSqrt(";
      SaveSource();
      printline(")");
      break;

    case OpCode::EXP:
      SaveDest();
      es::print::Get() << "exp2(";
      SaveSource();
      printline(")");
      break;

    case OpCode::NRM:
      SaveDest();
      es::print::Get() << "normalize(";
      SaveSource();
      printline(")");
      break;

    case OpCode::DST:
      SaveDest();
      es::print::Get() << "distance(";
      SaveSource();
      es::print::Get() << ", ";
      SaveSource();
      printline(")");
      break;

    case OpCode::CRS:
      SaveDest();
      es::print::Get() << "cross(";
      SaveSource();
      es::print::Get() << ", ";
      SaveSource();
      printline(")");
      break;

    case OpCode::LRP: {
      SaveDest();
      SourceToken source;
      rd.Read(source);
      es::print::Get() << "mix(";
      SaveSource();
      es::print::Get() << ", ";
      SaveSource();
      es::print::Get() << ", ";
      ToStr(es::print::Get(), source, dest, constRegs);
      printline(")");
      break;
    }

    case OpCode::ADD:
      SaveOp(" + ");
      break;

    case OpCode::MUL:
      SaveOp(" * ");
      break;

    case OpCode::SUB:
      SaveOp(" - ");
      break;

    case OpCode::POW:
      SaveOp(" ^ ");
      break;

    case OpCode::MOV:
      SaveDest();
      SaveSource();
      printline("");
      break;

    case OpCode::MAD:
      SaveDest();
      SaveSource();
      es::print::Get() << " * ";
      SaveSource();
      es::print::Get() << " + ";
      SaveSource();
      printline("");
      break;

    case OpCode::DP3:
      SaveDest();
      dest.writeMask = 7;
      es::print::Get() << "dot3(";
      SaveSource();
      es::print::Get() << ", ";
      SaveSource();
      printline(")");
      break;

    case OpCode::DP4:
      SaveDest();
      dest.writeMask = 0xf;
      es::print::Get() << "dot4(";
      SaveSource();
      es::print::Get() << ", ";
      SaveSource();
      printline(")");
      break;

    case OpCode::MIN:
      SaveDest();
      es::print::Get() << "min(";
      SaveSource();
      es::print::Get() << ", ";
      SaveSource();
      printline(")");
      break;

    case OpCode::MAX:
      SaveDest();
      es::print::Get() << "max(";
      SaveSource();
      es::print::Get() << ", ";
      SaveSource();
      printline(")");
      break;

    case OpCode::SLT: {
      SaveDest();
      SaveSource();
      es::print::Get() << " < ";
      SaveSource();
      uint32 numMasks = bool(dest.writeMask & 1) + bool(dest.writeMask & 2) +
                        bool(dest.writeMask & 4) + bool(dest.writeMask & 8);
      if (numMasks == 1) {
        es::print::Get() << " ? float(1) : ";
      } else {
        es::print::Get() << " ? vec" << numMasks << "(1.f) : ";
      }

      if (numMasks == 1) {
        printline("float(0)");
      } else {
        printline("vec" << numMasks << "(0)");
      }
      break;
    }

    default:
      es::print::Get(es::print::MPType::MSG) << OpName();
      const int8 opLen = opcodeLen[uint64(op.opCode)];

      if (opLen > 0) {
        DestinationToken dest;
        rd.Read(dest);
        es::print::Get() << " " << dest;

        for (int8 i = 1; i < opLen; i++) {
          SourceToken source;
          rd.Read(source);
          es::print::Get() << " ";
          ToStr(es::print::Get(), source, dest, constRegs);
        }
      } else if (opLen < 0) {
        throw std::runtime_error("Unhandled operation");
      }

      es::print::Get() << "\n";
      es::print::FlushAll();

      break;
    }
  }
}
