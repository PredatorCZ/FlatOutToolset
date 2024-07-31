cmake_minimum_required(VERSION 3.12)

project(BGM2GLTF)

build_target(
  NAME
  bgm_to_gltf
  TYPE
  ESMODULE
  VERSION
  1
  LINKS
  spike-interface
  gltf-interface
  INCLUDES
  SOURCES
  bgm_to_gltf.cpp
  AUTHOR
  "Lukas Cone"
  DESCR
  "Flatout bgm to GLTF converter"
  START_YEAR
  2022)

project(Ridge2GLTF)

build_target(
  NAME
  ridge_to_gltf
  TYPE
  ESMODULE
  VERSION
  1
  LINKS
  spike-interface
  gltf-interface
  INCLUDES
  SOURCES
  ridge_to_gltf.cpp
  AUTHOR
  "Lukas Cone"
  DESCR
  "Ridge Racer vhcl/scne to GLTF"
  START_YEAR
  2023)

project(DecompileShader)

build_target(
  NAME
  decompile_shader
  TYPE
  ESMODULE
  VERSION
  1
  LINKS
  spike-interface
  INCLUDES
  SOURCES
  decompile_shader.cpp
  AUTHOR
  "Lukas Cone"
  DESCR
  "Decompile FXO shaders"
  START_YEAR
  2023)

add_executable(normals normals.cpp)
target_link_libraries(normals spike-objects pugixml-objects)
