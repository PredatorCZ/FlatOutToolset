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

install(TARGETS bgm_to_gltf DESTINATION bin)
