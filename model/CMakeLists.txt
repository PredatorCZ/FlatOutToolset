cmake_minimum_required(VERSION 3.12)

project(BGM2GLTF VERSION 1.0)

build_target(
  NAME
  bgm_to_gltf
  TYPE
  ESMODULE
  LINKS
  precore
  gltf-shared
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
