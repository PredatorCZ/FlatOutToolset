cmake_minimum_required(VERSION 3.12)

project(CDB2GLTF)

build_target(
  NAME
  cdb_to_gltf
  TYPE
  ESMODULE
  VERSION
  1
  LINKS
  spike-interface
  gltf-interface
  INCLUDES
  SOURCES
  cdb_to_gltf.cpp
  AUTHOR
  "Lukas Cone"
  DESCR
  "Flatout track_cdb to GLTF converter"
  START_YEAR
  2023)
