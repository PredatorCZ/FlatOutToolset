cmake_minimum_required(VERSION 3.12)

project(Plants2GLTF VERSION 1.0)

build_target(
  NAME
  plants_to_gltf
  TYPE
  ESMODULE
  LINKS
  precore
  gltf
  INCLUDES
  SOURCES
  plants_to_gltf.cpp
  AUTHOR
  "Lukas Cone"
  DESCR
  "Flatout plant_geom to GLTF converter"
  START_YEAR
  2022)

install(TARGETS plants_to_gltf DESTINATION bin)
