cmake_minimum_required(VERSION 3.12)

project(zlib-objs)
file(GLOB ZLIB_SOURCES "${TPD_PATH}/zlib/*.c")
add_library(zlib_objs OBJECT ${ZLIB_SOURCES})
target_include_directories(zlib_objs PUBLIC ${TPD_PATH}/zlib)
set_target_properties(zlib_objs PROPERTIES POSITION_INDEPENDENT_CODE ON)

project(BFSExtract VERSION 1.0)

build_target(
  NAME
  bfs_extract
  TYPE
  ESMODULE
  LINKS
  precore
  zlib_objs
  SOURCES
  bfs_extract.cpp
  AUTHOR
  "Lukas Cone"
  DESCR
  "Flatout BFS extractor"
  START_YEAR
  2022)

install(TARGETS bfs_extract DESTINATION bin)

project(BFSPack VERSION 1.0)

build_target(
  NAME
  bfs_pack
  TYPE
  ESMODULE
  LINKS
  precore
  zlib_objs
  SOURCES
  bfs_pack.cpp
  AUTHOR
  "Lukas Cone"
  DESCR
  "Flatout BFS packer"
  START_YEAR
  2022)

install(TARGETS bfs_pack DESTINATION bin)
