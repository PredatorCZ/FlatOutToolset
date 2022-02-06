cmake_minimum_required(VERSION 3.12)

project(DB2JSON VERSION 1.0)

build_target(
  NAME
  db_to_json
  TYPE
  ESMODULE
  LINKS
  precore
  INCLUDES
  ${PRECORE_SOURCE_DIR}/3rd_party/json
  SOURCES
  db_to_json.cpp
  AUTHOR
  "Lukas Cone"
  DESCR
  "Flatout database to json converter"
  START_YEAR
  2022)

install(TARGETS bfs_extract DESTINATION bin)

# project(JSON2DB VERSION 1.0)

# build_target(
#   NAME
#   bfs_pack
#   TYPE
#   ESMODULE
#   LINKS
#   precore
#   zlib_objs
#   SOURCES
#   bfs_pack.cpp
#   AUTHOR
#   "Lukas Cone"
#   DESCR
#   "Flatout BFS packer"
#   START_YEAR
#   2022)
# 
# install(TARGETS bfs_pack DESTINATION bin)
