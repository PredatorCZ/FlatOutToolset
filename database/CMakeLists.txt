cmake_minimum_required(VERSION 3.12)

project(DB2JSON VERSION 1.0)

build_target(
  NAME
  db_to_json
  TYPE
  ESMODULE
  LINKS
  spike-interface
  INCLUDES
  ${TPD_PATH}/spike/3rd_party/json
  SOURCES
  db_to_json.cpp
  AUTHOR
  "Lukas Cone"
  DESCR
  "Flatout database to json converter"
  START_YEAR
  2022)
