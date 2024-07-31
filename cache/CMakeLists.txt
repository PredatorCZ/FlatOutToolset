cmake_minimum_required(VERSION 3.3)

project(FlatoutCache)

build_target(
  NAME
  make_cache
  TYPE
  ESMODULE
  SOURCES
  make_cache.cpp
  VERSION
  1
  LINKS
  bfcache
  AUTHOR
  "Lukas Cone"
  DESCR
  "Create Flatout mod cache"
  START_YEAR
  2023)
