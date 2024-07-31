cmake_minimum_required(VERSION 3.12)

set(CMAKE_CXX_STANDARD 23)
project(FlatOutToolset)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Wextra -Wno-ignored-qualifiers")

option(CLI "" ON)
option(GLTF "" ON)
set(EXPOSE_SYMBOLS spike;pugixml;gltf)

set(TPD_PATH ${CMAKE_CURRENT_SOURCE_DIR}/3rd_party)

add_subdirectory(${TPD_PATH}/spike)
include(version)
#toolset_version()

set_target_properties(spike_cli PROPERTIES OUTPUT_NAME flatout_toolset)

enable_language(ASM)
file(GLOB_RECURSE ZSTD_SOURCE_FILES "${TPD_PATH}/zstd/lib/*.c")

add_library(bfcache OBJECT src/bfcache.cpp ${ZSTD_SOURCE_FILES} ${TPD_PATH}/zstd/lib/decompress/huf_decompress_amd64.S)
target_link_libraries(bfcache spike-interface)
target_include_directories(bfcache PUBLIC include PRIVATE ${TPD_PATH}/zstd/lib/common ${TPD_PATH}/zstd/lib)
set_target_properties(bfcache PROPERTIES POSITION_INDEPENDENT_CODE TRUE)

add_spike_subdir(archive)
add_spike_subdir(model)
add_spike_subdir(cache)
add_spike_subdir(database)
add_spike_subdir(plants)
add_spike_subdir(track)
add_subdirectory(launcher)
