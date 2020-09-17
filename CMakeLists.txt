cmake_minimum_required(VERSION 3.6)

project(lz
        VERSION 0.3.0
        LANGUAGES CXX)

add_subdirectory(vendor)
add_subdirectory(src)

if(CMAKE_SOURCE_DIR STREQUAL PROJECT_SOURCE_DIR)
    add_subdirectory(example)
    add_subdirectory(test)
endif()
