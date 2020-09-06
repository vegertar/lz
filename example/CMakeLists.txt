
cmake_minimum_required (VERSION 3.6)
set(CMAKE_CXX_STANDARD 14)
set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} -Wextra -Wall")

add_executable (example example.cc)
target_link_libraries (example PRIVATE lz)
