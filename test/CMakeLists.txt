cmake_minimum_required (VERSION 3.6)

set(CMAKE_CXX_STANDARD 14)
set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} -Wextra -Wall")

add_executable(components components.cc)
target_link_libraries(components
    PRIVATE lz
    PRIVATE Catch2::Catch2
)

add_custom_command(TARGET components POST_BUILD 
    COMMAND "${CMAKE_BINARY_DIR}/test/components"
    WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}")

include(CTest)