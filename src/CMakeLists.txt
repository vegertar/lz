cmake_minimum_required(VERSION 3.6)

add_library(lz INTERFACE)

target_include_directories(lz INTERFACE
                           $<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/include>
                           $<INSTALL_INTERFACE:include>)

target_link_libraries(lz INTERFACE optional-lite)

target_compile_features(lz INTERFACE ${CXX_FEATURES_USED})
target_compile_definitions(lz INTERFACE)
