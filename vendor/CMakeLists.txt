if(NOT TARGET optional-lite)
    add_subdirectory(optional-lite)
endif()

if(NOT TARGET Catch2)
    add_subdirectory(Catch2)
    list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}/Catch2/contrib")
    include(Catch)
endif()