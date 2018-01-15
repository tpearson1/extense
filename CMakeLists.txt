cmake_minimum_required (VERSION 3.9)
project (extense VERSION 0.0.1)

# Support for clang tooling
set (CMAKE_EXPORT_COMPILE_COMMANDS ON)

# Variables
set (HEADER_ONLY_DEPENDS depend)
option (ENABLE_TESTS ON)

# Version
set (EXTENSE_VERSION_MAJOR 0)
set (EXTENSE_VERSION_MINOR 0)
set (EXTENSE_VERSION_PATCH 1)

# Default compiler flags
set (CMAKE_CXX_FLAGS
  "${CMAKE_CXX_FLAGS} -std=c++1z -Wall -Werror -pedantic -g -Og")

# Add Catch2 single header
if (ENABLE_TESTS)
  enable_testing()

  if (NOT EXISTS ${HEADER_ONLY_DEPENDS}/catch.hpp)
    file (DOWNLOAD https://raw.githubusercontent.com/catchorg/Catch2/master/single_include/catch.hpp
      ${HEADER_ONLY_DEPENDS}/catch.hpp)
  endif ()

  add_library(catch INTERFACE)
  target_include_directories(catch INTERFACE ${HEADER_ONLY_DEPENDS})
endif ()

# Configure config.h
configure_file (
  ${PROJECT_SOURCE_DIR}/include/config.h.in
  ${PROJECT_BINARY_DIR}/include/config.h
)


# Packaging
set (PACKAGE_DIR package)
set (CPACK_PACKAGE_NAME "Extense")
set (CPACK_PACKAGE_VERSION ${PROJECT_VERSION})
set (CPACK_MONOLITHIC_INSTALL 1)

# Subdirectories
add_subdirectory (extense)
add_subdirectory (lib/extense)
add_subdirectory (lib/stdlib)

include (CPack)
