set (source_dir src)
set (sources
    ${source_dir}/dud.cpp)

add_library (stdlib SHARED ${sources})
target_include_directories (stdlib PUBLIC
    ${HEADER_ONLY_DEPENDS} ${PROJECT_BINARY_DIR}/include include ${PROJECT_BINARY_DIR}/lib/extense/include)
target_link_libraries (stdlib PUBLIC api)

if (ENABLE_TESTS)
  set (test_dir test)
  set (test_sources)

  add_executable (stdlib_tests ${test_dir}/runner.cpp ${test_sources})
  target_include_directories (stdlib_tests PUBLIC
    ${HEADER_ONLY_DEPENDS} ${PROJECT_BINARY_DIR}/include include
    ${PROJECT_BINARY_DIR}/lib/extense/include)
  target_link_libraries (stdlib_tests PUBLIC stdlib catch)

  add_test(NAME stdlib_tests COMMAND stdlib_tests)
endif ()

install (TARGETS stdlib DESTINATION ${PACKAGE_DIR}/stdlib)
