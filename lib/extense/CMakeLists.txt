set (source_dir src)
set (detail_source_dir ${source_dir}/detail)

set (sources
    ${source_dir}/source.cpp
    ${source_dir}/token.cpp
    ${source_dir}/value.cpp
    ${source_dir}/ast.cpp
    ${source_dir}/parser.cpp
    ${detail_source_dir}/types.cpp
    ${detail_source_dir}/operations.cpp
    ${detail_source_dir}/proxy.cpp)

add_library (api SHARED ${sources})
target_include_directories (api PUBLIC
    ${HEADER_ONLY_DEPENDS} ${PROJECT_BINARY_DIR}/include include ${PROJECT_BINARY_DIR}/lib/extense/include)

if (ENABLE_TESTS)
  set (test_dir test)
  set (detail_test_dir ${test_dir}/detail)

  set (test_sources
    ${test_dir}/source.cpp
    ${test_dir}/token.cpp
    ${test_dir}/value.cpp
    ${test_dir}/ast.cpp
    ${test_dir}/parser.cpp
    ${detail_test_dir}/types.cpp
    ${detail_test_dir}/operations.cpp)

  add_executable (api_tests ${test_dir}/runner.cpp ${test_sources})
  target_include_directories (api_tests PUBLIC
    ${HEADER_ONLY_DEPENDS} ${PROJECT_BINARY_DIR}/include include
    ${PROJECT_BINARY_DIR}/lib/extense/include)
  target_link_libraries (api_tests PUBLIC api catch)

  add_test(NAME api_tests COMMAND api_tests)
endif ()

install (TARGETS api DESTINATION ${PACKAGE_DIR}/api)
