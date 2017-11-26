set (source_dir src)

set (sources
    ${source_dir}/main.cpp)

add_executable (extense ${sources})
target_include_directories (extense PUBLIC
    ${HEADER_ONLY_DEPENDS} ${PROJECT_BINARY_DIR}/include include
    ${PROJECT_BINARY_DIR}/lib/extense/include
    ${PROJECT_BINARY_DIR}/lib/stdlib/include)
target_link_libraries (extense api stdlib)

install (TARGETS extense DESTINATION ${PACKAGE_DIR}/extense)
