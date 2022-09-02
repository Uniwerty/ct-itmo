cmake_minimum_required(VERSION 3.21)
project(Homework2 C)

set(CMAKE_C_STANDARD 11)

add_executable(Homework2 main.c)
target_link_libraries(Homework2 ${CMAKE_SOURCE_DIR}/cmake-build-debug/libdeflate.dll)
target_link_libraries(Homework2 ${CMAKE_SOURCE_DIR}/cmake-build-debug/zlib1.dll)
target_link_libraries(Homework2 ${CMAKE_SOURCE_DIR}/cmake-build-debug/isa-l.dll)
