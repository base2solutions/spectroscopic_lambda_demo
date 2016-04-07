cmake_minimum_required (VERSION 2.8.11)
project (generate_spectroscopic_data C CXX)

SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -std=c++14")

include_directories("${PROJECT_BINARY_DATA}/../include")

add_executable (../../generate_spectroscopic_data main.cpp)
