cmake_minimum_required(VERSION 3.18)
project(hepcat_reduce LANGUAGES C CXX)
set(CMAKE_CXX_EXTENSIONS OFF)

find_package(Threads REQUIRED)
find_path(GMP_INCLUDE_DIR gmpxx.h REQUIRED)
find_library(GMPXX_LIBRARY gmpxx REQUIRED)
find_library(GMP_LIBRARY gmp REQUIRED)

add_library(hepcat_reduce
    amplitude.cpp completion.cpp continuations.cpp null_shift.cpp residue.cpp
    high_energy.cpp high_energy_chart.cpp high_energy_contacts.cpp
    collect.cpp algebra.cpp amp_expression.cpp kinematics.cpp spinor.cpp vector_basis.cpp)
target_compile_features(hepcat_reduce PUBLIC cxx_std_17)
target_include_directories(hepcat_reduce PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
target_include_directories(hepcat_reduce SYSTEM PRIVATE ${GMP_INCLUDE_DIR})
target_link_libraries(hepcat_reduce PRIVATE ${GMPXX_LIBRARY} ${GMP_LIBRARY} Threads::Threads)
add_executable(hepcat-reduce benchmark.cpp)
target_include_directories(hepcat-reduce SYSTEM PRIVATE ${GMP_INCLUDE_DIR})
target_link_libraries(hepcat-reduce PRIVATE hepcat_reduce)

add_executable(hepcat-reduce-amplitude batch_reduce.cpp)
target_include_directories(hepcat-reduce-amplitude SYSTEM PRIVATE ${GMP_INCLUDE_DIR})
target_link_libraries(hepcat-reduce-amplitude PRIVATE hepcat_reduce)

include(CTest)
if(BUILD_TESTING)
    foreach(suite IN ITEMS spinor kinematics collect vector_basis api amplitude completion residue continuations high_energy high_energy_chart)
        add_executable(test_${suite} ${suite}_tests.cpp)
        target_include_directories(test_${suite} SYSTEM PRIVATE ${GMP_INCLUDE_DIR})
        target_link_libraries(test_${suite} PRIVATE hepcat_reduce)
        add_test(NAME ${suite} COMMAND test_${suite})
    endforeach()
    add_executable(test_c_api c_api_test.c)
    target_link_libraries(test_c_api PRIVATE hepcat_reduce)
    add_test(NAME c_api COMMAND test_c_api)
endif()
