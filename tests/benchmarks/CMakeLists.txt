# HEPCAT - High Energy Physics Constructive Amplitude Tool
#
# Copyright (C) 2026 Neil Christensen
#
# This file is part of HEPCAT.
#
# HEPCAT is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# HEPCAT is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with HEPCAT.  If not, see <https://www.gnu.org/licenses/>.

cmake_minimum_required(VERSION 3.18)
project(HEPCATPhotonBenchmark LANGUAGES C CXX)
set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Release CACHE STRING "Benchmark build type" FORCE)
endif()
if(NOT CMAKE_BUILD_TYPE STREQUAL "Release")
  message(FATAL_ERROR "Use -DCMAKE_BUILD_TYPE=Release for comparable optimized timings.")
endif()
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
find_package(Python3 COMPONENTS Interpreter REQUIRED)
find_package(Threads REQUIRED)
get_filename_component(HEPCAT_ROOT "${CMAKE_CURRENT_SOURCE_DIR}/../.." ABSOLUTE)
get_filename_component(DEFAULT_CODE_DIRECTORY "${HEPCAT_ROOT}/.." ABSOLUTE)
set(SPINAS_ROOT "${DEFAULT_CODE_DIRECTORY}/spinas" CACHE PATH "SPINAS installation or checkout")
set(CALCHEP_ROOT "" CACHE PATH "CalcHEP installation containing lib/dynamic_me.a")
set(HEPCAT_PROCESS_DIRECTORIES
    "${HEPCAT_ROOT}/tests/results/spinas-output/eeAA;${HEPCAT_ROOT}/tests/results/spinas-output/eeAAA;${HEPCAT_ROOT}/tests/results/spinas-output/eeAAAA"
    CACHE STRING "Complete WriteSPINASProcess projects, one per photon multiplicity")
find_path(SPINAS_INCLUDE_DIR spinas.h HINTS "${SPINAS_ROOT}/include" REQUIRED)
find_library(SPINAS_LIBRARY NAMES spinas HINTS "${SPINAS_ROOT}/build" "${SPINAS_ROOT}/lib" REQUIRED)
if(NOT EXISTS "${CALCHEP_ROOT}/lib/dynamic_me.a")
  message(FATAL_ERROR "Set -DCALCHEP_ROOT=/path/to/calchep. Reference models are read from the exports.")
endif()
set(GENERATED_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/generated")
execute_process(COMMAND "${Python3_EXECUTABLE}" "${CMAKE_CURRENT_SOURCE_DIR}/configure.py"
  --output "${GENERATED_DIRECTORY}" --projects ${HEPCAT_PROCESS_DIRECTORIES}
  --compiler "${CMAKE_CXX_COMPILER}" --compiler-version "${CMAKE_CXX_COMPILER_VERSION}"
  "--flags=${CMAKE_CXX_FLAGS} ${CMAKE_CXX_FLAGS_RELEASE}"
  --spinas-library "${SPINAS_LIBRARY}" --calchep-directory "${CALCHEP_ROOT}"
  RESULT_VARIABLE CONFIGURE_STATUS ERROR_VARIABLE CONFIGURE_ERROR)
if(NOT CONFIGURE_STATUS EQUAL 0)
  message(FATAL_ERROR "Benchmark configuration failed: ${CONFIGURE_ERROR}")
endif()
include("${GENERATED_DIRECTORY}/projects.cmake")
# A matching mantissa width does not establish C++ ABI compatibility. In
# particular, macOS arm64 double and long double have equal precision but
# different mangled parameter types. Check the actual linked SPINAS library.
include(CheckCXXSourceCompiles)
include(CMakePushCheckState)
cmake_push_check_state(RESET)
set(CMAKE_REQUIRED_INCLUDES "${SPINAS_INCLUDE_DIR}")
set(CMAKE_REQUIRED_LIBRARIES "${SPINAS_LIBRARY}")
if(BENCHMARK_LONG_DOUBLE)
  set(CMAKE_REQUIRED_DEFINITIONS -DWITH_LONG_DOUBLE)
endif()
unset(HEPCAT_SPINAS_PRECISION_MATCHES CACHE)
check_cxx_source_compiles("#include <spinas.h>
int main() { spinas::particle p(ldouble(1)); return p.get_mass() > 0 ? 0 : 1; }"
  HEPCAT_SPINAS_PRECISION_MATCHES)
cmake_pop_check_state()
if(NOT HEPCAT_SPINAS_PRECISION_MATCHES)
  message(FATAL_ERROR "The SPINAS library does not match the exported scalar precision. Select a matching SPINAS_LIBRARY.")
endif()
add_executable(qed_photon_benchmark qed-photons.cpp ${BENCHMARK_SOURCES}
  "${CALCHEP_ROOT}/include/VandPgate.c")
target_include_directories(qed_photon_benchmark PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}"
  "${HEPCAT_ROOT}/source/internal/export" "${SPINAS_INCLUDE_DIR}" "${CALCHEP_ROOT}/include"
  "${CALCHEP_ROOT}/c_source/dynamicME/include" "${CALCHEP_ROOT}/c_source/ntools/include")
if(BENCHMARK_LONG_DOUBLE)
  target_compile_definitions(qed_photon_benchmark PRIVATE WITH_LONG_DOUBLE)
endif()
target_compile_definitions(qed_photon_benchmark PRIVATE
  HEPCAT_BENCHMARK_BUILD_INFO="${GENERATED_DIRECTORY}/build-info.json")
target_link_libraries(qed_photon_benchmark PRIVATE "${SPINAS_LIBRARY}"
  "${CALCHEP_ROOT}/lib/dynamic_vp.a" "${CALCHEP_ROOT}/lib/dynamic_me.a"
  "${CALCHEP_ROOT}/lib/num_c.a" "${CALCHEP_ROOT}/lib/servNoX11.a" "${CALCHEP_ROOT}/lib/serv.a"
  "${CALCHEP_ROOT}/lib/sqme_aux.so" "${CALCHEP_ROOT}/lib/libSLHAplus.a"
  "${CALCHEP_ROOT}/lib/dummy.a" "${CALCHEP_ROOT}/lib/ntools.a"
  ${CMAKE_DL_LIBS} m Threads::Threads)
set_target_properties(qed_photon_benchmark PROPERTIES BUILD_RPATH "${CALCHEP_ROOT}/lib")
include(CTest)
if(BUILD_TESTING)
  add_executable(qed_photon_benchmark_tests "${HEPCAT_ROOT}/tests/checks/qed-photon-benchmark-tests.cpp")
  target_include_directories(qed_photon_benchmark_tests PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}"
    "${HEPCAT_ROOT}/source/internal/export" "${SPINAS_INCLUDE_DIR}" "${CALCHEP_ROOT}/include"
    "${CALCHEP_ROOT}/c_source/dynamicME/include" "${CALCHEP_ROOT}/c_source/ntools/include")
  if(BENCHMARK_LONG_DOUBLE)
    target_compile_definitions(qed_photon_benchmark_tests PRIVATE WITH_LONG_DOUBLE)
  endif()
  target_link_libraries(qed_photon_benchmark_tests PRIVATE "${SPINAS_LIBRARY}" ${CMAKE_DL_LIBS} Threads::Threads)
  add_test(NAME benchmark_kinematics COMMAND qed_photon_benchmark_tests)
endif()
