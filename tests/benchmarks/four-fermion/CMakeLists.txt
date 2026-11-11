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
project(HEPCATFourFermionBenchmark LANGUAGES C CXX)
set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Release CACHE STRING "Benchmark build type" FORCE)
endif()
if(NOT CMAKE_BUILD_TYPE STREQUAL "Release")
  message(FATAL_ERROR "Use Release for comparable optimized timings.")
endif()
if(CMAKE_CXX_FLAGS MATCHES "-Ofast|-ffast-math")
  message(FATAL_ERROR "This validation benchmark does not permit unsafe fast-math options.")
endif()
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
find_package(Python3 COMPONENTS Interpreter REQUIRED)
find_package(Threads REQUIRED)
get_filename_component(HEPCAT_ROOT "${CMAKE_CURRENT_SOURCE_DIR}/../../.." ABSOLUTE)
get_filename_component(DEFAULT_CODE_DIRECTORY "${HEPCAT_ROOT}/.." ABSOLUTE)
set(SPINAS_ROOT "${DEFAULT_CODE_DIRECTORY}/spinas" CACHE PATH "SPINAS installation or checkout")
set(CALCHEP_ROOT "" CACHE PATH "CalcHEP installation containing lib/dynamic_me.a")
set(HEPCAT_PHYSICAL_PROCESS_DIRECTORIES "" CACHE STRING "Complete double-precision physical-pole four-fermion exports, including N=0")
set(HEPCAT_SPURIOUS_PROCESS_DIRECTORIES "" CACHE STRING "Complete double-precision spurious-pole four-fermion exports, matching the physical multiplicities")
if(NOT HEPCAT_PHYSICAL_PROCESS_DIRECTORIES OR NOT HEPCAT_SPURIOUS_PROCESS_DIRECTORIES)
  message(FATAL_ERROR "Set both HEPCAT_PHYSICAL_PROCESS_DIRECTORIES and HEPCAT_SPURIOUS_PROCESS_DIRECTORIES.")
endif()
find_path(SPINAS_INCLUDE_DIR spinas.h HINTS "${SPINAS_ROOT}/include" REQUIRED)
find_library(SPINAS_LIBRARY NAMES spinas HINTS "${SPINAS_ROOT}/build" "${SPINAS_ROOT}/lib" REQUIRED)
if(NOT EXISTS "${CALCHEP_ROOT}/lib/dynamic_me.a")
  message(FATAL_ERROR "Set CALCHEP_ROOT to the installation selected in the exports.")
endif()
set(GENERATED_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/generated")
execute_process(COMMAND "${Python3_EXECUTABLE}" "${HEPCAT_ROOT}/tests/benchmarks/four-fermion-configure.py"
  --output "${GENERATED_DIRECTORY}"
  --physical-projects ${HEPCAT_PHYSICAL_PROCESS_DIRECTORIES}
  --spurious-projects ${HEPCAT_SPURIOUS_PROCESS_DIRECTORIES}
  --compiler "${CMAKE_CXX_COMPILER}" --compiler-version "${CMAKE_CXX_COMPILER_VERSION}"
  "--flags=${CMAKE_CXX_FLAGS} ${CMAKE_CXX_FLAGS_RELEASE} -O3"
  --spinas-library "${SPINAS_LIBRARY}" --calchep-directory "${CALCHEP_ROOT}"
  RESULT_VARIABLE CONFIGURE_STATUS ERROR_VARIABLE CONFIGURE_ERROR)
if(NOT CONFIGURE_STATUS EQUAL 0)
  message(FATAL_ERROR "Four-fermion benchmark configuration failed: ${CONFIGURE_ERROR}")
endif()
include("${GENERATED_DIRECTORY}/projects.cmake")
# A linked call checks the scalar C++ ABI, not just the mantissa width. Apple
# arm64 double and long double have equal precision but different symbol names.
include(CheckCXXSourceCompiles)
include(CMakePushCheckState)
cmake_push_check_state(RESET)
set(CMAKE_REQUIRED_INCLUDES "${SPINAS_INCLUDE_DIR}")
set(CMAKE_REQUIRED_LIBRARIES "${SPINAS_LIBRARY}")
unset(HEPCAT_FOUR_FERMION_DOUBLE_ABI_MATCHES CACHE)
check_cxx_source_compiles("#include <spinas.h>
int main() { spinas::particle p(ldouble(1)); return p.get_mass() > 0 ? 0 : 1; }"
  HEPCAT_FOUR_FERMION_DOUBLE_ABI_MATCHES)
cmake_pop_check_state()
if(NOT HEPCAT_FOUR_FERMION_DOUBLE_ABI_MATCHES)
  message(FATAL_ERROR "Select a double-precision SPINAS_LIBRARY matching these exports.")
endif()
add_executable(qed_four_fermion_benchmark "${HEPCAT_ROOT}/tests/benchmarks/qed-four-fermions.cpp"
  ${FOUR_FERMION_SOURCES} "${CALCHEP_ROOT}/include/VandPgate.c")
target_include_directories(qed_four_fermion_benchmark PRIVATE "${HEPCAT_ROOT}/benchmarks"
  "${HEPCAT_ROOT}/source/internal/export" "${SPINAS_INCLUDE_DIR}" "${CALCHEP_ROOT}/include"
  "${CALCHEP_ROOT}/c_source/dynamicME/include" "${CALCHEP_ROOT}/c_source/ntools/include")
target_compile_options(qed_four_fermion_benchmark PRIVATE -O3)
target_compile_definitions(qed_four_fermion_benchmark PRIVATE
  HEPCAT_FOUR_FERMION_REGISTRATION_HEADER="${FOUR_FERMION_REGISTRY}"
  HEPCAT_BENCHMARK_BUILD_INFO="${GENERATED_DIRECTORY}/build-info.json")
target_link_libraries(qed_four_fermion_benchmark PRIVATE "${SPINAS_LIBRARY}"
  "${CALCHEP_ROOT}/lib/dynamic_vp.a" "${CALCHEP_ROOT}/lib/dynamic_me.a"
  "${CALCHEP_ROOT}/lib/num_c.a" "${CALCHEP_ROOT}/lib/servNoX11.a" "${CALCHEP_ROOT}/lib/serv.a"
  "${CALCHEP_ROOT}/lib/sqme_aux.so" "${CALCHEP_ROOT}/lib/libSLHAplus.a"
  "${CALCHEP_ROOT}/lib/dummy.a" "${CALCHEP_ROOT}/lib/ntools.a"
  ${CMAKE_DL_LIBS} m Threads::Threads)
set_target_properties(qed_four_fermion_benchmark PROPERTIES BUILD_RPATH "${CALCHEP_ROOT}/lib")
