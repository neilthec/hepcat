cmake_minimum_required(VERSION 3.10)
project(draftcoloredcurrent4 LANGUAGES CXX)
set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
if(NOT CMAKE_CONFIGURATION_TYPES AND NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Release CACHE STRING "Build type (Release, Debug, RelWithDebInfo)" FORCE)
endif()
set(SPINAS_ROOT "/Users/neil/code/spinas" CACHE PATH "SPINAS checkout or installation prefix")
find_path(SPINAS_INCLUDE_DIR spinas.h HINTS "${SPINAS_ROOT}/include")
find_library(SPINAS_LIBRARY NAMES spinas spinas_shared HINTS "${SPINAS_ROOT}/build" "${SPINAS_ROOT}/lib")
if(NOT SPINAS_INCLUDE_DIR OR NOT SPINAS_LIBRARY)
  message(FATAL_ERROR "SPINAS headers/library not found. Set -DSPINAS_ROOT=/path/to/spinas.")
endif()
add_library(draftcoloredcurrent4_process STATIC process.cpp)
target_include_directories(draftcoloredcurrent4_process PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}" "${SPINAS_INCLUDE_DIR}")
target_link_libraries(draftcoloredcurrent4_process PUBLIC "${SPINAS_LIBRARY}")
target_compile_definitions(draftcoloredcurrent4_process PUBLIC WITH_LONG_DOUBLE)
add_executable(draftcoloredcurrent4 main.cpp)
target_link_libraries(draftcoloredcurrent4 PRIVATE draftcoloredcurrent4_process)
enable_testing()
add_test(NAME phase_space_examples COMMAND draftcoloredcurrent4)

# Optional independent CalcHEP validator. Ordinary SPINAS code has no CalcHEP dependency.
set(CALCHEP_ROOT "/Users/neil/code/CalcHEP/calchep_3.9.2" CACHE PATH "CalcHEP installation")
add_library(draftcoloredcurrent4_calchep_validation STATIC calchep-validation.cpp)
target_link_libraries(draftcoloredcurrent4_calchep_validation PUBLIC draftcoloredcurrent4_process)
add_executable(draftcoloredcurrent4_validate_calchep calchep-main.cpp)
target_link_libraries(draftcoloredcurrent4_validate_calchep PRIVATE draftcoloredcurrent4_calchep_validation)
if(UNIX AND TRUE AND EXISTS "${CALCHEP_ROOT}/lib/dynamic_me.a")
  enable_language(C)
  target_compile_definitions(draftcoloredcurrent4_calchep_validation PRIVATE HEPCAT_HAS_CALCHEP)
  target_include_directories(draftcoloredcurrent4_calchep_validation PRIVATE
    "${CALCHEP_ROOT}/include" "${CALCHEP_ROOT}/c_source/dynamicME/include"
    "${CALCHEP_ROOT}/c_source/ntools/include")
  target_sources(draftcoloredcurrent4_calchep_validation PRIVATE "${CALCHEP_ROOT}/include/VandPgate.c")
  target_link_libraries(draftcoloredcurrent4_calchep_validation PUBLIC
    "${CALCHEP_ROOT}/lib/dynamic_vp.a" "${CALCHEP_ROOT}/lib/dynamic_me.a"
    "${CALCHEP_ROOT}/lib/num_c.a" "${CALCHEP_ROOT}/lib/servNoX11.a" "${CALCHEP_ROOT}/lib/serv.a"
    "${CALCHEP_ROOT}/lib/sqme_aux.so" "${CALCHEP_ROOT}/lib/libSLHAplus.a"
    "${CALCHEP_ROOT}/lib/dummy.a" "${CALCHEP_ROOT}/lib/ntools.a"
    ${CMAKE_DL_LIBS} m pthread)
  set_target_properties(draftcoloredcurrent4_validate_calchep PROPERTIES BUILD_RPATH "${CALCHEP_ROOT}/lib")
else()
  message(STATUS "CalcHEP validator will report unavailable; set CALCHEP_ROOT to enable the backend.")
endif()

# CalcHEP numerical integration with the SPINAS matrix element.
# This target neither runs s_calchep nor compiles a symbolic CalcHEP process.
if(NOT CALCHEP_ROOT)
  set(CALCHEP_ROOT "/Users/neil/code/CalcHEP/calchep_3.9.2" CACHE PATH "CalcHEP numerical installation" FORCE)
endif()
if(UNIX AND FALSE AND EXISTS "${CALCHEP_ROOT}/lib/n_calchep.o" AND EXISTS "${CALCHEP_ROOT}/lib/num_c.a")
  add_executable(n_calchep calchep-spinas.cpp "${CALCHEP_ROOT}/lib/n_calchep.o")
  target_include_directories(n_calchep PRIVATE "${CALCHEP_ROOT}/include")
  target_compile_definitions(n_calchep PRIVATE HEPCAT_CALCHEP_ROOT="${CALCHEP_ROOT}")
  find_library(CALCHEP_X11_LIBRARY NAMES X11 HINTS /opt/X11/lib /usr/X11R6/lib)
  target_link_libraries(n_calchep PRIVATE draftcoloredcurrent4_process
    "${CALCHEP_ROOT}/lib/num_c.a" "${CALCHEP_ROOT}/lib/sqme_aux.so"
    "${CALCHEP_ROOT}/lib/lhapdf.so" "${CALCHEP_ROOT}/lib/dynamic_me.a")
  if(CALCHEP_X11_LIBRARY)
    target_link_libraries(n_calchep PRIVATE "${CALCHEP_ROOT}/lib/serv.a" "${CALCHEP_X11_LIBRARY}")
  else()
    target_link_libraries(n_calchep PRIVATE "${CALCHEP_ROOT}/lib/servNoX11.a" "${CALCHEP_ROOT}/lib/serv.a")
    message(STATUS "CalcHEP numerical backend: headless menus and batch integration; X11 was not found.")
  endif()
  target_link_libraries(n_calchep PRIVATE "${CALCHEP_ROOT}/lib/libSLHAplus.a"
    "${CALCHEP_ROOT}/lib/dummy.a" "${CALCHEP_ROOT}/lib/ntools.a" ${CMAKE_DL_LIBS} m pthread)
  set(CALCHEP_SESSION "${CMAKE_CURRENT_BINARY_DIR}/results")
  # A generator expression keeps multi-configuration builds in this same session.
  set_target_properties(n_calchep PROPERTIES BUILD_RPATH "${CALCHEP_ROOT}/lib"
    RUNTIME_OUTPUT_DIRECTORY "${CALCHEP_SESSION}$<0:>")
  file(MAKE_DIRECTORY "${CALCHEP_SESSION}/aux/models" "${CALCHEP_SESSION}/aux/tmp"
    "${CALCHEP_SESSION}/aux/results" "${CALCHEP_SESSION}/aux/so_generated")
  add_custom_command(TARGET n_calchep POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E remove -f "${CMAKE_CURRENT_BINARY_DIR}/n_calchep")
else()
  message(STATUS "CalcHEP numerical integration is unavailable; ordinary SPINAS targets remain usable.")
endif()

# Independent MadGraph validation; generation and Fortran compilation happen only when run.
find_package(Threads REQUIRED)
add_library(draftcoloredcurrent4_madgraph_validation STATIC madgraph-validation.cpp)
target_link_libraries(draftcoloredcurrent4_madgraph_validation PUBLIC draftcoloredcurrent4_process ${CMAKE_DL_LIBS} Threads::Threads)
add_executable(draftcoloredcurrent4_validate_madgraph madgraph-main.cpp)
target_link_libraries(draftcoloredcurrent4_validate_madgraph PRIVATE draftcoloredcurrent4_madgraph_validation)
