# Optional independent CalcHEP validator. Ordinary SPINAS code has no CalcHEP dependency.
set(CALCHEP_ROOT "${HEPCAT_CALCHEP}" CACHE PATH "CalcHEP installation")
add_executable(${HEPCAT_PROCESS}_validate_calchep calchep-main.cpp)
target_link_libraries(${HEPCAT_PROCESS}_validate_calchep PRIVATE ${HEPCAT_PROCESS}_process)
if(UNIX AND HEPCAT_CALCHEP AND EXISTS "${CALCHEP_ROOT}/lib/dynamic_me.a")
  enable_language(C)
  target_compile_definitions(${HEPCAT_PROCESS}_validate_calchep PRIVATE HEPCAT_HAS_CALCHEP HEPCAT_CALCHEP_ROOT="${CALCHEP_ROOT}")
  target_include_directories(${HEPCAT_PROCESS}_validate_calchep PRIVATE
    "${CALCHEP_ROOT}/include" "${CALCHEP_ROOT}/c_source/dynamicME/include"
    "${CALCHEP_ROOT}/c_source/ntools/include")
  target_sources(${HEPCAT_PROCESS}_validate_calchep PRIVATE "${CALCHEP_ROOT}/include/VandPgate.c")
  target_link_libraries(${HEPCAT_PROCESS}_validate_calchep PUBLIC
    "${CALCHEP_ROOT}/lib/dynamic_vp.a" "${CALCHEP_ROOT}/lib/dynamic_me.a"
    "${CALCHEP_ROOT}/lib/num_c.a" "${CALCHEP_ROOT}/lib/servNoX11.a" "${CALCHEP_ROOT}/lib/serv.a"
    "${CALCHEP_ROOT}/lib/sqme_aux.so" "${CALCHEP_ROOT}/lib/libSLHAplus.a"
    "${CALCHEP_ROOT}/lib/dummy.a" "${CALCHEP_ROOT}/lib/ntools.a"
    ${CMAKE_DL_LIBS} m pthread)
  set_target_properties(${HEPCAT_PROCESS}_validate_calchep PROPERTIES BUILD_RPATH "${CALCHEP_ROOT}/lib")
else()
  message(STATUS "CalcHEP validator will report unavailable; set CALCHEP_ROOT to enable the backend.")
endif()

# CalcHEP numerical integration with the SPINAS matrix element.
# This target neither runs s_calchep nor compiles a symbolic CalcHEP process.
if(NOT CALCHEP_ROOT)
  set(CALCHEP_ROOT "${HEPCAT_CALCHEP}" CACHE PATH "CalcHEP numerical installation" FORCE)
endif()
if(UNIX AND HEPCAT_CALCHEP AND NOT HEPCAT_CALCHEP_NUMERICAL_ERROR AND EXISTS "${CALCHEP_ROOT}/lib/n_calchep.o" AND EXISTS "${CALCHEP_ROOT}/lib/num_c.a")
  add_executable(n_calchep calchep-spinas.cpp "${CALCHEP_ROOT}/lib/n_calchep.o")
  target_include_directories(n_calchep PRIVATE "${CALCHEP_ROOT}/include")
  target_compile_definitions(n_calchep PRIVATE HEPCAT_CALCHEP_ROOT="${CALCHEP_ROOT}")
  find_library(CALCHEP_X11_LIBRARY NAMES X11 HINTS /opt/X11/lib /usr/X11R6/lib)
  target_link_libraries(n_calchep PRIVATE ${HEPCAT_PROCESS}_process
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
else()
  message(STATUS "CalcHEP numerical integration unavailable. ${HEPCAT_CALCHEP_NUMERICAL_ERROR}")
endif()

# Independent MadGraph validation; generation and Fortran compilation happen only when run.
find_package(Threads REQUIRED)
add_executable(${HEPCAT_PROCESS}_validate_madgraph madgraph-main.cpp)
target_link_libraries(${HEPCAT_PROCESS}_validate_madgraph PRIVATE ${HEPCAT_PROCESS}_process ${CMAKE_DL_LIBS} Threads::Threads)
target_compile_definitions(${HEPCAT_PROCESS}_validate_madgraph PRIVATE HEPCAT_PROJECT_DIR="${CMAKE_CURRENT_SOURCE_DIR}")

# MadEvent uses this raw fixed-spin kernel; the reference validator stays independent.
# A static archive also supports an existing SPINAS library built without PIC.
add_library(${HEPCAT_PROCESS}_madevent_matrix STATIC madevent-spinas.cpp)
target_link_libraries(${HEPCAT_PROCESS}_madevent_matrix PUBLIC ${HEPCAT_PROCESS}_process)
if(APPLE AND CMAKE_CXX_COMPILER_ID MATCHES "Clang")
  set(HEPCAT_MADEVENT_CXX_RUNTIME "c++")
else()
  set(HEPCAT_MADEVENT_CXX_RUNTIME "stdc++")
endif()
get_filename_component(HEPCAT_MADEVENT_RUNTIME_DIRECTORY "${SPINAS_LIBRARY}" DIRECTORY)
if("${SPINAS_LIBRARY}" MATCHES "[.]a$")
  set(HEPCAT_MADEVENT_SPINAS_STATIC "${SPINAS_LIBRARY}")
else()
  find_file(HEPCAT_MADEVENT_SPINAS_STATIC NAMES libspinas.a
    HINTS "${HEPCAT_MADEVENT_RUNTIME_DIRECTORY}" "${SPINAS_ROOT}/build" "${SPINAS_ROOT}/lib")
  if(NOT HEPCAT_MADEVENT_SPINAS_STATIC)
    set(HEPCAT_MADEVENT_SPINAS_STATIC "")
  endif()
endif()
file(GENERATE OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/madevent-link.json" CONTENT
  "{\n  \"Libraries\": [\"$<TARGET_FILE:${HEPCAT_PROCESS}_madevent_matrix>\", \"$<TARGET_FILE:${HEPCAT_PROCESS}_process>\", \"${SPINAS_LIBRARY}\"],\n  \"SPINASStaticLibrary\": \"${HEPCAT_MADEVENT_SPINAS_STATIC}\",\n  \"SPINASIncludeDirectory\": \"${SPINAS_INCLUDE_DIR}\",\n  \"CXXRuntime\": \"${HEPCAT_MADEVENT_CXX_RUNTIME}\",\n  \"RuntimeLibraryDirectories\": [\"${HEPCAT_MADEVENT_RUNTIME_DIRECTORY}\"]\n}\n")
if(HEPCAT_MADGRAPH AND NOT HEPCAT_EXTERNAL_COLOR)
# Prepare the movable native process during an ordinary build, without running events.
# Repeated builds retain its existing cards and event runs.
option(HEPCAT_PREPARE_MADEVENT "Prepare the native HEPCAT MadEvent process during the build" ON)
set(HEPCAT_MADEVENT_DEFAULT_TARGET)
if(HEPCAT_PREPARE_MADEVENT)
  set(HEPCAT_MADEVENT_DEFAULT_TARGET ALL)
endif()
add_custom_target(${HEPCAT_PROCESS}_madevent_process ${HEPCAT_MADEVENT_DEFAULT_TARGET}
  COMMAND "${HEPCAT_PYTHON}" "${CMAKE_CURRENT_SOURCE_DIR}/madevent-runtime.py"
    --configuration "${CMAKE_CURRENT_SOURCE_DIR}/madevent-config.json"
    --bridge-link-file "${CMAKE_CURRENT_BINARY_DIR}/madevent-link.json" --prepare --refresh-generated
  DEPENDS ${HEPCAT_PROCESS}_madevent_matrix ${HEPCAT_PROCESS}_process
  WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}"
  COMMENT "Preparing PROC_${HEPCAT_PROCESS} with the HEPCAT/SPINAS amplitude"
  VERBATIM USES_TERMINAL)
endif()
