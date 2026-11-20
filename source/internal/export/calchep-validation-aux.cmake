# Link the validator and its generated matrix to the same standalone CalcHEP
# helper. CalcHEP 3.9.2 builds its distributed shared helper for micrOMEGAs,
# whose additional width array has only 40 entries. Use CalcHEP's own compiler,
# numerical types and flags so that the helper matches the matrix ABI.
if(NOT TARGET hepcat_calchep_validation_aux)
  add_library(hepcat_calchep_validation_aux INTERFACE)
  set(num_source "${CALCHEP_ROOT}/c_source/num")
  if(EXISTS "${num_source}/sqme_aux.c" AND EXISTS "${num_source}/ferror.c")
    set(aux "${CMAKE_CURRENT_BINARY_DIR}/libhepcat_calchep_validation_aux.so")
    if(APPLE)
      set(helper_id -install_name)
    else()
      set(helper_id -soname)
    endif()
    add_custom_command(OUTPUT "${aux}"
      COMMAND /bin/sh -c [=[export CALCHEP="$1" && . "$1/FlagsForSh" && exec $CC $CFLAGS -UforMICROMEGAS $SHARED -Xlinker "$3" -Xlinker "$2" -I"$1/include" -o "$2" "$1/c_source/num/sqme_aux.c" "$1/c_source/num/ferror.c" -lm $lQuad]=]
        sh "${CALCHEP_ROOT}" "${aux}" "${helper_id}"
      DEPENDS "${num_source}/sqme_aux.c" "${num_source}/ferror.c"
        "${CALCHEP_ROOT}/include/nType.h" "${CALCHEP_ROOT}/include/num_in.h" "${CALCHEP_ROOT}/FlagsForSh"
      COMMENT "Building CalcHEP's standalone matrix-element helper" VERBATIM)
    add_custom_target(hepcat_build_calchep_aux DEPENDS "${aux}")
    add_dependencies(hepcat_calchep_validation_aux hepcat_build_calchep_aux)
    target_compile_definitions(hepcat_calchep_validation_aux INTERFACE HEPCAT_CALCHEP_AUX="${aux}")
  else()
    set(aux "${CALCHEP_ROOT}/lib/sqme_aux.so")
  endif()
  target_link_libraries(hepcat_calchep_validation_aux INTERFACE "${aux}")
endif()
