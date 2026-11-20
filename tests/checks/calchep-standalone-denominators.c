// Exercise the helper actually loaded by a generated subprocess, using more
// denominators than the optional micrOMEGAs width table can accommodate.
#include "num_in.h"
#include <dlfcn.h>
#include <stdio.h>

int check_standalone_denominators(const char* matrix_path) {
  enum { count = 44 };
  void* matrix = dlopen(matrix_path, RTLD_NOW);
  if (!matrix) { fprintf(stderr, "%s\n", dlerror()); return 0; }
  // Both sides must resolve the same helper, including its polarization state.
  const int same_helper = dlsym(matrix, "prepDen") == (void*)prepDen;
  const int legacy_widths = dlsym(matrix, "omgWidth") != NULL;
  dlclose(matrix);
  if (!same_helper) return 0;
  if (legacy_widths) {
#ifdef HEPCAT_CALCHEP_AUX
    return 0;
#else
    // Binary-only installation. The runtime checks the count before calling sqme.
    return 1;
#endif
  }

  REAL mass[count+1] = {0}, width[count+1] = {0}, q0[count+1], q2[count+1];
  COMPLEX q1[count+1];
  REAL momenta[8] = {2, 0, 0, 0, 2, 0, 0, 0};
  char first_momentum[] = {1, 0};
  char* channel[count+1] = {NULL};
  for (int i = 1; i <= count; ++i) {
    channel[i] = first_momentum;
    mass[i] = 1;
  }
  const REAL saved = N_pol_11_;
  N_pol_11_ = 5;
  const int err = prepDen(count, 2, 100, mass, width, channel, momenta, q0, q1, q2);
  N_pol_11_ = saved;
  if (err) return 0;
  for (int i = 1; i <= count; ++i) {
    if (width[i] != 0 || q0[i] != 1 || Cabs(3*q1[i]+1) > 1e-14 ||
        Fabs(9*q2[i]-1) > 1e-14) return 0;
  }
  return 1;
}
