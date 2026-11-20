// CalcHEP supplies the numerical menus, VEGAS, cuts and histograms.
// Every matrix element comes from the generated SPINAS class below.
// No CalcHEP symbolic process generation is used by this executable.
#include "external-model.hpp"
#include "calchep-spinas-runtime.hpp"

#ifndef HEPCAT_CALCHEP_ROOT
#error "Define HEPCAT_CALCHEP_ROOT in the numerical target."
#endif

namespace {

using Process = native_export::Process;

#include "calchep-model.inc"

hepcat_calchep::NumericalProcess<Process> numerical_process(numerical_configuration());

} // namespace

// CalcHEP's existing numerical executable links against these model hooks.
extern "C" {

int nModelParticles = numerical_process.particle_count();
ModelPrtclsStr* ModelPrtcls = numerical_process.model_particles();
int nModelVars = numerical_process.independent_count();
int nModelFunc = numerical_process.dependent_count();
int current_variable = 0;
int* currentVarPtr = &current_variable;
char** varNames = numerical_process.model_names();
REAL* varValues = numerical_process.model_values();

int calcMainFunc() { return numerical_process.calculate_model(); }

CalcHEP_interface interface_ext = numerical_process.process_interface();
CalcHEP_interface* PtrInterface_ext = &interface_ext;

int findval(char* name, double* value) {
  if (name == nullptr || value == nullptr) return 1;
  for (int i = 0; i < numerical_process.model_count(); ++i) {
    if (std::strcmp(name, varNames[i]) == 0) {
      *value = static_cast<double>(varValues[i]);
      return 0;
    }
  }
  return 1;
}

int qnumbers(char* name, int* spin2, int* charge3, int* color) {
  return numerical_process.quantum_numbers(name, spin2, charge3, color);
}

double usrFF_(int incoming, int outgoing, double* momenta, char** names, int* pdgs) {
  return usrFF(incoming, outgoing, momenta, names, pdgs);
}

double usrfun_(char* name, int incoming, int outgoing, double* momenta, char** names, int* pdgs) {
  return usrfun(name, incoming, outgoing, momenta, names, pdgs);
}

} // extern C
