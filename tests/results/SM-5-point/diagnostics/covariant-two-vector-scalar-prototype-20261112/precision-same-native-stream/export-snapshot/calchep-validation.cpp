#include "process.hpp"
#include <iostream>
#include <stdexcept>

#ifdef HEPCAT_HAS_CALCHEP
#include "calchep-runtime.hpp"
#endif

namespace hepcat_generated {

hepcat_spinas::ValidationReport experimentaleebarZZh::validate_calchep(
    const hepcat_spinas::ValidationOptions& options) {
#ifdef HEPCAT_HAS_CALCHEP
  try {
    // Work on a copy: the caller's momenta and caches stay unchanged.
    experimentaleebarZZh spinas_process(*this);
    auto validation_parameters = spinas_process.get_parameters();
    spinas_process.set_parameters(validation_parameters);
    spinas_process.set_widths_enabled(false);

    // Physical process: e, E -> Z, Z, h
    const std::string model_directory = "/Users/neil/code/hepcat/tests/reference-models/calchep";
    const int model_number = 1;
    const std::vector<int> particle_pdgs = {11, -11, 23, 23, 25};
    const std::vector<int> allowed_particle_pdgs = {};

    // Match the current SPINAS parameters to the CalcHEP model by name.
    const auto parameters = spinas_process.get_parameters();
    const double Me = hepcat_spinas::checked_calchep_parameter(parameters[0], "Me");
    const double MZ = hepcat_spinas::checked_calchep_parameter(parameters[1], "MZ");
    const double Mh = hepcat_spinas::checked_calchep_parameter(parameters[2], "Mh");
    const double EE = hepcat_spinas::checked_calchep_parameter(parameters[3], "EE");
    const double SW = hepcat_spinas::checked_calchep_parameter(parameters[4], "SW");
    const double MW = hepcat_spinas::checked_calchep_parameter(parameters[5], "MW");
    const std::vector<hepcat_spinas::CalcHEPParameter> model_parameters = {
      {"Me", Me},
      {"MZ", MZ},
      {"Mh", Mh},
      {"EE", EE},
      {"SW", SW},
      {"MW", MW}
    };

    // Generate and compile the CalcHEP process in its private workspace.
    hepcat_spinas::CalcHEPProcess calchep_process(
      model_directory, model_number, particle_pdgs, model_parameters, 1.0, false, allowed_particle_pdgs, 0, {"Me", "Mh", "MW"});
    if (!allowed_particle_pdgs.empty()) {
      std::cout << "CalcHEP excluded internal particles:";
      for (const auto& excluded_particle : calchep_process.excluded_particle_names())
        std::cout << ' ' << excluded_particle;
      std::cout << '\n';
    }
    hepcat_spinas::check_calchep_masses(spinas_process.masses(), calchep_process.masses());

    // Each evaluator receives exactly the same physical phase-space point.
    auto spinas_squared = [&](const Momenta& point) -> ldouble {
      spinas_process.set_momenta(point);
      return spinas_process.amp2();
    };

    auto calchep_squared = [&](const Momenta& point) -> ldouble {
      return static_cast<ldouble>(calchep_process.squared_amplitude(
        point));
    };

    // Both results include the initial average and final symmetry factor.
    auto report = hepcat_spinas::compare_squared_amplitudes(
      spinas_process.masses(), incoming_legs(),
      spinas_squared, calchep_squared, options, &std::cout);
    const auto mass_scheme = calchep_process.mass_scheme_summary();
    if (!mass_scheme.empty()) { report.message += " " + mass_scheme; std::cout << mass_scheme << '\n'; }
    return report;
  } catch (const std::exception& error) {
    return hepcat_spinas::failed_validation(options, error.what());
  }
#else
  return hepcat_spinas::unavailable_validation(options,
    "CalcHEP support was not built. Configure CALCHEP_ROOT and rebuild this validator.");
#endif
}

} // namespace hepcat_generated
