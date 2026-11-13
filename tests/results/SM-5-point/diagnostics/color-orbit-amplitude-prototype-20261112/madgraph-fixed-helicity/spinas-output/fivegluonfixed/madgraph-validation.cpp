#include "process.hpp"
#include <iostream>

namespace hepcat_generated {

hepcat_spinas::MadGraphValidationReport fivegluonfixed::validate_madgraph(
    const hepcat_spinas::MadGraphValidationOptions& options) {
  hepcat_spinas::MadGraphConfiguration config;
  config.available = true;
  config.reason = "";
  config.madgraph_directory = "/Users/neil/code/MadGraph";
  config.model = "/Users/neil/code/hepcat/tests/reference-models/madgraph/sm";
  config.python_executable = "/opt/homebrew/opt/python@3.11/bin/python3.11";
  config.helper_path = "/Users/neil/code/hepcat/tests/results/SM-5-point/diagnostics/color-orbit-amplitude-prototype-20261112/madgraph-fixed-helicity/spinas-output/fivegluonfixed/madgraph-runtime.py";
  config.physical_pdgs = {21, 21, 21, 21, 21};
  config.allowed_particle_pdgs = {};
  config.leg_order = {0, 1, 2, 3, 4};
  config.spin2 = {2, 2, 2, 2, 2};
  config.full_color = true;
  config.color_representations = {8, 8, 8, 8, 8};
  config.parameter_names = {"GG"};
  config.helicities = {{2, 2, 2, -2, -2}};
  config.exact_zero_helicities = {};
  config.normalization = ldouble(1.5625e-2L);
  config.width_parameter_indices = {};
  config.parameter_rules["aEWM1"] = ldouble(1.27998802505567882537675e2L);
  config.parameter_rules["Gf"] = ldouble(1.19542477614740994733841e-5L);
  config.parameter_rules["MZ"] = ldouble(9.12921430831531714430205e1L);
  config.parameter_rules["lamWS"] = ldouble(0.0L);
  config.mass_parameter_pdgs["MZ"] = {23};
  config.mass_parameter_pdgs["MW"] = {24};
  config.mass_parameter_pdgs["Me"] = {11};
  config.mass_parameter_pdgs["Mm"] = {13};
  config.mass_parameter_pdgs["Ml"] = {15};
  config.mass_parameter_pdgs["Mu"] = {2};
  config.mass_parameter_pdgs["Md"] = {1};
  config.mass_parameter_pdgs["Mc"] = {4};
  config.mass_parameter_pdgs["Ms"] = {3};
  config.mass_parameter_pdgs["Mtp"] = {6};
  config.mass_parameter_pdgs["Mb"] = {5};
  config.mass_parameter_pdgs["Mh"] = {25};
  return hepcat_spinas::validate_madgraph(*this, config, options, &std::cout);
}

} // namespace hepcat_generated
