/*
HEPCAT - High Energy Physics Constructive Amplitude Tool

Copyright (C) 2026 Neil Christensen

This file is part of HEPCAT.

HEPCAT is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

HEPCAT is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with HEPCAT.  If not, see <https://www.gnu.org/licenses/>.
*/

#include "calchep-runtime.hpp"
#include <iostream>
#include <iomanip>

namespace {
std::string read_file(const std::string& path) {
  std::ifstream input(path.c_str(), std::ios::binary);
  if (!input) throw std::runtime_error("Cannot read test model table: " + path);
  return std::string(std::istreambuf_iterator<char>(input), std::istreambuf_iterator<char>());
}

void check_particle_widths(const std::string& table, const bool zero) {
  std::istringstream input(table);
  std::string line;
  std::vector<std::string> header;
  unsigned particles = 0;
  bool found_nonzero = false;
  while (std::getline(input, line)) {
    if (line.find('|') == std::string::npos) continue;
    const auto fields = hepcat_spinas::calchep_detail::split_table_fields(line);
    if (header.empty()) { header = fields; continue; }
    if (fields.size() < 7 || fields[1].empty()) continue;
    ++particles;
    const bool is_zero = fields[6] == "0";
    found_nonzero = found_nonzero || !is_zero;
    if (zero && !is_zero) throw std::runtime_error("A validation particle width was not zeroed.");
  }
  if (!particles || (!zero && !found_nonzero))
    throw std::runtime_error("The particle-width fixture was not exercised.");
}

void test_width_table_staging() {
  using namespace hepcat_spinas::calchep_detail;
  Workspace workspace(false);
  const std::string header = "Full Name|P|aP|number|spin2|mass|Width|color|aux|LaTeX|LaTeX anti|";
  const std::string rows = "charged vector|V+|V-|24|2|MV|!WV|1|G|V+|V-|\n"
    "neutral vector|Z|Z|23|2|MZ|0.75|1|G|Z|Z|\n"
    "photon|A|A|22|2|0|0|1|G|A|A|\n";
  for (unsigned native = 0; native < 2; ++native) {
    const std::string source = workspace.path() + (native ? "/native.mdl" : "/compact.mdl");
    const std::string staged = source + ".staged";
    const std::string ordinary = source + ".ordinary";
    const std::string original = "Width staging fixture\n" +
      (native ? std::string("Particles\n") : std::string()) + header + "\n" + rows;
    { std::ofstream file(source.c_str()); file << original; }
    copy_model_table(source, staged, "Particles", false, true);
    copy_model_table(source, ordinary, "Particles", false, false);
    check_particle_widths(read_file(staged), true);
    check_particle_widths(read_file(ordinary), false);
    if (read_file(source) != original)
      throw std::runtime_error("Width staging modified its source model.");
  }
  std::cout << "CALCHEP_ZERO_WIDTH_TABLE_FORMATS_PASS\n";
}
} // namespace

// argv[1] contains the repository's independently selected CalcHEP model tables:
// model 1 is the full SM and model 2 is massive-electron QED.
int main(int argc, char* argv[]) {
  if (argc != 2) {
    std::cerr << "Usage: calchep-particle-restriction-tests MODEL_DIRECTORY\n";
    return 64;
  }
  try {
    const std::string model = argv[1];
    test_width_table_staging();
    const std::string original_particles = read_file(model + "/prtcls1.mdl");
    hepcat_spinas::calchep_detail::Workspace staged_workspace(false);
    const std::string staged_model = hepcat_spinas::calchep_detail::stage_model(
      model, 1, staged_workspace.path());
    check_particle_widths(read_file(staged_model + "/prtcls1.mdl"), true);
    if (read_file(model + "/prtcls1.mdl") != original_particles) return 11;
    {
      const std::string original_functions = read_file(model + "/func1.mdl");
      const std::vector<hepcat_spinas::CalcHEPParameter> masses = {
        {"Mb", 4.2}, {"Mc", 1.3}, {"Ms", 0.095}, {"CW", 0.9}};
      const auto promoted = hepcat_spinas::calchep_detail::promote_private_mass_inputs(
          staged_model, masses, {"Mb", "Mc", "Ms", "CW"});
      const std::string functions = read_file(staged_model + "/func1.mdl");
      const std::string variables = read_file(staged_model + "/vars1.mdl");
      if (promoted.size() != 3 || functions.find("MbEff(") != std::string::npos ||
          functions.find("McEff(") != std::string::npos || functions.find("MqEff(") != std::string::npos ||
          functions.find("sqrt(1-SW^2)") == std::string::npos ||
          variables.find("Matched independent HEPCAT mass") == std::string::npos ||
          read_file(model + "/func1.mdl") != original_functions) return 14;
      // Repeating the matching step must not duplicate an already promoted input.
      if (!hepcat_spinas::calchep_detail::promote_private_mass_inputs(
            staged_model, masses, {"Mb", "Mc", "Ms"}).empty()) return 15;
      std::cout << "CALCHEP_PRIVATE_PRIMITIVE_MASS_SCHEME_PASS\n";
    }

    const std::vector<int> external = {11, -11, 11, -11};
    const std::vector<hepcat_spinas::CalcHEPParameter> parameters = {{"Me",1.0},{"EE",1.0/3.0}};
    const double momentum = std::sqrt(10000.0-1.0);
    const std::array<std::array<double,4>,4> point = {{{{100,0,0,momentum}},{{100,0,0,-momentum}},
      {{100,momentum*std::sqrt(0.91),0,0.3*momentum}},{{100,-momentum*std::sqrt(0.91),0,-0.3*momentum}}}};
    double sm, restricted, qed;
    {
      hepcat_spinas::CalcHEPProcess process(model,1,external,parameters);
      if (!process.excluded_particle_names().empty()) return 1;
      sm = process.squared_amplitude(point);
    }
    {
      hepcat_spinas::CalcHEPProcess process(model,1,external,parameters,1.0,false,{11,22});
      const auto& excluded = process.excluded_particle_names();
      if (std::find(excluded.begin(),excluded.end(),"Z") == excluded.end() ||
          std::find(excluded.begin(),excluded.end(),"h") == excluded.end() ||
          std::find(excluded.begin(),excluded.end(),"e") != excluded.end() ||
          std::find(excluded.begin(),excluded.end(),"E") != excluded.end() ||
          std::find(excluded.begin(),excluded.end(),"A") != excluded.end()) return 2;
      restricted = process.squared_amplitude(point);
      std::cout << "EXCLUDED_INTERNAL_TYPES=" << excluded.size() << '\n';
    }
    {
      hepcat_spinas::CalcHEPProcess process(model,2,external,parameters);
      qed = process.squared_amplitude(point);
    }
    {
      // The benchmark must compare optimized kernels without modifying the
      // selected installation or keeping compiler flags in the environment.
      const std::string original_directory = calchepDir;
      const char* original_flags_pointer = std::getenv("CFLAGS");
      const bool flags_present = original_flags_pointer != NULL;
      const std::string original_flags = flags_present ? original_flags_pointer : "";
      hepcat_spinas::CalcHEPProcess process(model, 2, external, parameters,
          1.0, false, {11, 22}, 3);
      const double optimized = process.squared_amplitude(point);
      const char* current_flags = std::getenv("CFLAGS");
      if (!std::isfinite(optimized) || std::abs(optimized - qed) > 1e-10 * std::abs(qed) ||
          std::string(calchepDir) != original_directory ||
          (current_flags != NULL) != flags_present ||
          (current_flags && current_flags != original_flags)) return 9;
      std::ifstream flags((process.work_directory() + "/optimized-calchep/FlagsForSh").c_str());
      const std::string flags_text((std::istreambuf_iterator<char>(flags)), std::istreambuf_iterator<char>());
      if (flags_text.find("CFLAGS=\"$CFLAGS -O3\"") == std::string::npos) return 10;
      std::cout << "CALCHEP_PRIVATE_OPTIMIZATION_PASS\n";
    }
    {
      // Native constraints and every dependent Yukawa remain tied to the
      // explicitly matched masses after private numerical-scheme conversion.
      auto values = parameters;
      values.push_back({"Mb", 4.2}); values.push_back({"Mc", 1.3}); values.push_back({"Ms", 0.095});
      hepcat_spinas::CalcHEPProcess process(model, 1, {11,-11,5,-5}, values,
          1.0, false, {}, 0, {"Mb", "Mc", "Ms"});
      if (process.promoted_mass_parameters().size() != 3 ||
          process.mass_scheme_summary().find("Mb=") == std::string::npos ||
          std::abs(process.masses()[2] - 4.2L) > 1e-12) return 16;
      for (const auto& mass : process.promoted_mass_parameters()) {
        double actual;
        if (findVal(mass.name.c_str(), &actual) ||
            !hepcat_spinas::calchep_detail::parameter_agrees(actual, mass.value)) return 17;
      }
      std::cout << "CALCHEP_PRIVATE_MASS_CONSTRAINT_RECOMPUTATION_PASS\n";
    }
    {
      // A deliberately enormous Z width in an independent private model must
      // not affect a zero-width validation. Keep the normal model untouched.
      using namespace hepcat_spinas::calchep_detail;
      Workspace fixture(false);
      const char* prefixes[] = {"vars", "func", "prtcls", "lgrng", "extlib"};
      const char* kinds[] = {"Variables", "Constraints", "Particles", "Lagrangian", "Libraries"};
      for (unsigned index = 0; index < 5; ++index)
        copy_model_table(model + "/" + prefixes[index] + "1.mdl",
          fixture.path() + "/" + prefixes[index] + "1.mdl", kinds[index], index == 4);
      const std::string particles_file = fixture.path() + "/prtcls1.mdl";
      std::string particles = read_file(particles_file);
      const auto z_width = particles.find("!wZ");
      if (z_width == std::string::npos) return 12;
      particles.replace(z_width, 3, " tW");
      { std::ofstream file(particles_file.c_str()); file << particles; }
      { std::ofstream file((fixture.path() + "/vars1.mdl").c_str(), std::ios::app);
        file << "tW      |123.0        |Width sentinel for validation only\n"; }
      double huge_width, zero_width;
      {
        auto values = parameters; values.push_back({"tW",123});
        hepcat_spinas::CalcHEPProcess process(fixture.path(),1,external,values);
        huge_width = process.squared_amplitude(point);
        check_particle_widths(read_file(process.work_directory() + "/input_model/prtcls1.mdl"), true);
      }
      {
        auto values = parameters; values.push_back({"tW",0});
        hepcat_spinas::CalcHEPProcess process(fixture.path(),1,external,values);
        zero_width = process.squared_amplitude(point);
      }
      if (read_file(particles_file) != particles ||
          read_file(model + "/prtcls1.mdl") != original_particles ||
          !std::isfinite(huge_width) ||
          std::abs(huge_width-zero_width) > 1e-12*std::abs(zero_width) ||
          std::abs(huge_width-sm) > 1e-12*std::abs(sm)) return 13;
      std::cout << "CALCHEP_ZERO_WIDTH_SM_Z_PASS\n";
    }
    std::cout << std::setprecision(17) << "SM=" << sm << "\nRESTRICTED_SM=" << restricted
              << "\nPURE_QED=" << qed << '\n';
    if (!std::isfinite(sm) || !std::isfinite(restricted) || !std::isfinite(qed) ||
        std::abs(restricted-qed) > 1e-10 * std::abs(qed) ||
        std::abs(sm-qed) < 1e-5 * std::abs(qed)) return 3;
    bool unknown_rejected = false;
    try { hepcat_spinas::CalcHEPProcess process(model,1,external,parameters,1.0,false,{11,22,990001}); }
    catch (const std::invalid_argument& error) {
      unknown_rejected = std::string(error.what()).find("990001") != std::string::npos;
      std::cout << "UNKNOWN_TYPE_DIAGNOSTIC=" << error.what() << '\n';
    }
    if (!unknown_rejected) return 4;

    // CalcHEP orders the photon before the outgoing fermions for this model.
    // Compare the same physical event in three requested external-leg orders.
    // The two incoming particles must never be matched to outgoing occurrences.
    const double beam = std::sqrt(99.0);
    const double transverse = std::sqrt(39.0);
    const std::array<std::array<double, 4>, 5> radiative_point = {{
      {{10, 0, 0, beam}}, {{10, 0, 0, -beam}},
      {{7, -3, transverse, 0}}, {{7, -3, -transverse, 0}}, {{6, 6, 0, 0}}
    }};
    double requested_order, photon_first, reversed_beams;
    {
      hepcat_spinas::CalcHEPProcess process(model, 2, {11, -11, 11, -11, 22}, parameters);
      if (process.masses() != std::vector<long double>({1, 1, 1, 1, 0})) return 6;
      requested_order = process.squared_amplitude(radiative_point);
    }
    {
      hepcat_spinas::CalcHEPProcess process(model, 2, {11, -11, 22, 11, -11}, parameters);
      if (process.masses() != std::vector<long double>({1, 1, 0, 1, 1})) return 7;
      const std::array<std::array<double, 4>, 5> point = {{
        radiative_point[0], radiative_point[1], radiative_point[4],
        radiative_point[2], radiative_point[3]
      }};
      photon_first = process.squared_amplitude(point);
    }
    {
      hepcat_spinas::CalcHEPProcess process(model, 2, {-11, 11, 11, -11, 22}, parameters);
      const std::array<std::array<double, 4>, 5> point = {{
        radiative_point[1], radiative_point[0], radiative_point[2],
        radiative_point[3], radiative_point[4]
      }};
      reversed_beams = process.squared_amplitude(point);
    }
    if (!std::isfinite(requested_order) || !std::isfinite(photon_first) ||
        !std::isfinite(reversed_beams) || requested_order <= 0 ||
        std::abs(requested_order - photon_first) > 1e-10 * requested_order ||
        std::abs(requested_order - reversed_beams) > 1e-10 * requested_order) return 8;
    std::cout << "CALCHEP_EXTERNAL_ORDER_PASS\n";
    std::cout << "CALCHEP_PARTICLE_RESTRICTION_PASS\n";
    return 0;
  } catch (const std::exception& error) {
    std::cerr << error.what() << '\n';
    return 5;
  }
}
