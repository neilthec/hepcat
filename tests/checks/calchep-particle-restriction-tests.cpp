#include "calchep-runtime.hpp"
#include <iostream>
#include <iomanip>

// argv[1] contains the repository's independently selected CalcHEP model tables:
// model 1 is the full SM and model 2 is massive-electron QED.
int main(int argc, char* argv[]) {
  if (argc != 2) {
    std::cerr << "Usage: calchep-particle-restriction-tests MODEL_DIRECTORY\n";
    return 64;
  }
  try {
    const std::string model = argv[1];
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
