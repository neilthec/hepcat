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
    std::cout << "CALCHEP_PARTICLE_RESTRICTION_PASS\n";
    return 0;
  } catch (const std::exception& error) {
    std::cerr << error.what() << '\n';
    return 5;
  }
}
