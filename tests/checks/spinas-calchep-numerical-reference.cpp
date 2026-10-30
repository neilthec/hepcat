// Independent direct-callback and angular-integration checks for the exported
// SPINAS backend. No CalcHEP symbolic calculation is involved.
#include "process.hpp"
extern "C" {
#include "num_out.h"
#include "VandP.h"
}
#include <atomic>
#include <cmath>
#include <cstdlib>
#include <iomanip>
#include <iostream>
#include <limits>
#include <stdexcept>
#include <string>
#include <thread>
#include <vector>

#ifndef HEPCAT_TEST_MASS_INDEX
#define HEPCAT_TEST_MASS_INDEX 0
#endif
#ifndef HEPCAT_TEST_CHARGE_INDEX
#define HEPCAT_TEST_CHARGE_INDEX 1
#endif

using Process = hepcat_generated::numerical_bridge;
static unsigned checks = 0;

static void require(bool condition, const char* label) {
  ++checks;
  if (!condition) throw std::runtime_error(label);
}

static bool close(ldouble a, ldouble b, ldouble tolerance = 2e-10L) {
  return std::isfinite(a) && std::isfinite(b)
    && std::abs(a-b) <= tolerance * std::max(std::abs(a), std::abs(b)) + 1e-23L;
}

static void set_model(const char* name, double value) {
  for (int i=0; i<nModelVars; ++i) {
    if (std::string(varNames[i]) == name) {
      varValues[i] = value;
      require(calcMainFunc() == 0, "model constraints rejected a valid parameter change");
      require(interface_ext.calcFunc() == 0, "process parameters rejected a valid parameter change");
      return;
    }
  }
  throw std::runtime_error(std::string("missing independent parameter: ") + name);
}

static Process direct_process(ldouble mass, ldouble charge) {
  auto values = Process::default_parameters();
  values[HEPCAT_TEST_MASS_INDEX] = mass;
  values[HEPCAT_TEST_CHARGE_INDEX] = charge;
  return Process(values);
}

static Process::Momenta momenta(ldouble mass, ldouble beam, ldouble cosine) {
  const ldouble energy = std::sqrt(mass*mass + beam*beam);
  const ldouble sine = std::sqrt(std::max(ldouble(0), 1-cosine*cosine));
  return {{{{energy, 0, 0, beam}}, {{energy, 0, 0, -beam}},
    {{energy, energy*sine, 0, energy*cosine}},
    {{energy, -energy*sine, 0, -energy*cosine}}}};
}

static double backend(const Process::Momenta& point, int* error) {
  REAL flat[4*Process::leg_count];
  for (std::size_t leg=0; leg<Process::leg_count; ++leg)
    for (std::size_t component=0; component<4; ++component)
      flat[4*leg+component] = static_cast<REAL>(point[leg][component]);
  return interface_ext.sqme(1, 0, flat, nullptr, error);
}

static ldouble quadrature(ldouble mass, ldouble charge, ldouble beam) {
  auto process = direct_process(mass, charge);
  const unsigned intervals = 2000;
  ldouble integral = 0;
  for (unsigned i=0; i<=intervals; ++i) {
    const ldouble cosine = -1 + ldouble(2)*i/intervals;
    process.set_momenta(momenta(mass, beam, cosine));
    const ldouble weight = (i==0 || i==intervals) ? 1 : (i%2==0 ? 2 : 4);
    integral += weight * process.amp2();
  }
  integral *= ldouble(2)/(ldouble(3)*intervals);
  const ldouble energy = std::sqrt(mass*mass+beam*beam);
  const ldouble s = 4*energy*energy;
  const ldouble beta = beam/energy;
  return integral * ldouble(389379660.0L)/(32*std::acos(ldouble(-1))*s*beta);
}

int main(int argc, char** argv) {
  try {
    if (argc == 5 && std::string(argv[1]) == "quadrature") {
      const ldouble mass = std::strtold(argv[2], nullptr);
      const ldouble charge = std::strtold(argv[3], nullptr);
      const ldouble beam = std::strtold(argv[4], nullptr);
      std::cout << std::setprecision(18) << "CROSS_SECTION_PB="
        << quadrature(mass, charge, beam) << '\n';
      return 0;
    }
    require(PtrInterface_ext == &interface_ext, "numerical interface pointer");
    require(interface_ext.nin==2 && interface_ext.nout==2 && interface_ext.nprc==1,
      "external process multiplicity");
    require(interface_ext.cb && interface_ext.cb[0].pow==0,
      "uncolored backend must not invent color channels");
    const bool derived = argc==2 && std::string(argv[1])=="derived";
    const char* mass_input = derived ? "massInput" : "Me";
    const char* charge_input = derived ? "chargeInput" : "EE";
    set_model(mass_input, derived ? 0.5 : 1);
    set_model(charge_input, derived ? 1.0/6 : 1.0/3);
    const auto first = momenta(1, 1, 0.37L);
    auto direct = direct_process(1, ldouble(1)/3);
    direct.set_momenta(first);
    int error = 0;
    const double original = backend(first, &error);
    require(error==0 && close(original, direct.amp2()),
      "backend agrees with the exported spin sum without an extra symmetry factor");
    REAL mass = 0;
    int pdg = 0;
    require(interface_ext.pinf(1,1,&mass,&pdg)!=nullptr && close(mass,1) && pdg==11,
      "particle metadata has the current electron mass and PDG");
    set_model(charge_input, derived ? 1.0/3 : 2.0/3);
    const double doubled_charge = backend(first, &error);
    require(error==0 && close(doubled_charge, 16*original),
      "changing an independent coupling updates the backend without rebuilding");
    set_model(mass_input, derived ? 0.75 : 1.5);
    require(interface_ext.pinf(1,1,&mass,&pdg)!=nullptr && close(mass,1.5),
      "mass metadata follows a changed independent model parameter");
    const auto changed = momenta(1.5L, 1, -0.23L);
    direct = direct_process(1.5L, ldouble(2)/3);
    direct.set_momenta(changed);
    require(close(backend(changed,&error),direct.amp2()) && error==0,
      "mass-dependent spinors and propagators update without rebuilding");
    const ldouble expected = direct.amp2();
    std::atomic<bool> threads_ok(true);
    std::vector<std::thread> threads;
    for (unsigned thread=0; thread<4; ++thread) {
      threads.emplace_back([&] {
        for (unsigned iteration=0; iteration<40; ++iteration) {
          int local_error=0;
          if (!close(backend(changed,&local_error),expected) || local_error)
            threads_ok.store(false);
        }
      });
    }
    for (auto& thread:threads) thread.join();
    require(threads_ok.load(), "concurrent VEGAS callbacks keep separate SPINAS caches");
    REAL flat[4*Process::leg_count] = {};
    error=0;
    const double invalid = interface_ext.sqme(2,0,flat,nullptr,&error);
    require(error!=0 && !std::isfinite(invalid), "an invalid subprocess is an explicit callback error");
    std::cout << "CALCHEP_NUMERICAL_CALLBACK_CHECKS=" << checks << " FAILURES=0\n";
    return 0;
  } catch (const std::exception& error) {
    std::cerr << "Numerical backend check failed: " << error.what() << '\n';
    return 1;
  }
}
