// Numerical MadEvent kernel. The independent MadGraph validator uses its own amplitude.
#include "process.hpp"
#include <cmath>
#include <memory>
#include <string>

namespace {
using Process = hepcat_generated::experimentaleebarZZh;
// Each numerical worker owns its parameters, particles and spinor-product cache.
thread_local std::unique_ptr<Process> process;
thread_local Process::Parameters previous_parameters;
thread_local Process::Momenta previous_momenta;
thread_local bool momenta_ready = false;
thread_local std::string last_error;
const std::array<std::size_t, Process::leg_count> leg_order = {{0, 1, 2, 3, 4}};
const std::array<int, Process::leg_count> spin2 = {{1, 1, 2, 2, 0}};
const std::array<bool, Process::leg_count> massless = {{false, false, false, false, false}};
const std::array<Process::Spins, 1> selected_helicities = {{
  Process::Spins{{0, 0, 0, 0, 0}}
}};

bool selected(const Process::Spins& spins) {
  for (const auto& helicities : selected_helicities) {
    bool match = true;
    for (std::size_t leg = 0; leg < Process::leg_count; ++leg)
      if (massless[leg] && spins[leg] != helicities[leg]) match = false;
    if (match) return true;
  }
  return false;
}
} // namespace

extern "C" int hepcat_madevent_leg_count() { return static_cast<int>(Process::leg_count); }
extern "C" int hepcat_madevent_parameter_count() { return static_cast<int>(Process::parameter_count); }
extern "C" const char* hepcat_madevent_error() { return last_error.c_str(); }

// MadEvent supplies physical momenta in incoming-then-outgoing order.
// The analytic exporter has already crossed outgoing momenta and spinors once.
// Return raw |amp|^2; MadEvent performs the spin average and identical-state factor.
extern "C" int hepcat_madevent_matrix(const double* momenta, const int* helicities,
                                      const double* parameters, double* answer) {
  last_error.clear();
  if (answer) *answer = 0;
  try {
    if (!momenta || !helicities || !answer || (Process::parameter_count && !parameters))
      throw std::invalid_argument("Missing MadEvent matrix-element input.");
    Process::Parameters values;
    for (std::size_t parameter = 0; parameter < Process::parameter_count; ++parameter) {
      if (!std::isfinite(parameters[parameter])) throw std::invalid_argument("Nonfinite MadEvent model parameter.");
      values[parameter] = cdouble(parameters[parameter], 0);
    }
    if (!process) {
      process.reset(new Process(values)); previous_parameters = values; momenta_ready = false;
    } else if (values != previous_parameters) {
      process->set_parameters(values); previous_parameters = values; momenta_ready = false;
    }
    Process::Momenta physical;
    Process::Spins spins;
    for (std::size_t mg_leg = 0; mg_leg < Process::leg_count; ++mg_leg) {
      const std::size_t leg = leg_order[mg_leg];
      const int helicity = helicities[mg_leg];
      if ((spin2[leg] == 0 && helicity != 0) ||
          (spin2[leg] == 1 && helicity != -1 && helicity != 1) ||
          (spin2[leg] == 2 && helicity != -1 && helicity != 1 && (massless[leg] || helicity != 0)))
        throw std::invalid_argument("Invalid MadEvent external helicity.");
      spins[leg] = spin2[leg] == 2 ? 2*helicity : helicity;
      // Convert HELAS outgoing-vector labels back to native SPINAS labels.
      if (massless[leg] && spin2[leg] == 2 && mg_leg >= Process::incoming_legs().size())
        spins[leg] = -spins[leg];
      if ((process->masses()[leg] == 0) != massless[leg])
        throw std::invalid_argument("An external massless/massive category changed; regenerate this process.");
      for (std::size_t component = 0; component < 4; ++component) {
        const double value = momenta[4*mg_leg + component];
        if (!std::isfinite(value)) throw std::invalid_argument("Nonfinite MadEvent momentum.");
        physical[leg][component] = value;
      }
    }
    // A fixed massless helicity is a selected contribution, not an extra average.
    if (!selected(spins)) return 0;
    // MadEvent visits many spins at one phase-space point. Reuse all products.
    if (!momenta_ready || physical != previous_momenta) {
      process->set_momenta(physical); previous_momenta = physical; momenta_ready = true;
    }
    const ldouble probability = std::norm(process->amp(spins));
    *answer = static_cast<double>(probability);
    if (!std::isfinite(*answer) || *answer < 0) throw std::runtime_error("Nonfinite HEPCAT squared amplitude.");
    return 0;
  } catch (const std::exception& error) { last_error = error.what(); }
  catch (...) { last_error = "Unknown HEPCAT numerical matrix-element error."; }
  // A failed update must not leave a partially rebound process in the next call.
  process.reset(); momenta_ready = false;
  if (answer) *answer = 0;
  return 1;
}
