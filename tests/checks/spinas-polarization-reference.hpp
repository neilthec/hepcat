#pragma once

// Independent state enumeration for partially polarized generated processes.
#include <algorithm>
#include <cmath>
#include <complex>
#include <functional>
#include <iostream>
#include <stdexcept>
#include "spinas.h"
#include "SM/include/eeAA.h"

namespace hepcat_polarization_reference {

inline bool close(cdouble actual, cdouble expected, const char* label) {
  const ldouble scale = std::max(std::abs(actual), std::abs(expected));
  if (!std::isfinite(std::real(actual)) || !std::isfinite(std::imag(actual)) ||
      std::abs(actual - expected) > ldouble(4.e-10L)*scale + ldouble(1.e-24L)) {
    std::cerr << label << ": actual=" << actual << " expected=" << expected << '\n';
    return false;
  }
  return true;
}

template<class Process, class Reference>
bool compare(unsigned helicities, unsigned average, unsigned divisor,
             bool native_eeAA = false) {
  Process process; Reference reference;
  if (Process::helicity_count != helicities ||
      !close(cdouble(Process::incoming_spin_average(),0),cdouble(average,0),"incoming average") ||
      !close(cdouble(Process::symmetry_factor(),0),cdouble(ldouble(1)/divisor,0),"symmetry factor")) return false;
  const auto incoming = Process::incoming_legs();
  const auto energy = hepcat_spinas::example_center_of_mass_energy(process.masses(),incoming);
  for (unsigned sample=0;sample<3;++sample) {
    const auto point = hepcat_spinas::make_example_momenta(process.masses(),incoming,energy,sample);
    process.set_momenta(point); reference.set_momenta(point);
    const auto default_value = process.amp(Process::default_spins());
    if (!hepcat_spinas::finite_complex(default_value)) return false;
    ldouble raw=0;
    for (const auto& configuration : Process::helicity_configurations()) {
      typename Process::Spins spins=configuration;
      std::function<bool(std::size_t)> enumerate = [&](std::size_t leg) -> bool {
        if (leg == Process::leg_count) {
          const auto value = process.amp(spins);
          if (!close(value,reference.amp(spins),"explicit-role/fixed complex amplitude")) return false;
          if (native_eeAA) {
            spinas::eeAA native(ldouble(1)/ldouble(3),process.masses()[0]);
            native.set_momenta(point[0].data(),point[1].data(),point[2].data(),point[3].data());
            // Preserve the existing model's mixed-helicity sign convention.
            const int sign = spins[2] == spins[3] ? 1 : -1;
            if (!close(value,ldouble(sign)*native.amp(spins[0],spins[1],spins[2],spins[3]),
                       "native complex polarized amplitude")) return false;
          }
          raw+=std::norm(value); return true;
        }
        if (process.masses()[leg] == 0) return enumerate(leg+1);
        for (const int spin : {-1,1}) {
          spins[leg]=spin; if (!enumerate(leg+1)) return false;
        }
        return true;
      };
      if (!enumerate(0)) return false;
    }
    if (!close(cdouble(process.spin_sum(),0),cdouble(raw,0),"restricted raw spin sum") ||
        !close(cdouble(process.amp2(),0),cdouble(raw/(average*divisor),0),"restricted normalized spin sum")) return false;
  }
  return true;
}

template<class All, class Plus, class Minus>
bool recombine(bool incoming_fixed) {
  All all; Plus plus; Minus minus;
  const auto incoming=All::incoming_legs();
  const auto energy=hepcat_spinas::example_center_of_mass_energy(all.masses(),incoming);
  for (unsigned sample=0;sample<3;++sample) {
    const auto point=hepcat_spinas::make_example_momenta(all.masses(),incoming,energy,sample);
    all.set_momenta(point); plus.set_momenta(point); minus.set_momenta(point);
    if (!close(cdouble(all.spin_sum(),0),cdouble(plus.spin_sum()+minus.spin_sum(),0),"raw helicity recombination")) return false;
    const auto normalized=(plus.amp2()+minus.amp2())/(incoming_fixed ? ldouble(2) : ldouble(1));
    if (!close(cdouble(all.amp2(),0),cdouble(normalized,0),"normalized helicity recombination")) return false;
  }
  return true;
}

} // namespace hepcat_polarization_reference
