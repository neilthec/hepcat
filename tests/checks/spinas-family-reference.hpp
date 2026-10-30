#pragma once

// Independent spin/helicity enumeration and native SPINAS reference values.
#include <algorithm>
#include <array>
#include <cmath>
#include <complex>
#include <functional>
#include <iostream>
#include <stdexcept>
#include <vector>
#include "spinas.h"
#include "SM/include/eeAA.h"
#include "SM/include/AAee.h"

namespace hepcat_family_reference {

inline bool close(cdouble actual, cdouble expected, const char* label) {
  const ldouble error = std::abs(actual - expected);
  const ldouble scale = std::max(std::abs(actual), std::abs(expected));
  if (!std::isfinite(std::real(actual)) || !std::isfinite(std::imag(actual)) ||
      error > ldouble(3.e-10L) * scale + ldouble(1.e-24L)) {
    std::cerr << label << ": actual=" << actual << " expected=" << expected << '\n';
    return false;
  }
  return true;
}

template<std::size_t N>
using Momenta = std::array<std::array<ldouble, 4>, N>;

template<std::size_t N>
std::array<spinas::particle, N> particles(const Momenta<N>& point,
                                        const std::array<ldouble, N>& masses) {
  std::array<spinas::particle, N> result;
  for (std::size_t i = 0; i < N; ++i) {
    result[i] = spinas::particle(masses[i]);
    result[i].set_momentum(point[i].data());
  }
  return result;
}

template<std::size_t N> struct oracle;

template<> struct oracle<4> {
  static cdouble value(const Momenta<4>& point, const std::array<ldouble,4>& masses,
                       const std::array<int,4>& spins, int kind,
                       cdouble plus, cdouble minus) {
    if (kind == 1) {
      spinas::eeAA native(ldouble(1.L)/ldouble(3.L), masses[0]);
      native.set_momenta(point[0].data(), point[1].data(), point[2].data(), point[3].data());
      // The supplied model has the opposite mixed-helicity overall sign.
      // Preserve it, as in the existing single-amplitude export regression.
      const int sign = spins[2] == spins[3] ? 1 : -1;
      return ldouble(sign) * native.amp(spins[0], spins[1], spins[2], spins[3]);
    }
    if (kind == 2) {
      spinas::AAee native(ldouble(1.L)/ldouble(3.L), masses[2]);
      native.set_momenta(point[0].data(), point[1].data(), point[2].data(), point[3].data());
      return native.amp(spins[0], spins[1], spins[2], spins[3]);
    }
    if (kind == 7) return cdouble(0,0);
    auto p = particles(point, masses);
    if (kind == 5) {
      if (spins[2] > 0) {
        spinas::sproduct first(ANGLE, &p[0], &p[1], &p[2]);
        spinas::sproduct second(SQUARE, &p[1], &p[2]);
        return plus * first.v(spins[0]) * second.v(spins[1]);
      }
      spinas::sproduct first(SQUARE, &p[0], &p[1], &p[2]);
      spinas::sproduct second(ANGLE, &p[1], &p[2]);
      // Both outgoing angle endpoints reverse, so their signs cancel.
      return minus * first.v(spins[0]) * second.v(spins[1]);
    }
    if (kind == 6) {
      if (spins[2] > 0 && spins[3] > 0) {
        spinas::sproduct square(SQUARE, &p[2], &p[3]);
        return square.v();
      }
      if (spins[2] < 0 && spins[3] < 0) {
        spinas::sproduct angle(ANGLE, &p[2], &p[3]);
        return angle.v();
      }
      if (spins[2] > 0) {
        spinas::sproduct chain(ANGLE, &p[3], &p[0], &p[2]);
        return -chain.v();
      }
      spinas::sproduct chain(ANGLE, &p[2], &p[0], &p[3]);
      // The identical-fermion permutation and outgoing angle each give minus.
      return chain.v();
    }
    throw std::runtime_error("Unknown four-leg family oracle");
  }
};

template<> struct oracle<5> {
  static cdouble value(const Momenta<5>&, const std::array<ldouble,5>&,
                       const std::array<int,5>&, int, cdouble, cdouble) {
    throw std::runtime_error("Five-leg fixture checks independent enumeration only");
  }
};

template<> struct oracle<6> {
  static cdouble value(const Momenta<6>& point, const std::array<ldouble,6>& masses,
                       const std::array<int,6>& spins, int kind, cdouble, cdouble) {
    if (kind != 4) throw std::runtime_error("Unknown six-leg family oracle");
    auto p = particles(point, masses);
    spinas::sproduct a12(ANGLE, &p[0], &p[1]);
    spinas::sproduct a34(ANGLE, &p[2], &p[3]);
    spinas::sproduct a56(ANGLE, &p[4], &p[5]);
    // Four outgoing angle endpoints give an even crossing sign.
    return a12.v(spins[0],spins[1]) * a34.v(spins[2],spins[3]) *
      a56.v(spins[4],spins[5]) / ldouble(81.L);
  }
};

template<std::size_t N> struct positional;
template<> struct positional<4> {
  template<class Process> static cdouble amp(Process& p, const typename Process::Spins& s) {
    return p.amp(s[0],s[1],s[2],s[3]);
  }
};
template<> struct positional<5> {
  template<class Process> static cdouble amp(Process& p, const typename Process::Spins& s) {
    return p.amp(s[0],s[1],s[2],s[3],s[4]);
  }
};
template<> struct positional<6> {
  template<class Process> static cdouble amp(Process& p, const typename Process::Spins& s) {
    return p.amp(s[0],s[1],s[2],s[3],s[4],s[5]);
  }
};

template<class Process>
int run(int kind, const std::array<int,Process::leg_count>& ranks,
        unsigned expected_helicities, ldouble initial_average,
        ldouble final_divisor, int plus_index = -1, int minus_index = -1) {
  constexpr std::size_t N = Process::leg_count;
  Process process;
  if (Process::helicity_count != expected_helicities ||
      Process::helicity_configurations().size() != expected_helicities)
    throw std::runtime_error("The emitted family does not cover every helicity");
  if (!close(cdouble(Process::incoming_spin_average(),0),cdouble(initial_average,0),"initial average") ||
      !close(cdouble(Process::symmetry_factor(),0),cdouble(1/final_divisor,0),"symmetry factor")) return 1;
  bool unready_rejected = false;
  try { process.amp(Process::default_spins()); }
  catch (const std::logic_error&) { unready_rejected = true; }
  if (!unready_rejected) throw std::runtime_error("amp accepted unset momenta");
  const auto incoming = Process::incoming_legs();
  const ldouble energy = hepcat_spinas::example_center_of_mass_energy(process.masses(), incoming);
  unsigned comparisons = 0;
  for (unsigned sample=0; sample<3; ++sample) {
    const unsigned parameter_trials = kind == 5 ? 2 : 1;
    for (unsigned trial=0; trial<parameter_trials; ++trial) {
      auto parameters = Process::default_parameters();
      cdouble plus(ldouble(1.L)/ldouble(3.L),ldouble(1.L)/ldouble(5.L));
      cdouble minus(ldouble(2.L)/ldouble(7.L),-ldouble(1.L)/ldouble(11.L));
      if (trial != 0) {
        plus=cdouble(2,-ldouble(1.L)/ldouble(3.L));
        minus=cdouble(-4,ldouble(1.L)/ldouble(7.L));
        parameters.at(std::size_t(plus_index))=plus;
        parameters.at(std::size_t(minus_index))=minus;
      }
      process.set_parameters(parameters);
      const auto point = hepcat_spinas::make_example_momenta(process.masses(), incoming, energy, sample);
      process.set_momenta(point);
      typename Process::Spins spins{};
      ldouble total = 0;
      unsigned states = 0;
      std::function<bool(std::size_t)> enumerate = [&](std::size_t leg) -> bool {
        if (leg == N) {
          const cdouble calculated = process.amp(spins);
          const cdouble expected = kind == 3 ? calculated :
            oracle<N>::value(point,process.masses(),spins,kind,plus,minus);
          if (kind == 2) {
            // Crossing-dependent overall phases do not enter an unpolarized sum.
            if (!close(cdouble(std::norm(calculated),0),cdouble(std::norm(expected),0),"incoming photon norm")) return false;
          } else if (!close(calculated,expected,"complex family amplitude")) return false;
          if (!close(positional<N>::amp(process,spins),calculated,"positional amp")) return false;
          total += std::norm(expected); ++states; ++comparisons;
          return true;
        }
        if (ranks[leg] == 0) { spins[leg]=0; return enumerate(leg+1); }
        if (process.masses()[leg] == 0) {
          for (const int spin : {-ranks[leg],ranks[leg]}) {
            spins[leg]=spin; if (!enumerate(leg+1)) return false;
          }
        } else {
          for (int spin=-ranks[leg];spin<=ranks[leg];spin+=2) {
            spins[leg]=spin; if (!enumerate(leg+1)) return false;
          }
        }
        return true;
      };
      if (!enumerate(0)) return 1;
      unsigned expected_states=1;
      for (std::size_t leg=0;leg<N;++leg)
        expected_states *= ranks[leg] == 0 ? 1 : (process.masses()[leg] == 0 ? 2 : ranks[leg]+1);
      if (states != expected_states) throw std::runtime_error("Incomplete independent spin enumeration");
      if (!close(cdouble(process.spin_sum(),0),cdouble(total,0),"raw family spin sum") ||
          !close(cdouble(process.amp2(),0),cdouble(total/(initial_average*final_divisor),0),"averaged symmetric amp2")) return 1;
      auto bad = Process::default_spins();
      const auto bad_leg=std::find_if(ranks.begin(),ranks.end(),[](int rank) { return rank>0; });
      bad[std::size_t(bad_leg-ranks.begin())]=0;
      bool bad_rejected=false;
      try { process.amp(bad); } catch (const std::invalid_argument&) { bad_rejected=true; }
      if (!bad_rejected) throw std::runtime_error("Invalid spin/helicity accepted");
    }
  }
  std::cout << "FAMILY_COMPARISONS=" << comparisons << '\n';
  return 0;
}

} // namespace hepcat_family_reference
