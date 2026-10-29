#ifndef HEPCAT_SPINAS_RUNTIME_HPP
#define HEPCAT_SPINAS_RUNTIME_HPP

// Runtime helpers copied into standalone HEPCAT-generated SPINAS projects.
// Four-momenta here are physical, future directed, with metric (+,-,-,-).
// Crossing the analytic all-ingoing expression is the exporter's responsibility.

#include "spinas.h"
#include <algorithm>
#include <array>
#include <cmath>
#include <cstddef>
#include <initializer_list>
#include <limits>
#include <stdexcept>
#include <string>
#include <vector>

namespace hepcat_spinas {

using FourMomentum = std::array<ldouble, 4>;
template <std::size_t N> using Momenta = std::array<FourMomentum, N>;

// Lorentz algebra, including composite momenta that need not be on shell.
inline ldouble dot(const FourMomentum& first, const FourMomentum& second) {
  return first[0]*second[0] - first[1]*second[1]
       - first[2]*second[2] - first[3]*second[3];
}
inline ldouble mass2(const FourMomentum& momentum) {
  return dot(momentum, momentum);
}
inline bool finite_complex(const cdouble& value) {
  return std::isfinite(std::real(value)) && std::isfinite(std::imag(value));
}
inline ldouble checked_mass(const cdouble& value) {
  const ldouble real=std::real(value);
  const ldouble imaginary=std::imag(value);
  const ldouble tolerance=64*std::numeric_limits<ldouble>::epsilon()
                         *std::max(ldouble(1), std::abs(real));
  if (!finite_complex(value) || real < 0 || std::abs(imaginary) > tolerance)
    throw std::invalid_argument("External particle masses must be finite, real and nonnegative.");
  return real;
}
inline cdouble matrix_trace(const spinas::cmatrix& matrix) {
  return matrix.get(0,0)+matrix.get(1,1);
}
inline spinas::cmatrix momentum_matrix(const FourMomentum& momentum,
                                      const bool upper) {
  return spinas::cmatrix(momentum.data(), upper);
}
template <std::size_t N>
FourMomentum sum_momenta(const Momenta<N>& momenta,
                        std::initializer_list<int> signed_labels) {
  FourMomentum result = {{0, 0, 0, 0}};
  for (const int label : signed_labels) {
    if (label == 0 || label == std::numeric_limits<int>::min())
      throw std::invalid_argument("Momentum labels must be signed one-based indices.");
    const std::size_t index = static_cast<std::size_t>(label > 0 ? label-1 : -label-1);
    if (index >= N)
      throw std::out_of_range("Momentum label lies outside this process.");
    const ldouble sign = label > 0 ? 1 : -1;
    for (std::size_t component=0; component<4; ++component)
      result[component] += sign*momenta[index][component];
  }
  return result;
}

// A normalized spin-j state is the symmetric tensor of rank 2j fundamental
// spinors. Each distinct assignment contributes 1/sqrt(binomial(2j,j+m)).
inline void check_spin(const unsigned rank, const int double_spin) {
  if (rank > static_cast<unsigned>(std::numeric_limits<int>::max()) ||
      double_spin < -static_cast<int>(rank) || double_spin > static_cast<int>(rank) ||
      (static_cast<long long>(rank) + double_spin)%2 != 0)
    throw std::invalid_argument("Invalid doubled spin for the declared spinor rank.");
}
inline ldouble spinor_normalization(const unsigned rank, const int double_spin) {
  check_spin(rank, double_spin);
  unsigned plus = static_cast<unsigned>((static_cast<long long>(rank) + double_spin)/2);
  plus = std::min(plus, rank-plus);
  ldouble combinations = 1;
  for (unsigned index=1; index<=plus; ++index)
    combinations *= static_cast<ldouble>(rank-plus+index)/static_cast<ldouble>(index);
  return 1/std::sqrt(combinations);
}
inline std::vector<std::vector<int> > spinor_assignments(const unsigned rank,
                                                        const int double_spin) {
  check_spin(rank, double_spin);
  const unsigned plus = static_cast<unsigned>(
    (static_cast<long long>(rank) + double_spin)/2);
  std::vector<int> spins(rank, -1);
  std::fill(spins.end()-plus, spins.end(), 1);
  std::vector<std::vector<int> > result;
  do { result.push_back(spins); } while (std::next_permutation(spins.begin(), spins.end()));
  return result;
}

// Input and output checks use both absolute and scale-normalized errors.
struct MomentumValidation {
  bool valid;
  ldouble maximum_mass_shell_error;
  ldouble maximum_conservation_error;
  ldouble maximum_scaled_error;
  std::string message;
};

template <std::size_t N>
void check_external_masses(const std::array<ldouble, N>& masses,
                           const std::array<std::size_t, 2>& incoming) {
  if (N < 4 || incoming[0] >= N || incoming[1] >= N || incoming[0] == incoming[1])
    throw std::invalid_argument("Examples require at least four legs and two distinct incoming labels.");
  for (std::size_t index=0; index<N; ++index)
    if (!std::isfinite(masses[index]) || masses[index] < 0)
      throw std::invalid_argument("External masses must be finite and nonnegative.");
}

template <std::size_t N>
MomentumValidation validate_momenta(const Momenta<N>& momenta,
                                    const std::array<ldouble, N>& masses,
                                    const std::array<std::size_t, 2>& incoming,
                                    const ldouble tolerance=1e-10L) {
  MomentumValidation result = {true, 0, 0, 0, "PASS"};
  check_external_masses(masses, incoming);
  if (!std::isfinite(tolerance) || tolerance <= 0)
    throw std::invalid_argument("The momentum tolerance must be positive and finite.");
  FourMomentum balance = {{0, 0, 0, 0}};
  ldouble energy_scale = 1;
  for (std::size_t index=0; index<N; ++index) {
    for (const ldouble value : momenta[index])
      if (!std::isfinite(value)) {
        result.valid = false;
        result.message = "A momentum component is not finite.";
        return result;
      }
    if (momenta[index][0] <= 0) {
      result.valid = false;
      result.message = "Physical external energies must be positive.";
      return result;
    }
    const ldouble invariant=mass2(momenta[index]);
    const ldouble squared_mass=masses[index]*masses[index];
    const ldouble squared_energy=momenta[index][0]*momenta[index][0];
    if (!std::isfinite(invariant) || !std::isfinite(squared_mass) || !std::isfinite(squared_energy)) {
      result.valid=false;
      result.message="Mass-shell arithmetic overflowed or became nonfinite.";
      return result;
    }
    const ldouble error = std::abs(invariant-squared_mass);
    const ldouble shell_scale = std::max(ldouble(1),
      std::max(squared_energy, squared_mass));
    result.maximum_mass_shell_error = std::max(result.maximum_mass_shell_error, error);
    result.maximum_scaled_error = std::max(result.maximum_scaled_error, error/shell_scale);
    const bool initial = index == incoming[0] || index == incoming[1];
    const ldouble sign = initial ? 1 : -1;
    if (initial) energy_scale += momenta[index][0];
    for (std::size_t component=0; component<4; ++component)
      balance[component] += sign*momenta[index][component];
  }
  for (const ldouble value : balance)
    result.maximum_conservation_error = std::max(result.maximum_conservation_error,
                                                 std::abs(value));
  result.maximum_scaled_error = std::max(result.maximum_scaled_error,
    result.maximum_conservation_error/energy_scale);
  result.valid = result.maximum_scaled_error <= tolerance;
  if (!result.valid) result.message = "External mass shells or momentum conservation failed.";
  return result;
}

namespace detail {
inline ldouble fractional_part(const ldouble number) {
  return number-std::floor(number);
}
inline ldouble two_body_momentum(const ldouble mass, const ldouble first_mass,
                                 const ldouble second_mass) {
  const ldouble sum = first_mass+second_mass;
  const ldouble difference = first_mass-second_mass;
  const ldouble radicand = (mass-sum)*(mass+sum)*(mass-difference)*(mass+difference);
  const ldouble scale = std::max(ldouble(1), mass*mass*mass*mass);
  if (mass <= 0 || radicand < -128*std::numeric_limits<ldouble>::epsilon()*scale)
    throw std::invalid_argument("A requested two-body decay is below threshold.");
  return std::sqrt(std::max(ldouble(0), radicand))/(2*mass);
}
inline FourMomentum boost_from_rest(const FourMomentum& momentum,
                                    const FourMomentum& frame,
                                    const ldouble frame_mass) {
  if (frame_mass <= 0 || frame[0] <= 0)
    throw std::invalid_argument("A timelike parent is required for a rest-frame boost.");
  const ldouble spatial_dot = frame[1]*momentum[1] + frame[2]*momentum[2]
                            + frame[3]*momentum[3];
  FourMomentum result;
  result[0] = (frame[0]*momentum[0]+spatial_dot)/frame_mass;
  const ldouble factor = momentum[0]/frame_mass
                      + spatial_dot/(frame_mass*(frame[0]+frame_mass));
  for (std::size_t component=1; component<4; ++component)
    result[component] = momentum[component] + factor*frame[component];
  return result;
}
}

// Deterministic examples, not a weighted phase-space integration algorithm.
// Sequential two-body decays work for arbitrary external masses and any 2->N.
template <std::size_t N>
ldouble example_center_of_mass_energy(const std::array<ldouble, N>& masses,
                                      const std::array<std::size_t, 2>& incoming) {
  check_external_masses(masses, incoming);
  ldouble outgoing_threshold=0;
  for (std::size_t index=0; index<N; ++index)
    if (index != incoming[0] && index != incoming[1]) outgoing_threshold += masses[index];
  const ldouble incoming_threshold = masses[incoming[0]]+masses[incoming[1]];
  return 3*std::max(ldouble(1), std::max(incoming_threshold, outgoing_threshold));
}

template <std::size_t N>
Momenta<N> make_example_momenta(const std::array<ldouble, N>& masses,
                               const std::array<std::size_t, 2>& incoming,
                               const ldouble sqrt_s, const unsigned point_index) {
  check_external_masses(masses, incoming);
  if (!std::isfinite(sqrt_s) || sqrt_s <= 0)
    throw std::invalid_argument("The center-of-mass energy must be positive and finite.");
  std::vector<std::size_t> outgoing;
  ldouble outgoing_threshold=0;
  for (std::size_t index=0; index<N; ++index)
    if (index != incoming[0] && index != incoming[1]) {
      outgoing.push_back(index); outgoing_threshold += masses[index];
    }
  const ldouble incoming_threshold = masses[incoming[0]]+masses[incoming[1]];
  if (sqrt_s <= std::max(incoming_threshold, outgoing_threshold))
    throw std::invalid_argument("The example energy must exceed both external thresholds.");

  Momenta<N> result;
  const ldouble initial_p = detail::two_body_momentum(sqrt_s,
    masses[incoming[0]], masses[incoming[1]]);
  result[incoming[0]] = {{std::sqrt(initial_p*initial_p+masses[incoming[0]]*masses[incoming[0]]),
                          0, 0, initial_p}};
  result[incoming[1]] = {{std::sqrt(initial_p*initial_p+masses[incoming[1]]*masses[incoming[1]]),
                          0, 0, -initial_p}};
  FourMomentum frame = {{sqrt_s, 0, 0, 0}};
  ldouble frame_mass=sqrt_s;
  const ldouble pi=std::acos(ldouble(-1));
  for (std::size_t step=0; step+1<outgoing.size(); ++step) {
    const std::size_t leg=outgoing[step];
    ldouble remaining_threshold=0;
    for (std::size_t rest=step+1; rest<outgoing.size(); ++rest)
      remaining_threshold += masses[outgoing[rest]];
    ldouble remaining_mass;
    if (step+2 == outgoing.size()) remaining_mass=masses[outgoing.back()];
    else {
      const ldouble fraction=0.28L+0.38L*detail::fractional_part(
        0.6180339887498948482L*(point_index+1)+0.4142135623730950488L*(step+1));
      remaining_mass=remaining_threshold
        +fraction*(frame_mass-masses[leg]-remaining_threshold);
    }
    const ldouble magnitude=detail::two_body_momentum(frame_mass,masses[leg],remaining_mass);
    const ldouble cosine=0.8L*(2*detail::fractional_part(
      0.7548776662466927600L*(point_index+1)+0.5698402909980532659L*(step+1))-1);
    const ldouble sine=std::sqrt(std::max(ldouble(0),1-cosine*cosine));
    const ldouble azimuth=2*pi*detail::fractional_part(
      0.6180339887498948482L*(point_index+1)+0.7320508075688772935L*(step+1));
    const FourMomentum rest_first={{std::sqrt(magnitude*magnitude+masses[leg]*masses[leg]),
      magnitude*sine*std::cos(azimuth),magnitude*sine*std::sin(azimuth),magnitude*cosine}};
    const FourMomentum rest_remaining={{std::sqrt(magnitude*magnitude+remaining_mass*remaining_mass),
      -rest_first[1],-rest_first[2],-rest_first[3]}};
    result[leg]=detail::boost_from_rest(rest_first,frame,frame_mass);
    frame=detail::boost_from_rest(rest_remaining,frame,frame_mass);
    frame_mass=remaining_mass;
  }
  result[outgoing.back()]=frame;
  const MomentumValidation validation=validate_momenta(result,masses,incoming);
  if (!validation.valid)
    throw std::runtime_error("The generated example failed its kinematic checks: "+validation.message);
  return result;
}

} // namespace hepcat_spinas
#endif
