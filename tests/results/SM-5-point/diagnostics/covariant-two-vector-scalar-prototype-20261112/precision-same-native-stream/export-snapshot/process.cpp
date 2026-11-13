#include "process.hpp"
#include <algorithm>
#include <cmath>
#include <stdexcept>

namespace hepcat_generated {
using namespace hepcat_spinas;

constexpr std::size_t experimentaleebarZZh::leg_count;
constexpr std::size_t experimentaleebarZZh::parameter_count;
constexpr std::size_t experimentaleebarZZh::helicity_count;


experimentaleebarZZh::experimentaleebarZZh() : experimentaleebarZZh(default_parameters()) {}

experimentaleebarZZh::experimentaleebarZZh(const Parameters& values) {
  set_parameters(values);
}

experimentaleebarZZh::experimentaleebarZZh(const ldouble& value_EE, const ldouble& value_SW, const ldouble&
  value_MW, const ldouble& value_Me, const ldouble& value_MZ, const ldouble& value_Mh)
  : experimentaleebarZZh(Parameters{{cdouble(value_Me), cdouble(value_MZ), cdouble(value_Mh), cdouble(value_EE),
    cdouble(value_SW), cdouble(value_MW)}}) {}

// Copies rebuild caches, so every sproduct points into its own process.
experimentaleebarZZh::experimentaleebarZZh(const experimentaleebarZZh& other) {
  widths_enabled = other.widths_enabled;
  set_parameters(other.parameters);
  if (other.momenta_ready) set_momenta(other.momenta);
}

experimentaleebarZZh& experimentaleebarZZh::operator=(const experimentaleebarZZh& other) {
  if (this != &other) {
    widths_enabled = other.widths_enabled;
    set_parameters(other.parameters);
    if (other.momenta_ready) set_momenta(other.momenta);
  }
  return *this;
}

experimentaleebarZZh::Parameters experimentaleebarZZh::default_parameters() {
  return Parameters{{
    cdouble(ldouble(5.0e-4L), ldouble(0.0L)), // "Me"
    cdouble(ldouble(9.12921430831531714430205e1L), ldouble(0.0L)), // "MZ"
    cdouble(ldouble(1.25e2L), ldouble(0.0L)), // "Mh"
    cdouble(ldouble(3.1333e-1L), ldouble(0.0L)), // "EE"
    cdouble(ldouble(4.74e-1L), ldouble(0.0L)), // "SW"
    cdouble(ldouble(8.0385e1L), ldouble(0.0L)) // "MW"
  }};
}

experimentaleebarZZh::Spins experimentaleebarZZh::default_spins() {
  return Spins{{-1, -1, -2, -2, 0}};
}

std::array<std::size_t, 2> experimentaleebarZZh::incoming_legs() {
  return {{0, 1}};
}

std::array<experimentaleebarZZh::Spins, experimentaleebarZZh::helicity_count>
experimentaleebarZZh::helicity_configurations() {
  return std::array<Spins, helicity_count>{{
    {{0, 0, 0, 0, 0}}
  }};
}

ldouble experimentaleebarZZh::incoming_spin_average() {
  return ldouble(4.0e0L);
}

ldouble experimentaleebarZZh::symmetry_factor() {
  return ldouble(5.0e-1L);
}

void experimentaleebarZZh::initialize_products() {
#ifdef SPINAS_HAS_MOMENTUM_CHAIN
  momentum_chain_1 = spinas::momentum_chain(::ANGLE, {&p4, &p3});
  momentum_chain_2 = spinas::momentum_chain(::ANGLE, {&p5, &p1});
  momentum_chain_3 = spinas::momentum_chain(::ANGLE, {&p3, &p1});
  momentum_chain_4 = spinas::momentum_chain(::ANGLE, {&p3, &p4});
  momentum_chain_5 = spinas::momentum_chain(::ANGLE, {&p2, &p4});
  momentum_chain_6 = spinas::momentum_chain(::ANGLE, {&p2, &p5});
  momentum_chain_7 = spinas::momentum_chain(::SQUARE, {&p4, &p3});
  momentum_chain_8 = spinas::momentum_chain(::SQUARE, {&p5, &p2});
  momentum_chain_9 = spinas::momentum_chain(::SQUARE, {&p5, &p3});
  momentum_chain_10 = spinas::momentum_chain(::SQUARE, {&p3, &p4});
  momentum_chain_11 = spinas::momentum_chain(::SQUARE, {&p5, &p4});
  momentum_chain_12 = spinas::momentum_chain(::SQUARE, {&p4, &p1});
  momentum_chain_13 = spinas::momentum_chain(::SQUARE, {&p5, &p1});
  momentum_chain_14 = spinas::momentum_chain(::SQUARE, {&p3, &p1});
  momentum_chain_15 = spinas::momentum_chain(::SQUARE, {&p2, &p1});
#endif
  a34a = spinas::sproduct(::ANGLE, &p3, &p4);
  s14s = spinas::sproduct(::SQUARE, &p1, &p4);
  s23s = spinas::sproduct(::SQUARE, &p2, &p3);
  a14a = spinas::sproduct(::ANGLE, &p1, &p4);
  a23a = spinas::sproduct(::ANGLE, &p2, &p3);
  s34s = spinas::sproduct(::SQUARE, &p3, &p4);
  s24s = spinas::sproduct(::SQUARE, &p2, &p4);
  s31s = spinas::sproduct(::SQUARE, &p3, &p1);
  a21a = spinas::sproduct(::ANGLE, &p2, &p1);
  s21s = spinas::sproduct(::SQUARE, &p2, &p1);
  a13a = spinas::sproduct(::ANGLE, &p1, &p3);
  a24a = spinas::sproduct(::ANGLE, &p2, &p4);
  s43s = spinas::sproduct(::SQUARE, &p4, &p3);
  a114s = spinas::sproduct(::ANGLE, &p1, &p1, &p4);
  a154s = spinas::sproduct(::ANGLE, &p1, &p5, &p4);
  a211s = spinas::sproduct(::ANGLE, &p2, &p1, &p1);
  a251s = spinas::sproduct(::ANGLE, &p2, &p5, &p1);
  a221s = spinas::sproduct(::ANGLE, &p2, &p2, &p1);
  a223s = spinas::sproduct(::ANGLE, &p2, &p2, &p3);
  a253s = spinas::sproduct(::ANGLE, &p2, &p5, &p3);
  a224s = spinas::sproduct(::ANGLE, &p2, &p2, &p4);
  a254s = spinas::sproduct(::ANGLE, &p2, &p5, &p4);
  a41a = spinas::sproduct(::ANGLE, &p4, &p1);
  a314s = spinas::sproduct(::ANGLE, &p3, &p1, &p4);
  a344s = spinas::sproduct(::ANGLE, &p3, &p4, &p4);
  a113s = spinas::sproduct(::ANGLE, &p1, &p1, &p3);
  a153s = spinas::sproduct(::ANGLE, &p1, &p5, &p3);
  a324s = spinas::sproduct(::ANGLE, &p3, &p2, &p4);
  a134s = spinas::sproduct(::ANGLE, &p1, &p3, &p4);
  a333s = spinas::sproduct(::ANGLE, &p3, &p3, &p3);
  a353s = spinas::sproduct(::ANGLE, &p3, &p5, &p3);
  a234s = spinas::sproduct(::ANGLE, &p2, &p3, &p4);
  s13s = spinas::sproduct(::SQUARE, &p1, &p3);
  a334s = spinas::sproduct(::ANGLE, &p3, &p3, &p4);
  a31a = spinas::sproduct(::ANGLE, &p3, &p1);
  a413s = spinas::sproduct(::ANGLE, &p4, &p1, &p3);
  a433s = spinas::sproduct(::ANGLE, &p4, &p3, &p3);
  a423s = spinas::sproduct(::ANGLE, &p4, &p2, &p3);
  a411s = spinas::sproduct(::ANGLE, &p4, &p1, &p1);
  a451s = spinas::sproduct(::ANGLE, &p4, &p5, &p1);
  a434s = spinas::sproduct(::ANGLE, &p4, &p3, &p4);
  a454s = spinas::sproduct(::ANGLE, &p4, &p5, &p4);
  a143s = spinas::sproduct(::ANGLE, &p1, &p4, &p3);
  a444s = spinas::sproduct(::ANGLE, &p4, &p4, &p4);
  a243s = spinas::sproduct(::ANGLE, &p2, &p4, &p3);
  a343s = spinas::sproduct(::ANGLE, &p3, &p4, &p3);
  a341s = spinas::sproduct(::ANGLE, &p3, &p4, &p1);
  a351s = spinas::sproduct(::ANGLE, &p3, &p5, &p1);
  a443s = spinas::sproduct(::ANGLE, &p4, &p4, &p3);
  a311s = spinas::sproduct(::ANGLE, &p3, &p1, &p1);
  a431s = spinas::sproduct(::ANGLE, &p4, &p3, &p1);
  s113a = spinas::sproduct(::SQUARE, &p1, &p1, &p3);
  s153a = spinas::sproduct(::SQUARE, &p1, &p5, &p3);
  s114a = spinas::sproduct(::SQUARE, &p1, &p1, &p4);
  s154a = spinas::sproduct(::SQUARE, &p1, &p5, &p4);
  s211a = spinas::sproduct(::SQUARE, &p2, &p1, &p1);
  s251a = spinas::sproduct(::SQUARE, &p2, &p5, &p1);
  s221a = spinas::sproduct(::SQUARE, &p2, &p2, &p1);
  s223a = spinas::sproduct(::SQUARE, &p2, &p2, &p3);
  s253a = spinas::sproduct(::SQUARE, &p2, &p5, &p3);
  s243a = spinas::sproduct(::SQUARE, &p2, &p4, &p3);
  s224a = spinas::sproduct(::SQUARE, &p2, &p2, &p4);
  s254a = spinas::sproduct(::SQUARE, &p2, &p5, &p4);
  s234a = spinas::sproduct(::SQUARE, &p2, &p3, &p4);
  s311a = spinas::sproduct(::SQUARE, &p3, &p1, &p1);
  s351a = spinas::sproduct(::SQUARE, &p3, &p5, &p1);
  a2413a = spinas::sproduct(::ANGLE, &p2, &p4, &p1, &p3);
  a2433a = spinas::sproduct(::ANGLE, &p2, &p4, &p3, &p3);
  a2513a = spinas::sproduct(::ANGLE, &p2, &p5, &p1, &p3);
  a2533a = spinas::sproduct(::ANGLE, &p2, &p5, &p3, &p3);
  a2314a = spinas::sproduct(::ANGLE, &p2, &p3, &p1, &p4);
  a2344a = spinas::sproduct(::ANGLE, &p2, &p3, &p4, &p4);
  a2514a = spinas::sproduct(::ANGLE, &p2, &p5, &p1, &p4);
  a2544a = spinas::sproduct(::ANGLE, &p2, &p5, &p4, &p4);
  a3214a = spinas::sproduct(::ANGLE, &p3, &p2, &p1, &p4);
  a3244a = spinas::sproduct(::ANGLE, &p3, &p2, &p4, &p4);
  a3314a = spinas::sproduct(::ANGLE, &p3, &p3, &p1, &p4);
  a3344a = spinas::sproduct(::ANGLE, &p3, &p3, &p4, &p4);
  a3124a = spinas::sproduct(::ANGLE, &p3, &p1, &p2, &p4);
  a3144a = spinas::sproduct(::ANGLE, &p3, &p1, &p4, &p4);
  a3324a = spinas::sproduct(::ANGLE, &p3, &p3, &p2, &p4);
  a3241a = spinas::sproduct(::ANGLE, &p3, &p2, &p4, &p1);
  a3251a = spinas::sproduct(::ANGLE, &p3, &p2, &p5, &p1);
  a3341a = spinas::sproduct(::ANGLE, &p3, &p3, &p4, &p1);
  a3351a = spinas::sproduct(::ANGLE, &p3, &p3, &p5, &p1);
  a4231a = spinas::sproduct(::ANGLE, &p4, &p2, &p3, &p1);
  a4251a = spinas::sproduct(::ANGLE, &p4, &p2, &p5, &p1);
  a4431a = spinas::sproduct(::ANGLE, &p4, &p4, &p3, &p1);
  a4451a = spinas::sproduct(::ANGLE, &p4, &p4, &p5, &p1);
  s1423s = spinas::sproduct(::SQUARE, &p1, &p4, &p2, &p3);
  s1433s = spinas::sproduct(::SQUARE, &p1, &p4, &p3, &p3);
  s1523s = spinas::sproduct(::SQUARE, &p1, &p5, &p2, &p3);
  s1533s = spinas::sproduct(::SQUARE, &p1, &p5, &p3, &p3);
  s1324s = spinas::sproduct(::SQUARE, &p1, &p3, &p2, &p4);
  s1344s = spinas::sproduct(::SQUARE, &p1, &p3, &p4, &p4);
  s1524s = spinas::sproduct(::SQUARE, &p1, &p5, &p2, &p4);
  s1544s = spinas::sproduct(::SQUARE, &p1, &p5, &p4, &p4);
  s2413s = spinas::sproduct(::SQUARE, &p2, &p4, &p1, &p3);
  s2433s = spinas::sproduct(::SQUARE, &p2, &p4, &p3, &p3);
  s2513s = spinas::sproduct(::SQUARE, &p2, &p5, &p1, &p3);
  s2533s = spinas::sproduct(::SQUARE, &p2, &p5, &p3, &p3);
  s2314s = spinas::sproduct(::SQUARE, &p2, &p3, &p1, &p4);
  s2344s = spinas::sproduct(::SQUARE, &p2, &p3, &p4, &p4);
  s2514s = spinas::sproduct(::SQUARE, &p2, &p5, &p1, &p4);
  s2544s = spinas::sproduct(::SQUARE, &p2, &p5, &p4, &p4);
  s3214s = spinas::sproduct(::SQUARE, &p3, &p2, &p1, &p4);
  s3244s = spinas::sproduct(::SQUARE, &p3, &p2, &p4, &p4);
  s3314s = spinas::sproduct(::SQUARE, &p3, &p3, &p1, &p4);
  s3344s = spinas::sproduct(::SQUARE, &p3, &p3, &p4, &p4);
  s4213s = spinas::sproduct(::SQUARE, &p4, &p2, &p1, &p3);
  s4233s = spinas::sproduct(::SQUARE, &p4, &p2, &p3, &p3);
  s4413s = spinas::sproduct(::SQUARE, &p4, &p4, &p1, &p3);
  s4433s = spinas::sproduct(::SQUARE, &p4, &p4, &p3, &p3);
}

void experimentaleebarZZh::set_parameters(const Parameters& values) {
  for (const auto& value : values) {
    if (!finite_complex(value)) {
      throw std::invalid_argument("Model parameters must be finite.");
    }
  }
  momenta_ready = false;
  all_point_cache_ready = false;
  summed_point_cache_ready = false;
  momentum_chain_1_ready = false;
  momentum_chain_2_ready = false;
  momentum_chain_3_ready = false;
  momentum_chain_4_ready = false;
  momentum_chain_5_ready = false;
  momentum_chain_6_ready = false;
  momentum_chain_7_ready = false;
  momentum_chain_8_ready = false;
  momentum_chain_9_ready = false;
  momentum_chain_10_ready = false;
  momentum_chain_11_ready = false;
  momentum_chain_12_ready = false;
  momentum_chain_13_ready = false;
  momentum_chain_14_ready = false;
  momentum_chain_15_ready = false;
  parameters = values;
  Me = parameters[0];
  MZ = parameters[1];
  Mh = parameters[2];
  EE = parameters[3];
  SW = parameters[4];
  MW = parameters[5];

  // Recompute model-only factors once after parameter changes.
  parameter_factor_1_2 = (EE * EE * EE);
  parameter_factor_2 = (Me * Me * Me);
  parameter_factor_3_3 = (SW * SW);
  parameter_factor_3_2 = ldouble(2.0L) * parameter_factor_3_3;
  parameter_factor_3 = (-ldouble(1.0L) + parameter_factor_3_2);
  parameter_factor_1 = parameter_factor_1_2 * parameter_factor_2 * parameter_factor_3;
  parameter_factor_7_2 = (MW * MW * MW);
  parameter_factor_7 = ldouble(2.0L) * parameter_factor_7_2 * SW;
  parameter_factor_9 = parameter_factor_1_2 * Me * parameter_factor_3;
  parameter_factor_10_2 = (ldouble(1.0L) - parameter_factor_3_3);
  parameter_factor_10 = MW * SW * parameter_factor_10_2;
  parameter_factor_12_2 = std::pow(cdouble(parameter_factor_10_2), ldouble(1.0L / 2.0L));
  parameter_factor_12 = (MW) / (parameter_factor_12_2);
  parameter_factor_14 = parameter_factor_1_2 * parameter_factor_2;
  parameter_factor_15_2 = (SW * SW * SW);
  parameter_factor_15 = ldouble(4.0L) * parameter_factor_7_2 * parameter_factor_15_2;
  parameter_factor_17 = parameter_factor_1_2 * Me;
  parameter_factor_18_2 = (Mh * Mh);
  parameter_factor_18 = ldouble(3.0L) * parameter_factor_1_2 * Me * parameter_factor_18_2;
  parameter_factor_20 = ldouble(2.0L) * MW * parameter_factor_15_2 * parameter_factor_10_2;
  parameter_factor_21_2 = (Me * Me);
  parameter_factor_21 = parameter_factor_1_2 * parameter_factor_21_2 * parameter_factor_3;
  parameter_factor_23 = parameter_factor_1_2 * parameter_factor_21_2;
  parameter_factor_24_2 = std::pow(cdouble(parameter_factor_3), 2);
  parameter_factor_24 = parameter_factor_1_2 * Me * parameter_factor_24_2;
  parameter_factor_26 = parameter_factor_1_2 * Me * SW;
  parameter_factor_27 = parameter_factor_1_2 * parameter_factor_21_2 * SW;
  parameter_factor_28 = ldouble(2.0L) * parameter_factor_1_2 * SW;
  parameter_factor_29 = MW * parameter_factor_10_2;
  parameter_factor_30 = parameter_factor_1_2 * parameter_factor_21_2 * parameter_factor_24_2;
  parameter_factor_31 = parameter_factor_1_2 * parameter_factor_24_2;
  parameter_factor_32 = parameter_factor_1_2 * SW;
  external_masses[0] = checked_mass(cdouble(Me));
  external_masses[1] = checked_mass(cdouble(Me));
  external_masses[2] = checked_mass(cdouble(MZ));
  external_masses[3] = checked_mass(cdouble(MZ));
  external_masses[4] = checked_mass(cdouble(Mh));
  if ((external_masses[0] == 0) != false) {
    throw std::invalid_argument(
      "Changing between massive and massless spin states requires re-exporting the process.");
  }
  p1 = spinas::particle(external_masses[0]);
  if ((external_masses[1] == 0) != false) {
    throw std::invalid_argument(
      "Changing between massive and massless spin states requires re-exporting the process.");
  }
  p2 = spinas::particle(external_masses[1]);
  if ((external_masses[2] == 0) != false) {
    throw std::invalid_argument(
      "Changing between massive and massless spin states requires re-exporting the process.");
  }
  p3 = spinas::particle(external_masses[2]);
  if ((external_masses[3] == 0) != false) {
    throw std::invalid_argument(
      "Changing between massive and massless spin states requires re-exporting the process.");
  }
  p4 = spinas::particle(external_masses[3]);
  p5 = spinas::particle(external_masses[4]);
  initialize_products();
}

void experimentaleebarZZh::set_widths_enabled(bool enabled) {
  if (widths_enabled == enabled) return;
  widths_enabled = enabled;
  momenta_ready = false;
  all_point_cache_ready = false;
  summed_point_cache_ready = false;
  momentum_chain_1_ready = false;
  momentum_chain_2_ready = false;
  momentum_chain_3_ready = false;
  momentum_chain_4_ready = false;
  momentum_chain_5_ready = false;
  momentum_chain_6_ready = false;
  momentum_chain_7_ready = false;
  momentum_chain_8_ready = false;
  momentum_chain_9_ready = false;
  momentum_chain_10_ready = false;
  momentum_chain_11_ready = false;
  momentum_chain_12_ready = false;
  momentum_chain_13_ready = false;
  momentum_chain_14_ready = false;
  momentum_chain_15_ready = false;
}

void experimentaleebarZZh::set_masses(const ldouble& value_Me, const ldouble& value_MZ, const ldouble& value_Mh)
  {
  Parameters values = parameters;
  values[0] = cdouble(value_Me);
  values[1] = cdouble(value_MZ);
  values[2] = cdouble(value_Mh);
  set_parameters(values);
}

void experimentaleebarZZh::set_momenta(const Momenta& values) {
  const auto validation = validate_momenta(values, external_masses, incoming_legs());
  if (!validation.valid) throw std::invalid_argument(validation.message);
  momenta_ready = false;
  all_point_cache_ready = false;
  summed_point_cache_ready = false;
  momentum_chain_1_ready = false;
  momentum_chain_2_ready = false;
  momentum_chain_3_ready = false;
  momentum_chain_4_ready = false;
  momentum_chain_5_ready = false;
  momentum_chain_6_ready = false;
  momentum_chain_7_ready = false;
  momentum_chain_8_ready = false;
  momentum_chain_9_ready = false;
  momentum_chain_10_ready = false;
  momentum_chain_11_ready = false;
  momentum_chain_12_ready = false;
  momentum_chain_13_ready = false;
  momentum_chain_14_ready = false;
  momentum_chain_15_ready = false;
  momenta = values;

  // External particle momenta.
  p1.set_momentum(momenta[0].data());
  p2.set_momentum(momenta[1].data());
  p3.set_momentum(momenta[2].data());
  p4.set_momentum(momenta[3].data());
  p5.set_momentum(momenta[4].data());

  // Propagator denominators and scalar invariants.
  t23 = ((std::imag(Me) == ldouble(0) && std::isfinite(std::real(Me))) ? cdouble(mass2(sum_momenta(momenta, {2,
    -3})) - (std::real(Me)) * (std::real(Me)), ldouble(0)) : (mass2(sum_momenta(momenta, {2, -3})) - (Me) *
    (Me)));
  t14 = ((std::imag(Me) == ldouble(0) && std::isfinite(std::real(Me))) ? cdouble(mass2(sum_momenta(momenta, {1,
    -4})) - (std::real(Me)) * (std::real(Me)), ldouble(0)) : (mass2(sum_momenta(momenta, {1, -4})) - (Me) *
    (Me)));
  t15 = ((std::imag(Me) == ldouble(0) && std::isfinite(std::real(Me))) ? cdouble(mass2(sum_momenta(momenta, {1,
    -5})) - (std::real(Me)) * (std::real(Me)), ldouble(0)) : (mass2(sum_momenta(momenta, {1, -5})) - (Me) *
    (Me)));
  t25 = ((std::imag(Me) == ldouble(0) && std::isfinite(std::real(Me))) ? cdouble(mass2(sum_momenta(momenta, {2,
    -5})) - (std::real(Me)) * (std::real(Me)), ldouble(0)) : (mass2(sum_momenta(momenta, {2, -5})) - (Me) *
    (Me)));
  t35 = mass2(sum_momenta(momenta, {3, 5})) - (parameter_factor_12) * (parameter_factor_12);
  t45 = mass2(sum_momenta(momenta, {4, 5})) - (parameter_factor_12) * (parameter_factor_12);
  t13 = ((std::imag(Me) == ldouble(0) && std::isfinite(std::real(Me))) ? cdouble(mass2(sum_momenta(momenta, {1,
    -3})) - (std::real(Me)) * (std::real(Me)), ldouble(0)) : (mass2(sum_momenta(momenta, {1, -3})) - (Me) *
    (Me)));
  t24 = ((std::imag(Me) == ldouble(0) && std::isfinite(std::real(Me))) ? cdouble(mass2(sum_momenta(momenta, {2,
    -4})) - (std::real(Me)) * (std::real(Me)), ldouble(0)) : (mass2(sum_momenta(momenta, {2, -4})) - (Me) *
    (Me)));
  t34 = ((std::imag(Mh) == ldouble(0) && std::isfinite(std::real(Mh))) ? cdouble(mass2(sum_momenta(momenta, {3,
    4})) - (std::real(Mh)) * (std::real(Mh)), ldouble(0)) : (mass2(sum_momenta(momenta, {3, 4})) - (Mh) *
    (Mh)));
  t12 = ((std::imag(Mh) == ldouble(0) && std::isfinite(std::real(Mh))) ? cdouble(mass2(sum_momenta(momenta, {1,
    2})) - (std::real(Mh)) * (std::real(Mh)), ldouble(0)) : (mass2(sum_momenta(momenta, {1, 2})) - (Mh) *
    (Mh)));
  momenta_ready = true;
}

// Prepare only data used by the retained helicity contributions.
void experimentaleebarZZh::prepare_summed_point_cache() {
  if (summed_point_cache_ready) return;
#ifdef SPINAS_HAS_MOMENTUM_CHAIN
  if (!momentum_chain_1_ready) {
    momentum_chain_1.update();
    momentum_chain_1_ready = true;
  }
  if (!momentum_chain_2_ready) {
    momentum_chain_2.update();
    momentum_chain_2_ready = true;
  }
  if (!momentum_chain_3_ready) {
    momentum_chain_3.update();
    momentum_chain_3_ready = true;
  }
  if (!momentum_chain_4_ready) {
    momentum_chain_4.update();
    momentum_chain_4_ready = true;
  }
  if (!momentum_chain_5_ready) {
    momentum_chain_5.update();
    momentum_chain_5_ready = true;
  }
  if (!momentum_chain_6_ready) {
    momentum_chain_6.update();
    momentum_chain_6_ready = true;
  }
  if (!momentum_chain_7_ready) {
    momentum_chain_7.update();
    momentum_chain_7_ready = true;
  }
  if (!momentum_chain_8_ready) {
    momentum_chain_8.update();
    momentum_chain_8_ready = true;
  }
  if (!momentum_chain_9_ready) {
    momentum_chain_9.update();
    momentum_chain_9_ready = true;
  }
  if (!momentum_chain_10_ready) {
    momentum_chain_10.update();
    momentum_chain_10_ready = true;
  }
  if (!momentum_chain_11_ready) {
    momentum_chain_11.update();
    momentum_chain_11_ready = true;
  }
  if (!momentum_chain_12_ready) {
    momentum_chain_12.update();
    momentum_chain_12_ready = true;
  }
  if (!momentum_chain_13_ready) {
    momentum_chain_13.update();
    momentum_chain_13_ready = true;
  }
  if (!momentum_chain_14_ready) {
    momentum_chain_14.update();
    momentum_chain_14_ready = true;
  }
  if (!momentum_chain_15_ready) {
    momentum_chain_15.update();
    momentum_chain_15_ready = true;
  }
#endif
  a113s.update();
  a114s.update();
  a134s.update();
  a13a.update();
  a143s.update();
  a14a.update();
  a153s.update();
  a154s.update();
  a211s.update();
  a21a.update();
  a221s.update();
  a223s.update();
  a224s.update();
  #ifdef SPINAS_HAS_MOMENTUM_CHAIN
  a2314a.update(momentum_chain_3);
#else
  a2314a.update();
#endif
  #ifdef SPINAS_HAS_MOMENTUM_CHAIN
  a2344a.update(momentum_chain_4);
#else
  a2344a.update();
#endif
  a234s.update();
  a23a.update();
  a2413a.update();
  #ifdef SPINAS_HAS_MOMENTUM_CHAIN
  a2433a.update(momentum_chain_1);
#else
  a2433a.update();
#endif
  a243s.update();
  a24a.update();
  #ifdef SPINAS_HAS_MOMENTUM_CHAIN
  a2513a.update(momentum_chain_2);
#else
  a2513a.update();
#endif
  #ifdef SPINAS_HAS_MOMENTUM_CHAIN
  a2514a.update(momentum_chain_2);
#else
  a2514a.update();
#endif
  a251s.update();
  a2533a.update();
  a253s.update();
  a2544a.update();
  a254s.update();
  a311s.update();
  a3124a.update();
  a3144a.update();
  a314s.update();
  a31a.update();
  a3214a.update();
  #ifdef SPINAS_HAS_MOMENTUM_CHAIN
  a3241a.update(momentum_chain_5);
#else
  a3241a.update();
#endif
  #ifdef SPINAS_HAS_MOMENTUM_CHAIN
  a3244a.update(momentum_chain_5);
#else
  a3244a.update();
#endif
  a324s.update();
  #ifdef SPINAS_HAS_MOMENTUM_CHAIN
  a3251a.update(momentum_chain_6);
#else
  a3251a.update();
#endif
  #ifdef SPINAS_HAS_MOMENTUM_CHAIN
  a3314a.update(momentum_chain_3);
#else
  a3314a.update();
#endif
  a3324a.update();
  a333s.update();
  #ifdef SPINAS_HAS_MOMENTUM_CHAIN
  a3341a.update(momentum_chain_4);
#else
  a3341a.update();
#endif
  #ifdef SPINAS_HAS_MOMENTUM_CHAIN
  a3344a.update(momentum_chain_4);
#else
  a3344a.update();
#endif
  a334s.update();
  a3351a.update();
  a341s.update();
  a343s.update();
  a344s.update();
  a34a.update();
  a351s.update();
  a353s.update();
  a411s.update();
  a413s.update();
  a41a.update();
  a4231a.update();
  a423s.update();
  #ifdef SPINAS_HAS_MOMENTUM_CHAIN
  a4251a.update(momentum_chain_6);
#else
  a4251a.update();
#endif
  a431s.update();
  a433s.update();
  a434s.update();
  #ifdef SPINAS_HAS_MOMENTUM_CHAIN
  a4431a.update(momentum_chain_1);
#else
  a4431a.update();
#endif
  a443s.update();
  a444s.update();
  a4451a.update();
  a451s.update();
  a454s.update();
  s113a.update();
  s114a.update();
  s1324s.update();
  #ifdef SPINAS_HAS_MOMENTUM_CHAIN
  s1344s.update(momentum_chain_10);
#else
  s1344s.update();
#endif
  s13s.update();
  s1423s.update();
  #ifdef SPINAS_HAS_MOMENTUM_CHAIN
  s1433s.update(momentum_chain_7);
#else
  s1433s.update();
#endif
  s14s.update();
  #ifdef SPINAS_HAS_MOMENTUM_CHAIN
  s1523s.update(momentum_chain_8);
#else
  s1523s.update();
#endif
  #ifdef SPINAS_HAS_MOMENTUM_CHAIN
  s1524s.update(momentum_chain_8);
#else
  s1524s.update();
#endif
  #ifdef SPINAS_HAS_MOMENTUM_CHAIN
  s1533s.update(momentum_chain_9);
#else
  s1533s.update();
#endif
  s153a.update();
  #ifdef SPINAS_HAS_MOMENTUM_CHAIN
  s1544s.update(momentum_chain_11);
#else
  s1544s.update();
#endif
  s154a.update();
  s211a.update();
  s21s.update();
  s221a.update();
  s223a.update();
  s224a.update();
  #ifdef SPINAS_HAS_MOMENTUM_CHAIN
  s2314s.update(momentum_chain_14);
#else
  s2314s.update();
#endif
  #ifdef SPINAS_HAS_MOMENTUM_CHAIN
  s2344s.update(momentum_chain_10);
#else
  s2344s.update();
#endif
  s234a.update();
  s23s.update();
  #ifdef SPINAS_HAS_MOMENTUM_CHAIN
  s2413s.update(momentum_chain_12);
#else
  s2413s.update();
#endif
  #ifdef SPINAS_HAS_MOMENTUM_CHAIN
  s2433s.update(momentum_chain_7);
#else
  s2433s.update();
#endif
  s243a.update();
  s24s.update();
  #ifdef SPINAS_HAS_MOMENTUM_CHAIN
  s2513s.update(momentum_chain_13);
#else
  s2513s.update();
#endif
  #ifdef SPINAS_HAS_MOMENTUM_CHAIN
  s2514s.update(momentum_chain_13);
#else
  s2514s.update();
#endif
  s251a.update();
  #ifdef SPINAS_HAS_MOMENTUM_CHAIN
  s2533s.update(momentum_chain_9);
#else
  s2533s.update();
#endif
  s253a.update();
  #ifdef SPINAS_HAS_MOMENTUM_CHAIN
  s2544s.update(momentum_chain_11);
#else
  s2544s.update();
#endif
  s254a.update();
  s311a.update();
  s31s.update();
  #ifdef SPINAS_HAS_MOMENTUM_CHAIN
  s3214s.update(momentum_chain_15);
#else
  s3214s.update();
#endif
  s3244s.update();
  #ifdef SPINAS_HAS_MOMENTUM_CHAIN
  s3314s.update(momentum_chain_14);
#else
  s3314s.update();
#endif
  #ifdef SPINAS_HAS_MOMENTUM_CHAIN
  s3344s.update(momentum_chain_10);
#else
  s3344s.update();
#endif
  s34s.update();
  s351a.update();
  #ifdef SPINAS_HAS_MOMENTUM_CHAIN
  s4213s.update(momentum_chain_15);
#else
  s4213s.update();
#endif
  s4233s.update();
  s43s.update();
  #ifdef SPINAS_HAS_MOMENTUM_CHAIN
  s4413s.update(momentum_chain_12);
#else
  s4413s.update();
#endif
  #ifdef SPINAS_HAS_MOMENTUM_CHAIN
  s4433s.update(momentum_chain_7);
#else
  s4433s.update();
#endif
  real_denominator_domain = std::imag(parameters[0]) == ldouble(0) && std::imag(parameters[1]) == ldouble(0) &&
    std::imag(parameters[2]) == ldouble(0) && std::imag(parameters[3]) == ldouble(0) && std::imag(parameters[4])
    == ldouble(0) && std::imag(parameters[5]) == ldouble(0);
  point_product_1[0][0] = a34a.v(-1, -1);
  point_product_1[0][1] = a34a.v(-1, 1);
  point_product_1[1][0] = a34a.v(1, -1);
  point_product_1[1][1] = a34a.v(1, 1);
  point_product_2[0][0] = s14s.v(-1, -1);
  point_product_2[0][1] = s14s.v(-1, 1);
  point_product_2[1][0] = s14s.v(1, -1);
  point_product_2[1][1] = s14s.v(1, 1);
  point_product_3[0][0] = s23s.v(-1, -1);
  point_product_3[0][1] = s23s.v(-1, 1);
  point_product_3[1][0] = s23s.v(1, -1);
  point_product_3[1][1] = s23s.v(1, 1);
  point_product_4[0][0] = a14a.v(-1, -1);
  point_product_4[0][1] = a14a.v(-1, 1);
  point_product_4[1][0] = a14a.v(1, -1);
  point_product_4[1][1] = a14a.v(1, 1);
  point_product_5[0][0] = a23a.v(-1, -1);
  point_product_5[0][1] = a23a.v(-1, 1);
  point_product_5[1][0] = a23a.v(1, -1);
  point_product_5[1][1] = a23a.v(1, 1);
  point_product_6[0][0] = s34s.v(-1, -1);
  point_product_6[0][1] = s34s.v(-1, 1);
  point_product_6[1][0] = s34s.v(1, -1);
  point_product_6[1][1] = s34s.v(1, 1);
  point_product_7[0][0] = s24s.v(-1, -1);
  point_product_7[0][1] = s24s.v(-1, 1);
  point_product_7[1][0] = s24s.v(1, -1);
  point_product_7[1][1] = s24s.v(1, 1);
  point_product_8[0][0] = s31s.v(-1, -1);
  point_product_8[0][1] = s31s.v(-1, 1);
  point_product_8[1][0] = s31s.v(1, -1);
  point_product_8[1][1] = s31s.v(1, 1);
  point_product_9[0][0] = a21a.v(-1, -1);
  point_product_9[0][1] = a21a.v(-1, 1);
  point_product_9[1][0] = a21a.v(1, -1);
  point_product_9[1][1] = a21a.v(1, 1);
  point_product_10[0][0] = s21s.v(-1, -1);
  point_product_10[0][1] = s21s.v(-1, 1);
  point_product_10[1][0] = s21s.v(1, -1);
  point_product_10[1][1] = s21s.v(1, 1);
  point_product_11[0][0] = a13a.v(-1, -1);
  point_product_11[0][1] = a13a.v(-1, 1);
  point_product_11[1][0] = a13a.v(1, -1);
  point_product_11[1][1] = a13a.v(1, 1);
  point_product_12[0][0] = a24a.v(-1, -1);
  point_product_12[0][1] = a24a.v(-1, 1);
  point_product_12[1][0] = a24a.v(1, -1);
  point_product_12[1][1] = a24a.v(1, 1);
  point_product_13[0][0] = s43s.v(-1, -1);
  point_product_13[0][1] = s43s.v(-1, 1);
  point_product_13[1][0] = s43s.v(1, -1);
  point_product_13[1][1] = s43s.v(1, 1);
  point_product_14[0][0] = a114s.v(-1, -1);
  point_product_14[0][1] = a114s.v(-1, 1);
  point_product_14[1][0] = a114s.v(1, -1);
  point_product_14[1][1] = a114s.v(1, 1);
  point_product_15[0][0] = a154s.v(-1, -1);
  point_product_15[0][1] = a154s.v(-1, 1);
  point_product_15[1][0] = a154s.v(1, -1);
  point_product_15[1][1] = a154s.v(1, 1);
  point_product_16[0][0] = a211s.v(-1, -1);
  point_product_16[0][1] = a211s.v(-1, 1);
  point_product_16[1][0] = a211s.v(1, -1);
  point_product_16[1][1] = a211s.v(1, 1);
  point_product_17[0][0] = a251s.v(-1, -1);
  point_product_17[0][1] = a251s.v(-1, 1);
  point_product_17[1][0] = a251s.v(1, -1);
  point_product_17[1][1] = a251s.v(1, 1);
  point_product_18[0][0] = a221s.v(-1, -1);
  point_product_18[0][1] = a221s.v(-1, 1);
  point_product_18[1][0] = a221s.v(1, -1);
  point_product_18[1][1] = a221s.v(1, 1);
  point_product_19[0][0] = a223s.v(-1, -1);
  point_product_19[0][1] = a223s.v(-1, 1);
  point_product_19[1][0] = a223s.v(1, -1);
  point_product_19[1][1] = a223s.v(1, 1);
  point_product_20[0][0] = a253s.v(-1, -1);
  point_product_20[0][1] = a253s.v(-1, 1);
  point_product_20[1][0] = a253s.v(1, -1);
  point_product_20[1][1] = a253s.v(1, 1);
  point_product_21[0][0] = a224s.v(-1, -1);
  point_product_21[0][1] = a224s.v(-1, 1);
  point_product_21[1][0] = a224s.v(1, -1);
  point_product_21[1][1] = a224s.v(1, 1);
  point_product_22[0][0] = a254s.v(-1, -1);
  point_product_22[0][1] = a254s.v(-1, 1);
  point_product_22[1][0] = a254s.v(1, -1);
  point_product_22[1][1] = a254s.v(1, 1);
  point_product_23[0][0] = a41a.v(-1, -1);
  point_product_23[0][1] = a41a.v(-1, 1);
  point_product_23[1][0] = a41a.v(1, -1);
  point_product_23[1][1] = a41a.v(1, 1);
  point_product_24[0][0] = a314s.v(-1, -1);
  point_product_24[0][1] = a314s.v(-1, 1);
  point_product_24[1][0] = a314s.v(1, -1);
  point_product_24[1][1] = a314s.v(1, 1);
  point_product_25[0][0] = a344s.v(-1, -1);
  point_product_25[0][1] = a344s.v(-1, 1);
  point_product_25[1][0] = a344s.v(1, -1);
  point_product_25[1][1] = a344s.v(1, 1);
  point_product_26[0][0] = a113s.v(-1, -1);
  point_product_26[0][1] = a113s.v(-1, 1);
  point_product_26[1][0] = a113s.v(1, -1);
  point_product_26[1][1] = a113s.v(1, 1);
  point_product_27[0][0] = a153s.v(-1, -1);
  point_product_27[0][1] = a153s.v(-1, 1);
  point_product_27[1][0] = a153s.v(1, -1);
  point_product_27[1][1] = a153s.v(1, 1);
  point_product_28[0][0] = a324s.v(-1, -1);
  point_product_28[0][1] = a324s.v(-1, 1);
  point_product_28[1][0] = a324s.v(1, -1);
  point_product_28[1][1] = a324s.v(1, 1);
  point_product_29[0][0] = a134s.v(-1, -1);
  point_product_29[0][1] = a134s.v(-1, 1);
  point_product_29[1][0] = a134s.v(1, -1);
  point_product_29[1][1] = a134s.v(1, 1);
  point_product_30[0][0] = a333s.v(-1, -1);
  point_product_30[0][1] = a333s.v(-1, 1);
  point_product_30[1][0] = a333s.v(1, -1);
  point_product_30[1][1] = a333s.v(1, 1);
  point_product_31[0][0] = a353s.v(-1, -1);
  point_product_31[0][1] = a353s.v(-1, 1);
  point_product_31[1][0] = a353s.v(1, -1);
  point_product_31[1][1] = a353s.v(1, 1);
  point_product_32[0][0] = a234s.v(-1, -1);
  point_product_32[0][1] = a234s.v(-1, 1);
  point_product_32[1][0] = a234s.v(1, -1);
  point_product_32[1][1] = a234s.v(1, 1);
  point_product_33[0][0] = s13s.v(-1, -1);
  point_product_33[0][1] = s13s.v(-1, 1);
  point_product_33[1][0] = s13s.v(1, -1);
  point_product_33[1][1] = s13s.v(1, 1);
  point_product_34[0][0] = a334s.v(-1, -1);
  point_product_34[0][1] = a334s.v(-1, 1);
  point_product_34[1][0] = a334s.v(1, -1);
  point_product_34[1][1] = a334s.v(1, 1);
  point_product_35[0][0] = a31a.v(-1, -1);
  point_product_35[0][1] = a31a.v(-1, 1);
  point_product_35[1][0] = a31a.v(1, -1);
  point_product_35[1][1] = a31a.v(1, 1);
  point_product_36[0][0] = a413s.v(-1, -1);
  point_product_36[0][1] = a413s.v(-1, 1);
  point_product_36[1][0] = a413s.v(1, -1);
  point_product_36[1][1] = a413s.v(1, 1);
  point_product_37[0][0] = a433s.v(-1, -1);
  point_product_37[0][1] = a433s.v(-1, 1);
  point_product_37[1][0] = a433s.v(1, -1);
  point_product_37[1][1] = a433s.v(1, 1);
  point_product_38[0][0] = a423s.v(-1, -1);
  point_product_38[0][1] = a423s.v(-1, 1);
  point_product_38[1][0] = a423s.v(1, -1);
  point_product_38[1][1] = a423s.v(1, 1);
  point_product_39[0][0] = a411s.v(-1, -1);
  point_product_39[0][1] = a411s.v(-1, 1);
  point_product_39[1][0] = a411s.v(1, -1);
  point_product_39[1][1] = a411s.v(1, 1);
  point_product_40[0][0] = a451s.v(-1, -1);
  point_product_40[0][1] = a451s.v(-1, 1);
  point_product_40[1][0] = a451s.v(1, -1);
  point_product_40[1][1] = a451s.v(1, 1);
  point_product_41[0][0] = a434s.v(-1, -1);
  point_product_41[0][1] = a434s.v(-1, 1);
  point_product_41[1][0] = a434s.v(1, -1);
  point_product_41[1][1] = a434s.v(1, 1);
  point_product_42[0][0] = a454s.v(-1, -1);
  point_product_42[0][1] = a454s.v(-1, 1);
  point_product_42[1][0] = a454s.v(1, -1);
  point_product_42[1][1] = a454s.v(1, 1);
  point_product_43[0][0] = a143s.v(-1, -1);
  point_product_43[0][1] = a143s.v(-1, 1);
  point_product_43[1][0] = a143s.v(1, -1);
  point_product_43[1][1] = a143s.v(1, 1);
  point_product_44[0][0] = a444s.v(-1, -1);
  point_product_44[0][1] = a444s.v(-1, 1);
  point_product_44[1][0] = a444s.v(1, -1);
  point_product_44[1][1] = a444s.v(1, 1);
  point_product_45[0][0] = a243s.v(-1, -1);
  point_product_45[0][1] = a243s.v(-1, 1);
  point_product_45[1][0] = a243s.v(1, -1);
  point_product_45[1][1] = a243s.v(1, 1);
  point_product_46[0][0] = a343s.v(-1, -1);
  point_product_46[0][1] = a343s.v(-1, 1);
  point_product_46[1][0] = a343s.v(1, -1);
  point_product_46[1][1] = a343s.v(1, 1);
  point_product_47[0][0] = a341s.v(-1, -1);
  point_product_47[0][1] = a341s.v(-1, 1);
  point_product_47[1][0] = a341s.v(1, -1);
  point_product_47[1][1] = a341s.v(1, 1);
  point_product_48[0][0] = a351s.v(-1, -1);
  point_product_48[0][1] = a351s.v(-1, 1);
  point_product_48[1][0] = a351s.v(1, -1);
  point_product_48[1][1] = a351s.v(1, 1);
  point_product_49[0][0] = a443s.v(-1, -1);
  point_product_49[0][1] = a443s.v(-1, 1);
  point_product_49[1][0] = a443s.v(1, -1);
  point_product_49[1][1] = a443s.v(1, 1);
  point_product_50[0][0] = a311s.v(-1, -1);
  point_product_50[0][1] = a311s.v(-1, 1);
  point_product_50[1][0] = a311s.v(1, -1);
  point_product_50[1][1] = a311s.v(1, 1);
  point_product_51[0][0] = a431s.v(-1, -1);
  point_product_51[0][1] = a431s.v(-1, 1);
  point_product_51[1][0] = a431s.v(1, -1);
  point_product_51[1][1] = a431s.v(1, 1);
  point_product_52[0][0] = s113a.v(-1, -1);
  point_product_52[0][1] = s113a.v(-1, 1);
  point_product_52[1][0] = s113a.v(1, -1);
  point_product_52[1][1] = s113a.v(1, 1);
  point_product_53[0][0] = s153a.v(-1, -1);
  point_product_53[0][1] = s153a.v(-1, 1);
  point_product_53[1][0] = s153a.v(1, -1);
  point_product_53[1][1] = s153a.v(1, 1);
  point_product_54[0][0] = s114a.v(-1, -1);
  point_product_54[0][1] = s114a.v(-1, 1);
  point_product_54[1][0] = s114a.v(1, -1);
  point_product_54[1][1] = s114a.v(1, 1);
  point_product_55[0][0] = s154a.v(-1, -1);
  point_product_55[0][1] = s154a.v(-1, 1);
  point_product_55[1][0] = s154a.v(1, -1);
  point_product_55[1][1] = s154a.v(1, 1);
  point_product_56[0][0] = s211a.v(-1, -1);
  point_product_56[0][1] = s211a.v(-1, 1);
  point_product_56[1][0] = s211a.v(1, -1);
  point_product_56[1][1] = s211a.v(1, 1);
  point_product_57[0][0] = s251a.v(-1, -1);
  point_product_57[0][1] = s251a.v(-1, 1);
  point_product_57[1][0] = s251a.v(1, -1);
  point_product_57[1][1] = s251a.v(1, 1);
  point_product_58[0][0] = s221a.v(-1, -1);
  point_product_58[0][1] = s221a.v(-1, 1);
  point_product_58[1][0] = s221a.v(1, -1);
  point_product_58[1][1] = s221a.v(1, 1);
  point_product_59[0][0] = s223a.v(-1, -1);
  point_product_59[0][1] = s223a.v(-1, 1);
  point_product_59[1][0] = s223a.v(1, -1);
  point_product_59[1][1] = s223a.v(1, 1);
  point_product_60[0][0] = s253a.v(-1, -1);
  point_product_60[0][1] = s253a.v(-1, 1);
  point_product_60[1][0] = s253a.v(1, -1);
  point_product_60[1][1] = s253a.v(1, 1);
  point_product_61[0][0] = s243a.v(-1, -1);
  point_product_61[0][1] = s243a.v(-1, 1);
  point_product_61[1][0] = s243a.v(1, -1);
  point_product_61[1][1] = s243a.v(1, 1);
  point_product_62[0][0] = s224a.v(-1, -1);
  point_product_62[0][1] = s224a.v(-1, 1);
  point_product_62[1][0] = s224a.v(1, -1);
  point_product_62[1][1] = s224a.v(1, 1);
  point_product_63[0][0] = s254a.v(-1, -1);
  point_product_63[0][1] = s254a.v(-1, 1);
  point_product_63[1][0] = s254a.v(1, -1);
  point_product_63[1][1] = s254a.v(1, 1);
  point_product_64[0][0] = s234a.v(-1, -1);
  point_product_64[0][1] = s234a.v(-1, 1);
  point_product_64[1][0] = s234a.v(1, -1);
  point_product_64[1][1] = s234a.v(1, 1);
  point_product_65[0][0] = s311a.v(-1, -1);
  point_product_65[0][1] = s311a.v(-1, 1);
  point_product_65[1][0] = s311a.v(1, -1);
  point_product_65[1][1] = s311a.v(1, 1);
  point_product_66[0][0] = s351a.v(-1, -1);
  point_product_66[0][1] = s351a.v(-1, 1);
  point_product_66[1][0] = s351a.v(1, -1);
  point_product_66[1][1] = s351a.v(1, 1);
  point_product_67[0][0] = a2413a.v(-1, -1);
  point_product_67[0][1] = a2413a.v(-1, 1);
  point_product_67[1][0] = a2413a.v(1, -1);
  point_product_67[1][1] = a2413a.v(1, 1);
  point_product_68[0][0] = a2433a.v(-1, -1);
  point_product_68[0][1] = a2433a.v(-1, 1);
  point_product_68[1][0] = a2433a.v(1, -1);
  point_product_68[1][1] = a2433a.v(1, 1);
  point_product_69[0][0] = a2513a.v(-1, -1);
  point_product_69[0][1] = a2513a.v(-1, 1);
  point_product_69[1][0] = a2513a.v(1, -1);
  point_product_69[1][1] = a2513a.v(1, 1);
  point_product_70[0][0] = a2533a.v(-1, -1);
  point_product_70[0][1] = a2533a.v(-1, 1);
  point_product_70[1][0] = a2533a.v(1, -1);
  point_product_70[1][1] = a2533a.v(1, 1);
  point_product_71[0][0] = a2314a.v(-1, -1);
  point_product_71[0][1] = a2314a.v(-1, 1);
  point_product_71[1][0] = a2314a.v(1, -1);
  point_product_71[1][1] = a2314a.v(1, 1);
  point_product_72[0][0] = a2344a.v(-1, -1);
  point_product_72[0][1] = a2344a.v(-1, 1);
  point_product_72[1][0] = a2344a.v(1, -1);
  point_product_72[1][1] = a2344a.v(1, 1);
  point_product_73[0][0] = a2514a.v(-1, -1);
  point_product_73[0][1] = a2514a.v(-1, 1);
  point_product_73[1][0] = a2514a.v(1, -1);
  point_product_73[1][1] = a2514a.v(1, 1);
  point_product_74[0][0] = a2544a.v(-1, -1);
  point_product_74[0][1] = a2544a.v(-1, 1);
  point_product_74[1][0] = a2544a.v(1, -1);
  point_product_74[1][1] = a2544a.v(1, 1);
  point_product_75[0][0] = a3214a.v(-1, -1);
  point_product_75[0][1] = a3214a.v(-1, 1);
  point_product_75[1][0] = a3214a.v(1, -1);
  point_product_75[1][1] = a3214a.v(1, 1);
  point_product_76[0][0] = a3244a.v(-1, -1);
  point_product_76[0][1] = a3244a.v(-1, 1);
  point_product_76[1][0] = a3244a.v(1, -1);
  point_product_76[1][1] = a3244a.v(1, 1);
  point_product_77[0][0] = a3314a.v(-1, -1);
  point_product_77[0][1] = a3314a.v(-1, 1);
  point_product_77[1][0] = a3314a.v(1, -1);
  point_product_77[1][1] = a3314a.v(1, 1);
  point_product_78[0][0] = a3344a.v(-1, -1);
  point_product_78[0][1] = a3344a.v(-1, 1);
  point_product_78[1][0] = a3344a.v(1, -1);
  point_product_78[1][1] = a3344a.v(1, 1);
  point_product_79[0][0] = a3124a.v(-1, -1);
  point_product_79[0][1] = a3124a.v(-1, 1);
  point_product_79[1][0] = a3124a.v(1, -1);
  point_product_79[1][1] = a3124a.v(1, 1);
  point_product_80[0][0] = a3144a.v(-1, -1);
  point_product_80[0][1] = a3144a.v(-1, 1);
  point_product_80[1][0] = a3144a.v(1, -1);
  point_product_80[1][1] = a3144a.v(1, 1);
  point_product_81[0][0] = a3324a.v(-1, -1);
  point_product_81[0][1] = a3324a.v(-1, 1);
  point_product_81[1][0] = a3324a.v(1, -1);
  point_product_81[1][1] = a3324a.v(1, 1);
  point_product_82[0][0] = a3241a.v(-1, -1);
  point_product_82[0][1] = a3241a.v(-1, 1);
  point_product_82[1][0] = a3241a.v(1, -1);
  point_product_82[1][1] = a3241a.v(1, 1);
  point_product_83[0][0] = a3251a.v(-1, -1);
  point_product_83[0][1] = a3251a.v(-1, 1);
  point_product_83[1][0] = a3251a.v(1, -1);
  point_product_83[1][1] = a3251a.v(1, 1);
  point_product_84[0][0] = a3341a.v(-1, -1);
  point_product_84[0][1] = a3341a.v(-1, 1);
  point_product_84[1][0] = a3341a.v(1, -1);
  point_product_84[1][1] = a3341a.v(1, 1);
  point_product_85[0][0] = a3351a.v(-1, -1);
  point_product_85[0][1] = a3351a.v(-1, 1);
  point_product_85[1][0] = a3351a.v(1, -1);
  point_product_85[1][1] = a3351a.v(1, 1);
  point_product_86[0][0] = a4231a.v(-1, -1);
  point_product_86[0][1] = a4231a.v(-1, 1);
  point_product_86[1][0] = a4231a.v(1, -1);
  point_product_86[1][1] = a4231a.v(1, 1);
  point_product_87[0][0] = a4251a.v(-1, -1);
  point_product_87[0][1] = a4251a.v(-1, 1);
  point_product_87[1][0] = a4251a.v(1, -1);
  point_product_87[1][1] = a4251a.v(1, 1);
  point_product_88[0][0] = a4431a.v(-1, -1);
  point_product_88[0][1] = a4431a.v(-1, 1);
  point_product_88[1][0] = a4431a.v(1, -1);
  point_product_88[1][1] = a4431a.v(1, 1);
  point_product_89[0][0] = a4451a.v(-1, -1);
  point_product_89[0][1] = a4451a.v(-1, 1);
  point_product_89[1][0] = a4451a.v(1, -1);
  point_product_89[1][1] = a4451a.v(1, 1);
  point_product_90[0][0] = s1423s.v(-1, -1);
  point_product_90[0][1] = s1423s.v(-1, 1);
  point_product_90[1][0] = s1423s.v(1, -1);
  point_product_90[1][1] = s1423s.v(1, 1);
  point_product_91[0][0] = s1433s.v(-1, -1);
  point_product_91[0][1] = s1433s.v(-1, 1);
  point_product_91[1][0] = s1433s.v(1, -1);
  point_product_91[1][1] = s1433s.v(1, 1);
  point_product_92[0][0] = s1523s.v(-1, -1);
  point_product_92[0][1] = s1523s.v(-1, 1);
  point_product_92[1][0] = s1523s.v(1, -1);
  point_product_92[1][1] = s1523s.v(1, 1);
  point_product_93[0][0] = s1533s.v(-1, -1);
  point_product_93[0][1] = s1533s.v(-1, 1);
  point_product_93[1][0] = s1533s.v(1, -1);
  point_product_93[1][1] = s1533s.v(1, 1);
  point_product_94[0][0] = s1324s.v(-1, -1);
  point_product_94[0][1] = s1324s.v(-1, 1);
  point_product_94[1][0] = s1324s.v(1, -1);
  point_product_94[1][1] = s1324s.v(1, 1);
  point_product_95[0][0] = s1344s.v(-1, -1);
  point_product_95[0][1] = s1344s.v(-1, 1);
  point_product_95[1][0] = s1344s.v(1, -1);
  point_product_95[1][1] = s1344s.v(1, 1);
  point_product_96[0][0] = s1524s.v(-1, -1);
  point_product_96[0][1] = s1524s.v(-1, 1);
  point_product_96[1][0] = s1524s.v(1, -1);
  point_product_96[1][1] = s1524s.v(1, 1);
  point_product_97[0][0] = s1544s.v(-1, -1);
  point_product_97[0][1] = s1544s.v(-1, 1);
  point_product_97[1][0] = s1544s.v(1, -1);
  point_product_97[1][1] = s1544s.v(1, 1);
  point_product_98[0][0] = s2413s.v(-1, -1);
  point_product_98[0][1] = s2413s.v(-1, 1);
  point_product_98[1][0] = s2413s.v(1, -1);
  point_product_98[1][1] = s2413s.v(1, 1);
  point_product_99[0][0] = s2433s.v(-1, -1);
  point_product_99[0][1] = s2433s.v(-1, 1);
  point_product_99[1][0] = s2433s.v(1, -1);
  point_product_99[1][1] = s2433s.v(1, 1);
  point_product_100[0][0] = s2513s.v(-1, -1);
  point_product_100[0][1] = s2513s.v(-1, 1);
  point_product_100[1][0] = s2513s.v(1, -1);
  point_product_100[1][1] = s2513s.v(1, 1);
  point_product_101[0][0] = s2533s.v(-1, -1);
  point_product_101[0][1] = s2533s.v(-1, 1);
  point_product_101[1][0] = s2533s.v(1, -1);
  point_product_101[1][1] = s2533s.v(1, 1);
  point_product_102[0][0] = s2314s.v(-1, -1);
  point_product_102[0][1] = s2314s.v(-1, 1);
  point_product_102[1][0] = s2314s.v(1, -1);
  point_product_102[1][1] = s2314s.v(1, 1);
  point_product_103[0][0] = s2344s.v(-1, -1);
  point_product_103[0][1] = s2344s.v(-1, 1);
  point_product_103[1][0] = s2344s.v(1, -1);
  point_product_103[1][1] = s2344s.v(1, 1);
  point_product_104[0][0] = s2514s.v(-1, -1);
  point_product_104[0][1] = s2514s.v(-1, 1);
  point_product_104[1][0] = s2514s.v(1, -1);
  point_product_104[1][1] = s2514s.v(1, 1);
  point_product_105[0][0] = s2544s.v(-1, -1);
  point_product_105[0][1] = s2544s.v(-1, 1);
  point_product_105[1][0] = s2544s.v(1, -1);
  point_product_105[1][1] = s2544s.v(1, 1);
  point_product_106[0][0] = s3214s.v(-1, -1);
  point_product_106[0][1] = s3214s.v(-1, 1);
  point_product_106[1][0] = s3214s.v(1, -1);
  point_product_106[1][1] = s3214s.v(1, 1);
  point_product_107[0][0] = s3244s.v(-1, -1);
  point_product_107[0][1] = s3244s.v(-1, 1);
  point_product_107[1][0] = s3244s.v(1, -1);
  point_product_107[1][1] = s3244s.v(1, 1);
  point_product_108[0][0] = s3314s.v(-1, -1);
  point_product_108[0][1] = s3314s.v(-1, 1);
  point_product_108[1][0] = s3314s.v(1, -1);
  point_product_108[1][1] = s3314s.v(1, 1);
  point_product_109[0][0] = s3344s.v(-1, -1);
  point_product_109[0][1] = s3344s.v(-1, 1);
  point_product_109[1][0] = s3344s.v(1, -1);
  point_product_109[1][1] = s3344s.v(1, 1);
  point_product_110[0][0] = s4213s.v(-1, -1);
  point_product_110[0][1] = s4213s.v(-1, 1);
  point_product_110[1][0] = s4213s.v(1, -1);
  point_product_110[1][1] = s4213s.v(1, 1);
  point_product_111[0][0] = s4233s.v(-1, -1);
  point_product_111[0][1] = s4233s.v(-1, 1);
  point_product_111[1][0] = s4233s.v(1, -1);
  point_product_111[1][1] = s4233s.v(1, 1);
  point_product_112[0][0] = s4413s.v(-1, -1);
  point_product_112[0][1] = s4413s.v(-1, 1);
  point_product_112[1][0] = s4413s.v(1, -1);
  point_product_112[1][1] = s4413s.v(1, 1);
  point_product_113[0][0] = s4433s.v(-1, -1);
  point_product_113[0][1] = s4433s.v(-1, 1);
  point_product_113[1][0] = s4433s.v(1, -1);
  point_product_113[1][1] = s4433s.v(1, 1);
  point_factor_1 = (parameter_factor_7 * t23);
  point_factor_2 = (point_factor_1 * t14);
  point_factor_3 = (point_factor_1 * t15);
  point_factor_4 = (parameter_factor_7 * t14);
  point_factor_5 = (point_factor_4 * t25);
  point_factor_6 = ((parameter_factor_10 * t14) * t35);
  point_factor_7 = ((parameter_factor_10 * t23) * t45);
  point_factor_8 = (parameter_factor_7 * t13);
  point_factor_9 = (point_factor_8 * t24);
  point_factor_10 = (parameter_factor_7 * t24);
  point_factor_11 = (point_factor_10 * t15);
  point_factor_12 = (point_factor_8 * t25);
  point_factor_13 = ((parameter_factor_10 * t24) * t35);
  point_factor_14 = ((parameter_factor_10 * t13) * t45);
  point_factor_15 = (parameter_factor_15 * t34);
  point_factor_16 = (point_factor_15 * t15);
  point_factor_17 = (point_factor_15 * t25);
  point_factor_18 = (parameter_factor_15 * t12);
  point_factor_19 = (point_factor_15 * t12);
  point_factor_20 = ((parameter_factor_20 * t35) * t12);
  point_factor_21 = ((parameter_factor_20 * t45) * t12);
  point_factor_22 = (parameter_factor_15 * t14);
  point_factor_23 = (point_factor_22 * t25);
  point_factor_24 = (parameter_factor_7_2 * t24);
  point_factor_25 = (point_factor_24 * t15);
  point_factor_26 = (point_factor_10 * t35);
  point_factor_27 = (point_factor_4 * t35);
  point_factor_28 = (parameter_factor_7_2 * t13);
  point_factor_29 = (point_factor_28 * t24);
  point_factor_30 = (point_factor_28 * t25);
  point_factor_31 = ((parameter_factor_29 * t13) * t45);
  point_factor_32 = (parameter_factor_15 * t23);
  point_factor_33 = (point_factor_32 * t14);
  point_factor_34 = (point_factor_32 * t15);
  point_factor_35 = ((parameter_factor_20 * t23) * t45);
  point_factor_36 = ((parameter_factor_20 * t14) * t35);
  point_factor_37 = ((parameter_factor_29 * t24) * t35);
  point_factor_38 = (parameter_factor_15 * t13);
  point_factor_39 = (point_factor_38 * t25);
  point_factor_40 = (parameter_factor_7_2 * t23);
  point_factor_41 = (point_factor_40 * t15);
  point_factor_42 = ((parameter_factor_15 * t35) * t12);
  point_factor_43 = (point_factor_1 * t45);
  point_factor_44 = (point_factor_8 * t45);
  point_factor_45 = ((parameter_factor_15 * t45) * t12);
  point_factor_46 = (point_factor_38 * t24);
  point_factor_47 = ((parameter_factor_20 * t13) * t45);
  point_factor_48 = (point_factor_40 * t14);
  point_factor_49 = ((parameter_factor_29 * t23) * t45);
  point_factor_50 = (parameter_factor_7_2 * t14);
  point_factor_51 = (point_factor_50 * t25);
  point_factor_52 = ((parameter_factor_29 * t14) * t35);
  point_factor_53 = (parameter_factor_15 * t24);
  point_factor_54 = (point_factor_53 * t15);
  point_factor_55 = ((parameter_factor_20 * t24) * t35);
  point_factor_56 = (point_factor_28 * t45);
  point_factor_57 = (point_factor_50 * t35);
  point_factor_58 = (point_factor_32 * t45);
  point_factor_59 = (point_factor_53 * t35);
  point_factor_60 = (point_factor_40 * t45);
  point_factor_61 = (point_factor_24 * t35);
  point_factor_62 = (point_factor_38 * t45);
  point_factor_63 = (point_factor_22 * t35);
  point_denominator_1 = hepcat_spinas::CachedDenominator(point_factor_2, real_denominator_domain);
  point_denominator_2 = hepcat_spinas::CachedDenominator(point_factor_3, real_denominator_domain);
  point_denominator_3 = hepcat_spinas::CachedDenominator(point_factor_5, real_denominator_domain);
  point_denominator_4 = hepcat_spinas::CachedDenominator(point_factor_9, real_denominator_domain);
  point_denominator_5 = hepcat_spinas::CachedDenominator(point_factor_11, real_denominator_domain);
  point_denominator_6 = hepcat_spinas::CachedDenominator(point_factor_12, real_denominator_domain);
  point_denominator_7 = hepcat_spinas::CachedDenominator(point_factor_16, real_denominator_domain);
  point_denominator_8 = hepcat_spinas::CachedDenominator(point_factor_17, real_denominator_domain);
  point_denominator_9 = hepcat_spinas::CachedDenominator(point_factor_18, real_denominator_domain);
  point_denominator_10 = hepcat_spinas::CachedDenominator(point_factor_19, real_denominator_domain);
  point_denominator_11 = hepcat_spinas::CachedDenominator(point_factor_23, real_denominator_domain);
  point_denominator_12 = hepcat_spinas::CachedDenominator(point_factor_25, real_denominator_domain);
  point_denominator_13 = hepcat_spinas::CachedDenominator(point_factor_29, real_denominator_domain);
  point_denominator_14 = hepcat_spinas::CachedDenominator(point_factor_30, real_denominator_domain);
  point_denominator_15 = hepcat_spinas::CachedDenominator(point_factor_33, real_denominator_domain);
  point_denominator_16 = hepcat_spinas::CachedDenominator(point_factor_34, real_denominator_domain);
  point_denominator_17 = hepcat_spinas::CachedDenominator(point_factor_39, real_denominator_domain);
  point_denominator_18 = hepcat_spinas::CachedDenominator(point_factor_41, real_denominator_domain);
  point_denominator_19 = hepcat_spinas::CachedDenominator(point_factor_46, real_denominator_domain);
  point_denominator_20 = hepcat_spinas::CachedDenominator(point_factor_48, real_denominator_domain);
  point_denominator_21 = hepcat_spinas::CachedDenominator(point_factor_51, real_denominator_domain);
  point_denominator_22 = hepcat_spinas::CachedDenominator(point_factor_54, real_denominator_domain);
  summed_point_cache_ready = true;
}

// Individual amplitudes can lazily request the remaining helicities.
void experimentaleebarZZh::prepare_all_point_cache() {
  if (all_point_cache_ready) return;
  prepare_summed_point_cache();
  all_point_cache_ready = true;
}

void experimentaleebarZZh::set_momenta(const ldouble mom1[4], const ldouble mom2[4], const ldouble mom3[4],
  const ldouble mom4[4], const ldouble mom5[4]) {
  Momenta values;
  std::copy(mom1, mom1 + 4, values[0].begin());
  std::copy(mom2, mom2 + 4, values[1].begin());
  std::copy(mom3, mom3 + 4, values[2].begin());
  std::copy(mom4, mom4 + 4, values[3].begin());
  std::copy(mom5, mom5 + 4, values[4].begin());
  set_momenta(values);
}

cdouble experimentaleebarZZh::amp(const Spins& spins) {
  return amp(spins[0], spins[1], spins[2], spins[3], spins[4]);
}

// set_momenta(...) must be called before evaluating an amplitude.
cdouble experimentaleebarZZh::amp(const int& ds1, const int& ds2, const int& ds3, const int& ds4, const int&
  ds5) {
  if (!momenta_ready) throw std::logic_error("Call set_momenta before amp.");
  check_spin(1, ds1);
  check_spin(1, ds2);
  check_spin(2, ds3);
  check_spin(2, ds4);
  check_spin(0, ds5);
  prepare_all_point_cache();
  return amp_unchecked(ds1, ds2, ds3, ds4, ds5);
}

// Numerical kernel; public amp and spin_sum validate their inputs first.
cdouble experimentaleebarZZh::amp_unchecked(const int& ds1, const int& ds2, const int& ds3, const int& ds4,
  const int& ds5) {
  cdouble amplitude(0, 0);
  const auto assignments3 = spinor_assignments(2, ds3);
  const auto assignments4 = spinor_assignments(2, ds4);
  for (const auto& indices3 : assignments3) {
    const int ds3_1 = indices3[0];
    const int ds3_2 = indices3[1];
    for (const auto& indices4 : assignments4) {
      const int ds4_1 = indices4[0];
      const int ds4_2 = indices4[1];
      const cdouble spin_factor_1 = (parameter_factor_1 * point_product_1[(ds3_1) > 0 ? 1 : 0][(ds4_1) > 0 ? 1 :
        0]);
      const cdouble spin_factor_2 = ((spin_factor_1 * point_product_2[(ds1) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0]) *
        point_product_3[(ds2) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0]);
      const cdouble spin_factor_3 = (parameter_factor_9 * point_product_1[(ds3_1) > 0 ? 1 : 0][(ds4_1) > 0 ? 1 :
        0]);
      const cdouble spin_factor_4 = ((spin_factor_3 * point_product_2[(ds1) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0]) *
        point_product_3[(ds2) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0]);
      const cdouble spin_factor_5 = (((parameter_factor_1 * point_product_4[(ds1) > 0 ? 1 : 0][(ds4_1) > 0 ? 1 :
        0]) * point_product_5[(ds2) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0]) * point_product_6[(ds3_1) > 0 ? 1 :
        0][(ds4_2) > 0 ? 1 : 0]);
      const cdouble spin_factor_6 = (parameter_factor_9 * point_product_4[(ds1) > 0 ? 1 : 0][(ds4_1) > 0 ? 1 :
        0]);
      const cdouble spin_factor_7 = ((spin_factor_6 * point_product_5[(ds2) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0]) *
        point_product_6[(ds3_1) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0]);
      const cdouble spin_factor_8 = ((spin_factor_1 * point_product_7[(ds2) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0]) *
        point_product_8[(ds3_2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
      const cdouble spin_factor_9 = ((spin_factor_3 * point_product_7[(ds2) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0]) *
        point_product_8[(ds3_2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
      const cdouble spin_factor_10 = (((parameter_factor_14 * point_product_9[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 :
        0]) * point_product_1[(ds3_1) > 0 ? 1 : 0][(ds4_1) > 0 ? 1 : 0]) * point_product_6[(ds3_2) > 0 ? 1 :
        0][(ds4_2) > 0 ? 1 : 0]);
      const cdouble spin_factor_11 = (parameter_factor_17 * point_product_9[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 :
        0]);
      const cdouble spin_factor_12 = ((spin_factor_11 * point_product_1[(ds3_1) > 0 ? 1 : 0][(ds4_1) > 0 ? 1 :
        0]) * point_product_6[(ds3_2) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0]);
      const cdouble spin_factor_13 = (((parameter_factor_14 * point_product_1[(ds3_1) > 0 ? 1 : 0][(ds4_1) > 0 ?
        1 : 0]) * point_product_10[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]) * point_product_6[(ds3_2) > 0 ? 1 :
        0][(ds4_2) > 0 ? 1 : 0]);
      const cdouble spin_factor_14 = (((parameter_factor_17 * point_product_1[(ds3_1) > 0 ? 1 : 0][(ds4_1) > 0 ?
        1 : 0]) * point_product_10[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]) * point_product_6[(ds3_2) > 0 ? 1 :
        0][(ds4_2) > 0 ? 1 : 0]);
      const cdouble spin_factor_15 = (((parameter_factor_1 * point_product_11[(ds1) > 0 ? 1 : 0][(ds3_2) > 0 ? 1
        : 0]) * point_product_12[(ds2) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0]) * point_product_13[(ds4_1) > 0 ? 1 :
        0][(ds3_1) > 0 ? 1 : 0]);
      const cdouble spin_factor_16 = (((parameter_factor_9 * point_product_11[(ds1) > 0 ? 1 : 0][(ds3_2) > 0 ? 1
        : 0]) * point_product_12[(ds2) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0]) * point_product_13[(ds4_1) > 0 ? 1 :
        0][(ds3_1) > 0 ? 1 : 0]);
      const cdouble spin_factor_17 = (parameter_factor_21 * point_product_1[(ds3_1) > 0 ? 1 : 0][(ds4_1) > 0 ? 1
        : 0]);
      const cdouble spin_factor_18 = (point_product_14[(ds1) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0] -
        point_product_15[(ds1) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0]);
      const cdouble spin_factor_19 = ((parameter_factor_23 * point_product_1[(ds3_1) > 0 ? 1 : 0][(ds4_1) > 0 ?
        1 : 0]) * point_product_6[(ds3_2) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0]);
      const cdouble spin_factor_20 = (point_product_19[(ds2) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0] -
        point_product_20[(ds2) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0]);
      const cdouble spin_factor_21 = (point_product_21[(ds2) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0] -
        point_product_22[(ds2) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0]);
      const cdouble spin_factor_22 = (point_product_30[(ds3_1) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0] +
        point_product_31[(ds3_1) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0]);
      const cdouble spin_factor_23 = ((parameter_factor_27 * point_product_12[(ds2) > 0 ? 1 : 0][(ds4_1) > 0 ? 1
        : 0]) * point_product_33[(ds1) > 0 ? 1 : 0][(ds3_1) > 0 ? 1 : 0]);
      const cdouble spin_factor_24 = ((-point_product_24[(ds3_2) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0]) +
        point_product_34[(ds3_2) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0]);
      const cdouble spin_factor_25 = (spin_factor_23 * spin_factor_24);
      const cdouble spin_factor_26 = ((parameter_factor_28 * point_product_12[(ds2) > 0 ? 1 : 0][(ds4_1) > 0 ? 1
        : 0]) * point_product_33[(ds1) > 0 ? 1 : 0][(ds3_1) > 0 ? 1 : 0]);
      const cdouble spin_factor_27 = ((parameter_factor_30 * point_product_23[(ds4_1) > 0 ? 1 : 0][(ds1) > 0 ? 1
        : 0]) * point_product_3[(ds2) > 0 ? 1 : 0][(ds3_1) > 0 ? 1 : 0]);
      const cdouble spin_factor_28 = ((-point_product_28[(ds3_2) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0]) +
        point_product_34[(ds3_2) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0]);
      const cdouble spin_factor_29 = (spin_factor_27 * spin_factor_28);
      const cdouble spin_factor_30 = (parameter_factor_31 * point_product_23[(ds4_1) > 0 ? 1 : 0][(ds1) > 0 ? 1
        : 0]);
      const cdouble spin_factor_31 = (spin_factor_30 * point_product_3[(ds2) > 0 ? 1 : 0][(ds3_1) > 0 ? 1 : 0]);
      const cdouble spin_factor_32 = (point_product_24[(ds3_2) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0] -
        point_product_25[(ds3_2) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0]);
      const cdouble spin_factor_33 = (spin_factor_27 * spin_factor_32);
      const cdouble spin_factor_34 = (point_product_28[(ds3_2) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0] -
        point_product_25[(ds3_2) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0]);
      const cdouble spin_factor_35 = (spin_factor_23 * spin_factor_34);
      const cdouble spin_factor_36 = (point_product_41[(ds4_1) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0] +
        point_product_42[(ds4_1) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0]);
      const cdouble spin_factor_37 = (parameter_factor_17 * point_product_10[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 :
        0]);
      const cdouble spin_factor_38 = (point_product_44[(ds4_1) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0] +
        point_product_42[(ds4_1) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0]);
      const cdouble spin_factor_39 = ((-point_product_46[(ds3_1) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0]) -
        point_product_31[(ds3_1) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0]);
      const cdouble spin_factor_40 = ((parameter_factor_30 * point_product_35[(ds3_1) > 0 ? 1 : 0][(ds1) > 0 ? 1
        : 0]) * point_product_7[(ds2) > 0 ? 1 : 0][(ds4_1) > 0 ? 1 : 0]);
      const cdouble spin_factor_41 = ((-point_product_36[(ds4_2) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0]) +
        point_product_37[(ds4_2) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0]);
      const cdouble spin_factor_42 = (spin_factor_40 * spin_factor_41);
      const cdouble spin_factor_43 = (parameter_factor_31 * point_product_35[(ds3_1) > 0 ? 1 : 0][(ds1) > 0 ? 1
        : 0]);
      const cdouble spin_factor_44 = (spin_factor_43 * point_product_7[(ds2) > 0 ? 1 : 0][(ds4_1) > 0 ? 1 : 0]);
      const cdouble spin_factor_45 = ((parameter_factor_27 * point_product_5[(ds2) > 0 ? 1 : 0][(ds3_1) > 0 ? 1
        : 0]) * point_product_2[(ds1) > 0 ? 1 : 0][(ds4_1) > 0 ? 1 : 0]);
      const cdouble spin_factor_46 = ((-point_product_38[(ds4_2) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0]) +
        point_product_37[(ds4_2) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0]);
      const cdouble spin_factor_47 = (spin_factor_45 * spin_factor_46);
      const cdouble spin_factor_48 = ((parameter_factor_28 * point_product_5[(ds2) > 0 ? 1 : 0][(ds3_1) > 0 ? 1
        : 0]) * point_product_2[(ds1) > 0 ? 1 : 0][(ds4_1) > 0 ? 1 : 0]);
      const cdouble spin_factor_49 = ((-point_product_36[(ds4_2) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0]) +
        point_product_49[(ds4_2) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0]);
      const cdouble spin_factor_50 = (spin_factor_45 * spin_factor_49);
      const cdouble spin_factor_51 = ((-point_product_38[(ds4_2) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0]) +
        point_product_49[(ds4_2) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0]);
      const cdouble spin_factor_52 = (spin_factor_40 * spin_factor_51);
      const cdouble spin_factor_53 = (parameter_factor_9 * point_product_7[(ds2) > 0 ? 1 : 0][(ds4_1) > 0 ? 1 :
        0]);
      const cdouble spin_factor_54 = (point_product_59[(ds2) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0] -
        point_product_60[(ds2) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0]);
      const cdouble spin_factor_55 = (point_product_62[(ds2) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0] -
        point_product_63[(ds2) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0]);
      const cdouble spin_factor_56 = (parameter_factor_9 * point_product_2[(ds1) > 0 ? 1 : 0][(ds4_1) > 0 ? 1 :
        0]);
      amplitude += (-point_denominator_1.divide(spin_factor_2) - point_denominator_2.divide(spin_factor_2) -
        point_denominator_3.divide(spin_factor_2) - (spin_factor_4 / point_factor_6) - (spin_factor_4 /
        point_factor_7) - point_denominator_1.divide(spin_factor_5) - point_denominator_2.divide(spin_factor_5)
        - point_denominator_3.divide(spin_factor_5) - (spin_factor_7 / point_factor_6) - (spin_factor_7 /
        point_factor_7) - point_denominator_4.divide(spin_factor_8) - point_denominator_5.divide(spin_factor_8)
        - point_denominator_6.divide(spin_factor_8) - (spin_factor_9 / point_factor_13) - (spin_factor_9 /
        point_factor_14) + point_denominator_7.divide(spin_factor_10) +
        point_denominator_8.divide(spin_factor_10) + point_denominator_9.divide(spin_factor_12) +
        point_denominator_10.divide((((parameter_factor_18 * point_product_9[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 :
        0]) * point_product_1[(ds3_1) > 0 ? 1 : 0][(ds4_1) > 0 ? 1 : 0]) * point_product_6[(ds3_2) > 0 ? 1 :
        0][(ds4_2) > 0 ? 1 : 0])) + (spin_factor_12 / point_factor_20) + (spin_factor_12 / point_factor_21) +
        point_denominator_7.divide(spin_factor_13) + point_denominator_8.divide(spin_factor_13) +
        point_denominator_9.divide(spin_factor_14) + point_denominator_10.divide((((parameter_factor_18 *
        point_product_1[(ds3_1) > 0 ? 1 : 0][(ds4_1) > 0 ? 1 : 0]) * point_product_10[(ds2) > 0 ? 1 : 0][(ds1) >
        0 ? 1 : 0]) * point_product_6[(ds3_2) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0])) + (spin_factor_14 /
        point_factor_20) + (spin_factor_14 / point_factor_21) - point_denominator_4.divide(spin_factor_15) -
        point_denominator_5.divide(spin_factor_15) - point_denominator_6.divide(spin_factor_15) -
        (spin_factor_16 / point_factor_13) - (spin_factor_16 / point_factor_14) -
        point_denominator_2.divide(((spin_factor_17 * point_product_3[(ds2) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0]) *
        spin_factor_18)) - point_denominator_7.divide((spin_factor_19 * (point_product_16[(ds2) > 0 ? 1 :
        0][(ds1) > 0 ? 1 : 0] - point_product_17[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]))) +
        point_denominator_8.divide((spin_factor_19 * (point_product_18[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0] -
        point_product_17[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]))) - point_denominator_3.divide(((spin_factor_17
        * point_product_2[(ds1) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0]) * spin_factor_20)) -
        point_denominator_6.divide(((spin_factor_17 * point_product_8[(ds3_2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]) *
        spin_factor_21)) - point_denominator_11.divide((((parameter_factor_24 * point_product_23[(ds4_1) > 0 ? 1
        : 0][(ds1) > 0 ? 1 : 0]) * spin_factor_20) * (point_product_24[(ds3_1) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0]
        - point_product_25[(ds3_1) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0]))) -
        point_denominator_12.divide((((parameter_factor_26 * point_product_12[(ds2) > 0 ? 1 : 0][(ds4_1) > 0 ? 1
        : 0]) * (point_product_26[(ds1) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0] - point_product_27[(ds1) > 0 ? 1 :
        0][(ds3_2) > 0 ? 1 : 0])) * (point_product_28[(ds3_1) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0] -
        point_product_25[(ds3_1) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0]))) + ((((parameter_factor_9 *
        point_product_12[(ds2) > 0 ? 1 : 0][(ds4_1) > 0 ? 1 : 0]) * ((-point_product_29[(ds1) > 0 ? 1 :
        0][(ds4_2) > 0 ? 1 : 0]) - point_product_15[(ds1) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0])) * spin_factor_22)
        / point_factor_26) + ((((parameter_factor_9 * point_product_23[(ds4_1) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0])
        * ((-point_product_32[(ds2) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0]) - point_product_22[(ds2) > 0 ? 1 :
        0][(ds4_2) > 0 ? 1 : 0])) * spin_factor_22) / point_factor_27) -
        point_denominator_13.divide(spin_factor_25) - point_denominator_14.divide(spin_factor_25) -
        ((spin_factor_26 * spin_factor_24) / point_factor_31) - point_denominator_15.divide(spin_factor_29) -
        point_denominator_16.divide(spin_factor_29) - ((spin_factor_31 * spin_factor_28) / point_factor_35) -
        point_denominator_15.divide(spin_factor_33) - point_denominator_11.divide(spin_factor_33) -
        ((spin_factor_31 * spin_factor_32) / point_factor_36) - point_denominator_13.divide(spin_factor_35) -
        point_denominator_12.divide(spin_factor_35) - ((spin_factor_26 * spin_factor_34) / point_factor_37) +
        point_denominator_17.divide((((parameter_factor_24 * point_product_35[(ds3_1) > 0 ? 1 : 0][(ds1) > 0 ? 1
        : 0]) * spin_factor_21) * ((-point_product_36[(ds4_1) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0]) +
        point_product_37[(ds4_1) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0]))) +
        point_denominator_18.divide((((parameter_factor_26 * point_product_5[(ds2) > 0 ? 1 : 0][(ds3_1) > 0 ? 1
        : 0]) * spin_factor_18) * ((-point_product_38[(ds4_1) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0]) +
        point_product_37[(ds4_1) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0]))) +
        point_denominator_16.divide((((parameter_factor_24 * point_product_3[(ds2) > 0 ? 1 : 0][(ds3_1) > 0 ? 1
        : 0]) * spin_factor_28) * (point_product_39[(ds4_1) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0] -
        point_product_40[(ds4_1) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]))) + (((spin_factor_11 * spin_factor_22) *
        spin_factor_36) / point_factor_42) + (((spin_factor_37 * spin_factor_22) * spin_factor_36) /
        point_factor_42) + ((((parameter_factor_9 * point_product_5[(ds2) > 0 ? 1 : 0][(ds3_1) > 0 ? 1 : 0]) *
        ((-point_product_43[(ds1) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0]) - point_product_27[(ds1) > 0 ? 1 :
        0][(ds3_2) > 0 ? 1 : 0])) * spin_factor_38) / point_factor_43) + ((((parameter_factor_9 *
        point_product_35[(ds3_1) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]) * ((-point_product_45[(ds2) > 0 ? 1 :
        0][(ds3_2) > 0 ? 1 : 0]) - point_product_20[(ds2) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0])) * spin_factor_38)
        / point_factor_44) - (((spin_factor_11 * spin_factor_39) * spin_factor_38) / point_factor_45) -
        (((spin_factor_37 * spin_factor_39) * spin_factor_38) / point_factor_45) + ((((parameter_factor_9 *
        point_product_3[(ds2) > 0 ? 1 : 0][(ds3_1) > 0 ? 1 : 0]) * ((-point_product_47[(ds3_2) > 0 ? 1 :
        0][(ds1) > 0 ? 1 : 0]) - point_product_48[(ds3_2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0])) * spin_factor_38) /
        point_factor_43) + point_denominator_19.divide(spin_factor_42) +
        point_denominator_17.divide(spin_factor_42) + ((spin_factor_44 * spin_factor_41) / point_factor_47) +
        point_denominator_20.divide(spin_factor_47) + point_denominator_18.divide(spin_factor_47) +
        ((spin_factor_48 * spin_factor_46) / point_factor_49) - point_denominator_20.divide(spin_factor_50) -
        point_denominator_21.divide(spin_factor_50) - ((spin_factor_48 * spin_factor_49) / point_factor_52) -
        point_denominator_19.divide(spin_factor_52) - point_denominator_22.divide(spin_factor_52) -
        ((spin_factor_44 * spin_factor_51) / point_factor_55) +
        point_denominator_22.divide((((parameter_factor_24 * point_product_7[(ds2) > 0 ? 1 : 0][(ds4_1) > 0 ? 1
        : 0]) * (point_product_50[(ds3_1) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0] - point_product_48[(ds3_1) > 0 ? 1 :
        0][(ds1) > 0 ? 1 : 0])) * spin_factor_51)) - (((spin_factor_53 * spin_factor_22) *
        (point_product_51[(ds4_2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0] + point_product_40[(ds4_2) > 0 ? 1 : 0][(ds1)
        > 0 ? 1 : 0])) / point_factor_26) - point_denominator_5.divide((((parameter_factor_21 *
        point_product_12[(ds2) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0]) * point_product_13[(ds4_1) > 0 ? 1 :
        0][(ds3_1) > 0 ? 1 : 0]) * (point_product_52[(ds1) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0] -
        point_product_53[(ds1) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0]))) -
        point_denominator_2.divide((((parameter_factor_21 * point_product_5[(ds2) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 :
        0]) * point_product_6[(ds3_1) > 0 ? 1 : 0][(ds4_1) > 0 ? 1 : 0]) * (point_product_54[(ds1) > 0 ? 1 :
        0][(ds4_2) > 0 ? 1 : 0] - point_product_55[(ds1) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0]))) -
        point_denominator_7.divide((spin_factor_19 * (point_product_56[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0] -
        point_product_57[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]))) + point_denominator_8.divide((spin_factor_19 *
        (point_product_58[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0] - point_product_57[(ds2) > 0 ? 1 : 0][(ds1) > 0
        ? 1 : 0]))) - point_denominator_3.divide((((parameter_factor_21 * point_product_4[(ds1) > 0 ? 1 :
        0][(ds4_1) > 0 ? 1 : 0]) * point_product_6[(ds3_1) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0]) * spin_factor_54))
        - point_denominator_21.divide((((parameter_factor_26 * point_product_2[(ds1) > 0 ? 1 : 0][(ds4_1) > 0 ?
        1 : 0]) * ((-point_product_36[(ds4_2) > 0 ? 1 : 0][(ds3_1) > 0 ? 1 : 0]) + point_product_49[(ds4_2) > 0
        ? 1 : 0][(ds3_1) > 0 ? 1 : 0])) * spin_factor_54)) - ((((parameter_factor_9 * point_product_33[(ds1) > 0
        ? 1 : 0][(ds3_1) > 0 ? 1 : 0]) * spin_factor_38) * ((-point_product_61[(ds2) > 0 ? 1 : 0][(ds3_2) > 0 ?
        1 : 0]) - point_product_60[(ds2) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0])) / point_factor_44) -
        point_denominator_6.divide((((parameter_factor_21 * point_product_11[(ds1) > 0 ? 1 : 0][(ds3_2) > 0 ? 1
        : 0]) * point_product_13[(ds4_1) > 0 ? 1 : 0][(ds3_1) > 0 ? 1 : 0]) * spin_factor_55)) -
        point_denominator_14.divide((((parameter_factor_26 * point_product_33[(ds1) > 0 ? 1 : 0][(ds3_1) > 0 ? 1
        : 0]) * ((-point_product_24[(ds3_2) > 0 ? 1 : 0][(ds4_1) > 0 ? 1 : 0]) + point_product_34[(ds3_2) > 0 ?
        1 : 0][(ds4_1) > 0 ? 1 : 0])) * spin_factor_55)) + (((spin_factor_56 * spin_factor_22) *
        (point_product_64[(ds2) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0] + point_product_63[(ds2) > 0 ? 1 : 0][(ds4_2)
        > 0 ? 1 : 0])) / point_factor_27) + point_denominator_5.divide(((spin_factor_17 * point_product_7[(ds2)
        > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0]) * (point_product_65[(ds3_2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0] -
        point_product_66[(ds3_2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]))) - ((((parameter_factor_32 *
        point_product_33[(ds1) > 0 ? 1 : 0][(ds3_1) > 0 ? 1 : 0]) * spin_factor_38) * (((point_product_67[(ds2)
        > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0] - point_product_68[(ds2) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0]) +
        point_product_69[(ds2) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0]) - point_product_70[(ds2) > 0 ? 1 : 0][(ds3_2)
        > 0 ? 1 : 0])) / point_factor_56) - ((((parameter_factor_32 * point_product_2[(ds1) > 0 ? 1 : 0][(ds4_1)
        > 0 ? 1 : 0]) * spin_factor_22) * (((point_product_71[(ds2) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0] -
        point_product_72[(ds2) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0]) + point_product_73[(ds2) > 0 ? 1 : 0][(ds4_2)
        > 0 ? 1 : 0]) - point_product_74[(ds2) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0])) / point_factor_57) +
        point_denominator_1.divide(((spin_factor_56 * point_product_3[(ds2) > 0 ? 1 : 0][(ds3_1) > 0 ? 1 : 0]) *
        (((point_product_75[(ds3_2) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0] - point_product_76[(ds3_2) > 0 ? 1 :
        0][(ds4_2) > 0 ? 1 : 0]) - point_product_77[(ds3_2) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0]) +
        point_product_78[(ds3_2) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0]))) +
        point_denominator_4.divide(((spin_factor_53 * point_product_8[(ds3_1) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]) *
        (((point_product_79[(ds3_2) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0] - point_product_80[(ds3_2) > 0 ? 1 :
        0][(ds4_2) > 0 ? 1 : 0]) - point_product_81[(ds3_2) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0]) +
        point_product_78[(ds3_2) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0]))) - ((((parameter_factor_31 *
        point_product_3[(ds2) > 0 ? 1 : 0][(ds3_1) > 0 ? 1 : 0]) * spin_factor_38) * (((point_product_82[(ds3_2)
        > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0] + point_product_83[(ds3_2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]) -
        point_product_84[(ds3_2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]) - point_product_85[(ds3_2) > 0 ? 1 : 0][(ds1)
        > 0 ? 1 : 0])) / point_factor_58) - ((((parameter_factor_31 * point_product_7[(ds2) > 0 ? 1 : 0][(ds4_1)
        > 0 ? 1 : 0]) * spin_factor_22) * (((point_product_86[(ds4_2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0] +
        point_product_87[(ds4_2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]) - point_product_88[(ds4_2) > 0 ? 1 : 0][(ds1)
        > 0 ? 1 : 0]) - point_product_89[(ds4_2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0])) / point_factor_59) -
        ((((parameter_factor_32 * point_product_5[(ds2) > 0 ? 1 : 0][(ds3_1) > 0 ? 1 : 0]) * spin_factor_38) *
        ((((-point_product_90[(ds1) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0]) + point_product_91[(ds1) > 0 ? 1 :
        0][(ds3_2) > 0 ? 1 : 0]) - point_product_92[(ds1) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0]) +
        point_product_93[(ds1) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0])) / point_factor_60) - ((((parameter_factor_32
        * point_product_12[(ds2) > 0 ? 1 : 0][(ds4_1) > 0 ? 1 : 0]) * spin_factor_22) *
        ((((-point_product_94[(ds1) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0]) + point_product_95[(ds1) > 0 ? 1 :
        0][(ds4_2) > 0 ? 1 : 0]) - point_product_96[(ds1) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0]) +
        point_product_97[(ds1) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0])) / point_factor_61) - (((spin_factor_43 *
        spin_factor_38) * ((((-point_product_98[(ds2) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0]) +
        point_product_99[(ds2) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0]) - point_product_100[(ds2) > 0 ? 1 : 0][(ds3_2)
        > 0 ? 1 : 0]) + point_product_101[(ds2) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0])) / point_factor_62) -
        (((spin_factor_30 * spin_factor_22) * ((((-point_product_102[(ds2) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0]) +
        point_product_103[(ds2) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0]) - point_product_104[(ds2) > 0 ? 1 :
        0][(ds4_2) > 0 ? 1 : 0]) + point_product_105[(ds2) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0])) /
        point_factor_63) + point_denominator_1.divide(((spin_factor_6 * point_product_5[(ds2) > 0 ? 1 :
        0][(ds3_1) > 0 ? 1 : 0]) * (((point_product_106[(ds3_2) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0] -
        point_product_107[(ds3_2) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0]) - point_product_108[(ds3_2) > 0 ? 1 :
        0][(ds4_2) > 0 ? 1 : 0]) + point_product_109[(ds3_2) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0]))) +
        point_denominator_4.divide((((parameter_factor_9 * point_product_11[(ds1) > 0 ? 1 : 0][(ds3_1) > 0 ? 1 :
        0]) * point_product_12[(ds2) > 0 ? 1 : 0][(ds4_1) > 0 ? 1 : 0]) * (((point_product_110[(ds4_2) > 0 ? 1 :
        0][(ds3_2) > 0 ? 1 : 0] - point_product_111[(ds4_2) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0]) -
        point_product_112[(ds4_2) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0]) + point_product_113[(ds4_2) > 0 ? 1 :
        0][(ds3_2) > 0 ? 1 : 0]))));
    }
  }
  return (spinor_normalization(2, ds3) * spinor_normalization(2, ds4)) * amplitude;
}

// Explicit spin and helicity sums.
ldouble experimentaleebarZZh::spin_sum() {
  ldouble result = 0;
  cdouble amplitude;
  if (!momenta_ready) throw std::logic_error("Call set_momenta before spin_sum.");
  prepare_all_point_cache();
  const int j5 = 0;
  for (int j1 = -1; j1 <= 1; j1 += 2) {
    for (int j2 = -1; j2 <= 1; j2 += 2) {
      for (int j3 = -2; j3 <= 2; j3 += 2) {
        for (int j4 = -2; j4 <= 2; j4 += 2) {
          amplitude = amp_unchecked(j1, j2, j3, j4, j5);
          result += std::norm(amplitude);
        }
      }
    }
  }
  return result;
}

ldouble experimentaleebarZZh::amp2() {
  // Initial-state average: 1/4.
  // Final-state identical-particle factor: 1/2.
  return spin_sum() / ldouble(8.0L);
}

} // namespace hepcat_generated
