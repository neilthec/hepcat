#include "process.hpp"
#include <algorithm>
#include <cmath>
#include <stdexcept>

namespace hepcat_generated {
using namespace hepcat_spinas;

constexpr std::size_t draftcoloredcurrent2_color_1::leg_count;
constexpr std::size_t draftcoloredcurrent2_color_1::parameter_count;
constexpr std::size_t draftcoloredcurrent2_color_1::helicity_count;


draftcoloredcurrent2_color_1::draftcoloredcurrent2_color_1() : draftcoloredcurrent2_color_1(default_parameters()) {}

draftcoloredcurrent2_color_1::draftcoloredcurrent2_color_1(const Parameters& values) {
  set_parameters(values);
}

draftcoloredcurrent2_color_1::draftcoloredcurrent2_color_1(const ldouble& value_EE, const ldouble& value_MW,
  const ldouble& value_SW, const ldouble& value_GG, const ldouble& value_Mu, const ldouble& value_Mh)
  : draftcoloredcurrent2_color_1(Parameters{{cdouble(value_Mu), cdouble(value_Mh), cdouble(value_EE),
    cdouble(value_MW), cdouble(value_SW), cdouble(value_GG)}}) {}

// Copies rebuild caches, so every sproduct points into its own process.
draftcoloredcurrent2_color_1::draftcoloredcurrent2_color_1(const draftcoloredcurrent2_color_1& other) {
  widths_enabled = other.widths_enabled;
  set_parameters(other.parameters);
  if (other.momenta_ready) set_momenta(other.momenta);
}

draftcoloredcurrent2_color_1& draftcoloredcurrent2_color_1::operator=(const draftcoloredcurrent2_color_1& other) {
  if (this != &other) {
    widths_enabled = other.widths_enabled;
    set_parameters(other.parameters);
    if (other.momenta_ready) set_momenta(other.momenta);
  }
  return *this;
}

draftcoloredcurrent2_color_1::Parameters draftcoloredcurrent2_color_1::default_parameters() {
  return Parameters{{
    cdouble(ldouble(2.16e-3L), ldouble(0.0L)), // "Mu"
    cdouble(ldouble(1.25e2L), ldouble(0.0L)), // "Mh"
    cdouble(ldouble(3.1333e-1L), ldouble(0.0L)), // "EE"
    cdouble(ldouble(8.0385e1L), ldouble(0.0L)), // "MW"
    cdouble(ldouble(4.74e-1L), ldouble(0.0L)), // "SW"
    cdouble(ldouble(1.117e0L), ldouble(0.0L)) // "GG"
  }};
}

draftcoloredcurrent2_color_1::Spins draftcoloredcurrent2_color_1::default_spins() {
  return Spins{{-1, -1, -1, -1, 0}};
}

std::array<std::size_t, 2> draftcoloredcurrent2_color_1::incoming_legs() {
  return {{0, 1}};
}

std::array<draftcoloredcurrent2_color_1::Spins, draftcoloredcurrent2_color_1::helicity_count>
draftcoloredcurrent2_color_1::helicity_configurations() {
  return std::array<Spins, helicity_count>{{
    {{0, 0, 0, 0, 0}}
  }};
}

ldouble draftcoloredcurrent2_color_1::incoming_spin_average() {
  return ldouble(4.0e0L);
}

ldouble draftcoloredcurrent2_color_1::symmetry_factor() {
  return ldouble(1.0e0L);
}

void draftcoloredcurrent2_color_1::initialize_products() {
#ifdef SPINAS_HAS_MOMENTUM_CHAIN
  momentum_chain_1 = spinas::momentum_chain(::ANGLE, {&p3, &p1});
  momentum_chain_2 = spinas::momentum_chain(::ANGLE, {&p3, &p5});
  momentum_chain_3 = spinas::momentum_chain(::ANGLE, {&p4, &p5});
  momentum_chain_4 = spinas::momentum_chain(::ANGLE, {&p2, &p4});
  momentum_chain_5 = spinas::momentum_chain(::ANGLE, {&p5, &p3});
  momentum_chain_6 = spinas::momentum_chain(::ANGLE, {&p5, &p4});
  momentum_chain_7 = spinas::momentum_chain(::SQUARE, {&p3, &p1});
  momentum_chain_8 = spinas::momentum_chain(::SQUARE, {&p3, &p5});
  momentum_chain_9 = spinas::momentum_chain(::SQUARE, {&p4, &p5});
  momentum_chain_10 = spinas::momentum_chain(::ANGLE, {&p4, &p3});
  momentum_chain_11 = spinas::momentum_chain(::SQUARE, {&p4, &p3});
  momentum_chain_12 = spinas::momentum_chain(::SQUARE, {&p2, &p4});
  momentum_chain_13 = spinas::momentum_chain(::SQUARE, {&p5, &p3});
  momentum_chain_14 = spinas::momentum_chain(::SQUARE, {&p5, &p4});
  momentum_chain_15 = spinas::momentum_chain(::ANGLE, {&p2, &p1});
  momentum_chain_16 = spinas::momentum_chain(::SQUARE, {&p2, &p1});
#endif
  a21a = spinas::sproduct(::ANGLE, &p2, &p1);
  a43a = spinas::sproduct(::ANGLE, &p4, &p3);
  a24a = spinas::sproduct(::ANGLE, &p2, &p4);
  s13s = spinas::sproduct(::SQUARE, &p1, &p3);
  s21s = spinas::sproduct(::SQUARE, &p2, &p1);
  a41a = spinas::sproduct(::ANGLE, &p4, &p1);
  s23s = spinas::sproduct(::SQUARE, &p2, &p3);
  a13a = spinas::sproduct(::ANGLE, &p1, &p3);
  s24s = spinas::sproduct(::SQUARE, &p2, &p4);
  a23a = spinas::sproduct(::ANGLE, &p2, &p3);
  s41s = spinas::sproduct(::SQUARE, &p4, &p1);
  s43s = spinas::sproduct(::SQUARE, &p4, &p3);
  a113s = spinas::sproduct(::ANGLE, &p1, &p1, &p3);
  a153s = spinas::sproduct(::ANGLE, &p1, &p5, &p3);
  a133s = spinas::sproduct(::ANGLE, &p1, &p3, &p3);
  a211s = spinas::sproduct(::ANGLE, &p2, &p1, &p1);
  a251s = spinas::sproduct(::ANGLE, &p2, &p5, &p1);
  a221s = spinas::sproduct(::ANGLE, &p2, &p2, &p1);
  a223s = spinas::sproduct(::ANGLE, &p2, &p2, &p3);
  a253s = spinas::sproduct(::ANGLE, &p2, &p5, &p3);
  a233s = spinas::sproduct(::ANGLE, &p2, &p3, &p3);
  a224s = spinas::sproduct(::ANGLE, &p2, &p2, &p4);
  a254s = spinas::sproduct(::ANGLE, &p2, &p5, &p4);
  a244s = spinas::sproduct(::ANGLE, &p2, &p4, &p4);
  a231s = spinas::sproduct(::ANGLE, &p2, &p3, &p1);
  a241s = spinas::sproduct(::ANGLE, &p2, &p4, &p1);
  a433s = spinas::sproduct(::ANGLE, &p4, &p3, &p3);
  a443s = spinas::sproduct(::ANGLE, &p4, &p4, &p3);
  a411s = spinas::sproduct(::ANGLE, &p4, &p1, &p1);
  a451s = spinas::sproduct(::ANGLE, &p4, &p5, &p1);
  a441s = spinas::sproduct(::ANGLE, &p4, &p4, &p1);
  s31s = spinas::sproduct(::SQUARE, &p3, &p1);
  a422s = spinas::sproduct(::ANGLE, &p4, &p2, &p2);
  a452s = spinas::sproduct(::ANGLE, &p4, &p5, &p2);
  a31a = spinas::sproduct(::ANGLE, &p3, &p1);
  a442s = spinas::sproduct(::ANGLE, &p4, &p4, &p2);
  a453s = spinas::sproduct(::ANGLE, &p4, &p5, &p3);
  s113a = spinas::sproduct(::SQUARE, &p1, &p1, &p3);
  s153a = spinas::sproduct(::SQUARE, &p1, &p5, &p3);
  s133a = spinas::sproduct(::SQUARE, &p1, &p3, &p3);
  s231a = spinas::sproduct(::SQUARE, &p2, &p3, &p1);
  s241a = spinas::sproduct(::SQUARE, &p2, &p4, &p1);
  s221a = spinas::sproduct(::SQUARE, &p2, &p2, &p1);
  s251a = spinas::sproduct(::SQUARE, &p2, &p5, &p1);
  s211a = spinas::sproduct(::SQUARE, &p2, &p1, &p1);
  s223a = spinas::sproduct(::SQUARE, &p2, &p2, &p3);
  s253a = spinas::sproduct(::SQUARE, &p2, &p5, &p3);
  s233a = spinas::sproduct(::SQUARE, &p2, &p3, &p3);
  a311s = spinas::sproduct(::ANGLE, &p3, &p1, &p1);
  a351s = spinas::sproduct(::ANGLE, &p3, &p5, &p1);
  s311a = spinas::sproduct(::SQUARE, &p3, &p1, &p1);
  s351a = spinas::sproduct(::SQUARE, &p3, &p5, &p1);
  a421s = spinas::sproduct(::ANGLE, &p4, &p2, &p1);
  a431s = spinas::sproduct(::ANGLE, &p4, &p3, &p1);
  a213s = spinas::sproduct(::ANGLE, &p2, &p1, &p3);
  a243s = spinas::sproduct(::ANGLE, &p2, &p4, &p3);
  s213a = spinas::sproduct(::SQUARE, &p2, &p1, &p3);
  s243a = spinas::sproduct(::SQUARE, &p2, &p4, &p3);
  s421a = spinas::sproduct(::SQUARE, &p4, &p2, &p1);
  s431a = spinas::sproduct(::SQUARE, &p4, &p3, &p1);
  s411a = spinas::sproduct(::SQUARE, &p4, &p1, &p1);
  s441a = spinas::sproduct(::SQUARE, &p4, &p4, &p1);
  s433a = spinas::sproduct(::SQUARE, &p4, &p3, &p3);
  s443a = spinas::sproduct(::SQUARE, &p4, &p4, &p3);
  s451a = spinas::sproduct(::SQUARE, &p4, &p5, &p1);
  s453a = spinas::sproduct(::SQUARE, &p4, &p5, &p3);
  a331s = spinas::sproduct(::ANGLE, &p3, &p3, &p1);
  s331a = spinas::sproduct(::SQUARE, &p3, &p3, &p1);
  a2311a = spinas::sproduct(::ANGLE, &p2, &p3, &p1, &p1);
  a2351a = spinas::sproduct(::ANGLE, &p2, &p3, &p5, &p1);
  a2411a = spinas::sproduct(::ANGLE, &p2, &p4, &p1, &p1);
  a2451a = spinas::sproduct(::ANGLE, &p2, &p4, &p5, &p1);
  a2231a = spinas::sproduct(::ANGLE, &p2, &p2, &p3, &p1);
  a2241a = spinas::sproduct(::ANGLE, &p2, &p2, &p4, &p1);
  a2531a = spinas::sproduct(::ANGLE, &p2, &p5, &p3, &p1);
  a2541a = spinas::sproduct(::ANGLE, &p2, &p5, &p4, &p1);
  s2311s = spinas::sproduct(::SQUARE, &p2, &p3, &p1, &p1);
  s2351s = spinas::sproduct(::SQUARE, &p2, &p3, &p5, &p1);
  s2411s = spinas::sproduct(::SQUARE, &p2, &p4, &p1, &p1);
  s2451s = spinas::sproduct(::SQUARE, &p2, &p4, &p5, &p1);
  a2133a = spinas::sproduct(::ANGLE, &p2, &p1, &p3, &p3);
  a2153a = spinas::sproduct(::ANGLE, &p2, &p1, &p5, &p3);
  a2433a = spinas::sproduct(::ANGLE, &p2, &p4, &p3, &p3);
  a2453a = spinas::sproduct(::ANGLE, &p2, &p4, &p5, &p3);
  s2133s = spinas::sproduct(::SQUARE, &p2, &p1, &p3, &p3);
  s2153s = spinas::sproduct(::SQUARE, &p2, &p1, &p5, &p3);
  s2433s = spinas::sproduct(::SQUARE, &p2, &p4, &p3, &p3);
  s2453s = spinas::sproduct(::SQUARE, &p2, &p4, &p5, &p3);
  s2231s = spinas::sproduct(::SQUARE, &p2, &p2, &p3, &p1);
  s2241s = spinas::sproduct(::SQUARE, &p2, &p2, &p4, &p1);
  s2531s = spinas::sproduct(::SQUARE, &p2, &p5, &p3, &p1);
  s2541s = spinas::sproduct(::SQUARE, &p2, &p5, &p4, &p1);
  a2213a = spinas::sproduct(::ANGLE, &p2, &p2, &p1, &p3);
  a2243a = spinas::sproduct(::ANGLE, &p2, &p2, &p4, &p3);
  a2513a = spinas::sproduct(::ANGLE, &p2, &p5, &p1, &p3);
  a2543a = spinas::sproduct(::ANGLE, &p2, &p5, &p4, &p3);
  s2213s = spinas::sproduct(::SQUARE, &p2, &p2, &p1, &p3);
  s2243s = spinas::sproduct(::SQUARE, &p2, &p2, &p4, &p3);
  s2513s = spinas::sproduct(::SQUARE, &p2, &p5, &p1, &p3);
  s2543s = spinas::sproduct(::SQUARE, &p2, &p5, &p4, &p3);
  a4211a = spinas::sproduct(::ANGLE, &p4, &p2, &p1, &p1);
  a4251a = spinas::sproduct(::ANGLE, &p4, &p2, &p5, &p1);
  a4311a = spinas::sproduct(::ANGLE, &p4, &p3, &p1, &p1);
  a4351a = spinas::sproduct(::ANGLE, &p4, &p3, &p5, &p1);
  s4211s = spinas::sproduct(::SQUARE, &p4, &p2, &p1, &p1);
  s4251s = spinas::sproduct(::SQUARE, &p4, &p2, &p5, &p1);
  s4311s = spinas::sproduct(::SQUARE, &p4, &p3, &p1, &p1);
  s4351s = spinas::sproduct(::SQUARE, &p4, &p3, &p5, &p1);
  a4421a = spinas::sproduct(::ANGLE, &p4, &p4, &p2, &p1);
  a4431a = spinas::sproduct(::ANGLE, &p4, &p4, &p3, &p1);
  a4521a = spinas::sproduct(::ANGLE, &p4, &p5, &p2, &p1);
  a4531a = spinas::sproduct(::ANGLE, &p4, &p5, &p3, &p1);
  s4421s = spinas::sproduct(::SQUARE, &p4, &p4, &p2, &p1);
  s4431s = spinas::sproduct(::SQUARE, &p4, &p4, &p3, &p1);
  s4521s = spinas::sproduct(::SQUARE, &p4, &p5, &p2, &p1);
  s4531s = spinas::sproduct(::SQUARE, &p4, &p5, &p3, &p1);
}

void draftcoloredcurrent2_color_1::set_parameters(const Parameters& values) {
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
  momentum_chain_16_ready = false;
  parameters = values;
  Mu = parameters[0];
  Mh = parameters[1];
  EE = parameters[2];
  MW = parameters[3];
  SW = parameters[4];
  GG = parameters[5];

  // Recompute model-only factors once after parameter changes.
  parameter_factor_1_2 = (EE * EE * EE);
  parameter_factor_2 = (Mu * Mu * Mu * Mu);
  parameter_factor_1 = parameter_factor_1_2 * parameter_factor_2;
  parameter_factor_4_2 = (MW * MW * MW);
  parameter_factor_5 = (SW * SW * SW);
  parameter_factor_4 = ldouble(8.0L) * parameter_factor_4_2 * parameter_factor_5;
  parameter_factor_7_2 = (Mh * Mh);
  parameter_factor_8 = (Mu * Mu);
  parameter_factor_7 = ldouble(3.0L) * parameter_factor_1_2 * parameter_factor_7_2 * parameter_factor_8;
  parameter_factor_10 = ldouble(4.0L) * parameter_factor_1_2 * parameter_factor_8;
  parameter_factor_11 = ldouble(9.0L) * MW * SW;
  parameter_factor_12 = ldouble(4.0L) * parameter_factor_1_2 * parameter_factor_8 * SW;
  parameter_factor_13_3 = (SW * SW);
  parameter_factor_13_2 = (ldouble(1.0L) - parameter_factor_13_3);
  parameter_factor_13 = ldouble(9.0L) * MW * parameter_factor_13_2;
  parameter_factor_16_2 = std::pow(cdouble(parameter_factor_13_2), ldouble(1.0L / 2.0L));
  parameter_factor_16 = (MW) / (parameter_factor_16_2);
  parameter_factor_18 = ldouble(8.0L) * parameter_factor_1_2 * MW * SW;
  parameter_factor_19_2 = std::pow(cdouble(parameter_factor_13_2), 2);
  parameter_factor_19 = ldouble(9.0L) * parameter_factor_19_2;
  parameter_factor_21_3 = (ldouble(4.0L) * parameter_factor_13_3) / (ldouble(3.0L));
  parameter_factor_21_2 = (ldouble(1.0L) - parameter_factor_21_3);
  parameter_factor_21 = parameter_factor_1_2 * parameter_factor_8 * parameter_factor_21_2;
  parameter_factor_24 = ldouble(3.0L) * MW * SW * parameter_factor_13_2;
  parameter_factor_25 = ldouble(2.0L) * parameter_factor_1_2 * MW * parameter_factor_21_2;
  parameter_factor_26 = ldouble(3.0L) * SW * parameter_factor_19_2;
  parameter_factor_27_2 = std::pow(cdouble(parameter_factor_21_2), 2);
  parameter_factor_27 = parameter_factor_1_2 * parameter_factor_8 * parameter_factor_27_2;
  parameter_factor_29 = ldouble(4.0L) * MW * parameter_factor_5 * parameter_factor_13_2;
  parameter_factor_30 = parameter_factor_1_2 * MW * parameter_factor_27_2;
  parameter_factor_31 = ldouble(2.0L) * parameter_factor_5 * parameter_factor_19_2;
  parameter_factor_32 = ldouble(4.0L) * parameter_factor_1_2 * Mu;
  parameter_factor_33 = ldouble(4.0L) * parameter_factor_1_2 * Mu * SW;
  parameter_factor_34 = parameter_factor_1_2 * Mu * parameter_factor_27_2;
  parameter_factor_35_2 = (Mu * Mu * Mu);
  parameter_factor_35 = parameter_factor_1_2 * parameter_factor_35_2;
  parameter_factor_37 = parameter_factor_1_2 * Mu * parameter_factor_21_2;
  parameter_factor_38 = ldouble(2.0L) * parameter_factor_1_2 * parameter_factor_8 * SW;
  parameter_factor_39 = ldouble(9.0L) * parameter_factor_4_2;
  parameter_factor_40 = ldouble(4.0L) * parameter_factor_1_2 * SW;
  parameter_factor_41 = ldouble(6.0L) * parameter_factor_4_2 * SW;
  parameter_factor_42 = parameter_factor_1_2 * parameter_factor_21_2;
  parameter_factor_43_2 = (GG * GG);
  parameter_factor_43 = EE * parameter_factor_43_2 * parameter_factor_8;
  parameter_factor_45 = MW * SW;
  parameter_factor_46 = EE * parameter_factor_43_2 * Mu;
  parameter_factor_47 = ldouble(3.0L) * parameter_factor_4_2 * SW;
  parameter_factor_48 = parameter_factor_1_2 * parameter_factor_27_2;
  parameter_factor_49 = ldouble(4.0L) * parameter_factor_4_2 * parameter_factor_5;
  parameter_factor_50 = ldouble(2.0L) * parameter_factor_1_2 * Mu * SW;
  external_masses[0] = checked_mass(cdouble(Mu));
  external_masses[1] = checked_mass(cdouble(Mu));
  external_masses[2] = checked_mass(cdouble(Mu));
  external_masses[3] = checked_mass(cdouble(Mu));
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

void draftcoloredcurrent2_color_1::set_widths_enabled(bool enabled) {
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
  momentum_chain_16_ready = false;
}

void draftcoloredcurrent2_color_1::set_masses(const ldouble& value_Mu, const ldouble& value_Mh) {
  Parameters values = parameters;
  values[0] = cdouble(value_Mu);
  values[1] = cdouble(value_Mh);
  set_parameters(values);
}

void draftcoloredcurrent2_color_1::set_momenta(const Momenta& values) {
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
  momentum_chain_16_ready = false;
  momenta = values;

  // External particle momenta.
  p1.set_momentum(momenta[0].data());
  p2.set_momentum(momenta[1].data());
  p3.set_momentum(momenta[2].data());
  p4.set_momentum(momenta[3].data());
  p5.set_momentum(momenta[4].data());

  // Propagator denominators and scalar invariants.
  t34 = ((std::imag(Mh) == ldouble(0) && std::isfinite(std::real(Mh))) ? cdouble(mass2(sum_momenta(momenta, {3,
    4})) - (std::real(Mh)) * (std::real(Mh)), ldouble(0)) : (mass2(sum_momenta(momenta, {3, 4})) - (Mh) *
    (Mh)));
  t15 = ((std::imag(Mu) == ldouble(0) && std::isfinite(std::real(Mu))) ? cdouble(mass2(sum_momenta(momenta, {1,
    -5})) - (std::real(Mu)) * (std::real(Mu)), ldouble(0)) : (mass2(sum_momenta(momenta, {1, -5})) - (Mu) *
    (Mu)));
  t25 = ((std::imag(Mu) == ldouble(0) && std::isfinite(std::real(Mu))) ? cdouble(mass2(sum_momenta(momenta, {2,
    -5})) - (std::real(Mu)) * (std::real(Mu)), ldouble(0)) : (mass2(sum_momenta(momenta, {2, -5})) - (Mu) *
    (Mu)));
  t12 = ((std::imag(Mh) == ldouble(0) && std::isfinite(std::real(Mh))) ? cdouble(mass2(sum_momenta(momenta, {1,
    2})) - (std::real(Mh)) * (std::real(Mh)), ldouble(0)) : (mass2(sum_momenta(momenta, {1, 2})) - (Mh) *
    (Mh)));
  t35 = ((std::imag(Mu) == ldouble(0) && std::isfinite(std::real(Mu))) ? cdouble(mass2(sum_momenta(momenta, {3,
    5})) - (std::real(Mu)) * (std::real(Mu)), ldouble(0)) : (mass2(sum_momenta(momenta, {3, 5})) - (Mu) *
    (Mu)));
  t45 = ((std::imag(Mu) == ldouble(0) && std::isfinite(std::real(Mu))) ? cdouble(mass2(sum_momenta(momenta, {4,
    5})) - (std::real(Mu)) * (std::real(Mu)), ldouble(0)) : (mass2(sum_momenta(momenta, {4, 5})) - (Mu) *
    (Mu)));
  s34 = mass2(sum_momenta(momenta, {3, 4})) - (ldouble(0.0L)) * (ldouble(0.0L));
  t34_2 = mass2(sum_momenta(momenta, {3, 4})) - (parameter_factor_16) * (parameter_factor_16);
  s12 = mass2(sum_momenta(momenta, {1, 2})) - (ldouble(0.0L)) * (ldouble(0.0L));
  t12_2 = mass2(sum_momenta(momenta, {1, 2})) - (parameter_factor_16) * (parameter_factor_16);
  t23 = ((std::imag(Mh) == ldouble(0) && std::isfinite(std::real(Mh))) ? cdouble(mass2(sum_momenta(momenta, {2,
    -3})) - (std::real(Mh)) * (std::real(Mh)), ldouble(0)) : (mass2(sum_momenta(momenta, {2, -3})) - (Mh) *
    (Mh)));
  t14 = ((std::imag(Mh) == ldouble(0) && std::isfinite(std::real(Mh))) ? cdouble(mass2(sum_momenta(momenta, {1,
    -4})) - (std::real(Mh)) * (std::real(Mh)), ldouble(0)) : (mass2(sum_momenta(momenta, {1, -4})) - (Mh) *
    (Mh)));
  p1_dot_p3 = p1.dot(p3);
  p1_dot_p4 = p1.dot(p4);
  p2_dot_p3 = p2.dot(p3);
  p2_dot_p4 = p2.dot(p4);
  s14 = mass2(sum_momenta(momenta, {1, -4})) - (ldouble(0.0L)) * (ldouble(0.0L));
  s23 = mass2(sum_momenta(momenta, {2, -3})) - (ldouble(0.0L)) * (ldouble(0.0L));
  t23_2 = mass2(sum_momenta(momenta, {2, -3})) - (parameter_factor_16) * (parameter_factor_16);
  t14_2 = mass2(sum_momenta(momenta, {1, -4})) - (parameter_factor_16) * (parameter_factor_16);
  p1_dot_p2 = p1.dot(p2);
  p3_dot_p4 = p3.dot(p4);
  momenta_ready = true;
}

// Prepare only data used by the retained helicity contributions.
void draftcoloredcurrent2_color_1::prepare_summed_point_cache() {
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
  if (!momentum_chain_16_ready) {
    momentum_chain_16.update();
    momentum_chain_16_ready = true;
  }
#endif
  a113s.update();
  a133s.update();
  a13a.update();
  a153s.update();
  a211s.update();
  a2133a.update();
  a213s.update();
  a2153a.update();
  a21a.update();
  #ifdef SPINAS_HAS_MOMENTUM_CHAIN
  a2213a.update(momentum_chain_15);
#else
  a2213a.update();
#endif
  a221s.update();
  a2231a.update();
  a223s.update();
  #ifdef SPINAS_HAS_MOMENTUM_CHAIN
  a2241a.update(momentum_chain_4);
#else
  a2241a.update();
#endif
  #ifdef SPINAS_HAS_MOMENTUM_CHAIN
  a2243a.update(momentum_chain_4);
#else
  a2243a.update();
#endif
  a224s.update();
  #ifdef SPINAS_HAS_MOMENTUM_CHAIN
  a2311a.update(momentum_chain_1);
#else
  a2311a.update();
#endif
  a231s.update();
  a233s.update();
  #ifdef SPINAS_HAS_MOMENTUM_CHAIN
  a2351a.update(momentum_chain_2);
#else
  a2351a.update();
#endif
  a23a.update();
  a2411a.update();
  a241s.update();
  #ifdef SPINAS_HAS_MOMENTUM_CHAIN
  a2433a.update(momentum_chain_10);
#else
  a2433a.update();
#endif
  a243s.update();
  a244s.update();
  #ifdef SPINAS_HAS_MOMENTUM_CHAIN
  a2451a.update(momentum_chain_3);
#else
  a2451a.update();
#endif
  #ifdef SPINAS_HAS_MOMENTUM_CHAIN
  a2453a.update(momentum_chain_3);
#else
  a2453a.update();
#endif
  a24a.update();
  a2513a.update();
  a251s.update();
  #ifdef SPINAS_HAS_MOMENTUM_CHAIN
  a2531a.update(momentum_chain_5);
#else
  a2531a.update();
#endif
  a253s.update();
  #ifdef SPINAS_HAS_MOMENTUM_CHAIN
  a2541a.update(momentum_chain_6);
#else
  a2541a.update();
#endif
  #ifdef SPINAS_HAS_MOMENTUM_CHAIN
  a2543a.update(momentum_chain_6);
#else
  a2543a.update();
#endif
  a254s.update();
  a311s.update();
  a31a.update();
  a331s.update();
  a351s.update();
  a411s.update();
  a41a.update();
  #ifdef SPINAS_HAS_MOMENTUM_CHAIN
  a4211a.update(momentum_chain_15);
#else
  a4211a.update();
#endif
  a421s.update();
  a422s.update();
  a4251a.update();
  #ifdef SPINAS_HAS_MOMENTUM_CHAIN
  a4311a.update(momentum_chain_1);
#else
  a4311a.update();
#endif
  a431s.update();
  a433s.update();
  #ifdef SPINAS_HAS_MOMENTUM_CHAIN
  a4351a.update(momentum_chain_2);
#else
  a4351a.update();
#endif
  a43a.update();
  a441s.update();
  a4421a.update();
  a442s.update();
  #ifdef SPINAS_HAS_MOMENTUM_CHAIN
  a4431a.update(momentum_chain_10);
#else
  a4431a.update();
#endif
  a443s.update();
  a451s.update();
  a4521a.update();
  a452s.update();
  #ifdef SPINAS_HAS_MOMENTUM_CHAIN
  a4531a.update(momentum_chain_5);
#else
  a4531a.update();
#endif
  a453s.update();
  s113a.update();
  s133a.update();
  s13s.update();
  s153a.update();
  s211a.update();
  s2133s.update();
  s213a.update();
  s2153s.update();
  s21s.update();
  #ifdef SPINAS_HAS_MOMENTUM_CHAIN
  s2213s.update(momentum_chain_16);
#else
  s2213s.update();
#endif
  s221a.update();
  s2231s.update();
  s223a.update();
  #ifdef SPINAS_HAS_MOMENTUM_CHAIN
  s2241s.update(momentum_chain_12);
#else
  s2241s.update();
#endif
  #ifdef SPINAS_HAS_MOMENTUM_CHAIN
  s2243s.update(momentum_chain_12);
#else
  s2243s.update();
#endif
  #ifdef SPINAS_HAS_MOMENTUM_CHAIN
  s2311s.update(momentum_chain_7);
#else
  s2311s.update();
#endif
  s231a.update();
  s233a.update();
  #ifdef SPINAS_HAS_MOMENTUM_CHAIN
  s2351s.update(momentum_chain_8);
#else
  s2351s.update();
#endif
  s23s.update();
  s2411s.update();
  s241a.update();
  #ifdef SPINAS_HAS_MOMENTUM_CHAIN
  s2433s.update(momentum_chain_11);
#else
  s2433s.update();
#endif
  s243a.update();
  #ifdef SPINAS_HAS_MOMENTUM_CHAIN
  s2451s.update(momentum_chain_9);
#else
  s2451s.update();
#endif
  #ifdef SPINAS_HAS_MOMENTUM_CHAIN
  s2453s.update(momentum_chain_9);
#else
  s2453s.update();
#endif
  s24s.update();
  s2513s.update();
  s251a.update();
  #ifdef SPINAS_HAS_MOMENTUM_CHAIN
  s2531s.update(momentum_chain_13);
#else
  s2531s.update();
#endif
  s253a.update();
  #ifdef SPINAS_HAS_MOMENTUM_CHAIN
  s2541s.update(momentum_chain_14);
#else
  s2541s.update();
#endif
  #ifdef SPINAS_HAS_MOMENTUM_CHAIN
  s2543s.update(momentum_chain_14);
#else
  s2543s.update();
#endif
  s311a.update();
  s31s.update();
  s331a.update();
  s351a.update();
  s411a.update();
  s41s.update();
  #ifdef SPINAS_HAS_MOMENTUM_CHAIN
  s4211s.update(momentum_chain_16);
#else
  s4211s.update();
#endif
  s421a.update();
  s4251s.update();
  #ifdef SPINAS_HAS_MOMENTUM_CHAIN
  s4311s.update(momentum_chain_7);
#else
  s4311s.update();
#endif
  s431a.update();
  s433a.update();
  #ifdef SPINAS_HAS_MOMENTUM_CHAIN
  s4351s.update(momentum_chain_8);
#else
  s4351s.update();
#endif
  s43s.update();
  s441a.update();
  s4421s.update();
  #ifdef SPINAS_HAS_MOMENTUM_CHAIN
  s4431s.update(momentum_chain_11);
#else
  s4431s.update();
#endif
  s443a.update();
  s451a.update();
  s4521s.update();
  #ifdef SPINAS_HAS_MOMENTUM_CHAIN
  s4531s.update(momentum_chain_13);
#else
  s4531s.update();
#endif
  s453a.update();
  real_denominator_domain = std::imag(parameters[0]) == ldouble(0) && std::imag(parameters[1]) == ldouble(0) &&
    std::imag(parameters[2]) == ldouble(0) && std::imag(parameters[3]) == ldouble(0) && std::imag(parameters[4])
    == ldouble(0) && std::imag(parameters[5]) == ldouble(0);
  point_product_1[0][0] = a21a.v(-1, -1);
  point_product_1[0][1] = a21a.v(-1, 1);
  point_product_1[1][0] = a21a.v(1, -1);
  point_product_1[1][1] = a21a.v(1, 1);
  point_product_2[0][0] = a43a.v(-1, -1);
  point_product_2[0][1] = a43a.v(-1, 1);
  point_product_2[1][0] = a43a.v(1, -1);
  point_product_2[1][1] = a43a.v(1, 1);
  point_product_3[0][0] = a24a.v(-1, -1);
  point_product_3[0][1] = a24a.v(-1, 1);
  point_product_3[1][0] = a24a.v(1, -1);
  point_product_3[1][1] = a24a.v(1, 1);
  point_product_4[0][0] = s13s.v(-1, -1);
  point_product_4[0][1] = s13s.v(-1, 1);
  point_product_4[1][0] = s13s.v(1, -1);
  point_product_4[1][1] = s13s.v(1, 1);
  point_product_5[0][0] = s21s.v(-1, -1);
  point_product_5[0][1] = s21s.v(-1, 1);
  point_product_5[1][0] = s21s.v(1, -1);
  point_product_5[1][1] = s21s.v(1, 1);
  point_product_6[0][0] = a41a.v(-1, -1);
  point_product_6[0][1] = a41a.v(-1, 1);
  point_product_6[1][0] = a41a.v(1, -1);
  point_product_6[1][1] = a41a.v(1, 1);
  point_product_7[0][0] = s23s.v(-1, -1);
  point_product_7[0][1] = s23s.v(-1, 1);
  point_product_7[1][0] = s23s.v(1, -1);
  point_product_7[1][1] = s23s.v(1, 1);
  point_product_8[0][0] = a13a.v(-1, -1);
  point_product_8[0][1] = a13a.v(-1, 1);
  point_product_8[1][0] = a13a.v(1, -1);
  point_product_8[1][1] = a13a.v(1, 1);
  point_product_9[0][0] = s24s.v(-1, -1);
  point_product_9[0][1] = s24s.v(-1, 1);
  point_product_9[1][0] = s24s.v(1, -1);
  point_product_9[1][1] = s24s.v(1, 1);
  point_product_10[0][0] = a23a.v(-1, -1);
  point_product_10[0][1] = a23a.v(-1, 1);
  point_product_10[1][0] = a23a.v(1, -1);
  point_product_10[1][1] = a23a.v(1, 1);
  point_product_11[0][0] = s41s.v(-1, -1);
  point_product_11[0][1] = s41s.v(-1, 1);
  point_product_11[1][0] = s41s.v(1, -1);
  point_product_11[1][1] = s41s.v(1, 1);
  point_product_12[0][0] = s43s.v(-1, -1);
  point_product_12[0][1] = s43s.v(-1, 1);
  point_product_12[1][0] = s43s.v(1, -1);
  point_product_12[1][1] = s43s.v(1, 1);
  point_product_13[0][0] = a113s.v(-1, -1);
  point_product_13[0][1] = a113s.v(-1, 1);
  point_product_13[1][0] = a113s.v(1, -1);
  point_product_13[1][1] = a113s.v(1, 1);
  point_product_14[0][0] = a153s.v(-1, -1);
  point_product_14[0][1] = a153s.v(-1, 1);
  point_product_14[1][0] = a153s.v(1, -1);
  point_product_14[1][1] = a153s.v(1, 1);
  point_product_15[0][0] = a133s.v(-1, -1);
  point_product_15[0][1] = a133s.v(-1, 1);
  point_product_15[1][0] = a133s.v(1, -1);
  point_product_15[1][1] = a133s.v(1, 1);
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
  point_product_21[0][0] = a233s.v(-1, -1);
  point_product_21[0][1] = a233s.v(-1, 1);
  point_product_21[1][0] = a233s.v(1, -1);
  point_product_21[1][1] = a233s.v(1, 1);
  point_product_22[0][0] = a224s.v(-1, -1);
  point_product_22[0][1] = a224s.v(-1, 1);
  point_product_22[1][0] = a224s.v(1, -1);
  point_product_22[1][1] = a224s.v(1, 1);
  point_product_23[0][0] = a254s.v(-1, -1);
  point_product_23[0][1] = a254s.v(-1, 1);
  point_product_23[1][0] = a254s.v(1, -1);
  point_product_23[1][1] = a254s.v(1, 1);
  point_product_24[0][0] = a244s.v(-1, -1);
  point_product_24[0][1] = a244s.v(-1, 1);
  point_product_24[1][0] = a244s.v(1, -1);
  point_product_24[1][1] = a244s.v(1, 1);
  point_product_25[0][0] = a231s.v(-1, -1);
  point_product_25[0][1] = a231s.v(-1, 1);
  point_product_25[1][0] = a231s.v(1, -1);
  point_product_25[1][1] = a231s.v(1, 1);
  point_product_26[0][0] = a241s.v(-1, -1);
  point_product_26[0][1] = a241s.v(-1, 1);
  point_product_26[1][0] = a241s.v(1, -1);
  point_product_26[1][1] = a241s.v(1, 1);
  point_product_27[0][0] = a433s.v(-1, -1);
  point_product_27[0][1] = a433s.v(-1, 1);
  point_product_27[1][0] = a433s.v(1, -1);
  point_product_27[1][1] = a433s.v(1, 1);
  point_product_28[0][0] = a443s.v(-1, -1);
  point_product_28[0][1] = a443s.v(-1, 1);
  point_product_28[1][0] = a443s.v(1, -1);
  point_product_28[1][1] = a443s.v(1, 1);
  point_product_29[0][0] = a411s.v(-1, -1);
  point_product_29[0][1] = a411s.v(-1, 1);
  point_product_29[1][0] = a411s.v(1, -1);
  point_product_29[1][1] = a411s.v(1, 1);
  point_product_30[0][0] = a451s.v(-1, -1);
  point_product_30[0][1] = a451s.v(-1, 1);
  point_product_30[1][0] = a451s.v(1, -1);
  point_product_30[1][1] = a451s.v(1, 1);
  point_product_31[0][0] = a441s.v(-1, -1);
  point_product_31[0][1] = a441s.v(-1, 1);
  point_product_31[1][0] = a441s.v(1, -1);
  point_product_31[1][1] = a441s.v(1, 1);
  point_product_32[0][0] = s31s.v(-1, -1);
  point_product_32[0][1] = s31s.v(-1, 1);
  point_product_32[1][0] = s31s.v(1, -1);
  point_product_32[1][1] = s31s.v(1, 1);
  point_product_33[0][0] = a422s.v(-1, -1);
  point_product_33[0][1] = a422s.v(-1, 1);
  point_product_33[1][0] = a422s.v(1, -1);
  point_product_33[1][1] = a422s.v(1, 1);
  point_product_34[0][0] = a452s.v(-1, -1);
  point_product_34[0][1] = a452s.v(-1, 1);
  point_product_34[1][0] = a452s.v(1, -1);
  point_product_34[1][1] = a452s.v(1, 1);
  point_product_35[0][0] = a31a.v(-1, -1);
  point_product_35[0][1] = a31a.v(-1, 1);
  point_product_35[1][0] = a31a.v(1, -1);
  point_product_35[1][1] = a31a.v(1, 1);
  point_product_36[0][0] = a442s.v(-1, -1);
  point_product_36[0][1] = a442s.v(-1, 1);
  point_product_36[1][0] = a442s.v(1, -1);
  point_product_36[1][1] = a442s.v(1, 1);
  point_product_37[0][0] = a453s.v(-1, -1);
  point_product_37[0][1] = a453s.v(-1, 1);
  point_product_37[1][0] = a453s.v(1, -1);
  point_product_37[1][1] = a453s.v(1, 1);
  point_product_38[0][0] = s113a.v(-1, -1);
  point_product_38[0][1] = s113a.v(-1, 1);
  point_product_38[1][0] = s113a.v(1, -1);
  point_product_38[1][1] = s113a.v(1, 1);
  point_product_39[0][0] = s153a.v(-1, -1);
  point_product_39[0][1] = s153a.v(-1, 1);
  point_product_39[1][0] = s153a.v(1, -1);
  point_product_39[1][1] = s153a.v(1, 1);
  point_product_40[0][0] = s133a.v(-1, -1);
  point_product_40[0][1] = s133a.v(-1, 1);
  point_product_40[1][0] = s133a.v(1, -1);
  point_product_40[1][1] = s133a.v(1, 1);
  point_product_41[0][0] = s231a.v(-1, -1);
  point_product_41[0][1] = s231a.v(-1, 1);
  point_product_41[1][0] = s231a.v(1, -1);
  point_product_41[1][1] = s231a.v(1, 1);
  point_product_42[0][0] = s241a.v(-1, -1);
  point_product_42[0][1] = s241a.v(-1, 1);
  point_product_42[1][0] = s241a.v(1, -1);
  point_product_42[1][1] = s241a.v(1, 1);
  point_product_43[0][0] = s221a.v(-1, -1);
  point_product_43[0][1] = s221a.v(-1, 1);
  point_product_43[1][0] = s221a.v(1, -1);
  point_product_43[1][1] = s221a.v(1, 1);
  point_product_44[0][0] = s251a.v(-1, -1);
  point_product_44[0][1] = s251a.v(-1, 1);
  point_product_44[1][0] = s251a.v(1, -1);
  point_product_44[1][1] = s251a.v(1, 1);
  point_product_45[0][0] = s211a.v(-1, -1);
  point_product_45[0][1] = s211a.v(-1, 1);
  point_product_45[1][0] = s211a.v(1, -1);
  point_product_45[1][1] = s211a.v(1, 1);
  point_product_46[0][0] = s223a.v(-1, -1);
  point_product_46[0][1] = s223a.v(-1, 1);
  point_product_46[1][0] = s223a.v(1, -1);
  point_product_46[1][1] = s223a.v(1, 1);
  point_product_47[0][0] = s253a.v(-1, -1);
  point_product_47[0][1] = s253a.v(-1, 1);
  point_product_47[1][0] = s253a.v(1, -1);
  point_product_47[1][1] = s253a.v(1, 1);
  point_product_48[0][0] = s233a.v(-1, -1);
  point_product_48[0][1] = s233a.v(-1, 1);
  point_product_48[1][0] = s233a.v(1, -1);
  point_product_48[1][1] = s233a.v(1, 1);
  point_product_49[0][0] = a311s.v(-1, -1);
  point_product_49[0][1] = a311s.v(-1, 1);
  point_product_49[1][0] = a311s.v(1, -1);
  point_product_49[1][1] = a311s.v(1, 1);
  point_product_50[0][0] = a351s.v(-1, -1);
  point_product_50[0][1] = a351s.v(-1, 1);
  point_product_50[1][0] = a351s.v(1, -1);
  point_product_50[1][1] = a351s.v(1, 1);
  point_product_51[0][0] = s311a.v(-1, -1);
  point_product_51[0][1] = s311a.v(-1, 1);
  point_product_51[1][0] = s311a.v(1, -1);
  point_product_51[1][1] = s311a.v(1, 1);
  point_product_52[0][0] = s351a.v(-1, -1);
  point_product_52[0][1] = s351a.v(-1, 1);
  point_product_52[1][0] = s351a.v(1, -1);
  point_product_52[1][1] = s351a.v(1, 1);
  point_product_53[0][0] = a421s.v(-1, -1);
  point_product_53[0][1] = a421s.v(-1, 1);
  point_product_53[1][0] = a421s.v(1, -1);
  point_product_53[1][1] = a421s.v(1, 1);
  point_product_54[0][0] = a431s.v(-1, -1);
  point_product_54[0][1] = a431s.v(-1, 1);
  point_product_54[1][0] = a431s.v(1, -1);
  point_product_54[1][1] = a431s.v(1, 1);
  point_product_55[0][0] = a213s.v(-1, -1);
  point_product_55[0][1] = a213s.v(-1, 1);
  point_product_55[1][0] = a213s.v(1, -1);
  point_product_55[1][1] = a213s.v(1, 1);
  point_product_56[0][0] = a243s.v(-1, -1);
  point_product_56[0][1] = a243s.v(-1, 1);
  point_product_56[1][0] = a243s.v(1, -1);
  point_product_56[1][1] = a243s.v(1, 1);
  point_product_57[0][0] = s213a.v(-1, -1);
  point_product_57[0][1] = s213a.v(-1, 1);
  point_product_57[1][0] = s213a.v(1, -1);
  point_product_57[1][1] = s213a.v(1, 1);
  point_product_58[0][0] = s243a.v(-1, -1);
  point_product_58[0][1] = s243a.v(-1, 1);
  point_product_58[1][0] = s243a.v(1, -1);
  point_product_58[1][1] = s243a.v(1, 1);
  point_product_59[0][0] = s421a.v(-1, -1);
  point_product_59[0][1] = s421a.v(-1, 1);
  point_product_59[1][0] = s421a.v(1, -1);
  point_product_59[1][1] = s421a.v(1, 1);
  point_product_60[0][0] = s431a.v(-1, -1);
  point_product_60[0][1] = s431a.v(-1, 1);
  point_product_60[1][0] = s431a.v(1, -1);
  point_product_60[1][1] = s431a.v(1, 1);
  point_product_61[0][0] = s411a.v(-1, -1);
  point_product_61[0][1] = s411a.v(-1, 1);
  point_product_61[1][0] = s411a.v(1, -1);
  point_product_61[1][1] = s411a.v(1, 1);
  point_product_62[0][0] = s441a.v(-1, -1);
  point_product_62[0][1] = s441a.v(-1, 1);
  point_product_62[1][0] = s441a.v(1, -1);
  point_product_62[1][1] = s441a.v(1, 1);
  point_product_63[0][0] = s433a.v(-1, -1);
  point_product_63[0][1] = s433a.v(-1, 1);
  point_product_63[1][0] = s433a.v(1, -1);
  point_product_63[1][1] = s433a.v(1, 1);
  point_product_64[0][0] = s443a.v(-1, -1);
  point_product_64[0][1] = s443a.v(-1, 1);
  point_product_64[1][0] = s443a.v(1, -1);
  point_product_64[1][1] = s443a.v(1, 1);
  point_product_65[0][0] = s451a.v(-1, -1);
  point_product_65[0][1] = s451a.v(-1, 1);
  point_product_65[1][0] = s451a.v(1, -1);
  point_product_65[1][1] = s451a.v(1, 1);
  point_product_66[0][0] = s453a.v(-1, -1);
  point_product_66[0][1] = s453a.v(-1, 1);
  point_product_66[1][0] = s453a.v(1, -1);
  point_product_66[1][1] = s453a.v(1, 1);
  point_product_67[0][0] = a331s.v(-1, -1);
  point_product_67[0][1] = a331s.v(-1, 1);
  point_product_67[1][0] = a331s.v(1, -1);
  point_product_67[1][1] = a331s.v(1, 1);
  point_product_68[0][0] = s331a.v(-1, -1);
  point_product_68[0][1] = s331a.v(-1, 1);
  point_product_68[1][0] = s331a.v(1, -1);
  point_product_68[1][1] = s331a.v(1, 1);
  point_product_69[0][0] = a2311a.v(-1, -1);
  point_product_69[0][1] = a2311a.v(-1, 1);
  point_product_69[1][0] = a2311a.v(1, -1);
  point_product_69[1][1] = a2311a.v(1, 1);
  point_product_70[0][0] = a2351a.v(-1, -1);
  point_product_70[0][1] = a2351a.v(-1, 1);
  point_product_70[1][0] = a2351a.v(1, -1);
  point_product_70[1][1] = a2351a.v(1, 1);
  point_product_71[0][0] = a2411a.v(-1, -1);
  point_product_71[0][1] = a2411a.v(-1, 1);
  point_product_71[1][0] = a2411a.v(1, -1);
  point_product_71[1][1] = a2411a.v(1, 1);
  point_product_72[0][0] = a2451a.v(-1, -1);
  point_product_72[0][1] = a2451a.v(-1, 1);
  point_product_72[1][0] = a2451a.v(1, -1);
  point_product_72[1][1] = a2451a.v(1, 1);
  point_product_73[0][0] = a2231a.v(-1, -1);
  point_product_73[0][1] = a2231a.v(-1, 1);
  point_product_73[1][0] = a2231a.v(1, -1);
  point_product_73[1][1] = a2231a.v(1, 1);
  point_product_74[0][0] = a2241a.v(-1, -1);
  point_product_74[0][1] = a2241a.v(-1, 1);
  point_product_74[1][0] = a2241a.v(1, -1);
  point_product_74[1][1] = a2241a.v(1, 1);
  point_product_75[0][0] = a2531a.v(-1, -1);
  point_product_75[0][1] = a2531a.v(-1, 1);
  point_product_75[1][0] = a2531a.v(1, -1);
  point_product_75[1][1] = a2531a.v(1, 1);
  point_product_76[0][0] = a2541a.v(-1, -1);
  point_product_76[0][1] = a2541a.v(-1, 1);
  point_product_76[1][0] = a2541a.v(1, -1);
  point_product_76[1][1] = a2541a.v(1, 1);
  point_product_77[0][0] = s2311s.v(-1, -1);
  point_product_77[0][1] = s2311s.v(-1, 1);
  point_product_77[1][0] = s2311s.v(1, -1);
  point_product_77[1][1] = s2311s.v(1, 1);
  point_product_78[0][0] = s2351s.v(-1, -1);
  point_product_78[0][1] = s2351s.v(-1, 1);
  point_product_78[1][0] = s2351s.v(1, -1);
  point_product_78[1][1] = s2351s.v(1, 1);
  point_product_79[0][0] = s2411s.v(-1, -1);
  point_product_79[0][1] = s2411s.v(-1, 1);
  point_product_79[1][0] = s2411s.v(1, -1);
  point_product_79[1][1] = s2411s.v(1, 1);
  point_product_80[0][0] = s2451s.v(-1, -1);
  point_product_80[0][1] = s2451s.v(-1, 1);
  point_product_80[1][0] = s2451s.v(1, -1);
  point_product_80[1][1] = s2451s.v(1, 1);
  point_product_81[0][0] = a2133a.v(-1, -1);
  point_product_81[0][1] = a2133a.v(-1, 1);
  point_product_81[1][0] = a2133a.v(1, -1);
  point_product_81[1][1] = a2133a.v(1, 1);
  point_product_82[0][0] = a2153a.v(-1, -1);
  point_product_82[0][1] = a2153a.v(-1, 1);
  point_product_82[1][0] = a2153a.v(1, -1);
  point_product_82[1][1] = a2153a.v(1, 1);
  point_product_83[0][0] = a2433a.v(-1, -1);
  point_product_83[0][1] = a2433a.v(-1, 1);
  point_product_83[1][0] = a2433a.v(1, -1);
  point_product_83[1][1] = a2433a.v(1, 1);
  point_product_84[0][0] = a2453a.v(-1, -1);
  point_product_84[0][1] = a2453a.v(-1, 1);
  point_product_84[1][0] = a2453a.v(1, -1);
  point_product_84[1][1] = a2453a.v(1, 1);
  point_product_85[0][0] = s2133s.v(-1, -1);
  point_product_85[0][1] = s2133s.v(-1, 1);
  point_product_85[1][0] = s2133s.v(1, -1);
  point_product_85[1][1] = s2133s.v(1, 1);
  point_product_86[0][0] = s2153s.v(-1, -1);
  point_product_86[0][1] = s2153s.v(-1, 1);
  point_product_86[1][0] = s2153s.v(1, -1);
  point_product_86[1][1] = s2153s.v(1, 1);
  point_product_87[0][0] = s2433s.v(-1, -1);
  point_product_87[0][1] = s2433s.v(-1, 1);
  point_product_87[1][0] = s2433s.v(1, -1);
  point_product_87[1][1] = s2433s.v(1, 1);
  point_product_88[0][0] = s2453s.v(-1, -1);
  point_product_88[0][1] = s2453s.v(-1, 1);
  point_product_88[1][0] = s2453s.v(1, -1);
  point_product_88[1][1] = s2453s.v(1, 1);
  point_product_89[0][0] = s2231s.v(-1, -1);
  point_product_89[0][1] = s2231s.v(-1, 1);
  point_product_89[1][0] = s2231s.v(1, -1);
  point_product_89[1][1] = s2231s.v(1, 1);
  point_product_90[0][0] = s2241s.v(-1, -1);
  point_product_90[0][1] = s2241s.v(-1, 1);
  point_product_90[1][0] = s2241s.v(1, -1);
  point_product_90[1][1] = s2241s.v(1, 1);
  point_product_91[0][0] = s2531s.v(-1, -1);
  point_product_91[0][1] = s2531s.v(-1, 1);
  point_product_91[1][0] = s2531s.v(1, -1);
  point_product_91[1][1] = s2531s.v(1, 1);
  point_product_92[0][0] = s2541s.v(-1, -1);
  point_product_92[0][1] = s2541s.v(-1, 1);
  point_product_92[1][0] = s2541s.v(1, -1);
  point_product_92[1][1] = s2541s.v(1, 1);
  point_product_93[0][0] = a2213a.v(-1, -1);
  point_product_93[0][1] = a2213a.v(-1, 1);
  point_product_93[1][0] = a2213a.v(1, -1);
  point_product_93[1][1] = a2213a.v(1, 1);
  point_product_94[0][0] = a2243a.v(-1, -1);
  point_product_94[0][1] = a2243a.v(-1, 1);
  point_product_94[1][0] = a2243a.v(1, -1);
  point_product_94[1][1] = a2243a.v(1, 1);
  point_product_95[0][0] = a2513a.v(-1, -1);
  point_product_95[0][1] = a2513a.v(-1, 1);
  point_product_95[1][0] = a2513a.v(1, -1);
  point_product_95[1][1] = a2513a.v(1, 1);
  point_product_96[0][0] = a2543a.v(-1, -1);
  point_product_96[0][1] = a2543a.v(-1, 1);
  point_product_96[1][0] = a2543a.v(1, -1);
  point_product_96[1][1] = a2543a.v(1, 1);
  point_product_97[0][0] = s2213s.v(-1, -1);
  point_product_97[0][1] = s2213s.v(-1, 1);
  point_product_97[1][0] = s2213s.v(1, -1);
  point_product_97[1][1] = s2213s.v(1, 1);
  point_product_98[0][0] = s2243s.v(-1, -1);
  point_product_98[0][1] = s2243s.v(-1, 1);
  point_product_98[1][0] = s2243s.v(1, -1);
  point_product_98[1][1] = s2243s.v(1, 1);
  point_product_99[0][0] = s2513s.v(-1, -1);
  point_product_99[0][1] = s2513s.v(-1, 1);
  point_product_99[1][0] = s2513s.v(1, -1);
  point_product_99[1][1] = s2513s.v(1, 1);
  point_product_100[0][0] = s2543s.v(-1, -1);
  point_product_100[0][1] = s2543s.v(-1, 1);
  point_product_100[1][0] = s2543s.v(1, -1);
  point_product_100[1][1] = s2543s.v(1, 1);
  point_product_101[0][0] = a4211a.v(-1, -1);
  point_product_101[0][1] = a4211a.v(-1, 1);
  point_product_101[1][0] = a4211a.v(1, -1);
  point_product_101[1][1] = a4211a.v(1, 1);
  point_product_102[0][0] = a4251a.v(-1, -1);
  point_product_102[0][1] = a4251a.v(-1, 1);
  point_product_102[1][0] = a4251a.v(1, -1);
  point_product_102[1][1] = a4251a.v(1, 1);
  point_product_103[0][0] = a4311a.v(-1, -1);
  point_product_103[0][1] = a4311a.v(-1, 1);
  point_product_103[1][0] = a4311a.v(1, -1);
  point_product_103[1][1] = a4311a.v(1, 1);
  point_product_104[0][0] = a4351a.v(-1, -1);
  point_product_104[0][1] = a4351a.v(-1, 1);
  point_product_104[1][0] = a4351a.v(1, -1);
  point_product_104[1][1] = a4351a.v(1, 1);
  point_product_105[0][0] = s4211s.v(-1, -1);
  point_product_105[0][1] = s4211s.v(-1, 1);
  point_product_105[1][0] = s4211s.v(1, -1);
  point_product_105[1][1] = s4211s.v(1, 1);
  point_product_106[0][0] = s4251s.v(-1, -1);
  point_product_106[0][1] = s4251s.v(-1, 1);
  point_product_106[1][0] = s4251s.v(1, -1);
  point_product_106[1][1] = s4251s.v(1, 1);
  point_product_107[0][0] = s4311s.v(-1, -1);
  point_product_107[0][1] = s4311s.v(-1, 1);
  point_product_107[1][0] = s4311s.v(1, -1);
  point_product_107[1][1] = s4311s.v(1, 1);
  point_product_108[0][0] = s4351s.v(-1, -1);
  point_product_108[0][1] = s4351s.v(-1, 1);
  point_product_108[1][0] = s4351s.v(1, -1);
  point_product_108[1][1] = s4351s.v(1, 1);
  point_product_109[0][0] = a4421a.v(-1, -1);
  point_product_109[0][1] = a4421a.v(-1, 1);
  point_product_109[1][0] = a4421a.v(1, -1);
  point_product_109[1][1] = a4421a.v(1, 1);
  point_product_110[0][0] = a4431a.v(-1, -1);
  point_product_110[0][1] = a4431a.v(-1, 1);
  point_product_110[1][0] = a4431a.v(1, -1);
  point_product_110[1][1] = a4431a.v(1, 1);
  point_product_111[0][0] = a4521a.v(-1, -1);
  point_product_111[0][1] = a4521a.v(-1, 1);
  point_product_111[1][0] = a4521a.v(1, -1);
  point_product_111[1][1] = a4521a.v(1, 1);
  point_product_112[0][0] = a4531a.v(-1, -1);
  point_product_112[0][1] = a4531a.v(-1, 1);
  point_product_112[1][0] = a4531a.v(1, -1);
  point_product_112[1][1] = a4531a.v(1, 1);
  point_product_113[0][0] = s4421s.v(-1, -1);
  point_product_113[0][1] = s4421s.v(-1, 1);
  point_product_113[1][0] = s4421s.v(1, -1);
  point_product_113[1][1] = s4421s.v(1, 1);
  point_product_114[0][0] = s4431s.v(-1, -1);
  point_product_114[0][1] = s4431s.v(-1, 1);
  point_product_114[1][0] = s4431s.v(1, -1);
  point_product_114[1][1] = s4431s.v(1, 1);
  point_product_115[0][0] = s4521s.v(-1, -1);
  point_product_115[0][1] = s4521s.v(-1, 1);
  point_product_115[1][0] = s4521s.v(1, -1);
  point_product_115[1][1] = s4521s.v(1, 1);
  point_product_116[0][0] = s4531s.v(-1, -1);
  point_product_116[0][1] = s4531s.v(-1, 1);
  point_product_116[1][0] = s4531s.v(1, -1);
  point_product_116[1][1] = s4531s.v(1, 1);
  point_factor_1 = (parameter_factor_4 * t34);
  point_factor_2 = (point_factor_1 * t15);
  point_factor_3 = (point_factor_1 * t25);
  point_factor_4 = (point_factor_1 * t12);
  point_factor_5 = (parameter_factor_4 * t35);
  point_factor_6 = (point_factor_5 * t12);
  point_factor_7 = (parameter_factor_4 * t45);
  point_factor_8 = (point_factor_7 * t12);
  point_factor_9 = (parameter_factor_11 * s34);
  point_factor_10 = (point_factor_9 * t15);
  point_factor_11 = (parameter_factor_13 * t34_2);
  point_factor_12 = (point_factor_11 * t15);
  point_factor_13 = (point_factor_9 * t25);
  point_factor_14 = (point_factor_11 * t25);
  point_factor_15 = ((parameter_factor_11 * t35) * s12);
  point_factor_16 = ((parameter_factor_11 * t45) * s12);
  point_factor_17 = ((parameter_factor_19 * t34_2) * t12_2);
  point_factor_18 = ((parameter_factor_13 * t35) * t12_2);
  point_factor_19 = ((parameter_factor_13 * t45) * t12_2);
  point_factor_20 = (parameter_factor_24 * t34_2);
  point_factor_21 = (point_factor_20 * t15);
  point_factor_22 = (point_factor_20 * t25);
  point_factor_23 = ((parameter_factor_26 * t34_2) * t12_2);
  point_factor_24 = ((parameter_factor_24 * t35) * t12_2);
  point_factor_25 = ((parameter_factor_24 * t45) * t12_2);
  point_factor_26 = (parameter_factor_29 * t34_2);
  point_factor_27 = (point_factor_26 * t15);
  point_factor_28 = (point_factor_26 * t25);
  point_factor_29 = ((parameter_factor_31 * t34_2) * t12_2);
  point_factor_30 = ((parameter_factor_29 * t35) * t12_2);
  point_factor_31 = ((parameter_factor_29 * t45) * t12_2);
  point_factor_32 = (parameter_factor_4 * t23);
  point_factor_33 = (point_factor_32 * t14);
  point_factor_34 = (parameter_factor_39 * t34_2);
  point_factor_35 = (point_factor_34 * t15);
  point_factor_36 = (point_factor_34 * t25);
  point_factor_37 = (point_factor_11 * t12_2);
  point_factor_38 = (parameter_factor_40 * p1_dot_p3);
  point_factor_39 = (point_factor_34 * t12_2);
  point_factor_40 = (parameter_factor_40 * p1_dot_p4);
  point_factor_41 = (parameter_factor_40 * p2_dot_p3);
  point_factor_42 = (parameter_factor_40 * p2_dot_p4);
  point_factor_43 = ((parameter_factor_39 * t35) * t12_2);
  point_factor_44 = ((parameter_factor_39 * t45) * t12_2);
  point_factor_45 = (parameter_factor_41 * t34_2);
  point_factor_46 = (point_factor_45 * t15);
  point_factor_47 = (point_factor_45 * t25);
  point_factor_48 = (point_factor_20 * t12_2);
  point_factor_49 = (parameter_factor_11 * s14);
  point_factor_50 = (point_factor_49 * t25);
  point_factor_51 = (parameter_factor_45 * s14);
  point_factor_52 = (point_factor_51 * t25);
  point_factor_53 = (parameter_factor_4 * t14);
  point_factor_54 = (point_factor_53 * t25);
  point_factor_55 = (point_factor_53 * t35);
  point_factor_56 = (parameter_factor_42 * p1_dot_p3);
  point_factor_57 = ((parameter_factor_47 * t34_2) * t12_2);
  point_factor_58 = (parameter_factor_42 * p1_dot_p4);
  point_factor_59 = (parameter_factor_42 * p2_dot_p3);
  point_factor_60 = (parameter_factor_42 * p2_dot_p4);
  point_factor_61 = ((parameter_factor_41 * t35) * t12_2);
  point_factor_62 = ((parameter_factor_41 * t45) * t12_2);
  point_factor_63 = (parameter_factor_11 * s23);
  point_factor_64 = (point_factor_63 * t15);
  point_factor_65 = (parameter_factor_45 * s23);
  point_factor_66 = (point_factor_65 * t15);
  point_factor_67 = ((parameter_factor_26 * t23_2) * t14_2);
  point_factor_68 = ((parameter_factor_31 * t23_2) * t14_2);
  point_factor_69 = ((parameter_factor_19 * t23_2) * t14_2);
  point_factor_70 = (parameter_factor_13 * t23_2);
  point_factor_71 = (point_factor_70 * t14_2);
  point_factor_72 = (parameter_factor_40 * p1_dot_p2);
  point_factor_73 = (parameter_factor_39 * t23_2);
  point_factor_74 = (point_factor_73 * t14_2);
  point_factor_75 = (parameter_factor_40 * p3_dot_p4);
  point_factor_76 = (parameter_factor_24 * t23_2);
  point_factor_77 = (point_factor_76 * t14_2);
  point_factor_78 = (parameter_factor_42 * p1_dot_p2);
  point_factor_79 = ((parameter_factor_47 * t23_2) * t14_2);
  point_factor_80 = (parameter_factor_42 * p3_dot_p4);
  point_factor_81 = (parameter_factor_29 * t23_2);
  point_factor_82 = (point_factor_81 * t14_2);
  point_factor_83 = (parameter_factor_48 * p1_dot_p2);
  point_factor_84 = ((parameter_factor_49 * t23_2) * t14_2);
  point_factor_85 = (parameter_factor_48 * p1_dot_p3);
  point_factor_86 = (parameter_factor_48 * p2_dot_p4);
  point_factor_87 = (parameter_factor_48 * p3_dot_p4);
  point_factor_88 = (parameter_factor_4 * t34_2);
  point_factor_89 = (point_factor_88 * t15);
  point_factor_90 = (point_factor_88 * t25);
  point_factor_91 = (point_factor_26 * t12_2);
  point_factor_92 = ((parameter_factor_49 * t34_2) * t12_2);
  point_factor_93 = (parameter_factor_48 * p1_dot_p4);
  point_factor_94 = (parameter_factor_48 * p2_dot_p3);
  point_factor_95 = (point_factor_32 * t15);
  point_factor_96 = (point_factor_32 * t45);
  point_factor_97 = (point_factor_63 * t45);
  point_factor_98 = (point_factor_65 * t45);
  point_factor_99 = (point_factor_49 * t35);
  point_factor_100 = (point_factor_51 * t35);
  point_factor_101 = (point_factor_5 * t12_2);
  point_factor_102 = (point_factor_7 * t12_2);
  point_factor_103 = (parameter_factor_24 * t14_2);
  point_factor_104 = (point_factor_103 * t35);
  point_factor_105 = (parameter_factor_29 * t14_2);
  point_factor_106 = (point_factor_105 * t35);
  point_factor_107 = (parameter_factor_13 * t14_2);
  point_factor_108 = (point_factor_107 * t35);
  point_factor_109 = (parameter_factor_39 * t14_2);
  point_factor_110 = (point_factor_109 * t35);
  point_factor_111 = (parameter_factor_41 * t14_2);
  point_factor_112 = (point_factor_111 * t35);
  point_factor_113 = (parameter_factor_4 * t14_2);
  point_factor_114 = (point_factor_113 * t35);
  point_factor_115 = (point_factor_103 * t25);
  point_factor_116 = (point_factor_105 * t25);
  point_factor_117 = (point_factor_107 * t25);
  point_factor_118 = (point_factor_109 * t25);
  point_factor_119 = (point_factor_111 * t25);
  point_factor_120 = (point_factor_113 * t25);
  point_factor_121 = (point_factor_76 * t15);
  point_factor_122 = (point_factor_81 * t15);
  point_factor_123 = (point_factor_70 * t15);
  point_factor_124 = (point_factor_73 * t15);
  point_factor_125 = (parameter_factor_41 * t23_2);
  point_factor_126 = (point_factor_125 * t15);
  point_factor_127 = (parameter_factor_4 * t23_2);
  point_factor_128 = (point_factor_127 * t15);
  point_factor_129 = (point_factor_76 * t45);
  point_factor_130 = (point_factor_81 * t45);
  point_factor_131 = (point_factor_70 * t45);
  point_factor_132 = (point_factor_73 * t45);
  point_factor_133 = (point_factor_125 * t45);
  point_factor_134 = (point_factor_127 * t45);
  point_denominator_1 = hepcat_spinas::CachedDenominator(point_factor_2, real_denominator_domain);
  point_denominator_2 = hepcat_spinas::CachedDenominator(point_factor_3, real_denominator_domain);
  point_denominator_3 = hepcat_spinas::CachedDenominator(point_factor_4, real_denominator_domain);
  point_denominator_4 = hepcat_spinas::CachedDenominator(point_factor_6, real_denominator_domain);
  point_denominator_5 = hepcat_spinas::CachedDenominator(point_factor_8, real_denominator_domain);
  point_denominator_6 = hepcat_spinas::CachedDenominator(point_factor_10, real_denominator_domain);
  point_denominator_7 = hepcat_spinas::CachedDenominator(point_factor_13, real_denominator_domain);
  point_denominator_8 = hepcat_spinas::CachedDenominator(point_factor_15, real_denominator_domain);
  point_denominator_9 = hepcat_spinas::CachedDenominator(point_factor_16, real_denominator_domain);
  point_denominator_10 = hepcat_spinas::CachedDenominator(point_factor_33, real_denominator_domain);
  point_denominator_11 = hepcat_spinas::CachedDenominator(ldouble(3.0L), real_denominator_domain);
  point_denominator_12 = hepcat_spinas::CachedDenominator(point_factor_50, real_denominator_domain);
  point_denominator_13 = hepcat_spinas::CachedDenominator(point_factor_52, real_denominator_domain);
  point_denominator_14 = hepcat_spinas::CachedDenominator(ldouble(9.0L), real_denominator_domain);
  point_denominator_15 = hepcat_spinas::CachedDenominator(point_factor_54, real_denominator_domain);
  point_denominator_16 = hepcat_spinas::CachedDenominator(point_factor_55, real_denominator_domain);
  point_denominator_17 = hepcat_spinas::CachedDenominator(point_factor_64, real_denominator_domain);
  point_denominator_18 = hepcat_spinas::CachedDenominator(point_factor_66, real_denominator_domain);
  point_denominator_19 = hepcat_spinas::CachedDenominator(point_factor_95, real_denominator_domain);
  point_denominator_20 = hepcat_spinas::CachedDenominator(point_factor_96, real_denominator_domain);
  point_denominator_21 = hepcat_spinas::CachedDenominator(point_factor_97, real_denominator_domain);
  point_denominator_22 = hepcat_spinas::CachedDenominator(point_factor_98, real_denominator_domain);
  point_denominator_23 = hepcat_spinas::CachedDenominator(point_factor_99, real_denominator_domain);
  point_denominator_24 = hepcat_spinas::CachedDenominator(point_factor_100, real_denominator_domain);
  summed_point_cache_ready = true;
}

// Individual amplitudes can lazily request the remaining helicities.
void draftcoloredcurrent2_color_1::prepare_all_point_cache() {
  if (all_point_cache_ready) return;
  prepare_summed_point_cache();
  all_point_cache_ready = true;
}

void draftcoloredcurrent2_color_1::set_momenta(const ldouble mom1[4], const ldouble mom2[4], const ldouble
  mom3[4], const ldouble mom4[4], const ldouble mom5[4]) {
  Momenta values;
  std::copy(mom1, mom1 + 4, values[0].begin());
  std::copy(mom2, mom2 + 4, values[1].begin());
  std::copy(mom3, mom3 + 4, values[2].begin());
  std::copy(mom4, mom4 + 4, values[3].begin());
  std::copy(mom5, mom5 + 4, values[4].begin());
  set_momenta(values);
}

cdouble draftcoloredcurrent2_color_1::amp(const Spins& spins) {
  return amp(spins[0], spins[1], spins[2], spins[3], spins[4]);
}

// set_momenta(...) must be called before evaluating an amplitude.
cdouble draftcoloredcurrent2_color_1::amp(const int& ds1, const int& ds2, const int& ds3, const int& ds4, const
  int& ds5) {
  if (!momenta_ready) throw std::logic_error("Call set_momenta before amp.");
  check_spin(1, ds1);
  check_spin(1, ds2);
  check_spin(1, ds3);
  check_spin(1, ds4);
  check_spin(0, ds5);
  prepare_all_point_cache();
  return amp_unchecked(ds1, ds2, ds3, ds4, ds5);
}

// Numerical kernel; public amp and spin_sum validate their inputs first.
cdouble draftcoloredcurrent2_color_1::amp_unchecked(const int& ds1, const int& ds2, const int& ds3, const int&
  ds4, const int& ds5) {
  const cdouble a2_P_1s_direct = (p2.langle(ds2, true) * momentum_matrix(sum_momenta(momenta, {1, 2}), false) *
    p1.rsquare(ds1, true));
  const cdouble a4_P_3s_direct = (p4.langle(ds4, true) * momentum_matrix(sum_momenta(momenta, {1, 2}), false) *
    p3.rsquare(ds3, true));
  const cdouble s2_P_1a_direct = (p2.lsquare(ds2, true) * momentum_matrix(sum_momenta(momenta, {1, 2}), true) *
    p1.rangle(ds1, true));
  const cdouble s4_P_3a_direct = (p4.lsquare(ds4, true) * momentum_matrix(sum_momenta(momenta, {1, 2}), true) *
    p3.rangle(ds3, true));
  const cdouble a4_4_P_3a_direct = (p4.langle(ds4, true) * momentum_matrix(sum_momenta(momenta, {4}), false) *
    momentum_matrix(sum_momenta(momenta, {1, 2}), true) * p3.rangle(ds3, true));
  const cdouble a4_5_P_3a_direct = (p4.langle(ds4, true) * momentum_matrix(sum_momenta(momenta, {5}), false) *
    momentum_matrix(sum_momenta(momenta, {1, 2}), true) * p3.rangle(ds3, true));
  const cdouble a4_P_3_3a_direct = (p4.langle(ds4, true) * momentum_matrix(sum_momenta(momenta, {1, 2}), false)
    * momentum_matrix(sum_momenta(momenta, {3}), true) * p3.rangle(ds3, true));
  const cdouble a4_P_5_3a_direct = (p4.langle(ds4, true) * momentum_matrix(sum_momenta(momenta, {1, 2}), false)
    * momentum_matrix(sum_momenta(momenta, {5}), true) * p3.rangle(ds3, true));
  const cdouble s4_4_P_3s_direct = (p4.lsquare(ds4, true) * momentum_matrix(sum_momenta(momenta, {4}), true) *
    momentum_matrix(sum_momenta(momenta, {1, 2}), false) * p3.rsquare(ds3, true));
  const cdouble s4_5_P_3s_direct = (p4.lsquare(ds4, true) * momentum_matrix(sum_momenta(momenta, {5}), true) *
    momentum_matrix(sum_momenta(momenta, {1, 2}), false) * p3.rsquare(ds3, true));
  const cdouble s4_P_3_3s_direct = (p4.lsquare(ds4, true) * momentum_matrix(sum_momenta(momenta, {1, 2}), true)
    * momentum_matrix(sum_momenta(momenta, {3}), false) * p3.rsquare(ds3, true));
  const cdouble s4_P_5_3s_direct = (p4.lsquare(ds4, true) * momentum_matrix(sum_momenta(momenta, {1, 2}), true)
    * momentum_matrix(sum_momenta(momenta, {5}), false) * p3.rsquare(ds3, true));
  const cdouble spin_factor_1 = (parameter_factor_1 * point_product_1[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_2 = (spin_factor_1 * point_product_2[(ds4) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_3 = (parameter_factor_7 * point_product_1[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_4 = (parameter_factor_10 * point_product_3[(ds2) > 0 ? 1 : 0][(ds4) > 0 ? 1 : 0]);
  const cdouble spin_factor_5 = (spin_factor_4 * point_product_4[(ds1) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_6 = (parameter_factor_12 * point_product_3[(ds2) > 0 ? 1 : 0][(ds4) > 0 ? 1 : 0]);
  const cdouble spin_factor_7 = (spin_factor_6 * point_product_4[(ds1) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_8 = (parameter_factor_18 * point_product_3[(ds2) > 0 ? 1 : 0][(ds4) > 0 ? 1 : 0]);
  const cdouble spin_factor_9 = ((parameter_factor_1 * point_product_2[(ds4) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]) *
    point_product_5[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_10 = ((parameter_factor_10 * point_product_6[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0])
    * point_product_7[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_11 = ((parameter_factor_21 * point_product_6[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0])
    * point_product_7[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_12 = ((parameter_factor_10 * point_product_8[(ds1) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0])
    * point_product_9[(ds2) > 0 ? 1 : 0][(ds4) > 0 ? 1 : 0]);
  const cdouble spin_factor_13 = ((parameter_factor_27 * point_product_8[(ds1) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0])
    * point_product_9[(ds2) > 0 ? 1 : 0][(ds4) > 0 ? 1 : 0]);
  const cdouble spin_factor_14 = ((parameter_factor_10 * point_product_10[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0])
    * point_product_11[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_15 = ((parameter_factor_21 * point_product_10[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0])
    * point_product_11[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_16 = (parameter_factor_7 * point_product_10[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_17 = (spin_factor_1 * point_product_12[(ds4) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_18 = ((parameter_factor_1 * point_product_5[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]) *
    point_product_12[(ds4) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_19 = (parameter_factor_32 * point_product_3[(ds2) > 0 ? 1 : 0][(ds4) > 0 ? 1 : 0]);
  const cdouble spin_factor_20 = (point_product_13[(ds1) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0] -
    point_product_14[(ds1) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_21 = (parameter_factor_33 * point_product_3[(ds2) > 0 ? 1 : 0][(ds4) > 0 ? 1 : 0]);
  const cdouble spin_factor_22 = (parameter_factor_32 * point_product_9[(ds2) > 0 ? 1 : 0][(ds4) > 0 ? 1 : 0]);
  const cdouble spin_factor_23 = ((-point_product_15[(ds1) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]) -
    point_product_14[(ds1) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_24 = (parameter_factor_34 * point_product_9[(ds2) > 0 ? 1 : 0][(ds4) > 0 ? 1 : 0]);
  const cdouble spin_factor_25 = (parameter_factor_35 * point_product_2[(ds4) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_26 = (point_product_16[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0] -
    point_product_17[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_27 = (parameter_factor_35 * point_product_12[(ds4) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_28 = (point_product_18[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0] -
    point_product_17[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_29 = (parameter_factor_32 * point_product_6[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_30 = (point_product_19[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0] -
    point_product_20[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_31 = (parameter_factor_37 * point_product_6[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_32 = (parameter_factor_32 * point_product_11[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_33 = ((-point_product_21[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]) -
    point_product_20[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_34 = (parameter_factor_37 * point_product_11[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_35 = (parameter_factor_32 * point_product_8[(ds1) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_36 = (point_product_22[(ds2) > 0 ? 1 : 0][(ds4) > 0 ? 1 : 0] -
    point_product_23[(ds2) > 0 ? 1 : 0][(ds4) > 0 ? 1 : 0]);
  const cdouble spin_factor_37 = (parameter_factor_34 * point_product_8[(ds1) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_38 = (parameter_factor_32 * point_product_4[(ds1) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_39 = ((-point_product_24[(ds2) > 0 ? 1 : 0][(ds4) > 0 ? 1 : 0]) -
    point_product_23[(ds2) > 0 ? 1 : 0][(ds4) > 0 ? 1 : 0]);
  const cdouble spin_factor_40 = (parameter_factor_33 * point_product_4[(ds1) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_41 = ((-point_product_25[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]) -
    point_product_26[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_42 = (point_product_27[(ds4) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0] +
    point_product_28[(ds4) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_43 = ((parameter_factor_38 * spin_factor_41) * spin_factor_42);
  const cdouble spin_factor_44 = (parameter_factor_32 * point_product_7[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_45 = (point_product_29[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0] -
    point_product_30[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_46 = (parameter_factor_37 * point_product_7[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_47 = (parameter_factor_32 * point_product_10[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_48 = (point_product_31[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0] +
    point_product_30[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_49 = (parameter_factor_37 * point_product_10[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_50 = (parameter_factor_32 * point_product_32[(ds3) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_51 = (point_product_33[(ds4) > 0 ? 1 : 0][(ds2) > 0 ? 1 : 0] -
    point_product_34[(ds4) > 0 ? 1 : 0][(ds2) > 0 ? 1 : 0]);
  const cdouble spin_factor_52 = (parameter_factor_33 * point_product_32[(ds3) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_53 = (parameter_factor_32 * point_product_35[(ds3) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_54 = (point_product_36[(ds4) > 0 ? 1 : 0][(ds2) > 0 ? 1 : 0] +
    point_product_34[(ds4) > 0 ? 1 : 0][(ds2) > 0 ? 1 : 0]);
  const cdouble spin_factor_55 = (parameter_factor_34 * point_product_35[(ds3) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_56 = (parameter_factor_35 * point_product_1[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_57 = (point_product_27[(ds4) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0] +
    point_product_37[(ds4) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_58 = (parameter_factor_35 * point_product_5[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_59 = (point_product_28[(ds4) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0] +
    point_product_37[(ds4) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_60 = ((parameter_factor_38 * a2_P_1s_direct) * a4_P_3s_direct);
  const cdouble spin_factor_61 = (point_product_38[(ds1) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0] -
    point_product_39[(ds1) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_62 = (point_product_40[(ds1) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0] +
    point_product_39[(ds1) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_63 = ((-point_product_41[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]) -
    point_product_42[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_64 = ((parameter_factor_21 * spin_factor_42) * spin_factor_63);
  const cdouble spin_factor_65 = (-((parameter_factor_10 * point_product_2[(ds4) > 0 ? 1 : 0][(ds3) > 0 ? 1 :
    0]) * point_product_5[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]));
  const cdouble spin_factor_66 = ((parameter_factor_10 * point_product_35[(ds3) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0])
    * point_product_9[(ds2) > 0 ? 1 : 0][(ds4) > 0 ? 1 : 0]);
  const cdouble spin_factor_67 = (spin_factor_4 * point_product_32[(ds3) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_68 = ((parameter_factor_10 * point_product_1[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0])
    * point_product_12[(ds4) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_69 = (parameter_factor_32 * point_product_2[(ds4) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_70 = (parameter_factor_32 * point_product_12[(ds4) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_71 = (point_product_43[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0] -
    point_product_44[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_72 = (-((parameter_factor_43 * point_product_2[(ds4) > 0 ? 1 : 0][(ds3) > 0 ? 1 :
    0]) * point_product_5[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]));
  const cdouble spin_factor_73 = ((parameter_factor_43 * point_product_35[(ds3) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0])
    * point_product_9[(ds2) > 0 ? 1 : 0][(ds4) > 0 ? 1 : 0]);
  const cdouble spin_factor_74 = ((parameter_factor_43 * point_product_3[(ds2) > 0 ? 1 : 0][(ds4) > 0 ? 1 : 0])
    * point_product_32[(ds3) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_75 = ((parameter_factor_43 * point_product_1[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0])
    * point_product_12[(ds4) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_76 = (parameter_factor_46 * point_product_2[(ds4) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_77 = (parameter_factor_46 * point_product_12[(ds4) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_78 = (point_product_45[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0] -
    point_product_44[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_79 = (parameter_factor_1 * point_product_10[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_80 = (spin_factor_79 * point_product_6[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_81 = ((parameter_factor_1 * point_product_6[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]) *
    point_product_7[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_82 = (spin_factor_79 * point_product_11[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_83 = ((parameter_factor_1 * point_product_7[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]) *
    point_product_11[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_84 = (parameter_factor_35 * point_product_6[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_85 = (parameter_factor_35 * point_product_11[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_86 = (point_product_46[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0] -
    point_product_47[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_87 = (point_product_48[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0] +
    point_product_47[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_88 = ((parameter_factor_21 * a4_P_3s_direct) * s2_P_1a_direct);
  const cdouble spin_factor_89 = (point_product_49[(ds3) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0] -
    point_product_50[(ds3) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_90 = (point_product_51[(ds3) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0] -
    point_product_52[(ds3) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_91 = (parameter_factor_46 * point_product_9[(ds2) > 0 ? 1 : 0][(ds4) > 0 ? 1 : 0]);
  const cdouble spin_factor_92 = (parameter_factor_46 * point_product_3[(ds2) > 0 ? 1 : 0][(ds4) > 0 ? 1 : 0]);
  const cdouble spin_factor_93 = (point_product_19[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0] -
    point_product_21[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_94 = ((-point_product_53[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]) +
    point_product_54[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_95 = ((-point_product_29[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]) +
    point_product_31[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_96 = (point_product_55[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0] -
    point_product_56[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_97 = ((-point_product_46[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]) +
    point_product_48[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_98 = (point_product_57[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0] -
    point_product_58[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_99 = (point_product_59[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0] -
    point_product_60[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_100 = (point_product_61[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0] -
    point_product_62[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_101 = (point_product_63[(ds4) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0] +
    point_product_64[(ds4) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_102 = ((parameter_factor_21 * spin_factor_41) * spin_factor_101);
  const cdouble spin_factor_103 = ((parameter_factor_27 * spin_factor_63) * spin_factor_101);
  const cdouble spin_factor_104 = (parameter_factor_35 * point_product_10[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 :
    0]);
  const cdouble spin_factor_105 = (parameter_factor_35 * point_product_7[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_106 = (point_product_61[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0] -
    point_product_65[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_107 = ((-point_product_62[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]) -
    point_product_65[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_108 = (parameter_factor_32 * point_product_1[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_109 = (parameter_factor_32 * point_product_5[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_110 = ((-point_product_64[(ds4) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]) -
    point_product_66[(ds4) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_111 = (parameter_factor_46 * point_product_1[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_112 = (parameter_factor_46 * point_product_5[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_113 = (point_product_63[(ds4) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0] +
    point_product_66[(ds4) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_114 = (point_product_67[(ds3) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0] +
    point_product_50[(ds3) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_115 = ((-point_product_68[(ds3) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]) -
    point_product_52[(ds3) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_116 = ((parameter_factor_21 * a2_P_1s_direct) * s4_P_3a_direct);
  const cdouble spin_factor_117 = ((parameter_factor_27 * s2_P_1a_direct) * s4_P_3a_direct);
  const cdouble spin_factor_118 = (parameter_factor_50 * spin_factor_42);
  const cdouble spin_factor_119 = ((((-point_product_69[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]) +
    point_product_70[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]) - point_product_71[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1
    : 0]) + point_product_72[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_120 = (parameter_factor_37 * spin_factor_101);
  const cdouble spin_factor_121 = (parameter_factor_37 * spin_factor_42);
  const cdouble spin_factor_122 = ((((-point_product_73[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]) -
    point_product_74[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]) + point_product_75[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1
    : 0]) + point_product_76[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_123 = (parameter_factor_34 * spin_factor_101);
  const cdouble spin_factor_124 = (parameter_factor_37 * a2_P_1s_direct);
  const cdouble spin_factor_125 = (a4_4_P_3a_direct + a4_5_P_3a_direct);
  const cdouble spin_factor_126 = (parameter_factor_34 * s2_P_1a_direct);
  const cdouble spin_factor_127 = (parameter_factor_50 * a2_P_1s_direct);
  const cdouble spin_factor_128 = ((-a4_P_3_3a_direct) - a4_P_5_3a_direct);
  const cdouble spin_factor_129 = (parameter_factor_37 * s2_P_1a_direct);
  const cdouble spin_factor_130 = ((((-point_product_77[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]) +
    point_product_78[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]) - point_product_79[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1
    : 0]) + point_product_80[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_131 = ((parameter_factor_21 * point_product_2[(ds4) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0])
    * point_product_5[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_132 = ((parameter_factor_27 * point_product_35[(ds3) > 0 ? 1 : 0][(ds1) > 0 ? 1 :
    0]) * point_product_9[(ds2) > 0 ? 1 : 0][(ds4) > 0 ? 1 : 0]);
  const cdouble spin_factor_133 = (spin_factor_6 * point_product_32[(ds3) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_134 = ((parameter_factor_21 * point_product_1[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0])
    * point_product_12[(ds4) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_135 = ((parameter_factor_38 * spin_factor_96) * spin_factor_95);
  const cdouble spin_factor_136 = (parameter_factor_37 * point_product_5[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_137 = ((parameter_factor_21 * spin_factor_95) * spin_factor_98);
  const cdouble spin_factor_138 = ((parameter_factor_21 * spin_factor_96) * spin_factor_100);
  const cdouble spin_factor_139 = ((parameter_factor_27 * spin_factor_98) * spin_factor_100);
  const cdouble spin_factor_140 = (parameter_factor_37 * point_product_1[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_141 = (parameter_factor_50 * spin_factor_95);
  const cdouble spin_factor_142 = (((point_product_81[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0] +
    point_product_82[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]) - point_product_83[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1
    : 0]) - point_product_84[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_143 = (parameter_factor_37 * spin_factor_100);
  const cdouble spin_factor_144 = (parameter_factor_37 * spin_factor_95);
  const cdouble spin_factor_145 = ((((-point_product_85[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]) -
    point_product_86[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]) + point_product_87[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1
    : 0]) + point_product_88[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_146 = (parameter_factor_34 * spin_factor_100);
  const cdouble spin_factor_147 = ((((-point_product_89[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]) -
    point_product_90[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]) + point_product_91[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1
    : 0]) + point_product_92[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_148 = (parameter_factor_37 * point_product_2[(ds4) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_149 = (parameter_factor_37 * point_product_12[(ds4) > 0 ? 1 : 0][(ds3) > 0 ? 1 :
    0]);
  const cdouble spin_factor_150 = (((point_product_93[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0] -
    point_product_94[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]) - point_product_95[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1
    : 0]) + point_product_96[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_151 = (((point_product_97[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0] -
    point_product_98[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]) - point_product_99[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1
    : 0]) + point_product_100[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_152 = ((parameter_factor_38 * spin_factor_93) * spin_factor_94);
  const cdouble spin_factor_153 = ((parameter_factor_21 * spin_factor_94) * spin_factor_97);
  const cdouble spin_factor_154 = ((parameter_factor_21 * spin_factor_93) * spin_factor_99);
  const cdouble spin_factor_155 = ((parameter_factor_27 * spin_factor_97) * spin_factor_99);
  const cdouble spin_factor_156 = (parameter_factor_50 * spin_factor_93);
  const cdouble spin_factor_157 = ((((-point_product_101[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]) +
    point_product_102[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]) + point_product_103[(ds4) > 0 ? 1 : 0][(ds1) > 0 ?
    1 : 0]) - point_product_104[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_158 = (parameter_factor_37 * spin_factor_97);
  const cdouble spin_factor_159 = (parameter_factor_37 * spin_factor_93);
  const cdouble spin_factor_160 = (((point_product_105[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0] -
    point_product_106[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]) - point_product_107[(ds4) > 0 ? 1 : 0][(ds1) > 0 ?
    1 : 0]) + point_product_108[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_161 = (parameter_factor_34 * spin_factor_97);
  const cdouble spin_factor_162 = (((point_product_109[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0] -
    point_product_110[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]) + point_product_111[(ds4) > 0 ? 1 : 0][(ds1) > 0 ?
    1 : 0]) - point_product_112[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_163 = ((((-point_product_113[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]) +
    point_product_114[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]) - point_product_115[(ds4) > 0 ? 1 : 0][(ds1) > 0 ?
    1 : 0]) + point_product_116[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_164 = ((-s4_4_P_3s_direct) - s4_5_P_3s_direct);
  const cdouble spin_factor_165 = ((-s4_P_3_3s_direct) - s4_P_5_3s_direct);
  return (-point_denominator_1.divide(spin_factor_2) - point_denominator_2.divide(spin_factor_2) -
    point_denominator_3.divide((spin_factor_3 * point_product_2[(ds4) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0])) -
    point_denominator_4.divide(spin_factor_2) - point_denominator_5.divide(spin_factor_2) +
    point_denominator_6.divide(spin_factor_5) + (spin_factor_7 / point_factor_12) +
    point_denominator_7.divide(spin_factor_5) + (spin_factor_7 / point_factor_14) +
    point_denominator_8.divide(spin_factor_5) + point_denominator_9.divide(spin_factor_5) + ((spin_factor_8 *
    point_product_4[(ds1) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]) / point_factor_17) + (spin_factor_7 /
    point_factor_18) + (spin_factor_7 / point_factor_19) - point_denominator_1.divide(spin_factor_9) -
    point_denominator_2.divide(spin_factor_9) - point_denominator_3.divide(((parameter_factor_7 *
    point_product_2[(ds4) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]) * point_product_5[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 :
    0])) - point_denominator_4.divide(spin_factor_9) - point_denominator_5.divide(spin_factor_9) -
    point_denominator_6.divide(spin_factor_10) + (spin_factor_11 / point_factor_21) -
    point_denominator_7.divide(spin_factor_10) + (spin_factor_11 / point_factor_22) -
    point_denominator_8.divide(spin_factor_10) - point_denominator_9.divide(spin_factor_10) +
    (((parameter_factor_25 * point_product_6[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]) * point_product_7[(ds2) > 0
    ? 1 : 0][(ds3) > 0 ? 1 : 0]) / point_factor_23) + (spin_factor_11 / point_factor_24) + (spin_factor_11 /
    point_factor_25) + point_denominator_6.divide(spin_factor_12) + (spin_factor_13 / point_factor_27) +
    point_denominator_7.divide(spin_factor_12) + (spin_factor_13 / point_factor_28) +
    point_denominator_8.divide(spin_factor_12) + point_denominator_9.divide(spin_factor_12) +
    (((parameter_factor_30 * point_product_8[(ds1) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]) * point_product_9[(ds2) > 0
    ? 1 : 0][(ds4) > 0 ? 1 : 0]) / point_factor_29) + (spin_factor_13 / point_factor_30) + (spin_factor_13 /
    point_factor_31) - point_denominator_6.divide(spin_factor_14) + (spin_factor_15 / point_factor_21) -
    point_denominator_7.divide(spin_factor_14) + (spin_factor_15 / point_factor_22) -
    point_denominator_8.divide(spin_factor_14) - point_denominator_9.divide(spin_factor_14) +
    (((parameter_factor_25 * point_product_10[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]) * point_product_11[(ds4) >
    0 ? 1 : 0][(ds1) > 0 ? 1 : 0]) / point_factor_23) + (spin_factor_15 / point_factor_24) + (spin_factor_15 /
    point_factor_25) + point_denominator_11.divide((((point_denominator_10.divide((spin_factor_16 *
    point_product_6[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0])) - point_denominator_10.divide(((parameter_factor_7 *
    point_product_6[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]) * point_product_7[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 :
    0]))) - point_denominator_10.divide((spin_factor_16 * point_product_11[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 :
    0]))) + point_denominator_10.divide(((parameter_factor_7 * point_product_7[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1
    : 0]) * point_product_11[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0])))) -
    point_denominator_1.divide(spin_factor_17) - point_denominator_2.divide(spin_factor_17) -
    point_denominator_3.divide((spin_factor_3 * point_product_12[(ds4) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0])) -
    point_denominator_4.divide(spin_factor_17) - point_denominator_5.divide(spin_factor_17) -
    point_denominator_1.divide(spin_factor_18) - point_denominator_2.divide(spin_factor_18) -
    point_denominator_3.divide(((parameter_factor_7 * point_product_5[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]) *
    point_product_12[(ds4) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0])) - point_denominator_4.divide(spin_factor_18) -
    point_denominator_5.divide(spin_factor_18) + point_denominator_6.divide((spin_factor_19 * spin_factor_20)) +
    ((spin_factor_21 * spin_factor_20) / point_factor_12) + point_denominator_8.divide((spin_factor_22 *
    spin_factor_23)) + ((spin_factor_24 * spin_factor_23) / point_factor_30) +
    point_denominator_1.divide((spin_factor_25 * spin_factor_26)) + point_denominator_1.divide((spin_factor_27 *
    spin_factor_26)) - point_denominator_2.divide((spin_factor_25 * spin_factor_28)) -
    point_denominator_2.divide((spin_factor_27 * spin_factor_28)) - point_denominator_7.divide((spin_factor_29 *
    spin_factor_30)) + ((spin_factor_31 * spin_factor_30) / point_factor_22) -
    point_denominator_8.divide((spin_factor_32 * spin_factor_33)) + ((spin_factor_34 * spin_factor_33) /
    point_factor_24) + point_denominator_7.divide((spin_factor_35 * spin_factor_36)) + ((spin_factor_37 *
    spin_factor_36) / point_factor_28) + point_denominator_9.divide((spin_factor_38 * spin_factor_39)) +
    ((spin_factor_40 * spin_factor_39) / point_factor_19) - (spin_factor_43 / point_factor_35) - (spin_factor_43
    / point_factor_36) - (((parameter_factor_40 * spin_factor_41) * spin_factor_42) / point_factor_37) -
    (((point_factor_38 * a2_P_1s_direct) * spin_factor_42) / point_factor_39) - (((point_factor_40 *
    a2_P_1s_direct) * spin_factor_42) / point_factor_39) - (((point_factor_41 * a2_P_1s_direct) *
    spin_factor_42) / point_factor_39) - (((point_factor_42 * a2_P_1s_direct) * spin_factor_42) /
    point_factor_39) + point_denominator_6.divide((spin_factor_44 * spin_factor_45)) - ((spin_factor_46 *
    spin_factor_45) / point_factor_21) - point_denominator_9.divide((spin_factor_47 * spin_factor_48)) +
    ((spin_factor_49 * spin_factor_48) / point_factor_25) - point_denominator_7.divide((spin_factor_50 *
    spin_factor_51)) - ((spin_factor_52 * spin_factor_51) / point_factor_14) +
    point_denominator_9.divide((spin_factor_53 * spin_factor_54)) + ((spin_factor_55 * spin_factor_54) /
    point_factor_31) + point_denominator_4.divide((spin_factor_56 * spin_factor_57)) +
    point_denominator_4.divide((spin_factor_58 * spin_factor_57)) - point_denominator_5.divide((spin_factor_56 *
    spin_factor_59)) - point_denominator_5.divide((spin_factor_58 * spin_factor_59)) + (((parameter_factor_40 *
    a2_P_1s_direct) * a4_P_3s_direct) / point_factor_37) + (spin_factor_60 / point_factor_43) + (spin_factor_60
    / point_factor_44) + point_denominator_6.divide((spin_factor_22 * spin_factor_61)) + ((spin_factor_24 *
    spin_factor_61) / point_factor_27) - point_denominator_8.divide((spin_factor_19 * spin_factor_62)) -
    ((spin_factor_21 * spin_factor_62) / point_factor_18) + (spin_factor_64 / point_factor_46) + (spin_factor_64
    / point_factor_47) + (((parameter_factor_42 * spin_factor_42) * spin_factor_63) / point_factor_48) +
    point_denominator_11.divide((((((((point_denominator_12.divide(spin_factor_65) -
    point_denominator_12.divide(spin_factor_66)) - point_denominator_12.divide(spin_factor_67)) -
    point_denominator_12.divide(spin_factor_68)) - point_denominator_12.divide((spin_factor_69 *
    spin_factor_28))) - point_denominator_12.divide((spin_factor_53 * spin_factor_36))) +
    point_denominator_12.divide((spin_factor_38 * spin_factor_51))) -
    point_denominator_12.divide((spin_factor_70 * spin_factor_71)))) +
    point_denominator_14.divide((ldouble(4.0L) * (((((((point_denominator_13.divide(spin_factor_72) -
    point_denominator_13.divide(spin_factor_73)) - point_denominator_13.divide(spin_factor_74)) -
    point_denominator_13.divide(spin_factor_75)) - point_denominator_13.divide((spin_factor_76 *
    spin_factor_28))) - point_denominator_13.divide(((parameter_factor_46 * point_product_35[(ds3) > 0 ? 1 :
    0][(ds1) > 0 ? 1 : 0]) * spin_factor_36))) + point_denominator_13.divide(((parameter_factor_46 *
    point_product_4[(ds1) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]) * spin_factor_51))) -
    point_denominator_13.divide((spin_factor_77 * spin_factor_71))))) +
    point_denominator_1.divide((spin_factor_25 * spin_factor_78)) + point_denominator_1.divide((spin_factor_27 *
    spin_factor_78)) - point_denominator_2.divide((spin_factor_25 * spin_factor_71)) -
    point_denominator_2.divide((spin_factor_27 * spin_factor_71)) +
    point_denominator_11.divide((((((((point_denominator_15.divide(spin_factor_80) -
    point_denominator_15.divide(spin_factor_81)) - point_denominator_15.divide(spin_factor_82)) +
    point_denominator_15.divide(spin_factor_83)) - point_denominator_15.divide((spin_factor_84 *
    spin_factor_30))) + point_denominator_15.divide((spin_factor_85 * spin_factor_30))) +
    point_denominator_15.divide((spin_factor_84 * spin_factor_86))) -
    point_denominator_15.divide((spin_factor_85 * spin_factor_86)))) -
    point_denominator_7.divide((spin_factor_32 * spin_factor_86)) + ((spin_factor_34 * spin_factor_86) /
    point_factor_22) + point_denominator_8.divide((spin_factor_29 * spin_factor_87)) - ((spin_factor_31 *
    spin_factor_87) / point_factor_24) +
    point_denominator_11.divide((((((((point_denominator_16.divide(spin_factor_80) -
    point_denominator_16.divide(spin_factor_81)) - point_denominator_16.divide(spin_factor_82)) +
    point_denominator_16.divide(spin_factor_83)) + point_denominator_16.divide((spin_factor_84 *
    spin_factor_33))) - point_denominator_16.divide((spin_factor_85 * spin_factor_33))) +
    point_denominator_16.divide((spin_factor_84 * spin_factor_87))) -
    point_denominator_16.divide((spin_factor_85 * spin_factor_87)))) + (((point_factor_56 * spin_factor_42) *
    s2_P_1a_direct) / point_factor_57) + (((point_factor_58 * spin_factor_42) * s2_P_1a_direct) /
    point_factor_57) + (((point_factor_59 * spin_factor_42) * s2_P_1a_direct) / point_factor_57) +
    (((point_factor_60 * spin_factor_42) * s2_P_1a_direct) / point_factor_57) - (((parameter_factor_42 *
    a4_P_3s_direct) * s2_P_1a_direct) / point_factor_48) - (spin_factor_88 / point_factor_61) - (spin_factor_88
    / point_factor_62) + point_denominator_11.divide((((((((point_denominator_17.divide(spin_factor_65) -
    point_denominator_17.divide(spin_factor_66)) - point_denominator_17.divide(spin_factor_67)) -
    point_denominator_17.divide(spin_factor_68)) + point_denominator_17.divide((spin_factor_70 *
    spin_factor_26))) + point_denominator_17.divide((spin_factor_22 * spin_factor_89))) +
    point_denominator_17.divide((spin_factor_69 * spin_factor_78))) +
    point_denominator_17.divide((spin_factor_19 * spin_factor_90)))) +
    point_denominator_14.divide((ldouble(4.0L) * (((((((point_denominator_18.divide(spin_factor_72) -
    point_denominator_18.divide(spin_factor_73)) - point_denominator_18.divide(spin_factor_74)) -
    point_denominator_18.divide(spin_factor_75)) + point_denominator_18.divide((spin_factor_77 *
    spin_factor_26))) + point_denominator_18.divide((spin_factor_91 * spin_factor_89))) +
    point_denominator_18.divide((spin_factor_76 * spin_factor_78))) +
    point_denominator_18.divide((spin_factor_92 * spin_factor_90))))) +
    point_denominator_11.divide(((((((((((((((((((((((((((((((parameter_factor_25 * point_product_2[(ds4) > 0 ?
    1 : 0][(ds3) > 0 ? 1 : 0]) * point_product_5[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]) / point_factor_67) -
    (((parameter_factor_30 * point_product_35[(ds3) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]) * point_product_9[(ds2) > 0
    ? 1 : 0][(ds4) > 0 ? 1 : 0]) / point_factor_68)) - ((spin_factor_8 * point_product_32[(ds3) > 0 ? 1 :
    0][(ds1) > 0 ? 1 : 0]) / point_factor_69)) + (((parameter_factor_25 * point_product_1[(ds2) > 0 ? 1 :
    0][(ds1) > 0 ? 1 : 0]) * point_product_12[(ds4) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]) / point_factor_67)) +
    (((parameter_factor_40 * spin_factor_93) * spin_factor_94) / point_factor_71)) - (((point_factor_72 *
    spin_factor_93) * spin_factor_95) / point_factor_74)) + (((point_factor_38 * spin_factor_93) *
    spin_factor_95) / point_factor_74)) + (((point_factor_42 * spin_factor_93) * spin_factor_95) /
    point_factor_74)) - (((point_factor_75 * spin_factor_93) * spin_factor_95) / point_factor_74)) +
    (((parameter_factor_40 * spin_factor_96) * spin_factor_95) / point_factor_71)) - (((parameter_factor_42 *
    spin_factor_94) * spin_factor_97) / point_factor_77)) + (((point_factor_78 * spin_factor_95) *
    spin_factor_97) / point_factor_79)) - (((point_factor_56 * spin_factor_95) * spin_factor_97) /
    point_factor_79)) - (((point_factor_60 * spin_factor_95) * spin_factor_97) / point_factor_79)) +
    (((point_factor_80 * spin_factor_95) * spin_factor_97) / point_factor_79)) + (((parameter_factor_42 *
    spin_factor_95) * spin_factor_98) / point_factor_77)) - (((parameter_factor_42 * spin_factor_93) *
    spin_factor_99) / point_factor_77)) + (((parameter_factor_48 * spin_factor_97) * spin_factor_99) /
    point_factor_82)) + (((point_factor_78 * spin_factor_93) * spin_factor_100) / point_factor_79)) -
    (((point_factor_56 * spin_factor_93) * spin_factor_100) / point_factor_79)) - (((point_factor_60 *
    spin_factor_93) * spin_factor_100) / point_factor_79)) + (((point_factor_80 * spin_factor_93) *
    spin_factor_100) / point_factor_79)) - (((parameter_factor_42 * spin_factor_96) * spin_factor_100) /
    point_factor_77)) - (((point_factor_83 * spin_factor_97) * spin_factor_100) / point_factor_84)) +
    (((point_factor_85 * spin_factor_97) * spin_factor_100) / point_factor_84)) + (((point_factor_86 *
    spin_factor_97) * spin_factor_100) / point_factor_84)) - (((point_factor_87 * spin_factor_97) *
    spin_factor_100) / point_factor_84)) - (((parameter_factor_48 * spin_factor_98) * spin_factor_100) /
    point_factor_82))) + (spin_factor_102 / point_factor_46) + (spin_factor_102 / point_factor_47) +
    (((parameter_factor_42 * spin_factor_41) * spin_factor_101) / point_factor_48) + (((point_factor_56 *
    a2_P_1s_direct) * spin_factor_101) / point_factor_57) + (((point_factor_58 * a2_P_1s_direct) *
    spin_factor_101) / point_factor_57) + (((point_factor_59 * a2_P_1s_direct) * spin_factor_101) /
    point_factor_57) + (((point_factor_60 * a2_P_1s_direct) * spin_factor_101) / point_factor_57) -
    (spin_factor_103 / point_factor_89) - (spin_factor_103 / point_factor_90) - (((parameter_factor_48 *
    spin_factor_63) * spin_factor_101) / point_factor_91) - (((point_factor_85 * s2_P_1a_direct) *
    spin_factor_101) / point_factor_92) - (((point_factor_93 * s2_P_1a_direct) * spin_factor_101) /
    point_factor_92) - (((point_factor_94 * s2_P_1a_direct) * spin_factor_101) / point_factor_92) -
    (((point_factor_86 * s2_P_1a_direct) * spin_factor_101) / point_factor_92) +
    point_denominator_11.divide((((((((point_denominator_19.divide(spin_factor_80) -
    point_denominator_19.divide(spin_factor_81)) - point_denominator_19.divide(spin_factor_82)) +
    point_denominator_19.divide(spin_factor_83)) - point_denominator_19.divide((spin_factor_104 *
    spin_factor_45))) + point_denominator_19.divide((spin_factor_105 * spin_factor_45))) +
    point_denominator_19.divide((spin_factor_104 * spin_factor_106))) -
    point_denominator_19.divide((spin_factor_105 * spin_factor_106)))) +
    point_denominator_11.divide((((((((point_denominator_20.divide(spin_factor_80) -
    point_denominator_20.divide(spin_factor_81)) - point_denominator_20.divide(spin_factor_82)) +
    point_denominator_20.divide(spin_factor_83)) - point_denominator_20.divide((spin_factor_104 *
    spin_factor_48))) + point_denominator_20.divide((spin_factor_105 * spin_factor_48))) -
    point_denominator_20.divide((spin_factor_104 * spin_factor_107))) +
    point_denominator_20.divide((spin_factor_105 * spin_factor_107)))) +
    point_denominator_6.divide((spin_factor_47 * spin_factor_106)) - ((spin_factor_49 * spin_factor_106) /
    point_factor_21) + point_denominator_9.divide((spin_factor_44 * spin_factor_107)) - ((spin_factor_46 *
    spin_factor_107) / point_factor_25) +
    point_denominator_11.divide((((((((point_denominator_21.divide(spin_factor_65) -
    point_denominator_21.divide(spin_factor_66)) - point_denominator_21.divide(spin_factor_67)) -
    point_denominator_21.divide(spin_factor_68)) - point_denominator_21.divide((spin_factor_50 *
    spin_factor_39))) - point_denominator_21.divide((spin_factor_35 * spin_factor_54))) -
    point_denominator_21.divide((spin_factor_108 * spin_factor_59))) +
    point_denominator_21.divide((spin_factor_109 * spin_factor_110)))) +
    point_denominator_14.divide((ldouble(4.0L) * (((((((point_denominator_22.divide(spin_factor_72) -
    point_denominator_22.divide(spin_factor_73)) - point_denominator_22.divide(spin_factor_74)) -
    point_denominator_22.divide(spin_factor_75)) - point_denominator_22.divide(((parameter_factor_46 *
    point_product_32[(ds3) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]) * spin_factor_39))) -
    point_denominator_22.divide(((parameter_factor_46 * point_product_8[(ds1) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]) *
    spin_factor_54))) - point_denominator_22.divide((spin_factor_111 * spin_factor_59))) +
    point_denominator_22.divide((spin_factor_112 * spin_factor_110))))) +
    point_denominator_5.divide((spin_factor_56 * spin_factor_110)) + point_denominator_5.divide((spin_factor_58
    * spin_factor_110)) + point_denominator_4.divide((spin_factor_56 * spin_factor_113)) +
    point_denominator_4.divide((spin_factor_58 * spin_factor_113)) +
    point_denominator_11.divide((((((((point_denominator_23.divide(spin_factor_65) -
    point_denominator_23.divide(spin_factor_66)) - point_denominator_23.divide(spin_factor_67)) -
    point_denominator_23.divide(spin_factor_68)) - point_denominator_23.divide((spin_factor_19 *
    spin_factor_114))) + point_denominator_23.divide((spin_factor_109 * spin_factor_57))) +
    point_denominator_23.divide((spin_factor_22 * spin_factor_115))) +
    point_denominator_23.divide((spin_factor_108 * spin_factor_113)))) +
    point_denominator_14.divide((ldouble(4.0L) * (((((((point_denominator_24.divide(spin_factor_72) -
    point_denominator_24.divide(spin_factor_73)) - point_denominator_24.divide(spin_factor_74)) -
    point_denominator_24.divide(spin_factor_75)) - point_denominator_24.divide((spin_factor_92 *
    spin_factor_114))) + point_denominator_24.divide((spin_factor_112 * spin_factor_57))) +
    point_denominator_24.divide((spin_factor_91 * spin_factor_115))) +
    point_denominator_24.divide((spin_factor_111 * spin_factor_113))))) - (((parameter_factor_42 *
    a2_P_1s_direct) * s4_P_3a_direct) / point_factor_48) - (spin_factor_116 / point_factor_61) -
    (spin_factor_116 / point_factor_62) + (((parameter_factor_48 * s2_P_1a_direct) * s4_P_3a_direct) /
    point_factor_91) + (spin_factor_117 / point_factor_101) + (spin_factor_117 / point_factor_102) +
    ((spin_factor_118 * spin_factor_119) / point_factor_35) - ((spin_factor_120 * spin_factor_119) /
    point_factor_46) + ((spin_factor_121 * spin_factor_122) / point_factor_47) - ((spin_factor_123 *
    spin_factor_122) / point_factor_90) - ((spin_factor_124 * spin_factor_125) / point_factor_62) +
    ((spin_factor_126 * spin_factor_125) / point_factor_102) + ((spin_factor_127 * spin_factor_128) /
    point_factor_43) - ((spin_factor_129 * spin_factor_128) / point_factor_61) - ((spin_factor_121 *
    spin_factor_130) / point_factor_46) + ((spin_factor_123 * spin_factor_130) / point_factor_89) +
    point_denominator_11.divide(((((((((((((((((spin_factor_131 / point_factor_104) - (spin_factor_132 /
    point_factor_106)) - (spin_factor_133 / point_factor_108)) + (spin_factor_134 / point_factor_104)) -
    ((spin_factor_21 * spin_factor_114) / point_factor_108)) + (spin_factor_135 / point_factor_110)) -
    ((spin_factor_136 * spin_factor_57) / point_factor_104)) + (spin_factor_137 / point_factor_112)) +
    ((spin_factor_24 * spin_factor_115) / point_factor_106)) - (spin_factor_138 / point_factor_112)) -
    (spin_factor_139 / point_factor_114)) - ((spin_factor_140 * spin_factor_113) / point_factor_104)) -
    ((spin_factor_141 * spin_factor_142) / point_factor_110)) + ((spin_factor_143 * spin_factor_142) /
    point_factor_112)) + ((spin_factor_144 * spin_factor_145) / point_factor_112)) - ((spin_factor_146 *
    spin_factor_145) / point_factor_114))) - ((spin_factor_118 * spin_factor_147) / point_factor_36) +
    ((spin_factor_120 * spin_factor_147) / point_factor_47) +
    point_denominator_11.divide(((((((((((((((((spin_factor_131 / point_factor_115) - (spin_factor_132 /
    point_factor_116)) - (spin_factor_133 / point_factor_117)) + (spin_factor_134 / point_factor_115)) +
    ((spin_factor_148 * spin_factor_28) / point_factor_115)) - ((spin_factor_55 * spin_factor_36) /
    point_factor_116)) + (spin_factor_135 / point_factor_118)) + ((spin_factor_40 * spin_factor_51) /
    point_factor_117)) + (spin_factor_137 / point_factor_119)) + ((spin_factor_149 * spin_factor_71) /
    point_factor_115)) - (spin_factor_138 / point_factor_119)) - (spin_factor_139 / point_factor_120)) +
    ((spin_factor_144 * spin_factor_150) / point_factor_119)) - ((spin_factor_146 * spin_factor_150) /
    point_factor_120)) + ((spin_factor_141 * spin_factor_151) / point_factor_118)) - ((spin_factor_143 *
    spin_factor_151) / point_factor_119))) + point_denominator_11.divide(((((((((((((((((spin_factor_131 /
    point_factor_121) - (spin_factor_132 / point_factor_122)) - (spin_factor_133 / point_factor_123)) +
    (spin_factor_134 / point_factor_121)) - ((spin_factor_149 * spin_factor_26) / point_factor_121)) +
    ((spin_factor_24 * spin_factor_89) / point_factor_122)) + (spin_factor_152 / point_factor_124)) -
    (spin_factor_153 / point_factor_126)) - ((spin_factor_148 * spin_factor_78) / point_factor_121)) +
    ((spin_factor_21 * spin_factor_90) / point_factor_123)) - (spin_factor_154 / point_factor_126)) +
    (spin_factor_155 / point_factor_128)) - ((spin_factor_156 * spin_factor_157) / point_factor_124)) +
    ((spin_factor_158 * spin_factor_157) / point_factor_126)) + ((spin_factor_159 * spin_factor_160) /
    point_factor_126)) - ((spin_factor_161 * spin_factor_160) / point_factor_128))) +
    point_denominator_11.divide(((((((((((((((((spin_factor_131 / point_factor_129) - (spin_factor_132 /
    point_factor_130)) - (spin_factor_133 / point_factor_131)) + (spin_factor_134 / point_factor_129)) -
    ((spin_factor_52 * spin_factor_39) / point_factor_131)) + (spin_factor_152 / point_factor_132)) -
    ((spin_factor_37 * spin_factor_54) / point_factor_130)) + ((spin_factor_140 * spin_factor_59) /
    point_factor_129)) - (spin_factor_153 / point_factor_133)) - (spin_factor_154 / point_factor_133)) +
    (spin_factor_155 / point_factor_134)) - ((spin_factor_136 * spin_factor_110) / point_factor_129)) -
    ((spin_factor_159 * spin_factor_162) / point_factor_133)) + ((spin_factor_161 * spin_factor_162) /
    point_factor_134)) + ((spin_factor_156 * spin_factor_163) / point_factor_132)) - ((spin_factor_158 *
    spin_factor_163) / point_factor_133))) - ((spin_factor_127 * spin_factor_164) / point_factor_44) +
    ((spin_factor_129 * spin_factor_164) / point_factor_62) - ((spin_factor_124 * spin_factor_165) /
    point_factor_61) + ((spin_factor_126 * spin_factor_165) / point_factor_101));
}

// Explicit spin and helicity sums.
ldouble draftcoloredcurrent2_color_1::spin_sum() {
  ldouble result = 0;
  cdouble amplitude;
  if (!momenta_ready) throw std::logic_error("Call set_momenta before spin_sum.");
  prepare_all_point_cache();
  const int j5 = 0;
  for (int j1 = -1; j1 <= 1; j1 += 2) {
    for (int j2 = -1; j2 <= 1; j2 += 2) {
      for (int j3 = -1; j3 <= 1; j3 += 2) {
        for (int j4 = -1; j4 <= 1; j4 += 2) {
          amplitude = amp_unchecked(j1, j2, j3, j4, j5);
          result += std::norm(amplitude);
        }
      }
    }
  }
  return result;
}

ldouble draftcoloredcurrent2_color_1::amp2() {
  // Initial-state average: 1/4.
  // Final-state identical-particle factor: 1.
  return spin_sum() / ldouble(4.0L);
}

} // namespace hepcat_generated

#include "process.hpp"
#include <algorithm>
#include <cmath>
#include <stdexcept>

namespace hepcat_generated {
using namespace hepcat_spinas;

constexpr std::size_t draftcoloredcurrent2_color_2::leg_count;
constexpr std::size_t draftcoloredcurrent2_color_2::parameter_count;
constexpr std::size_t draftcoloredcurrent2_color_2::helicity_count;


draftcoloredcurrent2_color_2::draftcoloredcurrent2_color_2() : draftcoloredcurrent2_color_2(default_parameters()) {}

draftcoloredcurrent2_color_2::draftcoloredcurrent2_color_2(const Parameters& values) {
  set_parameters(values);
}

draftcoloredcurrent2_color_2::draftcoloredcurrent2_color_2(const ldouble& value_EE, const ldouble& value_GG,
  const ldouble& value_MW, const ldouble& value_SW, const ldouble& value_Mu, const ldouble& value_Mh)
  : draftcoloredcurrent2_color_2(Parameters{{cdouble(value_Mu), cdouble(value_Mh), cdouble(value_EE),
    cdouble(value_GG), cdouble(value_MW), cdouble(value_SW)}}) {}

// Copies rebuild caches, so every sproduct points into its own process.
draftcoloredcurrent2_color_2::draftcoloredcurrent2_color_2(const draftcoloredcurrent2_color_2& other) {
  widths_enabled = other.widths_enabled;
  set_parameters(other.parameters);
  if (other.momenta_ready) set_momenta(other.momenta);
}

draftcoloredcurrent2_color_2& draftcoloredcurrent2_color_2::operator=(const draftcoloredcurrent2_color_2& other) {
  if (this != &other) {
    widths_enabled = other.widths_enabled;
    set_parameters(other.parameters);
    if (other.momenta_ready) set_momenta(other.momenta);
  }
  return *this;
}

draftcoloredcurrent2_color_2::Parameters draftcoloredcurrent2_color_2::default_parameters() {
  return Parameters{{
    cdouble(ldouble(2.16e-3L), ldouble(0.0L)), // "Mu"
    cdouble(ldouble(1.25e2L), ldouble(0.0L)), // "Mh"
    cdouble(ldouble(3.1333e-1L), ldouble(0.0L)), // "EE"
    cdouble(ldouble(1.117e0L), ldouble(0.0L)), // "GG"
    cdouble(ldouble(8.0385e1L), ldouble(0.0L)), // "MW"
    cdouble(ldouble(4.74e-1L), ldouble(0.0L)) // "SW"
  }};
}

draftcoloredcurrent2_color_2::Spins draftcoloredcurrent2_color_2::default_spins() {
  return Spins{{-1, -1, -1, -1, 0}};
}

std::array<std::size_t, 2> draftcoloredcurrent2_color_2::incoming_legs() {
  return {{0, 1}};
}

std::array<draftcoloredcurrent2_color_2::Spins, draftcoloredcurrent2_color_2::helicity_count>
draftcoloredcurrent2_color_2::helicity_configurations() {
  return std::array<Spins, helicity_count>{{
    {{0, 0, 0, 0, 0}}
  }};
}

ldouble draftcoloredcurrent2_color_2::incoming_spin_average() {
  return ldouble(4.0e0L);
}

ldouble draftcoloredcurrent2_color_2::symmetry_factor() {
  return ldouble(1.0e0L);
}

void draftcoloredcurrent2_color_2::initialize_products() {
#ifdef SPINAS_HAS_MOMENTUM_CHAIN
  momentum_chain_1 = spinas::momentum_chain(::ANGLE, {&p4, &p3});
  momentum_chain_2 = spinas::momentum_chain(::SQUARE, {&p4, &p3});
  momentum_chain_3 = spinas::momentum_chain(::ANGLE, {&p2, &p1});
  momentum_chain_4 = spinas::momentum_chain(::SQUARE, {&p2, &p1});
#endif
  a24a = spinas::sproduct(::ANGLE, &p2, &p4);
  s13s = spinas::sproduct(::SQUARE, &p1, &p3);
  a41a = spinas::sproduct(::ANGLE, &p4, &p1);
  s23s = spinas::sproduct(::SQUARE, &p2, &p3);
  a13a = spinas::sproduct(::ANGLE, &p1, &p3);
  s24s = spinas::sproduct(::SQUARE, &p2, &p4);
  a23a = spinas::sproduct(::ANGLE, &p2, &p3);
  s41s = spinas::sproduct(::SQUARE, &p4, &p1);
  a113s = spinas::sproduct(::ANGLE, &p1, &p1, &p3);
  a153s = spinas::sproduct(::ANGLE, &p1, &p5, &p3);
  a133s = spinas::sproduct(::ANGLE, &p1, &p3, &p3);
  a223s = spinas::sproduct(::ANGLE, &p2, &p2, &p3);
  a253s = spinas::sproduct(::ANGLE, &p2, &p5, &p3);
  a233s = spinas::sproduct(::ANGLE, &p2, &p3, &p3);
  a224s = spinas::sproduct(::ANGLE, &p2, &p2, &p4);
  a254s = spinas::sproduct(::ANGLE, &p2, &p5, &p4);
  a244s = spinas::sproduct(::ANGLE, &p2, &p4, &p4);
  a411s = spinas::sproduct(::ANGLE, &p4, &p1, &p1);
  a451s = spinas::sproduct(::ANGLE, &p4, &p5, &p1);
  a441s = spinas::sproduct(::ANGLE, &p4, &p4, &p1);
  s31s = spinas::sproduct(::SQUARE, &p3, &p1);
  a422s = spinas::sproduct(::ANGLE, &p4, &p2, &p2);
  a452s = spinas::sproduct(::ANGLE, &p4, &p5, &p2);
  a31a = spinas::sproduct(::ANGLE, &p3, &p1);
  a442s = spinas::sproduct(::ANGLE, &p4, &p4, &p2);
  s113a = spinas::sproduct(::SQUARE, &p1, &p1, &p3);
  s153a = spinas::sproduct(::SQUARE, &p1, &p5, &p3);
  s133a = spinas::sproduct(::SQUARE, &p1, &p3, &p3);
  a43a = spinas::sproduct(::ANGLE, &p4, &p3);
  s21s = spinas::sproduct(::SQUARE, &p2, &p1);
  a21a = spinas::sproduct(::ANGLE, &p2, &p1);
  s43s = spinas::sproduct(::SQUARE, &p4, &p3);
  a221s = spinas::sproduct(::ANGLE, &p2, &p2, &p1);
  a251s = spinas::sproduct(::ANGLE, &p2, &p5, &p1);
  s221a = spinas::sproduct(::SQUARE, &p2, &p2, &p1);
  s251a = spinas::sproduct(::SQUARE, &p2, &p5, &p1);
  s223a = spinas::sproduct(::SQUARE, &p2, &p2, &p3);
  s253a = spinas::sproduct(::SQUARE, &p2, &p5, &p3);
  s233a = spinas::sproduct(::SQUARE, &p2, &p3, &p3);
  a211s = spinas::sproduct(::ANGLE, &p2, &p1, &p1);
  a311s = spinas::sproduct(::ANGLE, &p3, &p1, &p1);
  a351s = spinas::sproduct(::ANGLE, &p3, &p5, &p1);
  s211a = spinas::sproduct(::SQUARE, &p2, &p1, &p1);
  s311a = spinas::sproduct(::SQUARE, &p3, &p1, &p1);
  s351a = spinas::sproduct(::SQUARE, &p3, &p5, &p1);
  a421s = spinas::sproduct(::ANGLE, &p4, &p2, &p1);
  a431s = spinas::sproduct(::ANGLE, &p4, &p3, &p1);
  a213s = spinas::sproduct(::ANGLE, &p2, &p1, &p3);
  a243s = spinas::sproduct(::ANGLE, &p2, &p4, &p3);
  s213a = spinas::sproduct(::SQUARE, &p2, &p1, &p3);
  s243a = spinas::sproduct(::SQUARE, &p2, &p4, &p3);
  s421a = spinas::sproduct(::SQUARE, &p4, &p2, &p1);
  s431a = spinas::sproduct(::SQUARE, &p4, &p3, &p1);
  s411a = spinas::sproduct(::SQUARE, &p4, &p1, &p1);
  s441a = spinas::sproduct(::SQUARE, &p4, &p4, &p1);
  s451a = spinas::sproduct(::SQUARE, &p4, &p5, &p1);
  a443s = spinas::sproduct(::ANGLE, &p4, &p4, &p3);
  a453s = spinas::sproduct(::ANGLE, &p4, &p5, &p3);
  s443a = spinas::sproduct(::SQUARE, &p4, &p4, &p3);
  s453a = spinas::sproduct(::SQUARE, &p4, &p5, &p3);
  a331s = spinas::sproduct(::ANGLE, &p3, &p3, &p1);
  a433s = spinas::sproduct(::ANGLE, &p4, &p3, &p3);
  s331a = spinas::sproduct(::SQUARE, &p3, &p3, &p1);
  s433a = spinas::sproduct(::SQUARE, &p4, &p3, &p3);
  a2133a = spinas::sproduct(::ANGLE, &p2, &p1, &p3, &p3);
  a2153a = spinas::sproduct(::ANGLE, &p2, &p1, &p5, &p3);
  a2433a = spinas::sproduct(::ANGLE, &p2, &p4, &p3, &p3);
  a2453a = spinas::sproduct(::ANGLE, &p2, &p4, &p5, &p3);
  s2133s = spinas::sproduct(::SQUARE, &p2, &p1, &p3, &p3);
  s2153s = spinas::sproduct(::SQUARE, &p2, &p1, &p5, &p3);
  s2433s = spinas::sproduct(::SQUARE, &p2, &p4, &p3, &p3);
  s2453s = spinas::sproduct(::SQUARE, &p2, &p4, &p5, &p3);
  a2213a = spinas::sproduct(::ANGLE, &p2, &p2, &p1, &p3);
  a2243a = spinas::sproduct(::ANGLE, &p2, &p2, &p4, &p3);
  a2513a = spinas::sproduct(::ANGLE, &p2, &p5, &p1, &p3);
  a2543a = spinas::sproduct(::ANGLE, &p2, &p5, &p4, &p3);
  s2213s = spinas::sproduct(::SQUARE, &p2, &p2, &p1, &p3);
  s2243s = spinas::sproduct(::SQUARE, &p2, &p2, &p4, &p3);
  s2513s = spinas::sproduct(::SQUARE, &p2, &p5, &p1, &p3);
  s2543s = spinas::sproduct(::SQUARE, &p2, &p5, &p4, &p3);
  a4211a = spinas::sproduct(::ANGLE, &p4, &p2, &p1, &p1);
  a4251a = spinas::sproduct(::ANGLE, &p4, &p2, &p5, &p1);
  a4311a = spinas::sproduct(::ANGLE, &p4, &p3, &p1, &p1);
  a4351a = spinas::sproduct(::ANGLE, &p4, &p3, &p5, &p1);
  s4211s = spinas::sproduct(::SQUARE, &p4, &p2, &p1, &p1);
  s4251s = spinas::sproduct(::SQUARE, &p4, &p2, &p5, &p1);
  s4311s = spinas::sproduct(::SQUARE, &p4, &p3, &p1, &p1);
  s4351s = spinas::sproduct(::SQUARE, &p4, &p3, &p5, &p1);
  a4421a = spinas::sproduct(::ANGLE, &p4, &p4, &p2, &p1);
  a4431a = spinas::sproduct(::ANGLE, &p4, &p4, &p3, &p1);
  a4521a = spinas::sproduct(::ANGLE, &p4, &p5, &p2, &p1);
  a4531a = spinas::sproduct(::ANGLE, &p4, &p5, &p3, &p1);
  s4421s = spinas::sproduct(::SQUARE, &p4, &p4, &p2, &p1);
  s4431s = spinas::sproduct(::SQUARE, &p4, &p4, &p3, &p1);
  s4521s = spinas::sproduct(::SQUARE, &p4, &p5, &p2, &p1);
  s4531s = spinas::sproduct(::SQUARE, &p4, &p5, &p3, &p1);
}

void draftcoloredcurrent2_color_2::set_parameters(const Parameters& values) {
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
  parameters = values;
  Mu = parameters[0];
  Mh = parameters[1];
  EE = parameters[2];
  GG = parameters[3];
  MW = parameters[4];
  SW = parameters[5];

  // Recompute model-only factors once after parameter changes.
  parameter_factor_1_2 = (GG * GG);
  parameter_factor_2 = (Mu * Mu);
  parameter_factor_1 = EE * parameter_factor_1_2 * parameter_factor_2;
  parameter_factor_4 = MW * SW;
  parameter_factor_5_2 = (EE * EE * EE);
  parameter_factor_6 = (Mh * Mh);
  parameter_factor_5 = ldouble(3.0L) * parameter_factor_5_2 * parameter_factor_6 * parameter_factor_2;
  parameter_factor_8_2 = (MW * MW * MW);
  parameter_factor_9 = (SW * SW * SW);
  parameter_factor_8 = ldouble(8.0L) * parameter_factor_8_2 * parameter_factor_9;
  parameter_factor_11 = EE * parameter_factor_1_2 * Mu;
  parameter_factor_12 = ldouble(4.0L) * parameter_factor_5_2 * parameter_factor_2;
  parameter_factor_13 = ldouble(9.0L) * MW * SW;
  parameter_factor_14 = ldouble(4.0L) * parameter_factor_5_2 * Mu;
  parameter_factor_15_2 = (Mu * Mu * Mu * Mu);
  parameter_factor_15 = parameter_factor_5_2 * parameter_factor_15_2;
  parameter_factor_17_2 = (Mu * Mu * Mu);
  parameter_factor_17 = parameter_factor_5_2 * parameter_factor_17_2;
  parameter_factor_19_4 = (SW * SW);
  parameter_factor_19_3 = (ldouble(4.0L) * parameter_factor_19_4) / (ldouble(3.0L));
  parameter_factor_19_2 = (ldouble(1.0L) - parameter_factor_19_3);
  parameter_factor_19 = ldouble(2.0L) * parameter_factor_5_2 * MW * parameter_factor_19_2;
  parameter_factor_23_3 = (ldouble(1.0L) - parameter_factor_19_4);
  parameter_factor_23_2 = std::pow(cdouble(parameter_factor_23_3), 2);
  parameter_factor_23 = ldouble(3.0L) * SW * parameter_factor_23_2;
  parameter_factor_26_2 = std::pow(cdouble(parameter_factor_23_3), ldouble(1.0L / 2.0L));
  parameter_factor_26 = (MW) / (parameter_factor_26_2);
  parameter_factor_28_2 = std::pow(cdouble(parameter_factor_19_2), 2);
  parameter_factor_28 = parameter_factor_5_2 * MW * parameter_factor_28_2;
  parameter_factor_30 = ldouble(2.0L) * parameter_factor_9 * parameter_factor_23_2;
  parameter_factor_31 = ldouble(8.0L) * parameter_factor_5_2 * MW * SW;
  parameter_factor_32 = ldouble(9.0L) * parameter_factor_23_2;
  parameter_factor_33 = ldouble(4.0L) * parameter_factor_5_2 * SW;
  parameter_factor_34 = ldouble(9.0L) * MW * parameter_factor_23_3;
  parameter_factor_35 = ldouble(9.0L) * parameter_factor_8_2;
  parameter_factor_36 = parameter_factor_5_2 * parameter_factor_19_2;
  parameter_factor_37 = ldouble(3.0L) * MW * SW * parameter_factor_23_3;
  parameter_factor_38 = ldouble(3.0L) * parameter_factor_8_2 * SW;
  parameter_factor_39 = parameter_factor_5_2 * parameter_factor_28_2;
  parameter_factor_40 = ldouble(4.0L) * MW * parameter_factor_9 * parameter_factor_23_3;
  parameter_factor_41 = ldouble(4.0L) * parameter_factor_8_2 * parameter_factor_9;
  parameter_factor_42 = parameter_factor_5_2 * parameter_factor_2 * parameter_factor_19_2;
  parameter_factor_43 = parameter_factor_5_2 * parameter_factor_2 * parameter_factor_28_2;
  parameter_factor_44 = ldouble(4.0L) * parameter_factor_5_2 * parameter_factor_2 * SW;
  parameter_factor_45 = ldouble(4.0L) * parameter_factor_5_2 * Mu * SW;
  parameter_factor_46 = ldouble(2.0L) * parameter_factor_5_2 * parameter_factor_2 * SW;
  parameter_factor_47 = parameter_factor_5_2 * Mu * parameter_factor_19_2;
  parameter_factor_48 = ldouble(6.0L) * parameter_factor_8_2 * SW;
  parameter_factor_49 = parameter_factor_5_2 * Mu * parameter_factor_28_2;
  parameter_factor_50 = ldouble(2.0L) * parameter_factor_5_2 * Mu * SW;
  external_masses[0] = checked_mass(cdouble(Mu));
  external_masses[1] = checked_mass(cdouble(Mu));
  external_masses[2] = checked_mass(cdouble(Mu));
  external_masses[3] = checked_mass(cdouble(Mu));
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

void draftcoloredcurrent2_color_2::set_widths_enabled(bool enabled) {
  if (widths_enabled == enabled) return;
  widths_enabled = enabled;
  momenta_ready = false;
  all_point_cache_ready = false;
  summed_point_cache_ready = false;
  momentum_chain_1_ready = false;
  momentum_chain_2_ready = false;
  momentum_chain_3_ready = false;
  momentum_chain_4_ready = false;
}

void draftcoloredcurrent2_color_2::set_masses(const ldouble& value_Mu, const ldouble& value_Mh) {
  Parameters values = parameters;
  values[0] = cdouble(value_Mu);
  values[1] = cdouble(value_Mh);
  set_parameters(values);
}

void draftcoloredcurrent2_color_2::set_momenta(const Momenta& values) {
  const auto validation = validate_momenta(values, external_masses, incoming_legs());
  if (!validation.valid) throw std::invalid_argument(validation.message);
  momenta_ready = false;
  all_point_cache_ready = false;
  summed_point_cache_ready = false;
  momentum_chain_1_ready = false;
  momentum_chain_2_ready = false;
  momentum_chain_3_ready = false;
  momentum_chain_4_ready = false;
  momenta = values;

  // External particle momenta.
  p1.set_momentum(momenta[0].data());
  p2.set_momentum(momenta[1].data());
  p3.set_momentum(momenta[2].data());
  p4.set_momentum(momenta[3].data());
  p5.set_momentum(momenta[4].data());

  // Propagator denominators and scalar invariants.
  s34 = mass2(sum_momenta(momenta, {3, 4})) - (ldouble(0.0L)) * (ldouble(0.0L));
  t15 = ((std::imag(Mu) == ldouble(0) && std::isfinite(std::real(Mu))) ? cdouble(mass2(sum_momenta(momenta, {1,
    -5})) - (std::real(Mu)) * (std::real(Mu)), ldouble(0)) : (mass2(sum_momenta(momenta, {1, -5})) - (Mu) *
    (Mu)));
  t25 = ((std::imag(Mu) == ldouble(0) && std::isfinite(std::real(Mu))) ? cdouble(mass2(sum_momenta(momenta, {2,
    -5})) - (std::real(Mu)) * (std::real(Mu)), ldouble(0)) : (mass2(sum_momenta(momenta, {2, -5})) - (Mu) *
    (Mu)));
  t35 = ((std::imag(Mu) == ldouble(0) && std::isfinite(std::real(Mu))) ? cdouble(mass2(sum_momenta(momenta, {3,
    5})) - (std::real(Mu)) * (std::real(Mu)), ldouble(0)) : (mass2(sum_momenta(momenta, {3, 5})) - (Mu) *
    (Mu)));
  s12 = mass2(sum_momenta(momenta, {1, 2})) - (ldouble(0.0L)) * (ldouble(0.0L));
  t45 = ((std::imag(Mu) == ldouble(0) && std::isfinite(std::real(Mu))) ? cdouble(mass2(sum_momenta(momenta, {4,
    5})) - (std::real(Mu)) * (std::real(Mu)), ldouble(0)) : (mass2(sum_momenta(momenta, {4, 5})) - (Mu) *
    (Mu)));
  t23 = ((std::imag(Mh) == ldouble(0) && std::isfinite(std::real(Mh))) ? cdouble(mass2(sum_momenta(momenta, {2,
    -3})) - (std::real(Mh)) * (std::real(Mh)), ldouble(0)) : (mass2(sum_momenta(momenta, {2, -3})) - (Mh) *
    (Mh)));
  t14 = ((std::imag(Mh) == ldouble(0) && std::isfinite(std::real(Mh))) ? cdouble(mass2(sum_momenta(momenta, {1,
    -4})) - (std::real(Mh)) * (std::real(Mh)), ldouble(0)) : (mass2(sum_momenta(momenta, {1, -4})) - (Mh) *
    (Mh)));
  s14 = mass2(sum_momenta(momenta, {1, -4})) - (ldouble(0.0L)) * (ldouble(0.0L));
  s23 = mass2(sum_momenta(momenta, {2, -3})) - (ldouble(0.0L)) * (ldouble(0.0L));
  t23_2 = mass2(sum_momenta(momenta, {2, -3})) - (parameter_factor_26) * (parameter_factor_26);
  t14_2 = mass2(sum_momenta(momenta, {1, -4})) - (parameter_factor_26) * (parameter_factor_26);
  p1_dot_p2 = p1.dot(p2);
  p1_dot_p3 = p1.dot(p3);
  p2_dot_p4 = p2.dot(p4);
  p3_dot_p4 = p3.dot(p4);
  momenta_ready = true;
}

// Prepare only data used by the retained helicity contributions.
void draftcoloredcurrent2_color_2::prepare_summed_point_cache() {
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
#endif
  a113s.update();
  a133s.update();
  a13a.update();
  a153s.update();
  a211s.update();
  a2133a.update();
  a213s.update();
  a2153a.update();
  a21a.update();
  #ifdef SPINAS_HAS_MOMENTUM_CHAIN
  a2213a.update(momentum_chain_3);
#else
  a2213a.update();
#endif
  a221s.update();
  a223s.update();
  a2243a.update();
  a224s.update();
  a233s.update();
  a23a.update();
  #ifdef SPINAS_HAS_MOMENTUM_CHAIN
  a2433a.update(momentum_chain_1);
#else
  a2433a.update();
#endif
  a243s.update();
  a244s.update();
  a2453a.update();
  a24a.update();
  a2513a.update();
  a251s.update();
  a253s.update();
  a2543a.update();
  a254s.update();
  a311s.update();
  a31a.update();
  a331s.update();
  a351s.update();
  a411s.update();
  a41a.update();
  #ifdef SPINAS_HAS_MOMENTUM_CHAIN
  a4211a.update(momentum_chain_3);
#else
  a4211a.update();
#endif
  a421s.update();
  a422s.update();
  a4251a.update();
  a4311a.update();
  a431s.update();
  a433s.update();
  a4351a.update();
  a43a.update();
  a441s.update();
  a4421a.update();
  a442s.update();
  #ifdef SPINAS_HAS_MOMENTUM_CHAIN
  a4431a.update(momentum_chain_1);
#else
  a4431a.update();
#endif
  a443s.update();
  a451s.update();
  a4521a.update();
  a452s.update();
  a4531a.update();
  a453s.update();
  s113a.update();
  s133a.update();
  s13s.update();
  s153a.update();
  s211a.update();
  s2133s.update();
  s213a.update();
  s2153s.update();
  s21s.update();
  #ifdef SPINAS_HAS_MOMENTUM_CHAIN
  s2213s.update(momentum_chain_4);
#else
  s2213s.update();
#endif
  s221a.update();
  s223a.update();
  s2243s.update();
  s233a.update();
  s23s.update();
  #ifdef SPINAS_HAS_MOMENTUM_CHAIN
  s2433s.update(momentum_chain_2);
#else
  s2433s.update();
#endif
  s243a.update();
  s2453s.update();
  s24s.update();
  s2513s.update();
  s251a.update();
  s253a.update();
  s2543s.update();
  s311a.update();
  s31s.update();
  s331a.update();
  s351a.update();
  s411a.update();
  s41s.update();
  #ifdef SPINAS_HAS_MOMENTUM_CHAIN
  s4211s.update(momentum_chain_4);
#else
  s4211s.update();
#endif
  s421a.update();
  s4251s.update();
  s4311s.update();
  s431a.update();
  s433a.update();
  s4351s.update();
  s43s.update();
  s441a.update();
  s4421s.update();
  #ifdef SPINAS_HAS_MOMENTUM_CHAIN
  s4431s.update(momentum_chain_2);
#else
  s4431s.update();
#endif
  s443a.update();
  s451a.update();
  s4521s.update();
  s4531s.update();
  s453a.update();
  real_denominator_domain = std::imag(parameters[0]) == ldouble(0) && std::imag(parameters[1]) == ldouble(0) &&
    std::imag(parameters[2]) == ldouble(0) && std::imag(parameters[3]) == ldouble(0) && std::imag(parameters[4])
    == ldouble(0) && std::imag(parameters[5]) == ldouble(0);
  point_product_1[0][0] = a24a.v(-1, -1);
  point_product_1[0][1] = a24a.v(-1, 1);
  point_product_1[1][0] = a24a.v(1, -1);
  point_product_1[1][1] = a24a.v(1, 1);
  point_product_2[0][0] = s13s.v(-1, -1);
  point_product_2[0][1] = s13s.v(-1, 1);
  point_product_2[1][0] = s13s.v(1, -1);
  point_product_2[1][1] = s13s.v(1, 1);
  point_product_3[0][0] = a41a.v(-1, -1);
  point_product_3[0][1] = a41a.v(-1, 1);
  point_product_3[1][0] = a41a.v(1, -1);
  point_product_3[1][1] = a41a.v(1, 1);
  point_product_4[0][0] = s23s.v(-1, -1);
  point_product_4[0][1] = s23s.v(-1, 1);
  point_product_4[1][0] = s23s.v(1, -1);
  point_product_4[1][1] = s23s.v(1, 1);
  point_product_5[0][0] = a13a.v(-1, -1);
  point_product_5[0][1] = a13a.v(-1, 1);
  point_product_5[1][0] = a13a.v(1, -1);
  point_product_5[1][1] = a13a.v(1, 1);
  point_product_6[0][0] = s24s.v(-1, -1);
  point_product_6[0][1] = s24s.v(-1, 1);
  point_product_6[1][0] = s24s.v(1, -1);
  point_product_6[1][1] = s24s.v(1, 1);
  point_product_7[0][0] = a23a.v(-1, -1);
  point_product_7[0][1] = a23a.v(-1, 1);
  point_product_7[1][0] = a23a.v(1, -1);
  point_product_7[1][1] = a23a.v(1, 1);
  point_product_8[0][0] = s41s.v(-1, -1);
  point_product_8[0][1] = s41s.v(-1, 1);
  point_product_8[1][0] = s41s.v(1, -1);
  point_product_8[1][1] = s41s.v(1, 1);
  point_product_9[0][0] = a113s.v(-1, -1);
  point_product_9[0][1] = a113s.v(-1, 1);
  point_product_9[1][0] = a113s.v(1, -1);
  point_product_9[1][1] = a113s.v(1, 1);
  point_product_10[0][0] = a153s.v(-1, -1);
  point_product_10[0][1] = a153s.v(-1, 1);
  point_product_10[1][0] = a153s.v(1, -1);
  point_product_10[1][1] = a153s.v(1, 1);
  point_product_11[0][0] = a133s.v(-1, -1);
  point_product_11[0][1] = a133s.v(-1, 1);
  point_product_11[1][0] = a133s.v(1, -1);
  point_product_11[1][1] = a133s.v(1, 1);
  point_product_12[0][0] = a223s.v(-1, -1);
  point_product_12[0][1] = a223s.v(-1, 1);
  point_product_12[1][0] = a223s.v(1, -1);
  point_product_12[1][1] = a223s.v(1, 1);
  point_product_13[0][0] = a253s.v(-1, -1);
  point_product_13[0][1] = a253s.v(-1, 1);
  point_product_13[1][0] = a253s.v(1, -1);
  point_product_13[1][1] = a253s.v(1, 1);
  point_product_14[0][0] = a233s.v(-1, -1);
  point_product_14[0][1] = a233s.v(-1, 1);
  point_product_14[1][0] = a233s.v(1, -1);
  point_product_14[1][1] = a233s.v(1, 1);
  point_product_15[0][0] = a224s.v(-1, -1);
  point_product_15[0][1] = a224s.v(-1, 1);
  point_product_15[1][0] = a224s.v(1, -1);
  point_product_15[1][1] = a224s.v(1, 1);
  point_product_16[0][0] = a254s.v(-1, -1);
  point_product_16[0][1] = a254s.v(-1, 1);
  point_product_16[1][0] = a254s.v(1, -1);
  point_product_16[1][1] = a254s.v(1, 1);
  point_product_17[0][0] = a244s.v(-1, -1);
  point_product_17[0][1] = a244s.v(-1, 1);
  point_product_17[1][0] = a244s.v(1, -1);
  point_product_17[1][1] = a244s.v(1, 1);
  point_product_18[0][0] = a411s.v(-1, -1);
  point_product_18[0][1] = a411s.v(-1, 1);
  point_product_18[1][0] = a411s.v(1, -1);
  point_product_18[1][1] = a411s.v(1, 1);
  point_product_19[0][0] = a451s.v(-1, -1);
  point_product_19[0][1] = a451s.v(-1, 1);
  point_product_19[1][0] = a451s.v(1, -1);
  point_product_19[1][1] = a451s.v(1, 1);
  point_product_20[0][0] = a441s.v(-1, -1);
  point_product_20[0][1] = a441s.v(-1, 1);
  point_product_20[1][0] = a441s.v(1, -1);
  point_product_20[1][1] = a441s.v(1, 1);
  point_product_21[0][0] = s31s.v(-1, -1);
  point_product_21[0][1] = s31s.v(-1, 1);
  point_product_21[1][0] = s31s.v(1, -1);
  point_product_21[1][1] = s31s.v(1, 1);
  point_product_22[0][0] = a422s.v(-1, -1);
  point_product_22[0][1] = a422s.v(-1, 1);
  point_product_22[1][0] = a422s.v(1, -1);
  point_product_22[1][1] = a422s.v(1, 1);
  point_product_23[0][0] = a452s.v(-1, -1);
  point_product_23[0][1] = a452s.v(-1, 1);
  point_product_23[1][0] = a452s.v(1, -1);
  point_product_23[1][1] = a452s.v(1, 1);
  point_product_24[0][0] = a31a.v(-1, -1);
  point_product_24[0][1] = a31a.v(-1, 1);
  point_product_24[1][0] = a31a.v(1, -1);
  point_product_24[1][1] = a31a.v(1, 1);
  point_product_25[0][0] = a442s.v(-1, -1);
  point_product_25[0][1] = a442s.v(-1, 1);
  point_product_25[1][0] = a442s.v(1, -1);
  point_product_25[1][1] = a442s.v(1, 1);
  point_product_26[0][0] = s113a.v(-1, -1);
  point_product_26[0][1] = s113a.v(-1, 1);
  point_product_26[1][0] = s113a.v(1, -1);
  point_product_26[1][1] = s113a.v(1, 1);
  point_product_27[0][0] = s153a.v(-1, -1);
  point_product_27[0][1] = s153a.v(-1, 1);
  point_product_27[1][0] = s153a.v(1, -1);
  point_product_27[1][1] = s153a.v(1, 1);
  point_product_28[0][0] = s133a.v(-1, -1);
  point_product_28[0][1] = s133a.v(-1, 1);
  point_product_28[1][0] = s133a.v(1, -1);
  point_product_28[1][1] = s133a.v(1, 1);
  point_product_29[0][0] = a43a.v(-1, -1);
  point_product_29[0][1] = a43a.v(-1, 1);
  point_product_29[1][0] = a43a.v(1, -1);
  point_product_29[1][1] = a43a.v(1, 1);
  point_product_30[0][0] = s21s.v(-1, -1);
  point_product_30[0][1] = s21s.v(-1, 1);
  point_product_30[1][0] = s21s.v(1, -1);
  point_product_30[1][1] = s21s.v(1, 1);
  point_product_31[0][0] = a21a.v(-1, -1);
  point_product_31[0][1] = a21a.v(-1, 1);
  point_product_31[1][0] = a21a.v(1, -1);
  point_product_31[1][1] = a21a.v(1, 1);
  point_product_32[0][0] = s43s.v(-1, -1);
  point_product_32[0][1] = s43s.v(-1, 1);
  point_product_32[1][0] = s43s.v(1, -1);
  point_product_32[1][1] = s43s.v(1, 1);
  point_product_33[0][0] = a221s.v(-1, -1);
  point_product_33[0][1] = a221s.v(-1, 1);
  point_product_33[1][0] = a221s.v(1, -1);
  point_product_33[1][1] = a221s.v(1, 1);
  point_product_34[0][0] = a251s.v(-1, -1);
  point_product_34[0][1] = a251s.v(-1, 1);
  point_product_34[1][0] = a251s.v(1, -1);
  point_product_34[1][1] = a251s.v(1, 1);
  point_product_35[0][0] = s221a.v(-1, -1);
  point_product_35[0][1] = s221a.v(-1, 1);
  point_product_35[1][0] = s221a.v(1, -1);
  point_product_35[1][1] = s221a.v(1, 1);
  point_product_36[0][0] = s251a.v(-1, -1);
  point_product_36[0][1] = s251a.v(-1, 1);
  point_product_36[1][0] = s251a.v(1, -1);
  point_product_36[1][1] = s251a.v(1, 1);
  point_product_37[0][0] = s223a.v(-1, -1);
  point_product_37[0][1] = s223a.v(-1, 1);
  point_product_37[1][0] = s223a.v(1, -1);
  point_product_37[1][1] = s223a.v(1, 1);
  point_product_38[0][0] = s253a.v(-1, -1);
  point_product_38[0][1] = s253a.v(-1, 1);
  point_product_38[1][0] = s253a.v(1, -1);
  point_product_38[1][1] = s253a.v(1, 1);
  point_product_39[0][0] = s233a.v(-1, -1);
  point_product_39[0][1] = s233a.v(-1, 1);
  point_product_39[1][0] = s233a.v(1, -1);
  point_product_39[1][1] = s233a.v(1, 1);
  point_product_40[0][0] = a211s.v(-1, -1);
  point_product_40[0][1] = a211s.v(-1, 1);
  point_product_40[1][0] = a211s.v(1, -1);
  point_product_40[1][1] = a211s.v(1, 1);
  point_product_41[0][0] = a311s.v(-1, -1);
  point_product_41[0][1] = a311s.v(-1, 1);
  point_product_41[1][0] = a311s.v(1, -1);
  point_product_41[1][1] = a311s.v(1, 1);
  point_product_42[0][0] = a351s.v(-1, -1);
  point_product_42[0][1] = a351s.v(-1, 1);
  point_product_42[1][0] = a351s.v(1, -1);
  point_product_42[1][1] = a351s.v(1, 1);
  point_product_43[0][0] = s211a.v(-1, -1);
  point_product_43[0][1] = s211a.v(-1, 1);
  point_product_43[1][0] = s211a.v(1, -1);
  point_product_43[1][1] = s211a.v(1, 1);
  point_product_44[0][0] = s311a.v(-1, -1);
  point_product_44[0][1] = s311a.v(-1, 1);
  point_product_44[1][0] = s311a.v(1, -1);
  point_product_44[1][1] = s311a.v(1, 1);
  point_product_45[0][0] = s351a.v(-1, -1);
  point_product_45[0][1] = s351a.v(-1, 1);
  point_product_45[1][0] = s351a.v(1, -1);
  point_product_45[1][1] = s351a.v(1, 1);
  point_product_46[0][0] = a421s.v(-1, -1);
  point_product_46[0][1] = a421s.v(-1, 1);
  point_product_46[1][0] = a421s.v(1, -1);
  point_product_46[1][1] = a421s.v(1, 1);
  point_product_47[0][0] = a431s.v(-1, -1);
  point_product_47[0][1] = a431s.v(-1, 1);
  point_product_47[1][0] = a431s.v(1, -1);
  point_product_47[1][1] = a431s.v(1, 1);
  point_product_48[0][0] = a213s.v(-1, -1);
  point_product_48[0][1] = a213s.v(-1, 1);
  point_product_48[1][0] = a213s.v(1, -1);
  point_product_48[1][1] = a213s.v(1, 1);
  point_product_49[0][0] = a243s.v(-1, -1);
  point_product_49[0][1] = a243s.v(-1, 1);
  point_product_49[1][0] = a243s.v(1, -1);
  point_product_49[1][1] = a243s.v(1, 1);
  point_product_50[0][0] = s213a.v(-1, -1);
  point_product_50[0][1] = s213a.v(-1, 1);
  point_product_50[1][0] = s213a.v(1, -1);
  point_product_50[1][1] = s213a.v(1, 1);
  point_product_51[0][0] = s243a.v(-1, -1);
  point_product_51[0][1] = s243a.v(-1, 1);
  point_product_51[1][0] = s243a.v(1, -1);
  point_product_51[1][1] = s243a.v(1, 1);
  point_product_52[0][0] = s421a.v(-1, -1);
  point_product_52[0][1] = s421a.v(-1, 1);
  point_product_52[1][0] = s421a.v(1, -1);
  point_product_52[1][1] = s421a.v(1, 1);
  point_product_53[0][0] = s431a.v(-1, -1);
  point_product_53[0][1] = s431a.v(-1, 1);
  point_product_53[1][0] = s431a.v(1, -1);
  point_product_53[1][1] = s431a.v(1, 1);
  point_product_54[0][0] = s411a.v(-1, -1);
  point_product_54[0][1] = s411a.v(-1, 1);
  point_product_54[1][0] = s411a.v(1, -1);
  point_product_54[1][1] = s411a.v(1, 1);
  point_product_55[0][0] = s441a.v(-1, -1);
  point_product_55[0][1] = s441a.v(-1, 1);
  point_product_55[1][0] = s441a.v(1, -1);
  point_product_55[1][1] = s441a.v(1, 1);
  point_product_56[0][0] = s451a.v(-1, -1);
  point_product_56[0][1] = s451a.v(-1, 1);
  point_product_56[1][0] = s451a.v(1, -1);
  point_product_56[1][1] = s451a.v(1, 1);
  point_product_57[0][0] = a443s.v(-1, -1);
  point_product_57[0][1] = a443s.v(-1, 1);
  point_product_57[1][0] = a443s.v(1, -1);
  point_product_57[1][1] = a443s.v(1, 1);
  point_product_58[0][0] = a453s.v(-1, -1);
  point_product_58[0][1] = a453s.v(-1, 1);
  point_product_58[1][0] = a453s.v(1, -1);
  point_product_58[1][1] = a453s.v(1, 1);
  point_product_59[0][0] = s443a.v(-1, -1);
  point_product_59[0][1] = s443a.v(-1, 1);
  point_product_59[1][0] = s443a.v(1, -1);
  point_product_59[1][1] = s443a.v(1, 1);
  point_product_60[0][0] = s453a.v(-1, -1);
  point_product_60[0][1] = s453a.v(-1, 1);
  point_product_60[1][0] = s453a.v(1, -1);
  point_product_60[1][1] = s453a.v(1, 1);
  point_product_61[0][0] = a331s.v(-1, -1);
  point_product_61[0][1] = a331s.v(-1, 1);
  point_product_61[1][0] = a331s.v(1, -1);
  point_product_61[1][1] = a331s.v(1, 1);
  point_product_62[0][0] = a433s.v(-1, -1);
  point_product_62[0][1] = a433s.v(-1, 1);
  point_product_62[1][0] = a433s.v(1, -1);
  point_product_62[1][1] = a433s.v(1, 1);
  point_product_63[0][0] = s331a.v(-1, -1);
  point_product_63[0][1] = s331a.v(-1, 1);
  point_product_63[1][0] = s331a.v(1, -1);
  point_product_63[1][1] = s331a.v(1, 1);
  point_product_64[0][0] = s433a.v(-1, -1);
  point_product_64[0][1] = s433a.v(-1, 1);
  point_product_64[1][0] = s433a.v(1, -1);
  point_product_64[1][1] = s433a.v(1, 1);
  point_product_65[0][0] = a2133a.v(-1, -1);
  point_product_65[0][1] = a2133a.v(-1, 1);
  point_product_65[1][0] = a2133a.v(1, -1);
  point_product_65[1][1] = a2133a.v(1, 1);
  point_product_66[0][0] = a2153a.v(-1, -1);
  point_product_66[0][1] = a2153a.v(-1, 1);
  point_product_66[1][0] = a2153a.v(1, -1);
  point_product_66[1][1] = a2153a.v(1, 1);
  point_product_67[0][0] = a2433a.v(-1, -1);
  point_product_67[0][1] = a2433a.v(-1, 1);
  point_product_67[1][0] = a2433a.v(1, -1);
  point_product_67[1][1] = a2433a.v(1, 1);
  point_product_68[0][0] = a2453a.v(-1, -1);
  point_product_68[0][1] = a2453a.v(-1, 1);
  point_product_68[1][0] = a2453a.v(1, -1);
  point_product_68[1][1] = a2453a.v(1, 1);
  point_product_69[0][0] = s2133s.v(-1, -1);
  point_product_69[0][1] = s2133s.v(-1, 1);
  point_product_69[1][0] = s2133s.v(1, -1);
  point_product_69[1][1] = s2133s.v(1, 1);
  point_product_70[0][0] = s2153s.v(-1, -1);
  point_product_70[0][1] = s2153s.v(-1, 1);
  point_product_70[1][0] = s2153s.v(1, -1);
  point_product_70[1][1] = s2153s.v(1, 1);
  point_product_71[0][0] = s2433s.v(-1, -1);
  point_product_71[0][1] = s2433s.v(-1, 1);
  point_product_71[1][0] = s2433s.v(1, -1);
  point_product_71[1][1] = s2433s.v(1, 1);
  point_product_72[0][0] = s2453s.v(-1, -1);
  point_product_72[0][1] = s2453s.v(-1, 1);
  point_product_72[1][0] = s2453s.v(1, -1);
  point_product_72[1][1] = s2453s.v(1, 1);
  point_product_73[0][0] = a2213a.v(-1, -1);
  point_product_73[0][1] = a2213a.v(-1, 1);
  point_product_73[1][0] = a2213a.v(1, -1);
  point_product_73[1][1] = a2213a.v(1, 1);
  point_product_74[0][0] = a2243a.v(-1, -1);
  point_product_74[0][1] = a2243a.v(-1, 1);
  point_product_74[1][0] = a2243a.v(1, -1);
  point_product_74[1][1] = a2243a.v(1, 1);
  point_product_75[0][0] = a2513a.v(-1, -1);
  point_product_75[0][1] = a2513a.v(-1, 1);
  point_product_75[1][0] = a2513a.v(1, -1);
  point_product_75[1][1] = a2513a.v(1, 1);
  point_product_76[0][0] = a2543a.v(-1, -1);
  point_product_76[0][1] = a2543a.v(-1, 1);
  point_product_76[1][0] = a2543a.v(1, -1);
  point_product_76[1][1] = a2543a.v(1, 1);
  point_product_77[0][0] = s2213s.v(-1, -1);
  point_product_77[0][1] = s2213s.v(-1, 1);
  point_product_77[1][0] = s2213s.v(1, -1);
  point_product_77[1][1] = s2213s.v(1, 1);
  point_product_78[0][0] = s2243s.v(-1, -1);
  point_product_78[0][1] = s2243s.v(-1, 1);
  point_product_78[1][0] = s2243s.v(1, -1);
  point_product_78[1][1] = s2243s.v(1, 1);
  point_product_79[0][0] = s2513s.v(-1, -1);
  point_product_79[0][1] = s2513s.v(-1, 1);
  point_product_79[1][0] = s2513s.v(1, -1);
  point_product_79[1][1] = s2513s.v(1, 1);
  point_product_80[0][0] = s2543s.v(-1, -1);
  point_product_80[0][1] = s2543s.v(-1, 1);
  point_product_80[1][0] = s2543s.v(1, -1);
  point_product_80[1][1] = s2543s.v(1, 1);
  point_product_81[0][0] = a4211a.v(-1, -1);
  point_product_81[0][1] = a4211a.v(-1, 1);
  point_product_81[1][0] = a4211a.v(1, -1);
  point_product_81[1][1] = a4211a.v(1, 1);
  point_product_82[0][0] = a4251a.v(-1, -1);
  point_product_82[0][1] = a4251a.v(-1, 1);
  point_product_82[1][0] = a4251a.v(1, -1);
  point_product_82[1][1] = a4251a.v(1, 1);
  point_product_83[0][0] = a4311a.v(-1, -1);
  point_product_83[0][1] = a4311a.v(-1, 1);
  point_product_83[1][0] = a4311a.v(1, -1);
  point_product_83[1][1] = a4311a.v(1, 1);
  point_product_84[0][0] = a4351a.v(-1, -1);
  point_product_84[0][1] = a4351a.v(-1, 1);
  point_product_84[1][0] = a4351a.v(1, -1);
  point_product_84[1][1] = a4351a.v(1, 1);
  point_product_85[0][0] = s4211s.v(-1, -1);
  point_product_85[0][1] = s4211s.v(-1, 1);
  point_product_85[1][0] = s4211s.v(1, -1);
  point_product_85[1][1] = s4211s.v(1, 1);
  point_product_86[0][0] = s4251s.v(-1, -1);
  point_product_86[0][1] = s4251s.v(-1, 1);
  point_product_86[1][0] = s4251s.v(1, -1);
  point_product_86[1][1] = s4251s.v(1, 1);
  point_product_87[0][0] = s4311s.v(-1, -1);
  point_product_87[0][1] = s4311s.v(-1, 1);
  point_product_87[1][0] = s4311s.v(1, -1);
  point_product_87[1][1] = s4311s.v(1, 1);
  point_product_88[0][0] = s4351s.v(-1, -1);
  point_product_88[0][1] = s4351s.v(-1, 1);
  point_product_88[1][0] = s4351s.v(1, -1);
  point_product_88[1][1] = s4351s.v(1, 1);
  point_product_89[0][0] = a4421a.v(-1, -1);
  point_product_89[0][1] = a4421a.v(-1, 1);
  point_product_89[1][0] = a4421a.v(1, -1);
  point_product_89[1][1] = a4421a.v(1, 1);
  point_product_90[0][0] = a4431a.v(-1, -1);
  point_product_90[0][1] = a4431a.v(-1, 1);
  point_product_90[1][0] = a4431a.v(1, -1);
  point_product_90[1][1] = a4431a.v(1, 1);
  point_product_91[0][0] = a4521a.v(-1, -1);
  point_product_91[0][1] = a4521a.v(-1, 1);
  point_product_91[1][0] = a4521a.v(1, -1);
  point_product_91[1][1] = a4521a.v(1, 1);
  point_product_92[0][0] = a4531a.v(-1, -1);
  point_product_92[0][1] = a4531a.v(-1, 1);
  point_product_92[1][0] = a4531a.v(1, -1);
  point_product_92[1][1] = a4531a.v(1, 1);
  point_product_93[0][0] = s4421s.v(-1, -1);
  point_product_93[0][1] = s4421s.v(-1, 1);
  point_product_93[1][0] = s4421s.v(1, -1);
  point_product_93[1][1] = s4421s.v(1, 1);
  point_product_94[0][0] = s4431s.v(-1, -1);
  point_product_94[0][1] = s4431s.v(-1, 1);
  point_product_94[1][0] = s4431s.v(1, -1);
  point_product_94[1][1] = s4431s.v(1, 1);
  point_product_95[0][0] = s4521s.v(-1, -1);
  point_product_95[0][1] = s4521s.v(-1, 1);
  point_product_95[1][0] = s4521s.v(1, -1);
  point_product_95[1][1] = s4521s.v(1, 1);
  point_product_96[0][0] = s4531s.v(-1, -1);
  point_product_96[0][1] = s4531s.v(-1, 1);
  point_product_96[1][0] = s4531s.v(1, -1);
  point_product_96[1][1] = s4531s.v(1, 1);
  point_factor_1 = (parameter_factor_4 * s34);
  point_factor_2 = (point_factor_1 * t15);
  point_factor_3 = (point_factor_1 * t25);
  point_factor_4 = ((parameter_factor_4 * t35) * s12);
  point_factor_5 = ((parameter_factor_4 * t45) * s12);
  point_factor_6 = (parameter_factor_8 * t23);
  point_factor_7 = (point_factor_6 * t14);
  point_factor_8 = (parameter_factor_13 * s14);
  point_factor_9 = (point_factor_8 * t25);
  point_factor_10 = (parameter_factor_4 * s14);
  point_factor_11 = (point_factor_10 * t25);
  point_factor_12 = (parameter_factor_8 * t14);
  point_factor_13 = (point_factor_12 * t25);
  point_factor_14 = (point_factor_12 * t35);
  point_factor_15 = (parameter_factor_13 * s23);
  point_factor_16 = (point_factor_15 * t15);
  point_factor_17 = (parameter_factor_4 * s23);
  point_factor_18 = (point_factor_17 * t15);
  point_factor_19 = ((parameter_factor_23 * t23_2) * t14_2);
  point_factor_20 = ((parameter_factor_30 * t23_2) * t14_2);
  point_factor_21 = ((parameter_factor_32 * t23_2) * t14_2);
  point_factor_22 = (parameter_factor_34 * t23_2);
  point_factor_23 = (point_factor_22 * t14_2);
  point_factor_24 = (parameter_factor_33 * p1_dot_p2);
  point_factor_25 = (parameter_factor_35 * t23_2);
  point_factor_26 = (point_factor_25 * t14_2);
  point_factor_27 = (parameter_factor_33 * p1_dot_p3);
  point_factor_28 = (parameter_factor_33 * p2_dot_p4);
  point_factor_29 = (parameter_factor_33 * p3_dot_p4);
  point_factor_30 = (parameter_factor_37 * t23_2);
  point_factor_31 = (point_factor_30 * t14_2);
  point_factor_32 = (parameter_factor_36 * p1_dot_p2);
  point_factor_33 = ((parameter_factor_38 * t23_2) * t14_2);
  point_factor_34 = (parameter_factor_36 * p1_dot_p3);
  point_factor_35 = (parameter_factor_36 * p2_dot_p4);
  point_factor_36 = (parameter_factor_36 * p3_dot_p4);
  point_factor_37 = (parameter_factor_40 * t23_2);
  point_factor_38 = (point_factor_37 * t14_2);
  point_factor_39 = (parameter_factor_39 * p1_dot_p2);
  point_factor_40 = ((parameter_factor_41 * t23_2) * t14_2);
  point_factor_41 = (parameter_factor_39 * p1_dot_p3);
  point_factor_42 = (parameter_factor_39 * p2_dot_p4);
  point_factor_43 = (parameter_factor_39 * p3_dot_p4);
  point_factor_44 = (point_factor_6 * t15);
  point_factor_45 = (point_factor_6 * t45);
  point_factor_46 = (point_factor_15 * t45);
  point_factor_47 = (point_factor_17 * t45);
  point_factor_48 = (point_factor_8 * t35);
  point_factor_49 = (point_factor_10 * t35);
  point_factor_50 = (parameter_factor_37 * t14_2);
  point_factor_51 = (point_factor_50 * t35);
  point_factor_52 = (parameter_factor_40 * t14_2);
  point_factor_53 = (point_factor_52 * t35);
  point_factor_54 = (parameter_factor_34 * t14_2);
  point_factor_55 = (point_factor_54 * t35);
  point_factor_56 = (parameter_factor_35 * t14_2);
  point_factor_57 = (point_factor_56 * t35);
  point_factor_58 = (parameter_factor_48 * t14_2);
  point_factor_59 = (point_factor_58 * t35);
  point_factor_60 = (parameter_factor_8 * t14_2);
  point_factor_61 = (point_factor_60 * t35);
  point_factor_62 = (point_factor_50 * t25);
  point_factor_63 = (point_factor_52 * t25);
  point_factor_64 = (point_factor_54 * t25);
  point_factor_65 = (point_factor_56 * t25);
  point_factor_66 = (point_factor_58 * t25);
  point_factor_67 = (point_factor_60 * t25);
  point_factor_68 = (point_factor_30 * t15);
  point_factor_69 = (point_factor_37 * t15);
  point_factor_70 = (point_factor_22 * t15);
  point_factor_71 = (point_factor_25 * t15);
  point_factor_72 = (parameter_factor_48 * t23_2);
  point_factor_73 = (point_factor_72 * t15);
  point_factor_74 = (parameter_factor_8 * t23_2);
  point_factor_75 = (point_factor_74 * t15);
  point_factor_76 = (point_factor_30 * t45);
  point_factor_77 = (point_factor_37 * t45);
  point_factor_78 = (point_factor_22 * t45);
  point_factor_79 = (point_factor_25 * t45);
  point_factor_80 = (point_factor_72 * t45);
  point_factor_81 = (point_factor_74 * t45);
  point_denominator_1 = hepcat_spinas::CachedDenominator(point_factor_2, real_denominator_domain);
  point_denominator_2 = hepcat_spinas::CachedDenominator(point_factor_3, real_denominator_domain);
  point_denominator_3 = hepcat_spinas::CachedDenominator(point_factor_4, real_denominator_domain);
  point_denominator_4 = hepcat_spinas::CachedDenominator(point_factor_5, real_denominator_domain);
  point_denominator_5 = hepcat_spinas::CachedDenominator(point_factor_7, real_denominator_domain);
  point_denominator_6 = hepcat_spinas::CachedDenominator(point_factor_9, real_denominator_domain);
  point_denominator_7 = hepcat_spinas::CachedDenominator(point_factor_11, real_denominator_domain);
  point_denominator_8 = hepcat_spinas::CachedDenominator(ldouble(3.0L), real_denominator_domain);
  point_denominator_9 = hepcat_spinas::CachedDenominator(point_factor_13, real_denominator_domain);
  point_denominator_10 = hepcat_spinas::CachedDenominator(point_factor_14, real_denominator_domain);
  point_denominator_11 = hepcat_spinas::CachedDenominator(point_factor_16, real_denominator_domain);
  point_denominator_12 = hepcat_spinas::CachedDenominator(point_factor_18, real_denominator_domain);
  point_denominator_13 = hepcat_spinas::CachedDenominator(point_factor_44, real_denominator_domain);
  point_denominator_14 = hepcat_spinas::CachedDenominator(point_factor_45, real_denominator_domain);
  point_denominator_15 = hepcat_spinas::CachedDenominator(point_factor_46, real_denominator_domain);
  point_denominator_16 = hepcat_spinas::CachedDenominator(point_factor_47, real_denominator_domain);
  point_denominator_17 = hepcat_spinas::CachedDenominator(point_factor_48, real_denominator_domain);
  point_denominator_18 = hepcat_spinas::CachedDenominator(point_factor_49, real_denominator_domain);
  summed_point_cache_ready = true;
}

// Individual amplitudes can lazily request the remaining helicities.
void draftcoloredcurrent2_color_2::prepare_all_point_cache() {
  if (all_point_cache_ready) return;
  prepare_summed_point_cache();
  all_point_cache_ready = true;
}

void draftcoloredcurrent2_color_2::set_momenta(const ldouble mom1[4], const ldouble mom2[4], const ldouble
  mom3[4], const ldouble mom4[4], const ldouble mom5[4]) {
  Momenta values;
  std::copy(mom1, mom1 + 4, values[0].begin());
  std::copy(mom2, mom2 + 4, values[1].begin());
  std::copy(mom3, mom3 + 4, values[2].begin());
  std::copy(mom4, mom4 + 4, values[3].begin());
  std::copy(mom5, mom5 + 4, values[4].begin());
  set_momenta(values);
}

cdouble draftcoloredcurrent2_color_2::amp(const Spins& spins) {
  return amp(spins[0], spins[1], spins[2], spins[3], spins[4]);
}

// set_momenta(...) must be called before evaluating an amplitude.
cdouble draftcoloredcurrent2_color_2::amp(const int& ds1, const int& ds2, const int& ds3, const int& ds4, const
  int& ds5) {
  if (!momenta_ready) throw std::logic_error("Call set_momenta before amp.");
  check_spin(1, ds1);
  check_spin(1, ds2);
  check_spin(1, ds3);
  check_spin(1, ds4);
  check_spin(0, ds5);
  prepare_all_point_cache();
  return amp_unchecked(ds1, ds2, ds3, ds4, ds5);
}

// Numerical kernel; public amp and spin_sum validate their inputs first.
cdouble draftcoloredcurrent2_color_2::amp_unchecked(const int& ds1, const int& ds2, const int& ds3, const int&
  ds4, const int& ds5) {
  const cdouble spin_factor_1 = (parameter_factor_1 * point_product_1[(ds2) > 0 ? 1 : 0][(ds4) > 0 ? 1 : 0]);
  const cdouble spin_factor_2 = (spin_factor_1 * point_product_2[(ds1) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_3 = ((parameter_factor_1 * point_product_3[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]) *
    point_product_4[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_4 = ((parameter_factor_1 * point_product_5[(ds1) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]) *
    point_product_6[(ds2) > 0 ? 1 : 0][(ds4) > 0 ? 1 : 0]);
  const cdouble spin_factor_5 = ((parameter_factor_1 * point_product_7[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]) *
    point_product_8[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_6 = (parameter_factor_5 * point_product_7[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_7 = (parameter_factor_11 * point_product_1[(ds2) > 0 ? 1 : 0][(ds4) > 0 ? 1 : 0]);
  const cdouble spin_factor_8 = (parameter_factor_11 * point_product_6[(ds2) > 0 ? 1 : 0][(ds4) > 0 ? 1 : 0]);
  const cdouble spin_factor_9 = (parameter_factor_11 * point_product_3[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_10 = (point_product_12[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0] -
    point_product_13[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_11 = (parameter_factor_11 * point_product_8[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_12 = ((-point_product_14[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]) -
    point_product_13[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_13 = (parameter_factor_11 * point_product_5[(ds1) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_14 = (point_product_15[(ds2) > 0 ? 1 : 0][(ds4) > 0 ? 1 : 0] -
    point_product_16[(ds2) > 0 ? 1 : 0][(ds4) > 0 ? 1 : 0]);
  const cdouble spin_factor_15 = (parameter_factor_11 * point_product_2[(ds1) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_16 = ((-point_product_17[(ds2) > 0 ? 1 : 0][(ds4) > 0 ? 1 : 0]) -
    point_product_16[(ds2) > 0 ? 1 : 0][(ds4) > 0 ? 1 : 0]);
  const cdouble spin_factor_17 = (parameter_factor_11 * point_product_4[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_18 = (point_product_18[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0] -
    point_product_19[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_19 = (parameter_factor_11 * point_product_7[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_20 = (point_product_20[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0] +
    point_product_19[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_21 = (parameter_factor_11 * point_product_21[(ds3) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_22 = (point_product_22[(ds4) > 0 ? 1 : 0][(ds2) > 0 ? 1 : 0] -
    point_product_23[(ds4) > 0 ? 1 : 0][(ds2) > 0 ? 1 : 0]);
  const cdouble spin_factor_23 = (parameter_factor_11 * point_product_24[(ds3) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_24 = (point_product_25[(ds4) > 0 ? 1 : 0][(ds2) > 0 ? 1 : 0] +
    point_product_23[(ds4) > 0 ? 1 : 0][(ds2) > 0 ? 1 : 0]);
  const cdouble spin_factor_25 = (-((parameter_factor_12 * point_product_29[(ds4) > 0 ? 1 : 0][(ds3) > 0 ? 1 :
    0]) * point_product_30[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]));
  const cdouble spin_factor_26 = ((parameter_factor_12 * point_product_24[(ds3) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0])
    * point_product_6[(ds2) > 0 ? 1 : 0][(ds4) > 0 ? 1 : 0]);
  const cdouble spin_factor_27 = ((parameter_factor_12 * point_product_1[(ds2) > 0 ? 1 : 0][(ds4) > 0 ? 1 : 0])
    * point_product_21[(ds3) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_28 = ((parameter_factor_12 * point_product_31[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0])
    * point_product_32[(ds4) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_29 = (parameter_factor_14 * point_product_29[(ds4) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_30 = (point_product_33[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0] -
    point_product_34[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_31 = (parameter_factor_14 * point_product_32[(ds4) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_32 = (point_product_35[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0] -
    point_product_36[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_33 = ((parameter_factor_1 * point_product_29[(ds4) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0])
    * point_product_30[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_34 = ((parameter_factor_1 * point_product_24[(ds3) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0])
    * point_product_6[(ds2) > 0 ? 1 : 0][(ds4) > 0 ? 1 : 0]);
  const cdouble spin_factor_35 = (spin_factor_1 * point_product_21[(ds3) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_36 = ((parameter_factor_1 * point_product_31[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0])
    * point_product_32[(ds4) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_37 = (parameter_factor_11 * point_product_29[(ds4) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_38 = (parameter_factor_11 * point_product_32[(ds4) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_39 = (parameter_factor_15 * point_product_7[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_40 = (spin_factor_39 * point_product_3[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_41 = ((parameter_factor_15 * point_product_3[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0])
    * point_product_4[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_42 = (spin_factor_39 * point_product_8[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_43 = ((parameter_factor_15 * point_product_4[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0])
    * point_product_8[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_44 = (parameter_factor_17 * point_product_3[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_45 = (parameter_factor_17 * point_product_8[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_46 = (point_product_37[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0] -
    point_product_38[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_47 = (point_product_39[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0] +
    point_product_38[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_48 = (point_product_40[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0] -
    point_product_34[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_49 = (parameter_factor_14 * point_product_6[(ds2) > 0 ? 1 : 0][(ds4) > 0 ? 1 : 0]);
  const cdouble spin_factor_50 = (point_product_41[(ds3) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0] -
    point_product_42[(ds3) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_51 = (point_product_43[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0] -
    point_product_36[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_52 = (parameter_factor_14 * point_product_1[(ds2) > 0 ? 1 : 0][(ds4) > 0 ? 1 : 0]);
  const cdouble spin_factor_53 = (point_product_44[(ds3) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0] -
    point_product_45[(ds3) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_54 = (point_product_12[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0] -
    point_product_14[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_55 = ((-point_product_46[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]) +
    point_product_47[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_56 = ((-point_product_18[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]) +
    point_product_20[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_57 = (point_product_48[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0] -
    point_product_49[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_58 = ((-point_product_37[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]) +
    point_product_39[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_59 = (point_product_50[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0] -
    point_product_51[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_60 = (point_product_52[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0] -
    point_product_53[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_61 = (point_product_54[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0] -
    point_product_55[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_62 = (parameter_factor_17 * point_product_7[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_63 = (parameter_factor_17 * point_product_4[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_64 = (point_product_54[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0] -
    point_product_56[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_65 = ((-point_product_55[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]) -
    point_product_56[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_66 = (parameter_factor_14 * point_product_31[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_67 = (point_product_57[(ds4) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0] +
    point_product_58[(ds4) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_68 = (parameter_factor_14 * point_product_30[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_69 = ((-point_product_59[(ds4) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]) -
    point_product_60[(ds4) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_70 = (parameter_factor_11 * point_product_31[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_71 = (parameter_factor_11 * point_product_30[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_72 = (point_product_61[(ds3) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0] +
    point_product_42[(ds3) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_73 = (point_product_62[(ds4) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0] +
    point_product_58[(ds4) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_74 = ((-point_product_63[(ds3) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]) -
    point_product_45[(ds3) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_75 = (point_product_64[(ds4) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0] +
    point_product_60[(ds4) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_76 = ((parameter_factor_42 * point_product_29[(ds4) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0])
    * point_product_30[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_77 = ((parameter_factor_43 * point_product_24[(ds3) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0])
    * point_product_6[(ds2) > 0 ? 1 : 0][(ds4) > 0 ? 1 : 0]);
  const cdouble spin_factor_78 = ((parameter_factor_44 * point_product_1[(ds2) > 0 ? 1 : 0][(ds4) > 0 ? 1 : 0])
    * point_product_21[(ds3) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_79 = ((parameter_factor_42 * point_product_31[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0])
    * point_product_32[(ds4) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_80 = (parameter_factor_45 * point_product_1[(ds2) > 0 ? 1 : 0][(ds4) > 0 ? 1 : 0]);
  const cdouble spin_factor_81 = ((parameter_factor_46 * spin_factor_57) * spin_factor_56);
  const cdouble spin_factor_82 = (parameter_factor_47 * point_product_30[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_83 = ((parameter_factor_42 * spin_factor_56) * spin_factor_59);
  const cdouble spin_factor_84 = (parameter_factor_49 * point_product_6[(ds2) > 0 ? 1 : 0][(ds4) > 0 ? 1 : 0]);
  const cdouble spin_factor_85 = ((parameter_factor_42 * spin_factor_57) * spin_factor_61);
  const cdouble spin_factor_86 = ((parameter_factor_43 * spin_factor_59) * spin_factor_61);
  const cdouble spin_factor_87 = (parameter_factor_47 * point_product_31[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_88 = (parameter_factor_50 * spin_factor_56);
  const cdouble spin_factor_89 = (((point_product_65[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0] +
    point_product_66[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]) - point_product_67[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1
    : 0]) - point_product_68[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_90 = (parameter_factor_47 * spin_factor_61);
  const cdouble spin_factor_91 = (parameter_factor_47 * spin_factor_56);
  const cdouble spin_factor_92 = ((((-point_product_69[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]) -
    point_product_70[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]) + point_product_71[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1
    : 0]) + point_product_72[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_93 = (parameter_factor_49 * spin_factor_61);
  const cdouble spin_factor_94 = (parameter_factor_47 * point_product_29[(ds4) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_95 = (parameter_factor_47 * point_product_32[(ds4) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_96 = (((point_product_73[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0] -
    point_product_74[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]) - point_product_75[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1
    : 0]) + point_product_76[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_97 = (((point_product_77[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0] -
    point_product_78[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]) - point_product_79[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1
    : 0]) + point_product_80[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_98 = ((parameter_factor_46 * spin_factor_54) * spin_factor_55);
  const cdouble spin_factor_99 = ((parameter_factor_42 * spin_factor_55) * spin_factor_58);
  const cdouble spin_factor_100 = ((parameter_factor_42 * spin_factor_54) * spin_factor_60);
  const cdouble spin_factor_101 = ((parameter_factor_43 * spin_factor_58) * spin_factor_60);
  const cdouble spin_factor_102 = (parameter_factor_50 * spin_factor_54);
  const cdouble spin_factor_103 = ((((-point_product_81[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]) +
    point_product_82[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]) + point_product_83[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1
    : 0]) - point_product_84[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_104 = (parameter_factor_47 * spin_factor_58);
  const cdouble spin_factor_105 = (parameter_factor_47 * spin_factor_54);
  const cdouble spin_factor_106 = (((point_product_85[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0] -
    point_product_86[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]) - point_product_87[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1
    : 0]) + point_product_88[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_107 = (parameter_factor_49 * spin_factor_58);
  const cdouble spin_factor_108 = (((point_product_89[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0] -
    point_product_90[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]) + point_product_91[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1
    : 0]) - point_product_92[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_109 = ((((-point_product_93[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]) +
    point_product_94[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]) - point_product_95[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1
    : 0]) + point_product_96[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  return (point_denominator_1.divide(spin_factor_2) + point_denominator_2.divide(spin_factor_2) +
    point_denominator_3.divide(spin_factor_2) + point_denominator_4.divide(spin_factor_2) -
    point_denominator_1.divide(spin_factor_3) - point_denominator_2.divide(spin_factor_3) -
    point_denominator_3.divide(spin_factor_3) - point_denominator_4.divide(spin_factor_3) +
    point_denominator_1.divide(spin_factor_4) + point_denominator_2.divide(spin_factor_4) +
    point_denominator_3.divide(spin_factor_4) + point_denominator_4.divide(spin_factor_4) -
    point_denominator_1.divide(spin_factor_5) - point_denominator_2.divide(spin_factor_5) -
    point_denominator_3.divide(spin_factor_5) - point_denominator_4.divide(spin_factor_5) + (ldouble(2.0L) *
    (((point_denominator_5.divide((spin_factor_6 * point_product_3[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0])) -
    point_denominator_5.divide(((parameter_factor_5 * point_product_3[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]) *
    point_product_4[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]))) - point_denominator_5.divide((spin_factor_6 *
    point_product_8[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]))) + point_denominator_5.divide(((parameter_factor_5 *
    point_product_4[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]) * point_product_8[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 :
    0])))) + point_denominator_1.divide((spin_factor_7 * (point_product_9[(ds1) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]
    - point_product_10[(ds1) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]))) + point_denominator_3.divide((spin_factor_8 *
    ((-point_product_11[(ds1) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]) - point_product_10[(ds1) > 0 ? 1 : 0][(ds3) > 0 ?
    1 : 0]))) - point_denominator_2.divide((spin_factor_9 * spin_factor_10)) -
    point_denominator_3.divide((spin_factor_11 * spin_factor_12)) + point_denominator_2.divide((spin_factor_13 *
    spin_factor_14)) + point_denominator_4.divide((spin_factor_15 * spin_factor_16)) +
    point_denominator_1.divide((spin_factor_17 * spin_factor_18)) - point_denominator_4.divide((spin_factor_19 *
    spin_factor_20)) - point_denominator_2.divide((spin_factor_21 * spin_factor_22)) +
    point_denominator_4.divide((spin_factor_23 * spin_factor_24)) + point_denominator_1.divide((spin_factor_8 *
    (point_product_26[(ds1) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0] - point_product_27[(ds1) > 0 ? 1 : 0][(ds3) > 0 ? 1
    : 0]))) - point_denominator_3.divide((spin_factor_7 * (point_product_28[(ds1) > 0 ? 1 : 0][(ds3) > 0 ? 1 :
    0] + point_product_27[(ds1) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]))) + (ldouble(2.0L) *
    (((((((point_denominator_6.divide(spin_factor_25) - point_denominator_6.divide(spin_factor_26)) -
    point_denominator_6.divide(spin_factor_27)) - point_denominator_6.divide(spin_factor_28)) -
    point_denominator_6.divide((spin_factor_29 * spin_factor_30))) -
    point_denominator_6.divide(((parameter_factor_14 * point_product_24[(ds3) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]) *
    spin_factor_14))) + point_denominator_6.divide(((parameter_factor_14 * point_product_2[(ds1) > 0 ? 1 :
    0][(ds3) > 0 ? 1 : 0]) * spin_factor_22))) - point_denominator_6.divide((spin_factor_31 * spin_factor_32))))
    + point_denominator_8.divide((((((((point_denominator_7.divide(spin_factor_33) +
    point_denominator_7.divide(spin_factor_34)) + point_denominator_7.divide(spin_factor_35)) +
    point_denominator_7.divide(spin_factor_36)) + point_denominator_7.divide((spin_factor_37 * spin_factor_30)))
    + point_denominator_7.divide((spin_factor_23 * spin_factor_14))) -
    point_denominator_7.divide((spin_factor_15 * spin_factor_22))) + point_denominator_7.divide((spin_factor_38
    * spin_factor_32)))) + (ldouble(2.0L) * (((((((point_denominator_9.divide(spin_factor_40) -
    point_denominator_9.divide(spin_factor_41)) - point_denominator_9.divide(spin_factor_42)) +
    point_denominator_9.divide(spin_factor_43)) - point_denominator_9.divide((spin_factor_44 * spin_factor_10)))
    + point_denominator_9.divide((spin_factor_45 * spin_factor_10))) +
    point_denominator_9.divide((spin_factor_44 * spin_factor_46))) - point_denominator_9.divide((spin_factor_45
    * spin_factor_46)))) - point_denominator_2.divide((spin_factor_11 * spin_factor_46)) +
    point_denominator_3.divide((spin_factor_9 * spin_factor_47)) + (ldouble(2.0L) *
    (((((((point_denominator_10.divide(spin_factor_40) - point_denominator_10.divide(spin_factor_41)) -
    point_denominator_10.divide(spin_factor_42)) + point_denominator_10.divide(spin_factor_43)) +
    point_denominator_10.divide((spin_factor_44 * spin_factor_12))) -
    point_denominator_10.divide((spin_factor_45 * spin_factor_12))) +
    point_denominator_10.divide((spin_factor_44 * spin_factor_47))) -
    point_denominator_10.divide((spin_factor_45 * spin_factor_47)))) + (ldouble(2.0L) *
    (((((((point_denominator_11.divide(spin_factor_25) - point_denominator_11.divide(spin_factor_26)) -
    point_denominator_11.divide(spin_factor_27)) - point_denominator_11.divide(spin_factor_28)) +
    point_denominator_11.divide((spin_factor_31 * spin_factor_48))) +
    point_denominator_11.divide((spin_factor_49 * spin_factor_50))) +
    point_denominator_11.divide((spin_factor_29 * spin_factor_51))) +
    point_denominator_11.divide((spin_factor_52 * spin_factor_53)))) +
    point_denominator_8.divide((((((((point_denominator_12.divide(spin_factor_33) +
    point_denominator_12.divide(spin_factor_34)) + point_denominator_12.divide(spin_factor_35)) +
    point_denominator_12.divide(spin_factor_36)) - point_denominator_12.divide((spin_factor_38 *
    spin_factor_48))) - point_denominator_12.divide((spin_factor_8 * spin_factor_50))) -
    point_denominator_12.divide((spin_factor_37 * spin_factor_51))) - point_denominator_12.divide((spin_factor_7
    * spin_factor_53)))) + (ldouble(2.0L) * ((((((((((((((((((((((((((((((parameter_factor_19 *
    point_product_29[(ds4) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]) * point_product_30[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1
    : 0]) / point_factor_19) - (((parameter_factor_28 * point_product_24[(ds3) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0])
    * point_product_6[(ds2) > 0 ? 1 : 0][(ds4) > 0 ? 1 : 0]) / point_factor_20)) - (((parameter_factor_31 *
    point_product_1[(ds2) > 0 ? 1 : 0][(ds4) > 0 ? 1 : 0]) * point_product_21[(ds3) > 0 ? 1 : 0][(ds1) > 0 ? 1 :
    0]) / point_factor_21)) + (((parameter_factor_19 * point_product_31[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]) *
    point_product_32[(ds4) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]) / point_factor_19)) + (((parameter_factor_33 *
    spin_factor_54) * spin_factor_55) / point_factor_23)) - (((point_factor_24 * spin_factor_54) *
    spin_factor_56) / point_factor_26)) + (((point_factor_27 * spin_factor_54) * spin_factor_56) /
    point_factor_26)) + (((point_factor_28 * spin_factor_54) * spin_factor_56) / point_factor_26)) -
    (((point_factor_29 * spin_factor_54) * spin_factor_56) / point_factor_26)) + (((parameter_factor_33 *
    spin_factor_57) * spin_factor_56) / point_factor_23)) - (((parameter_factor_36 * spin_factor_55) *
    spin_factor_58) / point_factor_31)) + (((point_factor_32 * spin_factor_56) * spin_factor_58) /
    point_factor_33)) - (((point_factor_34 * spin_factor_56) * spin_factor_58) / point_factor_33)) -
    (((point_factor_35 * spin_factor_56) * spin_factor_58) / point_factor_33)) + (((point_factor_36 *
    spin_factor_56) * spin_factor_58) / point_factor_33)) + (((parameter_factor_36 * spin_factor_56) *
    spin_factor_59) / point_factor_31)) - (((parameter_factor_36 * spin_factor_54) * spin_factor_60) /
    point_factor_31)) + (((parameter_factor_39 * spin_factor_58) * spin_factor_60) / point_factor_38)) +
    (((point_factor_32 * spin_factor_54) * spin_factor_61) / point_factor_33)) - (((point_factor_34 *
    spin_factor_54) * spin_factor_61) / point_factor_33)) - (((point_factor_35 * spin_factor_54) *
    spin_factor_61) / point_factor_33)) + (((point_factor_36 * spin_factor_54) * spin_factor_61) /
    point_factor_33)) - (((parameter_factor_36 * spin_factor_57) * spin_factor_61) / point_factor_31)) -
    (((point_factor_39 * spin_factor_58) * spin_factor_61) / point_factor_40)) + (((point_factor_41 *
    spin_factor_58) * spin_factor_61) / point_factor_40)) + (((point_factor_42 * spin_factor_58) *
    spin_factor_61) / point_factor_40)) - (((point_factor_43 * spin_factor_58) * spin_factor_61) /
    point_factor_40)) - (((parameter_factor_39 * spin_factor_59) * spin_factor_61) / point_factor_38))) +
    (ldouble(2.0L) * (((((((point_denominator_13.divide(spin_factor_40) -
    point_denominator_13.divide(spin_factor_41)) - point_denominator_13.divide(spin_factor_42)) +
    point_denominator_13.divide(spin_factor_43)) - point_denominator_13.divide((spin_factor_62 *
    spin_factor_18))) + point_denominator_13.divide((spin_factor_63 * spin_factor_18))) +
    point_denominator_13.divide((spin_factor_62 * spin_factor_64))) -
    point_denominator_13.divide((spin_factor_63 * spin_factor_64)))) + (ldouble(2.0L) *
    (((((((point_denominator_14.divide(spin_factor_40) - point_denominator_14.divide(spin_factor_41)) -
    point_denominator_14.divide(spin_factor_42)) + point_denominator_14.divide(spin_factor_43)) -
    point_denominator_14.divide((spin_factor_62 * spin_factor_20))) +
    point_denominator_14.divide((spin_factor_63 * spin_factor_20))) -
    point_denominator_14.divide((spin_factor_62 * spin_factor_65))) +
    point_denominator_14.divide((spin_factor_63 * spin_factor_65)))) +
    point_denominator_1.divide((spin_factor_19 * spin_factor_64)) + point_denominator_4.divide((spin_factor_17 *
    spin_factor_65)) + (ldouble(2.0L) * (((((((point_denominator_15.divide(spin_factor_25) -
    point_denominator_15.divide(spin_factor_26)) - point_denominator_15.divide(spin_factor_27)) -
    point_denominator_15.divide(spin_factor_28)) - point_denominator_15.divide(((parameter_factor_14 *
    point_product_21[(ds3) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]) * spin_factor_16))) -
    point_denominator_15.divide(((parameter_factor_14 * point_product_5[(ds1) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]) *
    spin_factor_24))) - point_denominator_15.divide((spin_factor_66 * spin_factor_67))) +
    point_denominator_15.divide((spin_factor_68 * spin_factor_69)))) +
    point_denominator_8.divide((((((((point_denominator_16.divide(spin_factor_33) +
    point_denominator_16.divide(spin_factor_34)) + point_denominator_16.divide(spin_factor_35)) +
    point_denominator_16.divide(spin_factor_36)) + point_denominator_16.divide((spin_factor_21 *
    spin_factor_16))) + point_denominator_16.divide((spin_factor_13 * spin_factor_24))) +
    point_denominator_16.divide((spin_factor_70 * spin_factor_67))) -
    point_denominator_16.divide((spin_factor_71 * spin_factor_69)))) + (ldouble(2.0L) *
    (((((((point_denominator_17.divide(spin_factor_25) - point_denominator_17.divide(spin_factor_26)) -
    point_denominator_17.divide(spin_factor_27)) - point_denominator_17.divide(spin_factor_28)) -
    point_denominator_17.divide((spin_factor_52 * spin_factor_72))) +
    point_denominator_17.divide((spin_factor_68 * spin_factor_73))) +
    point_denominator_17.divide((spin_factor_49 * spin_factor_74))) +
    point_denominator_17.divide((spin_factor_66 * spin_factor_75)))) +
    point_denominator_8.divide((((((((point_denominator_18.divide(spin_factor_33) +
    point_denominator_18.divide(spin_factor_34)) + point_denominator_18.divide(spin_factor_35)) +
    point_denominator_18.divide(spin_factor_36)) + point_denominator_18.divide((spin_factor_7 *
    spin_factor_72))) - point_denominator_18.divide((spin_factor_71 * spin_factor_73))) -
    point_denominator_18.divide((spin_factor_8 * spin_factor_74))) - point_denominator_18.divide((spin_factor_70
    * spin_factor_75)))) + (ldouble(2.0L) * ((((((((((((((((spin_factor_76 / point_factor_51) - (spin_factor_77
    / point_factor_53)) - (spin_factor_78 / point_factor_55)) + (spin_factor_79 / point_factor_51)) -
    ((spin_factor_80 * spin_factor_72) / point_factor_55)) + (spin_factor_81 / point_factor_57)) -
    ((spin_factor_82 * spin_factor_73) / point_factor_51)) + (spin_factor_83 / point_factor_59)) +
    ((spin_factor_84 * spin_factor_74) / point_factor_53)) - (spin_factor_85 / point_factor_59)) -
    (spin_factor_86 / point_factor_61)) - ((spin_factor_87 * spin_factor_75) / point_factor_51)) -
    ((spin_factor_88 * spin_factor_89) / point_factor_57)) + ((spin_factor_90 * spin_factor_89) /
    point_factor_59)) + ((spin_factor_91 * spin_factor_92) / point_factor_59)) - ((spin_factor_93 *
    spin_factor_92) / point_factor_61))) + (ldouble(2.0L) * ((((((((((((((((spin_factor_76 / point_factor_62) -
    (spin_factor_77 / point_factor_63)) - (spin_factor_78 / point_factor_64)) + (spin_factor_79 /
    point_factor_62)) + ((spin_factor_94 * spin_factor_30) / point_factor_62)) - (((parameter_factor_49 *
    point_product_24[(ds3) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]) * spin_factor_14) / point_factor_63)) +
    (spin_factor_81 / point_factor_65)) + (((parameter_factor_45 * point_product_2[(ds1) > 0 ? 1 : 0][(ds3) > 0
    ? 1 : 0]) * spin_factor_22) / point_factor_64)) + (spin_factor_83 / point_factor_66)) + ((spin_factor_95 *
    spin_factor_32) / point_factor_62)) - (spin_factor_85 / point_factor_66)) - (spin_factor_86 /
    point_factor_67)) + ((spin_factor_91 * spin_factor_96) / point_factor_66)) - ((spin_factor_93 *
    spin_factor_96) / point_factor_67)) + ((spin_factor_88 * spin_factor_97) / point_factor_65)) -
    ((spin_factor_90 * spin_factor_97) / point_factor_66))) + (ldouble(2.0L) * ((((((((((((((((spin_factor_76 /
    point_factor_68) - (spin_factor_77 / point_factor_69)) - (spin_factor_78 / point_factor_70)) +
    (spin_factor_79 / point_factor_68)) - ((spin_factor_95 * spin_factor_48) / point_factor_68)) +
    ((spin_factor_84 * spin_factor_50) / point_factor_69)) + (spin_factor_98 / point_factor_71)) -
    (spin_factor_99 / point_factor_73)) - ((spin_factor_94 * spin_factor_51) / point_factor_68)) +
    ((spin_factor_80 * spin_factor_53) / point_factor_70)) - (spin_factor_100 / point_factor_73)) +
    (spin_factor_101 / point_factor_75)) - ((spin_factor_102 * spin_factor_103) / point_factor_71)) +
    ((spin_factor_104 * spin_factor_103) / point_factor_73)) + ((spin_factor_105 * spin_factor_106) /
    point_factor_73)) - ((spin_factor_107 * spin_factor_106) / point_factor_75))) + (ldouble(2.0L) *
    ((((((((((((((((spin_factor_76 / point_factor_76) - (spin_factor_77 / point_factor_77)) - (spin_factor_78 /
    point_factor_78)) + (spin_factor_79 / point_factor_76)) - (((parameter_factor_45 * point_product_21[(ds3) >
    0 ? 1 : 0][(ds1) > 0 ? 1 : 0]) * spin_factor_16) / point_factor_78)) + (spin_factor_98 / point_factor_79)) -
    (((parameter_factor_49 * point_product_5[(ds1) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]) * spin_factor_24) /
    point_factor_77)) + ((spin_factor_87 * spin_factor_67) / point_factor_76)) - (spin_factor_99 /
    point_factor_80)) - (spin_factor_100 / point_factor_80)) + (spin_factor_101 / point_factor_81)) -
    ((spin_factor_82 * spin_factor_69) / point_factor_76)) - ((spin_factor_105 * spin_factor_108) /
    point_factor_80)) + ((spin_factor_107 * spin_factor_108) / point_factor_81)) + ((spin_factor_102 *
    spin_factor_109) / point_factor_79)) - ((spin_factor_104 * spin_factor_109) / point_factor_80))));
}

// Explicit spin and helicity sums.
ldouble draftcoloredcurrent2_color_2::spin_sum() {
  ldouble result = 0;
  cdouble amplitude;
  if (!momenta_ready) throw std::logic_error("Call set_momenta before spin_sum.");
  prepare_all_point_cache();
  const int j5 = 0;
  for (int j1 = -1; j1 <= 1; j1 += 2) {
    for (int j2 = -1; j2 <= 1; j2 += 2) {
      for (int j3 = -1; j3 <= 1; j3 += 2) {
        for (int j4 = -1; j4 <= 1; j4 += 2) {
          amplitude = amp_unchecked(j1, j2, j3, j4, j5);
          result += std::norm(amplitude);
        }
      }
    }
  }
  return result;
}

ldouble draftcoloredcurrent2_color_2::amp2() {
  // Initial-state average: 1/4.
  // Final-state identical-particle factor: 1.
  return spin_sum() / ldouble(4.0L);
}

} // namespace hepcat_generated

namespace hepcat_generated {
using namespace hepcat_spinas;
constexpr std::size_t draftcoloredcurrent2::leg_count;
constexpr std::size_t draftcoloredcurrent2::parameter_count;
constexpr std::size_t draftcoloredcurrent2::helicity_count;
constexpr std::size_t draftcoloredcurrent2::color_basis_count;
draftcoloredcurrent2::Parameters draftcoloredcurrent2::default_parameters() { return Parameters{{
  cdouble(ldouble(2.16e-3L), ldouble(0.0L)), // Mu
  cdouble(ldouble(1.25e2L), ldouble(0.0L)), // Mh
  cdouble(ldouble(3.1333e-1L), ldouble(0.0L)), // EE
  cdouble(ldouble(8.0385e1L), ldouble(0.0L)), // MW
  cdouble(ldouble(4.74e-1L), ldouble(0.0L)), // SW
  cdouble(ldouble(1.117e0L), ldouble(0.0L)) // GG
}}; }
void draftcoloredcurrent2::set_parameters(const Parameters& values) {
  component_1.set_parameters(draftcoloredcurrent2_color_1::Parameters{{values[0], values[1], values[2], values[3], values[4], values[5]}});
  component_2.set_parameters(draftcoloredcurrent2_color_2::Parameters{{values[0], values[1], values[2], values[5], values[3], values[4]}});
  parameters = values;
}
void draftcoloredcurrent2::set_momenta(const Momenta& momenta) {
  component_1.set_momenta(momenta);
  component_2.set_momenta(momenta);
}
void draftcoloredcurrent2::set_widths_enabled(bool enabled) {
  component_1.set_widths_enabled(enabled);
  component_2.set_widths_enabled(enabled);
}
draftcoloredcurrent2::ColorAmplitudes draftcoloredcurrent2::color_amplitudes(const Spins& spins) {
  return ColorAmplitudes{{component_1.amp(spins), component_2.amp(spins)}};
}
ldouble draftcoloredcurrent2::color_spin_sum(const Spins& spins) {
  const auto a = color_amplitudes(spins);
  cdouble value(0,0);
  ldouble scale = 0;
  { const cdouble term = std::conj(a[0]) * cdouble(ldouble(9.0e0L), ldouble(0.0L)) * a[0];
    value += term; scale += std::abs(term); }
  { const cdouble term = std::conj(a[1]) * cdouble(ldouble(2.0e0L), ldouble(0.0L)) * a[1];
    value += term; scale += std::abs(term); }
  const ldouble tolerance = 256 * std::numeric_limits<ldouble>::epsilon() * scale;
  if (std::abs(value.imag()) > tolerance || value.real() < -tolerance)
    throw std::runtime_error("The coherent color sum is not real and nonnegative.");
  return std::max(ldouble(0), value.real());
}
ldouble draftcoloredcurrent2::spin_sum() {
  ldouble total = 0;
  static const std::array<int,leg_count> ranks = {{1, 1, 1, 1, 0}};
  for (auto spins : helicity_configurations()) {
    std::function<ldouble(std::size_t)> sum = [&](std::size_t leg) -> ldouble {
      if (leg == leg_count) return color_spin_sum(spins);
      if (masses()[leg] == 0 || ranks[leg] == 0) return sum(leg+1);
      ldouble value = 0;
      for (int ds = -ranks[leg]; ds <= ranks[leg]; ds += 2) { spins[leg] = ds; value += sum(leg+1); }
      return value;
    };
    total += sum(0);
  }
  return total;
}
ldouble draftcoloredcurrent2::amp2() { return spin_sum() * symmetry_factor() / (incoming_spin_average() * incoming_color_average()); }
cdouble draftcoloredcurrent2::amp(const Spins& spins, const Colors& colors) {
  const auto a = color_amplitudes(spins); cdouble result(0,0);
  for (std::size_t i=0;i<color_basis_count;++i) result += a[i] * color_tensor(i,colors);
  return result;
}
cdouble draftcoloredcurrent2::color_tensor(std::size_t basis, const Colors& colors) {
  static const std::array<int,leg_count> dimensions = {{3, 3, 3, 3, 1}};
  std::size_t index = 0;
  for (std::size_t leg=0;leg<leg_count;++leg) {
    if (colors[leg]<1 || colors[leg]>dimensions[leg]) throw std::invalid_argument("External color index is out of range.");
    index = index * dimensions[leg] + colors[leg] - 1;
  }
  switch (basis) {
  case 0: switch (index) {
    case 0: return cdouble(ldouble(1.0e0L), ldouble(0.0L));
    case 4: return cdouble(ldouble(1.0e0L), ldouble(0.0L));
    case 8: return cdouble(ldouble(1.0e0L), ldouble(0.0L));
    case 36: return cdouble(ldouble(1.0e0L), ldouble(0.0L));
    case 40: return cdouble(ldouble(1.0e0L), ldouble(0.0L));
    case 44: return cdouble(ldouble(1.0e0L), ldouble(0.0L));
    case 72: return cdouble(ldouble(1.0e0L), ldouble(0.0L));
    case 76: return cdouble(ldouble(1.0e0L), ldouble(0.0L));
    case 80: return cdouble(ldouble(1.0e0L), ldouble(0.0L));
    default: return cdouble(0,0); }
  case 1: switch (index) {
    case 0: return cdouble(ldouble(3.33333333333333333333333e-1L), ldouble(0.0L));
    case 4: return cdouble(ldouble(-1.66666666666666666666666e-1L), ldouble(0.0L));
    case 8: return cdouble(ldouble(-1.66666666666666666666666e-1L), ldouble(0.0L));
    case 12: return cdouble(ldouble(5.0e-1L), ldouble(0.0L));
    case 24: return cdouble(ldouble(5.0e-1L), ldouble(0.0L));
    case 28: return cdouble(ldouble(5.0e-1L), ldouble(0.0L));
    case 36: return cdouble(ldouble(-1.66666666666666666666666e-1L), ldouble(0.0L));
    case 40: return cdouble(ldouble(3.33333333333333333333333e-1L), ldouble(0.0L));
    case 44: return cdouble(ldouble(-1.66666666666666666666666e-1L), ldouble(0.0L));
    case 52: return cdouble(ldouble(5.0e-1L), ldouble(0.0L));
    case 56: return cdouble(ldouble(5.0e-1L), ldouble(0.0L));
    case 68: return cdouble(ldouble(5.0e-1L), ldouble(0.0L));
    case 72: return cdouble(ldouble(-1.66666666666666666666666e-1L), ldouble(0.0L));
    case 76: return cdouble(ldouble(-1.66666666666666666666666e-1L), ldouble(0.0L));
    case 80: return cdouble(ldouble(3.33333333333333333333333e-1L), ldouble(0.0L));
    default: return cdouble(0,0); }
  default: throw std::invalid_argument("Color basis index is out of range.");
  }
}
} // namespace hepcat_generated
