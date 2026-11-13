#include "process.hpp"
#include <algorithm>
#include <cmath>
#include <stdexcept>

namespace hepcat_generated {
using namespace hepcat_spinas;

constexpr std::size_t sm5eebarZZh::leg_count;
constexpr std::size_t sm5eebarZZh::parameter_count;
constexpr std::size_t sm5eebarZZh::helicity_count;


sm5eebarZZh::sm5eebarZZh() : sm5eebarZZh(default_parameters()) {}

sm5eebarZZh::sm5eebarZZh(const Parameters& values) {
  set_parameters(values);
}

sm5eebarZZh::sm5eebarZZh(const ldouble& value_EE, const ldouble& value_MW, const ldouble& value_SW, const
  ldouble& value_Me, const ldouble& value_MZ, const ldouble& value_Mh)
  : sm5eebarZZh(Parameters{{cdouble(value_Me), cdouble(value_MZ), cdouble(value_Mh), cdouble(value_EE),
    cdouble(value_MW), cdouble(value_SW)}}) {}

// Copies rebuild caches, so every sproduct points into its own process.
sm5eebarZZh::sm5eebarZZh(const sm5eebarZZh& other) {
  widths_enabled = other.widths_enabled;
  set_parameters(other.parameters);
  if (other.momenta_ready) set_momenta(other.momenta);
}

sm5eebarZZh& sm5eebarZZh::operator=(const sm5eebarZZh& other) {
  if (this != &other) {
    widths_enabled = other.widths_enabled;
    set_parameters(other.parameters);
    if (other.momenta_ready) set_momenta(other.momenta);
  }
  return *this;
}

sm5eebarZZh::Parameters sm5eebarZZh::default_parameters() {
  return Parameters{{
    cdouble(ldouble(5.0e-4L), ldouble(0.0L)), // "Me"
    cdouble(ldouble(9.12921430831531714430205e1L), ldouble(0.0L)), // "MZ"
    cdouble(ldouble(1.25e2L), ldouble(0.0L)), // "Mh"
    cdouble(ldouble(3.1333e-1L), ldouble(0.0L)), // "EE"
    cdouble(ldouble(8.0385e1L), ldouble(0.0L)), // "MW"
    cdouble(ldouble(4.74e-1L), ldouble(0.0L)) // "SW"
  }};
}

sm5eebarZZh::Spins sm5eebarZZh::default_spins() {
  return Spins{{-1, -1, -2, -2, 0}};
}

std::array<std::size_t, 2> sm5eebarZZh::incoming_legs() {
  return {{0, 1}};
}

std::array<sm5eebarZZh::Spins, sm5eebarZZh::helicity_count>
sm5eebarZZh::helicity_configurations() {
  return std::array<Spins, helicity_count>{{
    {{0, 0, 0, 0, 0}}
  }};
}

ldouble sm5eebarZZh::incoming_spin_average() {
  return ldouble(4.0e0L);
}

ldouble sm5eebarZZh::symmetry_factor() {
  return ldouble(5.0e-1L);
}

void sm5eebarZZh::initialize_products() {
  a12a = spinas::sproduct(::ANGLE, &p1, &p2);
  a34a = spinas::sproduct(::ANGLE, &p3, &p4);
  s34s = spinas::sproduct(::SQUARE, &p3, &p4);
  s12s = spinas::sproduct(::SQUARE, &p1, &p2);
  s152a = spinas::sproduct(::SQUARE, &p1, &p5, &p2);
  s251a = spinas::sproduct(::SQUARE, &p2, &p5, &p1);
  s343a = spinas::sproduct(::SQUARE, &p3, &p4, &p3);
  s353a = spinas::sproduct(::SQUARE, &p3, &p5, &p3);
  s454a = spinas::sproduct(::SQUARE, &p4, &p5, &p4);
  s434a = spinas::sproduct(::SQUARE, &p4, &p3, &p4);
  s13s = spinas::sproduct(::SQUARE, &p1, &p3);
  s24s = spinas::sproduct(::SQUARE, &p2, &p4);
  a13a = spinas::sproduct(::ANGLE, &p1, &p3);
  a24a = spinas::sproduct(::ANGLE, &p2, &p4);
  s314a = spinas::sproduct(::SQUARE, &p3, &p1, &p4);
  a23a = spinas::sproduct(::ANGLE, &p2, &p3);
  s23s = spinas::sproduct(::SQUARE, &p2, &p3);
  s213a = spinas::sproduct(::SQUARE, &p2, &p1, &p3);
  s312a = spinas::sproduct(::SQUARE, &p3, &p1, &p2);
  s413a = spinas::sproduct(::SQUARE, &p4, &p1, &p3);
  a14a = spinas::sproduct(::ANGLE, &p1, &p4);
  s14s = spinas::sproduct(::SQUARE, &p1, &p4);
  s253a = spinas::sproduct(::SQUARE, &p2, &p5, &p3);
  s352a = spinas::sproduct(::SQUARE, &p3, &p5, &p2);
  s124a = spinas::sproduct(::SQUARE, &p1, &p2, &p4);
  s324a = spinas::sproduct(::SQUARE, &p3, &p2, &p4);
  s421a = spinas::sproduct(::SQUARE, &p4, &p2, &p1);
  s423a = spinas::sproduct(::SQUARE, &p4, &p2, &p3);
  s153a = spinas::sproduct(::SQUARE, &p1, &p5, &p3);
  s351a = spinas::sproduct(::SQUARE, &p3, &p5, &p1);
  s154a = spinas::sproduct(::SQUARE, &p1, &p5, &p4);
  s451a = spinas::sproduct(::SQUARE, &p4, &p5, &p1);
  s254a = spinas::sproduct(::SQUARE, &p2, &p5, &p4);
  s452a = spinas::sproduct(::SQUARE, &p4, &p5, &p2);
  s143a = spinas::sproduct(::SQUARE, &p1, &p4, &p3);
  s341a = spinas::sproduct(::SQUARE, &p3, &p4, &p1);
  a1243a = spinas::sproduct(::ANGLE, &p1, &p2, &p4, &p3);
  a1253a = spinas::sproduct(::ANGLE, &p1, &p2, &p5, &p3);
  s1243s = spinas::sproduct(::SQUARE, &p1, &p2, &p4, &p3);
  s1253s = spinas::sproduct(::SQUARE, &p1, &p2, &p5, &p3);
  s234a = spinas::sproduct(::SQUARE, &p2, &p3, &p4);
  s432a = spinas::sproduct(::SQUARE, &p4, &p3, &p2);
  a2134a = spinas::sproduct(::ANGLE, &p2, &p1, &p3, &p4);
  a2154a = spinas::sproduct(::ANGLE, &p2, &p1, &p5, &p4);
  s2134s = spinas::sproduct(::SQUARE, &p2, &p1, &p3, &p4);
  s2154s = spinas::sproduct(::SQUARE, &p2, &p1, &p5, &p4);
  a3124a = spinas::sproduct(::ANGLE, &p3, &p1, &p2, &p4);
  s3124s = spinas::sproduct(::SQUARE, &p3, &p1, &p2, &p4);
}

void sm5eebarZZh::set_parameters(const Parameters& values) {
  for (const auto& value : values) {
    if (!finite_complex(value)) {
      throw std::invalid_argument("Model parameters must be finite.");
    }
  }
  momenta_ready = false;
  all_point_cache_ready = false;
  summed_point_cache_ready = false;
  parameters = values;
  Me = parameters[0];
  MZ = parameters[1];
  Mh = parameters[2];
  EE = parameters[3];
  MW = parameters[4];
  SW = parameters[5];

  // Recompute model-only factors once after parameter changes.
  parameter_factor_1_2 = (EE * EE * EE);
  parameter_factor_1 = parameter_factor_1_2 * Me;
  parameter_factor_3_2 = (MW * MW * MW);
  parameter_factor_4 = (SW * SW * SW);
  parameter_factor_3 = parameter_factor_3_2 * parameter_factor_4;
  parameter_factor_6_2 = (Mh * Mh);
  parameter_factor_6 = ldouble(3.0L) * parameter_factor_1_2 * Me * parameter_factor_6_2;
  parameter_factor_8_2 = (Me * Me * Me);
  parameter_factor_8 = parameter_factor_1_2 * parameter_factor_8_2;
  parameter_factor_10_2 = (Me * Me);
  parameter_factor_10 = parameter_factor_1_2 * parameter_factor_10_2;
  parameter_factor_12_2 = (MW * MW);
  parameter_factor_12 = parameter_factor_1_2 * Me * parameter_factor_12_2;
  parameter_factor_14_2 = (SW * SW);
  parameter_factor_14 = (ldouble(1.0L) - parameter_factor_14_2);
  parameter_factor_16_2 = std::pow(cdouble(parameter_factor_14), ldouble(1.0L / 2.0L));
  parameter_factor_16 = (MW) / (parameter_factor_16_2);
  parameter_factor_18_3 = ldouble(2.0L) * parameter_factor_14_2;
  parameter_factor_18_2 = (-ldouble(1.0L) + parameter_factor_18_3);
  parameter_factor_18 = parameter_factor_1_2 * Me * parameter_factor_12_2 * parameter_factor_14_2 *
    parameter_factor_18_2;
  parameter_factor_21_2 = std::pow(cdouble(parameter_factor_18_2), 2);
  parameter_factor_21 = parameter_factor_1_2 * parameter_factor_12_2 * parameter_factor_21_2;
  parameter_factor_23 = parameter_factor_1_2 * Me * parameter_factor_14_2 * parameter_factor_18_2;
  parameter_factor_24_2 = (SW * SW * SW * SW);
  parameter_factor_24 = parameter_factor_1_2 * parameter_factor_24_2;
  parameter_factor_26 = Me * MW;
  parameter_factor_27 = parameter_factor_1_2 * parameter_factor_21_2;
  parameter_factor_28 = ldouble(2.0L) * parameter_factor_1_2 * parameter_factor_12_2 * parameter_factor_24_2;
  parameter_factor_29 = ldouble(2.0L) * Me * MW * parameter_factor_21_2;
  parameter_factor_30 = ldouble(4.0L) * parameter_factor_10_2 * parameter_factor_14_2 * parameter_factor_18_2;
  parameter_factor_31 = ldouble(8.0L) * Me * MW * parameter_factor_24_2;
  parameter_factor_32 = ldouble(2.0L) * Me * parameter_factor_14_2 * parameter_factor_18_2;
  parameter_factor_33 = ldouble(4.0L) * MW * parameter_factor_24_2;
  parameter_factor_34 = ldouble(4.0L) * parameter_factor_24_2;
  parameter_factor_35 = ldouble(8.0L) * Me * parameter_factor_24_2;
  parameter_factor_36 = MW * parameter_factor_21_2;
  parameter_factor_37 = ldouble(2.0L) * Me * parameter_factor_21_2;
  parameter_factor_38 = ldouble(4.0L) * parameter_factor_3_2 * parameter_factor_4;
  parameter_factor_39 = ldouble(8.0L) * parameter_factor_3_2 * parameter_factor_24_2;
  parameter_factor_40 = std::pow(cdouble(parameter_factor_14), ldouble(3.0L / 2.0L));
  parameter_factor_41 = ldouble(4.0L) * Me * parameter_factor_12_2 * parameter_factor_14_2 *
    parameter_factor_18_2;
  parameter_factor_42 = ldouble(2.0L) * parameter_factor_3_2 * parameter_factor_21_2;
  parameter_factor_43 = ldouble(8.0L) * parameter_factor_12_2 * parameter_factor_24_2;
  parameter_factor_44 = ldouble(8.0L) * parameter_factor_24_2;
  parameter_factor_45 = ldouble(2.0L) * parameter_factor_21_2;
  parameter_factor_46 = ldouble(2.0L) * parameter_factor_12_2 * parameter_factor_21_2;
  parameter_factor_47 = ldouble(2.0L) * parameter_factor_12_2 * parameter_factor_14_2 * parameter_factor_18_2;
  parameter_factor_48 = ldouble(4.0L) * Me * MW * parameter_factor_24_2;
  parameter_factor_49 = Me * MW * parameter_factor_21_2;
  parameter_factor_50 = ldouble(2.0L) * parameter_factor_10_2 * parameter_factor_14_2 * parameter_factor_18_2;
  parameter_factor_51 = ldouble(4.0L) * parameter_factor_14_2 * parameter_factor_18_2;
  parameter_factor_52 = ldouble(4.0L) * Me * parameter_factor_24_2;
  parameter_factor_53 = ldouble(2.0L) * MW * parameter_factor_14_2 * parameter_factor_18_2;
  parameter_factor_54 = Me * parameter_factor_21_2;
  parameter_factor_55 = ldouble(2.0L) * parameter_factor_14_2 * parameter_factor_18_2;
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

void sm5eebarZZh::set_widths_enabled(bool enabled) {
  if (widths_enabled == enabled) return;
  widths_enabled = enabled;
  momenta_ready = false;
  all_point_cache_ready = false;
  summed_point_cache_ready = false;
}

void sm5eebarZZh::set_masses(const ldouble& value_Me, const ldouble& value_MZ, const ldouble& value_Mh) {
  Parameters values = parameters;
  values[0] = cdouble(value_Me);
  values[1] = cdouble(value_MZ);
  values[2] = cdouble(value_Mh);
  set_parameters(values);
}

void sm5eebarZZh::set_momenta(const Momenta& values) {
  const auto validation = validate_momenta(values, external_masses, incoming_legs());
  if (!validation.valid) throw std::invalid_argument(validation.message);
  momenta_ready = false;
  all_point_cache_ready = false;
  summed_point_cache_ready = false;
  momenta = values;

  // External particle momenta.
  p1.set_momentum(momenta[0].data());
  p2.set_momentum(momenta[1].data());
  p3.set_momentum(momenta[2].data());
  p4.set_momentum(momenta[3].data());
  p5.set_momentum(momenta[4].data());

  // Propagator denominators and scalar invariants.
  t12 = ((std::imag(Mh) == ldouble(0) && std::isfinite(std::real(Mh))) ? cdouble(mass2(sum_momenta(momenta, {1,
    2})) - (std::real(Mh)) * (std::real(Mh)), ldouble(0)) : (mass2(sum_momenta(momenta, {1, 2})) - (Mh) *
    (Mh)));
  t34 = ((std::imag(Mh) == ldouble(0) && std::isfinite(std::real(Mh))) ? cdouble(mass2(sum_momenta(momenta, {3,
    4})) - (std::real(Mh)) * (std::real(Mh)), ldouble(0)) : (mass2(sum_momenta(momenta, {3, 4})) - (Mh) *
    (Mh)));
  t25 = ((std::imag(Me) == ldouble(0) && std::isfinite(std::real(Me))) ? cdouble(mass2(sum_momenta(momenta, {2,
    -5})) - (std::real(Me)) * (std::real(Me)), ldouble(0)) : (mass2(sum_momenta(momenta, {2, -5})) - (Me) *
    (Me)));
  t15 = ((std::imag(Me) == ldouble(0) && std::isfinite(std::real(Me))) ? cdouble(mass2(sum_momenta(momenta, {1,
    -5})) - (std::real(Me)) * (std::real(Me)), ldouble(0)) : (mass2(sum_momenta(momenta, {1, -5})) - (Me) *
    (Me)));
  t45 = mass2(sum_momenta(momenta, {4, 5})) - (parameter_factor_16) * (parameter_factor_16);
  t35 = mass2(sum_momenta(momenta, {3, 5})) - (parameter_factor_16) * (parameter_factor_16);
  p1_dot_p3 = p1.dot(p3);
  t13 = ((std::imag(Me) == ldouble(0) && std::isfinite(std::real(Me))) ? cdouble(mass2(sum_momenta(momenta, {1,
    -3})) - (std::real(Me)) * (std::real(Me)), ldouble(0)) : (mass2(sum_momenta(momenta, {1, -3})) - (Me) *
    (Me)));
  t14 = ((std::imag(Me) == ldouble(0) && std::isfinite(std::real(Me))) ? cdouble(mass2(sum_momenta(momenta, {1,
    -4})) - (std::real(Me)) * (std::real(Me)), ldouble(0)) : (mass2(sum_momenta(momenta, {1, -4})) - (Me) *
    (Me)));
  p2_dot_p4 = p2.dot(p4);
  t24 = ((std::imag(Me) == ldouble(0) && std::isfinite(std::real(Me))) ? cdouble(mass2(sum_momenta(momenta, {2,
    -4})) - (std::real(Me)) * (std::real(Me)), ldouble(0)) : (mass2(sum_momenta(momenta, {2, -4})) - (Me) *
    (Me)));
  t23 = ((std::imag(Me) == ldouble(0) && std::isfinite(std::real(Me))) ? cdouble(mass2(sum_momenta(momenta, {2,
    -3})) - (std::real(Me)) * (std::real(Me)), ldouble(0)) : (mass2(sum_momenta(momenta, {2, -3})) - (Me) *
    (Me)));
  p2_dot_p5 = p2.dot(p5);
  p1_dot_p5 = p1.dot(p5);
  p1_dot_p2 = p1.dot(p2);
  momenta_ready = true;
}

// Prepare only data used by the retained helicity contributions.
void sm5eebarZZh::prepare_summed_point_cache() {
  if (summed_point_cache_ready) return;
  a1243a.update();
  a1253a.update();
  a12a.update();
  a13a.update();
  a14a.update();
  a2134a.update();
  a2154a.update();
  a23a.update();
  a24a.update();
  a3124a.update();
  a34a.update();
  s1243s.update();
  s124a.update();
  s1253s.update();
  s12s.update();
  s13s.update();
  s143a.update();
  s14s.update();
  s152a.update();
  s153a.update();
  s154a.update();
  s2134s.update();
  s213a.update();
  s2154s.update();
  s234a.update();
  s23s.update();
  s24s.update();
  s251a.update();
  s253a.update();
  s254a.update();
  s3124s.update();
  s312a.update();
  s314a.update();
  s324a.update();
  s341a.update();
  s343a.update();
  s34s.update();
  s351a.update();
  s352a.update();
  s353a.update();
  s413a.update();
  s421a.update();
  s423a.update();
  s432a.update();
  s434a.update();
  s451a.update();
  s452a.update();
  s454a.update();
  real_denominator_domain = std::imag(parameters[0]) == ldouble(0) && std::imag(parameters[1]) == ldouble(0) &&
    std::imag(parameters[2]) == ldouble(0) && std::imag(parameters[3]) == ldouble(0) && std::imag(parameters[4])
    == ldouble(0) && std::imag(parameters[5]) == ldouble(0);
  point_product_1[0][0] = a12a.v(-1, -1);
  point_product_1[0][1] = a12a.v(-1, 1);
  point_product_1[1][0] = a12a.v(1, -1);
  point_product_1[1][1] = a12a.v(1, 1);
  point_product_2[0][0] = a34a.v(-1, -1);
  point_product_2[0][1] = a34a.v(-1, 1);
  point_product_2[1][0] = a34a.v(1, -1);
  point_product_2[1][1] = a34a.v(1, 1);
  point_product_3[0][0] = s34s.v(-1, -1);
  point_product_3[0][1] = s34s.v(-1, 1);
  point_product_3[1][0] = s34s.v(1, -1);
  point_product_3[1][1] = s34s.v(1, 1);
  point_product_4[0][0] = s12s.v(-1, -1);
  point_product_4[0][1] = s12s.v(-1, 1);
  point_product_4[1][0] = s12s.v(1, -1);
  point_product_4[1][1] = s12s.v(1, 1);
  point_product_5[0][0] = s152a.v(-1, -1);
  point_product_5[0][1] = s152a.v(-1, 1);
  point_product_5[1][0] = s152a.v(1, -1);
  point_product_5[1][1] = s152a.v(1, 1);
  point_product_6[0][0] = s251a.v(-1, -1);
  point_product_6[0][1] = s251a.v(-1, 1);
  point_product_6[1][0] = s251a.v(1, -1);
  point_product_6[1][1] = s251a.v(1, 1);
  point_product_7[0][0] = s343a.v(-1, -1);
  point_product_7[0][1] = s343a.v(-1, 1);
  point_product_7[1][0] = s343a.v(1, -1);
  point_product_7[1][1] = s343a.v(1, 1);
  point_product_8[0][0] = s353a.v(-1, -1);
  point_product_8[0][1] = s353a.v(-1, 1);
  point_product_8[1][0] = s353a.v(1, -1);
  point_product_8[1][1] = s353a.v(1, 1);
  point_product_9[0][0] = s454a.v(-1, -1);
  point_product_9[0][1] = s454a.v(-1, 1);
  point_product_9[1][0] = s454a.v(1, -1);
  point_product_9[1][1] = s454a.v(1, 1);
  point_product_10[0][0] = s434a.v(-1, -1);
  point_product_10[0][1] = s434a.v(-1, 1);
  point_product_10[1][0] = s434a.v(1, -1);
  point_product_10[1][1] = s434a.v(1, 1);
  point_product_11[0][0] = s13s.v(-1, -1);
  point_product_11[0][1] = s13s.v(-1, 1);
  point_product_11[1][0] = s13s.v(1, -1);
  point_product_11[1][1] = s13s.v(1, 1);
  point_product_12[0][0] = s24s.v(-1, -1);
  point_product_12[0][1] = s24s.v(-1, 1);
  point_product_12[1][0] = s24s.v(1, -1);
  point_product_12[1][1] = s24s.v(1, 1);
  point_product_13[0][0] = a13a.v(-1, -1);
  point_product_13[0][1] = a13a.v(-1, 1);
  point_product_13[1][0] = a13a.v(1, -1);
  point_product_13[1][1] = a13a.v(1, 1);
  point_product_14[0][0] = a24a.v(-1, -1);
  point_product_14[0][1] = a24a.v(-1, 1);
  point_product_14[1][0] = a24a.v(1, -1);
  point_product_14[1][1] = a24a.v(1, 1);
  point_product_15[0][0] = s314a.v(-1, -1);
  point_product_15[0][1] = s314a.v(-1, 1);
  point_product_15[1][0] = s314a.v(1, -1);
  point_product_15[1][1] = s314a.v(1, 1);
  point_product_16[0][0] = a23a.v(-1, -1);
  point_product_16[0][1] = a23a.v(-1, 1);
  point_product_16[1][0] = a23a.v(1, -1);
  point_product_16[1][1] = a23a.v(1, 1);
  point_product_17[0][0] = s23s.v(-1, -1);
  point_product_17[0][1] = s23s.v(-1, 1);
  point_product_17[1][0] = s23s.v(1, -1);
  point_product_17[1][1] = s23s.v(1, 1);
  point_product_18[0][0] = s213a.v(-1, -1);
  point_product_18[0][1] = s213a.v(-1, 1);
  point_product_18[1][0] = s213a.v(1, -1);
  point_product_18[1][1] = s213a.v(1, 1);
  point_product_19[0][0] = s312a.v(-1, -1);
  point_product_19[0][1] = s312a.v(-1, 1);
  point_product_19[1][0] = s312a.v(1, -1);
  point_product_19[1][1] = s312a.v(1, 1);
  point_product_20[0][0] = s413a.v(-1, -1);
  point_product_20[0][1] = s413a.v(-1, 1);
  point_product_20[1][0] = s413a.v(1, -1);
  point_product_20[1][1] = s413a.v(1, 1);
  point_product_21[0][0] = a14a.v(-1, -1);
  point_product_21[0][1] = a14a.v(-1, 1);
  point_product_21[1][0] = a14a.v(1, -1);
  point_product_21[1][1] = a14a.v(1, 1);
  point_product_22[0][0] = s14s.v(-1, -1);
  point_product_22[0][1] = s14s.v(-1, 1);
  point_product_22[1][0] = s14s.v(1, -1);
  point_product_22[1][1] = s14s.v(1, 1);
  point_product_23[0][0] = s253a.v(-1, -1);
  point_product_23[0][1] = s253a.v(-1, 1);
  point_product_23[1][0] = s253a.v(1, -1);
  point_product_23[1][1] = s253a.v(1, 1);
  point_product_24[0][0] = s352a.v(-1, -1);
  point_product_24[0][1] = s352a.v(-1, 1);
  point_product_24[1][0] = s352a.v(1, -1);
  point_product_24[1][1] = s352a.v(1, 1);
  point_product_25[0][0] = s124a.v(-1, -1);
  point_product_25[0][1] = s124a.v(-1, 1);
  point_product_25[1][0] = s124a.v(1, -1);
  point_product_25[1][1] = s124a.v(1, 1);
  point_product_26[0][0] = s324a.v(-1, -1);
  point_product_26[0][1] = s324a.v(-1, 1);
  point_product_26[1][0] = s324a.v(1, -1);
  point_product_26[1][1] = s324a.v(1, 1);
  point_product_27[0][0] = s421a.v(-1, -1);
  point_product_27[0][1] = s421a.v(-1, 1);
  point_product_27[1][0] = s421a.v(1, -1);
  point_product_27[1][1] = s421a.v(1, 1);
  point_product_28[0][0] = s423a.v(-1, -1);
  point_product_28[0][1] = s423a.v(-1, 1);
  point_product_28[1][0] = s423a.v(1, -1);
  point_product_28[1][1] = s423a.v(1, 1);
  point_product_29[0][0] = s153a.v(-1, -1);
  point_product_29[0][1] = s153a.v(-1, 1);
  point_product_29[1][0] = s153a.v(1, -1);
  point_product_29[1][1] = s153a.v(1, 1);
  point_product_30[0][0] = s351a.v(-1, -1);
  point_product_30[0][1] = s351a.v(-1, 1);
  point_product_30[1][0] = s351a.v(1, -1);
  point_product_30[1][1] = s351a.v(1, 1);
  point_product_31[0][0] = s154a.v(-1, -1);
  point_product_31[0][1] = s154a.v(-1, 1);
  point_product_31[1][0] = s154a.v(1, -1);
  point_product_31[1][1] = s154a.v(1, 1);
  point_product_32[0][0] = s451a.v(-1, -1);
  point_product_32[0][1] = s451a.v(-1, 1);
  point_product_32[1][0] = s451a.v(1, -1);
  point_product_32[1][1] = s451a.v(1, 1);
  point_product_33[0][0] = s254a.v(-1, -1);
  point_product_33[0][1] = s254a.v(-1, 1);
  point_product_33[1][0] = s254a.v(1, -1);
  point_product_33[1][1] = s254a.v(1, 1);
  point_product_34[0][0] = s452a.v(-1, -1);
  point_product_34[0][1] = s452a.v(-1, 1);
  point_product_34[1][0] = s452a.v(1, -1);
  point_product_34[1][1] = s452a.v(1, 1);
  point_product_35[0][0] = s143a.v(-1, -1);
  point_product_35[0][1] = s143a.v(-1, 1);
  point_product_35[1][0] = s143a.v(1, -1);
  point_product_35[1][1] = s143a.v(1, 1);
  point_product_36[0][0] = s341a.v(-1, -1);
  point_product_36[0][1] = s341a.v(-1, 1);
  point_product_36[1][0] = s341a.v(1, -1);
  point_product_36[1][1] = s341a.v(1, 1);
  point_product_37[0][0] = a1243a.v(-1, -1);
  point_product_37[0][1] = a1243a.v(-1, 1);
  point_product_37[1][0] = a1243a.v(1, -1);
  point_product_37[1][1] = a1243a.v(1, 1);
  point_product_38[0][0] = a1253a.v(-1, -1);
  point_product_38[0][1] = a1253a.v(-1, 1);
  point_product_38[1][0] = a1253a.v(1, -1);
  point_product_38[1][1] = a1253a.v(1, 1);
  point_product_39[0][0] = s1243s.v(-1, -1);
  point_product_39[0][1] = s1243s.v(-1, 1);
  point_product_39[1][0] = s1243s.v(1, -1);
  point_product_39[1][1] = s1243s.v(1, 1);
  point_product_40[0][0] = s1253s.v(-1, -1);
  point_product_40[0][1] = s1253s.v(-1, 1);
  point_product_40[1][0] = s1253s.v(1, -1);
  point_product_40[1][1] = s1253s.v(1, 1);
  point_product_41[0][0] = s234a.v(-1, -1);
  point_product_41[0][1] = s234a.v(-1, 1);
  point_product_41[1][0] = s234a.v(1, -1);
  point_product_41[1][1] = s234a.v(1, 1);
  point_product_42[0][0] = s432a.v(-1, -1);
  point_product_42[0][1] = s432a.v(-1, 1);
  point_product_42[1][0] = s432a.v(1, -1);
  point_product_42[1][1] = s432a.v(1, 1);
  point_product_43[0][0] = a2134a.v(-1, -1);
  point_product_43[0][1] = a2134a.v(-1, 1);
  point_product_43[1][0] = a2134a.v(1, -1);
  point_product_43[1][1] = a2134a.v(1, 1);
  point_product_44[0][0] = a2154a.v(-1, -1);
  point_product_44[0][1] = a2154a.v(-1, 1);
  point_product_44[1][0] = a2154a.v(1, -1);
  point_product_44[1][1] = a2154a.v(1, 1);
  point_product_45[0][0] = s2134s.v(-1, -1);
  point_product_45[0][1] = s2134s.v(-1, 1);
  point_product_45[1][0] = s2134s.v(1, -1);
  point_product_45[1][1] = s2134s.v(1, 1);
  point_product_46[0][0] = s2154s.v(-1, -1);
  point_product_46[0][1] = s2154s.v(-1, 1);
  point_product_46[1][0] = s2154s.v(1, -1);
  point_product_46[1][1] = s2154s.v(1, 1);
  point_product_47[0][0] = a3124a.v(-1, -1);
  point_product_47[0][1] = a3124a.v(-1, 1);
  point_product_47[1][0] = a3124a.v(1, -1);
  point_product_47[1][1] = a3124a.v(1, 1);
  point_product_48[0][0] = s3124s.v(-1, -1);
  point_product_48[0][1] = s3124s.v(-1, 1);
  point_product_48[1][0] = s3124s.v(1, -1);
  point_product_48[1][1] = s3124s.v(1, 1);
  point_factor_1 = (parameter_factor_3 * t12);
  point_factor_2 = (parameter_factor_3 * t34);
  point_factor_3 = (point_factor_2 * t12);
  point_factor_4 = (point_factor_2 * t25);
  point_factor_5 = (point_factor_2 * t15);
  point_factor_6 = ((parameter_factor_3 * t45) * t12);
  point_factor_7 = ((parameter_factor_3 * t35) * t12);
  point_factor_8 = ((parameter_factor_3 * t13) * t45);
  point_factor_9 = (parameter_factor_38 * t14);
  point_factor_10 = (point_factor_9 * t25);
  point_factor_11 = ((parameter_factor_3 * t24) * t35);
  point_factor_12 = ((parameter_factor_38 * t24) * t15);
  point_factor_13 = (parameter_factor_38 * t23);
  point_factor_14 = (point_factor_13 * t15);
  point_factor_15 = (parameter_factor_38 * t13);
  point_factor_16 = (point_factor_15 * t25);
  point_factor_17 = (parameter_factor_44 * p2_dot_p4);
  point_factor_18 = (parameter_factor_44 * p2_dot_p5);
  point_factor_19 = (parameter_factor_45 * p2_dot_p4);
  point_factor_20 = (parameter_factor_45 * p2_dot_p5);
  point_factor_21 = (point_factor_13 * t45);
  point_factor_22 = (parameter_factor_44 * p1_dot_p3);
  point_factor_23 = (parameter_factor_44 * p1_dot_p5);
  point_factor_24 = (parameter_factor_45 * p1_dot_p3);
  point_factor_25 = (parameter_factor_45 * p1_dot_p5);
  point_factor_26 = (point_factor_9 * t35);
  point_factor_27 = (parameter_factor_51 * p1_dot_p2);
  point_factor_28 = (point_factor_13 * t14);
  point_factor_29 = (point_factor_15 * t24);
  point_denominator_1 = hepcat_spinas::CachedDenominator(ldouble(4.0L), real_denominator_domain);
  point_denominator_2 = hepcat_spinas::CachedDenominator(point_factor_1, real_denominator_domain);
  point_denominator_3 = hepcat_spinas::CachedDenominator(point_factor_3, real_denominator_domain);
  point_denominator_4 = hepcat_spinas::CachedDenominator(point_factor_4, real_denominator_domain);
  point_denominator_5 = hepcat_spinas::CachedDenominator(point_factor_5, real_denominator_domain);
  point_denominator_6 = hepcat_spinas::CachedDenominator((ldouble(2.0L) * parameter_factor_14),
    real_denominator_domain);
  point_denominator_7 = hepcat_spinas::CachedDenominator(parameter_factor_14, real_denominator_domain);
  point_denominator_8 = hepcat_spinas::CachedDenominator(parameter_factor_16_2, real_denominator_domain);
  point_denominator_9 = hepcat_spinas::CachedDenominator(ldouble(2.0L), real_denominator_domain);
  point_denominator_10 = hepcat_spinas::CachedDenominator(point_factor_10, real_denominator_domain);
  point_denominator_11 = hepcat_spinas::CachedDenominator(point_factor_12, real_denominator_domain);
  point_denominator_12 = hepcat_spinas::CachedDenominator(point_factor_14, real_denominator_domain);
  point_denominator_13 = hepcat_spinas::CachedDenominator(point_factor_16, real_denominator_domain);
  point_denominator_14 = hepcat_spinas::CachedDenominator(parameter_factor_40, real_denominator_domain);
  point_denominator_15 = hepcat_spinas::CachedDenominator(point_factor_28, real_denominator_domain);
  point_denominator_16 = hepcat_spinas::CachedDenominator(point_factor_29, real_denominator_domain);
  summed_point_cache_ready = true;
}

// Individual amplitudes can lazily request the remaining helicities.
void sm5eebarZZh::prepare_all_point_cache() {
  if (all_point_cache_ready) return;
  prepare_summed_point_cache();
  all_point_cache_ready = true;
}

void sm5eebarZZh::set_momenta(const ldouble mom1[4], const ldouble mom2[4], const ldouble mom3[4], const ldouble
  mom4[4], const ldouble mom5[4]) {
  Momenta values;
  std::copy(mom1, mom1 + 4, values[0].begin());
  std::copy(mom2, mom2 + 4, values[1].begin());
  std::copy(mom3, mom3 + 4, values[2].begin());
  std::copy(mom4, mom4 + 4, values[3].begin());
  std::copy(mom5, mom5 + 4, values[4].begin());
  set_momenta(values);
}

cdouble sm5eebarZZh::amp(const Spins& spins) {
  return amp(spins[0], spins[1], spins[2], spins[3], spins[4]);
}

// set_momenta(...) must be called before evaluating an amplitude.
cdouble sm5eebarZZh::amp(const int& ds1, const int& ds2, const int& ds3, const int& ds4, const int& ds5) {
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
cdouble sm5eebarZZh::amp_unchecked(const int& ds1, const int& ds2, const int& ds3, const int& ds4, const int&
  ds5) {
  cdouble amplitude(0, 0);
  const auto assignments3 = spinor_assignments(2, ds3);
  const auto assignments4 = spinor_assignments(2, ds4);
  for (const auto& indices3 : assignments3) {
    const int ds3_1 = indices3[0];
    const int ds3_2 = indices3[1];
    for (const auto& indices4 : assignments4) {
      const int ds4_1 = indices4[0];
      const int ds4_2 = indices4[1];
      const cdouble spin_factor_1 = (parameter_factor_1 * point_product_1[(ds1) > 0 ? 1 : 0][(ds2) > 0 ? 1 :
        0]);
      const cdouble spin_factor_2 = (point_denominator_1.divide((-(((parameter_factor_8 * point_product_1[(ds1)
        > 0 ? 1 : 0][(ds2) > 0 ? 1 : 0]) * point_product_2[(ds3_1) > 0 ? 1 : 0][(ds4_1) > 0 ? 1 : 0]) *
        point_product_3[(ds3_2) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0]))) -
        point_denominator_1.divide((((parameter_factor_8 * point_product_2[(ds3_1) > 0 ? 1 : 0][(ds4_1) > 0 ? 1
        : 0]) * point_product_4[(ds1) > 0 ? 1 : 0][(ds2) > 0 ? 1 : 0]) * point_product_3[(ds3_2) > 0 ? 1 :
        0][(ds4_2) > 0 ? 1 : 0])));
      const cdouble spin_factor_3 = ((parameter_factor_10 * point_product_2[(ds3_1) > 0 ? 1 : 0][(ds4_1) > 0 ? 1
        : 0]) * point_product_3[(ds3_2) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0]);
      const cdouble spin_factor_4 = (point_denominator_6.divide((-(((parameter_factor_12 * point_product_1[(ds1)
        > 0 ? 1 : 0][(ds2) > 0 ? 1 : 0]) * point_product_2[(ds3_1) > 0 ? 1 : 0][(ds4_1) > 0 ? 1 : 0]) *
        point_product_3[(ds3_2) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0]))) -
        point_denominator_6.divide((((parameter_factor_12 * point_product_2[(ds3_1) > 0 ? 1 : 0][(ds4_1) > 0 ? 1
        : 0]) * point_product_4[(ds1) > 0 ? 1 : 0][(ds2) > 0 ? 1 : 0]) * point_product_3[(ds3_2) > 0 ? 1 :
        0][(ds4_2) > 0 ? 1 : 0])));
      const cdouble spin_factor_5 = (point_product_7[(ds3_1) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0] +
        point_product_8[(ds3_1) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0]);
      const cdouble spin_factor_6 = (parameter_factor_1 * point_product_4[(ds1) > 0 ? 1 : 0][(ds2) > 0 ? 1 :
        0]);
      const cdouble spin_factor_7 = (point_product_10[(ds4_1) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0] +
        point_product_9[(ds4_1) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0]);
      const cdouble spin_factor_8 = (point_denominator_7.divide((((parameter_factor_18 * point_product_2[(ds3_1)
        > 0 ? 1 : 0][(ds4_1) > 0 ? 1 : 0]) * point_product_11[(ds1) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0]) *
        point_product_12[(ds2) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0])) +
        point_denominator_7.divide((((parameter_factor_18 * point_product_13[(ds1) > 0 ? 1 : 0][(ds3_2) > 0 ? 1
        : 0]) * point_product_14[(ds2) > 0 ? 1 : 0][(ds4_1) > 0 ? 1 : 0]) * point_product_3[(ds3_1) > 0 ? 1 :
        0][(ds4_2) > 0 ? 1 : 0])));
      const cdouble spin_factor_9 = ((parameter_factor_21 * point_product_13[(ds1) > 0 ? 1 : 0][(ds3_1) > 0 ? 1
        : 0]) * point_product_12[(ds2) > 0 ? 1 : 0][(ds4_1) > 0 ? 1 : 0]);
      const cdouble spin_factor_10 = (MW * point_product_2[(ds3_2) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0]);
      const cdouble spin_factor_11 = ((parameter_factor_28 * point_product_14[(ds2) > 0 ? 1 : 0][(ds4_1) > 0 ? 1
        : 0]) * point_product_11[(ds1) > 0 ? 1 : 0][(ds3_1) > 0 ? 1 : 0]);
      const cdouble spin_factor_12 = (MW * point_product_3[(ds3_2) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0]);
      const cdouble spin_factor_13 = (parameter_factor_29 * point_product_21[(ds1) > 0 ? 1 : 0][(ds4_1) > 0 ? 1
        : 0]);
      const cdouble spin_factor_14 = (parameter_factor_30 * point_product_2[(ds3_1) > 0 ? 1 : 0][(ds4_1) > 0 ? 1
        : 0]);
      const cdouble spin_factor_15 = ((spin_factor_14 * point_product_22[(ds1) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 :
        0]) * point_product_17[(ds2) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0]);
      const cdouble spin_factor_16 = (((parameter_factor_30 * point_product_21[(ds1) > 0 ? 1 : 0][(ds4_1) > 0 ?
        1 : 0]) * point_product_16[(ds2) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0]) * point_product_3[(ds3_1) > 0 ? 1 :
        0][(ds4_2) > 0 ? 1 : 0]);
      const cdouble spin_factor_17 = (parameter_factor_31 * point_product_16[(ds2) > 0 ? 1 : 0][(ds3_2) > 0 ? 1
        : 0]);
      const cdouble spin_factor_18 = (parameter_factor_32 * point_product_21[(ds1) > 0 ? 1 : 0][(ds4_1) > 0 ? 1
        : 0]);
      const cdouble spin_factor_19 = (parameter_factor_33 * point_product_22[(ds1) > 0 ? 1 : 0][(ds4_1) > 0 ? 1
        : 0]);
      const cdouble spin_factor_20 = (parameter_factor_34 * point_product_22[(ds1) > 0 ? 1 : 0][(ds4_1) > 0 ? 1
        : 0]);
      const cdouble spin_factor_21 = ((parameter_factor_35 * point_product_16[(ds2) > 0 ? 1 : 0][(ds3_1) > 0 ? 1
        : 0]) * point_product_22[(ds1) > 0 ? 1 : 0][(ds4_1) > 0 ? 1 : 0]);
      const cdouble spin_factor_22 = (parameter_factor_36 * point_product_21[(ds1) > 0 ? 1 : 0][(ds4_1) > 0 ? 1
        : 0]);
      const cdouble spin_factor_23 = (parameter_factor_32 * point_product_2[(ds3_1) > 0 ? 1 : 0][(ds4_1) > 0 ? 1
        : 0]);
      const cdouble spin_factor_24 = ((parameter_factor_37 * point_product_21[(ds1) > 0 ? 1 : 0][(ds4_1) > 0 ? 1
        : 0]) * point_product_17[(ds2) > 0 ? 1 : 0][(ds3_1) > 0 ? 1 : 0]);
      const cdouble spin_factor_25 = (parameter_factor_21_2 * point_product_21[(ds1) > 0 ? 1 : 0][(ds4_1) > 0 ?
        1 : 0]);
      const cdouble spin_factor_26 = (parameter_factor_31 * point_product_14[(ds2) > 0 ? 1 : 0][(ds4_1) > 0 ? 1
        : 0]);
      const cdouble spin_factor_27 = ((spin_factor_14 * point_product_11[(ds1) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 :
        0]) * point_product_12[(ds2) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0]);
      const cdouble spin_factor_28 = (((parameter_factor_30 * point_product_13[(ds1) > 0 ? 1 : 0][(ds3_2) > 0 ?
        1 : 0]) * point_product_14[(ds2) > 0 ? 1 : 0][(ds4_1) > 0 ? 1 : 0]) * point_product_3[(ds3_1) > 0 ? 1 :
        0][(ds4_2) > 0 ? 1 : 0]);
      const cdouble spin_factor_29 = (parameter_factor_29 * point_product_13[(ds1) > 0 ? 1 : 0][(ds3_2) > 0 ? 1
        : 0]);
      const cdouble spin_factor_30 = ((parameter_factor_37 * point_product_13[(ds1) > 0 ? 1 : 0][(ds3_1) > 0 ? 1
        : 0]) * point_product_12[(ds2) > 0 ? 1 : 0][(ds4_1) > 0 ? 1 : 0]);
      const cdouble spin_factor_31 = ((parameter_factor_35 * point_product_14[(ds2) > 0 ? 1 : 0][(ds4_1) > 0 ? 1
        : 0]) * point_product_11[(ds1) > 0 ? 1 : 0][(ds3_1) > 0 ? 1 : 0]);
      const cdouble spin_factor_32 = (parameter_factor_21_2 * point_product_17[(ds2) > 0 ? 1 : 0][(ds3_1) > 0 ?
        1 : 0]);
      const cdouble spin_factor_33 = (parameter_factor_34 * point_product_16[(ds2) > 0 ? 1 : 0][(ds3_1) > 0 ? 1
        : 0]);
      const cdouble spin_factor_34 = (parameter_factor_39 * point_product_16[(ds2) > 0 ? 1 : 0][(ds3_2) > 0 ? 1
        : 0]);
      const cdouble spin_factor_35 = point_denominator_7.divide((((parameter_factor_41 * point_product_2[(ds3_1)
        > 0 ? 1 : 0][(ds4_1) > 0 ? 1 : 0]) * point_product_22[(ds1) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0]) *
        point_product_17[(ds2) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0]));
      const cdouble spin_factor_36 = point_denominator_7.divide((((parameter_factor_41 * point_product_21[(ds1)
        > 0 ? 1 : 0][(ds4_1) > 0 ? 1 : 0]) * point_product_16[(ds2) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0]) *
        point_product_3[(ds3_1) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0]));
      const cdouble spin_factor_37 = (parameter_factor_42 * point_product_21[(ds1) > 0 ? 1 : 0][(ds4_1) > 0 ? 1
        : 0]);
      const cdouble spin_factor_38 = ((parameter_factor_43 * point_product_16[(ds2) > 0 ? 1 : 0][(ds3_1) > 0 ? 1
        : 0]) * point_product_22[(ds1) > 0 ? 1 : 0][(ds4_1) > 0 ? 1 : 0]);
      const cdouble spin_factor_39 = (parameter_factor_33 * point_product_16[(ds2) > 0 ? 1 : 0][(ds3_1) > 0 ? 1
        : 0]);
      const cdouble spin_factor_40 = (parameter_factor_32 * point_product_17[(ds2) > 0 ? 1 : 0][(ds3_1) > 0 ? 1
        : 0]);
      const cdouble spin_factor_41 = (parameter_factor_32 * point_product_16[(ds2) > 0 ? 1 : 0][(ds3_1) > 0 ? 1
        : 0]);
      const cdouble spin_factor_42 = (parameter_factor_36 * point_product_17[(ds2) > 0 ? 1 : 0][(ds3_1) > 0 ? 1
        : 0]);
      const cdouble spin_factor_43 = ((parameter_factor_46 * point_product_21[(ds1) > 0 ? 1 : 0][(ds4_1) > 0 ? 1
        : 0]) * point_product_17[(ds2) > 0 ? 1 : 0][(ds3_1) > 0 ? 1 : 0]);
      const cdouble spin_factor_44 = (spin_factor_32 * point_product_9[(ds4_1) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 :
        0]);
      const cdouble spin_factor_45 = (spin_factor_33 * point_product_9[(ds4_1) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 :
        0]);
      const cdouble spin_factor_46 = (parameter_factor_32 * point_product_22[(ds1) > 0 ? 1 : 0][(ds4_1) > 0 ? 1
        : 0]);
      const cdouble spin_factor_47 = (spin_factor_18 * point_product_8[(ds3_1) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 :
        0]);
      const cdouble spin_factor_48 = (spin_factor_19 * point_product_8[(ds3_1) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 :
        0]);
      const cdouble spin_factor_49 = (spin_factor_20 * point_product_8[(ds3_1) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 :
        0]);
      const cdouble spin_factor_50 = (spin_factor_25 * point_product_8[(ds3_1) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 :
        0]);
      const cdouble spin_factor_51 = (parameter_factor_48 * point_product_16[(ds2) > 0 ? 1 : 0][(ds3_2) > 0 ? 1
        : 0]);
      const cdouble spin_factor_52 = (parameter_factor_49 * point_product_21[(ds1) > 0 ? 1 : 0][(ds4_1) > 0 ? 1
        : 0]);
      const cdouble spin_factor_53 = (parameter_factor_50 * point_product_2[(ds3_1) > 0 ? 1 : 0][(ds4_1) > 0 ? 1
        : 0]);
      const cdouble spin_factor_54 = ((parameter_factor_52 * point_product_16[(ds2) > 0 ? 1 : 0][(ds3_1) > 0 ? 1
        : 0]) * point_product_22[(ds1) > 0 ? 1 : 0][(ds4_1) > 0 ? 1 : 0]);
      const cdouble spin_factor_55 = ((parameter_factor_53 * point_product_22[(ds1) > 0 ? 1 : 0][(ds4_1) > 0 ? 1
        : 0]) * point_product_17[(ds2) > 0 ? 1 : 0][(ds3_1) > 0 ? 1 : 0]);
      const cdouble spin_factor_56 = ((parameter_factor_53 * point_product_21[(ds1) > 0 ? 1 : 0][(ds4_1) > 0 ? 1
        : 0]) * point_product_16[(ds2) > 0 ? 1 : 0][(ds3_1) > 0 ? 1 : 0]);
      const cdouble spin_factor_57 = ((parameter_factor_54 * point_product_21[(ds1) > 0 ? 1 : 0][(ds4_1) > 0 ? 1
        : 0]) * point_product_17[(ds2) > 0 ? 1 : 0][(ds3_1) > 0 ? 1 : 0]);
      const cdouble spin_factor_58 = (parameter_factor_48 * point_product_14[(ds2) > 0 ? 1 : 0][(ds4_1) > 0 ? 1
        : 0]);
      const cdouble spin_factor_59 = (parameter_factor_49 * point_product_13[(ds1) > 0 ? 1 : 0][(ds3_2) > 0 ? 1
        : 0]);
      const cdouble spin_factor_60 = ((parameter_factor_53 * point_product_13[(ds1) > 0 ? 1 : 0][(ds3_1) > 0 ? 1
        : 0]) * point_product_14[(ds2) > 0 ? 1 : 0][(ds4_1) > 0 ? 1 : 0]);
      const cdouble spin_factor_61 = ((parameter_factor_54 * point_product_13[(ds1) > 0 ? 1 : 0][(ds3_1) > 0 ? 1
        : 0]) * point_product_12[(ds2) > 0 ? 1 : 0][(ds4_1) > 0 ? 1 : 0]);
      const cdouble spin_factor_62 = ((parameter_factor_53 * point_product_11[(ds1) > 0 ? 1 : 0][(ds3_1) > 0 ? 1
        : 0]) * point_product_12[(ds2) > 0 ? 1 : 0][(ds4_1) > 0 ? 1 : 0]);
      const cdouble spin_factor_63 = ((parameter_factor_52 * point_product_14[(ds2) > 0 ? 1 : 0][(ds4_1) > 0 ? 1
        : 0]) * point_product_11[(ds1) > 0 ? 1 : 0][(ds3_1) > 0 ? 1 : 0]);
      amplitude += (point_denominator_2.divide((point_denominator_1.divide((-((spin_factor_1 *
        point_product_2[(ds3_1) > 0 ? 1 : 0][(ds4_1) > 0 ? 1 : 0]) * point_product_3[(ds3_2) > 0 ? 1 :
        0][(ds4_2) > 0 ? 1 : 0]))) - point_denominator_1.divide((((parameter_factor_1 * point_product_2[(ds3_1)
        > 0 ? 1 : 0][(ds4_1) > 0 ? 1 : 0]) * point_product_4[(ds1) > 0 ? 1 : 0][(ds2) > 0 ? 1 : 0]) *
        point_product_3[(ds3_2) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0])))) +
        point_denominator_3.divide((point_denominator_1.divide((-(((parameter_factor_6 * point_product_1[(ds1) >
        0 ? 1 : 0][(ds2) > 0 ? 1 : 0]) * point_product_2[(ds3_1) > 0 ? 1 : 0][(ds4_1) > 0 ? 1 : 0]) *
        point_product_3[(ds3_2) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0]))) -
        point_denominator_1.divide((((parameter_factor_6 * point_product_2[(ds3_1) > 0 ? 1 : 0][(ds4_1) > 0 ? 1
        : 0]) * point_product_4[(ds1) > 0 ? 1 : 0][(ds2) > 0 ? 1 : 0]) * point_product_3[(ds3_2) > 0 ? 1 :
        0][(ds4_2) > 0 ? 1 : 0])))) + point_denominator_4.divide(((spin_factor_2 +
        point_denominator_1.divide((spin_factor_3 * (((-Me) * point_product_4[(ds1) > 0 ? 1 : 0][(ds2) > 0 ? 1 :
        0]) - point_product_5[(ds1) > 0 ? 1 : 0][(ds2) > 0 ? 1 : 0])))) +
        point_denominator_1.divide((spin_factor_3 * (((-Me) * point_product_1[(ds1) > 0 ? 1 : 0][(ds2) > 0 ? 1 :
        0]) - point_product_6[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]))))) +
        point_denominator_5.divide(((spin_factor_2 - point_denominator_1.divide((spin_factor_3 * ((Me *
        point_product_1[(ds1) > 0 ? 1 : 0][(ds2) > 0 ? 1 : 0]) - point_product_5[(ds1) > 0 ? 1 : 0][(ds2) > 0 ?
        1 : 0])))) - point_denominator_1.divide((spin_factor_3 * ((Me * point_product_4[(ds1) > 0 ? 1 : 0][(ds2)
        > 0 ? 1 : 0]) - point_product_6[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]))))) + (((spin_factor_4 -
        point_denominator_1.divide(((spin_factor_1 * spin_factor_5) * point_product_9[(ds4_1) > 0 ? 1 :
        0][(ds4_2) > 0 ? 1 : 0]))) - point_denominator_1.divide(((spin_factor_6 * spin_factor_5) *
        point_product_9[(ds4_1) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0]))) / point_factor_6) + (((spin_factor_4 -
        point_denominator_1.divide(((spin_factor_1 * point_product_8[(ds3_1) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0])
        * spin_factor_7))) - point_denominator_1.divide(((spin_factor_6 * point_product_8[(ds3_1) > 0 ? 1 :
        0][(ds3_2) > 0 ? 1 : 0]) * spin_factor_7))) / point_factor_7) + (((((((spin_factor_8 -
        point_denominator_6.divide((spin_factor_9 * (point_denominator_8.divide(spin_factor_10) -
        point_product_15[(ds3_2) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0])))) +
        point_denominator_9.divide((((parameter_factor_23 * point_product_11[(ds1) > 0 ? 1 : 0][(ds3_1) > 0 ? 1
        : 0]) * (((Me * point_product_16[(ds2) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0]) +
        point_denominator_8.divide((MW * point_product_17[(ds2) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0]))) +
        point_product_18[(ds2) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0])) * point_product_9[(ds4_1) > 0 ? 1 :
        0][(ds4_2) > 0 ? 1 : 0]))) - (((parameter_factor_24 * point_product_11[(ds1) > 0 ? 1 : 0][(ds3_1) > 0 ?
        1 : 0]) * (((((parameter_factor_10_2 * point_product_16[(ds2) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0]) +
        point_denominator_7.divide((parameter_factor_12_2 * point_product_16[(ds2) > 0 ? 1 : 0][(ds3_2) > 0 ? 1
        : 0]))) - ((ldouble(2.0L) * p1_dot_p3) * point_product_16[(ds2) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0])) +
        point_denominator_8.divide((parameter_factor_26 * point_product_17[(ds2) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 :
        0]))) + (Me * point_product_18[(ds2) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0]))) * point_product_9[(ds4_1) > 0
        ? 1 : 0][(ds4_2) > 0 ? 1 : 0])) - point_denominator_9.divide((((parameter_factor_23 *
        point_product_13[(ds1) > 0 ? 1 : 0][(ds3_1) > 0 ? 1 : 0]) * ((point_denominator_8.divide((-(MW *
        point_product_16[(ds2) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0]))) - (Me * point_product_17[(ds2) > 0 ? 1 :
        0][(ds3_2) > 0 ? 1 : 0])) - point_product_19[(ds3_2) > 0 ? 1 : 0][(ds2) > 0 ? 1 : 0])) *
        point_product_9[(ds4_1) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0]))) +
        point_denominator_1.divide((((parameter_factor_27 * point_product_13[(ds1) > 0 ? 1 : 0][(ds3_1) > 0 ? 1
        : 0]) * ((((point_denominator_8.divide((-(parameter_factor_26 * point_product_16[(ds2) > 0 ? 1 :
        0][(ds3_2) > 0 ? 1 : 0]))) - (parameter_factor_10_2 * point_product_17[(ds2) > 0 ? 1 : 0][(ds3_2) > 0 ?
        1 : 0])) - point_denominator_7.divide((parameter_factor_12_2 * point_product_17[(ds2) > 0 ? 1 :
        0][(ds3_2) > 0 ? 1 : 0]))) + ((ldouble(2.0L) * p1_dot_p3) * point_product_17[(ds2) > 0 ? 1 : 0][(ds3_2)
        > 0 ? 1 : 0])) - (Me * point_product_19[(ds3_2) > 0 ? 1 : 0][(ds2) > 0 ? 1 : 0]))) *
        point_product_9[(ds4_1) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0]))) -
        point_denominator_7.divide((spin_factor_11 * (point_denominator_8.divide(spin_factor_12) -
        point_product_20[(ds4_2) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0])))) / point_factor_8) -
        point_denominator_10.divide((parameter_factor_1 *
        (((((((((((point_denominator_8.divide((-((spin_factor_13 * point_product_2[(ds3_1) > 0 ? 1 : 0][(ds4_2)
        > 0 ? 1 : 0]) * point_product_17[(ds2) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0]))) + spin_factor_15) +
        spin_factor_16) - point_denominator_8.divide(((spin_factor_17 * point_product_22[(ds1) > 0 ? 1 :
        0][(ds4_1) > 0 ? 1 : 0]) * point_product_3[(ds3_1) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0]))) -
        ((spin_factor_18 * point_product_3[(ds3_1) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0]) * point_product_23[(ds2) >
        0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0])) + point_denominator_8.divide(((spin_factor_19 *
        point_product_3[(ds3_1) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0]) * point_product_23[(ds2) > 0 ? 1 : 0][(ds3_2)
        > 0 ? 1 : 0]))) + ((spin_factor_20 * point_product_23[(ds2) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0]) *
        point_product_15[(ds3_1) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0])) - (spin_factor_21 *
        point_product_15[(ds3_2) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0])) +
        point_denominator_8.divide(((spin_factor_22 * point_product_2[(ds3_1) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0])
        * point_product_24[(ds3_2) > 0 ? 1 : 0][(ds2) > 0 ? 1 : 0]))) - ((spin_factor_23 *
        point_product_22[(ds1) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0]) * point_product_24[(ds3_2) > 0 ? 1 : 0][(ds2)
        > 0 ? 1 : 0])) - (spin_factor_24 * point_product_20[(ds4_2) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0])) +
        ((spin_factor_25 * point_product_24[(ds3_1) > 0 ? 1 : 0][(ds2) > 0 ? 1 : 0]) * point_product_20[(ds4_2)
        > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0])))) + (((((((spin_factor_8 -
        point_denominator_9.divide((((parameter_factor_23 * point_product_12[(ds2) > 0 ? 1 : 0][(ds4_1) > 0 ? 1
        : 0]) * (((Me * point_product_21[(ds1) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0]) +
        point_denominator_8.divide((MW * point_product_22[(ds1) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0]))) +
        point_product_25[(ds1) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0])) * point_product_8[(ds3_1) > 0 ? 1 :
        0][(ds3_2) > 0 ? 1 : 0]))) - point_denominator_1.divide((((parameter_factor_27 * point_product_12[(ds2)
        > 0 ? 1 : 0][(ds4_1) > 0 ? 1 : 0]) * ((((((-parameter_factor_10_2) * point_product_21[(ds1) > 0 ? 1 :
        0][(ds4_2) > 0 ? 1 : 0]) - point_denominator_7.divide((parameter_factor_12_2 * point_product_21[(ds1) >
        0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0]))) + ((ldouble(2.0L) * p2_dot_p4) * point_product_21[(ds1) > 0 ? 1 :
        0][(ds4_2) > 0 ? 1 : 0])) - point_denominator_8.divide((parameter_factor_26 * point_product_22[(ds1) > 0
        ? 1 : 0][(ds4_2) > 0 ? 1 : 0]))) - (Me * point_product_25[(ds1) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0]))) *
        point_product_8[(ds3_1) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0]))) + point_denominator_6.divide((spin_factor_9
        * (point_denominator_8.divide((-spin_factor_12)) - point_product_26[(ds3_2) > 0 ? 1 : 0][(ds4_2) > 0 ? 1
        : 0])))) + point_denominator_9.divide((((parameter_factor_23 * point_product_14[(ds2) > 0 ? 1 :
        0][(ds4_1) > 0 ? 1 : 0]) * point_product_8[(ds3_1) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0]) *
        ((point_denominator_8.divide((-(MW * point_product_21[(ds1) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0]))) - (Me *
        point_product_22[(ds1) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0])) - point_product_27[(ds4_2) > 0 ? 1 : 0][(ds1)
        > 0 ? 1 : 0])))) - (((parameter_factor_24 * point_product_14[(ds2) > 0 ? 1 : 0][(ds4_1) > 0 ? 1 : 0]) *
        point_product_8[(ds3_1) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0]) *
        ((((point_denominator_8.divide((-(parameter_factor_26 * point_product_21[(ds1) > 0 ? 1 : 0][(ds4_2) > 0
        ? 1 : 0]))) - (parameter_factor_10_2 * point_product_22[(ds1) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0])) -
        point_denominator_7.divide((parameter_factor_12_2 * point_product_22[(ds1) > 0 ? 1 : 0][(ds4_2) > 0 ? 1
        : 0]))) + ((ldouble(2.0L) * p2_dot_p4) * point_product_22[(ds1) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0])) -
        (Me * point_product_27[(ds4_2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0])))) +
        point_denominator_7.divide((spin_factor_11 * (point_denominator_8.divide((-spin_factor_10)) -
        point_product_28[(ds4_2) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0])))) / point_factor_11) +
        point_denominator_11.divide((parameter_factor_1 *
        (((((((((((point_denominator_8.divide((-((spin_factor_26 * point_product_2[(ds3_1) > 0 ? 1 : 0][(ds4_2)
        > 0 ? 1 : 0]) * point_product_11[(ds1) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0]))) + spin_factor_27) +
        spin_factor_28) - point_denominator_8.divide(((spin_factor_29 * point_product_12[(ds2) > 0 ? 1 :
        0][(ds4_1) > 0 ? 1 : 0]) * point_product_3[(ds3_1) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0]))) -
        (((parameter_factor_32 * point_product_14[(ds2) > 0 ? 1 : 0][(ds4_1) > 0 ? 1 : 0]) *
        point_product_3[(ds3_1) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0]) * point_product_29[(ds1) > 0 ? 1 : 0][(ds3_2)
        > 0 ? 1 : 0])) + point_denominator_8.divide((((parameter_factor_36 * point_product_12[(ds2) > 0 ? 1 :
        0][(ds4_1) > 0 ? 1 : 0]) * point_product_3[(ds3_1) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0]) *
        point_product_29[(ds1) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0]))) + (((parameter_factor_21_2 *
        point_product_12[(ds2) > 0 ? 1 : 0][(ds4_1) > 0 ? 1 : 0]) * point_product_29[(ds1) > 0 ? 1 : 0][(ds3_2)
        > 0 ? 1 : 0]) * point_product_26[(ds3_1) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0])) - (spin_factor_30 *
        point_product_26[(ds3_2) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0])) +
        point_denominator_8.divide((((parameter_factor_33 * point_product_14[(ds2) > 0 ? 1 : 0][(ds4_1) > 0 ? 1
        : 0]) * point_product_2[(ds3_1) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0]) * point_product_30[(ds3_2) > 0 ? 1 :
        0][(ds1) > 0 ? 1 : 0]))) - ((spin_factor_23 * point_product_12[(ds2) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0])
        * point_product_30[(ds3_2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0])) - (spin_factor_31 *
        point_product_28[(ds4_2) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0])) + (((parameter_factor_34 *
        point_product_14[(ds2) > 0 ? 1 : 0][(ds4_1) > 0 ? 1 : 0]) * point_product_30[(ds3_1) > 0 ? 1 : 0][(ds1)
        > 0 ? 1 : 0]) * point_product_28[(ds4_2) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0])))) -
        point_denominator_12.divide((parameter_factor_1 *
        (((((((((((point_denominator_8.divide((-((spin_factor_17 * point_product_2[(ds3_1) > 0 ? 1 : 0][(ds4_1)
        > 0 ? 1 : 0]) * point_product_22[(ds1) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0]))) + spin_factor_15) +
        spin_factor_16) - point_denominator_8.divide(((spin_factor_13 * point_product_17[(ds2) > 0 ? 1 :
        0][(ds3_2) > 0 ? 1 : 0]) * point_product_3[(ds3_1) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0]))) -
        (((parameter_factor_32 * point_product_16[(ds2) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0]) *
        point_product_3[(ds3_1) > 0 ? 1 : 0][(ds4_1) > 0 ? 1 : 0]) * point_product_31[(ds1) > 0 ? 1 : 0][(ds4_2)
        > 0 ? 1 : 0])) + point_denominator_8.divide((((parameter_factor_36 * point_product_17[(ds2) > 0 ? 1 :
        0][(ds3_2) > 0 ? 1 : 0]) * point_product_3[(ds3_1) > 0 ? 1 : 0][(ds4_1) > 0 ? 1 : 0]) *
        point_product_31[(ds1) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0]))) + (spin_factor_21 * point_product_26[(ds3_2)
        > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0])) - ((spin_factor_32 * point_product_31[(ds1) > 0 ? 1 : 0][(ds4_2) > 0
        ? 1 : 0]) * point_product_28[(ds4_1) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0])) - ((spin_factor_33 *
        point_product_26[(ds3_2) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0]) * point_product_32[(ds4_1) > 0 ? 1 :
        0][(ds1) > 0 ? 1 : 0])) + (spin_factor_24 * point_product_28[(ds4_2) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0]))
        + point_denominator_8.divide((((parameter_factor_33 * point_product_16[(ds2) > 0 ? 1 : 0][(ds3_2) > 0 ?
        1 : 0]) * point_product_2[(ds3_1) > 0 ? 1 : 0][(ds4_1) > 0 ? 1 : 0]) * point_product_32[(ds4_2) > 0 ? 1
        : 0][(ds1) > 0 ? 1 : 0]))) - ((spin_factor_23 * point_product_17[(ds2) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 :
        0]) * point_product_32[(ds4_2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0])))) +
        point_denominator_13.divide((parameter_factor_1 *
        (((((((((((point_denominator_8.divide((-((spin_factor_29 * point_product_2[(ds3_1) > 0 ? 1 : 0][(ds4_1)
        > 0 ? 1 : 0]) * point_product_12[(ds2) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0]))) + spin_factor_27) +
        spin_factor_28) - point_denominator_8.divide(((spin_factor_26 * point_product_11[(ds1) > 0 ? 1 :
        0][(ds3_2) > 0 ? 1 : 0]) * point_product_3[(ds3_1) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0]))) -
        (((parameter_factor_32 * point_product_13[(ds1) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0]) *
        point_product_3[(ds3_1) > 0 ? 1 : 0][(ds4_1) > 0 ? 1 : 0]) * point_product_33[(ds2) > 0 ? 1 : 0][(ds4_2)
        > 0 ? 1 : 0])) + point_denominator_8.divide((((parameter_factor_33 * point_product_11[(ds1) > 0 ? 1 :
        0][(ds3_2) > 0 ? 1 : 0]) * point_product_3[(ds3_1) > 0 ? 1 : 0][(ds4_1) > 0 ? 1 : 0]) *
        point_product_33[(ds2) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0]))) + (spin_factor_30 * point_product_15[(ds3_2)
        > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0])) - (((parameter_factor_34 * point_product_11[(ds1) > 0 ? 1 :
        0][(ds3_1) > 0 ? 1 : 0]) * point_product_33[(ds2) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0]) *
        point_product_20[(ds4_1) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0])) - (((parameter_factor_21_2 *
        point_product_13[(ds1) > 0 ? 1 : 0][(ds3_1) > 0 ? 1 : 0]) * point_product_15[(ds3_2) > 0 ? 1 :
        0][(ds4_2) > 0 ? 1 : 0]) * point_product_34[(ds4_1) > 0 ? 1 : 0][(ds2) > 0 ? 1 : 0])) + (spin_factor_31
        * point_product_20[(ds4_2) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0])) +
        point_denominator_8.divide((((parameter_factor_36 * point_product_13[(ds1) > 0 ? 1 : 0][(ds3_2) > 0 ? 1
        : 0]) * point_product_2[(ds3_1) > 0 ? 1 : 0][(ds4_1) > 0 ? 1 : 0]) * point_product_34[(ds4_2) > 0 ? 1 :
        0][(ds2) > 0 ? 1 : 0]))) - ((spin_factor_23 * point_product_11[(ds1) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0])
        * point_product_34[(ds4_2) > 0 ? 1 : 0][(ds2) > 0 ? 1 : 0])))) - ((parameter_factor_1_2 *
        (((((((((((((((((((((point_denominator_14.divide((-((spin_factor_34 * point_product_2[(ds3_1) > 0 ? 1 :
        0][(ds4_1) > 0 ? 1 : 0]) * point_product_22[(ds1) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0]))) + spin_factor_35)
        + spin_factor_36) - point_denominator_14.divide(((spin_factor_37 * point_product_17[(ds2) > 0 ? 1 :
        0][(ds3_2) > 0 ? 1 : 0]) * point_product_3[(ds3_1) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0]))) +
        point_denominator_7.divide((spin_factor_38 * point_product_26[(ds3_2) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 :
        0]))) - (((point_factor_17 * point_product_16[(ds2) > 0 ? 1 : 0][(ds3_1) > 0 ? 1 : 0]) *
        point_product_11[(ds1) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0]) * point_product_9[(ds4_1) > 0 ? 1 : 0][(ds4_2)
        > 0 ? 1 : 0])) - (((point_factor_18 * point_product_16[(ds2) > 0 ? 1 : 0][(ds3_1) > 0 ? 1 : 0]) *
        point_product_11[(ds1) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0]) * point_product_9[(ds4_1) > 0 ? 1 : 0][(ds4_2)
        > 0 ? 1 : 0])) - (((point_factor_19 * point_product_13[(ds1) > 0 ? 1 : 0][(ds3_1) > 0 ? 1 : 0]) *
        point_product_17[(ds2) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0]) * point_product_9[(ds4_1) > 0 ? 1 : 0][(ds4_2)
        > 0 ? 1 : 0])) - (((point_factor_20 * point_product_13[(ds1) > 0 ? 1 : 0][(ds3_1) > 0 ? 1 : 0]) *
        point_product_17[(ds2) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0]) * point_product_9[(ds4_1) > 0 ? 1 : 0][(ds4_2)
        > 0 ? 1 : 0])) - point_denominator_8.divide(((spin_factor_39 * point_product_35[(ds1) > 0 ? 1 :
        0][(ds3_2) > 0 ? 1 : 0]) * point_product_9[(ds4_1) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0]))) +
        ((spin_factor_40 * point_product_35[(ds1) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0]) * point_product_9[(ds4_1) >
        0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0])) - point_denominator_8.divide(((spin_factor_39 * point_product_29[(ds1)
        > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0]) * point_product_9[(ds4_1) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0]))) +
        ((spin_factor_40 * point_product_29[(ds1) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0]) * point_product_9[(ds4_1) >
        0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0])) + ((spin_factor_41 * point_product_36[(ds3_2) > 0 ? 1 : 0][(ds1) > 0 ?
        1 : 0]) * point_product_9[(ds4_1) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0])) -
        point_denominator_8.divide(((spin_factor_42 * point_product_36[(ds3_2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0])
        * point_product_9[(ds4_1) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0]))) + ((spin_factor_41 *
        point_product_30[(ds3_2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]) * point_product_9[(ds4_1) > 0 ? 1 : 0][(ds4_2)
        > 0 ? 1 : 0])) - point_denominator_8.divide(((spin_factor_42 * point_product_30[(ds3_2) > 0 ? 1 :
        0][(ds1) > 0 ? 1 : 0]) * point_product_9[(ds4_1) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0]))) +
        point_denominator_7.divide((spin_factor_43 * point_product_28[(ds4_2) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 :
        0]))) + (spin_factor_44 * point_product_37[(ds1) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0])) + (spin_factor_44 *
        point_product_38[(ds1) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0])) + (spin_factor_45 * point_product_39[(ds1) >
        0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0])) + (spin_factor_45 * point_product_40[(ds1) > 0 ? 1 : 0][(ds3_2) > 0 ?
        1 : 0]))) / point_factor_21) + ((parameter_factor_1_2 *
        (((((((((((((((((((((point_denominator_14.divide(((spin_factor_37 * point_product_2[(ds3_1) > 0 ? 1 :
        0][(ds4_2) > 0 ? 1 : 0]) * point_product_17[(ds2) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0])) - spin_factor_35)
        - spin_factor_36) + point_denominator_14.divide(((spin_factor_34 * point_product_22[(ds1) > 0 ? 1 :
        0][(ds4_1) > 0 ? 1 : 0]) * point_product_3[(ds3_1) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0]))) -
        (((point_factor_22 * point_product_14[(ds2) > 0 ? 1 : 0][(ds4_1) > 0 ? 1 : 0]) * point_product_22[(ds1)
        > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0]) * point_product_8[(ds3_1) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0])) -
        (((point_factor_23 * point_product_14[(ds2) > 0 ? 1 : 0][(ds4_1) > 0 ? 1 : 0]) * point_product_22[(ds1)
        > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0]) * point_product_8[(ds3_1) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0])) -
        (((point_factor_24 * point_product_21[(ds1) > 0 ? 1 : 0][(ds4_1) > 0 ? 1 : 0]) * point_product_12[(ds2)
        > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0]) * point_product_8[(ds3_1) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0])) -
        (((point_factor_25 * point_product_21[(ds1) > 0 ? 1 : 0][(ds4_1) > 0 ? 1 : 0]) * point_product_12[(ds2)
        > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0]) * point_product_8[(ds3_1) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0])) -
        point_denominator_8.divide(((spin_factor_22 * point_product_41[(ds2) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0])
        * point_product_8[(ds3_1) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0]))) + ((spin_factor_46 *
        point_product_41[(ds2) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0]) * point_product_8[(ds3_1) > 0 ? 1 : 0][(ds3_2)
        > 0 ? 1 : 0])) - point_denominator_8.divide(((spin_factor_22 * point_product_33[(ds2) > 0 ? 1 :
        0][(ds4_2) > 0 ? 1 : 0]) * point_product_8[(ds3_1) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0]))) +
        ((spin_factor_46 * point_product_33[(ds2) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0]) * point_product_8[(ds3_1) >
        0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0])) + point_denominator_7.divide((spin_factor_38 *
        point_product_15[(ds3_2) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0]))) +
        point_denominator_7.divide((spin_factor_43 * point_product_20[(ds4_2) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 :
        0]))) + (spin_factor_47 * point_product_42[(ds4_2) > 0 ? 1 : 0][(ds2) > 0 ? 1 : 0])) -
        point_denominator_8.divide((spin_factor_48 * point_product_42[(ds4_2) > 0 ? 1 : 0][(ds2) > 0 ? 1 : 0])))
        + (spin_factor_47 * point_product_34[(ds4_2) > 0 ? 1 : 0][(ds2) > 0 ? 1 : 0])) -
        point_denominator_8.divide((spin_factor_48 * point_product_34[(ds4_2) > 0 ? 1 : 0][(ds2) > 0 ? 1 : 0])))
        + (spin_factor_49 * point_product_43[(ds2) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0])) + (spin_factor_49 *
        point_product_44[(ds2) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0])) + (spin_factor_50 * point_product_45[(ds2) >
        0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0])) + (spin_factor_50 * point_product_46[(ds2) > 0 ? 1 : 0][(ds4_2) > 0 ?
        1 : 0]))) / point_factor_26) - point_denominator_15.divide((parameter_factor_1 *
        (((((((((((((((((((point_denominator_7.divide((((parameter_factor_47 * point_product_21[(ds1) > 0 ? 1 :
        0][(ds4_1) > 0 ? 1 : 0]) * point_product_16[(ds2) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0]) *
        point_product_2[(ds3_1) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0])) -
        point_denominator_8.divide(((spin_factor_51 * point_product_2[(ds3_1) > 0 ? 1 : 0][(ds4_1) > 0 ? 1 : 0])
        * point_product_22[(ds1) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0]))) -
        point_denominator_8.divide(((spin_factor_52 * point_product_2[(ds3_1) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0])
        * point_product_17[(ds2) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0]))) + ((spin_factor_53 *
        point_product_22[(ds1) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0]) * point_product_17[(ds2) > 0 ? 1 : 0][(ds3_2)
        > 0 ? 1 : 0])) - (((point_factor_27 * point_product_2[(ds3_1) > 0 ? 1 : 0][(ds4_1) > 0 ? 1 : 0]) *
        point_product_22[(ds1) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0]) * point_product_17[(ds2) > 0 ? 1 : 0][(ds3_2)
        > 0 ? 1 : 0])) + (((parameter_factor_50 * point_product_21[(ds1) > 0 ? 1 : 0][(ds4_1) > 0 ? 1 : 0]) *
        point_product_16[(ds2) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0]) * point_product_3[(ds3_1) > 0 ? 1 : 0][(ds4_2)
        > 0 ? 1 : 0])) - (((point_factor_27 * point_product_21[(ds1) > 0 ? 1 : 0][(ds4_1) > 0 ? 1 : 0]) *
        point_product_16[(ds2) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0]) * point_product_3[(ds3_1) > 0 ? 1 : 0][(ds4_2)
        > 0 ? 1 : 0])) - point_denominator_8.divide(((spin_factor_51 * point_product_22[(ds1) > 0 ? 1 :
        0][(ds4_1) > 0 ? 1 : 0]) * point_product_3[(ds3_1) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0]))) -
        point_denominator_8.divide(((spin_factor_52 * point_product_17[(ds2) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0])
        * point_product_3[(ds3_1) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0]))) +
        point_denominator_7.divide((((parameter_factor_47 * point_product_22[(ds1) > 0 ? 1 : 0][(ds4_1) > 0 ? 1
        : 0]) * point_product_17[(ds2) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0]) * point_product_3[(ds3_1) > 0 ? 1 :
        0][(ds4_2) > 0 ? 1 : 0]))) - (spin_factor_54 * point_product_15[(ds3_2) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 :
        0])) + point_denominator_8.divide((spin_factor_55 * point_product_15[(ds3_2) > 0 ? 1 : 0][(ds4_2) > 0 ?
        1 : 0]))) - point_denominator_8.divide((spin_factor_56 * point_product_26[(ds3_2) > 0 ? 1 : 0][(ds4_2) >
        0 ? 1 : 0]))) + (spin_factor_54 * point_product_26[(ds3_2) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0])) +
        point_denominator_8.divide((spin_factor_56 * point_product_20[(ds4_2) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 :
        0]))) - (spin_factor_57 * point_product_20[(ds4_2) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0])) + (spin_factor_57
        * point_product_28[(ds4_2) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0])) -
        point_denominator_8.divide((spin_factor_55 * point_product_28[(ds4_2) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 :
        0]))) + (((parameter_factor_55 * point_product_22[(ds1) > 0 ? 1 : 0][(ds4_1) > 0 ? 1 : 0]) *
        point_product_17[(ds2) > 0 ? 1 : 0][(ds3_1) > 0 ? 1 : 0]) * point_product_47[(ds3_2) > 0 ? 1 :
        0][(ds4_2) > 0 ? 1 : 0])) + (((parameter_factor_55 * point_product_21[(ds1) > 0 ? 1 : 0][(ds4_1) > 0 ? 1
        : 0]) * point_product_16[(ds2) > 0 ? 1 : 0][(ds3_1) > 0 ? 1 : 0]) * point_product_48[(ds3_2) > 0 ? 1 :
        0][(ds4_2) > 0 ? 1 : 0])))) + point_denominator_16.divide((parameter_factor_1 *
        (((((((((((((((((point_denominator_7.divide((((parameter_factor_47 * point_product_13[(ds1) > 0 ? 1 :
        0][(ds3_2) > 0 ? 1 : 0]) * point_product_14[(ds2) > 0 ? 1 : 0][(ds4_1) > 0 ? 1 : 0]) *
        point_product_2[(ds3_1) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0])) -
        point_denominator_8.divide(((spin_factor_58 * point_product_2[(ds3_1) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0])
        * point_product_11[(ds1) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0]))) -
        point_denominator_8.divide(((spin_factor_59 * point_product_2[(ds3_1) > 0 ? 1 : 0][(ds4_1) > 0 ? 1 : 0])
        * point_product_12[(ds2) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0]))) + ((spin_factor_53 *
        point_product_11[(ds1) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0]) * point_product_12[(ds2) > 0 ? 1 : 0][(ds4_2)
        > 0 ? 1 : 0])) + (((parameter_factor_50 * point_product_13[(ds1) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0]) *
        point_product_14[(ds2) > 0 ? 1 : 0][(ds4_1) > 0 ? 1 : 0]) * point_product_3[(ds3_1) > 0 ? 1 : 0][(ds4_2)
        > 0 ? 1 : 0])) - point_denominator_8.divide(((spin_factor_58 * point_product_11[(ds1) > 0 ? 1 :
        0][(ds3_2) > 0 ? 1 : 0]) * point_product_3[(ds3_1) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0]))) -
        point_denominator_8.divide(((spin_factor_59 * point_product_12[(ds2) > 0 ? 1 : 0][(ds4_1) > 0 ? 1 : 0])
        * point_product_3[(ds3_1) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0]))) +
        point_denominator_7.divide((((parameter_factor_47 * point_product_11[(ds1) > 0 ? 1 : 0][(ds3_2) > 0 ? 1
        : 0]) * point_product_12[(ds2) > 0 ? 1 : 0][(ds4_1) > 0 ? 1 : 0]) * point_product_3[(ds3_1) > 0 ? 1 :
        0][(ds4_2) > 0 ? 1 : 0]))) - point_denominator_8.divide((spin_factor_60 * point_product_15[(ds3_2) > 0 ?
        1 : 0][(ds4_2) > 0 ? 1 : 0]))) + (spin_factor_61 * point_product_15[(ds3_2) > 0 ? 1 : 0][(ds4_2) > 0 ? 1
        : 0])) - (spin_factor_61 * point_product_26[(ds3_2) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 : 0])) +
        point_denominator_8.divide((spin_factor_62 * point_product_26[(ds3_2) > 0 ? 1 : 0][(ds4_2) > 0 ? 1 :
        0]))) + (spin_factor_63 * point_product_20[(ds4_2) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0])) -
        point_denominator_8.divide((spin_factor_62 * point_product_20[(ds4_2) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 :
        0]))) + point_denominator_8.divide((spin_factor_60 * point_product_28[(ds4_2) > 0 ? 1 : 0][(ds3_2) > 0 ?
        1 : 0]))) - (spin_factor_63 * point_product_28[(ds4_2) > 0 ? 1 : 0][(ds3_2) > 0 ? 1 : 0])) -
        (((parameter_factor_55 * point_product_11[(ds1) > 0 ? 1 : 0][(ds3_1) > 0 ? 1 : 0]) *
        point_product_12[(ds2) > 0 ? 1 : 0][(ds4_1) > 0 ? 1 : 0]) * point_product_47[(ds3_2) > 0 ? 1 :
        0][(ds4_2) > 0 ? 1 : 0])) - (((parameter_factor_55 * point_product_13[(ds1) > 0 ? 1 : 0][(ds3_1) > 0 ? 1
        : 0]) * point_product_14[(ds2) > 0 ? 1 : 0][(ds4_1) > 0 ? 1 : 0]) * point_product_48[(ds3_2) > 0 ? 1 :
        0][(ds4_2) > 0 ? 1 : 0])))));
    }
  }
  return (spinor_normalization(2, ds3) * spinor_normalization(2, ds4)) * amplitude;
}

// Explicit spin and helicity sums.
ldouble sm5eebarZZh::spin_sum() {
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

ldouble sm5eebarZZh::amp2() {
  // Initial-state average: 1/4.
  // Final-state identical-particle factor: 1/2.
  return spin_sum() / ldouble(8.0L);
}

} // namespace hepcat_generated
