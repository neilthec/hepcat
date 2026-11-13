#include "process.hpp"
#include <algorithm>
#include <cmath>
#include <stdexcept>

namespace hepcat_generated {
using namespace hepcat_spinas;

constexpr std::size_t draftcoloredcurrent4_color_1::leg_count;
constexpr std::size_t draftcoloredcurrent4_color_1::parameter_count;
constexpr std::size_t draftcoloredcurrent4_color_1::helicity_count;


draftcoloredcurrent4_color_1::draftcoloredcurrent4_color_1() : draftcoloredcurrent4_color_1(default_parameters()) {}

draftcoloredcurrent4_color_1::draftcoloredcurrent4_color_1(const Parameters& values) {
  set_parameters(values);
}

draftcoloredcurrent4_color_1::draftcoloredcurrent4_color_1(const ldouble& value_EE, const ldouble& value_MW,
  const ldouble& value_SW, const ldouble& value_Mu, const ldouble& value_Md, const ldouble& value_Mc, const
  ldouble& value_Ms, const ldouble& value_Mh)
  : draftcoloredcurrent4_color_1(Parameters{{cdouble(value_Mu), cdouble(value_Md), cdouble(value_Mc),
    cdouble(value_Ms), cdouble(value_Mh), cdouble(value_EE), cdouble(value_MW), cdouble(value_SW)}}) {}

// Copies rebuild caches, so every sproduct points into its own process.
draftcoloredcurrent4_color_1::draftcoloredcurrent4_color_1(const draftcoloredcurrent4_color_1& other) {
  widths_enabled = other.widths_enabled;
  set_parameters(other.parameters);
  if (other.momenta_ready) set_momenta(other.momenta);
}

draftcoloredcurrent4_color_1& draftcoloredcurrent4_color_1::operator=(const draftcoloredcurrent4_color_1& other) {
  if (this != &other) {
    widths_enabled = other.widths_enabled;
    set_parameters(other.parameters);
    if (other.momenta_ready) set_momenta(other.momenta);
  }
  return *this;
}

draftcoloredcurrent4_color_1::Parameters draftcoloredcurrent4_color_1::default_parameters() {
  return Parameters{{
    cdouble(ldouble(2.16e-3L), ldouble(0.0L)), // "Mu"
    cdouble(ldouble(4.7e-3L), ldouble(0.0L)), // "Md"
    cdouble(ldouble(1.3e0L), ldouble(0.0L)), // "Mc"
    cdouble(ldouble(9.5e-2L), ldouble(0.0L)), // "Ms"
    cdouble(ldouble(1.25e2L), ldouble(0.0L)), // "Mh"
    cdouble(ldouble(3.1333e-1L), ldouble(0.0L)), // "EE"
    cdouble(ldouble(8.0385e1L), ldouble(0.0L)), // "MW"
    cdouble(ldouble(4.74e-1L), ldouble(0.0L)) // "SW"
  }};
}

draftcoloredcurrent4_color_1::Spins draftcoloredcurrent4_color_1::default_spins() {
  return Spins{{-1, -1, -1, -1, 0}};
}

std::array<std::size_t, 2> draftcoloredcurrent4_color_1::incoming_legs() {
  return {{0, 1}};
}

std::array<draftcoloredcurrent4_color_1::Spins, draftcoloredcurrent4_color_1::helicity_count>
draftcoloredcurrent4_color_1::helicity_configurations() {
  return std::array<Spins, helicity_count>{{
    {{0, 0, 0, 0, 0}}
  }};
}

ldouble draftcoloredcurrent4_color_1::incoming_spin_average() {
  return ldouble(4.0e0L);
}

ldouble draftcoloredcurrent4_color_1::symmetry_factor() {
  return ldouble(1.0e0L);
}

void draftcoloredcurrent4_color_1::initialize_products() {
  a14a = spinas::sproduct(::ANGLE, &p1, &p4);
  s23s = spinas::sproduct(::SQUARE, &p2, &p3);
  a12a = spinas::sproduct(::ANGLE, &p1, &p2);
  s12s = spinas::sproduct(::SQUARE, &p1, &p2);
  a34a = spinas::sproduct(::ANGLE, &p3, &p4);
  s34s = spinas::sproduct(::SQUARE, &p3, &p4);
  s152a = spinas::sproduct(::SQUARE, &p1, &p5, &p2);
  s154a = spinas::sproduct(::SQUARE, &p1, &p5, &p4);
  s251a = spinas::sproduct(::SQUARE, &p2, &p5, &p1);
  s253a = spinas::sproduct(::SQUARE, &p2, &p5, &p3);
  s352a = spinas::sproduct(::SQUARE, &p3, &p5, &p2);
  s354a = spinas::sproduct(::SQUARE, &p3, &p5, &p4);
  s451a = spinas::sproduct(::SQUARE, &p4, &p5, &p1);
  s453a = spinas::sproduct(::SQUARE, &p4, &p5, &p3);
}

void draftcoloredcurrent4_color_1::set_parameters(const Parameters& values) {
  for (const auto& value : values) {
    if (!finite_complex(value)) {
      throw std::invalid_argument("Model parameters must be finite.");
    }
  }
  momenta_ready = false;
  all_point_cache_ready = false;
  summed_point_cache_ready = false;
  parameters = values;
  Mu = parameters[0];
  Md = parameters[1];
  Mc = parameters[2];
  Ms = parameters[3];
  Mh = parameters[4];
  EE = parameters[5];
  MW = parameters[6];
  SW = parameters[7];

  // Recompute model-only factors once after parameter changes.
  parameter_factor_1_2 = (EE * EE * EE);
  parameter_factor_2 = (Mu * Mu);
  parameter_factor_1 = parameter_factor_1_2 * parameter_factor_2;
  parameter_factor_4_2 = (SW * SW * SW);
  parameter_factor_4 = ldouble(2.0L) * MW * parameter_factor_4_2;
  parameter_factor_6_2 = (Md * Md);
  parameter_factor_6 = parameter_factor_1_2 * parameter_factor_6_2;
  parameter_factor_8 = parameter_factor_1_2 * MW;
  parameter_factor_9_2 = (Mc * Mc);
  parameter_factor_9 = parameter_factor_1_2 * parameter_factor_9_2;
  parameter_factor_11_2 = (Ms * Ms);
  parameter_factor_11 = parameter_factor_1_2 * parameter_factor_11_2;
  parameter_factor_13_2 = (MW * MW * MW);
  parameter_factor_13 = ldouble(2.0L) * parameter_factor_13_2 * parameter_factor_4_2;
  parameter_factor_15 = parameter_factor_1_2 * Mu;
  parameter_factor_16 = Md * Mu;
  parameter_factor_17 = (Mh * Mh);
  parameter_factor_18 = ldouble(4.0L) * parameter_factor_13_2 * parameter_factor_4_2;
  parameter_factor_19 = parameter_factor_1_2 * Md;
  parameter_factor_20 = parameter_factor_1_2 * Mc;
  parameter_factor_21 = parameter_factor_1_2 * Ms;
  parameter_factor_22 = Mc * Ms;
  external_masses[0] = checked_mass(cdouble(Mu));
  external_masses[1] = checked_mass(cdouble(Md));
  external_masses[2] = checked_mass(cdouble(Mc));
  external_masses[3] = checked_mass(cdouble(Ms));
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

void draftcoloredcurrent4_color_1::set_widths_enabled(bool enabled) {
  if (widths_enabled == enabled) return;
  widths_enabled = enabled;
  momenta_ready = false;
  all_point_cache_ready = false;
  summed_point_cache_ready = false;
}

void draftcoloredcurrent4_color_1::set_masses(const ldouble& value_Mu, const ldouble& value_Md, const ldouble&
  value_Mc, const ldouble& value_Ms, const ldouble& value_Mh) {
  Parameters values = parameters;
  values[0] = cdouble(value_Mu);
  values[1] = cdouble(value_Md);
  values[2] = cdouble(value_Mc);
  values[3] = cdouble(value_Ms);
  values[4] = cdouble(value_Mh);
  set_parameters(values);
}

void draftcoloredcurrent4_color_1::set_momenta(const Momenta& values) {
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
  t34 = ((std::imag(MW) == ldouble(0) && std::isfinite(std::real(MW))) ? cdouble(mass2(sum_momenta(momenta, {3,
    4})) - (std::real(MW)) * (std::real(MW)), ldouble(0)) : (mass2(sum_momenta(momenta, {3, 4})) - (MW) *
    (MW)));
  t15 = ((std::imag(Mu) == ldouble(0) && std::isfinite(std::real(Mu))) ? cdouble(mass2(sum_momenta(momenta, {1,
    -5})) - (std::real(Mu)) * (std::real(Mu)), ldouble(0)) : (mass2(sum_momenta(momenta, {1, -5})) - (Mu) *
    (Mu)));
  t25 = ((std::imag(Md) == ldouble(0) && std::isfinite(std::real(Md))) ? cdouble(mass2(sum_momenta(momenta, {2,
    -5})) - (std::real(Md)) * (std::real(Md)), ldouble(0)) : (mass2(sum_momenta(momenta, {2, -5})) - (Md) *
    (Md)));
  t12 = ((std::imag(MW) == ldouble(0) && std::isfinite(std::real(MW))) ? cdouble(mass2(sum_momenta(momenta, {1,
    2})) - (std::real(MW)) * (std::real(MW)), ldouble(0)) : (mass2(sum_momenta(momenta, {1, 2})) - (MW) *
    (MW)));
  t35 = ((std::imag(Mc) == ldouble(0) && std::isfinite(std::real(Mc))) ? cdouble(mass2(sum_momenta(momenta, {3,
    5})) - (std::real(Mc)) * (std::real(Mc)), ldouble(0)) : (mass2(sum_momenta(momenta, {3, 5})) - (Mc) *
    (Mc)));
  t45 = ((std::imag(Ms) == ldouble(0) && std::isfinite(std::real(Ms))) ? cdouble(mass2(sum_momenta(momenta, {4,
    5})) - (std::real(Ms)) * (std::real(Ms)), ldouble(0)) : (mass2(sum_momenta(momenta, {4, 5})) - (Ms) *
    (Ms)));
  p1_dot_p3 = p1.dot(p3);
  p1_dot_p4 = p1.dot(p4);
  p2_dot_p3 = p2.dot(p3);
  p2_dot_p4 = p2.dot(p4);
  p1_dot_p5 = p1.dot(p5);
  p2_dot_p5 = p2.dot(p5);
  p4_dot_p5 = p4.dot(p5);
  p3_dot_p5 = p3.dot(p5);
  momenta_ready = true;
}

// Prepare only data used by the retained helicity contributions.
void draftcoloredcurrent4_color_1::prepare_summed_point_cache() {
  if (summed_point_cache_ready) return;
  a12a.update();
  a14a.update();
  a34a.update();
  s12s.update();
  s152a.update();
  s154a.update();
  s23s.update();
  s251a.update();
  s253a.update();
  s34s.update();
  s352a.update();
  s354a.update();
  s451a.update();
  s453a.update();
  real_denominator_domain = std::imag(parameters[0]) == ldouble(0) && std::imag(parameters[1]) == ldouble(0) &&
    std::imag(parameters[2]) == ldouble(0) && std::imag(parameters[3]) == ldouble(0) && std::imag(parameters[4])
    == ldouble(0) && std::imag(parameters[5]) == ldouble(0) && std::imag(parameters[6]) == ldouble(0) &&
    std::imag(parameters[7]) == ldouble(0);
  point_product_1[0][0] = a14a.v(-1, -1);
  point_product_1[0][1] = a14a.v(-1, 1);
  point_product_1[1][0] = a14a.v(1, -1);
  point_product_1[1][1] = a14a.v(1, 1);
  point_product_2[0][0] = s23s.v(-1, -1);
  point_product_2[0][1] = s23s.v(-1, 1);
  point_product_2[1][0] = s23s.v(1, -1);
  point_product_2[1][1] = s23s.v(1, 1);
  point_product_3[0][0] = a12a.v(-1, -1);
  point_product_3[0][1] = a12a.v(-1, 1);
  point_product_3[1][0] = a12a.v(1, -1);
  point_product_3[1][1] = a12a.v(1, 1);
  point_product_4[0][0] = s12s.v(-1, -1);
  point_product_4[0][1] = s12s.v(-1, 1);
  point_product_4[1][0] = s12s.v(1, -1);
  point_product_4[1][1] = s12s.v(1, 1);
  point_product_5[0][0] = a34a.v(-1, -1);
  point_product_5[0][1] = a34a.v(-1, 1);
  point_product_5[1][0] = a34a.v(1, -1);
  point_product_5[1][1] = a34a.v(1, 1);
  point_product_6[0][0] = s34s.v(-1, -1);
  point_product_6[0][1] = s34s.v(-1, 1);
  point_product_6[1][0] = s34s.v(1, -1);
  point_product_6[1][1] = s34s.v(1, 1);
  point_product_7[0][0] = s152a.v(-1, -1);
  point_product_7[0][1] = s152a.v(-1, 1);
  point_product_7[1][0] = s152a.v(1, -1);
  point_product_7[1][1] = s152a.v(1, 1);
  point_product_8[0][0] = s154a.v(-1, -1);
  point_product_8[0][1] = s154a.v(-1, 1);
  point_product_8[1][0] = s154a.v(1, -1);
  point_product_8[1][1] = s154a.v(1, 1);
  point_product_9[0][0] = s251a.v(-1, -1);
  point_product_9[0][1] = s251a.v(-1, 1);
  point_product_9[1][0] = s251a.v(1, -1);
  point_product_9[1][1] = s251a.v(1, 1);
  point_product_10[0][0] = s253a.v(-1, -1);
  point_product_10[0][1] = s253a.v(-1, 1);
  point_product_10[1][0] = s253a.v(1, -1);
  point_product_10[1][1] = s253a.v(1, 1);
  point_product_11[0][0] = s352a.v(-1, -1);
  point_product_11[0][1] = s352a.v(-1, 1);
  point_product_11[1][0] = s352a.v(1, -1);
  point_product_11[1][1] = s352a.v(1, 1);
  point_product_12[0][0] = s354a.v(-1, -1);
  point_product_12[0][1] = s354a.v(-1, 1);
  point_product_12[1][0] = s354a.v(1, -1);
  point_product_12[1][1] = s354a.v(1, 1);
  point_product_13[0][0] = s451a.v(-1, -1);
  point_product_13[0][1] = s451a.v(-1, 1);
  point_product_13[1][0] = s451a.v(1, -1);
  point_product_13[1][1] = s451a.v(1, 1);
  point_product_14[0][0] = s453a.v(-1, -1);
  point_product_14[0][1] = s453a.v(-1, 1);
  point_product_14[1][0] = s453a.v(1, -1);
  point_product_14[1][1] = s453a.v(1, 1);
  point_factor_1 = (parameter_factor_4 * t34);
  point_factor_2 = (point_factor_1 * t15);
  point_factor_3 = (point_factor_1 * t25);
  point_factor_4 = ((parameter_factor_4_2 * t34) * t12);
  point_factor_5 = ((parameter_factor_4 * t35) * t12);
  point_factor_6 = ((parameter_factor_4 * t45) * t12);
  point_factor_7 = (parameter_factor_1_2 * p1_dot_p3);
  point_factor_8 = ((parameter_factor_13 * t34) * t12);
  point_factor_9 = (parameter_factor_1_2 * p1_dot_p4);
  point_factor_10 = (parameter_factor_1_2 * p2_dot_p3);
  point_factor_11 = (parameter_factor_1_2 * p2_dot_p4);
  point_factor_12 = (parameter_factor_18 * t34);
  point_factor_13 = (point_factor_12 * t15);
  point_factor_14 = (point_factor_12 * t25);
  point_factor_15 = (point_factor_1 * t12);
  point_factor_16 = ((parameter_factor_18 * t35) * t12);
  point_factor_17 = ((parameter_factor_18 * t45) * t12);
  point_denominator_1 = hepcat_spinas::CachedDenominator(point_factor_2, real_denominator_domain);
  point_denominator_2 = hepcat_spinas::CachedDenominator(point_factor_3, real_denominator_domain);
  point_denominator_3 = hepcat_spinas::CachedDenominator(point_factor_4, real_denominator_domain);
  point_denominator_4 = hepcat_spinas::CachedDenominator(point_factor_5, real_denominator_domain);
  point_denominator_5 = hepcat_spinas::CachedDenominator(point_factor_6, real_denominator_domain);
  point_denominator_6 = hepcat_spinas::CachedDenominator(point_factor_8, real_denominator_domain);
  point_denominator_7 = hepcat_spinas::CachedDenominator(point_factor_13, real_denominator_domain);
  point_denominator_8 = hepcat_spinas::CachedDenominator(point_factor_14, real_denominator_domain);
  point_denominator_9 = hepcat_spinas::CachedDenominator(point_factor_15, real_denominator_domain);
  point_denominator_10 = hepcat_spinas::CachedDenominator(point_factor_16, real_denominator_domain);
  point_denominator_11 = hepcat_spinas::CachedDenominator(point_factor_17, real_denominator_domain);
  summed_point_cache_ready = true;
}

// Individual amplitudes can lazily request the remaining helicities.
void draftcoloredcurrent4_color_1::prepare_all_point_cache() {
  if (all_point_cache_ready) return;
  prepare_summed_point_cache();
  all_point_cache_ready = true;
}

void draftcoloredcurrent4_color_1::set_momenta(const ldouble mom1[4], const ldouble mom2[4], const ldouble
  mom3[4], const ldouble mom4[4], const ldouble mom5[4]) {
  Momenta values;
  std::copy(mom1, mom1 + 4, values[0].begin());
  std::copy(mom2, mom2 + 4, values[1].begin());
  std::copy(mom3, mom3 + 4, values[2].begin());
  std::copy(mom4, mom4 + 4, values[3].begin());
  std::copy(mom5, mom5 + 4, values[4].begin());
  set_momenta(values);
}

cdouble draftcoloredcurrent4_color_1::amp(const Spins& spins) {
  return amp(spins[0], spins[1], spins[2], spins[3], spins[4]);
}

// set_momenta(...) must be called before evaluating an amplitude.
cdouble draftcoloredcurrent4_color_1::amp(const int& ds1, const int& ds2, const int& ds3, const int& ds4, const
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
cdouble draftcoloredcurrent4_color_1::amp_unchecked(const int& ds1, const int& ds2, const int& ds3, const int&
  ds4, const int& ds5) {
  const cdouble spin_factor_1 = (Mu * point_product_4[(ds1) > 0 ? 1 : 0][(ds2) > 0 ? 1 : 0]);
  const cdouble spin_factor_2 = (((-Md) * point_product_3[(ds1) > 0 ? 1 : 0][(ds2) > 0 ? 1 : 0]) +
    spin_factor_1);
  const cdouble spin_factor_3 = (Ms * point_product_6[(ds3) > 0 ? 1 : 0][(ds4) > 0 ? 1 : 0]);
  const cdouble spin_factor_4 = ((Mc * point_product_5[(ds3) > 0 ? 1 : 0][(ds4) > 0 ? 1 : 0]) - spin_factor_3);
  const cdouble spin_factor_5 = (((Md * point_product_3[(ds1) > 0 ? 1 : 0][(ds2) > 0 ? 1 : 0]) - spin_factor_1)
    + point_product_9[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_6 = ((((-Mc) * point_product_5[(ds3) > 0 ? 1 : 0][(ds4) > 0 ? 1 : 0]) +
    spin_factor_3) - point_product_12[(ds3) > 0 ? 1 : 0][(ds4) > 0 ? 1 : 0]);
  return (point_denominator_1.divide(((parameter_factor_1 * point_product_1[(ds1) > 0 ? 1 : 0][(ds4) > 0 ? 1 :
    0]) * point_product_2[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0])) +
    point_denominator_2.divide(((parameter_factor_6 * point_product_1[(ds1) > 0 ? 1 : 0][(ds4) > 0 ? 1 : 0]) *
    point_product_2[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0])) + point_denominator_3.divide(((parameter_factor_8 *
    point_product_1[(ds1) > 0 ? 1 : 0][(ds4) > 0 ? 1 : 0]) * point_product_2[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 :
    0])) + point_denominator_4.divide(((parameter_factor_9 * point_product_1[(ds1) > 0 ? 1 : 0][(ds4) > 0 ? 1 :
    0]) * point_product_2[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0])) +
    point_denominator_5.divide(((parameter_factor_11 * point_product_1[(ds1) > 0 ? 1 : 0][(ds4) > 0 ? 1 : 0]) *
    point_product_2[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0])) - point_denominator_6.divide(((point_factor_7 *
    spin_factor_2) * spin_factor_4)) - point_denominator_6.divide(((point_factor_9 * spin_factor_2) *
    spin_factor_4)) - point_denominator_6.divide(((point_factor_10 * spin_factor_2) * spin_factor_4)) -
    point_denominator_6.divide(((point_factor_11 * spin_factor_2) * spin_factor_4)) +
    point_denominator_7.divide(((parameter_factor_15 * spin_factor_4) * ((((((-parameter_factor_16) *
    point_product_3[(ds1) > 0 ? 1 : 0][(ds2) > 0 ? 1 : 0]) + (parameter_factor_17 * point_product_4[(ds1) > 0 ?
    1 : 0][(ds2) > 0 ? 1 : 0])) + (parameter_factor_2 * point_product_4[(ds1) > 0 ? 1 : 0][(ds2) > 0 ? 1 : 0]))
    - ((ldouble(2.0L) * p1_dot_p5) * point_product_4[(ds1) > 0 ? 1 : 0][(ds2) > 0 ? 1 : 0])) + (Md *
    point_product_7[(ds1) > 0 ? 1 : 0][(ds2) > 0 ? 1 : 0])))) - point_denominator_8.divide(((parameter_factor_19
    * spin_factor_4) * (((((parameter_factor_6_2 * point_product_3[(ds1) > 0 ? 1 : 0][(ds2) > 0 ? 1 : 0]) +
    (parameter_factor_17 * point_product_3[(ds1) > 0 ? 1 : 0][(ds2) > 0 ? 1 : 0])) - ((ldouble(2.0L) *
    p2_dot_p5) * point_product_3[(ds1) > 0 ? 1 : 0][(ds2) > 0 ? 1 : 0])) - (parameter_factor_16 *
    point_product_4[(ds1) > 0 ? 1 : 0][(ds2) > 0 ? 1 : 0])) - (Mu * point_product_7[(ds1) > 0 ? 1 : 0][(ds2) > 0
    ? 1 : 0])))) - point_denominator_1.divide(((parameter_factor_15 * point_product_2[(ds2) > 0 ? 1 : 0][(ds3) >
    0 ? 1 : 0]) * (((-Mu) * point_product_1[(ds1) > 0 ? 1 : 0][(ds4) > 0 ? 1 : 0]) + point_product_8[(ds1) > 0 ?
    1 : 0][(ds4) > 0 ? 1 : 0]))) - point_denominator_7.divide(((parameter_factor_1 * spin_factor_4) *
    spin_factor_5)) - point_denominator_8.divide(((parameter_factor_6 * spin_factor_4) * spin_factor_5)) -
    point_denominator_9.divide(((parameter_factor_1_2 * spin_factor_4) * spin_factor_5)) -
    point_denominator_4.divide(((parameter_factor_20 * point_product_1[(ds1) > 0 ? 1 : 0][(ds4) > 0 ? 1 : 0]) *
    (((-Mc) * point_product_2[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]) + point_product_10[(ds2) > 0 ? 1 : 0][(ds3)
    > 0 ? 1 : 0]))) + point_denominator_2.divide(((parameter_factor_19 * point_product_1[(ds1) > 0 ? 1 :
    0][(ds4) > 0 ? 1 : 0]) * ((Md * point_product_2[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]) -
    point_product_11[(ds3) > 0 ? 1 : 0][(ds2) > 0 ? 1 : 0]))) -
    point_denominator_9.divide(((parameter_factor_1_2 * spin_factor_2) * spin_factor_6)) -
    point_denominator_10.divide(((parameter_factor_9 * spin_factor_2) * spin_factor_6)) -
    point_denominator_11.divide(((parameter_factor_11 * spin_factor_2) * spin_factor_6)) +
    point_denominator_5.divide(((parameter_factor_21 * point_product_2[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]) *
    ((Ms * point_product_1[(ds1) > 0 ? 1 : 0][(ds4) > 0 ? 1 : 0]) - point_product_13[(ds4) > 0 ? 1 : 0][(ds1) >
    0 ? 1 : 0]))) + point_denominator_11.divide(((parameter_factor_21 * spin_factor_2) *
    (((((parameter_factor_22 * point_product_5[(ds3) > 0 ? 1 : 0][(ds4) > 0 ? 1 : 0]) - (parameter_factor_17 *
    point_product_6[(ds3) > 0 ? 1 : 0][(ds4) > 0 ? 1 : 0])) - (parameter_factor_11_2 * point_product_6[(ds3) > 0
    ? 1 : 0][(ds4) > 0 ? 1 : 0])) - ((ldouble(2.0L) * p4_dot_p5) * point_product_6[(ds3) > 0 ? 1 : 0][(ds4) > 0
    ? 1 : 0])) - (Mc * point_product_14[(ds4) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0])))) -
    point_denominator_10.divide(((parameter_factor_20 * spin_factor_2) * ((((((-parameter_factor_9_2) *
    point_product_5[(ds3) > 0 ? 1 : 0][(ds4) > 0 ? 1 : 0]) - (parameter_factor_17 * point_product_5[(ds3) > 0 ?
    1 : 0][(ds4) > 0 ? 1 : 0])) - ((ldouble(2.0L) * p3_dot_p5) * point_product_5[(ds3) > 0 ? 1 : 0][(ds4) > 0 ?
    1 : 0])) + (parameter_factor_22 * point_product_6[(ds3) > 0 ? 1 : 0][(ds4) > 0 ? 1 : 0])) + (Ms *
    point_product_14[(ds4) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0])))));
}

// Explicit spin and helicity sums.
ldouble draftcoloredcurrent4_color_1::spin_sum() {
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

ldouble draftcoloredcurrent4_color_1::amp2() {
  // Initial-state average: 1/4.
  // Final-state identical-particle factor: 1.
  return spin_sum() / ldouble(4.0L);
}

} // namespace hepcat_generated

namespace hepcat_generated {
using namespace hepcat_spinas;
constexpr std::size_t draftcoloredcurrent4::leg_count;
constexpr std::size_t draftcoloredcurrent4::parameter_count;
constexpr std::size_t draftcoloredcurrent4::helicity_count;
constexpr std::size_t draftcoloredcurrent4::color_basis_count;
draftcoloredcurrent4::Parameters draftcoloredcurrent4::default_parameters() { return Parameters{{
  cdouble(ldouble(2.16e-3L), ldouble(0.0L)), // Mu
  cdouble(ldouble(4.7e-3L), ldouble(0.0L)), // Md
  cdouble(ldouble(1.3e0L), ldouble(0.0L)), // Mc
  cdouble(ldouble(9.5e-2L), ldouble(0.0L)), // Ms
  cdouble(ldouble(1.25e2L), ldouble(0.0L)), // Mh
  cdouble(ldouble(3.1333e-1L), ldouble(0.0L)), // EE
  cdouble(ldouble(8.0385e1L), ldouble(0.0L)), // MW
  cdouble(ldouble(4.74e-1L), ldouble(0.0L)) // SW
}}; }
void draftcoloredcurrent4::set_parameters(const Parameters& values) {
  component_1.set_parameters(draftcoloredcurrent4_color_1::Parameters{{values[0], values[1], values[2], values[3], values[4], values[5], values[6], values[7]}});
  parameters = values;
}
void draftcoloredcurrent4::set_momenta(const Momenta& momenta) {
  component_1.set_momenta(momenta);
}
void draftcoloredcurrent4::set_widths_enabled(bool enabled) {
  component_1.set_widths_enabled(enabled);
}
draftcoloredcurrent4::ColorAmplitudes draftcoloredcurrent4::color_amplitudes(const Spins& spins) {
  return ColorAmplitudes{{component_1.amp(spins)}};
}
ldouble draftcoloredcurrent4::color_spin_sum(const Spins& spins) {
  const auto a = color_amplitudes(spins);
  cdouble value(0,0);
  ldouble scale = 0;
  { const cdouble term = std::conj(a[0]) * cdouble(ldouble(9.0e0L), ldouble(0.0L)) * a[0];
    value += term; scale += std::abs(term); }
  const ldouble tolerance = 256 * std::numeric_limits<ldouble>::epsilon() * scale;
  if (std::abs(value.imag()) > tolerance || value.real() < -tolerance)
    throw std::runtime_error("The coherent color sum is not real and nonnegative.");
  return std::max(ldouble(0), value.real());
}
ldouble draftcoloredcurrent4::spin_sum() {
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
ldouble draftcoloredcurrent4::amp2() { return spin_sum() * symmetry_factor() / (incoming_spin_average() * incoming_color_average()); }
cdouble draftcoloredcurrent4::amp(const Spins& spins, const Colors& colors) {
  const auto a = color_amplitudes(spins); cdouble result(0,0);
  for (std::size_t i=0;i<color_basis_count;++i) result += a[i] * color_tensor(i,colors);
  return result;
}
cdouble draftcoloredcurrent4::color_tensor(std::size_t basis, const Colors& colors) {
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
  default: throw std::invalid_argument("Color basis index is out of range.");
  }
}
} // namespace hepcat_generated
