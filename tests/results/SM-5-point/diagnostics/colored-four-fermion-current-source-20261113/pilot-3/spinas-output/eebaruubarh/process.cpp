#include "process.hpp"
#include <algorithm>
#include <cmath>
#include <stdexcept>

namespace hepcat_generated {
using namespace hepcat_spinas;

constexpr std::size_t draftcoloredcurrent3_color_1::leg_count;
constexpr std::size_t draftcoloredcurrent3_color_1::parameter_count;
constexpr std::size_t draftcoloredcurrent3_color_1::helicity_count;


draftcoloredcurrent3_color_1::draftcoloredcurrent3_color_1() : draftcoloredcurrent3_color_1(default_parameters()) {}

draftcoloredcurrent3_color_1::draftcoloredcurrent3_color_1(const Parameters& values) {
  set_parameters(values);
}

draftcoloredcurrent3_color_1::draftcoloredcurrent3_color_1(const ldouble& value_EE, const ldouble& value_MW,
  const ldouble& value_SW, const ldouble& value_Me, const ldouble& value_Mu, const ldouble& value_Mh)
  : draftcoloredcurrent3_color_1(Parameters{{cdouble(value_Me), cdouble(value_Mu), cdouble(value_Mh),
    cdouble(value_EE), cdouble(value_MW), cdouble(value_SW)}}) {}

// Copies rebuild caches, so every sproduct points into its own process.
draftcoloredcurrent3_color_1::draftcoloredcurrent3_color_1(const draftcoloredcurrent3_color_1& other) {
  widths_enabled = other.widths_enabled;
  set_parameters(other.parameters);
  if (other.momenta_ready) set_momenta(other.momenta);
}

draftcoloredcurrent3_color_1& draftcoloredcurrent3_color_1::operator=(const draftcoloredcurrent3_color_1& other) {
  if (this != &other) {
    widths_enabled = other.widths_enabled;
    set_parameters(other.parameters);
    if (other.momenta_ready) set_momenta(other.momenta);
  }
  return *this;
}

draftcoloredcurrent3_color_1::Parameters draftcoloredcurrent3_color_1::default_parameters() {
  return Parameters{{
    cdouble(ldouble(5.0e-4L), ldouble(0.0L)), // "Me"
    cdouble(ldouble(2.16e-3L), ldouble(0.0L)), // "Mu"
    cdouble(ldouble(1.25e2L), ldouble(0.0L)), // "Mh"
    cdouble(ldouble(3.1333e-1L), ldouble(0.0L)), // "EE"
    cdouble(ldouble(8.0385e1L), ldouble(0.0L)), // "MW"
    cdouble(ldouble(4.74e-1L), ldouble(0.0L)) // "SW"
  }};
}

draftcoloredcurrent3_color_1::Spins draftcoloredcurrent3_color_1::default_spins() {
  return Spins{{-1, -1, -1, -1, 0}};
}

std::array<std::size_t, 2> draftcoloredcurrent3_color_1::incoming_legs() {
  return {{0, 1}};
}

std::array<draftcoloredcurrent3_color_1::Spins, draftcoloredcurrent3_color_1::helicity_count>
draftcoloredcurrent3_color_1::helicity_configurations() {
  return std::array<Spins, helicity_count>{{
    {{0, 0, 0, 0, 0}}
  }};
}

ldouble draftcoloredcurrent3_color_1::incoming_spin_average() {
  return ldouble(4.0e0L);
}

ldouble draftcoloredcurrent3_color_1::symmetry_factor() {
  return ldouble(1.0e0L);
}

void draftcoloredcurrent3_color_1::initialize_products() {
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
  s211a = spinas::sproduct(::SQUARE, &p2, &p1, &p1);
  s251a = spinas::sproduct(::SQUARE, &p2, &p5, &p1);
  s221a = spinas::sproduct(::SQUARE, &p2, &p2, &p1);
  s223a = spinas::sproduct(::SQUARE, &p2, &p2, &p3);
  s253a = spinas::sproduct(::SQUARE, &p2, &p5, &p3);
  s233a = spinas::sproduct(::SQUARE, &p2, &p3, &p3);
  s433a = spinas::sproduct(::SQUARE, &p4, &p3, &p3);
  s443a = spinas::sproduct(::SQUARE, &p4, &p4, &p3);
  s411a = spinas::sproduct(::SQUARE, &p4, &p1, &p1);
  s451a = spinas::sproduct(::SQUARE, &p4, &p5, &p1);
  s441a = spinas::sproduct(::SQUARE, &p4, &p4, &p1);
  s453a = spinas::sproduct(::SQUARE, &p4, &p5, &p3);
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
  s2231s = spinas::sproduct(::SQUARE, &p2, &p2, &p3, &p1);
  s2241s = spinas::sproduct(::SQUARE, &p2, &p2, &p4, &p1);
  s2531s = spinas::sproduct(::SQUARE, &p2, &p5, &p3, &p1);
  s2541s = spinas::sproduct(::SQUARE, &p2, &p5, &p4, &p1);
}

void draftcoloredcurrent3_color_1::set_parameters(const Parameters& values) {
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
  Mu = parameters[1];
  Mh = parameters[2];
  EE = parameters[3];
  MW = parameters[4];
  SW = parameters[5];

  // Recompute model-only factors once after parameter changes.
  parameter_factor_1_2 = (EE * EE * EE);
  parameter_factor_2 = (Me * Me * Me);
  parameter_factor_1 = parameter_factor_1_2 * parameter_factor_2 * Mu;
  parameter_factor_4_2 = (MW * MW * MW);
  parameter_factor_5 = (SW * SW * SW);
  parameter_factor_4 = ldouble(8.0L) * parameter_factor_4_2 * parameter_factor_5;
  parameter_factor_7_2 = (Mh * Mh);
  parameter_factor_7 = ldouble(3.0L) * parameter_factor_1_2 * Me * parameter_factor_7_2 * Mu;
  parameter_factor_9_2 = (Mu * Mu * Mu);
  parameter_factor_9 = parameter_factor_1_2 * Me * parameter_factor_9_2;
  parameter_factor_11_2 = (Me * Me);
  parameter_factor_11 = ldouble(2.0L) * parameter_factor_1_2 * parameter_factor_11_2;
  parameter_factor_13 = ldouble(3.0L) * MW * SW;
  parameter_factor_14 = ldouble(2.0L) * parameter_factor_1_2 * parameter_factor_11_2 * SW;
  parameter_factor_15_3 = (SW * SW);
  parameter_factor_15_2 = (ldouble(1.0L) - parameter_factor_15_3);
  parameter_factor_15 = ldouble(3.0L) * MW * parameter_factor_15_2;
  parameter_factor_18_2 = std::pow(cdouble(parameter_factor_15_2), ldouble(1.0L / 2.0L));
  parameter_factor_18 = (MW) / (parameter_factor_18_2);
  parameter_factor_20_2 = (Mu * Mu);
  parameter_factor_20 = ldouble(2.0L) * parameter_factor_1_2 * parameter_factor_20_2;
  parameter_factor_22 = ldouble(4.0L) * parameter_factor_1_2 * MW * SW;
  parameter_factor_23_2 = std::pow(cdouble(parameter_factor_15_2), 2);
  parameter_factor_23 = ldouble(3.0L) * parameter_factor_23_2;
  parameter_factor_25 = ldouble(2.0L) * parameter_factor_1_2 * parameter_factor_20_2 * SW;
  parameter_factor_26_3 = ldouble(2.0L) * parameter_factor_15_3;
  parameter_factor_26_2 = (-ldouble(1.0L) + parameter_factor_26_3);
  parameter_factor_26 = parameter_factor_1_2 * parameter_factor_11_2 * parameter_factor_26_2;
  parameter_factor_29 = ldouble(3.0L) * MW * SW * parameter_factor_15_2;
  parameter_factor_30 = ldouble(2.0L) * parameter_factor_1_2 * MW * parameter_factor_26_2;
  parameter_factor_31 = ldouble(3.0L) * SW * parameter_factor_23_2;
  parameter_factor_32 = parameter_factor_1_2 * parameter_factor_20_2 * parameter_factor_26_2;
  parameter_factor_33_3 = (ldouble(4.0L) * parameter_factor_15_3) / (ldouble(3.0L));
  parameter_factor_33_2 = (ldouble(1.0L) - parameter_factor_33_3);
  parameter_factor_33 = parameter_factor_1_2 * parameter_factor_11_2 * parameter_factor_33_2 *
    parameter_factor_26_2;
  parameter_factor_36 = ldouble(4.0L) * MW * parameter_factor_5 * parameter_factor_15_2;
  parameter_factor_37 = parameter_factor_1_2 * MW * parameter_factor_33_2 * parameter_factor_26_2;
  parameter_factor_38 = ldouble(2.0L) * parameter_factor_5 * parameter_factor_23_2;
  parameter_factor_39 = parameter_factor_1_2 * parameter_factor_20_2 * parameter_factor_33_2 *
    parameter_factor_26_2;
  parameter_factor_40 = parameter_factor_1_2 * parameter_factor_11_2 * parameter_factor_33_2;
  parameter_factor_41 = ldouble(2.0L) * MW * SW * parameter_factor_15_2;
  parameter_factor_42 = parameter_factor_1_2 * MW * parameter_factor_33_2;
  parameter_factor_43 = SW * parameter_factor_23_2;
  parameter_factor_44 = parameter_factor_1_2 * parameter_factor_20_2 * parameter_factor_33_2;
  parameter_factor_45 = ldouble(2.0L) * parameter_factor_1_2 * Me;
  parameter_factor_46 = ldouble(2.0L) * parameter_factor_1_2 * Me * SW;
  parameter_factor_47 = ldouble(2.0L) * parameter_factor_1_2 * Mu;
  parameter_factor_48 = parameter_factor_1_2 * Mu * parameter_factor_33_2 * parameter_factor_26_2;
  parameter_factor_49 = parameter_factor_1_2 * parameter_factor_11_2 * Mu;
  parameter_factor_50 = parameter_factor_1_2 * Me * parameter_factor_26_2;
  parameter_factor_51 = parameter_factor_1_2 * Mu * parameter_factor_33_2;
  parameter_factor_52 = parameter_factor_1_2 * Me * parameter_factor_33_2 * parameter_factor_26_2;
  parameter_factor_53 = ldouble(2.0L) * parameter_factor_1_2 * Mu * SW;
  parameter_factor_54 = parameter_factor_1_2 * parameter_factor_11_2 * SW;
  parameter_factor_55 = ldouble(3.0L) * parameter_factor_4_2;
  parameter_factor_56 = ldouble(2.0L) * parameter_factor_1_2 * SW;
  parameter_factor_57 = parameter_factor_1_2 * Me * parameter_factor_20_2;
  parameter_factor_58 = parameter_factor_1_2 * parameter_factor_20_2 * SW;
  parameter_factor_59 = ldouble(6.0L) * parameter_factor_4_2 * SW;
  parameter_factor_60 = parameter_factor_1_2 * parameter_factor_26_2;
  parameter_factor_61 = parameter_factor_1_2 * Me * parameter_factor_33_2;
  parameter_factor_62 = parameter_factor_1_2 * Mu * parameter_factor_26_2;
  parameter_factor_63 = ldouble(3.0L) * parameter_factor_4_2 * SW;
  parameter_factor_64 = ldouble(4.0L) * parameter_factor_4_2 * SW;
  parameter_factor_65 = parameter_factor_1_2 * parameter_factor_33_2;
  parameter_factor_66 = ldouble(2.0L) * parameter_factor_4_2 * SW;
  parameter_factor_67 = parameter_factor_1_2 * parameter_factor_33_2 * parameter_factor_26_2;
  parameter_factor_68 = ldouble(4.0L) * parameter_factor_4_2 * parameter_factor_5;
  parameter_factor_69 = parameter_factor_1_2 * Me * SW;
  parameter_factor_70 = parameter_factor_1_2 * Mu * SW;
  external_masses[0] = checked_mass(cdouble(Me));
  external_masses[1] = checked_mass(cdouble(Me));
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

void draftcoloredcurrent3_color_1::set_widths_enabled(bool enabled) {
  if (widths_enabled == enabled) return;
  widths_enabled = enabled;
  momenta_ready = false;
  all_point_cache_ready = false;
  summed_point_cache_ready = false;
}

void draftcoloredcurrent3_color_1::set_masses(const ldouble& value_Me, const ldouble& value_Mu, const ldouble&
  value_Mh) {
  Parameters values = parameters;
  values[0] = cdouble(value_Me);
  values[1] = cdouble(value_Mu);
  values[2] = cdouble(value_Mh);
  set_parameters(values);
}

void draftcoloredcurrent3_color_1::set_momenta(const Momenta& values) {
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
  t34 = ((std::imag(Mh) == ldouble(0) && std::isfinite(std::real(Mh))) ? cdouble(mass2(sum_momenta(momenta, {3,
    4})) - (std::real(Mh)) * (std::real(Mh)), ldouble(0)) : (mass2(sum_momenta(momenta, {3, 4})) - (Mh) *
    (Mh)));
  t15 = ((std::imag(Me) == ldouble(0) && std::isfinite(std::real(Me))) ? cdouble(mass2(sum_momenta(momenta, {1,
    -5})) - (std::real(Me)) * (std::real(Me)), ldouble(0)) : (mass2(sum_momenta(momenta, {1, -5})) - (Me) *
    (Me)));
  t25 = ((std::imag(Me) == ldouble(0) && std::isfinite(std::real(Me))) ? cdouble(mass2(sum_momenta(momenta, {2,
    -5})) - (std::real(Me)) * (std::real(Me)), ldouble(0)) : (mass2(sum_momenta(momenta, {2, -5})) - (Me) *
    (Me)));
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
  t34_2 = mass2(sum_momenta(momenta, {3, 4})) - (parameter_factor_18) * (parameter_factor_18);
  s12 = mass2(sum_momenta(momenta, {1, 2})) - (ldouble(0.0L)) * (ldouble(0.0L));
  t12_2 = mass2(sum_momenta(momenta, {1, 2})) - (parameter_factor_18) * (parameter_factor_18);
  p1_dot_p3 = p1.dot(p3);
  p1_dot_p4 = p1.dot(p4);
  p2_dot_p3 = p2.dot(p3);
  p2_dot_p4 = p2.dot(p4);
  momenta_ready = true;
}

// Prepare only data used by the retained helicity contributions.
void draftcoloredcurrent3_color_1::prepare_summed_point_cache() {
  if (summed_point_cache_ready) return;
  a113s.update();
  a133s.update();
  a13a.update();
  a153s.update();
  a211s.update();
  a21a.update();
  a221s.update();
  a2231a.update();
  a223s.update();
  a2241a.update();
  a224s.update();
  a2311a.update();
  a231s.update();
  a233s.update();
  a2351a.update();
  a23a.update();
  a2411a.update();
  a241s.update();
  a244s.update();
  a2451a.update();
  a24a.update();
  a251s.update();
  a2531a.update();
  a253s.update();
  a2541a.update();
  a254s.update();
  a31a.update();
  a411s.update();
  a41a.update();
  a422s.update();
  a433s.update();
  a43a.update();
  a441s.update();
  a442s.update();
  a443s.update();
  a451s.update();
  a452s.update();
  a453s.update();
  s113a.update();
  s133a.update();
  s13s.update();
  s153a.update();
  s211a.update();
  s21s.update();
  s221a.update();
  s2231s.update();
  s223a.update();
  s2241s.update();
  s2311s.update();
  s231a.update();
  s233a.update();
  s2351s.update();
  s23s.update();
  s2411s.update();
  s241a.update();
  s2451s.update();
  s24s.update();
  s251a.update();
  s2531s.update();
  s253a.update();
  s2541s.update();
  s31s.update();
  s411a.update();
  s41s.update();
  s433a.update();
  s43s.update();
  s441a.update();
  s443a.update();
  s451a.update();
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
  point_product_43[0][0] = s211a.v(-1, -1);
  point_product_43[0][1] = s211a.v(-1, 1);
  point_product_43[1][0] = s211a.v(1, -1);
  point_product_43[1][1] = s211a.v(1, 1);
  point_product_44[0][0] = s251a.v(-1, -1);
  point_product_44[0][1] = s251a.v(-1, 1);
  point_product_44[1][0] = s251a.v(1, -1);
  point_product_44[1][1] = s251a.v(1, 1);
  point_product_45[0][0] = s221a.v(-1, -1);
  point_product_45[0][1] = s221a.v(-1, 1);
  point_product_45[1][0] = s221a.v(1, -1);
  point_product_45[1][1] = s221a.v(1, 1);
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
  point_product_49[0][0] = s433a.v(-1, -1);
  point_product_49[0][1] = s433a.v(-1, 1);
  point_product_49[1][0] = s433a.v(1, -1);
  point_product_49[1][1] = s433a.v(1, 1);
  point_product_50[0][0] = s443a.v(-1, -1);
  point_product_50[0][1] = s443a.v(-1, 1);
  point_product_50[1][0] = s443a.v(1, -1);
  point_product_50[1][1] = s443a.v(1, 1);
  point_product_51[0][0] = s411a.v(-1, -1);
  point_product_51[0][1] = s411a.v(-1, 1);
  point_product_51[1][0] = s411a.v(1, -1);
  point_product_51[1][1] = s411a.v(1, 1);
  point_product_52[0][0] = s451a.v(-1, -1);
  point_product_52[0][1] = s451a.v(-1, 1);
  point_product_52[1][0] = s451a.v(1, -1);
  point_product_52[1][1] = s451a.v(1, 1);
  point_product_53[0][0] = s441a.v(-1, -1);
  point_product_53[0][1] = s441a.v(-1, 1);
  point_product_53[1][0] = s441a.v(1, -1);
  point_product_53[1][1] = s441a.v(1, 1);
  point_product_54[0][0] = s453a.v(-1, -1);
  point_product_54[0][1] = s453a.v(-1, 1);
  point_product_54[1][0] = s453a.v(1, -1);
  point_product_54[1][1] = s453a.v(1, 1);
  point_product_55[0][0] = a2311a.v(-1, -1);
  point_product_55[0][1] = a2311a.v(-1, 1);
  point_product_55[1][0] = a2311a.v(1, -1);
  point_product_55[1][1] = a2311a.v(1, 1);
  point_product_56[0][0] = a2351a.v(-1, -1);
  point_product_56[0][1] = a2351a.v(-1, 1);
  point_product_56[1][0] = a2351a.v(1, -1);
  point_product_56[1][1] = a2351a.v(1, 1);
  point_product_57[0][0] = a2411a.v(-1, -1);
  point_product_57[0][1] = a2411a.v(-1, 1);
  point_product_57[1][0] = a2411a.v(1, -1);
  point_product_57[1][1] = a2411a.v(1, 1);
  point_product_58[0][0] = a2451a.v(-1, -1);
  point_product_58[0][1] = a2451a.v(-1, 1);
  point_product_58[1][0] = a2451a.v(1, -1);
  point_product_58[1][1] = a2451a.v(1, 1);
  point_product_59[0][0] = a2231a.v(-1, -1);
  point_product_59[0][1] = a2231a.v(-1, 1);
  point_product_59[1][0] = a2231a.v(1, -1);
  point_product_59[1][1] = a2231a.v(1, 1);
  point_product_60[0][0] = a2241a.v(-1, -1);
  point_product_60[0][1] = a2241a.v(-1, 1);
  point_product_60[1][0] = a2241a.v(1, -1);
  point_product_60[1][1] = a2241a.v(1, 1);
  point_product_61[0][0] = a2531a.v(-1, -1);
  point_product_61[0][1] = a2531a.v(-1, 1);
  point_product_61[1][0] = a2531a.v(1, -1);
  point_product_61[1][1] = a2531a.v(1, 1);
  point_product_62[0][0] = a2541a.v(-1, -1);
  point_product_62[0][1] = a2541a.v(-1, 1);
  point_product_62[1][0] = a2541a.v(1, -1);
  point_product_62[1][1] = a2541a.v(1, 1);
  point_product_63[0][0] = s2311s.v(-1, -1);
  point_product_63[0][1] = s2311s.v(-1, 1);
  point_product_63[1][0] = s2311s.v(1, -1);
  point_product_63[1][1] = s2311s.v(1, 1);
  point_product_64[0][0] = s2351s.v(-1, -1);
  point_product_64[0][1] = s2351s.v(-1, 1);
  point_product_64[1][0] = s2351s.v(1, -1);
  point_product_64[1][1] = s2351s.v(1, 1);
  point_product_65[0][0] = s2411s.v(-1, -1);
  point_product_65[0][1] = s2411s.v(-1, 1);
  point_product_65[1][0] = s2411s.v(1, -1);
  point_product_65[1][1] = s2411s.v(1, 1);
  point_product_66[0][0] = s2451s.v(-1, -1);
  point_product_66[0][1] = s2451s.v(-1, 1);
  point_product_66[1][0] = s2451s.v(1, -1);
  point_product_66[1][1] = s2451s.v(1, 1);
  point_product_67[0][0] = s2231s.v(-1, -1);
  point_product_67[0][1] = s2231s.v(-1, 1);
  point_product_67[1][0] = s2231s.v(1, -1);
  point_product_67[1][1] = s2231s.v(1, 1);
  point_product_68[0][0] = s2241s.v(-1, -1);
  point_product_68[0][1] = s2241s.v(-1, 1);
  point_product_68[1][0] = s2241s.v(1, -1);
  point_product_68[1][1] = s2241s.v(1, 1);
  point_product_69[0][0] = s2531s.v(-1, -1);
  point_product_69[0][1] = s2531s.v(-1, 1);
  point_product_69[1][0] = s2531s.v(1, -1);
  point_product_69[1][1] = s2531s.v(1, 1);
  point_product_70[0][0] = s2541s.v(-1, -1);
  point_product_70[0][1] = s2541s.v(-1, 1);
  point_product_70[1][0] = s2541s.v(1, -1);
  point_product_70[1][1] = s2541s.v(1, 1);
  point_factor_1 = (parameter_factor_4 * t34);
  point_factor_2 = (point_factor_1 * t15);
  point_factor_3 = (point_factor_1 * t25);
  point_factor_4 = (point_factor_1 * t12);
  point_factor_5 = (parameter_factor_4 * t35);
  point_factor_6 = (point_factor_5 * t12);
  point_factor_7 = (parameter_factor_4 * t45);
  point_factor_8 = (point_factor_7 * t12);
  point_factor_9 = (parameter_factor_13 * s34);
  point_factor_10 = (point_factor_9 * t15);
  point_factor_11 = (parameter_factor_15 * t34_2);
  point_factor_12 = (point_factor_11 * t15);
  point_factor_13 = (point_factor_9 * t25);
  point_factor_14 = (point_factor_11 * t25);
  point_factor_15 = ((parameter_factor_13 * t35) * s12);
  point_factor_16 = ((parameter_factor_13 * t45) * s12);
  point_factor_17 = ((parameter_factor_23 * t34_2) * t12_2);
  point_factor_18 = ((parameter_factor_15 * t35) * t12_2);
  point_factor_19 = ((parameter_factor_15 * t45) * t12_2);
  point_factor_20 = (parameter_factor_29 * t34_2);
  point_factor_21 = (point_factor_20 * t15);
  point_factor_22 = (point_factor_20 * t25);
  point_factor_23 = ((parameter_factor_31 * t34_2) * t12_2);
  point_factor_24 = ((parameter_factor_29 * t35) * t12_2);
  point_factor_25 = ((parameter_factor_29 * t45) * t12_2);
  point_factor_26 = (parameter_factor_36 * t34_2);
  point_factor_27 = (point_factor_26 * t15);
  point_factor_28 = (point_factor_26 * t25);
  point_factor_29 = ((parameter_factor_38 * t34_2) * t12_2);
  point_factor_30 = ((parameter_factor_36 * t35) * t12_2);
  point_factor_31 = ((parameter_factor_36 * t45) * t12_2);
  point_factor_32 = (parameter_factor_41 * t34_2);
  point_factor_33 = (point_factor_32 * t15);
  point_factor_34 = (point_factor_32 * t25);
  point_factor_35 = ((parameter_factor_43 * t34_2) * t12_2);
  point_factor_36 = ((parameter_factor_41 * t35) * t12_2);
  point_factor_37 = ((parameter_factor_41 * t45) * t12_2);
  point_factor_38 = (parameter_factor_55 * t34_2);
  point_factor_39 = (point_factor_38 * t15);
  point_factor_40 = (point_factor_38 * t25);
  point_factor_41 = (point_factor_11 * t12_2);
  point_factor_42 = (parameter_factor_56 * p1_dot_p3);
  point_factor_43 = (point_factor_38 * t12_2);
  point_factor_44 = (parameter_factor_56 * p1_dot_p4);
  point_factor_45 = (parameter_factor_56 * p2_dot_p3);
  point_factor_46 = (parameter_factor_56 * p2_dot_p4);
  point_factor_47 = ((parameter_factor_55 * t35) * t12_2);
  point_factor_48 = ((parameter_factor_55 * t45) * t12_2);
  point_factor_49 = (parameter_factor_59 * t34_2);
  point_factor_50 = (point_factor_49 * t15);
  point_factor_51 = (point_factor_49 * t25);
  point_factor_52 = (point_factor_20 * t12_2);
  point_factor_53 = (parameter_factor_60 * p1_dot_p3);
  point_factor_54 = ((parameter_factor_63 * t34_2) * t12_2);
  point_factor_55 = (parameter_factor_60 * p1_dot_p4);
  point_factor_56 = (parameter_factor_60 * p2_dot_p3);
  point_factor_57 = (parameter_factor_60 * p2_dot_p4);
  point_factor_58 = ((parameter_factor_59 * t35) * t12_2);
  point_factor_59 = ((parameter_factor_59 * t45) * t12_2);
  point_factor_60 = (parameter_factor_64 * t34_2);
  point_factor_61 = (point_factor_60 * t15);
  point_factor_62 = (point_factor_60 * t25);
  point_factor_63 = (point_factor_32 * t12_2);
  point_factor_64 = (parameter_factor_65 * p1_dot_p3);
  point_factor_65 = ((parameter_factor_66 * t34_2) * t12_2);
  point_factor_66 = (parameter_factor_65 * p1_dot_p4);
  point_factor_67 = (parameter_factor_65 * p2_dot_p3);
  point_factor_68 = (parameter_factor_65 * p2_dot_p4);
  point_factor_69 = (parameter_factor_4 * t34_2);
  point_factor_70 = (point_factor_69 * t15);
  point_factor_71 = (point_factor_69 * t25);
  point_factor_72 = (point_factor_26 * t12_2);
  point_factor_73 = (parameter_factor_67 * p1_dot_p3);
  point_factor_74 = ((parameter_factor_68 * t34_2) * t12_2);
  point_factor_75 = (parameter_factor_67 * p1_dot_p4);
  point_factor_76 = (parameter_factor_67 * p2_dot_p3);
  point_factor_77 = (parameter_factor_67 * p2_dot_p4);
  point_factor_78 = ((parameter_factor_64 * t35) * t12_2);
  point_factor_79 = ((parameter_factor_64 * t45) * t12_2);
  point_factor_80 = (point_factor_5 * t12_2);
  point_factor_81 = (point_factor_7 * t12_2);
  point_denominator_1 = hepcat_spinas::CachedDenominator(point_factor_2, real_denominator_domain);
  point_denominator_2 = hepcat_spinas::CachedDenominator(point_factor_3, real_denominator_domain);
  point_denominator_3 = hepcat_spinas::CachedDenominator(point_factor_4, real_denominator_domain);
  point_denominator_4 = hepcat_spinas::CachedDenominator(point_factor_6, real_denominator_domain);
  point_denominator_5 = hepcat_spinas::CachedDenominator(point_factor_8, real_denominator_domain);
  point_denominator_6 = hepcat_spinas::CachedDenominator(point_factor_10, real_denominator_domain);
  point_denominator_7 = hepcat_spinas::CachedDenominator(point_factor_13, real_denominator_domain);
  point_denominator_8 = hepcat_spinas::CachedDenominator(point_factor_15, real_denominator_domain);
  point_denominator_9 = hepcat_spinas::CachedDenominator(point_factor_16, real_denominator_domain);
  summed_point_cache_ready = true;
}

// Individual amplitudes can lazily request the remaining helicities.
void draftcoloredcurrent3_color_1::prepare_all_point_cache() {
  if (all_point_cache_ready) return;
  prepare_summed_point_cache();
  all_point_cache_ready = true;
}

void draftcoloredcurrent3_color_1::set_momenta(const ldouble mom1[4], const ldouble mom2[4], const ldouble
  mom3[4], const ldouble mom4[4], const ldouble mom5[4]) {
  Momenta values;
  std::copy(mom1, mom1 + 4, values[0].begin());
  std::copy(mom2, mom2 + 4, values[1].begin());
  std::copy(mom3, mom3 + 4, values[2].begin());
  std::copy(mom4, mom4 + 4, values[3].begin());
  std::copy(mom5, mom5 + 4, values[4].begin());
  set_momenta(values);
}

cdouble draftcoloredcurrent3_color_1::amp(const Spins& spins) {
  return amp(spins[0], spins[1], spins[2], spins[3], spins[4]);
}

// set_momenta(...) must be called before evaluating an amplitude.
cdouble draftcoloredcurrent3_color_1::amp(const int& ds1, const int& ds2, const int& ds3, const int& ds4, const
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
cdouble draftcoloredcurrent3_color_1::amp_unchecked(const int& ds1, const int& ds2, const int& ds3, const int&
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
  const cdouble spin_factor_4 = (parameter_factor_9 * point_product_1[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_5 = (spin_factor_4 * point_product_2[(ds4) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_6 = ((parameter_factor_11 * point_product_3[(ds2) > 0 ? 1 : 0][(ds4) > 0 ? 1 : 0]) *
    point_product_4[(ds1) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_7 = ((parameter_factor_14 * point_product_3[(ds2) > 0 ? 1 : 0][(ds4) > 0 ? 1 : 0]) *
    point_product_4[(ds1) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_8 = ((parameter_factor_20 * point_product_3[(ds2) > 0 ? 1 : 0][(ds4) > 0 ? 1 : 0]) *
    point_product_4[(ds1) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_9 = ((parameter_factor_25 * point_product_3[(ds2) > 0 ? 1 : 0][(ds4) > 0 ? 1 : 0]) *
    point_product_4[(ds1) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_10 = ((parameter_factor_1 * point_product_2[(ds4) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]) *
    point_product_5[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_11 = ((parameter_factor_9 * point_product_2[(ds4) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]) *
    point_product_5[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_12 = ((parameter_factor_11 * point_product_6[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0])
    * point_product_7[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_13 = ((parameter_factor_26 * point_product_6[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0])
    * point_product_7[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_14 = ((parameter_factor_20 * point_product_6[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0])
    * point_product_7[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_15 = ((parameter_factor_32 * point_product_6[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0])
    * point_product_7[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_16 = ((parameter_factor_11 * point_product_8[(ds1) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0])
    * point_product_9[(ds2) > 0 ? 1 : 0][(ds4) > 0 ? 1 : 0]);
  const cdouble spin_factor_17 = ((parameter_factor_33 * point_product_8[(ds1) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0])
    * point_product_9[(ds2) > 0 ? 1 : 0][(ds4) > 0 ? 1 : 0]);
  const cdouble spin_factor_18 = ((parameter_factor_20 * point_product_8[(ds1) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0])
    * point_product_9[(ds2) > 0 ? 1 : 0][(ds4) > 0 ? 1 : 0]);
  const cdouble spin_factor_19 = ((parameter_factor_39 * point_product_8[(ds1) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0])
    * point_product_9[(ds2) > 0 ? 1 : 0][(ds4) > 0 ? 1 : 0]);
  const cdouble spin_factor_20 = ((parameter_factor_11 * point_product_10[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0])
    * point_product_11[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_21 = ((parameter_factor_40 * point_product_10[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0])
    * point_product_11[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_22 = ((parameter_factor_20 * point_product_10[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0])
    * point_product_11[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_23 = ((parameter_factor_44 * point_product_10[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0])
    * point_product_11[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_24 = (spin_factor_1 * point_product_12[(ds4) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_25 = (spin_factor_4 * point_product_12[(ds4) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_26 = ((parameter_factor_1 * point_product_5[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]) *
    point_product_12[(ds4) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_27 = ((parameter_factor_9 * point_product_5[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]) *
    point_product_12[(ds4) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_28 = (point_product_13[(ds1) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0] -
    point_product_14[(ds1) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_29 = ((-point_product_15[(ds1) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]) -
    point_product_14[(ds1) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_30 = (parameter_factor_49 * point_product_2[(ds4) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_31 = (point_product_16[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0] -
    point_product_17[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_32 = (parameter_factor_49 * point_product_12[(ds4) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_33 = (point_product_18[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0] -
    point_product_17[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_34 = (point_product_19[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0] -
    point_product_20[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_35 = ((-point_product_21[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]) -
    point_product_20[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_36 = (point_product_22[(ds2) > 0 ? 1 : 0][(ds4) > 0 ? 1 : 0] -
    point_product_23[(ds2) > 0 ? 1 : 0][(ds4) > 0 ? 1 : 0]);
  const cdouble spin_factor_37 = ((-point_product_24[(ds2) > 0 ? 1 : 0][(ds4) > 0 ? 1 : 0]) -
    point_product_23[(ds2) > 0 ? 1 : 0][(ds4) > 0 ? 1 : 0]);
  const cdouble spin_factor_38 = ((-point_product_25[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]) -
    point_product_26[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_39 = (point_product_27[(ds4) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0] +
    point_product_28[(ds4) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_40 = ((parameter_factor_54 * spin_factor_38) * spin_factor_39);
  const cdouble spin_factor_41 = (point_product_29[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0] -
    point_product_30[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_42 = (point_product_31[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0] +
    point_product_30[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_43 = (point_product_33[(ds4) > 0 ? 1 : 0][(ds2) > 0 ? 1 : 0] -
    point_product_34[(ds4) > 0 ? 1 : 0][(ds2) > 0 ? 1 : 0]);
  const cdouble spin_factor_44 = (point_product_36[(ds4) > 0 ? 1 : 0][(ds2) > 0 ? 1 : 0] +
    point_product_34[(ds4) > 0 ? 1 : 0][(ds2) > 0 ? 1 : 0]);
  const cdouble spin_factor_45 = (parameter_factor_57 * point_product_1[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_46 = (point_product_27[(ds4) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0] +
    point_product_37[(ds4) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_47 = (parameter_factor_57 * point_product_5[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_48 = (point_product_28[(ds4) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0] +
    point_product_37[(ds4) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_49 = ((parameter_factor_58 * a2_P_1s_direct) * a4_P_3s_direct);
  const cdouble spin_factor_50 = (point_product_38[(ds1) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0] -
    point_product_39[(ds1) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_51 = (point_product_40[(ds1) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0] +
    point_product_39[(ds1) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_52 = ((-point_product_41[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]) -
    point_product_42[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_53 = ((parameter_factor_26 * spin_factor_39) * spin_factor_52);
  const cdouble spin_factor_54 = (point_product_43[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0] -
    point_product_44[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_55 = (point_product_45[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0] -
    point_product_44[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_56 = (point_product_46[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0] -
    point_product_47[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_57 = (point_product_48[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0] +
    point_product_47[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_58 = ((parameter_factor_32 * a4_P_3s_direct) * s2_P_1a_direct);
  const cdouble spin_factor_59 = (point_product_49[(ds4) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0] +
    point_product_50[(ds4) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_60 = ((parameter_factor_40 * spin_factor_38) * spin_factor_59);
  const cdouble spin_factor_61 = ((parameter_factor_33 * spin_factor_52) * spin_factor_59);
  const cdouble spin_factor_62 = (point_product_51[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0] -
    point_product_52[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_63 = ((-point_product_53[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]) -
    point_product_52[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_64 = ((-point_product_50[(ds4) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]) -
    point_product_54[(ds4) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_65 = (point_product_49[(ds4) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0] +
    point_product_54[(ds4) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]);
  const cdouble spin_factor_66 = ((parameter_factor_44 * a2_P_1s_direct) * s4_P_3a_direct);
  const cdouble spin_factor_67 = ((parameter_factor_39 * s2_P_1a_direct) * s4_P_3a_direct);
  const cdouble spin_factor_68 = (parameter_factor_69 * spin_factor_39);
  const cdouble spin_factor_69 = ((((-point_product_55[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]) +
    point_product_56[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]) - point_product_57[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1
    : 0]) + point_product_58[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_70 = (parameter_factor_61 * spin_factor_59);
  const cdouble spin_factor_71 = (parameter_factor_50 * spin_factor_39);
  const cdouble spin_factor_72 = ((((-point_product_59[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]) -
    point_product_60[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]) + point_product_61[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1
    : 0]) + point_product_62[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_73 = (parameter_factor_52 * spin_factor_59);
  const cdouble spin_factor_74 = (parameter_factor_51 * a2_P_1s_direct);
  const cdouble spin_factor_75 = (a4_4_P_3a_direct + a4_5_P_3a_direct);
  const cdouble spin_factor_76 = (parameter_factor_48 * s2_P_1a_direct);
  const cdouble spin_factor_77 = (parameter_factor_70 * a2_P_1s_direct);
  const cdouble spin_factor_78 = ((-a4_P_3_3a_direct) - a4_P_5_3a_direct);
  const cdouble spin_factor_79 = (parameter_factor_62 * s2_P_1a_direct);
  const cdouble spin_factor_80 = ((((-point_product_63[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]) +
    point_product_64[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]) - point_product_65[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1
    : 0]) + point_product_66[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_81 = ((((-point_product_67[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]) -
    point_product_68[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]) + point_product_69[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1
    : 0]) + point_product_70[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]);
  const cdouble spin_factor_82 = ((-s4_4_P_3s_direct) - s4_5_P_3s_direct);
  const cdouble spin_factor_83 = ((-s4_P_3_3s_direct) - s4_P_5_3s_direct);
  return (-point_denominator_1.divide(spin_factor_2) - point_denominator_2.divide(spin_factor_2) -
    point_denominator_3.divide((spin_factor_3 * point_product_2[(ds4) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0])) -
    point_denominator_4.divide(spin_factor_5) - point_denominator_5.divide(spin_factor_5) -
    point_denominator_6.divide(spin_factor_6) - (spin_factor_7 / point_factor_12) -
    point_denominator_7.divide(spin_factor_6) - (spin_factor_7 / point_factor_14) -
    point_denominator_8.divide(spin_factor_8) - point_denominator_9.divide(spin_factor_8) -
    (((parameter_factor_22 * point_product_3[(ds2) > 0 ? 1 : 0][(ds4) > 0 ? 1 : 0]) * point_product_4[(ds1) > 0
    ? 1 : 0][(ds3) > 0 ? 1 : 0]) / point_factor_17) - (spin_factor_9 / point_factor_18) - (spin_factor_9 /
    point_factor_19) - point_denominator_1.divide(spin_factor_10) - point_denominator_2.divide(spin_factor_10) -
    point_denominator_3.divide(((parameter_factor_7 * point_product_2[(ds4) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]) *
    point_product_5[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0])) - point_denominator_4.divide(spin_factor_11) -
    point_denominator_5.divide(spin_factor_11) + point_denominator_6.divide(spin_factor_12) + (spin_factor_13 /
    point_factor_21) + point_denominator_7.divide(spin_factor_12) + (spin_factor_13 / point_factor_22) +
    point_denominator_8.divide(spin_factor_14) + point_denominator_9.divide(spin_factor_14) +
    (((parameter_factor_30 * point_product_6[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]) * point_product_7[(ds2) > 0
    ? 1 : 0][(ds3) > 0 ? 1 : 0]) / point_factor_23) + (spin_factor_15 / point_factor_24) + (spin_factor_15 /
    point_factor_25) - point_denominator_6.divide(spin_factor_16) + (spin_factor_17 / point_factor_27) -
    point_denominator_7.divide(spin_factor_16) + (spin_factor_17 / point_factor_28) -
    point_denominator_8.divide(spin_factor_18) - point_denominator_9.divide(spin_factor_18) +
    (((parameter_factor_37 * point_product_8[(ds1) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]) * point_product_9[(ds2) > 0
    ? 1 : 0][(ds4) > 0 ? 1 : 0]) / point_factor_29) + (spin_factor_19 / point_factor_30) + (spin_factor_19 /
    point_factor_31) + point_denominator_6.divide(spin_factor_20) - (spin_factor_21 / point_factor_33) +
    point_denominator_7.divide(spin_factor_20) - (spin_factor_21 / point_factor_34) +
    point_denominator_8.divide(spin_factor_22) + point_denominator_9.divide(spin_factor_22) -
    (((parameter_factor_42 * point_product_10[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]) * point_product_11[(ds4) >
    0 ? 1 : 0][(ds1) > 0 ? 1 : 0]) / point_factor_35) - (spin_factor_23 / point_factor_36) - (spin_factor_23 /
    point_factor_37) - point_denominator_1.divide(spin_factor_24) - point_denominator_2.divide(spin_factor_24) -
    point_denominator_3.divide((spin_factor_3 * point_product_12[(ds4) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0])) -
    point_denominator_4.divide(spin_factor_25) - point_denominator_5.divide(spin_factor_25) -
    point_denominator_1.divide(spin_factor_26) - point_denominator_2.divide(spin_factor_26) -
    point_denominator_3.divide(((parameter_factor_7 * point_product_5[(ds2) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]) *
    point_product_12[(ds4) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0])) - point_denominator_4.divide(spin_factor_27) -
    point_denominator_5.divide(spin_factor_27) - point_denominator_6.divide(((parameter_factor_45 *
    point_product_3[(ds2) > 0 ? 1 : 0][(ds4) > 0 ? 1 : 0]) * spin_factor_28)) - (((parameter_factor_46 *
    point_product_3[(ds2) > 0 ? 1 : 0][(ds4) > 0 ? 1 : 0]) * spin_factor_28) / point_factor_12) -
    point_denominator_8.divide(((parameter_factor_47 * point_product_9[(ds2) > 0 ? 1 : 0][(ds4) > 0 ? 1 : 0]) *
    spin_factor_29)) + (((parameter_factor_48 * point_product_9[(ds2) > 0 ? 1 : 0][(ds4) > 0 ? 1 : 0]) *
    spin_factor_29) / point_factor_30) + point_denominator_1.divide((spin_factor_30 * spin_factor_31)) +
    point_denominator_1.divide((spin_factor_32 * spin_factor_31)) - point_denominator_2.divide((spin_factor_30 *
    spin_factor_33)) - point_denominator_2.divide((spin_factor_32 * spin_factor_33)) +
    point_denominator_7.divide(((parameter_factor_45 * point_product_6[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]) *
    spin_factor_34)) + (((parameter_factor_50 * point_product_6[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]) *
    spin_factor_34) / point_factor_22) + point_denominator_8.divide(((parameter_factor_47 *
    point_product_11[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]) * spin_factor_35)) - (((parameter_factor_51 *
    point_product_11[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]) * spin_factor_35) / point_factor_36) -
    point_denominator_7.divide(((parameter_factor_45 * point_product_8[(ds1) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]) *
    spin_factor_36)) + (((parameter_factor_52 * point_product_8[(ds1) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]) *
    spin_factor_36) / point_factor_28) - point_denominator_9.divide(((parameter_factor_47 *
    point_product_4[(ds1) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]) * spin_factor_37)) - (((parameter_factor_53 *
    point_product_4[(ds1) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]) * spin_factor_37) / point_factor_19) +
    (spin_factor_40 / point_factor_39) + (spin_factor_40 / point_factor_40) + (((parameter_factor_56 *
    spin_factor_38) * spin_factor_39) / point_factor_41) + (((point_factor_42 * a2_P_1s_direct) *
    spin_factor_39) / point_factor_43) + (((point_factor_44 * a2_P_1s_direct) * spin_factor_39) /
    point_factor_43) + (((point_factor_45 * a2_P_1s_direct) * spin_factor_39) / point_factor_43) +
    (((point_factor_46 * a2_P_1s_direct) * spin_factor_39) / point_factor_43) -
    point_denominator_6.divide(((parameter_factor_45 * point_product_7[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]) *
    spin_factor_41)) - (((parameter_factor_50 * point_product_7[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]) *
    spin_factor_41) / point_factor_21) + point_denominator_9.divide(((parameter_factor_47 *
    point_product_10[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]) * spin_factor_42)) - (((parameter_factor_51 *
    point_product_10[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]) * spin_factor_42) / point_factor_37) +
    point_denominator_7.divide(((parameter_factor_45 * point_product_32[(ds3) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]) *
    spin_factor_43)) + (((parameter_factor_46 * point_product_32[(ds3) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]) *
    spin_factor_43) / point_factor_14) - point_denominator_9.divide(((parameter_factor_47 *
    point_product_35[(ds3) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]) * spin_factor_44)) + (((parameter_factor_48 *
    point_product_35[(ds3) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]) * spin_factor_44) / point_factor_31) +
    point_denominator_4.divide((spin_factor_45 * spin_factor_46)) + point_denominator_4.divide((spin_factor_47 *
    spin_factor_46)) - point_denominator_5.divide((spin_factor_45 * spin_factor_48)) -
    point_denominator_5.divide((spin_factor_47 * spin_factor_48)) - (((parameter_factor_56 * a2_P_1s_direct) *
    a4_P_3s_direct) / point_factor_41) - (spin_factor_49 / point_factor_47) - (spin_factor_49 / point_factor_48)
    - point_denominator_6.divide(((parameter_factor_45 * point_product_9[(ds2) > 0 ? 1 : 0][(ds4) > 0 ? 1 : 0])
    * spin_factor_50)) + (((parameter_factor_52 * point_product_9[(ds2) > 0 ? 1 : 0][(ds4) > 0 ? 1 : 0]) *
    spin_factor_50) / point_factor_27) + point_denominator_8.divide(((parameter_factor_47 *
    point_product_3[(ds2) > 0 ? 1 : 0][(ds4) > 0 ? 1 : 0]) * spin_factor_51)) + (((parameter_factor_53 *
    point_product_3[(ds2) > 0 ? 1 : 0][(ds4) > 0 ? 1 : 0]) * spin_factor_51) / point_factor_18) +
    (spin_factor_53 / point_factor_50) + (spin_factor_53 / point_factor_51) + (((parameter_factor_60 *
    spin_factor_39) * spin_factor_52) / point_factor_52) + point_denominator_1.divide((spin_factor_30 *
    spin_factor_54)) + point_denominator_1.divide((spin_factor_32 * spin_factor_54)) -
    point_denominator_2.divide((spin_factor_30 * spin_factor_55)) - point_denominator_2.divide((spin_factor_32 *
    spin_factor_55)) + point_denominator_7.divide(((parameter_factor_45 * point_product_11[(ds4) > 0 ? 1 :
    0][(ds1) > 0 ? 1 : 0]) * spin_factor_56)) - (((parameter_factor_61 * point_product_11[(ds4) > 0 ? 1 :
    0][(ds1) > 0 ? 1 : 0]) * spin_factor_56) / point_factor_34) -
    point_denominator_8.divide(((parameter_factor_47 * point_product_6[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]) *
    spin_factor_57)) - (((parameter_factor_62 * point_product_6[(ds4) > 0 ? 1 : 0][(ds1) > 0 ? 1 : 0]) *
    spin_factor_57) / point_factor_24) + (((point_factor_53 * spin_factor_39) * s2_P_1a_direct) /
    point_factor_54) + (((point_factor_55 * spin_factor_39) * s2_P_1a_direct) / point_factor_54) +
    (((point_factor_56 * spin_factor_39) * s2_P_1a_direct) / point_factor_54) + (((point_factor_57 *
    spin_factor_39) * s2_P_1a_direct) / point_factor_54) - (((parameter_factor_60 * a4_P_3s_direct) *
    s2_P_1a_direct) / point_factor_52) - (spin_factor_58 / point_factor_58) - (spin_factor_58 / point_factor_59)
    - (spin_factor_60 / point_factor_61) - (spin_factor_60 / point_factor_62) - (((parameter_factor_65 *
    spin_factor_38) * spin_factor_59) / point_factor_63) - (((point_factor_64 * a2_P_1s_direct) *
    spin_factor_59) / point_factor_65) - (((point_factor_66 * a2_P_1s_direct) * spin_factor_59) /
    point_factor_65) - (((point_factor_67 * a2_P_1s_direct) * spin_factor_59) / point_factor_65) -
    (((point_factor_68 * a2_P_1s_direct) * spin_factor_59) / point_factor_65) - (spin_factor_61 /
    point_factor_70) - (spin_factor_61 / point_factor_71) - (((parameter_factor_67 * spin_factor_52) *
    spin_factor_59) / point_factor_72) - (((point_factor_73 * s2_P_1a_direct) * spin_factor_59) /
    point_factor_74) - (((point_factor_75 * s2_P_1a_direct) * spin_factor_59) / point_factor_74) -
    (((point_factor_76 * s2_P_1a_direct) * spin_factor_59) / point_factor_74) - (((point_factor_77 *
    s2_P_1a_direct) * spin_factor_59) / point_factor_74) - point_denominator_6.divide(((parameter_factor_45 *
    point_product_10[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]) * spin_factor_62)) + (((parameter_factor_61 *
    point_product_10[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]) * spin_factor_62) / point_factor_33) -
    point_denominator_9.divide(((parameter_factor_47 * point_product_7[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]) *
    spin_factor_63)) - (((parameter_factor_62 * point_product_7[(ds2) > 0 ? 1 : 0][(ds3) > 0 ? 1 : 0]) *
    spin_factor_63) / point_factor_25) + point_denominator_5.divide((spin_factor_45 * spin_factor_64)) +
    point_denominator_5.divide((spin_factor_47 * spin_factor_64)) + point_denominator_4.divide((spin_factor_45 *
    spin_factor_65)) + point_denominator_4.divide((spin_factor_47 * spin_factor_65)) + (((parameter_factor_65 *
    a2_P_1s_direct) * s4_P_3a_direct) / point_factor_63) + (spin_factor_66 / point_factor_78) + (spin_factor_66
    / point_factor_79) + (((parameter_factor_67 * s2_P_1a_direct) * s4_P_3a_direct) / point_factor_72) +
    (spin_factor_67 / point_factor_80) + (spin_factor_67 / point_factor_81) - ((spin_factor_68 * spin_factor_69)
    / point_factor_39) + ((spin_factor_70 * spin_factor_69) / point_factor_61) + ((spin_factor_71 *
    spin_factor_72) / point_factor_51) - ((spin_factor_73 * spin_factor_72) / point_factor_71) +
    ((spin_factor_74 * spin_factor_75) / point_factor_79) + ((spin_factor_76 * spin_factor_75) /
    point_factor_81) - ((spin_factor_77 * spin_factor_78) / point_factor_47) - ((spin_factor_79 *
    spin_factor_78) / point_factor_58) - ((spin_factor_71 * spin_factor_80) / point_factor_50) +
    ((spin_factor_73 * spin_factor_80) / point_factor_70) + ((spin_factor_68 * spin_factor_81) /
    point_factor_40) - ((spin_factor_70 * spin_factor_81) / point_factor_62) + ((spin_factor_77 *
    spin_factor_82) / point_factor_48) + ((spin_factor_79 * spin_factor_82) / point_factor_59) +
    ((spin_factor_74 * spin_factor_83) / point_factor_78) + ((spin_factor_76 * spin_factor_83) /
    point_factor_80));
}

// Explicit spin and helicity sums.
ldouble draftcoloredcurrent3_color_1::spin_sum() {
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

ldouble draftcoloredcurrent3_color_1::amp2() {
  // Initial-state average: 1/4.
  // Final-state identical-particle factor: 1.
  return spin_sum() / ldouble(4.0L);
}

} // namespace hepcat_generated

namespace hepcat_generated {
using namespace hepcat_spinas;
constexpr std::size_t draftcoloredcurrent3::leg_count;
constexpr std::size_t draftcoloredcurrent3::parameter_count;
constexpr std::size_t draftcoloredcurrent3::helicity_count;
constexpr std::size_t draftcoloredcurrent3::color_basis_count;
draftcoloredcurrent3::Parameters draftcoloredcurrent3::default_parameters() { return Parameters{{
  cdouble(ldouble(5.0e-4L), ldouble(0.0L)), // Me
  cdouble(ldouble(2.16e-3L), ldouble(0.0L)), // Mu
  cdouble(ldouble(1.25e2L), ldouble(0.0L)), // Mh
  cdouble(ldouble(3.1333e-1L), ldouble(0.0L)), // EE
  cdouble(ldouble(8.0385e1L), ldouble(0.0L)), // MW
  cdouble(ldouble(4.74e-1L), ldouble(0.0L)) // SW
}}; }
void draftcoloredcurrent3::set_parameters(const Parameters& values) {
  component_1.set_parameters(draftcoloredcurrent3_color_1::Parameters{{values[0], values[1], values[2], values[3], values[4], values[5]}});
  parameters = values;
}
void draftcoloredcurrent3::set_momenta(const Momenta& momenta) {
  component_1.set_momenta(momenta);
}
void draftcoloredcurrent3::set_widths_enabled(bool enabled) {
  component_1.set_widths_enabled(enabled);
}
draftcoloredcurrent3::ColorAmplitudes draftcoloredcurrent3::color_amplitudes(const Spins& spins) {
  return ColorAmplitudes{{component_1.amp(spins)}};
}
ldouble draftcoloredcurrent3::color_spin_sum(const Spins& spins) {
  const auto a = color_amplitudes(spins);
  cdouble value(0,0);
  ldouble scale = 0;
  { const cdouble term = std::conj(a[0]) * cdouble(ldouble(3.0e0L), ldouble(0.0L)) * a[0];
    value += term; scale += std::abs(term); }
  const ldouble tolerance = 256 * std::numeric_limits<ldouble>::epsilon() * scale;
  if (std::abs(value.imag()) > tolerance || value.real() < -tolerance)
    throw std::runtime_error("The coherent color sum is not real and nonnegative.");
  return std::max(ldouble(0), value.real());
}
ldouble draftcoloredcurrent3::spin_sum() {
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
ldouble draftcoloredcurrent3::amp2() { return spin_sum() * symmetry_factor() / (incoming_spin_average() * incoming_color_average()); }
cdouble draftcoloredcurrent3::amp(const Spins& spins, const Colors& colors) {
  const auto a = color_amplitudes(spins); cdouble result(0,0);
  for (std::size_t i=0;i<color_basis_count;++i) result += a[i] * color_tensor(i,colors);
  return result;
}
cdouble draftcoloredcurrent3::color_tensor(std::size_t basis, const Colors& colors) {
  static const std::array<int,leg_count> dimensions = {{1, 1, 3, 3, 1}};
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
    default: return cdouble(0,0); }
  default: throw std::invalid_argument("Color basis index is out of range.");
  }
}
} // namespace hepcat_generated
