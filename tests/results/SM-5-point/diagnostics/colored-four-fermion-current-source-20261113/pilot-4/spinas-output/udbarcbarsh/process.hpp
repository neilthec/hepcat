#pragma once
#include <array>
#include <cstddef>
#include "spinas.h"
#include "spinas-runtime.hpp"
#include "spinas-validation.hpp"
#include "madgraph-runtime.hpp"

namespace hepcat_generated {

class draftcoloredcurrent4_color_1 {
public:
  static constexpr std::size_t leg_count = 5;
  static constexpr std::size_t parameter_count = 8;
  static constexpr std::size_t helicity_count = 1;
  using Parameters = std::array<cdouble, parameter_count>;
  using Momenta = hepcat_spinas::Momenta<leg_count>;
  using Spins = std::array<int, leg_count>;

  draftcoloredcurrent4_color_1();
  explicit draftcoloredcurrent4_color_1(const Parameters& values);
  draftcoloredcurrent4_color_1(const ldouble& value_EE, const ldouble& value_MW, const ldouble& value_SW, const
    ldouble& value_Mu, const ldouble& value_Md, const ldouble& value_Mc, const ldouble& value_Ms, const ldouble&
    value_Mh);
  draftcoloredcurrent4_color_1(const draftcoloredcurrent4_color_1& other);
  draftcoloredcurrent4_color_1& operator=(const draftcoloredcurrent4_color_1& other);

  static Parameters default_parameters();
  static Spins default_spins();
  static std::array<std::size_t, 2> incoming_legs();
  static std::array<Spins, helicity_count> helicity_configurations();
  static ldouble incoming_spin_average();
  static ldouble symmetry_factor();

  const Parameters& get_parameters() const { return parameters; }
  const std::array<ldouble, leg_count>& masses() const { return external_masses; }

  void set_parameters(const Parameters& values);
  // Disabling widths changes propagators only; parameters remain unchanged.
  void set_widths_enabled(bool enabled);
  void set_masses(const ldouble& value_Mu, const ldouble& value_Md, const ldouble& value_Mc, const ldouble&
    value_Ms, const ldouble& value_Mh);
  void set_momenta(const Momenta& values);
  void set_momenta(const ldouble mom1[4], const ldouble mom2[4], const ldouble mom3[4], const ldouble mom4[4],
    const ldouble mom5[4]);

  // Complex amplitude for explicit doubled spins and helicities.
  cdouble amp(const int& ds1, const int& ds2, const int& ds3, const int& ds4, const int& ds5);
  cdouble amp(const Spins& spins);
  ldouble spin_sum();
  ldouble amp2();

  hepcat_spinas::ValidationReport validate_calchep(
    const hepcat_spinas::ValidationOptions& options = hepcat_spinas::ValidationOptions());

  // Independent MadGraph comparison, resolved by the validation library.
  hepcat_spinas::MadGraphValidationReport validate_madgraph(
    const hepcat_spinas::MadGraphValidationOptions& options = hepcat_spinas::MadGraphValidationOptions());

private:
  Parameters parameters;
  std::array<ldouble, leg_count> external_masses;
  Momenta momenta;
  bool momenta_ready = false;
  bool widths_enabled = true;

  // Model parameters.
  cdouble Mu;
  cdouble Md;
  cdouble Mc;
  cdouble Ms;
  cdouble Mh;
  cdouble EE;
  cdouble MW;
  cdouble SW;

  // Constants and model-only factors.
  cdouble parameter_factor_1_2;
  cdouble parameter_factor_2;
  cdouble parameter_factor_1;
  cdouble parameter_factor_4_2;
  cdouble parameter_factor_4;
  cdouble parameter_factor_6_2;
  cdouble parameter_factor_6;
  cdouble parameter_factor_8;
  cdouble parameter_factor_9_2;
  cdouble parameter_factor_9;
  cdouble parameter_factor_11_2;
  cdouble parameter_factor_11;
  cdouble parameter_factor_13_2;
  cdouble parameter_factor_13;
  cdouble parameter_factor_15;
  cdouble parameter_factor_16;
  cdouble parameter_factor_17;
  cdouble parameter_factor_18;
  cdouble parameter_factor_19;
  cdouble parameter_factor_20;
  cdouble parameter_factor_21;
  cdouble parameter_factor_22;

  // External particles.
  spinas::particle p1;
  spinas::particle p2;
  spinas::particle p3;
  spinas::particle p4;
  spinas::particle p5;

  // Spinor-product and scalar caches. Prepared once per momentum point.
  spinas::sproduct a14a;
  spinas::sproduct s23s;
  spinas::sproduct a12a;
  spinas::sproduct s12s;
  spinas::sproduct a34a;
  spinas::sproduct s34s;
  spinas::sproduct s152a;
  spinas::sproduct s154a;
  spinas::sproduct s251a;
  spinas::sproduct s253a;
  spinas::sproduct s352a;
  spinas::sproduct s354a;
  spinas::sproduct s451a;
  spinas::sproduct s453a;
  cdouble t34;
  cdouble t15;
  cdouble t25;
  cdouble t12;
  cdouble t35;
  cdouble t45;
  ldouble p1_dot_p3;
  ldouble p1_dot_p4;
  ldouble p2_dot_p3;
  ldouble p2_dot_p4;
  ldouble p1_dot_p5;
  ldouble p2_dot_p5;
  ldouble p4_dot_p5;
  ldouble p3_dot_p5;
  bool real_denominator_domain;
  cdouble point_product_1[2][2];
  cdouble point_product_2[2][2];
  cdouble point_product_3[2][2];
  cdouble point_product_4[2][2];
  cdouble point_product_5[2][2];
  cdouble point_product_6[2][2];
  cdouble point_product_7[2][2];
  cdouble point_product_8[2][2];
  cdouble point_product_9[2][2];
  cdouble point_product_10[2][2];
  cdouble point_product_11[2][2];
  cdouble point_product_12[2][2];
  cdouble point_product_13[2][2];
  cdouble point_product_14[2][2];
  cdouble point_factor_1;
  cdouble point_factor_2;
  cdouble point_factor_3;
  cdouble point_factor_4;
  cdouble point_factor_5;
  cdouble point_factor_6;
  cdouble point_factor_7;
  cdouble point_factor_8;
  cdouble point_factor_9;
  cdouble point_factor_10;
  cdouble point_factor_11;
  cdouble point_factor_12;
  cdouble point_factor_13;
  cdouble point_factor_14;
  cdouble point_factor_15;
  cdouble point_factor_16;
  cdouble point_factor_17;
  hepcat_spinas::CachedDenominator point_denominator_1;
  hepcat_spinas::CachedDenominator point_denominator_2;
  hepcat_spinas::CachedDenominator point_denominator_3;
  hepcat_spinas::CachedDenominator point_denominator_4;
  hepcat_spinas::CachedDenominator point_denominator_5;
  hepcat_spinas::CachedDenominator point_denominator_6;
  hepcat_spinas::CachedDenominator point_denominator_7;
  hepcat_spinas::CachedDenominator point_denominator_8;
  hepcat_spinas::CachedDenominator point_denominator_9;
  hepcat_spinas::CachedDenominator point_denominator_10;
  hepcat_spinas::CachedDenominator point_denominator_11;

  // Internal kernel for already validated spin configurations.
  cdouble amp_unchecked(const int& ds1, const int& ds2, const int& ds3, const int& ds4, const int& ds5);

  // Each process owns its lazy point caches and shared middle matrices.
  bool all_point_cache_ready = false;
  bool summed_point_cache_ready = false;
  void prepare_all_point_cache();
  void prepare_summed_point_cache();

  void initialize_products();
};

} // namespace hepcat_generated

#include <functional>
#include <limits>
#include <stdexcept>
namespace hepcat_generated {
class draftcoloredcurrent4 {
public:
  static constexpr std::size_t leg_count = 5;
  static constexpr std::size_t parameter_count = 8;
  static constexpr std::size_t helicity_count = 1;
  static constexpr std::size_t color_basis_count = 1;
  using Parameters = std::array<cdouble, parameter_count>;
  using Momenta = hepcat_spinas::Momenta<leg_count>;
  using Spins = std::array<int, leg_count>;
  using Colors = std::array<int, leg_count>;
  using ColorAmplitudes = std::array<cdouble, color_basis_count>;
  draftcoloredcurrent4() : draftcoloredcurrent4(default_parameters()) {}
  explicit draftcoloredcurrent4(const Parameters& values) { set_parameters(values); }
  static Parameters default_parameters();
  static Spins default_spins() { return draftcoloredcurrent4_color_1::default_spins(); }
  static std::array<std::size_t, 2> incoming_legs() { return draftcoloredcurrent4_color_1::incoming_legs(); }
  static std::array<Spins, helicity_count> helicity_configurations() { return draftcoloredcurrent4_color_1::helicity_configurations(); }
  static ldouble incoming_spin_average() { return ldouble(4.0e0L); }
  static ldouble incoming_color_average() { return ldouble(9.0e0L); }
  static ldouble symmetry_factor() { return ldouble(1.0e0L); }
  const Parameters& get_parameters() const { return parameters; }
  const std::array<ldouble, leg_count>& masses() const { return component_1.masses(); }
  void set_parameters(const Parameters& values);
  void set_momenta(const Momenta& momenta);
  void set_widths_enabled(bool enabled);
  ColorAmplitudes color_amplitudes(const Spins& spins);
  // Explicit colors are 1..3 for quarks, 1..8 for gluons, 1 for singlets.
  cdouble amp(const Spins& spins, const Colors& colors);
  cdouble amp(const Spins&) { throw std::invalid_argument("A colored amplitude needs explicit colors; use amp(spins,colors), color_amplitudes or color_spin_sum."); }
  ldouble color_spin_sum(const Spins& spins);
  ldouble spin_sum();
  ldouble amp2();
  hepcat_spinas::ValidationReport validate_calchep(const hepcat_spinas::ValidationOptions& options = hepcat_spinas::ValidationOptions());
  // Independent MadGraph comparison, resolved by the validation library.
  hepcat_spinas::MadGraphValidationReport validate_madgraph(
    const hepcat_spinas::MadGraphValidationOptions& options = hepcat_spinas::MadGraphValidationOptions());

private:
  Parameters parameters;
  draftcoloredcurrent4_color_1 component_1;
  static cdouble color_tensor(std::size_t basis, const Colors& colors);
};
} // namespace hepcat_generated
