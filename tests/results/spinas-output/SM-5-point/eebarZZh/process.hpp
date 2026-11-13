#pragma once
#include <array>
#include <cstddef>
#include "spinas.h"
#include "spinas-runtime.hpp"
#include "spinas-validation.hpp"
#include "madgraph-runtime.hpp"

namespace hepcat_generated {

class sm5eebarZZh {
public:
  static constexpr std::size_t leg_count = 5;
  static constexpr std::size_t parameter_count = 6;
  static constexpr std::size_t helicity_count = 1;
  using Parameters = std::array<cdouble, parameter_count>;
  using Momenta = hepcat_spinas::Momenta<leg_count>;
  using Spins = std::array<int, leg_count>;

  sm5eebarZZh();
  explicit sm5eebarZZh(const Parameters& values);
  sm5eebarZZh(const ldouble& value_EE, const ldouble& value_MW, const ldouble& value_SW, const ldouble&
    value_Me, const ldouble& value_MZ, const ldouble& value_Mh);
  sm5eebarZZh(const sm5eebarZZh& other);
  sm5eebarZZh& operator=(const sm5eebarZZh& other);

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
  void set_masses(const ldouble& value_Me, const ldouble& value_MZ, const ldouble& value_Mh);
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
  cdouble Me;
  cdouble MZ;
  cdouble Mh;
  cdouble EE;
  cdouble MW;
  cdouble SW;

  // Constants and model-only factors.
  cdouble parameter_factor_1_2;
  cdouble parameter_factor_1;
  cdouble parameter_factor_3_2;
  cdouble parameter_factor_4;
  cdouble parameter_factor_3;
  cdouble parameter_factor_6_2;
  cdouble parameter_factor_6;
  cdouble parameter_factor_8_2;
  cdouble parameter_factor_8;
  cdouble parameter_factor_10_2;
  cdouble parameter_factor_10;
  cdouble parameter_factor_12_2;
  cdouble parameter_factor_12;
  cdouble parameter_factor_14_2;
  cdouble parameter_factor_14;
  cdouble parameter_factor_16_2;
  cdouble parameter_factor_16;
  cdouble parameter_factor_18_3;
  cdouble parameter_factor_18_2;
  cdouble parameter_factor_18;
  cdouble parameter_factor_21_2;
  cdouble parameter_factor_21;
  cdouble parameter_factor_23;
  cdouble parameter_factor_24_2;
  cdouble parameter_factor_24;
  cdouble parameter_factor_26;
  cdouble parameter_factor_27;
  cdouble parameter_factor_28;
  cdouble parameter_factor_29;
  cdouble parameter_factor_30;
  cdouble parameter_factor_31;
  cdouble parameter_factor_32;
  cdouble parameter_factor_33;
  cdouble parameter_factor_34;
  cdouble parameter_factor_35;
  cdouble parameter_factor_36;
  cdouble parameter_factor_37;
  cdouble parameter_factor_38;
  cdouble parameter_factor_39;
  cdouble parameter_factor_40;
  cdouble parameter_factor_41;
  cdouble parameter_factor_42;
  cdouble parameter_factor_43;
  cdouble parameter_factor_44;
  cdouble parameter_factor_45;
  cdouble parameter_factor_46;
  cdouble parameter_factor_47;
  cdouble parameter_factor_48;
  cdouble parameter_factor_49;
  cdouble parameter_factor_50;
  cdouble parameter_factor_51;
  cdouble parameter_factor_52;
  cdouble parameter_factor_53;
  cdouble parameter_factor_54;
  cdouble parameter_factor_55;

  // External particles.
  spinas::particle p1;
  spinas::particle p2;
  spinas::particle p3;
  spinas::particle p4;
  spinas::particle p5;

  // Spinor-product and scalar caches. Prepared once per momentum point.
  spinas::sproduct a12a;
  spinas::sproduct a34a;
  spinas::sproduct s34s;
  spinas::sproduct s12s;
  spinas::sproduct s152a;
  spinas::sproduct s251a;
  spinas::sproduct s343a;
  spinas::sproduct s353a;
  spinas::sproduct s454a;
  spinas::sproduct s434a;
  spinas::sproduct s13s;
  spinas::sproduct s24s;
  spinas::sproduct a13a;
  spinas::sproduct a24a;
  spinas::sproduct s314a;
  spinas::sproduct a23a;
  spinas::sproduct s23s;
  spinas::sproduct s213a;
  spinas::sproduct s312a;
  spinas::sproduct s413a;
  spinas::sproduct a14a;
  spinas::sproduct s14s;
  spinas::sproduct s253a;
  spinas::sproduct s352a;
  spinas::sproduct s124a;
  spinas::sproduct s324a;
  spinas::sproduct s421a;
  spinas::sproduct s423a;
  spinas::sproduct s153a;
  spinas::sproduct s351a;
  spinas::sproduct s154a;
  spinas::sproduct s451a;
  spinas::sproduct s254a;
  spinas::sproduct s452a;
  spinas::sproduct s143a;
  spinas::sproduct s341a;
  spinas::sproduct a1243a;
  spinas::sproduct a1253a;
  spinas::sproduct s1243s;
  spinas::sproduct s1253s;
  spinas::sproduct s234a;
  spinas::sproduct s432a;
  spinas::sproduct a2134a;
  spinas::sproduct a2154a;
  spinas::sproduct s2134s;
  spinas::sproduct s2154s;
  spinas::sproduct a3124a;
  spinas::sproduct s3124s;
  cdouble t12;
  cdouble t34;
  cdouble t25;
  cdouble t15;
  cdouble t45;
  cdouble t35;
  ldouble p1_dot_p3;
  cdouble t13;
  cdouble t14;
  ldouble p2_dot_p4;
  cdouble t24;
  cdouble t23;
  ldouble p2_dot_p5;
  ldouble p1_dot_p5;
  ldouble p1_dot_p2;
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
  cdouble point_product_15[2][2];
  cdouble point_product_16[2][2];
  cdouble point_product_17[2][2];
  cdouble point_product_18[2][2];
  cdouble point_product_19[2][2];
  cdouble point_product_20[2][2];
  cdouble point_product_21[2][2];
  cdouble point_product_22[2][2];
  cdouble point_product_23[2][2];
  cdouble point_product_24[2][2];
  cdouble point_product_25[2][2];
  cdouble point_product_26[2][2];
  cdouble point_product_27[2][2];
  cdouble point_product_28[2][2];
  cdouble point_product_29[2][2];
  cdouble point_product_30[2][2];
  cdouble point_product_31[2][2];
  cdouble point_product_32[2][2];
  cdouble point_product_33[2][2];
  cdouble point_product_34[2][2];
  cdouble point_product_35[2][2];
  cdouble point_product_36[2][2];
  cdouble point_product_37[2][2];
  cdouble point_product_38[2][2];
  cdouble point_product_39[2][2];
  cdouble point_product_40[2][2];
  cdouble point_product_41[2][2];
  cdouble point_product_42[2][2];
  cdouble point_product_43[2][2];
  cdouble point_product_44[2][2];
  cdouble point_product_45[2][2];
  cdouble point_product_46[2][2];
  cdouble point_product_47[2][2];
  cdouble point_product_48[2][2];
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
  cdouble point_factor_18;
  cdouble point_factor_19;
  cdouble point_factor_20;
  cdouble point_factor_21;
  cdouble point_factor_22;
  cdouble point_factor_23;
  cdouble point_factor_24;
  cdouble point_factor_25;
  cdouble point_factor_26;
  cdouble point_factor_27;
  cdouble point_factor_28;
  cdouble point_factor_29;
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
  hepcat_spinas::CachedDenominator point_denominator_12;
  hepcat_spinas::CachedDenominator point_denominator_13;
  hepcat_spinas::CachedDenominator point_denominator_14;
  hepcat_spinas::CachedDenominator point_denominator_15;
  hepcat_spinas::CachedDenominator point_denominator_16;

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
