#pragma once
#include <array>
#include <cstddef>
#include "spinas.h"
#include "spinas-runtime.hpp"
#include "spinas-validation.hpp"
#include "madgraph-runtime.hpp"

namespace hepcat_generated {

class draftcoloredcurrent3_color_1 {
public:
  static constexpr std::size_t leg_count = 5;
  static constexpr std::size_t parameter_count = 6;
  static constexpr std::size_t helicity_count = 1;
  using Parameters = std::array<cdouble, parameter_count>;
  using Momenta = hepcat_spinas::Momenta<leg_count>;
  using Spins = std::array<int, leg_count>;

  draftcoloredcurrent3_color_1();
  explicit draftcoloredcurrent3_color_1(const Parameters& values);
  draftcoloredcurrent3_color_1(const ldouble& value_EE, const ldouble& value_MW, const ldouble& value_SW, const
    ldouble& value_Me, const ldouble& value_Mu, const ldouble& value_Mh);
  draftcoloredcurrent3_color_1(const draftcoloredcurrent3_color_1& other);
  draftcoloredcurrent3_color_1& operator=(const draftcoloredcurrent3_color_1& other);

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
  void set_masses(const ldouble& value_Me, const ldouble& value_Mu, const ldouble& value_Mh);
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
  cdouble Mu;
  cdouble Mh;
  cdouble EE;
  cdouble MW;
  cdouble SW;

  // Constants and model-only factors.
  cdouble parameter_factor_1_2;
  cdouble parameter_factor_2;
  cdouble parameter_factor_1;
  cdouble parameter_factor_4_2;
  cdouble parameter_factor_5;
  cdouble parameter_factor_4;
  cdouble parameter_factor_7_2;
  cdouble parameter_factor_7;
  cdouble parameter_factor_9_2;
  cdouble parameter_factor_9;
  cdouble parameter_factor_11_2;
  cdouble parameter_factor_11;
  cdouble parameter_factor_13;
  cdouble parameter_factor_14;
  cdouble parameter_factor_15_3;
  cdouble parameter_factor_15_2;
  cdouble parameter_factor_15;
  cdouble parameter_factor_18_2;
  cdouble parameter_factor_18;
  cdouble parameter_factor_20_2;
  cdouble parameter_factor_20;
  cdouble parameter_factor_22;
  cdouble parameter_factor_23_2;
  cdouble parameter_factor_23;
  cdouble parameter_factor_25;
  cdouble parameter_factor_26_3;
  cdouble parameter_factor_26_2;
  cdouble parameter_factor_26;
  cdouble parameter_factor_29;
  cdouble parameter_factor_30;
  cdouble parameter_factor_31;
  cdouble parameter_factor_32;
  cdouble parameter_factor_33_3;
  cdouble parameter_factor_33_2;
  cdouble parameter_factor_33;
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
  cdouble parameter_factor_56;
  cdouble parameter_factor_57;
  cdouble parameter_factor_58;
  cdouble parameter_factor_59;
  cdouble parameter_factor_60;
  cdouble parameter_factor_61;
  cdouble parameter_factor_62;
  cdouble parameter_factor_63;
  cdouble parameter_factor_64;
  cdouble parameter_factor_65;
  cdouble parameter_factor_66;
  cdouble parameter_factor_67;
  cdouble parameter_factor_68;
  cdouble parameter_factor_69;
  cdouble parameter_factor_70;

  // External particles.
  spinas::particle p1;
  spinas::particle p2;
  spinas::particle p3;
  spinas::particle p4;
  spinas::particle p5;

  // Spinor-product and scalar caches. Prepared once per momentum point.
  spinas::sproduct a21a;
  spinas::sproduct a43a;
  spinas::sproduct a24a;
  spinas::sproduct s13s;
  spinas::sproduct s21s;
  spinas::sproduct a41a;
  spinas::sproduct s23s;
  spinas::sproduct a13a;
  spinas::sproduct s24s;
  spinas::sproduct a23a;
  spinas::sproduct s41s;
  spinas::sproduct s43s;
  spinas::sproduct a113s;
  spinas::sproduct a153s;
  spinas::sproduct a133s;
  spinas::sproduct a211s;
  spinas::sproduct a251s;
  spinas::sproduct a221s;
  spinas::sproduct a223s;
  spinas::sproduct a253s;
  spinas::sproduct a233s;
  spinas::sproduct a224s;
  spinas::sproduct a254s;
  spinas::sproduct a244s;
  spinas::sproduct a231s;
  spinas::sproduct a241s;
  spinas::sproduct a433s;
  spinas::sproduct a443s;
  spinas::sproduct a411s;
  spinas::sproduct a451s;
  spinas::sproduct a441s;
  spinas::sproduct s31s;
  spinas::sproduct a422s;
  spinas::sproduct a452s;
  spinas::sproduct a31a;
  spinas::sproduct a442s;
  spinas::sproduct a453s;
  spinas::sproduct s113a;
  spinas::sproduct s153a;
  spinas::sproduct s133a;
  spinas::sproduct s231a;
  spinas::sproduct s241a;
  spinas::sproduct s211a;
  spinas::sproduct s251a;
  spinas::sproduct s221a;
  spinas::sproduct s223a;
  spinas::sproduct s253a;
  spinas::sproduct s233a;
  spinas::sproduct s433a;
  spinas::sproduct s443a;
  spinas::sproduct s411a;
  spinas::sproduct s451a;
  spinas::sproduct s441a;
  spinas::sproduct s453a;
  spinas::sproduct a2311a;
  spinas::sproduct a2351a;
  spinas::sproduct a2411a;
  spinas::sproduct a2451a;
  spinas::sproduct a2231a;
  spinas::sproduct a2241a;
  spinas::sproduct a2531a;
  spinas::sproduct a2541a;
  spinas::sproduct s2311s;
  spinas::sproduct s2351s;
  spinas::sproduct s2411s;
  spinas::sproduct s2451s;
  spinas::sproduct s2231s;
  spinas::sproduct s2241s;
  spinas::sproduct s2531s;
  spinas::sproduct s2541s;
  cdouble t34;
  cdouble t15;
  cdouble t25;
  cdouble t12;
  cdouble t35;
  cdouble t45;
  ldouble s34;
  cdouble t34_2;
  ldouble s12;
  cdouble t12_2;
  ldouble p1_dot_p3;
  ldouble p1_dot_p4;
  ldouble p2_dot_p3;
  ldouble p2_dot_p4;
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
  cdouble point_product_49[2][2];
  cdouble point_product_50[2][2];
  cdouble point_product_51[2][2];
  cdouble point_product_52[2][2];
  cdouble point_product_53[2][2];
  cdouble point_product_54[2][2];
  cdouble point_product_55[2][2];
  cdouble point_product_56[2][2];
  cdouble point_product_57[2][2];
  cdouble point_product_58[2][2];
  cdouble point_product_59[2][2];
  cdouble point_product_60[2][2];
  cdouble point_product_61[2][2];
  cdouble point_product_62[2][2];
  cdouble point_product_63[2][2];
  cdouble point_product_64[2][2];
  cdouble point_product_65[2][2];
  cdouble point_product_66[2][2];
  cdouble point_product_67[2][2];
  cdouble point_product_68[2][2];
  cdouble point_product_69[2][2];
  cdouble point_product_70[2][2];
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
  cdouble point_factor_30;
  cdouble point_factor_31;
  cdouble point_factor_32;
  cdouble point_factor_33;
  cdouble point_factor_34;
  cdouble point_factor_35;
  cdouble point_factor_36;
  cdouble point_factor_37;
  cdouble point_factor_38;
  cdouble point_factor_39;
  cdouble point_factor_40;
  cdouble point_factor_41;
  cdouble point_factor_42;
  cdouble point_factor_43;
  cdouble point_factor_44;
  cdouble point_factor_45;
  cdouble point_factor_46;
  cdouble point_factor_47;
  cdouble point_factor_48;
  cdouble point_factor_49;
  cdouble point_factor_50;
  cdouble point_factor_51;
  cdouble point_factor_52;
  cdouble point_factor_53;
  cdouble point_factor_54;
  cdouble point_factor_55;
  cdouble point_factor_56;
  cdouble point_factor_57;
  cdouble point_factor_58;
  cdouble point_factor_59;
  cdouble point_factor_60;
  cdouble point_factor_61;
  cdouble point_factor_62;
  cdouble point_factor_63;
  cdouble point_factor_64;
  cdouble point_factor_65;
  cdouble point_factor_66;
  cdouble point_factor_67;
  cdouble point_factor_68;
  cdouble point_factor_69;
  cdouble point_factor_70;
  cdouble point_factor_71;
  cdouble point_factor_72;
  cdouble point_factor_73;
  cdouble point_factor_74;
  cdouble point_factor_75;
  cdouble point_factor_76;
  cdouble point_factor_77;
  cdouble point_factor_78;
  cdouble point_factor_79;
  cdouble point_factor_80;
  cdouble point_factor_81;
  hepcat_spinas::CachedDenominator point_denominator_1;
  hepcat_spinas::CachedDenominator point_denominator_2;
  hepcat_spinas::CachedDenominator point_denominator_3;
  hepcat_spinas::CachedDenominator point_denominator_4;
  hepcat_spinas::CachedDenominator point_denominator_5;
  hepcat_spinas::CachedDenominator point_denominator_6;
  hepcat_spinas::CachedDenominator point_denominator_7;
  hepcat_spinas::CachedDenominator point_denominator_8;
  hepcat_spinas::CachedDenominator point_denominator_9;

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
class draftcoloredcurrent3 {
public:
  static constexpr std::size_t leg_count = 5;
  static constexpr std::size_t parameter_count = 6;
  static constexpr std::size_t helicity_count = 1;
  static constexpr std::size_t color_basis_count = 1;
  using Parameters = std::array<cdouble, parameter_count>;
  using Momenta = hepcat_spinas::Momenta<leg_count>;
  using Spins = std::array<int, leg_count>;
  using Colors = std::array<int, leg_count>;
  using ColorAmplitudes = std::array<cdouble, color_basis_count>;
  draftcoloredcurrent3() : draftcoloredcurrent3(default_parameters()) {}
  explicit draftcoloredcurrent3(const Parameters& values) { set_parameters(values); }
  static Parameters default_parameters();
  static Spins default_spins() { return draftcoloredcurrent3_color_1::default_spins(); }
  static std::array<std::size_t, 2> incoming_legs() { return draftcoloredcurrent3_color_1::incoming_legs(); }
  static std::array<Spins, helicity_count> helicity_configurations() { return draftcoloredcurrent3_color_1::helicity_configurations(); }
  static ldouble incoming_spin_average() { return ldouble(4.0e0L); }
  static ldouble incoming_color_average() { return ldouble(1.0e0L); }
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
  draftcoloredcurrent3_color_1 component_1;
  static cdouble color_tensor(std::size_t basis, const Colors& colors);
};
} // namespace hepcat_generated
