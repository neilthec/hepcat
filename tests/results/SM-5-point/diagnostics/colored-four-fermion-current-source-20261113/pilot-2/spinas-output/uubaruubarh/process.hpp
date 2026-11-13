#pragma once
#include <array>
#include <cstddef>
#include "spinas.h"
#include "spinas-runtime.hpp"
#include "spinas-validation.hpp"
#include "madgraph-runtime.hpp"

namespace hepcat_generated {

class draftcoloredcurrent2_color_1 {
public:
  static constexpr std::size_t leg_count = 5;
  static constexpr std::size_t parameter_count = 6;
  static constexpr std::size_t helicity_count = 1;
  using Parameters = std::array<cdouble, parameter_count>;
  using Momenta = hepcat_spinas::Momenta<leg_count>;
  using Spins = std::array<int, leg_count>;

  draftcoloredcurrent2_color_1();
  explicit draftcoloredcurrent2_color_1(const Parameters& values);
  draftcoloredcurrent2_color_1(const ldouble& value_EE, const ldouble& value_MW, const ldouble& value_SW, const
    ldouble& value_GG, const ldouble& value_Mu, const ldouble& value_Mh);
  draftcoloredcurrent2_color_1(const draftcoloredcurrent2_color_1& other);
  draftcoloredcurrent2_color_1& operator=(const draftcoloredcurrent2_color_1& other);

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
  void set_masses(const ldouble& value_Mu, const ldouble& value_Mh);
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
  cdouble Mh;
  cdouble EE;
  cdouble MW;
  cdouble SW;
  cdouble GG;

  // Constants and model-only factors.
  cdouble parameter_factor_1_2;
  cdouble parameter_factor_2;
  cdouble parameter_factor_1;
  cdouble parameter_factor_4_2;
  cdouble parameter_factor_5;
  cdouble parameter_factor_4;
  cdouble parameter_factor_7_2;
  cdouble parameter_factor_8;
  cdouble parameter_factor_7;
  cdouble parameter_factor_10;
  cdouble parameter_factor_11;
  cdouble parameter_factor_12;
  cdouble parameter_factor_13_3;
  cdouble parameter_factor_13_2;
  cdouble parameter_factor_13;
  cdouble parameter_factor_16_2;
  cdouble parameter_factor_16;
  cdouble parameter_factor_18;
  cdouble parameter_factor_19_2;
  cdouble parameter_factor_19;
  cdouble parameter_factor_21_3;
  cdouble parameter_factor_21_2;
  cdouble parameter_factor_21;
  cdouble parameter_factor_24;
  cdouble parameter_factor_25;
  cdouble parameter_factor_26;
  cdouble parameter_factor_27_2;
  cdouble parameter_factor_27;
  cdouble parameter_factor_29;
  cdouble parameter_factor_30;
  cdouble parameter_factor_31;
  cdouble parameter_factor_32;
  cdouble parameter_factor_33;
  cdouble parameter_factor_34;
  cdouble parameter_factor_35_2;
  cdouble parameter_factor_35;
  cdouble parameter_factor_37;
  cdouble parameter_factor_38;
  cdouble parameter_factor_39;
  cdouble parameter_factor_40;
  cdouble parameter_factor_41;
  cdouble parameter_factor_42;
  cdouble parameter_factor_43_2;
  cdouble parameter_factor_43;
  cdouble parameter_factor_45;
  cdouble parameter_factor_46;
  cdouble parameter_factor_47;
  cdouble parameter_factor_48;
  cdouble parameter_factor_49;
  cdouble parameter_factor_50;

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
  spinas::sproduct s221a;
  spinas::sproduct s251a;
  spinas::sproduct s211a;
  spinas::sproduct s223a;
  spinas::sproduct s253a;
  spinas::sproduct s233a;
  spinas::sproduct a311s;
  spinas::sproduct a351s;
  spinas::sproduct s311a;
  spinas::sproduct s351a;
  spinas::sproduct a421s;
  spinas::sproduct a431s;
  spinas::sproduct a213s;
  spinas::sproduct a243s;
  spinas::sproduct s213a;
  spinas::sproduct s243a;
  spinas::sproduct s421a;
  spinas::sproduct s431a;
  spinas::sproduct s411a;
  spinas::sproduct s441a;
  spinas::sproduct s433a;
  spinas::sproduct s443a;
  spinas::sproduct s451a;
  spinas::sproduct s453a;
  spinas::sproduct a331s;
  spinas::sproduct s331a;
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
  spinas::sproduct a2133a;
  spinas::sproduct a2153a;
  spinas::sproduct a2433a;
  spinas::sproduct a2453a;
  spinas::sproduct s2133s;
  spinas::sproduct s2153s;
  spinas::sproduct s2433s;
  spinas::sproduct s2453s;
  spinas::sproduct s2231s;
  spinas::sproduct s2241s;
  spinas::sproduct s2531s;
  spinas::sproduct s2541s;
  spinas::sproduct a2213a;
  spinas::sproduct a2243a;
  spinas::sproduct a2513a;
  spinas::sproduct a2543a;
  spinas::sproduct s2213s;
  spinas::sproduct s2243s;
  spinas::sproduct s2513s;
  spinas::sproduct s2543s;
  spinas::sproduct a4211a;
  spinas::sproduct a4251a;
  spinas::sproduct a4311a;
  spinas::sproduct a4351a;
  spinas::sproduct s4211s;
  spinas::sproduct s4251s;
  spinas::sproduct s4311s;
  spinas::sproduct s4351s;
  spinas::sproduct a4421a;
  spinas::sproduct a4431a;
  spinas::sproduct a4521a;
  spinas::sproduct a4531a;
  spinas::sproduct s4421s;
  spinas::sproduct s4431s;
  spinas::sproduct s4521s;
  spinas::sproduct s4531s;
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
  cdouble t23;
  cdouble t14;
  ldouble p1_dot_p3;
  ldouble p1_dot_p4;
  ldouble p2_dot_p3;
  ldouble p2_dot_p4;
  ldouble s14;
  ldouble s23;
  cdouble t23_2;
  cdouble t14_2;
  ldouble p1_dot_p2;
  ldouble p3_dot_p4;
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
  cdouble point_product_71[2][2];
  cdouble point_product_72[2][2];
  cdouble point_product_73[2][2];
  cdouble point_product_74[2][2];
  cdouble point_product_75[2][2];
  cdouble point_product_76[2][2];
  cdouble point_product_77[2][2];
  cdouble point_product_78[2][2];
  cdouble point_product_79[2][2];
  cdouble point_product_80[2][2];
  cdouble point_product_81[2][2];
  cdouble point_product_82[2][2];
  cdouble point_product_83[2][2];
  cdouble point_product_84[2][2];
  cdouble point_product_85[2][2];
  cdouble point_product_86[2][2];
  cdouble point_product_87[2][2];
  cdouble point_product_88[2][2];
  cdouble point_product_89[2][2];
  cdouble point_product_90[2][2];
  cdouble point_product_91[2][2];
  cdouble point_product_92[2][2];
  cdouble point_product_93[2][2];
  cdouble point_product_94[2][2];
  cdouble point_product_95[2][2];
  cdouble point_product_96[2][2];
  cdouble point_product_97[2][2];
  cdouble point_product_98[2][2];
  cdouble point_product_99[2][2];
  cdouble point_product_100[2][2];
  cdouble point_product_101[2][2];
  cdouble point_product_102[2][2];
  cdouble point_product_103[2][2];
  cdouble point_product_104[2][2];
  cdouble point_product_105[2][2];
  cdouble point_product_106[2][2];
  cdouble point_product_107[2][2];
  cdouble point_product_108[2][2];
  cdouble point_product_109[2][2];
  cdouble point_product_110[2][2];
  cdouble point_product_111[2][2];
  cdouble point_product_112[2][2];
  cdouble point_product_113[2][2];
  cdouble point_product_114[2][2];
  cdouble point_product_115[2][2];
  cdouble point_product_116[2][2];
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
  cdouble point_factor_82;
  cdouble point_factor_83;
  cdouble point_factor_84;
  cdouble point_factor_85;
  cdouble point_factor_86;
  cdouble point_factor_87;
  cdouble point_factor_88;
  cdouble point_factor_89;
  cdouble point_factor_90;
  cdouble point_factor_91;
  cdouble point_factor_92;
  cdouble point_factor_93;
  cdouble point_factor_94;
  cdouble point_factor_95;
  cdouble point_factor_96;
  cdouble point_factor_97;
  cdouble point_factor_98;
  cdouble point_factor_99;
  cdouble point_factor_100;
  cdouble point_factor_101;
  cdouble point_factor_102;
  cdouble point_factor_103;
  cdouble point_factor_104;
  cdouble point_factor_105;
  cdouble point_factor_106;
  cdouble point_factor_107;
  cdouble point_factor_108;
  cdouble point_factor_109;
  cdouble point_factor_110;
  cdouble point_factor_111;
  cdouble point_factor_112;
  cdouble point_factor_113;
  cdouble point_factor_114;
  cdouble point_factor_115;
  cdouble point_factor_116;
  cdouble point_factor_117;
  cdouble point_factor_118;
  cdouble point_factor_119;
  cdouble point_factor_120;
  cdouble point_factor_121;
  cdouble point_factor_122;
  cdouble point_factor_123;
  cdouble point_factor_124;
  cdouble point_factor_125;
  cdouble point_factor_126;
  cdouble point_factor_127;
  cdouble point_factor_128;
  cdouble point_factor_129;
  cdouble point_factor_130;
  cdouble point_factor_131;
  cdouble point_factor_132;
  cdouble point_factor_133;
  cdouble point_factor_134;
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
  hepcat_spinas::CachedDenominator point_denominator_17;
  hepcat_spinas::CachedDenominator point_denominator_18;
  hepcat_spinas::CachedDenominator point_denominator_19;
  hepcat_spinas::CachedDenominator point_denominator_20;
  hepcat_spinas::CachedDenominator point_denominator_21;
  hepcat_spinas::CachedDenominator point_denominator_22;
  hepcat_spinas::CachedDenominator point_denominator_23;
  hepcat_spinas::CachedDenominator point_denominator_24;

  // Internal kernel for already validated spin configurations.
  cdouble amp_unchecked(const int& ds1, const int& ds2, const int& ds3, const int& ds4, const int& ds5);

  // Each process owns its lazy point caches and shared middle matrices.
  bool all_point_cache_ready = false;
  bool summed_point_cache_ready = false;
#ifdef SPINAS_HAS_MOMENTUM_CHAIN
  spinas::momentum_chain momentum_chain_1;
  spinas::momentum_chain momentum_chain_2;
  spinas::momentum_chain momentum_chain_3;
  spinas::momentum_chain momentum_chain_4;
  spinas::momentum_chain momentum_chain_5;
  spinas::momentum_chain momentum_chain_6;
  spinas::momentum_chain momentum_chain_7;
  spinas::momentum_chain momentum_chain_8;
  spinas::momentum_chain momentum_chain_9;
  spinas::momentum_chain momentum_chain_10;
  spinas::momentum_chain momentum_chain_11;
  spinas::momentum_chain momentum_chain_12;
  spinas::momentum_chain momentum_chain_13;
  spinas::momentum_chain momentum_chain_14;
  spinas::momentum_chain momentum_chain_15;
  spinas::momentum_chain momentum_chain_16;
#endif
  bool momentum_chain_1_ready = false;
  bool momentum_chain_2_ready = false;
  bool momentum_chain_3_ready = false;
  bool momentum_chain_4_ready = false;
  bool momentum_chain_5_ready = false;
  bool momentum_chain_6_ready = false;
  bool momentum_chain_7_ready = false;
  bool momentum_chain_8_ready = false;
  bool momentum_chain_9_ready = false;
  bool momentum_chain_10_ready = false;
  bool momentum_chain_11_ready = false;
  bool momentum_chain_12_ready = false;
  bool momentum_chain_13_ready = false;
  bool momentum_chain_14_ready = false;
  bool momentum_chain_15_ready = false;
  bool momentum_chain_16_ready = false;
  void prepare_all_point_cache();
  void prepare_summed_point_cache();

  void initialize_products();
};

} // namespace hepcat_generated

#pragma once
#include <array>
#include <cstddef>
#include "spinas.h"
#include "spinas-runtime.hpp"
#include "spinas-validation.hpp"
#include "madgraph-runtime.hpp"

namespace hepcat_generated {

class draftcoloredcurrent2_color_2 {
public:
  static constexpr std::size_t leg_count = 5;
  static constexpr std::size_t parameter_count = 6;
  static constexpr std::size_t helicity_count = 1;
  using Parameters = std::array<cdouble, parameter_count>;
  using Momenta = hepcat_spinas::Momenta<leg_count>;
  using Spins = std::array<int, leg_count>;

  draftcoloredcurrent2_color_2();
  explicit draftcoloredcurrent2_color_2(const Parameters& values);
  draftcoloredcurrent2_color_2(const ldouble& value_EE, const ldouble& value_GG, const ldouble& value_MW, const
    ldouble& value_SW, const ldouble& value_Mu, const ldouble& value_Mh);
  draftcoloredcurrent2_color_2(const draftcoloredcurrent2_color_2& other);
  draftcoloredcurrent2_color_2& operator=(const draftcoloredcurrent2_color_2& other);

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
  void set_masses(const ldouble& value_Mu, const ldouble& value_Mh);
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
  cdouble Mh;
  cdouble EE;
  cdouble GG;
  cdouble MW;
  cdouble SW;

  // Constants and model-only factors.
  cdouble parameter_factor_1_2;
  cdouble parameter_factor_2;
  cdouble parameter_factor_1;
  cdouble parameter_factor_4;
  cdouble parameter_factor_5_2;
  cdouble parameter_factor_6;
  cdouble parameter_factor_5;
  cdouble parameter_factor_8_2;
  cdouble parameter_factor_9;
  cdouble parameter_factor_8;
  cdouble parameter_factor_11;
  cdouble parameter_factor_12;
  cdouble parameter_factor_13;
  cdouble parameter_factor_14;
  cdouble parameter_factor_15_2;
  cdouble parameter_factor_15;
  cdouble parameter_factor_17_2;
  cdouble parameter_factor_17;
  cdouble parameter_factor_19_4;
  cdouble parameter_factor_19_3;
  cdouble parameter_factor_19_2;
  cdouble parameter_factor_19;
  cdouble parameter_factor_23_3;
  cdouble parameter_factor_23_2;
  cdouble parameter_factor_23;
  cdouble parameter_factor_26_2;
  cdouble parameter_factor_26;
  cdouble parameter_factor_28_2;
  cdouble parameter_factor_28;
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

  // External particles.
  spinas::particle p1;
  spinas::particle p2;
  spinas::particle p3;
  spinas::particle p4;
  spinas::particle p5;

  // Spinor-product and scalar caches. Prepared once per momentum point.
  spinas::sproduct a24a;
  spinas::sproduct s13s;
  spinas::sproduct a41a;
  spinas::sproduct s23s;
  spinas::sproduct a13a;
  spinas::sproduct s24s;
  spinas::sproduct a23a;
  spinas::sproduct s41s;
  spinas::sproduct a113s;
  spinas::sproduct a153s;
  spinas::sproduct a133s;
  spinas::sproduct a223s;
  spinas::sproduct a253s;
  spinas::sproduct a233s;
  spinas::sproduct a224s;
  spinas::sproduct a254s;
  spinas::sproduct a244s;
  spinas::sproduct a411s;
  spinas::sproduct a451s;
  spinas::sproduct a441s;
  spinas::sproduct s31s;
  spinas::sproduct a422s;
  spinas::sproduct a452s;
  spinas::sproduct a31a;
  spinas::sproduct a442s;
  spinas::sproduct s113a;
  spinas::sproduct s153a;
  spinas::sproduct s133a;
  spinas::sproduct a43a;
  spinas::sproduct s21s;
  spinas::sproduct a21a;
  spinas::sproduct s43s;
  spinas::sproduct a221s;
  spinas::sproduct a251s;
  spinas::sproduct s221a;
  spinas::sproduct s251a;
  spinas::sproduct s223a;
  spinas::sproduct s253a;
  spinas::sproduct s233a;
  spinas::sproduct a211s;
  spinas::sproduct a311s;
  spinas::sproduct a351s;
  spinas::sproduct s211a;
  spinas::sproduct s311a;
  spinas::sproduct s351a;
  spinas::sproduct a421s;
  spinas::sproduct a431s;
  spinas::sproduct a213s;
  spinas::sproduct a243s;
  spinas::sproduct s213a;
  spinas::sproduct s243a;
  spinas::sproduct s421a;
  spinas::sproduct s431a;
  spinas::sproduct s411a;
  spinas::sproduct s441a;
  spinas::sproduct s451a;
  spinas::sproduct a443s;
  spinas::sproduct a453s;
  spinas::sproduct s443a;
  spinas::sproduct s453a;
  spinas::sproduct a331s;
  spinas::sproduct a433s;
  spinas::sproduct s331a;
  spinas::sproduct s433a;
  spinas::sproduct a2133a;
  spinas::sproduct a2153a;
  spinas::sproduct a2433a;
  spinas::sproduct a2453a;
  spinas::sproduct s2133s;
  spinas::sproduct s2153s;
  spinas::sproduct s2433s;
  spinas::sproduct s2453s;
  spinas::sproduct a2213a;
  spinas::sproduct a2243a;
  spinas::sproduct a2513a;
  spinas::sproduct a2543a;
  spinas::sproduct s2213s;
  spinas::sproduct s2243s;
  spinas::sproduct s2513s;
  spinas::sproduct s2543s;
  spinas::sproduct a4211a;
  spinas::sproduct a4251a;
  spinas::sproduct a4311a;
  spinas::sproduct a4351a;
  spinas::sproduct s4211s;
  spinas::sproduct s4251s;
  spinas::sproduct s4311s;
  spinas::sproduct s4351s;
  spinas::sproduct a4421a;
  spinas::sproduct a4431a;
  spinas::sproduct a4521a;
  spinas::sproduct a4531a;
  spinas::sproduct s4421s;
  spinas::sproduct s4431s;
  spinas::sproduct s4521s;
  spinas::sproduct s4531s;
  ldouble s34;
  cdouble t15;
  cdouble t25;
  cdouble t35;
  ldouble s12;
  cdouble t45;
  cdouble t23;
  cdouble t14;
  ldouble s14;
  ldouble s23;
  cdouble t23_2;
  cdouble t14_2;
  ldouble p1_dot_p2;
  ldouble p1_dot_p3;
  ldouble p2_dot_p4;
  ldouble p3_dot_p4;
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
  cdouble point_product_71[2][2];
  cdouble point_product_72[2][2];
  cdouble point_product_73[2][2];
  cdouble point_product_74[2][2];
  cdouble point_product_75[2][2];
  cdouble point_product_76[2][2];
  cdouble point_product_77[2][2];
  cdouble point_product_78[2][2];
  cdouble point_product_79[2][2];
  cdouble point_product_80[2][2];
  cdouble point_product_81[2][2];
  cdouble point_product_82[2][2];
  cdouble point_product_83[2][2];
  cdouble point_product_84[2][2];
  cdouble point_product_85[2][2];
  cdouble point_product_86[2][2];
  cdouble point_product_87[2][2];
  cdouble point_product_88[2][2];
  cdouble point_product_89[2][2];
  cdouble point_product_90[2][2];
  cdouble point_product_91[2][2];
  cdouble point_product_92[2][2];
  cdouble point_product_93[2][2];
  cdouble point_product_94[2][2];
  cdouble point_product_95[2][2];
  cdouble point_product_96[2][2];
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
  hepcat_spinas::CachedDenominator point_denominator_10;
  hepcat_spinas::CachedDenominator point_denominator_11;
  hepcat_spinas::CachedDenominator point_denominator_12;
  hepcat_spinas::CachedDenominator point_denominator_13;
  hepcat_spinas::CachedDenominator point_denominator_14;
  hepcat_spinas::CachedDenominator point_denominator_15;
  hepcat_spinas::CachedDenominator point_denominator_16;
  hepcat_spinas::CachedDenominator point_denominator_17;
  hepcat_spinas::CachedDenominator point_denominator_18;

  // Internal kernel for already validated spin configurations.
  cdouble amp_unchecked(const int& ds1, const int& ds2, const int& ds3, const int& ds4, const int& ds5);

  // Each process owns its lazy point caches and shared middle matrices.
  bool all_point_cache_ready = false;
  bool summed_point_cache_ready = false;
#ifdef SPINAS_HAS_MOMENTUM_CHAIN
  spinas::momentum_chain momentum_chain_1;
  spinas::momentum_chain momentum_chain_2;
  spinas::momentum_chain momentum_chain_3;
  spinas::momentum_chain momentum_chain_4;
#endif
  bool momentum_chain_1_ready = false;
  bool momentum_chain_2_ready = false;
  bool momentum_chain_3_ready = false;
  bool momentum_chain_4_ready = false;
  void prepare_all_point_cache();
  void prepare_summed_point_cache();

  void initialize_products();
};

} // namespace hepcat_generated

#include <functional>
#include <limits>
#include <stdexcept>
namespace hepcat_generated {
class draftcoloredcurrent2 {
public:
  static constexpr std::size_t leg_count = 5;
  static constexpr std::size_t parameter_count = 6;
  static constexpr std::size_t helicity_count = 1;
  static constexpr std::size_t color_basis_count = 2;
  using Parameters = std::array<cdouble, parameter_count>;
  using Momenta = hepcat_spinas::Momenta<leg_count>;
  using Spins = std::array<int, leg_count>;
  using Colors = std::array<int, leg_count>;
  using ColorAmplitudes = std::array<cdouble, color_basis_count>;
  draftcoloredcurrent2() : draftcoloredcurrent2(default_parameters()) {}
  explicit draftcoloredcurrent2(const Parameters& values) { set_parameters(values); }
  static Parameters default_parameters();
  static Spins default_spins() { return draftcoloredcurrent2_color_1::default_spins(); }
  static std::array<std::size_t, 2> incoming_legs() { return draftcoloredcurrent2_color_1::incoming_legs(); }
  static std::array<Spins, helicity_count> helicity_configurations() { return draftcoloredcurrent2_color_1::helicity_configurations(); }
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
  draftcoloredcurrent2_color_1 component_1;
  draftcoloredcurrent2_color_2 component_2;
  static cdouble color_tensor(std::size_t basis, const Colors& colors);
};
} // namespace hepcat_generated
