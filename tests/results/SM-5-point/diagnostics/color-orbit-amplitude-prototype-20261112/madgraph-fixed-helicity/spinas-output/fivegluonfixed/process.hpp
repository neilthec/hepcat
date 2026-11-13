#pragma once
#include <array>
#include <cstddef>
#include "spinas.h"
#include "spinas-runtime.hpp"
#include "spinas-validation.hpp"
#include "madgraph-runtime.hpp"

namespace hepcat_generated {

class fivegluonfixed_color_1 {
public:
  static constexpr std::size_t leg_count = 5;
  static constexpr std::size_t parameter_count = 1;
  static constexpr std::size_t helicity_count = 1;
  using Parameters = std::array<cdouble, parameter_count>;
  using Momenta = hepcat_spinas::Momenta<leg_count>;
  using Spins = std::array<int, leg_count>;

  fivegluonfixed_color_1();
  explicit fivegluonfixed_color_1(const Parameters& values);
  fivegluonfixed_color_1(const ldouble& value_GG);
  fivegluonfixed_color_1(const fivegluonfixed_color_1& other);
  fivegluonfixed_color_1& operator=(const fivegluonfixed_color_1& other);

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
  cdouble GG;

  // Constants and model-only factors.
  static const cdouble constant_sqrt2;
  cdouble parameter_factor_1_2;
  cdouble parameter_factor_1;

  // External particles.
  spinas::particle p1;
  spinas::particle p2;
  spinas::particle p3;
  spinas::particle p4;
  spinas::particle p5;

  // Spinor-product and scalar caches. Prepared once per momentum point.
  spinas::sproduct a45a;
  spinas::sproduct a13a;
  spinas::sproduct a15a;
  spinas::sproduct a23a;
  spinas::sproduct a24a;
  cdouble point_product_1;
  cdouble point_product_2;
  cdouble point_product_3;
  cdouble point_product_4;
  cdouble point_product_5;

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

#pragma once
#include <array>
#include <cstddef>
#include "spinas.h"
#include "spinas-runtime.hpp"
#include "spinas-validation.hpp"
#include "madgraph-runtime.hpp"

namespace hepcat_generated {

class fivegluonfixed_color_2 {
public:
  static constexpr std::size_t leg_count = 5;
  static constexpr std::size_t parameter_count = 1;
  static constexpr std::size_t helicity_count = 1;
  using Parameters = std::array<cdouble, parameter_count>;
  using Momenta = hepcat_spinas::Momenta<leg_count>;
  using Spins = std::array<int, leg_count>;

  fivegluonfixed_color_2();
  explicit fivegluonfixed_color_2(const Parameters& values);
  fivegluonfixed_color_2(const ldouble& value_GG);
  fivegluonfixed_color_2(const fivegluonfixed_color_2& other);
  fivegluonfixed_color_2& operator=(const fivegluonfixed_color_2& other);

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
  cdouble GG;

  // Constants and model-only factors.
  static const cdouble constant_sqrt2;
  cdouble parameter_factor_1_2;
  cdouble parameter_factor_1;

  // External particles.
  spinas::particle p1;
  spinas::particle p2;
  spinas::particle p3;
  spinas::particle p4;
  spinas::particle p5;

  // Spinor-product and scalar caches. Prepared once per momentum point.
  spinas::sproduct a45a;
  spinas::sproduct a12a;
  spinas::sproduct a15a;
  spinas::sproduct a23a;
  spinas::sproduct a34a;
  cdouble point_product_1;
  cdouble point_product_2;
  cdouble point_product_3;
  cdouble point_product_4;
  cdouble point_product_5;

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

#pragma once
#include <array>
#include <cstddef>
#include "spinas.h"
#include "spinas-runtime.hpp"
#include "spinas-validation.hpp"
#include "madgraph-runtime.hpp"

namespace hepcat_generated {

class fivegluonfixed_color_3 {
public:
  static constexpr std::size_t leg_count = 5;
  static constexpr std::size_t parameter_count = 1;
  static constexpr std::size_t helicity_count = 1;
  using Parameters = std::array<cdouble, parameter_count>;
  using Momenta = hepcat_spinas::Momenta<leg_count>;
  using Spins = std::array<int, leg_count>;

  fivegluonfixed_color_3();
  explicit fivegluonfixed_color_3(const Parameters& values);
  fivegluonfixed_color_3(const ldouble& value_GG);
  fivegluonfixed_color_3(const fivegluonfixed_color_3& other);
  fivegluonfixed_color_3& operator=(const fivegluonfixed_color_3& other);

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
  cdouble GG;

  // Constants and model-only factors.
  static const cdouble constant_sqrt2;
  cdouble parameter_factor_1_2;
  cdouble parameter_factor_1;

  // External particles.
  spinas::particle p1;
  spinas::particle p2;
  spinas::particle p3;
  spinas::particle p4;
  spinas::particle p5;

  // Spinor-product and scalar caches. Prepared once per momentum point.
  spinas::sproduct a15a;
  spinas::sproduct a23a;
  spinas::sproduct a24a;
  spinas::sproduct a13a;
  spinas::sproduct a25a;
  spinas::sproduct a12a;
  spinas::sproduct a34a;
  spinas::sproduct a45a;
  spinas::sproduct a14a;
  cdouble point_product_1;
  cdouble point_product_2;
  cdouble point_product_3;
  cdouble point_product_4;
  cdouble point_product_5;
  cdouble point_product_6;
  cdouble point_product_7;
  cdouble point_product_8;
  cdouble point_product_9;

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

#pragma once
#include <array>
#include <cstddef>
#include "spinas.h"
#include "spinas-runtime.hpp"
#include "spinas-validation.hpp"
#include "madgraph-runtime.hpp"

namespace hepcat_generated {

class fivegluonfixed_color_4 {
public:
  static constexpr std::size_t leg_count = 5;
  static constexpr std::size_t parameter_count = 1;
  static constexpr std::size_t helicity_count = 1;
  using Parameters = std::array<cdouble, parameter_count>;
  using Momenta = hepcat_spinas::Momenta<leg_count>;
  using Spins = std::array<int, leg_count>;

  fivegluonfixed_color_4();
  explicit fivegluonfixed_color_4(const Parameters& values);
  fivegluonfixed_color_4(const ldouble& value_GG);
  fivegluonfixed_color_4(const fivegluonfixed_color_4& other);
  fivegluonfixed_color_4& operator=(const fivegluonfixed_color_4& other);

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
  cdouble GG;

  // Constants and model-only factors.
  static const cdouble constant_sqrt2;
  cdouble parameter_factor_1_2;
  cdouble parameter_factor_1;

  // External particles.
  spinas::particle p1;
  spinas::particle p2;
  spinas::particle p3;
  spinas::particle p4;
  spinas::particle p5;

  // Spinor-product and scalar caches. Prepared once per momentum point.
  spinas::sproduct a15a;
  spinas::sproduct a23a;
  spinas::sproduct a24a;
  spinas::sproduct a13a;
  spinas::sproduct a25a;
  spinas::sproduct a12a;
  spinas::sproduct a34a;
  spinas::sproduct a45a;
  cdouble point_product_1;
  cdouble point_product_2;
  cdouble point_product_3;
  cdouble point_product_4;
  cdouble point_product_5;
  cdouble point_product_6;
  cdouble point_product_7;
  cdouble point_product_8;

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

#pragma once
#include <array>
#include <cstddef>
#include "spinas.h"
#include "spinas-runtime.hpp"
#include "spinas-validation.hpp"
#include "madgraph-runtime.hpp"

namespace hepcat_generated {

class fivegluonfixed_color_5 {
public:
  static constexpr std::size_t leg_count = 5;
  static constexpr std::size_t parameter_count = 1;
  static constexpr std::size_t helicity_count = 1;
  using Parameters = std::array<cdouble, parameter_count>;
  using Momenta = hepcat_spinas::Momenta<leg_count>;
  using Spins = std::array<int, leg_count>;

  fivegluonfixed_color_5();
  explicit fivegluonfixed_color_5(const Parameters& values);
  fivegluonfixed_color_5(const ldouble& value_GG);
  fivegluonfixed_color_5(const fivegluonfixed_color_5& other);
  fivegluonfixed_color_5& operator=(const fivegluonfixed_color_5& other);

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
  cdouble GG;

  // Constants and model-only factors.
  static const cdouble constant_sqrt2;
  cdouble parameter_factor_1_2;
  cdouble parameter_factor_1;

  // External particles.
  spinas::particle p1;
  spinas::particle p2;
  spinas::particle p3;
  spinas::particle p4;
  spinas::particle p5;

  // Spinor-product and scalar caches. Prepared once per momentum point.
  spinas::sproduct a15a;
  spinas::sproduct a23a;
  spinas::sproduct a34a;
  spinas::sproduct a13a;
  spinas::sproduct a24a;
  spinas::sproduct a35a;
  spinas::sproduct a12a;
  spinas::sproduct a45a;
  cdouble point_product_1;
  cdouble point_product_2;
  cdouble point_product_3;
  cdouble point_product_4;
  cdouble point_product_5;
  cdouble point_product_6;
  cdouble point_product_7;
  cdouble point_product_8;

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

#pragma once
#include <array>
#include <cstddef>
#include "spinas.h"
#include "spinas-runtime.hpp"
#include "spinas-validation.hpp"
#include "madgraph-runtime.hpp"

namespace hepcat_generated {

class fivegluonfixed_color_6 {
public:
  static constexpr std::size_t leg_count = 5;
  static constexpr std::size_t parameter_count = 1;
  static constexpr std::size_t helicity_count = 1;
  using Parameters = std::array<cdouble, parameter_count>;
  using Momenta = hepcat_spinas::Momenta<leg_count>;
  using Spins = std::array<int, leg_count>;

  fivegluonfixed_color_6();
  explicit fivegluonfixed_color_6(const Parameters& values);
  fivegluonfixed_color_6(const ldouble& value_GG);
  fivegluonfixed_color_6(const fivegluonfixed_color_6& other);
  fivegluonfixed_color_6& operator=(const fivegluonfixed_color_6& other);

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
  cdouble GG;

  // Constants and model-only factors.
  static const cdouble constant_sqrt2;
  cdouble parameter_factor_1_2;
  cdouble parameter_factor_1;

  // External particles.
  spinas::particle p1;
  spinas::particle p2;
  spinas::particle p3;
  spinas::particle p4;
  spinas::particle p5;

  // Spinor-product and scalar caches. Prepared once per momentum point.
  spinas::sproduct a14a;
  spinas::sproduct a15a;
  spinas::sproduct a23a;
  spinas::sproduct a45a;
  spinas::sproduct s23s;
  spinas::sproduct a13a;
  spinas::sproduct a24a;
  spinas::sproduct a12a;
  spinas::sproduct a35a;
  cdouble point_product_1;
  cdouble point_product_2;
  cdouble point_product_3;
  cdouble point_product_4;
  cdouble point_product_5;
  cdouble point_product_6;
  cdouble point_product_7;
  cdouble point_product_8;
  cdouble point_product_9;
  cdouble point_factor_1;
  cdouble point_factor_2;

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
class fivegluonfixed {
public:
  static constexpr std::size_t leg_count = 5;
  static constexpr std::size_t parameter_count = 1;
  static constexpr std::size_t helicity_count = 1;
  static constexpr std::size_t color_basis_count = 6;
  using Parameters = std::array<cdouble, parameter_count>;
  using Momenta = hepcat_spinas::Momenta<leg_count>;
  using Spins = std::array<int, leg_count>;
  using Colors = std::array<int, leg_count>;
  using ColorAmplitudes = std::array<cdouble, color_basis_count>;
  fivegluonfixed() : fivegluonfixed(default_parameters()) {}
  explicit fivegluonfixed(const Parameters& values) { set_parameters(values); }
  static Parameters default_parameters();
  static Spins default_spins() { return fivegluonfixed_color_1::default_spins(); }
  static std::array<std::size_t, 2> incoming_legs() { return fivegluonfixed_color_1::incoming_legs(); }
  static std::array<Spins, helicity_count> helicity_configurations() { return fivegluonfixed_color_1::helicity_configurations(); }
  static ldouble incoming_spin_average() { return ldouble(1.0e0L); }
  static ldouble incoming_color_average() { return ldouble(6.4e1L); }
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
  fivegluonfixed_color_1 component_1;
  fivegluonfixed_color_2 component_2;
  fivegluonfixed_color_3 component_3;
  fivegluonfixed_color_4 component_4;
  fivegluonfixed_color_5 component_5;
  fivegluonfixed_color_6 component_6;
  static cdouble color_tensor(std::size_t basis, const Colors& colors);
};
} // namespace hepcat_generated
