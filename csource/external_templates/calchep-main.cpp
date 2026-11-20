#include "external-model.hpp"
#include "spinas-validation.hpp"
#ifdef HEPCAT_HAS_CALCHEP
#include "calchep-runtime.hpp"
#endif
#include <iostream>
#include <fstream>
#include <iomanip>

int main(int argc, char **argv) {
  using namespace hepcat_spinas;
  try {
    if (validation_help_requested(argc, argv)) {
      print_validation_usage(std::cout);
      return 0;
    }
    ValidationOptions defaults;
    defaults.points = native_export::default_points;
    std::vector<char *> common{argv[0]};
    std::ofstream outliers;
    for (int i = 1; i < argc; ++i) {
      if (std::string(argv[i]) == "--outliers") {
        if (++i == argc)
          throw std::invalid_argument("Expected outlier CSV path.");
        outliers.exceptions(std::ios::failbit | std::ios::badbit);
        outliers.open(argv[i]);
        outliers << "point,hepcat,calchep";
        for (size_t leg = 0; leg < native_export::Process::leg_count; ++leg)
          for (int component = 0; component < 4; ++component)
            outliers << ",p" << leg + 1 << "_" << component;
        outliers << '\n' << std::setprecision(std::numeric_limits<ldouble>::max_digits10);
      } else
        common.push_back(argv[i]);
    }
    auto options =
        parse_validation_arguments(static_cast<int>(common.size()), common.data(), defaults);
    if (options.points < 10000)
      throw std::invalid_argument("At least 10000 points are required.");
#ifdef HEPCAT_HAS_CALCHEP
    if (!native_export::complete_helicities)
      throw std::invalid_argument(
          "CalcHEP requires the complete unpolarized helicity family.");
    native_export::Process proc;
    auto values = proc.get_parameters();
    for (auto i : native_export::width_indices())
      values.at(i) = 0;
    proc.set_parameters(values);
    proc.set_widths_enabled(false);
    std::vector<CalcHEPParameter> inputs;
    const auto names = native_export::calchep_names();
    double strong = 1;
    for (std::size_t i = 0; i < names.size(); ++i) {
      const double value = checked_calchep_parameter(values[i], names[i]);
      inputs.push_back({names[i], value});
      if (names[i] == "GG")
        strong = value;
    }
    calchep_detail::CompilerInstallation installation(HEPCAT_CALCHEP_ROOT);
    CalcHEPProcess reference(native_export::calchep_model_directory,
                             native_export::calchep_model_number,
                             native_export::physical_pdgs(), inputs, strong,
                             false, native_export::allowed_pdgs(), 0,
                             native_export::independent_mass_names(),
                             /*omit_debug_symbols=*/true);
    const auto order = native_export::leg_order();
    check_calchep_masses(proc.masses(), reference.masses(), order);
    size_t point_index = 0, retained = 0;
    ldouble native = 0;
    auto report = compare_squared_amplitudes(
        proc.masses(), proc.incoming_legs(),
        [&](const native_export::Process::Momenta &point) {
          ++point_index;
          proc.set_momenta(point);
          return native = proc.amp2();
        },
        [&](const native_export::Process::Momenta &point) {
          const auto expected = reference.squared_amplitude(calchep_momenta_in_order(point, order));
          if (outliers.is_open() &&
              std::abs(native - expected) >
                  options.absolute_tolerance +
                      options.relative_tolerance *
                          std::max(std::abs(native), ldouble(std::abs(expected)))) {
            outliers << point_index << ',' << native << ',' << expected;
            for (const auto &p : point)
              for (auto x : p)
                outliers << ',' << x;
            outliers << '\n';
            ++retained;
          }
          return expected;
        },
        options);
    print_validation_report(report, std::cout);
    if (outliers.is_open())
      outliers.close();
    if (!report.passed && !report.evaluation_errors && report.compared_points == options.points &&
        retained && retained == report.mismatches)
      return 3; // every numerical outlier is available for replay
    return report.passed ? 0 : 1;
#else
    throw std::runtime_error(
        "Configure CALCHEP_ROOT and rebuild to enable CalcHEP validation.");
#endif
  } catch (const std::exception &e) {
    std::cerr << e.what() << '\n';
    return 2;
  }
}
