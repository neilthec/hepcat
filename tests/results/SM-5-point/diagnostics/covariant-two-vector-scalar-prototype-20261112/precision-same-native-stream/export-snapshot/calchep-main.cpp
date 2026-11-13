#include "process.hpp"
#include <iostream>
#include <stdexcept>

int main(int argc, char** argv) {
  try {
    if (hepcat_spinas::validation_help_requested(argc, argv)) {
      hepcat_spinas::print_validation_usage(std::cout);
      return 0;
    }

    hepcat_spinas::ValidationOptions defaults;
    defaults.points = 1000;
    // ValidationOptions chooses a fresh seed from the time at startup.
    const auto options = hepcat_spinas::parse_validation_arguments(argc, argv, defaults);

    hepcat_generated::experimentaleebarZZh process;
    const auto report = process.validate_calchep(options);
    hepcat_spinas::print_validation_report(report, std::cout);
    return report.passed ? 0 : 1;
  } catch (const std::exception& error) {
    std::cerr << "Validation failed: " << error.what() << '\n';
    return 2;
  }
}
