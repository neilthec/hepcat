#include "process.hpp"
#include <fstream>
#include <iostream>
#include <vector>

int main(int argc, char** argv) {
  try {
    if (hepcat_spinas::validation_help_requested(argc, argv)) {
      hepcat_spinas::print_validation_usage(std::cout);
      std::cout << "--report FILE --min-energy E --max-energy E --timeout SECONDS --zero-absolute-tolerance X\n";
      return 0;
    }
    hepcat_spinas::MadGraphValidationOptions options;
    options.points = 1000;
    // A fresh time-based seed is reported for every run.
    std::string report_file;
    std::vector<char*> ordinary(1, argv[0]);
    for (int i = 1; i < argc; ++i) {
      const std::string flag(argv[i]);
      if (flag == "--report" || flag == "--min-energy" || flag == "--max-energy" ||
          flag == "--timeout" || flag == "--zero-absolute-tolerance") {
        if (++i >= argc) throw std::invalid_argument("Missing validation option value.");
        if (flag == "--report") report_file = argv[i];
        else if (flag == "--min-energy") options.minimum_center_of_mass_energy = std::stold(argv[i]);
        else if (flag == "--max-energy") options.maximum_center_of_mass_energy = std::stold(argv[i]);
        else if (flag == "--zero-absolute-tolerance") options.zero_absolute_tolerance = std::stold(argv[i]);
        else { const auto seconds = std::stoul(argv[i]);
          if (seconds == 0 || seconds > 86400) throw std::invalid_argument("Timeout must be in [1,86400] seconds.");
          options.preparation_timeout_seconds = static_cast<unsigned>(seconds); }
      } else ordinary.push_back(argv[i]);
    }
    const auto parsed = hepcat_spinas::parse_validation_arguments(static_cast<int>(ordinary.size()), ordinary.data(), options);
    static_cast<hepcat_spinas::ValidationOptions&>(options) = parsed;
    hepcat_generated::experimentaleebarZZh process;
    const auto report = process.validate_madgraph(options);
    hepcat_spinas::print_madgraph_validation_report(report, std::cout);
    if (!report_file.empty()) {
      std::ofstream output(report_file);
      if (!output) throw std::runtime_error("Could not write the validation report.");
      hepcat_spinas::write_madgraph_validation_report_json(report, output);
    }
    return report.passed ? 0 : 1;
  } catch (const std::exception& error) {
    std::cerr << "MadGraph validation error: " << error.what() << '\n';
    return 2;
  }
}
