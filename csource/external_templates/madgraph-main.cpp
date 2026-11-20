#include "external-model.hpp"
#include "madgraph-runtime.hpp"
#include <fstream>
#include <iostream>

int main(int argc, char **argv) {
  using namespace hepcat_spinas;
  try {
    if (validation_help_requested(argc, argv)) {
      print_validation_usage(std::cout);
      return 0;
    }
    std::vector<char *> common{argv[0]};
    std::string file;
    for (int i = 1; i < argc; ++i) {
      if (std::string(argv[i]) == "--report") {
        if (++i == argc)
          throw std::invalid_argument("Expected --report output path.");
        file = argv[i];
      } else
        common.push_back(argv[i]);
    }
    ValidationOptions defaults;
    defaults.points = native_export::default_points;
    const auto options = parse_validation_arguments(
        static_cast<int>(common.size()), common.data(), defaults);
    if (options.points < 10000)
      throw std::invalid_argument("At least 10000 points are required.");
    native_export::Process proc;
    MadGraphConfiguration config;
#include "madgraph-model.inc"
    config.helper_path =
        std::string(HEPCAT_PROJECT_DIR) + "/madgraph-runtime.py";
    config.replay_binary128 = true;
    config.check_polarization_sum = true;
    config.complete_polarization_sum = native_export::complete_helicities;
    const auto result = validate_madgraph(proc, config, options);
    print_madgraph_validation_report(std::cout, result);
    if (!file.empty()) {
      std::ofstream out(file);
      write_madgraph_validation_report_json(out, result);
      out.close();
      if (!out)
        throw std::runtime_error("Could not write the MadGraph report " + file);
    }
    return result.passed ? 0 : 1;
  } catch (const std::exception &e) {
    std::cerr << e.what() << '\n';
    return 2;
  }
}
