#include "external_export.hpp"
#include "reduce.hpp"
#include <algorithm>
#include <chrono>
#include <fstream>
#include <iostream>
#include <stdexcept>

using namespace hepcat;
namespace fs = std::filesystem;

int main(int argc, char **argv) {
  try {
    if (argc != 3 && argc != 8)
      throw std::invalid_argument(
          "Usage: external_export_test MODEL_DIRECTORY RUNTIME_DIRECTORY "
          "[OUTPUT SPINAS CALCHEP CH_MODELS MADGRAPH]");
    const auto tmp = argc == 8
                         ? fs::path(argv[3])
                         : fs::temp_directory_path() /
                               ("hepcat-native-backend-test-" +
                                std::to_string(std::chrono::steady_clock::now()
                                                   .time_since_epoch()
                                                   .count()));
    model elementary(argv[1], 3), catalog(argv[1], 4);
    const auto me = catalog.particle("e").mass;
    Algebra a(&catalog.algebra());
    std::vector<spinas_component> components;
    for (const auto &v : catalog.vertices())
      if (v.legs.size() == 4 && v.legs[2].find("A.") == 0)
        components.push_back(
            {v.legs, amp_expression(a.format(a.mul({v.factor, v.expression})),
                                    {me, me, "0", "0"})});
    spinas_export_options export_options;
    export_options.runtime_directory = argv[2];
    export_options.overwrite = true;
    export_options.name = "eeAA";
    if (argc == 8)
      export_options.spinas_directory = argv[4];
    auto project =
        write_spinas_process(tmp, catalog, components, export_options);
    external_options o;
    o.runtime_directory = argv[2];
    o.seed = 927481;
    o.allowed_pdgs = {11, 22};
    o.calchep_directory = argc == 8 ? argv[5] : "calchep";
    o.calchep_model_directory = argc == 8 ? argv[6] : "models";
    o.calchep_model_number = 2;
    o.madgraph_model = "sm-full";
    o.madgraph_directory = argc == 8 ? argv[7] : "madgraph";
    if (const auto *python = std::getenv("MADGRAPH_PYTHON"))
      o.python_executable = python;
    auto too_few = o;
    too_few.points = 9999;
    try {
      write_external_project(project, elementary, too_few);
      throw std::runtime_error("accepted 9999 points");
    } catch (const std::invalid_argument &) {
    }
    write_external_project(project, elementary, o);
    for (const auto *path :
         {"external.cmake", "calchep-model.inc", "madevent-config.json",
          "calchep-main.cpp", "madgraph-main.cpp"})
      if (!fs::is_regular_file(tmp / path))
        throw std::runtime_error(std::string("Missing ") + path);
    std::ifstream ch(tmp / "calchep-model.inc");
    std::string text{std::istreambuf_iterator<char>(ch), {}};
    if (text.find("denominators.push_back") == std::string::npos)
      throw std::runtime_error("missing sampling channels");
    if (argc == 8) {
      const auto r = validate_external_project(project, o);
      std::cout << "CH " << r.calchep << " " << r.calchep_report << "\nMG "
                << r.madgraph << " " << r.madgraph_report << "\n";
      if (!r.passed())
        return 1;
    } else {
#if defined(__unix__) || defined(__APPLE__)
      // Some Fortran runtimes exit successfully before the validation report is written.
      const auto build = tmp / "build";
      fs::create_directories(build);
      for (const char* backend : {"calchep", "madgraph"}) {
        const auto validator = build / (project.name + "_validate_" + backend);
        std::ofstream(validator) << "#!/bin/sh\nexit 0\n";
        fs::permissions(validator, fs::perms::owner_exec, fs::perm_options::add);
      }
      o.cmake_executable = "true";
      std::vector<std::string> messages;
      o.progress = [&](const std::string& message) { messages.push_back(message); };
      const auto result = validate_external_project(project, o);
      if (result.configuration != 0 || result.calchep == 0 || result.madgraph == 0 || result.passed())
        throw std::runtime_error("An external program exited without producing its validation results");
      for (const char* backend : {"CalcHEP", "MadGraph"}) {
        const auto prefix = std::string("UNRESOLVED ") + backend;
        if (std::none_of(messages.begin(), messages.end(), [&](const std::string& message) {
              return message.rfind(prefix, 0) == 0;
            }))
          throw std::runtime_error("Missing unresolved validation message for " + std::string(backend));
      }
#endif
      fs::remove_all(tmp);
    }
  } catch (const std::exception &e) {
    std::cerr << e.what() << '\n';
    return 1;
  }
}
