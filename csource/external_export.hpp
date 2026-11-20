#pragma once

#include "spinas_export.hpp"
#include <optional>

namespace hepcat {
struct external_options {
  std::filesystem::path calchep_directory, calchep_model_directory;
  unsigned calchep_model_number = 0;
  std::filesystem::path madgraph_directory;
  std::string madgraph_model;
  std::string python_executable = "python3", cmake_executable = "cmake";
  std::filesystem::path runtime_directory, template_directory;
  std::vector<int>
      allowed_pdgs; // positive absolute PDGs; empty retains the full theory
  std::map<std::string, std::string> madgraph_parameter_map,
      calchep_parameter_map;
  std::map<std::string, double> madgraph_parameter_rules;
  std::map<int, int> charge3; // override three times the electric charge,
                              // indexed by particle PDG
  std::size_t points = 10000;
  std::optional<std::uint64_t> seed;
  bool run_calchep = true, run_madgraph = true;
};

void write_external_project(const spinas_project &, const model &,
                            const external_options &);
struct external_report {
  std::uint64_t seed;
  std::size_t points;
  int configuration = -1, calchep = -1, madgraph = -1;
  std::filesystem::path calchep_report, madgraph_report;
  bool requested_calchep = true, requested_madgraph = true;
  bool passed() const {
    return configuration == 0 && (requested_calchep || requested_madgraph) &&
           (!requested_calchep || calchep == 0) &&
           (!requested_madgraph || madgraph == 0);
  }
};
external_report validate_external_project(const spinas_project &,
                                          const external_options &);
// Run an executable with literal arguments and capture stdout/stderr in a new
// log.
int run_external_command(const std::vector<std::string> &arguments,
                         const std::filesystem::path &log);
} // namespace hepcat
