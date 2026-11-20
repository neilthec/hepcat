#pragma once

#include "amp_expression.hpp"
#include "model.hpp"
#include <array>
#include <filesystem>
#include <map>

namespace hepcat {

struct spinas_component {
    std::vector<std::string> particles;
    amp_expression expression;
};

struct spinas_export_options {
    std::string name;
    // Both selections use zero-based positions in the analytic process.
    std::array<std::size_t, 2> incoming{0, 1};
    std::filesystem::path runtime_directory, spinas_directory;
    std::map<std::size_t, int> fixed_helicities;
    bool overwrite = false;
};

// PDGs and external names are physical, after crossing. All per-leg arrays
// retain analytic order. Helicity rows use zero for a massive or scalar leg.
struct spinas_project {
    std::filesystem::path directory;
    std::string name;
    std::array<std::size_t, 2> incoming;
    std::vector<int> pdgs, spin2;
    std::vector<std::string> parameter_names, physical_particles;
    std::vector<double> parameter_defaults, mass_defaults;
    std::vector<std::vector<int>> helicities, exact_zero_helicities;
    std::vector<std::size_t> width_parameters;
    std::map<std::string, int> mass_parameter_pdgs;
    double normalization = 1, symmetry_factor = 1;
    bool complete_helicities = true;
};

spinas_project write_spinas_process(const std::filesystem::path& directory, const model& vertices,
                                    const std::vector<spinas_component>& family,
                                    const spinas_export_options& options);

} // namespace hepcat
