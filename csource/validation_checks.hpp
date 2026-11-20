#pragma once
#include <cstddef>
#include <string>
#include <utility>
#include <vector>

namespace hepcat {
class amplitude;
class amp_expression;

enum class check_outcome { pass, fail, unresolved };
struct builtin_check {
    std::string kind, label;
    check_outcome status;
    std::size_t samples;
    std::string detail;
};

using validation_parameter_points = std::vector<std::vector<std::pair<std::string, std::string>>>;
std::vector<builtin_check> native_builtin_checks(const amplitude&, const amp_expression&, int energy_bound,
                                                 const validation_parameter_points& parameter_points = {},
                                                 std::size_t samples_per_cut = 8,
                                                 std::size_t energy_directions = 4,
                                                 std::size_t sample_offset = 0,
                                                 const std::vector<int>& color_representations = {});
} // namespace hepcat
