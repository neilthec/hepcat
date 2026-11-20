#pragma once

#include "validation_checks.hpp"

#include <complex>
#include <cstdint>
#include <map>
#include <optional>

namespace hepcat {

struct validation_options {
    std::size_t points = 10000;
    std::size_t workers = 0;
    std::optional<std::uint64_t> seed;
    std::optional<int> energy_bound;
    std::size_t samples_per_cut = 8, energy_directions = 4;
    long double relative_tolerance = 1e-8L;
    long double absolute_tolerance = 0;
    // Bare names set common inputs in both models. Fully scoped names override
    // individual parameters. Derived parameters are evaluated in their own model.
    std::map<std::string, std::complex<long double>> parameters;
    validation_parameter_points exact_parameter_points;
};

struct numerical_check {
    std::string kind;
    check_outcome status = check_outcome::unresolved;
    std::size_t compared = 0, nonzero = 0, failures = 0;
    long double max_absolute_error = 0, max_relative_error = 0;
    // Repeat every out-of-tolerance comparison at the identical point.
    std::size_t replays = 0;
    long double max_replay_absolute_error = 0, max_replay_relative_error = 0;
    std::optional<std::uint64_t> first_raw_outlier_seed, first_failure_seed;
    std::string detail;
};

struct validation_report {
    std::uint64_t seed = 0;
    std::size_t requested = 0;
    std::vector<builtin_check> checks;
    std::vector<numerical_check> numerical;
    bool passed() const;
};

} // namespace hepcat
