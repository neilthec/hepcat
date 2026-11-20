#pragma once

#include <cstddef>
#include <cstdint>
#include <map>
#include <optional>
#include <string>
#include <vector>

namespace hepcat {

struct identity_check_options {
    std::size_t samples = 8;
    std::optional<std::uint64_t> seed;
    // For colored expressions, give the color representation of each incoming leg.
    std::vector<int> color_representations;
    // Optional fixed rational values ("7/3", for example) keyed by exact symbol.
    // Unbound independent scalar parameters vary across samples.
    std::map<std::string, std::string> parameters;
};

enum class identity_check_status { agrees_on_samples, mismatch, unresolved };

struct identity_check_report {
    identity_check_status status = identity_check_status::unresolved;
    std::uint64_t seed = 0;
    std::size_t compared = 0;
    std::optional<std::uint64_t> failing_sample;
    std::string detail;
    bool passed() const { return status == identity_check_status::agrees_on_samples; }
};

} // namespace hepcat
