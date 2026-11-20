#pragma once

#include <cstdint>
#include <map>
#include <string>
#include <vector>

namespace hepcat {

class Algebra;

// Internal writer. The amp_expression caller holds the arena lock.
std::string format_mathematica(const Algebra& arena, std::uint32_t root,
                              const std::vector<std::uint32_t>& masses,
                              const std::map<std::vector<int>, std::uint32_t>& shells,
                              bool include_mass_rules);

} // namespace hepcat
