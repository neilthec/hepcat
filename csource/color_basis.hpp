#pragma once

#include "reduce.hpp"

namespace hepcat {

// roots[j] = sum_i tensors[i] * coefficients[i][j] identically as an SU(3) tensor.
struct color_decomposition {
    std::vector<Id> tensors;
    std::vector<std::vector<Id>> coefficients;
};

color_decomposition decompose_color(Algebra&, const std::vector<int>& representations,
                                    const std::vector<Id>& roots, bool order_vector_ends = false);

} // namespace hepcat
