#pragma once

#include "amp_expression.hpp"
#include "reduce.hpp"
#include <stdexcept>

namespace hepcat {

// A singular sample may be retried on the same cut branch. An unsupported
// input cannot be declared regular or assigned a zero residue.
struct residue_singular : std::runtime_error {
    using runtime_error::runtime_error;
};
struct residue_unsupported : std::runtime_error {
    using runtime_error::runtime_error;
};

// Return one exact linear constraint across all expressions. External masses
// must already be declared in algebra. Independent sample indices vary the
// external spin states, little-group scales, kinematics and scalar parameters.
// No contour factor or sewing sign is included here.
std::vector<mpq_class> residue_sample(const Algebra& algebra, const numerator_shell& channel,
                                      const std::vector<Id>& expressions, std::size_t sample);

} // namespace hepcat
