#pragma once
#include "generation_plan.hpp"
#include "model.hpp"
#include "reduce.hpp"

namespace hepcat::generation {
// The supplied worker arena extends the immutable model expression tree.
Id sew(Algebra& arena, const model&, const std::vector<std::string>& process, const plan&, bool channel,
       bool replace_references = false, bool physical_references = false);
} // namespace hepcat::generation
