#pragma once
#include "generation_plan.hpp"
#include "model.hpp"
#include "reduce.hpp"

namespace hepcat::generation {
std::string feynman_external(const model&, const std::string& name);
Id sew_feynman(Algebra&, const model&, const std::vector<std::string>& process, const plan& diagram);
} // namespace hepcat::generation
