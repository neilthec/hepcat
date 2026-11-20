#pragma once
#include "validation_precision.hpp"
#include <map>

namespace hepcat::validation {
using wide_parameters = std::map<std::string, precision::number>;

// Decimal model inputs are parsed by the model reader as exact rational
// numbers. User overrides retain the value actually supplied by the caller.
wide_parameters precise_model_parameters(const model&, const parameters& overrides = {});
precision::number precise_scalar(const Algebra&, Id, const wide_parameters&);
} // namespace hepcat::validation
