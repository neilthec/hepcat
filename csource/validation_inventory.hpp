#pragma once
#include "amplitude.hpp"
#include "reduce.hpp"

namespace hepcat::validation_detail {
struct channel_obligation {
    numerator_shell shell;
    Id propagator, target;
    bool supplied, in_model;
};
struct cut_inventory {
    Id final;
    std::vector<channel_obligation> channels;
    std::vector<Id> other_denominators;
};
cut_inventory validation_inventory(Algebra&, const amplitude&, const amp_expression&);
} // namespace hepcat::validation_detail
