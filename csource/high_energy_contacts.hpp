#pragma once
#include "reduce.hpp"

namespace hepcat {
bool high_energy_local(const Algebra&, Id);
struct contact_catalog {
    std::vector<Id> polarization, minimal, extended;
};
contact_catalog high_energy_contact_catalog(Algebra&, Id);
} // namespace hepcat
