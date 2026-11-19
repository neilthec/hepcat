#pragma once

#include "reduce.hpp"

namespace hepcat {

using TensorCoordinates = std::map<Id, mpq_class>;

// Look for a lower-weight support in source + span(identities). Every returned
// coefficient is obtained by rational operations on the supplied identities.
TensorCoordinates reduceTensorSupport(const TensorCoordinates& source,
    const std::vector<TensorCoordinates>& identities,
    const std::map<Id, std::size_t>& weights);

} // namespace hepcat
