#pragma once
#include "reduce.hpp"

namespace hepcat {

struct six_radiation_terms {
    Id leaf, center;
};

// Three charged pairs in order (a,b), (x,y), (c,d). Sum the two left-pair
// orientations with a minus for leaf emission and a plus for center emission.
// Add the c,d image, then the reversed charged lines, then antisymmetrize
// the three particles and three antiparticles. Couplings multiply both terms.
six_radiation_terms six_radiation_kernel(Algebra&, const std::vector<int>& particles,
                                         const std::vector<int>& antiparticles, int photon,
                                         Id mass, bool square, bool physical);
}
