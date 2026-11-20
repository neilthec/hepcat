// Middle-photon exchange between two identical Dirac pairs. The kernel uses
// three photons of a common helicity. First subtract its image under the joint
// reversal (a,b)(c,d). Sum over photon permutations and alternate
// separately over particles (a,c) and antiparticles (b,d). The caller supplies
// the signed product of the five minimal Abelian vertices.
#pragma once
#include "reduce.hpp"

namespace hepcat {
Id chiral_radiation_kernel(Algebra&, int a, int b, int c, int d, const std::vector<int>& photons, Id mass,
                           bool square, bool physical);
}
