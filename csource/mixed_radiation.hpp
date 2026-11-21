#pragma once
#include "reduce.hpp"

namespace hepcat {
// One charged-line orientation with two like-helicity photons i,j and the
// opposite helicity k. The caller supplies the signed group and couplings.
Id mixed_radiation_kernel(Algebra&, int a, int b, int c, int d, int i, int j, int k,
                          Id mass, bool majority_square, bool physical);
}
