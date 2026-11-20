#pragma once
#include "reduce.hpp"

namespace hepcat {
// Contract a raw, all-incoming CalcHEP tree to spinor chains. Every spinor
// uses the constructive external-state convention and internal lines stay off shell.
Id feynman_to_spinors(Algebra&, Id expression);
} // namespace hepcat
