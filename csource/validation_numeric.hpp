#pragma once
#include "reduce.hpp"
#include <array>
#include <complex>
#include <functional>
#include <map>
#include <memory>

namespace hepcat {
class model;
}

namespace hepcat::validation {
using real = long double;
using complex = std::complex<real>;
using spinor = std::array<complex, 2>;
using four_vector = std::array<real, 4>;
using parameters = std::map<std::string, complex>;
namespace precision {
struct number;
struct phase_point;
} // namespace precision
using wide_parameters = std::map<std::string, precision::number>;

parameters model_parameters(const model &, const parameters &overrides = {});
complex evaluate_scalar(const Algebra &, Id, const parameters &);

// Physical 2 -> n-2 chart. All momenta stored incoming (outgoing energies negative).
// Arrays use zero-based storage; external labels in amplitude expressions start at one.
struct point {
    std::vector<four_vector> momenta;
    std::vector<real> masses;
    std::vector<spinor> angles, squares;
    std::vector<std::array<spinor, 2>> angle_basis, square_basis; // Columns, upper indices
    std::uint64_t phase_seed = 0, spin_seed = 0, reference_seed = 0, color_seed = 0;
    spinor reference(std::uint64_t label, bool angle) const;
    // External scalar products, including crossing signs (row-major).
    std::vector<real> scalar_products;
    void change_spins(std::uint64_t seed);
    void change_references(std::uint64_t seed) { reference_seed = seed; }
    void change_colors(std::uint64_t seed) { color_seed = seed; }
};
point sample_point(const std::vector<real> &masses, std::uint64_t seed);

// Compiles the kinematic DAG once. Concurrent calls use independent workspaces.
// Give the signed model color representations in external particle order.
class expression_evaluator {
    struct program;
    std::shared_ptr<const program> code_;

  public:
    expression_evaluator(const Algebra &, Id, const parameters &,
                         const std::vector<int> &color_representations = {},
                         const wide_parameters *exact = nullptr);
    complex operator()(const point &) const;
    // Sum |M|^2 over the two spin columns of each selected massive fermion.
    // Compile the spin dependencies once, then reuse the callable at each momentum.
    std::function<real(const point &)> spin_sum(const std::vector<std::size_t> &legs) const;
    // Replay the same phase-space draw and its amplitude in 160-bit GMP arithmetic.
    // Supply a shared reconstructed chart when comparing several amplitudes.
    complex precise(const point &) const;
    precision::number precise(const point &, const precision::phase_point &) const;
};
} // namespace hepcat::validation
