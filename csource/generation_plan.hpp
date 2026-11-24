#pragma once
#include <cstddef>
#include <string>
#include <vector>

namespace hepcat {
class model;
namespace generation {
struct vertex {
    std::size_t row;        // index into model.vertices()
    std::vector<int> slots; // model order: external leg 1..n or -(edge index + 1)
};
struct edge {
    // legs are external descendants incident on vertex a. In the momentum
    // flowing into a, this edge therefore denotes the complementary subset.
    std::vector<int> legs;
    std::size_t a, b;
    std::string particle_a, particle_b;
    std::size_t upper; // vertex with upper spin indices
};
struct term {
    std::vector<vertex> vertices;
    std::vector<edge> edges;
    int sign = 1; // external fermion permutation only
};
struct plan {
    std::vector<term> terms; // coherent internal-species/helicity alternatives
};
std::vector<plan> diagram_plans(const model&, const std::vector<std::string>& process);
// Restrict the internal lines to Dirac states when composing complete on-shell blocks.
std::vector<plan> charged_block_plans(const model&, const std::vector<std::string>& process);
std::vector<plan> channel_plans(const model&, const std::vector<std::string>& process);
} // namespace generation
} // namespace hepcat
