// Verify nine-point residues built from lower on-shell amplitudes.
// At a generic cut only terms containing that propagator can contribute.
// With a common independent photon reference the other denominators are
// brackets between an external photon and a reference, regular on a generic physical cut.
// The full intrinsic suite separately checks the cancellation of spurious
// poles. See constructive-reconstruction.md for arguments and conventions.
#include "generation_plan.hpp"
#include "generation_sew.hpp"
#include "residue.hpp"
#include <algorithm>
#include <filesystem>
#include <iostream>
#include <unordered_map>
using namespace hepcat;
unsigned bits(const std::vector<int> &labels) {
  unsigned b = 0;
  for (auto l : labels)
    b |= 1u << (l - 1);
  return b;
}
unsigned canon(unsigned b, unsigned all) {
  auto c = all ^ b;
  return std::make_pair(__builtin_popcount(b), b) <
                 std::make_pair(__builtin_popcount(c), c)
             ? b
             : c;
}
bool contains(const generation::term &t, const model &m, unsigned all,
              unsigned cut, bool massive) {
  if (massive) {
    for (const auto &edge : t.edges)
      if (canon(bits(edge.legs), all) == cut)
        return true;
    return false;
  }
  for (size_t v = 0; v < t.vertices.size(); ++v) {
    const auto &block = t.vertices[v];
    if (m.vertices()[block.row].legs.size() != 4)
      continue;
    auto slot = [&](int s) {
      if (s > 0)
        return 1u << (s - 1);
      const auto &edge = t.edges.at(-1 - s);
      const auto desc = bits(edge.legs);
      return edge.a == v ? all ^ desc : desc;
    };
    for (int j : {1, 2})
      if (canon(slot(block.slots[0]) | slot(block.slots[j]), all) == cut)
        return true;
  }
  return false;
}
int main(int argc, char **argv) try {
  if (argc < 5)
    throw std::invalid_argument("Use WORK helicities DRAWS CUT...");
  const auto root = std::filesystem::path(argv[1]);
  std::string hs = argv[2];
  if (hs != "+++" && hs != "++-")
    throw std::invalid_argument("Expected +++ or ++-");
  const auto draws = std::stoul(argv[3]);
  if (draws == 0 || draws > 96)
    throw std::invalid_argument("Use 1 to 96 exact draws");
  std::vector<std::string> legs{"e", "E", "e", "E", "e", "E"};
  for (char h : hs)
    legs.push_back(std::string("A.") + h);
  model blocks(root / "charged-blocks", 3, "qed-reconstruction"),
      lower(root / "models", 4, "qed-reconstruction");
  auto plans = generation::charged_block_plans(blocks, legs),
       targets = generation::channel_plans(lower, legs);
  const unsigned all = (1u << legs.size()) - 1;
  std::vector<std::string> ms;
  for (auto &l : legs)
    ms.push_back(lower.particle(l).mass);
  for (int k = 4; k < argc; ++k) {
    unsigned desired = 0;
    for (auto c : std::string(argv[k])) {
      if (c < '1' || c > '9' || (desired & (1u << (c - '1'))))
        throw std::invalid_argument("Invalid cut labels");
      desired |= 1u << (c - '1');
    }
    desired = canon(desired, all);
    // Only the designated cut is requested from the lower model. A pending
    // higher entry on a different cut cannot silently turn into a zero target.
    const auto it =
        std::find_if(targets.begin(), targets.end(), [&](const auto &plan) {
          return canon(bits(plan.terms.front().edges.front().legs), all) ==
                 desired;
        });
    if (it == targets.end())
      throw std::runtime_error("missing target for " + std::string(argv[k]));
    for (const auto &term : it->terms)
      for (const auto &v : term.vertices)
        if (lower.vertices()[v.row].pending_constructive_derivation)
          throw std::runtime_error("pending lower on this cut");
    const auto &edge = it->terms.front().edges.front();
    const auto mass = lower.particle(edge.particle_a).mass;
    Algebra cb(&blocks.algebra()), ct(&lower.algebra());
    std::vector<Id> bm, tm;
    for (auto &item : ms) {
      bm.push_back(cb.parse(item));
      tm.push_back(ct.parse(item));
    }
    cb.setKinematics(bm, {});
    ct.setKinematics(tm, {});
    std::vector<Id> contributions;
    for (auto &graph : plans)
      if (contains(graph.terms.front(), blocks, all, desired, mass != "0"))
        contributions.push_back(
            generation::sew(cb, blocks, legs, graph, false, false, false));
    unsigned included = contributions.size();
    if (!included)
      throw std::runtime_error("empty candidate pole");
    auto original = cb.add(std::move(contributions));
    std::cout << "cut " << argv[k] << " " << included << "/" << plans.size()
              << " sewings contribute" << std::endl;
    Id rhs = generation::sew(ct, lower, legs, *it, true, true, true);
    // The native channel carries z^-1. Removing the contour and multiplying
    // the result by -1 is the signed physical target used by
    // CompleteFactorization.
    std::unordered_map<Id, Id> cache;
    auto physical = [&](auto &self, Id id) -> Id {
      if (auto found = cache.find(id); found != cache.end())
        return found->second;
      const auto &node = ct.node(id);
      if (node.head == "ConstructiveDiagrams`z" && !node.compound)
        return ct.integer(1);
      if (!node.compound)
        return id;
      std::vector<Id> children;
      for (auto child : node.args)
        children.push_back(self(self, child));
      return cache[id] = ct.call(node.head, std::move(children));
    };
    rhs = ct.mul({ct.integer(-1), physical(physical, rhs)});
    amp_expression other(ct, rhs, ms);
    rhs = validation_detail::import_expression(cb, other);
    std::vector<int> side;
    for (unsigned j = 0; j < legs.size(); ++j)
      if (desired & (1u << j))
        side.push_back(j + 1);
    const numerator_shell shell{side, mass};
    unsigned completed = 0;
    for (unsigned j = 0; completed < draws && j < 96; ++j) {
      try {
        auto both =
            residue_components(cb, shell, {original, rhs}, 52900136 + j, {});
        if (both[0] != both[1]) {
          std::cerr << "FAIL " << argv[k] << " sample " << 52900136 + j << "\n";
          return 2;
        }
        if (std::any_of(both[0].begin(), both[0].end(),
                        [](const auto &x) { return x != 0; }))
          ++completed;
      } catch (const residue_singular &) {
      }
    }
    std::cout << argv[k] << " nonzero exact agreements " << completed
              << std::endl;
    if (completed < draws)
      return 3;
  }
  return 0;
} catch (const std::exception &e) {
  std::cerr << e.what() << "\n";
  return 1;
}
