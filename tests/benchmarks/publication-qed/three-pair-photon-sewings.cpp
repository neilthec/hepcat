// Construct the nine-point spinor amplitudes from the checked lower amplitudes.
// See constructive-reconstruction.md for compilation and use.
#include "generation_plan.hpp"
#include "generation_sew.hpp"
#include "model.hpp"
#include "reduce.hpp"
#include <algorithm>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <map>
#include <numeric>
#include <set>
using namespace hepcat;
namespace fs = std::filesystem;
using masks = std::vector<unsigned>;
unsigned canonical(unsigned x, unsigned all) {
  auto y = all ^ x;
  return std::make_pair(__builtin_popcount(x), x) <
                 std::make_pair(__builtin_popcount(y), y)
             ? x
             : y;
}
unsigned mask(const std::vector<int> &v) {
  unsigned x = 0;
  for (int k : v)
    x |= 1u << (k - 1);
  return x;
}
// Parity in the manuscript: in each four-fermion factor the order is
// a,d,b,c. Move all four (positron,electron) contractions in front of
// the six charge-sorted external fermions, preserving the leg labels.
int printed_parity(const model &m, const generation::term &tree) {
  using port = std::pair<bool, int>;
  std::vector<port> word, ordered;
  for (const auto &vertex : tree.vertices) {
    auto &row = m.vertices().at(vertex.row);
    for (bool anti : {false, true})
      for (size_t j = 0; j < row.legs.size(); ++j) {
        auto &part = m.particle(row.legs[j]);
        if (part.spin2 == 1 && (part.antiname == row.legs[j]) == anti)
          word.emplace_back(anti, vertex.slots[j]);
      }
  }
  for (size_t j = 0; j < tree.edges.size(); ++j) {
    const int id = -1 - static_cast<int>(j);
    ordered.emplace_back(true, id);
    ordered.emplace_back(false, id);
  }
  std::vector<port> survivors;
  for (auto entry : word)
    if (entry.second > 0)
      survivors.push_back(entry);
  std::sort(survivors.begin(), survivors.end());
  ordered.insert(ordered.end(), survivors.begin(), survivors.end());
  if (ordered.size() != word.size())
    throw std::logic_error("Unexpected charged slots");
  int sign = 1;
  for (size_t i = 0; i < word.size(); ++i) {
    const auto a = std::find(ordered.begin(), ordered.end(), word[i]);
    if (a == ordered.end())
      throw std::logic_error("Unknown slot");
    for (size_t j = 0; j < i; ++j)
      if (std::find(ordered.begin(), ordered.end(), word[j]) > a)
        sign = -sign;
  }
  return sign;
}
int main(int argc, char **argv) try {
  if (argc != 12)
    throw std::invalid_argument(
        "Use CHARGED_BLOCKS NEW_OUTPUT e E e E e E PHOTON PHOTON PHOTON");
  std::vector<std::string> legs(argv + 3, argv + argc);
  for (size_t i = 0; i < 6; ++i)
    if (legs[i] != (i % 2 ? "E" : "e"))
      throw std::invalid_argument("Expected three ordered charged pairs");
  for (size_t i = 6; i < 9; ++i)
    if (legs[i] != "A.+" && legs[i] != "A.-")
      throw std::invalid_argument("Expected three photon helicities");
  auto n = legs.size();
  if (n > 20)
    return 1;
  unsigned all = (1 << n) - 1;
  model m(argv[1], 3, "qed-reconstruction");
  auto plans = generation::charged_block_plans(m, legs);
  std::map<masks, std::size_t> keys;
  for (std::size_t g = 0; g < plans.size(); ++g) {
    masks k;
    auto &t = plans[g].terms;
    if (t.size() != 1)
      throw std::runtime_error("Unexpected internal-state multiplicity");
    if (t[0].edges.size() != 4 || t[0].sign != -printed_parity(m, t[0]))
      throw std::logic_error(
          "Printed fermion sign does not match the sewn expression");
    for (auto &e : t[0].edges)
      k.push_back(canonical(mask(e.legs), all));
    std::sort(k.begin(), k.end());
    if (!keys.emplace(k, g).second)
      throw std::runtime_error("duplicate assignment");
  }
  std::map<std::string, std::vector<int>> groups;
  for (size_t j = 0; j < n; ++j)
    groups[legs[j]].push_back(j);
  std::vector<int> identity(n);
  std::iota(identity.begin(), identity.end(), 0);
  std::vector<std::pair<std::vector<int>, int>> perms{{identity, 1}};
  for (auto &[species, ids] : groups) {
    std::vector<std::pair<std::vector<int>, int>> next;
    auto reorder = ids;
    do {
      int sign = 1;
      if (m.particle(species).spin2 % 2)
        for (size_t i = 0; i < reorder.size(); ++i)
          for (size_t j = 0; j < i; ++j)
            if (reorder[j] > reorder[i])
              sign = -sign;
      for (auto &[p, s] : perms) {
        auto t = p;
        for (size_t i = 0; i < ids.size(); ++i)
          t[ids[i]] = reorder[i];
        next.push_back({t, s * sign});
      }
    } while (std::next_permutation(reorder.begin(), reorder.end()));
    perms = std::move(next);
  }
  fs::path dir = argv[2];
  if (!fs::create_directories(dir))
    throw std::runtime_error("Choose a new output directory");
  std::ofstream info(dir / "orbits.tsv");
  info << "seed\tgroup_order\tstabilizer_order\tdistinct_sewings\tcharged_"
          "cuts\n";
  std::set<masks> done;
  int seeds = 0;
  for (auto &[k, g] : keys) {
    if (done.count(k))
      continue;
    ++seeds;
    std::map<masks, std::pair<std::vector<int>, int>> orbit;
    for (auto &[p, sgn] : perms) {
      masks b;
      for (auto v : k) {
        unsigned out = 0;
        for (size_t j = 0; j < n; ++j)
          if (v & (1 << j))
            out |= 1u << p[j];
        b.push_back(canonical(out, all));
      }
      std::sort(b.begin(), b.end());
      if (!keys.count(b))
        throw std::runtime_error("missing image");
      orbit.emplace(b, std::make_pair(p, sgn));
    }
    for (auto &x : orbit)
      done.insert(x.first);
    Algebra a(&m.algebra());
    std::vector<Id> masses;
    for (auto &l : legs)
      masses.push_back(a.parse(m.particle(l).mass));
    a.setKinematics(masses, {});
    auto root = generation::sew(a, m, legs, plans[g], false, false, false);
    std::ofstream raw(dir / ("seed-" + std::to_string(seeds) + ".expr"));
    raw << a.format(root);
    std::ofstream permutations(
        dir / ("seed-" + std::to_string(seeds) + ".permutations"));
    for (const auto &[b, p] : orbit) {
      permutations << p.second;
      for (auto j : p.first)
        permutations << " " << j + 1;
      permutations << "\n";
    }
    info << seeds << "\t" << perms.size() << "\t" << perms.size() / orbit.size()
         << "\t" << orbit.size() << "\t";
    for (auto v : k) {
      info << "(";
      for (size_t j = 0; j < n; ++j)
        if (v & (1 << j))
          info << j + 1;
      info << ")";
    }
    info << "\n";
  }
  std::cout << seeds << " on-shell expression seeds covering " << done.size()
            << " of " << plans.size() << " charged sewings\n";
  if (done.size() != 4536 || done.size() != plans.size())
    throw std::logic_error("Incomplete set of labeled sewings");
  std::ofstream manifest(dir / "checked-order.txt");
  for (const auto &leg : legs)
    manifest << leg << " ";
  manifest << "\n" << seeds << " " << done.size() << "\n";
  manifest.close();
  if (!manifest)
    throw std::runtime_error("Cannot write seed inventory");
  return 0;
} catch (const std::exception &e) {
  std::cerr << e.what() << "\n";
  return 1;
}
