// Direct evaluation of the manuscript's charged-line trees. This route
// keeps each fermion matching fixed while traversing the internal photons.
#pragma once
#include "validation_dirac_current.hpp"
#include <numeric>
namespace nine_point {
using namespace hepcat::validation;
using namespace hepcat::validation::dirac_current;
using edges = std::vector<std::pair<int, int>>;
struct port {
  vector polarization{};
  mask momentum{};
};
class tensors {
  const point &p;
  std::vector<int> species, bars, kets, photons, refs;
  gauge choice;
  bool project_ends;
  real mass, charge;
  std::vector<vector> momenta;
  std::vector<real> invariant;
  std::vector<mask> allocation;
  edges graph;
  mask branch(int here, int from) const {
    mask result = allocation[here] | (1u << bars[here]) | (1u << kets[here]);
    for (auto [v, w] : graph) {
      if (w == here)
        std::swap(v, w);
      if (v == here && w != from)
        result |= branch(w, here);
    }
    return result;
  }
  vector photon(int i, int v) const {
    const auto mode = choice == gauge::spurious && photons.size() == 1;
    auto eps =
        polarization(p, i, species[i], choice == gauge::spurious ? refs[i] : -1,
                     mode ? gauge::independent : choice, kets[v]);
    if (mode) {
      const auto &q = momenta[(1u << bars[v]) | (1u << kets[v])];
      const auto &k = momenta[1u << i];
      const auto shift = dot(q, eps) / dot(q, k);
      for (int j = 0; j < 4; ++j)
        eps[j] -= shift * k[j];
    }
    return eps;
  }
  // Contract one line with all photons on it. For an open virtual photon
  // insert its test four-vector successively along the four coordinate axes.
  complex line_word(int bar_leg, int ket_leg, const std::vector<port> &ports,
                    int anchor, int next, bool central) const {
    auto a = raise(p.angles[bar_leg]), b = p.squares[bar_leg];
    const vector bra{-a[0], -a[1], b[0], b[1]};
    a = p.angles[ket_leg];
    b = raise(p.squares[ket_leg]);
    const vector ket{a[0], a[1], -b[0], -b[1]};
    // Anticommute the endpoint momentum onto its external spinor when
    // a leaf carries one external and one exchanged photon (Eq. nine-leaf).
    if (anchor < 0 && ports.size() == 2) {
      for (int i = 0; i < 2; ++i) {
        mask k = ports[i].momentum;
        if (count(k) != 1 || std::abs(species[first(k)]) != 2)
          continue;
        const auto &eps = ports[i].polarization, &g = ports[1 - i].polarization;
        const auto a = ket_leg, b = bar_leg;
        const auto ta = 2 * p.scalar_products[a * species.size() + first(k)];
        const auto tb = 2 * p.scalar_products[b * species.size() + first(k)];
        auto field = [&](const vector &x) {
          return slash(momenta[k], slash(eps, x));
        };
        const auto gket = slash(g, ket);
        const auto orbital = 2.L * dot(momenta[1u << a], eps) / ta -
                             2.L * dot(momenta[1u << b], eps) / tb;
        return orbital * contract(bra, gket) +
               contract(bra, slash(g, field(ket))) / ta +
               contract(bra, field(gket)) / tb;
      }
    }
    const mask full = (1u << ports.size()) - 1;
    mask negative = 0;
    if (central)
      for (size_t k = 0; k < ports.size(); ++k)
        if (species[first(ports[k].momentum)] < 0)
          negative |= 1u << k;
    std::vector<vector> row(full + 1);
    std::vector<mask> side(full + 1, 1u << bar_leg);
    row[0] = bra;
    for (mask s = 1; s <= full; ++s) {
      side[s] = side[s & (s - 1)] | ports[first(s)].momentum;
      for (mask bits = s; bits; bits &= bits - 1) {
        const int k = first(bits);
        const mask before = s ^ (1u << k);
        if (k == next && !(before & (1u << anchor)))
          continue;
        auto previous = row[before];
        bool chiral = k == anchor && next < 0;
        if (central && count(before) == ports.size() / 2) {
          const auto left = count(before & negative);
          const auto right = count((full ^ s) & negative);
          if (left > right)
            continue;
          chiral = left == right;
        }
        if (chiral) {
          previous[2] = 0;
          previous[3] = 0;
        }
        const auto x = slash(ports[k].polarization, previous, true);
        for (int j = 0; j < 4; ++j)
          row[s][j] += x[j];
      }
      if (s != full) {
        const auto y = slash(momenta[side[s]], row[s], true);
        for (int j = 0; j < 4; ++j)
          row[s][j] =
              (mass * row[s][j] - y[j]) / (invariant[side[s]] - mass * mass);
      }
    }
    return contract(row[full], ket);
  }
  complex line(int v, const std::vector<port> &ports) const {
    int anchor = -1, next = -1;
    if (project_ends) {
      for (size_t i = 0; i < ports.size(); ++i) {
        if (count(ports[i].momentum) == 1)
          continue;
        if (anchor >= 0) {
          next = static_cast<int>(i);
          break;
        }
        anchor = static_cast<int>(i);
      }
    }
    const bool central = project_ends && anchor < 0 && ports.size() % 2;
    auto value = line_word(bars[v], kets[v], ports, anchor, next, central);
    if (anchor >= 0 || central) {
      const int sign = ports.size() % 2 ? 1 : -1;
      value += real(sign) * line_word(kets[v], bars[v], ports, anchor, next, central);
    }
    return value;
  }
  vector exchange(int v, int parent, mask parent_momentum) const {
    std::vector<port> ports;
    for (mask s = allocation[v]; s; s &= s - 1) {
      const int i = first(s);
      ports.push_back({photon(i, v), 1u << i});
    }
    for (auto [i, j] : graph) {
      if (j == v)
        std::swap(i, j);
      if (i != v || j == parent)
        continue;
      const auto side = branch(j, v);
      ports.push_back({exchange(j, v, side), side});
    }
    if (parent == -1)
      return {line(v, ports), 0, 0, 0};
    const mask full = momenta.size() - 1;
    ports.push_back({{}, full ^ parent_momentum});
    vector current{};
    for (int mu = 0; mu < 4; ++mu) {
      ports.back().polarization[mu] = mu == 0 ? 1 : -1;
      current[mu] = line(v, ports) / invariant[parent_momentum];
      ports.back().polarization[mu] = 0;
    }
    return current;
  }
  complex assignments(int i) {
    if (i == int(photons.size()))
      return exchange(0, -1, 0)[0];
    complex sum{};
    auto leg = 1u << photons[i];
    for (unsigned v = 0; v < allocation.size(); ++v) {
      allocation[v] ^= leg;
      sum += assignments(i + 1);
      allocation[v] ^= leg;
    }
    return sum;
  }

public:
  tensors(const point &chart, std::vector<int> types, real e, gauge g, bool project = false)
      : p(chart), species(std::move(types)), choice(g), project_ends(project), charge(e) {
    for (unsigned i = 0; i < species.size(); ++i) {
      if (species[i] == -1)
        bars.push_back(i);
      else if (species[i] == 1)
        kets.push_back(i);
      else
        photons.push_back(i);
    }
    refs.resize(species.size(), -1);
    for (auto i : photons) {
      auto ref = std::find_if(photons.begin(), photons.end(),
                              [&](int j) { return species[i] != species[j]; });
      if (ref == photons.end() && photons.size() > 1)
        ref = photons.begin() + (std::find(photons.begin(), photons.end(), i) -
                                 photons.begin() + 1) %
                                    photons.size();
      if (ref != photons.end())
        refs[i] = *ref;
    }
    mass = p.masses[kets.front()];
    allocation.resize(bars.size());
    momenta.resize(1u << species.size());
    invariant.resize(momenta.size());
    for (mask s = 1; s < momenta.size(); ++s) {
      auto i = first(s), r = s & (s - 1);
      for (int j = 0; j < 4; ++j)
        momenta[s][j] = momenta[r][j] + p.momenta[i][j];
      invariant[s] = invariant[r] + p.masses[i] * p.masses[i];
      for (mask b = r; b; b &= b - 1)
        invariant[s] += 2 * p.scalar_products[i * species.size() + first(b)];
    }
  }
  complex operator()() {
    const int pairs = bars.size();
    std::vector<edges> trees;
    if (pairs == 1)
      trees.push_back({});
    else {
      // Generate the distinct images of the manuscript's edge, path
      // and star. Simultaneous electron/positron swaps have even sign.
      std::vector<edges> shapes(1);
      for (int i = 1; i < pairs; ++i)
        shapes[0].emplace_back(i - 1, i);
      if (pairs == 4)
        shapes.push_back({{0, 1}, {0, 2}, {0, 3}});
      std::vector<int> perm(pairs);
      std::iota(perm.begin(), perm.end(), 0);
      do {
        for (const auto &shape : shapes) {
          edges image;
          for (auto [i, j] : shape)
            image.emplace_back(std::minmax(perm[i], perm[j]));
          std::sort(image.begin(), image.end());
          if (std::find(trees.begin(), trees.end(), image) == trees.end())
            trees.push_back(image);
        }
      } while (std::next_permutation(perm.begin(), perm.end()));
    }
    complex sum{};
    do {
      int sign = pairs % 2 ? -1 : 1;
      for (int i = 0; i < pairs; ++i)
        for (int j = 0; j < i; ++j)
          if (kets[i] < kets[j])
            sign = -sign;
      for (const auto &tree : trees) {
        graph = tree;
        sum += real(sign) * assignments(0);
      }
    } while (std::next_permutation(kets.begin(), kets.end()));
    return sum * std::pow(charge, int(species.size()) - 2);
  }
};
} // namespace nine_point
