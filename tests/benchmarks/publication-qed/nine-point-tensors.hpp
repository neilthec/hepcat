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
  complex line(int v, const std::vector<port> &ports) const {
    auto a = raise(p.angles[bars[v]]), b = p.squares[bars[v]];
    const vector bra{-a[0], -a[1], b[0], b[1]};
    a = p.angles[kets[v]];
    b = raise(p.squares[kets[v]]);
    const vector ket{a[0], a[1], -b[0], -b[1]};
    const mask full = (1u << ports.size()) - 1;
    std::vector<vector> row(full + 1);
    std::vector<mask> side(full + 1, 1u << bars[v]);
    row[0] = bra;
    for (mask s = 1; s <= full; ++s) {
      side[s] = side[s & (s - 1)] | ports[first(s)].momentum;
      for (mask bits = s; bits; bits &= bits - 1) {
        const unsigned k = first(bits);
        const auto x = slash(ports[k].polarization, row[s ^ (1u << k)], true);
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
  tensors(const point &chart, std::vector<int> types, real e, gauge g)
      : p(chart), species(std::move(types)), choice(g), charge(e) {
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
