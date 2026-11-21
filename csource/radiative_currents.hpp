// Exact endpoint radiation on minimal Abelian Dirac trees with several charged
// lines.
#pragma once

#include "continuations.hpp"
#include "generation_plan.hpp"
#include "model.hpp"
#include "reduce.hpp"
#include "validation_feynman.hpp"

#include <algorithm>
#include <map>
#include <set>
#include <tuple>
#include <unordered_map>

namespace hepcat::radiative_detail {
using labels = std::vector<int>;
using spin_components = std::map<int, Id>;

// A signed entry is twice the spin. Its sign distinguishes particle from
// antiparticle, or the two helicities of a massless vector.
inline std::vector<int> external_types(const model& m, const std::vector<std::string>& names) {
  std::vector<int> types;
  for (const auto& name : names) {
    const auto& p = m.particle(name);
    types.push_back(name == p.name ? p.spin2 : -p.spin2);
  }
  return types;
}

// Each amputated charged endpoint is a sum of two-spinors. A term carries
// either the original spinor or the helicity spinor of an attached photon.
class endpoint_radiation {
  Algebra &a;
  const std::vector<int> types;
  const std::map<Id, reference_choice> &gauge;
  std::map<std::tuple<int, bool, labels>, spin_components> waves;
  std::map<std::pair<int, int>, Id> orbitals, contacts;
  const Id one, zero, inv_sqrt2, mu;

  Id polarization(int photon) {
    return a.call("PolarizationVector", {a.integer(photon), mu, zero,
                                         a.integer(positive(photon) ? 1 : -1)});
  }

  Id contract(Id term) {
    return a.simplify(
        replace_external_references(a, feynman_to_spinors(a, term), gauge));
  }

  Id orbital(int p, int photon) {
    auto key = std::make_pair(p, photon);
    auto found = orbitals.find(key);
    if (found != orbitals.end())
      return found->second;
    Id momentum =
        a.call("LorentzMomentum", {a.call("Mom", {a.integer(p)}), mu});
    Id value = contract(
        a.mul({a.integer(2), inv_sqrt2, polarization(photon), momentum}));
    return orbitals.emplace(key, value).first->second;
  }

  Id seagull(int i, int j) {
    auto key = std::minmax(i, j);
    auto found = contacts.find(key);
    if (found != contacts.end())
      return found->second;
    Id value =
        contract(a.mul({a.integer(-1), polarization(i), polarization(j)}));
    return contacts.emplace(key, value).first->second;
  }

public:
  endpoint_radiation(Algebra &arena, const model &vertices,
                     const std::vector<std::string> &legs,
                     const std::map<Id, reference_choice> &choices)
      : endpoint_radiation(arena, external_types(vertices, legs), choices) {}
  endpoint_radiation(Algebra& arena, const std::vector<int>& leg_types,
                     const std::map<Id, reference_choice>& choices)
      : a(arena), types(leg_types), gauge(choices),
        one(a.integer(1)), zero(a.integer(0)),
        inv_sqrt2(a.parse("Power[2,Rational[-1,2]]")),
        mu(a.atom("radiationIndex")) {}

  bool positive(int i) const {
    return types.at(i-1) > 0;
  }

  Id spin(int leg, bool square) {
    bool photon = std::abs(types.at(leg-1)) == 2;
    return a.call("Spinor", {a.atom(photon ? "\"Helicity\"" : "\"Spin\""),
                             a.atom(square ? "\"Square\"" : "\"Angle\""),
                             a.integer(leg)});
  }

  Id propagator(labels side, Id mass) {
    std::sort(side.begin(), side.end());
    labels other;
    for (size_t i = 0; i < types.size(); ++i)
      if (!std::binary_search(side.begin(), side.end(), int(i + 1)))
        other.push_back(i + 1);
    if (other.size() < side.size() ||
        (other.size() == side.size() && other < side))
      side = std::move(other);
    std::vector<Id> args;
    for (int i : side)
      args.push_back(a.integer(i));
    return a.call(
        "PropDen",
        {a.call("Mom", {a.call("Multiparticle", std::move(args))}), mass});
  }

  using vector_current = std::map<std::pair<int, int>, Id>;

  // Sum all partitions of S between the particle and antiparticle before
  // contracting this conserved vector with its neighbour. Equal outgoing
  // spinors from distinct attachment orders then have one coefficient.
  vector_current current(int particle, int antiparticle,
                         const labels &photons) {
    std::map<std::pair<int, int>, std::vector<Id>> sums;
    auto partitions = [&](auto &self, size_t at, labels &left,
                          labels &right) -> void {
      if (at < photons.size()) {
        left.push_back(photons[at]);
        self(self, at + 1, left, right);
        left.pop_back();
        right.push_back(photons[at]);
        self(self, at + 1, left, right);
        right.pop_back();
        return;
      }
      for (bool square : {false, true})
        for (auto [i, u] : wave(particle, square, left))
          for (auto [j, v] : wave(antiparticle, !square, right)) {
            auto key = square ? std::make_pair(j, i) : std::make_pair(i, j);
            sums[key].push_back(a.mul({u, v}));
          }
    };
    labels left, right;
    partitions(partitions, 0, left, right);
    vector_current out;
    for (auto &[key, terms] : sums) {
      Id w = collect_scalar_factors(a, a.factorSum(a.add(std::move(terms))));
      if (w != zero)
        out.emplace(key, w);
    }
    return out;
  }

  Id contract_currents(const vector_current &left,
                       const vector_current &right) {
    std::vector<Id> terms;
    for (auto &[x, u] : left) {
      std::vector<Id> pieces;
      for (auto &[y, v] : right) {
        if (x.first == y.first || x.second == y.second)
          continue;
        auto ang =
            reduceSpinor(a, a.call("SpinorChain", {spin(x.first, false),
                                                   spin(y.first, false)}));
        auto sq =
            reduceSpinor(a, a.call("SpinorChain", {spin(x.second, true),
                                                   spin(y.second, true)}));
        pieces.push_back(a.mul({v, ang, sq}));
      }
      terms.push_back(a.mul({u, a.factorSum(a.add(std::move(pieces)))}));
    }
    return a.factorSum(a.add(std::move(terms)));
  }

  const spin_components &wave(int leg, bool square, const labels &photons) {
    auto key = std::make_tuple(leg, square, photons);
    if (auto hit = waves.find(key); hit != waves.end())
      return hit->second;
    if (photons.empty())
      return waves.emplace(key, spin_components{{leg, one}}).first->second;
    bool ket = positive(leg);
    std::map<int, std::vector<Id>> sums;
    // Move slash(P) through a polarization and apply the endpoint Dirac
    // equation. This gives 2P.epsilon, a rank-one Pauli insertion of the
    // active chirality, and an inverse propagator. The last piece cancels
    // the previous propagator: summing its two orders gives -2 eps_i.eps_j.
    // Each polarization here includes 1/sqrt(2) from the cubic normalization.
    for (size_t i = 0; i < photons.size(); ++i) {
      int photon = photons[i];
      labels rest = photons;
      rest.erase(rest.begin() + i);
      std::vector<Id> orbital_terms{orbital(leg, photon)};
      for (int earlier : rest)
        orbital_terms.push_back(orbital(earlier, photon));
      Id scalar = a.add(std::move(orbital_terms));
      if (!ket)
        scalar = a.mul({a.integer(-1), scalar});
      for (auto [end, coefficient] : wave(leg, square, rest)) {
        sums[end].push_back(a.mul({scalar, coefficient}));
        if (square == positive(photon) && end != photon) {
          Id left = spin(ket ? photon : end, square),
             right = spin(ket ? end : photon, square);
          sums[photon].push_back(
              a.mul({coefficient,
                     reduceSpinor(a, a.call("SpinorChain", {left, right}))}));
        }
      }
      for (size_t j = 0; j < i; ++j) {
        labels shorter = rest;
        shorter.erase(shorter.begin() + j);
        Id contact = seagull(photon, photons[j]);
        if (contact == zero)
          continue;
        for (auto [end, c] : wave(leg, square, shorter))
          sums[end].push_back(a.mul({contact, c}));
      }
    }
    labels cut = photons;
    cut.push_back(leg);
    Id inverse = a.pow(propagator(std::move(cut), a.masses()[leg - 1]), -1);
    spin_components result;
    for (auto &[end, terms] : sums) {
      Id scalar = a.mul({inverse, a.factorSum(a.add(std::move(terms)))});
      if (scalar != zero)
        result.emplace(end, scalar);
    }
    return waves.emplace(std::move(key), std::move(result)).first->second;
  }
};

// A rank-one x <ij> vertex is the on-shell projection of a conserved vector
// current. Summing both exchanged helicities sews it with the metric. A
// massive internal edge uses slash(P)+m. The tensor-to-spinor contraction
// is shared with the Feynman validator. Topologies, masses, signs and all
// couplings here are read exclusively from the constructive model.
class tree_current {
  Algebra &a;
  const model &m;
  const std::vector<std::string> &external;
  const generation::term &tree;
  std::map<Id, reference_choice> gauge;
  endpoint_radiation emit;
  std::map<int, labels> radiated;
  std::vector<bool> keep, kept_side;
  std::vector<Id> momenta, denominators;
  struct path {
    int bar, ket;
    std::vector<size_t> core;
  };
  std::vector<path> paths;
  const Id one, zero;

  Id lorentz(int port) { return a.atom("mu" + std::to_string(port)); }
  Id endpoint(const char *kind, int leg, bool one_chirality = false) {
    return a.call(kind,
                  {a.integer(leg), a.masses()[leg - 1], a.atom(one_chirality ? "\"R\"" : "\"Dirac\"")});
  }

  bool bare_leaf(const path &p) const {
    return p.core.size() == 1 && radiated.at(p.bar).empty() && radiated.at(p.ket).empty();
  }

  Id line(const path &p, bool project_leaves) {
    struct summand {
      Id factor;
      std::vector<Id> word;
    };
    std::vector<summand> words{{one, {}}};
    for (size_t j = 0; j < p.core.size(); ++j) {
      size_t v = p.core[j];
      const auto &ports = tree.vertices[v].slots;
      Id gamma = a.call("DiracGamma", {lorentz(ports[2])});
      for (auto &s : words)
        s.word.push_back(gamma);
      if (j + 1 == p.core.size())
        break;
      size_t edge_index = -1 - ports[0];
      const auto &edge = tree.edges[edge_index];
      Id mass = a.parse(m.particle(edge.particle_a).mass);
      Id sign = a.integer((v == edge.a) != kept_side[edge_index] ? 1 : -1);
      Id slash = a.call("DiracSlash", {momenta[edge_index]});
      const size_t count = words.size();
      for (size_t k = 0; k < count; ++k) {
        auto slashed = words[k];
        slashed.factor = a.mul({sign, slashed.factor});
        slashed.word.push_back(slash);
        words[k].factor = a.mul({mass, words[k].factor});
        words.push_back(std::move(slashed));
      }
    }
    std::vector<Id> terms;
    for (auto &s : words)
      terms.push_back(
          a.mul({s.factor,
                 a.call("DiracChain", {endpoint("FermionBar", p.bar, project_leaves && bare_leaf(p)),
                                       a.call("DiracWord", std::move(s.word)),
                                       endpoint("FermionKet", p.ket)})}));
    return a.add(std::move(terms));
  }

  Id dress(Id core) {
    std::unordered_map<Id, Id> cache;
    auto visit = [&](auto &self, Id id) -> Id {
      if (auto hit = cache.find(id); hit != cache.end())
        return hit->second;
      const auto &n = a.node(id);
      Id result = id;
      if (n.head == "SpinorChain") {
        int l = a.leg(a.node(n.args.front()).args[2]),
            r = a.leg(a.node(n.args.back()).args[2]);
        const bool sl =
            a.node(a.node(n.args.front()).args[1]).head == "\"Square\"";
        const bool sr =
            a.node(a.node(n.args.back()).args[1]).head == "\"Square\"";
        std::vector<Id> terms;
        for (auto [i, lv] : emit.wave(l, sl, radiated[l]))
          for (auto [j, rv] : emit.wave(r, sr, radiated[r])) {
            auto word = n.args;
            word.front() = emit.spin(i, sl);
            word.back() = emit.spin(j, sr);
            terms.push_back(a.mul(
                {lv, rv,
                 reduceSpinor(a, a.call("SpinorChain", std::move(word)))}));
          }
        result = a.factorSum(a.add(std::move(terms)));
      } else if (n.head == "Times" || n.head == "Plus" || n.head == "Power") {
        auto args = n.args;
        for (auto &arg : args)
          arg = self(self, arg);
        result = a.call(n.head, std::move(args));
      }
      return cache.emplace(id, result).first->second;
    };
    return visit(visit, core);
  }

public:
  using signature = std::vector<labels>;
  signature key;
  std::size_t orders = 1;

  tree_current(Algebra &arena, const model &vertices,
               const std::vector<std::string> &process,
               const generation::term &t, const std::map<int, int> &references)
      : a(arena), m(vertices), external(process), tree(t),
        emit(a, m, external, gauge), keep(t.vertices.size(), false),
        one(a.integer(1)), zero(a.integer(0)) {
    for (const auto &[i, ref] : references) {
      Id xi = a.call("Spinor",
                     {a.atom("\"Xi\""),
                      a.atom(emit.positive(i) ? "\"Angle\"" : "\"Square\""),
                      a.integer(i)});
      gauge.emplace(xi, reference_choice{i, std::abs(ref), ref < 0});
    }
    for (const auto &e : tree.edges) {
      labels other;
      for (size_t i = 0; i < external.size(); ++i)
        if (!std::binary_search(e.legs.begin(), e.legs.end(), int(i + 1)))
          other.push_back(i + 1);
      bool use = e.legs.size() < other.size() ||
                 (e.legs.size() == other.size() && e.legs < other);
      Id d = emit.propagator(e.legs, a.parse(m.particle(e.particle_a).mass));
      kept_side.push_back(use);
      denominators.push_back(d);
      momenta.push_back(a.node(d).args[0]);
    }
    for (size_t i = 0; i < tree.vertices.size(); ++i) {
      int bar = tree.vertices[i].slots[1];
      if (bar < 0)
        continue;
      std::vector<size_t> row;
      int ket = 0;
      for (size_t v = i; !ket;) {
        row.push_back(v);
        int port = tree.vertices[v].slots[0];
        if (port > 0)
          ket = port;
        else {
          const auto &e = tree.edges[-1 - port];
          v = v == e.a ? e.b : e.a;
        }
      }
      size_t begin = 0, end = row.size();
      auto photon = [&](size_t j) { return tree.vertices[row[j]].slots[2]; };
      while (begin < end && photon(begin) > 0)
        radiated[bar].push_back(photon(begin++));
      while (begin < end && photon(end - 1) > 0)
        radiated[ket].push_back(photon(--end));
      if (begin == end)
        throw std::logic_error(
            "Disconnected charged line in a multi-current tree");
      paths.push_back({bar, ket, {row.begin() + begin, row.begin() + end}});
      labels code{bar, ket};
      for (int leg : {bar, ket}) {
        auto &subset = radiated[leg];
        std::sort(subset.begin(), subset.end());
        for (size_t k = 2; k <= subset.size(); ++k)
          orders *= k;
        code.push_back(0);
        code.insert(code.end(), subset.begin(), subset.end());
      }
      code.push_back(0);
      for (size_t j = begin; j < end; ++j) {
        keep[row[j]] = true;
        int port = photon(j);
        // Edge numbers depend on the rooted plan. A canonical external
        // virtuality identifies the same exchange across all orderings.
        code.push_back(port > 0 ? port : -int(denominators[-1 - port]));
      }
      key.push_back(std::move(code));
    }
    std::sort(key.begin(), key.end());
  }

  // All poles between retained vertices. These distinguish the orders of
  // exchanged photons on the same charged line after its endpoint radiation
  // has already been summed.
  std::vector<Id> core_poles() const {
    std::vector<Id> poles;
    for (size_t i = 0; i < tree.edges.size(); ++i) {
      const auto &edge = tree.edges[i];
      if (keep[edge.a] && keep[edge.b])
        poles.push_back(denominators[i]);
    }
    std::sort(poles.begin(), poles.end());
    return poles;
  }

  std::vector<signed_exchange> leaf_exchanges() const {
    std::vector<signed_exchange> result;
    for (const auto &p : paths)
      if (bare_leaf(p)) result.push_back({{{p.bar,p.ket}},1});
    return result;
  }

  // Charge conjugation reverses a line of k vector insertions with sign
  // (-1)^(k-1). A single attachment has no separate reversed ordering.
  std::vector<signed_exchange> ordered_line_reversals() const {
    std::vector<signed_exchange> result;
    for (const auto &p : paths)
      if (p.core.size() > 1)
        result.push_back({{{p.bar, p.ket}}, p.core.size() % 2 ? 1 : -1});
    return result;
  }

  Id expression(Id coefficient, bool project_leaves = false) {
    size_t core_vertices = 0;
    int sign = 1;
    std::vector<Id> numerator, factors{coefficient};
    for (const auto &p : paths)
      numerator.push_back(line(p,project_leaves));
    for (size_t i = 0; i < tree.vertices.size(); ++i) {
      if (!keep[i])
        continue;
      ++core_vertices;
      int photon = tree.vertices[i].slots[2];
      if (photon > 0)
        numerator.push_back(a.call(
            "PolarizationVector", {a.integer(photon), lorentz(photon), zero,
                                   a.integer(emit.positive(photon) ? 1 : -1)}));
    }
    for (size_t k = 0; k < tree.edges.size(); ++k) {
      const auto &e = tree.edges[k];
      if (m.particle(e.particle_a).spin2 == 1)
        sign = -sign;
      if (keep[e.a] && keep[e.b])
        factors.push_back(a.pow(denominators[k], -1));
    }
    factors.push_back(a.integer(sign));
    factors.push_back(
        a.pow(a.parse("Power[2,Rational[-1,2]]"), int(core_vertices)));
    Id core = replace_external_references(
        a, feynman_to_spinors(a, a.mul(numerator)), gauge);
    factors.push_back(collect_scalar_factors(a, a.simplify(dress(core))));
    return a.mul(std::move(factors));
  }
};
} // namespace hepcat::radiative_detail
