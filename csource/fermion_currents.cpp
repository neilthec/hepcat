#include "fermion_currents.hpp"
#include "generation_plan.hpp"
#include "radiative_currents.hpp"
#include <algorithm>
#include <array>
#include <limits>
#include <map>
#include <set>

namespace hepcat {
namespace {
using labels = std::vector<int>;
using line = std::array<int, 2>;
struct emitted_line {
  line ends;
  labels attachments;
};
struct graph {
  std::vector<emitted_line> paths;
  Id coefficient;
};
bool same_kind(const std::string &h, const std::string &k) {
  return h.substr(h.rfind('`') + 1) == k;
}

// Spinor algebra for two open Dirac lines, also usable when reading a saved
// expression with only mass and spin information.
class line_kinematics {
public:
  Algebra& a;
  labels photons;
  Id integer(int n) { return a.integer(n); }
  Id prod(std::initializer_list<Id> x) { return a.mul(x); }
  Id sum(std::initializer_list<Id> x) { return a.add(x); }
  Id neg(Id x) { return prod({integer(-1), x}); }
  Id inv(Id x) { return a.pow(x, -1); }
  bool is_photon(int i) const {
    return std::find(photons.begin(), photons.end(), i) != photons.end();
  }
  Id spin(bool square, int leg) {
    return a.call("Spinor",
                  {a.atom(is_photon(leg) ? "\"Helicity\"" : "\"Spin\""),
                   a.atom(square ? "\"Square\"" : "\"Angle\""), integer(leg)});
  }
  Id chain(bool square, int i, std::initializer_list<int> momenta, int j) {
    if (i == j && momenta.size() == 0)
      return integer(0);
    std::vector<Id> v{spin(square, i)};
    for (int k : momenta)
      v.push_back(a.call("Mom", {integer(k)}));
    v.push_back(spin(square != (momenta.size() % 2 == 1), j));
    return a.call("SpinorChain", v);
  }
  Id pole(labels legs, Id mass) {
    std::sort(legs.begin(), legs.end());
    labels complement;
    for (size_t k = 1; k <= a.masses().size(); ++k)
      if (!std::binary_search(legs.begin(), legs.end(), int(k)))
        complement.push_back(int(k));
    if (complement.size() < legs.size() ||
        (complement.size() == legs.size() && complement < legs))
      legs = std::move(complement);
    std::vector<Id> args;
    for (int k : legs)
      args.push_back(integer(k));
    return a.call("PropDen",
                  {a.call("Mom", {a.call("Multiparticle", args)}), mass});
  }
  struct term {
    Id factor;
    int angle, square;
  };
  using pieces = std::vector<term>;
  Id fermion_pole(int leg, int photon) {
    return pole({leg, photon}, a.masses()[leg - 1]);
  }
  Id inv_pole(int leg, int photon) { return inv(fermion_pole(leg, photon)); }
  Id inv_triple(int leg, int i, int j) {
    return prod({inv(pole({leg, i, j}, a.masses()[leg - 1])), inv_pole(leg, i),
                 inv_pole(leg, j)});
  }
  pieces chiral(line pair, labels emitted, bool flip, bool physical) {
    auto [u, v] = pair;
    const int first = photons.front();
    const Id mass = a.masses()[u - 1];
    auto soft = [&](int x, int y, int i, bool split) {
      // Schouten combines the two soft emissions into a reference-independent
      // dipole.
      if (physical || split || i == first)
        return neg(
            prod({chain(!flip, i, {y, x}, i), inv_pole(x, i), inv_pole(y, i)}));
      return prod(
          {sum({prod({chain(!flip, i, {x}, first), inv_pole(x, i)}),
                neg(prod({chain(!flip, i, {y}, first), inv_pole(y, i)}))}),
           inv(chain(flip, i, {}, first))});
    };
    Id scalar = integer(1);
    for (int i : emitted)
      scalar = prod({scalar, soft(u, v, i, true)});
    if (emitted.size() == 2) {
      int i = emitted[0], j = emitted[1];
      scalar =
          sum({scalar,
               neg(prod({a.pow(mass, 2), a.pow(chain(!flip, i, {}, j), 2),
                         sum({inv_triple(u, i, j), inv_triple(v, i, j)})}))});
    }
    pieces out{{scalar, u, v}, {scalar, v, u}};
    for (int x : {u, v}) {
      int y = x == u ? v : u;
      for (int i : emitted) {
        Id shift = prod({chain(!flip, i, {}, x), inv_pole(x, i)});
        if (emitted.size() == 1 && x == v)
          shift = neg(shift);
        if (emitted.size() == 2) {
          int j = emitted.front() == i ? emitted.back() : emitted.front();
          shift = sum({prod({shift, soft(x, y, j, false)}),
                       prod({mass, a.pow(chain(!flip, i, {}, j), 2),
                             chain(flip, i, {}, x), inv_triple(x, i, j)})});
        }
        out.push_back({shift, y, i});
      }
    }
    return out;
  }
  explicit line_kinematics(Algebra& algebra, labels emitters = {})
      : a(algebra), photons(std::move(emitters)) {}
};

// Certify the two chiral halves of the same minimal vector coupling from
// every actual diagram. Extra scalar, contact, or nonminimal contributions
// must go through the general constructor.
class builder : public line_kinematics {
public:
  const model &m;
  const std::vector<std::string> &external;
  labels starts;
  std::map<size_t, bool> checked;
  builder(Algebra &algebra, const model &model,
          const std::vector<std::string> &particles)
      : line_kinematics(algebra), m(model), external(particles) {}
  bool minimal(size_t vi) {
    auto seen = checked.find(vi);
    if (seen != checked.end())
      return seen->second;
    const auto &v = m.vertices()[vi];
    if (v.legs.size() != 3)
      return false;
    const auto &f = m.particle(v.legs[0]);
    const auto &g = m.particle(v.legs[2]);
    if (f.name != v.legs[0] || f.antiname != v.legs[1] ||
        f.name == f.antiname || f.mass == "0" || f.spin2 != 1 || f.color != 1 ||
        g.mass != "0" || g.spin2 != 2 || g.color != 1)
      return false;
    bool sq = v.legs[2] == g.antiname;
    std::string t = sq ? "Square" : "Angle";
    auto expect = a.parse("Times[ConstructiveDiagrams`" +
                          std::string(sq ? "xTildeFactor" : "xFactor") +
                          "[1,2],SpinorChain[Spinor[\"Spin\",\"" + t +
                          "\",1],Spinor[\"Spin\",\"" + t + "\",2]]]");
    return checked
        .emplace(vi, a.simplify(a.parse(m.algebra().format(v.expression))) ==
                         a.simplify(expect))
        .first->second;
  }
  bool inventory() {
    for (size_t i = 0; i < external.size(); ++i) {
      auto &p = m.particle(external[i]);
      if (p.color != 1)
        return false;
      if (p.mass == "0" && p.spin2 == 2) {
        photons.push_back(i + 1);
        continue;
      }
      if (p.spin2 != 1 || p.mass == "0" || p.name == p.antiname)
        return false;
      if (external[i] == p.name)
        starts.push_back(i + 1);
    }
    if (starts.size() >= 2 && (!photons.empty() || starts.size() > 3))
      return external.size() == starts.size() * 2 + photons.size();
    return (starts.size() == 2 &&
            (photons.size() == 1 || photons.size() == 2) &&
            external.size() == 4 + photons.size()) ||
           (starts.size() == 3 && photons.empty() && external.size() == 6);
  }
  bool graph_of(const generation::plan &plan, graph &out) {
    if (plan.terms.size() != (size_t(1) << (starts.size() - 1)))
      return false;
    auto &tree = plan.terms.front();
    if (tree.vertices.size() != external.size() - 2 ||
        tree.edges.size() + 1 != tree.vertices.size())
      return false;
    std::set<labels> exchange_helicities;
    for (const auto &alt : plan.terms) {
      if (alt.vertices.size() != tree.vertices.size() || alt.sign != tree.sign)
        return false;
      labels assignment;
      for (size_t j = 0; j < alt.vertices.size(); ++j) {
        const auto &v = alt.vertices[j];
        const auto &first = tree.vertices[j];
        if (v.slots.size() != 3 || v.slots != first.slots || !minimal(v.row))
          return false;
        const auto &row = m.vertices()[v.row];
        const auto &orig = m.vertices()[first.row];
        if (row.legs[0] != orig.legs[0] || row.legs[1] != orig.legs[1] ||
            row.factor != orig.factor)
          return false;
        if (v.slots[2] > 0) {
          if (row.legs[2] != external[v.slots[2] - 1])
            return false;
        } else {
          if (m.particle(row.legs[2]).name != m.particle(orig.legs[2]).name)
            return false;
          assignment.push_back(row.legs[2] == m.particle(row.legs[2]).name);
        }
      }
      if (!exchange_helicities.insert(assignment).second)
        return false;
    }
    std::vector<Id> factors{integer(tree.sign)};
    for (const auto &v : tree.vertices)
      factors.push_back(
          a.parse(m.algebra().format(m.vertices()[v.row].factor)));
    out.coefficient = a.mul(factors);
    std::set<size_t> used;
    for (int s : starts) {
      emitted_line path{{s, 0}, {}};
      int port = s;
      do {
        auto it =
            std::find_if(tree.vertices.begin(), tree.vertices.end(),
                         [&](const auto &v) { return v.slots[0] == port; });
        if (it == tree.vertices.end() ||
            !used.insert(it - tree.vertices.begin()).second)
          return false;
        path.attachments.push_back(it->slots[2]);
        port = it->slots[1];
      } while (port < 0);
      if (!port || external[port - 1] != m.antiparticle(external[s - 1]))
        return false;
      path.ends[1] = port;
      out.paths.push_back(std::move(path));
    }
    return used.size() == tree.vertices.size();
  }
  Id six(line left, line center, line right) {
    const int a1 = left[0], b = left[1], x = center[0], y = center[1],
              c = right[0], d = right[1];
    Id m0 = a.masses()[x - 1];
    auto A = [&](int i, int j) { return chain(false, i, {}, j); };
    auto S = [&](int i, int j) { return chain(true, i, {}, j); };
    auto B = [&](int i, int k, int j) { return chain(true, i, {k}, j); };
    auto J = [&](int i, int j) {
      return sum({prod({A(j, d), S(i, c)}), prod({A(j, c), S(i, d)})});
    };
    auto sa = [&](int j) {
      return sum({prod({S(b, x), sum({prod({m0, S(a1, j)}), B(j, b, a1)})}),
                  prod({S(a1, x), sum({prod({m0, S(b, j)}), B(j, a1, b)})})});
    };
    auto aa = [&](int j) {
      return sum({prod({A(b, x), sum({prod({m0, A(a1, j)}), B(a1, b, j)})}),
                  prod({A(a1, x), sum({prod({m0, A(b, j)}), B(b, a1, j)})})});
    };
    return sum({prod({A(y, d), sa(c)}), prod({A(y, c), sa(d)}),
                prod({S(y, d), aa(c)}), prod({S(y, c), aa(d)}),
                neg(prod({sum({J(x, y), J(y, x)}),
                          sum({B(a1, x, b), B(b, x, a1)})}))});
  }
  Id contract(const pieces &l, const pieces &r, bool flip) {
    std::vector<Id> sum;
    for (auto &u : l)
      for (auto &v : r)
        sum.push_back(
            prod({u.factor, v.factor, chain(flip, u.angle, {}, v.angle),
                  chain(!flip, u.square, {}, v.square)}));
    return a.add(sum);
  }
  Id mixed(line left, line right, labels L, labels R, int plus, int minus,
           bool physical) {
    auto A = [&](int i, int j) { return chain(false, i, {}, j); };
    auto S = [&](int i, int j) { return chain(true, i, {}, j); };
    auto chi = [&](int i) { return chain(true, plus, {i}, minus); };
    auto soft = [&](line l, int h, bool cancel) {
      auto [i, j] = l;
      if (cancel)
        return prod({h == plus ? neg(chain(true, plus, {j, i}, plus))
                               : chain(false, minus, {j, i}, minus),
                     inv_pole(i, h), inv_pole(j, h)});
      return prod({sum({prod({chi(i), inv_pole(i, h)}),
                        neg(prod({chi(j), inv_pole(j, h)}))}),
                   inv(h == plus ? A(plus, minus) : S(plus, minus))});
    };
    auto current = [&](line l, labels shifted) {
      pieces out;
      for (int i : l) {
        int j = i == l[0] ? l[1] : l[0];
        int al = i, sq = j;
        Id coef = integer(1);
        for (int h : shifted) {
          auto k = h == plus ? j : i;
          Id bracket = h == plus ? S(plus, k) : neg(A(minus, k));
          coef =
              prod({coef, k == l[0] ? bracket : neg(bracket), inv_pole(k, h)});
          if (h == plus)
            sq = plus;
          else
            al = minus;
        }
        out.push_back({coef, al, sq});
      }
      return out;
    };
    auto remainder = [&](line l, line other) {
      std::vector<Id> terms;
      for (int i : l) {
        int j = i == l[0] ? l[1] : l[0];
        pieces cur{{chi(i), i, j},
                   {chi(i), j, i},
                   {neg(prod({S(plus, minus), A(minus, i)})), minus, j},
                   {prod({A(plus, minus), S(plus, i)}), j, plus}};
        terms.push_back(prod({chi(i), contract(cur, current(other, {}), false),
                              inv_triple(i, plus, minus)}));
      }
      return a.add(terms);
    };
    std::vector<Id> result;
    for (unsigned lmask = 0; lmask < (1u << L.size()); ++lmask)
      for (unsigned rmask = 0; rmask < (1u << R.size()); ++rmask) {
        bool cancel = physical || L.size() == 1 || (!lmask && !rmask);
        labels lshift, rshift;
        Id scalar = integer(1);
        for (size_t k = 0; k < L.size(); ++k)
          if (lmask & (1u << k))
            lshift.push_back(L[k]);
          else
            scalar = prod({scalar, soft(left, L[k], cancel)});
        for (size_t k = 0; k < R.size(); ++k)
          if (rmask & (1u << k))
            rshift.push_back(R[k]);
          else
            scalar = prod({scalar, soft(right, R[k], cancel)});
        result.push_back(
            prod({scalar, contract(current(left, lshift),
                                   current(right, rshift), false)}));
      }
    if (L.size() == 2)
      result.push_back(remainder(left, right));
    if (R.size() == 2)
      result.push_back(remainder(right, left));
    return a.add(result);
  }
};
using group_key = std::tuple<line, line, labels, labels>;
struct group {
  Id coefficient;
  Id propagator;
  std::set<std::pair<labels, labels>> orders;
};

Id contracted_radiation(line_kinematics& b, const std::vector<int>& types,
                        line l, line r, const labels& L, const labels& R, bool physical,
                        const std::function<bool(Id)>& accepts = {}) {
  auto& a = b.a;
  using current = radiative_detail::endpoint_radiation::vector_current;
  // Each of the two conserved currents can choose its own spin frames.
  // Include both physical endpoints or the distinct common-photon gauges.
  // Compare the complete contracted numerator, including cancellations.
  auto alternatives = [&](line side, const labels &photons) {
    std::vector<current> result;
    if (photons.size() <= 2 &&
        (photons.empty() ||
         std::all_of(photons.begin(), photons.end(), [&](int i) {
           return types[i-1] == types[photons.front()-1];
         }))) {
      const bool flip = !photons.empty() && types[photons.front()-1] < 0;
      current terms;
      for (const auto &t : b.chiral(side, photons, flip, true)) {
        auto coord = flip ? std::make_pair(t.square, t.angle)
                          : std::make_pair(t.angle, t.square);
        auto [where, inserted] = terms.emplace(coord, t.factor);
        if (!inserted)
          where->second = b.sum({where->second, t.factor});
      }
      result.push_back(std::move(terms));
      return result;
    }
    std::set<std::vector<int>> tried;
    const size_t gauges = physical ? 2 : b.photons.size() + 1;
    for (size_t choice = 0; choice < gauges; ++choice) {
      std::map<Id, reference_choice> gauge;
      std::vector<int> selected;
      for (int i : photons) {
        int spectator;
        if (physical)
          spectator = side[choice];
        else {
          auto position = std::find(b.photons.begin(), b.photons.end(), i) -
                          b.photons.begin();
          spectator = b.photons[(position + 1) % b.photons.size()];
          if (choice && b.photons[choice - 1] != i)
            spectator = b.photons[choice - 1];
          else if (!choice) {
            auto other = std::find_if(
                b.photons.begin(), b.photons.end(),
                [&](int j) { return types[j-1] != types[i-1]; });
            if (other != b.photons.end())
              spectator = *other;
          }
        }
        selected.push_back(spectator);
        bool pos = types[i-1] > 0;
        Id xi = a.call("Spinor", {a.atom("\"Xi\""),
                                  a.atom(pos ? "\"Angle\"" : "\"Square\""),
                                  a.integer(i)});
        gauge.emplace(xi, reference_choice{i, spectator, physical});
      }
      if (!tried.insert(selected).second)
        continue;
      radiative_detail::endpoint_radiation rad(a, types, gauge);
      result.push_back(rad.current(side[0], side[1], photons));
    }
    return result;
  };
  const auto lc = alternatives(l, L), rc = alternatives(r, R);
  const std::map<Id, reference_choice> no_gauge;
  radiative_detail::endpoint_radiation rad(a, types, no_gauge);
  std::optional<std::pair<size_t, size_t>> best;
  Id joined = b.integer(0);
  for (const auto &left : lc)
    for (const auto &right : rc) {
      auto val = rad.contract_currents(left, right);
      if (accepts) {
        const Id signed_value = b.photons.size() % 2 ? val : b.neg(val);
        if (accepts(signed_value)) return signed_value;
        continue;
      }
      // A comparison budget for presentation must never discard a correct
      // current at a new multiplicity. Prefer a measured score where
      // available and use the exact tree size to break ties.
      size_t weight = std::numeric_limits<size_t>::max();
      try {
        weight = expression_complexity(a, val);
      } catch (const std::length_error &) {
      } catch (const std::overflow_error &) {
      }
      auto score = std::make_pair(weight, a.cost(val));
      if (!best || score < *best) {
        best = score;
        joined = val;
      }
    }
  if (!(b.photons.size() % 2)) joined = b.neg(joined);
  return joined;
}

// Radiation assigned to either side of the exchanged photon is separately
// gauge invariant after all attachment orders on that side have been summed.
// Consequently equivalent allocations may have chosen different references.
std::optional<permutation_sum> exchange_orbits(builder& b, const std::map<group_key, group>& groups,
                                                const std::vector<Id>& terms, bool physical) {
  const auto n = b.photons.size();
  if (b.starts.size() != 2 || n < 3 || n >= std::numeric_limits<size_t>::digits-1 ||
      b.external[b.starts[0]-1] != b.external[b.starts[1]-1] ||
      !std::all_of(b.photons.begin(), b.photons.end(), [&](int i) {
        return b.external[i-1] == b.external[b.photons.front()-1];
      })) return {};
  // Two complete matchings and every binary assignment of photons in each.
  if (groups.size() != (size_t{2} << n) || terms.size() != groups.size()) return {};
  std::vector<Id> representatives(n/2+1);
  std::vector<size_t> counts(n/2+1);
  std::optional<Id> direct_coefficient;
  labels antiparticles;
  size_t index = 0;
  for (const auto& [key, allocation] : groups) {
    const auto& [left,right,L,R] = key;
    labels ends{left[1],right[1]};
    std::sort(ends.begin(),ends.end());
    if (antiparticles.empty()) antiparticles = ends;
    if (left[0] != b.starts[0] || right[0] != b.starts[1] || ends != antiparticles) return {};
    const Id oriented = left[1] < right[1] ? allocation.coefficient : b.neg(allocation.coefficient);
    if (!direct_coefficient) direct_coefficient = oriented;
    if (*direct_coefficient != oriented) return {};
    if (L.size()+R.size() != n) return {};
    const auto k = std::min(L.size(),R.size());
    const Id term = terms[index++];
    // With identical photons an orbit is specified by the smaller of the
    // two occupation numbers. Keep the shortest complete contracted term.
    if (!counts[k]++ || b.a.cost(term) < b.a.cost(representatives[k]))
      representatives[k] = term;
  }
  if (n == 3) {
    const int a = b.starts[0], photon = b.photons.front();
    const bool square = b.external[photon-1] == b.m.particle(b.external[photon-1]).name;
    return chiral_exchange_sum(b.a,b.starts,antiparticles,b.photons,b.a.masses()[a-1],
                               *direct_coefficient,square,physical);
  }
  mpz_class order = 4;  // two independent identical-fermion transpositions
  for (size_t i = 2; i <= n; ++i) order *= i;
  for (size_t k = 0; k < representatives.size(); ++k) {
    if (!counts[k]) return {};
    mpq_class inverse_stabilizer(mpz_class(counts[k]),order);
    inverse_stabilizer.canonicalize();
    representatives[k] = b.prod({representatives[k],b.a.exact(inverse_stabilizer)});
  }
  return permutation_sum{b.photons,b.a.add(representatives),{}, {},{b.starts,antiparticles}};
}
} // namespace

bool verify_radiation_exchange(Algebra& a, const labels& fermions, const labels& antifermions,
                               const labels& photons, bool square, bool physical,
                               const std::function<bool(Id,Id)>& accepts) {
  if (a.masses().size() != 7 || photons.size() != 3 || fermions.size() != 2 ||
      antifermions.size() != 2) return false;
  labels legs = photons;
  legs.insert(legs.end(),fermions.begin(),fermions.end());
  legs.insert(legs.end(),antifermions.begin(),antifermions.end());
  std::sort(legs.begin(),legs.end());
  for (std::size_t i=0; i<legs.size(); ++i)
    if (legs[i] != static_cast<int>(i+1)) return false;
  line_kinematics b(a,photons);
  std::vector<int> types(a.masses().size(), square ? 2 : -2);
  for (int i : fermions) types[i-1] = 1;
  for (int i : antifermions) types[i-1] = -1;
  const auto zero = b.integer(0);
  for (unsigned swap = 0; swap < 2; ++swap) {
    const line left{fermions[0],antifermions[swap]}, right{fermions[1],antifermions[1-swap]};
    for (unsigned mask = 0; mask < (1u << photons.size()); ++mask) {
      labels L,R;
      for (std::size_t i=0; i<photons.size(); ++i)
        (mask & (1u<<i) ? L : R).push_back(photons[i]);
      labels cut{left[0],left[1]};
      cut.insert(cut.end(),L.begin(),L.end());
      const Id pole = b.pole(cut,zero);
      auto matches = [&](Id current) {
        return accepts(pole,b.prod({swap ? b.neg(current) : current,b.inv(pole)}));
      };
      if (contracted_radiation(b,types,left,right,L,R,physical,matches) == zero) return false;
    }
  }
  return true;
}

std::optional<Id>
fermion_current_proposal(Algebra &a, const model &m,
                         const std::vector<std::string> &names, size_t count,
                         bool physical, std::optional<permutation_sum>* presentation) {
  if (presentation) presentation->reset();
  builder b(a, m, names);
  if (!b.inventory())
    return {};
  auto plans = generation::diagram_plans(m, names);
  if (plans.empty() || plans.size() != count)
    return {};
  if (b.starts.size() > 2 && (!b.photons.empty() || b.starts.size() > 3)) {
    std::map<std::vector<Id>, std::vector<Id>> attachments;
    struct orbit {
      size_t expected, count;
      Id coefficient;
    };
    std::map<radiative_detail::tree_current::signature, orbit> orbits;
    for (const auto &plan : plans) {
      graph g;
      if (!b.graph_of(plan, g))
        return {};
      std::map<int, int> refs;
      std::vector<Id> exchange;
      for (const auto &path : g.paths) {
        for (int k : path.attachments) {
          if (k < 0)
            continue;
          const auto it = std::find(b.photons.begin(), b.photons.end(), k);
          if (!physical && b.photons.size() > 1) {
            int ref = 0;
            for (int j : b.photons)
              if (names[j - 1] != names[k - 1]) {
                ref = j;
                break;
              }
            refs[k] = ref ? ref
                          : b.photons[(it - b.photons.begin() + 1) %
                                      b.photons.size()];
          } else {
            // Every ordering of the attachments on this conserved current
            // uses the same spectator. The emitting endpoint then has zero
            // orbital emission.
            refs[k] = -path.ends[0];
          }
        }
      }
      const auto &tree = plan.terms.front();
      for (const auto &e : tree.edges)
        if (m.particle(e.particle_a).spin2 == 2)
          exchange.push_back(b.pole(e.legs, b.integer(0)));
      std::sort(exchange.begin(), exchange.end());
      radiative_detail::tree_current current(a, m, names, tree, refs);
      auto [it, first] =
          orbits.emplace(current.key, orbit{current.orders, 0, g.coefficient});
      if (it->second.coefficient != g.coefficient ||
          it->second.expected != current.orders)
        return {};
      ++it->second.count;
      if (first)
        attachments[exchange].push_back(current.expression(g.coefficient));
    }
    std::vector<Id> terms;
    for (const auto &entry : orbits)
      if (entry.second.expected != entry.second.count)
        return {};
    for (auto &[k, rows] : attachments)
      terms.push_back(collect_scalar_factors(a, a.factorSum(a.add(rows))));
    return a.add(terms);
  }
  std::map<group_key, group> groups;
  std::map<std::pair<Id, Id>, std::vector<Id>> six_groups;
  for (auto &plan : plans) {
    graph graph;
    if (!b.graph_of(plan, graph))
      return {};
    auto &paths = graph.paths;
    if (paths.size() == 3) {
      auto it = std::find_if(paths.begin(), paths.end(),
                             [](auto &p) { return p.attachments.size() == 2; });
      if (it == paths.end())
        return {};
      auto side = [&](int edge) -> std::optional<line> {
        auto found = std::find_if(paths.begin(), paths.end(), [&](auto &p) {
          return p.attachments == labels{edge};
        });
        if (found == paths.end())
          return {};
        return found->ends;
      };
      auto l = side(it->attachments[0]), r = side(it->attachments[1]);
      if (!l || !r || *l == *r)
        return {};
      Id lp = b.pole({l->front(), l->back()}, a.integer(0)),
         rp = b.pole({r->front(), r->back()}, a.integer(0));
      Id ferm = b.pole({l->front(), l->back(), it->ends[0]},
                       a.masses()[it->ends[0] - 1]);
      six_groups[std::minmax(lp, rp)].push_back(b.neg(
          b.prod({graph.coefficient, b.six(*l, it->ends, *r), b.inv(ferm)})));
    } else {
      auto L = paths[0], R = paths[1];
      if (std::count_if(L.attachments.begin(), L.attachments.end(),
                        [](int i) { return i < 0; }) != 1 ||
          std::count_if(R.attachments.begin(), R.attachments.end(),
                        [](int i) { return i < 0; }) != 1)
        return {};
      auto emitted = [](labels s) {
        s.erase(std::remove_if(s.begin(), s.end(), [](int x) { return x < 0; }),
                s.end());
        std::sort(s.begin(), s.end());
        return s;
      };
      labels l = emitted(L.attachments), r = emitted(R.attachments), cut = l;
      cut.insert(cut.end(), L.ends.begin(), L.ends.end());
      Id prop = b.pole(cut, a.integer(0));
      auto key = std::make_tuple(L.ends, R.ends, l, r);
      auto [it, created] =
          groups.emplace(key, group{graph.coefficient, prop, {}});
      if (it->second.coefficient != graph.coefficient ||
          it->second.propagator != prop)
        return {};
      for (auto *row : {&L, &R})
        for (auto &k : row->attachments)
          if (k < 0)
            k = 0;
      if (!it->second.orders.emplace(L.attachments, R.attachments).second)
        return {};
    }
  }
  const auto types = radiative_detail::external_types(m,names);
  std::vector<Id> total;
  for (auto &[den, terms] : six_groups) {
    if (terms.size() != 2)
      return {};
    total.push_back(
        b.prod({a.add(terms), b.inv(den.first), b.inv(den.second)}));
  }
  for (auto &[key, g] : groups) {
    auto &[l, r, L, R] = key;
    auto ways = [](size_t n) {
      size_t result = 1;
      for (size_t k = 2; k <= n + 1; ++k)
        result *= k;
      return result;
    };
    if (g.orders.size() != ways(L.size()) * ways(R.size()))
      return {};
    if (b.photons.size() > 2) {
      Id joined = contracted_radiation(b,types,l,r,L,R,physical);
      total.push_back(b.prod({g.coefficient, joined, b.inv(g.propagator)}));
      continue;
    }
    int pos = 0, neg = 0;
    for (int i : b.photons) {
      if (names[i - 1] == m.particle(names[i - 1]).name)
        pos = i;
      else
        neg = i;
    }
    bool flip = !pos;
    Id num;
    if (!pos || !neg) {
      num = b.contract(b.chiral(l, L, flip, physical),
                       b.chiral(r, R, flip, physical), flip);
      if (b.photons.size() == 2)
        num = b.neg(num);
    } else
      num = b.mixed(l, r, L, R, pos, neg, physical);
    total.push_back(b.prod({g.coefficient, num, b.inv(g.propagator)}));
  }
  if (presentation && b.photons.size() > 2 && !groups.empty())
    *presentation = exchange_orbits(b,groups,total,physical);
  return a.add(total);
}
} // namespace hepcat
