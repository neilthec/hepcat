#include "fermion_currents.hpp"
#include "generation_plan.hpp"
#include <algorithm>
#include <array>
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

// Certify the two chiral halves of the same minimal vector coupling from
// every actual diagram. Extra scalar, contact, or nonminimal contributions
// must go through the general constructor.
class builder {
public:
  Algebra &a;
  const model &m;
  const std::vector<std::string> &external;
  labels photons, starts;
  std::map<size_t, bool> checked;
  builder(Algebra &algebra, const model &model,
          const std::vector<std::string> &particles)
      : a(algebra), m(model), external(particles) {}
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
    for (size_t k = 1; k <= external.size(); ++k)
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
  struct term {
    Id factor;
    int angle, square;
  };
  using pieces = std::vector<term>;
  Id contract(const pieces &l, const pieces &r, bool flip) {
    std::vector<Id> sum;
    for (auto &u : l)
      for (auto &v : r)
        sum.push_back(
            prod({u.factor, v.factor, chain(flip, u.angle, {}, v.angle),
                  chain(!flip, u.square, {}, v.square)}));
    return a.add(sum);
  }
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
} // namespace

std::optional<Id>
fermion_current_proposal(Algebra &a, const model &m,
                         const std::vector<std::string> &names, size_t count,
                         bool physical) {
  builder b(a, m, names);
  if (!b.inventory())
    return {};
  auto plans = generation::diagram_plans(m, names);
  if (plans.empty() || plans.size() != count)
    return {};
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
  std::vector<Id> total;
  for (auto &[den, terms] : six_groups) {
    if (terms.size() != 2)
      return {};
    total.push_back(
        b.prod({a.add(terms), b.inv(den.first), b.inv(den.second)}));
  }
  for (auto &[key, g] : groups) {
    auto &[l, r, L, R] = key;
    auto ways = [](size_t n) { return n == 2 ? 6u : n == 1 ? 2u : 1u; };
    if (g.orders.size() != ways(L.size()) * ways(R.size()))
      return {};
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
  return a.add(total);
}
} // namespace hepcat
