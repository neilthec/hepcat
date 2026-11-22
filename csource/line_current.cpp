#include "line_current.hpp"
#include "generation_plan.hpp"
#include "mixed_physical_current.hpp"
#include <algorithm>
#include <limits>
#include <map>
#include <set>
#include <tuple>

namespace hepcat {
namespace {
using mask = unsigned;
class line {
public:
  Algebra &a;
  int left, right;
  std::vector<int> photons;
  std::vector<bool> square;
  Id mass, coefficient;
  mask full;
  bool primary; // true selects square spinors
  std::map<std::tuple<int, mask, mask>, Id> weights;
  bool symmetric_kernel = false;

  Id spin(bool sq, int leg) {
    return a.call(
        "Spinor",
        {a.atom(leg == left || leg == right ? "\"Spin\"" : "\"Helicity\""),
         a.atom(sq ? "\"Square\"" : "\"Angle\""), a.integer(leg)});
  }
  Id chain(bool sq, int i, std::initializer_list<int> momenta, int j) {
    if (i == j && momenta.size() == 0)
      return a.integer(0);
    std::vector<Id> word{spin(sq, i)};
    for (int leg : momenta)
      word.push_back(a.call("Mom", {a.integer(leg)}));
    word.push_back(spin(sq != (momenta.size() % 2 != 0), j));
    return a.call("SpinorChain", std::move(word));
  }
  Id pole(int end, mask subset) {
    std::vector<int> labels{end};
    for (unsigned i = 0; i < photons.size(); ++i)
      if (subset & (1u << i))
        labels.push_back(photons[i]);
    std::sort(labels.begin(), labels.end());
    std::vector<Id> ids;
    for (int leg : labels)
      ids.push_back(a.integer(leg));
    return a.call("PropDen",
                  {a.call("Mom", {a.call("Multiparticle", ids)}), mass});
  }
  Id inv(Id x) { return a.pow(x, -1); }
  Id minus(Id x) { return a.mul({a.integer(-1), x}); }
  template <class F> std::vector<Id> each(mask set, F f) {
    std::vector<Id> values;
    for (unsigned i = 0; i < photons.size(); ++i)
      if (set & (1u << i))
        values.push_back(f(1u << i, photons[i]));
    return values;
  }
  template <class F> std::vector<Id> pairs(mask set, F f) {
    std::vector<Id> values;
    for (unsigned i = 0; i < photons.size(); ++i)
      if (set & (1u << i))
        for (unsigned j = i + 1; j < photons.size(); ++j)
          if (set & (1u << j))
            values.push_back(f((1u << i) | (1u << j), photons[i], photons[j]));
    return values;
  }
  // Within an outer unnormalized Bose sum, choose one representative
  // of a symmetric subset and carry the number of equivalent choices.
  // Products of labeled denominators must still visit every member.
  template <class F> std::vector<Id> sum_members(mask set, F term) {
    if (!symmetric_kernel || !set)
      return each(set, term);
    const unsigned i = __builtin_ctz(set);
    return {a.mul({a.integer(bit_count(set)), term(1u << i, photons[i])})};
  }
  template <class F> std::vector<Id> sum_pairs(mask set, F term) {
    const auto n = bit_count(set);
    if (!symmetric_kernel || n < 2)
      return pairs(set, term);
    const unsigned i = __builtin_ctz(set), j = __builtin_ctz(set & (set - 1));
    return {a.mul({a.integer(n * (n - 1) / 2), term((1u << i) | (1u << j), photons[i], photons[j])})};
  }
  Id endpoint(int i) { return chain(!primary, i, {left, right}, i); }
  Id pair_square(int i, int j) { return a.pow(chain(!primary, i, {}, j), 2); }
  Id emission(int i, mask earlier) {
    return a.add(each(earlier, [&](mask, int j) {
      return a.mul({chain(!primary, i, {}, j), chain(primary, j, {left}, i)});
    }));
  }
  // Sum the coefficient of the determinant over its marked photon pair
  // first. Every smaller subset then has only one scalar correction.
  Id physical_correction(mask set) {
    if (!(set & (set - 1)))
      return a.integer(0);
    auto key = std::make_tuple(0, set, 0);
    if (auto it = weights.find(key); it != weights.end())
      return it->second;
    auto pair_numerator = [&](mask ij, int i, int j) {
      return a.mul({pair_square(i, j), a.mul(each(ij, [&](mask k, int) {
                      return pole(left, k);
                    }))});
    };
    auto projected = [&](mask i, int leg) {
      if (bit_count(full ^ set) <= bit_count(set))
        return emission(leg, full ^ set);
      return a.add({endpoint(leg), minus(emission(leg, set ^ i))});
    };
    auto terms = sum_pairs(set, [&](mask ij, int i, int j) {
      mask rest = set ^ ij;
      Id tail = a.mul(each(rest, [&](mask, int k) { return endpoint(k); }));
      if (rest && !(rest & (rest - 1))) {
        auto insertion = each(rest, [&](mask k, int leg) {
          return a.mul(
              {projected(k, leg), pole(right, k), inv(pole(right, ij))});
        });
        tail = a.add({tail, insertion.front()});
      }
      return a.mul({pair_numerator(ij, i, j), tail});
    });
    if (bit_count(set) >= 4) {
      auto singles = sum_members(set, [&](mask i, int leg) {
        return a.mul(
            {projected(i, leg), pole(right, i), physical_correction(set ^ i)});
      });
      auto pairs_correction = sum_pairs(set, [&](mask ij, int i, int j) {
        return minus(
            a.mul({a.pow(mass, 2), pair_square(i, j),
                   a.mul(each(ij, [&](mask k, int) { return pole(right, k); })),
                   physical_correction(set ^ ij)}));
      });
      terms.insert(terms.end(), singles.begin(), singles.end());
      terms.insert(terms.end(), pairs_correction.begin(),
                   pairs_correction.end());
    }
    return weights.emplace(key, a.mul({a.add(terms), inv(pole(right, set))}))
        .first->second;
  }
  static unsigned bit_count(mask set) {
    unsigned count = 0;
    for (; set; set &= set - 1)
      ++count;
    return count;
  }
  Id physical_current(mask set) {
    Id leading = a.mul(each(set, [&](mask, int i) { return endpoint(i); }));
    Id numerator = a.add(
        {leading, minus(a.mul({a.pow(mass, 2), physical_correction(set)}))});
    return a.mul({numerator, a.mul(each(set, [&](mask i, int) {
                    return inv(pole(right, i));
                  }))});
  }
  Id ratio(int ref, int i, int p) {
    if (p == ref || p == i)
      return a.integer(0);
    return a.mul(
        {chain(primary, ref, {p}, i), inv(chain(primary, ref, {}, i))});
  }
  Id spurious_leading(int ref, mask set) {
    return a.mul(each(set, [&](mask, int i) { return ratio(ref, i, left); }));
  }
  Id spurious_weight(int ref, mask set, mask pair) {
    auto key = std::make_tuple(ref, set, pair);
    if (auto it = weights.find(key); it != weights.end())
      return it->second;
    auto terms = sum_members(set ^ pair, [&](mask i, int leg) {
      auto direct =
          each(set ^ i, [&](mask, int p) { return ratio(ref, leg, p); });
      direct.push_back(ratio(ref, leg, left));
      auto complement =
          each(full ^ set, [&](mask, int p) { return ratio(ref, leg, p); });
      complement.push_back(ratio(ref, leg, right));
      return a.mul({pole(left, i),
                    complement.size() - 1 < direct.size()
                        ? minus(a.add(complement))
                        : a.add(direct),
                    spurious_weight(ref, set ^ i, pair)});
    });
    terms.push_back(spurious_leading(ref, set ^ pair));
    return weights.emplace(key, a.mul({a.add(terms), inv(pole(left, set))}))
        .first->second;
  }
  Id spurious_current(int ref, mask set) {
    auto correction = a.add(sum_pairs(
        set, [&](mask ij, int i, int j) { return a.mul({pair_square(i, j), spurious_weight(ref, set, ij)}); }));
    return a.mul(
        {a.add({spurious_leading(ref, set),
                minus(a.mul({a.pow(mass, 2), correction}))}),
         a.mul(each(set, [&](mask i, int) { return inv(pole(left, i)); }))});
  }
  // In a common photon gauge the scalar emission vertices contain a
  // single sandwich. An ordered pair anchors the end of the charged line;
  // the remaining photons occur in their order under the full Bose sum.
  Id ordered_equal_spurious() {
    const auto count = photons.size();
    const int ref = photons[count - 2], partner = photons.back();
    const mask ref_bit = 1u << (count - 2);
    std::vector<Id> factors{coefficient,
                            mass,
                            chain(primary, left, {}, right),
                            chain(!primary, ref, {}, partner),
                            inv(chain(primary, ref, {}, partner)),
                            inv(pole(right, ref_bit))};
    mask prefix = 0;
    for (size_t i = 0; i < count - 2; ++i) {
      const mask bit = 1u << i;
      const mask opposite = full ^ (prefix | bit | ref_bit);
      const bool use_right = bit_count(opposite) < bit_count(prefix);
      auto labels = each(use_right ? opposite : prefix, [&](mask, int leg) { return a.integer(leg); });
      labels.insert(labels.begin(), a.integer(use_right ? right : left));
      const Id momentum =
          a.call("Mom", {labels.size() == 1 ? labels.front() : a.call("Multiparticle", labels)});
      Id vertex = a.call("SpinorChain", {spin(primary, ref), momentum, spin(!primary, photons[i])});
      factors.push_back(use_right ? minus(vertex) : vertex);
      factors.push_back(inv(chain(primary, ref, {}, photons[i])));
      prefix |= bit;
      factors.push_back(
          inv(bit_count(prefix) <= bit_count(full ^ prefix) ? pole(left, prefix) : pole(right, full ^ prefix)));
    }
    return a.mul(factors);
  }

  Id equal(bool physical) {
    auto terms = sum_pairs(full, [&](mask ij, int i, int j) {
      if (physical)
        return a.mul({pair_square(i, j), physical_current(full ^ ij)});
      return a.mul({chain(!primary, i, {}, j), inv(chain(primary, i, {}, j)),
                    a.add(sum_members(ij, [&](mask k, int leg) {
                      return a.mul({spurious_current(leg, full ^ ij),
                                    inv(pole(right, k))});
                    }))});
    });
    if (physical && photons.size() % 2)
      coefficient = minus(coefficient);
    return a.mul(
        {coefficient, mass, chain(primary, left, {}, right), a.add(terms),
         physical ? a.mul(each(full,
                               [&](mask i, int) { return inv(pole(left, i)); }))
                  : a.integer(1)});
  }
  // In a common chiral gauge the opposite-helicity photon supplies the
  // only spin emission. Sum the scalar emissions on its two sides.
  Id one_opposite(mask common, int opp) {
    int ref = 0;
    each(common, [&](mask, int i) {
      if (!ref)
        ref = i;
      return a.integer(0);
    });
    auto x = [&](int i, int p) {
      return a.mul(
          {chain(!primary, i, {p}, opp), inv(chain(primary, i, {}, opp))});
    };
    auto reference = [&](int p) {
      return a.mul(
          {chain(!primary, ref, {p}, opp), inv(chain(!primary, ref, {}, opp))});
    };
    auto angle12 = chain(primary, left, {}, right);
    auto spin_term =
        a.mul({chain(primary, left, {}, opp), chain(primary, right, {}, opp)});
    auto plus_pair = [&](int p, mask ij, int i, int j) {
      auto geom = a.add({a.mul({x(i, p), x(j, p)}),
                         minus(a.mul({a.pow(mass, 2), pair_square(i, j),
                                      inv(pole(p, ij))}))});
      return a.mul({geom, a.mul(each(ij, [&](mask k, int) {
                      return inv(pole(p, k));
                    }))});
    };
    Id body;
    if (photons.size() == 4) {
      auto mixed = [&](int end, mask c, int leg, int sign, Id center) {
        auto sandwich = chain(!primary, leg, {end}, opp);
        Id first = a.mul({x(leg, end),
                          a.add({a.mul({angle12, reference(end)}),
                                 a.mul({a.integer(sign), spin_term})}),
                          center});
        Id second =
            a.mul({sandwich, a.add({a.mul({angle12, sandwich}),
                                    a.mul({a.integer(sign), spin_term,
                                           chain(!primary, leg, {}, opp)})})});
        return a.mul({a.add({first, second}), inv(pole(end, c)),
                      inv(pole(end, full ^ common))});
      };
      body = a.add(pairs(common, [&](mask ij, int i, int j) {
        return a.add(each(common ^ ij, [&](mask c, int leg) {
          return a.add({a.mul({plus_pair(left, ij, i, j),
                               mixed(right, c, leg, 1, pole(left, ij))}),
                        a.mul({plus_pair(right, ij, i, j),
                               mixed(left, c, leg, -1, pole(right, ij))})});
        }));
      }));
    } else {
      std::map<std::pair<int, mask>, Id> memo;
      auto scalar = [&](auto &self, int end, mask subset) -> Id {
        if (!subset)
          return a.integer(1);
        auto key = std::make_pair(end, subset);
        if (auto it = memo.find(key); it != memo.end())
          return it->second;
        auto pair = pairs(subset, [&](mask ij, int i, int j) {
          return plus_pair(end, ij, i, j);
        });
        if (pair.size() == 1)
          return pair.front();
        auto terms = each(subset, [&](mask i, int leg) {
          auto emissions =
              each(subset ^ i, [&](mask, int j) { return x(leg, j); });
          emissions.push_back(x(leg, end));
          return a.mul({self(self, end, subset ^ i), a.add(emissions)});
        });
        return memo.emplace(key, a.mul({a.add(terms), inv(pole(end, subset))}))
            .first->second;
      };
      std::vector<Id> terms;
      for (mask sub = common;; sub = (sub - 1) & common) {
        auto later = common ^ sub;
        auto vertices =
            each(sub, [&](mask, int leg) { return reference(leg); });
        vertices.push_back(reference(left));
        auto trailing = each(later, [&](mask, int) { return a.integer(-1); });
        terms.push_back(a.mul(
            {a.mul(trailing), scalar(scalar, left, sub),
             scalar(scalar, right, later),
             a.add({a.mul({angle12, a.add(vertices)}), minus(spin_term)})}));
        if (!sub)
          break;
      }
      body = a.add(terms);
    }
    return a.mul({photons.size() % 2 ? minus(coefficient) : coefficient,
                  inv(mass), body});
  }
  // Combining the two orderings with opposite chiralities gives a contracted
  // Dirac current times its momentum sandwich over the two distinct
  // propagators.
  Id mixed_pair() {
    int p = photons[square[0] ? 1 : 0], n = photons[square[0] ? 0 : 1];
    auto half = [&](int i, int j) {
      return a.mul({chain(true, i, {}, p), chain(false, j, {}, n)});
    };
    return minus(a.mul(
        {coefficient, a.add({half(left, right), half(right, left)}),
         chain(true, p, {left}, n), inv(pole(left, 1)), inv(pole(left, 2))}));
  }
};
} // namespace

std::optional<Id> dirac_line_proposal(Algebra &a, const model &vertices,
                                      const std::vector<std::string> &external,
                                      std::size_t count, bool physical, std::optional<permutation_sum>* presentation) {
  if (presentation) presentation->reset();
  int left = 0, right = 0;
  std::vector<int> photons;
  for (std::size_t i = 0; i < external.size(); ++i) {
    const auto &p = vertices.particle(external[i]);
    if (p.color != 1)
      return {};
    if (p.spin2 == 2 && p.mass == "0")
      photons.push_back(i + 1);
    else if (p.spin2 == 1 && p.mass != "0" && p.name != p.antiname) {
      int &end = external[i] == p.name ? left : right;
      if (end)
        return {};
      end = i + 1;
    } else
      return {};
  }
  if (!left || !right ||
      vertices.antiparticle(external[left - 1]) != external[right - 1] ||
      photons.size() < 2 || photons.size() > 7)
    return {};
  std::size_t orders = 1;
  for (std::size_t k = 2; k <= photons.size(); ++k)
    orders *= k;
  if (count != orders)
    return {};
  // Certify every topology and every ordered cubic factor against the model.
  // Additional contact, exchange or nonminimal couplings invalidate this
  // identity.
  std::set<std::vector<int>> seen;
  auto plans = generation::diagram_plans(vertices, external);
  if (plans.size() != orders)
    return {};
  const auto &m = vertices.algebra();
  std::map<int, std::pair<bool, Id>> factors;
  std::map<int, std::size_t> factor_rows;
  for (const auto &plan : plans) {
    if (plan.terms.size() != 1)
      return {};
    const auto &tree = plan.terms.front();
    if (tree.sign != 1 || tree.vertices.size() != photons.size() ||
        tree.edges.size() + 1 != photons.size())
      return {};
    int port = left;
    std::vector<int> order;
    while (port != right) {
      auto it = std::find_if(tree.vertices.begin(), tree.vertices.end(),
                             [&](const auto &v) {
                               return v.slots.size() == 3 && v.slots[0] == port;
                             });
      if (it == tree.vertices.end() || order.size() == photons.size())
        return {};
      const auto &row = vertices.vertices()[it->row];
      auto photon = it->slots[2];
      if (photon <= 0 || static_cast<std::size_t>(photon) > external.size() ||
          row.legs[0] != external[left - 1] ||
          row.legs[1] != external[right - 1] ||
          row.legs[2] != external[photon - 1] ||
          std::find(photons.begin(), photons.end(), photon) == photons.end())
        return {};
      auto [entry, fresh] = factor_rows.emplace(photon, it->row);
      if (entry->second != it->row)
        return {};
      if (fresh) {
        auto sq = external[photon - 1] ==
                  vertices.particle(external[photon - 1]).antiname;
        auto type = sq ? "Square" : "Angle";
        auto expected = a.parse("Times[ConstructiveDiagrams`" +
                                std::string(sq ? "xTildeFactor" : "xFactor") +
                                "[1,2],SpinorChain[Spinor[\"Spin\",\"" + type +
                                "\",1],Spinor[\"Spin\",\"" + type + "\",2]]]");
        auto shape = a.parse(m.format(row.expression));
        if (a.simplify(shape) != a.simplify(expected))
          return {};
        factors.emplace(photon,
                        std::make_pair(sq, a.parse(m.format(row.factor))));
      }
      port = it->slots[1];
      order.push_back(photon);
    }
    if (order.size() != photons.size() || !seen.insert(order).second)
      return {};
  }
  std::vector<bool> square;
  std::vector<Id> vertex_factors;
  for (int leg : photons) {
    square.push_back(factors.at(leg).first);
    vertex_factors.push_back(factors.at(leg).second);
  }
  return minimal_dirac_line(a, left, right, photons, square, a.mul(vertex_factors), physical, presentation);
}

// This expression is also a candidate for recognizing a saved amplitude.
// The caller must certify its topology or check exact algebraic equality.
std::optional<Id> minimal_dirac_line(Algebra& a, int left, int right, const std::vector<int>& photons,
                                     const std::vector<bool>& square, Id vertex_product, bool physical,
                                     std::optional<permutation_sum>* presentation) {
  if (photons.size() < 2 || photons.size() > 7 || square.size() != photons.size())
    return {};
  if (presentation)
    presentation->reset();
  line current{a,
               left,
               right,
               photons,
               square,
               a.masses()[left - 1],
               vertex_product,
               (1u << photons.size()) - 1,
               square.front(),
               {}};
  if (std::all_of(square.begin(), square.end(), [&](bool sq) { return sq == square.front(); })) {
    if (presentation && photons.size() > 6) {
      unsigned orders = 1;
      for (unsigned i = 2; i <= photons.size(); ++i)
        orders *= i;
      auto folded = current;
      folded.symmetric_kernel = true;
      *presentation =
          permutation_sum{photons, a.mul({a.pow(a.integer(orders), -1), folded.equal(physical)}), {}};
      if (!physical) {
        const auto ordered = current.ordered_equal_spurious();
        if (expression_complexity(a, ordered) < expression_complexity(a, (*presentation)->kernel))
          (*presentation)->kernel = ordered;
      }
    }
    return current.equal(physical || photons.size() == 2);
  }
  if (photons.size() == 2)
    return current.mixed_pair();
  std::vector<int> positive, negative;
  for (std::size_t i = 0; i < photons.size(); ++i)
    (square[i] ? negative : positive).push_back(photons[i]);
  const auto coefficient = current.coefficient;
  auto joined =
      physical ? physical_current_with_presentation(a, left, right, current.mass, positive, negative,
                                                    coefficient, presentation)
               : spurious_current_with_presentation(a, left, right, current.mass, positive, negative,
                                                    coefficient, photons.size() > 6 ? presentation : nullptr);
  if (joined) {
    if (presentation && !physical && !*presentation) {
      *presentation = ordered_minority_sum(a, left, right, current.mass, positive, negative, coefficient);
      if (!*presentation && photons.size()==6)
        *presentation = paired_weyl_sum(a, left, right, current.mass, positive, negative, coefficient, false);
    }
    if (presentation && photons.size() > 6) {
      // Compare exact projected forms on the same footing. The Ward
      // insertion applies to a single opposite helicity. With more than
      // one, the central Dirac vertex is an alternative to the Pauli split.
      const bool single = std::min(positive.size(), negative.size()) == 1;
      std::optional<permutation_sum> candidate;
      if (!single)
        candidate = paired_weyl_sum(a, left, right, current.mass, positive, negative, coefficient, physical);
      else if (!physical)
        candidate = ordered_minority_sum(a, left, right, current.mass, positive, negative, coefficient);
      if (candidate) {
        auto score = [&](permutation_sum& form) {
          form.kernel = reduce_projected_candidate(a, form.kernel, form);
          try {
            return expression_complexity(a, form.kernel);
          } catch (const std::length_error&) {
            return std::numeric_limits<std::size_t>::max();
          } catch (const std::overflow_error&) {
            return std::numeric_limits<std::size_t>::max();
          }
        };
        const auto weight = score(*candidate);
        if (!*presentation || weight < score(**presentation))
          *presentation = std::move(candidate);
      }
    }
    return joined;
  }
  if (physical)
    return {};
  {
    mask angles = 0;
    for (unsigned i = 0; i < square.size(); ++i)
      if (!square[i])
        angles |= 1u << i;
    for (mask set : {angles, current.full ^ angles}) {
      auto singleton = current.full ^ set;
      if (singleton && !(singleton & (singleton - 1))) {
        current.primary = set != angles;
        for (unsigned i = 0; i < photons.size(); ++i)
          if (singleton & (1u << i))
            return current.one_opposite(set, photons[i]);
      }
    }
  }
  return {};
}
} // namespace hepcat
