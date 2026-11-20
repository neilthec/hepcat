#include "mixed_physical_current.hpp"
#include <algorithm>
#include <map>
#include <bitset>

namespace hepcat {
namespace {
using mask = unsigned;
using legs = std::vector<int>;
size_t first_member(mask members) {
    size_t i = 0;
    while ((members & (1u << i)) == 0)
        ++i;
    return i;
}
struct dirac_recursion {
    Algebra& a;
    int l, r;
    Id mass, g;
    legs vectors;
    std::vector<bool> minor;
    bool swapped;
    mask full;
    std::map<mask, std::map<legs, Id>> memo;
    Id zero() { return a.integer(0); }
    Id one() { return a.integer(1); }
    Id neg(Id x) { return a.mul({a.integer(-1), x}); }
    Id ep(bool sq, int i) {
        return a.call("Spinor", {a.atom(i == l || i == r ? "\"Spin\"" : "\"Helicity\""),
                                 a.atom(sq ? "\"Square\"" : "\"Angle\""), a.integer(i)});
    }
    Id chain(bool sq, int i, std::initializer_list<int> ks, int j) {
        if (i == j && !ks.size())
            return zero();
        std::vector<Id> w{ep(sq, i)};
        for (int k : ks)
            w.push_back(a.call("Mom", {a.integer(k)}));
        w.push_back(ep(sq != (ks.size() % 2 != 0), j));
        return a.call("SpinorChain", w);
    }
    Id pole(int end, mask set) {
        legs v{end};
        for (size_t j = 0; j < vectors.size(); ++j)
            if (set & (1u << j))
                v.push_back(vectors[j]);
        std::sort(v.begin(), v.end());
        std::vector<Id> w;
        for (int k : v)
            w.push_back(a.integer(k));
        return a.call("PropDen", {a.call("Mom", {a.call("Multiparticle", w)}), mass});
    }
    Id orbital(int idx, mask earlier) {
        int i = vectors[idx];
        bool q = minor[idx] ? swapped : !swapped;
        // Choose the shorter expression using total momentum conservation.
        // The projected polarization annihilates p_l and p_i.
        std::vector<Id> plus{chain(q, i, {l, r}, i)}, minus;
        for (size_t k = 0; k < vectors.size(); ++k)
            if (k != idx) {
                auto v = chain(q, i, {l, vectors[k]}, i);
                (earlier & (1u << k) ? plus : minus).push_back(v);
            }
        return plus.size() <= minus.size() ? a.add(plus) : neg(a.add(minus));
    }
    Id contact(int i, int j) {
        if (minor[i] == minor[j]) {
            bool q = minor[i] ? swapped : !swapped;
            return a.mul({a.pow(mass, 2), a.pow(chain(q, vectors[i], {}, vectors[j]), 2)});
        }
        if (!minor[i])
            std::swap(i, j);
        return a.pow(chain(swapped, vectors[i], {l}, vectors[j]), 2);
    }
    const std::map<legs, Id>& build(mask set) {
        auto hit = memo.find(set);
        if (hit != memo.end())
            return hit->second;
        if (!set)
            return memo.emplace(set, std::map<legs, Id>{{{}, one()}}).first->second;
        std::map<legs, std::vector<Id>> sums;
        for (size_t i = 0; i < vectors.size(); ++i)
            if (set & (1u << i)) {
                auto rest = set ^ (1u << i);
                auto weight = neg(orbital(i, rest));
                for (auto& [spins, current] : build(rest)) {
                    if (weight != zero())
                        sums[spins].push_back(a.mul({weight, current}));
                    if (minor[i]) {
                        legs q{vectors[i]};
                        q.insert(q.end(), spins.begin(), spins.end());
                        sums[q].push_back(a.mul({pole(l, 1u << i), current}));
                    }
                }
                for (size_t j = 0; j < i; ++j)
                    if (rest & (1u << j)) {
                        auto seagull = neg(contact(i, j));
                        for (auto& [spins, current] : build(rest ^ (1u << j)))
                            sums[spins].push_back(a.mul({seagull, current}));
                    }
            }
        Id den = one();
        if (set != full) {
            auto other = full ^ set;
            den = a.pow(std::bitset<sizeof(mask) * 8>(other).count() <
                                std::bitset<sizeof(mask) * 8>(set).count()
                            ? pole(l, other)
                            : pole(r, set),
                        -1);
        }
        std::map<legs, Id> results;
        for (auto& [key, terms] : sums)
            results.emplace(key, a.mul({a.add(terms), den}));
        return memo.emplace(set, std::move(results)).first->second;
    }
    Id grouped_one_minority() {
        const size_t hidx = vectors.size() - 1;
        int h = vectors[hidx];
        mask hm = 1u << hidx, majority = full ^ hm;
        auto endpoint = [&](size_t i) { return chain(!swapped, vectors[i], {l, r}, vectors[i]); };
        // Right-end orbital current in the left-end physical gauge. In the
        // two-emission current the determinant leaves one seagull remainder.
        auto scalar_current = [&](mask set) {
            std::vector<Id> product;
            legs two;
            for (size_t i = 0; i < hidx; ++i)
                if (set & (1u << i)) {
                    product.push_back(a.mul({endpoint(i), a.pow(pole(r, 1u << i), -1)}));
                    two.push_back(i);
                }
            Id leading = a.mul(product);
            if (two.size() != 2)
                return leading;
            int i = two[0], j = two[1];
            return a.add({leading, neg(a.mul({contact(i, j), pole(l, 1u << i), pole(l, 1u << j),
                                              a.pow(pole(r, set), -1), a.pow(pole(r, 1u << i), -1),
                                              a.pow(pole(r, 1u << j), -1)}))});
        };
        const Id spin = a.mul({chain(swapped, l, {}, h), chain(swapped, r, {}, h)}),
                 unit = chain(swapped, l, {}, r);
        auto mixed_terminal = [&](int end, int j) {
            Id chi = chain(swapped, h, {end}, j), insertion = a.mul({chain(!swapped, j, {}, h), spin});
            return a.mul({chi, a.add({a.mul({unit, chi}), end == l ? neg(insertion) : insertion})});
        };
        // Combine the orbital vertex and Pauli insertion either touching the
        // endpoint or immediately adjacent to a like-helicity seagull.
        Id spin_terminal = a.add(
            {a.mul({a.add({pole(l, hm), pole(r, hm)}), spin}), a.mul({unit, chain(swapped, h, {l, r}, h)})});
        std::vector<Id> terms;
        for (size_t i = 0; i < hidx; ++i)
            terms.push_back(a.mul({mixed_terminal(l, vectors[i]), scalar_current(majority ^ (1u << i))}));
        for (size_t j = 0; j < hidx; ++j)
            for (size_t k = j + 1; k < hidx; ++k) {
                mask rest = majority ^ (1u << j) ^ (1u << k);
                Id rhs = a.mul({spin_terminal, scalar_current(rest), a.pow(pole(r, hm), -1)});
                if (rest) {
                    auto i = first_member(rest);
                    // A further orbital attachment may lie on either side of the
                    // minority vertex. Shift its momentum through that vertex
                    // and collect the remainder over the routed fermion pole.
                    rhs = a.add({rhs, neg(a.mul({mixed_terminal(r, vectors[i]), pole(l, hm), pole(l, rest),
                                                 a.pow(pole(r, hm), -1), a.pow(pole(r, rest), -1),
                                                 a.pow(pole(l, majority ^ rest), -1)}))});
                }
                terms.push_back(a.mul({contact(j, k), rhs}));
            }
        std::vector<Id> pref{g, a.pow(mass, -1), a.add(terms)};
        for (size_t i = 0; i < vectors.size(); ++i)
            pref.push_back(a.pow(pole(l, 1u << i), -1));
        if (vectors.size() % 2)
            pref.push_back(a.integer(-1));
        return a.mul(pref);
    }
    // Combine the two orders of a correlated same-chirality pair before
    // attaching the other emissions. At most one such pair occurs here.
    Id connected_emissions(bool physical) {
      const size_t h_index = vectors.size() - 1;
      const int h = vectors[h_index], ref = vectors.front();
      auto x = [&](int end, int i) { return chain(swapped, h, {end}, i); };
      auto right_sigma = [&](int i, int j) {
        return chain(!swapped, i, {}, j);
      };
      auto inv = [&](Id d) { return a.pow(d, -1); };
      Id spin = a.mul({chain(swapped, l, {}, h), chain(swapped, r, {}, h)});
      Id unit = chain(swapped, l, {}, r);
      Id norm = physical ? pole(l, 1u << h_index) : right_sigma(h, ref);
      Id left_weight =
          physical ? spin : a.add({spin, a.mul({unit, x(l, ref), inv(norm)})});
      Id right_spin = physical ? chain(swapped, h, {l, r}, h) : neg(x(r, ref));
      Id right_weight = a.add({spin, a.mul({unit, right_spin, inv(norm)})});
      mask companions = full ^ (1u << h_index);
      auto unattached = [&](int end, mask set) {
        Id weight = end == l ? left_weight : right_weight;
        std::vector<Id> product{weight}, terms;
        std::vector<std::pair<Id, Id>> factors;
        for (size_t i = 0; i < h_index; ++i)
          if (set & (1u << i)) {
            int v = vectors[i];
            Id delta = a.mul({chain(!swapped, v, {r, l}, v),
                              inv(pole(l, 1u << i)), inv(pole(r, 1u << i))});
            Id projected = physical ? x(l, v) : right_sigma(v, ref);
            Id shift = a.mul({unit, projected, x(end, v), inv(norm),
                              inv(pole(end, 1u << i))});
            product.push_back(delta);
            factors.emplace_back(delta, shift);
          }
        terms.push_back(a.mul(product));
        for (size_t j = 0; j < factors.size(); ++j) {
          product = {factors[j].second};
          for (size_t i = 0; i < factors.size(); ++i)
            if (i != j)
              product.push_back(factors[i].first);
          terms.push_back(a.mul(product));
        }
        return a.add(terms);
      };
      std::vector<Id> result{unattached(l, companions)};
      for (size_t i = 0; i < h_index; ++i)
        for (size_t j = i + 1; j < h_index; ++j) {
          mask pair = (1u << i) | (1u << j), rest = companions ^ pair;
          for (int end : {l, r}) {
            const int other = end == l ? r : l;
            auto pair_factor = a.mul(
                {a.pow(mass, 2), a.pow(right_sigma(vectors[i], vectors[j]), 2),
                 inv(pole(end, 1u << i)), inv(pole(end, 1u << j)),
                 inv(pole(end, pair))});
            Id tail = unattached(other, rest);
            if (rest) {
              int k = vectors[first_member(rest)];
              std::vector<Id> insertions;
              for (auto p : {vectors[i], vectors[j]})
                insertions.push_back(
                    a.mul({right_sigma(p, k), chain(swapped, p, {end}, k)}));
              Id recoil = a.mul({a.add(insertions), inv(pole(end, rest)),
                                 inv(pole(end, companions))});
              tail = a.add(
                  {tail, a.mul({end == l ? right_weight : neg(left_weight),
                                recoil})});
            }
            result.push_back(neg(a.mul({pair_factor, tail})));
          }
        }
      return a.mul({g, inv(mass), a.add(result)});
    }
    // A cut at the middle of an ordered fermion line either separates two
    // two-emission currents or crosses a seagull. Sum both orders in each
    // two-emission current before performing the spinor contraction.
    Id midpoint(bool physical) {
      using current = std::map<int, Id>;
      auto square = [&](int i, int j) { return chain(!swapped, i, {}, j); };
      auto angle = [&](int i, int j) { return chain(swapped, i, {}, j); };
      auto inv = [&](Id x) { return a.pow(x, -1); };
      auto sandwich = [&](int i, int p, int j) {
        return chain(!swapped, i, {p}, j);
      };
      const auto first_negative = static_cast<size_t>(
          std::find(minor.begin(), minor.end(), true) - minor.begin());
      const auto last_negative = minor.size() - 1;
      auto x = [&](size_t i, int p) {
        const int v = vectors[i];
        if (!physical && i == last_negative)
          return a.mul(
              {sandwich(vectors[0], p, v), inv(square(vectors[0], v))});
        const int endpoint = (!physical && i == first_negative) ? r : l;
        if (p == endpoint)
          return zero();
        Id emission = minor[i] ? chain(swapped, v, {endpoint, p}, v)
                               : chain(!swapped, v, {p, endpoint}, v);
        return a.mul({emission, inv(pole(endpoint, 1u << i))});
      };
      auto polarization_contact = [&](size_t i, size_t j) {
        const int u = vectors[i], v = vectors[j], ref = vectors[0];
        Id di = inv(pole(l, 1u << i)), dj = inv(pole(l, 1u << j));
        if (physical || !minor[j]) {
          if (minor[i] == minor[j])
            return a.mul({a.pow(mass, 2),
                          a.pow(minor[j] ? angle(u, v) : square(u, v), 2), di,
                          dj});
          return neg(a.mul({a.pow(sandwich(u, l, v), 2), di, dj}));
        }
        if (!minor[i] && j == first_negative)
          return neg(a.mul({sandwich(u, l, v), sandwich(u, r, v), di,
                            inv(pole(r, 1u << j))}));
        if (!minor[i])
          return a.mul(
              {sandwich(u, l, v), square(u, ref), di, inv(square(ref, v))});
        return neg(a.mul({angle(u, v), chain(swapped, u, {r}, ref),
                          inv(pole(r, 1u << i)), inv(square(ref, v))}));
      };
      auto single = [&](int p, size_t i) {
        current v{{p, x(i, p)}};
        if (minor[i])
          v[vectors[i]] = neg(angle(vectors[i], p));
        return v;
      };
      auto pair = [&](int end, size_t i, size_t j) {
        if (minor[i] && !minor[j])
          std::swap(i, j);
        int I = vectors[i], J = vectors[j];
        Id di = pole(end, 1u << i), dj = pole(end, 1u << j),
           dij = pole(end, (1u << i) | (1u << j));
        Id xi = x(i, end), xj = x(j, end), den = a.mul({inv(di), inv(dj)});
        Id scalar = a.mul({xi, xj, dij});
        current v;
        if (minor[i] == minor[j]) {
          Id ij2 = a.pow(minor[i] ? angle(I, J) : square(I, J), 2);
          scalar = a.add({scalar, neg(a.mul({a.pow(mass, 2), ij2}))});
          if (minor[i]) {
            auto shift = [&](int k, Id spectator) {
              return a.mul(
                  {den, a.add({neg(a.mul({angle(k, end), spectator, dij})),
                               a.mul({mass, ij2, square(k, end)})})});
            };
            v[I] = shift(I, xj);
            v[J] = shift(J, xi);
          }
        } else {
          Id chi = chain(!swapped, I, {end}, J);
          scalar = a.add({scalar, a.pow(chi, 2)});
          v[J] = neg(
              a.mul({den, angle(J, end),
                     a.add({a.mul({xi, dij}), a.mul({square(I, J), chi})})}));
        }
        v[end] = a.mul({den, scalar});
        return v;
      };
      auto dot = [&](const current &left, const current &right) {
        std::vector<Id> terms;
        for (auto [i, lv] : left)
          for (auto [j, rv] : right)
            if (i != j)
              terms.push_back(a.mul({lv, rv, angle(i, j)}));
        return a.add(terms);
      };
      std::vector<Id> all;
      for (size_t i = 0; i < vectors.size(); ++i)
        for (size_t j = i + 1; j < vectors.size(); ++j) {
          mask chosen = (1u << i) | (1u << j), rest = full ^ chosen;
          size_t k = first_member(rest), t = first_member(rest ^ (1u << k));
          all.push_back(
              a.mul({dot(pair(l, i, j), pair(r, k, t)), inv(pole(l, chosen))}));
          Id c = polarization_contact(i, j);
          if (c != zero()) {
            auto regular = [&](size_t u, size_t v) {
              return a.mul({dot(single(l, u), single(r, v)),
                            inv(pole(l, 1u << u)), inv(pole(r, 1u << v))});
            };
            all.push_back(a.mul({c, a.add({regular(k, t), regular(t, k)})}));
          }
        }
      return neg(a.mul({g, inv(mass), a.add(all)}));
    }
    Id run() {
        std::vector<Id> terms, prefactors{g, a.pow(mass, -1)};
        for (size_t i = 0; i < vectors.size(); ++i)
            prefactors.push_back(a.pow(pole(l, 1u << i), -1));
        for (auto& [spins, val] : build(full)) {
            int prev = l;
            std::vector<Id> term{val};
            for (int i : spins) {
                term.push_back(chain(swapped, prev, {}, i));
                prev = i;
            }
            term.push_back(chain(swapped, prev, {}, r));
            terms.push_back(a.mul(term));
        }
        prefactors.push_back(a.add(terms));
        return neg(a.mul(prefactors));
    }
};
} // namespace
std::optional<Id> mixed_physical_current(Algebra& a, int l, int r, Id mass, const std::vector<int>& plus,
                                         const std::vector<int>& minus, Id coefficient) {
    if (plus.empty() || minus.empty() || plus.size() + minus.size() < 3 || plus.size() + minus.size() > 6)
        return {};
    bool swapped = minus.size() > plus.size();
    legs major = swapped ? minus : plus, minor = swapped ? plus : minus;
    std::vector<bool> chi(major.size(), false);
    for (int i : minor) {
        major.push_back(i);
        chi.push_back(true);
    }
    dirac_recursion d{a, l, r, mass, coefficient, major, chi, swapped, (1u << major.size()) - 1, {}};
    if (minor.size() == 1 && major.size() == 3)
      return d.connected_emissions(true);
    if (minor.size() == 1 && major.size() == 4)
      return d.grouped_one_minority();
    if (minor.size() == 2 && major.size() == 4)
      return d.midpoint(true);
    return d.run();
}
std::optional<Id> mixed_spurious_current(Algebra& a, int l, int r, Id mass, const std::vector<int>& plus,
                                         const std::vector<int>& minus, Id coefficient) {
  if (plus.empty() || minus.empty() || plus.size() + minus.size() < 3 ||
      plus.size() + minus.size() > 4)
    return {};
  bool swapped = minus.size() > plus.size();
  legs vectors = swapped ? minus : plus, other = swapped ? plus : minus;
  std::vector<bool> minor(vectors.size(), false);
  vectors.insert(vectors.end(), other.begin(), other.end());
  minor.resize(vectors.size(), true);
  dirac_recursion d{a,     l,           r,
                    mass,  coefficient, vectors,
                    minor, swapped,     (1u << vectors.size()) - 1,
                    {}};
  return other.size() == 1 ? d.connected_emissions(false) : d.midpoint(false);
}

} // namespace hepcat
