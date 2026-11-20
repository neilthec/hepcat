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
    Id contact(int i, int j) {
        if (minor[i] == minor[j]) {
            bool q = minor[i] ? swapped : !swapped;
            return a.mul({a.pow(mass, 2), a.pow(chain(q, vectors[i], {}, vectors[j]), 2)});
        }
        if (!minor[i])
            std::swap(i, j);
        return a.pow(chain(swapped, vectors[i], {l}, vectors[j]), 2);
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
};

// A second-order Dirac line is an ordered sequence of spin and orbital
// emissions and scalar contacts. Split it near its middle and contract
// the two short endpoint currents. Every allowed ordering occurs once.
struct balanced_line : dirac_recursion {
    using current = std::map<int, Id>;
    using cache_key = std::pair<bool, mask>;

    bool physical;
    mask minority = 0;
    std::map<cache_key, Id> remainders;
    std::map<cache_key, current> currents;

    balanced_line(dirac_recursion input, bool physical_gauge)
        : dirac_recursion(std::move(input)), physical(physical_gauge) {
        for (size_t i = 0; i < vectors.size(); ++i)
            if (minor[i]) minority |= 1u << i;
    }

    static size_t length(mask set) { return std::bitset<sizeof(mask) * 8>(set).count(); }

    int reference(size_t i) const {
        if (physical) return minor[i] ? r : l;
        return minor[i] ? vectors.front() : vectors.back();
    }

    bool chirality(size_t i) const { return minor[i] ? swapped : !swapped; }

    Id norm(size_t i) {
        return physical ? pole(reference(i), 1u << i) : chain(!chirality(i), reference(i), {}, vectors[i]);
    }

    Id projection(size_t i, int p) {
        if (p == vectors[i] || p == reference(i)) return zero();
        return physical ? chain(chirality(i), vectors[i], {reference(i), p}, vectors[i])
                        : chain(!chirality(i), reference(i), {p}, vectors[i]);
    }

    Id pair_weight(size_t i, size_t j) {
        if (physical) {
            if (minor[i] == minor[j]) return neg(contact(i, j));
            if (!minor[i]) std::swap(i, j);
            return neg(a.mul({chain(swapped, vectors[i], {reference(i)}, vectors[j]),
                              chain(swapped, vectors[i], {reference(j)}, vectors[j])}));
        }
        if (minor[i] == minor[j]) return zero();
        if (!minor[i]) std::swap(i, j);
        return a.mul({chain(!swapped, reference(i), {}, vectors[j]),
                      chain(swapped, vectors[i], {}, reference(j))});
    }

    Id orbital_weight(bool left, size_t i, mask rest) {
        std::vector<Id> direct, complement;
        auto append = [&](std::vector<Id>& terms, int p) {
            Id x = projection(i, p);
            if (x != zero()) terms.push_back(x);
        };
        append(direct, left ? l : r);
        append(complement, left ? r : l);
        for (size_t j = 0; j < vectors.size(); ++j)
            if (j != i) append(rest & (1u << j) ? direct : complement, vectors[j]);
        if (direct.size() <= complement.size())
            return left ? a.add(direct) : neg(a.add(direct));
        return left ? neg(a.add(complement)) : a.add(complement);
    }

    Id denominator(bool left, mask set) {
        if (length(set) > length(full ^ set)) return pole(left ? r : l, full ^ set);
        return pole(left ? l : r, set);
    }

    template<class F> std::vector<Id> each(mask set, F f) {
        std::vector<Id> terms;
        for (size_t i = 0; i < vectors.size(); ++i)
            if (set & (1u << i)) terms.push_back(f(i));
        return terms;
    }

    Id leading(bool left, mask set) {
        return a.mul(each(set, [&](size_t i) { return orbital_weight(left, i, 0); }));
    }

    // Subtract the product of the single emissions from a like-helicity
    // current. Each remaining source is a marked two-photon determinant,
    // since the projected polarization is transverse to its photon.
    Id determinant(bool left, mask set) {
        if (length(set) < 2) return zero();
        const auto key = std::make_pair(left, set);
        if (auto it = remainders.find(key); it != remainders.end()) return it->second;
        const int end = left ? l : r;
        std::vector<Id> terms;
        for (size_t i = 0; i < vectors.size(); ++i) {
            if (!(set & (1u << i))) continue;
            const mask rest = set ^ (1u << i);
            const Id di = pole(end, 1u << i);
            terms.push_back(a.mul({orbital_weight(left, i, rest), di, determinant(left, rest)}));
            for (size_t j = 0; j < i; ++j) {
                if (!(rest & (1u << j))) continue;
                const mask rest_pair = rest ^ (1u << j);
                terms.push_back(a.mul({norm(i), norm(j), a.pow(chain(chirality(i), vectors[i], {}, vectors[j]), 2),
                                      leading(left, rest_pair)}));
                terms.push_back(a.mul({pair_weight(i, j), di, pole(end, 1u << j),
                                      determinant(left, rest_pair)}));
            }
        }
        return remainders.emplace(key, a.mul({a.add(terms), a.pow(denominator(left, set), -1)})).first->second;
    }

    const current& extend(bool left, mask set) {
        const auto key = std::make_pair(left, set);
        if (auto it = currents.find(key); it != currents.end()) return it->second;
        if (!set) return currents.emplace(key, current{{left ? l : r, one()}}).first->second;
        if (length(set) >= 3 && !(set & minority)) {
            const int end = left ? l : r;
            Id scalar = a.add({leading(left, set), neg(a.mul({a.pow(mass, 2), determinant(left, set)}))});
            scalar = a.mul({scalar, a.pow(a.mul(each(set, [&](size_t i) { return pole(end, 1u << i); })), -1)});
            return currents.emplace(key, current{{end, scalar}}).first->second;
        }
        std::map<int, std::vector<Id>> sums;
        if (length(set) == 2) {
            const size_t i = first_member(set), j = first_member(set ^ (1u << i));
            const int end = left ? l : r;
            // The two scalar orderings and the contact can be written as
            // independent endpoint projections plus their field-strength
            // contraction. In the end's own gauge both projections vanish,
            // leaving just the contraction from the scalar contact.
            const auto di = pole(end, 1u << i), dj = pole(end, 1u << j);
            Id remainder;
            if (minor[i] == minor[j])
                remainder = a.mul({a.pow(mass, 2), a.pow(chain(chirality(i), vectors[i], {}, vectors[j]), 2)});
            else {
                const size_t m = minor[i] ? i : j, v = minor[i] ? j : i;
                remainder = a.pow(chain(swapped, vectors[m], {end}, vectors[v]), 2);
            }
            const auto leading = a.mul({projection(i, end), projection(j, end), pole(end, set)});
            const auto divide = a.mul({a.pow(di, -1), a.pow(dj, -1)});
            sums[end] = {a.mul({a.add({leading, neg(a.mul({norm(i), norm(j), remainder}))}), divide})};
            // Move the orbital spectator past one Pauli emission. For two
            // Pauli emissions Schouten and the massive endpoint equation
            // collapse their recoil to a single mass times a bracket.
            auto pauli = [&](size_t u, size_t v) {
                if (!minor[u]) return;
                const int U = vectors[u], V = vectors[v];
                const Id endpoint = chain(swapped, U, {}, end);
                Id shift;
                if (minor[v]) {
                    shift = a.mul({mass, norm(v), a.pow(chain(swapped, U, {}, V), 2),
                                   chain(!swapped, U, {}, end)});
                } else {
                    shift = neg(a.mul({endpoint, norm(v), chain(!swapped, V, {end}, U),
                                       chain(!swapped, U, {}, V)}));
                }
                sums[U] = {a.mul({norm(u), divide, a.add({shift,
                    neg(a.mul({endpoint, projection(v, end), pole(end, set)}))})})};
            };
            pauli(i, j);
            pauli(j, i);
        } else {
            for (size_t i = 0; i < vectors.size(); ++i) {
                mask bit = 1u << i;
                if (!(set & bit)) continue;
                mask rest = set ^ bit;
                Id weight = orbital_weight(left, i, rest);
                const auto& tail = extend(left, rest);
                std::vector<Id> spin;
                for (const auto& [end, v] : tail) {
                    if (weight != zero()) sums[end].push_back(a.mul({weight, v}));
                    if (minor[i])
                        spin.push_back(a.mul({v, left ? chain(swapped, end, {}, vectors[i])
                                                      : chain(swapped, vectors[i], {}, end)}));
                }
                if (!spin.empty()) sums[vectors[i]].push_back(a.mul({norm(i), a.add(spin)}));
                for (size_t j = 0; j < i; ++j) {
                    if (!(rest & (1u << j))) continue;
                    const Id w = pair_weight(i, j);
                    if (w == zero()) continue;
                    for (const auto& [end, v] : extend(left, rest ^ (1u << j)))
                        sums[end].push_back(a.mul({w, v}));
                }
            }
        }
        const Id inv = a.pow(denominator(left, set), -1);
        current result{{left ? l : r, zero()}};
        for (auto& [k, v] : sums) result[k] = a.mul({a.add(v), inv});
        return currents.emplace(key, std::move(result)).first->second;
    }

    Id dot(const current& u, const current& v) {
        std::vector<Id> terms;
        for (const auto& [i, x] : u)
            for (const auto& [j, y] : v)
                if (i != j) terms.push_back(a.mul({x, y, chain(swapped, i, {}, j)}));
        return a.add(terms);
    }

    Id join_emission(mask start, size_t i) {
        const mask end = full ^ start ^ (1u << i);
        const auto& left = extend(true, start);
        const auto& right = extend(false, end);
        const Id scalar = orbital_weight(true, i, start);
        if (minor[i] && (full & minority) == (1u << i)) {
            const Id spin = a.mul({norm(i), chain(swapped, l, {}, vectors[i]), chain(swapped, vectors[i], {}, r)});
            const Id vertex = a.add({spin, a.mul({scalar, chain(swapped, l, {}, r)})});
            return a.mul({vertex, left.at(l), right.at(r)});
        }
        Id result = a.mul({scalar, dot(left, right)});
        if (minor[i]) {
            current polarization{{vectors[i], one()}};
            result = a.add({result, a.mul({norm(i), dot(left, polarization), dot(polarization, right)})});
        }
        return result;
    }

    Id join_contact(mask start, size_t i, size_t j) {
        const Id weight = pair_weight(i, j);
        if (weight == zero()) return zero();
        const mask end = full ^ start ^ (1u << i) ^ (1u << j);
        return a.mul({weight, dot(extend(true, start), extend(false, end))});
    }

    Id run() {
        const size_t half = vectors.size() / 2;
        const bool odd = vectors.size() % 2;
        // Centering on the only Pauli emission leaves two purely orbital
        // tails. The crossed reference photons also kill every scalar contact
        // involving the central photon, so the determinant form stays small.
        const bool marked = !physical && length(minority) == 1;
        std::vector<Id> terms;
        for (mask start = 0; start < full; ++start) {
            const auto count = length(start);
            if (marked) {
                const auto h = first_member(minority);
                if (start & minority) continue;
                terms.push_back(join_emission(start, h));
                for (size_t j = 0; j < vectors.size(); ++j)
                    if (j != h && !(start & (1u << j))) terms.push_back(join_contact(start, h, j));
                continue;
            }
            if (count == half && odd) {
                for (size_t i = 0; i < vectors.size(); ++i)
                    if (!(start & (1u << i))) terms.push_back(join_emission(start, i));
            }
            if (count == half && !odd) {
                // Both currents carry the joining propagator. Amputate one.
                const mask end = full ^ start;
                const auto den = denominator(false, end);
                current amputated;
                for (const auto& [k, x] : extend(false, end)) amputated[k] = a.mul({den, x});
                terms.push_back(dot(extend(true, start), amputated));
            }
            if (count + 1 == half || (odd && count == half)) {
                for (size_t i = 0; i < vectors.size(); ++i)
                    if (!(start & (1u << i)))
                        for (size_t j = 0; j < i; ++j)
                            if (!(start & (1u << j))) terms.push_back(join_contact(start, i, j));
            }
        }
        std::vector<Id> prefactors{neg(g), a.pow(mass, -1), a.add(terms)};
        for (size_t i = 0; i < vectors.size(); ++i) prefactors.push_back(a.pow(norm(i), -1));
        return a.mul(prefactors);
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
    dirac_recursion d{a, l, r, mass, coefficient, major, chi, swapped, (1u << major.size()) - 1};
    if (minor.size() == 1 && major.size() == 3)
      return d.connected_emissions(true);
    if (minor.size() == 1 && major.size() == 4)
      return d.grouped_one_minority();
    if (minor.size() == 2 && major.size() == 4)
      return d.midpoint(true);
    return balanced_line(std::move(d), true).run();
}
std::optional<Id> mixed_spurious_current(Algebra& a, int l, int r, Id mass, const std::vector<int>& plus,
                                         const std::vector<int>& minus, Id coefficient) {
  if (plus.empty() || minus.empty() || plus.size() + minus.size() < 3 ||
      plus.size() + minus.size() > 6)
    return {};
  bool swapped = minus.size() > plus.size();
  legs vectors = swapped ? minus : plus, other = swapped ? plus : minus;
  std::vector<bool> minor(vectors.size(), false);
  vectors.insert(vectors.end(), other.begin(), other.end());
  minor.resize(vectors.size(), true);
  dirac_recursion d{a,     l,           r,
                    mass,  coefficient, vectors,
                    minor, swapped,     (1u << vectors.size()) - 1};
  if (vectors.size() > 4) return balanced_line(std::move(d), false).run();
  return other.size() == 1 ? d.connected_emissions(false) : d.midpoint(false);
}

} // namespace hepcat
