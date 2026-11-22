#include "mixed_physical_current.hpp"
#include <algorithm>
#include <set>
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
    // Sum the insertions of the only opposite-helicity photon into a fixed
    // ordering of its companions. Their common chiral reference makes every
    // orbital numerator independent of the position of the insertion.
    Id ward_summand(bool fold_ends) {
        const int n = static_cast<int>(vectors.size()) - 1;
        const int h = vectors.back();
        auto prefix = [](int k) -> mask { return (mask{1} << k) - 1; };
        const mask companions = prefix(n);
        auto virtuality = [&](bool left, int k) {
            const mask before = prefix(k);
            return pole(left ? l : r,left ? before : companions ^ before);
        };
        // Q_k = p_l + p_0 + ... + p_(k-1). Its complementary routing
        // differs by -p_h, which annihilates the spinor at the right endpoint.
        auto sandwich = [&](int leg, int k) {
            legs before{l}, after{r};
            for (int j = 0; j < n; ++j)
                if (vectors[j] != leg) (j < k ? before : after).push_back(vectors[j]);
            bool reverse = after.size() < before.size();
            auto& momenta = reverse ? after : before;
            std::sort(momenta.begin(),momenta.end());
            std::vector<Id> labels;
            for (int i : momenta) labels.push_back(a.integer(i));
            const Id label = labels.size() == 1 ? labels.front() : a.call("Multiparticle",labels);
            Id value = a.call("SpinorChain",{ep(!swapped,leg),a.call("Mom",{label}),ep(swapped,h)});
            return reverse ? neg(value) : value;
        };
        auto difference = [&](int k) {
            if (k == 0) return pole(l,mask{1} << n);
            if (k == n) return neg(pole(r,mask{1} << n));
            return sandwich(h,k);
        };
        const Id spin = a.mul({chain(swapped,l,{},h),chain(swapped,h,{},r)});
        const Id unit = chain(swapped,l,{},r);
        auto inv = [&](Id x) { return a.pow(x,-1); };
        auto tail = [&](bool left, int marked) {
            const int end = left ? l : r;
            const int count = left ? marked : n-1-marked;
            if (count >= 2) {
                // Write the symmetric scalar tail as its product of independent
                // endpoint emissions plus a remainder. The remainder is sourced
                // by one unordered two-photon determinant. At rank s it can
                // also inherit the remainder at rank s-1 through one emission.
                const int step = left ? 1 : -1;
                const int i = left ? 0 : n-1, j = i+step;
                auto bit = [](int v) { return mask{1} << v; };
                auto single = [&](int index) {
                    const int v = vectors[index];
                    const Id numerator = chain(!swapped,v,{end},h);
                    return a.mul({left ? numerator : neg(numerator),inv(pole(end,bit(index))),
                                  inv(chain(swapped,h,{},v))});
                };
                const auto total_pole = [&](mask set) {
                    return std::bitset<32>(full^set).count() < std::bitset<32>(set).count()
                        ? pole(left ? r : l,full^set) : pole(end,set);
                };
                mask set = bit(i)|bit(j);
                const Id source = neg(a.mul({a.pow(mass,2),a.pow(chain(!swapped,vectors[i],{},vectors[j]),2),
                                              inv(pole(end,bit(i))),inv(pole(end,bit(j)))}));
                const Id pair = a.mul({source,inv(total_pole(set))});
                Id product = a.mul({single(i),single(j)});
                Id remainder = a.mul({pair,inv(a.integer(2))});
                Id spectators = one();
                unsigned factorial = 2;
                for (int s=3; s<=count; ++s) {
                    const int k=i+step*(s-1), leg=vectors[k];
                    set |= bit(k);
                    factorial *= s;
                    const Id emission=single(k);
                    product=a.mul({product,emission});
                    spectators=a.mul({spectators,emission});
                    if (s==3) {
                        const Id recoil = a.mul({a.integer(left ? -1 : 1),chain(!swapped,leg,{},vectors[i]),
                            chain(!swapped,leg,{end},vectors[i]),inv(pole(end,bit(k)))});
                        remainder = a.mul({pair,inv(a.integer(2)),
                                    a.add({emission,a.mul({a.integer(2),recoil,inv(total_pole(set))})})});
                    } else {
                        const auto vertex=a.mul({sandwich(leg,k),inv(chain(swapped,h,{},leg))});
                        const auto pairs=inv(a.integer(factorial/(s*(s-1)/2)));
                        remainder=a.mul({inv(total_pole(set)),
                               a.add({a.mul({vertex,remainder}),a.mul({pairs,source,spectators})})});
                    }
                }
                return a.add({a.mul({product,inv(a.integer(factorial))}),remainder});
            }
            std::vector<Id> factors;
            for (int j = left ? 0 : marked+1; j < (left ? marked : n); ++j)
                factors.push_back(a.mul({sandwich(vectors[j],j),inv(chain(swapped,h,{},vectors[j])),
                                         inv(virtuality(left,left ? j+1 : j))}));
            return a.mul(factors);
        };
        std::vector<Id> marked;
        for (int k = 0; k < n; ++k) {
            // Reflect the ordered word and exchange the massive endpoints.
            // The orbital tails give (-1)^(n-1) and the marked vertex gives -1.
            // An unpaired central insertion occurs twice under this projector.
            if (fold_ends && k > n-1-k) break;
            const Id multiplicity = fold_ends && k==n-1-k ? inv(a.integer(2)) : one();
            const int leg = vectors[k];
            const Id momentum = sandwich(leg,k);
            // Consecutive insertion weights differ by Schouten. Their
            // longitudinal parts telescope, including the two external ends.
            marked.push_back(a.mul({multiplicity,tail(true,k),tail(false,k),momentum,
                    a.add({a.mul({unit,momentum}),neg(a.mul({spin,chain(!swapped,h,{},leg)}))}),
                    inv(difference(k)),inv(difference(k+1))}));
        }
        return a.mul({neg(g),inv(mass),a.add(marked)});
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
    legs reference_legs;
    bool normalized_pairs = false;
    std::map<cache_key, Id> remainders;
    std::map<cache_key, current> currents;

    balanced_line(dirac_recursion input, bool physical_gauge)
        : dirac_recursion(std::move(input)), physical(physical_gauge) {
        for (size_t i = 0; i < vectors.size(); ++i)
            if (minor[i]) minority |= 1u << i;
    }

    static size_t length(mask set) { return std::bitset<sizeof(mask) * 8>(set).count(); }

    int reference(size_t i) const {
        if (!reference_legs.empty()) return reference_legs[i];
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
        if (minor[i] == minor[j]) {
            if (reference(i)==reference(j)) return zero();
            return a.mul({chain(chirality(i),vectors[i],{},vectors[j]),
                          chain(!chirality(i),reference(i),{},reference(j))});
        }
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
            // Either keep the external polarization denominators together or
            // absorb them into each two-photon current before the Weyl sewing.
            auto spin_weight = [&](size_t k) { return normalized_pairs ? one() : norm(k); };
            auto orbital = [&](size_t k) {
                const auto value = projection(k,end);
                return normalized_pairs ? a.mul({value,a.pow(norm(k),-1)}) : value;
            };
            const auto leading = a.mul({orbital(i), orbital(j), pole(end, set)});
            const auto divide = a.mul({a.pow(di, -1), a.pow(dj, -1)});
            sums[end] = {a.mul({a.add({leading, neg(a.mul({spin_weight(i), spin_weight(j), remainder}))}), divide})};
            // Move the orbital spectator past one Pauli emission. For two
            // Pauli emissions Schouten and the massive endpoint equation
            // collapse their recoil to a single mass times a bracket.
            auto pauli = [&](size_t u, size_t v) {
                if (!minor[u]) return;
                const int U = vectors[u], V = vectors[v];
                const Id endpoint = chain(swapped, U, {}, end);
                Id shift;
                if (minor[v]) {
                    shift = a.mul({mass, spin_weight(v), a.pow(chain(swapped, U, {}, V), 2),
                                   chain(!swapped, U, {}, end)});
                } else {
                    shift = neg(a.mul({endpoint, spin_weight(v), chain(!swapped, V, {end}, U),
                                       chain(!swapped, U, {}, V)}));
                }
                sums[U] = {a.mul({spin_weight(u), divide, a.add({shift,
                    neg(a.mul({endpoint, orbital(v), pole(end, set)}))})})};
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

    Id run(std::optional<permutation_sum>* presentation = nullptr) {
        const size_t half = vectors.size() / 2;
        const bool odd = vectors.size() % 2;
        // Centering on the only Pauli emission leaves two purely orbital
        // tails. The crossed reference photons also kill every scalar contact
        // involving the central photon, so the determinant form stays small.
        const bool marked = !physical && length(minority) == 1;
        std::vector<Id> terms;
        std::vector<Id> ordered;
        // Fixed photon references are external labels of the kernel. Permute
        // the remaining labels independently within each helicity class.
        std::vector<legs> groups(2);
        if (presentation) {
            std::set<int> anchors;
            if (!physical)
                for (size_t i = 0; i < vectors.size(); ++i)
                    anchors.insert(reference(i));
            for (size_t i = 0; i < vectors.size(); ++i)
                if (!anchors.count(vectors[i]))
                    groups[minor[i]].push_back(i);
        }
        auto add_split = [&](Id value, mask start, mask center) {
            terms.push_back(value);
            if (!presentation || value == zero()) return;
            // Choose one allocation of the like-helicity legs to the left
            // current, central vertex and right current. Each current has
            // already summed all internal orderings in its own block.
            long repetitions = 1;
            for (const auto& group : groups) {
                int previous = -1;
                unsigned counts[3]{};
                for (int i : group) {
                    const mask bit = 1u << i;
                    const int block = (start & bit) ? 0 : (center & bit) ? 1 : 2;
                    if (block < previous)
                        return;
                    previous = block;
                    repetitions *= ++counts[block];
                }
            }
            ordered.push_back(a.mul({a.pow(a.integer(repetitions),-1),value}));
        };
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
                    if (!(start & (1u << i))) add_split(join_emission(start,i),start,1u<<i);
            }
            if (count == half && !odd) {
                // Both currents carry the joining propagator. Amputate one.
                const mask end = full ^ start;
                const auto den = denominator(false, end);
                current amputated;
                for (const auto& [k, x] : extend(false, end)) amputated[k] = a.mul({den, x});
                add_split(dot(extend(true,start),amputated),start,0);
            }
            if (count + 1 == half || (odd && count == half)) {
                for (size_t i = 0; i < vectors.size(); ++i)
                    if (!(start & (1u << i)))
                        for (size_t j = 0; j < i; ++j)
                            if (!(start & (1u << j))) add_split(join_contact(start,i,j),start,(1u<<i)|(1u<<j));
            }
        }
        std::vector<Id> prefactors{neg(g), a.pow(mass, -1), a.add(terms)};
        for (size_t i = 0; i < vectors.size(); ++i) prefactors.push_back(a.pow(norm(i), -1));
        if (presentation && !ordered.empty()) {
            auto seed = prefactors;
            seed[2] = a.add(ordered);
            permutation_sum result{{}, a.mul(seed), {}};
            for (const auto& group : groups) {
                if (group.size() < 2)
                    continue;
                legs permuted;
                for (int i : group)
                    permuted.push_back(vectors[i]);
                if (result.legs.empty())
                    result.legs = std::move(permuted);
                else
                    result.other_groups.push_back(std::move(permuted));
            }
            if (!result.legs.empty())
                *presentation = std::move(result);
        }
        return a.mul(prefactors);
    }
};
// Place the central fermion propagator between two three-photon subcurrents.
// Summing over the last photon on each side leaves the already reduced,
// symmetric two-photon currents at the external fermion and antifermion.
permutation_sum central_fermion_sum(Algebra& a, int l, int r, Id mass, const legs& majority,
                                    const legs& minority, bool conjugate, Id coupling, bool physical) {
    legs photons = majority;
    photons.insert(photons.end(), minority.begin(), minority.end());
    const mask full = (mask{1} << photons.size()) - 1;
    auto make_line = [&](bool square) {
        std::vector<bool> pauli;
        legs references;
        for (std::size_t i = 0; i < photons.size(); ++i) {
            const bool major = i < majority.size();
            pauli.push_back((major != conjugate) == square);
            references.push_back(physical ? l
                                 : major  ? minority.front()
                                          : minority[(i - majority.size() + 1) % minority.size()]);
        }
        balanced_line line({a, l, r, mass, coupling, photons, pauli, square, full}, physical);
        line.reference_legs = std::move(references);
        line.normalized_pairs = true;
        return line;
    };
    // Pair currents already contain both polarization denominators. A full
    // three-photon current keeps its three denominators outside the recursion.
    auto angle = make_line(false), square = make_line(true);
    auto full_angle = make_line(false), full_square = make_line(true);
    full_angle.normalized_pairs = full_square.normalized_pairs = false;
    auto project = [&](bool sq, bool left, mask set, int k, bool whole = false) {
        auto& line = whole ? (sq ? full_square : full_angle) : (sq ? square : angle);
        const bool projected = sq != line.chirality(k);
        const int photon = photons[k], ref = line.reference(k);
        std::vector<Id> terms;
        for (const auto& [end, c] : line.extend(left, set)) {
            Id bracket;
            if (physical && projected)
                bracket =
                    left ? line.neg(line.chain(sq, end, {ref}, photon)) : line.chain(!sq, photon, {ref}, end);
            else {
                const int spin = projected ? ref : photon;
                bracket = left ? line.chain(sq, end, {}, spin) : line.chain(sq, spin, {}, end);
            }
            terms.push_back(a.mul({c, bracket}));
        }
        return a.add(terms);
    };
    auto bridge = [&](bool sq, int k, int j, std::optional<Id> momentum) {
        const bool right_square = sq != momentum.has_value();
        const bool first = sq != angle.chirality(k), last = right_square != angle.chirality(j);
        const int start = first && !physical ? angle.reference(k) : photons[k];
        const int end = last && !physical ? angle.reference(j) : photons[j];
        std::vector<Id> word{angle.ep(sq != (first && physical), start)};
        if (first && physical) word.push_back(a.call("Mom", {a.integer(angle.reference(k))}));
        if (momentum) word.push_back(*momentum);
        if (last && physical) word.push_back(a.call("Mom", {a.integer(angle.reference(j))}));
        word.push_back(angle.ep(right_square != (last && physical), end));
        if (start == end && word.size() == 2) return angle.zero();
        const Id chain = a.call("SpinorChain", std::move(word));
        return last && physical ? angle.neg(chain) : chain;
    };
    // A massless reference singles out the first minority leg. Permute all
    // minority labels only after summing the complete amplitude in this gauge.
    std::vector<legs> blocks(1);
    for (int i = 0; i < static_cast<int>(photons.size()); ++i) {
        if (i >= static_cast<int>(majority.size()) && (!physical || i == static_cast<int>(majority.size())))
            blocks.emplace_back();
        blocks.back().push_back(i);
    }
    int gauges = 1;
    if (!physical)
        for (int i = 2; i <= static_cast<int>(minority.size()); ++i)
            gauges *= i;
    permutation_sum out{majority, angle.zero(), {}, {minority}};
    if (!physical) out.joint_exchange = signed_exchange{{{l, r}}, -1};
    std::vector<Id> terms;
    for (mask left = 1; left < full; ++left) {
        if (angle.length(left) != 3) continue;
        // Within each reference-preserving block take its first nl labels
        // on the left. The factorial weight removes equivalent permutations.
        legs counts, opposite;
        std::map<int, int> left_centers, right_centers;
        int stabilizer = gauges;
        bool representative = true;
        for (const auto& block : blocks) {
            auto on_left = [&](int i) { return (left & (mask{1} << i)) != 0; };
            const auto boundary = std::find_if_not(block.begin(), block.end(), on_left);
            if (std::any_of(boundary, block.end(), on_left)) {
                representative = false;
                break;
            }
            const int nl = static_cast<int>(boundary - block.begin());
            const int nr = static_cast<int>(block.end() - boundary);
            if (nl) left_centers.emplace(block.front(), nl);
            if (nr) right_centers.emplace(*boundary, nr);
            for (int count : {nl, nr})
                for (int i = 2; i <= count; ++i)
                    stabilizer *= i;
            counts.push_back(nl);
            opposite.push_back(nr);
        }
        if (!representative || (!physical && counts < opposite)) continue;
        if (!physical && counts == opposite) stabilizer *= 2;
        const mask right = full ^ left;
        const Id denominator = angle.pole(l, left);
        const Id momentum = a.node(denominator).args.front();
        std::vector<Id> pairs;
        for (const auto& [k, nk] : left_centers)
            for (const auto& [j, nj] : right_centers) {
                const mask lp = left ^ (mask{1} << k), rp = right ^ (mask{1} << j);
                std::vector<Id> spin;
                for (bool s : {false, true}) {
                    const Id diagonal =
                        a.mul({mass, bridge(!s, k, j, std::nullopt), project(s, false, rp, j)});
                    const Id off_diagonal = a.mul({bridge(!s, k, j, momentum), project(!s, false, rp, j)});
                    spin.push_back(a.mul({project(s, true, lp, k), a.add({diagonal, off_diagonal})}));
                }
                pairs.push_back(a.mul(
                    {a.integer(nk * nj), a.add(spin), a.pow(angle.norm(k), -1), a.pow(angle.norm(j), -1)}));
            }
        // The same central propagator can instead be absorbed into either
        // three-photon current. The remaining central photon then connects
        // two Weyl projections, as in an odd-length line.
        auto one_vertex = [&](bool whole_left) {
            const mask whole = whole_left ? left : right;
            const auto& centers = whole_left ? right_centers : left_centers;
            std::vector<Id> choices;
            for (const auto& [k, multiplicity] : centers) {
                const mask pair = (full ^ whole) ^ (mask{1} << k);
                std::vector<Id> spin;
                for (bool s : {false, true}) {
                    spin.push_back(
                        a.mul({project(s, whole_left, whole, k, true), project(!s, !whole_left, pair, k)}));
                }
                choices.push_back(a.mul({a.integer(multiplicity), a.pow(angle.norm(k), -1), a.add(spin)}));
            }
            std::vector<Id> factors{a.add(choices)};
            for (std::size_t i = 0; i < photons.size(); ++i)
                if (whole & (mask{1} << i)) factors.push_back(a.pow(angle.norm(i), -1));
            return a.mul(factors);
        };
        const Id central = a.mul({a.pow(denominator, -1), a.add(pairs)});
        const auto rank = [&](Id k) { return std::make_pair(expression_complexity(a, k), a.cost(k)); };
        Id best = reduce_projected_candidate(a, central, out);
        for (bool whole_left : {false, true}) {
            const Id trial = reduce_projected_candidate(a, one_vertex(whole_left), out);
            if (rank(trial) < rank(best)) best = trial;
        }
        terms.push_back(a.mul({a.pow(a.integer(stabilizer), -1), best}));
    }
    out.kernel = a.mul({angle.neg(coupling), a.add(terms)});
    return out;
}
} // namespace
std::optional<Id> physical_current_with_presentation(Algebra& a, int l, int r, Id mass, const std::vector<int>& plus,
                                         const std::vector<int>& minus, Id coefficient, std::optional<permutation_sum>* presentation) {
    if (plus.empty() || minus.empty() || plus.size() + minus.size() < 3 || plus.size() + minus.size() > 7)
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
    if (presentation && major.size()==6 && minor.size()>=2)
        *presentation=paired_weyl_sum(a,l,r,mass,plus,minus,coefficient,true);
    return balanced_line(std::move(d), true)
        .run(minor.size() == 1 || major.size() > 6 ? presentation : nullptr);
}
std::optional<Id> mixed_physical_current(Algebra& a, int l, int r, Id mass,
        const legs& plus, const legs& minus, Id coefficient) {
    return physical_current_with_presentation(a,l,r,mass,plus,minus,coefficient,nullptr);
}

std::optional<Id> spurious_current_with_presentation(Algebra& a, int l, int r, Id mass,
                                                     const std::vector<int>& plus,
                                                     const std::vector<int>& minus, Id coefficient,
                                                     std::optional<permutation_sum>* presentation) {
    if (plus.empty() || minus.empty() || plus.size() + minus.size() < 3 || plus.size() + minus.size() > 7)
        return {};
    bool swapped = minus.size() > plus.size();
    legs vectors = swapped ? minus : plus, other = swapped ? plus : minus;
    std::vector<bool> minor(vectors.size(), false);
    vectors.insert(vectors.end(), other.begin(), other.end());
    minor.resize(vectors.size(), true);
    dirac_recursion d{a, l, r, mass, coefficient, vectors, minor, swapped, (1u << vectors.size()) - 1};
    if (vectors.size() > 4)
        return balanced_line(std::move(d), false).run(presentation);
    return other.size() == 1 ? d.connected_emissions(false) : d.midpoint(false);
}

std::optional<Id> mixed_spurious_current(Algebra& a, int l, int r, Id mass, const legs& plus, const legs& minus,
                                         Id coefficient) {
    return spurious_current_with_presentation(a, l, r, mass, plus, minus, coefficient, nullptr);
}

std::optional<permutation_sum> ordered_minority_sum(Algebra& a, int l, int r, Id mass,
        const legs& plus, const legs& minus, Id coefficient) {
    const bool swapped = minus.size() > plus.size();
    const auto& companions = swapped ? minus : plus;
    const auto& minority = swapped ? plus : minus;
    if (minority.size() != 1 || companions.size() < 3 || companions.size() > 6) return {};
    auto vectors = companions;
    vectors.push_back(minority.front());
    dirac_recursion line{a,l,r,mass,coefficient,vectors,{},swapped,(1u << vectors.size())-1};
    const bool fold_ends = companions.size() >= 5;
    permutation_sum sum{companions,line.ward_summand(fold_ends),{}};
    if (fold_ends) sum.joint_exchange = signed_exchange{{{l,r}},companions.size()%2 ? -1 : 1};
    return sum;
}

std::optional<permutation_sum> paired_weyl_sum(Algebra& a, int l, int r, Id mass,
        const legs& plus, const legs& minus, Id coupling, bool physical) {
    // Two symmetric pairs can be sewn to either Weyl component of one
    // photon. Lower multiplicities already have shorter presentations.
    const bool conjugate = plus.size() < minus.size();
    const auto& majority = conjugate ? minus : plus;
    const auto& minority = conjugate ? plus : minus;
    const auto count = majority.size() + minority.size();
    if (minority.empty() || (minority.size() == 1 && (!physical || count != 7)))
        return {};
    if (count == 6)
        return central_fermion_sum(a,l,r,mass,majority,minority,conjugate,coupling,physical);
    if (count != 5 && count != 7)
        return {};
    legs vectors = majority;
    vectors.insert(vectors.end(), minority.begin(), minority.end());
    const mask full = (mask{1} << vectors.size()) - 1;
    auto sew = [&](unsigned gauge) {
        auto make_line = [&](bool square) {
            std::vector<bool> pauli;
            legs refs;
            for (std::size_t i = 0; i < vectors.size(); ++i) {
                const bool major = i < majority.size();
                const bool positive = major != conjugate;
                pauli.push_back(positive == square);
                const auto& group = major ? majority : minority;
                const auto position = major ? i : i - majority.size();
                const int cyclic = group[(position + 1) % group.size()];
                const int crossed = (major ? minority : majority).front();
                if (physical)
                    refs.push_back(gauge && (major == (gauge == 2)) ? r : l);
                else
                    refs.push_back(gauge == 1 || (gauge == 2 && major) ? crossed : cyclic);
            }
            balanced_line line({a, l, r, mass, coupling, vectors, pauli, square, full}, physical);
            line.reference_legs = std::move(refs);
            line.normalized_pairs = !physical && count == 5;
            return line;
        };
        auto angle = make_line(false), square = make_line(true);
        auto projection = [&](balanced_line& line, bool left, mask pair, std::size_t center) {
            const bool hand = line.swapped;
            const bool projected = hand != line.chirality(center);
            const int leg = vectors[center], ref = line.reference(center);
            std::vector<Id> terms;
            for (const auto& [end, value] : line.extend(left, pair)) {
                Id bracket;
                if (projected && physical) {
                    // Reversing a projected reference from bra to ket reverses
                    // its sign in HEPCAT's incoming spinor convention.
                    bracket =
                        left ? line.neg(line.chain(hand, end, {ref}, leg)) : line.chain(!hand, leg, {ref}, end);
                } else {
                    const int endpoint = projected ? ref : leg;
                    bracket = left ? line.chain(hand, end, {}, endpoint) : line.chain(hand, endpoint, {}, end);
                }
                terms.push_back(a.mul({value, bracket}));
            }
            return a.add(terms);
        };
        // Orbits of (left unordered pair, center, right unordered pair). A
        // cyclic photon reference commutes with rotations of its own group.
        // Include each orbit once, divided by its stabilizer.
        std::vector<legs> images(1);
        unsigned symmetric_orders = 1;
        unsigned first = 0;
        for (const auto& group : {majority, minority}) {
            legs order;
            for (unsigned i = 0; i < group.size(); ++i) {
                order.push_back(first + i);
                symmetric_orders *= i + 1;
            }
            std::vector<legs> product;
            do {
                for (auto prefix : images) {
                    prefix.insert(prefix.end(), order.begin(), order.end());
                    product.push_back(std::move(prefix));
                }
            } while (std::next_permutation(order.begin(), order.end()));
            images = std::move(product);
            first += group.size();
        }
        if (!physical) {
            // A relabeling preserves this gauge if it carries the reference of
            // every photon to the reference of the image photon.
            images.erase(
                std::remove_if(images.begin(), images.end(),
                               [&](const legs& image) {
                                   for (size_t i = 0; i < vectors.size(); ++i) {
                                       auto ref = std::find(vectors.begin(), vectors.end(), angle.reference(i));
                                       if (ref == vectors.end() ||
                                           vectors[image[ref - vectors.begin()]] != angle.reference(image[i]))
                                           return true;
                                   }
                                   return false;
                               }),
                images.end());
        }
        std::vector<Id> terms;
        for (mask left = 1; left < full; ++left) {
            if (balanced_line::length(left) != count / 2)
                continue;
            for (std::size_t i = 0; i < vectors.size(); ++i) {
                if (left & (mask{1} << i))
                    continue;
                const auto key = std::make_pair(left, static_cast<int>(i));
                unsigned stabilizer = 0;
                bool representative = true;
                for (const auto& image : images) {
                    mask transformed = 0;
                    for (std::size_t j = 0; j < vectors.size(); ++j)
                        if (left & (mask{1} << j))
                            transformed |= mask{1} << image[j];
                    const auto mapped = std::make_pair(transformed, image[i]);
                    if (mapped < key) {
                        representative = false;
                        break;
                    }
                    if (mapped == key)
                        ++stabilizer;
                }
                if (!representative)
                    continue;
                const mask right = full ^ left ^ (mask{1} << i);
                const auto plus_term =
                    a.mul({projection(angle, true, left, i), projection(square, false, right, i)});
                const auto minus_term =
                    a.mul({projection(square, true, left, i), projection(angle, false, right, i)});
                Id weight = a.pow(a.integer(stabilizer), -1);
                if (angle.normalized_pairs)
                    weight = a.mul({weight, a.pow(angle.norm(i), -1)});
                terms.push_back(a.mul({weight, plus_term}));
                terms.push_back(a.mul({weight, minus_term}));
            }
        }
        std::vector<Id> factors{angle.neg(coupling), a.add(terms)};
        if (!angle.normalized_pairs)
            for (std::size_t i = 0; i < vectors.size(); ++i)
                factors.push_back(a.pow(angle.norm(i), -1));
        // Average the complete amplitude over the cosets of the subgroup
        // that preserves the reference assignment.
        if (images.size() < symmetric_orders)
            factors.push_back(a.pow(a.integer(symmetric_orders / images.size()), -1));
        return permutation_sum{majority, a.mul(factors), {}, {minority}};
    };
    auto best = sew(0);
    if (count > 6) {
        auto rank = [&](Id kernel) {
            try { return std::make_pair(false, expression_complexity(a, kernel)); }
            catch (const std::length_error&) {}
            catch (const std::overflow_error&) {}
            return std::make_pair(true, static_cast<std::size_t>(a.cost(kernel)));
        };
        auto best_rank = rank(best.kernel);
        for (unsigned ref : {1u, 2u}) {
            auto alternative = sew(ref);
            const auto weight = rank(alternative.kernel);
            if (weight < best_rank) {
                best = std::move(alternative);
                best_rank = weight;
            }
        }
    }
    return best;
}
} // namespace hepcat
