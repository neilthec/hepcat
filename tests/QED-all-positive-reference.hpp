// Symbolic transcription of the two all-positive conjectures in the QED notebook.
// Used for benchmark comparisons. The constructor never receives this as a seed.
#pragma once
#include "amp_expression.hpp"
#include "model.hpp"
#include "reduce.hpp"
#include <map>
#include <stdexcept>

namespace qed_notebook {
class all_positive_reference {
    using Id = hepcat::Id;
    using Mask = unsigned;
    hepcat::Algebra a;
    Id mass, charge;
    unsigned photons;
    bool expanded_poles;
    Mask all;
    std::map<std::tuple<unsigned, Mask, Mask>, Id> weights;
    Id bracket(bool angle, int first, int last, const std::vector<int>& momenta = {}, bool same = true) {
        auto end = [&](bool is_angle, int leg) {
            return a.call("Spinor", {a.atom(leg < 3 ? "\"Spin\"" : "\"Helicity\""),
                                     a.atom(is_angle ? "\"Angle\"" : "\"Square\""), a.integer(leg)});
        };
        std::vector<Id> slots{end(angle, first)};
        for (int p : momenta)
            slots.push_back(a.call("Mom", {a.integer(p)}));
        slots.push_back(end(same ? angle : !angle, last));
        return a.call("SpinorChain", std::move(slots));
    }
    template <class F> void each(Mask set, F f) {
        for (unsigned i = 0; i < photons; ++i)
            if (set & (1u << i))
                f(i + 3, 1u << i);
    }
    template <class F> void pairs(Mask set, F f) {
        each(set,
             [&](int i, Mask b) { each(set & ~(b | (b - 1)), [&](int j, Mask c) { f(i, j, b, c); }); });
    }
    template <class F> Id product(Mask set, F f) {
        std::vector<Id> factors;
        each(set, [&](int i, Mask b) { factors.push_back(f(i, b)); });
        return a.mul(std::move(factors));
    }
    Id tau(int leg, Mask set) {
        if (expanded_poles) {
            std::vector<Id> dots;
            each(set, [&](int i, Mask) {
                dots.push_back(a.call("MomProd", {a.integer(leg), a.integer(i)}));
            });
            pairs(set, [&](int i, int j, Mask, Mask) {
                dots.push_back(a.call("MomProd", {a.integer(i), a.integer(j)}));
            });
            return a.mul({a.integer(2), a.add(dots)});
        }
        std::vector<Id> v{a.integer(leg)};
        each(set, [&](int i, Mask) { v.push_back(a.integer(i)); });
        return a.call("PropDen", {a.call("Mom", {a.call("Multiparticle", std::move(v))}), mass});
    }
    Id sandwich(int i) { return bracket(false, i, i, {1, 2}); }
    Id ratio(int r, int p, int i) {
        return a.mul({bracket(true, r, i, {p}, false), a.pow(bracket(true, r, i), -1)});
    }
    Id leading(int r, Mask set) {
        return product(set, [&](int i, Mask) { return r ? ratio(r, 1, i) : sandwich(i); });
    }
    Id emission(int r, Mask set, int i, Mask b) {
        std::vector<Id> emissions;
        auto direct = set & ~b;
        auto comp = all & ~set & ~(1u << (r - 3));
        std::vector<int> x{1}, y{2};
        each(direct, [&](int j, Mask) { x.push_back(j); });
        each(comp, [&](int j, Mask) { y.push_back(j); });
        bool shorter = y.size() < x.size();
        for (int j : shorter ? y : x)
            emissions.push_back(ratio(r, j, i));
        return a.mul({a.integer(shorter ? -1 : 1), a.add(emissions)});
    }
    std::map<std::pair<int, Mask>, Id> ordered_cache;
    Id ordered_current(int r, Mask set) {
        if (!set)
            return a.integer(1);
        if (auto it = ordered_cache.find({r, set}); it != ordered_cache.end())
            return it->second;
        std::vector<Id> terms;
        each(set, [&](int i, Mask b) {
            terms.push_back(a.mul({ordered_current(r, set & ~b), emission(r, set, i, b)}));
        });
        return ordered_cache[{r, set}] = a.mul({a.pow(tau(1, set), -1), a.add(terms)});
    }
    Id weight(int r, Mask set, Mask pair) {
        auto key = std::make_tuple(r, set, pair);
        if (auto it = weights.find(key); it != weights.end())
            return it->second;
        Mask rest = set & ~pair;
        std::vector<Id> terms{leading(r, rest)};
        each(rest, [&](int i, Mask b) {
            std::vector<Id> emissions;
            if (r) {
                terms.push_back(a.mul({emission(r, set, i, b), tau(1, b), weight(r, set & ~b, pair)}));
            } else {
                each(all & ~set, [&](int j, Mask) {
                    emissions.push_back(a.mul({bracket(false, i, j), bracket(true, j, i, {1}, false)}));
                });
                terms.push_back(a.mul({a.add(emissions), tau(2, b), a.pow(tau(2, set & ~b), -1),
                                       weight(0, set & ~b, pair)}));
            }
        });
        if (!r)
            pairs(rest, [&](int i, int j, Mask b, Mask c) {
                auto remaining = set & ~b & ~c;
                terms.push_back(
                    a.mul({a.integer(-1), a.pow(mass, 2), a.pow(bracket(false, i, j), 2), tau(2, b),
                           tau(2, c), a.pow(tau(2, remaining), -1), weight(0, remaining, pair)}));
            });
        Id result = a.add(terms);
        if (r)
            result = a.mul({result, a.pow(tau(1, set), -1)});
        return weights.emplace(key, result).first->second;
    }
    // Sum the fixed-pair weights before recurring. All terms with the same
    // first removed leg or pair share the correction of the smaller subset.
    std::map<Mask, Id> corrections;
    Id collected_correction(Mask set) {
        if (!(set & (set - 1))) return a.integer(0);
        if (auto it = corrections.find(set); it != corrections.end()) return it->second;
        std::vector<Id> terms;
        pairs(set, [&](int i, int j, Mask b, Mask c) {
            Mask rest = set ^ b ^ c;
            terms.push_back(a.mul({a.pow(bracket(false, i, j), 2), tau(1, b), tau(1, c),
                                   leading(0, rest)}));
            if (rest & (rest - 1))
                terms.push_back(a.mul({a.integer(-1), a.pow(mass, 2), a.pow(bracket(false, i, j), 2),
                                       tau(2, b), tau(2, c), collected_correction(rest)}));
        });
        each(set, [&](int i, Mask b) {
            Mask rest = set ^ b;
            if (!(rest & (rest - 1))) return;
            std::vector<Id> emissions;
            each(all ^ set, [&](int j, Mask) {
                emissions.push_back(a.mul({bracket(false, i, j), bracket(true, j, i, {1}, false)}));
            });
            terms.push_back(a.mul({a.add(emissions), tau(2, b), collected_correction(rest)}));
        });
        return corrections[set] = a.mul({a.add(terms), a.pow(tau(2, set), -1)});
    }
    Id collected_current(Mask set) {
        return a.mul({product(set, [&](int, Mask b) { return a.pow(tau(2, b), -1); }),
                      a.add({leading(0, set), a.mul({a.integer(-1), a.pow(mass, 2),
                                                     collected_correction(set)})})});
    }
    Id current(int r, Mask set) {
        auto single_poles = product(set, [&](int, Mask b) { return a.pow(tau(r ? 1 : 2, b), -1); });
        std::vector<Id> terms;
        pairs(set, [&](int i, int j, Mask b, Mask c) {
            std::vector<Id> factors{a.pow(bracket(false, i, j), 2), weight(r, set, b | c)};
            if (!r)
                factors.insert(factors.end(), {tau(1, b), tau(1, c)});
            terms.push_back(a.mul(factors));
        });
        Id pair_term = a.mul({a.integer(-1), a.pow(mass, 2), a.add(terms)});
        if (!r && !terms.empty())
            pair_term = a.mul({pair_term, a.pow(tau(2, set), -1)});
        return a.mul({single_poles, a.add({leading(r, set), pair_term})});
    }

  public:
    all_positive_reference(const hepcat::model& vertices, unsigned n, bool expanded = false)
        : a(&vertices.algebra()), mass(a.parse(vertices.particle("e").mass)), charge(0), photons(n), expanded_poles(expanded),
          all(0) {
        if (n < 2 || n > 8)
            throw std::invalid_argument("all-positive benchmark accepts two to eight photons");
        all = (1u << n) - 1;
        for (auto& p : vertices.variables())
            if (p.name == "EE") {
                charge = p.symbol;
                return;
            }
        throw std::invalid_argument("QED benchmark requires the EE input");
    }
    const hepcat::Algebra& arena() const { return a; }
    Id coefficient(bool spurious, bool ordered = false, bool collected = false) {
        std::vector<Id> contributions;
        pairs(all, [&](int i, int j, Mask b, Mask c) {
            auto rest = all & ~b & ~c;
            auto sq = bracket(false, i, j);
            if (spurious) {
                auto jr = [&](int r) { return ordered ? ordered_current(r, rest) : current(r, rest); };
                auto endpoints =
                    a.add({a.mul({jr(i), a.pow(tau(2, b), -1)}), a.mul({jr(j), a.pow(tau(2, c), -1)})});
                contributions.push_back(a.mul({sq, a.pow(bracket(true, i, j), -1), endpoints}));
            } else
                contributions.push_back(a.mul({a.pow(sq, 2), collected ? collected_current(rest) : current(0, rest)}));
        });
        Id coupling = a.mul({a.integer(spurious ? -1 : 1),
                             a.call("Power", {a.integer(2), a.exact(mpq_class(1, 2))}), charge});
        Id amp = a.mul({a.pow(coupling, photons), mass, a.add(contributions)});
        if (!spurious)
            amp = a.mul({amp, product(all, [&](int, Mask b) { return a.pow(tau(1, b), -1); })});
        return amp;
    }
    hepcat::amp_expression expression(bool spurious) {
        Id amp = a.mul({coefficient(spurious), bracket(true, 1, 2)});
        std::vector<std::string> masses(photons + 2, "0");
        masses[0] = masses[1] = a.format(mass);
        return hepcat::amp_expression(a.format(amp), masses);
    }
};
} // namespace qed_notebook
