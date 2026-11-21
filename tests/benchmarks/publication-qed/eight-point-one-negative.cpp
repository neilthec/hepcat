// Independent transcription of the three eight-point one-negative-photon formulas.
// Build and run instructions are in eight-point-one-negative.md.
#include "radiation_kinematics.hpp"
#include "generation_feynman.hpp"
#include "continuations.hpp"
#include "permutation_sum.hpp"
#include "validation_feynman.hpp"
#include "validation_numeric.hpp"
#include "validation_precision.hpp"
#include "madgraph-runtime.hpp"

#include <algorithm>
#include <cmath>
#include <iostream>
#include <map>
#include <optional>
#include <stdexcept>

using namespace hepcat;
using namespace hepcat::validation;

namespace {
class paper_formula : private detail::radiation_kinematics {
    static constexpr int h = 8;
    const Id u, spin, normalization;
    Id v(int i, legs p) {
        const int ref = i == h ? 2 : 1;
        p.erase(std::remove_if(p.begin(), p.end(), [&](int k) { return k == ref || k == i; }), p.end());
        if (p.empty())
            return ar.integer(0);
        return over(chain(i != h, i, i, {{ref}, p}), {tau({ref, i})});
    }
    Id contact(int i, int j) {
        if (i == h)
            return over(neg(ar.mul({chain(true, j, h, {{1}}), chain(true, j, h, {{2}})})),
                        {tau({2, h}), tau({1, j})});
        return over(neg(ar.pow(ar.mul({mass, chain(true, i, j)}), 2)), {tau({1, i}), tau({1, j})});
    }
    Id left(int i, int j) { return over(contact(i, j), {tau({1, i, j})}); }
    Id right(int i) { return over(neg(v(i, {2})), {tau({2, i})}); }
    Id right(int i, int j) {
        return ar.add({ar.mul({right(i), right(j)}), over(neg(ar.pow(ar.mul({mass, chain(true, i, j)}), 2)),
                                                          {tau({2, i}), tau({2, j}), tau({2, i, j})})});
    }
    Id right(int i, int j, int k) {
        std::vector<Id> terms;
        for (unsigned t = 0; t < 3; ++t) {
            terms.push_back(ar.mul({neg(v(i, {2, j, k})), right(j, k)}));
            terms.push_back(ar.mul({contact(j, k), right(i)}));
            const int next = i;
            i = j;
            j = k;
            k = next;
        }
        return over(ar.add(terms), {tau({2, i, j, k})});
    }
    Id bare(int t) { return over(ar.add({spin, ar.mul({u, v(h, {t})})}), {tau({t, h})}); } // V_h(p2)=0
    Id mixed(int t, int i) {
        const Id b = chain(true, i, h, {{t}});
        const Id recoil =
            ar.mul({spin, ar.add({ar.mul({b, chain(true, h, i)}), ar.mul({v(i, {t}), tau({t, h, i})})})});
        return over(ar.add({neg(ar.mul({u, ar.pow(b, 2)})), t == 1 ? recoil : neg(recoil)}),
                    {tau({t, h}), tau({t, i}), tau({t, h, i})});
    }
    // This representative always multiplies an expression symmetric in i,j.
    Id mixed_right_pair(int i, int j) {
        return over(
            ar.add({ar.mul({ar.integer(-2), v(j, {2, i, h}), mixed(2, i)}),
                    ar.mul({minus(spin, ar.mul({ar.integer(2), u, v(h, {i})})), right(i, j)}),
                    ar.mul({contact(i, j), bare(2)}), ar.mul({ar.integer(2), u, contact(h, i), right(j)})}),
            {tau({2, i, j, h})});
    }

  public:
    paper_formula(Algebra &a, Id m, Id e)
        : radiation_kinematics(a, m, true, true), u(chain(false, 1, 2)),
          spin(a.mul({chain(false, 1, h), chain(false, h, 2)})),
          normalization(over(a.mul({a.integer(-8), a.pow(e, 6)}), {m})) {}
    Id physical() {
        const int a = 3, b = 4, c = 5, d = 6, f = 7;
        const Id r3 = right(c, d, f), r2 = right(d, f);
        const Id mixed_left = ar.mul({mixed(1, a), ar.add({ar.mul({ar.integer(2), v(b, {a, h}), r3}),
                                                           ar.mul({ar.integer(3), contact(b, c), r2})})});
        const Id scalar_left =
            ar.mul({left(a, b), ar.add({ar.mul({ar.add({spin, neg(ar.mul({ar.integer(3), u, v(h, {c})})),
                                                        ar.mul({tau({1, a, b}), bare(1)})}),
                                                r3}),
                                        ar.mul({ar.integer(3), u, contact(h, c), r2}),
                                        ar.mul({ar.integer(6), v(c, {a}), mixed_right_pair(d, f)}),
                                        ar.mul({ar.integer(3), contact(c, d), mixed(2, f)})})});
        return over(ar.mul({normalization, ar.add({mixed_left, scalar_left})}), {ar.integer(12)});
    }
    // The projected summand in the paper has three marked emissions.
    Id ward() {
        auto w = [&](int j) {
            legs before{1};
            for (int i = 3; i < j; ++i)
                before.push_back(i);
            const Id x = chain(true, j, h, {before});
            auto z = [&](int end) {
                if (end == 3)
                    return tau({1, h});
                legs p{1};
                for (int i = 3; i < end; ++i)
                    p.push_back(i);
                return chain(true, h, h, {p});
            };
            return over(ar.mul({x, minus(ar.mul({u, x}), ar.mul({spin, chain(true, h, j)}))}),
                        {z(j), z(j + 1)});
        };
        auto tail = [&](int t, legs list) {
            if (list.empty())
                return ar.integer(1);
            const auto sig = ar.integer(t == 1 ? 1 : -1);
            auto ell = [&](int i) {
                return over(ar.mul({sig, chain(true, i, h, {{t}})}), {chain(false, h, i), tau({t, i})});
            };
            const int a = list[0];
            if (list.size() == 1)
                return ell(a);
            const int b = list[1];
            const Id pair = over(neg(ar.pow(ar.mul({mass, chain(true, a, b)}), 2)),
                                 {tau({t, a}), tau({t, b}), tau({t, a, b})});
            const Id two = ar.mul({ell(a), ell(b)});
            if (list.size() == 2)
                return over(ar.add({two, pair}), {ar.integer(2)});
            const int c = list[2];
            const Id recoil =
                over(neg(ar.mul({sig, chain(true, c, a), chain(true, c, a, {{t}})})), {tau({t, c})});
            const Id correction = ar.add({ell(c), over(ar.mul({ar.integer(2), recoil}), {tau({t, a, b, c})})});
            const Id three = ar.mul({two, ell(c)});
            if (list.size() == 3)
                return ar.add(
                    {over(three, {ar.integer(6)}), over(ar.mul({pair, correction}), {ar.integer(2)})});
            const int d = list.at(3);
            // The only four-leg tail in this representative lies to the right of photon 3.
            const Id vertex = over(chain(true, d, h, {{1, 3}}), {chain(false, h, d)});
            return ar.add({over(ar.mul({three, ell(d)}), {ar.integer(24)}),
                           over(ar.mul({pair, ar.add({ar.mul({ar.integer(2), vertex, correction}),
                                                      ar.mul({tau({t, a, b}), ell(c), ell(d)})})}),
                                {ar.integer(4), tau({1, 3, h})})});
        };
        const Id short_half =
            ar.add({ar.mul({w(3), tail(2, {7, 6, 5, 4})}), ar.mul({tail(1, {3}), w(4), tail(2, {7, 6, 5})}),
                    over(ar.mul({tail(1, {3, 4}), w(5), tail(2, {7, 6})}), {ar.integer(2)})});
        return apply_exchanges(ar, ar.mul({normalization, short_half}), {signed_exchange{{{1, 2}}, -1}});
    }
    Id spurious() {
        // Reference q is the middle positive photon in this ordered word.
        constexpr int q = 5;
        std::vector<Id> orbital{normalization}, terms;
        // A photon at either end of its own sandwich contributes zero.
        // Momentum conservation lets us evaluate an identical shorter route.
        auto sandwich = [&](int leg, int k) {
            legs l{1}, r{2};
            for (int j = 3; j <= 7; ++j)
                if (j != leg)
                    (j < 3 + k ? l : r).push_back(j);
            return r.size() < l.size() ? neg(chain(true, leg, h, {r})) : chain(true, leg, h, {l});
        };
        auto prop = [&](legs d) {
            if (d.size() <= 4)
                return tau(d);
            legs other;
            for (int j = 1; j <= 8; ++j)
                if (std::find(d.begin(), d.end(), j) == d.end())
                    other.push_back(j);
            return tau(other);
        };
        for (int i = 3; i <= 7; ++i) {
            orbital.push_back(over(sandwich(i, i - 3), {chain(false, h, i)}));
        }
        for (int k = 0; k < 3; ++k) {
            std::vector<Id> factors{ar.add({spin, over(ar.mul({u, sandwich(q, k)}), {chain(true, q, h)})})};
            for (int s = 1; s <= k; ++s) {
                legs d{1};
                for (int j = 3; j < 3 + s; ++j)
                    d.push_back(j);
                factors.push_back(ar.pow(prop(d), -1));
            }
            for (int s = k; s < 5; ++s) {
                legs d{2};
                for (int j = 3 + s; j <= 7; ++j)
                    d.push_back(j);
                factors.push_back(ar.pow(prop(d), -1));
            }
            terms.push_back(ar.mul(factors));
        }
        orbital.push_back(ar.add(terms));
        return apply_exchanges(ar, ar.mul(orbital), {signed_exchange{{{1, 2}}, -1}});
    }
};
} // namespace

int main(int argc, char **argv) {
    try {
        if (argc != 3 && argc != 4)
            throw std::invalid_argument("usage: eight-point-one-negative POINTS SEED [MG_LIBRARY]");
        const auto points = std::stoul(argv[1]);
        const auto seed = std::stoull(argv[2]);
        if (!points)
            throw std::invalid_argument("positive sample count required");
        const bool mg = argc == 4;
        model qed("models", 3), rules("tests/reference-models/calchep", 2);
        Algebra a(&qed.algebra()), fa(&rules.algebra());
        const auto parameters =
            mg ? model_parameters(qed, {{"Me", 1}, {"EE", 1.L / 3}}) : model_parameters(qed);
        const std::vector<std::string> names{"e", "E", "A.+", "A.+", "A.+", "A.+", "A.+", "A.-"};
        std::vector<Id> masses, fm;
        std::vector<real> numeric;
        for (const auto &tag : names) {
            masses.push_back(a.parse(qed.particle(tag).mass));
            fm.push_back(fa.parse(rules.particle(generation::feynman_external(rules, tag)).mass));
            numeric.push_back(evaluate_scalar(a, masses.back(), parameters).real());
        }
        a.setKinematics(masses, {});
        fa.setKinematics(fm, {});
        const auto e = std::find_if(qed.variables().begin(), qed.variables().end(),
                                    [](const auto &v) { return v.name == "EE"; });
        if (e == qed.variables().end())
            throw std::runtime_error("missing QED charge");
        paper_formula paper(a, masses.front(), e->symbol);
        struct test {
            const char *name;
            expression_evaluator evaluator;
            real maximum = 0, replay_max = 0;
            unsigned replays = 0, failures = 0;
        };
        std::vector<test> comparisons;
        for (auto [name, kernel] : {std::pair{"physical", paper.physical()},
                                    std::pair{"spurious", paper.spurious()}, std::pair{"ward", paper.ward()}}) {
            std::cout << name << " summand complexity " << expression_complexity(a, kernel) << '\n';
            const Id sum = expand_permutation_groups(a, kernel, {{3, 4, 5, 6, 7}});
            comparisons.push_back({name, expression_evaluator(a, sum, parameters)});
        }
        std::optional<expression_evaluator> feynman, replay_feynman;
        std::optional<hepcat_spinas::madgraph_detail::Reference> external;
        if (mg)
            external.emplace(argv[3]);
        else {
            std::vector<Id> trees;
            for (const auto &tree : generation::diagram_plans(rules, names))
                trees.push_back(feynman_to_spinors(fa, generation::sew_feynman(fa, rules, names, tree)));
            if (trees.size() != 720)
                throw std::runtime_error("expected 720 Feynman diagrams");
            const auto tree_sum = fa.add(trees);
            feynman.emplace(fa, tree_sum, model_parameters(rules));
            // The native test uses independent Xi spinors. In the 160-bit
            // replay, the same six polarizations can use a common chiral
            // reference. Vanishing brackets then drop out of the same 720 trees.
            std::map<Id, reference_choice> refs;
            for (int i = 3; i <= 8; ++i) {
                auto xi = fa.call(
                    "Spinor", {fa.atom("\"Xi\""), fa.atom(i == 8 ? "\"Square\"" : "\"Angle\""), fa.integer(i)});
                refs.emplace(xi, reference_choice{i, i == 8 ? 4 : 8, false});
            }
            replay_feynman.emplace(fa, fa.simplify(replace_external_references(fa, tree_sum, refs)),
                                   model_parameters(rules));
        }
        std::cout << (mg ? "Polarized MadGraph" : "Signed Feynman diagrams") << ", " << points
                  << " points, seed " << seed << std::endl;
        for (unsigned long n = 0; n < points; ++n) {
            if (n && n % 10000 == 0)
                std::cout << n << " points checked" << std::endl;
            auto p = sample_point(numeric, seed + n);
            if (mg) {
                std::vector<double> momenta;
                for (unsigned i = 0; i < names.size(); ++i)
                    for (auto x : p.momenta[i])
                        momenta.push_back(i < 2 ? x : -x);
                std::vector<int> helicities(names.size(), 1);
                helicities.back() = -1;
                real expected = 0;
                std::vector<real> rates(comparisons.size(), 0);
                for (int i : {0, 1})
                    for (int j : {0, 1}) {
                        helicities[0] = 2 * i - 1;
                        helicities[1] = 2 * j - 1;
                        const auto weight = external->evaluate(momenta, helicities);
                        if (!(std::isfinite(weight) && weight >= 0))
                            throw std::runtime_error("invalid MadGraph weight");
                        expected += weight;
                        for (unsigned t : {0u, 1u}) {
                            const int s = t == 0 ? i : j;
                            p.angles[t] = p.angle_basis[t][s];
                            p.squares[t] = p.square_basis[t][s];
                        }
                        for (unsigned k = 0; k < rates.size(); ++k)
                            rates[k] += std::norm(comparisons[k].evaluator(p));
                    }
                for (unsigned k = 0; k < rates.size(); ++k) {
                    auto &t = comparisons[k];
                    const auto scale = std::max(rates[k], expected);
                    const auto diff = scale ? std::abs(rates[k] - expected) / scale : 0;
                    t.maximum = std::max(t.maximum, diff);
                    if (std::isfinite(rates[k]) && diff <= 1e-7L)
                        continue;
                    ++t.replays;
                    auto chart = precision::reconstruct(p);
                    precision::scalar total(0, precision::bits);
                    for (int i : {0, 1})
                        for (int j : {0, 1}) {
                            for (unsigned leg : {0u, 1u}) {
                                const int state = leg == 0 ? i : j;
                                chart.angles[leg] = chart.angle_basis[leg][state];
                                chart.squares[leg] = chart.square_basis[leg][state];
                            }
                            const auto z = t.evaluator.precise(p, chart);
                            total += z.x * z.x + z.y * z.y;
                        }
                    const auto corrected = precision::narrow_real(total);
                    const auto high_scale = std::max(corrected, expected);
                    const auto replay = high_scale ? std::abs(corrected - expected) / high_scale : 0;
                    t.replay_max = std::max(t.replay_max, replay);
                    if (!(std::isfinite(corrected) && replay <= 1e-7L))
                        ++t.failures;
                }
            } else {
                const auto expected = (*feynman)(p);
                std::optional<precision::phase_point> chart;
                std::optional<precision::number> high_reference;
                for (auto &t : comparisons) {
                    const auto value = t.evaluator(p);
                    const auto scale = std::max(std::abs(value), std::abs(expected));
                    const auto diff = scale ? std::abs(value - expected) / scale : 0;
                    t.maximum = std::max(t.maximum, diff);
                    if (std::isfinite(std::abs(value)) && std::isfinite(std::abs(expected)) && diff <= 1e-8L)
                        continue;
                    ++t.replays;
                    if (!chart) {
                        chart = precision::reconstruct(p);
                        high_reference = replay_feynman->precise(p, *chart);
                    }
                    const auto x = t.evaluator.precise(p, *chart);
                    const auto &y = *high_reference;
                    const auto norm = std::max(x.magnitude(), y.magnitude());
                    const real replay = norm == 0 ? 0. : precision::scalar((x - y).magnitude() / norm).get_d();
                    t.replay_max = std::max(t.replay_max, replay);
                    if (!(replay <= 1e-8L)) {
                        if (t.failures < 2)
                            std::cerr << t.name << " failed at seed " << seed + n << " ratio "
                                      << value / expected << '\n';
                        ++t.failures;
                    }
                }
            }
        }
        bool passed = true;
        for (const auto &t : comparisons) {
            std::cout << t.name << " maximum " << t.maximum << ", replays " << t.replays << ", replay maximum "
                      << t.replay_max << ", failures " << t.failures << std::endl;
            passed &= t.failures == 0;
        }
        return passed ? 0 : 1;
    } catch (const std::exception &error) {
        std::cerr << error.what() << '\n';
        return 1;
    }
}
