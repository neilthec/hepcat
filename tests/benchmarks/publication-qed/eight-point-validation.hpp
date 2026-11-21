// Compare each full printed formula to the signed tree sum or a polarized MadGraph rate.
#pragma once
#include "continuations.hpp"
#include "generation_feynman.hpp"
#include "madgraph-runtime.hpp"
#include "validation_feynman.hpp"
#include "validation_numeric.hpp"
#include "validation_precision.hpp"
#include <algorithm>
#include <cmath>
#include <iostream>
#include <map>
#include <numeric>
#include <optional>
#include <stdexcept>

namespace eight_point_validation {
using namespace hepcat;
using namespace hepcat::validation;
using formulas = std::vector<std::pair<const char *, Id>>;
template <class Build>
int run(int argc, char **argv, const std::vector<int> &photons, Build build,
        std::vector<std::string> names = {"e", "E"}, std::size_t expected_trees = 720) {
    try {
        if (argc != 3 && argc != 4)
            throw std::invalid_argument("usage: replay POINTS SEED [MG_LIBRARY]");
        const auto points = std::stoul(argv[1]);
        const auto seed = std::stoull(argv[2]);
        if (!points)
            throw std::invalid_argument("positive sample count required");
        const bool mg = argc == 4;
        model qed("models", 3), rules("tests/reference-models/calchep", 2);
        Algebra a(&qed.algebra()), fa(&rules.algebra());
        const auto parameters =
            mg ? model_parameters(qed, {{"Me", 1}, {"EE", 1.L / 3}}) : model_parameters(qed);
        const auto fermions = names.size();
        for (int sign : photons)
            names.push_back(sign > 0 ? "A.+" : "A.-");
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
        struct test {
            const char *name;
            expression_evaluator evaluator;
            real maximum = 0, replay_max = 0;
            unsigned replays = 0, failures = 0;
        };
        std::vector<test> comparisons;
        for (auto [name, sum] : build(a, masses.front(), e->symbol))
            comparisons.push_back({name, expression_evaluator(a, sum, parameters)});
        std::vector<std::function<real(const point &)>> spin_sums;
        if (mg) {
            std::vector<size_t> massive(fermions);
            std::iota(massive.begin(), massive.end(), 0);
            for (const auto &t : comparisons)
                spin_sums.push_back(t.evaluator.spin_sum(massive));
        }
        std::optional<expression_evaluator> feynman, replay_feynman;
        std::optional<hepcat_spinas::madgraph_detail::Reference> external;
        if (mg)
            external.emplace(argv[3]);
        else {
            std::vector<Id> trees;
            for (const auto &tree : generation::diagram_plans(rules, names))
                trees.push_back(feynman_to_spinors(fa, generation::sew_feynman(fa, rules, names, tree)));
            if (trees.size() != expected_trees)
                throw std::runtime_error("unexpected Feynman graph count " + std::to_string(trees.size()));
            const auto tree_sum = fa.add(trees);
            feynman.emplace(fa, tree_sum, model_parameters(rules));
            // The native test uses independent Xi spinors. In the 160-bit
            // replay, the external polarizations can use known chiral
            // reference. Vanishing brackets then drop out of the same tree sum.
            std::map<Id, reference_choice> refs;
            for (std::size_t i = 0; i < photons.size(); ++i) {
                const int leg = static_cast<int>(fermions + i + 1);
                auto opposite = std::find(photons.begin(), photons.end(), -photons[i]);
                if (opposite == photons.end())
                    opposite = photons.begin() + (i + 1) % photons.size();
                if (photons[i] < 0) {
                    const auto second = std::find(opposite + 1, photons.end(), 1);
                    if (second != photons.end())
                        opposite = second;
                }
                const int ref = static_cast<int>(fermions + 1 + (opposite - photons.begin()));
                auto xi = fa.call("Spinor",
                                  {fa.atom("\"Xi\""), fa.atom(photons[i] > 0 ? "\"Angle\"" : "\"Square\""),
                                   fa.integer(leg)});
                refs.emplace(xi, reference_choice{leg, ref, false});
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
                std::copy(photons.begin(), photons.end(), helicities.begin() + fermions);
                real expected = 0;
                std::vector<real> rates(comparisons.size(), 0);
                for (unsigned state = 0; state < (1u << fermions); ++state) {
                    for (unsigned t = 0; t < fermions; ++t) {
                        const int s = (state >> t) & 1;
                        helicities[t] = 2 * s - 1;
                    }
                    const auto weight = external->evaluate(momenta, helicities);
                    if (!(std::isfinite(weight) && weight >= 0))
                        throw std::runtime_error("invalid MadGraph weight");
                    expected += weight;
                }
                for (unsigned k = 0; k < rates.size(); ++k) {
                    auto &t = comparisons[k];
                    rates[k] = spin_sums[k](p);
                    const auto scale = std::max(rates[k], expected);
                    const auto diff = scale ? std::abs(rates[k] - expected) / scale : 0;
                    t.maximum = std::max(t.maximum, diff);
                    if (std::isfinite(rates[k]) && diff <= 1e-7L)
                        continue;
                    ++t.replays;
                    auto chart = precision::reconstruct(p);
                    precision::scalar total(0, precision::bits);
                    for (unsigned state = 0; state < (1u << fermions); ++state) {
                        for (unsigned leg = 0; leg < fermions; ++leg) {
                            const int spin = (state >> leg) & 1;
                            chart.angles[leg] = chart.angle_basis[leg][spin];
                            chart.squares[leg] = chart.square_basis[leg][spin];
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
                    const real replay =
                        norm == 0 ? 0. : precision::scalar((x - y).magnitude() / norm).get_d();
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
            std::cout << t.name << " maximum " << t.maximum << ", replays " << t.replays
                      << ", replay maximum " << t.replay_max << ", failures " << t.failures << std::endl;
            passed &= t.failures == 0;
        }
        return passed ? 0 : 1;
    } catch (const std::exception &error) {
        std::cerr << error.what() << '\n';
        return 1;
    }
}

} // namespace eight_point_validation
