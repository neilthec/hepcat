#include "amplitude.hpp"
#include "chart_scalar.hpp"
#include "generation_feynman.hpp"
#include "native_fit.hpp"
#include "validation_feynman.hpp"
#include "validation_dirac_current.hpp"
#include "validation_numeric.hpp"
#include "validation_precision_parameters.hpp"

#include <algorithm>
#include <chrono>
#include <cmath>
#include <limits>
#include <set>
#include <thread>

namespace hepcat {
namespace {
using validation::complex;
using validation::expression_evaluator;
using validation::parameters;
using validation::real;
namespace wide = validation::precision;

bool contains_reference(const Algebra& a, Id root) {
    std::set<Id> visited;
    std::vector<Id> stack{root};
    while (!stack.empty()) {
        auto id = stack.back();
        stack.pop_back();
        if (!visited.insert(id).second)
            continue;
        const auto& n = a.node(id);
        if (n.head == "\"Xi\"")
            return true;
        stack.insert(stack.end(), n.args.begin(), n.args.end());
    }
    return false;
}

struct discrepancy {
    real scale, absolute, relative;
    bool exceeds(const validation_options& o) const {
        return absolute > o.absolute_tolerance + o.relative_tolerance * scale;
    }
};

discrepancy difference(complex lhs, complex rhs) {
    const real scale = std::max(std::abs(lhs), std::abs(rhs));
    const real error = std::abs(lhs - rhs);
    if (!std::isfinite(scale) || !std::isfinite(error))
        throw std::domain_error("nonfinite amplitude evaluation");
    return {scale, error, scale == 0 ? 0 : error / scale};
}

discrepancy difference(const wide::number& lhs, const wide::number& rhs) {
    const auto scale = std::max(lhs.magnitude(), rhs.magnitude());
    const auto error = (lhs - rhs).magnitude();
    return {wide::narrow_real(scale), wide::narrow_real(error),
            scale == 0 ? 0 : wide::narrow_real(wide::scalar(error / scale, wide::bits))};
}

template <class Replay, class Roundoff>
void compare(numerical_check& row, complex lhs, complex rhs, std::uint64_t seed,
             const validation_options& options, Replay replay, Roundoff roundoff) {
    const auto raw = difference(lhs, rhs);
    row.max_absolute_error = std::max(row.max_absolute_error, raw.absolute);
    row.max_relative_error = std::max(row.max_relative_error, raw.relative);
    if (raw.exceeds(options)) {
        ++row.replays;
        if (!row.first_raw_outlier_seed)
            row.first_raw_outlier_seed = seed;
        const auto [a, b] = replay();
        const auto precise = difference(a, b);
        row.max_replay_absolute_error = std::max(row.max_replay_absolute_error, precise.absolute);
        row.max_replay_relative_error = std::max(row.max_replay_relative_error, precise.relative);
        if (precise.exceeds(options) && precise.absolute > roundoff()) {
            ++row.failures;
            if (!row.first_failure_seed)
                row.first_failure_seed = seed;
        }
    }
    ++row.compared;
    if (raw.scale > 0)
        ++row.nonzero;
}

void unresolved(numerical_check& row, std::uint64_t seed, const std::string& why) {
    if (!row.first_failure_seed)
        row.first_failure_seed = seed;
    if (row.detail.empty())
        row.detail = why;
}

real mass_value(const Algebra& a, Id mass, const parameters& values) {
    auto val = validation::evaluate_scalar(a, mass, values);
    if (val.imag() != 0 || !std::isfinite(val.real()) || val.real() < 0)
        throw std::invalid_argument("expected a finite nonnegative external mass for " + a.format(mass));
    return val.real();
}

// For large tree sums with a single minimal vector vertex, the same
// Feynman diagrams can be accumulated as currents on disjoint leg sets.
// The ordinary diagram evaluator remains the independent precision oracle.
using dirac_reference = std::pair<validation::dirac_current::recursion, real>;
std::optional<dirac_reference> compile_dirac_reference(const model& rules,
        const std::vector<std::string>& legs, const parameters& values) {
    if (legs.size() > 12 || rules.vertices().size() != 1) return {};
    const auto& vertex = rules.vertices().front();
    if (vertex.legs.size() != 3) return {};
    const auto& f = rules.particle(vertex.legs[0]);
    const auto& v = rules.particle(vertex.legs[2]);
    if (f.spin2 != 1 || f.name == f.antiname || vertex.legs[0] != f.antiname ||
        vertex.legs[1] != f.name || f.color != 1 || f.mass == "0" || f.width != "0" ||
        v.spin2 != 2 || v.name != v.antiname || v.color != 1 || v.mass != "0" || v.width != "0")
        return {};
    const auto& algebra = rules.algebra();
    if (algebra.format(vertex.expression) !=
        "ConstructiveDiagrams`DiracWord[ConstructiveDiagrams`DiracGamma[ConstructiveDiagrams`LorentzIndex[3]]]")
        return {};
    const auto coupling = -validation::evaluate_scalar(algebra, vertex.factor, values);
    if (coupling.imag() != 0 || !std::isfinite(coupling.real())) return {};
    std::vector<int> species;
    for (const auto& leg : legs) {
        const auto name = generation::feynman_external(rules, leg);
        if (name == f.name) species.push_back(1);
        else if (name == f.antiname) species.push_back(-1);
        else if (leg == v.name + ".+" || leg == v.name + ".-")
            species.push_back(leg.back() == '+' ? 2 : -2);
        else return {};
    }
    return dirac_reference{validation::dirac_current::recursion(species),coupling.real()};
}

enum class comparison { finite, reduction, reference, feynman, reference_amplitude };
struct probe {
    comparison type;
    std::string kind;
    std::optional<expression_evaluator> other;
};
} // namespace

bool validation_report::passed() const {
    return requested >= 10000 && !checks.empty() && !numerical.empty() &&
           std::all_of(checks.begin(), checks.end(),
                       [](const auto& r) { return r.status == check_outcome::pass; }) &&
           std::all_of(numerical.begin(), numerical.end(), [&](const auto& r) {
               return r.status == check_outcome::pass && r.compared == requested;
           });
}

validation_report amplitude::validate(const validation_options& options) {
    const auto& final = final_amplitude();
    auto settings = options;
    if (!settings.energy_bound)
        settings.energy_bound = high_energy_report_.bound;
    return validate_impl(final, before_final_reduction_ ? &*before_final_reduction_ : nullptr, settings);
}

validation_report amplitude::validate(const amp_expression& candidate, const validation_options& options) {
    return validate_impl(candidate, nullptr, options);
}

validation_report amplitude::validate_against(const amp_expression& reference, const validation_options& options) {
    auto settings = options;
    if (!settings.energy_bound) settings.energy_bound = last_high_energy().bound;
    return validate_impl(final_amplitude(), before_final_reduction_ ? &*before_final_reduction_ : nullptr,
                         settings, &reference);
}

validation_report amplitude::validate_impl(amp_expression candidate, const amp_expression* original,
                                           const validation_options& options, const amp_expression* reference) {
    if (options.points < 10000)
        throw std::invalid_argument("validation requires at least 10000 phase-space points");
    if (!std::isfinite(options.relative_tolerance) || !std::isfinite(options.absolute_tolerance) ||
        options.relative_tolerance < 0 || options.absolute_tolerance < 0)
        throw std::invalid_argument("invalid validation tolerances");

    if (options.samples_per_cut < 4 || !options.energy_directions)
        throw std::invalid_argument(
            "validation needs at least four cut samples and one fixed-angle direction");

    Algebra tree;
    const auto masses = candidate.import_masses(tree);
    std::vector<Id> expected;
    for (const auto& mass : masses_)
        expected.push_back(tree.parse(mass));
    if (masses != expected)
        throw std::invalid_argument("candidate has different external masses");
    tree.setKinematics(masses, {});
    auto root = candidate.import_into(tree);
    auto before = original ? std::optional<Id>(original->import_into(tree)) : std::nullopt;
    const auto seed = options.seed.value_or(
        static_cast<std::uint64_t>(std::chrono::high_resolution_clock::now().time_since_epoch().count()));
    validation_report report;
    report.seed = seed;
    report.requested = options.points;
    const auto bound = options.energy_bound.value_or(4 - static_cast<int>(masses.size()));
    std::vector<int> colors;
    if (vertices_model_) {
        for (const auto& name : particles_) {
            const auto mapped =
                vertices_model_->is_feynman() ? generation::feynman_external(*vertices_model_, name) : name;
            const auto& p = vertices_model_->particle(mapped);
            colors.push_back(p.color == 3 && mapped == p.antiname ? -3 : p.color);
        }
    }
    high_energy_options exact;
    exact.parameter_points = options.exact_parameter_points;
    try {
        // Validating a separately supplied result must preserve that result even
        // when the lower-point products have yet to be generated.
        std::optional<amplitude> staged;
        if (channels_model_ && !channels_generated_) {
            staged = *this;
            staged->generate_factorization_products(options.workers);
        }
        const amplitude& inventory = staged ? *staged : *this;
        if (exact.parameter_points.empty() && vertices_model_)
            inventory.bind_model_parameters(tree, root, exact, true);
        report.checks =
            native_builtin_checks(inventory, candidate, bound, exact.parameter_points,
                                  options.samples_per_cut, options.energy_directions, seed, colors,
                                  options.workers);
    } catch (const std::exception& e) {
        report.checks.push_back(
            {"BuiltIn", "External-kinematics checks", check_outcome::unresolved, 0, e.what()});
    }

    try {
        std::vector<probe> probes;
        parameters values = options.parameters;
        validation::wide_parameters exact_values;
        for (const auto& [name, value] : options.parameters)
            exact_values.emplace(name, wide::number(value));
        std::set<std::string> read_scopes;
        for (const auto* source : {&vertices_model_, &channels_model_}) {
            if (!*source || !read_scopes.insert((*source)->parameter_scope()).second)
                continue;
            auto local = validation::model_parameters(**source, options.parameters);
            values.insert(local.begin(), local.end());
            auto exact_local = validation::precise_model_parameters(**source, options.parameters);
            exact_values.insert(exact_local.begin(), exact_local.end());
        }
        std::vector<real> numeric_masses;
        for (Id mass : masses)
            numeric_masses.push_back(mass_value(tree, mass, values));
        if (masses.size() < 4)
            throw std::invalid_argument("real two-beam scattering requires at least four legs");
        std::vector<wide::scalar> exact_masses;
        std::string replay_mass_error;
        try {
            for (Id mass : masses) {
                auto m = validation::precise_scalar(tree, mass, exact_values);
                if (m.y != 0 || m.x < 0)
                    throw std::domain_error("invalid external mass in precision replay");
                exact_masses.push_back(std::move(m.x));
            }
        } catch (const std::exception& e) {
            replay_mass_error = e.what();
        }
        const expression_evaluator current(tree, root, values, colors, &exact_values);
        if (reference) {
            if (reference->import_masses(tree) != masses)
                throw std::invalid_argument("reference amplitude has different external masses");
            probes.push_back({comparison::reference_amplitude, "ReferenceAmplitude",
                              expression_evaluator(tree, reference->import_into(tree), values, colors, &exact_values)});
        }
        if (before && *before != root)
            probes.push_back({comparison::reduction, "FinalReduction",
                              expression_evaluator(tree, *before, values, colors, &exact_values)});
        if (contains_reference(tree, root))
            probes.push_back({comparison::reference, "Reference", {}});
        Algebra feynman_tree;
        std::optional<dirac_reference> fast_reference;
        std::vector<expression_evaluator> zero_comparison_terms;
        if (feynman_model_) {
            try {
                if (!feynman_generated_)
                    generate_feynman_diagrams(options.workers);
                const auto& rules = *feynman_model_;
                auto fvalues = validation::model_parameters(rules, options.parameters);
                auto exact_fvalues = validation::precise_model_parameters(rules, options.parameters);
                std::vector<Id> fm;
                for (std::size_t i = 0; i < particles_.size(); ++i) {
                    const auto& def = rules.particle(generation::feynman_external(rules, particles_[i]));
                    fm.push_back(feynman_tree.parse(def.mass));
                    real m = mass_value(feynman_tree, fm.back(), fvalues);
                    if (std::abs(m - numeric_masses[i]) >
                        64 * std::numeric_limits<real>::epsilon() * std::max(m, numeric_masses[i]))
                        throw std::invalid_argument("different numerical external masses for " +
                                                    particles_[i]);
                }
                feynman_tree.setKinematics(fm, {});
                // Pull common scalar factors out of each signed diagram before
                // compiling the independently generated Feynman sum. Keep
                // color networks intact across contractions of dummy indices.
                const bool singlets = std::all_of(colors.begin(), colors.end(), [](int rep) { return rep == 1; });
                std::vector<Id> terms;
                for (const auto& row : feynman_rows_) {
                    auto raw = row.expression.import_into(feynman_tree);
                    const auto diagram = feynman_to_spinors(feynman_tree, raw);
                    terms.push_back(singlets ? feynman_tree.factorSum(diagram) : diagram);
                }
                if (root == tree.integer(0) && terms.size() > 1)
                    for (Id term : terms)
                        zero_comparison_terms.emplace_back(feynman_tree, term, fvalues, colors, &exact_fvalues);
                auto sum = feynman_tree.add(std::move(terms));
                probes.push_back({comparison::feynman, "Feynman",
                                  expression_evaluator(feynman_tree, sum, fvalues, colors, &exact_fvalues)});
                if (feynman_rows_.size() >= 4096 && root != tree.integer(0)) {
                    fast_reference = compile_dirac_reference(rules,particles_,fvalues);
                    // Check conventions against the actual stored diagrams before
                    // using the shared subcurrents on the remaining points.
                    for (unsigned j = 0; fast_reference && j < 4; ++j) {
                        auto p = validation::sample_point(numeric_masses, chart_detail::mix(seed+j));
                        auto x = fast_reference->first(p,fast_reference->second,
                                                       validation::dirac_current::gauge::independent);
                        auto y = (*probes.back().other)(p);
                        if (!(std::abs(x-y) <= 1e-8L * std::max(std::abs(x),std::abs(y))))
                            fast_reference.reset();
                    }
                }
            } catch (const std::exception& e) {
                numerical_check failed{"Feynman"};
                failed.detail = e.what();
                report.numerical.push_back(std::move(failed));
            }
        }
        if (probes.empty())
            probes.push_back({comparison::finite, "FiniteAmplitude", {}});
        constexpr std::size_t batch_size = 128;
        const auto count = 1 + (options.points - 1) / batch_size;
        const auto workers =
            std::min(count, options.workers ? options.workers
                                            : std::size_t(std::max(1u, std::thread::hardware_concurrency())));
        std::vector<numerical_check> empty;
        for (const auto& p : probes)
            empty.push_back(numerical_check{p.kind});
        std::vector<std::vector<numerical_check>> chunks(count, empty);
        fit_detail::parallel_indices(count, workers, [&](std::size_t chunk) {
            auto& local = chunks[chunk];
            auto start = chunk * batch_size;
            for (std::size_t j = 0; j < std::min(batch_size, options.points - start); ++j) {
                const auto point_seed = chart_detail::mix(seed + start + j);
                try {
                    auto pt = validation::sample_point(numeric_masses, point_seed);
                    const auto value = current(pt);
                    std::optional<wide::phase_point> wide_point;
                    for (std::size_t i = 0; i < probes.size(); ++i) {
                        try {
                            const auto& p = probes[i];
                            std::optional<validation::point> alternate;
                            if (p.type == comparison::reference) {
                                alternate = pt;
                                alternate->change_references(chart_detail::mix(pt.reference_seed + 1));
                            }
                            const auto& target_point = alternate ? *alternate : pt;
                            const auto& evaluator = p.other ? *p.other : current;
                            complex target;
                            if (p.type == comparison::feynman && fast_reference)
                                target = fast_reference->first(target_point,fast_reference->second,
                                                        validation::dirac_current::gauge::independent);
                            else target = p.type == comparison::finite ? value : evaluator(target_point);
                            compare(local[i], value, target, point_seed, options, [&]() {
                                if (!replay_mass_error.empty())
                                    throw std::domain_error(replay_mass_error);
                                if (!wide_point)
                                    wide_point = wide::reconstruct(pt, exact_masses);
                                auto lhs = current.precise(pt, *wide_point);
                                if (alternate) {
                                    auto second = *wide_point;
                                    second.reference_seed = alternate->reference_seed;
                                    return std::make_pair(std::move(lhs),
                                                          evaluator.precise(target_point, second));
                                }
                                return std::make_pair(std::move(lhs),
                                                      evaluator.precise(target_point, *wide_point));
                            }, [&]() -> real {
                                // Where an exact zero is compared to canceling independent diagrams,
                                // measure 160-bit rounding against the size of the summands. A
                                // physically small but noncanceling amplitude retains relative sensitivity.
                                if (p.type != comparison::feynman || zero_comparison_terms.empty()) return 0;
                                real scale = 0;
                                for (const auto& term : zero_comparison_terms)
                                    scale += wide::narrow_real(term.precise(target_point, *wide_point).magnitude());
                                if (!std::isfinite(scale))
                                    throw std::runtime_error("Nonfinite diagram scale in precision replay");
                                return std::ldexp(scale, -static_cast<int>(wide::bits) + 24);
                            });
                        } catch (const std::exception& e) {
                            unresolved(local[i], point_seed, e.what());
                        }
                    }
                } catch (const std::exception& e) {
                    for (auto& row : local)
                        unresolved(row, point_seed, e.what());
                }
            }
        });
        for (std::size_t i = 0; i < probes.size(); ++i) {
            auto row = empty[i];
            for (const auto& chunk : chunks) {
                const auto& r = chunk[i];
                row.compared += r.compared;
                row.nonzero += r.nonzero;
                row.failures += r.failures;
                row.replays += r.replays;
                row.max_replay_absolute_error =
                    std::max(row.max_replay_absolute_error, r.max_replay_absolute_error);
                row.max_replay_relative_error =
                    std::max(row.max_replay_relative_error, r.max_replay_relative_error);
                if (!row.first_raw_outlier_seed)
                    row.first_raw_outlier_seed = r.first_raw_outlier_seed;
                row.max_absolute_error = std::max(row.max_absolute_error, r.max_absolute_error);
                row.max_relative_error = std::max(row.max_relative_error, r.max_relative_error);
                if (!row.first_failure_seed)
                    row.first_failure_seed = r.first_failure_seed;
                if (row.detail.empty())
                    row.detail = r.detail;
            }
            row.status = row.failures ? check_outcome::fail
                                      : (row.compared == options.points ? check_outcome::pass
                                                                        : check_outcome::unresolved);
            report.numerical.push_back(std::move(row));
        }
    } catch (const std::exception& e) {
        numerical_check failed{"PhaseSpace"};
        failed.detail = e.what();
        report.numerical.push_back(std::move(failed));
    }
    return report;
}
} // namespace hepcat
