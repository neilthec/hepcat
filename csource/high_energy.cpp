#include "amplitude.hpp"
#include "high_energy_chart.hpp"
#include "high_energy_contacts.hpp"
#include "vector_compact.hpp"
#include "native_fit.hpp"
#include "modular_fit.hpp"
#include "chart_scalar.hpp"
#include <algorithm>
#include <cmath>
#include <functional>
#include <map>
#include <numeric>
#include <set>
#include <thread>

namespace hepcat {
namespace {
using Q = mpq_class;
using point = std::map<Id, Q>;

struct high_energy_fit {
    Algebra& a;
    Id seed;
    std::optional<Id> expanded_seed;
    std::vector<point> bindings;
    high_energy_report report;
    std::size_t index = 0, workers = 1;
    std::vector<Id> basis;
    std::set<Id> unique;
    high_energy_fit(Algebra& tree, Id source, const high_energy_options& options) : a(tree), seed(source) {
        report.bound = options.energy_bound.value_or(4 - static_cast<int>(a.masses().size()));
        workers = options.workers ? options.workers : std::max(1u, std::thread::hardware_concurrency());
        for (const auto& assignments : options.parameter_points) {
            point parameters;
            for (const auto& assignment : assignments) {
                Id key = a.parse(assignment.first), value = a.parse(assignment.second);
                Q q;
                if (!a.number(value, q))
                    throw std::invalid_argument("high-energy parameter values must be exact rational numbers");
                if (!chart_detail::free_parameter(a.node(key)))
                    throw std::invalid_argument("expected a scalar parameter name");
                if (!parameters.emplace(key, q).second)
                    throw std::invalid_argument("duplicate high-energy parameter");
            }
            if (std::find(bindings.begin(), bindings.end(), parameters) == bindings.end())
                bindings.push_back(std::move(parameters));
        }
        if (bindings.empty()) bindings.emplace_back();
    }
    template <class F> auto charts(std::size_t count, F evaluate, bool validation) {
        using result_type = decltype(evaluate(std::size_t{}, std::declval<const point&>()));
        std::vector<result_type> out(count);
        auto threads = std::min(count, workers);
        report.workers = std::max(report.workers, threads);
        fit_detail::parallel_indices(count, threads, [&](std::size_t j) {
            auto ordinal = index + j;
            for (std::size_t retry = 0;; ++retry) {
                try {
                    out[j] = evaluate(ordinal * 128 + retry, bindings[ordinal % bindings.size()]);
                    break;
                } catch (const residue_singular&) {
                    if (retry >= 127) throw;
                }
            }
        });
        index += count;
        (validation ? report.validation_samples : report.training_samples) += count;
        return out;
    }
    bool validate(Id value) {
        // Four different conserved directions at every supplied parameter
        // point. Spin projections vary independently across these directions.
        auto tests = charts(
            std::max<std::size_t>(4, 4 * bindings.size()),
            [&](std::size_t i, const point& p) {
                return high_energy_chart_rows(a, {value}, report.bound, i, p).empty() ? 1 : 0;
            },
            true);
        if (!std::all_of(tests.begin(), tests.end(), [](int v) { return v != 0; })) return false;
        auto spin = charts(
            std::max<std::size_t>(2, bindings.size()),
            [&](std::size_t i, const point& p) {
                return high_energy_validate_chart(a, value, report.bound, i, p);
            },
            true);
        report.exhaustive_spin = true;
        for (const auto& check : spin) {
            report.validation_spin_states += check.spin_states;
            report.exhaustive_spin &= check.exhaustive_spin;
            if (!check.passes) return false;
        }
        return true;
    }
    bool add(Id x, bool required = false) {
        if (!high_energy_local(a, x)) {
            if (required)
                throw std::invalid_argument(
                    "high-energy correction directions must be pole-free local expressions");
            return false;
        }
        if (x == a.integer(0) || !unique.insert(x).second) return false;
        basis.push_back(x);
        return true;
    }
    std::optional<Id> solve(const char* route, bool exact = false) {
        if (basis.empty()) return {};
        exact = exact || basis.size() <= 20;
        fit_detail::sparse_system rational(basis.size());
        fit_detail::modular_system modular(basis.size());
        std::vector<Id> roots{seed};
        roots.insert(roots.end(), basis.begin(), basis.end());
        const auto block = std::max<std::size_t>(4, std::min<std::size_t>(16, bindings.size() * 4));
        std::size_t previous_rank = 0, unchanged = 0;
        for (std::size_t used = 0; used < 3 * basis.size() + 32; used += block) {
            auto samples = charts(
                block,
                [&](std::size_t i, const point& p) {
                    return high_energy_chart_rows(a, roots, report.bound, i, p);
                },
                false);
            for (const auto& sample : samples)
                for (const auto& row : sample) {
                    if (exact) {
                        if (!rational.add(row, 1, -row[0])) return {};
                    } else {
                        try {
                            if (!modular.add(row, 1, -row[0])) return {};
                        } catch (const residue_singular&) {
                            continue;
                        }
                    }
                }
            auto rank = exact ? rational.rank() : modular.rank();
            if (!rank) {
                if (used < 4 * bindings.size() + 8) continue;
                return {};
            }
            unchanged = rank == previous_rank ? unchanged + 1 : 0;
            previous_rank = rank;
            std::optional<std::vector<Q>> coefficients;
            if (exact)
                coefficients = rational.solution();
            else
                coefficients = modular.solution();
            if (coefficients) {
                std::vector<Id> result{seed};
                for (std::size_t c = 0; c < basis.size(); ++c)
                    if ((*coefficients)[c] != 0)
                        result.push_back(a.mul({a.exact((*coefficients)[c]), basis[c]}));
                Id proposal = a.add(result);
                if (expanded_seed) {
                    result.front() = *expanded_seed;
                    Id expanded = a.add(std::move(result));
                    if (a.cost(expanded) < a.cost(proposal)) proposal = expanded;
                }
                if (validate(proposal)) {
                    report.candidates = basis.size();
                    report.rank = rank;
                    report.route = route;
                    report.free_coefficients = basis.size() - rank;
                    return proposal;
                }
            }
            if (unchanged >= 3) {
                // A stable consistent modular system can require coefficients
                // beyond the single-prime reconstruction range. Solve in Q
                // before discarding that space. A determined contradiction in
                // ordinary candidate spaces is encountered during row addition.
                if (!exact && !coefficients) return solve(route, true);
                return {};
            }
        }
        return {};
    }
};

} // namespace

const amp_expression& amplitude::final_amplitude() const {
    if (!high_energy_result_) throw std::logic_error("complete the high-energy stage before validating the final amplitude");
    return *high_energy_result_;
}

const high_energy_report& amplitude::last_high_energy() const {
    if (!high_energy_result_) throw std::logic_error("no validated high-energy result");
    return high_energy_report_;
}
void amplitude::bind_model_parameters(Algebra& a, Id seed, high_energy_options& settings, bool include_targets) const {
    std::vector<Id> roots{seed};
    for (Id mass : a.masses())
        roots.push_back(mass);
    for (const auto& expr : settings.correction_basis)
        roots.push_back(a.parse(expr));
    // Retained terms can carry a parameter that has canceled from the seed.
    for (const auto* items : {&diagrams_, &products_})
        for (const auto& row : *items)
            if (auto rest = row.expression.discarded_remainder()) roots.push_back(rest->import_into(a));
    if (include_targets)
        for (const auto& row : products_) {
            roots.push_back(row.expression.import_into(a));
            for (const auto& shell : row.shells) roots.push_back(a.parse(shell.mass));
        }
    std::set<Id> visited;
    std::vector<std::string> active;
    while (!roots.empty()) {
        Id id = roots.back();
        roots.pop_back();
        if (!visited.insert(id).second) continue;
        const auto& n = a.node(id);
        if (n.head == "ModelParameter")
            active.push_back(a.format(id));
        else
            roots.insert(roots.end(), n.args.begin(), n.args.end());
    }
    std::set<std::string> scopes, resolved;
    for (const auto* source : {&vertices_model_, &channels_model_}) {
        if (!*source || !scopes.insert((*source)->parameter_scope()).second) continue;
        auto points = (*source)->high_energy_parameter_points(active);
        if (points.empty()) continue;
        for (const auto& pair : points[0])
            resolved.insert(pair.first);
        if (settings.parameter_points.empty())
            settings.parameter_points = std::move(points);
        else {
            if (points.size() != settings.parameter_points.size())
                throw std::logic_error("incompatible model sample counts");
            for (std::size_t i = 0; i < points.size(); ++i)
                settings.parameter_points[i].insert(settings.parameter_points[i].end(), points[i].begin(),
                                                    points[i].end());
        }
    }
    for (const auto& key : active)
        if (!resolved.count(key)) throw std::invalid_argument("model does not declare " + key);
}

const amp_expression& amplitude::complete_high_energy(std::size_t workers) {
    high_energy_options options;
    options.workers = workers;
    return complete_high_energy(options);
}
const amp_expression& amplitude::complete_high_energy(const high_energy_options& options) {
    if (!completed_) {
        // A failed downstream stage must not partially publish preparation or
        // factorization on a previously unprepared process.
        auto staged = *this;
        staged.complete_factorization(options.workers);
        staged.complete_high_energy(*staged.completed_, options);
        *this = std::move(staged);
        return *high_energy_result_;
    }
    return complete_high_energy(*completed_, options);
}

const amp_expression& amplitude::complete_high_energy(const amp_expression& expression,
                                                      const high_energy_options& options) {
    complete_energy_components(expression, options);
    auto& result = *high_energy_result_;
    // Work on the whole expression after the color components and diagrams
    // have been combined. Factoring an isolated component can prevent sharing
    // across components. Keep only the winning tree in the amplitude arena.
    Algebra trial;
    const Id initial = result.import_into(trial);
    Id compact = masses_.size() == 4 ? compact_commuting_products(trial, initial) : initial;
    compact = collect_scalar_factors(trial, compact, 3);
    compact = collect_simple_fractions(trial, compact);
    if (compact != initial) result.set_from(trial, compact);
    high_energy_report_.nodes_after_final_reduction = result.leaf_count();
    return result;
}

const amp_expression& amplitude::complete_energy_components(const amp_expression& expression,
                                                            const high_energy_options& options) {
    if (complete_color_rows(nullptr, &options, &expression)) return *high_energy_result_;
    Algebra a;
    std::vector<Id> masses;
    for (const auto& input : masses_)
        masses.push_back(a.parse(input));
    if (masses != expression.import_masses(a))
        throw std::invalid_argument("high-energy input has different external masses");
    a.setKinematics(std::move(masses), {});
    auto seed = expression.import_into(a);
    auto settings = options;
    if (settings.parameter_points.empty() && vertices_model_) bind_model_parameters(a, seed, settings);
    high_energy_fit work(a, seed, settings);
    auto publish = [&](Id answer, const char* unchanged = nullptr) -> const amp_expression& {
        amp_expression result("0", masses_);
        // Every diagram and cut has already been combined. Only external
        // kinematics apply to the expression returned to the caller.
        result.set_from(a, answer);
        if (answer == seed) {
            result.ordered_formula_ = expression.ordered_formula_;
            result.sewing_ = expression.sewing_;
        }
        auto original = result; // The two roots share the original interned tree.
        if (vertices_model_) result.set_from(a, contract_model_couplings(a,answer,*vertices_model_));
        result.reduce_with_factor_passes(1);
        if (a.masses().size() == 4 && std::all_of(a.masses().begin(), a.masses().end(),
                [&](Id mass) { return chart_detail::free_parameter(a.node(mass)); })) {
            const Id reduced = result.import_into(a);
            const Id compact = compact_vector_amplitude(a, reduced);
            if (compact != reduced) result.set_from(a, compact);
        }
        work.report.nodes_before_final_reduction = original.arena_->cost(original.root_);
        work.report.nodes_after_final_reduction = result.arena_->cost(result.root_);
        before_final_reduction_ = std::move(original);
        if (unchanged) work.report.route = unchanged;
        construction_.reset();
        high_energy_result_ = std::move(result);
        high_energy_report_ = std::move(work.report);
        return *high_energy_result_;
    };
    for (const auto& entry : options.correction_basis)
        work.add(a.parse(entry), true);
    // Keep a simple direction independent of bounded offsets ahead of an
    // equivalent direction mixed with a regular piece. Free coefficients are
    // set to zero in this ordered basis.
    std::stable_sort(work.basis.begin(), work.basis.end(), [&](Id x, Id y) { return a.cost(x) < a.cost(y); });
    if (work.validate(seed)) return publish(seed, "Unchanged");
    if (!work.basis.empty())
        if (auto x = work.solve("ContactBasis", true)) return publish(*x);
    std::size_t before = work.basis.size();
    // Separate additive terms before admitting them as independent local
    // directions. Reweighting a factored sum such as g*(7+p.q) as one
    // direction would erase its unconstrained constant when removing p.q.
    // A cap skips automatic expansion of a large product. No discarded terms
    // are ever removed from the seed.
    std::function<std::vector<Id>(Id)> monomials = [&](Id value) -> std::vector<Id> {
        constexpr std::size_t limit = 256;
        const auto& node = a.node(value);
        if (node.head != "Plus" && node.head != "Times") {
            if (node.head == "Power" && node.args.size() == 2 && a.node(node.args[0]).head == "Plus") {
                mpq_class exp;
                if (a.number(node.args[1], exp) && exp.get_den() == 1 && exp > 1 && exp <= 8) {
                    auto parts = monomials(node.args[0]);
                    std::vector<Id> out{a.integer(1)};
                    for (long i = 0; i < exp.get_num().get_si(); ++i) {
                        if (parts.empty() || out.size() > limit / parts.size()) return {};
                        std::vector<Id> next;
                        for (Id l : out)
                            for (Id r : parts)
                                next.push_back(a.mul({l, r}));
                        out = std::move(next);
                    }
                    return out;
                }
                if (a.number(node.args[1], exp) && exp > 0) return {};
            }
            return {value};
        }
        std::vector<Id> out;
        if (node.head == "Times") out.push_back(a.integer(1));
        for (Id child : node.args) {
            auto parts = monomials(child);
            if (node.head == "Plus") {
                if (parts.empty() || parts.size() + out.size() > limit) return {};
                out.insert(out.end(), parts.begin(), parts.end());
            } else {
                if (parts.empty() || out.size() > limit / parts.size()) return {};
                std::vector<Id> next;
                for (Id l : out)
                    for (Id r : parts)
                        next.push_back(a.mul({l, r}));
                out = std::move(next);
            }
        }
        return out;
    };
    auto retain_terms = [&](Id value) {
        for (Id term : monomials(value))
            work.add(term);
    };
    for (const auto& d : diagrams_)
        retain_terms(d.expression.import_into(a, true));
    for (const auto& c : products_)
        retain_terms(c.expression.import_into(a, true));
    if (work.basis.size() > before)
        if (auto x = work.solve("RetainedContacts")) return publish(*x);
    // Existing explicit contact contributions can be reweighted independently.
    before = work.basis.size();
    auto seed_terms = monomials(seed);
    if (!seed_terms.empty()) {
        work.expanded_seed = a.add(seed_terms);
        for (Id term : seed_terms)
            work.add(term);
    }
    if (work.basis.size() > before)
        if (auto x = work.solve("InputContacts")) return publish(*x);
    auto contacts = high_energy_contact_catalog(a, seed);
    for (auto stage : {std::make_pair("PolarizationContacts", &contacts.polarization),
                       std::make_pair("LocalSpinorContacts", &contacts.minimal),
                       std::make_pair("LocalMassRatios", &contacts.extended)}) {
        before = work.basis.size();
        for (Id direction : *stage.second)
            work.add(direction);
        if (work.basis.size() > before)
            if (auto x = work.solve(stage.first)) return publish(*x);
    }
    throw std::runtime_error("high-energy completion unresolved: no local correction in the available basis "
                             "passed the growth bound");
}
} // namespace hepcat
