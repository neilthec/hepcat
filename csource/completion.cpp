#include "amplitude.hpp"
#include "line_current.hpp"
#include "four_point_numerators.hpp"
#include "fermion_currents.hpp"
#include "native_fit.hpp"
#include "modular_fit.hpp"
#include "continuations.hpp"
#include "residue.hpp"
#include "validation_inventory.hpp"

#include <algorithm>
#include <atomic>
#include <exception>
#include <functional>
#include <map>
#include <set>
#include <stdexcept>
#include <thread>
#include <unordered_map>
#include <unordered_set>
#include <utility>

namespace hepcat {
namespace {
using Q = mpq_class;
using Cut = std::pair<std::vector<int>, Id>;
struct sewn_source {
    Id prepared, full;
    numerator_shell channel;
};

Cut key(const Algebra& a, std::vector<int> legs, Id mass) {
    std::sort(legs.begin(), legs.end());
    if (legs.size() < 2 || legs.size() + 2 > a.masses().size() || legs.front() < 1 ||
        static_cast<std::size_t>(legs.back()) > a.masses().size() ||
        std::adjacent_find(legs.begin(), legs.end()) != legs.end())
        throw residue_unsupported(
            "factorization requires at least two external legs on each side of a partition");
    std::vector<int> other;
    for (std::size_t i = 1; i <= a.masses().size(); ++i)
        if (!std::binary_search(legs.begin(), legs.end(), static_cast<int>(i)))
            other.push_back(static_cast<int>(i));
    if (other.size() < legs.size() || (other.size() == legs.size() && other < legs)) legs.swap(other);
    return {std::move(legs), mass};
}

Cut prop_key(const Algebra& a, Id id) {
    const auto& args = a.node(id).args;
    if (args.size() != 2 || a.node(args[0]).head != "Mom" || a.node(args[0]).args.size() != 1)
        throw residue_unsupported("unsupported propagator momentum");
    const auto& multi = a.node(a.node(args[0]).args[0]);
    if (multi.head != "Multiparticle")
        throw residue_unsupported("expected an external-leg partition in PropDen");
    std::vector<int> legs;
    for (Id slot : multi.args)
        legs.push_back(a.leg(slot));
    return key(a, std::move(legs), args[1]);
}

class expressions {
    Algebra& a;
    std::unordered_map<Id, Id> normal;
    std::unordered_map<Id, bool> kinematic, singular;

    Id propagator(const Cut& cut) {
        std::vector<Id> labels;
        for (int leg : cut.first)
            labels.push_back(a.integer(leg));
        return a.call("PropDen", {a.call("Mom", {a.call("Multiparticle", std::move(labels))}), cut.second});
    }

    std::vector<int> invariant_legs(Id id) {
        const auto& n = a.node(id);
        std::vector<Id> labels;
        if (n.head == "Mandelstahm")
            labels = n.args;
        else if (n.head == "Power" && n.args.size() == 2 && n.args[1] == a.integer(2)) {
            const auto& mom = a.node(n.args[0]);
            if (mom.head == "Mom" && mom.args.size() == 1) labels = mom.args;
        }
        std::vector<int> legs;
        for (Id label : labels) {
            const auto& l = a.node(label);
            if (l.head == "Multiparticle")
                for (Id arg : l.args)
                    legs.push_back(a.leg(arg));
            else
                legs.push_back(a.leg(label));
        }
        std::sort(legs.begin(), legs.end());
        if (legs.size() < 2 || legs.size() + 2 > a.masses().size() || legs.front() < 1 ||
            static_cast<std::size_t>(legs.back()) > a.masses().size() ||
            std::adjacent_find(legs.begin(), legs.end()) != legs.end())
            return {};
        return legs;
    }

    std::optional<Id> square_root_mass(Id squared) {
        const auto& n = a.node(squared);
        if (n.head == "Power" && n.args.size() == 2 && n.args[1] == a.integer(2) && !has_kinematics(n.args[0]))
            return n.args[0];
        Q value;
        if (!a.number(squared, value) || value < 0 || !mpz_perfect_square_p(value.get_num_mpz_t()) ||
            !mpz_perfect_square_p(value.get_den_mpz_t()))
            return {};
        mpz_class num, den;
        mpz_sqrt(num.get_mpz_t(), value.get_num_mpz_t());
        mpz_sqrt(den.get_mpz_t(), value.get_den_mpz_t());
        return a.exact(Q(num, den));
    }

    // Only normalize a complete inverse factor. An invariant in a numerator
    // or a summand of an unrelated denominator does not introduce its own pole.
    Id denominator(Id id) {
        const auto& n = a.node(id);
        if (n.head == "Times") {
            auto args = n.args;
            for (Id& arg : args)
                arg = denominator(arg);
            return a.mul(std::move(args));
        }
        Id mass = a.integer(0);
        auto legs = invariant_legs(id);
        if (n.head == "Plus" && n.args.size() == 2) {
            for (unsigned side = 0; side < 2; ++side) {
                legs = invariant_legs(n.args[side]);
                if (legs.empty()) continue;
                auto m = square_root_mass(a.mul({a.integer(-1), n.args[1 - side]}));
                if (!m) return id;
                mass = *m;
                break;
            }
        }
        if (!legs.empty()) return propagator(key(a, std::move(legs), mass));
        if (n.head == "MomProd" && n.args.size() == 2 && a.masses().size() >= 4) {
            int i = a.leg(n.args[0]), j = a.leg(n.args[1]);
            if (i < 1 || j < 1 || i == j || static_cast<std::size_t>(std::max(i, j)) > a.masses().size())
                return id;
            Id m1 = a.masses()[i - 1], m2 = a.masses()[j - 1];
            if (m1 == mass || m2 == mass)
                return a.mul({a.exact(Q(1, 2)), propagator(key(a, {i, j}, m1 == mass ? m2 : m1))});
        }
        return id;
    }

  public:
    explicit expressions(Algebra& arena) : a(arena) {}
    Id canonical(Id id) {
        if (auto it = normal.find(id); it != normal.end()) return it->second;
        const auto& n = a.node(id);
        Id result = id;
        if (n.compound) {
            auto args = n.args;
            for (Id& child : args)
                child = canonical(child);
            if (n.head == "Power" && args.size() == 2) {
                Q exponent;
                if (a.number(args[1], exponent) && exponent < 0 && exponent.get_den() == 1) {
                    if (a.node(args[0]).head == "Mom" && exponent.get_num() % 2 == 0) {
                        Id normalized = denominator(a.pow(args[0], 2));
                        if (a.node(normalized).head == "PropDen") args = {normalized, a.exact(exponent / 2)};
                    } else {
                        args[0] = denominator(args[0]);
                    }
                }
            }
            result = a.call(n.head, std::move(args));
            if (n.head == "PropDen") result = propagator(prop_key(a, result));
        }
        normal.emplace(id, result);
        return result;
    }
    bool has_kinematics(Id id) {
        if (auto it = kinematic.find(id); it != kinematic.end()) return it->second;
        const auto& n = a.node(id);
        bool found = n.head == "PropDen" || n.head == "SpinorChain" || n.head == "SpinorTrace" ||
                     n.head == "Mom" || n.head == "Mandelstahm" || n.head == "MomProd";
        for (Id x : n.args)
            if (!found) found = has_kinematics(x);
        kinematic.emplace(id, found);
        return found;
    }
    bool has_pole(Id id) {
        if (auto it = singular.find(id); it != singular.end()) return it->second;
        const auto& n = a.node(id);
        Q q;
        bool found = n.head == "Power" && n.args.size() == 2 && a.number(n.args[1], q) && q < 0 &&
                     has_kinematics(n.args[0]);
        for (Id x : n.args)
            if (!found) found = has_pole(x);
        singular.emplace(id, found);
        return found;
    }
    // Split only unsquared sums and products. Leave large products factored.
    // In a mixed sum inside a common scalar factor, isolate the free contact
    // term before allowing a fitted coefficient on any part of the seed.
    std::pair<Id, Id> split_poles(Id id) {
        Id zero = a.integer(0);
        if (!has_pole(id)) return {id, zero};
        const auto& n = a.node(id);
        if (n.head == "Plus") {
            std::vector<Id> regular, poles;
            for (Id x : n.args) {
                auto parts = split_poles(x);
                regular.push_back(parts.first);
                poles.push_back(parts.second);
            }
            return {a.add(std::move(regular)), a.add(std::move(poles))};
        }
        if (n.head == "Times") {
            std::vector<Id> regular;
            for (Id x : n.args) {
                Id r = split_poles(x).first;
                if (r == zero) return {zero, id};
                regular.push_back(r);
            }
            Id r = a.mul(std::move(regular));
            return {r, a.add({id, a.mul({a.integer(-1), r})})};
        }
        Q exponent;
        if (n.head == "Power" && n.args.size() == 2 && a.number(n.args[1], exponent) && exponent > 0 &&
            exponent.get_den() == 1 && exponent.get_num().fits_slong_p()) {
            Id r = split_poles(n.args[0]).first;
            if (r != zero) {
                r = a.pow(r, exponent.get_num().get_si());
                return {r, a.add({id, a.mul({a.integer(-1), r})})};
            }
        }
        return {zero, id};
    }
};

bool contains(const Algebra& a, Id id, const std::function<bool(const Node&)>& predicate) {
    std::vector<Id> stack{id};
    std::unordered_set<Id> visited;
    while (!stack.empty()) {
        Id x = stack.back();
        stack.pop_back();
        if (!visited.insert(x).second) continue;
        const auto& n = a.node(x);
        if (predicate(n)) return true;
        stack.insert(stack.end(), n.args.begin(), n.args.end());
    }
    return false;
}

Id sewn(Algebra& a, Id input) {
    auto contour = [](const Node& n) {
        return !n.compound && (n.head == "ConstructiveDiagrams`z" || n.head == "z");
    };
    if (!contains(a, input, contour)) return a.mul({a.integer(-1), input});
    // f is admissible only when z f has no contour dependence. Compute its
    // exact homogeneity while removing z, including a contour inside a sum.
    using Term = std::pair<mpz_class, Id>;
    std::unordered_map<Id, Term> cache;
    std::function<Term(Id)> strip = [&](Id id) -> Term {
        if (auto it = cache.find(id); it != cache.end()) return it->second;
        const auto& n = a.node(id);
        Term term{0, id};
        if (contour(n))
            term = {1, a.integer(1)};
        else if (n.compound) {
            std::vector<Id> args;
            std::vector<mpz_class> degrees;
            for (Id x : n.args) {
                auto [d, y] = strip(x);
                degrees.push_back(std::move(d));
                args.push_back(y);
            }
            if (n.head == "Plus" && !degrees.empty()) {
                term.first = degrees.front();
                for (const auto& d : degrees)
                    if (d != term.first) throw residue_unsupported("sum has residual contour dependence");
            } else if (n.head == "Times") {
                for (const auto& d : degrees)
                    term.first += d;
            } else if (n.head == "Power" && args.size() == 2 && degrees[0] != 0) {
                Q q;
                if (degrees[1] != 0 || !a.number(args[1], q) || q.get_den() != 1)
                    throw residue_unsupported("unsupported contour exponent");
                term.first = degrees[0] * q.get_num();
            } else
                for (const auto& d : degrees)
                    if (d != 0) throw residue_unsupported("unremoved contour deformation");
            term.second = a.call(n.head, std::move(args));
        }
        cache.emplace(id, term);
        return term;
    };
    auto [degree, result] = strip(input);
    if (degree != -1) throw residue_unsupported("channel expression must have contour degree -1");
    return a.mul({a.integer(-1), result});
}

void add_poles(const Algebra& a, Id id, std::map<Cut, Id>& targets, Id zero) {
    std::unordered_set<Id> visited;
    std::function<void(Id)> scan = [&](Id x) {
        if (!visited.insert(x).second) return;
        const auto& n = a.node(x);
        Q q;
        if (n.head == "Power" && n.args.size() == 2 && a.number(n.args[1], q) && q < 0) {
            const auto& base = a.node(n.args[0]);
            if (base.head == "PropDen") targets.emplace(prop_key(a, n.args[0]), zero);
            // A product denominator can contain several inverse propagators.
            if (base.head == "Times")
                for (Id f : base.args)
                    if (a.node(f).head == "PropDen") targets.emplace(prop_key(a, f), zero);
        }
        for (Id child : n.args)
            scan(child);
    };
    scan(id);
}

struct channel {
    numerator_shell cut;
    Id target;
};

struct sample_row {
    std::size_t cut, index;
    std::vector<Q> values;
};
std::vector<sample_row> sample(const Algebra& a, const std::vector<channel>& channels, Id seed,
                               const std::vector<Id>& basis, std::size_t first, std::size_t count,
                               std::size_t workers) {
    std::vector<sample_row> rows(channels.size() * count);
    fit_detail::parallel_indices(channels.size(), workers, [&](std::size_t i) {
        std::vector<Id> expressions{channels[i].target, seed};
        expressions.insert(expressions.end(), basis.begin(), basis.end());
        for (std::size_t j = 0; j < count; ++j) {
            // A retry retains the two chirality bits. Distinct sample blocks
            // never reuse a point already incorporated in fitting.
            auto index = (first + j) * 256 + (first + j) % 4;
            for (unsigned retry = 0;; ++retry, index += 4) {
                try {
                    rows[i * count + j] = {i, index, residue_sample(a, channels[i].cut, expressions, index)};
                    break;
                } catch (const residue_singular& e) {
                    if (retry == 63)
                        throw residue_singular(std::string("cannot sample cut ") + std::to_string(i + 1) +
                                               ": " + e.what());
                }
            }
        }
    });
    return rows;
}

struct completion {
    Algebra& a;
    expressions trees;
    std::vector<channel> cuts;
    factorization_report report;
    Id seed, zero;
    std::vector<Id> basis;
    std::set<Id> known;
    std::size_t next_sample = 0;
    explicit completion(Algebra& arena) : a(arena), trees(a), zero(a.integer(0)) {}

    void direction(Id candidate) {
        Id pole = trees.split_poles(candidate).second;
        if (pole == zero) return;
        // Keep the first representative of proportional directions.
        const auto& n = a.node(pole);
        Q coefficient;
        if (n.head == "Times" && a.number(n.args.front(), coefficient))
            pole = a.mul({n.args.begin() + 1, n.args.end()});
        if (known.insert(pole).second) basis.push_back(pole);
    }
    std::vector<sample_row> samples(Id root, const std::vector<Id>& columns, std::size_t count, bool holdout) {
        auto rows = sample(a, cuts, root, columns, next_sample, count, report.workers);
        next_sample += count;
        (holdout ? report.validation_samples : report.training_samples) += rows.size();
        return rows;
    }
    bool verified(Id expression) {
        const auto rows = samples(expression, {}, 4, true);
        return std::all_of(rows.begin(), rows.end(), [](const auto& r) { return r.values[0] == r.values[1]; });
    }

    std::optional<Id> fit_numerator() {
        if (basis.empty())
            return {};
        fit_detail::modular_system system(basis.size());
        // Four consecutive samples visit both chiral branches on both cuts.
        for (std::size_t batch = 0; batch <= basis.size() / 4 + 4; ++batch) {
            auto rows = samples(seed, basis, 4, false);
            auto before = system.rank();
            for (const auto& row : rows) {
                try {
                    if (!system.add(row.values, 2, row.values[0] - row.values[1]))
                        return {};
                } catch (const residue_singular&) {
                    continue;
                }
            }
            if (auto coefficients = system.solution()) {
                std::vector<Id> terms{seed};
                for (std::size_t i = 0; i < basis.size(); ++i)
                    if (coefficients->at(i) != 0)
                        terms.push_back(a.mul({a.exact(coefficients->at(i)), basis[i]}));
                Id answer = a.add(std::move(terms));
                if (verified(answer)) {
                    report.route = "LittleGroupNumerator";
                    report.candidates = basis.size();
                    report.rank = system.rank();
                    return answer;
                }
            }
            if (system.rank() == before) {
                // The exact rational coefficients need not fit the reconstruction
                // range of a single prime. Refit the active columns over Q before
                // rejecting a consistent modular fit. Their support is typically
                // much smaller than the catalog of possible numerator monomials.
                auto entries = system.residues();
                std::vector<Id> supported;
                for (std::size_t i = 0; i < entries.size(); ++i)
                    if (entries[i]) supported.push_back(basis[i]);
                if (supported.empty()) return {};
                auto catalog = std::move(basis);
                basis = std::move(supported);
                auto result = fit("LittleGroupNumerator");
                basis = std::move(catalog);
                return result;
            }
        }
        return {};
    }

    std::optional<Id> fit(const std::string& route) {
        if (basis.empty()) return {};
        auto training = samples(seed, basis, 4, false);
        fit_detail::sparse_system system(basis.size());
        for (const auto& r : training)
            if (!system.add(r.values, 2, r.values[0] - r.values[1])) return {};
        for (;;) {
            auto coefficients = system.solution();
            std::vector<Id> terms{seed};
            for (std::size_t j = 0; j < coefficients.size(); ++j)
                if (coefficients[j] != 0) terms.push_back(a.mul({a.exact(coefficients[j]), basis[j]}));
            Id proposal = a.add(std::move(terms));
            auto checks = samples(proposal, {}, 4, true);
            bool passed = true;
            auto old_rank = system.rank();
            for (const auto& r : checks) {
                if (r.values[0] == r.values[1]) continue;
                passed = false;
                // Return this very counterexample to the simultaneous system.
                std::vector<Id> cols{cuts[r.cut].target, seed};
                cols.insert(cols.end(), basis.begin(), basis.end());
                try {
                    auto row = residue_sample(a, cuts[r.cut].cut, cols, r.index);
                    ++report.training_samples;
                    if (!system.add(row, 2, row[0] - row[1])) return {};
                } catch (const residue_singular&) {
                    auto fresh = samples(seed, basis, 4, false);
                    for (const auto& row : fresh)
                        if (!system.add(row.values, 2, row.values[0] - row.values[1])) return {};
                }
            }
            if (passed) {
                report.route = route;
                report.candidates = basis.size();
                report.rank = system.rank();
                return proposal;
            }
            if (system.rank() == old_rank) return {};
        }
    }
};

// For a single radiated massless vector, the orbital current contains a
// two-momentum sandwich with the same massless spinor at both ends. Its spin
// term contains bare brackets. Try those directions sharing a hard bracket
// first, so a spectator spinor factor can stay outside the entire numerator.
std::vector<Id> radiation_factors(const Algebra& a, Id direction, int light_leg) {
    std::vector<Id> brackets;
    const auto& n = a.node(direction);
    auto factors = n.head == "Times" ? n.args : std::vector<Id>{direction};
    for (Id factor : factors) {
        const auto& power = a.node(factor);
        if (power.head == "Power" && power.args.size() == 2 && a.leg(power.args[1]) > 0)
            factor = power.args[0];
        const auto& word = a.node(factor);
        if (word.head != "SpinorChain")
            continue;
        const auto& first = a.node(word.args.front());
        const auto& last = a.node(word.args.back());
        if (first.args.size() != 3 || last.args.size() != 3)
            return {};
        const int l = a.leg(first.args[2]), r = a.leg(last.args[2]);
        if (word.args.size() == 2) {
            if (l != light_leg && r != light_leg)
                brackets.push_back(factor);
        } else if (word.args.size() != 4 || l != light_leg || r != light_leg)
            return {};
    }
    return brackets;
}

std::optional<Id> physical_four_point_fit(completion& work, const model& model,
                                          const std::vector<Id>& diagrams,
                                          const std::vector<sewn_source>& products) {
    auto& a = work.a;
    std::vector<Id> poles, model_masses;
    std::set<Cut> seen;
    for (const auto& p : products) {
        auto cut = key(a, p.channel.legs, a.parse(p.channel.mass));
        if (!seen.insert(cut).second)
            continue;
        std::vector<Id> labels;
        for (int leg : cut.first)
            labels.push_back(a.integer(leg));
        poles.push_back(a.call("PropDen", {a.call("Mom", {a.call("Multiparticle", labels)}), cut.second}));
    }
    if (poles.size() < 2 || poles.size() > 3)
        return {};
    for (const auto& particle : model.particles())
        model_masses.push_back(a.parse(particle.mass));
    const auto original = work.seed;
    work.seed = work.trees.split_poles(original).first;
    std::array<std::vector<Id>, 3> columns;
    for (unsigned n = 0; n < columns.size(); ++n) {
        std::set<Id> included;
        for (Id source : diagrams)
            for (Id d : four_point_numerators(a, source, poles, model_masses, n))
                if (included.insert(d).second)
                    columns[n].push_back(d);
    }
    if (std::count(a.masses().begin(), a.masses().end(), work.zero) == 1) {
        int light =
            1 + std::distance(a.masses().begin(), std::find(a.masses().begin(), a.masses().end(), work.zero));
        std::map<Id, std::vector<Id>> by_factor;
        for (auto n : {0, 2})
            for (Id d : columns[n])
                for (Id factor : radiation_factors(a, d, light))
                    by_factor[factor].push_back(d);
        std::optional<Id> best;
        std::size_t score = 0, best_rank = 0, best_count = 0;
        for (const auto& [factor, group] : by_factor) {
            work.basis.clear();
            work.known.clear();
            for (Id d : group)
                work.direction(d);
            if (auto x = work.fit_numerator()) {
                auto complexity = expression_complexity(a, *x);
                if (!best || complexity < score || (complexity == score && a.cost(*x) < a.cost(*best))) {
                    best = x;
                    score = complexity;
                    best_rank = work.report.rank;
                    best_count = work.report.candidates;
                }
            }
        }
        if (best) {
            work.report.rank = best_rank;
            work.report.candidates = best_count;
            return best;
        }
    }
    work.basis.clear();
    work.known.clear();
    for (const auto& columns_with_same_degree : columns) {
        auto before = work.basis.size();
        for (Id d : columns_with_same_degree)
            work.direction(d);
        if (work.basis.size() != before)
            if (auto x = work.fit_numerator())
                return x;
    }
    work.seed = original;
    work.known.clear();
    work.basis.clear();
    return {};
}
} // namespace

validation_detail::cut_inventory validation_detail::validation_inventory(Algebra& a, const amplitude& process,
                                                                         const amp_expression& candidate) {
    expressions trees(a);
    auto zero = a.integer(0);
    auto input = [&](const amp_expression& e, bool remainder = false) {
        return trees.canonical(validation_detail::import_expression(a, e, remainder));
    };
    auto root = input(candidate);
    std::map<Cut, Id> targets;
    std::set<Cut> supplied;
    for (const auto& item : process.diagrams()) {
        for (const auto& shell : item.shells)
            targets.emplace(key(a, shell.legs, trees.canonical(a.parse(shell.mass))), zero);
        add_poles(a, input(item.expression), targets, zero);
        add_poles(a, input(item.expression, true), targets, zero);
    }
    for (const auto& row : process.factorization_products()) {
        const auto& shell = row.shells.at(0);
        auto cut = key(a, shell.legs, trees.canonical(a.parse(shell.mass)));
        Id full = a.add({input(row.expression), input(row.expression, true)});
        add_poles(a, full, targets, zero);
        auto& target = targets.try_emplace(cut, zero).first->second;
        target = a.add({target, sewn(a, full)});
        supplied.insert(cut);
    }
    std::set<Cut> model_cuts;
    for (const auto& entry : targets) model_cuts.insert(entry.first);
    add_poles(a, root, targets, zero);
    cut_inventory out{root, {}, {}};
    for (const auto& [cut, target] : targets) {
        std::vector<Id> slots;
        for (int leg : cut.first)
            slots.push_back(a.integer(leg));
        auto p = a.call("Mom", {a.call("Multiparticle", std::move(slots))});
        out.channels.push_back({{cut.first, a.format(cut.second)},
                                a.call("PropDen", {p, cut.second}),
                                target,
                                supplied.count(cut) != 0, model_cuts.count(cut) != 0});
    }
    std::set<Id> extras;
    std::function<void(Id)> denominator = [&](Id base) {
        const auto& factor = a.node(base);
        if (factor.head == "Times") {
            for (Id child : factor.args)
                denominator(child);
        } else if (factor.head != "PropDen" && trees.has_kinematics(base)) {
            extras.insert(base);
        }
    };
    contains(a, root, [&](const Node& n) {
        Q exponent;
        if (n.head == "Power" && n.args.size() == 2 && a.number(n.args[1], exponent) && exponent < 0)
            denominator(n.args[0]);
        return false;
    });
    out.other_denominators.assign(extras.begin(), extras.end());
    return out;
}

const amp_expression& amplitude::complete_factorization(std::size_t workers) {
    factorization_options options;
    options.workers = workers;
    return complete_factorization(options);
}

const amp_expression& amplitude::complete_factorization(const factorization_options& options) {
    if(vertices_model_ && (!channels_model_ || vertices_model_->is_feynman()))
        throw std::logic_error("native factorization completion requires both constructive model roles");
    if(vertices_model_ && (!diagrams_generated_ || !channels_generated_)) {
        amplitude staged = *this;
        staged.generate(options.workers);
        staged.complete_factorization(options);
        *this=std::move(staged);
        return *completed_;
    }
    auto pending = [](const amplitude_item& item) { return !item.reduced_; };
    if (std::any_of(diagrams_.begin(), diagrams_.end(), pending) ||
        std::any_of(products_.begin(), products_.end(), pending)) {
        // Entries share immutable roots until their local reduction succeeds.
        // Publish the prepared rows and the completed result in one operation.
        amplitude prepared = *this;
        prepared.reduce(options.workers);
        prepared.complete_factorization(options);
        *this = std::move(prepared);
        return *completed_;
    }
    if (complete_color_rows(&options, nullptr, nullptr)) return *completed_;
    Algebra a;
    std::vector<Id> masses;
    for (const auto& mass : masses_)
        masses.push_back(a.parse(mass));
    completion work(a);
    auto& trees = work.trees;
    for (auto& mass : masses)
        mass = trees.canonical(mass);
    a.setKinematics(std::move(masses), {});
    Id zero = work.zero;
    std::vector<Id> diagrams, remains, user;
    std::vector<sewn_source> products;
    std::vector<std::pair<numerator_shell, Id>> signed_sources, prepared_sources;
    bool different_lifts = false;
    std::map<Cut, Id> targets;
    auto import = [&](const amplitude_item& item, bool rest = false) {
        return trees.canonical(item.expression.import_into(a, rest));
    };
    std::size_t generated_diagrams = 0;
    bool has_manual_contribution = false;
    for (const auto& item : diagrams_) {
        diagrams.push_back(import(item));
        remains.push_back(import(item, true));
        generated_diagrams += item.generated_;
        if (!item.generated_ && (diagrams.back() != zero || remains.back() != zero))
            has_manual_contribution = true;
        for (const auto& cut : item.shells)
            targets.emplace(key(a, cut.legs, trees.canonical(a.parse(cut.mass))), zero);
    }
    for (const auto& item : products_) {
        const auto& shell = item.shells.at(0);
        auto cut = key(a, shell.legs, trees.canonical(a.parse(shell.mass)));
        numerator_shell selected{shell.legs, a.format(cut.second)};
        Id expr = import(item), rest = import(item, true);
        Id full = sewn(a, a.add({expr, rest}));
        Id reduced = sewn(a, expr);
        products.push_back({reduced, full, selected});
        signed_sources.emplace_back(selected, full);
        prepared_sources.emplace_back(selected, reduced);
        different_lifts |= reduced != full;
        auto& target = targets.try_emplace(cut, zero).first->second;
        target = a.add({target, full});
    }
    for (const auto& input : options.correction_basis)
        user.push_back(trees.canonical(a.parse(input)));
    for (const auto* entries : {&diagrams, &remains, &user})
        for (Id expr : *entries)
            add_poles(a, expr, targets, zero);
    for (const auto& p : products) {
        add_poles(a, p.prepared, targets, zero);
        add_poles(a, p.full, targets, zero);
    }
    std::vector<numerator_shell> inventory;
    for (const auto& [cut, target] : targets) {
        inventory.push_back({cut.first, a.format(cut.second)});
        work.cuts.push_back({inventory.back(), target});
    }
    work.seed = a.add(diagrams);
    work.report.channels = work.cuts.size();
    auto workers = options.workers ? options.workers : std::max(1u, std::thread::hardware_concurrency());
    work.report.workers = std::min(workers, work.cuts.size());
    auto publish = [&](Id answer) -> const amp_expression& {
        amp_expression result("0", masses_);
        result.set_from(a, answer);
        construction_.reset();
        completed_ = std::move(result);
        high_energy_result_.reset();
        before_final_reduction_.reset();
        report_ = std::move(work.report);
        return *completed_;
    };
    // Group elementary graphs only when the entire input is generated from the model.
    // A caller can supply additional terms, including contact terms invisible on every cut.
    if (options.try_grouped_currents && vertices_model_ && !products_.empty() &&
        references_ != generation_references::keep && !has_manual_contribution) {
        const bool physical = references_ == generation_references::physical;
        try {
            auto candidate = dirac_line_proposal(a, *vertices_model_, particles_, generated_diagrams, physical);
            const char* name = "DiracLineCurrent";
            if (!candidate) {
                candidate = fermion_current_proposal(a, *vertices_model_, particles_, generated_diagrams, physical);
                name = "ContractedDiracCurrents";
            }
            if (candidate) {
                auto proposed = trees.canonical(*candidate);
                if (work.verified(proposed)) {
                    work.report.route = name;
                    return publish(proposed);
                }
            }
        } catch (const residue_unsupported&) {
            // Try the ordinary residue fit if this representation exceeds the cut evaluator.
        } catch (const residue_singular&) {
            // A singular trial does not invalidate the diagram seed or the other continuations.
        }
    }
    if (products_.empty() && trees.has_pole(work.seed))
        throw residue_unsupported("a pole-bearing amplitude requires physical factorization channels");
    std::exception_ptr singular_seed;
    try {
        if (work.verified(work.seed)) {
            work.report.route = work.cuts.empty() ? "Contact" : "Unchanged";
            return publish(work.seed);
        }
    } catch (const residue_unsupported&) {
        if (masses_.size() != 4 || !vertices_model_)
            throw;
        singular_seed = std::current_exception();
    }
    // At four points a reference chosen on one diagram can create a double
    // pole on another diagram's cut. Fit a simple-pole numerator directly
    // against the *own-cut* lower-point products in that situation.
    if (vertices_model_ && masses_.size() == 4 && references_ != generation_references::cyclic &&
        references_ != generation_references::keep &&
        std::find(a.masses().begin(), a.masses().end(), zero) != a.masses().end()) {
        if (auto answer = physical_four_point_fit(work, *vertices_model_, diagrams, products))
            return publish(*answer);
    }
    if (singular_seed)
        std::rethrow_exception(singular_seed);
    auto attempt = [&](const char* name, std::size_t before) -> std::optional<Id> {
        if (work.basis.size() == before) return {};
        return work.fit(name);
    };
    auto append = [&](const std::vector<Id>& columns) {
        for (Id col : columns)
            work.direction(col);
    };
    auto pole_sum = trees.split_poles(work.seed).second;
    work.direction(pole_sum);
    if (auto answer = attempt("RescaledSeed", 0)) return publish(*answer);
    auto before = work.basis.size();
    append(remains);
    if (auto answer = attempt("RetainedRemainders", before)) return publish(*answer);
    before = work.basis.size();
    append(diagrams);
    append(user);
    if (auto answer = attempt("DiagramDirections", before)) return publish(*answer);

    // A sewn expression is constrained on its own cut. Carry it into the
    // full system only as a candidate off that cut. Some continuations (e.g.
    // an internal helicity spinor) cannot be evaluated on an unrelated cut.
    // Exclude those individual directions while still trying the others.
    auto add_lift = [&](Id source) {
        Id col = trees.canonical(source);
        std::map<Cut, Id> poles;
        add_poles(a, col, poles, zero);
        for (const auto& [cut, _] : poles)
            if (!targets.count(cut)) return;
        if (work.known.count(col) || !trees.has_pole(col)) return;
        try {
            sample(a, work.cuts, zero, {col}, 0, 4, work.report.workers);
        } catch (const residue_singular&) {
            return;
        } catch (const residue_unsupported&) {
            return;
        }
        work.direction(col);
    };
    before = work.basis.size();
    for (const auto& p : products) {
        add_lift(p.prepared);
        if (p.full != p.prepared) add_lift(p.full);
    }
    if (auto answer = attempt("ChannelContinuations", before)) return publish(*answer);
    before = work.basis.size();
    std::vector<Id> lifts;
    for (const auto& p : products) {
        auto extra = channel_continuations(a, p.full, p.channel, inventory);
        lifts.insert(lifts.end(), extra.begin(), extra.end());
    }
    for (Id lift : lifts)
        add_lift(lift);
    if (auto answer = attempt("ReferenceContinuations", before)) return publish(*answer);
    before = work.basis.size();
    std::vector<Id> sources = diagrams;
    sources.insert(sources.end(), remains.begin(), remains.end());
    for (const auto& p : products) {
        sources.push_back(p.prepared);
        sources.push_back(p.full);
    }
    sources.insert(sources.end(), lifts.begin(), lifts.end());
    // Distribute numerator sums only. A combinatorial product stays factored
    // when expanding it would exceed the budget for this optional basis.
    std::function<std::vector<Id>(Id)> terms = [&](Id id) -> std::vector<Id> {
        constexpr std::size_t bound = 128;
        const auto& n = a.node(id);
        if (n.head != "Plus" && n.head != "Times") return {id};
        std::vector<Id> result;
        if (n.head == "Times") result.push_back(a.integer(1));
        for (Id x : n.args) {
            auto t = terms(x);
            if (n.head == "Plus") {
                if (result.size() + t.size() > bound) return {id};
                result.insert(result.end(), t.begin(), t.end());
            } else {
                if (result.size() > bound / t.size()) return {id};
                std::vector<Id> next;
                for (Id left : result)
                    for (Id right : t)
                        next.push_back(a.mul({left, right}));
                result.swap(next);
            }
        }
        return result;
    };
    auto shifted_sum = [&](const auto& catalog) -> std::optional<Id> {
        Id contact = trees.split_poles(work.seed).first;
        for (std::size_t s = 0; s < masses_.size(); ++s)
            if (a.masses()[s] == zero)
                for (std::size_t t = 0; t < masses_.size(); ++t)
                    if (s != t && a.masses()[t] == zero) {
                        auto pieces = shifted_channel_continuations(a, catalog, static_cast<int>(s + 1),
                                                                    static_cast<int>(t + 1));
                        if (pieces.empty()) continue;
                        pieces.push_back(contact);
                        Id sum = trees.canonical(a.add(std::move(pieces)));
                        std::map<Cut, Id> poles;
                        add_poles(a, sum, poles, zero);
                        if (std::any_of(poles.begin(), poles.end(),
                                        [&](const auto& p) { return !targets.count(p.first); }))
                            continue;
                        try {
                            if (work.verified(sum)) {
                                work.report.route = "ShiftedChannelSum";
                                work.report.candidates = 1;
                                return sum;
                            }
                        } catch (const residue_singular&) {
                        } catch (const residue_unsupported&) {
                        }
                    }
        return {};
    };
    // Wide exact systems are expensive: a single verified current assembled
    // from the channels costs four samples per physical partition.
    if (work.cuts.size() >= 10) {
        // A prepared state sum and its restored form agree on the selected
        // cut. The prepared one can expose the propagator needed for the shift
        // even when the restored expression hides it in a nested sum.
        if (auto answer = shifted_sum(prepared_sources)) return publish(*answer);
        if (different_lifts)
            if (auto answer = shifted_sum(signed_sources)) return publish(*answer);
    }
    for (Id x : sources) {
        auto parts = terms(x);
        if (parts.size() > 1)
            for (Id part : parts)
                if (work.basis.size() < 512) add_lift(part);
    }
    if (auto answer = attempt("ExpandedContinuations", before)) return publish(*answer);
    // At vanishing boundary, a two-line shift supplies a simultaneous lift
    // of several channels. A nonvanishing boundary simply fails validation.
    auto snapshot = work.basis;
    auto visited = work.known;
    for (std::size_t s = 0; s < masses_.size(); ++s)
        if (a.masses()[s] == zero)
            for (std::size_t t = 0; t < masses_.size(); ++t)
                if (s != t && a.masses()[t] == zero) {
                    work.basis = snapshot;
                    work.known = visited;
                    auto shifted = shifted_channel_continuations(a, signed_sources, static_cast<int>(s + 1),
                                                                 static_cast<int>(t + 1));
                    for (Id x : shifted)
                        add_lift(x);
                    if (auto answer = attempt("ShiftedChannels", snapshot.size())) return publish(*answer);
                }
    throw std::runtime_error("factorization unresolved: no combination of available corrections passed all " +
                             std::to_string(work.cuts.size()) + " physical channels");
}
} // namespace hepcat
