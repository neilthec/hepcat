#include "amplitude.hpp"
#include "continuations.hpp"
#include "residue.hpp"

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

class sparse_system {
    using Sparse = std::map<std::size_t, Q>;
    std::map<std::size_t, Sparse> pivots;
    std::size_t dimension;

  public:
    explicit sparse_system(std::size_t columns) : dimension(columns) {}
    bool add(const std::vector<Q>& values) {
        Sparse row;
        for (std::size_t i = 0; i < dimension; ++i)
            if (values[i + 2] != 0) row[i] = values[i + 2];
        Q rhs = values[0] - values[1];
        if (rhs != 0) row.emplace(dimension, std::move(rhs));
        while (!row.empty()) {
            auto col = row.begin()->first;
            if (col == dimension) return false;
            auto it = pivots.find(col);
            Q scale = row.begin()->second;
            if (it == pivots.end()) {
                for (auto& x : row)
                    x.second /= scale;
                pivots.emplace(col, std::move(row));
                return true;
            }
            for (const auto& [j, x] : it->second) {
                auto& entry = row[j];
                entry -= scale * x;
                if (!entry) row.erase(j);
            }
        }
        return true;
    }
    std::size_t rank() const { return pivots.size(); }
    std::vector<Q> solution() const {
        std::vector<Q> result(dimension);
        for (auto it = pivots.rbegin(); it != pivots.rend(); ++it) {
            const auto& [col, row] = *it;
            Q value = 0;
            for (const auto& [j, x] : row) {
                if (j == dimension)
                    value += x;
                else if (j != col)
                    value -= x * result[j];
            }
            result[col] = std::move(value);
        }
        return result;
    }
};

template <class F> void by_channel(std::size_t count, std::size_t workers, F task) {
    if (workers <= 1) {
        for (std::size_t i = 0; i < count; ++i)
            task(i);
        return;
    }
    std::atomic<std::size_t> next{0};
    std::vector<std::exception_ptr> errors(count);
    auto run = [&] {
        for (;;) {
            auto i = next.fetch_add(1, std::memory_order_relaxed);
            if (i >= count) return;
            try {
                task(i);
            } catch (...) {
                errors[i] = std::current_exception();
            }
        }
    };
    std::vector<std::thread> pool;
    try {
        for (std::size_t i = 1; i < workers; ++i)
            pool.emplace_back(run);
    } catch (...) {
        for (auto& thread : pool)
            thread.join();
        throw;
    }
    run();
    for (auto& thread : pool)
        thread.join();
    for (const auto& error : errors)
        if (error) std::rethrow_exception(error);
}

struct sample_row {
    std::size_t cut, index;
    std::vector<Q> values;
};
std::vector<sample_row> sample(const Algebra& a, const std::vector<channel>& channels, Id seed,
                               const std::vector<Id>& basis, std::size_t first, std::size_t count,
                               std::size_t workers) {
    std::vector<sample_row> rows(channels.size() * count);
    by_channel(channels.size(), workers, [&](std::size_t i) {
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

    std::optional<Id> fit(const std::string& route) {
        if (basis.empty()) return {};
        auto training = samples(seed, basis, 4, false);
        sparse_system system(basis.size());
        for (const auto& r : training)
            if (!system.add(r.values)) return {};
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
                    if (!system.add(row)) return {};
                } catch (const residue_singular&) {
                    auto fresh = samples(seed, basis, 4, false);
                    for (const auto& row : fresh)
                        if (!system.add(row.values)) return {};
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
} // namespace

const amp_expression& amplitude::complete_factorization(std::size_t workers) {
    factorization_options options;
    options.workers = workers;
    return complete_factorization(options);
}

const amp_expression& amplitude::complete_factorization(const factorization_options& options) {
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
    for (const auto& item : diagrams_) {
        diagrams.push_back(import(item));
        remains.push_back(import(item, true));
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
        completed_ = std::move(result);
        report_ = std::move(work.report);
        return *completed_;
    };
    if (products_.empty() && trees.has_pole(work.seed))
        throw residue_unsupported("a pole-bearing amplitude requires physical factorization channels");
    if (work.verified(work.seed)) {
        work.report.route = work.cuts.empty() ? "Contact" : "Unchanged";
        return publish(work.seed);
    }
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
