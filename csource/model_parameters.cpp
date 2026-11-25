#include "model.hpp"
#include "chart_scalar.hpp"
#include <algorithm>
#include <unordered_set>

namespace hepcat {
namespace {
using chart_detail::Q;

// Unit-circle rational samples keep sqrt(1-x^2) rational for bounded input
// couplings. For dimensionful inputs, retain the order of magnitude of the
// model value. The original parameter stays symbolic in the amplitude.
Q sample_variable(const Q& value, unsigned index, unsigned variant) {
    if (!value) return 0;
    const int sign = value < 0 ? -1 : 1;
    Q magnitude = abs(value);
    if (magnitude < 1) {
        Q t(1 + (index + 2 * variant) % 7, 9 + (3 * index + variant) % 11);
        t.canonicalize();
        return sign * 2 * t / (1 + t * t);
    }
    const auto numerator_bits = mpz_sizeinbase(magnitude.get_num_mpz_t(), 2);
    const auto denominator_bits = mpz_sizeinbase(magnitude.get_den_mpz_t(), 2);
    auto shift = numerator_bits - denominator_bits;
    mpz_class scale = mpz_class(1) << shift;
    if (magnitude < scale) scale >>= 1;
    Q sampled(sign * (1 + static_cast<long>((index + variant) % 5)) * scale,
              1 + static_cast<long>((index + 2 * variant) % 3));
    sampled.canonicalize();
    return sampled;
}

struct definition {
    Id expression;
    unsigned variable;
};

class assignments {
    const Algebra& src;
    const std::unordered_map<Id, definition>& definitions;
    unsigned variant;
    Algebra numeric;
    chart_detail::Parameters scalars;
    std::unordered_map<Id, Id> memo;
    std::unordered_set<Id> active;
    Id substitute(Id id) {
        if (auto it = memo.find(id); it != memo.end()) return it->second;
        const auto& n = src.node(id);
        Id out;
        if (n.head == "ModelParameter") {
            auto it = definitions.find(id);
            if (it == definitions.end())
                throw std::invalid_argument("undeclared parameter " + src.format(id));
            if (!active.insert(id).second)
                throw std::invalid_argument("cyclic definition of " + src.format(id));
            auto d = it->second;
            Q default_value;
            if (d.variable && src.number(d.expression, default_value))
                out = numeric.exact(sample_variable(default_value, d.variable, variant));
            else
                out = substitute(d.expression);
            active.erase(id);
        } else if (n.compound) {
            if (n.head.rfind("System`", 0) == 0 || n.head.rfind("ConstructiveDiagrams`", 0) == 0)
                throw std::invalid_argument("unsupported exact model function " + n.head);
            std::vector<Id> args;
            for (Id child : n.args)
                args.push_back(substitute(child));
            out = numeric.call(n.head, std::move(args));
        } else
            out = numeric.atom(n.head);
        return memo.emplace(id, out).first->second;
    }

  public:
    assignments(const Algebra& a, const std::unordered_map<Id, definition>& d, unsigned v)
        : src(a), definitions(d), variant(v), scalars(numeric, v) {}
    std::string value(Id id) {
        auto result = scalars.value(substitute(id));
        if (!result.rational())
            throw std::invalid_argument("active model parameter is irrational on the generic sample: " +
                                        src.format(id));
        return numeric.format(numeric.exact(result.c[0]));
    }
};
} // namespace
model::parameter_points model::high_energy_parameter_points(const std::vector<std::string>& keys) const {
    return high_energy_parameter_points(keys, 1);
}
model::parameter_points model::high_energy_parameter_points(const std::vector<std::string>& keys, unsigned first_variant) const {
    if (keys.empty()) return {};
    const auto& arena = algebra();
    std::unordered_map<Id, definition> definitions;
    std::unordered_map<std::string, Id> by_name;
    auto add = [&](const model_parameter& p, unsigned variable) {
        if (!definitions.emplace(p.symbol, definition{p.definition, variable}).second)
            throw std::invalid_argument("duplicate model parameter " + p.name);
        by_name.emplace(p.name, p.symbol);
    };
    unsigned index = 0;
    for (const auto& v : variables())
        add(v, ++index);
    for (const auto& d : functions())
        add(d, 0);
    Algebra input;
    std::vector<Id> active;
    std::string scope_atom = "\"" + parameter_scope() + "\"";
    for (const auto& key : keys) {
        Id id = input.parse(key);
        const auto& n = input.node(id);
        if (n.head != "ModelParameter" || n.args.size() != 3)
            throw std::invalid_argument("expected a scoped model parameter");
        if (input.node(n.args[1]).head != scope_atom) continue;
        const auto& name = input.node(n.args[0]).head;
        auto i = name.size() > 1 ? by_name.find(name.substr(1, name.size() - 2)) : by_name.end();
        if (i == by_name.end() || arena.format(i->second) != input.format(id))
            throw std::invalid_argument("unknown active model parameter " + key);
        if (std::find(active.begin(), active.end(), i->second) == active.end()) active.push_back(i->second);
    }
    if (active.empty()) return {};
    parameter_points points;
    for (unsigned k = 0; k < 3; ++k) {
        assignments sample(arena, definitions, first_variant + k);
        auto& point = points.emplace_back();
        for (Id id : active) {
            try {
                point.emplace_back(arena.format(id), sample.value(id));
            } catch (const std::exception& e) {
                throw std::invalid_argument("supply explicit high-energy parameter points for " +
                                            arena.format(id) + ": " + e.what());
            }
        }
    }
    return points;
}
// Apply a model identity only if a sum of declared couplings reduces to an
// exact constant. Masses, input values and other useful abbreviations stay symbolic.
Id contract_model_couplings(Algebra &a, Id root, const model &m) {
    if (m.functions().empty())
        return root;
    std::map<Id, Id> definitions, memo;
    for (const auto &f : m.functions())
        definitions.emplace(a.parse(m.algebra().format(f.symbol)), a.parse(m.algebra().format(f.definition)));
    auto visit = [&](Id id, const auto &self) -> Id {
        if (auto it = memo.find(id); it != memo.end())
            return it->second;
        const auto &n = a.node(id);
        if (n.head != "Plus" && n.head != "Times" && n.head != "Power")
            return id;
        std::vector<Id> children;
        for (Id x : n.args)
            children.push_back(self(x, self));
        Id out = children == n.args ? id : a.call(n.head, children);
        if (n.head == "Plus") {
            std::vector<Id> expanded;
            for (Id term : children) {
                const auto &product = a.node(term);
                Id scalar = a.integer(1), symbol = term;
                if (product.head == "Times" && product.args.size() == 2) {
                    scalar = product.args[0];
                    symbol = product.args[1];
                    if (definitions.count(scalar))
                        std::swap(scalar, symbol);
                }
                mpq_class q;
                auto def = definitions.find(symbol);
                if (def == definitions.end() || !a.number(scalar, q)) {
                    expanded.clear();
                    break;
                }
                const auto &rhs = a.node(def->second);
                for (Id x : rhs.head == "Plus" ? rhs.args : std::vector<Id>{def->second})
                    expanded.push_back(a.mul({scalar, x}));
            }
            if (!expanded.empty()) {
                Id value = a.simplify(a.add(expanded));
                mpq_class q;
                if (a.number(value, q))
                    out = value;
            }
        }
        return memo.emplace(id, out).first->second;
    };
    return visit(root, visit);
}
} // namespace hepcat
