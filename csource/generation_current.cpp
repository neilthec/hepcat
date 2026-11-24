#include "generation_current.hpp"
#include <algorithm>
#include <array>
#include <stdexcept>
#include <unordered_map>

namespace hepcat::generation {
namespace {
std::string short_head(const Node& n) {
    const auto pos = n.head.find_last_of('`');
    return n.head.substr(pos == std::string::npos ? 0 : pos + 1);
}
bool color_head(const std::string& h) {
    return h == "ColorDelta" || h == "ColorGenerator" || h == "ColorStructure" || h == "ColorTrace";
}

bool contains_color(const Algebra& a, Id expr) {
    const auto& n = a.node(expr);
    return color_head(short_head(n)) ||
           std::any_of(n.args.begin(), n.args.end(), [&](Id x) { return contains_color(a, x); });
}
int representation(const model& m, const std::string& name) {
    const auto& p = m.particle(name);
    if (p.color != 1 && p.color != 8 && std::abs(p.color) != 3)
        throw std::invalid_argument("unsupported SU(3) representation for " + name);
    return std::abs(p.color) == 3 && name == p.antiname && p.name != p.antiname ? -p.color : p.color;
}

Id index_for(Algebra& a, const term& t, int slot, int rep, std::size_t n) {
    if (std::abs(rep) == 1) throw std::invalid_argument("color index on a singlet");
    if (slot > 0) return a.call("ConstructiveDiagrams`ColorIndex", {a.integer(slot)});
    auto cut = t.edges.at(-slot - 1).legs;
    std::vector<int> rest;
    for (std::size_t k = 1; k <= n; ++k)
        if (!std::binary_search(cut.begin(), cut.end(), int(k))) rest.push_back(int(k));
    if (rest.size() < cut.size() || (rest.size() == cut.size() && rest < cut)) cut = std::move(rest);
    std::vector<Id> labels;
    for (int k : cut)
        labels.push_back(a.integer(k));
    return a.call("ConstructiveDiagrams`Private`colorInternalIndex",
                  {a.call("List", labels), a.integer(std::abs(rep))});
}

// Certify antisymmetry before attaching an adjoint structure constant. All
// three-point pure-gauge bracket monomials can be checked by swapping the
// equal bosonic slots and canonically orienting their brackets.
Id permuted_brackets(Algebra& a, Id expr, int from, int to) {
    const auto& n = a.node(expr);
    if (!n.compound || n.head == "ModelParameter") return expr;
    if (n.head == "Spinor" && n.args.size() == 3) {
        auto args = n.args;
        if (a.leg(args[2]) == from)
            args[2] = a.integer(to);
        else if (a.leg(args[2]) == to)
            args[2] = a.integer(from);
        return a.call(n.head, std::move(args));
    }
    if (n.head == "SpinorChain" && n.args.size() != 2)
        throw std::invalid_argument("explicit adjoint color tensor required for this vertex");
    std::vector<Id> v;
    for (auto c : n.args)
        v.push_back(permuted_brackets(a, c, from, to));
    if (n.head == "SpinorChain" && v.front() > v.back()) {
        std::swap(v.front(), v.back());
        return a.mul({a.integer(-1), a.call(n.head, std::move(v))});
    }
    return a.call(n.head, std::move(v));
}
bool antisymmetric(Algebra& a, const model& m, const model_vertex& v) {
    bool swapped = false;
    const auto original = permuted_brackets(a, v.expression, 0, 0);
    for (int i = 0; i < 3; ++i)
        for (int j = i + 1; j < 3; ++j)
            if (v.legs[i] == v.legs[j] && !(m.particle(v.legs[i]).spin2 % 2)) {
                swapped = true;
                if (a.add({original, permuted_brackets(a, v.expression, i + 1, j + 1)}) != a.integer(0))
                    return false;
            }
    return swapped && original != a.integer(0);
}
} // namespace

Id relabel_color(Algebra& a, const model& m, const term& t, std::size_t vi, Id expr,
                 const std::vector<std::string>& p) {
    const auto& n = a.node(expr);
    const auto& v = t.vertices.at(vi);
    const auto& row = m.vertices().at(v.row);
    auto h = short_head(n);
    if (h == "ColorIndex" && n.args.size() == 1) {
        auto i = a.leg(n.args[0]);
        if (i < 1 || std::size_t(i) > v.slots.size()) throw std::invalid_argument("color slot out of range");
        return index_for(a, t, v.slots[i - 1], representation(m, row.legs[i - 1]), p.size());
    }
    if ((h == "ColorInternalIndex" || h == "colorInternalIndex") && n.args.size() == 2) {
        auto namespaced = a.call("List", {a.atom("\"modelColor\""), a.integer(vi + 1), n.args[0]});
        return a.call("ConstructiveDiagrams`Private`colorInternalIndex", {namespaced, n.args[1]});
    }
    if (!color_head(h)) throw std::invalid_argument("unrecognized saved color tensor " + n.head);
    std::vector<Id> indices;
    for (Id c : n.args)
        indices.push_back(relabel_color(a, m, t, vi, c, p));
    return a.call("ConstructiveDiagrams`" + h, std::move(indices));
}

Id vertex_color(Algebra& a, const model& m, const term& t, std::size_t vi, const std::vector<std::string>& p) {
    const auto& v = t.vertices.at(vi);
    const auto& row = m.vertices().at(v.row);
    if (std::all_of(row.legs.begin(), row.legs.end(),
                    [&](const auto& name) { return m.particle(name).color == 1; }))
        return a.integer(1);
    if (contains_color(a, row.expression) || contains_color(a, row.factor)) return a.integer(1);
    std::vector<int> reps;
    std::vector<Id> indices;
    for (std::size_t i = 0; i < v.slots.size(); ++i) {
        int rep = representation(m, row.legs[i]);
        if (rep != 1) {
            reps.push_back(rep);
            indices.push_back(index_for(a, t, v.slots[i], rep, p.size()));
        }
    }
    if (reps.size() == 2 && ((std::abs(reps[0]) == 3 && reps[1] == -reps[0]) || (reps[0] == 8 && reps[1] == 8)))
        return a.call("ConstructiveDiagrams`ColorDelta", indices);
    if (reps.size() == 3) {
        if (reps == std::vector<int>{8, 8, 8} && row.legs.size() == 3 && antisymmetric(a, m, row))
            return a.mul({a.atom("System`I"), a.call("ConstructiveDiagrams`ColorStructure", indices)});
        std::vector<Id> ordered;
        for (int rep : {-3, 3, 8}) {
            auto found = std::find(reps.begin(), reps.end(), rep);
            if (found != reps.end()) ordered.push_back(indices[found - reps.begin()]);
        }
        if (ordered.size() == 3) return a.call("ConstructiveDiagrams`ColorGenerator", std::move(ordered));
    }
    throw std::invalid_argument("ambiguous vertex color invariant: supply an explicit model color tensor");
}

void resolve_diagram_colors(std::vector<plan>& diagrams, const model& m,
                            const std::vector<std::string>& names) {
    if (std::none_of(names.begin(), names.end(), [&](const auto& n) { return m.particle(n).color != 1; }))
        return;
    std::vector<plan> output;
    for (auto& diagram : diagrams) {
        bool split = false;
        if (diagram.terms.size() > 1) {
            Algebra colors(&m.algebra());
            std::optional<Id> first;
            for (const auto& term : diagram.terms) {
                std::vector<Id> factors;
                for (std::size_t v = 0; v < term.vertices.size(); ++v)
                    factors.push_back(vertex_color(colors, m, term, v, names));
                auto tensor = colors.mul(std::move(factors));
                if (first && *first != tensor) {
                    split = true;
                    break;
                }
                first = tensor;
            }
        }
        if (split)
            for (auto& assignment : diagram.terms) {
                plan one;
                one.terms.push_back(std::move(assignment));
                output.push_back(std::move(one));
            }
        else
            output.push_back(std::move(diagram));
    }
    diagrams = std::move(output);
}

} // namespace hepcat::generation
