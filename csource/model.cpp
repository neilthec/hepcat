#include "model.hpp"
#include "model_expression.hpp"
#include "model_orbit.hpp"
#include <algorithm>
#include <array>
#include <cctype>
#include <fstream>
#include <functional>
#include <iomanip>
#include <limits>
#include <map>
#include <sstream>
#include <stdexcept>
#include <unordered_map>

namespace hepcat {
namespace fs = std::filesystem;
namespace {
using fields = std::vector<std::string>;
std::string trim(std::string_view s) {
    auto begin = s.find_first_not_of(" \t\r\n"), end = s.find_last_not_of(" \t\r\n");
    return begin == s.npos ? std::string() : std::string(s.substr(begin, end - begin + 1));
}
std::string read_file(const fs::path& path) {
    std::ifstream in(path);
    if (!in) throw std::runtime_error("cannot open model file " + path.string());
    std::ostringstream out;
    out << in.rdbuf();
    if (in.bad()) throw std::runtime_error("cannot read model file " + path.string());
    return out.str();
}
template <class F> void read_rows(const fs::path& path, bool require_separator, F handle) {
    std::ifstream in(path);
    if (!in) throw std::runtime_error("cannot open model file " + path.string());
    std::string line;
    size_t number = 0;
    while (std::getline(in, line)) {
        ++number;
        if (line.find_first_not_of(" \t\r") == std::string::npos) continue;
        if (trim(line).rfind("====", 0) == 0) return;
        if (line.find('|') == std::string::npos) continue;
        fields row;
        size_t i = 0;
        while (i < line.size()) {
            size_t j = line.find('|', i);
            row.push_back(trim(std::string_view(line).substr(i, j == std::string::npos ? j : j - i)));
            if (j == std::string::npos) break;
            i = j + 1;
        }
        if (!line.empty() && line.back() == '\r' && line.size() >= 2 && line[line.size() - 2] == '|')
            row.pop_back();
        try {
            handle(row);
        } catch (const std::exception& e) {
            throw std::invalid_argument(path.string() + ":" + std::to_string(number) + ": " + e.what());
        }
    }
    if (in.bad()) throw std::runtime_error("cannot read model file " + path.string());
    if (require_separator) throw std::invalid_argument(path.string() + ": missing final separator");
}
bool identifier(const std::string& s) {
    auto alpha = [](unsigned char c) { return std::isalpha(c) || c == '_'; };
    return !s.empty() && alpha(s.front()) &&
           std::all_of(s.begin() + 1, s.end(), [&](unsigned char c) { return alpha(c) || std::isdigit(c); });
}
// A native scope is a non-secret fingerprint of a directory and its two scalar
// tables. Prefix lengths distinguish the fields even when the file text is empty.
// An importing caller can supply its existing Wolfram scope instead.
std::string native_scope(const fs::path& vars, const fs::path& funcs) {
    std::uint64_t lo = 0x243f6a8885a308d3ULL, hi = 0x13198a2e03707344ULL;
    auto byte = [&](unsigned char x) {
        lo = (lo ^ x) * 1099511628211ULL;
        hi = (hi + x + lo) * 0x9e3779b185ebca87ULL;
        hi = (hi << 27) | (hi >> 37);
    };
    auto append = [&](const std::string& input) {
        std::uint64_t length = input.size();
        for (int i = 0; i < 8; ++i) {
            byte(length & 255);
            length >>= 8;
        }
        for (unsigned char c : input)
            byte(c);
    };
    append(fs::absolute(vars).lexically_normal().parent_path().string());
    append(read_file(vars));
    append(read_file(funcs));
    std::ostringstream out;
    out << "native-" << std::hex << std::setfill('0') << std::setw(16) << hi << std::setw(16) << lo;
    return out.str();
}
int int_field(const Algebra& a, Id x) {
    mpq_class q;
    if (!a.number(x, q) || q.get_den() != 1 || !q.get_num().fits_sint_p())
        throw std::invalid_argument("expected integer particle field");
    return q.get_num().get_si();
}
bool feynman_tensor(const Algebra& a, Id id) {
    const auto& n = a.node(id);
    if (n.head == "Mom" || n.head == "MomProd" || n.head == "ConstructiveDiagrams`DiracWord" ||
        n.head.rfind("ConstructiveDiagrams`Lorentz", 0) == 0)
        return true;
    return std::any_of(n.args.begin(), n.args.end(), [&](Id x) { return feynman_tensor(a, x); });
}
bool has_lorentz_index(const Algebra& a, Id id) {
    const auto& n = a.node(id);
    if (n.head == "ConstructiveDiagrams`LorentzIndex") return true;
    return std::any_of(n.args.begin(), n.args.end(), [&](Id x) { return has_lorentz_index(a, x); });
}
} // namespace

struct model::data {
    Algebra arena;
    std::string scope;
    std::vector<model_particle> particles;
    std::unordered_map<std::string, size_t> by_label;
    std::unordered_map<std::string, model_particle> tensor_auxiliaries;
    std::vector<model_vertex> vertices;
    std::vector<model_parameter> variables, functions;
    bool feynman;
    const model_particle& find(const std::string& name) const {
        auto it = by_label.find(name);
        if (it != by_label.end()) return particles[it->second];
        auto aux = tensor_auxiliaries.find(name);
        if (aux != tensor_auxiliaries.end()) return aux->second;
        throw std::invalid_argument("unknown model particle: " + name);
    }
    int lorentz_rank(std::string name) const {
        if (auto i = by_label.find(name); i != by_label.end()) {
            const auto& p = particles[i->second];
            return p.auxiliary == "T" ? 2 : (p.spin2 == 2 ? 1 : 0);
        }
        // CalcHEP's ghost, Goldstone and tensor auxiliary fields can be
        // implicit extensions of a field declared in the particle file.
        if (name.size() > 2 && name[name.size() - 2] == '.' &&
            std::string("fcCt").find(name.back()) != std::string::npos) {
            const auto& p = find(name.substr(0, name.size() - 2));
            return name.back() == 't' && p.spin2 == 2 ? 2 : 0;
        }
        throw std::invalid_argument("unknown Feynman vertex field: " + name);
    }
    void check_indices(const model_vertex& row) const {
        const auto& a = arena;
        std::vector<int> ranks;
        for (const auto& leg : row.legs)
            ranks.push_back(lorentz_rank(leg));
        const auto& root = a.node(row.expression);
        auto inspect = [&](Id term) {
            mpq_class v;
            if (a.number(term, v) && !v) return;
            std::vector<std::array<int, 2>> counts(ranks.size());
            auto walk = [&](auto&& self, Id t, bool contracted = false) -> void {
                const auto& n = a.node(t);
                if (n.head == "ConstructiveDiagrams`LorentzIndex" || n.head == "Mom") {
                    if (!contracted || n.args.empty())
                        throw std::invalid_argument("uncontracted Lorentz or momentum slot");
                    int leg = a.leg(n.args[0]);
                    if (leg < 1 || size_t(leg) > counts.size())
                        throw std::invalid_argument("Lorentz or momentum slot outside vertex");
                    if (n.head != "Mom") ++counts[leg - 1][n.args.size() == 2];
                    return;
                }
                if (n.head == "Power" && has_lorentz_index(a, n.args[0]) && a.leg(n.args[1]) != 1)
                    throw std::invalid_argument("invalid Lorentz tensor power");
                bool in_tensor = contracted || n.head == "ConstructiveDiagrams`DiracWord" ||
                                 n.head == "ConstructiveDiagrams`LorentzMetric" ||
                                 n.head == "ConstructiveDiagrams`LorentzMomentum" ||
                                 n.head == "ConstructiveDiagrams`LorentzScalarProduct";
                for (Id child : n.args)
                    self(self, child, in_tensor);
            };
            walk(walk, term);
            for (size_t i = 0; i < ranks.size(); ++i)
                if (counts[i][0] != (ranks[i] >= 1) || counts[i][1] != (ranks[i] >= 2))
                    throw std::invalid_argument("Lorentz slots do not match the vertex fields");
        };
        if (root.head == "Plus")
            for (Id term : root.args)
                inspect(term);
        else
            inspect(row.expression);
    }
    data() : feynman(false) {}
    data(const fs::path& dir, unsigned number, std::string supplied_scope) {
        auto file = [&](std::string_view stem) {
            return dir / (std::string(stem) + std::to_string(number) + ".mdl");
        };
        feynman = !fs::exists(file("amps")) && fs::exists(file("lgrng"));
        scope = supplied_scope.empty() ? native_scope(file("vars"), file("func"))
                                       : std::move(supplied_scope);
        if (scope.find_first_of("\"\\\r\n") != scope.npos)
            throw std::invalid_argument("invalid model parameter scope");
        Id scope_id = arena.atom("\"" + scope + "\"");
        auto parse = [&](const std::string& s, const std::vector<bool>& massive = std::vector<bool>{}) {
            return model_detail::parse_expression(arena, s, scope_id, massive, feynman);
        };
        auto read_parameters = [&](bool derived, std::vector<model_parameter>& target) {
            read_rows(file(derived ? "func" : "vars"), !feynman, [&](const fields& row) {
                if (row.front() == "Name") return;
                if (row.size() < 2 || (!feynman && row.size() != (derived ? 2 : 3)))
                    throw std::invalid_argument("malformed parameter row");
                if (!identifier(row[0])) throw std::invalid_argument("invalid parameter name");
                std::string expression = row[1], comment = row.size() > 2 ? row[2] : "";
                if (derived || feynman)
                    if (auto p = expression.find('%'); p != expression.npos) {
                        comment = trim(expression.substr(p + 1));
                        expression.resize(p);
                    }
                Id symbol = arena.call("ModelParameter",
                                       {arena.atom("\"" + row[0] + "\""), scope_id, arena.atom("\"Real\"")});
                Id value = parse(expression);
                if (feynman && feynman_tensor(arena, value))
                    throw std::invalid_argument("expected scalar parameter");
                target.push_back({row[0], symbol, value, std::move(comment)});
            });
        };
        read_parameters(false, variables);
        read_parameters(true, functions);
        read_rows(file("prtcls"), !feynman, [&](const fields& row) {
            if (row.front().find("Full") != std::string::npos &&
                row.front().find("Name") != std::string::npos)
                return;
            if (row.size() != 11 || row[1].empty() || row[2].empty())
                throw std::invalid_argument("expected eleven particle fields");
            Id mass = parse(row[5]);
            if (feynman && arena.node(mass).head != "ModelParameter" && mass != arena.integer(0))
                throw std::invalid_argument("expected a mass parameter or zero");
            auto index = particles.size();
            auto insert = [&](const std::string& label) {
                if (!by_label.emplace(label, index).second)
                    throw std::invalid_argument("repeated particle label: " + label);
            };
            insert(row[1]);
            if (row[2] != row[1]) insert(row[2]);
            particles.push_back({row[0], row[1], row[2], int_field(arena, parse(row[3])),
                                 int_field(arena, parse(row[4])), arena.format(mass), row[6],
                                 int_field(arena, parse(row[7])), row[8], row[9], row[10]});
        });
        if (particles.empty()) throw std::invalid_argument("empty particle model");
        read_rows(file(feynman ? "lgrng" : "amps"), !feynman, [&](const fields& row) {
            if (row[0] == "P1" || row[0].find('%') != std::string::npos) return;
            if (row.size() < 5 || (feynman && row.size() != 6))
                throw std::invalid_argument("invalid vertex arity");
            std::vector<bool> massive;
            model_vertex v{};
            for (size_t i = 0; i < row.size() - 2; ++i) {
                if (row[i].empty()) {
                    if (i < 3) throw std::invalid_argument("missing required particle label");
                    continue;
                }
                if (!feynman) massive.push_back(find(row[i]).mass != "0");
                v.legs.push_back(row[i]);
            }
            v.factor = parse(row[row.size() - 2]);
            if (feynman && feynman_tensor(arena, v.factor))
                throw std::invalid_argument("non-scalar Feynman factor");
            auto expression = feynman ? row.back().substr(0, row.back().find('%')) : row.back();
            v.pending_constructive_derivation = !feynman && expression.compare(0, 30, "PendingConstructiveDerivation:") == 0;
            v.expression = v.pending_constructive_derivation
                ? arena.atom("PendingConstructiveDerivation") : parse(expression, massive);
            if (feynman) {
                check_indices(v);
                for (const auto& label : v.legs) {
                    if (by_label.count(label) || label.size() < 3 ||
                        label.compare(label.size() - 2, 2, ".t") != 0)
                        continue;
                    const auto& p = find(label.substr(0, label.size() - 2));
                    if (p.spin2 == 2 && p.mass == "0")
                        tensor_auxiliaries.emplace(label, model_particle{p.description + " contact auxiliary",
                                                                         label, label, 0, 4, "0", "0",
                                                                         p.color, "T", label, label});
                }
            }
            vertices.push_back(std::move(v));
        });
    }
};

model::model(const fs::path& directory, unsigned number, std::string scope)
    : data_(std::make_shared<data>(directory, number, std::move(scope))) {}
// Regenerate a minimal conserved current when only an elementary expression
// and its external masses are available. A complete algebraic comparison
// with the input is required before its current identity can be used.
const std::vector<model_particle>& model::particles() const { return data_->particles; }
const std::vector<model_vertex>& model::vertices() const { return data_->vertices; }
const std::vector<model_parameter>& model::variables() const { return data_->variables; }
const std::vector<model_parameter>& model::functions() const { return data_->functions; }
const model_particle& model::particle(const std::string& label) const { return data_->find(label); }
std::string model::antiparticle(const std::string& name) const {
    const auto& p = particle(name);
    return p.name == name ? p.antiname : p.name;
}
amp_expression model::retrieve_amplitude(const std::vector<std::string>& process) const {
    if (is_feynman()) throw std::invalid_argument("retrieve_amplitude requires a constructive model");
    const model_vertex* match = nullptr;
    bool exact = false, ambiguous = false;
    for (const auto& row : vertices()) {
        if (row.legs.size() != process.size()) continue;
        const bool same = row.legs == process;
        if (exact && !same) continue;
        if (!same && !std::is_permutation(row.legs.begin(), row.legs.end(), process.begin())) continue;
        ambiguous = match && same == exact;
        exact = same;
        match = &row;
    }
    if (ambiguous) throw std::invalid_argument("multiple model amplitudes match the requested particles");

    std::vector<std::string> masses;
    for (const auto& leg : process) masses.push_back(particle(leg).mass);
    amp_expression result("0", masses);
    if (!match) return result;
    if (match->pending_constructive_derivation)
        throw std::domain_error("Stored amplitude is pending a constructive derivation");
    Algebra arena(&algebra());
    Id root = arena.mul({match->factor, model_detail::expand_leg_orbit(arena, match->expression)});
    if (!exact) {
        // Match successive identical particles in their original order. The
        // stored expression already carries the identical-fermion antisymmetry.
        std::vector<Id> labels;
        std::vector<bool> used(process.size());
        for (const auto& leg : match->legs) {
            std::size_t i = 0;
            while (used[i] || process[i] != leg) ++i;
            used[i] = true;
            labels.push_back(arena.integer(i + 1));
        }
        std::unordered_map<Id, Id> memo;
        std::function<Id(Id)> visit = [&](Id id) -> Id {
            if (auto it = memo.find(id); it != memo.end()) return it->second;
            const auto& n = arena.node(id);
            if (!n.compound) return id;
            const auto head = n.head.substr(n.head.find_last_of('`') + 1);
            auto args = n.args;
            auto slot = [&](std::size_t i) {
                const Id old = args.at(i);
                args[i] = arena.node(old).head == "Multiparticle" ? visit(old) : labels.at(arena.leg(old)-1);
            };
            if (head == "Spinor" || head == "SpinorHat") {
                if (arena.node(args.at(0)).head == "\"Xi\"") return id;
                slot(args.size() == 5 ? 3 : 2);
                if (args.size() >= 4) args.back() = visit(args.back());
            } else if (head == "ColorIndex" || head == "VectorSpinIndex" || head == "Mom" ||
                       head == "MomHat" || head == "MomQ" || head == "Mass") {
                slot(0);
            } else if (head == "MomProd" || head == "Mandelstahm" || head == "xFactor" ||
                       head == "xTildeFactor" || head == "Multiparticle") {
                for (std::size_t i = 0; i < args.size(); ++i) slot(i);
                if (head == "Multiparticle")
                    std::sort(args.begin(), args.end(), [&](Id a, Id b) { return arena.leg(a) < arena.leg(b); });
            } else if (head == "colorInternalIndex" || head == "ColorInternalIndex" || head == "ModelParameter") {
                return id;
            } else {
                for (auto& arg : args) arg = visit(arg);
            }
            const Id out = args == n.args ? id : arena.call(n.head, std::move(args));
            memo.emplace(id, out);
            return out;
        };
        root = visit(root);
    }
    result.set_from(arena, root);
    return result;
}
const Algebra& model::algebra() const { return data_->arena; }
const std::string& model::parameter_scope() const { return data_->scope; }
bool model::is_feynman() const { return data_->feynman; }
} // namespace hepcat
