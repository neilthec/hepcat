#include "validation_feynman.hpp"

#include <algorithm>
#include <map>
#include <stdexcept>
#include <string_view>

namespace hepcat {
namespace {
std::string_view local(const Node& n) {
    std::string_view s = n.head;
    auto p = s.rfind('`');
    return p == s.npos ? s : s.substr(p + 1);
}

enum class matrix_kind { epsilon, sigma, momentum };
struct matrix {
    matrix_kind kind;
    Id arg = 0;
    int block = 0; // 0 is sigma, 1 is barsigma
};
using word = std::vector<matrix>;
const matrix eps{matrix_kind::epsilon};

void append(word& out, const word& rest) {
    out.insert(out.end(), rest.begin(), rest.end());
}
word slice(const word& w, size_t b, size_t e) {
    return {w.begin() + b, w.begin() + e};
}
word join(word out, const word& second) {
    append(out, second);
    return out;
}
word transpose(const word& w, int& sign) {
    word out;
    for (auto i = w.rbegin(); i != w.rend(); ++i) {
        sign = -sign;
        if (i->kind == matrix_kind::epsilon)
            out.push_back(eps);
        else {
            out.push_back(eps);
            out.push_back({i->kind, i->arg, 1 - i->block});
            out.push_back(eps);
        }
    }
    return out;
}

struct boundary {
    Id spinor;
    int block;
};
struct chain {
    std::optional<boundary> left, right;
    word matrices;
};
enum class tensor_kind { metric, momentum, polarization };
struct tensor {
    tensor_kind kind;
    Id first, second;
};
struct term {
    std::vector<Id> scalars;
    std::vector<chain> chains;
    std::vector<tensor> tensors;
};
using terms = std::vector<term>;
// Match MaxExpandedTerms in HEPCAT's FeynmanToSpinorProducts. The bound is
// per diagram; a large sum of independent diagrams has separate budgets.
constexpr size_t term_limit = 100000;
[[noreturn]] void expansion_limit() {
    throw std::length_error(
        "Feynman contraction: exact Lorentz expansion exceeds 100000 terms in one diagram");
}
size_t checked_product(size_t lhs, size_t rhs) {
    if (rhs != 0 && lhs > term_limit / rhs)
        expansion_limit();
    return lhs * rhs;
}
terms product(const terms& x, const terms& y) {
    terms out;
    out.reserve(checked_product(x.size(), y.size()));
    for (const auto& l : x)
        for (const auto& r : y) {
            term t = l;
            t.scalars.insert(t.scalars.end(), r.scalars.begin(), r.scalars.end());
            t.chains.insert(t.chains.end(), r.chains.begin(), r.chains.end());
            t.tensors.insert(t.tensors.end(), r.tensors.begin(), r.tensors.end());
            out.push_back(std::move(t));
        }
    return out;
}

class conversion {
    Algebra& a;
    const Id zero, one, inv_sqrt2;
    std::map<Id, bool> tensor_nodes;
    std::map<std::pair<Id, Id>, Id> scalar_products;
    static void fail(const std::string& reason) {
        throw std::invalid_argument("Feynman contraction: " + reason);
    }
    bool contains_tensor(Id id) {
        auto found = tensor_nodes.find(id);
        if (found != tensor_nodes.end())
            return found->second;
        const auto& n = a.node(id);
        auto h = local(n);
        bool result = h == "DiracChain" || h == "DiracWord" || h == "PolarizationVector" ||
                      h == "LorentzMetric" || h == "LorentzMomentum" || h == "LorentzScalarProduct" ||
                      h == "DiracGamma" || h == "DiracGamma5" || h == "DiracSlash" || h == "FermionBar" ||
                      h == "FermionKet";
        if (!result)
            for (auto child : n.args)
                if (contains_tensor(child)) {
                    result = true;
                    break;
                }
        tensor_nodes[id] = result;
        return result;
    }
    boundary spinor(Id leg, int block, const char* kind, std::optional<Id> vector_index = {}) {
        Id k = a.atom(std::string("\"") + kind + "\""), type = a.atom(block ? "\"Square\"" : "\"Angle\"");
        if (!vector_index)
            return {a.call("Spinor", {k, type, leg}), block};
        return {a.call("Spinor", {k, a.atom("\"Upper\""), type, leg, *vector_index}), block};
    }
    std::vector<boundary> endpoint(Id id, bool bar) {
        const auto& n = a.node(id);
        if (local(n) != (bar ? "FermionBar" : "FermionKet") || n.args.size() != 3)
            fail("invalid Dirac endpoint");
        const auto& hand = a.node(n.args[2]).head;
        const bool massive = n.args[1] != zero;
        if (hand == "\"Dirac\"" && !massive)
            fail("massless Dirac field needs a helicity");
        if (hand != "\"Dirac\"" && hand != "\"L\"" && hand != "\"R\"")
            fail("unknown fermion chirality");
        std::vector<boundary> choices;
        for (int b : {0, 1}) {
            if (hand != "\"Dirac\"" && b != (bar == (hand == "\"L\"") ? 1 : 0))
                continue;
            choices.push_back(spinor(n.args[0], b, massive ? "Spin" : "Helicity"));
        }
        return choices;
    }
    terms dirac(const Node& n) {
        auto lefts = endpoint(n.args.at(0), true), rights = endpoint(n.args.at(2), false);
        const auto& ops = a.node(n.args.at(1));
        if (local(ops) != "DiracWord")
            fail("invalid Dirac word");
        terms out;
        for (auto l : lefts) {
            int block = l.block, sign = block ? -1 : 1;
            word matrices;
            if (!block)
                matrices.push_back(eps);
            for (Id id : ops.args) {
                const auto& op = a.node(id);
                if (id == one)
                    continue;
                auto h = local(op);
                if (h == "DiracGamma5") {
                    if (!block)
                        sign = -sign;
                    continue;
                }
                if ((h != "DiracGamma" && h != "DiracSlash") || op.args.size() != 1)
                    fail("unsupported Dirac operator " + op.head);
                matrices.push_back(
                    {h == "DiracGamma" ? matrix_kind::sigma : matrix_kind::momentum, op.args[0], block});
                block = 1 - block;
            }
            for (auto r : rights)
                if (r.block == block) {
                    auto w = matrices;
                    if (block)
                        w.push_back(eps);
                    out.push_back({{a.integer(-sign)}, {{l, r, std::move(w)}}, {}});
                }
        }
        return out;
    }
    terms polarization(const Node& n) {
        Id leg = n.args.at(0), index = n.args.at(1), mass = n.args.at(2), hel = n.args.at(3);
        if (mass == zero) {
            if (hel != one && hel != a.integer(-1))
                fail("massless vector needs helicity +1 or -1");
            int block = hel == one ? 0 : 1;
            auto ref = spinor(leg, block, "Xi"), p = spinor(leg, 1 - block, "Helicity");
            auto den = a.call("SpinorChain", {ref.spinor, spinor(leg, block, "Helicity").spinor});
            word w;
            if (!block)
                w.push_back(eps);
            w.push_back({matrix_kind::sigma, index, block});
            if (!block)
                w.push_back(eps);
            return {{{a.integer(block ? -1 : 1), inv_sqrt2, a.pow(den, -1)}, {{ref, p, std::move(w)}}, {}}};
        }
        if (local(a.node(hel)) != "Automatic")
            fail("massive vector requires symmetric spin indices");
        Id first = a.call("VectorSpinIndex", {leg, one}),
           second = a.call("VectorSpinIndex", {leg, a.integer(2)});
        terms out;
        for (auto pair : {std::make_pair(first, second), std::make_pair(second, first)})
            out.push_back({{a.exact(mpq_class(1, 2)), inv_sqrt2, a.pow(mass, -1)},
                           {{spinor(leg, 0, "Spin", pair.first),
                             spinor(leg, 1, "Spin", pair.second),
                             {eps, {matrix_kind::sigma, index, 0}, eps}}},
                           {}});
        return out;
    }
    std::vector<std::pair<Id, Id>> momenta(Id id) {
        const auto& n = a.node(id);
        if (local(n) != "Mom" || n.args.size() != 1)
            fail("invalid momentum " + a.format(id));
        const auto& p = a.node(n.args[0]);
        if (!p.compound)
            return {{one, n.args[0]}};
        if (local(p) != "Multiparticle")
            fail("unsupported momentum " + a.format(id));
        std::vector<std::pair<Id, Id>> out;
        for (auto leg : p.args)
            out.push_back({one, leg});
        return out;
    }
    Id dot(Id x, Id y) {
        std::vector<Id> out;
        for (auto [c, i] : momenta(x))
            for (auto [d, j] : momenta(y)) {
                Id value = a.call("MomProd", {i, j});
                if (i == j && a.leg(i) > 0 && size_t(a.leg(i)) <= a.masses().size())
                    value = a.pow(a.masses()[a.leg(i) - 1], 2);
                out.push_back(a.mul({c, d, value}));
            }
        return a.add(std::move(out));
    }
    terms expansion(Id id, bool keep_vectors) {
        if (!contains_tensor(id))
            return {{{id}, {}, {}}};
        const auto& n = a.node(id);
        auto h = local(n);
        if (h == "DiracChain")
            return dirac(n);
        if (h == "PolarizationVector") {
            if (keep_vectors)
                return {{{}, {}, {{tensor_kind::polarization, id, n.args.at(1)}}}};
            return polarization(n);
        }
        if (h == "LorentzScalarProduct")
            return {{{dot(n.args.at(0), n.args.at(1))}, {}, {}}};
        if (h == "LorentzMetric" || h == "LorentzMomentum")
            return {{{},
                     {},
                     {{h == "LorentzMetric" ? tensor_kind::metric : tensor_kind::momentum, n.args.at(0),
                       n.args.at(1)}}}};
        if (h == "Plus") {
            terms out;
            for (Id child : n.args) {
                auto t = expansion(child, keep_vectors);
                if (t.size() > term_limit - out.size())
                    expansion_limit();
                out.insert(out.end(), std::make_move_iterator(t.begin()), std::make_move_iterator(t.end()));
            }
            return out;
        }
        if (h == "Times") {
            terms out(1);
            for (Id child : n.args)
                out = product(out, expansion(child, keep_vectors));
            return out;
        }
        if (h == "Power" && a.leg(n.args.at(1)) > 0) {
            auto base = expansion(n.args[0], keep_vectors);
            if (base.empty())
                return {};
            int exponent = a.leg(n.args[1]);
            if (base.size() > 1) {
                size_t count = 1;
                for (int k = 0; k < exponent; ++k)
                    count = checked_product(count, base.size());
            } else if (size_t(exponent) > term_limit) {
                throw std::length_error(
                    "Feynman contraction: excessive repeated tensor factors in one monomial");
            }
            terms out(1);
            for (int k = 0; k < exponent; ++k)
                out = product(out, base);
            return out;
        }
        fail("unsupported tensor expression " + n.head);
        return {};
    }
    static void normalize(word& w, int& sign) {
        word out;
        for (matrix m : w) {
            if (m.kind == matrix_kind::epsilon && !out.empty() && out.back().kind == matrix_kind::epsilon) {
                out.pop_back();
                sign = -sign;
            } else
                out.push_back(m);
        }
        w = std::move(out);
    }
    struct location {
        size_t chain_index, matrix_index;
    };
    void contract_pair(term& t, location x, location y, int& sign) {
        auto c = t.chains[x.chain_index], d = t.chains[y.chain_index];
        if (x.chain_index == y.chain_index) {
            const auto& w = c.matrices;
            auto p = slice(w, 0, x.matrix_index), middle = slice(w, x.matrix_index + 1, y.matrix_index),
                 s = slice(w, y.matrix_index + 1, w.size());
            if (w[x.matrix_index].block == w[y.matrix_index].block) {
                p.push_back(eps);
                append(p, transpose(middle, sign));
                p.push_back(eps);
            } else
                t.chains.push_back({{}, {}, std::move(middle)});
            c.matrices = join(std::move(p), s);
            t.chains[x.chain_index] = std::move(c);
            return;
        }
        t.chains.erase(t.chains.begin() + y.chain_index);
        t.chains.erase(t.chains.begin() + x.chain_index);
        if (!c.left && d.left) {
            std::swap(c, d);
            std::swap(x, y);
        }
        auto p = slice(c.matrices, 0, x.matrix_index),
             q = slice(c.matrices, x.matrix_index + 1, c.matrices.size());
        auto r = slice(d.matrices, 0, y.matrix_index),
             s = slice(d.matrices, y.matrix_index + 1, d.matrices.size());
        const bool same = c.matrices[x.matrix_index].block == d.matrices[y.matrix_index].block;
        if (c.left && d.left) {
            if (same) {
                p.push_back(eps);
                append(p, transpose(r, sign));
                auto qt = transpose(q, sign);
                qt.push_back(eps);
                t.chains.push_back({c.left, d.left, std::move(p)});
                t.chains.push_back({c.right, d.right, join(std::move(qt), s)});
            } else {
                t.chains.push_back({c.left, d.right, join(std::move(p), s)});
                t.chains.push_back({d.left, c.right, join(std::move(r), q)});
            }
            return;
        }
        auto middle = join(std::move(s), r);
        if (!c.left) {
            p = join(std::move(q), p);
            q.clear();
        }
        if (same) {
            sign = -sign;
            p.push_back(eps);
            append(p, transpose(middle, sign));
            p.push_back(eps);
        } else
            append(p, middle);
        t.chains.push_back({c.left, c.right, join(std::move(p), q)});
    }
    Id finish(chain c, int& sign) {
        auto& w = c.matrices;
        normalize(w, sign);
        int block = 0;
        if (c.left) {
            block = c.left->block;
            if (!block) {
                if (w.empty() || w.front().kind != matrix_kind::epsilon)
                    fail("unpaired angle boundary");
                w.erase(w.begin());
            }
            if (c.right->block) {
                if (w.empty() || w.back().kind != matrix_kind::epsilon)
                    fail("unpaired square boundary");
                w.pop_back();
                sign = -sign;
            }
        } else {
            if (w.empty())
                return a.integer(2);
            if (w.front().block == 1)
                std::rotate(w.begin(), w.begin() + 1, w.end());
        }
        std::vector<Id> args;
        if (c.left)
            args.push_back(c.left->spinor);
        for (auto m : w) {
            if (m.kind != matrix_kind::momentum || m.block != block)
                fail("invalid open Weyl contraction");
            args.push_back(m.arg);
            block = 1 - block;
        }
        if (block != (c.right ? c.right->block : 0))
            fail("chirality mismatch");
        if (c.right)
            args.push_back(c.right->spinor);
        return a.call(c.left ? "SpinorChain" : "SpinorTrace", std::move(args));
    }
    // Once the metrics are eliminated, a scalar/vector tree is a sum of
    // products of two-vector contractions. Convert each pair separately so
    // the spin choices of unrelated external vectors never multiply out.
    Id vector_dot(tensor first, tensor second) {
        if (first.kind == tensor_kind::momentum && second.kind == tensor_kind::momentum)
            return dot(first.first, second.first);
        const Id mu = a.atom("HEPCATNativeValidationLorentzIndex");
        auto relabel = [&](tensor field) {
            if (field.kind == tensor_kind::momentum)
                return a.call("LorentzMomentum", {field.first, mu});
            const auto& n = a.node(field.first);
            auto args = n.args;
            args[1] = mu;
            return a.call("PolarizationVector", std::move(args));
        };
        Id l = relabel(first), r = relabel(second);
        auto key = std::minmax(l, r);
        auto it = scalar_products.find(key);
        if (it != scalar_products.end())
            return it->second;
        Id pair = a.mul({l, r});
        std::vector<Id> out;
        for (auto& t : expansion(pair, false))
            out.push_back(contract(std::move(t)));
        return scalar_products.emplace(key, a.add(std::move(out))).first->second;
    }
    Id contract(term t) {
        std::map<Id, size_t> counts;
        for (auto tensor : t.tensors) {
            if (tensor.kind == tensor_kind::metric)
                ++counts[tensor.first];
            ++counts[tensor.second];
        }
        for (const auto& c : t.chains)
            for (auto m : c.matrices)
                if (m.kind == matrix_kind::sigma)
                    ++counts[m.arg];
        for (auto [key, n] : counts)
            if (n != 2)
                fail("uncontracted Lorentz index " + a.format(key));
        for (;;) {
            auto i = std::find_if(t.tensors.begin(), t.tensors.end(),
                                  [](tensor s) { return s.kind == tensor_kind::metric; });
            if (i == t.tensors.end())
                break;
            auto mu = i->first, nu = i->second;
            t.tensors.erase(i);
            if (mu == nu) {
                t.scalars.push_back(a.integer(4));
                continue;
            }
            for (auto& s : t.tensors) {
                if (s.kind == tensor_kind::metric && s.first == mu)
                    s.first = nu;
                if (s.second == mu)
                    s.second = nu;
            }
            for (auto& c : t.chains)
                for (auto& m : c.matrices)
                    if (m.kind == matrix_kind::sigma && m.arg == mu)
                        m.arg = nu;
        }
        while (!t.tensors.empty()) {
            auto p = t.tensors.back();
            t.tensors.pop_back();
            auto i = std::find_if(t.tensors.begin(), t.tensors.end(),
                                  [&](tensor s) { return s.second == p.second; });
            if (i != t.tensors.end()) {
                t.scalars.push_back(vector_dot(*i, p));
                t.tensors.erase(i);
                continue;
            }
            bool found = false;
            for (auto& c : t.chains)
                for (auto& m : c.matrices)
                    if (m.kind == matrix_kind::sigma && m.arg == p.second) {
                        m.kind = matrix_kind::momentum;
                        m.arg = p.first;
                        found = true;
                    }
            if (!found)
                fail("uncontracted Lorentz momentum");
        }
        int sign = 1;
        for (;;) {
            std::map<Id, location> seen;
            bool paired = false;
            for (size_t ci = 0; ci < t.chains.size() && !paired; ++ci) {
                const auto& w = t.chains[ci].matrices;
                for (size_t j = 0; j < w.size(); ++j)
                    if (w[j].kind == matrix_kind::sigma) {
                        auto [it, inserted] = seen.emplace(w[j].arg, location{ci, j});
                        if (!inserted) {
                            contract_pair(t, it->second, {ci, j}, sign);
                            t.scalars.push_back(a.integer(2));
                            paired = true;
                            break;
                        }
                    }
            }
            if (!paired)
                break;
        }
        for (auto& c : t.chains)
            t.scalars.push_back(finish(std::move(c), sign));
        t.scalars.push_back(a.integer(sign));
        return a.mul(std::move(t.scalars));
    }

  public:
    explicit conversion(Algebra& arena)
        : a(arena), zero(a.integer(0)), one(a.integer(1)), inv_sqrt2(a.parse("Power[2,Rational[-1,2]]")) {}
    Id run(Id expression) {
        if (!contains_tensor(expression))
            return expression;
        std::map<Id, bool> dirac;
        auto has_dirac = [&](auto&& self, Id id) -> bool {
            if (auto it = dirac.find(id); it != dirac.end())
                return it->second;
            const auto& n = a.node(id);
            return dirac[id] =
                       local(n) == "DiracChain" ||
                       std::any_of(n.args.begin(), n.args.end(), [&](Id child) { return self(self, child); });
        };
        const bool keep_vectors = !has_dirac(has_dirac, expression);
        std::vector<Id> converted;
        for (auto& t : expansion(expression, keep_vectors))
            converted.push_back(contract(std::move(t)));
        return a.add(std::move(converted));
    }
};
} // namespace
Id feynman_to_spinors(Algebra& a, Id expression) {
    return conversion(a).run(expression);
}
} // namespace hepcat
