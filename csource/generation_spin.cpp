#include "generation_spin.hpp"
#include <algorithm>
#include <functional>
#include <numeric>
#include <stdexcept>

namespace hepcat::generation {
namespace {
using word = std::vector<Id>;
struct spin_sum {
    Algebra& a;
    Id momentum, mass;
    int spin;
    Id upper, lower, angle;
    std::unordered_map<Id, bool> dependent;
    bool endpoint(Id id) const {
        const Node& n = a.node(id);
        return n.head == "Spinor" && n.args.size() == 5 && n.args[3] == momentum &&
               (n.args[1] == upper || n.args[1] == lower);
    }
    bool depends(Id id) {
        if (auto it = dependent.find(id); it != dependent.end()) return it->second;
        const auto& n = a.node(id);
        bool answer =
            endpoint(id) || std::any_of(n.args.begin(), n.args.end(), [&](Id x) { return depends(x); });
        return dependent.emplace(id, answer).first->second;
    }
    // Distribute only products containing the internal spin variable. Scalar
    // prefactors, other spin variables and propagator denominators stay intact.
    using terms = std::vector<word>;
    static terms multiply(const terms& x, const terms& y) {
        terms out;
        if (!x.empty() && y.size() > out.max_size() / x.size())
            throw std::length_error("spin polynomial too large");
        out.reserve(x.size() * y.size());
        for (const auto& left : x)
            for (const auto& right : y) {
                auto& row = out.emplace_back(left);
                row.insert(row.end(), right.begin(), right.end());
            }
        return out;
    }
    terms expand(Id id) {
        if (!depends(id)) return {{id}};
        const auto& n = a.node(id);
        if (n.head == "SpinorChain") return {{id}};
        if (n.head == "Plus") {
            terms out;
            for (Id child : n.args) {
                auto part = expand(child);
                out.insert(out.end(), std::make_move_iterator(part.begin()),
                           std::make_move_iterator(part.end()));
            }
            return out;
        }
        if (n.head == "Times") {
            terms out(1);
            for (Id child : n.args)
                out = multiply(out, expand(child));
            return out;
        }
        if (n.head == "Power" && n.args.size() == 2) {
            mpq_class q;
            if (a.number(n.args[1], q) && q.get_den() == 1 && q >= 0 && q <= 2 * spin) {
                auto factor = expand(n.args[0]);
                terms out(1);
                for (long j = 0; j < q.get_num().get_si(); ++j)
                    out = multiply(out, factor);
                return out;
            }
        }
        throw std::invalid_argument("an internal massive spin must enter polynomially in spinor chains: " +
                                    n.head);
    }
    struct port {
        size_t chain;
        bool left;
    };
    Id contract(std::vector<word> chains, word factors, const std::vector<int>& order) {
        int ui = 0, li = 0;
        for (auto& chain : chains)
            for (auto pos : {size_t(0), chain.size() - 1}) {
                auto id = chain[pos];
                if (!endpoint(id)) continue;
                auto args = a.node(id).args;
                int index = args[1] == upper ? ui++ : order.at(li++);
                args.back() = a.integer(index + 1);
                chain[pos] = a.call("Spinor", std::move(args));
            }
        if (ui != spin || li != spin)
            throw std::invalid_argument("internal spinor count disagrees with the model's twice-spin");
        int sign = 1;
        for (int ix = 1; ix <= spin; ++ix) {
            port up{}, down{};
            for (size_t c = 0; c < chains.size(); ++c) {
                auto& z = chains[c];
                if (z.empty()) continue;
                for (bool left : {true, false}) {
                    Id id = left ? z.front() : z.back();
                    if (endpoint(id) && a.leg(a.node(id).args[4]) == ix)
                        (a.node(id).args[1] == upper ? up : down) = {c, left};
                }
            }
            auto& x = chains[up.chain];
            auto& y = chains[down.chain];
            const bool au = a.node(up.left ? x.front() : x.back()).args[2] == angle;
            const bool ad = a.node(down.left ? y.front() : y.back()).args[2] == angle;
            if (up.chain == down.chain) {
                // Closing the upper ket onto the lower bra completes a chiral trace.
                if (up.left ? !ad : !au) sign = -sign;
                if (up.left) sign = -sign;
                word body(x.begin() + 1, x.end() - 1);
                if (au == ad) {
                    factors.push_back(mass);
                    if (!au && body.size() > 1) std::rotate(body.begin(), body.begin() + 1, body.end());
                } else if (a.node(x.front()).args[2] == angle)
                    body.push_back(a.call("Mom", {momentum}));
                else
                    body.insert(body.begin(), a.call("Mom", {momentum}));
                factors.push_back(a.call("SpinorTrace", std::move(body)));
                x.clear();
            } else {
                if (up.left) {
                    if (x.size() % 2 == 0) sign = -sign;
                    std::reverse(x.begin(), x.end());
                }
                if (!down.left) {
                    if (y.size() % 2 == 0) sign = -sign;
                    std::reverse(y.begin(), y.end());
                }
                if (!au) sign = -sign;
                x.pop_back();
                if (au == ad)
                    factors.push_back(mass);
                else
                    x.push_back(a.call("Mom", {momentum}));
                x.insert(x.end(), y.begin() + 1, y.end());
                y.clear();
            }
        }
        for (auto& chain : chains)
            if (!chain.empty()) factors.push_back(a.call("SpinorChain", std::move(chain)));
        factors.push_back(a.integer(sign));
        return a.mul(std::move(factors));
    }
    Id apply(Id expr) {
        if (expr == a.integer(0)) return expr;
        if (spin < 1 || spin > 4)
            throw std::invalid_argument("constructive massive sewing supports twice-spin one through four");
        std::vector<int> perm(spin);
        std::iota(perm.begin(), perm.end(), 0);
        word result;
        for (auto& monomial : expand(expr)) {
            std::vector<word> chains;
            word factors;
            int up = 0, lo = 0;
            for (auto id : monomial) {
                const auto& n = a.node(id);
                if (n.head == "SpinorChain" && depends(id)) {
                    if (n.args.size() < 2) throw std::invalid_argument("invalid spinor chain in model");
                    chains.push_back(n.args);
                    for (Id e : {n.args.front(), n.args.back()})
                        if (endpoint(e)) (a.node(e).args[1] == upper ? up : lo)++;
                } else
                    factors.push_back(id);
            }
            if (up != spin || lo != spin)
                throw std::invalid_argument("internal massive spinor count " + std::to_string(up) + "/" +
                                            std::to_string(lo) + " disagrees with model twice-spin " +
                                            std::to_string(spin));
            word pieces;
            do {
                pieces.push_back(contract(chains, {}, perm));
            } while (std::next_permutation(perm.begin(), perm.end()));
            mpq_class normalization(1, static_cast<unsigned long>(pieces.size()));
            factors.push_back(a.exact(normalization));
            factors.push_back(a.add(std::move(pieces)));
            result.push_back(a.mul(std::move(factors)));
        }
        return a.add(std::move(result));
    }
};
} // namespace
Id sew_spin(Algebra& a, Id expr, Id momentum, Id mass, int twice_spin) {
    spin_sum engine{
        a, momentum, mass, twice_spin, a.atom("\"Upper\""), a.atom("\"Lower\""), a.atom("\"Angle\""), {}};
    return engine.apply(expr);
}
} // namespace hepcat::generation
