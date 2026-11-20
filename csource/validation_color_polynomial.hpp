#pragma once
#include "reduce.hpp"
#include <algorithm>
#include <stdexcept>

namespace hepcat::validation_detail {
inline std::string short_head(const Node& node) { return node.head.substr(node.head.find_last_of('`') + 1); }
inline bool color_head(const std::string& h) {
    return h == "ColorDelta" || h == "ColorTrace" || h == "ColorGenerator" || h == "ColorStructure";
}
class color_polynomial {
    const Algebra& a;
    std::map<Id, bool> color_memo;

  public:
    explicit color_polynomial(const Algebra& arena) : a(arena) {}
    bool contains_color(Id id) {
        if (auto it = color_memo.find(id); it != color_memo.end())
            return it->second;
        const auto& n = a.node(id);
        return color_memo[id] =
                   color_head(short_head(n)) ||
                   std::any_of(n.args.begin(), n.args.end(), [&](Id x) { return contains_color(x); });
    }
    struct term {
        std::vector<Id> tensors;
        std::vector<Id> coefficients;
    };
    static std::vector<term> multiply(const std::vector<term>& a, const std::vector<term>& b) {
        constexpr size_t limit = 100000;
        if (!b.empty() && a.size() > limit / b.size())
            throw std::invalid_argument("validation color expansion exceeds budget");
        std::vector<term> out;
        for (const auto& u : a)
            for (const auto& v : b) {
                term t = u;
                t.tensors.insert(t.tensors.end(), v.tensors.begin(), v.tensors.end());
                t.coefficients.insert(t.coefficients.end(), v.coefficients.begin(), v.coefficients.end());
                out.push_back(std::move(t));
            }
        return out;
    }
    std::vector<term> split(Id id) {
        const auto& n = a.node(id);
        if (!contains_color(id))
            return {{{}, {id}}};
        if (color_head(short_head(n)))
            return {{{id}, {}}};
        if (n.head == "Plus") {
            std::vector<term> out;
            for (Id child : n.args) {
                auto v = split(child);
                out.insert(out.end(), v.begin(), v.end());
            }
            return out;
        }
        if (n.head == "Times") {
            std::vector<term> out(1);
            for (Id child : n.args)
                out = multiply(out, split(child));
            return out;
        }
        if (n.head == "Power" && n.args.size() == 2) {
            int nterms = a.leg(n.args[1]);
            if (nterms > 0 && nterms <= 16) {
                auto base = split(n.args[0]);
                std::vector<term> out(1);
                for (int i = 0; i < nterms; ++i)
                    out = multiply(out, base);
                return out;
            }
        }
        throw std::invalid_argument("unsupported color expression " + a.format(id));
    }
};
} // namespace hepcat::validation_detail
