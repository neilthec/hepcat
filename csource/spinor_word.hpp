#pragma once
#include "reduce.hpp"

#include <algorithm>
#include <cctype>
#include <utility>

namespace hepcat::spinor_detail {
using Word = std::vector<Id>;
enum class Kind { Unknown, Spin, Helicity, Xi };
struct End {
    Kind kind = Kind::Unknown;
    bool angle = false;
    Id label = 0;
    int leg = 0;
    unsigned typeSlot = 0;
    bool indexed = false;
    bool upper = false;
    Id index = 0;
};

inline End endpoint(const Algebra& a, Id id) {
    const auto& n = a.node(id);
    if (n.head != "Spinor" || n.args.size() < 3 || n.args.size() > 5) return {};
    const auto& tag = a.node(n.args[0]).head;
    End e;
    if (tag == "\"Spin\"") e.kind = Kind::Spin;
    else if (tag == "\"Helicity\"") e.kind = Kind::Helicity;
    else if (tag == "\"Xi\"") e.kind = Kind::Xi;
    else return {};
    if (n.args.size() == 5) {
        const auto& variance = a.node(n.args[1]).head;
        if (e.kind != Kind::Spin || (variance != "\"Upper\"" && variance != "\"Lower\"")) return {};
        e.upper = variance == "\"Upper\"";
        e.indexed = true;
        e.index = n.args[4];
        e.typeSlot = 2;
    } else {
        if (n.args.size() == 4 && e.kind != Kind::Helicity) return {};
        e.typeSlot = 1;
    }
    const auto& type = a.node(n.args[e.typeSlot]).head;
    if (type != "\"Square\"" && type != "\"Angle\"") return {};
    e.angle = type == "\"Angle\"";
    e.label = n.args[e.typeSlot + 1];
    e.leg = a.leg(e.label);
    return e;
}

inline Id flip(Algebra& a, Id endpointId, const End& e) {
    Word args = a.node(endpointId).args;
    args[e.typeSlot] = a.atom(e.angle ? "\"Square\"" : "\"Angle\"");
    return a.call("Spinor", std::move(args));
}

inline int momentumLeg(const Algebra& a, Id id) {
    const auto& n = a.node(id);
    return n.head == "Mom" && n.args.size() == 1 ? a.leg(n.args[0]) : 0;
}

// A literal spin projection denotes one component. Only a named dummy
// index licenses the sum used in the massive completeness relation.
inline bool namedIndex(const Algebra& a, Id index) {
    const auto& n=a.node(index);
    if (n.compound || n.head.empty()) return false;
    const auto first=static_cast<unsigned char>(n.head.front());
    return std::isalpha(first) || first=='$' || first>=128;
}

inline bool external(const Algebra& a, int leg) {
    return leg > 0 && static_cast<std::size_t>(leg) <= a.masses().size();
}

inline Id dot(Algebra& a, int i, int j) {
    if (i == j) return a.pow(a.masses()[i - 1], 2);
    if (i > j) std::swap(i,j);
    return a.call("MomProd", {a.integer(i), a.integer(j)});
}

inline Id signedValue(Algebra& a, int sign, Id value) {
    return sign == 1 ? value : a.mul({a.integer(-1), value});
}

} // namespace hepcat::spinor_detail
