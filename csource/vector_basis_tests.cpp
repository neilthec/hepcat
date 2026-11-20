#include "residue.hpp"
#include "vector_basis.hpp"
#include "vector_compact.hpp"

#include <iostream>

namespace {
// The pole fixes B. Writing its denominator in terms of p1.p2 creates
// a local multiple of B whose coefficient is invisible to every cut.
void check_local_remainder() {
    using namespace hepcat;
    Algebra a;
    const Id g = a.atom("g"), m = a.atom("m"), mu = a.atom("mu");
    a.setKinematics({m, m, m, m}, {});
    auto bracket = [&](int i, int j, bool angle) {
        auto endpoint = [&](int k) {
            return a.call("Spinor",
                          {a.atom("\"Spin\""), a.atom(angle ? "\"Angle\"" : "\"Square\""), a.integer(k)});
        };
        return a.call("SpinorChain", {endpoint(i), endpoint(j)});
    };
    const Id B = a.add(
        {a.mul({bracket(1, 2, true), bracket(3, 4, true), bracket(1, 2, false), bracket(3, 4, false)}),
         a.mul({bracket(1, 3, true), bracket(2, 4, true), bracket(1, 3, false), bracket(2, 4, false)})});
    const Id denominator = a.parse("PropDen[Mom[Multiparticle[1,2]],mu]");
    const Id expanded = a.parse("Plus[Times[2,MomProd[1,2]],Times[2,Power[m,2]],Times[-1,Power[mu,2]]]");
    const Id simple_pole = a.mul({B, a.pow(denominator, -1)});
    const Id probe = a.pow(a.parse("PropDen[Mom[Multiparticle[1,3]],probe]"), -1);
    auto trial = [&](Id coefficient, bool expect_smaller) {
        const Id source = a.mul({simple_pole, a.add({a.integer(1), a.mul({coefficient, expanded})})});
        const Id answer = compact_vector_amplitude(a, source);
        if (expect_smaller && expression_complexity(a, answer) >= expression_complexity(a, source))
            throw std::runtime_error("ordinary vector reduction failed to simplify a removable pole");
        const Id expected = a.add({simple_pole, a.mul({coefficient, B})});
        const Id difference = a.mul({probe, a.add({expected, a.mul({a.integer(-1), answer})})});
        const Id lost = a.mul({probe, coefficient, B});
        unsigned checked = 0;
        for (size_t sample = 910; sample < 960 && checked < 6; ++sample)
            try {
                const auto values = residue_components(a, {{1, 3}, "probe"}, {difference, lost}, sample,
                                                       {{g, 7}, {m, 3}, {mu, 5}});
                if (values[0] != std::array<mpq_class, 4>{} || values[1] == std::array<mpq_class, 4>{})
                    throw std::runtime_error("ordinary off-pole test lost a cut-invisible contact");
                ++checked;
            } catch (const residue_singular &) {
            }
        if (checked != 6)
            throw std::runtime_error("insufficient generic vector checks");
    };
    trial(g, true);
    // This coupling combination vanishes at the first support proposal's
    // parameter assignment. Independent symbolic/off-pole checks still retain it.
    trial(a.add({a.integer(-5), a.mul({a.integer(3), g})}), false);
}
} // namespace
int main() {
    using namespace hepcat;
    check_local_remainder();
    // Combining a+b=c with b+d=e replaces (a-d)/3 by (c-e)/3, canceling
    // the intermediate coefficient of b. The sixth coordinate is a spectator.
    TensorCoordinates source{{1, mpq_class(1, 3)}, {4, mpq_class(-1, 3)}, {6, 1}};
    std::vector<TensorCoordinates> rows{{{1, 1}, {2, 1}, {3, -1}}, {{2, 1}, {4, 1}, {5, -1}}};
    std::map<Id, std::size_t> weights{{1, 9}, {4, 9}};
    TensorCoordinates expected{{3, mpq_class(1, 3)}, {5, mpq_class(-1, 3)}, {6, 1}};
    if (reduceTensorSupport(source, rows, weights) != expected) {
        std::cerr << "Failed to eliminate both costly coordinates using the two identities.\n";
        return 1;
    }
    // A dependent rescaling can change the proposed basis but cannot change
    // the exact result. An independent spectator has to remain untouched.
    rows.push_back({{1, mpq_class(2, 7)}, {2, mpq_class(2, 7)}, {3, mpq_class(-2, 7)}});
    if (reduceTensorSupport(source, rows, weights) != expected ||
        reduceTensorSupport({{6, 1}}, rows, weights) != TensorCoordinates{{6, 1}} ||
        !reduceTensorSupport(rows.back(), rows, weights).empty())
        return 1;
}
