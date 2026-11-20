#include "amplitude.hpp"
#include "validation_checks.hpp"
#include <algorithm>
#include <iostream>
#include <stdexcept>

using namespace hepcat;
namespace {
void require(const std::vector<builtin_check>& rows, const std::string& kind, check_outcome status) {
    auto i = std::find_if(rows.begin(), rows.end(),
                          [&](const auto& row) { return row.kind == kind && row.status == status; });
    if (i != rows.end())
        return;
    for (auto& r : rows)
        std::cerr << r.kind << ' ' << r.label << ' ' << r.detail << '\n';
    throw std::runtime_error("missing expected " + kind + " outcome");
}
std::string bracket(int a, int b, const std::string& ch) {
    auto spin = [&](int p) { return "Spinor[\"Helicity\",\"" + ch + "\"," + std::to_string(p) + "]"; };
    return "SpinorChain[" + spin(a) + "," + spin(b) + "]";
}
} // namespace
int main() {
    const std::vector<std::string> masses(4, "0");
    amplitude amp(masses);
    const std::string p = "PropDen[Mom[Multiparticle[1,2]],0]", inv = "Power[" + p + ",-1]";
    const std::string extra = "Power[PropDen[Mom[Multiparticle[1,3]],0],-1]";
    amp.add_factorization_product("Times[-1," + inv + "]", {{1, 2}, "0"});
    auto check = [&](std::string expr, int bound = 4) {
        return native_builtin_checks(amp, amp_expression(expr, masses), bound, {}, 4, 1, 89);
    };
    auto valid = check(inv, -2);
    require(valid, "Physical", check_outcome::pass);
    // Adding an unlisted pole exercises both a passing physical cut and a
    // failing extra cut. Worker count must preserve their evidence and order.
    const amp_expression two_channels("Plus[" + inv + "," + extra + "]", masses);
    const auto serial = native_builtin_checks(amp, two_channels, 4, {}, 4, 1, 89, {}, 1);
    const auto parallel = native_builtin_checks(amp, two_channels, 4, {}, 4, 1, 89, {}, 2);
    if (serial.size() != parallel.size()) throw std::runtime_error("Changed cut inventory");
    for (std::size_t i = 0; i < serial.size(); ++i) {
        const auto& x = serial[i];
        const auto& y = parallel[i];
        if (x.label != y.label || x.kind != y.kind || x.status != y.status ||
            x.samples != y.samples || x.detail != y.detail)
            throw std::runtime_error("Parallel cut report differs from serial report");
    }
    require(valid, "HighEnergy", check_outcome::pass);
    require(valid, "Spurious", check_outcome::pass);
    require(check("Times[-1," + inv + "]"), "Physical", check_outcome::fail);
    require(check("Plus[" + inv + "," + extra + "]"), "Spurious", check_outcome::fail);
    require(check("Plus[" + inv + ",Mandelstahm[2,4]]", 0), "HighEnergy", check_outcome::fail);
    const auto angle = bracket(1, 2, "Angle"), square = bracket(1, 2, "Square");
    // 1/(<12>[12]) = -1/s12. Neither explicit bracket alone classifies this denominator.
    auto bracket_physical = check("Times[-1,Power[" + angle + ",-1],Power[" + square + ",-1]]", -2);
    require(bracket_physical, "Spurious", check_outcome::pass);
    require(bracket_physical, "Physical", check_outcome::pass);
    // A genuine extra pole, including one whose proper fraction begins only at z^-8.
    const auto other = bracket(1, 3, "Angle");
    require(check("Plus[" + inv + ",Power[" + other + ",-8]]"), "Spurious", check_outcome::fail);
    // An uncancelled scalar sum in a denominator has no propagator assignment.
    require(check("Power[Plus[1,Mandelstahm[1,2]],-1]"), "Spurious", check_outcome::fail);
    auto denom = "Plus[1," + angle + "]";
    require(check("Plus[Power[" + denom + ",-1],Times[" + angle + ",Power[" + denom + ",-1]]]"), "Spurious",
            check_outcome::pass);
    // A gluon exchange expressed either as generators or as two delta flows
    // must match in every external-color component. Flipping the subleading
    // singlet flow preserves the octet exchange but changes the residue.
    const std::string c1 = "ColorIndex[1]", c2 = "ColorIndex[2]", c3 = "ColorIndex[3]", c4 = "ColorIndex[4]";
    auto delta = [](std::string i, std::string j) { return "ColorDelta[" + i + "," + j + "]"; };
    const std::string generators =
        "Times[ColorGenerator[" + c1 + "," + c2 + ",a],ColorGenerator[" + c3 + "," + c4 + ",a]]";
    auto flows = [&](std::string coefficient) {
        return "Plus[Times[Rational[1,2]," + delta(c1, c4) + "," + delta(c3, c2) + "],Times[" + coefficient +
               "," + delta(c1, c2) + "," + delta(c3, c4) + "]]";
    };
    amplitude quarks(masses);
    quarks.add_factorization_product("Times[-1," + generators + "," + inv + "]", {{1, 2}, "0"});
    auto check_color = [&](const std::string& value, const std::vector<int>& reps) {
        return native_builtin_checks(quarks, amp_expression("Times[" + value + "," + inv + "]", masses), 0,
                                     {}, 4, 1, 73, reps);
    };
    const std::vector<int> reps{3, -3, 3, -3};
    require(check_color(flows("Rational[-1,6]"), reps), "Physical", check_outcome::pass);
    require(check_color(flows("Rational[1,6]"), reps), "Physical", check_outcome::fail);
    require(check_color(generators, {}), "Physical", check_outcome::unresolved);
    amplitude no_data(masses);
    require(native_builtin_checks(no_data, amp_expression("0", masses), 0), "Physical",
            check_outcome::unresolved);
    std::cout << "Independent validation of signed cuts, growth and denominator cancellation passed.\n";
}
