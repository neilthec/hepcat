#include "continuations.hpp"
#include "residue.hpp"

#include <algorithm>
#include <iostream>
#include <stdexcept>

namespace {
void require(bool test, const char* message) {
    if (!test) throw std::runtime_error(message);
}
} // namespace

int main() {
    using namespace hepcat;
    Algebra a;
    Id m = a.atom("m"), zero = a.integer(0);
    a.setKinematics({m, m, zero, zero, zero}, {});
    numerator_shell own{{1, 3}, "m"};
    std::vector<numerator_shell> inventory{own, {{2, 3}, "m"}, {{1, 4}, "m"}, {{1, 5}, "m"}};
    Id prop = a.parse("PropDen[Mom[Multiparticle[1,3]],m]");
    Id h3 = a.parse("Spinor[\"Helicity\",\"Angle\",3]");
    Id h4 = a.parse("Spinor[\"Helicity\",\"Angle\",4]");
    Id xi = a.parse("Spinor[\"Xi\",\"Angle\",r]");
    auto chain = [&](std::vector<Id> parts) { return a.call("SpinorChain", std::move(parts)); };
    Id num = chain({xi, h4}), den = chain({xi, h3});
    auto inv = [&](Id x) { return a.pow(x, -1); };
    Id origin = a.mul({num, inv(den), inv(prop)});
    auto lifts = channel_continuations(a, origin, own, inventory);
    Id den23 = a.parse("PropDen[Mom[Multiparticle[2,3]],m]");
    Id physical_num =
        a.parse("SpinorChain[Spinor[\"Helicity\",\"Square\",3],Mom[2],Spinor[\"Helicity\",\"Angle\",4]]");
    require(std::find(lifts.begin(), lifts.end(), a.mul({physical_num, inv(den23), inv(prop)})) != lifts.end(),
            "emitter 3 may use momentum 2 but cannot reuse its own cut");
    for (Id p : lifts)
        require(a.format(p).find("\"Xi\"") == std::string::npos, "leftover reference");
    auto reverse = channel_continuations(a, a.mul({num, inv(chain({h3, xi})), inv(prop)}), own, inventory);
    require(std::find(reverse.begin(), reverse.end(),
                      a.mul({a.integer(-1), physical_num, inv(den23), inv(prop)})) != reverse.end(),
            "right-hand denominator sign");
    Id ambiguous = a.mul({origin, inv(chain({xi, a.parse("Spinor[\"Helicity\",\"Angle\",5]")}))});
    require(channel_continuations(a, ambiguous, own, inventory).empty(), "ambiguous shared reference");

    // The test numerator contains a shifted spinor endpoint, an internal
    // momentum insertion and a scalar product. Independent cut samples must
    // recover the same state sum for this channel.
    Id c = a.parse("SpinorChain[Spinor[\"Helicity\",\"Square\",3],Mom[4],Spinor[\"Helicity\",\"Angle\",5]]");
    Id other = a.parse("MomProd[1,4]");
    Id full = a.mul({a.add({c, other}), inv(prop)});
    auto shifted = shifted_channel_continuations(a, {{own, full}}, 3, 4);
    require(shifted.size() == 1 && shifted[0] != full, "nontrivial null shift");
    for (unsigned sample = 0, okay = 0; okay < 8; ++sample) {
        require(sample < 200, "too many singular samples");
        try {
            auto r = residue_sample(a, own, {full, shifted[0]}, sample);
            require(r[0] == r[1], "shift changed the original residue");
            ++okay;
        } catch (const residue_singular&) {
        }
    }
    // Compare off-cut sign and slope with the defining shift [3| -> [3| + z [4|.
    Id endpoint = a.parse("SpinorChain[Spinor[\"Helicity\",\"Square\",3],Spinor[\"Helicity\",\"Square\",5]]");
    Id added = a.parse("SpinorChain[Spinor[\"Helicity\",\"Square\",4],Spinor[\"Helicity\",\"Square\",5]]");
    Id slope =
        a.parse("SpinorChain[Spinor[\"Helicity\",\"Angle\",3],Mom[1],Spinor[\"Helicity\",\"Square\",4]]");
    Id z = a.mul({a.integer(-1), prop, inv(slope)});
    auto shifted_endpoint = shifted_channel_continuations(a, {{own, a.mul({endpoint, inv(prop)})}}, 3, 4);
    require(shifted_endpoint == std::vector<Id>{a.mul({inv(prop), a.add({endpoint, a.mul({z, added})})})},
            "incorrect off-cut null shift sign or slope");
    auto repeated = shifted_channel_continuations(a, {{own, full}, {own, full}}, 3, 4);
    require(repeated.size() == 2 && repeated[0] == repeated[1],
            "a direct shifted sum must count equal state contributions independently");
    require(shifted_channel_continuations(a, {{own, full}}, 4, 5).empty(), "cut independent of shifted pair");
    require(shifted_channel_continuations(a, {{own, full}}, 1, 4).empty(), "massive shift");
    // On either mixed shift the selected pole is reached with z = 0 on
    // its own cut. Include both spinors of the massive leg and a momentum
    // insertion. A missing moving channel invalidates the whole contour sum.
    Id mixed = a.parse("Times[SpinorChain[Spinor[\"Spin\",\"Angle\",2],Spinor[\"Helicity\",\"Angle\",5]],"
                       "SpinorChain[Spinor[\"Spin\",\"Square\",2],Mom[3],Spinor[\"Helicity\",\"Angle\",5]]]");
    Id unshifted = a.mul({mixed, inv(prop)});
    for (bool square : {false, true}) {
        auto lifted = massive_shifted_channel_continuations(a, {{own, unshifted}}, 3, 2, square);
        require(lifted.size() == 1 && lifted.front() != unshifted, "nontrivial massive shift");
        for (unsigned sample = 23, checked = 0; checked < 8; ++sample) {
            require(sample < 223, "singular mixed-shift cut samples");
            try {
                auto r = residue_sample(a, own, {unshifted, lifted.front()}, sample);
                require(r[0] == r[1], "mixed shift changed the original residue");
                ++checked;
            } catch (const residue_singular&) {}
        }
        // Check the transported Dirac equation away from the original pole.
        // This detects a wrong sign in a spinor shift that vanishes at z = 0.
        identity_check_options probe;
        probe.seed = 56113;
        probe.samples = 32;
        const std::vector<std::string> masses{"m","m","0","0","0"};
        for (bool angle : {true, false}) {
            auto endpoint = [&](std::string source, bool ang, int leg) {
                return a.call("Spinor", {a.atom(source), a.atom(ang ? "\"Angle\"" : "\"Square\""), a.integer(leg)});
            };
            Id spin = endpoint("\"Spin\"", angle, 2), flipped = endpoint("\"Spin\"", !angle, 2);
            Id end = endpoint("\"Helicity\"", !angle, 5);
            Id raw = a.mul({inv(prop), chain({spin,a.call("Mom",{a.integer(2)}),end})});
            Id reduced = a.mul({inv(prop),m,chain({flipped,end})});
            auto states = massive_shifted_channel_continuations(a, {{own, raw},{own, reduced}}, 3, 2, square);
            require(states.size() == 2, "incomplete transported Dirac comparison");
            require(amp_expression(a,states[0],masses).check_identity(amp_expression(a,states[1],masses),probe).passed(),
                    "massive spin-frame transport violates the Dirac equation");
        }
        require(massive_shifted_channel_continuations(a, {{own, unshifted}, {own, mixed}}, 3, 2, square).empty(),
                "incomplete mixed shift returned a partial sum");
    }
    std::cout << "Reference and null-pair continuation checks passed.\n";
}
