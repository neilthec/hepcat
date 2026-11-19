#include "amp_expression.hpp"

#include <iostream>
#include <stdexcept>

using hepcat::amp_expression;
void check(bool ok, const char* message) {
    if (!ok) throw std::runtime_error(message);
}

int main() {
    try {
        const std::vector<std::string> masses{"0","0","0","0"};
        const std::string f="MomProd[1,2]", g="MomProd[1,3]";
        const std::string pole="PropDen[Mom[Multiparticle[1,2]],2]";
        // Shells (p1+p2)^2=4, (p1+p3)^2=16. Off either cut the cross
        // term matters, and the very same dot can appear in a denominator.
        std::string input="Plus[Times["+f+","+g+"],Power["+f+",-1],Power["+pole+",-1]]";
        amp_expression amp(input,masses,{{{1,2},"2"},{{1,3},"4"}});
        auto untouched=amp;
        check(&amp.reduce()==&amp, "must update in place");
        check(untouched.str()==input, "a value copy changed");
        const auto first=amp.str();
        auto tail=amp.discarded_remainder();
        check(tail.has_value() && !tail->discarded_remainder(), "missing or nested remainder");
        const auto firstTail=tail->str();
        check(first.find("16")!=std::string::npos && first.find("Power["+f+",-1]")!=std::string::npos &&
              first.find(pole)!=std::string::npos, "numerator or denominator scoping error");
        // Evaluate off both cuts with p1.p2=5 and p1.p3=7.
        auto evaluate=[](std::string value, std::string dot12="5", std::string dot13="7") {
            for (const auto& replacement : std::vector<std::pair<std::string,std::string>>{
                {"MomProd[1,2]",dot12},{"MomProd[1,3]",dot13},{"PropDen[Mom[Multiparticle[1,2]],2]","6"}}) {
                size_t pos;
                while ((pos=value.find(replacement.first))!=std::string::npos)
                    value.replace(pos,replacement.first.size(),replacement.second);
            }
            return amp_expression(value,{"0","0","0","0"}).reduce().str();
        };
        check(evaluate("Plus["+first+","+firstTail+"]")==evaluate(input), "off-cut reconstruction");
        check(evaluate(firstTail,"2","8")=="0","discarded remainder must vanish on the two cuts");
        amp.reduce();
        check(amp.str()==first && amp.discarded_remainder()->str()==firstTail,"second reduce lost remainder");
        tail->reduce();
        check(evaluate(tail->str())==evaluate(firstTail),"remainder reused its discarded cut");
        // A numerator can lose a factor proportional to its own propagator.
        // The corresponding amplitude remainder then survives away from the pole.
        const std::string removable="Times[Plus[Times[2,MomProd[1,2]],-4],Power["+pole+",-1]]";
        amp_expression exchange(removable,masses,{{{1,2},"2"}});
        exchange.reduce();
        check(exchange.str()=="0" && exchange.discarded_remainder() &&
              evaluate(exchange.discarded_remainder()->str())=="1","lost regular propagator remainder");
        amp_expression uncut(f,masses);
        uncut.reduce();
        check(!uncut.discarded_remainder() && uncut.str()==f,"unexpected shell or remainder");
        amp_expression fractional("Times[y,Power[x,0.5]]",masses);
        fractional.reduce();
        check(fractional.str().find("Power[x,0.5]")!=std::string::npos,"lost opaque noninteger power");
        amp_expression parameterizedPole("PropDen[Mom[Multiparticle[1,3]],Mass[1]]",
                                         {"me","me","0","0"},{{{1,3},"me"}});
        parameterizedPole.reduce();
        check(parameterizedPole.str()=="PropDen[Mom[Multiparticle[1,3]],me]", "propagator parameter mass");
        bool rejected=false;
        try { amp_expression invalid(f,masses,{{{1,2},"2"},{{3,4},"4"}}); }
        catch (const std::invalid_argument&) { rejected=true; }
        check(rejected,"contradictory complementary shells");
        return 0;
    } catch (const std::exception& e) { std::cerr << e.what() << '\n'; return 1; }
}
