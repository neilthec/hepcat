#include "amp_expression.hpp"

#include <algorithm>
#include <cctype>
#include <iostream>
#include <stdexcept>

using hepcat::amp_expression;
void check(bool ok, const char* message) {
    if (!ok) throw std::runtime_error(message);
}

// The notebook display may wrap long formulas. Check their mathematical
// punctuation separately from whitespace and from the exact exchange format.
void check_plain_text(const std::string& input, const std::string& expected, std::size_t legs = 4) {
    const amp_expression amp(input, std::vector<std::string>(legs, "0"));
    auto text = amp.plain_text();
    text.erase(std::remove_if(text.begin(), text.end(), [](unsigned char c) {
        return std::isspace(c);
    }), text.end());
    if (text != expected)
        throw std::runtime_error("plain_text(" + input + ") produced " + text + ", expected " + expected);
    check(amp.str() == input, "display changed the exact expression");
}

void test_plain_text() {
    check_plain_text(R"(SpinorChain[Spinor["Helicity","Angle",1],Spinor["Helicity","Angle",2]])", "<12>");
    check_plain_text(R"(SpinorChain[Spinor["Helicity","Square",1],Spinor["Helicity","Square",2]])", "[12]");
    check_plain_text(R"(SpinorChain[Spinor["Helicity","Square",2],Mom[1],Spinor["Helicity","Angle",3]])",
                     "[213>");
    check_plain_text(R"(SpinorChain[Spinor["Helicity","Angle",2],Mom[1],Spinor["Helicity","Square",3]])",
                     "<213]");
    check_plain_text(R"(SpinorChain[Spinor["Helicity","Square",1],Mom[2],Mom[3],Spinor["Helicity","Square",4]])",
                     "[1234]");
    check_plain_text(R"(SpinorChain[Spinor["Spin","Upper","Angle",1,i],Spinor["Spin","Lower","Angle",2,j]])",
                     "<1^{i},2_{j}>");
    check_plain_text(R"(SpinorChain[Spinor["Xi","Square",3],Spinor["Helicity","Square",2]])",
                     "[xi_{3},2]");
    check_plain_text(R"(SpinorChain[Spinor["Helicity","Angle",1],Spinor["Helicity","Angle",10]])",
                     "<1,10>", 10);

    const std::string momentum = "Mom[Multiparticle[1,3]]";
    const std::string mass = R"(ModelParameter["Me","QED","Real"])";
    check_plain_text("PropDen[" + momentum + ",0]", "s_13");
    check_plain_text("Power[PropDen[" + momentum + "," + mass + "],-1]", "1/(s_13-Me^2)");
    check_plain_text("PropDen[" + momentum + "," + mass + ",width]", "s_13-Me^2+I*Me*width");
    check_plain_text("Power[Plus[x,y],-2]", "1/(x+y)^2");
    check_plain_text("Power[-0.5,2]", "(-0.5)^2");
    check_plain_text("Power[Power[x,y],z]", "(x^y)^z");
    check_plain_text("Plus[x,Times[-1,y]]", "x-y");
    check_plain_text("Times[-1,Times[-1,x]]", "x");

    // Distinct model scopes and color-index positions must remain distinguishable.
    const std::string other_mass = R"(ModelParameter["Me","SM","Real"])";
    check_plain_text("Plus[" + mass + "," + other_mass + "]", mass + "+" + other_mass);
    check_plain_text("ColorGenerator[ColorIndex[2],ColorIndex[1],colorInternalIndex[List[1,2],8]]",
                     "(T^{a_{1,2}})_{c_{2},c_{1}}");
    check_plain_text("UnknownHead[Plus[x,y],1]", "UnknownHead[Plus[x,y],1]");
}

int main() {
    try {
        test_plain_text();
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
