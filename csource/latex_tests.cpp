#include "amp_expression.hpp"

#include <iostream>
#include <stdexcept>

namespace {
std::string latex(const std::string& input) {
    hepcat::amp_expression amplitude(input, std::vector<std::string>(12, "0"));
    const auto output = amplitude.latex();
    if (amplitude.str() != input)
        throw std::runtime_error("LaTeX output changed " + input);
    return output;
}

void expect(const std::string& input, const std::string& expected) {
    const auto result = latex(input);
    if (result != expected)
        throw std::runtime_error(input + "\n  expected: " + expected + "\n  obtained: " + result);
}

void spinors_and_color() {
    expect(R"(SpinorChain[Spinor["Spin","Angle",1],Spinor["Helicity","Angle",2]])",
           R"(\langle \mathbf{1}2\rangle)");
    expect(R"(SpinorChain[Spinor["Helicity","Square",2],Mom[1],Spinor["Helicity","Angle",3]])",
           R"([213\rangle)");
    expect(R"(SpinorChain[Spinor["Xi","Angle",4],Mom[Multiparticle[1,10]],Spinor["Spin","Lower","Square",12,J]])",
           R"(\langle \xi_{4},P_{1,10},\mathbf{12}_{J}])");
    expect(R"(SpinorChain[Spinor["Zeta","Square",1],Spinor["Spin","Upper","Square",2,J]])",
           R"([\tilde{\zeta}_{1},\mathbf{2}^{J}])");
    expect("SpinorTrace[Mom[1],Mom[2]]", R"(\mathrm{Tr}\left(p_{1}\,p_{2}\right))");
    expect("ColorGenerator[ColorIndex[2],ColorIndex[1],colorInternalIndex[List[1,3],8]]",
           R"(\left(T^{a_{1,3}}\right)_{c_{2},c_{1}})");
    expect("ColorDelta[ColorIndex[1],ColorIndex[2]]", R"(\delta_{c_{1},c_{2}})");
    expect("Power[ColorStructure[i,j,k],2]", R"({f^{i,j,k}}^{2})");
}

void arithmetic_and_invariants() {
    expect("Times[2,3]", R"(2\cdot 3)");
    expect("Times[Rational[-2,3],x,Power[Plus[a,b],-1]]", R"(-\frac{2\,x}{3\,\left(a + b\right)})");
    expect("Times[x,Power[Plus[a,b],-1]]", R"(\frac{x}{a + b})");
    expect("Times[x,Power[Times[Plus[a,b],Plus[c,d]],-1]]",
           R"(\frac{x}{\left(a + b\right)\,\left(c + d\right)})");
    expect("Times[x,Power[Times[b,c],-1],Power[Plus[d,e],-1]]",
           R"(\frac{x}{b\,c\,\left(d + e\right)})");
    expect("Times[x,Power[a,-1],Power[Times[-1,b],-1]]", R"(\frac{x}{a\,\left(-b\right)})");
    expect("Power[-0.5,2]", R"({\left(-0.5\right)}^{2})");
    expect("Power[Power[x,2],3]", R"({\left({x}^{2}\right)}^{3})");
    expect("Power[x,Rational[-1,2]]", R"(\frac{1}{\sqrt{x}})");
    expect("Power[Plus[x,y],Plus[p,q]]", R"({\left(x + y\right)}^{p + q})");
    expect("Power[PropDen[Mom[Multiparticle[1,3]],0],-1]", R"(\frac{1}{s_{13}})");
    expect("PropDen[Mom[Multiparticle[1,10]],m,w]", R"(s_{1,10} - {m}^{2} + \mathrm{i}\,m\,w)");
    expect("Mandelstahm[1,2]", "s_{12}");
    expect("Mom[12]", R"(p_{\{12\}})");
    expect("MomProd[1,2]", R"((p_{1} \cdot p_{2}))");
    expect("Power[1e-20,2]", R"({\left(1\times 10^{-20}\right)}^{2})");
    expect("Power[Complex[2,-3],2]", R"({\left(2 - 3\,\mathrm{i}\right)}^{2})");
}

void names_and_fallbacks() {
    expect(R"(ModelParameter["g_L","SM","Real"])", R"(\mathrm{g\_L})");
    const auto scoped = latex(R"(Plus[ModelParameter["g","QED","Real"],ModelParameter["g","SM","Real"]])");
    if (scoped.find("QED") == scoped.npos || scoped.find("SM") == scoped.npos)
        throw std::runtime_error("LaTeX merged distinct model parameters");
    const auto escaped = latex(R"(OddHead["a%&{}_#~^\\verb",1])");
    for (auto unsafe : {'%', '&', '#', '~', '^'})
        if (escaped.find(unsafe) != escaped.npos) throw std::runtime_error("Unescaped TeX in literal");
    if (escaped.find("OddHead") == escaped.npos || escaped.find("\\verb") != escaped.npos)
        throw std::runtime_error("Unrecognized expression escaped incorrectly");
}
} // namespace

int main() {
    try {
        spinors_and_color();
        arithmetic_and_invariants();
        names_and_fallbacks();
    } catch (const std::exception& e) {
        std::cerr << e.what() << '\n';
        return 1;
    }
}
