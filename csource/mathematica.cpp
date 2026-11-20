#include "mathematica.hpp"
#include "reduce.hpp"

#include <cctype>
#include <stdexcept>
#include <string_view>
#include <unordered_set>

namespace hepcat {
namespace {

// The HEPCAT native wire protocol shortens these package heads. Restore their
// contexts so opening the file cannot accidentally use a shadowing notebook symbol.
using names = std::unordered_set<std::string_view>;
const names package_names{
    "Spinor", "SpinorHat", "SpinorChain", "SpinorChainReduced", "SpinorTrace", "SpinorTraceReduced",
    "SpinorEps", "SpinorZ", "Mom", "MomHat", "MomProd", "MomProdHat1", "MomProdHat2", "MomProdHat12",
    "Multiparticle", "MultiparticleDiff", "Mass", "Mandelstahm", "MandelstahmHat", "PropDen",
    "Coupling", "ModelParameter", "ModelFunction", "Particle", "xFactor", "xTildeFactor",
    "VectorSpinIndex", "ColorIndex", "ColorDelta", "ColorGenerator", "ColorStructure", "ColorTrace",
    "DiracWord", "DiracChain", "DiracSlash", "DiracGamma", "DiracGamma5", "FermionBar", "FermionKet",
    "LorentzIndex", "LorentzMetric", "LorentzMomentum", "LorentzScalarProduct", "PolarizationVector"
};
const names system_names{
    "Plus", "Times", "Power", "Rational", "Complex", "I", "Pi", "E", "List", "Rule",
    "Automatic", "True", "False", "Null", "Abs", "Sqrt", "Exp", "Log", "Sin", "Cos", "Tan",
    "ArcSin", "ArcCos", "ArcTan", "Conjugate", "Re", "Im", "KroneckerDelta"
};

bool symbol_name(std::string_view token) {
    bool start = true;
    for (unsigned char c : token) {
        if (c == '`') {
            if (start) return false;
            start = true;
        } else {
            if (!(c == '$' || std::isalpha(c) || c >= 128 || (!start && std::isdigit(c))))
                return false;
            start = false;
        }
    }
    return !start;
}

void append_symbol(std::string& out, const std::string& name) {
    if (!symbol_name(name))
        throw std::invalid_argument("Cannot represent native token as a Mathematica symbol: " + name);
    if (name.find('`') == std::string::npos) {
        if (package_names.count(name)) out += "ConstructiveDiagrams`";
        else if (system_names.count(name)) out += "System`";
        else if (name == "colorInternalIndex") out += "ConstructiveDiagrams`Private`";
        else out += "HEPCATNative`Symbols`"; // Unscoped, user-supplied native symbol.
    }
    out += name;
}

// Integers and Wolfram decimal/precision literals are written as supplied.
// Also accept C++ scientific notation and translate its exponent for InputForm.
bool append_number(std::string& out, const std::string& token) {
    std::size_t pos = 0;
    auto sign = [&] {
        if (pos < token.size() && (token[pos] == '+' || token[pos] == '-')) ++pos;
    };
    auto digits = [&] {
        const auto begin = pos;
        while (pos < token.size() && token[pos] >= '0' && token[pos] <= '9') ++pos;
        return pos != begin;
    };
    auto decimal = [&] {
        bool any = digits();
        if (pos < token.size() && token[pos] == '.') {
            ++pos;
            any = digits() || any;
        }
        return any;
    };
    sign();
    if (!decimal()) return false;
    if (pos < token.size() && token[pos] == '`') {
        ++pos;
        if (pos < token.size() && token[pos] == '`') ++pos;
        sign();
        decimal();
    }
    const auto exponent = pos;
    const bool cpp = pos < token.size() && (token[pos] == 'E' || token[pos] == 'e');
    if (cpp || token.compare(pos, 2, "*^") == 0) {
        pos += cpp ? 1 : 2;
        sign();
        if (!digits()) return false;
    }
    if (pos != token.size()) return false;
    if (cpp) {
        out.append(token, 0, exponent);
        if (token.find_first_of(".`") == token.npos) out += '.';
        out += "*^";
        out.append(token, exponent + 1);
    } else out += token;
    return true;
}

void append(std::string& out, const Algebra& a, Id id) {
    const auto& n = a.node(id);
    if (!n.compound && !n.head.empty()) {
        if (n.head.front() == '"') {
            out += n.head; // Already a quoted and escaped string in native FullForm.
            return;
        }
        if (append_number(out, n.head)) return;
    }
    const bool list = n.compound && (n.head == "List" || n.head == "System`List");
    if (!list) append_symbol(out, n.head);
    if (!n.compound) return;
    out += list ? '{' : '[';
    for (std::size_t i = 0; i < n.args.size(); ++i) {
        if (i) out += ", ";
        append(out, a, n.args[i]);
    }
    out += list ? '}' : ']';
}

} // namespace

std::string format_mathematica(const Algebra& arena, Id root, const std::vector<Id>& masses,
                              const std::map<std::vector<int>, Id>& shells, bool include_mass_rules) {
    std::string result;
    if (include_mass_rules) {
        result += "{{";
        for (std::size_t i = 0; i < masses.size(); ++i) {
            if (i) result += ", ";
            result += "ConstructiveDiagrams`Mass[" + std::to_string(i + 1) + "] -> ";
            append(result, arena, masses[i]);
        }
        for (const auto& [legs, mass] : shells) {
            result += ", ConstructiveDiagrams`Mass[ConstructiveDiagrams`Multiparticle[";
            for (std::size_t i = 0; i < legs.size(); ++i) {
                if (i) result += ", ";
                result += std::to_string(legs[i]);
            }
            result += "]] -> ";
            append(result, arena, mass);
        }
        result += "},\n ";
    }
    append(result, arena, root);
    if (include_mass_rules) result += '}';
    return result;
}

} // namespace hepcat
