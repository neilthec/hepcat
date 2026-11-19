#include "model_expression.hpp"
#include <algorithm>
#include <cctype>
#include <limits>
#include <set>
#include <stdexcept>
#include <string_view>
#include <unordered_map>

namespace hepcat::model_detail {
namespace {
using view = std::string_view;
bool digit(char c) { return c >= '0' && c <= '9'; }
bool alpha(char c) { return (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || c == '_'; }
bool namechar(char c) { return alpha(c) || digit(c); }
const std::string context = "ConstructiveDiagrams`";

class parser {
    Algebra& a;
    view source;
    size_t pos = 0;
    Id scope;
    const std::vector<bool>& massive;
    bool feynman;
    char at(size_t i) const { return i < source.size() ? source[i] : '\0'; }
    char peek() {
        while (pos < source.size() && std::isspace(static_cast<unsigned char>(source[pos])))
            ++pos;
        return at(pos);
    }
    [[noreturn]] void error(const std::string& why) const {
        throw std::invalid_argument(why + " at column " + std::to_string(pos + 1));
    }
    bool eat(char c) {
        if (peek() != c) return false;
        ++pos;
        return true;
    }
    void expect(char c) {
        if (!eat(c)) error(std::string("expected '") + c + "'");
    }
    Id string_node(view s) { return a.atom("\"" + std::string(s) + "\""); }
    Id parameter(view s) { return a.call("ModelParameter", {string_node(s), scope, string_node("Real")}); }
    Id number() {
        std::string digits;
        while (digit(at(pos)))
            digits.push_back(source[pos++]);
        long scale = 0;
        if (at(pos) == '.') {
            ++pos;
            while (digit(at(pos))) {
                digits.push_back(source[pos++]);
                ++scale;
            }
        }
        size_t q = pos;
        if ((at(q) == 'e' || at(q) == 'E') || (!feynman && source.substr(q, 2) == "*^")) {
            q += at(q) == '*' ? 2 : 1;
            bool neg = at(q) == '-';
            if (neg || at(q) == '+') ++q;
            if (digit(at(q))) {
                long exponent = 0;
                while (digit(at(q))) {
                    if (exponent > 10000) error("exponent exceeds supported decimal range");
                    exponent = 10 * exponent + (at(q++) - '0');
                }
                scale -= neg ? -exponent : exponent;
                pos = q;
            }
        }
        mpz_class numerator(digits, 10), power;
        mpz_ui_pow_ui(power.get_mpz_t(), 10, std::abs(scale));
        mpq_class value = scale > 0 ? mpq_class(numerator, power) : mpq_class(numerator * power);
        value.canonicalize();
        return a.exact(value);
    }
    bool compact() const {
        if (massive.empty() || (at(pos) != '<' && at(pos) != '[')) return false;
        size_t q = pos + 1;
        while (digit(at(q)))
            ++q;
        return q >= pos + 3 && (at(q) == ']' || at(q) == '>');
    }
    int slot(char c) const {
        int i = c - '0';
        if (i <= 0 || size_t(i) > massive.size()) error("compact particle label outside vertex");
        return i;
    }
    Id chain() {
        bool left_angle = source[pos++] == '<';
        std::vector<int> indices;
        while (digit(at(pos)))
            indices.push_back(slot(source[pos++]));
        bool right_angle = source[pos++] == '>';
        auto endpoint = [&](int leg, bool angle) {
            return a.call("Spinor", {string_node(massive[leg - 1] ? "Spin" : "Helicity"),
                                     string_node(angle ? "Angle" : "Square"), a.integer(leg)});
        };
        std::vector<Id> args{endpoint(indices.front(), left_angle)};
        for (size_t i = 1; i + 1 < indices.size(); ++i)
            args.push_back(a.call("Mom", {a.integer(indices[i])}));
        args.push_back(endpoint(indices.back(), right_angle));
        return a.call("SpinorChain", std::move(args));
    }
    // Recognize the model writer's literal s13-Me^2 before parsing arithmetic.
    // Spaced subtraction is ordinary arithmetic in the existing model dialect.
    Id propagator(view s) {
        auto momentum =
            a.call("Mom", {a.call("Multiparticle", {a.integer(slot(s[1])), a.integer(slot(s[2]))})});
        Id mass = a.integer(0);
        if (at(pos) == '-' && (digit(at(pos + 1)) || alpha(at(pos + 1)))) {
            size_t q = pos + 1, start = q;
            if (alpha(at(q)))
                while (namechar(at(q)))
                    ++q;
            else
                while (digit(at(q)))
                    ++q;
            if (source.substr(q, 2) == "^2" && !namechar(at(q + 2))) {
                auto txt = source.substr(start, q - start);
                mass =
                    alpha(txt.front()) ? parameter(txt) : a.atom(mpz_class(std::string(txt), 10).get_str());
                pos = q + 2;
            }
        }
        return a.call("PropDen", {momentum, mass});
    }
    bool word(Id id) const { return a.node(id).head == context + "DiracWord"; }
    bool contains_word(Id id) const {
        const auto& n = a.node(id);
        if (word(id)) return true;
        return std::any_of(n.args.begin(), n.args.end(), [&](Id v) { return contains_word(v); });
    }
    bool tensor_operand(Id id) const {
        const auto& n = a.node(id);
        if (word(id) || n.head == "Mom" || n.head == context + "LorentzIndex") return true;
        return std::any_of(n.args.begin(), n.args.end(), [&](Id v) { return tensor_operand(v); });
    }
    Id times(Id l, Id r) {
        if (!feynman) return a.mul({l, r});
        const auto& ln = a.node(l);
        const auto& rn = a.node(r);
        if (ln.head == "Plus" || rn.head == "Plus") {
            std::vector<Id> terms;
            if (ln.head == "Plus")
                for (Id t : ln.args)
                    terms.push_back(times(t, r));
            else
                for (Id t : rn.args)
                    terms.push_back(times(l, t));
            return a.add(std::move(terms));
        }
        if (!contains_word(l) || !contains_word(r)) return a.mul({l, r});
        std::vector<Id> factors, operators;
        auto take = [&](Id x) {
            auto term = [&](Id t) {
                if (word(t))
                    operators.insert(operators.end(), a.node(t).args.begin(), a.node(t).args.end());
                else
                    factors.push_back(t);
            };
            if (a.node(x).head == "Times")
                for (Id t : a.node(x).args)
                    term(t);
            else
                term(x);
        };
        take(l);
        take(r);
        factors.push_back(a.call(context + "DiracWord", std::move(operators)));
        return a.mul(std::move(factors));
    }
    Id function(const std::string& s, std::vector<Id> args) {
        static const std::unordered_map<std::string, std::string> scalars = {
            {"Sqrt", "Power"},    {"sqrt", "Power"},  {"Sin", "Sin"},       {"sin", "Sin"},
            {"Cos", "Cos"},       {"cos", "Cos"},     {"Tan", "Tan"},       {"tan", "Tan"},
            {"ArcSin", "ArcSin"}, {"asin", "ArcSin"}, {"ArcCos", "ArcCos"}, {"acos", "ArcCos"},
            {"ArcTan", "ArcTan"}, {"atan", "ArcTan"}, {"Exp", "Exp"},       {"exp", "Exp"},
            {"Log", "Log"},       {"log", "Log"},     {"Abs", "Abs"},       {"abs", "Abs"},
            {"fabs", "Abs"},      {"cabs", "Abs"},    {"Power", "Power"},   {"pow", "Power"}};
        static const std::set<std::string> core = {"SpinorChain", "Spinor",        "SpinorTrace", "Mom",
                                                   "MomProd",     "Multiparticle", "Mass",        "PropDen",
                                                   "Mandelstahm", "List"};
        static const std::set<std::string> qualified = {"xFactor",         "xTildeFactor",   "Coupling",
                                                        "VectorSpinIndex", "ColorIndex",     "ColorDelta",
                                                        "ColorGenerator",  "ColorStructure", "ColorTrace"};
        auto require_scalar_arguments = [&] {
            if (feynman && std::any_of(args.begin(), args.end(), [&](Id id) { return tensor_operand(id); }))
                error("expected scalar arguments to " + s);
        };
        if (auto it = scalars.find(s); it != scalars.end()) {
            require_scalar_arguments();
            size_t n = args.size();
            if (s == "Sqrt" || s == "sqrt") {
                if (n != 1) error("square root expects one argument");
                args.push_back(a.exact(mpq_class(1, 2)));
            } else if (s == "Power" || s == "pow") {
                if (n != 2) error("power expects two arguments");
            } else if (n != 1 && !(n == 2 && !feynman && (it->second == "Log" || it->second == "ArcTan")))
                error("invalid arguments to " + s);
            return a.call(it->second == "Power" ? "Power" : "System`" + it->second, std::move(args));
        }
        if (!feynman) {
            if (core.count(s)) return a.call(s, std::move(args));
            if (qualified.count(s)) return a.call(context + s, std::move(args));
            if (s == "ColorInternalIndex" && args.size() == 2 && (a.leg(args[1]) == 3 || a.leg(args[1]) == 8))
                return a.call(context + "Private`colorInternalIndex", std::move(args));
        } else if (s == "G" && args.size() == 1) {
            auto h = a.node(args.front()).head;
            if (h == "Mom" || h == context + "LorentzIndex")
                return a.call(
                    context + "DiracWord",
                    {a.call(context + (h == "Mom" ? "DiracSlash" : "DiracGamma"), std::move(args))});
        }
        static const std::unordered_map<std::string, size_t> external = {
            {"initQCD5", 4}, {"MbEff", 1}, {"McEff", 1}, {"MqEff", 2}};
        if (auto i = external.find(s); i != external.end() && (!feynman || i->second == args.size())) {
            require_scalar_arguments();
            return a.call(context + "ModelFunction",
                          {string_node(s), scope, a.call("List", std::move(args))});
        }
        error("unsupported model function " + s);
    }
    Id identifier() {
        size_t start = pos;
        while (namechar(at(pos)))
            ++pos;
        std::string s(source.substr(start, pos - start));
        if (!massive.empty()) {
            if (s.size() == 3 && s[0] == 's' && digit(s[1]) && digit(s[2])) return propagator(s);
            size_t n = s.size();
            if ((n == 3 || (n == 4 && s[1] == 't')) && s[0] == 'x' && digit(s[n - 2]) && digit(s[n - 1]))
                return a.call(context + (n == 3 ? "xFactor" : "xTildeFactor"),
                              {a.integer(slot(s[n - 2])), a.integer(slot(s[n - 1]))});
        }
        char next = peek();
        if (next == '(' || (!feynman && next == '[')) {
            ++pos;
            char end = next == '(' ? ')' : ']';
            std::vector<Id> args;
            if (!eat(end)) {
                do {
                    args.push_back(sum());
                } while (eat(','));
                expect(end);
            }
            return function(s, std::move(args));
        }
        if (s == "i" || (!feynman && s == "I")) return a.atom("System`I");
        if (s == "Pi" && !feynman) return a.atom("System`Pi");
        if (s == "Sqrt2") return a.call("Power", {a.integer(2), a.exact(mpq_class(1, 2))});
        if (feynman) {
            if (s == "G5") return a.call(context + "DiracWord", {a.atom(context + "DiracGamma5")});
            if (s.size() > 1 && (s.front() == 'p' || s.front() == 'm' || s.front() == 'M') &&
                std::all_of(s.begin() + 1, s.end(), digit)) {
                if (s[1] == '0') error("invalid Lorentz slot");
                std::vector<Id> args{a.atom(s.substr(1))};
                if (s.front() == 'M') args.push_back(a.integer(2));
                return a.call(s.front() == 'p' ? "Mom" : context + "LorentzIndex", std::move(args));
            }
        }
        return parameter(s);
    }
    Id primary() {
        char c = peek();
        if (compact()) return chain();
        if (digit(c) || (c == '.' && digit(at(pos + 1)))) return number();
        if (alpha(c)) return identifier();
        if (eat('(')) {
            auto v = sum();
            expect(')');
            return v;
        }
        if (!feynman && eat('"')) {
            size_t start = pos;
            while (pos < source.size() && at(pos) != '"' && at(pos) != '\\')
                ++pos;
            auto s = source.substr(start, pos - start);
            if (at(pos++) != '"') error("unterminated or escaped string");
            return string_node(s);
        }
        error("expected an expression");
    }
    Id power() {
        Id out = primary();
        while (feynman && eat('.')) {
            Id arg = primary();
            auto l = a.node(out).head, r = a.node(arg).head;
            if (l == context + "LorentzIndex" && r == l)
                out = a.call(context + "LorentzMetric", {out, arg});
            else if (l == "Mom" && r == l)
                out = a.call(context + "LorentzScalarProduct", {out, arg});
            else if (l == "Mom" && r == context + "LorentzIndex")
                out = a.call(context + "LorentzMomentum", {out, arg});
            else if (r == "Mom" && l == context + "LorentzIndex")
                out = a.call(context + "LorentzMomentum", {arg, out});
            else
                error("uncontracted Lorentz dot product");
        }
        if (eat('^')) {
            Id exponent = unary();
            if (feynman && tensor_operand(exponent)) error("expected scalar exponent");
            if (feynman && contains_word(out)) {
                mpq_class k;
                if (!a.number(exponent, k) || k.get_den() != 1 || k < 0 || k > 32)
                    error("invalid power of Dirac word");
                Id base = out;
                out = a.integer(1);
                for (int i = 0; i < k.get_num().get_si(); ++i)
                    out = times(out, base);
            } else
                out = a.call("Power", {out, exponent});
        }
        return out;
    }
    Id unary() {
        if (eat('+')) return unary();
        if (eat('-')) return times(a.integer(-1), unary());
        return power();
    }
    Id product() {
        Id out = unary();
        for (;;) {
            if (eat('*')) {
                out = times(out, unary());
                continue;
            }
            if (eat('/')) {
                Id denominator = unary();
                if (feynman && contains_word(denominator)) error("Dirac expression in denominator");
                out = times(out, a.pow(denominator, -1));
                continue;
            }
            char c = peek();
            if (!feynman && (c == '(' || digit(c) || alpha(c) || compact())) {
                out = times(out, unary());
                continue;
            }
            return out;
        }
    }
    Id sum() {
        Id out = product();
        for (;;) {
            if (eat('+'))
                out = a.add({out, product()});
            else if (eat('-'))
                out = a.add({out, times(a.integer(-1), product())});
            else
                return out;
        }
    }

  public:
    parser(Algebra& arena, view text, Id parameter_scope, const std::vector<bool>& spins, bool feynman_mode)
        : a(arena), source(text), scope(parameter_scope), massive(spins), feynman(feynman_mode) {}
    Id parse() {
        Id out = sum();
        if (peek() != '\0' || pos != source.size()) error("unexpected token");
        return out;
    }
};
} // namespace
Id parse_expression(Algebra& a, std::string_view text, Id scope, const std::vector<bool>& massive,
                    bool feynman) {
    return parser(a, text, scope, massive, feynman).parse();
}
} // namespace hepcat::model_detail
