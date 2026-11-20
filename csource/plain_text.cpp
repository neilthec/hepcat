#include "amp_expression.hpp"
#include "reduce.hpp"

#include <algorithm>
#include <cctype>
#include <regex>
#include <set>

namespace hepcat {
namespace {

enum precedence { sum, product, negative, power, atom };

struct term {
    std::string text;
    precedence order = atom;

    std::string group_below(precedence minimum, bool tex = false) const {
        if (order >= minimum)
            return text;
        return tex ? "\\left(" + text + "\\right)" : "(" + text + ")";
    }
};

std::string join(const std::vector<std::string>& pieces, const std::string& separator) {
    std::string result;
    for (const auto& piece : pieces) {
        if (!result.empty())
            result += separator;
        result += piece;
    }
    return result;
}

std::string tex_product(const std::vector<std::string>& factors) {
    std::string text;
    for (const auto& factor : factors) {
        if (!text.empty()) {
            // Adjacent numeric factors need an explicit multiplication symbol.
            const bool digits_touch = std::isdigit(static_cast<unsigned char>(text.back())) &&
                (std::isdigit(static_cast<unsigned char>(factor.front())) || factor.front() == '.');
            text += digits_touch ? "\\cdot " : "\\,";
        }
        text += factor;
    }
    return text;
}

bool identifier(const std::string& text) {
    return !text.empty() && std::isalpha(static_cast<unsigned char>(text.front())) &&
           std::all_of(text.begin(), text.end(),
                       [](unsigned char c) { return std::isalnum(c) || c == '_'; });
}

bool short_legs(const std::vector<std::string>& labels) {
    return std::all_of(labels.begin(), labels.end(),
                       [](const std::string& s) { return s.size() == 1 && s[0] >= '0' && s[0] <= '9'; });
}

// A fallback is literal Wolfram input. Never interpret a model or symbol name
// (which can contain arbitrary user text) as a TeX command or a math operator.
std::string tex_literal(const std::string& value) {
    std::string out = "\\mbox{\\ttfamily ";
    for (unsigned char c : value) {
        if (std::isalnum(c) || c == '.' || c == ',' || c == '[' || c == ']')
            out += static_cast<char>(c);
        else if (c >= 32 && c <= 126)
            out += "\\char" + std::to_string(c) + "{}";
        else {
            // Preserve control and non-ASCII bytes even with a pdfLaTeX font.
            const char hex[] = "0123456789ABCDEF";
            out += "\\char92{}x";
            out += hex[c >> 4];
            out += hex[c & 15];
        }
    }
    return out + "}";
}

std::string tex_name(const std::string& name) {
    if (name.size() == 1)
        return name;
    std::string roman;
    for (char c : name)
        roman += c == '_' ? "\\_" : std::string(1, c);
    return "\\mathrm{" + roman + "}";
}

std::string tex_atom(const std::string& value) {
    if (value == "System`I" || value == "I") return "\\mathrm{i}";
    if (value == "System`Pi" || value == "Pi") return "\\pi";
    if (value == "System`E" || value == "E") return "\\mathrm{e}";
    if (identifier(value)) return tex_name(value);
    static const std::regex decimal(R"(([+-]?(?:[0-9]+(?:\.[0-9]*)?|\.[0-9]+))(?:(?:[eE]|\*\^)([+-]?[0-9]+))?)");
    std::smatch parts;
    if (std::regex_match(value, parts, decimal)) {
        if (parts[2].matched)
            return "\\left(" + parts[1].str() + "\\times 10^{" + parts[2].str() + "}\\right)";
        return value;
    }
    return tex_literal(value);
}

class formatter {
    const Algebra& algebra;
    const bool tex;
    // A short parameter name is safe only if it refers to exactly one object in this expression.
    std::map<std::string, Id> parameters;
    std::set<std::string> ambiguous;
    std::set<Id> visited;

    const Node& node(Id id) const { return algebra.node(id); }

    std::string literal(Id id) const {
        auto raw = algebra.format(id);
        return tex ? tex_literal(raw) : raw;
    }

    std::string subscript(const std::string& symbol, const std::string& labels) const {
        if (!tex) return symbol + "_" + labels;
        if (labels.front() != '{') return symbol + "_{" + labels + "}";
        // A single leg 12 must remain distinguishable from the pair 1,2.
        const auto list = labels.substr(1, labels.size() - 2);
        return symbol + "_{" + (list.find(',') == list.npos ? "\\{" + list + "\\}" : list) + "}";
    }

    std::string fraction(const std::string& num, const std::string& den) const {
        return "\\frac{" + num + "}{" + den + "}";
    }

    std::string exponentiate(const term& base, const std::string& exponent) const {
        const auto text = base.group_below(atom, tex);
        // Group the entire base, which may already have an index or superscript.
        return tex ? "{" + text + "}^{" + exponent + "}" : text + "^" + exponent;
    }

    term square_root(const std::string& radicand) const {
        return {tex ? "\\sqrt{" + radicand + "}" : "sqrt(" + radicand + ")"};
    }

    std::string parameter_name(const Node& n) const {
        if (n.head != "ModelParameter" || n.args.size() != 3)
            return {};
        const auto& name = node(n.args[0]);
        if (name.compound || name.head.size() < 3 || name.head.front() != '"' || name.head.back() != '"')
            return {};
        auto unquoted = name.head.substr(1, name.head.size() - 2);
        if (unquoted == "I" || unquoted == "Pi" || unquoted == "E" || unquoted.rfind("s_", 0) == 0 ||
            unquoted.rfind("p_", 0) == 0 || unquoted.rfind("M_", 0) == 0)
            return {};
        return identifier(unquoted) ? unquoted : "";
    }

    void collect_names(Id id) {
        if (!visited.insert(id).second)
            return;
        const auto& n = node(id);
        if (auto name = parameter_name(n); !name.empty()) {
            if (auto [it, inserted] = parameters.emplace(name, id); !inserted && it->second != id)
                ambiguous.insert(name);
        } else if (!n.compound && identifier(n.head))
            ambiguous.insert(n.head);
        for (auto child : n.args)
            collect_names(child);
    }

    std::string leg(Id id) const {
        if (algebra.leg(id))
            return node(id).head;
        return {};
    }

    std::string leg_list(Id id) const {
        const auto& n = node(id);
        if (n.head != "Multiparticle" || n.args.empty()) {
            auto label = leg(id);
            return label.size() > 1 ? "{" + label + "}" : label;
        }
        std::vector<std::string> labels;
        for (auto child : n.args) {
            auto label = leg(child);
            if (label.empty())
                return {};
            labels.push_back(label);
        }
        if (short_legs(labels))
            return join(labels, "");
        return "{" + join(labels, ",") + "}";
    }

    // Bracket chirality is independent at the two ends of a momentum chain.
    bool spinor_end(Id id, bool left, std::string& delimiter, std::string& label, bool& compact) const {
        const auto& n = node(id);
        if (n.head != "Spinor" || (n.args.size() != 3 && n.args.size() != 5))
            return false;
        const auto& kind = node(n.args[0]).head;
        const bool indexed = n.args.size() == 5;
        const auto& chirality = node(n.args[indexed ? 2 : 1]).head;
        if (chirality != "\"Angle\"" && chirality != "\"Square\"")
            return false;
        delimiter = chirality == "\"Angle\"" ? (left ? "<" : ">") : (left ? "[" : "]");
        if (tex && chirality == "\"Angle\"") delimiter = left ? "\\langle " : "\\rangle";
        label = leg(n.args[indexed ? 3 : 2]);
        if (label.empty())
            return false;
        compact &= label.size() == 1 && !indexed && (kind == "\"Helicity\"" || kind == "\"Spin\"");
        if (tex && kind == "\"Spin\"")
            label = "\\mathbf{" + label + "}";
        if (indexed) {
            const auto& position = node(n.args[1]).head;
            if (kind != "\"Spin\"" || (position != "\"Upper\"" && position != "\"Lower\""))
                return false;
            label += std::string(position == "\"Upper\"" ? "^{" : "_{") +
                     (tex ? render(n.args[4]).text : algebra.format(n.args[4])) + "}";
        } else if (kind == "\"Xi\"")
            label = (tex ? "\\xi_{" : "xi_{") + label + "}";
        else if (kind == "\"Zeta\"")
            label = (chirality == "\"Angle\"" ? (tex ? "\\zeta_{" : "zeta_{")
                                             : (tex ? "\\tilde{\\zeta}_{" : "tzeta_{")) + label + "}";
        else if (kind != "\"Spin\"" && kind != "\"Helicity\"")
            return false;
        return true;
    }

    std::string chain(const Node& n) const {
        if (n.args.size() < 2)
            return {};
        std::string left, first, right, last;
        bool compact = true;
        if (!spinor_end(n.args.front(), true, left, first, compact) ||
            !spinor_end(n.args.back(), false, right, last, compact))
            return {};
        std::vector<std::string> labels{first};
        for (std::size_t i = 1; i + 1 < n.args.size(); ++i) {
            const auto& insertion = node(n.args[i]);
            if (insertion.head != "Mom" || insertion.args.size() != 1)
                return {};
            auto momentum = leg(insertion.args[0]);
            compact &= momentum.size() == 1;
            if (momentum.empty()) {
                auto legs = leg_list(insertion.args[0]);
                if (legs.empty())
                    return {};
                momentum = subscript("P", legs);
            }
            labels.push_back(momentum);
        }
        labels.push_back(last);
        return left + join(labels, compact ? "" : ",") + right;
    }

    term raised(Id base, const mpq_class& exponent) const {
        auto value = render(base);
        if (exponent == 1)
            return value;
        if (exponent == mpq_class(1, 2))
            return square_root(value.text);
        auto exp = exponent.get_str();
        if (exponent.get_den() != 1)
            exp = tex ? fraction(exponent.get_num().get_str(), exponent.get_den().get_str()) : "(" + exp + ")";
        return {exponentiate(value, exp), power};
    }

    term multiply(const std::vector<Id>& factors) const {
        bool minus = false;
        std::vector<std::string> numerator;
        std::vector<term> denominator;
        for (auto factor : factors) {
            mpq_class value;
            if (algebra.number(factor, value)) {
                if (value < 0) {
                    minus = !minus;
                    value = -value;
                }
                if (tex && value.get_den() != 1) {
                    if (value.get_num() != 1)
                        numerator.push_back(value.get_num().get_str());
                    denominator.push_back({value.get_den().get_str()});
                } else if (value != 1)
                    numerator.push_back(value.get_str());
                continue;
            }
            const auto& f = node(factor);
            if (f.head == "Power" && f.args.size() == 2 && algebra.number(f.args[1], value) &&
                value < 0) {
                denominator.push_back(raised(f.args[0], -value));
            } else {
                auto part = render(factor);
                if (part.order != sum && part.text.front() == '-') {
                    minus = !minus;
                    part.text.erase(0, 1);
                }
                numerator.push_back(part.group_below(product, tex));
            }
        }
        auto text = numerator.empty() ? "1" : (tex ? tex_product(numerator) : join(numerator, " * "));
        if (tex && denominator.size() == 1) {
            // The fraction bar already groups the whole divisor, including a sum.
            text = fraction(text, denominator.front().text);
        } else if (tex && !denominator.empty()) {
            std::vector<std::string> divisors;
            for (const auto& divisor : denominator) {
                const bool signed_factor = divisor.text.front() == '-' || divisor.text.front() == '+';
                divisors.push_back(divisor.group_below(signed_factor ? power : product, true));
            }
            text = fraction(text, tex_product(divisors));
        } else {
            for (const auto& divisor : denominator)
                text += " / " + divisor.group_below(power);
        }
        return {(minus ? "-" : "") + text, product};
    }

    std::string color_index(Id id) const {
        const auto& n = node(id);
        const auto label = [&](Id child) { return tex ? render(child).text : algebra.format(child); };
        if ((n.head == "ColorIndex" || n.head == "ConstructiveDiagrams`ColorIndex") && n.args.size() == 1)
            return "c_{" + label(n.args[0]) + "}";
        if ((n.head == "ConstructiveDiagrams`Private`colorInternalIndex" ||
             n.head == "colorInternalIndex") &&
            n.args.size() == 2) {
            const auto& dimension = node(n.args[1]).head;
            if (dimension != "3" && dimension != "8")
                return literal(id);
            const auto& index = node(n.args[0]);
            std::vector<std::string> labels;
            if (index.head == "List") {
                for (auto child : index.args)
                    labels.push_back(label(child));
            } else {
                labels.push_back(label(n.args[0]));
            }
            return (dimension == "3" ? "d_{" : "a_{") + join(labels, ",") + "}";
        }
        return tex && !n.compound ? tex_atom(n.head) : literal(id);
    }

    std::string color(Id id) const {
        const auto& n = node(id);
        auto head = n.head;
        const std::string context = "ConstructiveDiagrams`";
        if (head.compare(0, context.size(), context) == 0)
            head.erase(0, context.size());
        if (head == "ColorIndex" || head == "Private`colorInternalIndex" || head == "colorInternalIndex")
            return color_index(id);
        if (head != "ColorGenerator" && head != "ColorDelta" && head != "ColorTrace" &&
            head != "ColorStructure")
            return {};
        std::vector<std::string> indices;
        for (auto child : n.args)
            indices.push_back(color_index(child));
        if (head == "ColorGenerator" && indices.size() == 3)
            return (tex ? "\\left(T^{" : "(T^{") + indices[2] + (tex ? "}\\right)_{" : "})_{") +
                   indices[0] + "," + indices[1] + "}";
        if (head == "ColorDelta" && indices.size() == 2)
            return (tex ? "\\delta_{" : "delta_{") + join(indices, ",") + "}";
        if (head == "ColorStructure" && indices.size() == 3)
            return "f^{" + join(indices, ",") + "}";
        if (head == "ColorTrace") {
            for (auto& index : indices)
                index = "T^{" + index + "}";
            return tex ? "\\mathrm{Tr}\\left(" + join(indices, "\\,") + "\\right)"
                       : "Tr(" + join(indices, " ") + ")";
        }
        return {};
    }

    std::string tex_function(const Node& n) const {
        const auto tail = n.head.rfind('`');
        const std::string head = n.head.substr(tail == n.head.npos ? 0 : tail + 1);
        const std::string context = tail == n.head.npos ? "" : n.head.substr(0, tail + 1);
        if (context.empty() || context == "System`") {
            static const std::map<std::string, std::string> functions = {
                {"Sin", "\\sin"}, {"Cos", "\\cos"}, {"Tan", "\\tan"}, {"ArcSin", "\\arcsin"},
                {"ArcCos", "\\arccos"}, {"ArcTan", "\\arctan"}, {"Log", "\\log"}, {"Exp", "\\exp"}};
            if (n.args.size() == 1) {
                if (head == "Abs") return "\\left|" + render(n.args[0]).text + "\\right|";
                if (auto f = functions.find(head); f != functions.end())
                    return f->second + "\\left(" + render(n.args[0]).text + "\\right)";
            }
        }
        if (!context.empty() && context != "ConstructiveDiagrams`") return {};
        if (n.args.size() == 2 && (head == "Mandelstahm" || head == "xFactor" || head == "xTildeFactor")) {
            std::vector<std::string> labels{leg(n.args[0]), leg(n.args[1])};
            if (labels[0].empty() || labels[1].empty()) return {};
            return std::string(head == "Mandelstahm" ? "s" : head == "xFactor" ? "x" : "\\tilde{x}") +
                   "_{" + join(labels, short_legs(labels) ? "" : ",") + "}";
        }
        if (head == "Coupling" && n.args.size() == 1) return "g_{" + render(n.args[0]).text + "}";
        if (head == "SpinorTrace") {
            std::vector<std::string> factors;
            for (auto id : n.args) factors.push_back(render(id).group_below(product, true));
            return "\\mathrm{Tr}\\left(" + join(factors, "\\,") + "\\right)";
        }
        return {};
    }

  public:
    formatter(const Algebra& algebra, Id root, bool tex = false) : algebra(algebra), tex(tex) {
        collect_names(root);
    }

    term render(Id id) const {
        const auto& n = node(id);
        mpq_class value;
        if (algebra.number(id, value)) {
            auto order = value.get_den() != 1 ? product : (value < 0 ? negative : atom);
            if (tex && value.get_den() != 1) {
                bool minus = value < 0;
                if (minus) value = -value;
                return {(minus ? "-" : "") + fraction(value.get_num().get_str(), value.get_den().get_str()),
                        minus ? negative : atom};
            }
            return {value.get_str(), order};
        }
        if (!n.compound) {
            if (tex) {
                auto text = tex_atom(n.head);
                const auto order = text.front() == '-' || text.front() == '+' ? negative : atom;
                return {std::move(text), order};
            }
            if (n.head == "System`I")
                return {"I"};
            const bool signed_literal =
                !n.head.empty() && (n.head.front() == '-' || n.head.front() == '+');
            return {n.head, signed_literal ? negative : atom};
        }
        if (tex && (n.head == "Complex" || n.head == "System`Complex") && n.args.size() == 2) {
            const auto real = render(n.args[0]);
            auto imaginary = render(n.args[1]).group_below(product, true);
            const bool subtract = imaginary.front() == '-';
            if (subtract) imaginary.erase(0, 1);
            if (imaginary == "0") return real;
            imaginary = imaginary == "1" ? "\\mathrm{i}" : imaginary + "\\,\\mathrm{i}";
            if (real.text == "0") return {(subtract ? "-" : "") + imaginary, product};
            return {real.text + (subtract ? " - " : " + ") + imaginary, sum};
        }
        if (n.head == "Times")
            return multiply(n.args);
        if (n.head == "Plus" && !n.args.empty()) {
            std::string text;
            for (auto child : n.args) {
                auto addend = render(child);
                if (text.empty())
                    text = addend.text;
                else if (addend.order != sum && addend.text.front() == '-')
                    text += " - " + addend.text.substr(1);
                else
                    text += " + " + addend.text;
            }
            return {text, sum};
        }
        if (n.head == "Power" && n.args.size() == 2) {
            if (algebra.number(n.args[1], value)) {
                if (value < 0) {
                    auto divisor = raised(n.args[0], -value);
                    if (tex) return {fraction("1", divisor.text)};
                    return {"1 / " + divisor.group_below(power), product};
                }
                return raised(n.args[0], value);
            }
            auto exponent = render(n.args[1]);
            return {exponentiate(render(n.args[0]), tex ? exponent.text : exponent.group_below(atom)), power};
        }
        if (n.head == "Sqrt" && n.args.size() == 1)
            return square_root(render(n.args[0]).text);
        if (auto name = parameter_name(n); !name.empty() && !ambiguous.count(name))
            return {tex ? tex_name(name) : name};
        if (n.head == "SpinorChain") {
            if (auto brackets = chain(n); !brackets.empty())
                return {brackets};
        }
        if (n.head == "PropDen" && (n.args.size() == 2 || n.args.size() == 3)) {
            const auto& p = node(n.args[0]);
            if (p.head == "Mom" && p.args.size() == 1) {
                auto legs = leg_list(p.args[0]);
                if (!legs.empty()) {
                    std::string text = subscript("s", legs);
                    const auto mass = n.args[1];
                    if (algebra.number(mass, value) && value == 0)
                        return {text};
                    text += " - " + raised(mass, 2).text;
                    if (n.args.size() == 3 && !(algebra.number(n.args[2], value) && value == 0))
                        text += (tex ? " + \\mathrm{i}\\," : " + I * ") +
                                render(mass).group_below(product, tex) + (tex ? "\\," : " * ") +
                                render(n.args[2]).group_below(product, tex);
                    return {text, sum};
                }
            }
        }
        if ((n.head == "Mom" || n.head == "Mass") && n.args.size() == 1) {
            if (auto legs = leg_list(n.args[0]); !legs.empty())
                return {subscript(n.head == "Mom" ? "p" : "M", legs)};
        }
        if (n.head == "MomProd" && n.args.size() == 2) {
            auto left = leg_list(n.args[0]), right = leg_list(n.args[1]);
            if (!left.empty() && !right.empty())
                return {"(" + subscript("p", left) + (tex ? " \\cdot " : " . ") + subscript("p", right) + ")"};
        }
        if (auto tensor = color(id); !tensor.empty())
            return {tensor};
        if (tex) {
            if (auto function = tex_function(n); !function.empty()) return {function};
        }
        return {literal(id)};
    }
};

// Break before an existing arithmetic operator, preferring outer additions.
// A model parameter string or an unfamiliar FullForm subtree stays intact.
std::string wrap(const std::string& text) {
    constexpr std::size_t width = 92;
    if (text.size() <= width)
        return text;
    struct boundary {
        std::size_t offset;
        unsigned depth;
        bool add;
    };
    std::vector<boundary> breaks;
    unsigned depth = 0;
    bool quoted = false;
    for (std::size_t i = 0; i < text.size(); ++i) {
        const char c = text[i];
        if (quoted && c == '\\') {
            ++i;
            continue;
        }
        if (c == '"') {
            quoted = !quoted;
            continue;
        }
        if (quoted)
            continue;
        if (c == '(' || c == '{')
            ++depth;
        if ((c == ')' || c == '}') && depth > 0)
            --depth;
        if (i && i + 1 < text.size() && text[i - 1] == ' ' && text[i + 1] == ' ' &&
            (c == '+' || c == '-' || c == '*' || c == '/'))
            breaks.push_back({i, depth, c == '+' || c == '-'});
    }
    std::string output;
    std::size_t start = 0, indent = 0, next = 0;
    while (text.size() - start + indent > width && next < breaks.size()) {
        const auto limit = start + width - indent;
        std::size_t best = next;
        for (auto i = next; i < breaks.size() && breaks[i].offset <= limit; ++i) {
            const auto& a = breaks[i];
            const auto& b = breaks[best];
            const bool filled = a.offset - start + indent >= width / 3;
            const bool best_filled = b.offset - start + indent >= width / 3;
            if (filled != best_filled ? filled
                                      : (a.depth < b.depth || (a.depth == b.depth && a.add >= b.add)))
                best = i;
        }
        const auto& cut = breaks[best];
        output.append(indent, ' ').append(text, start, cut.offset - start - 1).push_back('\n');
        indent = 2 + 2 * std::min(cut.depth, 3u);
        start = cut.offset;
        next = best + 1;
    }
    return output.append(indent, ' ').append(text, start, std::string::npos);
}

} // namespace

std::string amp_expression::format_plain_text(const Algebra& algebra, std::uint32_t root) {
    return wrap(formatter(algebra, root).render(root).text);
}

std::string amp_expression::format_latex(const Algebra& algebra, std::uint32_t root) {
    auto text = formatter(algebra, root, true).render(root).text;
    // Physical line breaks make the TeX source editable. The author chooses
    // the display environment and the mathematical line breaks in the paper.
    std::size_t line_start = 0;
    for (std::size_t i = 1; i + 1 < text.size(); ++i) {
        if (i - line_start >= 80 && text[i] == ' ' &&
            (text[i + 1] == '+' || text[i + 1] == '-')) {
            text[i] = '\n';
            line_start = i + 1;
        }
    }
    return text;
}

} // namespace hepcat
