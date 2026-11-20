#include "amp_expression.hpp"
#include "reduce.hpp"

#include <algorithm>
#include <cctype>
#include <set>

namespace hepcat {
namespace {

enum precedence { sum, product, negative, power, atom };

struct term {
    std::string text;
    precedence order = atom;

    std::string group_below(precedence minimum) const {
        return order < minimum ? "(" + text + ")" : text;
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

bool identifier(const std::string& text) {
    return !text.empty() && std::isalpha(static_cast<unsigned char>(text.front())) &&
           std::all_of(text.begin(), text.end(),
                       [](unsigned char c) { return std::isalnum(c) || c == '_'; });
}

bool short_legs(const std::vector<std::string>& labels) {
    return std::all_of(labels.begin(), labels.end(),
                       [](const std::string& s) { return s.size() == 1 && s[0] >= '0' && s[0] <= '9'; });
}

class formatter {
    const Algebra& algebra;
    // A short parameter name is safe only if it refers to exactly one object in this expression.
    std::map<std::string, Id> parameters;
    std::set<std::string> ambiguous;
    std::set<Id> visited;

    const Node& node(Id id) const { return algebra.node(id); }

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
    bool spinor_end(Id id, bool left, std::string& delimiter, std::string& label) const {
        const auto& n = node(id);
        if (n.head != "Spinor" || (n.args.size() != 3 && n.args.size() != 5))
            return false;
        const auto& kind = node(n.args[0]).head;
        const bool indexed = n.args.size() == 5;
        const auto& chirality = node(n.args[indexed ? 2 : 1]).head;
        if (chirality != "\"Angle\"" && chirality != "\"Square\"")
            return false;
        delimiter = chirality == "\"Angle\"" ? (left ? "<" : ">") : (left ? "[" : "]");
        label = leg(n.args[indexed ? 3 : 2]);
        if (label.empty())
            return false;
        if (indexed) {
            const auto& position = node(n.args[1]).head;
            if (kind != "\"Spin\"" || (position != "\"Upper\"" && position != "\"Lower\""))
                return false;
            label += std::string(position == "\"Upper\"" ? "^{" : "_{") + algebra.format(n.args[4]) + "}";
        } else if (kind == "\"Xi\"")
            label = "xi_{" + label + "}";
        else if (kind == "\"Zeta\"")
            label = (chirality == "\"Angle\"" ? "zeta_{" : "tzeta_{") + label + "}";
        else if (kind != "\"Spin\"" && kind != "\"Helicity\"")
            return false;
        return true;
    }

    std::string chain(const Node& n) const {
        if (n.args.size() < 2)
            return {};
        std::string left, first, right, last;
        if (!spinor_end(n.args.front(), true, left, first) ||
            !spinor_end(n.args.back(), false, right, last))
            return {};
        std::vector<std::string> labels{first};
        for (std::size_t i = 1; i + 1 < n.args.size(); ++i) {
            const auto& insertion = node(n.args[i]);
            if (insertion.head != "Mom" || insertion.args.size() != 1)
                return {};
            auto momentum = leg(insertion.args[0]);
            if (momentum.empty()) {
                auto legs = leg_list(insertion.args[0]);
                if (legs.empty())
                    return {};
                momentum = "P_" + legs;
            }
            labels.push_back(momentum);
        }
        labels.push_back(last);
        return left + join(labels, short_legs(labels) ? "" : ",") + right;
    }

    term raised(Id base, const mpq_class& exponent) const {
        auto value = render(base);
        if (exponent == 1)
            return value;
        if (exponent == mpq_class(1, 2))
            return {"sqrt(" + value.text + ")"};
        auto exp = exponent.get_str();
        if (exponent.get_den() != 1)
            exp = "(" + exp + ")";
        return {value.group_below(atom) + "^" + exp, power};
    }

    term multiply(const std::vector<Id>& factors) const {
        bool minus = false;
        std::vector<std::string> numerator, denominator;
        for (auto factor : factors) {
            mpq_class value;
            if (algebra.number(factor, value)) {
                if (value < 0) {
                    minus = !minus;
                    value = -value;
                }
                if (value != 1)
                    numerator.push_back(value.get_str());
                continue;
            }
            const auto& f = node(factor);
            if (f.head == "Power" && f.args.size() == 2 && algebra.number(f.args[1], value) &&
                value < 0) {
                denominator.push_back(raised(f.args[0], -value).group_below(power));
            } else {
                auto part = render(factor);
                if (part.order != sum && part.text.front() == '-') {
                    minus = !minus;
                    part.text.erase(0, 1);
                }
                numerator.push_back(part.group_below(product));
            }
        }
        auto text = numerator.empty() ? "1" : join(numerator, " * ");
        for (const auto& divisor : denominator)
            text += " / " + divisor;
        return {(minus ? "-" : "") + text, product};
    }

    std::string color_index(Id id) const {
        const auto& n = node(id);
        if ((n.head == "ColorIndex" || n.head == "ConstructiveDiagrams`ColorIndex") && n.args.size() == 1)
            return "c_{" + algebra.format(n.args[0]) + "}";
        if ((n.head == "ConstructiveDiagrams`Private`colorInternalIndex" ||
             n.head == "colorInternalIndex") &&
            n.args.size() == 2) {
            const auto& dimension = node(n.args[1]).head;
            if (dimension != "3" && dimension != "8")
                return algebra.format(id);
            const auto& index = node(n.args[0]);
            std::vector<std::string> labels;
            if (index.head == "List") {
                for (auto child : index.args)
                    labels.push_back(algebra.format(child));
            } else {
                labels.push_back(algebra.format(n.args[0]));
            }
            return (dimension == "3" ? "d_{" : "a_{") + join(labels, ",") + "}";
        }
        return algebra.format(id);
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
            return "(T^{" + indices[2] + "})_{" + indices[0] + "," + indices[1] + "}";
        if (head == "ColorDelta" && indices.size() == 2)
            return "delta_{" + join(indices, ",") + "}";
        if (head == "ColorStructure" && indices.size() == 3)
            return "f^{" + join(indices, ",") + "}";
        if (head == "ColorTrace") {
            for (auto& index : indices)
                index = "T^{" + index + "}";
            return "Tr(" + join(indices, " ") + ")";
        }
        return {};
    }

  public:
    formatter(const Algebra& algebra, Id root) : algebra(algebra) { collect_names(root); }

    term render(Id id) const {
        const auto& n = node(id);
        mpq_class value;
        if (algebra.number(id, value)) {
            auto order = value.get_den() != 1 ? product : (value < 0 ? negative : atom);
            return {value.get_str(), order};
        }
        if (!n.compound) {
            if (n.head == "System`I")
                return {"I"};
            const bool signed_literal =
                !n.head.empty() && (n.head.front() == '-' || n.head.front() == '+');
            return {n.head, signed_literal ? negative : atom};
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
                if (value < 0)
                    return {"1 / " + raised(n.args[0], -value).group_below(power), product};
                return raised(n.args[0], value);
            }
            return {render(n.args[0]).group_below(atom) + "^" + render(n.args[1]).group_below(atom),
                    power};
        }
        if (n.head == "Sqrt" && n.args.size() == 1)
            return {"sqrt(" + render(n.args[0]).text + ")"};
        if (auto name = parameter_name(n); !name.empty() && !ambiguous.count(name))
            return {name};
        if (n.head == "SpinorChain") {
            if (auto brackets = chain(n); !brackets.empty())
                return {brackets};
        }
        if (n.head == "PropDen" && (n.args.size() == 2 || n.args.size() == 3)) {
            const auto& p = node(n.args[0]);
            if (p.head == "Mom" && p.args.size() == 1) {
                auto legs = leg_list(p.args[0]);
                if (!legs.empty()) {
                    std::string text = "s_" + legs;
                    const auto mass = n.args[1];
                    if (algebra.number(mass, value) && value == 0)
                        return {text};
                    text += " - " + raised(mass, 2).text;
                    if (n.args.size() == 3 && !(algebra.number(n.args[2], value) && value == 0))
                        text += " + I * " + render(mass).group_below(product) + " * " +
                                render(n.args[2]).group_below(product);
                    return {text, sum};
                }
            }
        }
        if ((n.head == "Mom" || n.head == "Mass") && n.args.size() == 1) {
            if (auto legs = leg_list(n.args[0]); !legs.empty())
                return {(n.head == "Mom" ? "p_" : "M_") + legs};
        }
        if (n.head == "MomProd" && n.args.size() == 2) {
            auto left = leg_list(n.args[0]), right = leg_list(n.args[1]);
            if (!left.empty() && !right.empty())
                return {"(p_" + left + " . p_" + right + ")"};
        }
        if (auto tensor = color(id); !tensor.empty())
            return {tensor};
        return {algebra.format(id)};
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

} // namespace hepcat
