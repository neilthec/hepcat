#include "reduce.hpp"

#include <algorithm>
#include <charconv>
#include <cctype>
#include <climits>
#include <functional>
#include <limits>
#include <set>
#include <stdexcept>
#include <unordered_set>

namespace hepcat {
namespace {

bool integerText(const std::string& s) {
    auto first = s.begin();
    if (first != s.end() && *first == '-') ++first;
    return first != s.end() && std::all_of(first, s.end(),
        [](char c) { return c >= '0' && c <= '9'; });
}

bool machineInteger(const Node& n, long& v) {
    if (n.compound || n.head.empty()) return false;
    const char* end = n.head.data() + n.head.size();
    long parsed;
    const auto result = std::from_chars(n.head.data(), end, parsed);
    if (result.ec != std::errc() || result.ptr != end) return false;
    v = parsed;
    return true;
}

long checkedAdd(long a, long b) {
    if ((b > 0 && a > LONG_MAX - b) || (b < 0 && a < LONG_MIN - b))
        throw std::overflow_error("integer exponent sum outside supported range");
    return a + b;
}

size_t saturatingAdd(size_t a, size_t b) {
    return a > SIZE_MAX - b ? SIZE_MAX : a + b;
}

} // namespace

Id Algebra::find(size_t hash, const Node& candidate) const {
    const auto range = index_.equal_range(hash);
    for (auto it = range.first; it != range.second; ++it)
        if (node(it->second) == candidate) return it->second;
    return parent_ ? parent_->find(hash, candidate) : UINT32_MAX;
}

Id Algebra::raw(std::string head, std::vector<Id> args, bool compound) {
    compound = compound || !args.empty();
    size_t hash = std::hash<std::string>{}(head) ^ static_cast<size_t>(compound);
    size_t weight = 1;
    for (auto child : args) {
        hash ^= size_t(child) + 0x9e3779b9 + (hash << 6) + (hash >> 2);
        weight = saturatingAdd(weight, cost(child));
    }
    Node candidate{std::move(head), std::move(args), compound};
    const auto known = find(hash, candidate);
    if (known != UINT32_MAX) return known;
    if (size() >= UINT32_MAX) throw std::length_error("expression arena exhausted");
    const Id id = static_cast<Id>(size());
    nodes_.push_back(std::move(candidate));
    costs_.push_back(weight);
    index_.emplace(hash, id);
    return id;
}

int Algebra::leg(Id id) const {
    long v;
    return machineInteger(node(id), v) && v > 0 && v <= INT_MAX ? static_cast<int>(v) : 0;
}

bool Algebra::number(Id id, mpq_class& result) const {
    const auto& n = node(id);
    if (!n.compound && integerText(n.head)) {
        result = mpz_class(n.head, 10);
        return true;
    }
    if (n.head != "Rational" || n.args.size() != 2) return false;
    const auto& a = node(n.args[0]);
    const auto& b = node(n.args[1]);
    if (a.compound || b.compound || !integerText(a.head) || !integerText(b.head))
        throw std::invalid_argument("Rational expects integer numerator and denominator");
    if (mpz_class(b.head, 10) == 0) throw std::domain_error("zero rational denominator");
    result = mpq_class(mpz_class(a.head, 10), mpz_class(b.head, 10));
    result.canonicalize();
    return true;
}

Id Algebra::exact(const mpq_class& value) {
    auto numerator = atom(value.get_num().get_str());
    if (value.get_den() == 1) return numerator;
    return raw("Rational", {numerator, atom(value.get_den().get_str())});
}

Id Algebra::pow(Id base, long exponent) {
    if (exponent == 1) return base;
    if (exponent == LONG_MIN) throw std::overflow_error("unsupported minimum exponent");
    const auto& b = node(base);
    mpq_class value;
    if (number(base, value)) {
        if (exponent <= 0 && value == 0) throw std::domain_error("undefined power of zero");
        if (exponent == 0) return integer(1);
        auto n = value.get_num(), d = value.get_den();
        auto power = static_cast<unsigned long>(std::abs(exponent));
        mpz_pow_ui(n.get_mpz_t(), n.get_mpz_t(), power);
        mpz_pow_ui(d.get_mpz_t(), d.get_mpz_t(), power);
        mpq_class r(n,d);
        if (exponent < 0) r = 1/r;
        return exact(r);
    }
    if (exponent == 0) return integer(1);
    long inner;
    if (b.head == "Power" && b.args.size() == 2 && machineInteger(node(b.args[1]), inner)) {
        long combined;
        if (__builtin_mul_overflow(inner, exponent, &combined))
            throw std::overflow_error("integer exponent product outside supported range");
        return pow(b.args[0], combined);
    }
    // Pull out only a numerical coefficient. Keep products of sums factored.
    if (b.head == "Times" && !b.args.empty() && number(b.args.front(), value)) {
        std::vector<Id> rest(b.args.begin()+1, b.args.end());
        return mul({pow(b.args.front(), exponent), pow(mul(std::move(rest)), exponent)});
    }
    return raw("Power", {base, integer(exponent)});
}

Id Algebra::mul(std::vector<Id> input) {
    mpq_class coefficient = 1, value;
    std::map<Id,long> powers;
    while (!input.empty()) {
        Id id = input.back();
        input.pop_back();
        const auto& n = node(id);
        if (number(id,value)) {
            coefficient *= value;
        } else if (n.compound && n.head == "Times") {
            input.insert(input.end(), n.args.begin(), n.args.end());
        } else {
            long exp = 1;
            if (n.head == "Power" && n.args.size() == 2 && machineInteger(node(n.args[1]), exp))
                id = n.args[0];
            auto& accumulated = powers[id];
            accumulated = checkedAdd(accumulated, exp);
        }
    }
    if (coefficient == 0) return integer(0);
    std::vector<Id> factors;
    if (coefficient != 1) factors.push_back(exact(coefficient));
    for (const auto& entry : powers)
        if (entry.second) factors.push_back(pow(entry.first,entry.second));
    if (factors.empty()) return integer(1);
    if (factors.size() == 1) return factors.front();
    return raw("Times", std::move(factors));
}

Id Algebra::add(std::vector<Id> input) {
    std::map<Id,mpq_class> coefficients;
    mpq_class constant = 0, value;
    while (!input.empty()) {
        Id id = input.back();
        input.pop_back();
        const auto& n = node(id);
        if (number(id,value)) { constant += value; continue; }
        if (n.compound && n.head == "Plus") {
            input.insert(input.end(), n.args.begin(), n.args.end());
            continue;
        }
        value = 1;
        if (n.head == "Times" && !n.args.empty() && number(n.args.front(), value)) {
            id = mul({n.args.begin()+1,n.args.end()});
        }
        coefficients[id] += value;
    }
    std::vector<Id> terms;
    if (constant != 0) terms.push_back(exact(constant));
    for (const auto& entry : coefficients)
        if (entry.second != 0) terms.push_back(mul({exact(entry.second),entry.first}));
    if (terms.empty()) return integer(0);
    if (terms.size() == 1) return terms.front();
    return raw("Plus",std::move(terms));
}

Id Algebra::call(std::string head, std::vector<Id> args) {
    if (head == "Plus") return add(std::move(args));
    if (head == "Times") return mul(std::move(args));
    long exponent;
    if (head == "Power" && args.size() == 2 && machineInteger(node(args[1]),exponent))
        return pow(args[0],exponent);
    auto id = raw(std::move(head),std::move(args),true);
    if (node(id).head == "Rational") {
        mpq_class value;
        if (!number(id,value)) throw std::invalid_argument("invalid rational");
        return exact(value);
    }
    return id;
}

Id Algebra::factorSum(Id expression) {
    const auto& sum = node(expression);
    if (sum.head != "Plus" || sum.args.size() < 2) return expression;
    // Common symbolic multiplicative factors, including propagator powers.
    auto factorsOf = [&](Id term) {
        const auto& t = node(term);
        std::vector<Id> factors = t.head == "Times" ? t.args : std::vector<Id>{term};
        std::sort(factors.begin(),factors.end());
        return factors;
    };
    auto common = factorsOf(sum.args.front());
    std::vector<Id> next;
    for (size_t i=1; i<sum.args.size() && !common.empty(); ++i) {
        const auto factors = factorsOf(sum.args[i]);
        next.clear();
        std::set_intersection(common.begin(),common.end(),factors.begin(),factors.end(),std::back_inserter(next));
        common.swap(next);
    }
    if (common.empty()) return expression;
    std::vector<Id> terms;
    for (auto term : sum.args) {
        auto factors = factorsOf(term);
        next.clear();
        std::set_difference(factors.begin(),factors.end(),common.begin(),common.end(),std::back_inserter(next));
        terms.push_back(mul(next));
    }
    common.push_back(add(std::move(terms)));
    auto candidate = mul(std::move(common));
    return cost(candidate) < cost(expression) ? candidate : expression;
}

void Algebra::setKinematics(std::vector<Id> masses, std::map<std::vector<int>,Id> shells) {
    if (masses.empty() || masses.size() > INT_MAX)
        throw std::invalid_argument("specify one mass per external leg");
    for (auto m : masses) node(m);
    masses_ = std::move(masses);
    shells_ = std::move(shells);
}

Id Algebra::simplify(Id expression) {
    // A different cache for a numerator and a denominator prevents assumptions
    // made on a cut from leaking to the same subexpression off that cut.
    for (bool factoring : {false, true}) {
        std::unordered_set<Id> seen;
        for (int pass=0; pass<12 && seen.insert(expression).second; ++pass) {
            std::unordered_map<Id,Id> memo[2];
            std::function<Id(Id,bool)> visit = [&](Id id,bool internal) -> Id {
                auto& cache = memo[internal];
                if (auto it=cache.find(id); it!=cache.end()) return it->second;
                const auto& n = node(id);
                Id result=id;
                if (n.compound && n.head == "PropDen") {
                    // Preserve the propagator momentum. Its mass and width
                    // may themselves contain known external mass labels.
                    auto args = n.args;
                    for (size_t i=1; i<args.size(); ++i) args[i]=visit(args[i],false);
                    result=raw(n.head,std::move(args),true);
                } else if (n.compound && n.head != "ModelParameter") {
                    auto args = n.args;
                    // A symbolic or negative exponent cannot license internal shells
                    // in its base. Square roots and other fractional powers are opaque
                    // to multiplicative exponent algebra.
                    long exponent;
                    const bool denominator = n.head == "Power" && args.size() == 2 &&
                        (!machineInteger(node(args[1]),exponent) || exponent < 0);
                    for (size_t i=0; i<args.size(); ++i)
                        args[i] = visit(args[i], internal && !(denominator && i==0));
                    result = call(n.head,std::move(args));
                    result = reduceKinematics(*this,result,internal);
                    result = reduceSpinor(*this,result);
                    if (factoring) result = factorSum(result);
                }
                cache.emplace(id,result);
                return result;
            };
            auto next=visit(expression,true);
            if (next==expression) break;
            expression=next;
        }
    }
    return expression;
}

Id Algebra::parse(const std::string& text) {
    size_t pos=0;
    auto whitespace = [&]() {
        while (pos<text.size() && std::isspace(static_cast<unsigned char>(text[pos]))) ++pos;
    };
    std::function<Id(unsigned)> read = [&](unsigned depth) -> Id {
        if (depth>1024) throw std::invalid_argument("expression nesting too deep");
        whitespace();
        auto begin=pos;
        if (pos<text.size() && text[pos]=='"') {
            ++pos;
            bool closed=false;
            while (pos<text.size()) {
                char ch=text[pos++];
                if (ch=='\\' && pos<text.size()) ++pos;
                else if (ch=='"') { closed=true; break; }
            }
            if (!closed) throw std::invalid_argument("unterminated string");
        } else {
            while (pos<text.size() && text[pos]!='[' && text[pos]!=']' && text[pos]!=',' &&
                   !std::isspace(static_cast<unsigned char>(text[pos]))) ++pos;
        }
        if (begin==pos) throw std::invalid_argument("expected expression at "+std::to_string(pos));
        std::string head=text.substr(begin,pos-begin);
        whitespace();
        if (pos==text.size() || text[pos]!='[') return atom(std::move(head));
        ++pos;
        whitespace();
        std::vector<Id> args;
        if (pos<text.size() && text[pos]!=']') {
            for (;;) {
                args.push_back(read(depth+1));
                whitespace();
                if (pos==text.size() || text[pos]!=',') break;
                ++pos;
            }
        }
        if (pos==text.size() || text[pos++]!=']') throw std::invalid_argument("expected closing bracket");
        return raw(std::move(head),std::move(args),true);
    };
    Id id=read(0);
    whitespace();
    if (pos!=text.size()) throw std::invalid_argument("trailing input at "+std::to_string(pos));
    return id;
}

std::string Algebra::format(Id id) const {
    std::string text;
    std::function<void(Id)> append = [&](Id i) {
        const auto& n=node(i);
        text += n.head;
        if (!n.compound) return;
        text.push_back('[');
        for (size_t j=0; j<n.args.size(); ++j) {
            if (j) text.push_back(',');
            append(n.args[j]);
        }
        text.push_back(']');
    };
    append(id);
    return text;
}

} // namespace hepcat
