#include "validation_precision_parameters.hpp"
#include "model.hpp"
#include <cmath>
#include <functional>
#include <set>

namespace hepcat::validation {
namespace {
using precision::bits;
using precision::number;
using precision::scalar;

using lookup = std::function<number(Id)>;
number evaluate(const Algebra& a, Id id, const lookup& parameter) {
    mpq_class q;
    if (a.number(id, q))
        return {scalar(q, bits), scalar(0, bits)};
    const auto& n = a.node(id);
    const auto head = n.head.substr(n.head.rfind('`') + 1);
    if (head == "I")
        return {scalar(0, bits), scalar(1, bits)};
    if (head == "Pi") {
        // More decimal digits than the 160-bit replay mantissa.
        static const scalar pi("3.1415926535897932384626433832795028841971693993751058209749445923078164",
                               bits);
        return {pi, scalar(0, bits)};
    }
    if (head == "ModelParameter" || !n.compound)
        return parameter(id);
    auto arg = [&](std::size_t i) { return evaluate(a, n.args.at(i), parameter); };
    if (head == "Times" || head == "Plus") {
        number result(complex{head == "Times" ? 1.L : 0.L, 0});
        for (Id child : n.args) {
            auto v = evaluate(a, child, parameter);
            result = head == "Plus" ? result + v : result * v;
        }
        return result;
    }
    if (head == "Power" && n.args.size() == 2 && a.number(n.args[1], q) && q.get_num().fits_slong_p()) {
        if (q.get_den() == 1)
            return arg(0).power(q.get_num().get_si());
        if (q.get_den() == 2)
            return arg(0).square_root().power(q.get_num().get_si());
    }
    if (head == "Sqrt" && n.args.size() == 1)
        return arg(0).square_root();
    if (head == "Conjugate" && n.args.size() == 1)
        return arg(0).conj();
    if (head == "Abs" && n.args.size() == 1) {
        auto v = arg(0);
        return {scalar(sqrt(v.x * v.x + v.y * v.y), bits), scalar(0, bits)};
    }
    if (head == "Complex" && n.args.size() == 2) {
        auto im = arg(1);
        return arg(0) + number(scalar(-im.y, bits), im.x);
    }
    throw std::invalid_argument("unavailable 160-bit model function: " + a.format(id));
}
} // namespace

number precise_scalar(const Algebra& a, Id id, const wide_parameters& parameters) {
    return evaluate(a, id, [&](Id atom) {
        auto name = a.format(atom);
        auto it = parameters.find(name);
        if (it == parameters.end())
            throw std::invalid_argument("missing precision-replay input for " + name);
        return it->second;
    });
}
wide_parameters precise_model_parameters(const model& m, const parameters& overrides) {
    wide_parameters result;
    const auto& a = m.algebra();
    std::map<Id, const model_parameter*> definitions;
    for (auto entries : {&m.variables(), &m.functions()})
        for (const auto& item : *entries)
            definitions.emplace(item.symbol, &item);
    std::set<Id> pending;
    lookup resolve = [&](Id id) -> number {
        auto key = a.format(id);
        auto known = result.find(key);
        if (known != result.end())
            return known->second;
        const auto it = definitions.find(id);
        if (it == definitions.end())
            throw std::invalid_argument("undeclared 160-bit model parameter: " + key);
        auto user = overrides.find(key);
        if (user == overrides.end())
            user = overrides.find(it->second->name);
        if (user != overrides.end()) {
            if (!std::isfinite(user->second.real()) || !std::isfinite(user->second.imag()))
                throw std::invalid_argument("nonfinite override for " + key);
            return result.emplace(key, number(user->second)).first->second;
        }
        if (!pending.insert(id).second)
            throw std::invalid_argument("cyclic model definition: " + key);
        try {
            auto value = evaluate(a, it->second->definition, resolve);
            pending.erase(id);
            return result.emplace(key, value).first->second;
        } catch (...) {
            pending.erase(id);
            throw;
        }
    };
    for (const auto& [id, entry] : definitions) {
        // Unused CalcHEP library functions need no implementation. An active
        // unresolved input fails at lookup instead of seeding a replay with a
        // rounded lower-precision approximation.
        try {
            resolve(id);
        } catch (const std::invalid_argument&) {
        } catch (const std::domain_error&) {
        }
    }
    return result;
}
} // namespace hepcat::validation
