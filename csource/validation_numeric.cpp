#include "validation_color.hpp"
#include "validation_numeric.hpp"
#include "validation_precision.hpp"
#include "validation_precision_parameters.hpp"
#include "model.hpp"
#include "chart_scalar.hpp"
#include <algorithm>
#include <cmath>
#include <functional>
#include <limits>
#include <numeric>
#include <set>
#include <stdexcept>
#include <unordered_map>

namespace hepcat::validation {
namespace {
using chart_detail::hashText;
using chart_detail::mix;
const complex imaginary{0, 1};
const real pi = std::acos(-1.L);
std::string head(const Node &n) {
    return n.head.substr(n.head.find_last_of('`') + 1);
}

struct draw {
    std::uint64_t state;
    real uniform() {
        state += 0x9e3779b97f4a7c15ULL;
        return ((mix(state) >> 12) + 0.5L) / 4503599627370496.L;
    }
    spinor unit_spinor() {
        real r = uniform(), a = 2 * pi * uniform(), b = 2 * pi * uniform();
        return {std::polar(std::sqrt(r), a), std::polar(std::sqrt(1 - r), b)};
    }
    std::array<real, 3> direction() {
        real z = 2 * uniform() - 1, phi = 2 * pi * uniform(), t = std::sqrt((1 - z) * (1 + z));
        return {t * std::cos(phi), t * std::sin(phi), z};
    }
};
real dot(const four_vector &p, const four_vector &q) {
    return p[0] * q[0] - p[1] * q[1] - p[2] * q[2] - p[3] * q[3];
}
using mat = std::array<complex, 4>;
mat sigma(const four_vector &p) {
    return {p[0] + p[3], complex(p[1], -p[2]), complex(p[1], p[2]), p[0] - p[3]};
}
using wide_mat = std::array<precision::number, 4>;
wide_mat sigma(const precision::four &p) {
    using precision::number;
    using precision::scalar;
    return {number(scalar(p[0] + p[3], precision::bits)), number(p[1], scalar(-p[2], precision::bits)),
            number(p[1], p[2]), number(scalar(p[0] - p[3], precision::bits))};
}
template <class T> std::array<T, 4> adj(const std::array<T, 4> &m) {
    return {m[3], -m[1], -m[2], m[0]};
}
template <class T> std::array<T, 2> times(const std::array<T, 2> &v, const std::array<T, 4> &m) {
    return {v[0] * m[0] + v[1] * m[2], v[0] * m[1] + v[1] * m[3]};
}
template <class T> std::array<T, 2> raised(const std::array<T, 2> &v) {
    return {v[1], -v[0]};
}
bool finite(complex x) {
    return std::isfinite(x.real()) && std::isfinite(x.imag());
}
complex integer_power(complex z, long k) {
    if (k < 0) {
        if (z == complex{})
            throw std::domain_error("zero validation denominator");
        z = 1.L / z;
        if (k == std::numeric_limits<long>::min())
            throw std::invalid_argument("exponent out of range");
        k = -k;
    }
    complex v = 1;
    for (; k; k /= 2) {
        if (k % 2)
            v *= z;
        if (k > 1)
            z *= z;
    }
    return v;
}
complex scalar(const Algebra &a, Id id, const std::function<complex(Id)> &parameter) {
    const Node &n = a.node(id);
    mpq_class q;
    if (a.number(id, q)) {
        // Parse numerator/denominator separately. get_d() only preserves binary64.
        return std::stold(q.get_num().get_str()) / std::stold(q.get_den().get_str());
    }
    const auto h = head(n);
    if (n.head == "System`I" || n.head == "I")
        return imaginary;
    if (n.head == "System`Pi" || n.head == "Pi")
        return pi;
    if (h == "ModelParameter" || !n.compound)
        return parameter(id);
    auto val = [&](size_t i) { return scalar(a, n.args.at(i), parameter); };
    if (h == "Times" || h == "Plus") {
        complex out = h == "Times" ? 1 : 0;
        for (Id v : n.args) {
            auto x = scalar(a, v, parameter);
            if (h == "Times")
                out *= x;
            else
                out += x;
        }
        return out;
    }
    if (h == "Power" && n.args.size() == 2) {
        if (a.number(n.args[1], q) && q.get_den() == 1 && q.get_num().fits_slong_p())
            return integer_power(val(0), q.get_num().get_si());
        return std::pow(val(0), val(1));
    }
    if (h == "Complex" && n.args.size() == 2)
        return val(0) + imaginary * val(1);
    if (n.args.size() == 1) {
        if (h == "Sqrt")
            return std::sqrt(val(0));
        if (h == "Cos")
            return std::cos(val(0));
        if (h == "Sin")
            return std::sin(val(0));
        if (h == "Tan")
            return std::tan(val(0));
        if (h == "ArcSin")
            return std::asin(val(0));
        if (h == "ArcCos")
            return std::acos(val(0));
        if (h == "ArcTan")
            return std::atan(val(0));
        if (h == "Exp")
            return std::exp(val(0));
        if (h == "Log")
            return std::log(val(0));
        if (h == "Abs")
            return std::abs(val(0));
        if (h == "Conjugate")
            return std::conj(val(0));
    }
    throw std::invalid_argument("unsupported numerical scalar: " + a.format(id));
}
} // namespace

complex evaluate_scalar(const Algebra &a, Id id, const parameters &pars) {
    auto value = scalar(a, id, [&](Id key) {
        auto name = a.format(key);
        auto it = pars.find(name);
        if (it == pars.end())
            throw std::invalid_argument("no numerical value for " + name);
        return it->second;
    });
    if (!finite(value))
        throw std::invalid_argument("nonfinite scalar " + a.format(id));
    return value;
}
parameters model_parameters(const model &m, const parameters &overrides) {
    parameters result;
    const auto &a = m.algebra();
    std::map<Id, const model_parameter *> definitions;
    for (const auto *rows : {&m.variables(), &m.functions()})
        for (const auto &entry : *rows)
            definitions[entry.symbol] = &entry;
    std::set<Id> pending;
    std::function<complex(Id)> resolve = [&](Id id) -> complex {
        auto key = a.format(id);
        if (auto it = result.find(key); it != result.end())
            return it->second;
        auto it = definitions.find(id);
        if (it == definitions.end())
            throw std::invalid_argument("undefined model parameter " + key);
        const auto &entry = *it->second;
        auto ov = overrides.find(key);
        if (ov == overrides.end())
            ov = overrides.find(entry.name);
        if (ov != overrides.end()) {
            if (!finite(ov->second))
                throw std::invalid_argument("nonfinite override " + key);
            return result[key] = ov->second;
        }
        if (!pending.insert(id).second)
            throw std::invalid_argument("cyclic model parameter " + key);
        try {
            auto value = scalar(a, entry.definition, resolve);
            if (!finite(value))
                throw std::invalid_argument("nonfinite model parameter " + key);
            pending.erase(id);
            return result[key] = value;
        } catch (...) {
            pending.erase(id);
            throw;
        }
    };
    // External CalcHEP library functions can be present in unused definitions.
    // Only resolved values are exported. An expression using one of the others
    // produces an explicit missing-parameter error at evaluator construction.
    for (const auto &entry : definitions) {
        try {
            resolve(entry.first);
        } catch (const std::invalid_argument &) {
        }
    }
    return result;
}

spinor point::reference(std::uint64_t label, bool angle) const {
    draw gen{mix(reference_seed ^ mix(label))};
    auto s = gen.unit_spinor();
    if (!angle)
        for (auto &v : s)
            v = std::conj(v);
    return s;
}
void point::change_spins(std::uint64_t seed) {
    spin_seed = seed;
    draw gen{seed};
    for (size_t i = 0; i < masses.size(); ++i) {
        if (masses[i] == 0)
            continue;
        auto w = gen.unit_spinor();
        for (size_t j = 0; j < 2; ++j) {
            angles[i][j] = angle_basis[i][0][j] * w[0] + angle_basis[i][1][j] * w[1];
            squares[i][j] = square_basis[i][0][j] * w[0] + square_basis[i][1][j] * w[1];
        }
    }
}
point sample_point(const std::vector<real> &masses, std::uint64_t seed) {
    size_t n = masses.size();
    if (n < 4)
        throw std::invalid_argument("physical two-beam phase space requires at least four legs");
    for (real m : masses)
        if (!std::isfinite(m) || m < 0)
            throw std::invalid_argument("invalid external mass");
    draw gen{seed};
    point pt;
    pt.phase_seed = seed;
    pt.masses = masses;
    pt.momenta.resize(n);
    pt.angles.resize(n);
    pt.squares.resize(n);
    pt.angle_basis.resize(n);
    pt.square_basis.resize(n);
    const real threshold =
        std::max(masses[0] + masses[1], std::accumulate(masses.begin() + 2, masses.end(), 0.L));
    const real energy =
        threshold + std::max(threshold, 1.L) * std::exp((3 * gen.uniform() - 1) * std::log(3.L));
    real beam_p = std::sqrt((energy - masses[0] - masses[1]) * (energy + masses[0] + masses[1]) *
                            (energy - masses[0] + masses[1]) * (energy + masses[0] - masses[1])) /
                  (2 * energy);
    auto beam = gen.direction();
    for (size_t i = 0; i < 2; ++i) {
        pt.momenta[i][0] = std::hypot(beam_p, masses[i]);
        for (size_t j = 0; j < 3; ++j)
            pt.momenta[i][j + 1] = (i ? -1 : 1) * beam_p * beam[j];
    }
    // RAMBO massless directions in their common rest frame. A common positive
    // scale then enforces the requested energies of massive final particles.
    // These unweighted validation draws are not integration weights.
    if (n == 4) {
        const real p = std::sqrt((energy - masses[2] - masses[3]) * (energy + masses[2] + masses[3]) *
                                 (energy - masses[2] + masses[3]) * (energy + masses[2] - masses[3])) /
                       (2 * energy);
        auto axis = gen.direction();
        for (size_t i = 2; i < 4; ++i) {
            pt.momenta[i][0] = -std::hypot(p, masses[i]);
            for (size_t j = 0; j < 3; ++j)
                pt.momenta[i][j + 1] = (i == 2 ? -1 : 1) * p * axis[j];
        }
    } else {
        four_vector total{};
        for (size_t i = 2; i < n; ++i) {
            auto axis = gen.direction();
            auto &q = pt.momenta[i];
            q[0] = -std::log(gen.uniform() * gen.uniform());
            for (size_t j = 0; j < 3; ++j)
                q[j + 1] = q[0] * axis[j];
            for (size_t j = 0; j < 4; ++j)
                total[j] += q[j];
        }
        real cm = std::sqrt(dot(total, total));
        if (!(cm > 0))
            throw std::domain_error("singular phase-space boost");
        for (size_t i = 2; i < n; ++i) {
            auto &q = pt.momenta[i];
            real rest = dot(q, total) / cm;
            for (size_t j = 1; j < 4; ++j)
                q[j] = (q[j] - total[j] * (q[0] + rest) / (total[0] + cm)) * energy / cm;
            q[0] = rest * energy / cm;
        }
        // Enforce spatial closure on the hardest leg, where the correction has
        // the smallest relative effect. Use its new norm in the energy equation.
        size_t pivot = 2;
        for (size_t i = 3; i < n; ++i)
            if (pt.momenta[i][0] > pt.momenta[pivot][0])
                pivot = i;
        for (size_t j = 1; j < 4; ++j) {
            real sum = 0;
            for (size_t i = 2; i < n; ++i)
                if (i != pivot)
                    sum += pt.momenta[i][j];
            pt.momenta[pivot][j] = -sum;
        }
        real spatial_energy = 0;
        for (size_t i = 2; i < n; ++i) {
            auto &q = pt.momenta[i];
            q[0] = std::hypot(q[1], q[2], q[3]);
            spatial_energy += q[0];
        }
        real lo = 0, hi = energy / spatial_energy;
        if (std::any_of(masses.begin() + 2, masses.end(), [](real x) { return x > 0; }))
            for (int it = 0; it < std::numeric_limits<real>::digits + 2; ++it) {
                real mid = (lo + hi) / 2, sum = 0;
                for (size_t i = 2; i < n; ++i)
                    sum += std::hypot(mid * pt.momenta[i][0], masses[i]);
                if (sum < energy)
                    lo = mid;
                else
                    hi = mid;
            }
        for (size_t i = 2; i < n; ++i) {
            auto &q = pt.momenta[i];
            q[0] = -std::hypot(hi * q[0], masses[i]);
            for (size_t j = 1; j < 4; ++j)
                q[j] *= -hi;
        }
    }
    for (size_t i = 0; i < n; ++i) {
        const auto &p = pt.momenta[i];
        const real sign = p[0] < 0 ? -1 : 1, px = sign * p[1], py = sign * p[2], pz = sign * p[3];
        const real k = std::hypot(px, py, pz), e = std::abs(p[0]);
        // Use the nonsingular hemisphere of the eigenspinor atlas.
        spinor u = {1, 0};
        if (k && pz >= 0) {
            real s = std::sqrt(2 * k * (k + pz));
            u = {(k + pz) / s, complex(px, py) / s};
        } else if (k) {
            real s = std::sqrt(2 * k * (k - pz));
            u = {complex(px, -py) / s, (k - pz) / s};
        }
        spinor v = {-std::conj(u[1]), std::conj(u[0])};
        real big = std::sqrt(e + k), small = masses[i] / big;
        auto &ab = pt.angle_basis[i];
        auto &sb = pt.square_basis[i];
        for (size_t j = 0; j < 2; ++j) {
            ab[0][j] = big * u[j];
            ab[1][j] = small * v[j];
            // epsilon.adj(p).angle/m evaluated without subtracting E-|p|.
            sb[0][j] = -sign * small * std::conj(v[j]);
            sb[1][j] = sign * big * std::conj(u[j]);
            pt.angles[i][j] = ab[0][j];
            pt.squares[i][j] = sign * big * std::conj(u[j]);
        }
    }
    // Each future-directed momentum is a sum of two rank-one outer products.
    // The determinant of their sum gives 2 p.q as a sum of absolute squares.
    // This remains accurate for almost parallel momenta, including p_i^2=m_i^2.
    pt.scalar_products.resize(n * n);
    for (size_t i = 0; i < n; ++i) {
        pt.scalar_products[i * n + i] = masses[i] * masses[i];
        for (size_t j = 0; j < i; ++j) {
            real value = 0;
            for (const auto &u : pt.angle_basis[i])
                for (const auto &v : pt.angle_basis[j])
                    value += std::norm(u[0] * v[1] - u[1] * v[0]);
            value *= ((i < 2) == (j < 2) ? 0.5L : -0.5L);
            pt.scalar_products[i * n + j] = pt.scalar_products[j * n + i] = value;
        }
    }
    pt.change_spins(mix(seed + 1));
    pt.change_references(mix(seed + 2));
    pt.change_colors(mix(seed + 3));
    return pt;
}
namespace {
struct momentum_sum {
    std::vector<std::pair<size_t, real>> terms;
    std::vector<precision::scalar> replay_weights;
    std::string replay_error;
    const precision::scalar &weight(size_t index) const {
        if (!replay_error.empty())
            throw std::invalid_argument(replay_error);
        return replay_weights.at(index);
    }
    four_vector operator()(const point &p) const {
        four_vector v{};
        for (auto [i, w] : terms)
            for (size_t j = 0; j < 4; ++j)
                v[j] += w * p.momenta.at(i)[j];
        return v;
    }
    precision::four operator()(const precision::phase_point &p) const {
        precision::four v;
        for (auto &x : v)
            x.set_prec(precision::bits);
        for (size_t k = 0; k < terms.size(); ++k) {
            auto i = terms[k].first;
            const auto &scale = weight(k);
            for (size_t j = 0; j < 4; ++j)
                v[j] += scale * p.momenta.at(i)[j];
        }
        return v;
    }
};
// Apply a momentum as the sum of its two Weyl outer products. Lightlike
// momenta have a single nonzero column and nearly lightlike ones retain
// their mass-suppressed second column.
spinor apply_momentum(const point &p, const spinor &row, const momentum_sum &momentum, bool barred) {
    spinor result{};
    for (auto [index, weight] : momentum.terms) {
        if (p.momenta.at(index)[0] < 0)
            weight = -weight;
        for (auto basis : p.angle_basis.at(index)) {
            if (barred)
                basis = raised(spinor{std::conj(basis[0]), std::conj(basis[1])});
            auto inner = weight * (row[0] * basis[0] + row[1] * basis[1]);
            for (size_t j = 0; j < 2; ++j)
                result[j] += inner * std::conj(basis[j]);
        }
    }
    return result;
}
precision::vector apply_momentum(const precision::phase_point &p, const precision::vector &row,
                                 const momentum_sum &momentum, bool barred) {
    auto matrix = sigma(momentum(p));
    return times(row, barred ? adj(matrix) : matrix);
}
precision::number bilinear(const precision::phase_point &p, const momentum_sum &a, const momentum_sum &b) {
    precision::scalar sum(0, precision::bits);
    for (size_t u = 0; u < a.terms.size(); ++u)
        for (size_t v = 0; v < b.terms.size(); ++v) {
            auto i = a.terms[u].first, j = b.terms[v].first;
            if (i >= p.masses.size() || j >= p.masses.size())
                throw std::invalid_argument("invalid replay momentum");
            sum += a.weight(u) * b.weight(v) * p.scalar_products.at(i * p.masses.size() + j);
        }
    return precision::number(sum);
}
precision::number invariant(const precision::phase_point &p, const momentum_sum &a,
                            const precision::number &mass = {}) {
    return bilinear(p, a, a) - mass * mass;
}
precision::number integer_power(const precision::number &z, long k) {
    return z.power(k);
}
real pair_dot(const point &p, size_t i, size_t j) {
    if (i >= p.masses.size() || j >= p.masses.size())
        throw std::invalid_argument("momentum leg outside phase-space point");
    return p.scalar_products.at(i * p.masses.size() + j);
}
real bilinear(const point &p, const momentum_sum &a, const momentum_sum &b) {
    real sum = 0;
    for (auto [i, wi] : a.terms)
        for (auto [j, wj] : b.terms)
            sum += wi * wj * pair_dot(p, i, j);
    return sum;
}
complex invariant(const point &p, const momentum_sum &a, complex mass = {}) {
    // Subtract the propagator mass from the external self-terms first:
    // the cancellation in (p_i+p_j)^2-m_i^2 is exact even in a soft limit.
    complex result = -mass * mass;
    for (auto [i, wi] : a.terms) {
        real m = wi * p.masses.at(i);
        result += m * m;
    }
    for (size_t k = 0; k < a.terms.size(); ++k) {
        auto [i, wi] = a.terms[k];
        for (size_t l = 0; l < k; ++l) {
            auto [j, wj] = a.terms[l];
            result += 2 * wi * wj * pair_dot(p, i, j);
        }
    }
    return result;
}
size_t leg(const Algebra &a, Id id) {
    int k = a.leg(id);
    if (k <= 0)
        throw std::invalid_argument("invalid external leg " + a.format(id));
    return static_cast<size_t>(k - 1);
}
void append_legs(momentum_sum &m, const Algebra &a, Id id, real scale,
                 const precision::scalar &exact_scale = precision::scalar(1, precision::bits)) {
    const auto &n = a.node(id);
    auto append = [&](Id x) {
        m.terms.emplace_back(leg(a, x), scale);
        m.replay_weights.push_back(exact_scale);
    };
    if (n.head == "Multiparticle")
        for (Id x : n.args)
            append(x);
    else
        append(id);
}
bool momentum_valued(const Algebra &a, Id id) {
    const auto &n = a.node(id);
    return n.head == "Mom" ||
           ((n.head == "Plus" || n.head == "Times") &&
            std::any_of(n.args.begin(), n.args.end(), [&](Id child) { return momentum_valued(a, child); }));
}
void append_momentum(momentum_sum &m, const Algebra &a, Id id, real w, const parameters &pars,
                     precision::scalar exact_weight, const wide_parameters &exact) {
    const auto &n = a.node(id);
    if (n.head == "Mom" && n.args.size() == 1)
        return append_legs(m, a, n.args[0], w, exact_weight);
    if (n.head == "Plus") {
        for (auto x : n.args)
            append_momentum(m, a, x, w, pars, exact_weight, exact);
        return;
    }
    if (n.head == "Times") {
        std::optional<Id> mom;
        for (auto x : n.args) {
            if (momentum_valued(a, x)) {
                if (mom)
                    throw std::invalid_argument("nonlinear momentum insertion");
                mom = x;
            } else {
                auto v = evaluate_scalar(a, x, pars);
                if (v.imag() != 0)
                    throw std::invalid_argument("complex momentum coefficient");
                w *= v.real();
                try {
                    auto precise = precise_scalar(a, x, exact);
                    if (precise.y != 0)
                        throw std::invalid_argument("complex precision replay momentum weight");
                    exact_weight *= precise.x;
                } catch (const std::logic_error &e) {
                    m.replay_error = e.what();
                }
            }
        }
        if (mom)
            return append_momentum(m, a, *mom, w, pars, exact_weight, exact);
    }
    throw std::invalid_argument("unsupported momentum insertion " + a.format(id));
}
struct endpoint {
    enum type { helicity, massive, reference } kind;
    bool angle;
    size_t label;
    int column = -1, sign = 1;
    template <class Chart> auto operator()(const Chart &p) const {
        if (kind == reference)
            return p.reference(label, angle);
        if ((kind == helicity) != (p.masses.at(label) == 0))
            throw std::invalid_argument("endpoint spinor inconsistent with external mass");
        if (column < 0)
            return (angle ? p.angles : p.squares).at(label);
        auto v = (angle ? p.angle_basis : p.square_basis).at(label)[column];
        if (sign < 0) {
            v[0] = -v[0];
            v[1] = -v[1];
        }
        return v;
    }
};
endpoint compile_endpoint(const Algebra &a, Id id) {
    const auto &n = a.node(id);
    size_t sz = n.args.size(), t = sz == 5 ? 2 : 1;
    if (n.head != "Spinor" || sz < 3 || sz > 5)
        throw std::invalid_argument("invalid spinor " + a.format(id));
    auto type = a.node(n.args[0]).head, chirality = a.node(n.args[t]).head;
    if (chirality != "\"Angle\"" && chirality != "\"Square\"")
        throw std::invalid_argument("invalid spinor chirality");
    bool angle = chirality == "\"Angle\"";
    Id label = n.args[t + 1];
    if (type == "\"Xi\"" && sz <= 4) {
        int numeric = a.leg(label);
        return {endpoint::reference, angle, numeric > 0 ? std::uint64_t(numeric) : hashText(a.format(label))};
    }
    size_t i = leg(a, label);
    if (type == "\"Helicity\"" && sz <= 4)
        return {endpoint::helicity, angle, i};
    if (type == "\"Spin\"" && sz == 3)
        return {endpoint::massive, angle, i};
    if (type == "\"Spin\"" && sz == 5) {
        auto variance = a.node(n.args[1]).head;
        bool upper = variance == "\"Upper\"";
        if (!upper && variance != "\"Lower\"")
            throw std::invalid_argument("invalid spin index variance");
        auto &index = a.node(n.args[4]);
        if (upper && head(index) == "VectorSpinIndex" && index.args.size() == 2 &&
            leg(a, index.args[0]) == i && (a.leg(index.args[1]) == 1 || a.leg(index.args[1]) == 2))
            return {endpoint::massive, angle, i};
        mpq_class q;
        if (a.number(n.args[4], q) && (q == mpq_class(1, 2) || q == mpq_class(-1, 2))) {
            bool first = q < 0;
            int sign = !upper && !first ? -1 : 1;
            if (!upper)
                first = !first;
            return {endpoint::massive, angle, i, first ? 0 : 1, sign};
        }
    }
    throw std::invalid_argument("unsupported spinor endpoint " + a.format(id));
}
using registers = std::vector<complex>;
using operation = std::function<complex(const point &, const registers &)>;
using wide_complex = precision::number;
using wide_registers = std::vector<wide_complex>;
using wide_operation = std::function<wide_complex(const precision::phase_point &, const wide_registers &)>;
struct replay_step {
    enum type { leaf, add, multiply, power, half_power } kind = leaf;
    std::vector<size_t> inputs;
    long exponent = 0;
    wide_operation evaluate;
};
// Color contractions are kinematics independent. In particular two nonzero
// adjoint components can cancel exactly for a fixed external color choice.
class color_term {
    validation_detail::color_network network;
    std::vector<int> dimensions, degrees, legs;
    std::vector<unsigned> lookup;
    std::vector<std::pair<complex, wide_complex>> weights;

    wide_complex evaluate(const std::vector<int> &colors) const {
        auto v = network(colors);
        if (v.real() == 0 && v.imag() == 0)
            return {};
        mpz_class denominator = 1;
        for (auto i : legs)
            if (dimensions[i] == 8) {
                auto metric = validation_detail::color_basis::metric(colors[i]);
                for (int k = 0; k < degrees[i]; ++k)
                    denominator *= metric;
            }
        precision::scalar scale(sqrt(precision::scalar(denominator, precision::bits)), precision::bits);
        return {precision::scalar(precision::scalar(v.real(), precision::bits) / scale, precision::bits),
                precision::scalar(precision::scalar(v.imag(), precision::bits) / scale, precision::bits)};
    }

  public:
    color_term(const Algebra &a, const std::vector<Id> &factors, const std::vector<int> &reps)
        : network(a, reps, factors), dimensions(reps), degrees(network.external_degrees(reps.size())) {
        // Keep precomputation bounded at higher multiplicity. A large tensor
        // contracts exactly at the individual color assignments sampled instead.
        std::size_t count = 1;
        constexpr size_t limit = 8192;
        for (size_t i = 0; i < reps.size(); ++i)
            if (degrees[i]) {
                legs.push_back(i);
                dimensions[i] = std::abs(reps[i]);
                if (count <= limit)
                    count *= dimensions[i];
            }
        if (count > limit)
            return;
        std::map<std::pair<precision::scalar, precision::scalar>, unsigned> distinct;
        std::vector<int> colors(reps.size());
        lookup.reserve(count);
        for (size_t index = 0; index < count; ++index) {
            size_t code = index;
            for (auto i : legs) {
                colors[i] = code % dimensions[i];
                code /= dimensions[i];
            }
            auto value = evaluate(colors);
            auto [it, added] = distinct.emplace(std::make_pair(value.x, value.y), weights.size());
            if (added)
                weights.emplace_back(value.narrow(), std::move(value));
            lookup.push_back(it->second);
        }
    }
    template <class Chart> auto operator()(const Chart &p) const {
        using Number = typename std::decay_t<decltype(p.angles)>::value_type::value_type;
        if (lookup.empty()) {
            std::vector<int> colors(dimensions.size());
            for (auto i : legs)
                colors[i] = mix(p.color_seed ^ mix(i + 1)) % dimensions[i];
            auto value = evaluate(colors);
            if constexpr (std::is_same_v<Number, complex>)
                return value.narrow();
            else
                return value;
        }
        size_t index = 0;
        for (auto it = legs.rbegin(); it != legs.rend(); ++it)
            index = index * dimensions[*it] + mix(p.color_seed ^ mix(*it + 1)) % dimensions[*it];
        const auto &pair = weights[lookup[index]];
        if constexpr (std::is_same_v<Number, complex>)
            return pair.first;
        else
            return pair.second;
    }
};
class compiler {
    const Algebra &a;
    const parameters &pars;
    const wide_parameters &exact;
    const std::vector<int> &reps;
    std::vector<operation> &ops;
    std::vector<replay_step> &replay;
    validation_detail::color_polynomial colors;
    std::map<Id, size_t> scalar_memo;
    size_t store(operation op, replay_step step = {}) {
        replay.push_back(std::move(step));
        ops.push_back(std::move(op));
        return ops.size() - 1;
    }
    template <class Op> size_t typed(Op op) {
        replay_step step;
        step.evaluate = [op](const precision::phase_point &p, const wide_registers &r) {
            return wide_complex(op(p, r));
        };
        return store([op](const point &p, const registers &r) { return complex(op(p, r)); }, std::move(step));
    }
    size_t arith(std::vector<size_t> children, bool add) {
        if (children.size() == 1)
            return children[0];
        replay_step step{add ? replay_step::add : replay_step::multiply, children};
        return store(
            [v = std::move(children), add](const point &, const registers &r) {
                complex out = add ? 0 : 1;
                for (auto j : v) {
                    if (add)
                        out += r[j];
                    else
                        out *= r[j];
                }
                return out;
            },
            std::move(step));
    }
    size_t numeric(Id id) {
        if (auto it = scalar_memo.find(id); it != scalar_memo.end())
            return it->second;
        return scalar_memo[id] = primitive(id);
    }
    size_t primitive(Id id) {
        const auto &n = a.node(id);
        auto h = head(n);
        auto mom = [&](Id x) {
            momentum_sum s;
            append_momentum(s, a, x, 1, pars, precision::scalar(1, precision::bits), exact);
            return s;
        };
        if (h == "Times" || h == "Plus") {
            std::vector<size_t> v;
            for (auto x : n.args)
                v.push_back(numeric(x));
            return arith(std::move(v), h == "Plus");
        }
        if (h == "Power" && n.args.size() == 2) {
            mpq_class q;
            if (!a.number(n.args[1], q) || !q.get_num().fits_slong_p())
                throw std::invalid_argument("expected a rational validation exponent");
            if (a.node(n.args[0]).head == "Mom") {
                if (q.get_den() != 1 || (q.get_num().get_si() % 2))
                    throw std::invalid_argument("odd power of momentum");
                return typed([s = mom(n.args[0]), k = q.get_num().get_si() / 2](const auto &p, const auto &) {
                    return integer_power(invariant(p, s), k);
                });
            }
            auto b = numeric(n.args[0]);
            if (q.get_den() == 1)
                return store([b, k = q.get_num().get_si()](
                                 const point &, const registers &r) { return integer_power(r[b], k); },
                             {replay_step::power, {b}, q.get_num().get_si()});
            replay_step step;
            if (q.get_den() == 2)
                step = {replay_step::half_power, {b}, q.get_num().get_si()};
            else
                step.evaluate = [](const precision::phase_point &, const wide_registers &) -> wide_complex {
                    throw std::invalid_argument("unsupported non-half-integer precision replay power");
                };
            auto exponent = evaluate_scalar(a, n.args[1], pars);
            return store(
                [b, exponent](const point &, const registers &r) { return std::pow(r[b], exponent); },
                std::move(step));
        }
        if (h == "SpinorChain") {
            if (n.args.size() < 2)
                throw std::invalid_argument("empty spinor chain");
            auto l = compile_endpoint(a, n.args.front()), r = compile_endpoint(a, n.args.back());
            std::vector<momentum_sum> v;
            for (size_t j = 1; j + 1 < n.args.size(); ++j)
                v.push_back(mom(n.args[j]));
            if ((l.angle == r.angle) != (v.size() % 2 == 0))
                throw std::invalid_argument("inconsistent spinor-chain chirality");
            return typed([l, r, v = std::move(v)](const auto &p, const auto &) {
                auto x = l(p), y = r(p);
                if (l.angle)
                    x = raised(x);
                if (!r.angle)
                    y = raised(y);
                for (size_t i = 0; i < v.size(); ++i) {
                    x = apply_momentum(p, x, v[i], (i % 2 == 0) != l.angle);
                }
                return x[0] * y[0] + x[1] * y[1];
            });
        }
        if (h == "SpinorTrace") {
            if (n.args.size() % 2)
                throw std::invalid_argument("odd spinor trace");
            std::vector<momentum_sum> ms;
            for (auto x : n.args)
                ms.push_back(mom(x));
            return typed([ms = std::move(ms)](const auto &p, const auto &) {
                using row = typename std::decay_t<decltype(p.angles)>::value_type;
                using value = typename row::value_type;
                row r{value(complex(1, 0)), value{}}, s{value{}, value(complex(1, 0))};
                for (size_t i = 0; i < ms.size(); ++i) {
                    auto m = sigma(ms[i](p));
                    if (i % 2)
                        m = adj(m);
                    r = times(r, m);
                    s = times(s, m);
                }
                return r[0] + s[1];
            });
        }
        if (h == "MomProd" && n.args.size() == 2) {
            momentum_sum l, r;
            append_legs(l, a, n.args[0], 1);
            append_legs(r, a, n.args[1], 1);
            return typed([l, r](const auto &p, const auto &) { return bilinear(p, l, r); });
        }
        if (h == "Mandelstahm" && n.args.size() >= 2) {
            momentum_sum sum;
            for (auto x : n.args)
                append_legs(sum, a, x, 1);
            return typed([sum](const auto &p, const auto &) { return invariant(p, sum); });
        }
        if (h == "Mass" && n.args.size() == 1)
            return typed([i = leg(a, n.args[0])](const auto &p, const auto &) { return p.masses.at(i); });
        if (h == "PropDen" && n.args.size() == 2) {
            auto mass = numeric(n.args[1]);
            return typed([m = mom(n.args[0]), mass](const auto &p, const auto &r) {
                return invariant(p, m, r[mass]);
            });
        }
        complex constant = evaluate_scalar(a, id, pars);
        replay_step step;
        try {
            auto resolved = precise_scalar(a, id, exact);
            step.evaluate = [resolved](const precision::phase_point &, const wide_registers &) {
                return resolved;
            };
        } catch (const std::logic_error &e) {
            step.evaluate = [why = std::string(e.what())](const precision::phase_point &,
                                                          const wide_registers &) -> wide_complex {
                throw std::invalid_argument(why);
            };
        }
        return store([constant](const point &, const registers &) { return constant; }, std::move(step));
    }

  public:
    compiler(const Algebra &arena, const parameters &par, const std::vector<int> &rep,
             std::vector<operation> &code, std::vector<replay_step> &replay_steps,
             const wide_parameters &exact_values)
        : a(arena), pars(par), exact(exact_values), reps(rep), ops(code), replay(replay_steps),
          colors(arena) {}
    size_t compile(Id id) {
        if (!colors.contains_color(id))
            return numeric(id);
        auto terms = colors.split(id);
        std::map<std::vector<Id>, std::vector<size_t>> grouped;
        for (auto &t : terms) {
            std::sort(t.tensors.begin(), t.tensors.end());
            std::vector<std::size_t> coefficients;
            for (Id child : t.coefficients)
                coefficients.push_back(numeric(child));
            grouped[t.tensors].push_back(arith(std::move(coefficients), false));
        }
        std::vector<size_t> total;
        for (auto &[factors, coeffs] : grouped) {
            size_t coefficient = arith(std::move(coeffs), true);
            auto color = std::make_shared<color_term>(a, factors, reps);
            auto tensor =
                typed([color = std::move(color)](const auto &p, const auto &) { return (*color)(p); });
            total.push_back(arith({tensor, coefficient}, false));
        }
        return arith(std::move(total), true);
    }
};
} // namespace

struct expression_evaluator::program {
    std::vector<operation> ops;
    std::vector<replay_step> replay;
    size_t result;
};
expression_evaluator::expression_evaluator(const Algebra &a, Id id, const parameters &pars,
                                           const std::vector<int> &reps, const wide_parameters *exact) {
    auto code = std::make_shared<program>();
    wide_parameters promoted;
    if (!exact)
        for (const auto &[name, value] : pars)
            promoted.emplace(name, wide_complex(value));
    code->result = compiler(a, pars, reps, code->ops, code->replay, exact ? *exact : promoted).compile(id);
    code_ = std::move(code);
}
complex expression_evaluator::operator()(const point &p) const {
    registers out;
    out.reserve(code_->ops.size());
    for (const auto &op : code_->ops) {
        complex z = op(p, out);
        if (!finite(z))
            throw std::domain_error("nonfinite amplitude at validation point");
        out.push_back(z);
    }
    return out[code_->result];
}
complex expression_evaluator::precise(const point &p) const {
    return precise(p, precision::reconstruct(p)).narrow();
}
precision::number expression_evaluator::precise(const point &p, const precision::phase_point &chart) const {
    if (p.momenta.size() != chart.momenta.size() || p.color_seed != chart.color_seed ||
        p.reference_seed != chart.reference_seed)
        throw std::invalid_argument("mismatched precision replay chart");
    std::vector<wide_complex> accumulators;
    accumulators.reserve(code_->ops.size());
    for (size_t i = 0; i < code_->ops.size(); ++i) {
        const auto &step = code_->replay[i];
        wide_complex z;
        switch (step.kind) {
        case replay_step::leaf:
            if (!step.evaluate)
                throw std::logic_error("unbound precision validation primitive");
            z = step.evaluate(chart, accumulators);
            break;
        case replay_step::add:
            for (size_t j : step.inputs) {
                z.x += accumulators[j].x;
                z.y += accumulators[j].y;
            }
            break;
        case replay_step::multiply:
            z.x = 1;
            for (size_t j : step.inputs)
                z = z * accumulators[j];
            break;
        case replay_step::power:
            z = accumulators[step.inputs.front()].power(step.exponent);
            break;
        case replay_step::half_power:
            z = accumulators[step.inputs.front()].square_root().power(step.exponent);
            break;
        }
        accumulators.push_back(std::move(z));
    }
    return accumulators[code_->result];
}
} // namespace hepcat::validation
