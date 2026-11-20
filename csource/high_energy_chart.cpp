#include "high_energy_chart.hpp"
#include "chart_scalar.hpp"
#include "regularity_field.hpp"
#include <algorithm>
#include <limits>
#include <functional>
#include <random>
#include <numeric>

namespace hepcat {
namespace {
using namespace chart_detail;
using Vector = std::array<Q, 2>;
struct Matrix {
    Q a, b, c, d;
};
Q wedge(const Vector& a, const Vector& b) { return a[0] * b[1] - a[1] * b[0]; }
Matrix outer(const Vector& a, const Vector& b) { return {a[0] * b[0], a[0] * b[1], a[1] * b[0], a[1] * b[1]}; }
Matrix operator+(const Matrix& a, const Matrix& b) { return {a.a + b.a, a.b + b.b, a.c + b.c, a.d + b.d}; }
Vector add(const Vector& a, const Vector& b) { return {a[0] + b[0], a[1] + b[1]}; }
Vector scale(const Vector& a, const Q& b) { return {a[0] * b, a[1] * b}; }
struct Generator {
    std::mt19937_64 gen;
    Q integer() {
        const long magnitude = 1 + gen() % 13;
        return Q(gen() % 2 ? magnitude : -magnitude);
    }
    Vector vector() { return {integer(), integer()}; }
};

// u = sqrt(E). Choose independent null directions for the two Laurent
// coefficients of each P. Solve for the last two leading right spinors,
// then use four free shifts of the subleading left spinors to impose conservation.
struct Frame {
    Vector l, r, b, h;
    Q x, mass, spin;
};
std::vector<Frame> make_chart(const std::vector<Q>& masses, std::size_t sample) {
    const auto n = masses.size();
    if (n < 4) throw residue_unsupported("fixed-angle charts require at least four legs");
    Generator draw{std::mt19937_64(mix(sample + 0x38bcf341ULL))};
    std::vector<Frame> f(n);
    for (std::size_t i = 0; i < n; ++i) {
        auto& k = f[i];
        k.l = draw.vector();
        k.r = draw.vector();
        k.h = draw.vector();
        if (masses[i] < 0) throw residue_unsupported("negative external mass");
        k.mass = masses[i];
        Q numerator = draw.integer();
        k.spin = numerator / draw.integer();
    }
    Matrix p{};
    for (std::size_t i = 0; i + 2 < n; ++i)
        p = p + outer(f[i].l, f[i].r);
    auto& a = f[n - 2];
    auto& b = f[n - 1];
    Q pivot = wedge(a.l, b.l);
    if (!pivot) throw residue_singular("dependent leading spinors");
    a.r = {(-p.a * b.l[1] + p.c * b.l[0]) / pivot, (-p.b * b.l[1] + p.d * b.l[0]) / pivot};
    b.r = {(p.a * a.l[1] - p.c * a.l[0]) / pivot, (p.b * a.l[1] - p.d * a.l[0]) / pivot};
    for (std::size_t i = 0; i < n; ++i)
        for (std::size_t j = 0; j < i; ++j)
            if (!wedge(f[i].l, f[j].l) || !wedge(f[i].r, f[j].r))
                throw residue_singular("collinear leading directions");
    Matrix small{};
    std::array<std::array<Q, 5>, 4> equations{};
    for (std::size_t i = 0; i < n; ++i) {
        auto& k = f[i];
        Q d = wedge(k.r, k.h);
        if (!d) throw residue_singular("singular subleading direction");
        // det(l,b)*det(r,h)=m² for any b=b0+x*l.
        k.b = {0, k.mass * k.mass / (k.l[0] * d)};
        k.x = i < 4 ? Q(0) : draw.integer();
        k.b = add(k.b, scale(k.l, k.x));
        small = small + outer(k.b, k.h);
        if (i < 4) {
            Matrix m = outer(k.l, k.h);
            equations[0][i] = m.a;
            equations[1][i] = m.b;
            equations[2][i] = m.c;
            equations[3][i] = m.d;
        }
    }
    equations[0][4] = -small.a;
    equations[1][4] = -small.b;
    equations[2][4] = -small.c;
    equations[3][4] = -small.d;
    for (std::size_t col = 0; col < 4; ++col) {
        auto row = col;
        while (row < 4 && !equations[row][col])
            ++row;
        if (row == 4) throw residue_singular("degenerate subleading conservation system");
        std::swap(equations[col], equations[row]);
        Q d = equations[col][col];
        for (std::size_t j = col; j <= 4; ++j)
            equations[col][j] /= d;
        for (std::size_t i = 0; i < 4; ++i)
            if (i != col) {
                Q multiple = equations[i][col];
                for (std::size_t j = col; j <= 4; ++j)
                    equations[i][j] -= multiple * equations[col][j];
            }
    }
    for (std::size_t i = 0; i < 4; ++i) {
        f[i].x = equations[i][4];
        f[i].b = add(f[i].b, scale(f[i].l, f[i].x));
    }
    return f;
}

// With Q the total momentum of a pair, L=1+zN and R=1-z Q^-1 N Q
// satisfy L Q R = Q when N^2=0. Both have unit determinant. Applying
// L and R^T to the two spin frames gives a polynomial, on-shell curve
// through generic kinematics for massive and massless legs alike.
struct shear {
    Matrix angle{}, square{};
};
Matrix product(Matrix m, Matrix n) {
    return {m.a * n.a + m.b * n.c, m.a * n.b + m.b * n.d, m.c * n.a + m.d * n.c, m.c * n.b + m.d * n.d};
}
std::vector<shear> pair_shears(const std::vector<Frame>& frame, std::size_t sample, std::size_t pairing) {
    std::vector<shear> result(frame.size());
    std::vector<std::size_t> legs(frame.size() + frame.size() % 2);
    std::iota(legs.begin(), legs.end(), 0);
    auto turn = pairing % (legs.size() - 1);
    std::rotate(legs.begin() + 1, legs.begin() + 1 + turn, legs.end());
    Generator draw{std::mt19937_64(mix(sample ^ 0x399c71aULL))};
    auto momentum = [&](std::size_t i) {
        return outer(frame[i].l, frame[i].r) + outer(frame[i].b, frame[i].h);
    };
    for (std::size_t i = 0; i < legs.size() / 2; ++i) {
        auto left = legs[i], right = legs[legs.size() - 1 - i];
        if (left == frame.size() || right == frame.size())
            continue;
        auto q = momentum(left) + momentum(right);
        Q det = q.a * q.d - q.b * q.c;
        if (!det)
            throw residue_singular("null total momentum in rotation chart");
        auto v = draw.vector();
        Matrix n{v[0] * v[1], -v[0] * v[0], v[1] * v[1], -v[0] * v[1]};
        auto b = product(product({-q.d / det, q.b / det, q.c / det, -q.a / det}, n), q);
        result[left] = result[right] = {n, {b.a, b.c, b.b, b.d}};
    }
    return result;
}
// Sparse exact Laurent coefficients. unknown marks the highest power that
// truncation might still affect. No term at or below it can establish a pass.
constexpr int exact = std::numeric_limits<int>::min() / 2;
struct MoreTerms {};
struct Jet {
    std::map<int, Scalar> terms;
    int unknown = exact;
    int degree() const { return terms.empty() ? unknown : terms.rbegin()->first; }
    void add(int k, const Scalar& s) {
        if (s.zero() || k <= unknown) return;
        auto [it, fresh] = terms.emplace(k, s);
        if (!fresh) {
            it->second = it->second + s;
            if (it->second.zero()) terms.erase(it);
        }
    }
    void truncate(int cutoff) {
        unknown = std::max(unknown, cutoff);
        terms.erase(terms.begin(), terms.upper_bound(unknown));
    }
};
Jet monomial(int k, const Scalar& v) {
    Jet r;
    r.add(k, v);
    return r;
}
Jet qmon(int k, const Q& v) { return monomial(k, Scalar(v)); }
int shift(int order, int delta) {
    if (order == exact || delta == exact) return exact;
    if (std::abs(order) > 100000 || std::abs(delta) > 100000)
        throw residue_unsupported("excessive Laurent degree");
    return order + delta;
}
class Jets {
    int width;

  public:
    explicit Jets(int span) : width(span) {}
    Jet add(Jet p, const Jet& q) const {
        if (q.unknown > p.unknown) p.truncate(q.unknown);
        for (const auto& [k, c] : q.terms)
            p.add(k, c);
        return p;
    }
    Jet mul(const Jet& p, const Jet& q) const {
        Jet z;
        int top = shift(p.degree(), q.degree());
        if (top == exact) return z;
        z.unknown = std::max(shift(p.unknown, q.degree()), shift(q.unknown, p.degree()));
        for (auto& [i, a] : p.terms)
            for (auto& [j, b] : q.terms) {
                if (i + j <= z.unknown) continue;
                if (i + j < top - width) {
                    z.unknown = std::max(z.unknown, top - width - 1);
                    continue;
                }
                z.add(i + j, a * b);
            }
        return z;
    }
    Jet invert(const Jet& p) const {
        if (p.terms.empty()) {
            if (p.unknown != exact) throw MoreTerms{};
            throw residue_singular("identically zero denominator on high-energy chart");
        }
        const auto& [k, leading] = *p.terms.rbegin();
        auto factor = inverse(leading);
        Jet r = monomial(-k, factor);
        if (p.terms.size() == 1 && p.unknown == exact) return r;
        int last = std::max(-k - width, 1 + shift(p.unknown, -2 * k));
        r.unknown = last - 1;
        for (int j = -k - 1; j >= last; --j) {
            Scalar sum;
            for (const auto& [i, v] : p.terms) {
                if (i == k) continue;
                auto it = r.terms.find(j + k - i);
                if (it != r.terms.end()) sum = sum + v * it->second;
            }
            r.add(j, Scalar(-1) * factor * sum);
        }
        return r;
    }
    Jet pow(Jet value, long k) const {
        if (k < -4096 || k > 4096) throw residue_unsupported("excessive Laurent power");
        if (k < 0) {
            value = invert(value);
            k = -k;
        }
        Jet result = qmon(0, 1);
        for (; k; k /= 2) {
            if (k % 2) result = mul(result, value);
            if (k > 1) value = mul(value, value);
        }
        return result;
    }
    Jet minus(const Jet& x) const { return mul(qmon(0, -1), x); }
};
using JVec = std::array<Jet, 2>;
using JMat = std::array<Jet, 4>;
class Evaluation {
    const Algebra& a;
    Parameters& params;
    const std::vector<Frame>& f;
    Jets jets;
    std::size_t sample;
    const std::vector<shear>* shears;
    std::vector<JMat> p;
    Jet component(int energy_power, const Q& value) const { return qmon(shears ? 0 : energy_power, value); }
    JVec transform(const JVec& v, Matrix slope) const {
        auto t = [&](const Q& k, const Jet& x) { return jets.mul(qmon(1, k), x); };
        return {jets.add(v[0], jets.add(t(slope.a, v[0]), t(slope.b, v[1]))),
                jets.add(v[1], jets.add(t(slope.c, v[0]), t(slope.d, v[1])))};
    }
    std::map<Id, Jet> memo;
    std::map<Id, JVec> spinors;
    std::map<Id, JMat> insertions;
    JMat plus(JMat m, const JMat& n) const {
        for (int j = 0; j < 4; ++j)
            m[j] = jets.add(std::move(m[j]), n[j]);
        return m;
    }
    JMat adj(const JMat& m) const { return {m[3], jets.minus(m[1]), jets.minus(m[2]), m[0]}; }
    Jet dot(const JVec& x, const JVec& y) const { return jets.add(jets.mul(x[0], y[0]), jets.mul(x[1], y[1])); }
    JVec row(const JVec& r, const JMat& m) const { return {dot(r, {m[0], m[2]}), dot(r, {m[1], m[3]})}; }
    Jet det(const JMat& m) const { return jets.add(jets.mul(m[0], m[3]), jets.minus(jets.mul(m[1], m[2]))); }
    int leg(Id label) const {
        int k = a.leg(label);
        if (k < 1 || std::size_t(k) > f.size()) throw residue_unsupported("invalid external leg");
        return k - 1;
    }
    JMat sum(Id id) const {
        auto& n = a.node(id);
        if (n.head != "Multiparticle") return p[leg(id)];
        JMat m{};
        for (Id l : n.args)
            m = plus(std::move(m), p[leg(l)]);
        return m;
    }
    JMat momentum(Id id) {
        if (auto it = insertions.find(id); it != insertions.end()) return it->second;
        auto& n = a.node(id);
        JMat v{};
        if (n.head == "Mom" && n.args.size() == 1)
            v = sum(n.args[0]);
        else if (n.head == "Plus")
            for (Id x : n.args)
                v = plus(std::move(v), momentum(x));
        else if (n.head == "Times" && n.args.size() == 2) {
            auto s = monomial(0, params.value(n.args[0]));
            v = momentum(n.args[1]);
            for (auto& j : v)
                j = jets.mul(s, j);
        } else
            throw residue_unsupported("unsupported momentum insertion: " + n.head);
        return insertions.emplace(id, v).first->second;
    }
    bool internal_partition(Id momentum) const {
        const auto& mom = a.node(momentum);
        if (mom.head != "Mom" || mom.args.size() != 1) return false;
        const auto& group = a.node(mom.args.front());
        return group.head == "Multiparticle" && group.args.size() >= 2 && group.args.size() + 2 <= f.size();
    }
    bool angle(Id id) const {
        auto& n = a.node(id);
        if (n.head != "Spinor" || (n.args.size() != 3 && n.args.size() != 4 && n.args.size() != 5))
            throw residue_unsupported("unsupported high-energy spinor endpoint");
        auto& type = a.node(n.args[n.args.size() == 5 ? 2 : 1]).head;
        if (type == "\"Angle\"") return true;
        if (type == "\"Square\"") return false;
        throw residue_unsupported("unknown spinor chirality");
    }
    JVec spinor(Id id) {
        if (auto it = spinors.find(id); it != spinors.end()) return it->second;
        auto& n = a.node(id);
        bool ang = angle(id);
        auto kind = a.node(n.args[0]).head;
        Id label = n.args[n.args.size() == 5 ? 3 : 2];
        JVec out{};
        if (kind == "\"Xi\"" && n.args.size() <= 4) {
            Generator draw{std::mt19937_64(mix(hashText(a.format(id)) ^ mix(sample + 0x1fb793)))};
            auto v = draw.vector();
            out = {qmon(0, v[0]), qmon(0, v[1])};
            if (shears) {
                auto slope = draw.vector();
                for (int s = 0; s < 2; ++s)
                    out[s].add(1, Scalar(slope[s]));
            }
        } else {
            int j = leg(label);
            const auto& k = f[j];
            if (kind == "\"Helicity\"" && n.args.size() <= 4 && !k.mass) {
                for (int s = 0; s < 2; ++s)
                    out[s] = ang ? component(1, k.l[s])
                                 : jets.add(component(1, k.r[s]), component(-3, k.x * k.h[s]));
            } else if (kind == "\"Spin\"" && k.mass && (n.args.size() == 3 || n.args.size() == 5)) {
                // Angle frame A=[u*l,(m/det(l,b))*b/u].
                // The square frame is epsilon*adj(P)*A/m, giving S=[-m*h/(det(r,h)*u),u*r].
                Q w1 = 1, w2 = k.spin;
                if (n.args.size() == 5) {
                    auto upper = a.node(n.args[1]).head == "\"Upper\"";
                    auto& ix = a.node(n.args[4]);
                    Q v;
                    if (upper && ix.head == "ConstructiveDiagrams`VectorSpinIndex" && ix.args.size() == 2 &&
                        a.leg(ix.args[0]) == j + 1 && (a.leg(ix.args[1]) == 1 || a.leg(ix.args[1]) == 2)) {
                    } else if ((upper || a.node(n.args[1]).head == "\"Lower\"") && a.number(n.args[4], v) &&
                               (v == Q(1, 2) || v == Q(-1, 2))) {
                        w1 = upper ? (v < 0 ? 1 : 0) : (v > 0 ? -1 : 0);
                        w2 = upper ? (v > 0 ? 1 : 0) : (v < 0 ? 1 : 0);
                    } else
                        throw residue_unsupported("uncontracted high-energy spin index");
                }
                Q da = k.mass / wedge(k.l, k.b), ds = -k.mass / wedge(k.r, k.h);
                for (int s = 0; s < 2; ++s)
                    out[s] = ang ? jets.add(component(1, w1 * k.l[s]), component(-1, w2 * da * k.b[s]))
                                 : jets.add(component(-1, w1 * ds * k.h[s]), component(1, w2 * k.r[s]));
            } else
                throw residue_unsupported("spinor metadata inconsistent with high-energy mass");
            if (shears)
                out = transform(out, ang ? (*shears)[j].angle : (*shears)[j].square);
        }
        return spinors.emplace(id, out).first->second;
    }
    Jet chain(const std::vector<Id>& w) {
        if (w.size() < 2) throw residue_unsupported("empty high-energy chain");
        bool start = angle(w.front()), end = angle(w.back());
        if ((start == end) != ((w.size() - 2) % 2 == 0))
            throw residue_unsupported("inconsistent chain chirality");
        JVec r = spinor(w.front()), s = spinor(w.back());
        if (start) r = {r[1], jets.minus(r[0])};
        for (std::size_t i = 1; i + 1 < w.size(); ++i) {
            auto m = momentum(w[i]);
            r = row(r, ((i % 2 == 1) == start) ? m : adj(m));
        }
        if (!end) s = {s[1], jets.minus(s[0])};
        return dot(r, s);
    }
    Jet evaluate(Id id) {
        auto& n = a.node(id);
        Q q;
        if (n.head == "Plus") {
            Jet r;
            for (Id x : n.args)
                r = jets.add(std::move(r), get(x));
            return r;
        }
        if (n.head == "Times") {
            Jet r = qmon(0, 1);
            for (Id x : n.args)
                r = jets.mul(r, get(x));
            return r;
        }
        if (n.head == "SpinorChain") return chain(n.args);
        if (n.head == "MomProd" && n.args.size() == 2) {
            auto u = sum(n.args[0]), v = sum(n.args[1]);
            return jets.mul(qmon(0, Q(1, 2)), jets.add(det(plus(u, v)), jets.minus(jets.add(det(u), det(v)))));
        }
        if (n.head == "Mandelstahm" && n.args.size() >= 2) {
            JMat v{};
            for (Id x : n.args)
                v = plus(v, sum(x));
            return det(v);
        }
        if (n.head == "Mass" && n.args.size() == 1) return qmon(0, f[leg(n.args[0])].mass);
        if (n.head == "PropDen" && n.args.size() == 2) {
            Q m = params.rational(n.args[1]);
            auto v = det(momentum(n.args[0]));
            // A zero leading multi-leg invariant describes a collinear or
            // exceptional direction, outside the fixed-angle scattering chart.
            if (!shears && internal_partition(n.args[0]) && v.degree() != 4)
                throw residue_singular("nongeneric fixed-angle propagator");
            return jets.add(std::move(v), qmon(0, -m * m));
        }
        if (n.head == "Power" && n.args.size() == 2 && a.number(n.args[1], q) && q.get_den() == 1 &&
            q.get_num().fits_slong_p()) {
            long k = q.get_num().get_si();
            if (a.node(n.args[0]).head == "Mom" && k % 2 == 0) return jets.pow(det(momentum(n.args[0])), k / 2);
            return jets.pow(get(n.args[0]), k);
        }
        if (n.head == "SpinorTrace" && n.args.size() % 2 == 0) {
            JVec r{qmon(0, 1), Jet{}}, s{Jet{}, qmon(0, 1)};
            for (std::size_t i = 0; i < n.args.size(); ++i) {
                auto m = momentum(n.args[i]);
                if (i % 2) m = adj(m);
                r = row(r, m);
                s = row(s, m);
            }
            return jets.add(r[0], s[1]);
        }
        return monomial(0, params.value(id));
    }

  public:
    Evaluation(const Algebra& tree, Parameters& parameters, const std::vector<Frame>& frame, std::size_t seed,
               int width, const std::vector<shear>* deformation = nullptr)
        : a(tree), params(parameters), f(frame), jets(width), sample(seed), shears(deformation) {
        for (auto& k : f) {
            Matrix large = outer(k.l, k.r), small = outer(k.b, k.h);
            if (shears) {
                auto& shift = (*shears)[p.size()];
                auto base = large + small;
                Matrix right{shift.square.a, shift.square.c, shift.square.b, shift.square.d};
                auto linear = product(shift.angle, base) + product(base, right);
                auto quadratic = product(product(shift.angle, base), right);
                auto entry = [&](const Q& v, const Q& l, const Q& q) {
                    Jet j = qmon(0, v);
                    j.add(1, Scalar(l));
                    j.add(2, Scalar(q));
                    return j;
                };
                p.push_back({entry(base.a, linear.a, quadratic.a), entry(base.b, linear.b, quadratic.b),
                             entry(base.c, linear.c, quadratic.c), entry(base.d, linear.d, quadratic.d)});
                continue;
            }
            JMat v{qmon(2, large.a), qmon(2, large.b), qmon(2, large.c), qmon(2, large.d)};
            std::array<Q, 4> sub{small.a, small.b, small.c, small.d};
            for (int j = 0; j < 4; ++j)
                v[j].add(-2, Scalar(sub[j]));
            p.push_back(v);
        }
    }
    Jet invariant(Id mom) { return det(momentum(mom)); }
    void forget_spin(const std::vector<Id>& affected) {
        for (Id id : affected) {
            memo.erase(id);
            spinors.erase(id);
        }
    }
    const Jet& get(Id id) {
        if (auto it = memo.find(id); it != memo.end()) return it->second;
        return memo.emplace(id, evaluate(id)).first->second;
    }
};
// Each massive spin-J wavefunction is a homogeneous polynomial in its two
// little-group coordinates. Substituting {1,s} retains every coefficient.
// Bound the degree per external leg before choosing an interpolation grid.
class SpinDegree {
    const Algebra& a;
    std::size_t cap;
    using Degrees = std::vector<std::size_t>;
    std::map<Id, Degrees> memo;

  public:
    std::vector<Id> dependent;
    bool polynomial = true;
    SpinDegree(const Algebra& arena, std::size_t limit) : a(arena), cap(limit) {}
    const Degrees& get(Id id) {
        if (auto it = memo.find(id); it != memo.end()) return it->second;
        const auto& n = a.node(id);
        Degrees result(a.masses().size());
        if (n.head == "Spinor" && (n.args.size() == 3 || n.args.size() == 5) &&
            a.node(n.args[0]).head == "\"Spin\"") {
            if (n.args.size() == 3 || a.node(n.args[4]).head == "ConstructiveDiagrams`VectorSpinIndex") {
                int label = a.leg(n.args[n.args.size() == 3 ? 2 : 3]);
                if (label < 1 || std::size_t(label) > result.size())
                    throw residue_unsupported("invalid spin label");
                result[label - 1] = 1;
            }
        } else if (n.head == "Plus" || n.head == "Times" || n.head == "SpinorChain") {
            for (Id x : n.args) {
                const auto& part = get(x);
                for (std::size_t j = 0; j < result.size(); ++j)
                    result[j] =
                        n.head == "Plus" ? std::max(result[j], part[j]) : std::min(cap, result[j] + part[j]);
            }
        } else if (n.head == "Power" && n.args.size() == 2) {
            result = get(n.args[0]);
            Q k;
            for (auto& d : result)
                if (d) {
                    if (!a.number(n.args[1], k) || k < 0 || k.get_den() != 1)
                        polynomial = false;
                    else if (k > cap)
                        d = cap;
                    else
                        d = std::min(cap, d * k.get_num().get_ui());
                }
        }
        if (std::any_of(result.begin(), result.end(), [](auto d) { return d != 0; })) dependent.push_back(id);
        return memo.emplace(id, std::move(result)).first->second;
    }
};
} // namespace

std::vector<std::vector<mpq_class>> high_energy_chart_rows(const Algebra& algebra,
                                                           const std::vector<Id>& expressions, int bound,
                                                           std::size_t sample,
                                                           const std::map<Id, Q>& parameter_values) {
    if (bound < -10000 || bound > 10000) throw residue_unsupported("energy bound outside the Laurent budget");
    Parameters parameters(algebra, sample, parameter_values);
    std::vector<Q> masses;
    for (Id mass : algebra.masses())
        masses.push_back(parameters.rational(mass));
    auto frame = make_chart(masses, sample);
    for (int width = 8; width <= 256; width *= 2) {
        Evaluation evaluator(algebra, parameters, frame, sample, width);
        std::map<std::pair<int, std::size_t>, std::vector<Q>> rows;
        try {
            for (std::size_t j = 0; j < expressions.size(); ++j) {
                const Jet& v = evaluator.get(expressions[j]);
                if (v.unknown > 2 * bound) throw MoreTerms{};
                for (const auto& [k, z] : v.terms)
                    if (k > 2 * bound)
                        for (std::size_t part = 0; part < 4; ++part)
                            if (z.c[part]) {
                                auto& row = rows[{k, part}];
                                if (row.empty()) row.resize(expressions.size());
                                row[j] = z.c[part];
                            }
            }
        } catch (const MoreTerms&) {
            continue;
        }
        std::vector<std::vector<Q>> result;
        for (auto& [key, row] : rows)
            result.push_back(std::move(row));
        return result;
    }
    throw residue_unsupported("unable to certify the requested Laurent powers within the series budget");
}
energy_chart_validation high_energy_validate_chart(const Algebra& algebra, Id expression, int bound,
                                                   std::size_t sample, const std::map<Id, Q>& fixed,
                                                   std::size_t max_spin_states) {
    if (bound < -10000 || bound > 10000) throw residue_unsupported("energy bound outside the Laurent budget");
    if (max_spin_states > 65536) throw residue_unsupported("excessive coherent-spin grid");
    Parameters parameters(algebra, sample, fixed);
    std::vector<Q> masses;
    for (Id mass : algebra.masses())
        masses.push_back(parameters.rational(mass));
    auto initial = make_chart(masses, sample);
    SpinDegree degree(algebra, std::max<std::size_t>(1, max_spin_states));
    auto bounds = degree.get(expression);
    bool all = degree.polynomial && max_spin_states;
    std::size_t count = 1;
    for (auto d : bounds) {
        if (count > max_spin_states / (d + 1)) {
            all = false;
            break;
        }
        count *= d + 1;
    }
    if (!all) count = 1;
    for (int width = 8; width <= 256; width *= 2) {
        auto frame = initial;
        Evaluation evaluator(algebra, parameters, frame, sample, width);
        try {
            for (std::size_t point = 0; point < count; ++point) {
                if (point) {
                    auto code = point;
                    for (std::size_t j = 0; j < frame.size(); ++j) {
                        frame[j].spin = initial[j].spin + static_cast<unsigned long>(code % (bounds[j] + 1));
                        code /= bounds[j] + 1;
                    }
                    evaluator.forget_spin(degree.dependent);
                }
                const auto& value = evaluator.get(expression);
                if (value.unknown > 2 * bound) throw MoreTerms{};
                if (value.degree() > 2 * bound) return {false, point + 1, all && point + 1 == count};
            }
            return {true, count, all};
        } catch (const MoreTerms&) {
            continue;
        }
    }
    throw residue_unsupported("unable to certify the requested Laurent powers within the series budget");
}
regular_chart_validation regularity_chart(const Algebra& a, Id cleared, const std::vector<Id>& probes,
                                          std::size_t sample, std::size_t pairing,
                                          const std::map<Id, Q>& fixed) {
    Parameters parameters(a, sample, fixed);
    std::vector<Q> masses;
    for (auto id : a.masses())
        masses.push_back(parameters.rational(id));
    auto frame = make_chart(masses, sample);
    auto shears = pair_shears(frame, sample, pairing);
    // Only primitive Lorentz and spin contractions use the Laurent engine.
    // On this deformation all are polynomials. Combine rational expressions
    // by exact polynomial division in a sampled finite field; nested rational
    // continuations would otherwise produce very long GMP Laurent tails.
    polynomial_field field(pairing + 4 * mix(sample));
    using fraction = polynomial_field::fraction;
    for (int width = 8; width <= 256; width *= 2) {
        Evaluation ev(a, parameters, frame, sample, width, &shears);
        std::map<Id, fraction> memo;
        std::function<const fraction&(Id)> evaluate = [&](Id id) -> const fraction& {
            if (auto it = memo.find(id); it != memo.end())
                return it->second;
            const auto& n = a.node(id);
            fraction value;
            Q exp;
            if (n.head == "Times" || n.head == "Plus") {
                bool sum = n.head == "Plus";
                if (!sum)
                    value.num = {1};
                for (auto arg : n.args)
                    value = sum ? field.add(value, evaluate(arg)) : field.multiply(value, evaluate(arg));
            } else if (n.head == "Power" && n.args.size() == 2 && a.number(n.args[1], exp) &&
                       exp.get_den() == 1 && exp.get_num().fits_slong_p() &&
                       a.node(n.args[0]).head != "Mom") {
                value = field.pow(evaluate(n.args[0]), exp.get_num().get_si());
            } else {
                // Re-express Mom[...]^(2 k) as its polynomial invariant to the
                // power k. Inverse powers must pass through the GCD operation.
                if (n.head == "Power" && n.args.size() == 2 && a.node(n.args[0]).head == "Mom" &&
                    a.number(n.args[1], exp) && exp.get_den() == 1 && exp.get_num().fits_slong_p()) {
                    long exponent = exp.get_num().get_si();
                    if (exponent % 2)
                        throw residue_unsupported("odd momentum power in regularity test");
                    const auto& j = ev.invariant(n.args[0]);
                    if (j.unknown != exact)
                        throw MoreTerms{};
                    for (const auto& [i, c] : j.terms) {
                        value.num.resize(i + 1);
                        value.num[i] = field.coefficient(c);
                    }
                    value = field.pow(field.reduced(std::move(value.num), {1}), exponent / 2);
                } else {
                    const auto& j = ev.get(id);
                    if (j.unknown != exact)
                        throw MoreTerms{};
                    for (const auto& [i, c] : j.terms) {
                        if (i < 0)
                            throw residue_unsupported("nonpolynomial chart primitive");
                        value.num.resize(i + 1);
                        value.num[i] = field.coefficient(c);
                    }
                    value = field.reduced(std::move(value.num), {1});
                }
            }
            return memo.emplace(id, std::move(value)).first->second;
        };
        try {
            regular_chart_validation result{evaluate(cleared).den.size() == 1, {}};
            for (Id id : probes) {
                const auto& fraction = evaluate(id);
                result.varied.push_back(fraction.num.size() > 1 || fraction.den.size() > 1);
            }
            return result;
        } catch (const MoreTerms&) {
        }
    }
    throw residue_unsupported("regularity primitive exceeds the polynomial chart budget");
}
} // namespace hepcat
