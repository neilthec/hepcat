// Rational cut charts and HEPCAT's bispinor conventions. Matrices are
// complexified momenta with rational entries, so det(p) is p^2 and both
// mass shells and momentum conservation can be imposed without roundoff.
#include "residue.hpp"

#include <algorithm>
#include <array>
#include <cctype>
#include <cstdlib>
#include <cstdint>
#include <random>
#include <string_view>
#include <unordered_map>

namespace hepcat {
namespace {
using Q = mpq_class;
using Legs = std::vector<int>;
using Vector = std::array<Q, 2>;
struct Matrix {
    Q a, b, c, d;
};
Q det(const Matrix& p) { return p.a * p.d - p.b * p.c; }
Matrix operator+(const Matrix& p, const Matrix& q) { return {p.a + q.a, p.b + q.b, p.c + q.c, p.d + q.d}; }
Matrix operator-(const Matrix& p, const Matrix& q) { return {p.a - q.a, p.b - q.b, p.c - q.c, p.d - q.d}; }
Matrix scale(const Q& s, const Matrix& p) { return {s * p.a, s * p.b, s * p.c, s * p.d}; }
Matrix adj(const Matrix& p) { return {p.d, -p.b, -p.c, p.a}; }
Matrix transpose(const Matrix& p) { return {p.a, p.c, p.b, p.d}; }
Matrix multiply(const Matrix& p, const Matrix& q) {
    return {p.a * q.a + p.b * q.c, p.a * q.b + p.b * q.d, p.c * q.a + p.d * q.c, p.c * q.b + p.d * q.d};
}
Vector right(const Vector& v, const Matrix& p) { return {v[0] * p.a + v[1] * p.c, v[0] * p.b + v[1] * p.d}; }
Vector left(const Matrix& p, const Vector& v) { return {p.a * v[0] + p.b * v[1], p.c * v[0] + p.d * v[1]}; }
Q inner(const Vector& p, const Vector& q) { return p[0] * q[0] + p[1] * q[1]; }
const Matrix identity{1, 0, 0, 1}, epsilon{0, 1, -1, 0};

std::uint64_t hashText(std::string_view s) {
    std::uint64_t h = 14695981039346656037ULL;
    for (unsigned char c : s) {
        h ^= c;
        h *= 1099511628211ULL;
    }
    return h;
}
std::uint64_t mix(std::uint64_t h) {
    h = (h ^ (h >> 30)) * 0xbf58476d1ce4e5b9ULL;
    h = (h ^ (h >> 27)) * 0x94d049bb133111ebULL;
    return h ^ (h >> 31);
}
struct Draw {
    std::mt19937_64 rng;
    int small() {
        int x = static_cast<int>(rng() % 18);
        return x < 9 ? x - 9 : x - 8;
    }
    Q ratio() {
        Q a = small(), b = small();
        return a / b;
    }
    Matrix shell(const Q& squared) {
        Q a = small(), b = small(), c = small();
        return {a, b, c, (squared + b * c) / a};
    }
};
Legs complement(const Legs& cut, std::size_t count) {
    Legs result;
    for (std::size_t i = 1; i <= count; ++i)
        if (!std::binary_search(cut.begin(), cut.end(), static_cast<int>(i)))
            result.push_back(static_cast<int>(i));
    return result;
}
std::pair<Matrix, Matrix> splitTwo(Matrix total, const Q& m1, const Q& m2, bool flip, Draw& draw) {
    Matrix r = flip ? transpose(total) : total;
    Q a = draw.small(), b = draw.small(), pivot = a * r.b - b * r.a;
    if (!r.a || !pivot) throw residue_singular("singular two-body chart pivot");
    Q k = det(r) + m1 * m1 - m2 * m2;
    Q x = (m1 * m1 * r.a - a * (k - r.d * a + r.c * b)) / pivot;
    Q y = (k - r.d * a + r.c * b + r.b * x) / r.a;
    Matrix p{a, b, x, y};
    if (flip) p = transpose(p);
    return {p, total - p};
}

// Exact coefficients in Q(sqrt(2), i), including the usual helicity and
// imaginary-vertex normalization. Use one random rational linear projection
// only after computing the exact residue of every expression in the row.
struct Scalar {
    std::array<Q, 4> c{}; // 1, sqrt2, i, i*sqrt2
    Scalar() = default;
    explicit Scalar(const Q& q) { c[0] = q; }
    static Scalar root2() {
        Scalar z;
        z.c[1] = 1;
        return z;
    }
    static Scalar imaginary() {
        Scalar z;
        z.c[2] = 1;
        return z;
    }
    bool zero() const { return !c[0] && !c[1] && !c[2] && !c[3]; }
    bool rational() const { return !c[1] && !c[2] && !c[3]; }
};
Scalar operator+(Scalar p, const Scalar& q) {
    for (unsigned i = 0; i < 4; ++i)
        p.c[i] += q.c[i];
    return p;
}
Scalar operator*(const Scalar& p, const Scalar& q) {
    if (p.rational() && q.rational()) return Scalar(p.c[0] * q.c[0]);
    Scalar z;
    for (unsigned i = 0; i < 4; ++i)
        if (p.c[i])
            for (unsigned j = 0; j < 4; ++j)
                if (q.c[j]) {
                    int factor = ((i & j) & 1) ? 2 : 1;
                    if ((i & j) & 2) factor = -factor;
                    z.c[i ^ j] += factor * p.c[i] * q.c[j];
                }
    return z;
}
Scalar inverse(const Scalar& p) {
    if (p.zero()) throw residue_singular("zero auxiliary denominator at selected cut");
    if (p.rational()) return Scalar(1 / p.c[0]);
    Scalar conj = p;
    conj.c[2] *= -1;
    conj.c[3] *= -1;
    Scalar norm = p * conj, conjugateNorm = norm;
    conjugateNorm.c[1] *= -1;
    Scalar den = norm * conjugateNorm;
    if (!den.rational() || !den.c[0]) throw std::logic_error("invalid quadratic coefficient norm");
    return conj * conjugateNorm * Scalar(1 / den.c[0]);
}
Scalar power(Scalar p, long n) {
    if (n < -100000 || n > 100000) throw residue_unsupported("exponent exceeds exact residue budget");
    if (n < 0) {
        p = inverse(p);
        n = -n;
    }
    Scalar z(Q(1));
    for (; n; n /= 2) {
        if (n % 2) z = z * p;
        if (n > 1) p = p * p;
    }
    return z;
}
bool sqrtRational(const Q& r, Q& out) {
    if (r < 0 || !mpz_perfect_square_p(r.get_num_mpz_t()) || !mpz_perfect_square_p(r.get_den_mpz_t()))
        return false;
    mpz_class n, d;
    mpz_sqrt(n.get_mpz_t(), r.get_num_mpz_t());
    mpz_sqrt(d.get_mpz_t(), r.get_den_mpz_t());
    out = Q(n, d);
    return true;
}
Scalar squareRoot(const Scalar& value) {
    if (!value.rational()) throw residue_unsupported("nested algebraic radical");
    Q rad = value.c[0], root;
    bool neg = rad < 0;
    if (neg) rad = -rad;
    Scalar r;
    if (sqrtRational(rad, root))
        r = Scalar(root);
    else if (sqrtRational(rad / 2, root))
        r = Scalar(root) * Scalar::root2();
    else
        throw residue_unsupported("radical outside Q(sqrt(2),i)");
    return neg ? r * Scalar::imaginary() : r;
}

class Parameters {
    const Algebra& a;
    std::uint64_t salt;
    std::unordered_map<Id, Scalar> cache;

  public:
    Parameters(const Algebra& arena, std::size_t sample) : a(arena), salt(mix(sample + 13)) {}
    // Unknown atomic scalars are free model parameters. Compound heads require
    // explicit semantics: in particular a color tensor is never randomized.
    Scalar value(Id id) {
        if (auto found = cache.find(id); found != cache.end()) return found->second;
        const auto& n = a.node(id);
        Q q;
        Scalar z;
        if (a.number(id, q))
            z = Scalar(q);
        else if (n.head == "System`I" || n.head == "I")
            z = Scalar::imaginary();
        else if ((!n.compound && !n.head.empty() && n.head[0] != '"' &&
                  (std::isalpha(static_cast<unsigned char>(n.head[0])) || n.head[0] == '$')) ||
                 (n.head == "ModelParameter" && n.args.size() == 3)) {
            if (n.head.compare(0, 7, "System`") == 0 || n.head == "ConstructiveDiagrams`z" || n.head == "z" ||
                n.head == "Indeterminate" || n.head == "ComplexInfinity" || n.head == "Infinity" ||
                n.head == "Null" || n.head == "True" || n.head == "False")
                throw residue_unsupported("unremoved contour or unsupported scalar: " + n.head);
            auto h = mix(hashText(a.format(id)) ^ salt);
            // Denominator and numerator are positive. This avoids zero masses
            // and fixes the square-root branch for positive mass parameters.
            z = Scalar(
                Q(static_cast<unsigned long>(2 + h % 89), static_cast<unsigned long>(1 + (h >> 22) % 17)));
            z.c[0].canonicalize();
        } else if (n.head == "Plus" || n.head == "Times") {
            bool sum = n.head == "Plus";
            z = Scalar(Q(sum ? 0 : 1));
            for (Id child : n.args)
                z = sum ? z + value(child) : z * value(child);
        } else if (n.head == "Power" && n.args.size() == 2 && a.number(n.args[1], q)) {
            if (!q.get_num().fits_slong_p()) throw residue_unsupported("exponent too large");
            if (q.get_den() == 1)
                z = power(value(n.args[0]), q.get_num().get_si());
            else if (q.get_den() == 2)
                z = power(squareRoot(value(n.args[0])), q.get_num().get_si());
            else
                throw residue_unsupported("unsupported scalar root");
        } else if ((n.head == "System`Complex" || n.head == "Complex") && n.args.size() == 2) {
            z = value(n.args[0]) + Scalar::imaginary() * value(n.args[1]);
        } else
            throw residue_unsupported("unmapped scalar head: " + n.head);
        cache.emplace(id, z);
        return z;
    }
    Q rational(Id id) {
        Scalar z = value(id);
        if (!z.rational()) throw residue_unsupported("chart mass must be rational at the test point");
        return z.c[0];
    }
};

class Chart {
  public:
    Legs cut, other;
    std::vector<Q> masses;
    std::vector<Matrix> momenta;
    std::vector<Vector> angles, squares;
    std::vector<Matrix> angleBasis, squareBasis;
    Q internalMass;
    std::uint64_t salt;
    std::array<Q, 4> projection;
    Chart(Legs legs, std::vector<Q> external, Q exchanged, std::size_t sample)
        : cut(std::move(legs)), masses(std::move(external)), internalMass(std::move(exchanged)),
          salt(mix(sample + 71)) {
        std::sort(cut.begin(), cut.end());
        if (cut.size() < 2 || cut.size() + 2 > masses.size() || cut.front() < 1 ||
            static_cast<std::size_t>(cut.back()) > masses.size() ||
            std::adjacent_find(cut.begin(), cut.end()) != cut.end())
            throw residue_unsupported("invalid physical channel subset");
        other = complement(cut, masses.size());
        if (std::make_pair(other.size(), other) < std::make_pair(cut.size(), cut)) cut.swap(other);
        std::uint64_t seed = salt;
        for (int i : cut)
            seed = mix(seed ^ static_cast<unsigned>(i));
        Draw draw{std::mt19937_64(seed)};
        std::size_t n = masses.size();
        momenta.resize(n);
        angles.resize(n);
        squares.resize(n);
        angleBasis.resize(n);
        squareBasis.resize(n);
        const Matrix total = draw.shell(internalMass * internalMass);
        bool left = sample & 1u, right = (sample >> 1) & 1u;
        if (n == 4 && internalMass == 0 &&
            std::all_of(masses.begin(), masses.end(), [](const Q& m) { return !m; }))
            right = !left;
        auto side = [&](const Legs& sideLegs, Matrix rem, bool transposed) {
            for (std::size_t j = 0; j + 2 < sideLegs.size(); ++j) {
                int i = sideLegs[j] - 1;
                momenta[i] = draw.shell(masses[i] * masses[i]);
                rem = rem - momenta[i];
            }
            int i = sideLegs[sideLegs.size() - 2] - 1, j = sideLegs.back() - 1;
            auto pair = splitTwo(rem, masses[i], masses[j], transposed, draw);
            momenta[i] = pair.first;
            momenta[j] = pair.second;
        };
        side(cut, total, left);
        side(other, scale(-1, total), right);
        Matrix check{};
        for (std::size_t i = 0; i < n; ++i) {
            const Matrix& p = momenta[i];
            const Q& m = masses[i];
            check = check + p;
            if (det(p) != m * m) throw std::logic_error("bad external shell in native cut chart");
            if (!m) {
                if (!p.a) throw residue_singular("singular external helicity frame");
                Q s = draw.ratio();
                angles[i] = {s * p.a, s * p.c};
                squares[i] = {1 / s, p.b / (p.a * s)};
            } else {
                Vector spin{Q(draw.small()), Q(draw.small())};
                angleBasis[i] = draw.shell(m);
                squareBasis[i] = scale(1 / m, multiply(epsilon, multiply(adj(p), angleBasis[i])));
                angles[i] = leftMultiply(angleBasis[i], spin);
                squares[i] = leftMultiply(squareBasis[i], spin);
            }
        }
        if (check.a || check.b || check.c || check.d || det(sum(cut)) != internalMass * internalMass)
            throw std::logic_error("bad conservation in native cut chart");
        projection = {1, draw.ratio(), draw.ratio(), draw.ratio()};
    }
    static Vector leftMultiply(const Matrix& m, const Vector& v) { return left(m, v); }
    Matrix sum(const Legs& labels) const {
        Matrix m{};
        for (int leg : labels)
            m = m + momenta.at(static_cast<std::size_t>(leg - 1));
        return m;
    }
    bool isCut(Legs legs) const {
        std::sort(legs.begin(), legs.end());
        return legs == cut || legs == other;
    }
    Vector reference(const std::string& kind, const std::string& label) const {
        Draw draw{std::mt19937_64(mix(hashText(kind + ":" + label) ^ salt))};
        return {draw.ratio(), draw.ratio()};
    }
};

struct PoleValue {
    // The structural power describes explicit selected propagators only.
    // Its coefficient has been restricted to the cut. Higher poles are
    // rejected: a zero leading coefficient could hide a nonzero derivative.
    int order = 0;
    Scalar coefficient;
};
class Evaluator {
    const Algebra& a;
    Parameters& parameters;
    const Chart& c;
    Id cutMass;
    std::unordered_map<Id, PoleValue> values;
    std::unordered_map<Id, Matrix> momenta;
    std::unordered_map<Id, Vector> spinors;
    std::unordered_map<Id, long> weights;
    static long addOrders(long x, long y) {
        long result;
        if (__builtin_add_overflow(x, y, &result) || result < -100000 || result > 100000)
            throw residue_unsupported("excessive pole or internal spinor weight");
        return result;
    }
    int leg(Id label) const {
        int k = a.leg(label);
        if (k <= 0 || static_cast<std::size_t>(k) > c.masses.size())
            throw residue_unsupported("unknown external leg");
        return k;
    }
    Legs legs(Id label) const {
        const auto& n = a.node(label);
        Legs out;
        if (n.head == "Multiparticle" && n.compound) {
            for (Id child : n.args)
                out.push_back(leg(child));
        } else
            out.push_back(leg(label));
        return out;
    }
    Matrix momentum(Id id) {
        if (auto found = momenta.find(id); found != momenta.end()) return found->second;
        const auto& n = a.node(id);
        Matrix p{};
        if (n.head == "Mom" && n.args.size() == 1)
            p = c.sum(legs(n.args[0]));
        else if (n.head == "Plus")
            for (Id child : n.args)
                p = p + momentum(child);
        else if (n.head == "Times" && n.args.size() == 2)
            p = scale(parameters.rational(n.args[0]), momentum(n.args[1]));
        else
            throw residue_unsupported("unsupported momentum insertion: " + n.head);
        momenta.emplace(id, p);
        return p;
    }
    std::size_t typeSlot(Id id) const {
        const auto& n = a.node(id);
        if (n.head != "Spinor" || (n.args.size() != 3 && n.args.size() != 4 && n.args.size() != 5))
            throw residue_unsupported("invalid spinor endpoint");
        return n.args.size() == 5 ? 2 : 1;
    }
    bool angle(Id id) const {
        const auto& ch = a.node(a.node(id).args[typeSlot(id)]).head;
        if (ch == "\"Angle\"") return true;
        if (ch == "\"Square\"") return false;
        throw residue_unsupported("unknown spinor chirality");
    }
    Vector cutSpinor(const Legs& components, bool isAngle) const {
        if (c.internalMass || !c.isCut(components))
            throw residue_unsupported("composite helicity spinor outside its own massless cut");
        auto p = c.sum(components);
        if (!p.a) throw residue_singular("singular composite spinor frame");
        return isAngle ? Vector{p.a, p.c} : Vector{1, p.b / p.a};
    }
    Vector spinor(Id id) {
        if (auto found = spinors.find(id); found != spinors.end()) return found->second;
        const auto& n = a.node(id);
        std::size_t t = typeSlot(id);
        const std::string& kind = a.node(n.args[0]).head;
        bool isAngle = angle(id);
        Id label = n.args[t + 1];
        Vector v{};
        if (kind == "\"Xi\"" && n.args.size() <= 4)
            v = c.reference(isAngle ? "A" : "S", a.format(label));
        else if (kind == "\"Helicity\"" && a.node(label).head == "Multiparticle" && n.args.size() <= 4)
            v = cutSpinor(legs(label), isAngle);
        else {
            int i = leg(label) - 1;
            if (kind == "\"Helicity\"" && n.args.size() <= 4 && !c.masses[i])
                v = isAngle ? c.angles[i] : c.squares[i];
            else if (kind == "\"Spin\"" && n.args.size() == 3 && c.masses[i])
                v = isAngle ? c.angles[i] : c.squares[i];
            else if (kind == "\"Spin\"" && n.args.size() == 5 && c.masses[i]) {
                bool upper = a.node(n.args[1]).head == "\"Upper\"";
                if (!upper && a.node(n.args[1]).head != "\"Lower\"")
                    throw residue_unsupported("unknown index variance");
                const auto& index = a.node(n.args[4]);
                Q projection;
                if (upper && index.head == "ConstructiveDiagrams`VectorSpinIndex" && index.args.size() == 2 &&
                    a.leg(index.args[0]) == i + 1 && (a.leg(index.args[1]) == 1 || a.leg(index.args[1]) == 2))
                    v = isAngle ? c.angles[i] : c.squares[i];
                else if (a.number(n.args[4], projection) && (projection == Q(-1, 2) || projection == Q(1, 2))) {
                    bool first = projection < 0;
                    int sign = !upper && !first ? -1 : 1;
                    if (!upper) first = !first;
                    auto b = isAngle ? c.angleBasis[i] : c.squareBasis[i];
                    v = first ? Vector{sign * b.a, sign * b.c} : Vector{sign * b.b, sign * b.d};
                } else
                    throw residue_unsupported("uncontracted spin index");
            } else
                throw residue_unsupported("spinor metadata inconsistent with external mass");
        }
        spinors.emplace(id, v);
        return v;
    }
    static Q bracket(bool isAngle, Vector r, Vector v) {
        if (isAngle)
            r = {r[1], -r[0]};
        else
            v = {v[1], -v[0]};
        return inner(r, v);
    }
    static bool isX(const std::string& head) {
        return head == "xFactor" || head == "ConstructiveDiagrams`xFactor";
    }
    static bool isXTilde(const std::string& head) {
        return head == "xTildeFactor" || head == "ConstructiveDiagrams`xTildeFactor";
    }
    Q xFactor(Id id) const {
        const auto& n = a.node(id);
        Legs first = legs(n.args[0]);
        int j = leg(n.args[1]);
        Legs all = first;
        all.push_back(j);
        std::sort(all.begin(), all.end());
        if (c.internalMass || !c.isCut(all) || !c.masses[j - 1] ||
            det(c.sum(first)) != c.masses[j - 1] * c.masses[j - 1])
            throw residue_unsupported("x factor requires a null cut between equal-mass legs");
        bool isAngle = isX(n.head);
        // Reversing the cut momentum reverses its angle spinor and the
        // momentum-flow sign together. The ratio is independent of this choice.
        Vector r = c.reference(isAngle ? "A" : "S", "CutPhoton");
        Vector v = cutSpinor(c.cut, !isAngle);
        Q den = c.masses[j - 1] * bracket(isAngle, r, cutSpinor(c.cut, isAngle));
        if (!den) throw residue_singular("singular cut-photon x-factor reference");
        if (isAngle) {
            r = {r[1], -r[0]};
            v = {v[1], -v[0]};
        }
        Q sign = all == c.cut ? -1 : 1;
        return sign * inner(right(r, isAngle ? c.momenta[j - 1] : adj(c.momenta[j - 1])), v) / den;
    }
    Q chain(Id id) {
        const auto& w = a.node(id).args;
        if (w.size() < 2) throw residue_unsupported("empty chain");
        bool start = angle(w.front()), end = angle(w.back());
        if ((start == end) != ((w.size() - 2) % 2 == 0))
            throw residue_unsupported("inconsistent chain chirality");
        Vector row = spinor(w.front()), v = spinor(w.back());
        if (start) row = {row[1], -row[0]};
        for (std::size_t i = 1; i + 1 < w.size(); ++i) {
            auto p = momentum(w[i]);
            row = right(row, ((i % 2 == 1) == start) ? p : adj(p));
        }
        if (!end) v = {v[1], -v[0]};
        return inner(row, v);
    }
    // On a null two-leg cut, one chirality vanishes linearly and the other
    // remains regular. The exact identity <ij>[ij] = -s_ij fixes the leading
    // coefficient on either branch, including poles hidden in spinor brackets.
    PoleValue chainValue(Id id) {
        Q value = chain(id);
        if (value || c.internalMass) return {0, Scalar(value)};
        const auto& w = a.node(id).args;
        if (w.size() != 2) return {0, Scalar(value)};
        Legs pair;
        for (Id endpoint : w) {
            const auto& n = a.node(endpoint);
            if (n.args.size() != 3 || a.node(n.args[0]).head != "\"Helicity\"" ||
                a.node(n.args[2]).head == "Multiparticle")
                return {0, Scalar(value)};
            pair.push_back(leg(n.args[2]));
        }
        if (!c.isCut(pair) || pair[0] == pair[1]) return {0, Scalar(value)};
        bool isAngle = angle(w[0]);
        const auto& opposite = isAngle ? c.squares : c.angles;
        Q other = bracket(!isAngle, opposite[pair[0] - 1], opposite[pair[1] - 1]);
        if (!other) throw residue_singular("both chiralities vanish at the collinear cut");
        return {1, Scalar(-1 / other)};
    }
    PoleValue evaluate(Id id) {
        const auto& n = a.node(id);
        Q q;
        if (n.head == "Plus") {
            PoleValue best;
            bool any = false;
            for (Id child : n.args) {
                auto term = get(child);
                if (!any || term.order < best.order) {
                    best = term;
                    any = true;
                } else if (term.order == best.order)
                    best.coefficient = best.coefficient + term.coefficient;
            }
            return best;
        }
        if (n.head == "Times") {
            PoleValue p{0, Scalar(Q(1))};
            for (Id child : n.args) {
                auto factor = get(child);
                p.order = static_cast<int>(addOrders(p.order, factor.order));
                p.coefficient = p.coefficient * factor.coefficient;
            }
            return p;
        }
        if (n.head == "Power" && n.args.size() == 2 && a.number(n.args[1], q)) {
            const auto& base = a.node(n.args[0]);
            if (base.head == "Mom" && q.get_den() == 1 && q.get_num().fits_slong_p() &&
                q.get_num().get_si() % 2 == 0) {
                long exponent = q.get_num().get_si() / 2;
                if (base.args.size() == 1 && !c.internalMass && c.isCut(legs(base.args[0]))) {
                    if (exponent < -10000 || exponent > 10000)
                        throw residue_unsupported("unsupported Mandelstam power");
                    return {static_cast<int>(exponent), Scalar(Q(1))};
                }
                return {0, power(Scalar(det(momentum(n.args[0]))), exponent)};
            }
            if (q.get_den() == 1 && q.get_num().fits_slong_p()) {
                long k = q.get_num().get_si();
                auto p = get(n.args[0]);
                if (k < -100000 || k > 100000 || (p.order && std::abs(k) > 10000))
                    throw residue_unsupported("exponent outside residue budget");
                long order = p.order * k;
                if (std::abs(order) > 10000) throw residue_unsupported("unsupported pole multiplicity");
                return {static_cast<int>(order), power(p.coefficient, k)};
            }
            return {0, parameters.value(id)};
        }
        if (n.head == "SpinorChain") return chainValue(id);
        if ((isX(n.head) || isXTilde(n.head)) && n.args.size() == 2) return {0, Scalar(xFactor(id))};
        if (n.head == "SpinorTrace") {
            if (n.args.size() % 2) throw residue_unsupported("odd momentum trace");
            Matrix m = identity;
            for (std::size_t i = 0; i < n.args.size(); ++i) {
                auto p = momentum(n.args[i]);
                m = multiply(m, i % 2 ? adj(p) : p);
            }
            return {0, Scalar(m.a + m.d)};
        }
        if (n.head == "MomProd" && n.args.size() == 2) {
            auto p = c.sum(legs(n.args[0])), r = c.sum(legs(n.args[1]));
            Q dot = (det(p + r) - det(p) - det(r)) / 2;
            if (!dot && a.leg(n.args[0]) && a.leg(n.args[1]) && c.isCut({leg(n.args[0]), leg(n.args[1])}))
                return {1, Scalar(Q(1, 2))}; // p_i.p_j = D_ij / 2 on this cut
            return {0, Scalar(dot)};
        }
        if (n.head == "Mandelstahm" && n.args.size() >= 2) {
            Legs all;
            for (Id label : n.args) {
                auto list = legs(label);
                all.insert(all.end(), list.begin(), list.end());
            }
            if (!c.internalMass && c.isCut(all)) return {1, Scalar(Q(1))};
            return {0, Scalar(det(c.sum(all)))};
        }
        if (n.head == "Mass" && n.args.size() == 1) return {0, Scalar(c.masses[leg(n.args[0]) - 1])};
        if (n.head == "PropDen" && n.args.size() == 2 && a.node(n.args[0]).head == "Mom" &&
            a.node(n.args[0]).args.size() == 1) {
            Legs label = legs(a.node(n.args[0]).args[0]);
            if (c.isCut(label) && n.args[1] == cutMass) return {1, Scalar(Q(1))};
            Q m = parameters.rational(n.args[1]);
            return {0, Scalar(det(c.sum(label)) - m * m)};
        }
        return {0, parameters.value(id)};
    }

  public:
    Evaluator(const Algebra& arena, Parameters& par, const Chart& chart, Id mass)
        : a(arena), parameters(par), c(chart), cutMass(mass) {}
    long internalWeight(Id id) {
        if (auto it = weights.find(id); it != weights.end()) return it->second;
        const auto& n = a.node(id);
        long weight = 0;
        if (isX(n.head))
            weight = -2;
        else if (isXTilde(n.head))
            weight = 2;
        else if (n.head == "Spinor" && n.args.size() >= 3 && a.node(n.args[0]).head == "\"Helicity\"" &&
                 a.node(n.args[2]).head == "Multiparticle")
            weight = angle(id) ? 1 : -1;
        else if (n.head == "Power" && n.args.size() == 2) {
            long base = internalWeight(n.args[0]);
            Q exponent;
            if (base) {
                if (!a.number(n.args[1], exponent) || exponent.get_den() != 1 || abs(exponent) > 10000)
                    throw residue_unsupported("nonintegral internal helicity weight");
                weight = addOrders(0, base * exponent.get_num().get_si());
            }
        } else if (n.head == "Plus") {
            if (!n.args.empty()) weight = internalWeight(n.args.front());
            for (Id child : n.args)
                if (internalWeight(child) != weight)
                    throw residue_unsupported("inhomogeneous sewn internal helicity weights");
        } else if (n.head == "Times" || n.head == "SpinorChain") {
            for (Id child : n.args)
                weight = addOrders(weight, internalWeight(child));
        }
        weights.emplace(id, weight);
        return weight;
    }
    PoleValue get(Id id) {
        if (auto found = values.find(id); found != values.end()) return found->second;
        auto p = evaluate(id);
        values.emplace(id, p);
        return p;
    }
};
} // namespace

std::vector<mpq_class> residue_sample(const Algebra& algebra, const numerator_shell& channel,
                                      const std::vector<Id>& expressions, std::size_t sample) {
    Algebra local(&algebra);
    Id mass = local.parse(channel.mass);
    Parameters params(local, sample);
    std::vector<Q> masses;
    for (Id id : algebra.masses())
        masses.push_back(params.rational(id));
    Chart chart(channel.legs, std::move(masses), params.rational(mass), sample);
    Evaluator evaluator(local, params, chart, mass);
    std::vector<Q> row;
    row.reserve(expressions.size());
    for (Id id : expressions) {
        if (evaluator.internalWeight(id) != 0)
            throw residue_unsupported("unbalanced sewn internal helicity weight");
        auto p = evaluator.get(id);
        if (p.order < -1) throw residue_unsupported("higher-order physical pole needs a Laurent expansion");
        Q result = 0;
        if (p.order == -1)
            for (unsigned j = 0; j < 4; ++j)
                result += chart.projection[j] * p.coefficient.c[j];
        row.push_back(std::move(result));
    }
    return row;
}
} // namespace hepcat
