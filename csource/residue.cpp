// Rational cut charts and HEPCAT's bispinor conventions. Matrices are
// complexified momenta with rational entries, so det(p) is p^2 and both
// mass shells and momentum conservation can be imposed without roundoff.
#include "residue.hpp"
#include "chart_scalar.hpp"

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
using namespace chart_detail;
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

class Chart {
  public:
    Legs cut, other;
    bool displaced = false;
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
        return !displaced && (legs == cut || legs == other);
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
    std::unordered_map<Id, int> orders;
    bool prune_regular_terms = false;
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
        if (n.head == "NullChannelBranch" && n.args.size() == 3) {
            auto side = legs(n.args[0]);
            if (c.internalMass || side.size() != 2 || !c.isCut(side))
                throw residue_unsupported("three-point helicity branch outside its own massless cut");
            auto i = side[0]-1, j = side[1]-1;
            bool ang = a.node(n.args[1]).head == "\"Angle\"";
            const auto& frames = ang ? c.angles : c.squares;
            if (!bracket(ang, frames[i], frames[j])) return {0, Scalar{}};
            return get(n.args[2]);
        }
        if (n.head == "Plus") {
            if (prune_regular_terms) {
                const int leading = order(id);
                Scalar coefficient;
                for (Id child : n.args)
                    if (order(child) == leading)
                        coefficient += get(child).coefficient;
                return {leading, coefficient};
            }
            PoleValue best;
            bool any = false;
            for (Id child : n.args) {
                const auto& term = get(child);
                if (!any || term.order < best.order) {
                    best = term;
                    any = true;
                } else if (term.order == best.order)
                    best.coefficient += term.coefficient;
            }
            return best;
        }
        if (n.head == "Times") {
            if (n.args.empty()) return {0, Scalar(Q(1))};
            auto p = get(n.args.front());
            for (size_t i = 1; i < n.args.size(); ++i) {
                const auto& factor = get(n.args[i]);
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
                const auto& p = get(n.args[0]);
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
    void prune_large_sums(bool enabled) { prune_regular_terms = enabled; }
    // The existing Laurent rules choose the lowest structural order even
    // when its coefficient vanishes. Determine that same order without
    // multiplying every regular subtree on a large physical cut. Reciprocals
    // and primitives still evaluate on the chart, preserving singular retries
    // and errors for unsupported factors.
    int order(Id id) {
        if (auto it = values.find(id); it != values.end())
            return it->second.order;
        if (auto it = orders.find(id); it != orders.end())
            return it->second;
        const auto &n = a.node(id);
        if (n.head != "Times" && n.head != "Plus")
            return get(id).order;
        int result = 0;
        if (n.head == "Times") {
            for (Id f : n.args)
                result = static_cast<int>(addOrders(result, order(f)));
        } else if (!n.args.empty()) {
            result = order(n.args.front());
            for (Id f : n.args)
                result = std::min(result, order(f));
        }
        orders.emplace(id, result);
        return result;
    }
    long internalWeight(Id id) {
        if (auto it = weights.find(id); it != weights.end()) return it->second;
        const auto& n = a.node(id);
        long weight = 0;
        if (n.head == "NullChannelBranch" && n.args.size() == 3)
            weight = internalWeight(n.args[2]);
        else if (isX(n.head))
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
    const PoleValue& get(Id id) {
        if (auto found = values.find(id); found != values.end()) return found->second;
        return values.emplace(id, evaluate(id)).first->second;
    }
};
// A single spurious extra power of the cut propagator can cancel between
// diagrams. Evaluate its constant and linear numerator coefficients exactly
// on a deformation that keeps all external momenta on shell. The parameter
// is the selected inverse propagator itself, fixing the residue normalization.
class double_cut_limit {
  struct dual {
    int order;
    Scalar value, slope;
  };
  const Algebra &a;
  Chart center;
  Parameters &parameters;
  Id cut_mass;
  Q jacobian;
  int l = -1, r = -1;
  Matrix transfer;
  std::vector<Chart> frames;
  std::vector<std::unique_ptr<Evaluator>> evaluators;
  std::unordered_map<Id, dual> cache;

  dual multiply(const dual &x, const dual &y) {
    return {x.order + y.order, x.value * y.value,
            x.value * y.slope + x.slope * y.value};
  }
  dual raised(const dual &x, long k) {
    if (k == 0)
      return {0, Scalar(Q(1)), {}};
    return {static_cast<int>(x.order * k), power(x.value, k),
            x.slope * Scalar(Q(k)) * power(x.value, k == 1 ? 0 : k - 1)};
  }
  dual add(const dual &x, const dual &y) {
    if (x.order > y.order)
      return add(y, x);
    if (x.order == y.order)
      return {x.order, x.value + y.value, x.slope + y.slope};
    return {x.order, x.value,
            x.order + 1 == y.order ? x.slope + y.value : x.slope};
  }
  // Chain and trace entries are polynomials: each external spinor and
  // each matrix is at most linear in this particular deformation.
  dual primitive(Id id) {
    const auto &n = a.node(id);
    auto base = evaluators.front()->get(id).coefficient;
    int degree = 0;
    if (n.head == "SpinorChain" || n.head == "SpinorTrace")
      degree = static_cast<int>(n.args.size());
    else if (n.head == "PropDen" || n.head == "Mandelstahm" ||
             n.head == "MomProd")
      degree = 2;
    else if (n.head == "Power" && n.args.size() == 2 &&
             a.node(n.args[0]).head == "Mom") {
      Q exponent;
      if (!a.number(n.args[1], exponent) || exponent != 2)
        throw residue_unsupported(
            "unsupported momentum power in a double-pole derivative");
      degree = 2;
    } else if (n.head == "xFactor" || n.head == "xTildeFactor" ||
               n.head == "ConstructiveDiagrams`xFactor" ||
               n.head == "ConstructiveDiagrams`xTildeFactor")
      throw residue_unsupported(
          "internal helicity factor has no chosen continuation off this cut");
    if (!degree)
      return {0, base, {}};
    if (degree > static_cast<int>(frames.size()) - 1)
      throw residue_unsupported("spinor chain exceeds cut-derivative bound");
    Scalar slope;
    // Newton forward differences, using f'(0) = sum (-1)^(k-1) Delta^k f(0)/k.
    std::vector<Scalar> differences{base};
    for (int j = 1; j <= degree; ++j)
      differences.push_back(evaluators[j]->get(id).coefficient);
    for (int k = 1; k <= degree; ++k) {
      for (int j = 0; j < degree + 1 - k; ++j)
        differences[j] = differences[j + 1] + differences[j] * Scalar(Q(-1));
      slope = slope + differences.front() * Scalar(Q(k % 2 ? 1 : -1, k));
    }
    return {0, base, slope};
  }
  dual visit(Id id) {
    if (auto f = cache.find(id); f != cache.end())
      return f->second;
    const auto &n = a.node(id);
    dual v{0, Scalar(Q(1)), {}};
    if (n.head == "Times") {
      for (auto child : n.args)
        v = multiply(v, visit(child));
    } else if (n.head == "Plus" && !n.args.empty()) {
      v = visit(n.args.front());
      for (std::size_t i = 1; i < n.args.size(); ++i)
        v = add(v, visit(n.args[i]));
    } else if (n.head == "Power" && n.args.size() == 2) {
      Q e;
      if (a.number(n.args[1], e) && e.get_den() == 1 && abs(e) < 10000 &&
          a.node(n.args[0]).head != "Mom")
        v = raised(visit(n.args[0]), e.get_num().get_si());
      else
        v = primitive(id);
    } else if (n.head == "PropDen" && n.args.size() == 2 &&
               n.args[1] == cut_mass && a.node(n.args[0]).head == "Mom") {
      const auto &label = a.node(a.node(n.args[0]).args.at(0));
      std::vector<int> legs;
      for (auto i : label.args)
        legs.push_back(a.leg(i));
      v = label.head == "Multiparticle" && center.isCut(legs)
              ? dual{1, Scalar(Q(1)), {}}
              : primitive(id);
    } else
      v = primitive(id);
    cache.emplace(id, v);
    return v;
  }

public:
  double_cut_limit(const Algebra &a_, Parameters &params, const Chart &chart,
                   Id mass, std::size_t max_degree)
      : a(a_), center(chart), parameters(params), cut_mass(mass) {
    auto k = center.sum(center.cut);
    for (int i : center.cut)
      for (int j : center.other) {
        if (l >= 0 || center.masses[i - 1] || center.masses[j - 1])
          continue;
        for (bool reverse : {false, true}) {
          int x = (reverse ? j : i) - 1, y = (reverse ? i : j) - 1;
          auto u = center.angles[y], v = center.squares[x];
          Matrix q{u[0] * v[0], u[0] * v[1], u[1] * v[0], u[1] * v[1]};
          Q delta = det(k + (reverse ? scale(-1, q) : q)) - det(k);
          if (delta) {
            l = x;
            r = y;
            transfer = q;
            jacobian = delta;
            break;
          }
        }
      }
    if (l < 0)
      throw residue_unsupported("double-pole derivative requires a transverse "
                                "massless shift across the cut");
    frames.reserve(max_degree + 1);
    for (std::size_t t = 0; t <= max_degree; ++t) {
      frames.push_back(center);
      auto &f = frames.back();
      f.displaced = true;
      Q z = Q(t) / jacobian;
      f.momenta[l] = f.momenta[l] + scale(z, transfer);
      f.momenta[r] = f.momenta[r] - scale(z, transfer);
      for (int k = 0; k < 2; ++k) {
        f.angles[l][k] += z * center.angles[r][k];
        f.squares[r][k] -= z * center.squares[l][k];
      }
      evaluators.push_back(
          std::make_unique<Evaluator>(a, parameters, f, cut_mass));
    }
  }
  Scalar residue(Id root) {
    auto v = visit(root);
    if (v.order < -2 || (v.order == -2 && !v.value.zero()))
      throw residue_unsupported(
          "uncancelled or unresolved higher-order physical pole");
    return v.order == -2 ? v.slope : (v.order == -1 ? v.value : Scalar{});
  }
};

template <class F>
auto sample_cut(const Algebra& algebra, const numerator_shell& channel, const std::vector<Id>& expressions,
                std::size_t sample, const std::map<Id, Q>& parameter_values, F project) {
    Algebra local(&algebra);
    Id mass = local.parse(channel.mass);
    Parameters params(local, sample, parameter_values);
    std::vector<Q> masses;
    for (Id id : algebra.masses())
        masses.push_back(params.rational(id));
    Chart chart(channel.legs, std::move(masses), params.rational(mass), sample);
    Evaluator evaluator(local, params, chart, mass);
    std::optional<double_cut_limit> limit;
    std::vector<decltype(project(chart, Scalar{}))> row;
    row.reserve(expressions.size());
    for (Id id : expressions) {
        if (evaluator.internalWeight(id) != 0)
            throw residue_unsupported("unbalanced sewn internal helicity weight");
        evaluator.prune_large_sums(algebra.cost(id) > 4096);
        const auto& p = evaluator.get(id);
        if (p.order < -1) {
            if (p.order < -2 || !p.coefficient.zero())
                throw residue_unsupported("uncancelled or unresolved higher-order physical pole");
            if (!limit) limit.emplace(local, params, chart, mass, 16);
            row.push_back(project(chart, limit->residue(id)));
        } else row.push_back(project(chart, p.order == -1 ? p.coefficient : Scalar{}));
    }
    return row;
}
} // namespace

std::vector<mpq_class> residue_sample(const Algebra& a, const numerator_shell& cut,
                                      const std::vector<Id>& expressions, std::size_t sample) {
    return sample_cut(a, cut, expressions, sample, {}, [](const Chart& chart, const Scalar& s) {
        Q result = 0;
        for (unsigned j = 0; j < 4; ++j)
            result += chart.projection[j] * s.c[j];
        return result;
    });
}
std::vector<std::array<mpq_class, 4>> residue_components(const Algebra& a, const numerator_shell& cut,
                                                         const std::vector<Id>& expressions,
                                                         std::size_t sample,
                                                         const std::map<Id, mpq_class>& parameters) {
    return sample_cut(a, cut, expressions, sample, parameters,
                      [](const Chart&, const Scalar& s) { return s.c; });
}
} // namespace hepcat
