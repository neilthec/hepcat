// Independent transcription of the two three-photon currents in the paper.
// See eight-point-two-negative.md for build and run instructions.
#include "radiation_kinematics.hpp"
#include "permutation_sum.hpp"
#include "eight-point-validation.hpp"
#include <filesystem>
#include <fstream>

using namespace hepcat;
namespace {

class pair_sewing : private hepcat::detail::radiation_kinematics {
    using Id = hepcat::Id;

    const int first_negative;

    int reference(int i) const {
        if (physical)
            return 1;
        // Positive photons use the first negative. Negatives use the next
        // negative in cyclic label order. This assigns one gauge to each leg.
        return i < first_negative ? first_negative
                                  : first_negative + (i - first_negative + 1) % (9 - first_negative);
    }
    Id norm(int i) { return physical ? tau({reference(i), i}) : chain(i >= first_negative, reference(i), i); }
    Id x(int i, int t) {
        int r = reference(i);
        if (physical && r == t)
            return ar.integer(0);
        if (physical)
            return over(chain(i < first_negative, i, i, {{r}, {t}}), {norm(i)});
        if (i < first_negative)
            return over(chain(true, i, r, {{t}}), {norm(i)});
        return over(chain(true, r, i, {{t}}), {norm(i)});
    }
    using current = std::map<int, Id>;
    current pair(int t, int i, int j, bool sq) {
        Id fij;
        if ((i < first_negative) == (j < first_negative))
            fij = ar.pow(ar.mul({mass, chain(i < first_negative, i, j)}), 2);
        else
            fij = ar.pow(chain(true, std::min(i, j), std::max(i, j), {{t}}), 2);
        Id pole = tau({t, i, j});
        current c{{t, minus(ar.mul({x(i, t), x(j, t)}), over(fij, {pole}))}};
        for (auto [a, b] : {std::pair{i, j}, std::pair{j, i}}) {
            if (sq != (a < first_negative))
                continue;
            Id ta = chain(sq, t, a);
            Id f = sq == (b < first_negative) ? ar.mul({mass, ar.pow(chain(sq, a, b), 2), chain(!sq, a, t)})
                                              : ar.mul({ta, chain(!sq, b, a, {{t}}), chain(!sq, a, b)});
            c[a] = ar.add({ar.mul({ta, x(b, t)}), over(f, {pole})});
        }
        return c;
    }
    // The angle and square spinors of each polarization. Physical reference
    // spinors are momentum projections; their ket carries the reordering sign.
    Id bracket(bool sq, int i, bool pol_i, int j, bool pol_j, legs q = {}) {
        const bool projected_i = pol_i && sq == (i >= first_negative);
        const bool right_square = q.empty() ? sq : !sq;
        const bool projected_j = pol_j && right_square == (j >= first_negative);
        std::vector<Id> word;
        int r = i, s = j;
        if (projected_i && !physical)
            r = reference(i);
        if (projected_j && !physical)
            s = reference(j);
        word.push_back(spinor(r, sq != (physical && projected_i)));
        if (projected_i && physical)
            word.push_back(momentum({reference(i)}));
        if (!q.empty())
            word.push_back(momentum(q));
        if (projected_j && physical)
            word.push_back(momentum({reference(j)}));
        word.push_back(spinor(s, right_square != (physical && projected_j)));
        if (word.size() == 2 && r == s)
            return ar.integer(0);
        Id b = ar.call("SpinorChain", word);
        return projected_j && physical ? neg(b) : b;
    }
    Id project(int t, int i, int j, int k, bool sq) {
        std::vector<Id> terms;
        for (auto [end, c] : pair(t, i, j, sq))
            terms.push_back(
                ar.mul({c, t == 1 ? bracket(sq, end, false, k, true) : bracket(sq, k, true, end, false)}));
        return over(ar.add(terms), {tau({t, i}), tau({t, j}), norm(k)});
    }

  public:
    pair_sewing(hepcat::Algebra &a, Id m, bool physical, int negatives)
        : radiation_kinematics(a, m, true, physical), first_negative(9 - negatives) {}
    Id join(legs L, legs R) {
        std::vector<Id> terms;
        for (size_t i = 0; i < 3; ++i)
            for (size_t j = 0; j < 3; ++j) {
                int k = L[i], l = R[j];
                for (bool s : {false, true})
                    for (bool u : {false, true}) {
                        Id left = project(1, L[(i + 1) % 3], L[(i + 2) % 3], k, s);
                        Id right = project(2, R[(j + 1) % 3], R[(j + 2) % 3], l, u);
                        legs q = s == u ? legs{} : legs{1, L[0], L[1], L[2]};
                        Id mid = bracket(!s, k, true, l, true, q);
                        terms.push_back(ar.mul({left, right, mid, s == u ? mass : ar.integer(1)}));
                    }
            }
        return over(ar.add(terms), {tau({1, L[0], L[1], L[2]})});
    }
    Id amplitude(Id coupling) {
        if (first_negative == 6) {
            // Evaluate D after assigning the leg labels, keeping the reference
            // table fixed throughout the sum. Each distinct triple occurs once.
            std::vector<Id> terms;
            for (unsigned mask = 0; mask < 64; ++mask) {
                legs left, right;
                unsigned minus_left = 0;
                for (int i = 3; i <= 8; ++i) {
                    if (mask & (1u << (i - 3))) {
                        left.push_back(i);
                        minus_left += i >= first_negative;
                    } else
                        right.push_back(i);
                }
                if (left.size() == 3 && (physical || minus_left <= 1))
                    terms.push_back(join(left, right));
            }
            Id total = ar.add(terms);
            if (!physical)
                total = apply_exchanges(ar, total, {signed_exchange{{{1, 2}}, -1}});
            return ar.mul({neg(coupling), total});
        }
        auto A = join({3, 4, 5}, {6, 7, 8});
        auto B = join({3, 4, 7}, {5, 6, 8});
        Id seed;
        if (physical) {
            auto C = join({3, 7, 8}, {4, 5, 6});
            seed = ar.add({over(ar.add({A, C}), {ar.integer(12)}), over(B, {ar.integer(4)})});
        } else {
            seed = ar.add({over(A, {ar.integer(12)}), over(B, {ar.integer(8)})});
            seed = apply_exchanges(ar, seed, {signed_exchange{{{1, 2}}, -1}});
        }
        return ar.mul({neg(coupling), expand_permutation_groups(ar, seed, {{3, 4, 5, 6}, {7, 8}})});
    }
};
Id automatic(Algebra &a, const std::filesystem::path &directory, bool physical, Id mass, int negatives) {
    const auto path = directory / (physical ? "physical" : "spurious") / "eeAAAAAA" /
                      (std::string(6 - negatives, '+') + std::string(negatives, '-') + ".expr");
    std::ifstream input(path);
    std::string expression;
    if (!std::getline(input, expression))
        throw std::runtime_error("Cannot read " + path.string());
    std::vector<std::string> masses(8, "0");
    masses[0] = masses[1] = a.format(mass);
    amp_expression amp(expression, masses);
    auto sum = amp.reduce().permutation_form();
    if (!sum || sum->legs.size() != static_cast<std::size_t>(6 - negatives) || !sum->orbit_sizes.empty())
        throw std::runtime_error("Missing midpoint representation for " + path.string());
    std::cout << (physical ? "physical" : "spurious") << " automatic summand complexity "
              << *amp.permutation_summand_complexity() << '\n';
    // This view includes the second helicity group and the signed reflection.
    return expand_permutation_groups(a, a.parse(sum->summand), {sum->legs});
}
} // namespace
int main(int argc, char **argv) {
    int negatives = 2;
    if (argc >= 3 && std::string(argv[1]) == "--negatives") {
        negatives = std::stoi(argv[2]);
        if (negatives != 2 && negatives != 3)
            return 2;
        argc -= 2;
        argv += 2;
    }
    std::filesystem::path native;
    if (argc >= 3 && std::string(argv[1]) == "--construction") {
        native = argv[2];
        argc -= 2;
        argv += 2;
    }
    std::vector<int> helicities(6, 1);
    std::fill(helicities.end() - negatives, helicities.end(), -1);
    return eight_point_validation::run(argc, argv, helicities, [&](Algebra &a, Id m, Id e) {
        const Id coupling = a.pow(a.mul({a.parse("Power[2,Rational[1,2]]"), e}), 6);
        pair_sewing physical(a, m, true, negatives), spurious(a, m, false, negatives);
        eight_point_validation::formulas results{{"physical", physical.amplitude(coupling)},
                                                 {"spurious", spurious.amplitude(coupling)}};
        if (!native.empty()) {
            results.emplace_back("automatic physical", automatic(a, native, true, m, negatives));
            results.emplace_back("automatic spurious", automatic(a, native, false, m, negatives));
        }
        return results;
    });
}
