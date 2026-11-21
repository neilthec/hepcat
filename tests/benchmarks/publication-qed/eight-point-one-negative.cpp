// Independent transcription of the three eight-point one-negative-photon formulas.
// Build and run instructions are in eight-point-one-negative.md.
#include "radiation_kinematics.hpp"
#include "permutation_sum.hpp"
#include "eight-point-validation.hpp"

using namespace hepcat;

namespace {
class paper_formula : private detail::radiation_kinematics {
    static constexpr int h = 8;
    const Id u, spin, normalization;
    Id v(int i, legs p) {
        const int ref = i == h ? 2 : 1;
        p.erase(std::remove_if(p.begin(), p.end(), [&](int k) { return k == ref || k == i; }), p.end());
        if (p.empty())
            return ar.integer(0);
        return over(chain(i != h, i, i, {{ref}, p}), {tau({ref, i})});
    }
    Id contact(int i, int j) {
        if (i == h)
            return over(neg(ar.mul({chain(true, j, h, {{1}}), chain(true, j, h, {{2}})})),
                        {tau({2, h}), tau({1, j})});
        return over(neg(ar.pow(ar.mul({mass, chain(true, i, j)}), 2)), {tau({1, i}), tau({1, j})});
    }
    Id left(int i, int j) { return over(contact(i, j), {tau({1, i, j})}); }
    Id right(int i) { return over(neg(v(i, {2})), {tau({2, i})}); }
    Id right(int i, int j) {
        return ar.add({ar.mul({right(i), right(j)}), over(neg(ar.pow(ar.mul({mass, chain(true, i, j)}), 2)),
                                                          {tau({2, i}), tau({2, j}), tau({2, i, j})})});
    }
    Id right(int i, int j, int k) {
        std::vector<Id> terms;
        for (unsigned t = 0; t < 3; ++t) {
            terms.push_back(ar.mul({neg(v(i, {2, j, k})), right(j, k)}));
            terms.push_back(ar.mul({contact(j, k), right(i)}));
            const int next = i;
            i = j;
            j = k;
            k = next;
        }
        return over(ar.add(terms), {tau({2, i, j, k})});
    }
    Id bare(int t) { return over(ar.add({spin, ar.mul({u, v(h, {t})})}), {tau({t, h})}); } // V_h(p2)=0
    Id mixed(int t, int i) {
        const Id b = chain(true, i, h, {{t}});
        const Id recoil =
            ar.mul({spin, ar.add({ar.mul({b, chain(true, h, i)}), ar.mul({v(i, {t}), tau({t, h, i})})})});
        return over(ar.add({neg(ar.mul({u, ar.pow(b, 2)})), t == 1 ? recoil : neg(recoil)}),
                    {tau({t, h}), tau({t, i}), tau({t, h, i})});
    }
    // This representative always multiplies an expression symmetric in i,j.
    Id mixed_right_pair(int i, int j) {
        return over(
            ar.add({ar.mul({ar.integer(-2), v(j, {2, i, h}), mixed(2, i)}),
                    ar.mul({minus(spin, ar.mul({ar.integer(2), u, v(h, {i})})), right(i, j)}),
                    ar.mul({contact(i, j), bare(2)}), ar.mul({ar.integer(2), u, contact(h, i), right(j)})}),
            {tau({2, i, j, h})});
    }

  public:
    paper_formula(Algebra &a, Id m, Id e)
        : radiation_kinematics(a, m, true, true), u(chain(false, 1, 2)),
          spin(a.mul({chain(false, 1, h), chain(false, h, 2)})),
          normalization(over(a.mul({a.integer(-8), a.pow(e, 6)}), {m})) {}
    Id physical() {
        const int a = 3, b = 4, c = 5, d = 6, f = 7;
        const Id r3 = right(c, d, f), r2 = right(d, f);
        const Id mixed_left = ar.mul({mixed(1, a), ar.add({ar.mul({ar.integer(2), v(b, {a, h}), r3}),
                                                           ar.mul({ar.integer(3), contact(b, c), r2})})});
        const Id scalar_left =
            ar.mul({left(a, b), ar.add({ar.mul({ar.add({spin, neg(ar.mul({ar.integer(3), u, v(h, {c})})),
                                                        ar.mul({tau({1, a, b}), bare(1)})}),
                                                r3}),
                                        ar.mul({ar.integer(3), u, contact(h, c), r2}),
                                        ar.mul({ar.integer(6), v(c, {a}), mixed_right_pair(d, f)}),
                                        ar.mul({ar.integer(3), contact(c, d), mixed(2, f)})})});
        return over(ar.mul({normalization, ar.add({mixed_left, scalar_left})}), {ar.integer(12)});
    }
    // The projected summand in the paper has three marked emissions.
    Id ward() {
        auto w = [&](int j) {
            legs before{1};
            for (int i = 3; i < j; ++i)
                before.push_back(i);
            const Id x = chain(true, j, h, {before});
            auto z = [&](int end) {
                if (end == 3)
                    return tau({1, h});
                legs p{1};
                for (int i = 3; i < end; ++i)
                    p.push_back(i);
                return chain(true, h, h, {p});
            };
            return over(ar.mul({x, minus(ar.mul({u, x}), ar.mul({spin, chain(true, h, j)}))}),
                        {z(j), z(j + 1)});
        };
        auto tail = [&](int t, legs list) {
            if (list.empty())
                return ar.integer(1);
            const auto sig = ar.integer(t == 1 ? 1 : -1);
            auto ell = [&](int i) {
                return over(ar.mul({sig, chain(true, i, h, {{t}})}), {chain(false, h, i), tau({t, i})});
            };
            const int a = list[0];
            if (list.size() == 1)
                return ell(a);
            const int b = list[1];
            const Id pair = over(neg(ar.pow(ar.mul({mass, chain(true, a, b)}), 2)),
                                 {tau({t, a}), tau({t, b}), tau({t, a, b})});
            const Id two = ar.mul({ell(a), ell(b)});
            if (list.size() == 2)
                return over(ar.add({two, pair}), {ar.integer(2)});
            const int c = list[2];
            const Id recoil =
                over(neg(ar.mul({sig, chain(true, c, a), chain(true, c, a, {{t}})})), {tau({t, c})});
            const Id correction = ar.add({ell(c), over(ar.mul({ar.integer(2), recoil}), {tau({t, a, b, c})})});
            const Id three = ar.mul({two, ell(c)});
            if (list.size() == 3)
                return ar.add(
                    {over(three, {ar.integer(6)}), over(ar.mul({pair, correction}), {ar.integer(2)})});
            const int d = list.at(3);
            // The only four-leg tail in this representative lies to the right of photon 3.
            const Id vertex = over(chain(true, d, h, {{1, 3}}), {chain(false, h, d)});
            return ar.add({over(ar.mul({three, ell(d)}), {ar.integer(24)}),
                           over(ar.mul({pair, ar.add({ar.mul({ar.integer(2), vertex, correction}),
                                                      ar.mul({tau({t, a, b}), ell(c), ell(d)})})}),
                                {ar.integer(4), tau({1, 3, h})})});
        };
        const Id short_half =
            ar.add({ar.mul({w(3), tail(2, {7, 6, 5, 4})}), ar.mul({tail(1, {3}), w(4), tail(2, {7, 6, 5})}),
                    over(ar.mul({tail(1, {3, 4}), w(5), tail(2, {7, 6})}), {ar.integer(2)})});
        return apply_exchanges(ar, ar.mul({normalization, short_half}), {signed_exchange{{{1, 2}}, -1}});
    }
    Id spurious() {
        // Reference q is the middle positive photon in this ordered word.
        constexpr int q = 5;
        std::vector<Id> orbital{normalization}, terms;
        // A photon at either end of its own sandwich contributes zero.
        // Momentum conservation lets us evaluate an identical shorter route.
        auto sandwich = [&](int leg, int k) {
            legs l{1}, r{2};
            for (int j = 3; j <= 7; ++j)
                if (j != leg)
                    (j < 3 + k ? l : r).push_back(j);
            return r.size() < l.size() ? neg(chain(true, leg, h, {r})) : chain(true, leg, h, {l});
        };
        auto prop = [&](legs d) {
            if (d.size() <= 4)
                return tau(d);
            legs other;
            for (int j = 1; j <= 8; ++j)
                if (std::find(d.begin(), d.end(), j) == d.end())
                    other.push_back(j);
            return tau(other);
        };
        for (int i = 3; i <= 7; ++i) {
            orbital.push_back(over(sandwich(i, i - 3), {chain(false, h, i)}));
        }
        for (int k = 0; k < 3; ++k) {
            std::vector<Id> factors{ar.add({spin, over(ar.mul({u, sandwich(q, k)}), {chain(true, q, h)})})};
            for (int s = 1; s <= k; ++s) {
                legs d{1};
                for (int j = 3; j < 3 + s; ++j)
                    d.push_back(j);
                factors.push_back(ar.pow(prop(d), -1));
            }
            for (int s = k; s < 5; ++s) {
                legs d{2};
                for (int j = 3 + s; j <= 7; ++j)
                    d.push_back(j);
                factors.push_back(ar.pow(prop(d), -1));
            }
            terms.push_back(ar.mul(factors));
        }
        orbital.push_back(ar.add(terms));
        return apply_exchanges(ar, ar.mul(orbital), {signed_exchange{{{1, 2}}, -1}});
    }
};
} // namespace

int main(int argc, char** argv) {
    return eight_point_validation::run(argc, argv, {1, 1, 1, 1, 1, -1}, [](Algebra& a, Id m, Id e) {
        paper_formula paper(a, m, e);
        eight_point_validation::formulas results;
        for (auto [name, kernel] :
             {std::pair{"physical", paper.physical()}, std::pair{"spurious", paper.spurious()},
              std::pair{"ward", paper.ward()}}) {
            std::cout << name << " summand complexity " << expression_complexity(a, kernel) << '\n';
            results.emplace_back(name, expand_permutation_groups(a, kernel, {{3, 4, 5, 6, 7}}));
        }
        return results;
    });
}
