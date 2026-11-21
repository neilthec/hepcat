// Independent transcription of the charged-tree kernels and their stabilizers.
// Build like eight-point-two-negative.cpp. Arguments: FERMION_PAIRS NEGATIVES POINTS SEED [MG_LIBRARY].
#include "eight-point-validation.hpp"
#include "eight-point-weyl.hpp"
#include "permutation_sum.hpp"
#include "radiative_currents.hpp"
#include <numeric>

using namespace hepcat;
namespace {
using legs = std::vector<int>;
using edges = std::vector<std::pair<int, int>>;
using allocation = std::vector<legs>;

class kernels {
    Algebra &a;
    const int pairs, negatives;
    legs electron, positron, plus, minus;
    const Id mass, charge;
    bool physical;
    std::vector<int> types;

    Id index(int i) { return a.atom("mu" + std::to_string(i)); }
    Id momentum(const legs &side) {
        std::vector<Id> labels;
        for (int i : side)
            labels.push_back(a.integer(i));
        return a.call("Mom", {a.call("Multiparticle", labels)});
    }

    // Collect the external momenta on the far side of an exchanged photon.
    legs branch(const edges &tree, const allocation &photons, int here, int from) {
        legs out{electron[here], positron[here]};
        out.insert(out.end(), photons[here].begin(), photons[here].end());
        for (auto [l, r] : tree) {
            if (r == here)
                std::swap(l, r);
            if (l != here || r == from)
                continue;
            auto more = branch(tree, photons, r, here);
            out.insert(out.end(), more.begin(), more.end());
        }
        return out;
    }

    Id line(int vertex, const std::map<int, legs> &attachments, bool ordered) {
        legs order;
        for (const auto &entry : attachments)
            order.push_back(entry.first);
        std::vector<Id> sum;
        const auto spinor = [&](const char *name, int i) {
            return a.call(name, {a.integer(i), mass, a.atom("\"Dirac\"")});
        };
        do {
            legs side{positron[vertex]};
            std::vector<std::pair<Id, std::vector<Id>>> words{{a.integer(1), {}}};
            for (auto port : order) {
                for (auto &word : words)
                    word.second.push_back(a.call("DiracGamma", {index(port)}));
                if (port == order.back())
                    break;
                const auto &added = attachments.at(port);
                side.insert(side.end(), added.begin(), added.end());
                std::sort(side.begin(), side.end());
                const Id p = momentum(side);
                const Id inv = a.pow(a.call("PropDen", {p, mass}), -1);
                const Id slash = a.call("DiracSlash", {p});
                std::vector<std::pair<Id, std::vector<Id>>> next;
                for (const auto &[factor, operators] : words) {
                    next.emplace_back(a.mul({factor, mass, inv}), operators);
                    next.emplace_back(a.mul({a.integer(-1), factor, inv}), operators);
                    next.back().second.push_back(slash);
                }
                words = std::move(next);
            }
            for (const auto &[factor, operators] : words)
                sum.push_back(
                    a.mul({factor, a.call("DiracChain", {spinor("FermionBar", positron[vertex]),
                                                         a.call("DiracWord", operators),
                                                         spinor("FermionKet", electron[vertex])})}));
        } while (!ordered && std::next_permutation(order.begin(), order.end()));
        return a.add(std::move(sum));
    }

    Id kernel(const edges &tree, const allocation &photons) {
        std::map<Id, reference_choice> refs;
        std::vector<Id> numerator, inverse;
        std::vector<std::map<int, legs>> attachments(pairs);
        for (int v = 0; v < pairs; ++v)
            for (int i : photons[v]) {
                const int h = types[i - 1] > 0 ? 1 : -1;
                const auto &others = h == 1 ? minus : plus;
                const int ref = physical          ? electron[v]
                                : !others.empty() ? others.front()
                                : i == 8          ? 2 * pairs + 1
                                                  : i + 1;
                Id xi = a.call("Spinor",
                               {a.atom("\"Xi\""), a.atom(h == 1 ? "\"Angle\"" : "\"Square\""), a.integer(i)});
                refs.emplace(xi, reference_choice{i, ref, physical});
                attachments[v][i] = {i};
                numerator.push_back(
                    a.call("PolarizationVector", {a.integer(i), index(i), a.integer(0), a.integer(h)}));
            }
        radiative_detail::endpoint_radiation emit(a, types, refs);
        for (unsigned k = 0; k < tree.size(); ++k) {
            const auto [l, r] = tree[k];
            attachments[l][9 + k] = branch(tree, photons, r, l);
            attachments[r][9 + k] = branch(tree, photons, l, r);
            inverse.push_back(a.pow(emit.propagator(attachments[l][9 + k], a.integer(0)), -1));
        }
        if (pairs == 2) {
            // Two open vectors, contracted by the Weyl Fierz identity.
            const auto x = emit.current(electron[0], positron[0], photons[0]);
            const auto y = emit.current(electron[1], positron[1], photons[1]);
            inverse.push_back(
                a.mul({a.integer(-1), a.pow(a.mul({a.parse("Power[2,Rational[1,2]]"), charge}), 6),
                       emit.contract_currents(x, y)}));
        } else {
            for (int v = 0; v < pairs; ++v)
                numerator.push_back(line(v, attachments[v], pairs == 4));
            const auto spin = feynman_to_spinors(a, a.mul(numerator));
            inverse.push_back(a.simplify(replace_external_references(a, spin, refs)));
            inverse.push_back(a.pow(charge, 6));
            inverse.push_back(a.integer(pairs % 2 ? -1 : 1));
        }
        return a.mul(inverse);
    }

  public:
    kernels(Algebra &arena, int r, int n, Id m, Id e, bool phys)
        : a(arena), pairs(r), negatives(n), mass(m), charge(e), physical(phys), types(8) {
        electron = r == 2 ? legs{1, 4} : r == 3 ? legs{1, 3, 5} : legs{1, 3, 5, 7};
        positron = r == 2 ? legs{2, 3} : r == 3 ? legs{2, 4, 6} : legs{2, 4, 6, 8};
        for (auto i : electron)
            types[i - 1] = 1;
        for (auto i : positron)
            types[i - 1] = -1;
        for (int i = 2 * r + 1; i <= 8; ++i) {
            types[i - 1] = i <= 8 - n ? 2 : -2;
            (types[i - 1] > 0 ? plus : minus).push_back(i);
        }
    }

    Id amplitude() {
        const edges path2{{0, 1}}, path3{{0, 1}, {1, 2}}, path4{{0, 1}, {1, 2}, {2, 3}},
            star{{0, 1}, {0, 2}, {0, 3}};
        std::vector<Id> terms;
        auto add = [&](const edges &tree, allocation assignment, int divisor) {
            terms.push_back(a.mul({kernel(tree, assignment), a.exact(mpq_class(1, divisor))}));
        };
        if (pairs == 2) {
            // One representative for each occupation up to line exchange.
            for (unsigned i = 0; i <= plus.size(); ++i)
                for (unsigned j = 0; j <= minus.size(); ++j) {
                    unsigned k = plus.size() - i, l = minus.size() - j;
                    if (std::pair{i, j} > std::pair{k, l})
                        continue;
                    allocation sets(2);
                    for (unsigned n = 0; n < plus.size(); ++n)
                        sets[n < i ? 0 : 1].push_back(plus[n]);
                    for (unsigned n = 0; n < minus.size(); ++n)
                        sets[n < j ? 0 : 1].push_back(minus[n]);
                    int divisor = (i == k && j == l) ? 2 : 1;
                    for (auto n : {i, j, k, l})
                        for (unsigned f = 2; f <= n; ++f)
                            divisor *= f;
                    add(path2, sets, divisor);
                }
        } else if (pairs == 3) {
            const bool same = negatives == 0;
            add(path3, {{7, 8}, {}, {}}, same ? 2 : 1);
            add(path3, {{}, {7, 8}, {}}, same ? 4 : 2);
            add(path3, {{7}, {}, {8}}, same ? 2 : 1);
            add(path3, {{7}, {8}, {}}, 1);
            if (!same)
                add(path3, {{8}, {7}, {}}, 1);
        } else {
            // Relabeling the three leaves in the two alternating sums supplies
            // every ordering on the star. On the path, reversing a two-vector
            // line gives the missing order with a minus sign.
            add(star, allocation(4), 1);
            Id ordered_path = kernel(path4, allocation(4));
            auto both_orders = apply_exchanges(
                a, ordered_path, {signed_exchange{{{3, 4}}, -1}, signed_exchange{{{5, 6}}, -1}});
            terms.push_back(a.mul({both_orders, a.exact(mpq_class(1, 2))}));
        }
        auto value = expand_permutation_groups(a, a.add(terms), {plus, minus});
        return expand_permutation_groups(a, value, {electron, positron}, true);
    }
};
} // namespace

int main(int argc, char **argv) {
    const bool compare_orders = argc > 1 && std::string(argv[1]) == "--compare-orders";
    if (compare_orders) {
        ++argv;
        --argc;
    }
    if (argc != 5 && argc != 6) {
        std::cerr << "Usage: eight-point-charged-trees [--compare-orders] PAIRS NEGATIVES POINTS SEED "
                     "[MG_LIBRARY]\n";
        return 2;
    }
    const bool madgraph = argc == 6;
    const int pairs = std::stoi(argv[1]), negatives = std::stoi(argv[2]);
    if (pairs < 2 || pairs > 4 || negatives < 0 || negatives > 4 - pairs)
        return 2;
    const std::vector<std::string> fermions =
        pairs == 2   ? std::vector<std::string>{"e", "E", "E", "e"}
        : pairs == 3 ? std::vector<std::string>{"e", "E", "e", "E", "e", "E"}
                     : std::vector<std::string>{"e", "E", "e", "E", "e", "E", "e", "E"};
    std::vector<int> photons(8 - 2 * pairs, 1);
    std::fill(photons.end() - negatives, photons.end(), -1);
    return eight_point_validation::run(
        argc - 2, argv + 2, photons,
        [&](Algebra &a, Id m, Id e) {
            kernels physical(a, pairs, negatives, m, e, true), spurious(a, pairs, negatives, m, e, false);
            eight_point_validation::formulas forms;
            if (pairs < 4 || !madgraph || compare_orders)
                forms.emplace_back("physical", physical.amplitude());
            if (pairs < 4)
                forms.emplace_back("spurious", spurious.amplitude());
            else {
                eight_point_weyl::charged_line waves(a, m);
                if (!madgraph || compare_orders) {
                    auto seed = a.add({waves.star(), a.mul({a.exact(mpq_class(1, 2)), waves.path()})});
                    auto sum = expand_permutation_groups(a, seed, {{1, 3, 5, 7}, {2, 4, 6, 8}}, true);
                    forms.emplace_back("Weyl", a.mul({a.pow(e, 6), sum}));
                }
                if (madgraph || compare_orders)
                    forms.emplace_back("Weyl (grouped)", a.mul({a.pow(e, 6), waves.grouped()}));
            }
            return forms;
        },
        fermions,
        pairs == 2   ? 1680
        : pairs == 3 ? 2016
                     : 1728);
}
