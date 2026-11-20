#include "permutation_sum.hpp"
#include "spinor_word.hpp"

#include <algorithm>
#include <limits>
#include <map>
#include <set>
#include <unordered_map>

namespace hepcat {
namespace {

using Terms = std::vector<Id>;
struct Unsupported {};
constexpr std::size_t max_terms = 256;

// Only expand the commutative sum and product. A reciprocal sum or an
// unfamiliar indexed tensor is outside the supported permutation grammar.
Terms expand(Algebra& a, Id id, std::size_t& work) {
    const auto& n = a.node(id);
    if (n.head != "Plus" && n.head != "Times") return {id};
    Terms terms;
    if (n.head == "Times") terms.push_back(a.integer(1));
    for (auto arg : n.args) {
        const auto next = expand(a, arg, work);
        if (n.head == "Plus") {
            if (next.size() + terms.size() > max_terms) throw Unsupported{};
            terms.insert(terms.end(), next.begin(), next.end());
        } else {
            if (next.size() * terms.size() > max_terms) throw Unsupported{};
            Terms products;
            for (auto x : terms)
                for (auto y : next) products.push_back(a.mul({x, y}));
            terms = std::move(products);
        }
        if (terms.size() > work) throw Unsupported{};
        work -= terms.size();
    }
    return terms;
}

// Relabel only declared external leg slots. Numerical powers, model scopes,
// spin indices and the order of noncommuting momentum insertions are distinct.
class relabel {
    Algebra& a;
    std::vector<int> images;
    std::unordered_map<Id, Id> cache;

    Id label(Id id) {
        const auto& n = a.node(id);
        if (n.head == "Multiparticle") {
            Terms labels;
            for (Id k : n.args) {
                if (!a.leg(k)) throw Unsupported{};
                labels.push_back(label(k));
            }
            std::sort(labels.begin(), labels.end(), [&](Id x, Id y) { return a.leg(x) < a.leg(y); });
            return a.call(n.head, std::move(labels));
        }
        const auto k = static_cast<std::size_t>(a.leg(id));
        if (!k || k > images.size()) throw Unsupported{};
        return a.integer(images[k - 1]);
    }
public:
    relabel(Algebra& a, const std::vector<int>& from, const std::vector<int>& to)
        : a(a), images(a.masses().size()) {
        for (std::size_t i = 0; i < images.size(); ++i) images[i] = static_cast<int>(i + 1);
        for (std::size_t i = 0; i < from.size(); ++i) images[from[i] - 1] = to[i];
    }
    Id operator()(Id id) {
        if (auto it = cache.find(id); it != cache.end()) return it->second;
        const auto& n = a.node(id);
        auto args = n.args;
        Id out = id;
        if (n.head == "Spinor") {
            const auto e = spinor_detail::endpoint(a,id);
            if (args.size() != 3 || !e.leg ||
                (e.kind != spinor_detail::Kind::Helicity && e.kind != spinor_detail::Kind::Spin))
                throw Unsupported{};
            args.back() = label(args.back());
            out = a.call(n.head,std::move(args));
        } else if (n.head == "Mom" && args.size() == 1) {
            out = a.call(n.head,{label(args.front())});
        } else if (n.head == "Times" || n.head == "Plus" || n.head == "Power" ||
                   n.head == "PropDen" || n.head == "SpinorChain") {
            for (auto& k : args) k = (*this)(k);
            bool negative = false;
            if (n.head == "SpinorChain") {
                if (args.size() < 2) throw Unsupported{};
                const auto left = spinor_detail::endpoint(a,args.front());
                const auto right = spinor_detail::endpoint(a,args.back());
                const bool odd = args.size() % 2;
                if (!left.leg || !right.leg || (left.angle != right.angle) != odd ||
                    !std::all_of(args.begin()+1,args.end()-1,
                                 [&](Id k) { return a.node(k).head == "Mom"; }))
                    throw Unsupported{};
                // Reversing an even-insertion chain changes its sign.
                // Only the orientation is needed to compare permutation images.
                if (odd ? left.angle : left.leg > right.leg) {
                    std::reverse(args.begin(),args.end());
                    negative = !odd;
                }
            }
            out = a.call(n.head,std::move(args));
            if (negative) out = a.mul({a.integer(-1),out});
        } else if (n.compound && n.head != "Rational" && n.head != "ModelParameter") {
            throw Unsupported{};
        }
        return cache.emplace(id,out).first->second;
    }
};

std::optional<permutation_sum> try_group(Algebra& a, Id input, const std::vector<int>& legs) {
    try {
        std::size_t work = 16000;
        relabel canonical(a,legs,legs);
        const auto expanded = a.add(expand(a,canonical(input),work));
        const auto& n = a.node(expanded);
        if (n.head != "Plus" || n.args.size() < 6) return {};
        std::set<Id> pending(n.args.begin(),n.args.end());
        std::vector<relabel> swaps;
        mpz_class permutations = 1;
        for (std::size_t i = 1; i < legs.size(); ++i) {
            auto order = legs;
            std::swap(order[i - 1],order[i]);
            swaps.emplace_back(a,legs,order);
            permutations *= i + 1;
        }
        Terms seeds;
        std::vector<std::size_t> sizes;
        while (!pending.empty()) {
            const Id first = *pending.begin();
            std::set<Id> orbit{first};
            Terms queue{first};
            // Adjacent transpositions generate the whole symmetric group.
            // Each distinct signed image is visited once, including stabilizers.
            for (std::size_t i = 0; i < queue.size(); ++i) {
                for (auto& swap : swaps) {
                    const Id image = swap(queue[i]);
                    if (!pending.count(image) || swap(image) != queue[i]) return {};
                    if (orbit.insert(image).second) queue.push_back(image);
                }
            }
            for (Id k : orbit) pending.erase(k);
            mpq_class multiplicity(mpz_class(orbit.size()),permutations);
            multiplicity.canonicalize();
            const Id norm = a.exact(multiplicity);
            // Equivalent representatives can expose common factors in the kernel.
            Id chosen = first;
            std::size_t cost = std::numeric_limits<std::size_t>::max();
            for (Id k : orbit) {
                Id term = a.mul({norm,k});
                auto trial = seeds;
                trial.push_back(term);
                const auto size = a.cost(a.factorSum(a.add(trial)));
                if (size < cost) { cost = size; chosen = term; }
            }
            seeds.push_back(chosen);
            sizes.push_back(orbit.size());
        }
        if (seeds.size() * 3 > n.args.size()) return {};
        Id kernel = compact_commuting_products(a,a.add(seeds));
        kernel = collect_scalar_factors(a,kernel,3);
        if (a.cost(kernel) + 16 >= a.cost(input)) return {};
        return permutation_sum{legs,kernel,std::move(sizes)};
    } catch (const Unsupported&) {
        return {};
    }
}

} // namespace

std::optional<permutation_sum> compact_permutations(Algebra& a, Id input) {
    // Equal external masses permit the relabeling. The terms and their signs
    // must still prove that the expression is invariant under every swap.
    if (a.cost(input) < 300 || a.cost(input) > 18000 || !a.shells().empty()) return {};
    std::map<Id,std::vector<int>> groups;
    for (std::size_t i = 0; i < a.masses().size(); ++i)
        groups[a.masses()[i]].push_back(static_cast<int>(i + 1));

    std::optional<permutation_sum> best;
    for (const auto& group : groups) {
        const auto& legs = group.second;
        if (legs.size() < 3 || legs.size() > 12) continue;
        auto sum = try_group(a,input,legs);
        if (sum && (!best || a.cost(sum->kernel) < a.cost(best->kernel))) best = std::move(sum);
    }
    return best;
}

} // namespace hepcat
