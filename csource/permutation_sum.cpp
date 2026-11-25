#include "permutation_sum.hpp"
#include "generation_plan.hpp"
#include "spinor_word.hpp"

#include <algorithm>
#include <limits>
#include <numeric>
#include <map>
#include <set>
#include <unordered_map>

namespace hepcat {
namespace {

using Terms = std::vector<Id>;
struct Unsupported {};
constexpr std::size_t max_terms = 768;

// Only expand the commutative sum and product. A reciprocal sum or an
// unfamiliar indexed tensor is outside the supported permutation grammar.
Terms expand(Algebra& a, Id id, std::size_t& work, std::size_t limit = max_terms) {
    const auto& n = a.node(id);
    if (n.head != "Plus" && n.head != "Times") return {id};
    Terms terms;
    if (n.head == "Times") terms.push_back(a.integer(1));
    for (auto arg : n.args) {
        const auto next = expand(a, arg, work, limit);
        if (n.head == "Plus") {
            if (next.size() + terms.size() > limit) throw Unsupported{};
            terms.insert(terms.end(), next.begin(), next.end());
        } else {
            if (next.size() * terms.size() > limit) throw Unsupported{};
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
    bool match_partitions;
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
    relabel(Algebra& a, const std::vector<int>& from, const std::vector<int>& to, bool match_partitions = false)
        : a(a), images(a.masses().size()), match_partitions(match_partitions) {
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
        } else if (n.head == "MomProd" && args.size() == 2) {
            if (!a.leg(args[0]) || !a.leg(args[1])) throw Unsupported{};
            for (auto& leg : args) leg = label(leg);
            // The scalar product is symmetric in its momentum arguments.
            if (a.leg(args[0]) > a.leg(args[1])) std::swap(args[0],args[1]);
            out = a.call(n.head,std::move(args));
        } else if (n.head == "Mom" && args.size() == 1) {
            out = a.call(n.head,{label(args.front())});
        } else if (n.head == "Times" || n.head == "Plus" || n.head == "Power" ||
                   n.head == "PropDen" || n.head == "SpinorChain") {
            for (auto& k : args) k = (*this)(k);
            // Compare rational monomials independently of whether the input
            // printer grouped several denominator factors into one reciprocal.
            if (n.head == "Power" && args.size() == 2 && a.node(args[0]).head == "Times") {
                mpq_class power;
                if (a.number(args[1],power) && power.get_den() == 1 && power >= -32 && power <= 32) {
                    Terms factors;
                    for (Id base : a.node(args[0]).args) factors.push_back(a.pow(base,power.get_num().get_si()));
                    return cache.emplace(id,a.mul(std::move(factors))).first->second;
                }
            }
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
            if (match_partitions && n.head == "PropDen") out = canonical_propagator(a,out);
            if (negative) out = a.mul({a.integer(-1),out});
        } else if (n.compound && n.head != "Rational" && n.head != "ModelParameter") {
            throw Unsupported{};
        }
        return cache.emplace(id,out).first->second;
    }
};

// Each row contains interchangeable signed representatives of a single orbit.
// Factor across rows while keeping all representatives compatible with the
// factors already chosen. No distribution of an opaque spinor chain is needed.
using OrbitChoices = std::vector<Terms>;

Id factor_choices(Algebra& a, const OrbitChoices& rows, bool weight_by_size,
                  std::size_t& budget, bool prefer_last_tie = false) {
    std::map<Id,std::size_t> coverage;
    auto factors = [&](Id id) {
        const auto& n = a.node(id);
        return n.head == "Times" ? n.args : Terms{id};
    };
    auto plain = [&] {
        Terms terms;
        for (const auto& row : rows) terms.push_back(row.front());
        return a.add(terms);
    };
    if (rows.size() < 2 || budget == 0) return plain();
    for (const auto& row : rows) {
        std::set<Id> found;
        for (Id term : row) {
            const auto list = factors(term);
            if (list.size() > budget) { budget = 0; return plain(); }
            budget -= list.size();
            for (Id f : list) {
                mpq_class value;
                if (!a.number(f,value)) found.insert(f);
            }
        }
        for (Id f : found) ++coverage[f];
    }
    Id common = 0;
    std::size_t best = 0;
    for (auto [f,count] : coverage) {
        if (count < 2) continue;
        const auto gain = (count-1) * (weight_by_size ? a.cost(f) : 1);
        if (gain > best || (prefer_last_tie && gain == best)) { common=f; best=gain; }
    }
    if (!best) return plain();
    OrbitChoices shared, remaining;
    for (const auto& row : rows) {
        Terms stripped;
        for (Id term : row) {
            auto list = factors(term);
            auto it = std::find(list.begin(),list.end(),common);
            if (it == list.end()) continue;
            list.erase(it);
            stripped.push_back(a.mul(std::move(list)));
        }
        if (stripped.empty()) remaining.push_back(row);
        else shared.push_back(std::move(stripped));
    }
    const Id product = a.mul({common,factor_choices(a,shared,weight_by_size,budget,prefer_last_tie)});
    if (remaining.empty()) return product;
    return a.add({product,factor_choices(a,remaining,weight_by_size,budget,prefer_last_tie)});
}

// Inside a complete permutation sum a summand may be replaced by any of its
// images. At a product, only swaps preserving the other factors can act on
// one selected factor. This keeps cancellations inside factored currents.
class kernel_reducer {
    Algebra& a;
    std::vector<relabel> permutations;
    // The signed two-image constructor marks its one nontrivial image.
    bool subtract_second = false;
    Id signed_image(std::size_t index, Id term) {
        const auto x = permutations[index](term);
        return subtract_second && index == 1 ? a.mul({a.integer(-1),x}) : x;
    }
    std::size_t remaining = 40000;
    using Group = std::vector<std::size_t>;
    Id visit(Id id, const Group& group) {
        const auto& node = a.node(id);
        if (group.size() < 2 || !remaining || (node.head != "Plus" && node.head != "Times")) return id;
        if (node.args.size()*group.size() > remaining) { remaining = 0; return id; }
        remaining -= node.args.size()*group.size();
        if (node.head == "Times") {
            auto factors = node.args;
            for (std::size_t i = 0; i < factors.size(); ++i) {
                if (a.node(factors[i]).head != "Plus") continue;
                auto rest = factors;
                rest.erase(rest.begin()+i);
                const auto spectator = a.mul(std::move(rest));
                Group stable;
                for (auto k : group)
                    if (permutations[k](spectator) == spectator) stable.push_back(k);
                factors[i] = visit(factors[i],stable);
            }
            return a.mul(std::move(factors));
        }
        // Average equivalent signed addends by collecting their coefficients
        // on one representative. Keep every image available for later factoring.
        Terms results;
        std::map<Id,mpq_class> coefficients;
        for (Id child : node.args) {
            Id term = visit(child,group);
            results.push_back(term);
            Id best = 0;
            mpq_class coefficient;
            bool first = true, antisymmetric = false;
            for (auto k : group) {
                Id word = signed_image(k,term);
                const auto& n = a.node(word);
                mpq_class scale = 1;
                if (n.head == "Times" && a.number(n.args.front(),scale))
                    word = a.mul({n.args.begin()+1,n.args.end()});
                if (first || word < best) {
                    first = false; best = word; coefficient = scale; antisymmetric = false;
                } else if (word == best && coefficient == -scale) antisymmetric = true;
            }
            if (!antisymmetric) coefficients[best] += coefficient;
        }
        const Id together = a.add(results);
        OrbitChoices choices;
        for (const auto& [word,coefficient] : coefficients) {
            if (coefficient == 0) continue;
            std::set<Id> alternatives;
            for (auto k : group) alternatives.insert(a.mul({a.exact(coefficient),signed_image(k,word)}));
            choices.emplace_back(alternatives.begin(),alternatives.end());
        }
        std::size_t budget = remaining;
        const auto factored = factor_choices(a,choices,1,budget);
        remaining = budget;
        return a.cost(factored) < a.cost(together) ? factored : together;
    }
public:
    kernel_reducer(Algebra& algebra, const std::vector<std::vector<int>>& groups) : a(algebra) {
        std::vector<int> from;
        std::vector<std::vector<int>> images(1);
        constexpr unsigned limit = 120;
        for (auto group : groups) {
            std::sort(group.begin(),group.end());
            from.insert(from.end(),group.begin(),group.end());
            std::vector<std::vector<int>> product;
            do {
                if (product.size()+images.size()>limit) throw Unsupported{};
                for (auto prefix : images) {
                    prefix.insert(prefix.end(),group.begin(),group.end());
                    product.push_back(std::move(prefix));
                }
            } while (std::next_permutation(group.begin(),group.end()));
            images=std::move(product);
        }
        for (const auto& image:images) permutations.emplace_back(a,from,image);
    }
    kernel_reducer(Algebra& algebra, const std::vector<int>& from,
                   const std::vector<int>& to, bool subtract)
        : a(algebra), subtract_second(subtract) {
        permutations.emplace_back(a,from,from);
        permutations.emplace_back(a,from,to);
    }
    Id run(Id id) {
        Group group(permutations.size());
        for (std::size_t i=0;i<group.size();++i) group[i]=i;
        return visit(permutations.front()(id),group);
    }
};

std::optional<permutation_sum> try_group(Algebra& a, Id input, Id expanded,
                                         const std::vector<int>& legs) {
    const auto& n = a.node(expanded);
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
    OrbitChoices choices;
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
        Id chosen = a.mul({norm,first});
        std::size_t cost = std::numeric_limits<std::size_t>::max();
        Terms options;
        for (Id k : orbit) {
            Id term = a.mul({norm,k});
            options.push_back(term);
            // The joint factor search below considers all orbit images. For wide
            // polynomials avoid building a separate factored sum for each image.
            if (n.args.size() > max_terms) continue;
            auto trial = seeds;
            trial.push_back(term);
            const auto size = a.cost(a.factorSum(a.add(trial)));
            if (size < cost) { cost = size; chosen = term; }
        }
        choices.push_back(std::move(options));
        seeds.push_back(chosen);
        sizes.push_back(orbit.size());
    }
    if (seeds.size() * 3 > n.args.size()) return {};
    Id kernel = compact_commuting_products(a,a.add(seeds));
    kernel = collect_scalar_factors(a,kernel,3);
    if (choices.size() >= 5 && choices.size() <= 160) {
        const bool wide = n.args.size() > max_terms;
        for (unsigned preference = 0; preference < (wide ? 4u : 2u); ++preference) {
            std::size_t budget = wide ? 500000 : 200000;
            auto factored = factor_choices(a,choices,preference % 2 != 0,budget,preference >= 2);
            factored = collect_scalar_factors(a,compact_commuting_products(a,factored),3);
            if (a.cost(factored) < a.cost(kernel) &&
                expression_complexity(a,factored) <= expression_complexity(a,kernel))
                kernel = factored;
        }
    }
    if (a.cost(kernel) + 16 >= a.cost(input)) return {};
    return permutation_sum{legs,kernel,std::move(sizes)};
}

} // namespace

std::optional<std::size_t> bounded_summand_weight(const Algebra& a, Id kernel) {
    try { return expression_complexity(a, kernel); }
    catch (const std::length_error&) {}
    catch (const std::overflow_error&) {}
    return {};
}

std::optional<Id> permute_current(Algebra &a, Id input, const std::vector<int> &from,
                                  const std::vector<int> &to) {
    auto domain = from, range = to;
    std::sort(domain.begin(), domain.end());
    std::sort(range.begin(), range.end());
    if (domain != range || std::adjacent_find(domain.begin(), domain.end()) != domain.end() ||
        (!domain.empty() && (domain.front() < 1 || static_cast<size_t>(domain.back()) > a.masses().size())))
        return {};
    for (std::size_t i = 0; i < from.size(); ++i)
        if (a.masses()[from[i] - 1] != a.masses()[to[i] - 1])
            return {};
    try {
        return relabel(a, from, to, true)(input);
    } catch (const Unsupported &) {
        return {};
    }
}

Id expand_permutation_groups(Algebra& a, Id kernel, const std::vector<std::vector<int>>& groups, bool alternating) {
    for (auto legs : groups) {
        std::sort(legs.begin(),legs.end());
        auto order = legs;
        Terms terms;
        do {
            Id image = relabel(a,legs,order)(kernel);
            if (alternating) {
                bool odd = false;
                for (std::size_t i = 0; i < order.size(); ++i)
                    for (std::size_t j = i + 1; j < order.size(); ++j)
                        if (order[i] > order[j]) odd = !odd;
                if (odd) image = a.mul({a.integer(-1),image});
            }
            terms.push_back(image);
        }
        while (std::next_permutation(order.begin(),order.end()));
        kernel = a.add(std::move(terms));
    }
    return kernel;
}

Id apply_exchanges(Algebra& a, Id kernel, const std::vector<signed_exchange>& exchanges) {
    for (const auto& op : exchanges) {
        std::vector<int> from, to;
        for (const auto& [left,right] : op.swaps) {
            from.insert(from.end(),{left,right});
            to.insert(to.end(),{right,left});
        }
        Id image = relabel(a,from,to)(kernel);
        kernel = a.add({kernel,a.mul({a.integer(op.sign),image})});
    }
    return kernel;
}
Id sum_projected_terms(Algebra& a, const std::vector<projected_kernel>& terms) {
    Terms sum;
    for (const auto& term : terms) sum.push_back(apply_exchanges(a,term.kernel,term.exchanges));
    return a.add(std::move(sum));
}
Id expand_signed_operators(Algebra& a, const permutation_sum& sum) {
    Id kernel = apply_exchanges(a,sum.kernel,sum.inner_exchanges);
    if (sum.joint_exchange) kernel = apply_exchanges(a,kernel,{*sum.joint_exchange});
    return expand_permutation_groups(a,kernel,sum.alternating_groups,true);
}

Id simplify_permutation_kernel(Algebra& a, Id input, const std::vector<int>& legs,
                               const std::vector<std::vector<int>>& other_groups) {
    if (legs.size() < 3 || legs.size() > 5 || a.cost(input) > 16000) return input;
    try {
        std::vector<std::vector<int>> groups{legs};
        groups.insert(groups.end(),other_groups.begin(),other_groups.end());
        auto sorted = legs;
        for (const auto& other:other_groups) sorted.insert(sorted.end(),other.begin(),other.end());
        std::sort(sorted.begin(),sorted.end());
        if (sorted.front() < 1 || static_cast<std::size_t>(sorted.back()) > a.masses().size() ||
            std::adjacent_find(sorted.begin(),sorted.end()) != sorted.end()) return input;
        const auto compact = kernel_reducer(a,groups).run(input);
        if (a.cost(compact) < a.cost(input) &&
            expression_complexity(a,compact) <= expression_complexity(a,input)) return compact;
    } catch (const Unsupported&) {
        return input;
    } catch (const std::length_error&) {
        return input;
    } catch (const std::overflow_error&) {
        return input;
    }
    return input;
}

// In a signed sum, K and sign(g) g(K) have identical projections. Try each
// involution individually to avoid enumerating a factorial product of groups.
// A spectator multiplying a subexpression must be invariant under the swap.
Id simplify_signed_kernel(Algebra& a, Id input, const permutation_sum& sum) {
    if (a.cost(input) > 16000 || (sum.alternating_groups.empty() && !sum.joint_exchange && sum.inner_exchanges.empty())) return input;
    try {
        std::vector<int> conjugate(a.masses().size()+1);
        std::iota(conjugate.begin(),conjugate.end(),0);
        Id best = input;
        auto attempt = [&](const std::vector<int>& from, const std::vector<int>& to, bool subtract) {
            auto candidate = kernel_reducer(a,from,to,subtract).run(best);
            if (a.cost(candidate) < a.cost(best) &&
                expression_complexity(a,candidate) <= expression_complexity(a,best)) best = candidate;
        };
        const signed_exchange* first = sum.inner_exchanges.empty()
            ? (sum.joint_exchange ? &*sum.joint_exchange : nullptr) : &sum.inner_exchanges.front();
        if (first) {
            std::vector<int> from,to;
            for (auto [l,r] : first->swaps) {
                if (l<=0 || r<=0 || static_cast<size_t>(std::max(l,r))>=conjugate.size() ||
                    r==l || conjugate[l]!=l || conjugate[r]!=r) return input;
                std::swap(conjugate[l],conjugate[r]);
                from.insert(from.end(),{l,r}); to.insert(to.end(),{r,l});
            }
            attempt(from,to,first->sign<0);
            // Only the innermost identity can always be used on the bare
            // summand. Later projectors can mix its leg classes.
            if (!sum.inner_exchanges.empty()) return best;
        }
        auto fold_groups = [&](const std::vector<std::vector<int>>& groups, bool subtract) {
            for (const auto& group : groups) for (size_t n=1; n<group.size(); ++n) {
                const auto l=group[n-1], r=group[n];
                if (l<=0 || r<=0 || static_cast<size_t>(std::max(l,r))>=conjugate.size()) continue;
                // H(1 +/- C)g = sign(g) H(1 +/- C) when both g and CgC
                // are transpositions in H with the same exchange sign.
                if (std::any_of(groups.begin(),groups.end(),[&](const auto& block) {
                    return std::find(block.begin(),block.end(),conjugate[l])!=block.end() &&
                           std::find(block.begin(),block.end(),conjugate[r])!=block.end();
                })) attempt({l,r},{r,l},subtract);
            }
        };
        auto symmetric=sum.other_groups;
        symmetric.insert(symmetric.begin(),sum.legs);
        fold_groups(symmetric,false);
        fold_groups(sum.alternating_groups,true);
        return best;
    } catch (const Unsupported&) {
        return input;
    } catch (const std::length_error&) {
        return input;
    } catch (const std::overflow_error&) {
        return input;
    }
}

static std::optional<permutation_sum> symmetric_permutations(Algebra& a, Id input) {
    // Equal external masses permit the relabeling. The terms and their signs
    // must still prove that the expression is invariant under every swap.
    const auto leaves = a.cost(input);
    if (leaves < 300 || leaves > 180000 || !a.shells().empty()) return {};
    std::map<Id,std::vector<int>> groups;
    for (std::size_t i = 0; i < a.masses().size(); ++i)
        groups[a.masses()[i]].push_back(static_cast<int>(i + 1));

    if (std::none_of(groups.begin(),groups.end(),[](const auto& group) {
            return group.second.size() >= 3 && group.second.size() <= 12;
        })) return {};
    // A single orbit on six labels can already contain 720 monomials.
    // Reserve the larger bounded expansion only for a matching external group.
    const bool wide = std::any_of(groups.begin(),groups.end(),[](const auto& group) {
        return group.second.size() >= 6 && group.second.size() <= 12;
    });
    if (!wide && leaves > 18000) return {};
    std::optional<permutation_sum> best;
    try {
        relabel canonical(a,{},{});
        std::size_t work = wide ? 600000 : 50000;
        const auto expanded = a.add(expand(a,canonical(input),work,wide ? 8192 : max_terms));
        const auto& polynomial = a.node(expanded);
        if (polynomial.head != "Plus" || polynomial.args.size() < 6) return {};
        std::set<Id> support(polynomial.args.begin(),polynomial.args.end());

        // Invariance under a transposition is an equivalence relation on the
        // labels (conjugate two swaps to obtain the third). A negative-helicity
        // photon or a fixed reference can therefore fall outside the orbit.
        for (const auto& group : groups) {
            const auto& legs = group.second;
            if (legs.size() < 3 || legs.size() > 12) continue;
            std::vector<std::vector<int>> classes;
            for (int leg : legs) {
                auto block = std::find_if(classes.begin(),classes.end(),[&](const auto& members) {
                    const int first = members.front();
                    relabel swap(a,{first,leg},{leg,first});
                    return std::all_of(support.begin(),support.end(),[&](Id term) {
                        const auto image = swap(term);
                        return support.count(image) && swap(image) == term;
                    });
                });
                if (block == classes.end()) classes.push_back({leg});
                else block->push_back(leg);
            }
            for (const auto& members : classes) {
                if (members.size() < 3) continue;
                auto sum = try_group(a,input,expanded,members);
                if (sum && (!best || a.cost(sum->kernel) < a.cost(best->kernel))) best = std::move(sum);
            }
        }
    } catch (const Unsupported&) {
        return {};
    } catch (const std::length_error&) {
        return {};
    } catch (const std::overflow_error&) {
        return {};
    }
    return best;
}

// Put one monomial from each signed orbit in the displayed summand. Equal
// masses and spin weights only suggest exchanges. Their signs and all
// coefficients, including the stabilizer multiplicities, are checked exactly.
static std::optional<permutation_sum> alternating_pairs(Algebra& a, Id input) {
    if (!a.shells().empty() || a.cost(input) < 160 || a.cost(input) > 100000)
        return {};
    const auto weights = external_spin_weights(a,input);
    if (!weights) return {};
    std::vector<std::vector<int>> pairs;
    for (std::size_t i=0; i<weights->size(); ++i) {
        if ((*weights)[i]%2 == 0) continue;
        for (std::size_t j=i+1; j<weights->size(); ++j)
            if ((*weights)[i]==(*weights)[j] && a.masses()[i]==a.masses()[j])
                pairs.push_back({int(i+1),int(j+1)});
    }
    if (pairs.empty() || pairs.size()>28) return {};
    std::size_t budget=500000;
    auto polynomial=[&](Id x){return a.add(expand(a,x,budget,8192));};
    const auto minus=a.integer(-1);
    const Id source=polynomial(relabel(a,{},{},true)(input));
    const auto& p=a.node(source);
    if (p.head!="Plus" || p.args.size()<4) return {};
    std::set<Id> terms(p.args.begin(),p.args.end());
    std::set<int> used;
    std::vector<std::vector<int>> groups;
    std::vector<relabel> swaps;
    for (const auto& pair:pairs) {
        if (used.count(pair[0]) || used.count(pair[1])) continue;
        relabel swap(a,pair,{pair[1],pair[0]},true);
        const auto has_partner=[&](Id term){return terms.count(a.mul({minus,swap(term)}));};
        if (!std::all_of(terms.begin(),terms.end(),has_partner)) continue;
        used.insert(pair.begin(),pair.end());
        groups.push_back(pair);
        swaps.push_back(std::move(swap));
        if (swaps.size()==4) break; // At most sixteen images per orbit.
    }
    if (swaps.empty()) return {};
    Terms seeds;
    while (!terms.empty()) {
        std::set<Id> orbit{*terms.begin()};
        for (auto& swap:swaps) {
            Terms images;
            for (Id t:orbit) images.push_back(a.mul({minus,swap(t)}));
            orbit.insert(images.begin(),images.end());
        }
        for (Id t:orbit) if (!terms.erase(t)) return {};
        mpq_class coeff(orbit.size(),std::size_t(1) << swaps.size());
        coeff.canonicalize();
        seeds.push_back(a.mul({a.exact(coeff),*orbit.begin()}));
    }
    const Id kernel=collect_scalar_factors(a,a.add(std::move(seeds)),3);
    if (a.cost(kernel)+16>=a.cost(input)) return {};
    return permutation_sum{{},kernel,{}, {},std::move(groups)};
}

std::optional<permutation_sum> compact_permutations(Algebra& a, Id input) {
    if (auto symmetric = symmetric_permutations(a,input)) return symmetric;
    try {
        return alternating_pairs(a,input);
    } catch (const Unsupported&) {
        return {};
    } catch (const std::length_error&) {
        return {};
    } catch (const std::overflow_error&) {
        return {};
    }
}

} // namespace hepcat
