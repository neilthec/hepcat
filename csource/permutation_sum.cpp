#include "permutation_sum.hpp"
#include "fermion_currents.hpp"
#include "generation_plan.hpp"
#include "chiral_radiation.hpp"
#include "mixed_radiation.hpp"
#include "spinor_word.hpp"
#include "mixed_physical_current.hpp"

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
        for (auto group : groups) {
            std::sort(group.begin(),group.end());
            from.insert(from.end(),group.begin(),group.end());
            std::vector<std::vector<int>> product;
            do {
                if (product.size()+images.size()>24) throw Unsupported{};
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

// The minority leg fixes the common chiral reference. A denominator of the
// opposite chirality reveals which companion was used as its square (or
// angle) reference in the elementary expression.
std::pair<bool,int> reference_in(const Algebra& a, Id polynomial, int minority, bool conjugate) {
    bool physical = true;
    for (Id term : a.node(polynomial).args) {
        const auto& product = a.node(term);
        for (Id factor : product.head == "Times" ? product.args : Terms{term}) {
            const auto& power = a.node(factor);
            mpq_class exponent;
            if (power.head != "Power" || power.args.size() != 2 || !a.number(power.args[1],exponent) || exponent >= 0) continue;
            const auto& chain = a.node(power.args[0]);
            if (chain.head != "SpinorChain") continue;
            physical = false;
            if (chain.args.size() != 2) continue;
            const auto l = spinor_detail::endpoint(a,chain.args.front());
            const auto r = spinor_detail::endpoint(a,chain.args.back());
            if (l.angle == conjugate && (l.leg == minority || r.leg == minority))
                return {false,l.leg == minority ? r.leg : l.leg};
        }
    }
    return {physical,0};
}

// Separate spinor chains and propagators from the scalar coefficient of each
// Laurent monomial. The scale subsequently pulled through the permutation
// sum may contain masses and couplings, all invariant under its leg swaps.
using Polynomial = std::map<Id,Id>;
Polynomial scalar_coefficients(Algebra& a, Id expanded) {
    Polynomial polynomial;
    const auto& sum = a.node(expanded);
    for (Id term : sum.head == "Plus" ? sum.args : Terms{expanded}) {
        Terms word, scalar;
        const auto& product = a.node(term);
        for (Id factor : product.head == "Times" ? product.args : Terms{term}) {
            const auto& power = a.node(factor);
            if (power.head == "Power" && power.args.size() != 2) throw Unsupported{};
            const auto& base = power.head == "Power" ? a.node(power.args.front()) : power;
            if (base.head == "SpinorChain" || base.head == "PropDen" || base.head == "MomProd") word.push_back(factor);
            else if (leg_independent_scalar(a,factor)) scalar.push_back(factor);
            else throw Unsupported{};
        }
        auto key = a.mul(std::move(word)), coeff = a.mul(std::move(scalar));
        auto [it,inserted] = polynomial.emplace(key,coeff);
        if (!inserted) it->second = a.add({it->second,coeff});
    }

    const Id zero = a.integer(0);
    for (auto it = polynomial.begin(); it != polynomial.end(); ) {
        if (it->second == zero) it = polynomial.erase(it);
        else ++it;
    }
    return polynomial;
}

std::optional<Id> current_scale(Algebra& a, const Polynomial& target, const Polynomial& current) {
    if (target.empty() || target.size() != current.size()) return {};
    auto expected = current.begin(), actual = target.begin();
    if (actual->first != expected->first) return {};
    // Invert scalar factors individually so an arbitrary pivot containing
    // both a mass and a coupling cancels before checking other coefficients.
    const auto& denominator = a.node(expected->second);
    Terms factors{actual->second};
    if (denominator.head == "Times") {
        for (Id factor : denominator.args) factors.push_back(a.pow(factor,-1));
    } else factors.push_back(a.pow(expected->second,-1));
    const auto scale = a.mul(std::move(factors));
    for (; actual != target.end(); ++actual, ++expected)
        if (actual->first != expected->first || a.mul({scale,expected->second}) != actual->second)
            return {};
    return scale;
}

// The two-leg photon exchange poles determine a bipartite graph. They suggest
// the two charge classes but do not establish any permutation identity.
std::optional<permutation_sum> recognize_exchange(Algebra& a, Id input, std::optional<bool> preference) {
    const auto& masses = a.masses();
    const auto zero = a.integer(0);
    const bool three_lines = std::count(masses.begin(),masses.end(),zero)==1 && masses.size()==7;
    if (a.cost(input) > (three_lines ? 300000 : 100000)) return {};
    std::vector<int> massive, photons, types(masses.size());
    const auto weights = external_spin_weights(a,input);
    if (!weights) return {};
    for (std::size_t i = 0; i < masses.size(); ++i) {
        if (masses[i] == zero) {
            if (std::abs((*weights)[i]) != 2) return {};
            photons.push_back(i+1);
            types[i] = -(*weights)[i];
        } else {
            if ((*weights)[i] != 1 || (!massive.empty() && masses[i] != masses[massive.front()-1])) return {};
            massive.push_back(i+1);
        }
    }
    std::set<std::pair<int,int>> edges;
    bool physical = true;
    std::set<Id> visited;
    auto scan = [&](auto& self, Id id) -> void {
        if (!visited.insert(id).second) return;
        const auto& node = a.node(id);
        if (node.head == "Power" && node.args.size() == 2) {
            mpq_class exponent;
            const auto& base = a.node(node.args[0]);
            if (a.number(node.args[1],exponent) && exponent < 0) {
                if (base.head == "SpinorChain") physical = false;
                if (base.head == "PropDen" && base.args.size() == 2 && base.args[1] == zero) {
                    const auto& mom = a.node(base.args[0]);
                    if (mom.head != "Mom" || mom.args.size() != 1) throw Unsupported{};
                    const auto& side = a.node(mom.args[0]);
                    if (side.head == "Multiparticle" && side.args.size() == 2) {
                        const int u = a.leg(side.args[0]), v = a.leg(side.args[1]);
                        if (!std::binary_search(massive.begin(),massive.end(),u) ||
                            !std::binary_search(massive.begin(),massive.end(),v)) throw Unsupported{};
                        edges.insert(std::minmax(u,v));
                    }
                }
            }
        }
        for (Id child : node.args) self(self,child);
    };
    relabel canonical(a,{},{});
    scan(scan,canonical(input));
    const size_t pairs=massive.size()/2;
    if (massive.empty() || edges.size()!=pairs*pairs) return {};
    std::vector<int> starts, ends;
    for (int leg : massive)
        (edges.count({massive.front(),leg}) ? ends : starts).push_back(leg);
    if (starts.size()!=pairs || ends.size()!=pairs) return {};
    for (int l : starts) for (int r : ends)
        if (!edges.count(std::minmax(l,r))) return {};
    const auto mass = masses[massive.front()-1];
    auto polynomial = [&](Id expr) {
        size_t work = 1000000;
        return scalar_coefficients(a,a.add(expand(a,canonical(expr),work,18000)));
    };
    if (three_lines) {
        // The graph of neutral two-body poles only proposes the two charge
        // classes. Regenerate the complete unit-coupling current and require
        // one common, external-leg-independent coefficient on every Laurent
        // monomial, including the signs of both Fermi permutations.
        const auto reference = radiative_detail::unit_vector_model(a.format(mass));
        const auto& fermion = reference.particles().at(1);
        const auto& vector = reference.particles().at(0);
        const auto target = polynomial(input);
        for (unsigned orientation=0; orientation<2; ++orientation) {
            if (orientation) starts.swap(ends);
            std::vector<std::string> names(masses.size(),fermion.antiname);
            for (int leg : starts) names[leg-1]=fermion.name;
            for (int leg : photons) names[leg-1]=types[leg-1]>0 ? vector.name : vector.antiname;
            std::optional<permutation_sum> proposed;
            const auto count = generation::diagram_plans(reference,names).size();
            const auto generated=fermion_current_proposal(a,reference,names,count,preference.value_or(physical),&proposed);
            if (!generated || !proposed) continue;
            const auto source=reduceInto(a,*generated,masses,{});
            auto scale=current_scale(a,target,polynomial(source));
            if (!scale) continue;
            for (auto& term : proposed->projected_terms) term.kernel=a.mul({*scale,term.kernel});
            proposed->kernel=a.mul({*scale,proposed->kernel});
            return proposed;
        }
        return {};
    }
    // Preserve the massless exchange pole in every Laurent word. No contact
    // term or term with a second massless propagator can pass this check.
    std::map<Id,Polynomial> allocations;
    for (auto [word,value] : polynomial(input)) {
        Id pole = zero;
        for (Id factor : a.node(word).args) {
            const auto& power = a.node(factor);
            if (power.head != "Power" || power.args.size() != 2) continue;
            const auto& den = a.node(power.args[0]);
            if (den.head != "PropDen" || den.args.size() != 2 || den.args[1] != zero) continue;
            if (pole != zero || power.args[1] != a.integer(-1)) return {};
            pole = power.args[0];
        }
        if (pole == zero) return {};
        allocations[pole][word] = value;
    }
    if (allocations.size() != (2u << photons.size())) return {};
    for (int orientation=0; orientation<2; ++orientation) {
        if (orientation) starts.swap(ends);
        for (int i : starts) types[i-1] = 1;
        for (int i : ends) types[i-1] = -1;
        std::optional<Id> scale;
        auto matches = [&](Id pole, Id current) {
            const auto found = allocations.find(pole);
            if (found == allocations.end()) return false;
            const auto candidate = reduceInto(a,current,masses,{});
            const auto ratio = current_scale(a,found->second,polynomial(candidate));
            if (!ratio || (scale && *ratio != *scale)) return false;
            scale = ratio;
            return true;
        };
        if (!verify_radiation_exchange(a,starts,ends,photons,types,physical,matches)) continue;
        return radiation_exchange_sum(a,starts,ends,photons,types,mass,*scale,preference.value_or(physical));
    }
    return {};
}

} // namespace

permutation_sum radiation_exchange_sum(Algebra& a, const std::vector<int>& fermions,
                                      const std::vector<int>& antifermions, const std::vector<int>& photons,
                                      const std::vector<int>& types, Id mass, Id coefficient, bool physical) {
    const int l = fermions.at(0), r = antifermions.at(0);
    const int u = fermions.at(1), v = antifermions.at(1);
    std::vector<int> positive, negative;
    for (int i : photons) (types.at(i-1) > 0 ? positive : negative).push_back(i);
    const bool square = positive.size() > negative.size();
    const auto& majority = square ? positive : negative;
    const auto& minority = square ? negative : positive;
    const Id seed = minority.empty()
        ? chiral_radiation_kernel(a,l,r,u,v,majority,mass,square,physical)
        : mixed_radiation_kernel(a,l,r,u,v,majority.at(0),majority.at(1),minority.at(0),mass,square,physical);
    // Each external vector changes sign under charge conjugation. Reversing
    // both currents exchanges the two ends of the internal photon as well.
    const int charge_parity = photons.size()%2 == 0 ? 1 : -1;
    const signed_exchange conjugate{{{l,r},{u,v}},charge_parity};
    return {majority,a.mul({coefficient,seed}),{}, {},{fermions,antifermions},conjugate};
}

// Infer the candidate from external spin weights and denominator chirality.
// Only coefficient-by-coefficient agreement authorizes the ordered identity.
std::optional<permutation_sum> recognize_ordered_current(Algebra& a, Id input, std::optional<bool> preference) {
    const auto& masses = a.masses();
    const auto zero = a.integer(0);
    if (masses.size() < 6 || masses.size() > 8 || !a.shells().empty() || a.cost(input) < 300)
        return {};
    const auto massless = static_cast<std::size_t>(std::count(masses.begin(),masses.end(),zero));
    try {
        if (masses.size() == 7 && (massless == 3 || massless == 1)) return recognize_exchange(a,input,preference);
        if (massless + 2 != masses.size() || a.cost(input) > 18000) return {};
        const auto weights = external_spin_weights(a,input);
        if (!weights) return {};
        std::vector<int> ends, positive, negative;
        for (std::size_t i=0; i<masses.size(); ++i) {
            const int w = (*weights)[i];
            if (masses[i] != zero && w == 1) ends.push_back(i+1);
            else if (masses[i] == zero && w == -2) positive.push_back(i+1);
            else if (masses[i] == zero && w == 2) negative.push_back(i+1);
            else return {};
        }
        const auto count = std::min(positive.size(),negative.size());
        const bool paired = count == 2 && positive.size()+negative.size() == 5;
        if (ends.size() != 2 || masses[ends[0]-1] != masses[ends[1]-1] ||
            (count != 1 && !paired)) return {};
        relabel canonical(a,{},{});
        auto expanded = [&](Id expr) {
            std::size_t work = paired ? 200000 : 50000;
            return a.add(expand(a,canonical(expr),work,paired ? 4096 : max_terms));
        };
        const auto original = expanded(input);
        const auto& sum = a.node(original);
        if (sum.head != "Plus" || sum.args.size() < std::max(positive.size(),negative.size())) return {};
        const bool conjugate = negative.size() > positive.size();
        auto& majority = conjugate ? negative : positive;
        auto& minorities = conjugate ? positive : negative;
        const int minority = minorities.front();
        const auto [physical,reference] = reference_in(a,original,minority,conjugate);
        if (reference) {
            const auto it = std::find(majority.begin(),majority.end(),reference);
            if (it == majority.end()) return {};
            std::rotate(majority.begin(),it,it+1);
        }
        const auto target = scalar_coefficients(a,original);
        const Id m = masses[ends.front()-1], unit = a.integer(1);
        // A stored expression can put either minority leg at the common
        // reference. Only an exact match of the complete coefficient map
        // licenses a new gauge or a new current representation.
        const unsigned routings = paired && !physical ? 2 : 1;
        for (unsigned routing=0; routing<routings; ++routing) {
            if (routing) std::swap(minorities.front(),minorities.back());
            for (int l : ends) {
                const int r = l == ends.front() ? ends.back() : ends.front();
                std::optional<permutation_sum> ordered;
                auto current = physical
                    ? physical_current_with_presentation(a,l,r,m,positive,negative,unit,&ordered)
                    : mixed_spurious_current(a,l,r,m,positive,negative,unit);
                if (!current || (physical && !paired && !ordered)) continue;
                const auto reduced = reduceInto(a,*current,masses,{});
                const auto scale = current_scale(a,target,scalar_coefficients(a,expanded(reduced)));
                if (!scale) continue;
                const bool output_physical = preference.value_or(physical);
                if (paired) ordered = paired_weyl_sum(a,l,r,m,positive,negative,unit,output_physical);
                else if (!output_physical) ordered = ordered_minority_sum(a,l,r,m,positive,negative,unit);
                else if (!physical) physical_current_with_presentation(a,l,r,m,positive,negative,unit,&ordered);
                if (ordered) {
                    ordered->kernel = a.mul({*scale,ordered->kernel});
                    std::sort(ordered->legs.begin(),ordered->legs.end());
                    for (auto& group : ordered->other_groups) std::sort(group.begin(),group.end());
                    return ordered;
                }
            }
        }
    } catch (const Unsupported&) {
        return {};
    } catch (const std::length_error&) {
        return {};
    } catch (const std::overflow_error&) {
        return {};
    }
    return {};
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
    if (legs.size() < 3 || legs.size() > 4 || a.cost(input) > 16000) return input;
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

std::optional<permutation_sum> compact_permutations(Algebra& a, Id input) {
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

} // namespace hepcat
