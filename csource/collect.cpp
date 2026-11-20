// Compare exact sparse normal forms in a small number of external-momentum
// bases. A basis choice is shared by every occurrence of an oriented chain.
#include "reduce.hpp"
#include "vector_basis.hpp"

#include <algorithm>
#include <array>
#include <set>
#include <limits>
#include <optional>
#include <unordered_map>
#include <unordered_set>

namespace hepcat {
namespace {
using Terms = std::vector<Id>;
using Rank = std::pair<std::size_t, std::size_t>;
constexpr std::size_t maxTerms = 4096;
constexpr std::size_t maxProducts = 60000;
constexpr std::size_t maxScore = std::numeric_limits<std::size_t>::max()/2;
std::size_t cappedMul(std::size_t a, std::size_t b) {
    return a && b > maxScore/a ? maxScore : a*b;
}
struct TooLarge {};

int spinLeg(const Algebra& a, Id spin) {
    const auto& n = a.node(spin);
    if (n.head != "Spinor" || (n.args.size()!=3 && n.args.size()!=5)) return 0;
    const auto& kind=a.node(n.args[0]).head;
    if (kind!="\"Spin\"" && kind!="\"Helicity\"") return 0;
    auto position=n.args.size()==5 ? 2 : 1;
    const auto& chirality=a.node(n.args[position]).head;
    if (chirality!="\"Square\"" && chirality!="\"Angle\"") return 0;
    if (position==2) {
        const auto& variance=a.node(n.args[1]).head;
        if (kind!="\"Spin\"" || (variance!="\"Upper\"" && variance!="\"Lower\"")) return 0;
    }
    int leg=a.leg(n.args[position+1]);
    return static_cast<std::size_t>(leg)<=a.masses().size() ? leg : 0;
}

bool ordinaryMomentum(const Algebra& a, Id value) {
    const auto& mom=a.node(value);
    if (mom.head!="Mom" || mom.args.size()!=1) return false;
    const auto& data=a.node(mom.args[0]);
    auto external=[&](Id index) {
        int leg=a.leg(index);
        return leg>0 && static_cast<std::size_t>(leg)<=a.masses().size();
    };
    if (data.head=="Multiparticle")
        return !data.args.empty() && std::all_of(data.args.begin(),data.args.end(),external);
    return external(mom.args[0]);
}

int chirality(const Algebra& a, Id endpoint) {
    if (!spinLeg(a,endpoint)) return 0;
    const auto& s=a.node(endpoint);
    const auto& ch=a.node(s.args[s.args.size()==5 ? 2 : 1]);
    return ch.head=="\"Square\"" ? -1 : 1;
}

// Subtracting the coefficient of the pivot from every momentum coefficient
// uses sum_i p_i = 0. Prefer a nonendpoint pivot so that a momentum on either
// endpoint can subsequently disappear by its own external Dirac equation.
Id projectChain(Algebra& a, Id value, unsigned order) {
    const auto& n = a.node(value);
    if (n.head != "SpinorChain" || n.args.size()!=3) return value;
    int left = spinLeg(a,n.args.front()), right = spinLeg(a,n.args.back());
    if (!left || !right || left==right ||
        chirality(a,n.args.front())!=-chirality(a,n.args.back()) ||
        !ordinaryMomentum(a,n.args[1])) return value;
    int count = static_cast<int>(a.masses().size());
    const auto& slot = a.node(n.args[1]);
    if (slot.head != "Mom" || slot.args.size()!=1) return value;
    const auto& label = a.node(slot.args[0]);
    Terms members = label.head=="Multiparticle" ? label.args : Terms{slot.args[0]};
    std::vector<int> weight(count+1);
    for (Id member : members) {
        int i = a.leg(member);
        if (i<1 || i>count) return value;
        ++weight[i];
    }
    int pivot = 0;
    // The extra trial keeps the original momentum basis and only expands sums.
    if (order<static_cast<unsigned>(count)) {
        for (int j=0; j<count && !pivot; ++j) {
            int leg = 1+(order+j)%count;
            if (leg!=left && leg!=right) pivot=leg;
        }
        if (!pivot) return value;
    }
    Terms output;
    for (int i=1; i<=count; ++i) {
        int coefficient=weight[i]-weight[pivot];
        if (!coefficient) continue;
        Id changed=a.call("SpinorChain",{n.args.front(),a.call("Mom",{a.integer(i)}),n.args.back()});
        output.push_back(a.mul({a.integer(coefficient), reduceSpinor(a,changed)}));
    }
    return a.add(std::move(output));
}

// Numeric coefficients and repeated factors are combined by Algebra at each
// multiplication boundary. Inverse powers are indivisible coefficients here.
class Expansion {
public:
    Expansion(Algebra& a, int pivot, std::size_t terms = maxTerms, std::size_t products = maxProducts, int power_limit = 8)
        : algebra(a), order(pivot), term_limit(terms), product_limit(products), exponent_limit(power_limit) {}
    Id run(Id id) {
        auto cached=memo.find(id);
        if (cached!=memo.end()) return cached->second;
        const auto& n=algebra.node(id);
        Id result=id;
        if (n.head=="Plus") {
            Terms terms;
            for (Id term : n.args) terms.push_back(run(term));
            result=algebra.add(std::move(terms));
        } else if (n.head=="Times") {
            result=algebra.integer(1);
            for (Id factor : n.args) result=product(result,run(factor));
        } else if (n.head=="Power" && n.args.size()==2) {
            int exponent=algebra.leg(n.args[1]);
            if (exponent>exponent_limit && (algebra.node(n.args[0]).head=="Plus" || algebra.node(n.args[0]).head=="Times"))
                throw TooLarge{};
            if (exponent && exponent<=exponent_limit) {
                Id base=run(n.args[0]);
                if (algebra.node(base).head=="Plus") {
                    result=base;
                    for (int j=1; j<exponent; ++j) result=product(result,base);
                } else result=algebra.pow(base,exponent);
            }
        } else if (order>=0 && n.head=="SpinorChain") {
            result=projectChain(algebra,id,order);
        }
        if (algebra.node(result).head=="Plus" && algebra.node(result).args.size()>term_limit) throw TooLarge{};
        memo.emplace(id,result);
        return result;
    }
private:
    Algebra& algebra;
    int order;
    std::size_t term_limit, product_limit;
    int exponent_limit;
    std::size_t work=0;
    std::unordered_map<Id,Id> memo;
    Id product(Id x, Id y) {
        const auto& nx=algebra.node(x); const auto& ny=algebra.node(y);
        const bool xsum=nx.head=="Plus", ysum=ny.head=="Plus";
        if (!xsum && !ysum) return algebra.mul({x,y});
        const Terms xs=xsum ? nx.args : Terms{x}, ys=ysum ? ny.args : Terms{y};
        if (xs.size() > term_limit/ys.size() || (work+=xs.size()*ys.size())>product_limit) throw TooLarge{};
        Terms terms;
        terms.reserve(xs.size()*ys.size());
        for (Id u : xs) for (Id v : ys) terms.push_back(algebra.mul({u,v}));
        return algebra.add(std::move(terms));
    }
};

struct Measure { std::size_t branches=1, degree=0; };
Measure measure(const Algebra& a, Id id) {
    const auto& n=a.node(id);
    if (n.head=="Times") {
        Measure result;
        for (Id factor : n.args) {
            auto m=measure(a,factor);
            result.branches=cappedMul(result.branches,m.branches);
            result.degree=std::min(maxScore,result.degree+m.degree);
        }
        return result;
    }
    if (n.head=="Power" && n.args.size()==2) {
        const int power=a.leg(n.args[1]);
        if (!power) return {};
        auto m=measure(a,n.args[0]);
        m.degree=cappedMul(m.degree,power);
        if (m.branches>1) {
            auto branches=m.branches;
            for (int j=1; j<power && m.branches<maxScore; ++j) m.branches=cappedMul(m.branches,branches);
        }
        return m;
    }
    if (n.head=="MomProd" || n.head=="Mandelstahm") return {1,2};
    if (n.head=="Mom") return {1,1};
    if (n.head!="SpinorChain" && n.head!="SpinorTrace") return {};
    std::size_t start=n.head=="SpinorChain" ? 1 : 0;
    if (n.args.size()<2*start) return {};
    Measure result{1,n.args.size()-2*start};
    for (std::size_t i=start; i<n.args.size()-start; ++i) {
        const auto& slot=a.node(n.args[i]);
        if (slot.head=="Mom" && slot.args.size()==1) {
            const auto& label=a.node(slot.args[0]);
            if (label.head=="Multiparticle" && label.args.size()>=2) {
                std::set<Id> members;
                for (Id value : label.args) {
                    mpq_class literal;
                    if (!a.number(value,literal) || literal.get_den()!=1 || !members.insert(value).second) break;
                }
                if (members.size()==label.args.size()) result.branches=cappedMul(result.branches,members.size());
            }
        }
    }
    return result;
}

std::size_t score(const Algebra& a, Id expanded) {
    const auto& n=a.node(expanded);
    if (n.head!="Plus") {
        Measure m=measure(a,expanded);
        return cappedMul(m.branches,1+cappedMul(4,m.degree));
    }
    std::size_t total=0;
    for (Id term : n.args) total=std::min(maxScore,total+score(a,term));
    return total;
}

// Collect common literal factors from subsets of the terms as well as across
// the whole sum. A product remains factored at its original pole.
Id factor(Algebra& a, Id expanded, unsigned depth=0) {
    const auto& n=a.node(expanded);
    if (n.head!="Plus" || n.args.size()<2 || depth>60) return expanded;
    std::map<Id,std::size_t> freq;
    for (Id term : n.args) {
        const auto& t=a.node(term);
        if (t.head=="Times") for (Id f : t.args) ++freq[f];
        else ++freq[term];
    }
    std::size_t best=0;
    Id atom=0;
    for (const auto& entry : freq) {
        if (entry.second < 2) continue;
        std::size_t gain=(entry.second-1)*a.cost(entry.first);
        if (gain>best) {atom=entry.first; best=gain;}
    }
    if (!best) return expanded;
    Terms included,others;
    for (Id term : n.args) {
        const auto& t=a.node(term);
        Terms factors=t.head=="Times" ? t.args : Terms{term};
        auto pos=std::find(factors.begin(),factors.end(),atom);
        if (pos==factors.end()) others.push_back(term);
        else {
            factors.erase(pos);
            included.push_back(a.mul(std::move(factors)));
        }
    }
    Id grouped=factor(a,a.add(std::move(included)),depth+1);
    Id outside=factor(a,a.add(std::move(others)),depth+1);
    Id result=a.add({a.mul({atom,grouped}),outside});
    return a.cost(result)<a.cost(expanded) ? result : expanded;
}

// Expose a Dirac endpoint with the two-component determinant identity.
// Try a bare bracket times a one-momentum sandwich. This is a two-output
// identity. Keeping both outputs includes every off-shell remainder.
Id schoutenPair(Algebra& a, Id bracket, Id sandwich, bool cutAtRight) {
    const auto& u=a.node(bracket).args;
    const auto& v=a.node(sandwich).args;
    if (!ordinaryMomentum(a,v[1])) return bracket;
    int type=chirality(a,u.front());
    int v1=chirality(a,v.front()), v2=chirality(a,v.back());
    if (!type || chirality(a,u.back())!=type || !v1 || v1!=-v2 ||
        (cutAtRight ? v2 : v1)!=type) return bracket;
    auto sew=[&](Id left, bool inserted, Id right) {
        Terms w{left};
        if (inserted) w.push_back(v[1]);
        w.push_back(right);
        return reduceSpinor(a,a.call("SpinorChain",std::move(w)));
    };
    Id t1=a.mul({sew(v.back(),!cutAtRight,u.back()),sew(v.front(),cutAtRight,u.front())});
    Id t2=a.mul({sew(v.back(),!cutAtRight,u.front()),sew(v.front(),cutAtRight,u.back())});
    int sign=cutAtRight ? 1 : -1;
    return a.add({a.mul({a.integer(sign),t1}),a.mul({a.integer(-sign),t2})});
}

// An exact sparse polynomial in spinor monomials. Isolating the rational
// coefficient lets an identity cancel a fraction of a term or several terms
// with different overall coefficients, as often occurs after sewing.
using Polynomial = TensorCoordinates;

void accumulate(Algebra& a, Polynomial& poly, Id expression, long sign=1) {
    const auto& n=a.node(expression);
    if (n.head=="Plus") {
        for (auto term : n.args) accumulate(a,poly,term,sign);
        return;
    }
    mpq_class coeff(1);
    Id mono=expression;
    if (a.number(expression,coeff)) mono=a.integer(1);
    else if (n.head=="Times" && !n.args.empty() && a.number(n.args.front(),coeff))
        mono=a.mul(Terms(n.args.begin()+1,n.args.end()));
    auto& c=poly[mono];
    c+=sign*coeff;
    if (c==0) poly.erase(mono);
}

Id polynomialExpr(Algebra& a,const Polynomial& poly) {
    Terms terms;
    for (const auto& [monomial,coefficient] : poly)
        terms.push_back(a.mul({a.exact(coefficient),monomial}));
    return a.add(std::move(terms));
}

// Ordinary Schouten on two bare brackets, oriented to match their supplied
// endpoints: (ab)(cd) = (ac)(bd) - (ad)(bc).
std::optional<Id> bareExchange(Algebra& a, const Terms& u, const Terms& v) {
    auto type=chirality(a,u.front());
    if (!type || chirality(a,u.back())!=type || chirality(a,v.front())!=type ||
        chirality(a,v.back())!=type) return std::nullopt;
    auto bracket=[&](Id l,Id r) { return reduceSpinor(a,a.call("SpinorChain",{l,r})); };
    return a.add({a.mul({bracket(u[0],v[0]),bracket(u[1],v[1])}),
        a.mul({a.integer(-1),bracket(u[0],v[1]),bracket(u[1],v[0])})});
}

Terms rewrites(Algebra& a,Id unit) {
    const auto& n=a.node(unit);
    Terms factors=n.head=="Times" ? n.args : Terms{unit};
    struct Factor { Id base; int power; std::size_t slot; };
    std::vector<Factor> chains;
    for (size_t i=0;i<factors.size();++i) {
        Id base=factors[i];
        int power=1;
        const auto& f=a.node(base);
        if (f.head=="Power" && f.args.size()==2) {
            base=f.args[0];
            power=a.leg(f.args[1]);
        }
        const auto& chain=a.node(base);
        if (power>0 && chain.head=="SpinorChain" && (chain.args.size()==2 || chain.args.size()==3))
            chains.push_back({base,power,i});
    }
    // Cap the quadratic pair enumeration independently of powers.
    if (chains.size()>12) return {};
    Terms alternatives;
    auto withSwap=[&](const Factor& f,const Factor* g,Id replacement) {
        auto rest=factors;
        rest[f.slot]=a.pow(f.base,f.power-1);
        if (g) rest[g->slot]=a.pow(g->base,g->power-1);
        rest.push_back(replacement);
        const auto alt=Expansion(a,-1).run(a.mul(std::move(rest)));
        if (alt!=unit && std::find(alternatives.begin(),alternatives.end(),alt)==alternatives.end())
            alternatives.push_back(alt);
    };
    for (size_t i=0;i<chains.size();++i) {
        const auto& f=chains[i];
        const auto& u=a.node(f.base).args;
        if (u.size()==3 && ordinaryMomentum(a,u[1]) && a.node(a.node(u[1]).args[0]).args.empty() &&
            chirality(a,u[0]) && chirality(a,u[0])==-chirality(a,u[2])) {
            int leg=a.leg(a.node(u[1]).args[0]);
            Terms sum;
            for (size_t j=1;j<=a.masses().size();++j) {
                if (int(j)==leg) continue;
                sum.push_back(reduceSpinor(a,a.call("SpinorChain",{u[0],a.call("Mom",{a.integer(j)}),u[2]})));
            }
            withSwap(f,nullptr,a.mul({a.integer(-1),a.add(std::move(sum))}));
        }
        for (size_t j=i+1;j<chains.size();++j) {
            const auto& g=chains[j];
            const auto& v=a.node(g.base).args;
            if (u.size()==3 && v.size()==3) {
                if (auto exchange=commonMomentumSwap(a,f.base,g.base)) withSwap(f,&g,*exchange);
            } else if (u.size()==2 && v.size()==2) {
                auto exchange=bareExchange(a,u,v);
                if (exchange) withSwap(f,&g,*exchange);
            } else {
                Id bare=u.size()==2 ? f.base : g.base;
                Id slash=u.size()==3 ? f.base : g.base;
                for (bool right : {false,true}) {
                    Id next=schoutenPair(a,bare,slash,right);
                    if (next!=bare) withSwap(f,&g,next);
                }
            }
        }
    }
    return alternatives;
}

using Relations = std::map<Id,std::vector<Polynomial>>;
Id sparseDescent(Algebra& a,Id source,Relations& relations) {
    const auto& n=a.node(source);
    if (n.head!="Plus" || n.args.size()>220) return source;
    Polynomial value;
    accumulate(a,value,source);
    std::map<Id,long> weights;
    auto weight=[&](Id mono) {
        auto [it,added]=weights.emplace(mono,0L);
        if (added) it->second=static_cast<long>(std::min<std::size_t>(1000000,score(a,mono)));
        return it->second;
    };
    // Only a scale that zeros an existing coordinate can improve support.
    // Compare the weight removed at each exact scale with the weight introduced.
    for (int iteration=0;iteration<256;++iteration) {
        long bestGain=0;
        const Polynomial* best=nullptr;
        mpq_class scale;
        for (const auto& [sourceID,sourceCoeff] : value) {
            auto [cache,fresh]=relations.try_emplace(sourceID);
            if (fresh) {
                for (Id alt : rewrites(a,sourceID)) {
                    Polynomial direction;
                    accumulate(a,direction,alt);
                    accumulate(a,direction,sourceID,-1);
                    if (!direction.empty() && direction.size()<=10) cache->second.push_back(std::move(direction));
                }
            }
            for (const auto& direction : cache->second) {
                long penalty=0;
                std::map<mpq_class,long> hits;
                for (const auto& [id,coef] : direction) {
                    auto existing=value.find(id);
                    if (existing==value.end()) penalty+=weight(id);
                    else hits[-existing->second/coef]+=weight(id);
                }
                for (const auto& [candidate,saved] : hits) {
                    if (saved-penalty>bestGain) {
                        bestGain=saved-penalty;
                        best=&direction;
                        scale=candidate;
                    }
                }
            }
        }
        if (!best) break;
        for (const auto& [id,coef] : *best) {
            auto& entry=value[id];
            entry+=scale*coef;
            if (entry==0) value.erase(id);
        }
    }
    return polynomialExpr(a,value);
}

// Build an identity neighborhood only for ordinary coherent four-leg tensors.
// Each massive spin state occurs exactly twice in every eligible monomial.
bool coherentTensor(const Algebra& a,Id source) {
    if (a.masses().size()!=4) return false;
    for (Id mass : a.masses()) if (a.node(mass).head=="0") return false;
    const auto& n=a.node(source);
    auto check=[&](Id term) {
        const auto& t=a.node(term);
        std::array<unsigned,4> occurrences{};
        for (Id f : (t.head=="Times" ? t.args : Terms{term})) {
            Id base=f;
            int power=1;
            if (a.node(f).head=="Power" && a.node(f).args.size()==2) {
                base=a.node(f).args[0];
                power=a.leg(a.node(f).args[1]);
            }
            const auto& s=a.node(base);
            if (s.head!="SpinorChain") continue;
            if (power<1 || power>2 || s.args.size()<2 || s.args.size()>3) return false;
            if (s.args.size()==3 && !ordinaryMomentum(a,s.args[1])) return false;
            for (Id end : {s.args.front(),s.args.back()}) {
                const auto& k=a.node(end);
                if (k.head!="Spinor" || k.args.size()!=3 || a.node(k.args[0]).head!="\"Spin\"") return false;
                int label=spinLeg(a,end);
                if (!label) return false;
                occurrences[label-1]+=power;
            }
        }
        return std::all_of(occurrences.begin(),occurrences.end(),[](auto i){return i==2;});
    };
    return n.head=="Plus" && !n.args.empty() && n.args.size()<=256 &&
           std::all_of(n.args.begin(),n.args.end(),check);
}

// Use a common momentum basis for the source and both sides of every identity.
// Contracting the resulting Dirac endpoints merges different witnesses onto
// the same sparse coordinates. Terms can then cancel across several identities.
Id tensorSupport(Algebra& a,Id source,unsigned order) {
    Id normalized=Expansion(a,order).run(source);
    Polynomial coordinates;
    accumulate(a,coordinates,normalized);
    if (coordinates.empty() || coordinates.size()>150) return source;
    std::vector<Polynomial> rows;
    std::set<Polynomial> seenRows;
    std::set<Id> visited, front;
    std::map<Id,std::size_t> weights;
    for (const auto& [id,c] : coordinates) {
        front.insert(id);
        weights[id]=score(a,id);
    }
    Polynomial best=coordinates;
    for (int depth=0;depth<2;++depth) {
        std::set<Id> next;
        for (Id id : front) {
            if (!visited.insert(id).second) continue;
            for (Id alt : rewrites(a,id)) {
                Id projected=Expansion(a,order).run(alt);
                Polynomial row;
                accumulate(a,row,projected);
                accumulate(a,row,id,-1);
                if (row.empty()) continue;
                // Different exchange orders often give the same rational row.
                const mpq_class leading=row.begin()->second;
                for (auto& [key,c] : row) {
                    c/=leading;
                    if (!visited.count(key)) next.insert(key);
                    if (!weights.count(key)) weights[key]=score(a,key);
                }
                if (!seenRows.insert(row).second) continue;
                rows.push_back(std::move(row));
                if (rows.size()>=1024) break;
            }
            if (rows.size()>=1024) break;
        }
        if (rows.empty()) break;
        best=reduceTensorSupport(best,rows,weights);
        if (next.empty() || weights.size()>400 || rows.size()>=1024) break;
        front=std::move(next);
    }
    return polynomialExpr(a,best);
}

// An exchange diagram's different propagators can favour different momentum
// bases. Group by inverse spectators, keeping external-mass powers inside a
// group because the Dirac equation can change those powers.
Id bestGroup(Algebra& a, Id group) {
    Relations relations;
    std::unordered_set<Id> seen;
    Id best=group;
    Rank rank{score(a,group),a.cost(group)};
    auto consider = [&](Id expanded) {
        const auto complexity=score(a,expanded);
        if (complexity>rank.first) return;
        Id candidate=factor(a,expanded);
        Rank candidateRank{complexity,a.cost(candidate)};
        if (candidateRank<rank) {
            best=candidate;
            rank=candidateRank;
        }
    };
    unsigned count=static_cast<unsigned>(a.masses().size());
    for (int basis=-1; basis<=static_cast<int>(count); ++basis) {
        try {
            Id expanded=basis<0 ? group : Expansion(a,basis).run(group);
            Id contracted=reduceSpinor(a,expanded);
            if (expanded!=contracted) expanded=Expansion(a,-1).run(contracted);
            if (seen.insert(expanded).second) consider(sparseDescent(a,expanded,relations));
        } catch (const TooLarge&) {
            // A different elimination order may generate fewer terms.
        }
    }
    if (coherentTensor(a,group)) {
        for (unsigned pivot=0; pivot<count; ++pivot) {
            try {
                Id expanded=Expansion(a,-1).run(best);
                consider(tensorSupport(a,expanded,pivot));
            } catch (const TooLarge&) {
                // Other completed frames remain usable when this one expands too far.
            }
        }
    }
    return best;
}

} // namespace

Id collectCandidate(Algebra& a, Id input) {
    if (a.masses().size()>12) return input;
    try {
        Id initial=Expansion(a,-1).run(input);
        const auto& sum=a.node(initial);
        if (sum.head!="Plus") return input;
        std::map<Terms,Terms> groups;
        for (Id term : sum.args) {
            const auto& t=a.node(term);
            Terms numerator, inverses;
            for (Id f : (t.head=="Times" ? t.args : Terms{term})) {
                const auto& n=a.node(f);
                if (n.head=="Power" && n.args.size()==2 && !a.leg(n.args[1]) &&
                    std::find(a.masses().begin(),a.masses().end(),n.args[0])==a.masses().end())
                    inverses.push_back(f);
                else numerator.push_back(f);
            }
            groups[inverses].push_back(a.mul(std::move(numerator)));
        }
        if (groups.size()>64) return input;
        Terms results;
        for (const auto& entry : groups)
            results.push_back(a.mul({a.mul(entry.first), bestGroup(a,a.add(entry.second))}));
        Id candidate=a.add(std::move(results));
        if (a.node(candidate).head=="Plus") candidate=factor(a,candidate);
        Rank next{score(a,Expansion(a,-1).run(candidate)),a.cost(candidate)};
        Rank old{score(a,initial),a.cost(input)};
        if (next<old) {
            // Collection can expose contractions and common factors inside
            // the accepted form. One local pass avoids another basis search.
            Id polished=a.simplify(candidate);
            if (polished!=candidate) {
                try {
                    Rank finalRank{score(a,Expansion(a,-1).run(polished)),a.cost(polished)};
                    if (finalRank<next) return polished;
                } catch (const TooLarge&) {
                    // Keep the measured improvement made before polishing.
                }
            }
            return candidate;
        }
    } catch (const TooLarge&) {
        return input;
    }
    return input;
}
std::size_t expression_complexity(const Algebra& source, Id root) {
    Algebra trial(&source);
    try {
        auto expanded = Expansion(trial, -1, 131072, 4000000, 4096).run(root);
        if (expanded == trial.integer(0)) return 0;
        auto result = score(trial, expanded);
        if (result >= maxScore) throw std::overflow_error("expression complexity exceeds the numeric range");
        return result;
    } catch (const TooLarge&) {
        throw std::length_error("complexity expansion exceeds the monomial budget");
    }
}
} // namespace hepcat
