// Compare exact sparse normal forms in a small number of external-momentum
// bases. A basis choice is shared by every occurrence of an oriented chain.
#include "reduce.hpp"

#include <algorithm>
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
    Expansion(Algebra& a, int pivot) : algebra(a), order(pivot) {}
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
            if (exponent>8 && (algebra.node(n.args[0]).head=="Plus" || algebra.node(n.args[0]).head=="Times"))
                throw TooLarge{};
            if (exponent && exponent<=8) {
                Id base=run(n.args[0]);
                if (algebra.node(base).head=="Plus") {
                    result=base;
                    for (int j=1; j<exponent; ++j) result=product(result,base);
                } else result=algebra.pow(base,exponent);
            }
        } else if (order>=0 && n.head=="SpinorChain") {
            result=projectChain(algebra,id,order);
        }
        if (algebra.node(result).head=="Plus" && algebra.node(result).args.size()>maxTerms) throw TooLarge{};
        memo.emplace(id,result);
        return result;
    }
private:
    Algebra& algebra;
    int order;
    std::size_t work=0;
    std::unordered_map<Id,Id> memo;
    Id product(Id x, Id y) {
        const auto& nx=algebra.node(x); const auto& ny=algebra.node(y);
        const bool xsum=nx.head=="Plus", ysum=ny.head=="Plus";
        if (!xsum && !ysum) return algebra.mul({x,y});
        const Terms xs=xsum ? nx.args : Terms{x}, ys=ysum ? ny.args : Terms{y};
        if (xs.size()*ys.size()>maxTerms || (work+=xs.size()*ys.size())>maxProducts) throw TooLarge{};
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
            if (label.head=="Multiparticle") result.branches=cappedMul(result.branches,label.args.size());
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

Id lookahead(Algebra& a, Id expanded) {
    const auto& n=a.node(expanded);
    if (n.head!="Plus" || n.args.size()>256) return expanded;
    std::unordered_set<Id> present(n.args.begin(),n.args.end());
    Terms added;
    std::size_t trials=0;
    // Compare changes against terms still available in the polynomial.
    for (Id term : n.args) {
        const auto& t=a.node(term);
        if (trials>=8192) break;
        if (!present.count(term) || t.head!="Times") continue;
        bool changed=false;
        for (std::size_t i=0; i<t.args.size() && !changed; ++i) {
            Id bracket=t.args[i];
            if (a.node(bracket).head!="SpinorChain" || a.node(bracket).args.size()!=2) continue;
            for (std::size_t j=0; j<t.args.size() && !changed; ++j) {
                Id sandwich=t.args[j];
                if (a.node(sandwich).head!="SpinorChain" || a.node(sandwich).args.size()!=3) continue;
                for (bool right : {false,true}) {
                    ++trials;
                    Id replaced=schoutenPair(a,bracket,sandwich,right);
                    if (replaced==bracket) continue;
                    Terms rest=t.args;
                    rest[i]=a.integer(1);
                    rest[j]=replaced;
                    Id replacement=Expansion(a,-1).run(a.mul(std::move(rest)));
                    std::size_t before=score(a,term);
                    Terms keep, cancel;
                    const auto& v=a.node(replacement);
                    for (Id newTerm : (v.head=="Plus" ? v.args : Terms{replacement})) {
                        Id opposite=a.mul({a.integer(-1),newTerm});
                        if (opposite!=term && present.count(opposite)) {
                            before+=score(a,opposite);
                            cancel.push_back(opposite);
                        } else keep.push_back(newTerm);
                    }
                    Id remain=a.add(std::move(keep));
                    if (score(a,remain)>=before) continue;
                    present.erase(term);
                    for (Id other : cancel) present.erase(other);
                    added.push_back(remain);
                    changed=true;
                    break;
                }
            }
        }
    }
    if (added.empty()) return expanded;
    for (Id term : n.args) if (present.count(term)) added.push_back(term);
    return a.add(std::move(added));
}

// An exchange diagram's different propagators can favour different momentum
// bases. Keep terms with precisely the same inverse factors together and
// choose the basis for each group independently.
Id bestGroup(Algebra& a, Id group) {
    Id best=group;
    Rank rank{score(a,group),a.cost(group)};
    unsigned count=static_cast<unsigned>(a.masses().size());
    for (int basis=-1; basis<=static_cast<int>(count); ++basis) {
        try {
            Id expanded=basis<0 ? group : Expansion(a,basis).run(group);
            Id contracted=reduceSpinor(a,expanded);
            if (expanded!=contracted) expanded=Expansion(a,-1).run(contracted);
            for (int pass=0; pass<2; ++pass) {
                Id next=lookahead(a,expanded);
                if (next==expanded) break;
                expanded=next;
            }
            std::size_t complexity=score(a,expanded);
            if (complexity>rank.first) continue;
            Id candidate=factor(a,expanded);
            Rank next{complexity,a.cost(candidate)};
            if (next<rank) { best=candidate; rank=next; }
        } catch (const TooLarge&) {
            // A different elimination order may generate fewer terms.
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
                if (n.head=="Power" && n.args.size()==2 && !a.leg(n.args[1]))
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
} // namespace hepcat
