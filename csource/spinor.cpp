// Exact identities for ordinary two-component spinor chains.
// A momentum word alternates angle and square spaces at every insertion.
#include "reduce.hpp"

#include <algorithm>
#include <array>
#include <cctype>
#include <map>
#include <utility>
#include <vector>

namespace hepcat {
namespace {
using Word = std::vector<Id>;
enum class Kind { Unknown, Spin, Helicity, Xi };
struct End {
    Kind kind = Kind::Unknown;
    bool angle = false;
    Id label = 0;
    int leg = 0;
    unsigned typeSlot = 0;
    bool indexed = false;
    bool upper = false;
    Id index = 0;
};

End endpoint(const Algebra& a, Id id) {
    const auto& n = a.node(id);
    if (n.head != "Spinor" || n.args.size() < 3 || n.args.size() > 5) return {};
    const auto& tag = a.node(n.args[0]).head;
    End e;
    if (tag == "\"Spin\"") e.kind = Kind::Spin;
    else if (tag == "\"Helicity\"") e.kind = Kind::Helicity;
    else if (tag == "\"Xi\"") e.kind = Kind::Xi;
    else return {};
    if (n.args.size() == 5) {
        const auto& variance = a.node(n.args[1]).head;
        if (e.kind != Kind::Spin || (variance != "\"Upper\"" && variance != "\"Lower\"")) return {};
        e.upper = variance == "\"Upper\"";
        e.indexed = true;
        e.index = n.args[4];
        e.typeSlot = 2;
    } else {
        if (n.args.size() == 4 && e.kind != Kind::Helicity) return {};
        e.typeSlot = 1;
    }
    const auto& type = a.node(n.args[e.typeSlot]).head;
    if (type != "\"Square\"" && type != "\"Angle\"") return {};
    e.angle = type == "\"Angle\"";
    e.label = n.args[e.typeSlot + 1];
    e.leg = a.leg(e.label);
    return e;
}

Id flip(Algebra& a, Id endpointId, const End& e) {
    Word args = a.node(endpointId).args;
    args[e.typeSlot] = a.atom(e.angle ? "\"Square\"" : "\"Angle\"");
    return a.call("Spinor", std::move(args));
}

int momentumLeg(const Algebra& a, Id id) {
    const auto& n = a.node(id);
    return n.head == "Mom" && n.args.size() == 1 ? a.leg(n.args[0]) : 0;
}

// A literal spin projection denotes one component. Only a named dummy
// index licenses the sum used in the massive completeness relation.
bool namedIndex(const Algebra& a, Id index) {
    const auto& n=a.node(index);
    if (n.compound || n.head.empty()) return false;
    const auto first=static_cast<unsigned char>(n.head.front());
    return std::isalpha(first) || first=='$' || first>=128;
}

bool external(const Algebra& a, int leg) {
    return leg > 0 && static_cast<std::size_t>(leg) <= a.masses().size();
}

Id dot(Algebra& a, int i, int j) {
    if (i == j) return a.pow(a.masses()[i - 1], 2);
    if (i > j) std::swap(i,j);
    return a.call("MomProd", {a.integer(i), a.integer(j)});
}

Id signedValue(Algebra& a, int sign, Id value) {
    return sign == 1 ? value : a.mul({a.integer(-1), value});
}

// Each recursive branch removes at least one momentum. Canonical orientation
// is applied only when returning a word and never changes an index variance.
Id chain(Algebra& a, Word w) {
    if (w.size() < 2) return a.call("SpinorChain", std::move(w));
    const End l = endpoint(a, w.front()), r = endpoint(a, w.back());
    const std::size_t count = w.size() - 2;
    if (l.kind == Kind::Unknown || r.kind == Kind::Unknown ||
        (l.angle == r.angle) != (count % 2 == 0))
        return a.call("SpinorChain", std::move(w));
    // Reversal is equally valid for an unsplit sum of momenta. This often
    // lets two factored chains share storage without distributing the sum.
    bool reverse = count%2 ? l.angle : l.leg && r.leg && l.leg > r.leg;
    bool ordinary = std::all_of(w.begin()+1,w.end()-1,[&](Id k) {
        return a.node(k).head=="Mom" && a.node(k).args.size()==1;
    });
    if (!ordinary) return a.call("SpinorChain",std::move(w));
    for (std::size_t i=1; i<=count; ++i) {
        if (external(a,momentumLeg(a,w[i]))) continue;
        if (reverse) std::reverse(w.begin(),w.end());
        return signedValue(a, reverse && count%2==0 ? -1 : 1,
                           a.call("SpinorChain",std::move(w)));
    }
    if (count == 0 && w.front() == w.back()) return a.integer(0);

    if (count && l.kind != Kind::Xi && momentumLeg(a,w[1]) == l.leg) {
        if (l.kind == Kind::Helicity) return a.integer(0);
        w[0] = flip(a,w[0],l);
        w.erase(w.begin()+1);
        return a.mul({a.masses()[l.leg-1], chain(a, std::move(w))});
    }
    if (count && r.kind != Kind::Xi && momentumLeg(a,w[count]) == r.leg) {
        if (r.kind == Kind::Helicity) return a.integer(0);
        w.back() = flip(a,w.back(),r);
        w.erase(w.end()-2);
        return a.mul({a.integer(-1), a.masses()[r.leg-1], chain(a,std::move(w))});
    }
    if (count == 1 && l.kind == Kind::Helicity && external(a,l.leg) &&
        a.node(w.front()).args.size() == 3 && flip(a,w.front(),l) == w.back())
        return a.mul({a.integer(2), dot(a,l.leg,momentumLeg(a,w[1]))});

    // Clifford algebra: commute a repeated momentum through the intervening
    // word, contracting at each position, then use p_i^2 = m_i^2.
    for (std::size_t i = 1; i <= count; ++i) {
        auto end = std::find(w.begin()+i+1, w.end()-1, w[i]);
        if (end == w.end()-1) continue;
        const std::size_t j = static_cast<std::size_t>(end-w.begin());
        int leg = momentumLeg(a,w[i]);
        Word terms;
        for (std::size_t k = i+1; k < j; ++k) {
            Word shorter = w;
            shorter.erase(shorter.begin()+k);
            shorter.erase(shorter.begin()+i);
            int sign = (k-i-1) % 2 == 0 ? 2 : -2;
            terms.push_back(a.mul({a.integer(sign),dot(a,leg,momentumLeg(a,w[k])),
                                   chain(a,std::move(shorter))}));
        }
        Word shorter = w;
        shorter.erase(shorter.begin()+j);
        shorter.erase(shorter.begin()+i);
        terms.push_back(signedValue(a, (j-i-1)%2 == 0 ? 1 : -1,
            a.mul({dot(a,leg,leg),chain(a,std::move(shorter))})));
        return a.add(std::move(terms));
    }

    // A lightlike momentum is a rank-one outer product. The first factor
    // ends in the chirality reached after the preceding momentum insertions.
    for (std::size_t i = 1; i <= count; ++i) {
        int leg = momentumLeg(a,w[i]);
        if (a.masses()[leg-1] != a.integer(0)) continue;
        bool angle = (i%2 == 1) == l.angle;
        Id h = a.call("Spinor",{a.atom("\"Helicity\""),a.atom(angle ? "\"Angle\"" : "\"Square\""),a.integer(leg)});
        Word prefix(w.begin(),w.begin()+i), suffix(w.begin()+i+1,w.end());
        prefix.push_back(h);
        suffix.insert(suffix.begin(),flip(a,h,endpoint(a,h)));
        return a.mul({chain(a,std::move(prefix)),chain(a,std::move(suffix))});
    }
    if (reverse) {
        std::reverse(w.begin(),w.end());
        return signedValue(a,count%2 ? 1 : -1,a.call("SpinorChain",std::move(w)));
    }
    return a.call("SpinorChain",std::move(w));
}

// Join an explicitly contracted massive little-group index. The product of
// two bare helicity brackets with the same two lightlike legs is -2 p_i.p_j
// in the sorted orientation. General lightlike outer products stay factored.
Id contractProduct(Algebra& a, Id original) {
    const Word& factors = a.node(original).args;
    struct ChainFactor { std::size_t position; Id base; int exponent; };
    std::vector<ChainFactor> chains;
    for (std::size_t i=0; i<factors.size(); ++i) {
        Id base=factors[i];
        int exponent=1;
        if (a.node(base).head == "Power" && a.node(base).args.size()==2) {
            exponent=a.leg(a.node(base).args[1]);
            base=a.node(base).args[0];
        }
        if (exponent>0 && a.node(base).head=="SpinorChain" && a.node(base).args.size()>=2)
            chains.push_back({i,base,exponent});
    }
    for (std::size_t i=0; i<chains.size(); ++i) for (std::size_t j=i+1; j<chains.size(); ++j) {
        const auto& f=chains[i]; const auto& g=chains[j];
        const Word& u=a.node(f.base).args; const Word& v=a.node(g.base).args;
        for (unsigned sideU=0; sideU<2; ++sideU) for (unsigned sideV=0; sideV<2; ++sideV) {
            End p=endpoint(a,sideU ? u.back() : u.front()), q=endpoint(a,sideV ? v.back() : v.front());
            if (!external(a,p.leg) || p.leg!=q.leg) continue;
            bool massive=p.indexed && q.indexed && p.index==q.index && p.upper!=q.upper && namedIndex(a,p.index);
            bool massless=p.kind==Kind::Helicity && q.kind==Kind::Helicity && p.angle!=q.angle &&
                u.size()==2 && v.size()==2 && a.node(sideU ? u.back() : u.front()).args.size()==3 &&
                a.node(sideV ? v.back() : v.front()).args.size()==3;
            if (!massive && !massless) continue;
            if (massive && (f.exponent!=1 || g.exponent!=1)) continue;
            if (massless) {
                Id pOuter=sideU ? u.front() : u.back(), qOuter=sideV ? v.front() : v.back();
                End other=endpoint(a,pOuter);
                if (other.kind!=Kind::Helicity || !external(a,other.leg) ||
                    a.node(pOuter).args.size()!=3 || flip(a,pOuter,other)!=qOuter) continue;
            }
            Word left=u, right=v;
            int sign=1;
            if (!sideU) {
                std::reverse(left.begin(),left.end());
                if (left.size()%2==0) sign=-sign;
            }
            if (sideV) {
                std::reverse(right.begin(),right.end());
                if (right.size()%2==0) sign=-sign;
            }
            left.pop_back();
            if (massive && (p.angle!=p.upper)) sign=-sign;
            if (p.angle!=q.angle) left.push_back(a.call("Mom",{p.label}));
            left.insert(left.end(),right.begin()+1,right.end());
            int copies=massive ? 1 : std::min(f.exponent,g.exponent);
            Word out;
            for (std::size_t k=0;k<factors.size();++k) {
                if (k==f.position) {if (f.exponent>copies) out.push_back(a.pow(f.base,f.exponent-copies));}
                else if (k==g.position) {if (g.exponent>copies) out.push_back(a.pow(g.base,g.exponent-copies));}
                else out.push_back(factors[k]);
            }
            Id joined=chain(a,std::move(left));
            if (massive && p.angle==q.angle) joined=a.mul({a.masses()[p.leg-1],joined});
            out.push_back(a.pow(signedValue(a,sign,joined),copies));
            return a.mul(std::move(out));
        }
    }
    return original;
}

// For four distinct endpoints A,B,C,D of the same chirality the three bracket
// pairings P=(AB)(CD), Q=(AC)(BD), R=(AD)(BC) satisfy P-Q+R=0. Match two
// summands with the same remaining multiplicative factors before substituting.
Id schouten(Algebra& a, Id original) {
    const Word& sum=a.node(original).args;
    struct Entry { std::size_t term; unsigned pairing; };
    std::map<std::array<Id,5>, std::vector<Entry>> groups;
    std::vector<bool> used(sum.size(),false);
    Word replacements;
    constexpr int weights[] = {1,-1,1};
    constexpr unsigned partitions[][4] = {{0,1,2,3},{0,2,1,3},{0,3,1,2}};
    for (std::size_t term=0; term<sum.size(); ++term) {
        const auto& times=a.node(sum[term]);
        if (times.head!="Times") continue;
        struct Bracket { std::size_t position; Id base; int exponent; };
        std::vector<Bracket> brackets;
        for (std::size_t i=0; i<times.args.size(); ++i) {
            Id base=times.args[i]; int exponent=1;
            if (a.node(base).head=="Power" && a.node(base).args.size()==2) {
                exponent=a.leg(a.node(base).args[1]);
                base=a.node(base).args[0];
            }
            const auto& bracket=a.node(base);
            if (exponent>0 && bracket.head=="SpinorChain" && bracket.args.size()==2)
                brackets.push_back({i,base,exponent});
        }
        for (std::size_t i=0; i<brackets.size() && !used[term]; ++i)
            for (std::size_t j=i+1; j<brackets.size() && !used[term]; ++j) {
                auto left=brackets[i], right=brackets[j];
                const Word& x=a.node(left.base).args;
                const Word& y=a.node(right.base).args;
                std::array<Id,4> ends{x[0],x[1],y[0],y[1]};
                const End first=endpoint(a,ends[0]);
                if (first.kind==Kind::Unknown || !std::all_of(ends.begin()+1,ends.end(),[&](Id e) {
                      auto record=endpoint(a,e); return record.kind!=Kind::Unknown && record.angle==first.angle;
                    })) continue;
                std::sort(ends.begin(),ends.end());
                if (std::adjacent_find(ends.begin(),ends.end())!=ends.end()) continue;
                const Word& containsA=(x[0]==ends[0] || x[1]==ends[0]) ? x : y;
                Id partner=containsA[0]==ends[0] ? containsA[1] : containsA[0];
                unsigned pairing=static_cast<unsigned>(std::find(ends.begin(),ends.end(),partner)-ends.begin()-1);
                int orientation = (x[0]>x[1]) == (y[0]>y[1]) ? 1 : -1;
                Word cofactor{a.integer(orientation*weights[pairing])};
                for (std::size_t k=0; k<times.args.size(); ++k) {
                    if (k==left.position) {
                        if (left.exponent>1) cofactor.push_back(a.pow(left.base,left.exponent-1));
                    } else if (k==right.position) {
                        if (right.exponent>1) cofactor.push_back(a.pow(right.base,right.exponent-1));
                    } else cofactor.push_back(times.args[k]);
                }
                Id scale=a.mul(std::move(cofactor));
                auto& matches=groups[{ends[0],ends[1],ends[2],ends[3],scale}];
                auto match=std::find_if(matches.begin(),matches.end(),[&](const Entry& e){
                    return !used[e.term] && e.term!=term && e.pairing!=pairing;
                });
                if (match==matches.end()) { matches.push_back({term,pairing}); continue; }
                unsigned remaining=3-pairing-match->pairing;
                const auto& p=partitions[remaining];
                Id b1=chain(a,{ends[p[0]],ends[p[1]]}), b2=chain(a,{ends[p[2]],ends[p[3]]});
                replacements.push_back(a.mul({a.integer(-weights[remaining]),scale,b1,b2}));
                used[term]=true; used[match->term]=true;
            }
    }
    if (replacements.empty()) return original;
    for (std::size_t i=0; i<sum.size(); ++i) if (!used[i]) replacements.push_back(sum[i]);
    return a.add(std::move(replacements));
}

Id trace(Algebra& a, Id original) {
    const Word& w=a.node(original).args;
    if (w.empty()) return a.integer(2);
    if (w.size()%2 || !std::all_of(w.begin(),w.end(),[&](Id m){return external(a,momentumLeg(a,m));}))
        return original;
    if (w.size()==2) return a.mul({a.integer(2),dot(a,momentumLeg(a,w[0]),momentumLeg(a,w[1]))});
    auto repeated=std::adjacent_find(w.begin(),w.end());
    if (repeated==w.end()) return original;
    Word shorter=w;
    auto offset=repeated-w.begin();
    shorter.erase(shorter.begin()+offset,shorter.begin()+offset+2);
    return a.mul({a.pow(a.masses()[momentumLeg(a,*repeated)-1],2),
                  trace(a,a.call("SpinorTrace",std::move(shorter)))});
}
} // namespace

Id reduceSpinor(Algebra& a, Id id) {
    const auto& n=a.node(id);
    if (!n.compound) return id;
    if (n.head=="SpinorChain") return chain(a,n.args);
    if (n.head=="SpinorTrace") return trace(a,id);
    if (n.head=="Times") return contractProduct(a,id);
    if (n.head=="Plus") return schouten(a,id);
    return id;
}
} // namespace hepcat
