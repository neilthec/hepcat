#include "model_orbit.hpp"
#include "model_expression.hpp"
#include <algorithm>
#include <charconv>
#include <functional>
#include <numeric>
#include <set>
#include <stdexcept>

namespace hepcat::model_detail {
namespace {
using view=std::string_view;
std::vector<view> split(view s, char delimiter) {
    std::vector<view> result;
    while(true) {
        const auto end=s.find(delimiter);
        result.push_back(s.substr(0,end));
        if(end==view::npos) return result;
        s.remove_prefix(end+1);
    }
}
int label(view str,size_t n) {
    int value=0;
    auto parsed=std::from_chars(str.data(),str.data()+str.size(),value);
    if(parsed.ec!=std::errc{} || parsed.ptr!=str.data()+str.size() || value<=0 || size_t(value)>n)
        throw std::invalid_argument("Invalid external leg in LegOrbit");
    return value;
}
std::vector<Id> labels(Algebra& a, view raw,size_t n) {
    std::vector<Id> result;
    std::set<int> used;
    for(const auto item:split(raw,'.')) {
        int x=label(item,n);
        if(!used.insert(x).second)throw std::invalid_argument("Repeated label in LegOrbit");
        result.push_back(a.integer(x));
    }
    return result;
}
}
Id read_leg_orbit(Algebra& a,view raw,Id scope,const std::vector<bool>& massive) {
    if(raw.size()<12||raw.substr(0,9)!="LegOrbit["||raw.back()!=']')
        throw std::invalid_argument("Invalid LegOrbit envelope");
    raw.remove_prefix(9);raw.remove_suffix(1);
    const auto field=[&]() {
        auto stop=raw.find(';');
        if(stop==view::npos)throw std::invalid_argument("Incomplete LegOrbit header");
        auto part=raw.substr(0,stop);raw.remove_prefix(stop+1);return part;
    };
    const auto n=massive.size();
    auto groups=field(), rules=field(), common=field();
    std::vector<Id> symmetries, projections;
    std::set<Id> used;
    if(groups!="-")for(auto g:split(groups,',')) {
        if(g.size()<2 || (g[0]!='+'&&g[0]!='-'))throw std::invalid_argument("Invalid LegOrbit group");
        const bool odd=g[0]=='-';
        auto members=labels(a,g.substr(1),n);
        for(auto j:members)if(!used.insert(j).second)throw std::invalid_argument("Overlapping LegOrbit groups");
        symmetries.push_back(a.call(odd?"OrbitAlternating":"OrbitSymmetric",std::move(members)));
    }
    std::set<std::pair<bool,int>> mapped;
    if(rules!="-")for(auto rule:split(rules,',')) {
        auto eq=rule.find('=');
        if(eq==view::npos || (rule[0]!='A'&&rule[0]!='S'))throw std::invalid_argument("Invalid LegOrbit endpoint rule");
        bool ang=rule[0]=='A';
        int from=label(rule.substr(1,eq-1),n);
        if(massive[from-1]||!mapped.emplace(ang,from).second)throw std::invalid_argument("Invalid LegOrbit photon slot");
        auto rhs=rule.substr(eq+1);auto at=rhs.find('@');int to=label(rhs.substr(0,at),n);
        if(massive[to-1])throw std::invalid_argument("LegOrbit endpoint must be massless");
        std::vector<Id> args{a.integer(from),a.integer(to)};
        if(at!=view::npos) {
            auto m=labels(a,rhs.substr(at+1),n);
            args.push_back(a.call("Mom",{m.size()==1?m[0]:a.call("Multiparticle",m)}));
        }
        projections.push_back(a.call(ang?"OrbitAngle":"OrbitSquare",std::move(args)));
    }
    if(raw.substr(0,7)!="Shared[")throw std::invalid_argument("LegOrbit requires a shared polynomial");
    return a.call("ModelLegOrbit",{a.integer(n),a.call("List",symmetries),a.call("List",projections),
                                parse_expression(a,common,scope,massive),parse_expression(a,raw,scope,massive)});
}
Id expand_leg_orbit(Algebra& a,Id root) {
    const auto& r=a.node(root);
    if(r.head!="ModelLegOrbit")return root;
    const auto n=static_cast<size_t>(a.leg(r.args.at(0)));
    std::vector<int> p(n+1);std::iota(p.begin(),p.end(),0);
    struct group { bool antisymmetric; std::vector<int> legs; };
    std::vector<group> groups;
    for(Id g:a.node(r.args[1]).args) {
        group item{a.node(g).head=="OrbitAlternating",{}};
        for(Id j:a.node(g).args)item.legs.push_back(a.leg(j));
        std::sort(item.legs.begin(),item.legs.end());groups.push_back(std::move(item));
    }
    const auto angle=a.atom("\"Angle\""),square=a.atom("\"Square\""),helicity=a.atom("\"Helicity\"");
    std::map<std::pair<bool,int>,std::pair<Id,Id>> projections;
    for(Id id:a.node(r.args[2]).args) {
        const auto& rule=a.node(id);
        projections[{rule.head=="OrbitAngle",a.leg(rule.args[0])}]={rule.args[1],rule.args.size()==3?rule.args[2]:0};
    }
    // Rank reductions and spin contraction are finished before this stage.
    // The shared kernel holds ordinary external chains and physical propagators.
    std::vector<Id> sum;
    std::function<void(size_t,int)> emit=[&](size_t level,int sign) {
        if(level<groups.size()) {
            const auto& g=groups[level];auto perm=g.legs;
            do {
                int s=sign;
                for(size_t i=0;i<perm.size();++i) {
                    p[g.legs[i]]=perm[i];
                    if(g.antisymmetric)for(size_t j=0;j<i;++j)if(perm[j]>perm[i])s=-s;
                }
                emit(level+1,s);
            } while(std::next_permutation(perm.begin(),perm.end()));
            return;
        }
        std::unordered_map<Id,Id> cache;
        auto map_label=[&](Id x) { return a.integer(p.at(a.leg(x))); };
        auto visit=[&](auto& self,Id id)->Id {
            if(auto it=cache.find(id);it!=cache.end())return it->second;
            const auto& v=a.node(id);
            if(!v.compound||v.head=="ModelParameter")return id;
            Id out;
            if(v.head=="SpinorChain") {
                std::vector<Id> chain;int orientation=1;
                for(size_t j=0;j<v.args.size();++j) {
                    auto term=v.args[j];const auto& z=a.node(term);
                    if(z.head!="Spinor"){chain.push_back(self(self,term));continue;}
                    if(z.args.size()!=3)throw std::invalid_argument("LegOrbit requires contracted external spinors");
                    auto c=z.args;const bool ang=c[1]==angle;int dest=p.at(a.leg(c[2]));
                    c[2]=a.integer(dest);
                    if(a.node(c[0]).head!="\"Polarization\""){chain.push_back(a.call("Spinor",c));continue;}
                    c[0]=helicity;
                    auto it=projections.find({ang,dest});
                    if(it==projections.end()){chain.push_back(a.call("Spinor",c));continue;}
                    auto [target,mom]=it->second;c[2]=target;
                    if(mom) {
                        c[1]=ang?square:angle;
                        if(j!=0){chain.push_back(mom);orientation=-orientation;}
                    }
                    chain.push_back(a.call("Spinor",c));
                    if(mom&&j==0)chain.push_back(mom);
                }
                out=a.call("SpinorChain",chain);
                if(orientation<0)out=a.mul({a.integer(-1),out});
            } else if(v.head=="Multiparticle") {
                std::vector<int> ordered;
                for(auto j:v.args)ordered.push_back(p.at(a.leg(j)));
                std::sort(ordered.begin(),ordered.end());
                std::vector<Id> args;for(int j:ordered)args.push_back(a.integer(j));
                out=a.call(v.head,args);
            } else if(v.head=="Mom") {
                Id j=v.args.at(0);
                out=a.call(v.head,{a.node(j).compound?self(self,j):map_label(j)});
            } else if (v.head=="Plus" || v.head=="Times" || v.head=="Power" || v.head=="Rational" ||
                       v.head=="Sqrt" || v.head=="PropDen") {
                std::vector<Id> args;
                for(auto x:v.args)args.push_back(self(self,x));
                if(v.head=="Times")out=a.mul(std::move(args));
                else if(v.head=="Plus")out=a.add(std::move(args));
                else out=a.call(v.head,std::move(args));
            } else {
                throw std::invalid_argument("Unsupported head in external-spinor orbit: " + v.head);
            }
            cache.emplace(id,out);return out;
        };
        auto result=visit(visit,r.args[4]);
        sum.push_back(sign<0?a.mul({a.integer(-1),result}):result);
    };
    emit(0,1);
    return a.mul({r.args[3],a.add(std::move(sum))});
}
}
