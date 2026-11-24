// Evaluate fully contracted external-spinor polynomials at ordinary on-shell external momenta.
#pragma once
#include "validation_numeric.hpp"
#include <algorithm>
#include <chrono>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <map>
#include <numeric>
#include <set>
#include <sstream>
#include <unordered_map>

namespace contracted_qed {
using namespace hepcat::validation;
using mask = unsigned;
using perm = std::vector<unsigned>;
using clock_type = std::chrono::steady_clock;
using mat = std::array<complex,4>;

struct spin_word {
    unsigned a, b;
    bool angle, end_angle;
    std::vector<mask> inserts;
    bool operator<(const spin_word &w) const {
        return std::tie(a,b,angle,end_angle,inserts) < std::tie(w.a,w.b,w.angle,w.end_angle,w.inserts);
    }
};
inline mask labels(const std::string &s) {
    mask g=0;
    for(char c:s) {
        if(c<'1'||c>'9'||(g & (1u<<(c-'1')))) throw std::runtime_error("Invalid subset "+s);
        g |= 1u<<(c-'1');
    }
    return g;
}
inline spin_word parse_word(const std::string &s) {
    if(s.size()<4 || (s.front()!='[' && s.front()!='<') || (s.back()!=']' && s.back()!='>'))
        throw std::runtime_error("Expected an external bracket: "+s);
    spin_word w{static_cast<unsigned>(s[1]-'1'),static_cast<unsigned>(s[s.size()-2]-'1'),s[0]=='<',s.back()=='>',{}};
    size_t i=2;
    for(;i<s.size()-2;) {
        auto j=s.find(')',i);
        if(s[i]!='('||j==s.npos) throw std::runtime_error(s);
        w.inserts.push_back(labels(s.substr(i+1,j-i-1))); i=j+1;
    }
    if(i!=s.size()-2 || (w.angle!=w.end_angle) != (w.inserts.size()%2 != 0)) throw std::runtime_error(s);
    return w;
}
inline int parity(const perm &p) {
    int s=1;
    for(unsigned i=0;i<p.size();++i)for(unsigned j=0;j<i;++j)if(p[j]>p[i])s=-s;
    return s;
}
inline mask remap(mask x,const perm &p) {
    mask y=0;
    for(;x;x&=x-1)y|=1u<<p[__builtin_ctz(x)];
    return y;
}
inline std::vector<std::pair<perm,int>> permutations(const std::vector<std::string> &legs) {
    perm id(legs.size()); std::iota(id.begin(),id.end(),0);
    std::vector<std::pair<perm,int>> out{{id,1}};
    for(auto name:{"e","E","A.+"}) {
        perm members;
        for(auto i:id)if(legs[i]==name)members.push_back(i);
        auto list=members;
        decltype(out) next;
        do {
            int s=std::string(name)=="A.+"?1:parity(list);
            for(auto entry:out) {
                entry.second*=s;
                for(size_t i=0;i<members.size();++i)entry.first[members[i]]=list[i];
                next.push_back(std::move(entry));
            }
        } while(std::next_permutation(list.begin(),list.end()));
        out=std::move(next);
    }
    return out;
}
struct fraction {
    unsigned divisor;
    std::vector<std::pair<mask,bool>> poles;
    std::vector<std::pair<int,std::vector<std::string>>> terms;
};
struct table {
    std::vector<std::string> legs;
    unsigned q=0,nphot=0,count=0;
    std::vector<fraction> rows;
    explicit table(const std::string &path) {
        std::ifstream in(path);std::string line;
        while(std::getline(in,line)) {
            std::istringstream fields(line); std::string tag,token;fields>>tag;
            if(tag=="particles") while(fields>>token)legs.push_back(token);
            else if(tag=="coupling")fields>>q>>nphot;
            else if(tag=="pole-products")fields>>count;
            else if(tag=="fraction") {
                rows.emplace_back(); fields>>rows.back().divisor;
                while(fields>>token)rows.back().poles.emplace_back(labels(token.substr(1)),token[0]=='t');
            } else {
                auto &term=rows.at(rows.size()-1).terms.emplace_back();
                term.first=std::stoi(tag);
                while(fields>>token)term.second.push_back(token);
            }
        }
        if(count==0||rows.empty()||legs.size()!=q+2)throw std::runtime_error("Incomplete table "+path);
    }
};

inline spinor raised(const spinor &x) {return {x[1],-x[0]};}
inline spinor times(const spinor &v,const mat &m) {return {v[0]*m[0]+v[1]*m[2],v[0]*m[1]+v[1]*m[3]};}
inline mat adj(const mat &m) {return {m[3],-m[1],-m[2],m[0]};}
inline complex inner(const spinor &v,const spinor &u){return v[0]*u[0]+v[1]*u[1];}
struct kinematics {
    std::vector<spinor> a,b;
    std::vector<mat> momenta,barred;
    std::vector<real> inv;
    complex normalization=1;
    kinematics(const point &p,const std::vector<int> &roles,bool physical,int longitudinal)
       :a(p.angles),b(p.squares),momenta(1u<<a.size()),barred(momenta.size()),inv(momenta.size()) {
        const unsigned n=a.size();
        for(unsigned g=1;g<momenta.size();++g) {
            const unsigned i=__builtin_ctz(g), other=g&(g-1);
            const auto &v=p.momenta[i];
            const mat m{v[0]+v[3],complex(v[1],-v[2]),complex(v[1],v[2]),v[0]-v[3]};
            for(int j=0;j<4;++j)momenta[g][j]=momenta[other][j]+m[j];
            barred[g]=adj(momenta[g]);
            inv[g]=inv[other]+p.masses[i]*p.masses[i];
            for(unsigned u=other;u;u&=u-1)inv[g]+=2*p.scalar_products[i*n+__builtin_ctz(u)];
        }
        std::vector<unsigned> photons;
        for(unsigned i=0;i<n;++i)if(std::abs(roles[i])==2)photons.push_back(i);
        for(unsigned l=0;l<photons.size();++l) {
            const auto k=photons[l]; const bool plus=roles[k]>0;
            // A Ward probe replaces the two polarization endpoints by those
            // of its own null momentum, with no polarization denominator.
            if (static_cast<int>(k)==longitudinal) continue;
            if(physical||photons.size()==1) {
                const mask ref=physical?1u:3u;
                if(plus) {
                    // <r i>=[k|Q|i>. A square bra uses the barred sigma matrix.
                    const auto cov=times(p.squares[k],barred[ref]);
                    a[k]={-cov[1],cov[0]};
                } else {
                    // [r i]=<k|Q|i]. The two row components are the square spinor itself.
                    b[k]=times(raised(p.angles[k]),momenta[ref]);
                }
            } else {
                const unsigned other=photons[(l+1)%photons.size()];
                if(plus)a[k]=p.angles[other];else b[k]=p.squares[other];
            }
            normalization*=plus?inner(raised(a[k]),p.angles[k]):inner(b[k],raised(p.squares[k]));
        }
    }
    complex evaluate(const spin_word &w) const {
        spinor x=w.angle?raised(a[w.a]):b[w.a];
        const auto y=w.end_angle?a[w.b]:raised(b[w.b]);
        bool bar=!w.angle;
        for(auto g:w.inserts){x=times(x,bar?barred[g]:momenta[g]);bar=!bar;}
        return inner(x,y);
    }
};
struct local_polynomial {
    // Factor repeated scalars before the external permutations are instantiated.
    struct node {int op,left,right;}; // variable, integer, product, sum
    std::vector<node> nodes;
    std::vector<std::string> variables;
    std::map<std::string,int> variable_ids;
    int build(std::vector<std::pair<int,std::vector<int>>> terms) {
        std::map<int,unsigned> counts;
        bool empty=true;
        for(auto &term:terms) {
            std::set<int> distinct(term.second.begin(),term.second.end());
            for(auto i:distinct)++counts[i];
            if(!term.second.empty())empty=false;
        }
        if(empty) {
            int value=0;for(auto &t:terms)value+=t.first;
            nodes.push_back({1,value,0});return nodes.size()-1;
        }
        auto best=std::max_element(counts.begin(),counts.end(),[](auto &a,auto &b){return a.second<b.second;})->first;
        decltype(terms) grouped,rest;
        for(auto &t:terms) {
            auto it=std::find(t.second.begin(),t.second.end(),best);
            if(it==t.second.end())rest.push_back(std::move(t));
            else {t.second.erase(it);grouped.push_back(std::move(t));}
        }
        int child=build(std::move(grouped));
        nodes.push_back({0,best,0});int value=nodes.size()-1;
        nodes.push_back({2,value,child});value=nodes.size()-1;
        if(!rest.empty()){int r=build(std::move(rest));nodes.push_back({3,value,r});value=nodes.size()-1;}
        return value;
    }
    explicit local_polynomial(const fraction &f) {
        std::vector<std::pair<int,std::vector<int>>> terms;
        for(auto &t:f.terms) {
            auto &item=terms.emplace_back(t.first,std::vector<int>{});
            for(auto &v:t.second) {
                auto [it,fresh]=variable_ids.emplace(v,variables.size());
                if(fresh)variables.push_back(v);
                item.second.push_back(it->second);
            }
        }
        build(std::move(terms));
    }
};
struct program {
    struct image {int sign;std::vector<unsigned> vars,poles;};
    struct piece {local_polynomial poly;std::vector<image> images;};
    std::vector<piece> pieces;
    std::vector<spin_word> words;
    std::map<spin_word,unsigned> lookup;
    unsigned nphot,q;
    explicit program(const table &t):nphot(t.nphot),q(t.q) {
        auto maps=permutations(t.legs);unsigned total=0, all=(1u<<t.legs.size())-1;
        for(auto &f:t.rows) {
            pieces.push_back({local_polynomial(f),{}});auto &piece=pieces.back();
            std::set<std::vector<unsigned>> seen;
            for(auto &[p,s]:maps) {
                image im{s,{},{}};
                for(auto [mask,mass]:f.poles) {
                    auto g=remap(mask,p);g=std::min(g,all^g);
                    im.poles.push_back(2*g+mass);
                }
                std::sort(im.poles.begin(),im.poles.end());
                if(!seen.insert(im.poles).second)continue;
                for(auto &v:piece.poly.variables) {
                    if(v=="m"){im.vars.push_back(0);continue;}
                    auto w=parse_word(v);w.a=p[w.a];w.b=p[w.b];
                    for(auto &g:w.inserts)g=remap(g,p);
                    auto [it,fresh]=lookup.emplace(w,words.size()+1);
                    if(fresh)words.push_back(w);
                    im.vars.push_back(it->second);
                }
                piece.images.push_back(std::move(im));
            }
            if(seen.size()*f.divisor!=maps.size())throw std::runtime_error("Wrong pole stabilizer");
            total+=seen.size();
        }
        if(total!=t.count)throw std::runtime_error("Incomplete contracted polynomial");
        lookup.clear();
    }
    complex operator()(const point &p,const std::vector<int> &roles,bool physical,real e,real m,int longitudinal=-1) const {
        kinematics k(p,roles,physical,longitudinal);std::vector<complex> w{m},vals;
        w.reserve(words.size()+1);for(auto &x:words)w.push_back(k.evaluate(x));
        std::vector<real> pole(2*k.inv.size());
        for(unsigned g=1;g<k.inv.size();++g){pole[2*g]=k.inv[g];pole[2*g+1]=k.inv[g]-m*m;}
        complex sum{},lost{};
        for(auto &piece:pieces) {
            vals.resize(piece.poly.nodes.size());
            for(auto &im:piece.images) {
                for(unsigned j=0;j<vals.size();++j) {
                    auto n=piece.poly.nodes[j];
                    switch(n.op) {
                    case 0:vals[j]=w[im.vars[n.left]];break;
                    case 1:vals[j]=n.left;break;
                    case 2:vals[j]=vals[n.left]*vals[n.right];break;
                    default:vals[j]=vals[n.left]+vals[n.right];
                    }
                }
                real den=im.sign;for(auto i:im.poles)den*=pole[i];
                complex y=vals.back()/den-lost,next=sum+y;
                lost=(next-sum)-y;sum=next;
            }
        }
        return std::pow(e,int(q))*std::pow(std::sqrt(2.L),int(nphot))*sum/k.normalization;
    }
};
} // namespace contracted_qed
