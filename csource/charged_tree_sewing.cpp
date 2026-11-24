#include "charged_tree_sewing.hpp"
#include "validation_dirac_current.hpp"
#include <sstream>

namespace hepcat {
using namespace validation::dirac_current;

namespace {
// Choose xi directly from q.epsilon=0. The unnormalized polarization is
// linear in xi, so (d1,-d0) spans the null space of that one constraint.
// Its normalization has the selected physical or pair-momentum denominator.
vector photon_wave(const validation::point& p, unsigned leg, int helicity, int reference,
                   int anchor, int partner, bool physical) {
    if (!physical && reference>=0)
        return polarization(p,leg,helicity,reference,gauge::spurious,anchor);
    vector q{};
    for (unsigned j=0;j<4;++j)
        q[j]=p.momenta[anchor][j]+(physical ? 0 : p.momenta[partner][j]);
    auto numerator=[&](validation::spinor xi) {
        if (helicity>0) {
            auto a=raise(xi), b=raise(p.squares[leg]);
            return current_vector({a[0],a[1],0,0},{0,0,b[0],b[1]});
        }
        auto a=p.angles[leg];
        return current_vector({0,0,xi[0],xi[1]},{a[0],a[1],0,0});
    };
    const validation::spinor xi{dot(q,numerator({0,1})),-dot(q,numerator({1,0}))};
    auto value=numerator(xi);
    const auto bracket=helicity>0 ? inner(raise(xi),p.angles[leg]) : inner(xi,raise(p.squares[leg]));
    for(auto& component:value) component/=-std::sqrt(2.L)*bracket;
    return value;
}
}

charged_tree_sewing::charged_tree_sewing(std::vector<int> types, bool physical, std::string coupling)
    : types_(std::move(types)),references_(types_.size(),-1),
      physical_(physical),coupling_(std::move(coupling)) {
    const mask full=(1u<<types_.size())-1;
    std::vector<unsigned> electrons,positrons;
    for(unsigned i=0;i<types_.size();++i) {
        if(std::abs(types_[i])==1) {
            charged_|=1u<<i;
            (types_[i]>0 ? electrons : positrons).push_back(i);
        } else {
            for(unsigned offset=1;offset<types_.size();++offset) {
                unsigned j=(i+offset)%types_.size();
                if(types_[j]==-types_[i]) { references_[i]=j; break; }
                if(references_[i]<0 && types_[j]==types_[i]) references_[i]=j;
            }
        }
    }
    pairs_=electrons.size();
    partner_=types_[first(charged_)]>0 ? positrons.front() : electrons.front();
    orientation_=pairs_%2 ? -1 : 1;
    mask previous=0;
    for(unsigned i=0;i<pairs_;++i)
        for(unsigned j:{positrons[i],electrons[i]}) {
            orientation_*=shuffle(previous,1u<<j);
            previous|=1u<<j;
        }
    charge_.resize(full+1);
    for(mask s=1;s<=full;++s) {
        mask low=s&-s;
        charge_[s]=charge_[s^low]+((low&charged_) ? types_[first(low)] : 0);
    }
    for(mask s=1;s<full;++s) {
        mask other=full^s;
        unsigned n=count(s&charged_);
        if(!n || n>pairs_) continue;
        bool balanced=n==pairs_;
        bool neutral=charge_[s]==0;
        if(neutral && balanced && s>other) continue;
        if(!neutral && (!balanced || charge_[s]!=1)) continue;
        // At an odd balanced edge anchor immediately after the internal photon
        // on the net-electron side. Real emissions on the bridge then all have
        // a unique owner. At a vertex center use its unique virtual photon.
        unsigned a=build(s,neutral ? end::dressed : end::exchanged);
        unsigned b=build(other,neutral ? (balanced ? end::amputated : end::center) : end::dressed);
        if(!nodes_[a].graphs || !nodes_[b].graphs) continue;
        const int sign=neutral ? shuffle(s&charged_,other&charged_) : shuffle(other&charged_,s&charged_);
        cuts_.push_back({s,other,neutral ? edge::photon : edge::fermion,balanced});
        roots_.push_back({a,b,sign*orientation_});
        diagrams_+=nodes_[a].graphs*nodes_[b].graphs;
    }
    indices_.clear();
}

unsigned charged_tree_sewing::build(mask s,end boundary) {
    auto key=std::make_pair(s,boundary);
    if(auto it=indices_.find(key);it!=indices_.end()) return it->second;
    node v{s,charge_[s],boundary,1u<<count(s&charged_),{}, {}, 0};
    if(count(s)==1) v.graphs=boundary==end::dressed;
    else for(mask a=(s-1)&s;a;a=(a-1)&s) {
        mask b=s^a;
        if(v.charge==0) {
            if(charge_[a]!=-1 || charge_[b]!=1) continue;
            if(boundary==end::center && std::max(count(a&charged_),count(b&charged_))>=pairs_) continue;
        } else {
            if(charge_[a] || charge_[b]!=v.charge) continue;
            if(!(a&charged_) && (count(a)!=1 || boundary==end::exchanged)) continue;
        }
        unsigned left=build(a,end::dressed), right=build(b,end::dressed);
        if(!nodes_[left].graphs || !nodes_[right].graphs) continue;
        v.terms.push_back({left,right,shuffle(a&charged_,b&charged_)});
        v.graphs+=nodes_[left].graphs*nodes_[right].graphs;
    }
    unsigned states=1u<<(2*pairs_);
    v.slots.resize(states);
    for(unsigned i=0;i<states;++i) {
        unsigned globalBit=1,localBit=1;
        for(mask f=charged_;f;f&=f-1,globalBit<<=1) {
            if(!(s&(f&-f))) continue;
            if(i&globalBit) v.slots[i]|=localBit;
            localBit<<=1;
        }
    }
    unsigned id=nodes_.size();
    nodes_.push_back(std::move(v));
    indices_.emplace(key,id);
    return id;
}

charged_tree_sewing::complex charged_tree_sewing::evaluate(const point& p,real e,bool sum) const {
    const unsigned n=types_.size(), states=sum ? 1u<<(2*pairs_) : 1u;
    if(p.masses.size()!=n || p.momenta.size()!=n || p.angles.size()!=n || p.squares.size()!=n ||
       p.scalar_products.size()!=n*n || (sum && (p.angle_basis.size()!=n || p.square_basis.size()!=n)))
        throw std::invalid_argument("sewing requires the external kinematics of the declared process");
    const auto m=p.masses[first(charged_)];
    for(unsigned i=0;i<n;++i) if(p.masses[i] != ((charged_&(1u<<i)) ? m : 0))
        throw std::invalid_argument("incompatible masses in charged tree sewing");
    std::vector<std::vector<wave>> values(nodes_.size());
    for(unsigned k=0;k<nodes_.size();++k) {
        const auto& v=nodes_[k];
        auto& value=values[k];
        value.resize(sum ? v.spins : 1);
        wave momentum{};
        real s=0;
        for(mask bits=v.legs;bits;bits&=bits-1) {
            unsigned i=first(bits);
            for(unsigned j=0;j<4;++j) momentum[j]+=p.momenta[i][j];
            s+=p.masses[i]*p.masses[i];
            for(mask rest=bits&(bits-1);rest;rest&=rest-1) s+=2*p.scalar_products[i*n+first(rest)];
        }
        if(count(v.legs)==1) {
            unsigned i=first(v.legs);
            if(std::abs(types_[i])==2) {
                value[0]=photon_wave(p,i,types_[i],references_[i],first(charged_),partner_,physical());
            } else for(unsigned col=0;col<value.size();++col) {
                auto a=sum ? p.angle_basis[i][col] : p.angles[i];
                auto b=sum ? p.square_basis[i][col] : p.squares[i];
                if(types_[i]>0) {
                    b=raise(b);
                    value[col]={a[0],a[1],-b[0],-b[1]};
                } else {
                    a=raise(a);
                    value[col]={-a[0],-a[1],b[0],b[1]};
                }
            }
            continue;
        }
        // The first representative of each local spin assignment has zero
        // spectator bits. Do not evaluate this block again for its spectators.
        unsigned localMask=0,bit=1;
        for(mask f=charged_;f;f&=f-1,bit<<=1) if(v.legs&(f&-f)) localMask|=bit;
        unsigned h=0;
        do {
            auto& x=value[v.slots[h]];
            for(const auto& t:v.terms) {
                const auto& a=values[t.a][nodes_[t.a].slots[h]];
                const auto& b=values[t.b][nodes_[t.b].slots[h]];
                auto y=v.charge ? slash(a,b,v.charge<0) : current_vector(a,b);
                for(unsigned j=0;j<4;++j) x[j]+=real(t.sign)*y[j];
            }
            if(v.boundary==end::dressed) {
                if(v.charge) {
                    auto y=slash(momentum,x,v.charge<0);
                    for(unsigned j=0;j<4;++j) x[j]=(m*x[j]+real(v.charge)*y[j])/(s-m*m);
                } else for(auto& z:x) z/=s;
            }
            h=(h-localMask)&localMask&(states-1);
        } while(h);
    }
    std::vector<complex> amplitudes(states);
    for(unsigned k=0;k<roots_.size();++k) {
        const auto& t=roots_[k];
        const real factor=t.sign;
        const bool photon=cuts_[k].propagator==edge::photon;
        for(unsigned h=0;h<states;++h) {
            const auto& a=values[t.a][nodes_[t.a].slots[h]];
            const auto& b=values[t.b][nodes_[t.b].slots[h]];
            amplitudes[h]+=factor*(photon ? dot(a,b) : contract(b,a));
        }
    }
    const real power=std::pow(e,int(n)-2);
    if(!sum) return power*amplitudes.front();
    real rate=0;
    for(auto z:amplitudes) rate+=std::norm(power*z);
    return rate;
}

std::shared_ptr<const charged_tree_sewing> charged_tree_sewing::alternate_gauge() const {
    auto copy = std::make_shared<charged_tree_sewing>(*this);
    copy->physical_ = !physical_;
    return copy;
}

std::string charged_tree_sewing::latex() const { return format_formula(true); }

std::string charged_tree_sewing::format_formula(bool tex) const {
    std::vector<std::string> terms;
    auto has = [&](edge type, bool balanced) {
        return std::any_of(cuts_.begin(),cuts_.end(),[&](const cut& c) {
            return c.propagator==type && c.balanced==balanced;
        });
    };
    if (has(edge::fermion,true)) {
        // All charged cuts form a single orbit under the two alternating groups.
        // Use one representative with the real-photon labels still unassigned.
        const auto& chosen=*std::find_if(cuts_.begin(),cuts_.end(),[](const cut& c) {
            return c.propagator==edge::fermion;
        });
        auto labels=[](mask legs) {
            std::string result;
            for(;legs;legs&=legs-1) {
                if(!result.empty()) result+=",";
                result+=std::to_string(first(legs)+1);
            }
            return result;
        };
        const mask t=chosen.left&charged_, b=chosen.right&charged_;
        unsigned factorial=1;
        for(unsigned j=2;j<=pairs_/2;++j) factorial*=j;
        const unsigned stabilizer=factorial*factorial*(pairs_/2+1);
        const bool radiation=count(charged_)!=types_.size();
        std::ostringstream kernel;
        int sign=shuffle(b,t);
        std::string row, column;
        if(count(t)==3 && count(b)==3) {
            // Each rooted triple has just two local images. The two outer
            // alternating groups already generate both of those exchanges.
            // Pick one orientation and retain its Grassmann sign.
            auto oriented=[&](mask side, int net, bool last) {
                mask like=0;
                for(mask bits=side;bits;bits&=bits-1)
                    if(types_[first(bits)]==net) like|=bits&-bits;
                mask tip=like&-like;
                if(last) tip=like^tip;
                mask pair=side^tip;
                mask bra=net>0 ? (side^like) : (like^tip);
                mask ket=pair^bra;
                sign*=shuffle(pair,tip)*shuffle(bra,ket);
                return labels(tip)+(tex ? "\\leftarrow " : "<-")+labels(bra)+","+labels(ket);
            };
            column=oriented(t,1,false);
            row=oriented(b,-1,true);
            if(sign<0) kernel<<"-";
            kernel<<(tex ? "\\mathcal A_e\\mathcal A_E" : "Alt(e) Alt(E) ");
        } else {
            column=labels(t);
            row=labels(b);
            if(sign<0) kernel<<"-";
            if(tex) kernel<<"\\frac{\\mathcal A_e\\mathcal A_E}{"<<stabilizer*stabilizer<<"}";
            else kernel<<"Alt(e) Alt(E) / "<<stabilizer*stabilizer<<" ";
        }
        if(radiation) kernel<<(tex ? "\\sum_{L\\subseteq P}" : "sum(L subset P) ");
        if(tex) kernel<<"\\overline W_{"<<row<<"}"<<(radiation?"(P\\setminus L)":"")
              <<"X_{"<<column<<"}"<<(radiation?"(L)":"");
        else kernel<<"Wbar["<<row<<"]"<<(radiation?"(P minus L)":"")<<" X["<<column<<"]"<<(radiation?"(L)":"");
        terms.push_back(kernel.str());
    }
    if (has(edge::photon,true) || has(edge::photon,false))
        terms.push_back(tex ? R"(\sum_{\{S,B\}\in\mathcal C_\gamma}\epsilon(S,B)\,\frac{J_{<}(S):J_{<}(B)}{s_S})"
                            : "sum(central photon S|B) sign(S,B) J_<(S):J_<(B)/s(S)");
    std::ostringstream out;
    if (orientation_<0) out<<"-";
    out<<(tex?"e^{":"e^")<<types_.size()-2<<(tex?"}\\left[":" [");
    for (unsigned i=0;i<terms.size();++i) out<<(i?" + ":"")<<terms[i];
    return out.str()+(tex?"\\right]":"]");
}

std::string charged_tree_sewing::description() const {
    std::ostringstream out;
    out<<"Signed central sewings ("<<cuts_.size()<<" terms, "<<diagrams_<<" labeled trees).\n"
       <<format_formula(false)<<"\ne = "<<coupling_<<".\n";
    const bool hasPhotons=count(charged_)!=types_.size();
    if(hasPhotons)
        out<<(physical()?"Use the first charged momentum as photon reference.\n":
                         "Use the opposite-helicity or cyclic photon reference, or a charged-pair momentum for a single photon.\n");
    bool photon=false,fermion=false;
    for(const auto& c:cuts_) (c.propagator==edge::photon ? photon : fermion)=true;
    if(photon)
        out<<"Sum sign(S,B) J(S):J(B)/s(S) over the central neutral cuts S|B. "
           <<"In each amputated tensor J, both charged branches have fewer than "
           <<pairs_<<" charged legs.\n";
    if(fermion)
        out<<"Sum sign(B,T) Wbar(B) Xexch(T) over balanced net-particle cuts T|B. "
           <<"Xexch ends at its last virtual photon. Wbar includes the bridge. "
           <<"One charged partition and alternating sums over both sets of like charges suffice. In a triple, i<-b,j denotes the one image attaching the ordered pair (b,j) to i.\n";
    out<<"Assign each real photon to either side. Shuffle signs use only the charged labels.";
    return out.str();
}
} // namespace hepcat
