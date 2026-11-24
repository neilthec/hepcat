// Eight incoming massive fermions, ordered e,E,e,E,e,E,e,E.
// The derivation is in four-pair-sewing.tex. Build as described in eight-point-remaining.md.
#include "generation_feynman.hpp"
#include "validation_feynman.hpp"
#include "validation_numeric.hpp"
#include "madgraph-runtime.hpp"
#include <algorithm>
#include <array>
#include <cmath>
#include <chrono>
#include <vector>
#include <iostream>
#include <optional>

namespace {
using namespace hepcat;
using namespace hepcat::validation;
struct wave { spinor a, s; };
complex angle(const spinor& x, const spinor& y) { return x[1]*y[0]-x[0]*y[1]; }
complex square(const spinor& x, const spinor& y) { return x[0]*y[1]-x[1]*y[0]; }

using current = std::array<complex,4>; // One angle and one square index.
struct triple { int mask, i, j, k; };
struct sewing { int first, second, sign; };
struct side { int mask; std::vector<sewing> terms; };

int parity(const std::vector<int>& order) {
    int sign=1;
    for(size_t i=0;i<order.size();++i) for(size_t j=0;j<i;++j)
        if(order[j]>order[i]) sign=-sign;
    return sign;
}
struct catalog {
    std::vector<triple> triples;
    std::vector<side> sides;
    std::vector<sewing> channels;
    // Compact offsets into the spin tables. Spectator spins never change a slot.
    std::array<std::array<int,256>,256> slot{};
    int wave_count=0,current_count=0;
    void allocate(int mask,int& count) {
        int local=0;
        for(int state=0;state<256;++state)
            if(!(state&~mask)) slot[mask][state]=count+local++;
        for(int state=0;state<256;++state) slot[mask][state]=slot[mask][state&mask];
        count+=local;
    }
    catalog() {
        std::array<std::vector<int>,256> blocks;
        for(int i=0;i<8;++i) blocks[1<<i]={i};
        for(int i=0;i<8;++i) for(int j=i+2;j<8;j+=2) for(int k=1-i%2;k<8;k+=2) {
            int mask=(1<<i)|(1<<j)|(1<<k);
            triples.push_back({mask,i,j,k});
            blocks[mask]={i,j,k}; // Like charges first, in label order.
        }
        for(int mask=1;mask<255;++mask) if(!blocks[mask].empty()) allocate(mask,wave_count);
        for(int mask=1;mask<255;++mask) {
            side part{mask,{}};
            for(int a=(mask-1)&mask;a;a=(a-1)&mask) {
                const int b=mask^a;
                if(blocks[a].empty() || blocks[b].empty()) continue;
                if(blocks[a][0]%2!=0 || blocks[b][0]%2!=1) continue;
                auto word=blocks[a];
                word.insert(word.end(),blocks[b].begin(),blocks[b].end());
                part.terms.push_back({a,b,parity(word)});
            }
            if(part.terms.empty()) continue;
            allocate(mask,current_count);
            sides.push_back(std::move(part));
            const int other=255^mask;
            const int n=__builtin_popcount(mask);
            if(n>4 || (n==4 && mask>=other)) continue;
            std::vector<int> word;
            for(int group:{mask,other})
                for(int i=0;i<8;++i) if(group&(1<<i)) word.push_back(i);
            channels.push_back({mask,other,parity(word)});
        }
        if(triples.size()!=48 || sides.size()!=68 || channels.size()!=34)
            throw std::logic_error("Incomplete four-pair channels");
    }
};

struct kinematics {
    const catalog& catalog_;
    real mass;
    std::array<four_vector,256> momentum{};
    std::array<real,256> invariant{};
    explicit kinematics(const catalog& catalog, const point& p) : catalog_(catalog), mass(p.masses[0]) {
        for(int mask=1;mask<256;++mask) {
            const int i=__builtin_ctz(mask), rest=mask&(mask-1);
            for(int h=0;h<4;++h) momentum[mask][h]=momentum[rest][h]+p.momenta[i][h];
            invariant[mask]=invariant[rest]+mass*mass;
            for(int set=rest;set;set&=set-1) invariant[mask]+=2*p.scalar_products[8*i+__builtin_ctz(set)];
        }
    }
    wave dressed(const wave& i,const wave& c,const wave& d,const four_vector& q) const {
        // J(c,d) on the external spinor, followed by Q-slash minus m.
        spinor x{},y{};
        const auto di=angle(d.a,i.a), ci=angle(c.a,i.a);
        const auto ds=square(d.s,i.s), cs=square(c.s,i.s);
        for(int h=0;h<2;++h) {
            x[h]=di*c.s[h]+ci*d.s[h];
            y[h]=cs*d.a[h]+ds*c.a[h];
        }
        const complex upper(q[1],-q[2]), lower(q[1],q[2]);
        const real plus=q[0]+q[3], minus=q[0]-q[3];
        return {{{plus*x[1]-upper*x[0]-mass*y[0], lower*x[1]-minus*x[0]-mass*y[1]}},
                {{lower*y[0]-plus*y[1]-mass*x[0], minus*y[0]-upper*y[1]-mass*x[1]}}};
    }
    // In coherent mode each subset has one spin assignment. The rate uses
    // the same arithmetic and fills all assignments local to that subset.
    complex evaluate(const point& p, real charge, bool sum_spins) const {
        const auto& slot=catalog_.slot;
        std::vector<wave> w(catalog_.wave_count);
        std::vector<current> currents(catalog_.current_count);
        const int active=sum_spins?255:0;
        for(int i=0;i<8;++i) {
            const int mask=1<<i;
            if(sum_spins) {
                for(int h=0;h<2;++h)
                    w[slot[mask][h<<i]]={p.angle_basis[i][h],p.square_basis[i][h]};
            } else w[slot[mask][0]]={p.angles[i],p.squares[i]};
        }
        for(auto [mask,i,j,k]:catalog_.triples) {
            const auto t=1/(invariant[mask]-mass*mass);
            const auto a=t/invariant[(1<<j)|(1<<k)], b=t/invariant[(1<<i)|(1<<k)];
            int state=0;
            do {
                const auto& x=w[slot[1<<i][state]]; const auto& y=w[slot[1<<j][state]];
                const auto& z=w[slot[1<<k][state]];
                auto first=dressed(x,y,z,momentum[mask]), second=dressed(y,x,z,momentum[mask]);
                auto& out=w[slot[mask][state]];
                for(int h=0;h<2;++h) {
                    out.a[h]=a*first.a[h]-b*second.a[h];
                    out.s[h]=a*first.s[h]-b*second.s[h];
                }
                // Next assignment confined to the legs of this triple.
                state=(state-mask)&mask&active;
            } while(state);
        }
        for(const auto& part:catalog_.sides) {
            for(auto [a,b,sign]:part.terms) {
                int state=0;
                do {
                    auto& j=currents[slot[part.mask][state]];
                    const auto& x=w[slot[a][state]]; const auto& y=w[slot[b][state]];
                    for(int l=0;l<2;++l) for(int r=0;r<2;++r) {
                        const auto v=x.a[l]*y.s[r]+y.a[l]*x.s[r];
                        j[2*l+r]+=sign>0 ? v : -v;
                    }
                    state=(state-part.mask)&part.mask&active;
                } while(state);
            }
        }
        std::array<complex,256> total{};
        for(auto [a,b,sign]:catalog_.channels) {
            auto factor=sign/invariant[a];
            for(int state=0;state<=active;++state) {
                const auto& x=currents[slot[a][state]]; const auto& y=currents[slot[b][state]];
                total[state]+=factor*(x[0]*y[3]+x[3]*y[0]-x[1]*y[2]-x[2]*y[1]);
            }
        }
        const real factor=8*std::pow(charge,6);
        if(!sum_spins) return factor*total[0];
        real rate=0;
        for(auto z:total) rate+=std::norm(factor*z);
        return rate;
    }
};

int run(int points, unsigned seed, const char* mg_path) {
    catalog labels;
    model rules("tests/reference-models/calchep",2);
    Algebra tree(&rules.algebra());
    std::vector<std::string> names{"e","E","e","E","e","E","e","E"};
    auto m=tree.parse(rules.particle("e").mass);
    tree.setKinematics(std::vector<Id>(8,m),{});
    std::optional<expression_evaluator> reference;
    std::optional<hepcat_spinas::madgraph_detail::Reference> mg;
    const real mass=mg_path ? 1.L : 0.0005L, charge=1.L/3;
    if(mg_path) mg.emplace(mg_path);
    else {
        std::vector<Id> terms;
        for(const auto& graph:generation::diagram_plans(rules,names))
            terms.push_back(tree.factorSum(feynman_to_spinors(tree,generation::sew_feynman(tree,rules,names,graph))));
        if(terms.size()!=1728) throw std::logic_error("Unexpected Feynman diagram count");
        reference.emplace(tree,tree.add(terms),model_parameters(rules,{{"Me",mass},{"EE",charge}}));
    }
    real worst=0;
    double candidate_seconds=0, preparation_seconds=0, mg_seconds=0;
    using clock=std::chrono::steady_clock;
    for(int n=0;n<points;++n) {
        auto event_start=clock::now();
        auto p=sample_point(std::vector<real>(8,mass),seed+n);
        auto start=clock::now();
        preparation_seconds+=std::chrono::duration<double>(start-event_start).count();
        auto x=kinematics(labels,p).evaluate(p,charge,mg.has_value());
        candidate_seconds+=std::chrono::duration<double>(clock::now()-start).count();
        complex y;
        if(mg) {
            std::vector<double> momenta;
            for(int i=0;i<8;++i) for(auto q:p.momenta[i]) momenta.push_back(i<2?q:-q);
            start=clock::now();
            if(mg->has_sum()) y=mg->evaluate_sum(momenta);
            else {
                y=0;
                std::vector<int> h(8);
                for(int state=0;state<256;++state) {
                    for(int i=0;i<8;++i) h[i]=((state>>i)&1)?1:-1;
                    y+=mg->evaluate(momenta,h);
                }
            }
            mg_seconds+=std::chrono::duration<double>(clock::now()-start).count();
        } else {
            y=(*reference)(p);
        }
        const auto delta=std::abs(x-y), scale=std::max(std::abs(x),std::abs(y));
        const auto residual=scale==0?0:delta/scale;
        if(!std::isfinite(residual)||residual>1e-8L) {
            std::cerr<<"Mismatch at seed "<<seed+n<<", candidate "<<x<<", reference "<<y<<"\n";
            return 1;
        }
        worst=std::max(worst,residual);
    }
    std::cout<<(mg?"MadGraph spin sum":"Signed Feynman amplitude")<<", "<<points
             <<" points, seed "<<seed<<", maximum relative difference "<<worst<<"\n";
    if(mg) std::cout<<"Mean ms per full spin sum: sewing "<<1000*candidate_seconds/points
                    <<", sewing including event generation and external spinors "
                    <<1000*(preparation_seconds+candidate_seconds)/points<<", MadGraph "<<1000*mg_seconds/points<<"\n";
    return 0;
}
}
int main(int argc,char** argv) try {
    if(argc!=3 && argc!=4) throw std::invalid_argument("Usage: four-pair-sewing POINTS SEED [MG_LIBRARY]");
    auto n=std::stoi(argv[1]);
    if(n<=0) throw std::invalid_argument("Expected at least one point");
    return run(n,std::stoul(argv[2]),argc==4?argv[3]:nullptr);
} catch(const std::exception& e) { std::cerr<<e.what()<<'\n'; return 2; }
