// Direct evaluation of the one-image paper formula. It fixes the two ordered
// terminal pairs, then explicitly antisymmetrizes all 36 external assignments.
#pragma once
#include "validation_dirac_current.hpp"

namespace triple_seed {
using namespace hepcat::validation;
using namespace hepcat::validation::dirac_current;

inline complex evaluate(const point& p, const std::vector<int>& species, real e, gauge policy) {
    const unsigned n=species.size(), photon_count=n-6, sets=1u<<photon_count;
    std::vector<vector> eps(photon_count);
    for (unsigned i=0;i<photon_count;++i) {
        int ref=-1;
        for(unsigned d=1;d<photon_count;++d) {
            unsigned j=6+(i+d)%photon_count;
            if(species[j]!=species[6+i]) { ref=j; break; }
            if(ref<0) ref=j;
        }
        eps[i]=polarization(p,6+i,species[6+i],policy==gauge::spurious ? ref : -1,policy,0,
                            policy==gauge::spurious && ref<0 ? 1 : -1);
    }
    std::vector<vector> momenta(1u<<n);
    std::vector<real> s(1u<<n);
    for(mask k=1;k<s.size();++k) {
        unsigned i=first(k),rest=k&(k-1);
        for(unsigned j=0;j<4;++j) momenta[k][j]=momenta[rest][j]+p.momenta[i][j];
        s[k]=s[rest]+p.masses[i]*p.masses[i];
        for(mask bits=rest;bits;bits&=bits-1) s[k]+=2*p.scalar_products[i*n+first(bits)];
    }
    auto add=[](vector& x,const vector& y) { for(unsigned j=0;j<4;++j) x[j]+=y[j]; };
    auto propagate=[&](vector x,mask legs,bool bar) {
        const auto y=slash(momenta[legs],x,bar);
        const real mass=p.masses[0];
        for(unsigned j=0;j<4;++j) x[j]=(mass*x[j]+(bar ? -y[j] : y[j]))/(s[legs]-mass*mass);
        return x;
    };
    std::array<std::vector<vector>,6> ends;
    for(unsigned i=0;i<6;++i) {
        const bool bar=i%2;
        auto a=p.angles[i],b=p.squares[i];
        if(bar) a=raise(a); else b=raise(b);
        ends[i].resize(sets);
        ends[i][0]=bar ? vector{-a[0],-a[1],b[0],b[1]} : vector{a[0],a[1],-b[0],-b[1]};
        for(mask photons=1;photons<sets;++photons) {
            vector x{};
            for(mask z=photons;z;z&=z-1) add(x,slash(eps[first(z)],ends[i][photons^(z&-z)],bar));
            ends[i][photons]=propagate(x,(1u<<i)|(photons<<6),bar);
        }
    }
    auto neutral=[&](int bar,int ket) {
        std::vector<vector> h(sets);
        for(mask photons=0;photons<sets;++photons) {
            for(mask d=photons;;d=(d-1)&photons) {
                add(h[photons],current_vector(ends[bar][d],ends[ket][photons^d]));
                if(!d) break;
            }
            for(auto& z:h[photons]) z/=s[(1u<<bar)|(1u<<ket)|(photons<<6)];
        }
        return h;
    };
    complex total{};
    std::array<int,3> electron{0,2,4}, positron{1,3,5};
    auto parity=[](const auto& legs) {
        return ((legs[0]>legs[1])+(legs[0]>legs[2])+(legs[1]>legs[2]))%2 ? -1 : 1;
    };
    do {
        do {
            auto left_pair=neutral(positron[0],electron[1]), right_pair=neutral(positron[1],electron[2]);
            std::vector<vector> x(sets), wbar(sets);
            for(mask photons=0;photons<sets;++photons) {
                for(mask d=photons;;d=(d-1)&photons) {
                    add(x[photons],slash(left_pair[d],ends[electron[0]][photons^d]));
                    add(wbar[photons],slash(right_pair[d],ends[positron[2]][photons^d],true));
                    if(!d) break;
                }
                for(mask z=photons;z;z&=z-1)
                    add(wbar[photons],slash(eps[first(z)],wbar[photons^(z&-z)],true));
                mask triple=(1u<<positron[1])|(1u<<electron[2])|(1u<<positron[2]);
                wbar[photons]=propagate(wbar[photons],triple|(photons<<6),true);
            }
            for(mask d=0;d<sets;++d)
                total-=real(parity(electron)*parity(positron))*contract(wbar[sets-1-d],x[d]);
        } while(std::next_permutation(positron.begin(),positron.end()));
    } while(std::next_permutation(electron.begin(),electron.end()));
    return std::pow(e,int(n)-2)*total;
}
}
