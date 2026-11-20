#include "process.hpp"
#include "feynman-sig.hpp"
#include "spinas-validation.hpp"
#include <array>
#include <chrono>
#include <cmath>
#include <cstdlib>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <limits>
#include <random>
#include <vector>

using Candidate=hepcat_generated::CONSTRUCTIVE_PROCESS;
using Feynman=hepcat_generated::FEYNMAN_PROCESS;
struct row {
    std::uint64_t points=0, components=0, nonfinite=0, component_outliers=0, tensor_outliers=0, worst=0;
    long double component_max=0, tensor_max=0;
    void compare(const std::vector<cdouble>& values,const std::vector<cdouble>& refs,std::size_t point) {
        ++points;
        long double max_diff=0, max_ref=0;
        for(std::size_t k=0;k<refs.size();++k) {
            ++components;
            const auto diff=std::abs(values[k]-refs[k]), ref=std::abs(refs[k]);
            if(!std::isfinite(diff)||!std::isfinite(ref)) {++nonfinite;continue;}
            max_diff=std::max(max_diff,static_cast<long double>(diff));
            max_ref=std::max(max_ref,static_cast<long double>(ref));
            auto relative=ref? diff/ref:diff?INFINITY:0;
            component_outliers += relative>1e-7;
            component_max=std::max(component_max,static_cast<long double>(relative));
        }
        auto relative=max_ref?max_diff/max_ref:max_diff?INFINITY:0;
        tensor_outliers+=relative>1e-7;
        if(relative>tensor_max) {tensor_max=relative;worst=point;}
    }
};
int main(int argc,char** argv) {
    Candidate candidate;
    Feynman feynman;
    candidate.set_widths_enabled(false);
    feynman.set_widths_enabled(false);
    if(candidate.masses()!=feynman.masses() || candidate.incoming_legs()!=feynman.incoming_legs()) return 2;
    hepcat_spinas::ValidationOptions opts;
    opts.seed=861811;
    opts.points=argc>1?std::stoull(argv[1]):100000;
    std::mt19937_64 rng(opts.seed);
    const auto mass=candidate.masses(); const auto helicities=feynman.helicity_configurations();
    using states_t=std::vector<Candidate::Spins>;
    std::array<states_t,Feynman::helicity_count> states;
    for(std::size_t h=0;h<states.size();++h) {
        states[h].push_back(helicities[h]);
        for(std::size_t i=0;i<mass.size();++i) if(mass[i]) {
           states_t v;
           for(auto s:states[h]) {
               for(int sign:{1,-1}) {s[i]=sign;v.push_back(s);}
           }
           states[h]=std::move(v);
        }
    }
    std::vector<std::pair<ldouble,cdouble>> rotations;
    for(std::size_t i=0;i<mass.size();++i) if(mass[i]) {
        double theta=0.31+0.09*i,phase=0.42+0.37*i;
        rotations.emplace_back(std::cos(theta),std::polar(ldouble(std::sin(theta)),ldouble(phase)));
    }
    auto rotate=[&](auto& v) {
        std::size_t stride=v.size();
        for(auto u:rotations) {
            stride/=2;
            for(std::size_t b=0;b<v.size();b+=2*stride) for(std::size_t k=0;k<stride;++k) {
                auto x=v[b+k],y=v[b+k+stride];
                v[b+k]=u.first*x+u.second*y;
                v[b+k+stride]=-std::conj(u.second)*x+u.first*y;
            }
        }
    };
    std::array<std::array<row,2>,Feynman::helicity_count> rows{};
    std::vector<cdouble> actual,ref;
    for(std::size_t ip=0;ip<opts.points;++ip) {
         auto p=hepcat_spinas::make_random_validation_momenta(mass,candidate.incoming_legs(),rng,opts);
         candidate.set_momenta(p);feynman.set_momenta(p);
         for(std::size_t h=0;h<states.size();++h) {
             actual.clear();ref.clear();
             for(const auto& spins:states[h]) {
                 actual.push_back(candidate.amp(spins));ref.push_back(feynman.amp(spins));
             }
             rows[h][0].compare(actual,ref,ip);
             rotate(actual);rotate(ref);
             rows[h][1].compare(actual,ref,ip);
         }
    }
    std::cout<<"basis,helicities,points,components,nonfinite,component_outliers,component_max,tensor_outliers,tensor_max,worst_point\n"<<std::setprecision(17);
    for(std::size_t h=0;h<states.size();++h) for(std::size_t b=0;b<2;++b) {
        auto r=rows[h][b];std::cout<<(b?"rotated":"aligned")<<",";
        for(int spin:helicities[h])std::cout<<(spin==0?"0":spin>0?"+":"-");
        std::cout<<","<<r.points<<","<<r.components<<","<<r.nonfinite<<","<<r.component_outliers<<","<<r.component_max<<","<<r.tensor_outliers<<","<<r.tensor_max<<","<<r.worst<<"\n";
    }
}
