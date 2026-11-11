// Run from a directory in which reports should be written. Example:
// clang++ -O2 -std=c++11 -DWITH_LONG_DOUBLE -I/path/to/spinas/include
//   -I/path/to/spinas -I/path/to/hepcat/source/internal/export -I/path/to/hepcat/benchmarks
//   literature-all-positive-validation.cpp /path/to/libspinas.a -o literature-all-positive
// ./literature-all-positive [points_per_N=1000] [prefix=literature-all-positive]
/*
HEPCAT - High Energy Physics Constructive Amplitude Tool

Copyright (C) 2026 Neil Christensen

This file is part of HEPCAT.

HEPCAT is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

HEPCAT is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with HEPCAT.  If not, see <https://www.gnu.org/licenses/>.
*/

#include "literature-all-positive.hpp"
#include "all-positive-formulas.hpp"
#include "spinas-validation.hpp"
#include <fstream>
#include <iomanip>
#include <iostream>
#include <limits>
#include <random>
#include <string>

namespace {
template<std::size_t N> ldouble das_zeng_error(const std::array<std::array<ldouble,4>,N+2>&,
    ldouble,ldouble,const cdouble&){return 0;}
template<> ldouble das_zeng_error<3>(const std::array<std::array<ldouble,4>,5>&p,
    ldouble mass,ldouble charge,const cdouble&known){
  const auto result=hepcat_literature_all_positive::das_zeng_410_coefficient(p,mass,charge);
  return std::abs(result-known)/std::max(std::abs(result),std::abs(known));
}
template<std::size_t N> void run(std::size_t samples,const std::string&prefix){
  constexpr std::uint64_t seed=180206730+N;
  std::mt19937_64 rng(seed);
  std::ofstream report(prefix+"-N"+std::to_string(N)+".csv");
  report<<std::setprecision(20)<<"point,mass,charge,physical_complex_error,spurious_complex_error,spin_sum_physical_error,spin_sum_spurious_error,literature_condition\n";
  ldouble worstp=0,worsts=0,worstsqp=0,worstsqs=0,worstcondition=0,worstdz=0;
  ldouble incomplete_error=0,wrong_coupling_error=0;
  std::size_t failures=0;
  for(std::size_t sample=0;sample<samples;++sample){
    const ldouble masses_cases[]={ldouble(0.1L),ldouble(1),ldouble(1.5L),ldouble(3)};
    const ldouble charge_cases[]={ldouble(1)/3,ldouble(2)/7,ldouble(-0.25L),ldouble(0.7L)};
    const ldouble m=masses_cases[sample%4],e=charge_cases[sample%4];
    std::array<ldouble,N+2> masses={};masses[0]=masses[1]=m;
    hepcat_spinas::ValidationOptions options;
    options.minimum_center_of_mass_energy=8*m;options.maximum_center_of_mass_energy=80*m;
    auto p=hepcat_spinas::make_random_validation_momenta(masses,{{0,1}},rng,options);
    hepcat_literature_all_positive::Ochirov35<N> known(p,m,e);
    hepcat_all_positive::Formulas<N> ours(p,m,e);
    if(sample==0){
      const auto full=known.coefficient(),incomplete=known.first_ordering_coefficient();
      const auto wrong_coupling=full/std::pow(std::sqrt(ldouble(2)),int(N));
      incomplete_error=std::abs(full-incomplete)/std::max(std::abs(full),std::abs(incomplete));
      wrong_coupling_error=std::abs(full-wrong_coupling)/std::max(std::abs(full),std::abs(wrong_coupling));
      if(incomplete_error<1e-4L||wrong_coupling_error<1e-4L)
        throw std::runtime_error("An incomplete Abelianization or wrong coupling was not rejected.");
    }
    ldouble scale=0,dp=0,ds=0,sqknown=0,sqp=0,sqs=0;
    for(int a:{-1,1})for(int b:{-1,1}){
      const auto old=known.amplitude(a,b);const auto now=ours.amplitudes(a,b);
      scale=std::max(scale,std::max(std::abs(old),std::max(std::abs(now.physical),std::abs(now.spurious))));
      dp=std::max(dp,std::abs(old-now.physical));ds=std::max(ds,std::abs(old-now.spurious));
      sqknown+=std::norm(old);sqp+=std::norm(now.physical);sqs+=std::norm(now.spurious);
    }
    const ldouble errp=dp/scale,errs=ds/scale;
    const ldouble errsp=std::abs(sqknown-sqp)/std::max(sqknown,sqp);
    const ldouble errss=std::abs(sqknown-sqs)/std::max(sqknown,sqs);
    worstp=std::max(worstp,errp);worsts=std::max(worsts,errs);
    worstsqp=std::max(worstsqp,errsp);worstsqs=std::max(worstsqs,errss);
    worstcondition=std::max(worstcondition,known.cancellation_condition());
    const ldouble dz=das_zeng_error<N>(p,m,e,known.coefficient());worstdz=std::max(worstdz,dz);
    if(!std::isfinite(errp)||!std::isfinite(errs)||!std::isfinite(errsp)||
       !std::isfinite(errss)||!std::isfinite(dz)||errp>1e-7L||errs>1e-7L||
       errsp>1e-7L||errss>1e-7L||dz>1e-7L)++failures;
    report<<sample+1<<','<<m<<','<<e<<','<<errp<<','<<errs<<','<<errsp<<','<<errss<<','<<known.cancellation_condition()<<'\n';
  }
  std::ofstream out(prefix+"-N"+std::to_string(N)+".json");
  out<<std::setprecision(20)<<"{\n\"Source\":\"Ochirov arXiv:1802.06730v3 Eq.35, Abelianized complete permutation sum\",\n"
    <<"\"Photons\":"<<N<<",\n\"Samples\":"<<samples<<",\n\"ComplexSpinComparisons\":"<<4*samples<<",\n"
    <<"\"Seed\":"<<seed<<",\n\"MantissaBits\":"<<std::numeric_limits<ldouble>::digits<<",\n"
    <<"\"Masses\":[0.1,1,1.5,3],\n\"Charges\":[0.3333333333333333,0.2857142857142857,-0.25,0.7],\n"
    <<"\"EnergyOverMassRange\":[8,80],\n\"Tolerance\":1e-7,\n\"Failures\":"<<failures<<",\n"
    <<"\"WorstComplexPhysicalError\":"<<worstp<<",\n\"WorstComplexSpuriousError\":"<<worsts<<",\n"
    <<"\"WorstSquaredPhysicalError\":"<<worstsqp<<",\n\"WorstSquaredSpuriousError\":"<<worstsqs<<",\n"
    <<"\"WorstLiteratureCancellationCondition\":"<<worstcondition<<",\n"
    <<"\"DasZeng410Compared\":"<<(N==3?"true":"false")<<",\n\"WorstDasZeng410Error\":"<<worstdz<<",\n"
    <<"\"NegativeControlIncompletePermutationError\":"<<incomplete_error<<",\n"
    <<"\"NegativeControlWrongCouplingError\":"<<wrong_coupling_error<<"\n}\n";
  std::cout<<"N="<<N<<", points="<<samples<<", failures="<<failures<<", complex physical="<<worstp
    <<", spurious="<<worsts<<", permutation condition="<<worstcondition<<", DasZeng410="<<worstdz<<std::endl;
  if(failures)throw std::runtime_error("Literature validation failed.");
}
}
int main(int argc,char**argv){
  try{
    const std::size_t samples=argc>1?std::stoull(argv[1]):1000;
    if(samples==0)throw std::invalid_argument("Use at least one phase-space point per multiplicity.");
    const std::string prefix=argc>2?argv[2]:"literature-all-positive";
    std::cout<<std::setprecision(10);
    run<2>(samples,prefix);run<3>(samples,prefix);run<4>(samples,prefix);run<5>(samples,prefix);run<6>(samples,prefix);
  }catch(const std::exception&error){std::cerr<<error.what()<<'\n';return 1;}
}
