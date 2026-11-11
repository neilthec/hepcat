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

#include "one-minus-formulas.hpp"
#include "spinas-validation.hpp"
#include <iostream>
#include <iomanip>
#include <limits>
#include <random>
#include <stdexcept>
#include <string>
#include <vector>

// Independent transcription of Ochirov, arXiv:1802.06730v3, Eq. (58),
// restricted to four and five external legs. The BCJ relations in Eqs. (28),
// (53) reconstruct the full photon permutation sum, not only adjacent-minus
// color orderings. See literature-one-minus/README.md for the exact dictionary,
// declared common phase, and limitations. This does not call HEPCAT reduction,
// diagram or completion code. Only low-level SPINAS spinors are shared.
template<size_t N> struct Published {
 static_assert(N==2||N==3,"This independent transcription covers N=2,3 photons.");
 using Four = std::array<ldouble,4>;
 using Momenta = std::array<Four,N+2>;
 Momenta p; std::array<spinas::particle,N+2> particles; ldouble m,e;
 Published(const Momenta& q,ldouble mass,ldouble charge):p(q),m(mass),e(charge) {
  for(size_t i=0;i<N+2;++i){particles[i]=spinas::particle(i<2?m:0);particles[i].set_momentum(p[i].data());}
 }
 ldouble sign(int leg) const{return leg<2?1:-1;}
 static ldouble dot(const Four&a,const Four&b){return a[0]*b[0]-a[1]*b[1]-a[2]*b[2]-a[3]*b[3];}
 ldouble d(int a,int b) const{return sign(a)*sign(b)*dot(p[a],p[b]);}
 ldouble invariant(const std::vector<int>& set) const {Four q={};for(int i:set)for(int mu=0;mu<4;++mu)q[mu]+=sign(i)*p[i][mu];return dot(q,q);}
 cdouble aa(int a,int b,int s1=0,int s2=0){spinas::sproduct z(ANGLE,&particles[a],&particles[b]);cdouble v=a<2?(b<2?z.v(s1,s2):z.v(s1)):(b<2?z.v(s2):z.v());return sign(a)*sign(b)*v;}
 cdouble a2(int a,int j,int k,int b){spinas::sproduct z(ANGLE,&particles[a],&particles[j],&particles[k],&particles[b]);return sign(a)*sign(b)*sign(j)*sign(k)*z.v();}
 cdouble a1(int a,int j,int b){spinas::sproduct z(ANGLE,&particles[a],&particles[j],&particles[b]);return sign(a)*sign(j)*z.v();}
 // Eq. (15) uses <p|p=-m[p|. SPINAS has the opposite sign for
 // this relation, so one massive square spinor contributes a minus.
 cdouble square(int a,int b,int spin){spinas::sproduct z(SQUARE,&particles[a],&particles[b]);return -z.v(spin);}
 cdouble eq32(int r,int a,int s1,int s2){return a1(r,0,a)*(aa(0,r,s1)*square(1,a,s2)+square(0,a,s1)*aa(1,r,s2))/((invariant({0,r})-m*m)*invariant({r,a}));}
 cdouble primitive(int r,int a,int b,int s1,int s2) {
  std::vector<int> all={r,a};if(N==3)all.push_back(b);
  cdouble B=0,C=0;for(int j:all){B+=a2(r,0,j,r);C+=a2(r,0,j,N==2?a:b);}
  ldouble s=invariant(all);
  cdouble x=aa(0,1,s1,s2), y=aa(0,r,s1)*aa(1,r,s2);
  cdouble result=B*(x*B+y*s)/(m*s*C);
  if(N==3){
   cdouble Bk=a2(r,0,r,r)+a2(r,0,a,r);
   cdouble Ck=a2(r,0,r,a)+a2(r,0,a,a);
   cdouble Ek=a2(r,0,r,b)+a2(r,0,a,b);
   cdouble F=a1(r,r,b)+a1(r,a,b);
   ldouble sk=invariant({r,a}),D=invariant({0,r,a})-m*m;
   result+=m*aa(a,b)*Bk*F*(x*Bk+y*sk)/(sk*D*Ck*Ek);
  }
  return result/(aa(r,a)*(N==3?aa(a,b):cdouble(1,0)));
 }
 cdouble amplitude(int s1,int s2,bool adjacent_only=false,ldouble* bcj_error=nullptr) {
  int r=2,a=3,b=4;cdouble A=primitive(r,a,b,s1,s2),sum;
  if(N==2){sum=adjacent_only?A:A*(1+d(0,r)/d(0,a));}
  else{
   cdouble B=primitive(r,b,a,s1,s2);
   ldouble cr=d(0,r),ca=d(0,a),cb=d(0,b),ra=d(r,a),rb=d(r,b),ab=d(a,b);
   cdouble ar_b=-((ca+ra)*A+(ca+ra+ab)*B)/ca;
   cdouble br_a=-((cb+rb)*B+(cb+rb+ab)*A)/cb;
   cdouble ab_r=-(cr*A+(cr+ra)*ar_b)/(cr+ra+rb);
   cdouble ba_r=-(cr*B+(cr+rb)*br_a)/(cr+ra+rb);
   sum=adjacent_only?A+B:A+B+ar_b+br_a+ab_r+ba_r;
   // Four relations above construct the missing orderings. Check the two
   // remaining fundamental BCJ relations rather than solving those too.
   if(bcj_error){
    const cdouble t1=ca*ab_r,t2=(ca+ab)*ba_r,t3=(ca+ab+ra)*br_a;
    const cdouble u1=cb*ba_r,u2=(cb+ab)*ab_r,u3=(cb+ab+rb)*ar_b;
    const ldouble z1=std::abs(t1)+std::abs(t2)+std::abs(t3);
    const ldouble z2=std::abs(u1)+std::abs(u2)+std::abs(u3);
    *bcj_error=std::max(z1?std::abs(t1+t2+t3)/z1:0,z2?std::abs(u1+u2+u3)/z2:0);
   }
  }
  // T -> sqrt(2) e and the explicitly declared one-minus polarization phase.
  // This factor is fixed before evaluation and identical for N=2,3.
  ldouble coefficient=-1;for(size_t j=0;j<N;++j)coefficient*=std::sqrt(ldouble(2))*e;
  return coefficient*sum;
 }
};
struct Result {
 int photons=0,points=0,partial_sum_failures=0;
 ldouble amplitude_error=0,squared_error=0,eq32_error=0,bcj_error=0,partial_sum_error=0;
 bool pass()const{return amplitude_error<1e-8&&squared_error<1e-8&&eq32_error<1e-8&&bcj_error<1e-8&&partial_sum_failures==points;}
};
template<size_t N> Result check(int points_per_configuration,std::uint64_t seed) {
 Result result;result.photons=N;std::mt19937_64 rng(seed+N);
 for(ldouble mass:{ldouble(.01),ldouble(1),ldouble(7)})for(ldouble charge:{ldouble(.2),ldouble(1.3)}){
  std::array<ldouble,N+2> masses={};masses[0]=masses[1]=mass;
  hepcat_spinas::ValidationOptions options;options.minimum_center_of_mass_energy=8*mass;options.maximum_center_of_mass_energy=80*mass;
  for(int point=0;point<points_per_configuration;++point){auto p=hepcat_spinas::make_random_validation_momenta(masses,{{0,1}},rng,options);
   Published<N> published(p,mass,charge);hepcat_one_minus::Formula<N> ours(p,mass,charge,0);
   ldouble error=0,scale=0,partial_error=0,partial_scale=0,square_a=0,square_b=0;
   ldouble control_error=0,control_scale=0;
   for(int s1:{-1,1})for(int s2:{-1,1}){
    ldouble bcj_error=0;
    cdouble A=ours.amplitude(s1,s2),B=published.amplitude(s1,s2,false,&bcj_error);
    cdouble partial=published.amplitude(s1,s2,true);
    for(const cdouble value:{A,B,partial})
     if(!std::isfinite(value.real())||!std::isfinite(value.imag()))
      throw std::runtime_error("A validation sample produced a non-finite amplitude.");
    if(!std::isfinite(bcj_error))throw std::runtime_error("A BCJ control was non-finite.");
    error=std::max(error,std::abs(A-B));scale=std::max(scale,std::max(std::abs(A),std::abs(B)));
    partial_error=std::max(partial_error,std::abs(A-partial));partial_scale=std::max(partial_scale,std::max(std::abs(A),std::abs(partial)));
    square_a+=std::norm(A);square_b+=std::norm(B);
    result.bcj_error=std::max(result.bcj_error,bcj_error);
    if(N==2){
     cdouble p32=published.eq32(2,3,s1,s2),p58=published.primitive(2,3,4,s1,s2);
     for(const cdouble value:{p32,p58})
      if(!std::isfinite(value.real())||!std::isfinite(value.imag()))
       throw std::runtime_error("The independent four-point control was non-finite.");
     control_error=std::max(control_error,std::abs(p32-p58));
     control_scale=std::max(control_scale,std::max(std::abs(p32),std::abs(p58)));
    }
   }
   result.amplitude_error=std::max(result.amplitude_error,error/scale);
   result.squared_error=std::max(result.squared_error,std::abs(square_a-square_b)/std::max(square_a,square_b));
   result.eq32_error=std::max(result.eq32_error,control_scale?control_error/control_scale:0);
   result.partial_sum_error=std::max(result.partial_sum_error,partial_error/partial_scale);
   if(partial_error/partial_scale>1e-6)++result.partial_sum_failures;
   ++result.points;
  }
 }
 return result;
}
void print(const Result&r){
 std::cout<<"    {\"photons\": "<<r.photons<<", \"points\": "<<r.points
  <<", \"complex_spin_components\": "<<4*r.points
  <<", \"max_complex_amplitude_error\": "<<r.amplitude_error
  <<", \"max_spin_summed_squared_error\": "<<r.squared_error
  <<", \"eq32_vs_eq58_error\": "<<r.eq32_error
  <<", \"unused_bcj_relation_error\": "<<r.bcj_error
  <<", \"incomplete_permutation_sum_failures\": "<<r.partial_sum_failures
  <<", \"incomplete_permutation_sum_max_error\": "<<r.partial_sum_error
  <<", \"pass\": "<<(r.pass()?"true":"false")<<"}";
}
int main(int argc,char**argv){
 try{
  int points=1000;std::uint64_t seed=731042;
  for(int i=1;i<argc;++i){std::string option=argv[i];
   if(option=="--points-per-configuration"&&i+1<argc)points=std::stoi(argv[++i]);
   else if(option=="--seed"&&i+1<argc)seed=std::stoull(argv[++i]);
   else throw std::invalid_argument("Use --points-per-configuration N and --seed N.");
  }
  if(points<1)throw std::invalid_argument("Use at least one point per mass/charge configuration.");
  const Result a=check<2>(points,seed),b=check<3>(points,seed);
  std::cout<<std::setprecision(18)<<"{\n  \"source\": \"Ochirov arXiv:1802.06730v3 Eqs.58,53,32\",\n"
   <<"  \"mantissa_bits\": "<<std::numeric_limits<ldouble>::digits<<",\n"
   <<"  \"seed\": "<<seed<<",\n  \"points_per_mass_charge_configuration\": "<<points<<",\n"
   <<"  \"masses\": [0.01,1,7],\n  \"charges\": [0.2,1.3],\n"
   <<"  \"center_of_mass_energy_over_mass\": [8,80],\n"
   <<"  \"qed_multiplier_for_A_divided_by_i\": \"-(sqrt(2)*e)^N; fixed one-minus phase, no fitted ratios\",\n"
   <<"  \"results\": [\n";print(a);std::cout<<",\n";print(b);
  std::cout<<"\n  ],\n  \"pass\": "<<(a.pass()&&b.pass()?"true":"false")<<"\n}\n";
  return !(a.pass()&&b.pass());
 }catch(const std::exception&error){std::cerr<<error.what()<<"\n";return 2;}
}
