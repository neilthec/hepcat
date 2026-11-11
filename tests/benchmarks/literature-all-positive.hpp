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

#pragma once

// Independent transcription of A. Ochirov, JHEP 04 (2018) 089,
// arXiv:1802.06730v3, Eq. (35), followed by the Abelian permutation sum.
// https://arxiv.org/html/1802.06730v3#S4.E35
//
// Ordered primitive, with its overall i removed:
// m <1^a 2^b> [r1| product_{j=1}^{N-2}
//   { slash(P_j) slash(q_{r(j+1)}) + (P_j^2-m^2) I } |rN]
// / [ product_{j=1}^{N-1}(P_j^2-m^2)
//     product_{j=1}^{N-1}<rj r(j+1)> ],
// where P_j=q1+q_r1+...+q_rj and (r1,...,rN) is a photon ordering.
//
// QED normalization: the ordered rules have a 1/sqrt(2) vertex and
// Tr(T^a T^b)=delta^{ab}, as stated in Eq. (24) and its footnote 5.
// Replacing each normalized color generator by sqrt(2)*e gives
// M_QED=(sqrt(2)*e)^N sum_{permutations} A_ordered/i.
// This one normalization is used for every multiplicity, with no fitted ratios.
// N=2 is an explicit control, not a fitted normalization.
//
// Input: e- e+ -> N positive-helicity photons, positive-energy physical
// momenta. In the analytic all-incoming convention q1=p1, q2=p2,
// q_photon=-k_photon. Outgoing angle spinors change sign, square spinors
// do not. There are two photon angle spinors per denominator bracket;
// these signs cancel. The even slashed products need no extra spinor sign.
// All internal P_j are ordinary off-shell 2x2 matrices, never on-shell
// particle objects. No internal on-shell identities are applied.
//
// This implementation does not use either HEPCAT subset current, any
// constructive diagram, or any completion/reduction function. Only the
// low-level SPINAS spinors and matrices are shared with the tested formulas.
#include "spinas.h"
#include <algorithm>
#include <array>
#include <cmath>
#include <cstddef>
#include <cstdint>
#include <stdexcept>

namespace hepcat_literature_all_positive {

template<std::size_t N> class Ochirov35 {
 public:
  static_assert(N >= 2 && N <= 9, "Factorial validation supports 2..9 photons.");
  using FourMomentum=std::array<ldouble,4>;
  using Momenta=std::array<FourMomentum,N+2>;

  Ochirov35(const Momenta& momenta, ldouble mass, ldouble charge)
      : mass_(mass), charge_(charge), coefficient_(0), first_ordering_(0), cancellation_(0) {
    if (!(mass>0) || !std::isfinite(mass) || !std::isfinite(charge))
      throw std::invalid_argument("Require a finite positive mass and finite charge.");
    for(std::size_t i=0;i<N+2;++i){
      particles_[i]=spinas::particle(i<2?mass:ldouble(0));
      particles_[i].set_momentum(momenta[i].data());
    }
    spinas::sproduct angle12(ANGLE,&particles_[0],&particles_[1]);
    for(int a:{-1,1})for(int b:{-1,1})angle12_[index(a)*2+index(b)]=angle12.v(a,b);
    std::array<std::size_t,N> order;
    for(std::size_t i=0;i<N;++i)order[i]=i;
    cdouble sum(0), correction(0); ldouble absolute_sum=0;
    bool first=true;
    do {
      const cdouble term=ordered(momenta,order);
      if(first){first_ordering_=term;first=false;}
      // Compensated summation reduces numerical cancellation across the
      // published color-ordering sum without changing the analytic formula.
      const cdouble adjusted=term-correction;
      const cdouble next=sum+adjusted;
      correction=(next-sum)-adjusted;
      sum=next; absolute_sum+=std::abs(term);
    }while(std::next_permutation(order.begin(),order.end()));
    ldouble coupling=1;
    for(std::size_t i=0;i<N;++i)coupling*=std::sqrt(ldouble(2))*charge;
    coefficient_=mass*coupling*sum;
    first_ordering_*=mass*coupling;
    cancellation_=std::abs(sum)?absolute_sum/std::abs(sum):0;
  }

  cdouble amplitude(int a,int b)const{return coefficient_*angle12_[index(a)*2+index(b)];}
  cdouble coefficient()const{return coefficient_;}
  // Deliberately incomplete Abelianization, for a benchmark negative control.
  cdouble first_ordering_coefficient()const{return first_ordering_;}
  ldouble cancellation_condition()const{return cancellation_;}

 private:
  ldouble mass_,charge_;
  std::array<spinas::particle,N+2> particles_;
  std::array<cdouble,4> angle12_;
  cdouble coefficient_;
  cdouble first_ordering_;
  ldouble cancellation_;
  static std::size_t index(int s){
    if(s!=-1&&s!=1)throw std::invalid_argument("Massive doubled spins must be +/-1.");
    return s==-1?0:1;
  }
  static ldouble dot(const FourMomentum&a,const FourMomentum&b){
    return std::fma(-a[3],b[3],std::fma(-a[2],b[2],std::fma(-a[1],b[1],a[0]*b[0])));
  }
  cdouble ordered(const Momenta&p,const std::array<std::size_t,N>&r){
    FourMomentum cumulative=p[0];
    spinas::cmatrix product(cdouble(1),cdouble(0),cdouble(0),cdouble(1));
    ldouble pole_product=1;cdouble angle_product(1);
    ldouble pole=0;
    for(std::size_t j=0;j<N-1;++j){
      const auto photon=r[j]+2;
      // Expanded invariant P_j^2-m^2 using external on-shell conditions only.
      // The pairwise form avoids subtracting almost equal squared masses.
      pole-=2*dot(p[0],p[photon]);
      for(std::size_t k=0;k<j;++k)pole+=2*dot(p[r[k]+2],p[photon]);
      for(std::size_t mu=0;mu<4;++mu)cumulative[mu]-=p[photon][mu];
      pole_product*=pole;
      spinas::sproduct angle(ANGLE,&particles_[photon],&particles_[r[j+1]+2]);
      angle_product*=angle.v();
      if(j<N-2){
        FourMomentum next;
        for(std::size_t mu=0;mu<4;++mu)next[mu]=-p[r[j+1]+2][mu];
        spinas::cmatrix first(cumulative.data(),UPPER),second(next.data(),LOWER);
        spinas::cmatrix scalar(cdouble(pole),cdouble(0),cdouble(0),cdouble(pole));
        product*=first*second+scalar;
      }
    }
    const cdouble numerator=particles_[r[0]+2].lsquare()*product*particles_[r[N-1]+2].rsquare();
    if(pole_product==0||angle_product==cdouble(0))
      throw std::runtime_error("The literature formula hits a singular denominator.");
    return numerator/pole_product/angle_product;
  }
};
// Independent five-point transcription of Das and Zeng, arXiv:2608.18218v1,
// PDF Eq. (4.10), HTML global Eq. (77), specialized to s=1/2 and with i
// removed and e^3 restored. Their (1,2,3,4,5) becomes our (1,3,4,5,2).
// q is the sum of the three analytic photon momenta. This is the complete
// six-permutation QED amplitude, unlike their color-ordered PDF Eq. (3.44).
inline cdouble das_zeng_410_coefficient(const std::array<std::array<ldouble,4>,5>&p,
                                      ldouble mass,ldouble charge){
  std::array<spinas::particle,5> part;
  std::array<ldouble,4> q={};
  for(std::size_t i=0;i<5;++i){
    part[i]=spinas::particle(i<2?mass:ldouble(0));part[i].set_momentum(p[i].data());
    if(i>=2)for(std::size_t mu=0;mu<4;++mu)q[mu]-=p[i][mu];
  }
  auto dot=[](const std::array<ldouble,4>&a,const std::array<ldouble,4>&b){
    return std::fma(-a[3],b[3],std::fma(-a[2],b[2],std::fma(-a[1],b[1],a[0]*b[0])));
  };
  spinas::cmatrix p1(p[0].data(),UPPER),total(q.data(),LOWER);
  const spinas::cmatrix chain=p1*total;
  std::array<std::size_t,3> order={{2,3,4}};cdouble sum(0),correction(0);
  do{
    const auto a=order[0],b=order[1],c=order[2];
    spinas::sproduct ab(ANGLE,&part[a],&part[b]),bc(ANGLE,&part[b],&part[c]);
    const cdouble term=(part[a].lsquare()*chain*part[c].rsquare())/
      ((-2*dot(p[0],p[a]))*(-2*dot(p[1],p[c]))*ab.v()*bc.v());
    const cdouble adjusted=term-correction,next=sum+adjusted;
    correction=(next-sum)-adjusted;sum=next;
  }while(std::next_permutation(order.begin(),order.end()));
  return 2*std::sqrt(ldouble(2))*charge*charge*charge*mass*sum;
}
} // namespace hepcat_literature_all_positive
