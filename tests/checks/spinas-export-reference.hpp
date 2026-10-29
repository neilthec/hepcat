#pragma once

// Independent native-SPINAS oracles for the generated-process tests.
// They use physical positive-energy momenta, with legs 1 and 2 incoming.
#include <array>
#include <algorithm>
#include <cmath>
#include <complex>
#include <iostream>
#include <limits>
#include <stdexcept>
#include <tuple>
#include <vector>
#include "spinas.h"
#include "spinas-runtime.hpp"
#include "SM/include/eeAA.h"

namespace hepcat_export_reference {

template<std::size_t N>
using Momenta = std::array<std::array<ldouble, 4>, N>;

inline ldouble dot(const std::array<ldouble, 4>& a,
                   const std::array<ldouble, 4>& b) {
  return a[0]*b[0]-a[1]*b[1]-a[2]*b[2]-a[3]*b[3];
}

template<std::size_t N>
std::array<spinas::particle, N> particles(const Momenta<N>& momenta,
                                       const std::array<ldouble, N>& masses) {
  std::array<spinas::particle, N> result;
  for (std::size_t i=0; i<N; ++i) {
    result[i] = spinas::particle(masses[i]);
    result[i].set_momentum(momenta[i].data());
  }
  return result;
}

template<std::size_t N>
ldouble electron_denominator(const Momenta<N>& momenta, std::size_t initial,
                            std::size_t photon, ldouble mass) {
  std::array<ldouble, 4> q;
  for (std::size_t mu=0; mu<4; ++mu)
    q[mu] = momenta[initial][mu] - momenta[photon][mu];
  return dot(q,q)-mass*mass;
}

inline cdouble five_all_plus(const Momenta<5>& momenta,
                             const std::array<ldouble,5>& masses,
                             ldouble charge, int spin1, int spin2) {
  auto p=particles(momenta,masses);
  spinas::sproduct angle12(ANGLE,&p[0],&p[1]);
  cdouble total(0,0);
  ldouble denominator=1;
  for (std::size_t c=2; c<5; ++c) {
    std::array<std::size_t,2> other;
    std::size_t cursor=0;
    for (std::size_t a=2; a<5; ++a) if (a!=c) other[cursor++]=a;
    spinas::sproduct square(SQUARE,&p[other[0]],&p[other[1]]);
    spinas::sproduct k(SQUARE,&p[c],&p[0],&p[1],&p[c]);
    const cdouble bracket=square.v();
    total+=bracket*bracket*k.v()/electron_denominator(momenta,1,c,masses[0]);
    denominator*=electron_denominator(momenta,0,c,masses[0]);
  }
  return ldouble(2.L)*std::sqrt(ldouble(2.L))*charge*charge*charge*masses[0]*
    angle12.v(spin1,spin2)*total/denominator;
}

inline cdouble six_all_plus(const Momenta<6>& momenta,
                            const std::array<ldouble,6>& masses,
                            ldouble charge, int spin1, int spin2) {
  auto p=particles(momenta,masses);
  spinas::sproduct angle12(ANGLE,&p[0],&p[1]);
  const cdouble common=ldouble(4.L)*charge*charge*charge*charge*
    angle12.v(spin1,spin2);
  ldouble electron_poles=1;
  for (std::size_t a=2; a<6; ++a)
    electron_poles*=electron_denominator(momenta,0,a,masses[0]);
  cdouble total(0,0);
  for (std::size_t a=2; a<6; ++a) for (std::size_t b=a+1; b<6; ++b) {
    std::array<std::size_t,2> other;
    std::size_t cursor=0;
    for (std::size_t x=2; x<6; ++x) if (x!=a && x!=b) other[cursor++]=x;
    spinas::sproduct pair(SQUARE,&p[a],&p[b]);
    spinas::sproduct complement(SQUARE,&p[other[0]],&p[other[1]]);
    spinas::sproduct ka(SQUARE,&p[a],&p[0],&p[1],&p[a]);
    spinas::sproduct kb(SQUARE,&p[b],&p[0],&p[1],&p[b]);
    std::array<ldouble,4> q;
    for (std::size_t mu=0; mu<4; ++mu)
      q[mu]=momenta[0][mu]-momenta[other[0]][mu]-momenta[other[1]][mu];
    const ldouble triple=dot(q,q)-masses[0]*masses[0];
    const ldouble pair_poles=electron_denominator(momenta,1,a,masses[0])*
      electron_denominator(momenta,1,b,masses[0]);
    const cdouble paired=pair.v(), remaining=complement.v();
    total-=common*masses[0]*masses[0]*masses[0]*paired*paired*remaining*remaining/
      (electron_denominator(momenta,0,other[0],masses[0])*
       electron_denominator(momenta,0,other[1],masses[0])*pair_poles*triple);
    total+=common*masses[0]*remaining*remaining*ka.v()*kb.v()/
      (electron_poles*pair_poles);
  }
  return total;
}

inline cdouble composite_width(const Momenta<4>& momenta,
                                const std::array<ldouble,4>& masses,
                                const std::array<int,4>& spins) {
  auto p=particles(momenta,masses);
  std::array<ldouble,4> q;
  for (std::size_t mu=0; mu<4; ++mu) q[mu]=momenta[0][mu]-momenta[2][mu];
  const cdouble angle_chain=p[0].langle(spins[0])*
    spinas::cmatrix(q.data(),LOWER)*p[1].umat()*p[2].rangle(spins[2]);
  const cdouble square=p[1].lsquare(spins[1])*p[3].rsquare(spins[3]);
  const cdouble coefficient=cdouble(ldouble(1.L)/ldouble(3.L),ldouble(1.L)/ldouble(7.L))*std::sqrt(ldouble(2.L))*std::acos(-ldouble(1.L));
  const ldouble scalar=-dot(momenta[0],momenta[2])+masses[0]*masses[0]/ldouble(5.L);
  const cdouble denominator(dot(q,q)-masses[0]*masses[0],masses[0]/ldouble(11.L));
  // The outgoing angle spinor on leg 3 contributes the displayed minus.
  return -coefficient*scalar*angle_chain*square/denominator;
}

inline cdouble vector_tensor(const Momenta<4>& momenta,
                              const std::array<ldouble,4>& masses,
                              int spin1,int spin2) {
  auto p=particles(momenta,masses);
  spinas::sproduct angle(ANGLE,&p[0],&p[1]);
  spinas::sproduct square(SQUARE,&p[0],&p[1]);
  spinas::process native;
  int a1,b1,a2,b2;
  const int count=native.get_num_spin_loops(spin1,spin2);
  const ldouble normalization=native.get_spin_normalization(spin1,spin2);
  cdouble result(0,0);
  for (int i=0; i<count; ++i) {
    native.get_spinor_spins(spin1,a1,b1,spin2,a2,b2,i);
    result+=normalization*angle.v(a1,a2)*square.v(b1,b2);
  }
  return result;
}

inline bool agree(cdouble calculated,cdouble expected,const char* label,
                  std::size_t sample,int first,int second) {
  const ldouble scale=std::max(std::abs(calculated),std::abs(expected));
  const ldouble error=std::abs(calculated-expected);
  if (!std::isfinite(std::real(calculated)) || !std::isfinite(std::imag(calculated)) ||
      !std::isfinite(std::real(expected)) || !std::isfinite(std::imag(expected)) ||
      error>ldouble(2.e-10L)*scale+ldouble(1.e-25L)) {
    std::cerr<<label<<" sample="<<sample<<" spins="<<first<<","<<second
      <<" calculated="<<calculated<<" expected="<<expected<<" error="<<error<<"\n";
    return false;
  }
  return true;
}

} // namespace hepcat_export_reference

namespace hepcat_export_reference {

template<std::size_t N> struct oracle;

template<> struct oracle<4> {
  static cdouble value(const Momenta<4>& point,const std::array<ldouble,4>& masses,
                       const std::array<int,4>& spins,int kind,int photon3,int photon4) {
    if (kind==1 || kind==8) {
      spinas::eeAA native(ldouble(1.L)/ldouble(3.L),masses[0]);
      native.set_momenta(point[0].data(),point[1].data(),point[2].data(),point[3].data());
      // models/amps3.mdl explicitly uses -2*EE^2 for mixed helicities,
      // while SM/eeAA.cpp uses +2*e^2. Two outgoing angle factors
      // cancel each other, so crossing preserves that specified minus.
      // This fixed input convention is identified algebraically, never fitted.
      const int model_sign=(kind==1 && photon3!=photon4) ? -1 : 1;
      return ldouble(model_sign)*native.amp(spins[0],spins[1],photon3,photon4);
    }
    if (kind==4) return composite_width(point,masses,spins);
    if (kind==5) return vector_tensor(point,masses,spins[0],spins[1]);
    if (kind==6) return -dot(point[0],point[1]);
    if (kind==7) return ldouble(2.L)*(dot(point[0],point[0])-dot(point[0],point[2]));
    throw std::runtime_error("Unknown four-point test oracle");
  }
};

template<> struct oracle<5> {
  static cdouble value(const Momenta<5>& point,const std::array<ldouble,5>& masses,
                       const std::array<int,5>& spins,int,int,int) {
    return five_all_plus(point,masses,ldouble(1.L)/ldouble(3.L),spins[0],spins[1]);
  }
};

template<> struct oracle<6> {
  static cdouble value(const Momenta<6>& point,const std::array<ldouble,6>& masses,
                       const std::array<int,6>& spins,int,int,int) {
    return six_all_plus(point,masses,ldouble(1.L)/ldouble(3.L),spins[0],spins[1]);
  }
};

template<typename Spins>
std::vector<Spins> add_spin_leg(const std::vector<Spins>& states,std::size_t leg,int rank) {
  std::vector<Spins> result;
  for (const auto& state:states) for (int spin=-rank;spin<=rank;spin+=2) {
    Spins next=state;
    next[leg]=spin;
    result.push_back(next);
  }
  return result;
}

template<typename Generated>
int run_process(int kind,int photon3=2,int photon4=2) {
  try {
    Generated generated;
    const auto masses=generated.masses();
    const auto incoming=Generated::incoming_legs();
    constexpr std::size_t N=std::tuple_size<typename Generated::Momenta>::value;
    const auto initial=Generated::default_spins();
    if (((kind==1 || kind==8) && (initial[2]!=photon3 || initial[3]!=photon4)) ||
        incoming[0]!=0 || incoming[1]!=(kind==6 ? 2u : 1u)) {
      std::cerr<<"Fixed helicity or incoming-leg metadata disagrees with the request\n";
      return 5;
    }
    if (N>4) for (std::size_t leg=2;leg<N;++leg) if (initial[leg]!=2) return 5;
    std::vector<typename Generated::Spins> states(1,initial);
    ldouble average=1;
    if (kind==5) {
      states=add_spin_leg(states,0,2); states=add_spin_leg(states,1,2); average=9;
    } else if (kind!=6 && kind!=7) {
      states=add_spin_leg(states,0,1); states=add_spin_leg(states,1,1); average=4;
      if (kind==4) { states=add_spin_leg(states,2,1); states=add_spin_leg(states,3,1); }
    }
    std::size_t comparisons=0;
    ldouble maximum=0;
    for (unsigned sample=0;sample<3;++sample) {
      const auto point=hepcat_spinas::make_example_momenta<N>(masses,incoming,ldouble(8.L),sample);
      const auto kinematics=hepcat_spinas::validate_momenta<N>(point,masses,incoming);
      if (!kinematics.valid) {
        std::cerr<<"Invalid test phase space: "<<kinematics.message<<"\n";
        return 2;
      }
      generated.set_momenta(point);
      ldouble expected_squared=0;
      for (const auto& spin:states) {
        const cdouble expected=oracle<N>::value(point,masses,spin,kind,photon3,photon4);
        const cdouble actual=generated.amp(spin);
        if (!agree(actual,expected,"amplitude",sample,spin[0],spin[1])) return 1;
        maximum=std::max(maximum,std::abs(expected));
        expected_squared+=std::norm(expected);
        ++comparisons;
      }
      if (!agree(cdouble(generated.amp2(),0),cdouble(expected_squared/average,0),
                 "spin average without phase-space symmetry factor",sample,0,0)) return 3;
    }
    if (!(maximum>ldouble(1.e-15L))) {
      std::cerr<<"The native oracle has no nonzero amplitude in the tested components\n";
      return 4;
    }
    std::cout<<"NUMERICAL_COMPARISONS="<<comparisons<<"\n";
    return 0;
  } catch (const std::exception& error) {
    std::cerr<<"Exception: "<<error.what()<<"\n";
    return 6;
  }
}

} // namespace hepcat_export_reference
