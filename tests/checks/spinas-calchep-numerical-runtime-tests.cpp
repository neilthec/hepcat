// Synthetic process: no symbolic model or previously exported amplitude.
#include "spinas-runtime.hpp"
#include "calchep-spinas-runtime.hpp"
#include <thread>
#include <atomic>
#include <iostream>
class ProbeProcess {
public:
  static constexpr std::size_t leg_count=4, parameter_count=2;
  using Parameters=std::array<cdouble,2>;
  using Momenta=std::array<std::array<ldouble,4>,4>;
  ProbeProcess():ProbeProcess(Parameters{{cdouble(1,0),cdouble(4,0)}}){}
  explicit ProbeProcess(const Parameters&v){set_parameters(v);}
  static std::array<std::size_t,2> incoming_legs(){return {{2,0}};}
  void set_parameters(const Parameters&v){if(v[0].real()<0)throw std::invalid_argument("mass");p=v;m={{v[0].real(),0,v[0].real(),0}};}
  const std::array<ldouble,4>&masses()const{return m;}
  void set_momenta(const Momenta&v){k=v;}
  ldouble amp2(){return k[0][0]+2*k[1][0]+3*k[2][0]+4*k[3][0]+p[1].real();}
private:Parameters p;Momenta k;std::array<ldouble,4>m;
};
class ConstantProcess {
public:
  static constexpr std::size_t leg_count=4, parameter_count=0;
  using Parameters=std::array<cdouble,0>;
  using Momenta=std::array<std::array<ldouble,4>,4>;
  ConstantProcess()=default;
  explicit ConstantProcess(const Parameters&) {}
  static std::array<std::size_t,2> incoming_legs(){return {{0,1}};}
  void set_parameters(const Parameters&) {}
  const std::array<ldouble,4>& masses()const{return mass;}
  void set_momenta(const Momenta&) {}
  ldouble amp2()const{return 3;}
private:
  std::array<ldouble,4> mass={{0,0,0,0}};
};
int dependencies(REAL*v){v[2]=v[1]*v[1];return 0;}
int main(){
  hepcat_calchep::Configuration c;
  c.calchep_directory="/unused/numerical-runtime-unit-test";
  c.independent_count=2;
  c.parameters={{"Me",1},{"GG",2},{"GG2",4}};
  c.particles={{"e","E",11,1,1,-3,"Me","0",false},{"A","A",22,2,1,0,"0","0",true},{"q","Q",2,1,3,2,"Me","0",false}};
  c.legs={{"e",11,1,1,false,2},{"E",-11,1,1,false,2},{"A",22,2,1,true,2},{"A",22,2,1,true,2}};
  c.external_order={2,0,1,3};c.process_parameter_indices={0,2};c.evaluate=dependencies;c.running_strong_parameter=1;
  c.denominators={{"\1\3",0,-1,0}};
  int checks=0;
  auto check=[&](bool good){++checks;if(!good)throw std::runtime_error("unit check "+std::to_string(checks));};
  {
    hepcat_calchep::NumericalProcess<ProbeProcess>a(c);
    auto i=a.process_interface();
    REAL mass;int pdg; i.pinf(1,1,&mass,&pdg);check(mass==1&&pdg==11);
    REAL p[16]={20,0,0,0,10,0,0,0,5,0,0,0,25,0,0,0};int err=0;
    check(i.sqme(1,3,p,nullptr,&err)==189&&err==0);
    check(a.model_values()[1]==2&&a.model_values()[2]==4);
    a.model_values()[0]=2;check(a.calculate_model()==0);check(i.calcFunc()==0);
    i.pinf(1,2,&mass,&pdg);check(mass==2&&pdg==-11);
    std::atomic<bool>good(true);std::vector<std::thread>threads;
    for(int t=0;t<4;t++)threads.emplace_back([&](){for(int n=0;n<50;n++){int e;double v=i.sqme(1,3,p,nullptr,&e);if(e||v!=189)good=false;}});
    for(auto&t:threads)t.join();check(good.load());
    *i.twidth=1;check(std::isnan(i.sqme(1,3,p,nullptr,&err))&&err!=0);*i.twidth=0;
    check(std::isnan(i.sqme(2,3,p,nullptr,&err))&&err!=0);
    check(std::isnan(i.sqme(1,-1,p,nullptr,&err))&&err!=0);
    a.model_values()[1]=std::numeric_limits<REAL>::quiet_NaN();int failure=a.calculate_model();check(failure>0&&a.model_names()[failure]!=nullptr);
    a.model_values()[1]=2;check(a.calculate_model()==0);
    int s,q,col;check(a.quantum_numbers("Q",&s,&q,&col)==-2&&s==1&&q==-2&&col==-3);
    int mi,wi,pi;char*den=i.den_info(1,1,&mi,&wi,&pi);check(den&&den[0]==1&&den[1]==3&&mi==1&&wi==0&&pi==0);
    check(i.pinf(1,0,&mass,&pdg)==nullptr&&std::isnan(mass));
  }
  c.amplitude_factor=static_cast<REAL>(0.5L);
  {hepcat_calchep::NumericalProcess<ProbeProcess>a(c);auto i=a.process_interface();REAL p[16]={20,0,0,0,10,0,0,0,5,0,0,0,25,0,0,0};int e;check(i.sqme(1,3,p,nullptr,&e)==94.5&&e==0);}
  hepcat_calchep::Configuration zero;
  zero.calchep_directory="/unused/numerical-runtime-unit-test";
  zero.independent_count=0;
  zero.particles={{"h","h",25,0,1,0,"0","0",true}};
  zero.legs={{"h",25,0,1,true,1},{"h",25,0,1,true,1},
    {"h",25,0,1,true,1},{"h",25,0,1,true,1}};
  zero.external_order={0,1,2,3};
  {
    hepcat_calchep::NumericalProcess<ConstantProcess> adapter(zero);
    auto interface=adapter.process_interface();
    check(adapter.model_count()==0&&interface.nvar==0);
    check(adapter.calculate_model()==0&&interface.calcFunc()==0);
    REAL momenta[16]={};int error=1;
    check(interface.sqme(1,0,momenta,nullptr,&error)==3&&error==0);
  }
  std::cout<<"CALCHEP_NUMERICAL_RUNTIME_CHECKS="<<checks<<" FAILURES=0\n";
}
