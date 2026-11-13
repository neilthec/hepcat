
#include "process.hpp"
#include "calchep-runtime.hpp"
#include <fstream>
#include <iomanip>
#include <iostream>
int main(){try{
 using Process=hepcat_generated::experimentaleebarZZh;
 Process process;process.set_widths_enabled(false);
 const auto values=process.get_parameters();
 std::vector<hepcat_spinas::CalcHEPParameter> parameters={{"Me",hepcat_spinas::checked_calchep_parameter(values[0],"Me")},{"MZ",hepcat_spinas::checked_calchep_parameter(values[1],"MZ")},{"Mh",hepcat_spinas::checked_calchep_parameter(values[2],"Mh")},{"EE",hepcat_spinas::checked_calchep_parameter(values[3],"EE")},{"SW",hepcat_spinas::checked_calchep_parameter(values[4],"SW")},{"MW",hepcat_spinas::checked_calchep_parameter(values[5],"MW")}};
 hepcat_spinas::CalcHEPProcess reference("/Users/neil/code/hepcat/tests/reference-models/calchep",1,{11,-11,23,23,25},parameters,1.0,true,{},0,{"Me","Mh","MW"});
 hepcat_spinas::check_calchep_masses(process.masses(),reference.masses());
 std::ofstream state("/Users/neil/code/hepcat/tests/results/SM-5-point/diagnostics/covariant-two-vector-scalar-prototype-20261112/precision-same-native-stream/reference-state.txt");state<<reference.work_directory()<<'\n'<<reference.process_name()<<'\n';
 for(std::size_t i=0;i<reference.excluded_particle_names().size();++i){if(i)state<<',';state<<reference.excluded_particle_names()[i];}state<<'\n';state.close();
 auto text=reference.process_name();std::vector<char> description(text.begin(),text.end());description.push_back(0);
 char library[]="hepcat_validation",empty[]="";
 numout* code=getMEcode(0,1,description.data(),NULL,empty,library);
 if(!code)throw std::runtime_error("The cached CalcHEP interface is unavailable.");
 std::ofstream params("/Users/neil/code/hepcat/tests/results/SM-5-point/diagnostics/covariant-two-vector-scalar-prototype-20261112/precision-same-native-stream/parameters.csv");params<<"name,hex_value\n"<<std::hexfloat;
 for(int i=1;i<=code->interface->nvar;++i)params<<code->interface->varName[i]<<','<<static_cast<long double>(code->interface->va[i])<<'\n';
 params.close();
 std::ofstream out("/Users/neil/code/hepcat/tests/results/SM-5-point/diagnostics/covariant-two-vector-scalar-prototype-20261112/precision-same-native-stream/points.csv");out<<std::setprecision(17)<<"point,hepcat,calchep";
 for(std::size_t leg=1;leg<=Process::leg_count;++leg)for(int component=0;component<4;++component)out<<",p"<<leg<<'_'<<component;out<<'\n';
 auto evaluate=[&](std::size_t id,const Process::Momenta& point){
  const auto validation=hepcat_spinas::validate_momenta(point,process.masses(),Process::incoming_legs());
  if(!validation.valid)throw std::runtime_error("Saved point failed kinematic checks: "+validation.message);
  process.set_momenta(point);const double actual=double(process.amp2()*1.0);
  const double expected=reference.squared_amplitude(point);
  if(!std::isfinite(actual)||!std::isfinite(expected)||actual<0||expected<0)throw std::runtime_error("Invalid squared amplitude.");
  out<<id<<','<<actual<<','<<expected;for(const auto& leg:point)for(auto value:leg)out<<','<<double(value);out<<'\n';
 };
 hepcat_spinas::ValidationOptions options;options.points=1000;options.seed=1794524287008317000ULL;
 std::mt19937_64 engine(options.seed);for(std::size_t i=0;i<options.points;++i)evaluate(i+1,hepcat_spinas::make_random_validation_momenta(process.masses(),Process::incoming_legs(),engine,options));
 std::cout<<"Captured native comparison for "<<reference.process_name()<<'\n';return 0;
}catch(const std::exception& error){std::cerr<<error.what()<<'\n';return 1;}}
