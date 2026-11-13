#include "process.hpp"
#include <fstream>
#include <iomanip>
#include <iostream>
int main(int argc,char** argv){
 hepcat_generated::sm5GGGGG process;
  hepcat_spinas::MadGraphConfiguration config;
  config.available = true;
  config.reason = "";
  config.madgraph_directory = "/Users/neil/code/MadGraph";
  config.model = "/Users/neil/code/hepcat/tests/reference-models/madgraph/sm";
  config.python_executable = "/opt/homebrew/opt/python@3.11/bin/python3.11";
  config.helper_path = "/Users/neil/code/hepcat/tests/results/spinas-output/SM-5-point/GGGGG/madgraph-runtime.py";
  config.physical_pdgs = {21, 21, 21, 21, 21};
  config.allowed_particle_pdgs = {};
  config.leg_order = {0, 1, 2, 3, 4};
  config.spin2 = {2, 2, 2, 2, 2};
  config.full_color = true;
  config.color_representations = {8, 8, 8, 8, 8};
  config.parameter_names = {"GG"};
  config.helicities = {{-2, -2, -2, -2, -2}, {-2, -2, -2, -2, 2}, {-2, -2, -2, 2, -2}, {-2, -2, -2, 2, 2}, {-2, -2, 2, -2, -2}, {-2, -2, 2, -2, 2}, {-2, -2, 2, 2, -2}, {-2, -2, 2, 2, 2}, {-2, 2, -2, -2, -2}, {-2, 2, -2, -2, 2}, {-2, 2, -2, 2, -2}, {-2, 2, -2, 2, 2}, {-2, 2, 2, -2, -2}, {-2, 2, 2, -2, 2}, {-2, 2, 2, 2, -2}, {-2, 2, 2, 2, 2}, {2, -2, -2, -2, -2}, {2, -2, -2, -2, 2}, {2, -2, -2, 2, -2}, {2, -2, -2, 2, 2}, {2, -2, 2, -2, -2}, {2, -2, 2, -2, 2}, {2, -2, 2, 2, -2}, {2, -2, 2, 2, 2}, {2, 2, -2, -2, -2}, {2, 2, -2, -2, 2}, {2, 2, -2, 2, -2}, {2, 2, -2, 2, 2}, {2, 2, 2, -2, -2}, {2, 2, 2, -2, 2}, {2, 2, 2, 2, -2}, {2, 2, 2, 2, 2}};
  config.exact_zero_helicities = {{-2, -2, -2, -2, -2}, {-2, -2, -2, -2, 2}, {-2, -2, -2, 2, -2}, {-2, -2, 2, -2, -2}, {-2, 2, -2, -2, -2}, {-2, 2, 2, 2, 2}, {2, -2, -2, -2, -2}, {2, -2, 2, 2, 2}, {2, 2, -2, 2, 2}, {2, 2, 2, -2, 2}, {2, 2, 2, 2, -2}, {2, 2, 2, 2, 2}};
  config.normalization = ldouble(6.51041666666666666666666e-4L);
  config.width_parameter_indices = {};
  config.parameter_rules["aEWM1"] = ldouble(1.27998802505567882537675e2L);
  config.parameter_rules["Gf"] = ldouble(1.19542477614740994733841e-5L);
  config.parameter_rules["MZ"] = ldouble(9.12921430831531714430205e1L);
  config.parameter_rules["lamWS"] = ldouble(0.0L);
  config.mass_parameter_pdgs["MZ"] = {23};
  config.mass_parameter_pdgs["MW"] = {24};
  config.mass_parameter_pdgs["Me"] = {11};
  config.mass_parameter_pdgs["Mm"] = {13};
  config.mass_parameter_pdgs["Ml"] = {15};
  config.mass_parameter_pdgs["Mu"] = {2};
  config.mass_parameter_pdgs["Md"] = {1};
  config.mass_parameter_pdgs["Mc"] = {4};
  config.mass_parameter_pdgs["Ms"] = {3};
  config.mass_parameter_pdgs["Mtp"] = {6};
  config.mass_parameter_pdgs["Mb"] = {5};
  config.mass_parameter_pdgs["Mh"] = {25};

 hepcat_spinas::madgraph_detail::write_configuration(argv[1],process,config,600);
 hepcat_spinas::ValidationOptions options;options.seed=1794538767447593000ULL;options.points=1000;
 options.minimum_center_of_mass_energy=ldouble(1.5);options.maximum_center_of_mass_energy=ldouble(6);
 std::mt19937_64 engine(options.seed);std::ofstream binary(argv[2],std::ios::binary);
 std::cout<<std::setprecision(25)<<"point,h1,h2,h3,h4,h5,normalized,raw\n";
 for(size_t point_index=0;point_index<1000;++point_index){
  const auto point=hepcat_spinas::make_random_validation_momenta(process.masses(),process.incoming_legs(),engine,options);
  for(const auto& momentum:point)for(ldouble component:momentum){const double value=static_cast<double>(component);binary.write(reinterpret_cast<const char*>(&value),sizeof(value));}
  process.set_momenta(point);
  for(const auto& hel:process.helicity_configurations()){
   const auto value=process.color_spin_sum(hel);std::cout<<point_index+1<<",";
   for(int h:hel)std::cout<<h<<",";std::cout<<config.normalization*value<<","<<value<<"\n";
  }
 }
}
