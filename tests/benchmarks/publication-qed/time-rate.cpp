// Time one backend per invocation. Input vectors and model setup are outside the timer.
#include "validation_numeric.hpp"
#include <dlfcn.h>
#include <algorithm>
#include <chrono>
#include <ctime>
#include <cmath>
#include <fstream>
#include <filesystem>
#include <functional>
#include <iomanip>
#include <iostream>
#include <numeric>

template<class T> T symbol(void* handle, const char* name) {
    auto ptr=dlsym(handle,name);
    if (!ptr) throw std::runtime_error(std::string("Missing symbol ")+name);
    return reinterpret_cast<T>(ptr);
}

int main(int argc,char** argv) try {
    if (argc!=9) throw std::invalid_argument("time-rate BACKEND LIB PAIRS PHOTONS POINTS SWEEPS SEED OUTPUT");
    const std::string backend=argv[1];
    const int pairs=std::stoi(argv[3]), photons=std::stoi(argv[4]), legs=2*pairs+photons;
    const int count=std::stoi(argv[5]), sweeps=std::stoi(argv[6]);
    const auto seed=std::stoull(argv[7]);
    if (count < 1 || sweeps < 1 || pairs < 1 || photons < 0 || legs < 4 || legs > 9)
        throw std::invalid_argument("Invalid sample size or multiplicity");
    std::vector<long double> masses(2*pairs,1); masses.resize(legs,0);
    std::vector<std::vector<double>> momenta(count);
    for (int i=0;i<count;++i) {
        const auto point=hepcat::validation::sample_point(masses,seed+i);
        for (int j=0;j<legs;++j) {
            for (auto x:point.momenta[j]) momenta[i].push_back(j<2 ? x : -x);
        }
    }
    auto library=dlopen(argv[2],RTLD_LOCAL|RTLD_NOW);
    if (!library) throw std::runtime_error(dlerror());
    std::function<double(double*)> evaluate;
    if (backend=="SPINAS") {
        symbol<void(*)(int,int)>(library,"rate_init")(0,1);
        evaluate=symbol<double(*)(double*)>(library,"rate");
    } else if (backend=="MadGraph") {
        // The original bridge can retain an absolute card path from a different checkout.
        const auto card=(std::filesystem::absolute(argv[2]).parent_path()/"process/Cards/param_card.dat").string();
        if (!std::filesystem::is_regular_file(card)) throw std::runtime_error("Missing MadGraph parameter card");
        int quiet=0;
        symbol<void(*)(int*)>(library,"setparamlog_")(&quiet);
        symbol<void(*)(const char*,std::size_t)>(library,"setpara_")(card.c_str(),card.size());
        const double norm=1./(4*std::tgamma(1+photons)*std::pow(std::tgamma(pairs),2));
        auto sum=reinterpret_cast<void(*)(double*,double*)>(dlsym(library,"hepcat_mg_evaluate_sum"));
        if (sum) evaluate=[=](double* p){ double ans=0;sum(p,&ans);return norm*ans; };
        else {
            auto fixed=symbol<void(*)(double*,int*,double*)>(library,"hepcat_mg_evaluate");
            std::vector<std::vector<int>> states(1<<legs,std::vector<int>(legs));
            for (int s=0;s<(1<<legs);++s) for(int j=0;j<legs;++j) states[s][j]=(s>>j&1)?1:-1;
            evaluate=[=](double* p) mutable {double sum=0,x;for(auto& s:states){fixed(p,s.data(),&x);sum+=x;}return norm*sum;};
        }
    } else if (backend=="CalcHEP") {
        const std::string suffix=dlsym(library,"va_ext") ? "_ext" : "_hepcat_validation_u";
        auto ch=[&](const std::string& key){ return (key+suffix); };
        // In CalcHEP 3.9.2 the optional micrOMEGAs helper indexes omgWidth[40]
        // by the one-based denominator number, including for zero-width models.
        if (dlsym(library, "omgWidth")) {
            auto denominator = symbol<char*(*)(int,int,int*,int*,int*)>(library,ch("den_info").c_str());
            int mass = 0, width = 0;
            if (denominator(1, 40, &mass, &width, nullptr))
                throw std::runtime_error("Link the standalone CalcHEP sqme_aux helper: this subprocess exceeds the optional 40-element width array.");
        }
        auto pinf=symbol<char*(*)(int,int,double*,int*)>(library,ch("pinf").c_str());
        std::vector<int> types{11,-11}, order;
        for (int i=1;i<pairs;++i) {
            types.push_back(pairs==2 ? 11 : -11);
            types.push_back(pairs==2 ? -11 : 11);
        }
        types.resize(legs,22);
        for(int i=0;i<legs;++i) {
            int pdg=0;
            if(!pinf(1,i+1,nullptr,&pdg))throw std::runtime_error("Missing CalcHEP leg");
            auto found=std::find(types.begin()+(i<2?0:2),i<2?types.begin()+2:types.end(),pdg);
            if(found==(i<2?types.begin()+2:types.end()))throw std::runtime_error("CalcHEP leg order mismatch");
            order.push_back(found-types.begin()); *found=0;
        }
        for(auto& p:momenta) {
            const auto original=p;
            for(int i=0;i<legs;++i)std::copy_n(original.begin()+4*order[i],4,p.begin()+4*i);
        }
        auto values=symbol<double*>(library,ch("va").c_str());
        auto names=symbol<char**>(library,ch("varName").c_str());
        for(int j=1;j<=*symbol<int*>(library,ch("nvar").c_str());++j) {
            const std::string key=names[j];
            if(key=="Me")values[j]=1;
            else if(key=="EE")values[j]=1./3;
            else throw std::runtime_error("Unexpected CalcHEP parameter "+key);
        }
        if(symbol<int(*)()>(library,ch("calcFunc").c_str())()!=0)throw std::runtime_error("CalcHEP init");
        auto sqme=symbol<double(*)(int,double,double*,double*,int*)>(library,ch("sqme").c_str());
        evaluate=[=](double* p){int err=0;double x=sqme(1,0,p,nullptr,&err);if(err)throw std::runtime_error("CalcHEP evaluation error "+std::to_string(err));return x;};
    } else throw std::invalid_argument("Unknown backend");
    std::ofstream out(argv[8]);
    if (!out) throw std::runtime_error("Cannot open timing output");
    out<<std::setprecision(17);
    for(auto& point:momenta) {
        const double x=evaluate(point.data());
        if(!(std::isfinite(x)&&x>0))throw std::runtime_error("Invalid spin-summed rate");
        out<<"value "<<x<<'\n';
    }
    double checksum=0;
    unsigned loops=1;
    auto measure=[&](){
        const auto start=std::chrono::steady_clock::now();
        for(unsigned j=0;j<loops;++j)for(auto& point:momenta) checksum+=evaluate(point.data());
        return std::chrono::duration<double>(std::chrono::steady_clock::now()-start).count();
    };
    while(measure()<0.03)loops*=2;
    for(int sweep=0;sweep<sweeps;++sweep) {
        const auto before = std::clock();
        const double elapsed = measure();
        const double cpu = double(std::clock() - before) / CLOCKS_PER_SEC;
        out<<"seconds_per_point "<<elapsed/(loops*count)<<'\n';
        out<<"cpu_seconds_per_point "<<cpu/(loops*count)<<'\n';
    }
    out<<"passes_per_sweep "<<loops<<"\nchecksum "<<checksum<<'\n';
    return 0;
} catch(const std::exception& e){std::cerr<<e.what()<<'\n';return 1;}
