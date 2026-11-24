// Compile with C++17, -O3, -I csource, and the native HEPCAT library.
// Run with TABLE NUMBER_OF_POINTS FIRST_SEED. Times measure fixed-spin amplitudes.
// The Feynman comparator is constructed only after the external polynomials.
#include "contracted-evaluation.hpp"
#include "validation_dirac_current.hpp"
using namespace contracted_qed;

int main(int argc,char **argv) try {
    if(argc!=4)throw std::invalid_argument("Use TABLE POINTS FIRST_SEED");
    const table t(argv[1]);const program amp(t);
    const int count=std::stoi(argv[2]);if(count<=0)throw std::invalid_argument("Positive point count required");
    const auto first=std::stoull(argv[3]);const real e=1.L/3,m=1;
    std::vector<real> masses;std::vector<int> roles;
    for(auto &leg:t.legs){roles.push_back(leg=="e"?1:leg=="E"?-1:2);masses.push_back(leg=="A.+"?0:m);}
    int failures=0;
    std::cout<<"photons negatives points max_physical max_spurious worst_physical_seed worst_spurious_seed physical_s spurious_s feynman_s failures\n";
    for(unsigned minuses=0;minuses<=t.nphot;++minuses) {
        for(unsigned i=0;i<t.nphot;++i)roles[roles.size()-t.nphot+i]=i+minuses>=t.nphot?-2:2;
        dirac_current::recursion reference(roles);
        real worst[2]{},seconds[3]{};size_t seeds[2]{};unsigned bad=0;
        for(int j=0;j<count;++j) {
            auto p=sample_point(masses,first+j);
            auto now=clock_type::now();const auto ref=reference(p,e,dirac_current::gauge::independent);
            seconds[2]+=std::chrono::duration<double>(clock_type::now()-now).count();
            for(int g=0;g<2;++g) {
                now=clock_type::now();const auto v=amp(p,roles,g==0,e,m);
                seconds[g]+=std::chrono::duration<double>(clock_type::now()-now).count();
                const real scale=std::max(std::abs(v),std::abs(ref));
                const real err=scale?std::abs(v-ref)/scale:0;
                if(!std::isfinite(err)||!std::isfinite(scale)||scale==0||err>1e-7L) {
                    ++bad;
                    std::cerr << "flagged " << minuses << " " << g << " " << first+j
                              << " " << std::setprecision(16) << err << "\n";
                }
                if(err>worst[g]){worst[g]=err;seeds[g]=first+j;}
            }
        }
        std::cout<<std::setprecision(7)<<t.nphot<<" "<<minuses<<" "<<count<<" "<<worst[0]<<" "<<worst[1]<<" "<<seeds[0]<<" "<<seeds[1]<<" "<<seconds[0]<<" "<<seconds[1]<<" "<<seconds[2]<<" "<<bad<<std::endl;
        failures+=bad;
    }
    return failures?2:0;
} catch(const std::exception &e){std::cerr<<e.what()<<"\n";return 1;}
