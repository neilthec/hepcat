// Ward identity for each labeled photon with the other polarizations fixed.
// The longitudinal amplitude has one extra power of energy. Compare it
// with sqrt(s) times the corresponding transverse matrix element.
#include "contracted-evaluation.hpp"

int main(int argc,char **argv) try {
    using namespace contracted_qed;
    if(argc!=4) throw std::invalid_argument("Use SPINOR_TABLE POINTS FIRST_SEED");
    const table input(argv[1]);
    const program amplitude(input);
    const size_t n=input.legs.size(), charged=n-input.nphot;
    const auto count=std::stoul(argv[2]); const auto seed=std::stoull(argv[3]);
    if(count==0) throw std::invalid_argument("Positive sample count required");
    std::vector<real> masses(n,0);
    std::fill_n(masses.begin(),charged,1);
    std::vector<int> roles(n,2);
    for(size_t j=0;j<charged;++j) roles[j]=input.legs[j]=="e"?1:-1;
    real largest=0; unsigned comparisons=0;
    for(unsigned sample=0; sample<count; ++sample) {
        auto point=sample_point(masses,seed+sample);
        const auto energy=point.momenta[0][0]+point.momenta[1][0];
        for(unsigned negatives=0; negatives<=input.nphot; ++negatives) {
            for(size_t j=charged;j<n;++j) roles[j]=j+negatives>=n?-2:2;
            for(bool physical:{true,false}) {
                const real scale=energy*std::abs(amplitude(point,roles,physical,1.L/3,1));
                if(!(scale>0))throw std::runtime_error("Singular transverse normalization");
                for(size_t j=charged;j<n;++j) {
                    real ratio=std::abs(amplitude(point,roles,physical,1.L/3,1,j))/scale;
                    if(!std::isfinite(ratio)||ratio>1e-7L)
                        throw std::runtime_error("Failed Ward identity at leg "+std::to_string(j+1)+
                            " and seed "+std::to_string(seed+sample));
                    largest=std::max(largest,ratio);
                    ++comparisons;
                }
            }
        }
    }
    std::cout<<comparisons<<" "<<std::setprecision(12)<<largest<<"\n";
    return 0;
} catch(const std::exception& e) {std::cerr<<e.what()<<"\n";return 1;}
