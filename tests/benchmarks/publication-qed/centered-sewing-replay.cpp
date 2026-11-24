// Build as described in centered-sewing.md. Run from the repository root.
// The HEPCAT point and both libraries use m=1, e=1/3, and zero widths.
#include "charged_tree_sewing.hpp"
#include "model.hpp"
#include "madgraph-runtime.hpp"
#include "nine-point-tensors.hpp"
#include "triple-sewing-seed.hpp"
#include <chrono>
#include <iomanip>
#include <iostream>
#include <sys/resource.h>

using namespace hepcat;
using namespace hepcat::validation;
using clock_type = std::chrono::steady_clock;
using plan = std::shared_ptr<const charged_tree_sewing>;

static real residual(complex a, complex b) {
    real scale = std::max(std::abs(a),std::abs(b));
    if (!std::isfinite(scale) || !std::isfinite(std::abs(a-b))) throw std::runtime_error("nonfinite amplitude");
    return scale ? std::abs(a-b)/scale : 0;
}

int main(int argc,char** argv) try {
    const bool isolated=argc==7 && std::string(argv[5])=="--rate-only";
    if(argc!=5 && argc!=6 && !isolated)
        throw std::invalid_argument("Usage: centered-sewing-replay PAIRS PHOTONS POINTS SEED [MG_LIBRARY|rooted|seed|--rate-only physical|spurious|MG_LIBRARY]");
    const int pairs=std::stoi(argv[1]), photons=std::stoi(argv[2]), points=std::stoi(argv[3]);
    const unsigned seed=std::stoul(argv[4]);
    if(pairs<3 || pairs>4 || photons<0 || 2*pairs+photons>9 || points<=0)
        throw std::invalid_argument("Unsupported replay dimensions");
    const std::string evaluator=isolated ? argv[6] : "";
    const bool isolated_mg=isolated && evaluator!="physical" && evaluator!="spurious";
    std::optional<model> vertices;
    if(!isolated_mg) vertices.emplace("models",3);
    std::vector<std::vector<int>> types;
    std::vector<std::array<plan,2>> plans;
    std::vector<real> masses(2*pairs+photons);
    std::fill_n(masses.begin(),2*pairs,1);
    for(int h=0;h<(1<<photons);++h) {
        std::vector<int> legs;
        std::vector<std::string> names;
        for(int j=0;j<2*pairs;++j) {
            legs.push_back(j%2 ? -1 : 1);
            names.emplace_back(j%2 ? "E" : "e");
        }
        for(int j=0;j<photons;++j) {
            legs.push_back(h&(1<<j) ? -2 : 2);
            names.emplace_back(h&(1<<j) ? "A.-" : "A.+");
        }
        std::array<plan,2> gauges;
        for(int j=0;j<2;++j) {
            if(isolated && evaluator!=(j ? "spurious" : "physical")) continue;
            gauges[j]=charged_tree_sewing::from_model(*vertices,names,j==0);
            if(!gauges[j]) throw std::logic_error("Model was not recognized");
        }
        plans.push_back(std::move(gauges));
        types.push_back(std::move(legs));
    }
    std::optional<hepcat_spinas::madgraph_detail::Reference> mg;
    constexpr real e=1.L/3;
    if(isolated) {
        if(isolated_mg) mg.emplace(evaluator);
        real checksum=0;
        for(int n=0;n<points;++n) {
            const auto p=sample_point(masses,seed+n);
            if(mg) {
                std::vector<double> momenta;
                for(unsigned i=0;i<p.momenta.size();++i)
                    for(auto x:p.momenta[i]) momenta.push_back(i<2 ? x : -x);
                checksum+=mg->evaluate_sum(momenta);
            } else {
                for(const auto& gauges:plans)
                    checksum+=gauges[evaluator=="spurious"]->spin_sum(p,e);
            }
        }
        rusage usage{};
        if(getrusage(RUSAGE_SELF,&usage)) throw std::runtime_error("Cannot read peak resident memory");
        double mib=double(usage.ru_maxrss)/1024;
#ifdef __APPLE__
        mib/=1024;
#endif
        std::cout<<std::setprecision(15)<<"rate checksum "<<checksum<<", peak resident MiB "<<mib<<"\n";
        return 0;
    }
    std::cout<<"central sewings "<<plans[0][0]->sewings().size()<<", diagrams "<<plans[0][0]->diagrams()
             <<", model charge "<<plans[0][0]->coupling()<<"\n";
    const bool rooted = argc==6 && std::string(argv[5])=="rooted";
    const bool one_seed = argc==6 && std::string(argv[5])=="seed";
    if(one_seed && pairs!=3) throw std::invalid_argument("one-image check requires three pairs");
    if(argc==6 && !rooted && !one_seed) mg.emplace(argv[5]);
    std::vector<dirac_current::recursion> references;
    if(rooted) for(const auto& row:types) references.emplace_back(row);
    double setup_seconds=0;
    std::array<real,2> peak{};
    std::array<double,3> seconds{};
    real worst_polarized=0;
    for(int n=0;n<points;++n) {
        auto setup=clock_type::now();
        auto p=sample_point(masses,seed+n);
        setup_seconds+=std::chrono::duration<double>(clock_type::now()-setup).count();
        std::array<real,2> sum{};
        std::vector<double> mom;
        for(unsigned i=0;i<p.momenta.size();++i) for(auto x:p.momenta[i]) mom.push_back(i<2 ? x : -x);
        for(unsigned h=0;h<plans.size();++h) {
            complex expected{};
            // The published tensor sums line orderings independently of the central cuts.
            if(rooted) expected=references[h](p,e,dirac_current::gauge::independent);
            else if(!mg && !one_seed) expected=nine_point::tensors(p,types[h],e,dirac_current::gauge::independent,false)();
            std::array<real,2> rates{};
            for(int j=0;j<2;++j) {
                auto start=clock_type::now();
                if(mg) {
                    rates[j]=plans[h][j]->spin_sum(p,e);
                    sum[j]+=rates[j];
                } else {
                    if(one_seed) expected=triple_seed::evaluate(p,types[h],e,
                            j ? dirac_current::gauge::spurious : dirac_current::gauge::physical);
                    peak[j]=std::max(peak[j],residual(expected,plans[h][j]->amplitude(p,e)));
                }
                seconds[j]+=std::chrono::duration<double>(clock_type::now()-start).count();
            }
            if(mg && n<std::min(12,points)) {
                real fixed=0;
                std::vector<int> helicities=types[h];
                for(auto& value:helicities) value=value>0 ? 1 : -1;
                for(int spin=0;spin<(1<<(2*pairs));++spin) {
                    for(int i=0;i<2*pairs;++i) helicities[i]=spin&(1<<i) ? 1 : -1;
                    fixed+=mg->evaluate(mom,helicities);
                }
                for(auto r:rates) worst_polarized=std::max(worst_polarized,residual(r,fixed));
            }
        }
        if(mg) {
            auto start=clock_type::now();
            auto y=mg->evaluate_sum(mom);
            seconds[2]+=std::chrono::duration<double>(clock_type::now()-start).count();
            for(int j=0;j<2;++j) peak[j]=std::max(peak[j],residual(y,sum[j]));
        }
        if(std::max({peak[0],peak[1],worst_polarized})>1e-7L)
            throw std::runtime_error("comparison failed at seed "+std::to_string(seed+n)+
                ", physical "+std::to_string(double(peak[0]))+", spurious "+std::to_string(double(peak[1]))+
                ", polarized "+std::to_string(double(worst_polarized)));
    }
    std::cout<<std::setprecision(7)<<"points "<<points<<", seed "<<seed<<", reference "
             <<(mg?"MadGraph squared":rooted?"signed external-leg recursion":one_seed?"signed 36-image paper formula":"signed ordered line tensors")<<", maxPhysical "<<peak[0]
             <<", maxSpurious "<<peak[1]<<"\n";
    if(mg) std::cout<<"polarized max ("<<std::min(12,points)<<" points) "<<worst_polarized<<", ms per complete rate: physical "
                    <<1000*seconds[0]/points<<", spurious "<<1000*seconds[1]/points
                    <<", MadGraph "<<1000*seconds[2]/points
                    <<", additional phase-space and external spinor preparation "<<1000*setup_seconds/points<<"\n";
    return 0;
} catch(const std::exception& e) { std::cerr<<e.what()<<'\n'; return 1; }
