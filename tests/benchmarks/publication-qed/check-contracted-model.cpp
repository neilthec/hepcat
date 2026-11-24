// Read a stored model row through the user-facing API and compare its signed
// value to the fully contracted external-spinor polynomial on the same points.
// Usage: check-contracted-model MODEL_DIRECTORY MODEL_NUMBER TABLE [FIRST_SEED]
#include "contracted-evaluation.hpp"
#include "model.hpp"

int main(int argc, char **argv) try {
    using namespace contracted_qed;
    if (argc != 4 && argc != 5)
        throw std::invalid_argument("Use MODELS NUMBER TABLE [FIRST_SEED]");
    const int number = std::stoi(argv[2]);
    const table input(argv[3]);
    const program expected(input);
    const hepcat::model model(argv[1],number);
    const auto parameters = model_parameters(model, {{"Me",1},{"EE",1.L/3}});
    const auto first = argc==5 ? std::stoull(argv[4]) : 2026112401ULL;
    auto legs=input.legs;
    for (unsigned negatives=0; negatives<=input.nphot; ++negatives) {
        std::vector<int> roles;
        std::vector<real> masses;
        for (unsigned j=0;j<legs.size();++j) {
            if (j>=legs.size()-input.nphot)
                legs[j]=j+negatives>=legs.size() ? "A.-" : "A.+";
            roles.push_back(legs[j]=="e"?1:legs[j]=="E"?-1:legs[j]=="A.+"?2:-2);
            masses.push_back(std::abs(roles.back())==1 ? 1 : 0);
        }
        hepcat::Algebra arena;
        const auto row=model.retrieve_amplitude(legs);
        auto root=hepcat::validation_detail::import_expression(arena,row,false);
        expression_evaluator evaluate(arena,root,parameters);
        real worst=0;
        for (unsigned i=0;i<3;++i) {
            auto point=sample_point(masses,first+i);
            auto value=evaluate(point), target=expected(point,roles,number==4,1.L/3,1);
            auto scale=std::max(std::abs(value),std::abs(target));
            if (!std::isfinite(scale)||scale==0) throw std::runtime_error("Invalid sample");
            worst=std::max(worst,std::abs(value-target)/scale);
        }
        std::cout<<number<<" "<<legs.size()<<" "<<input.nphot<<" "<<negatives<<" "<<arena.size()<<" "<<worst<<std::endl;
        if (!(worst<1e-7L)) throw std::runtime_error("Stored amplitude disagrees with its external polynomial");
    }
    return 0;
} catch(const std::exception &error) { std::cerr<<error.what()<<"\n"; return 1; }
