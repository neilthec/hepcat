// Export one complete photon-helicity family for a process-size or timing study.
// See rate-study.md for inputs, normalization and validation.
#include "generation_sew.hpp"
#include "model.hpp"
#include "reduce.hpp"
#include "spinas_export.hpp"
#include <fstream>
#include <iostream>
#include <numeric>

using namespace hepcat;
namespace fs = std::filesystem;

int main(int argc, char** argv) try {
    if (argc != 5 && argc != 6)
        throw std::invalid_argument("Usage: export-rate-family PAIRS PHOTONS MODEL OUTPUT [EXPRESSION_DIR]");
    const int pairs = std::stoi(argv[1]), photons = std::stoi(argv[2]);
    const unsigned model_number = std::stoul(argv[3]);
    if (pairs < 1 || photons < 0 || pairs * 2 + photons > 9 || (model_number != 4 && model_number != 5))
        throw std::invalid_argument("Expected QED through nine external legs and model 4 or 5");
    model vertices("models", 3);
    std::vector<spinas_component> family;
    if (argc == 5) {
        model catalog("models", model_number);
        for (std::size_t i = 0; i < catalog.vertices().size(); ++i) {
            const auto& v = catalog.vertices()[i];
            if (int(v.legs.size()) != 2 * pairs + photons ||
                std::count(v.legs.begin(), v.legs.end(), "e") != pairs) continue;
            family.push_back({v.legs, catalog.retrieve_amplitude(v.legs)});
        }
    } else {
        std::vector<std::string> fermions;
        for (int i=0; i<pairs; ++i) fermions.insert(fermions.end(), {"e","E"});
        if (pairs==2) std::swap(fermions[2],fermions[3]);
        std::vector<std::string> masses(2*pairs,"Me");
        masses.resize(2*pairs+photons,"0");
        for (int h=0; h<=photons/2; ++h) {
            std::string word(photons-h,'+'); word.append(h,'-');
            std::ifstream input(fs::path(argv[5])/(word+".expr"));
            if (!input) throw std::runtime_error("Missing representative "+word);
            std::string expression((std::istreambuf_iterator<char>(input)), {});
            auto legs = fermions;
            for (char helicity : word) legs.push_back(helicity=='+' ? "A.+" : "A.-");
            family.push_back({legs,amp_expression(expression,masses)});
            if (2*h == photons) continue;
            // Parity interchanges the two Weyl spinors for these real QED
            // expressions. A common phase per branch drops out of its rate.
            std::string opposite;
            opposite.reserve(expression.size());
            std::size_t begin=0;
            for (std::size_t pos=0; (pos=expression.find('"',pos))!=std::string::npos;) {
                const auto end=expression.find('"',pos+1);
                if (end==std::string::npos) throw std::invalid_argument("Unclosed string");
                const auto token=std::string_view(expression).substr(pos,end-pos+1);
                if (token=="\"Angle\"" || token=="\"Square\"") {
                    opposite.append(expression,begin,pos-begin);
                    opposite+=token=="\"Angle\"" ? "\"Square\"" : "\"Angle\"";
                    begin=end+1;
                }
                pos=end+1;
            }
            opposite.append(expression,begin,std::string::npos);
            for (int i=0; i<photons; ++i) legs[2*pairs+i]=word[i]=='+' ? "A.-" : "A.+";
            family.push_back({legs,amp_expression(opposite,masses)});
        }
    }
    if (family.size()!=std::size_t(photons+1)) throw std::runtime_error("Incomplete helicity inventory");
    spinas_export_options options;
    options.name="qed_rate";
    options.overwrite=true;
    const auto project=write_spinas_process(argv[4],vertices,family,options);
    if (!project.complete_helicities || project.helicities.size()!=(1u<<photons))
        throw std::runtime_error("Incomplete generated process");
    std::cout << project.helicities.size() << " labeled photon helicities\n";
    return 0;
} catch (const std::exception& e) { std::cerr << e.what() << '\n'; return 1; }
