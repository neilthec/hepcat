#include "generation_plan.hpp"
#include "generation_feynman.hpp"
#include "model.hpp"

#include <algorithm>
#include <filesystem>
#include <iostream>
#include <stdexcept>

using hepcat::model;
using hepcat::generation::channel_plans;
using hepcat::generation::diagram_plans;
using process = std::vector<std::string>;

namespace {
void require(bool ok, const char* message) {
    if (!ok) throw std::runtime_error(message);
}

void counts(const process& legs, const model& vertices, const model& amplitudes, std::size_t diagrams,
            std::size_t assignments, std::size_t channels) {
    const auto ds = diagram_plans(vertices, legs), cs = channel_plans(amplitudes, legs);
    std::size_t total = 0;
    for (const auto& d : ds) {
        total += d.terms.size();
        for (const auto& t : d.terms) {
            require(t.vertices.size() == t.edges.size() + 1, "diagram is not a tree");
            std::vector<unsigned> external(legs.size()), endpoints(t.edges.size());
            for (const auto& v : t.vertices) {
                const auto& particles = vertices.vertices().at(v.row).legs;
                require(v.slots.size() == particles.size(), "incorrect vertex arity");
                for (std::size_t j = 0; j < v.slots.size(); ++j) {
                    int label = v.slots[j];
                    if (label > 0) {
                        require(legs.at(label - 1) == particles[j], "incorrect model slot assignment");
                        ++external[label - 1];
                    } else
                        ++endpoints.at(-1 - label);
                }
            }
            require(std::all_of(external.begin(), external.end(), [](unsigned n) { return n == 1; }),
                    "external leg missing or repeated");
            require(std::all_of(endpoints.begin(), endpoints.end(), [](unsigned n) { return n == 2; }),
                    "internal line missing or repeated");
        }
    }
    require(ds.size() == diagrams, "incorrect diagram count");
    require(total == assignments, "incorrect number of compatible internal helicities");
    require(cs.size() == channels, "incorrect factorization inventory");
}

void raw_feynman_rules(const std::filesystem::path& directory) {
    model sm(directory, 1), qed(directory, 2);
    process hhhh{"h", "h", "h", "h"};
    auto higgs = diagram_plans(sm, hhhh);
    require(higgs.size() == 4, "expected three Higgs exchanges and one contact");
    unsigned contacts = 0;
    for (const auto& diagram : higgs) {
        hepcat::Algebra a(&sm.algebra());
        auto raw = hepcat::generation::sew_feynman(a, sm, hhhh, diagram);
        auto& tree = diagram.terms.front();
        if (tree.edges.empty()) {
            auto& vertex = sm.vertices()[tree.vertices.at(0).row];
            require(raw == a.mul({vertex.factor, vertex.expression}), "Higgs contact normalization");
            ++contacts;
        } else {
            require(a.format(raw).find("PropDen[") != std::string::npos, "missing Higgs exchange propagator");
        }
    }
    require(contacts == 1, "missing Higgs contact");

    process gluons{"G.+", "G.+", "G.-", "G.-"};
    auto four_gluon = diagram_plans(sm, gluons);
    require(four_gluon.size() == 6, "expected three gluon exchanges and three tensor contacts");
    contacts = 0;
    for (const auto& diagram : four_gluon) {
        auto& edge = diagram.terms.front().edges.at(0);
        hepcat::Algebra a(&sm.algebra());
        auto expression = a.format(hepcat::generation::sew_feynman(a, sm, gluons, diagram));
        bool auxiliary = sm.particle(edge.particle_a).auxiliary == "T";
        require((expression.find("PropDen[") == std::string::npos) == auxiliary,
                "incorrect contact auxiliary propagator");
        require(expression.find("ColorStructure[") != std::string::npos, "missing adjoint tensor");
        contacts += auxiliary;
    }
    require(contacts == 3, "incorrect auxiliary contact count");
    process compton{"e", "E", "A.+", "A.-"};
    auto diagrams = diagram_plans(qed, compton);
    require(diagrams.size() == 2, "incorrect Compton poles");
    for (const auto& diagram : diagrams) {
        hepcat::Algebra a(&qed.algebra());
        auto expression = a.format(hepcat::generation::sew_feynman(a, qed, compton, diagram));
        for (const auto* object :
             {"DiracChain[", "DiracSlash[Mom[Multiparticle[", "PolarizationVector[", "PropDen["})
            require(expression.find(object) != std::string::npos, "incomplete off-shell fermion exchange");
    }
}
} // namespace
int main(int argc, char** argv) {
    try {
        auto dir = argc > 1 ? std::filesystem::path(argv[1])
                            : std::filesystem::path(__FILE__).parent_path().parent_path() / "models";
        model qed(dir, 3), qed_lower(dir, 4), sm(dir, 1), sm_lower(dir, 2);
        counts({"h", "h", "h", "h"}, sm, sm_lower, 4, 4, 3);
        counts({"W+", "W+", "W-", "W-"}, sm, sm_lower, 6, 8, 6);
        counts({"Z", "Z", "W+", "W-"}, sm, sm_lower, 3, 3, 3);
        counts({"G.+", "G.+", "G.-", "G.-"}, sm, sm_lower, 3, 5, 3);
        counts({"e", "E", "A.+", "A.+"}, qed, qed_lower, 2, 2, 2);
        counts({"e", "E", "A.+", "A.+", "A.-"}, qed, qed_lower, 6, 6, 6);
        counts({"e", "E", "A.+", "A.+", "A.-", "A.-"}, qed, qed_lower, 24, 24, 14);
        counts({"e", "E", "E", "e", "A.+"}, qed, qed_lower, 8, 16, 8);
        counts({"e", "E", "E", "e", "e", "E"}, qed, qed_lower, 36, 144, 18);
        counts({"A.+", "A.-", "A.+", "A.-"}, qed, qed_lower, 0, 0, 0);

        // The identical-fermion direct and exchange diagrams carry opposite
        // signs for each of the two correlated photon helicities.
        process repeated{"e", "E", "E", "e"};
        for (const auto& d : diagram_plans(qed, repeated)) {
            for (const auto& t : d.terms) {
                auto side = t.edges.at(0).legs;
                require(side == std::vector<int>{2, 4} || side == std::vector<int>{3, 4},
                        "unexpected photon cut");
                require(t.sign == (side.front() == 3 ? 1 : -1), "incorrect external fermion permutation");
            }
        }
        auto channels = channel_plans(qed_lower, repeated);
        require(channels.size() == 2, "lost a direct or exchange factorization channel");
        require(channels.front().terms.size() == 2 && channels.back().terms.size() == 2,
                "lost one orientation of the massless exchanged vector");
        raw_feynman_rules(dir.parent_path() / "tests" / "reference-models" / "calchep");
        std::cout << "Native tree topology checks passed.\n";
    } catch (const std::exception& error) {
        std::cerr << error.what() << '\n';
        return 1;
    }
}
