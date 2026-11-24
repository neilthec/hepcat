#include "amplitude.hpp"
#include "charged_tree_sewing.hpp"

#include <stdexcept>

using namespace hepcat;
namespace {
void require(bool ok, const char *message) {
  if (!ok)
    throw std::runtime_error(message);
}
} // namespace

int main(int argc, char **argv) {
  require(argc == 2, "supply the model directory");
  const std::vector<std::string> masses{"m", "m", "0", "0"};
  auto score = [&](const std::string &expression) {
    return amp_expression(expression, masses).complexity();
  };
  require(score("0") == 0 && score("Power[Plus[x,y],2]") == 3,
          "additive complexity");
  require(score("Power[Plus[x,y],12]") == 13, "positive power expansion");
  require(score("Times[MomProd[1,2],Power[Plus[x,y],-2]]") == 9,
          "denominator opacity");
  require(score("Times[2,MomProd[1,2],Mom[1]]") == 13,
          "numerator momentum degree");

  model vertices(argv[1], 3), channels(argv[1], 4);
  amplitude process({"e", "E", "A.+", "A.-"}, vertices, channels);
  construct_options options;
  options.workers = 2;
  options.validation.seed = 4711;
  std::vector<std::string> stages;
  options.progress = [&](const auto &name) { stages.push_back(name); };
  auto physical = process.construct(options);
  require(!physical.discarded_remainder() && physical.complexity() == 10,
          "final reduction");
  require(process.last_construction().validation.passed() && stages.size() == 4,
          "stages or validation");
  auto diagram_count = process.diagrams().size();
  auto channel_count = process.factorization_products().size();
  options.reference_amplitude.emplace("0", process.external_masses());
  bool failed = false;
  try {
    process.construct(options);
  } catch (const construction_validation_error &error) {
    failed = !error.report.passed();
  }
  require(failed && process.final_amplitude().str() == physical.str() &&
              process.last_construction().validation.passed(),
          "failed validation replaced a good result");
  options.reference_amplitude.reset();
  options.pole_preference = generation_references::cyclic;
  process.construct(options);
  require(process.diagrams().size() == diagram_count &&
              process.factorization_products().size() == channel_count,
          "switching pole policy duplicated rows");
  require(process.last_construction().validation.passed(),
          "cyclic construction failed");

  process.add_diagram("0");
  options.pole_preference = generation_references::physical;
  process.construct(options);
  require(process.diagrams().size() == diagram_count + 1,
          "manual row lost during policy change");
  require(!process.final_amplitude().sewing_form(), "four-point presentation changed");
  const std::vector<std::string> legs{"e","E","e","E","e","E"};
  amplitude three_pairs(legs,vertices,channels);
  auto result=three_pairs.construct(options);
  const auto sewn=result.sewing_form();
  require(sewn && sewn->sewings().size()==9 && sewn->diagrams()==36,
          "the complete constructor lost the balanced fermion cuts");
  require(sewn->description().find("Wbar[6<-4,5]")!=std::string::npos &&
          result.latex()!=sewn->latex(), "short six-point result lost its original display");
  result.reduce(generation_references::cyclic);
  require(result.sewing_form() && !result.sewing_form()->physical(), "requested reference was lost");
  result.reduce(generation_references::keep);
  require(!result.sewing_form(), "an explicit reference was replaced");
  model standard_model(argv[1],1);
  require(!charged_tree_sewing::from_model(standard_model,legs,true),
          "an additional interacting sector was silently dropped");
}
