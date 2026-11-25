#include "amplitude.hpp"

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

  // Five massless vectors with a single opposite helicity have cubic
  // diagrams, including unphysical individual poles, but no nonzero cuts.
  const model sm(argv[1], 1, "SM"), sm_cuts(argv[1], 2, "SM");
  for (const std::string isolated : {"G.+", "G.-"}) {
    const auto other = sm.antiparticle(isolated);
    amplitude gluons({other, other, other, other, isolated}, sm, sm_cuts);
    construct_options zero_options;
    zero_options.workers = 1;
    zero_options.validation.seed = 49117;
    require(gluons.construct(zero_options).str() == "0" && !gluons.diagrams().empty() &&
                gluons.last_factorization().route == "VanishingResiduesAndLocality" &&
                gluons.last_construction().validation.passed(),
            "massless pole-free amplitude with negative energy dimension");
  }

  // Both chiralities of the nonvanishing five-gluon tree require a
  // simultaneous solution in the independent SU(3) components.
  for (unsigned negative : {2u, 3u}) {
    std::vector<std::string> legs(5, "G.+");
    std::fill(legs.end() - negative, legs.end(), "G.-");
    amplitude gluons(legs, sm, sm_cuts);
    gluons.generate_diagrams(1).generate_factorization_products(1);
    const auto &answer = gluons.complete_factorization(1);
    const auto &fit = gluons.last_factorization();
    require(answer.str() != "0" && answer.complexity() <= 6 &&
                fit.route == "ColorCoefficients" && fit.rank > 0 && fit.validation_samples > 0,
            "chiral five-vector completion from the lower-point amplitudes");
  }

  process.add_diagram("0");
  options.pole_preference = generation_references::physical;
  process.construct(options);
  require(process.diagrams().size() == diagram_count + 1,
          "manual row lost during policy change");
}
