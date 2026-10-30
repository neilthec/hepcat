// Independent checks of random common kinematics and honest comparison reports.
#include "spinas-validation.hpp"
#include <iostream>
#include <sstream>

namespace {
unsigned checks = 0;
unsigned failures = 0;
void check(const char* name, const bool value) {
  ++checks;
  if (!value) {
    ++failures;
    std::cerr << "FAIL: " << name << '\n';
  }
}

template <std::size_t N>
bool random_kinematics(const std::array<ldouble, N>& masses,
                       const std::array<std::size_t, 2>& incoming) {
  std::mt19937_64 random(1729);
  hepcat_spinas::ValidationOptions options;
  const auto range = hepcat_spinas::detail::validation_energy_range(masses, incoming, options);
  for (unsigned index = 0; index < 1000; ++index) {
    const auto point = hepcat_spinas::make_random_validation_momenta(masses, incoming, random, options);
    if (!hepcat_spinas::validate_momenta(point, masses, incoming).valid) return false;
    const ldouble energy = point[incoming[0]][0] + point[incoming[1]][0];
    if (energy < range[0] || energy > range[1]) return false;
    for (const auto& momentum : point) {
      for (const ldouble component : momentum) {
        if (component != static_cast<ldouble>(static_cast<double>(component))) return false;
      }
    }
  }
  return true;
}
} // namespace

int main() {
  using namespace hepcat_spinas;
  try {
    const std::array<ldouble, 4> masses = {{1, 1, 0, 0}};
    const std::array<std::size_t, 2> incoming = {{0, 1}};
    check("1000 four-point random points share exact double inputs and conserve momenta",
      random_kinematics(masses, incoming));
    check("1000 fully massive five-point points satisfy every external shell",
      random_kinematics(std::array<ldouble, 5>{{1, 2, 0.3L, 0.4L, 0.5L}}, incoming));
    check("1000 mixed six-point points support non-leading incoming labels",
      random_kinematics(std::array<ldouble, 6>{{0, 1, 0.2L, 0.3L, 2, 0}},
        std::array<std::size_t, 2>{{1, 4}}));

    ValidationOptions options;
    options.seed = 93;
    const auto parse = [&](std::vector<std::string> arguments) {
      std::vector<char*> argv;
      for (auto& argument : arguments) argv.push_back(&argument[0]);
      return parse_validation_arguments(static_cast<int>(argv.size()), argv.data(), options);
    };
    const auto parsed = parse({"validator", "--points", "17", "--seed", "12",
      "--relative-tolerance", "1e-5", "--absolute-tolerance", "0"});
    check("readable validator main delegates exact point, seed and tolerance parsing",
      parsed.points == 17 && parsed.seed == 12 && parsed.relative_tolerance == ldouble(1e-5L)
      && parsed.absolute_tolerance == 0);
    bool rejects_bad_arguments = true;
    for (const auto& arguments : std::vector<std::vector<std::string>>{
        {"validator", "--points"}, {"validator", "--points", "-1"},
        {"validator", "--seed", "12junk"}, {"validator", "--unknown", "1"},
        {"validator", "--relative-tolerance", "nan"},
        {"validator", "--absolute-tolerance", "-1"}}) {
      bool rejected = false;
      try { parse(arguments); } catch (const std::exception&) { rejected = true; }
      rejects_bad_arguments = rejects_bad_arguments && rejected;
    }
    check("shared argument parsing refuses missing, invalid and unsafe numeric controls",
      rejects_bad_arguments);
    const auto unavailable = unavailable_validation(options, "missing backend");
    const auto configuration_error = failed_validation(options, "wrong model");
    check("readable report helpers preserve requested seed and cannot mark failure as a pass",
      !unavailable.passed && unavailable.status == "UNAVAILABLE" && unavailable.seed == 93
      && unavailable.requested_points == options.points && !configuration_error.passed
      && configuration_error.status == "ERROR" && configuration_error.errors.size() == 1);
    std::mt19937_64 first_engine(options.seed), second_engine(options.seed), other_engine(options.seed + 1);
    const auto first_point = make_random_validation_momenta(masses, incoming, first_engine, options);
    check("the seed replays the identical complete phase-space point",
      first_point == make_random_validation_momenta(masses, incoming, second_engine, options));
    check("different seeds generate distinct phase-space points",
      first_point != make_random_validation_momenta(masses, incoming, other_engine, options));

    std::size_t first_calls = 0;
    std::size_t second_calls = 0;
    Momenta<4> shared;
    bool identical_inputs = true;
    const auto value = [](const Momenta<4>& point) {
      return 1 + std::abs(dot(point[0], point[2])) + std::abs(point[3][1]);
    };
    std::ostringstream progress;
    const auto equal = compare_squared_amplitudes(masses, incoming,
      [&](const Momenta<4>& point) { ++first_calls; shared = point; return value(point); },
      [&](const Momenta<4>& point) {
        ++second_calls;
        identical_inputs = identical_inputs && shared == point;
        return value(point);
      }, options, &progress);
    check("all 1000 random points agree for equal evaluators",
      equal.passed && equal.status == "PASS" && equal.requested_points == 1000
      && equal.generated_points == 1000 && equal.compared_points == 1000
      && equal.mismatches == 0 && equal.evaluation_errors == 0);
    check("both implementations receive all 1000 bit-identical common points",
      first_calls == 1000 && second_calls == 1000 && identical_inputs);
    check("successful comparison has no hidden failing point or errors",
      !equal.has_first_failure && equal.errors.empty() && equal.maximum_relative_error == 0);
    check("progress reaches the full requested count",
      progress.str().find("1000/1000") != std::string::npos);

    const auto wrong = compare_squared_amplitudes(masses, incoming, value,
      [&](const Momenta<4>& point) { return 2 * value(point); }, options);
    check("a wrong normalization fails every point without fitting or resampling",
      !wrong.passed && wrong.status == "FAIL" && wrong.mismatches == 1000
      && wrong.compared_points == 1000 && wrong.generated_points == 1000
      && wrong.evaluation_errors == 0 && wrong.maximum_relative_error == ldouble(0.5));
    check("a mismatch preserves the first point and both raw results",
      wrong.has_first_failure && wrong.first_failure.point_index == 1
      && wrong.first_failure.reference_value == 2 * wrong.first_failure.hepcat_value
      && std::equal(first_point.begin(), first_point.end(), wrong.first_failure.momenta.begin()));
    std::ostringstream failure_text;
    print_validation_report(wrong, failure_text);
    check("the report prints the seed, first failure and its complete momenta",
      failure_text.str().find("Seed: 93") != std::string::npos
      && failure_text.str().find("First failing point: 1") != std::string::npos
      && failure_text.str().find("p4 =") != std::string::npos);

    const auto tiny_wrong = compare_squared_amplitudes(masses, incoming,
      [](const Momenta<4>&) { return ldouble(1e-30L); },
      [](const Momenta<4>&) { return ldouble(0); }, options);
    check("small higher-point-sized amplitudes cannot pass because of a fixed absolute floor",
      !tiny_wrong.passed && tiny_wrong.mismatches == 1000
      && tiny_wrong.maximum_relative_error == 1);
    const auto zeros = compare_squared_amplitudes(masses, incoming,
      [](const Momenta<4>&) { return ldouble(0); },
      [](const Momenta<4>&) { return ldouble(0); }, options);
    check("two exact zero squared amplitudes pass without an undefined relative error",
      zeros.passed && zeros.maximum_relative_error == 0 && zeros.compared_points == 1000);

    std::size_t surviving_calls = 0;
    const auto exception = compare_squared_amplitudes(masses, incoming,
      [](const Momenta<4>&) -> ldouble { throw std::runtime_error("known evaluator failure"); },
      [&](const Momenta<4>&) { ++surviving_calls; return ldouble(1); }, options);
    check("evaluation exceptions never count as passed or compared points",
      !exception.passed && exception.status == "ERROR" && exception.evaluation_errors == 1000
      && exception.compared_points == 0 && exception.generated_points == 1000
      && surviving_calls == 1000 && !exception.errors.empty());
    check("exceptions retain a replay point and the other implementation's value",
      exception.has_first_failure && exception.first_failure.reference_value == 1
      && exception.first_failure.message.find("known evaluator failure") != std::string::npos);
    const auto nonfinite = compare_squared_amplitudes(masses, incoming, value,
      [](const Momenta<4>&) { return std::numeric_limits<ldouble>::quiet_NaN(); }, options);
    check("nonfinite reference amplitudes explicitly fail all affected points",
      !nonfinite.passed && nonfinite.evaluation_errors == 1000
      && nonfinite.compared_points == 0 && nonfinite.status == "ERROR");
    const auto negative = compare_squared_amplitudes(masses, incoming,
      [](const Momenta<4>&) { return ldouble(-1); }, value, options);
    check("negative squared amplitudes cannot silently pass",
      !negative.passed && negative.evaluation_errors == 1000 && negative.compared_points == 0);

    ValidationOptions invalid = options;
    invalid.points = 0;
    const auto empty = compare_squared_amplitudes(masses, incoming, value, value, invalid);
    check("a zero-size validation cannot claim success",
      !empty.passed && empty.status == "ERROR" && empty.compared_points == 0);
    invalid = options;
    invalid.minimum_center_of_mass_energy = 1;
    const auto below_threshold = compare_squared_amplitudes(masses, incoming, value, value, invalid);
    check("invalid energy bounds fail before evaluating a point",
      !below_threshold.passed && below_threshold.status == "ERROR"
      && below_threshold.generated_points == 0 && below_threshold.compared_points == 0);

    ValidationOptions strict = options;
    strict.relative_tolerance = 1e-12L;
    const auto near = [](const Momenta<4>&) { return ldouble(1 + 1e-9L); };
    const auto one = [](const Momenta<4>&) { return ldouble(1); };
    check("the default relative tolerance admits a controlled small roundoff difference",
      compare_squared_amplitudes(masses, incoming, one, near, options).passed);
    check("an explicit tighter tolerance detects the same difference",
      !compare_squared_amplitudes(masses, incoming, one, near, strict).passed);
    strict = options;
    strict.minimum_center_of_mass_energy = 4;
    strict.maximum_center_of_mass_energy = 5;
    bool in_range = true;
    const auto custom = compare_squared_amplitudes(masses, incoming,
      [&](const Momenta<4>& point) {
        const auto energy = point[0][0] + point[1][0];
        in_range = in_range && energy >= 4 && energy <= 5;
        return value(point);
      }, value, strict);
    check("explicit energy limits constrain every sampled point",
      custom.passed && in_range && custom.minimum_center_of_mass_energy == 4
      && custom.maximum_center_of_mass_energy == 5);
    strict.maximum_center_of_mass_energy = 4;
    bool fixed_energy = true;
    const auto fixed = compare_squared_amplitudes(masses, incoming,
      [&](const Momenta<4>& point) {
        const auto energy = point[0][0] + point[1][0];
        fixed_energy = fixed_energy && std::abs(energy - 4) <= 1e-12L;
        return value(point);
      }, value, strict);
    check("equal explicit energy bounds produce a fixed-energy random angular sample",
      fixed.passed && fixed_energy && fixed.minimum_center_of_mass_energy == 4
      && fixed.maximum_center_of_mass_energy == 4);
  } catch (const std::exception& error) {
    ++failures;
    std::cerr << "Unexpected exception: " << error.what() << '\n';
  }
  std::cout << "SPINAS_VALIDATION_RUNTIME_CHECKS=" << checks << "; FAILURES=" << failures << '\n';
  return failures ? 1 : 0;
}
