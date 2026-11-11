// Independent checks of random common kinematics and honest comparison reports.
/*
HEPCAT - High Energy Physics Constructive Amplitude Tool

Copyright (C) 2026 Neil Christensen

This file is part of HEPCAT.

HEPCAT is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

HEPCAT is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with HEPCAT.  If not, see <https://www.gnu.org/licenses/>.
*/

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

    // Independent division examples cover the optimization's real domain and
    // the ranges where retaining the original complex division is essential.
    const CachedDenominator real_denominator(cdouble(3, 0), true);
    const cdouble ordinary_numerator(7, -11);
    check("normal real denominators reuse their reciprocal",
      real_denominator.uses_real_reciprocal() &&
      std::abs(real_denominator.divide(ordinary_numerator) - ordinary_numerator/cdouble(3,0))
        <= 8*std::numeric_limits<ldouble>::epsilon()*std::abs(ordinary_numerator/cdouble(3,0)));
    const CachedDenominator declared_width(cdouble(3, 0), false);
    check("a width-bearing expression never enables the real shortcut, even at zero imaginary value",
      !declared_width.uses_real_reciprocal() &&
      declared_width.divide(ordinary_numerator) == ordinary_numerator/cdouble(3,0));
    const cdouble tiny_imaginary_denominator(3, std::numeric_limits<ldouble>::min());
    const CachedDenominator nonreal_denominator(tiny_imaginary_denominator, true);
    check("an arbitrarily small imaginary denominator is retained exactly",
      !nonreal_denominator.uses_real_reciprocal() &&
      nonreal_denominator.divide(ordinary_numerator) == ordinary_numerator/tiny_imaginary_denominator);
    const cdouble width_value(3, -ldouble(0.125L));
    const CachedDenominator finite_width(width_value, false);
    check("finite-width division retains its complex phase",
      !finite_width.uses_real_reciprocal() &&
      finite_width.divide(ordinary_numerator) == ordinary_numerator/width_value);
    const cdouble signed_zero(ldouble(-0.0L), ldouble(0));
    const cdouble signed_expected = signed_zero/cdouble(-3,0);
    const cdouble signed_observed = CachedDenominator(cdouble(-3,0),true).divide(signed_zero);
    check("zero numerator components preserve the original division and signed zeros",
      signed_expected == signed_observed &&
      std::signbit(std::real(signed_expected)) == std::signbit(std::real(signed_observed)) &&
      std::signbit(std::imag(signed_expected)) == std::signbit(std::imag(signed_observed)));
    const ldouble largest = std::numeric_limits<ldouble>::max();
    const ldouble smallest = std::numeric_limits<ldouble>::min();
    const cdouble huge_numerator(largest/4, -largest/8);
    const CachedDenominator huge_denominator(cdouble(largest/2,0),true);
    check("subnormal reciprocal ranges retain scaled complex division",
      !huge_denominator.uses_real_reciprocal() &&
      huge_denominator.divide(huge_numerator) == huge_numerator/cdouble(largest/2,0));
    const CachedDenominator tiny_denominator(cdouble(smallest/2,0),true);
    const cdouble tiny_numerator(smallest/4,smallest/8);
    check("subnormal denominator and numerator ranges retain scaled complex division",
      !tiny_denominator.uses_real_reciprocal() &&
      tiny_denominator.divide(tiny_numerator) == tiny_numerator/cdouble(smallest/2,0));
    const cdouble underflow_numerator(smallest*2,smallest*3);
    check("underflowing products use the original division",
      CachedDenominator(cdouble(4,0),true).divide(underflow_numerator)
        == underflow_numerator/cdouble(4,0));
    const cdouble overflow_numerator(largest/2,largest/3);
    const auto overflow_expected = overflow_numerator/cdouble(ldouble(0.25L),0);
    const auto overflow_observed = CachedDenominator(cdouble(ldouble(0.25L),0),true).divide(overflow_numerator);
    check("overflowing products use the original division",
      std::isinf(std::real(overflow_observed)) == std::isinf(std::real(overflow_expected)) &&
      std::isinf(std::imag(overflow_observed)) == std::isinf(std::imag(overflow_expected)));
    const CachedDenominator exact_pole(cdouble(0,0),true);
    check("an exact pole never enables reciprocal multiplication",
      !exact_pole.uses_real_reciprocal());
    const cdouble nonfinite_numerator(std::numeric_limits<ldouble>::quiet_NaN(), 1);
    const auto nonfinite_expected = nonfinite_numerator/cdouble(3,0);
    const auto nonfinite_observed = real_denominator.divide(nonfinite_numerator);
    check("nonfinite numerator behavior remains the original complex division",
      std::isnan(std::real(nonfinite_observed)) == std::isnan(std::real(nonfinite_expected)) &&
      std::isnan(std::imag(nonfinite_observed)) == std::isnan(std::imag(nonfinite_expected)));
    const cdouble infinite_value(std::numeric_limits<ldouble>::infinity(),0);
    const CachedDenominator infinite_denominator(infinite_value,true);
    check("nonfinite denominators retain the original complex evaluator",
      !infinite_denominator.uses_real_reciprocal() &&
      infinite_denominator.divide(ordinary_numerator) == ordinary_numerator/infinite_value);


    const ldouble epsilon = std::numeric_limits<ldouble>::epsilon();
    const ldouble wide = std::pow(ldouble(10),
      std::numeric_limits<ldouble>::max_exponent10/3);
    const ldouble tiny = 1/wide;
    check("physical two-body momenta avoid a quartic overflow",
      detail::two_body_momentum(wide,0,0) == wide/2);
    check("physical two-body momenta avoid a quartic underflow",
      detail::two_body_momentum(tiny,0,0) == tiny/2);
    const ldouble just_above_threshold = std::nextafter(ldouble(2),ldouble(3));
    const ldouble threshold_reference = std::sqrt(
      (just_above_threshold-2)*(just_above_threshold+2))/2;
    check("near-threshold two-body momenta retain the small physical difference",
      std::abs(detail::two_body_momentum(just_above_threshold,1,1)/threshold_reference-1)
        <= 64*epsilon && detail::two_body_momentum(2,1,1) == 0);
    check("negative-mass helper conventions retain the same squared-mass kinematics",
      std::abs(detail::two_body_momentum(10,-1,2)/detail::two_body_momentum(10,1,2)-1)
        <= 64*epsilon);

    const FourMomentum fast_frame = {{std::hypot(ldouble(1e8L),ldouble(1)),0,0,1e8L}};
    const FourMomentum backward_photon = {{1,0,0,-1}};
    const auto soft_photon = detail::boost_from_rest(backward_photon,fast_frame,1);
    const ldouble soft_energy = 1/(fast_frame[0]+fast_frame[3]);
    check("a backward massless particle keeps positive energy under a large boost",
      soft_photon[0] > 0 && std::abs(soft_photon[0]/soft_energy-1) <= 64*epsilon
      && soft_photon[3] == -soft_photon[0]);
    const ldouble child_mass = 1e-4L;
    const FourMomentum backward_massive = {{std::hypot(ldouble(1),child_mass),0,0,-1}};
    const FourMomentum faster_frame = {{std::hypot(ldouble(1e12L),ldouble(1)),0,0,1e12L}};
    const auto massive_boosted = detail::boost_on_shell_from_rest(
      backward_massive,child_mass,faster_frame,1);
    // E' = p(E-1)+(E_frame-p)E, with both small differences rationalized.
    const ldouble massive_energy = faster_frame[3]*child_mass*
      (child_mass/(backward_massive[0]+1))
      + backward_massive[0]/(faster_frame[0]+faster_frame[3]);
    check("a known on-shell child does not subtract its tiny E-minus-p component",
      std::isfinite(massive_boosted[0]) &&
      std::abs(massive_boosted[0]/massive_energy-1) <= 128*epsilon);
    const FourMomentum oblique_frame = {{std::hypot(ldouble(5e7L),ldouble(1)),3e7L,4e7L,0}};
    const ldouble oblique_p = std::hypot(oblique_frame[1],oblique_frame[2]);
    const ldouble nx=oblique_frame[1]/oblique_p,ny=oblique_frame[2]/oblique_p;
    // Reuse the represented direction so this tests an exactly collinear input.
    const ldouble oblique_child_energy=std::hypot(nx,ny);
    const auto oblique_soft = detail::boost_on_shell_from_rest(
      FourMomentum{{oblique_child_energy,-nx,-ny,0}},0,oblique_frame,1);
    const ldouble oblique_energy = oblique_child_energy/(oblique_frame[0]+oblique_p);
    check("stable backward boosts are not tied to a coordinate axis",
      std::abs(oblique_soft[0]/oblique_energy-1) <= 128*epsilon &&
      std::abs(oblique_soft[1]/oblique_energy+nx) <= 128*epsilon &&
      std::abs(oblique_soft[2]/oblique_energy+ny) <= 128*epsilon);

    const FourMomentum slow_frame = {{1,0,0,1e-20L}};
    const auto tiny_boost = detail::boost_from_rest(FourMomentum{{1,0,0,0}},slow_frame,1);
    check("tiny boosts preserve a small newly generated spatial component",
      tiny_boost[3] == slow_frame[3]);
    const FourMomentum ordinary_frame = {{std::sqrt(ldouble(29)),0,0,5}};
    const auto virtual_boost = detail::boost_from_rest(FourMomentum{{3,1,2,-1}},ordinary_frame,2);
    const auto past_boost = detail::boost_from_rest(FourMomentum{{-3,1,2,-1}},ordinary_frame,2);
    check("the generic boost does not impose a child mass shell",
      std::abs(virtual_boost[0]-(3*ordinary_frame[0]-5)/2) <= 128*epsilon*10 &&
      std::abs(mass2(virtual_boost)-3) <= 256*epsilon*100);
    check("the generic boost preserves negative-energy vector conventions",
      past_boost[0] < 0 &&
      std::abs(past_boost[0]-(-3*ordinary_frame[0]-5)/2) <= 128*epsilon*10 &&
      std::abs(mass2(past_boost)-3) <= 256*epsilon*100);

    const std::array<ldouble,4> massless = {{0,0,0,0}};
    const auto wide_example = make_example_momenta(massless,incoming,wide,0);
    const auto tiny_example = make_example_momenta(massless,incoming,tiny,0);
    check("complete example kinematics work beyond the quartic arithmetic range",
      validate_momenta(wide_example,massless,incoming).valid &&
      std::abs((wide_example[0][0]+wide_example[1][0])/wide-1) <= 64*epsilon);
    check("complete example kinematics retain very small positive energies",
      validate_momenta(tiny_example,massless,incoming).valid && tiny_example[2][0] > 0 &&
      std::abs((tiny_example[0][0]+tiny_example[1][0])/tiny-1) <= 64*epsilon);
    std::mt19937_64 wide_random(20261101);
    bool wide_random_valid = true;
    for (unsigned sample=0;sample<16;++sample) {
      const std::array<ldouble,6> zero_masses = {{0,0,0,0,0,0}};
      const auto point = make_random_momenta(zero_masses,incoming,wide,wide_random);
      wide_random_valid = wide_random_valid && validate_momenta(point,zero_masses,incoming).valid;
    }
    check("successive random decays retain finite conserved high-scale momenta",
      wide_random_valid);

    const ValidationOptions first_automatic_options;
    const ValidationOptions second_automatic_options;
    check("new validation options choose distinct time-based seeds",
      first_automatic_options.seed != second_automatic_options.seed);
    const ValidationOptions copied_automatic_options = first_automatic_options;
    check("copying validation settings preserves their chosen replay seed",
      copied_automatic_options.seed == first_automatic_options.seed);

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
    const auto zero_seed = parse({"validator", "--seed", "0"});
    const auto largest_seed = parse({"validator", "--seed", "18446744073709551615"});
    check("explicit zero and maximum unsigned seeds remain valid replay choices",
      zero_seed.seed == 0 && largest_seed.seed == std::numeric_limits<std::uint64_t>::max());
    std::mt19937_64 first_zero_engine(zero_seed.seed), second_zero_engine(zero_seed.seed);
    check("an explicit zero seed replays the same complete physical point",
      make_random_validation_momenta(masses, incoming, first_zero_engine, zero_seed) ==
        make_random_validation_momenta(masses, incoming, second_zero_engine, zero_seed));
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
    ValidationOptions automatic_run_options;
    automatic_run_options.points = 8;
    const auto automatic_report = compare_squared_amplitudes(
      masses, incoming, value, value, automatic_run_options);
    std::ostringstream automatic_text;
    print_validation_report(automatic_report, automatic_text);
    check("automatic runs report the concrete time-based seed used for all their points",
      automatic_report.passed && automatic_report.seed == automatic_run_options.seed &&
        automatic_text.str().find("Seed: " + std::to_string(automatic_run_options.seed)) != std::string::npos);

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
