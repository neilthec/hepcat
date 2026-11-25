#include "amplitude.hpp"
#include "generation_plan.hpp"

#include <algorithm>
#include <chrono>
#include <sstream>

namespace hepcat {
namespace {
std::string validation_failure(const validation_report &report) {
  std::ostringstream reason;
  reason << "construction validation failed (seed " << report.seed << ")";
  for (const auto &row : report.checks)
    if (row.status != check_outcome::pass)
      reason << "\n" << row.kind << " " << row.label << ": " << row.detail;
  for (const auto &row : report.numerical)
    if (row.status != check_outcome::pass) {
      reason << "\n"
             << row.kind << ": " << row.failures << " mismatches in "
             << row.compared << "/" << report.requested << " points";
      if (!row.detail.empty())
        reason << ", " << row.detail;
    }
  return reason.str();
}
} // namespace

construction_validation_error::construction_validation_error(
    validation_report checks)
    : std::runtime_error(validation_failure(checks)),
      report(std::move(checks)) {}

const construction_report &amplitude::last_construction() const {
  if (!construction_)
    throw std::logic_error("no successful construction for this amplitude");
  return *construction_;
}

const amp_expression &amplitude::construct(const construct_options &options) {
  using clock = std::chrono::steady_clock;
  const auto start = clock::now();
  if (vertices_model_ && vertices_model_->is_feynman())
    throw std::invalid_argument("Construction requires a constructive vertex model. Supply a Feynman model for validation.");
  amplitude staged = *this;
  construction_report report;
  auto run = [&](const char *name, auto action) {
    if (options.progress)
      options.progress(name);
    auto before = clock::now();
    action();
    report.seconds.emplace_back(
        name, std::chrono::duration<double>(clock::now() - before).count());
  };
  auto policy = options.pole_preference.value_or(
      references_ == generation_references::automatic
          ? generation_references::physical
          : references_);
  auto select_references = [&](generation_references choice) {
    if (!staged.vertices_model_ || choice == staged.references_) return;
    auto remove_generated = [](auto &rows) {
      rows.erase(std::remove_if(rows.begin(), rows.end(),
                                [](const auto &row) { return row.generated_; }),
                 rows.end());
    };
    staged.references_ = choice;
    remove_generated(staged.diagrams_);
    remove_generated(staged.products_);
    staged.diagrams_generated_ = staged.channels_generated_ = false;
  };
  select_references(policy);
  auto factor = options.factorization;
  if (!factor.workers)
    factor.workers = options.workers;

  run("Generate and reduce", [&] {
    if (staged.vertices_model_) {
      staged.generate_diagrams(options.workers);
      if (staged.channels_model_) staged.generate_factorization_products(options.workers);
    }
    staged.reduce(options.workers);
  });
  run("Complete factorization", [&] {
    // With only massless external states, a local polynomial in their spinors
    // has nonnegative fixed-angle energy degree. If every lower-point product
    // vanishes, locality and a negative growth bound therefore force zero.
    // A user-supplied row, a contact vertex, or a different bound retains the
    // ordinary completion path. The final validation still checks every cut.
    const auto bound = options.high_energy.energy_bound.value_or(4 - int(masses_.size()));
    if (vertices_model_ && channels_model_ && staged.channels_generated_ &&
        staged.products_.empty() && masses_.size() > 4 && bound < 0 &&
        std::all_of(masses_.begin(), masses_.end(), [](const auto &m) { return m == "0"; }) &&
        std::all_of(staged.diagrams_.begin(), staged.diagrams_.end(),
                    [](const auto &d) { return d.generated_ && !d.shells.empty(); }) &&
        factor.correction_basis.empty() && options.high_energy.correction_basis.empty()) {
      staged.completed_.emplace("0", masses_);
      staged.report_ = {};
      staged.report_.route = "VanishingResiduesAndLocality";
    } else {
      staged.complete_factorization(factor);
    }
  });
  auto energy = options.high_energy;
  if (!energy.workers)
    energy.workers = options.workers;
  run("Complete high energy and reduce",
      [&] {
        try {
          staged.complete_high_energy(energy);
        } catch (const std::runtime_error&) {
          if (!staged.vertices_model_ || !staged.channels_model_ ||
              policy != generation_references::cyclic)
            throw;
          // Try a different constructive continuation only after the intrinsic
          // energy test fails. The comparison model is read in the next stage.
          select_references(generation_references::physical);
          staged.generate_diagrams(options.workers);
          staged.generate_factorization_products(options.workers);
          staged.reduce(options.workers);
          staged.complete_factorization(factor);
          staged.complete_high_energy(energy);
        }
      });
  auto validation = options.validation;
  if (!validation.workers)
    validation.workers = options.workers;
  run("Validate", [&] {
    if (staged.feynman_model_) staged.generate_feynman_diagrams(options.workers);
    report.validation =
        options.reference_amplitude
            ? staged.validate_against(*options.reference_amplitude, validation)
            : staged.validate(validation);
    if (!report.validation.passed())
      throw construction_validation_error(std::move(report.validation));
  });
  report.total_seconds =
      std::chrono::duration<double>(clock::now() - start).count();
  staged.construction_ = std::move(report);
  *this = std::move(staged);
  return final_amplitude();
}
} // namespace hepcat
