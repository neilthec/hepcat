#include "amplitude.hpp"

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
  if (staged.vertices_model_ && policy != staged.references_) {
    auto remove_generated = [](auto &rows) {
      rows.erase(std::remove_if(rows.begin(), rows.end(),
                                [](const auto &row) { return row.generated_; }),
                 rows.end());
    };
    staged.references_ = policy;
    remove_generated(staged.diagrams_);
    remove_generated(staged.products_);
    staged.diagrams_generated_ = staged.channels_generated_ = false;
  }
  run("Generate and reduce", [&] {
    if (staged.vertices_model_)
      staged.generate(options.workers);
    staged.reduce(options.workers);
  });
  auto factor = options.factorization;
  factor.try_grouped_currents = true;
  if (!factor.workers)
    factor.workers = options.workers;
  run("Complete factorization", [&] { staged.complete_factorization(factor); });
  auto energy = options.high_energy;
  if (!energy.workers)
    energy.workers = options.workers;
  run("Complete high energy and reduce",
      [&] { staged.complete_high_energy(energy); });
  auto validation = options.validation;
  if (!validation.workers)
    validation.workers = options.workers;
  run("Validate", [&] {
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
