#include "amp_expression.hpp"
#include "chart_scalar.hpp"
#include "high_energy_chart.hpp"
#include "validation_color.hpp"

#include <algorithm>
#include <chrono>
#include <stdexcept>

namespace hepcat {

identity_check_report amp_expression::check_identity(const amp_expression& alternative,
                                                      const identity_check_options& options) const {
    if (!options.samples)
        throw std::invalid_argument("An identity check needs at least one sample");
    identity_check_report report;
    report.seed = options.seed.value_or(std::chrono::high_resolution_clock::now().time_since_epoch().count());
    Algebra a;
    const auto masses = import_masses(a);
    if (alternative.import_masses(a) != masses)
        throw std::invalid_argument("Identity check needs the same ordered external masses on both sides");
    a.setKinematics(masses, {});
    std::vector<Id> roots{import_into(a), alternative.import_into(a)};
    std::map<Id, mpq_class> parameters;
    for (const auto& [name, literal] : options.parameters) {
        const Id key = a.parse(name);
        mpq_class value;
        if (!chart_detail::free_parameter(a.node(key)) ||
            mpq_set_str(value.get_mpq_t(), literal.c_str(), 10) != 0 || value.get_den() == 0)
            throw std::invalid_argument("Expected a scalar parameter and rational value for " + name);
        value.canonicalize();
        if (!parameters.emplace(key, std::move(value)).second)
            throw std::invalid_argument("Duplicate parameter in identity check: " + name);
    }
    const auto& colors = options.color_representations;
    if (!colors.empty() && (colors.size() != masses.size() ||
        std::any_of(colors.begin(), colors.end(), [](int c) { return c != 1 && c != 3 && c != -3 && c != 8; })))
        throw std::invalid_argument("Specify 1, 3, -3 or 8 for each external color representation");
    try {
        validation_detail::color_polynomial color(a);
        std::vector<std::vector<Id>> pairs{roots};
        if (color.contains_color(roots[0]) || color.contains_color(roots[1])) {
            if (colors.empty())
                throw residue_unsupported("Supply the external color representations to check colored expressions");
            pairs = validation_detail::fundamental_color_projections(a, colors).project(roots);
        }
        unsigned consecutive_singular = 0;
        for (auto sample = report.seed; report.compared < options.samples; ++sample) {
            report.failing_sample = sample;
            try {
                const bool agrees = equal_at_external_point(a, pairs, sample, parameters);
                ++report.compared;
                if (!agrees) {
                    report.status = identity_check_status::mismatch;
                    report.detail = "Different signed values at this external on-shell point";
                    return report;
                }
                consecutive_singular = 0;
            } catch (const residue_singular& e) {
                if (++consecutive_singular == 64) {
                    report.detail = std::string("64 consecutive singular samples: ") + e.what();
                    return report;
                }
            }
        }
        report.failing_sample.reset();
        report.status = identity_check_status::agrees_on_samples;
        report.detail = "Signed expressions agree at the sampled external on-shell points";
    } catch (const residue_unsupported& e) {
        report.detail = e.what();
    } catch (const std::invalid_argument& e) {
        // Includes unsupported color tensors and projection budget exhaustion.
        report.detail = e.what();
    }
    return report;
}

} // namespace hepcat
