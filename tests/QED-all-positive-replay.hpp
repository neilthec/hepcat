// Re-evaluate every raw all-positive outlier using the same measured momenta.
// Record unchanged-input MadGraph comparisons and a separate projected-input identity check.
#pragma once
#include "QED-all-positive-reference.hpp"
#include "validation_precision_parameters.hpp"
#include <fstream>
#include <iomanip>
#include <set>
#include <sstream>

namespace qed_notebook {
namespace precise = hepcat::validation::precision;
using precise::scalar;
inline std::vector<std::string> csv_row(const std::string& line) {
    std::vector<std::string> fields;
    std::istringstream input(line);
    for (std::string field; std::getline(input, field, ',');)
        fields.push_back(std::move(field));
    return fields;
}
inline scalar norm3(const precise::four& p) {
    return scalar(sqrt(p[1] * p[1] + p[2] * p[2] + p[3] * p[3]), precise::bits);
}
// The phase-space generator supplies two equal-mass beams along z and real
// photons in their COM. Retain the recorded photon three-momenta except for
// the most energetic photon's rounding residual, then restore the beam shells.
inline precise::phase_point replay_chart(std::vector<precise::four> momenta, const scalar& mass,
                                         long double& max_relative_change) {
    const auto input = momenta;
    const auto n = momenta.size();
    size_t pivot = 2;
    for (size_t j = 2; j < n; ++j)
        if (momenta[pivot][0] < momenta[j][0])
            pivot = j;
    for (size_t k = 1; k < 4; ++k) {
        momenta[pivot][k] = 0;
        for (size_t j = 2; j < n; ++j)
            if (j != pivot)
                momenta[pivot][k] -= momenta[j][k];
    }
    scalar energy(0, precise::bits);
    for (size_t j = 2; j < n; ++j) {
        momenta[j][0] = norm3(momenta[j]);
        energy += momenta[j][0];
    }
    energy /= 2;
    scalar k(sqrt(energy * energy - mass * mass), precise::bits);
    for (size_t j = 0; j < 2; ++j) {
        momenta[j][0] = energy;
        momenta[j][1] = momenta[j][2] = 0;
        momenta[j][3] = j == 0 ? k : scalar(-k, precise::bits);
    }
    max_relative_change = 0;
    for (size_t j = 0; j < n; ++j)
        for (size_t c = 0; c < 4; ++c) {
            const scalar deviation(abs(input[j][c] - momenta[j][c]) / energy, precise::bits);
            max_relative_change = std::max(max_relative_change, precise::narrow_real(deviation));
            if (j >= 2)
                momenta[j][c] *= -1;
        }
    if (max_relative_change > 1e-12L)
        throw std::runtime_error("phase-space repair exceeds the rounding budget for the stored point");
    std::vector<scalar> masses(n, scalar(0, precise::bits));
    masses[0] = masses[1] = mass;
    return precise::from_momenta(std::move(momenta), std::move(masses), 0);
}
inline long double relative(const precise::number& a, const precise::number& b) {
    auto scale = std::max(a.magnitude(), b.magnitude());
    return scale == 0 ? 0 : precise::narrow_real(scalar((a - b).magnitude() / scale, precise::bits));
}
struct formula_precision_report {
    std::size_t replayed = 0, unchanged_failures = 0, projected_identity_failures = 0;
    long double worst_unchanged_mg = 0, worst_projected_identity = 0, largest_shift = 0;
};

// The unmodified event may have small residuals in its shells and momentum
// conservation. Preserve its measured momenta even when evaluating invariants.
inline precise::phase_point unchanged_chart(std::vector<precise::four> p, const scalar& mass) {
    std::vector<scalar> masses(p.size(), scalar(0, precise::bits));
    masses[0] = masses[1] = mass;
    for (std::size_t i = 2; i < p.size(); ++i)
        for (auto& component : p[i]) component = -component;
    auto chart = precise::from_momenta(std::move(p), std::move(masses), 0);
    for (std::size_t i = 0; i < chart.momenta.size(); ++i)
        for (std::size_t j = 0; j <= i; ++j) {
            const auto &a = chart.momenta[i], &b = chart.momenta[j];
            scalar dot(a[0]*b[0]-a[1]*b[1]-a[2]*b[2]-a[3]*b[3], precise::bits);
            chart.scalar_products[i * chart.momenta.size() + j] = dot;
            chart.scalar_products[j * chart.momenta.size() + i] = dot;
        }
    return chart;
}
inline formula_precision_report replay_formulas(const hepcat::model& vertices, unsigned n,
        const std::filesystem::path& prefix, size_t requested, double m, long double tolerance = 1e-7L,
        bool sparse = false, bool expanded_poles = true, mpq_class charge_value = mpq_class(1, 3),
        unsigned input_bits = std::numeric_limits<long double>::digits) {
    if (input_bits > std::numeric_limits<long double>::digits)
        throw std::invalid_argument("this host cannot reconstruct the original event component precision");
    auto component = [input_bits](const std::string& text) {
        if (input_bits <= std::numeric_limits<double>::digits)
            return precise::wide_real(std::stod(text));
        return precise::wide_real(std::stold(text));
    };
    std::ifstream points(prefix.string() + "-points.csv"), values(prefix.string() + "-values.csv");
    std::ofstream evidence(prefix.string() + "-precision.tsv");
    if (!points || !values || !evidence)
        throw std::runtime_error("could not open formula comparison and precision files");
    std::string point_line, rate_line;
    std::getline(points, point_line);
    std::getline(values, rate_line);
    evidence << "point\traw_max\tunchanged_physical_mg\tunchanged_spurious_mg\tunchanged_collected_mg"
                "\tunchanged_physical_spurious\tunchanged_ordered_spurious\tunchanged_collected_physical"
                "\tprojected_physical_spurious\tprojected_ordered_spurious\tprojected_collected_physical"
                "\tprojected_kinematic_shift\n" << std::setprecision(17);
    all_positive_reference formulas(vertices, n, expanded_poles);
    hepcat::validation::parameters inputs;
    hepcat::validation::wide_parameters exact;
    const scalar mass(m, precise::bits), charge(charge_value, precise::bits);
    for (const auto& p : vertices.variables()) {
        if (p.name == "EE" || p.name == "Me") {
            auto key = vertices.algebra().format(p.symbol);
            auto val = p.name == "EE" ? charge : mass;
            inputs[key] = precise::narrow_real(val);
            exact.emplace(key, precise::number(val));
        }
    }
    std::vector<hepcat::validation::expression_evaluator> compiled;
    hepcat::validation::point labels;
    labels.momenta.resize(n + 2);
    auto coefficients = [&](const precise::phase_point& chart) {
        std::array<precise::number, 4> result;
        for (std::size_t i = 0; i < result.size(); ++i) result[i] = compiled[i].precise(labels, chart);
        return result;
    };
    formula_precision_report report;
    size_t rows = 0, previous = 0;
    while (std::getline(values, rate_line)) {
        ++rows;
        auto r = csv_row(rate_line);
        if (r.size() != 8 && r.size() != 11) throw std::runtime_error("invalid formula validation row");
        const auto event = std::stoull(r[0]);
        if (event <= previous || event > requested || (!sparse && event != rows))
            throw std::runtime_error("invalid formula validation event index");
        previous = event;
        long double worst = 0;
        for (size_t j = 4; j < r.size(); ++j) {
            if (j == 8) continue; // The collected rate, followed by its two residuals.
            auto err = std::stold(r[j]);
            if (!std::isfinite(err) || err < 0) throw std::runtime_error("invalid formula residual");
            worst = std::max(worst, err);
        }
        if (sparse && worst <= tolerance) throw std::runtime_error("non-outlier in sparse precision file");
        std::vector<precise::four> p;
        for (unsigned i = 0; i < n + 2; ++i) {
            if (!std::getline(points, point_line)) throw std::runtime_error("missing phase-space point");
            if (worst <= tolerance) continue;
            auto v = csv_row(point_line);
            if (v.size() != 6 || std::stoull(v[0]) != event || std::stoul(v[1]) != i + 1)
                throw std::runtime_error("invalid replay leg");
            precise::four momentum;
            for (unsigned k = 0; k < 4; ++k) momentum[k] = component(v[k+2]);
            p.push_back(std::move(momentum));
        }
        if (worst <= tolerance) continue;
        ++report.replayed;
        if (compiled.empty())
            for (auto id : {formulas.coefficient(false), formulas.coefficient(true),
                            formulas.coefficient(true, true), formulas.coefficient(false, false, true)})
                compiled.emplace_back(formulas.arena(), id, inputs, std::vector<int>{}, &exact);
        auto same = unchanged_chart(p, mass);
        auto u = coefficients(same);
        auto spin = precise::number(scalar(2 * same.scalar_products[1], precise::bits));
        precise::number mg(scalar(r[3], precise::bits));
        auto rate_error = [&](const precise::number& z) { return relative(z * z.conj() * spin, mg); };
        std::array<long double, 6> unchanged{rate_error(u[0]), rate_error(u[1]), rate_error(u[3]),
               relative(u[0], u[1]), relative(u[2], u[1]), relative(u[3], u[0])};
        report.worst_unchanged_mg = std::max({report.worst_unchanged_mg, unchanged[0], unchanged[1], unchanged[2]});
        if (*std::max_element(unchanged.begin(), unchanged.end()) > tolerance) ++report.unchanged_failures;
        long double shift = 0;
        auto proj = coefficients(replay_chart(std::move(p), mass, shift));
        if (shift > 64 * n * std::numeric_limits<double>::epsilon())
            throw std::runtime_error("formula replay would substantially change the measured momenta");
        report.largest_shift = std::max(report.largest_shift, shift);
        std::array<long double, 3> identities{relative(proj[0], proj[1]), relative(proj[2], proj[1]),
                                               relative(proj[3], proj[0])};
        auto max_id = *std::max_element(identities.begin(), identities.end());
        report.worst_projected_identity = std::max(report.worst_projected_identity, max_id);
        if (max_id > tolerance) ++report.projected_identity_failures;
        evidence << event << '\t' << worst;
        for (auto d : unchanged) evidence << '\t' << d;
        for (auto d : identities) evidence << '\t' << d;
        evidence << '\t' << shift << '\n';
    }
    if ((!sparse && rows != requested) || !report.replayed || std::getline(points, point_line))
        throw std::runtime_error("incomplete extended formula replay: " + prefix.string());
    return report;
}
} // namespace qed_notebook
