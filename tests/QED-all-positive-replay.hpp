// Re-evaluate every raw all-positive outlier using the same measured momenta.
// The comparison retains the original MadGraph rate and all unrounded residuals.
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
inline std::size_t replay_formulas(const hepcat::model& vertices, unsigned n,
                                   const std::filesystem::path& prefix, size_t requested, double m,
                                   long double tolerance = 1e-7L) {
    std::ifstream points(prefix.string() + "-points.csv"), values(prefix.string() + "-values.csv");
    std::ofstream evidence(prefix.string() + "-precision.tsv");
    if (!points || !values || !evidence)
        throw std::runtime_error("could not open formula comparison and precision files");
    std::string point_line, rate_line;
    std::getline(points, point_line);
    std::getline(values, rate_line);
    evidence
        << "point\traw_max\tphysical_mg\tspurious_mg\tcomplex_forms\tordered_current\tkinematic_shift\n"
        << std::setprecision(17);
    all_positive_reference formulas(vertices, n);
    hepcat::validation::parameters inputs;
    hepcat::validation::wide_parameters exact;
    const scalar mass(m, precise::bits), charge(mpq_class(1, 3), precise::bits);
    for (const auto& p : vertices.variables()) {
        auto key = vertices.algebra().format(p.symbol);
        if (p.name == "EE" || p.name == "Me") {
            auto val = p.name == "EE" ? charge : mass;
            inputs[key] = precise::narrow_real(val);
            exact.emplace(key, precise::number(val));
        }
    }
    std::vector<hepcat::validation::expression_evaluator> compiled;
    size_t row = 0, replayed = 0, failed = 0;
    while (std::getline(values, rate_line)) {
        ++row;
        auto r = csv_row(rate_line);
        if (r.size() != 8 || std::stoull(r[0]) != row)
            throw std::runtime_error("invalid formula validation row");
        long double worst = 0;
        for (size_t j = 4; j < 8; ++j) {
            auto err = std::stold(r[j]);
            if (!std::isfinite(err))
                throw std::runtime_error("nonfinite formula residual");
            worst = std::max(worst, err);
        }
        std::vector<precise::four> p;
        for (unsigned i = 0; i < n + 2; ++i) {
            if (!std::getline(points, point_line))
                throw std::runtime_error("missing phase-space point");
            if (worst <= tolerance)
                continue;
            auto v = csv_row(point_line);
            if (v.size() != 6 || std::stoull(v[0]) != row || std::stoul(v[1]) != i + 1)
                throw std::runtime_error("invalid replay leg");
            p.push_back({scalar(v[2], precise::bits), scalar(v[3], precise::bits),
                         scalar(v[4], precise::bits), scalar(v[5], precise::bits)});
        }
        if (worst <= tolerance)
            continue;
        ++replayed;
        if (compiled.empty()) {
            auto physical = formulas.coefficient(false), spurious = formulas.coefficient(true),
                 ordered = formulas.coefficient(true, true);
            for (auto id : {physical, spurious, ordered})
                compiled.emplace_back(formulas.arena(), id, inputs, std::vector<int>{}, &exact);
        }
        long double shift = 0;
        auto chart = replay_chart(std::move(p), mass, shift);
        // The chart must still describe this recorded MadGraph point. Its
        // kinematic roundoff grows with the number of final-state momenta.
        if (shift > 64 * n * std::numeric_limits<double>::epsilon())
            throw std::runtime_error("formula replay would substantially change the recorded momenta");
        hepcat::validation::point labels;
        labels.momenta.resize(n + 2);
        auto physical = compiled[0].precise(labels, chart),
             spurious = compiled[1].precise(labels, chart), ordered = compiled[2].precise(labels, chart);
        // Sum over both massive spin indices of |<12>|^2 = 2 p1.p2.
        precise::number spin(scalar(2 * chart.scalar_products[1], precise::bits)),
            mg(scalar(r[3], precise::bits));
        auto pm = relative(physical * physical.conj() * spin, mg),
             sm = relative(spurious * spurious.conj() * spin, mg);
        auto cm = relative(physical, spurious), om = relative(ordered, spurious);
        if (std::max({pm, sm, cm, om}) > tolerance)
            ++failed;
        evidence << row << '\t' << worst << '\t' << pm << '\t' << sm << '\t' << cm << '\t' << om << '\t'
                 << shift << '\n';
    }
    if (row != requested || !replayed || std::getline(points, point_line) || failed)
        throw std::runtime_error("incomplete or failed same-point extended formula replay: " +
                                 prefix.string());
    return replayed;
}
} // namespace qed_notebook
