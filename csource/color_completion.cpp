#include "amplitude.hpp"
#include "color_basis.hpp"
#include "validation_color_polynomial.hpp"

namespace hepcat {

bool amplitude::complete_color_rows(const factorization_options* factor, const high_energy_options* energy,
                                    const amp_expression* seed) {
    if (!vertices_model_ || vertices_model_->is_feynman())
        return false;
    std::vector<int> colors;
    for (const auto& name : particles_) {
        const auto& p = vertices_model_->particle(name);
        colors.push_back(p.color == 3 && name == p.antiname ? -3 : p.color);
    }
    if (std::all_of(colors.begin(), colors.end(), [](int c) { return std::abs(c) == 1; }))
        return false;
    Algebra a;
    std::vector<Id> masses;
    for (const auto& m : masses_)
        masses.push_back(a.parse(m));
    a.setKinematics(std::move(masses), {});
    if (seed && seed->import_masses(a) != a.masses())
        throw std::invalid_argument("high-energy input has different external masses");

    std::vector<Id> roots;
    for (const auto* rows : {&diagrams_, &products_})
        for (const auto& row : *rows) {
            roots.push_back(row.expression.import_into(a));
            roots.push_back(row.expression.import_into(a, true));
        }
    if (seed)
        roots.push_back(seed->import_into(a));
    const auto& corrections = factor ? factor->correction_basis : energy->correction_basis;
    for (const auto& correction : corrections)
        roots.push_back(a.parse(correction));
    validation_detail::color_polynomial pieces(a);
    if (std::none_of(roots.begin(), roots.end(), [&](Id x) { return pieces.contains_color(x); }))
        return false;

    auto decomposition = decompose_color(a, colors, roots);
    std::vector<Id> answers, originals;
    factorization_report fr;
    high_energy_report hr;
    fr.route = hr.route = "ColorCoefficients";
    hr.bound = energy && energy->energy_bound ? *energy->energy_bound : 4 - int(masses_.size());
    hr.exhaustive_spin = true;
    const Id zero = a.integer(0);
    for (std::size_t i = 0; i < decomposition.tensors.size(); ++i) {
        const auto& c = decomposition.coefficients[i];
        amplitude partial(masses_);
        partial.vertices_model_ = vertices_model_;
        partial.channels_model_ = channels_model_;
        partial.particles_ = particles_;
        partial.references_ = references_;
        partial.diagrams_generated_ = partial.channels_generated_ = true;
        std::size_t pos = 0;
        for (const auto& row : diagrams_) {
            if (c[pos] != zero || c[pos + 1] != zero)
                partial.add_prepared_diagram(a.format(c[pos]), row.shells, a.format(c[pos + 1]));
            pos += 2;
        }
        for (const auto& row : products_) {
            partial.add_prepared_factorization_product(a.format(c[pos]), row.shells.at(0),
                                                       a.format(c[pos + 1]));
            pos += 2;
        }
        Id scalar_seed = seed ? c[pos++] : zero;
        std::vector<std::string> scalar_corrections;
        for (; pos < c.size(); ++pos)
            if (c[pos] != zero)
                scalar_corrections.push_back(a.format(c[pos]));
        if (factor) {
            auto opts = *factor;
            opts.correction_basis = std::move(scalar_corrections);
            auto& result = partial.complete_factorization(opts);
            answers.push_back(a.mul({decomposition.tensors[i], result.import_into(a)}));
            const auto& part = partial.last_factorization();
            fr.channels = std::max(fr.channels, part.channels);
            fr.workers = std::max(fr.workers, part.workers);
            fr.candidates += part.candidates;
            fr.rank += part.rank;
            fr.training_samples += part.training_samples;
            fr.validation_samples += part.validation_samples;
        } else {
            auto opts = *energy;
            opts.correction_basis = std::move(scalar_corrections);
            amp_expression input("0", masses_);
            input.set_from(a, scalar_seed);
            const auto& result = partial.complete_energy_components(input, opts);
            answers.push_back(a.mul({decomposition.tensors[i], result.import_into(a)}));
            originals.push_back(
                a.mul({decomposition.tensors[i], partial.before_final_reduction_->import_into(a)}));
            const auto& part = partial.last_high_energy();
            hr.workers = std::max(hr.workers, part.workers);
            hr.validation_spin_states = std::max(hr.validation_spin_states, part.validation_spin_states);
            hr.exhaustive_spin &= part.exhaustive_spin;
            hr.candidates += part.candidates;
            hr.rank += part.rank;
            hr.validation_samples += part.validation_samples;
            hr.training_samples += part.training_samples;
            if (part.free_coefficients)
                hr.free_coefficients = hr.free_coefficients.value_or(0) + *part.free_coefficients;
        }
    }
    amp_expression result("0", masses_);
    result.set_from(a, a.add(std::move(answers)));
    construction_.reset();
    if (factor) {
        completed_ = std::move(result);
        high_energy_result_.reset();
        before_final_reduction_.reset();
        report_ = std::move(fr);
    } else {
        amp_expression before("0", masses_);
        before.set_from(a, a.add(std::move(originals)));
        hr.nodes_before_final_reduction = before.leaf_count();
        hr.nodes_after_final_reduction = result.leaf_count();
        before_final_reduction_ = std::move(before);
        high_energy_result_ = std::move(result);
        high_energy_report_ = std::move(hr);
    }
    return true;
}
} // namespace hepcat
