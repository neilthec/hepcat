#pragma once

#include "amp_expression.hpp"
#include "model.hpp"
#include "validation.hpp"

#include <cstddef>
#include <functional>
#include <optional>
#include <string>
#include <stdexcept>
#include <vector>
#include <utility>

namespace hepcat {

struct factorization_options {
    std::size_t workers = 0;
    std::vector<std::string> correction_basis;
    bool try_grouped_currents = false;
};

struct factorization_report {
    std::size_t channels = 0, candidates = 0, rank = 0, workers = 0;
    std::size_t training_samples = 0, validation_samples = 0;
    std::string route;
};

struct high_energy_options {
    // Zero selects the available hardware. The default fixed-angle bound
    // for a renormalizable amplitude is E^(4 - number of external legs).
    std::size_t workers = 0;
    std::optional<int> energy_bound;
    std::vector<std::string> correction_basis;
    // Exact scalar parameters and values at each point, in native input syntax.
    // Supply consistent assignments here when a model relates its parameters.
    std::vector<std::vector<std::pair<std::string,std::string>>> parameter_points;
};

struct high_energy_report {
    int bound = 0;
    // Missing on the unchanged fast path. Algebraically equivalent basis
    // directions also contribute to the kernel dimension of the sampled fit.
    std::optional<std::size_t> free_coefficients;
    std::size_t validation_spin_states = 0;
    bool exhaustive_spin = false;
    std::size_t workers = 0, candidates = 0, rank = 0;
    std::size_t training_samples = 0, validation_samples = 0;
    // Leaf counts include repeated occurrences of shared expression nodes.
    std::size_t nodes_before_final_reduction = 0, nodes_after_final_reduction = 0;
    std::string route;
};


struct construct_options {
    std::size_t workers = 0;
    // With no override, construct honors an explicit constructor policy.
    // A new amplitude with automatic references starts with physical poles.
    std::optional<generation_references> pole_preference;
    factorization_options factorization;
    high_energy_options high_energy;
    validation_options validation;
    std::optional<amp_expression> reference_amplitude;
    // Called on the calling thread, at stage boundaries.
    std::function<void(const std::string& stage)> progress;
};

struct construction_report {
    std::vector<std::pair<std::string, double>> seconds;
    double total_seconds = 0;
    validation_report validation;
};

class construction_validation_error : public std::runtime_error {
public:
    explicit construction_validation_error(validation_report checks);
    validation_report report;
};

struct amplitude_item {
    std::vector<numerator_shell> shells;
    amp_expression expression;

private:
    friend class amplitude;
    amplitude_item(const std::string& input, const std::vector<std::string>& masses,
                   std::vector<numerator_shell> conditions);
    std::size_t input_size_;
    bool reduced_ = false, generated_ = false;
};

// One process in all-incoming leg order. Each entry owns its algebra and its
// numerator assumptions. The vectors retain insertion order within each kind.
// Coordinate access to this object while a caller is modifying it.
class amplitude {
public:
    explicit amplitude(std::vector<std::string> external_masses);
    // A model handle shares its immutable parsed tables across processes and workers.
    amplitude(std::vector<std::string> particles, model vertex_model,
              generation_references references = generation_references::automatic);
    amplitude(std::vector<std::string> particles, model vertex_model, model factorization_model,
              generation_references references = generation_references::automatic);
    amplitude(std::vector<std::string> particles, model vertex_model, model factorization_model,
              model feynman_model, generation_references references = generation_references::automatic);
    // A separate CalcHEP model supplies the independent, off-shell diagrams.
    amplitude& set_feynman_model(model rules);
    amplitude& generate_feynman_diagrams(std::size_t workers = 0);
    amplitude& generate_diagrams(std::size_t workers = 0);
    amplitude& generate_factorization_products(std::size_t workers = 0);
    // Generate the collections for the supplied models in one worker queue.
    amplitude& generate(std::size_t workers = 0);

    void add_diagram(const std::string& expression, std::vector<numerator_shell> shells = {});
    void add_factorization_product(const std::string& expression, numerator_shell channel);

    void add_prepared_diagram(const std::string& expression, std::vector<numerator_shell> shells = {},
                              const std::string& remainder = "0");
    void add_prepared_factorization_product(const std::string& expression, numerator_shell channel,
                                            const std::string& remainder = "0");

    // Uses all supplied channel rows in HEPCAT sewing conventions. The returned
    // expression belongs to this process until its next successful modification.
    // Throws if a physical cut cannot be sampled or the fit does not validate.
    const amp_expression& complete_factorization(std::size_t workers = 0);
    const amp_expression& complete_factorization(const factorization_options& options);
    const factorization_report& last_factorization() const;

    // Complete the factorized amplitude, or apply the high-energy stage alone
    // to a supplied seed. Only local corrections can change its growth.
    const amp_expression& complete_high_energy(std::size_t workers = 0);
    const amp_expression& complete_high_energy(const high_energy_options& options);
    const amp_expression& complete_high_energy(const amp_expression& seed, const high_energy_options& options = {});
    const high_energy_report& last_high_energy() const;

    // The externally reduced expression from the last successful high-energy completion.
    const amp_expression& final_amplitude() const;
    // Publish only when generation, both completions and independent checks succeed.
    const amp_expression& construct(const construct_options& options = {});
    const construction_report& last_construction() const;
    validation_report validate_against(const amp_expression& reference, const validation_options& options = {});
    validation_report validate(const validation_options& options = {});
    validation_report validate(const amp_expression& candidate, const validation_options& options = {});

    // Zero workers selects the hardware concurrency. One runs on the caller.
    // A successful call publishes all reduced expressions and their remainders
    // together. Later calls reduce only entries appended since the last call.
    amplitude& reduce(std::size_t workers = 0);

    const std::vector<std::string>& particles() const { return particles_; }
    const std::vector<std::string>& external_masses() const { return masses_; }
    const std::vector<amplitude_item>& diagrams() const { return diagrams_; }
    bool factorization_products_generated() const { return channels_generated_; }
    const std::vector<amplitude_item>& factorization_products() const { return products_; }
    const std::vector<amplitude_item>& feynman_diagrams() const { return feynman_rows_; }
    std::size_t last_worker_count() const { return last_workers_; }

private:
    const amp_expression& complete_energy_components(const amp_expression&, const high_energy_options&);
    bool complete_color_rows(const factorization_options*, const high_energy_options*, const amp_expression* seed);
    validation_report validate_impl(amp_expression candidate, const amp_expression* original,
                                    const validation_options&, const amp_expression* reference = nullptr);
    struct no_grouped_current {};
    const amp_expression& complete_factorization_impl(const factorization_options&, bool grouped_only);
    amplitude& generate_rows(bool diagrams, bool products, bool feynman, std::size_t workers,
                             bool search = true);
    void bind_model_parameters(Algebra&, std::uint32_t seed, high_energy_options&, bool include_targets = false) const;
    std::vector<std::string> masses_, particles_;
    std::optional<model> vertices_model_, channels_model_, feynman_model_;
    generation_references references_ = generation_references::automatic;
    bool diagrams_generated_ = false, channels_generated_ = false, feynman_generated_ = false;
    std::vector<amplitude_item> diagrams_, products_, feynman_rows_;
    std::size_t last_workers_ = 0;
    std::optional<amp_expression> completed_;
    factorization_report report_;
    std::optional<amp_expression> high_energy_result_, before_final_reduction_;
    high_energy_report high_energy_report_;
    std::optional<construction_report> construction_;
};

} // namespace hepcat
