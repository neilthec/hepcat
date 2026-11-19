#pragma once

#include "amp_expression.hpp"

#include <cstddef>
#include <optional>
#include <string>
#include <vector>

namespace hepcat {

struct factorization_options {
    std::size_t workers = 0;
    std::vector<std::string> correction_basis;
};

struct factorization_report {
    std::size_t channels = 0, candidates = 0, rank = 0, workers = 0;
    std::size_t training_samples = 0, validation_samples = 0;
    std::string route;
};

struct amplitude_item {
    std::vector<numerator_shell> shells;
    amp_expression expression;

private:
    friend class amplitude;
    amplitude_item(const std::string& input, const std::vector<std::string>& masses,
                   std::vector<numerator_shell> conditions);
    std::size_t input_size_;
    bool reduced_ = false;
};

// One process in all-incoming leg order. Each entry owns its algebra and its
// numerator assumptions. The vectors retain insertion order within each kind.
// Coordinate access to this object while a caller is modifying it.
class amplitude {
public:
    explicit amplitude(std::vector<std::string> external_masses);
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

    // Zero workers selects the hardware concurrency. One runs on the caller.
    // A successful call publishes all reduced expressions and their remainders
    // together. Later calls reduce only entries appended since the last call.
    amplitude& reduce(std::size_t workers = 0);

    const std::vector<std::string>& external_masses() const { return masses_; }
    const std::vector<amplitude_item>& diagrams() const { return diagrams_; }
    const std::vector<amplitude_item>& factorization_products() const { return products_; }
    std::size_t last_worker_count() const { return last_workers_; }

private:
    std::vector<std::string> masses_;
    std::vector<amplitude_item> diagrams_, products_;
    std::size_t last_workers_ = 0;
    std::optional<amp_expression> completed_;
    factorization_report report_;
};

} // namespace hepcat
