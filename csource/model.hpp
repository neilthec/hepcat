#pragma once
#include "amp_expression.hpp"
#include <cstdint>
#include <filesystem>
#include <memory>
#include <string>
#include <utility>
#include <vector>

namespace hepcat {
class Algebra;
class model;
struct model_particle {
    std::string description, name, antiname;
    int pdg = 0, spin2 = 0;
    std::string mass, width;
    int color = 1;
    std::string auxiliary, latex, anti_latex;
};
struct model_vertex {
    std::vector<std::string> legs;
    std::uint32_t factor, expression; // Node IDs in model::algebra().
    bool pending_constructive_derivation = false;
};
struct model_parameter {
    std::string name;
    std::uint32_t symbol, definition;
    std::string comment;
};

// Copies share an immutable table and interned expression arena.
class model {
  public:
    model(const std::filesystem::path& directory, unsigned number, std::string parameter_scope = {});
    static model read(const std::filesystem::path& directory, unsigned number) {
        return model(directory, number);
    }
    const std::vector<model_particle>& particles() const;
    const std::vector<model_vertex>& vertices() const;
    const std::vector<model_parameter>& variables() const;
    const std::vector<model_parameter>& functions() const;
    const model_particle& particle(const std::string& name) const;
    std::string antiparticle(const std::string& name) const;
    // Return the stored factor times expression, with legs numbered in process order.
    // An absent row gives the zero amplitude for these external particles. An ambiguous table or a Feynman model is an error.
    amp_expression retrieve_amplitude(const std::vector<std::string>& process) const;
    const Algebra& algebra() const;
    const std::string& parameter_scope() const;
    bool is_feynman() const;
    // Bind only active model-scoped atoms and their dependencies. A foreign
    // scope belongs to another model and is omitted from the returned points.
    using parameter_points = std::vector<std::vector<std::pair<std::string, std::string>>>;
    parameter_points high_energy_parameter_points(const std::vector<std::string>& active_parameters) const;
    // The second overload selects three consecutive deterministic model assignments.
    parameter_points high_energy_parameter_points(const std::vector<std::string>& active_parameters,
                                                  unsigned first_variant) const;

  private:
    struct data;
    std::shared_ptr<const data> data_;
};
std::uint32_t contract_model_couplings(Algebra&, std::uint32_t root, const model&);
} // namespace hepcat
