#pragma once

#include <cstddef>
#include <cstdint>
#include <memory>
#include <optional>
#include <string>
#include <vector>

namespace hepcat {

class Algebra;
class amp_expression;
namespace validation_detail {
std::uint32_t import_expression(Algebra&, const amp_expression&, bool remainder = false);
}

struct numerator_shell {
    std::vector<int> legs;
    std::string mass;
};

// FullForm input preserves model-local symbolic masses. External particle
// labels are 1 through masses.size(), in the all-incoming convention.
class amp_expression {
public:
    amp_expression(const std::string& expression,
                   const std::vector<std::string>& masses,
                   const std::vector<numerator_shell>& shells = {});
    amp_expression& reduce();
    // Evaluated with external kinematics only. Adding this to the current
    // expression recovers the input away from the cut.
    std::optional<amp_expression> discarded_remainder() const;
    std::string str() const;
    std::size_t particle_count() const;
    std::size_t complexity() const;
    std::size_t leaf_count() const;
private:
    friend class amplitude;
    friend std::uint32_t validation_detail::import_expression(Algebra&, const amp_expression&, bool);
    void restore_prepared(const std::string& remainder);
    std::uint32_t import_into(Algebra& destination, bool remainder = false) const;
    std::vector<std::uint32_t> import_masses(Algebra& destination) const;
    void reduce_from(Algebra& worker, std::uint32_t source, std::optional<std::uint32_t> original = {});
    void set_from(const Algebra& source, std::uint32_t result);
    struct context;
    std::shared_ptr<const context> kinematics_;
    std::shared_ptr<Algebra> arena_;
    std::uint32_t root_;
    bool reduced_ = false;
    std::optional<std::uint32_t> remainder_;
};

} // namespace hepcat
