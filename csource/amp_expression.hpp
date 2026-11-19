#pragma once

#include <cstddef>
#include <cstdint>
#include <memory>
#include <optional>
#include <string>
#include <vector>

namespace hepcat {

class Algebra;

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
private:
    friend class amplitude;
    void restore_prepared(const std::string& remainder);
    std::uint32_t import_into(Algebra& destination, bool remainder = false) const;
    void set_from(const Algebra& source, std::uint32_t result);
    struct context;
    std::shared_ptr<const context> kinematics_;
    std::shared_ptr<Algebra> arena_;
    std::uint32_t root_;
    bool reduced_ = false;
    std::optional<std::uint32_t> remainder_;
};

} // namespace hepcat
