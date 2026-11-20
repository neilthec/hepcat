#pragma once

#include <cstdint>
#include <deque>
#include <map>
#include <optional>
#include <string>
#include <vector>
#include <unordered_map>
#include <gmpxx.h>

namespace hepcat {

using Id = std::uint32_t;

struct Node {
    std::string head;
    std::vector<Id> args;
    bool compound = false;
    bool operator==(const Node& other) const {
        return head == other.head && args == other.args && compound == other.compound;
    }
};

class Algebra {
public:
    explicit Algebra(const Algebra* parent = nullptr) : parent_(parent), base_(parent ? parent->size() : 0) {}
    Algebra(const Algebra&) = delete;
    const Node& node(Id id) const { return id < base_ ? parent_->node(id) : nodes_.at(id-base_); }
    std::size_t size() const { return base_ + nodes_.size(); }
    std::size_t cost(Id id) const { return id < base_ ? parent_->cost(id) : costs_.at(id-base_); }
    Id raw(std::string head, std::vector<Id> args = {}, bool compound = false);
    Id atom(std::string text) { return raw(std::move(text)); }
    Id integer(long value) { return atom(std::to_string(value)); }
    Id call(std::string head, std::vector<Id> args);
    Id add(std::vector<Id> terms);
    Id mul(std::vector<Id> factors);
    Id pow(Id base, long exponent);
    int leg(Id id) const;
    const std::vector<Id>& masses() const { return masses_; }
    const std::map<std::vector<int>,Id>& shells() const { return shells_; }
    void setKinematics(std::vector<Id> masses, std::map<std::vector<int>,Id> shells);
    Id parse(const std::string& expression);
    std::string format(Id expression) const;
    bool number(Id id, mpq_class& result) const;
    Id exact(const mpq_class& value);
    Id simplify(Id expression);
private:
    const Algebra* parent_;
    size_t base_;
    Id find(size_t hash, const Node& n) const;
    std::deque<Node> nodes_;
    std::vector<std::size_t> costs_;
    std::unordered_multimap<std::size_t,Id> index_;
    std::vector<Id> masses_;
    std::map<std::vector<int>,Id> shells_;
    Id factorSum(Id expression);
};

Id reduceInto(Algebra& storage, Id expression, const std::vector<Id>& masses,
              const std::map<std::vector<int>,Id>& shells);
Id collectCandidate(Algebra&, Id);
Id collect_scalar_factors(Algebra&, Id);
Id compact_commuting_products(Algebra&, Id);
std::size_t expression_complexity(const Algebra&, Id);
Id reduceSpinor(Algebra&, Id);
std::optional<Id> commonMomentumSwap(Algebra&, Id first, Id second);
Id reduceKinematics(Algebra&, Id, bool useInternal = true);

} // namespace hepcat
