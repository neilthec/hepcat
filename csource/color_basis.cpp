#include "color_basis.hpp"
#include "chart_scalar.hpp"
#include "validation_color.hpp"

#include <algorithm>
#include <map>

namespace hepcat {
namespace {
using chart_detail::Scalar;

Id scalar_expression(Algebra& a, const Scalar& x) {
    if (x.c[1] || x.c[3])
        throw std::logic_error("unexpected radical in SU(3) decomposition");
    return a.add({a.exact(x.c[0]), a.mul({a.atom("System`I"), a.exact(x.c[2])})});
}

// Contract internal delta lines before choosing the basis. A closed trace still
// belongs to the projector, which knows the dimension of the representation.
std::vector<Id> contract_deltas(Algebra& a, std::vector<Id> tensors) {
    for (std::size_t i = 0; i < tensors.size();) {
        auto delta = a.node(tensors[i]);
        if (validation_detail::short_head(delta) != "ColorDelta" || delta.args.size() != 2 ||
            delta.args[0] == delta.args[1]) {
            ++i;
            continue;
        }
        Id from = delta.args[0], to = delta.args[1];
        if (validation_detail::short_head(a.node(from)) == "ColorIndex")
            std::swap(from, to);
        if (validation_detail::short_head(a.node(from)) == "ColorIndex") {
            ++i;
            continue;
        }
        std::size_t occurrences = 0;
        for (std::size_t j = 0; j < tensors.size(); ++j)
            if (i != j) {
                const auto& args = a.node(tensors[j]).args;
                occurrences += std::count(args.begin(), args.end(), from);
            }
        if (occurrences != 1) {
            ++i;
            continue;
        }
        for (std::size_t j = 0; j < tensors.size(); ++j)
            if (i != j) {
                auto n = a.node(tensors[j]);
                std::replace(n.args.begin(), n.args.end(), from, to);
                tensors[j] = a.call(n.head, n.args);
            }
        tensors.erase(tensors.begin() + i);
        i = 0;
    }
    return tensors;
}
} // namespace

color_decomposition decompose_color(Algebra& a, const std::vector<int>& reps, const std::vector<Id>& roots) {
    validation_detail::color_polynomial split(a);
    std::vector<std::map<Id, Id>> terms;
    std::set<Id> inventory;
    const Id zero = a.integer(0);
    for (Id root : roots) {
        auto& grouped = terms.emplace_back();
        for (auto term : split.split(root)) {
            Id coefficient = a.mul(std::move(term.coefficients));
            if (coefficient == zero)
                continue;
            Id tensor = a.mul(term.tensors);
            inventory.insert(a.mul(contract_deltas(a, std::move(term.tensors))));
            inventory.insert(tensor);
            auto [it, inserted] = grouped.emplace(tensor, coefficient);
            if (!inserted)
                it->second = a.add({it->second, coefficient});
        }
    }
    std::vector<Id> tensors(inventory.begin(), inventory.end());
    std::stable_sort(tensors.begin(), tensors.end(), [&](Id x, Id y) { return a.cost(x) < a.cost(y); });
    auto components = validation_detail::fundamental_color_projections(a, reps).project(tensors);
    // Row-reduce exact tensor values. In reduced echelon form every pivot column
    // is a physical basis tensor, and its row gives its coefficient in every input tensor.
    chart_detail::Parameters numbers(a, 0);
    std::vector<std::vector<Scalar>> matrix;
    for (const auto& component : components) {
        auto& row = matrix.emplace_back();
        for (Id entry : component)
            row.push_back(numbers.value(entry));
    }
    color_decomposition result;
    for (std::size_t col = 0; col < tensors.size(); ++col) {
        const auto rank = result.tensors.size();
        std::size_t pivot = rank;
        while (pivot < matrix.size() && matrix[pivot][col].zero())
            ++pivot;
        if (pivot == matrix.size())
            continue;
        std::swap(matrix[pivot], matrix[rank]);
        auto scale = inverse(matrix[rank][col]);
        for (auto& value : matrix[rank])
            value = value * scale;
        for (std::size_t i = 0; i < matrix.size(); ++i) {
            if (i == rank || matrix[i][col].zero())
                continue;
            auto multiplier = Scalar(-1) * matrix[i][col];
            for (std::size_t j = col; j < tensors.size(); ++j)
                matrix[i][j] = matrix[i][j] + multiplier * matrix[rank][j];
        }
        result.tensors.push_back(tensors[col]);
    }
    for (std::size_t i = 0; i < result.tensors.size(); ++i) {
        std::map<Id, Id> weights;
        for (std::size_t j = 0; j < tensors.size(); ++j)
            weights.emplace(tensors[j], scalar_expression(a, matrix[i][j]));
        auto& values = result.coefficients.emplace_back();
        for (const auto& grouped : terms) {
            std::vector<Id> sum;
            for (const auto& [tensor, coefficient] : grouped)
                sum.push_back(a.mul({weights.at(tensor), coefficient}));
            values.push_back(a.add(std::move(sum)));
        }
    }
    return result;
}
} // namespace hepcat
