#include "vector_basis.hpp"

#include <algorithm>
#include <cmath>
#include <numeric>
#include <tuple>

namespace hepcat {
namespace {
using Vector = std::vector<double>;
using Row = std::map<std::size_t, mpq_class>;

// Sparse Gaussian elimination with the proposed zero coordinates first. Any
// row added to the amplitude is an exact combination of input identities.
// A poor or near-singular floating-point proposal can only affect the score.
void subtractMultiple(Row& target, const Row& row, mpq_class multiplier) {
    for (const auto& [column, value] : row) {
        auto& entry = target[column];
        entry -= multiplier * value;
        if (entry == 0) target.erase(column);
    }
}

struct SupportSearch {
    const TensorCoordinates& source;
    const std::vector<TensorCoordinates>& identities;
    std::vector<Id> keys;
    std::map<Id, std::size_t> index;
    Vector weight;
    TensorCoordinates best;
    std::pair<std::size_t, std::size_t> bestRank;

    std::pair<std::size_t, std::size_t> rank(const TensorCoordinates& form) const {
        std::size_t sum = 0;
        for (const auto& [key, coefficient] : form) if (coefficient != 0)
            sum += static_cast<std::size_t>(weight[index.at(key)]);
        return {sum, form.size()};
    }

    void reconstruct(const Vector& proposal) {
        std::vector<std::size_t> order(keys.size());
        std::iota(order.begin(), order.end(), 0);
        // Prioritize expensive terms within the floating-point zero group.
        auto priority = [&](std::size_t i) {
            const double value = std::abs(proposal[i]);
            return std::make_tuple(value < 1.e-6 ? 0. : value, -weight[i], i);
        };
        std::sort(order.begin(), order.end(), [&](auto i, auto j) { return priority(i) < priority(j); });
        std::vector<std::size_t> column(keys.size());
        for (std::size_t i=0; i<order.size(); ++i) column[order[i]] = i;
        auto exactRow = [&](const TensorCoordinates& coefficients) {
            Row row;
            for (const auto& [key, value] : coefficients) if (value != 0)
                row.emplace(column[index.at(key)], value);
            return row;
        };
        std::map<std::size_t, Row> pivots;
        for (const auto& identity : identities) {
            Row row = exactRow(identity);
            while (!row.empty()) {
                auto first = row.begin();
                auto pivot = pivots.find(first->first);
                if (pivot == pivots.end()) {
                    const mpq_class scale = first->second;
                    if (scale != 1) for (auto& term : row) term.second /= scale;
                    const auto columnId = first->first;
                    pivots.emplace(columnId, std::move(row));
                    break;
                }
                subtractMultiple(row, pivot->second, first->second);
            }
        }
        Row reduced = exactRow(source);
        for (const auto& [pivot, identity] : pivots) {
            auto term = reduced.find(pivot);
            if (term != reduced.end()) subtractMultiple(reduced, identity, term->second);
        }
        TensorCoordinates candidate;
        for (const auto& [position, value] : reduced) candidate.emplace(keys[order[position]], value);
        const auto proposedRank = rank(candidate);
        if (proposedRank < bestRank) { best = std::move(candidate); bestRank = proposedRank; }
    }

    TensorCoordinates run() {
        best = source;
        bestRank = rank(source);
        Vector input(keys.size());
        double scale = 0.;
        for (const auto& [id, value] : source) {
            input[index.at(id)] = value.get_d();
            scale = std::max(scale, std::abs(value.get_d()));
        }
        if (!(scale > 0.) || !std::isfinite(scale)) return source;
        for (auto& value : input) value /= scale;
        // The exact source support often already identifies good pivots.
        reconstruct(input);
        if (best.empty()) return best;
        // An orthonormal basis gives the Euclidean projection onto the identity
        // span. Cap the search before its dense proposal exceeds the budget.
        std::vector<Vector> basis;
        const std::size_t dimension = keys.size();
        for (const auto& identity : identities) {
            Vector direction(dimension);
            double largest = 0.;
            for (const auto& [id, value] : identity) {
                direction[index.at(id)] = value.get_d();
                largest = std::max(largest, std::abs(value.get_d()));
            }
            if (!(largest > 0.) || !std::isfinite(largest)) continue;
            for (auto& value : direction) value /= largest;
            for (const auto& q : basis) {
                double dot = std::inner_product(q.begin(), q.end(), direction.begin(), 0.);
                for (std::size_t i=0; i<dimension; ++i) direction[i] -= dot * q[i];
            }
            double norm = std::sqrt(std::inner_product(direction.begin(), direction.end(), direction.begin(), 0.));
            if (norm < 1.e-8) continue;
            for (auto& value : direction) value /= norm;
            basis.push_back(std::move(direction));
            if (basis.size() == dimension) break;
        }
        const double maxWeight = *std::max_element(weight.begin(), weight.end());
        Vector z=input, dual(dimension), argument(dimension), affine(dimension);
        // ADMM minimizes sum_i w_i |z_i| on the affine identity space. Here it
        // only proposes the monomials to remove. Exact elimination certifies
        // every accepted change even if the numerical iteration is incomplete.
        for (int iteration=1; iteration<=120; ++iteration) {
            for (std::size_t i=0; i<dimension; ++i) argument[i] = z[i] - dual[i] - input[i];
            affine=input;
            for (const auto& q : basis) {
                double dot = std::inner_product(q.begin(), q.end(), argument.begin(), 0.);
                for (std::size_t i=0; i<dimension; ++i) affine[i] += dot*q[i];
            }
            for (std::size_t i=0; i<dimension; ++i) {
                double value = affine[i] + dual[i];
                double threshold = 0.2 * weight[i] / maxWeight;
                z[i] = std::copysign(std::max(0., std::abs(value)-threshold), value);
                dual[i] += affine[i] - z[i];
            }
            if (iteration == 30 || iteration == 120) reconstruct(z);
        }
        return best;
    }
};
} // namespace

TensorCoordinates reduceTensorSupport(const TensorCoordinates& source,
        const std::vector<TensorCoordinates>& identities, const std::map<Id, std::size_t>& weights) {
    if (source.empty() || identities.empty() || identities.size() > 2400) return source;
    SupportSearch search{source, identities, {}, {}, {}, source, {0, 0}};
    for (const auto& term : source) search.index.emplace(term.first, 0);
    for (const auto& row : identities) for (const auto& term : row) search.index.emplace(term.first, 0);
    if (search.index.size() > 800 || search.index.size() * search.index.size() * identities.size() > 600000000)
        return source;
    for (auto& [id, i] : search.index) {
        i = search.keys.size(); search.keys.push_back(id);
        auto it = weights.find(id);
        search.weight.push_back(it == weights.end() ? 1. : static_cast<double>(std::max<std::size_t>(1, it->second)));
    }
    return search.run();
}
} // namespace hepcat
