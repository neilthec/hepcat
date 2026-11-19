#pragma once
#include <gmpxx.h>
#include <atomic>
#include <exception>
#include <map>
#include <thread>
#include <vector>

namespace hepcat::fit_detail {
class sparse_system {
    using Q = mpq_class;
    using Sparse = std::map<std::size_t, Q>;
    std::map<std::size_t, Sparse> pivots;
    std::size_t dimension;

  public:
    explicit sparse_system(std::size_t columns) : dimension(columns) {}
    bool add(const std::vector<Q>& values, std::size_t first, Q rhs) {
        Sparse row;
        for (std::size_t i = 0; i < dimension; ++i)
            if (values[i + first] != 0) row[i] = values[i + first];
        if (rhs != 0) row.emplace(dimension, std::move(rhs));
        while (!row.empty()) {
            auto col = row.begin()->first;
            if (col == dimension) return false;
            auto it = pivots.find(col);
            Q scale = row.begin()->second;
            if (it == pivots.end()) {
                for (auto& x : row)
                    x.second /= scale;
                pivots.emplace(col, std::move(row));
                return true;
            }
            for (const auto& [j, x] : it->second) {
                auto& entry = row[j];
                entry -= scale * x;
                if (!entry) row.erase(j);
            }
        }
        return true;
    }
    std::size_t rank() const { return pivots.size(); }
    std::vector<Q> solution() const {
        std::vector<Q> result(dimension);
        for (auto it = pivots.rbegin(); it != pivots.rend(); ++it) {
            const auto& [col, row] = *it;
            Q value = 0;
            for (const auto& [j, x] : row) {
                if (j == dimension)
                    value += x;
                else if (j != col)
                    value -= x * result[j];
            }
            result[col] = std::move(value);
        }
        return result;
    }
};

template <class F> void parallel_indices(std::size_t count, std::size_t workers, F task) {
    if (workers <= 1) {
        for (std::size_t i = 0; i < count; ++i)
            task(i);
        return;
    }
    std::atomic<std::size_t> next{0};
    std::vector<std::exception_ptr> errors(count);
    auto run = [&] {
        for (;;) {
            auto i = next.fetch_add(1, std::memory_order_relaxed);
            if (i >= count) return;
            try {
                task(i);
            } catch (...) {
                errors[i] = std::current_exception();
            }
        }
    };
    std::vector<std::thread> pool;
    try {
        for (std::size_t i = 1; i < workers; ++i)
            pool.emplace_back(run);
    } catch (...) {
        for (auto& thread : pool)
            thread.join();
        throw;
    }
    run();
    for (auto& thread : pool)
        thread.join();
    for (const auto& error : errors)
        if (error) std::rethrow_exception(error);
}

} // namespace hepcat::fit_detail
