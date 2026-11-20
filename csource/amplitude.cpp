#include "amplitude.hpp"

#include <algorithm>
#include <atomic>
#include <climits>
#include <exception>
#include <stdexcept>
#include <thread>
#include <utility>

namespace hepcat {

amplitude_item::amplitude_item(const std::string& input, const std::vector<std::string>& masses,
                               std::vector<numerator_shell> conditions)
    : shells(std::move(conditions)), expression(input, masses, shells), input_size_(input.size()) {}

amplitude::amplitude(std::vector<std::string> external_masses)
    : masses_(std::move(external_masses)) {
    if (masses_.empty() || masses_.size() > INT_MAX)
        throw std::invalid_argument("invalid external mass count");
}

void amplitude::add_diagram(const std::string& input, std::vector<numerator_shell> shells) {
    diagrams_.push_back(amplitude_item(input, masses_, std::move(shells)));
    construction_.reset();
    completed_.reset();
    high_energy_result_.reset();
    before_final_reduction_.reset();
}

void amplitude::add_factorization_product(const std::string& input, numerator_shell channel) {
    std::vector<numerator_shell> shells;
    shells.push_back(std::move(channel));
    products_.push_back(amplitude_item(input, masses_, std::move(shells)));
    construction_.reset();
    completed_.reset();
    high_energy_result_.reset();
    before_final_reduction_.reset();
}

void amplitude::add_prepared_diagram(const std::string& input, std::vector<numerator_shell> shells,
                                     const std::string& remainder) {
    amplitude_item item(input, masses_, std::move(shells));
    item.expression.restore_prepared(remainder);
    item.reduced_ = true;
    diagrams_.push_back(std::move(item));
    construction_.reset();
    completed_.reset();
    high_energy_result_.reset();
    before_final_reduction_.reset();
}

void amplitude::add_prepared_factorization_product(const std::string& input, numerator_shell channel,
                                                   const std::string& remainder) {
    amplitude_item item(input, masses_, {std::move(channel)});
    item.expression.restore_prepared(remainder);
    item.reduced_ = true;
    products_.push_back(std::move(item));
    construction_.reset();
    completed_.reset();
    high_energy_result_.reset();
    before_final_reduction_.reset();
}

const factorization_report& amplitude::last_factorization() const {
    if (!completed_) throw std::logic_error("no validated factorization result");
    return report_;
}

amplitude& amplitude::reduce(std::size_t workers) {
    // Copies share their own item's input arena; different items have separate
    // arenas and locks. Results are held here until all reductions succeed.
    struct task {
        amplitude_item* item;
        amp_expression result;
        std::exception_ptr error;
        const char* kind;
        std::size_t index;
    };
    std::vector<task> work;
    auto enqueue = [&](std::vector<amplitude_item>& entries, const char* kind) {
        for (std::size_t i = 0; i < entries.size(); ++i) {
            if (!entries[i].reduced_)
                work.push_back({&entries[i], entries[i].expression, {}, kind, i});
        }
    };
    enqueue(diagrams_, "diagram");
    enqueue(products_, "factorization product");
    if (work.empty()) {
        last_workers_ = 0;
        return *this;
    }

    if (!workers) workers = std::max(1u, std::thread::hardware_concurrency());
    workers = std::min(workers, work.size());
    if (workers > 1) {
        // Wire length is a cheap scheduling hint. A long product can start
        // immediately even when all diagrams were loaded first.
        std::stable_sort(work.begin(), work.end(), [](const task& a, const task& b) {
            return a.item->input_size_ > b.item->input_size_;
        });
    }

    std::atomic<std::size_t> next{0};
    std::atomic<bool> failed{false};
    auto run = [&] {
        while (!failed.load(std::memory_order_relaxed)) {
            auto i = next.fetch_add(1, std::memory_order_relaxed);
            if (i >= work.size()) return;
            try {
                work[i].result.reduce();
            } catch (...) {
                work[i].error = std::current_exception();
                failed.store(true, std::memory_order_relaxed);
            }
        }
    };
    std::vector<std::thread> helpers;
    helpers.reserve(workers - 1);
    try {
        for (std::size_t i = 1; i < workers; ++i) helpers.emplace_back(run);
    } catch (...) {
        failed.store(true, std::memory_order_relaxed);
        for (auto& helper : helpers) helper.join();
        throw;
    }
    run();
    for (auto& helper : helpers) helper.join();

    for (const auto& t : work) {
        if (!t.error) continue;
        try {
            std::rethrow_exception(t.error);
        } catch (const std::exception& e) {
            std::throw_with_nested(std::runtime_error(std::string(t.kind) + " " +
                std::to_string(t.index + 1) + ": " + e.what()));
        }
    }
    static_assert(noexcept(std::declval<amp_expression&>() = std::declval<amp_expression&&>()));
    for (auto& t : work) {
        t.item->expression = std::move(t.result);
        t.item->reduced_ = true;
    }
    last_workers_ = workers;
    return *this;
}

} // namespace hepcat
