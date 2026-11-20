#pragma once

#include "validation.hpp"

#include <iostream>
#include <map>

namespace notebook_output {

inline void print_stage(const std::string& message) {
    std::cout << "    " << message << std::endl;
}

inline const char* status(hepcat::check_outcome outcome) {
    switch (outcome) {
        case hepcat::check_outcome::pass: return "PASS";
        case hepcat::check_outcome::fail: return "FAIL";
        default: return "UNRESOLVED";
    }
}

inline void print_validation(const hepcat::validation_report& report) {
    // Keep a long list of passing residues readable. Print every individual failure.
    struct counts { std::size_t passed = 0, failed = 0, unresolved = 0; };
    std::map<std::string, counts> groups;
    for (const auto& row : report.checks) {
        auto& group = groups[row.kind];
        switch (row.status) {
            case hepcat::check_outcome::pass: ++group.passed; break;
            case hepcat::check_outcome::fail: ++group.failed; break;
            default: ++group.unresolved; break;
        }
    }
    for (const auto& [kind, group] : groups) {
        const auto total = group.passed + group.failed + group.unresolved;
        std::cout << "    " << kind << " checks: " << group.passed << '/' << total << " PASS";
        if (group.failed) std::cout << ", " << group.failed << " FAIL";
        if (group.unresolved) std::cout << ", " << group.unresolved << " UNRESOLVED";
        std::cout << '\n';
    }
    for (const auto& row : report.checks)
        if (row.status != hepcat::check_outcome::pass)
            std::cout << "      " << status(row.status) << ' ' << row.label << ": " << row.detail << '\n';

    for (const auto& row : report.numerical) {
        std::cout << "    " << status(row.status) << ' ' << row.kind << ": " << row.compared
                  << '/' << report.requested << " points compared, " << row.failures << " mismatches";
        if (row.replays) std::cout << ", " << row.replays << " higher-precision replays";
        if (row.first_failure_seed) std::cout << ", first failed seed " << *row.first_failure_seed;
        if (row.status != hepcat::check_outcome::pass && !row.detail.empty()) std::cout << ". " << row.detail;
        std::cout << '\n';
    }
    std::cout.flush();
}
} // namespace notebook_output
