// Console formatting shared by the two C++ notebooks. The saved validation
// reports retain their original identifiers and full numerical detail.
#pragma once

#include "amp_expression.hpp"
#include "validation.hpp"

#include <filesystem>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <map>
#include <sstream>
#include <stdexcept>
#include <string>
#include <vector>

namespace notebook_output {

inline std::string number(std::size_t value) {
    std::string digits = std::to_string(value);
    for (auto end = digits.size(); end > 3; end -= 3)
        digits.insert(end - 3, ",");
    return digits;
}

inline std::string seconds(double elapsed) {
    if (elapsed > 0 && elapsed < 0.01)
        return "<0.01 s";
    std::ostringstream text;
    text << std::fixed << std::setprecision(2) << elapsed << " s";
    return text.str();
}

inline void print_stage(const std::string& message) {
    std::cout << "    " << message << std::endl;
}

// Keep short amplitudes visible during a run. A long amplitude has its own
// text file, so it never pushes the validation results off the terminal.
inline void write_expression(const std::filesystem::path& base, const hepcat::amp_expression& amplitude) {
    const auto write = [&](const char* suffix, const std::string& text) {
        const auto path = base.string() + suffix;
        std::ofstream file(path);
        file << text << '\n';
        file.close();
        if (!file) throw std::runtime_error("Could not write " + path);
    };
    write(".expr", amplitude.str());
    const auto formula = amplitude.plain_text();
    write(".txt", formula);

    std::istringstream input(formula);
    std::string line;
    unsigned lines = 0;
    while (std::getline(input, line))
        if (++lines > 2 || line.size() > 74) return;
    std::cout << "    Amplitude\n";
    for (std::istringstream preview(formula); std::getline(preview, line);)
        std::cout << "      " << line << '\n';
}

inline const char* status(hepcat::check_outcome outcome) {
    switch (outcome) {
        case hepcat::check_outcome::pass: return "PASS";
        case hepcat::check_outcome::fail: return "FAIL";
        default: return "UNRESOLVED";
    }
}

inline std::string check_name(const std::string& kind) {
    static const std::map<std::string, std::string> names = {
        {"Physical", "Physical residues"}, {"Spurious", "Spurious poles"},
        {"HighEnergy", "High-energy growth"}, {"Reference", "Reference spinors"},
        {"ReferenceAmplitude", "Signed reference amplitude"}, {"Feynman", "Signed Feynman diagrams"},
        {"FinalReduction", "Final simplification"}, {"PhaseSpace", "Phase-space evaluation"},
        {"FiniteAmplitude", "Finite amplitude"}, {"BuiltIn", "Kinematic checks"},
    };
    const auto entry = names.find(kind);
    return entry == names.end() ? kind : entry->second;
}

inline void print_validation(const hepcat::validation_report& report, std::ostream& output = std::cout) {
    // One line per kind keeps a large set of passing poles readable. Every
    // failed or unresolved check still gets its own diagnostic under that row.
    std::ostringstream table;
    auto columns = [&](const std::string& check, const std::string& outcome) -> std::ostream& {
        return table << "      " << std::left << std::setw(26) << check << "  "
                     << std::setw(10) << outcome << "  ";
    };
    columns("Check", "Status") << "Coverage\n";

    std::map<std::string, std::vector<const hepcat::builtin_check*>> groups;
    for (const auto& row : report.checks)
        groups[row.kind].push_back(&row);

    using hepcat::check_outcome;
    for (const auto& [kind, checks] : groups) {
        std::size_t passed = 0, failed = 0, samples = 0;
        for (const auto* check : checks) {
            passed += check->status == check_outcome::pass;
            failed += check->status == check_outcome::fail;
            samples += check->samples;
        }
        const auto unresolved = checks.size() - passed - failed;
        const auto outcome = failed ? check_outcome::fail
                                    : unresolved ? check_outcome::unresolved : check_outcome::pass;
        columns(check_name(kind), status(outcome));
        if (outcome != check_outcome::pass) {
            table << passed << " passed, " << failed << " failed, " << unresolved << " unresolved";
        } else if (kind == "Spurious" && samples == 0 && checks.size() == 1 &&
                   checks.front()->label == "Kinematic denominator cancellation") {
            table << "Covered by propagator checks";
        } else if (checks.size() == 1 && samples == 0 && !checks.front()->detail.empty()) {
            table << checks.front()->detail;
        } else if (kind == "HighEnergy" && checks.size() == 1) {
            table << checks.front()->label;
        } else {
            table << number(checks.size()) << (checks.size() == 1 ? " check" : " checks");
        }
        if (samples) table << ", " << number(samples) << (samples == 1 ? " chart sample" : " chart samples");
        table << '\n';
        for (const auto* check : checks)
            if (check->status != check_outcome::pass)
                table << "        " << status(check->status) << "  " << check->label
                      << ": " << check->detail << '\n';
    }

    for (const auto& row : report.numerical) {
        columns(check_name(row.kind), status(row.status)) << number(row.compared);
        if (row.compared != report.requested) table << '/' << number(report.requested);
        table << (report.requested == 1 ? " point, " : " points, ")
              << number(row.failures) << (row.failures == 1 ? " mismatch" : " mismatches") << '\n';
        if (row.replays)
            table << "        Rechecked " << number(row.replays) << (row.replays == 1 ? " point" : " points")
                  << " at higher precision.\n";
        if (row.first_failure_seed) table << "        First failing seed: " << *row.first_failure_seed << '\n';
        if (row.status != check_outcome::pass && !row.detail.empty())
            table << "        " << row.detail << '\n';
    }
    output << table.str() << std::flush;
}

} // namespace notebook_output
