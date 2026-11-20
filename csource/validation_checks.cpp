#include "validation_checks.hpp"
#include "validation_color.hpp"
#include "chart_scalar.hpp"
#include "high_energy_chart.hpp"

#include <algorithm>
#include <limits>
#include <set>

namespace hepcat {
namespace {
using point = std::map<Id, mpq_class>;
using validation_detail::cut_inventory;
constexpr auto pass = check_outcome::pass, fail = check_outcome::fail, unresolved = check_outcome::unresolved;

std::vector<point> assignments(Algebra& a, const validation_parameter_points& values) {
    std::vector<point> result;
    for (auto& group : values) {
        point p;
        for (const auto& [name, value] : group) {
            Id id = a.parse(name);
            mpq_class q;
            if (!chart_detail::free_parameter(a.node(id)) || !a.number(a.parse(value), q))
                throw std::invalid_argument(
                    "exact chart parameters require scalar names and rational values");
            if (!p.emplace(id, q).second)
                throw std::invalid_argument("duplicate chart parameter");
        }
        if (std::find(result.begin(), result.end(), p) == result.end())
            result.push_back(std::move(p));
    }
    if (result.empty())
        result.emplace_back();
    return result;
}

// Keep confirmation charts outside the small consecutive chart ordinals used by
// the fitting routines. For cut charts the last two bits select chiral branches.
std::size_t chart_index(std::size_t seed, std::size_t ordinal, std::size_t retry) {
    constexpr auto domain = std::size_t(1) << (std::numeric_limits<std::size_t>::digits - 1);
    return domain |
           ((chart_detail::mix(seed ^ chart_detail::mix(ordinal)) & (domain - 1)) & ~std::size_t(255)) |
           (retry * 4) | (ordinal % 4);
}

template <class F> auto at_chart(std::size_t seed, std::size_t j, F evaluate) {
    for (std::size_t retry = 0;; ++retry) {
        try {
            return evaluate(chart_index(seed, j, retry));
        } catch (const residue_singular&) {
            if (retry == 63)
                throw;
        }
    }
}

builtin_check physical(const Algebra& a, Id root, const validation_detail::channel_obligation& channel,
                       const std::vector<point>& points, std::size_t count, std::size_t seed) {
    builtin_check row{channel.in_model ? "Physical" : "Spurious", a.format(channel.propagator), pass, 0, {}};
    try {
        for (const auto& params : points) {
            for (std::size_t j = 0; j < count; ++j) {
                auto values = at_chart(seed, row.samples, [&](auto chart) {
                    return residue_components(a, channel.shell, {root, channel.target}, chart, params);
                });
                ++row.samples;
                if (values[0] != values[1]) {
                    row.status = fail;
                    auto mismatch = std::mismatch(values[0].begin(), values[0].end(), values[1].begin());
                    const char* basis[] = {"1", "sqrt(2)", "i", "i sqrt(2)"};
                    auto k = mismatch.first - values[0].begin();
                    row.detail = std::string("Signed residue difference in the coefficient of ") + basis[k] +
                                 " = " + mpq_class(*mismatch.first - *mismatch.second).get_str();
                    return row;
                }
            }
        }
        row.detail = channel.supplied ? "Agrees with the signed sewn product"
                                      : "Vanishing residue on an unlisted channel";
    } catch (const std::exception& error) {
        row.status = unresolved;
        row.detail = error.what();
    }
    return row;
}

bool explicit_reference(const Algebra& a, Id root) {
    std::set<Id> visited;
    std::vector<Id> todo{root};
    while (!todo.empty()) {
        Id id = todo.back();
        todo.pop_back();
        if (!visited.insert(id).second)
            continue;
        const auto& n = a.node(id);
        if (n.head == "Spinor" && !n.args.empty() && a.node(n.args.front()).head == "\"Xi\"")
            return true;
        todo.insert(todo.end(), n.args.begin(), n.args.end());
    }
    return false;
}

builtin_check spurious(Algebra& a, const cut_inventory& inventory, const std::vector<point>& params,
                       std::size_t sample_offset) {
    builtin_check row{"Spurious", "Kinematic denominator cancellation", pass, 0, {}};
    if (inventory.other_denominators.empty()) {
        row.detail = "Every kinematic denominator is covered by a propagator check";
        return row;
    }
    std::vector<Id> factors{inventory.final};
    for (const auto& channel : inventory.channels)
        if (channel.supplied)
            factors.push_back(channel.propagator);
    Id cleared = a.mul(std::move(factors));
    std::vector<bool> varied(inventory.other_denominators.size());
    std::size_t pairings = a.masses().size() + a.masses().size() % 2 - 1;
    try {
        for (const auto& binding : params) {
            std::fill(varied.begin(), varied.end(), false);
            for (std::size_t j = 0; j < pairings; ++j) {
                auto check = at_chart(sample_offset, row.samples, [&](auto chart) {
                    return regularity_chart(a, cleared, inventory.other_denominators, chart, j, binding);
                });
                ++row.samples;
                if (!check.polynomial) {
                    row.status = fail;
                    row.detail =
                        "A finite pole remains after multiplication by the physical propagator denominators";
                    return row;
                }
                for (std::size_t k = 0; k < varied.size(); ++k)
                    varied[k] = varied[k] || check.varied[k];
            }
            auto missed = std::find(varied.begin(), varied.end(), false);
            if (missed != varied.end()) {
                row.status = unresolved;
                row.detail = "Denominator constant on all probed deformations: " +
                             a.format(inventory.other_denominators[std::size_t(missed - varied.begin())]);
                return row;
            }
        }
        row.detail = "Every detected denominator varied; the cleared expression was polynomial on every "
                     "sampled prime-field chart";
    } catch (const std::exception& error) {
        row.status = unresolved;
        row.detail = error.what();
    }
    return row;
}

builtin_check energy_check(const Algebra& a, Id final, int bound, const std::vector<point>& points,
                           std::size_t energy_directions, std::size_t offset) {
    builtin_check energy{"HighEnergy", "Fixed-angle E^" + std::to_string(bound), pass, 0, {}};
    bool all_spins = true;
    try {
        for (const auto& params : points) {
            for (std::size_t j = 0; j < energy_directions; ++j) {
                auto state = at_chart(offset, energy.samples, [&](auto chart) {
                    return high_energy_validate_chart(a, final, bound, chart, params);
                });
                ++energy.samples;
                all_spins &= state.exhaustive_spin;
                if (!state.passes) {
                    energy.status = fail;
                    energy.detail = "A nonzero Laurent coefficient exceeds the requested power";
                    break;
                }
            }
            if (energy.status == fail)
                break;
        }
        if (energy.status == pass)
            energy.detail = all_spins ? "Bound holds on each chart for every coherent spin coefficient"
                                      : "Bound holds on the sampled coherent spin projections";
    } catch (const std::exception& error) {
        energy.status = unresolved;
        energy.detail = error.what();
    }
    return energy;
}

// Every color assignment is projected. Coinciding scalar functions share
// their charts. Count the actual chart evaluations across distinct components.
template <class T, class F> builtin_check components(const std::set<T>& versions, F evaluate) {
    builtin_check out;
    bool first = true;
    for (const auto& id : versions) {
        auto row = evaluate(id);
        if (first) {
            out = row;
            first = false;
        } else {
            out.samples += row.samples;
            if (row.status == fail || (out.status == pass && row.status == unresolved)) {
                out.status = row.status;
                out.detail = row.detail;
            }
        }
        if (out.status == fail)
            break;
    }
    if (versions.size() > 1)
        out.detail += " (" + std::to_string(versions.size()) + " distinct color components)";
    return out;
}
} // namespace

std::vector<builtin_check> native_builtin_checks(const amplitude& process, const amp_expression& expression,
                                                 int bound,
                                                 const validation_parameter_points& parameter_points,
                                                 std::size_t samples_per_cut, std::size_t energy_directions,
                                                 std::size_t offset,
                                                 const std::vector<int>& color_representations) {
    if (samples_per_cut < 4 || energy_directions == 0)
        throw std::invalid_argument(
            "validation requires all four cut branches and at least one energy direction");
    if (process.external_masses().size() != expression.particle_count())
        throw std::invalid_argument("validation expression has incompatible external multiplicity");
    Algebra a;
    std::vector<Id> masses;
    for (const auto& mass : process.external_masses())
        masses.push_back(a.parse(mass));
    a.setKinematics(std::move(masses), {});
    auto points = assignments(a, parameter_points);
    auto final = validation_detail::import_expression(a, expression);
    std::vector<builtin_check> rows;
    if (!explicit_reference(a, final))
        rows.push_back({"Reference", "Reference independence", pass, 0, "No explicit Xi spinors"});
    try {
        auto inventory = validation_detail::validation_inventory(a, process, expression);
        std::vector<Id> roots{inventory.final};
        for (const auto& channel : inventory.channels)
            roots.push_back(channel.target);
        validation_detail::fundamental_color_projections projection(a, color_representations);
        auto versions = projection.project(roots);
        if (!process.factorization_products_generated() && process.factorization_products().empty()) {
            rows.push_back({"Physical", "Factorization inventory", unresolved, 0,
                            "No channel products were generated or supplied"});
        } else if (inventory.channels.empty()) {
            rows.push_back({"Physical", "Factorization inventory", pass, 0,
                            "Generated process has no exchange channels"});
        } else {
            // Residues are linear in the two roots. Use a pair of IDs for
            // de-duplication because their difference can contain distinct sewn
            // helicity weights across summands.
            for (std::size_t i = 0; i < inventory.channels.size(); ++i) {
                std::set<std::pair<Id, Id>> distinct;
                for (const auto& v : versions)
                    distinct.emplace(v.front(), v[i + 1]);
                rows.push_back(components(distinct, [&](const auto& pair) {
                    auto channel = inventory.channels[i];
                    auto [root, target] = pair;
                    channel.target = target;
                    return physical(a, root, channel, points, samples_per_cut, offset);
                }));
            }
        }
        std::set<Id> finals;
        for (const auto& version : versions)
            finals.insert(version.front());
        rows.push_back(components(finals, [&](Id id) {
            inventory.final = id;
            return spurious(a, inventory, points, offset + 1);
        }));
        rows.push_back(components(finals, [&](Id id) {
            return energy_check(a, id, bound, points, energy_directions, offset + 2);
        }));
    } catch (const std::exception& error) {
        for (const auto& kind : {"Physical", "Spurious", "HighEnergy"})
            rows.push_back({kind, "External-kinematics checks", unresolved, 0, error.what()});
    }
    return rows;
}
} // namespace hepcat
