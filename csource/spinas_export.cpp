#include "spinas_export.hpp"
#include "reduce.hpp"
#include "validation_numeric.hpp"
#include <algorithm>
#include <cctype>
#include <cmath>
#include <cstdlib>
#include <fstream>
#include <functional>
#include <iomanip>
#include <limits>
#include <numeric>
#include <set>
#include <sstream>
#include <stdexcept>

namespace hepcat {
namespace {
namespace fs = std::filesystem;
using ints = std::vector<int>;
using strings = std::vector<std::string>;
std::string join(const strings& items, const std::string& delimiter) {
    std::string result;
    for (std::size_t i = 0; i < items.size(); ++i) {
        if (i)
            result += delimiter;
        result += items[i];
    }
    return result;
}
std::string literal(long double value) {
    if (!std::isfinite(value))
        throw std::invalid_argument("nonfinite numeric default in SPINAS export");
    std::ostringstream stream;
    stream << std::scientific << std::setprecision(std::numeric_limits<long double>::max_digits10) << value
           << 'L';
    return stream.str();
}
std::string string_value(std::string s) {
    if (s.size() < 2 || s.front() != '"' || s.back() != '"')
        throw std::invalid_argument("expected a model string");
    return s.substr(1, s.size() - 2);
}
std::string head(const Node& n) { return n.head.substr(n.head.rfind('`') + 1); }
std::string species(std::string s) {
    if (s.size() > 2 && s[s.size() - 2] == '.' && (s.back() == '+' || s.back() == '-'))
        s.resize(s.size() - 2);
    return s;
}
std::string integer_list(const ints& v) {
    strings text;
    for (int i : v)
        text.push_back(std::to_string(i));
    return "{" + join(text, ", ") + "}";
}
bool incoming(const spinas_project& p, std::size_t i) { return p.incoming[0] == i || p.incoming[1] == i; }

struct leg_info {
    bool massive;
    ints helicities;
};
int physical_helicity(const model_particle& p, const std::string& input, bool beam) {
    int all_in = 0;
    if (input != species(input))
        all_in = input.back() == '+' ? p.spin2 : -p.spin2;
    else if (p.spin2 == 1 && (p.auxiliary == "L" || p.auxiliary == "R"))
        all_in = (input == p.name ? 1 : -1) * (p.auxiliary == "L" ? -1 : 1);
    else if (p.spin2 && p.mass == "0")
        throw std::invalid_argument("a massless spin-carrying external state needs a definite helicity: " +
                                    input);
    return !beam && p.spin2 % 2 ? -all_in : all_in;
}
struct plan {
    spinas_project project;
    std::vector<leg_info> legs;
    const model& m;
    validation::parameters values;
    std::vector<std::complex<long double>> defaults;
    std::map<std::string, std::pair<std::string, std::complex<long double>>> known;
    std::set<std::string> widths;
    strings masses, point_lines, chain_lines;
    std::map<std::string, size_t> scalars, chains;
    std::vector<std::string> functions;
    std::vector<std::string> dispatch;
    std::vector<ints> exchangeable;
    std::set<size_t> conditioned_components;
    plan(const model& model, const spinas_export_options& opt, const strings& names)
        : m(model), values(validation::model_parameters(m)) {
        auto& p = project;
        p.name = opt.name;
        p.incoming = opt.incoming;
        const auto n = names.size();
        if (n < 4 || p.incoming[0] >= n || p.incoming[1] >= n || p.incoming[0] == p.incoming[1])
            throw std::invalid_argument(
                "SPINAS expects two distinct incoming legs in a process with at least four legs");
        const auto& a = m.algebra();
        for (auto entries : {&m.variables(), &m.functions()})
            for (auto& par : *entries) {
                auto key = a.format(par.symbol);
                if (auto it = values.find(key); it != values.end())
                    known.emplace(par.name, std::make_pair(key, it->second));
            }
        for (auto& particle : m.particles())
            if (particle.width != "0")
                widths.insert(particle.width);
        std::map<int, int> outgoing_counts;
        for (size_t i = 0; i < n; ++i) {
            const auto& part = m.particle(names[i]);
            if (part.color != 1)
                throw std::invalid_argument(
                    "native SPINAS export needs a color-singlet family; external color sums are unavailable");
            bool beam = incoming(p, i), massive = part.mass != "0",
                 neutral = species(part.name) == species(part.antiname);
            int pdg = part.pdg * ((neutral || names[i] == part.name) ? 1 : -1);
            if (!beam && !neutral)
                pdg = -pdg;
            p.pdgs.push_back(pdg);
            p.physical_particles.push_back(species(beam ? names[i] : m.antiparticle(names[i])));
            p.spin2.push_back(part.spin2);
            ints helicities{0};
            if (!massive && part.spin2) {
                if (part.spin2 == 1 && (part.auxiliary == "L" || part.auxiliary == "R"))
                    helicities = {physical_helicity(part, names[i], beam)};
                else {
                    helicities.clear();
                    for (const auto& r : m.particles())
                        for (const auto& tag : {r.name, r.antiname})
                            if (species(tag) == species(names[i]) && tag != species(tag))
                                helicities.push_back(physical_helicity(r, tag, beam));
                    if (helicities.empty())
                        helicities = {-part.spin2, part.spin2};
                    std::sort(helicities.begin(), helicities.end());
                    helicities.erase(std::unique(helicities.begin(), helicities.end()), helicities.end());
                }
            }
            auto selected = opt.fixed_helicities.find(i);
            if (selected != opt.fixed_helicities.end()) {
                if (massive || !part.spin2 ||
                    std::find(helicities.begin(), helicities.end(), selected->second) == helicities.end())
                    throw std::invalid_argument("invalid fixed massless helicity selection");
                if (helicities.size() > 1)
                    p.complete_helicities = false;
                helicities = {selected->second};
            }
            legs.push_back({massive, helicities});
            if (beam)
                p.normalization /= massive ? part.spin2 + 1 : helicities.size();
            else
                p.symmetry_factor /= ++outgoing_counts[pdg];
            if (!massive) {
                masses.push_back("ldouble(0)");
                p.mass_defaults.push_back(0);
            } else {
                auto mass = values.find(part.mass);
                if (mass == values.end() || mass->second.imag() != 0 || mass->second.real() <= 0)
                    throw std::invalid_argument("unresolved massive external parameter: " + part.mass);
                auto par = std::find_if(known.begin(), known.end(),
                                        [&](const auto& x) { return x.second.first == part.mass; });
                if (par == known.end())
                    throw std::invalid_argument("unsupported external mass expression: " + part.mass);
                p.mass_parameter_pdgs.emplace(par->first, std::abs(pdg));
                p.mass_defaults.push_back(static_cast<double>(mass->second.real()));
                masses.push_back("hepcat_spinas::checked_mass(" + parameter(par->first) + ")");
            }
        }
        if (!opt.fixed_helicities.empty() && opt.fixed_helicities.rbegin()->first >= n)
            throw std::out_of_range("fixed helicity outside the process");
        p.normalization *= p.symmetry_factor;
        for (size_t i = 0; i < n; ++i) {
            if (legs[i].massive || p.spin2[i] % 2)
                continue;
            auto equivalent = [&](const ints& group) {
                auto j = static_cast<size_t>(group.front());
                return p.pdgs[i] == p.pdgs[j] && incoming(p, i) == incoming(p, j) &&
                       legs[i].helicities == legs[j].helicities;
            };
            auto it = std::find_if(exchangeable.begin(), exchangeable.end(), equivalent);
            if (it == exchangeable.end())
                exchangeable.push_back({static_cast<int>(i)});
            else
                it->push_back(static_cast<int>(i));
        }
        exchangeable.erase(std::remove_if(exchangeable.begin(), exchangeable.end(),
                                          [](const auto& group) { return group.size() < 2; }),
                           exchangeable.end());
    }
    std::string parameter(const std::string& name) {
        const auto it = known.find(name);
        if (it == known.end())
            throw std::invalid_argument("SPINAS cannot resolve model parameter " + name);
        auto& p = project;
        auto index =
            std::find(p.parameter_names.begin(), p.parameter_names.end(), name) - p.parameter_names.begin();
        if (static_cast<size_t>(index) == p.parameter_names.size()) {
            p.parameter_names.push_back(name);
            defaults.push_back(it->second.second);
            p.parameter_defaults.push_back(it->second.second.imag() == 0
                                               ? it->second.second.real()
                                               : std::numeric_limits<double>::quiet_NaN());
            if (widths.count(it->second.first))
                p.width_parameters.push_back(index);
        }
        return "parameters[" + std::to_string(index) + "]";
    }
    std::string cache(const std::string& expr) {
        auto [it, inserted] = scalars.emplace(expr, scalars.size());
        auto name = "invariants[" + std::to_string(it->second) + "]";
        if (inserted)
            point_lines.push_back("    " + name + " = " + expr + ";");
        return name;
    }
};
struct endpoint {
    int leg;
    bool angle, upper;
};
struct compiler {
    const Algebra& a;
    plan& p;
    const ints& perm;
    bool track = false;
    strings lines;
    std::map<std::string, std::string> temporaries;
    bool point_scope = false;
    // Massive slot occurrences along a product define an arbitrary ordering of
    // the symmetric spin tensor. Every summand starts with the same slot offsets.
    size_t mapped(Id id) const {
        int l = a.leg(id);
        if (l <= 0 || static_cast<size_t>(l) > perm.size())
            throw std::invalid_argument("invalid external momentum label: " + a.format(id));
        return perm[l - 1];
    }
    std::string temporary(const std::string& s) {
        if (point_scope)
            return p.cache(s);
        auto [it, added] = temporaries.emplace(s, "v" + std::to_string(temporaries.size()));
        if (added)
            lines.push_back(std::string("    const ") + (track ? "Scalar " : "cdouble ") + it->second +
                            " = " + s + ";");
        return it->second;
    }
    endpoint end(Id id) const {
        auto& n = a.node(id);
        if (head(n) != "Spinor" || n.args.size() < 3)
            throw std::invalid_argument("unresolved spinor endpoint: " + a.format(id));
        auto kind = a.node(n.args.front()).head;
        if (kind != "\"Spin\"" && kind != "\"Helicity\"")
            throw std::invalid_argument("SPINAS needs physical external spinors: " + a.format(id));
        size_t type = 1;
        bool upper = true;
        if (a.node(n.args[type]).head == "\"Lower\"" || a.node(n.args[type]).head == "\"Upper\"")
            upper = a.node(n.args[type++]).head == "\"Upper\"";
        auto side = a.node(n.args.at(type)).head;
        if (side != "\"Angle\"" && side != "\"Square\"")
            throw std::invalid_argument("invalid spinor chirality");
        int leg = static_cast<int>(mapped(n.args.at(type + 1)));
        if ((kind == "\"Spin\"") != p.legs[leg].massive)
            throw std::invalid_argument("wrong spinor type for external mass");
        if (n.args.size() > type + 2) {
            mpq_class fixed;
            if (n.args.size() != type + 3 || a.number(n.args[type + 2], fixed))
                throw std::invalid_argument("fixed massive spin index in exported complete spin tensor");
        }
        return {leg, side == "\"Angle\"", upper};
    }
    void momentum(Id id, std::vector<mpq_class>& coefficients, mpq_class weight = 1) const {
        auto& n = a.node(id);
        auto h = head(n);
        if (h == "Mom" && n.args.size() == 1)
            return momentum(n.args[0], coefficients, weight);
        if (h == "Plus" || h == "Multiparticle") {
            for (auto arg : n.args)
                momentum(arg, coefficients, weight);
        } else if (h == "Times") {
            std::optional<Id> vector;
            for (Id arg : n.args) {
                mpq_class factor;
                if (a.number(arg, factor))
                    weight *= factor;
                else if (vector)
                    throw std::invalid_argument("nonlinear momentum: " + a.format(id));
                else
                    vector = arg;
            }
            if (!vector)
                throw std::invalid_argument("expected a momentum in " + a.format(id));
            momentum(*vector, coefficients, weight);
        } else {
            auto i = mapped(id);
            coefficients[i] += incoming(p.project, i) ? weight : -weight;
        }
    }
    std::string vector(const std::vector<Id>& labels) const {
        std::vector<mpq_class> weights(perm.size());
        for (Id l : labels)
            momentum(l, weights);
        ints signed_labels;
        for (size_t i = 0; i < weights.size(); ++i) {
            auto w = weights[i];
            if (w.get_den() != 1 || abs(w) > 32) {
                strings components;
                for (int mu = 0; mu < 4; ++mu) {
                    strings terms;
                    for (size_t j = 0; j < weights.size(); ++j)
                        if (weights[j] != 0)
                            terms.push_back(rational(weights[j]) + " * momenta[" + std::to_string(j) + "][" +
                                            std::to_string(mu) + "]");
                    components.push_back(terms.empty() ? "0" : join(terms, " + "));
                }
                return "hepcat_spinas::FourMomentum{" + join(components, ", ") + "}";
            }
            for (long k = 0; k < std::abs(w.get_num().get_si()); ++k)
                signed_labels.push_back((w > 0 ? 1 : -1) * static_cast<int>(i + 1));
        }
        return "hepcat_spinas::sum_momenta(momenta, " + integer_list(signed_labels) + ")";
    }
    static std::string rational(const mpq_class& q) {
        auto base = "ldouble(" + q.get_num().get_str() + ".0L)";
        return q.get_den() == 1 ? base : "(" + base + " / ldouble(" + q.get_den().get_str() + ".0L))";
    }
    std::string chain(const Node& n, ints& slots) {
        if (n.args.size() < 2)
            throw std::invalid_argument("SPINAS chain requires two endpoints");
        endpoint l = end(n.args.front()), r = end(n.args.back());
        const bool odd = (n.args.size() - 2) % 2 != 0;
        if ((l.angle != r.angle) != odd)
            throw std::invalid_argument("inconsistent chain endpoint chiralities");
        auto ep = [&](endpoint s, bool left) {
            auto fn = std::string(left ? "l" : "r") + (s.angle ? "angle" : "square");
            return "particles[" + std::to_string(s.leg) + "]." + fn + "(" +
                   (p.legs[s.leg].massive
                        ? std::string(left ? "ls" : "rs") + ", " + (s.upper ? "true" : "false")
                        : "") +
                   ")";
        };
        strings parts{ep(l, true)};
        for (size_t j = 1; j + 1 < n.args.size(); ++j)
            parts.push_back("hepcat_spinas::momentum_matrix(" + vector({n.args[j]}) + ", " +
                            ((l.angle != (j % 2 != 0)) ? "true" : "false") + ")");
        parts.push_back(ep(r, false));
        bool minus = (l.angle && !incoming(p.project, l.leg)) != (r.angle && !incoming(p.project, r.leg));
        auto s = std::string(minus ? "-(" : "(") + join(parts, " * ") + ")";
        auto [c, new_chain] = p.chains.emplace(s, p.chains.size());
        std::string name = "chains[" + std::to_string(c->second) + "]";
        auto index = [&](endpoint e) {
            if (!p.legs[e.leg].massive)
                return std::string("0");
            int slot = slots[e.leg]++;
            if (slot >= p.project.spin2[e.leg])
                throw std::invalid_argument("too many massive spinor slots on leg " +
                                            std::to_string(e.leg + 1));
            return "(indices[" + std::to_string(e.leg) + "][" + std::to_string(slot) + "] > 0)";
        };
        auto left = index(l), right = index(r);
        if (new_chain) {
            bool lm = p.legs[l.leg].massive, rm = p.legs[r.leg].massive;
            auto statement = name + "[" + std::string(lm ? "2 * (ls > 0)" : "0") + " + " +
                             (rm ? "(rs > 0)" : "0") + "] = " + s + ";";
            std::string indent = "    ";
            if (lm) {
                p.chain_lines.push_back(indent + "for (int ls : {-1, 1})");
                indent += "    ";
            }
            if (rm) {
                p.chain_lines.push_back(indent + "for (int rs : {-1, 1})");
                indent += "    ";
            }
            p.chain_lines.push_back(indent + statement);
        }
        return name + "[2 * " + left + " + " + right + "]";
    }
    std::string emit(Id id, ints& slots) {
        const auto h = head(a.node(id));
        auto value = emit_raw(id, slots);
        if (track && (h == "SpinorChain" || h == "ModelParameter" || h == "PropDen" || h == "MomProd" ||
                      h == "Mandelstahm" || h == "SpinorTrace" || h == "Mass" || p.known.count(h)))
            return "Scalar(" + value + ")";
        return value;
    }
    std::string emit_raw(Id id, ints& slots) {
        mpq_class q;
        if (a.number(id, q))
            return rational(q);
        const auto& n = a.node(id);
        auto h = head(n);
        if (h == "SpinorChain") {
            if (point_scope)
                throw std::invalid_argument("propagator mass or width contains a spinor");
            return chain(n, slots);
        }
        if (h == "ModelParameter")
            return p.parameter(string_value(a.node(n.args.at(0)).head));
        if (h == "Pi")
            return "ldouble(std::acos(ldouble(-1)))";
        if (h == "I")
            return "cdouble(0, 1)";
        if (h == "Mass" && n.args.size() == 1)
            return p.masses.at(mapped(n.args[0]));
        if (h == "MomProd" && n.args.size() == 2)
            return p.cache("hepcat_spinas::dot(" + vector({n.args[0]}) + ", " + vector({n.args[1]}) + ")");
        if (h == "Mandelstahm")
            return p.cache("hepcat_spinas::mass2(" + vector(n.args) + ")");
        if (h == "PropDen" && (n.args.size() == 2 || n.args.size() == 3)) {
            // Propagators are cached in set_momenta. Their mass and width can
            // themselves be arithmetic expressions of model parameters.
            compiler scalar{a, p, perm, false, {}, {}, true};
            auto mass = scalar.emit_raw(n.args[1], slots);
            std::string w = n.args.size() == 2 ? ""
                                               : " + (widths_enabled ? cdouble(0, 1) * " + mass + " * " +
                                                     scalar.emit_raw(n.args[2], slots) + " : cdouble(0, 0))";
            return p.cache("hepcat_spinas::mass2(" + vector({n.args[0]}) + ") - " + mass + " * " + mass + w);
        }
        if (h == "SpinorTrace") {
            if (n.args.size() % 2 != 0)
                throw std::invalid_argument("odd chiral spinor trace");
            if (n.args.empty())
                return "ldouble(2)";
            strings matrices;
            for (size_t i = 0; i < n.args.size(); ++i)
                matrices.push_back("hepcat_spinas::momentum_matrix(" + vector({n.args[i]}) + ", " +
                                   (i % 2 ? "true" : "false") + ")");
            return p.cache("hepcat_spinas::matrix_trace(" + join(matrices, " * ") + ")");
        }
        if (h == "Times" || h == "Plus") {
            strings terms;
            auto initial = slots;
            std::optional<ints> sum_degree;
            for (auto arg : n.args) {
                if (h == "Plus")
                    slots = initial;
                terms.push_back(emit(arg, slots));
                if (h == "Plus" && sum_degree && *sum_degree != slots)
                    throw std::invalid_argument("inconsistent massive spin weights in sum");
                sum_degree = slots;
            }
            return temporary(join(terms, h == "Times" ? " * " : " + "));
        }
        if (h == "Power" && n.args.size() == 2) {
            auto before = slots;
            auto base = emit(n.args[0], slots);
            if (slots != before) {
                if (!a.number(n.args[1], q) || q.get_den() != 1 || q <= 0 || q > 64)
                    throw std::invalid_argument("non-polynomial massive spin tensor");
                strings factors{base};
                for (long j = 1; j < q.get_num().get_si(); ++j)
                    factors.push_back(emit(n.args[0], slots));
                return temporary(join(factors, " * "));
            }
            if (a.number(n.args[1], q) && q == -1)
                return temporary("ldouble(1) / (" + base + ")");
            if (a.number(n.args[1], q) && q == 2)
                return temporary(base + " * " + base);
            if (a.number(n.args[1], q) && q == mpq_class(1, 2))
                return temporary(std::string(track ? "hepcat_spinas::root(Scalar(" : "std::sqrt(cdouble(") +
                                 base + "))");
            return temporary(std::string(track ? "hepcat_spinas::power(Scalar(" : "std::pow(cdouble(") +
                             base + "), " + emit(n.args[1], slots) + ")");
        }
        const std::map<std::string, std::string> unary{
            {"Sqrt", "sqrt"}, {"Conjugate", "conj"}, {"Abs", "abs"}, {"Re", "real"}, {"Im", "imag"},
            {"Sin", "sin"},   {"Cos", "cos"},        {"Tan", "tan"}, {"Exp", "exp"}, {"Log", "log"}};
        if (auto it = unary.find(h); it != unary.end() && n.args.size() == 1)
            return temporary("std::" + it->second + "(cdouble(" + emit(n.args[0], slots) + "))");
        if (!n.compound && p.known.count(h))
            return p.parameter(h);
        throw std::invalid_argument("unsupported native SPINAS expression: " + a.format(id));
    }
};
bool condition_sensitive(const Algebra& a, Id id) {
    bool has_inverse_chain = false, supported = true;
    std::set<Id> visited;
    std::function<bool(Id)> contains_chain = [&](Id base) {
        const auto& n = a.node(base);
        return head(n) == "SpinorChain" || std::any_of(n.args.begin(), n.args.end(), contains_chain);
    };
    std::function<void(Id)> inspect = [&](Id term) {
        if (!visited.insert(term).second)
            return;
        const auto& n = a.node(term);
        auto h = head(n);
        if (h == "Power" && n.args.size() == 2) {
            mpq_class q;
            if (a.number(n.args[1], q) && q < 0 && contains_chain(n.args[0]))
                has_inverse_chain = true;
        }
        // Other analytic functions retain their original numeric evaluator.
        if (h == "Sqrt" || h == "Conjugate" || h == "Abs" || h == "Re" || h == "Im" || h == "Sin" ||
            h == "Cos" || h == "Tan" || h == "Exp" || h == "Log")
            supported = false;
        for (auto child : n.args)
            inspect(child);
    };
    inspect(id);
    return has_inverse_chain && supported;
}
void compile_family(plan& p, const std::vector<spinas_component>& family) {
    const auto& names = family.front().particles;
    const size_t n = names.size();
    // Permute labels while the expression still has its analytic all-incoming
    // meaning. Crossing angles and momenta uses the target physical leg.
    struct source {
        Id expr;
        ints helicities;
    };
    Algebra tree;
    std::vector<source> sources;
    std::set<ints> provided;
    for (auto& row : family) {
        if (row.expression.particle_count() != n || row.particles.size() != n)
            throw std::invalid_argument("helicity family has inconsistent external leg count");
        ints spins;
        for (size_t i = 0; i < n; ++i) {
            if (species(row.particles[i]) != species(names[i]))
                throw std::invalid_argument("inconsistent species/order in helicity family");
            auto& part = p.m.particle(row.particles[i]);
            spins.push_back(
                p.legs[i].massive ? 0 : physical_helicity(part, row.particles[i], incoming(p.project, i)));
        }
        if (!provided.insert(spins).second)
            throw std::invalid_argument("duplicate explicit helicity component");
        sources.push_back({validation_detail::import_expression(tree, row.expression), spins});
    }
    std::function<void(ints&)> append = [&](ints& target) {
        if (target.size() < n) {
            for (int h : p.legs[target.size()].helicities) {
                target.push_back(h);
                append(target);
                target.pop_back();
            }
            return;
        }
        for (const auto& source : sources) {
            ints perm(n, -1);
            std::vector<bool> used(n);
            // Prefer the user's explicit representative, then Bose-related ones.
            if (provided.count(target) && target != source.helicities)
                continue;
            bool success = true;
            int sign = 1;
            for (size_t i = 0; i < n; ++i) {
                if (source.helicities[i] == target[i]) {
                    used[i] = true;
                    perm[i] = static_cast<int>(i);
                }
            }
            for (size_t i = 0; i < n && success; ++i)
                if (perm[i] < 0) {
                    auto compatible = [&](size_t j) {
                        if (used[j] || p.legs[i].massive || p.legs[j].massive ||
                            species(names[j]) != species(names[i]))
                            return false;
                        // Fermionic helicity reverses when the external role changes.
                        int h = source.helicities[i];
                        if (p.project.spin2[i] % 2 && incoming(p.project, i) != incoming(p.project, j))
                            h = -h;
                        return h == target[j];
                    };
                    size_t j = i;
                    for (j = 0; j < n && !compatible(j); ++j) {
                    }
                    if (j == n)
                        success = false;
                    else {
                        perm[i] = static_cast<int>(j);
                        used[j] = true;
                    }
                }
            if (!success)
                continue;
            for (size_t i = 0; i < n; ++i)
                if (p.project.spin2[i] % 2 && !p.legs[i].massive)
                    for (size_t j = i + 1; j < n; ++j)
                        if (species(names[i]) == species(names[j]) && perm[i] > perm[j])
                            sign = -sign;
            const bool track = !p.exchangeable.empty() && condition_sensitive(tree, source.expr);
            compiler gen{tree, p, perm, track, {}, {}};
            ints slots(n);
            auto value = gen.emit(source.expr, slots);
            mpq_class q;
            bool zero = tree.number(source.expr, q) && q == 0;
            for (size_t i = 0; i < n; ++i)
                if (!zero && p.legs[i].massive && slots[i] != p.project.spin2[i])
                    throw std::invalid_argument("incomplete massive spin tensor on leg " +
                                                std::to_string(i + 1));
            const auto member = "component_" + std::to_string(p.functions.size());
            if (track)
                p.conditioned_components.insert(p.functions.size());
            strings body{std::string(track ? "template<class Scalar> Scalar " : "cdouble ") + p.project.name +
                         "::" + member + "(const Indices& indices) const {"};
            body.insert(body.end(), gen.lines.begin(), gen.lines.end());
            body.push_back(std::string("    return ") + (sign < 0 ? "-(" : "(") + value + ");\n}");
            p.functions.push_back(join(body, "\n"));
            strings conditions;
            for (size_t i = 0; i < n; ++i)
                if (!p.legs[i].massive && p.project.spin2[i])
                    conditions.push_back("spins[" + std::to_string(i) + "] == " + std::to_string(target[i]));
            p.dispatch.push_back(
                "    " + (conditions.empty() ? std::string{} : "if (" + join(conditions, " && ") + ") ") +
                "return " + member + (track ? "<Scalar>" : "") + "(indices);");
            p.project.helicities.push_back(target);
            if (zero)
                p.project.exact_zero_helicities.push_back(target);
            return;
        }
        throw std::invalid_argument("incomplete SPINAS helicity coverage for " + integer_list(target));
    };
    ints t;
    append(t);
}
std::string header(const plan& p) {
    auto& name = p.project.name;
    std::ostringstream out;
    int max_rank = 1;
    for (size_t i = 0; i < p.legs.size(); ++i)
        if (p.legs[i].massive)
            max_rank = std::max(max_rank, p.project.spin2[i]);
    out << "#pragma once\n#include \"spinas-runtime.hpp\"\n";
    if (!p.conditioned_components.empty())
        out << "#include \"spinas-conditioning.hpp\"\n";
    out << "\nnamespace hepcat_generated {\nclass " << name << " {\npublic:\n"
        << "    static constexpr std::size_t leg_count = " << p.legs.size()
        << ", parameter_count = " << p.defaults.size() << ";\n"
        << "    using Parameters = std::array<cdouble, parameter_count>;\n"
        << "    using Spins = std::array<int, leg_count>;\n    using Momenta = "
           "hepcat_spinas::Momenta<leg_count>;\n"
        << "    " << name << "();\n    explicit " << name << "(const Parameters& values);\n"
        << "    static Parameters default_parameters();\n    static Spins default_spins();\n"
        << "    static constexpr std::array<std::size_t, 2> incoming_legs() { return {"
        << p.project.incoming[0] << ", " << p.project.incoming[1] << "}; }\n"
        << "    const Parameters& get_parameters() const { return parameters; }\n"
        << "    const std::array<ldouble, leg_count>& masses() const { return external_masses; }\n"
        << "    void set_parameters(const Parameters& values);\n"
        << "    void set_widths_enabled(bool enabled) { if (enabled != widths_enabled) { "
           "widths_enabled=enabled; ready=false; } }\n"
        << "    void set_momenta(const Momenta& physical);\n"
        << "    cdouble amp(const Spins& spins) const;\n    ldouble amp_squared(const Spins& spins) const;\n "
           "   ldouble spin_sum() const;\n"
        << "    ldouble amp2() const { return spin_sum() * " << literal(p.project.normalization) << "; }\n"
        << "private:\n    using Indices = std::array<std::array<int, " << max_rank << ">, leg_count>;\n"
        << "    Parameters parameters;\n    std::array<ldouble, leg_count> external_masses{};\n"
        << "    std::array<spinas::particle, leg_count> particles;\n    Momenta momenta;\n"
        << "    bool ready=false, widths_enabled=true;\n"
        << "    std::array<cdouble, " << p.scalars.size() << "> invariants;\n"
        << "    std::array<std::array<cdouble, 4>, " << p.chains.size() << "> chains;\n"
        << "    template<class Scalar> Scalar evaluate(const Spins&) const;\n"
        << "    template<class Scalar> Scalar component(const Spins&, const Indices&) const;\n";
    for (size_t i = 0; i < p.functions.size(); ++i)
        out << (p.conditioned_components.count(i) ? "    template<class Scalar> Scalar component_"
                                                  : "    cdouble component_")
            << i << "(const Indices&) const;\n";
    out << "};\n} // namespace hepcat_generated\n";
    return out.str();
}
std::string source(const plan& p) {
    const auto& project = p.project;
    auto& name = project.name;
    std::ostringstream out;
    out << "#include \"process.hpp\"\n\nnamespace hepcat_generated {\n"
        << name << "::" << name << "() : " << name << "(default_parameters()) {}\n"
        << name << "::" << name << "(const Parameters& values) { set_parameters(values); }\n\n"
        << name << "::Parameters " << name << "::default_parameters() {\n    return {\n";
    for (size_t i = 0; i < p.defaults.size(); ++i)
        out << "        cdouble(" << literal(p.defaults[i].real()) << ", " << literal(p.defaults[i].imag())
            << "), // " << project.parameter_names[i] << "\n";
    out << "    };\n}\n\n";
    ints defaults = project.helicities.front();
    for (size_t i = 0; i < p.legs.size(); ++i)
        if (p.legs[i].massive)
            defaults[i] = -project.spin2[i];
    out << name << "::Spins " << name << "::default_spins() { return " << integer_list(defaults) << "; }\n"
        << "void " << name << "::set_parameters(const Parameters& values) {\n    ready = false;\n"
        << "    for (auto v : values) if (!hepcat_spinas::finite_complex(v)) throw "
           "std::invalid_argument(\"Nonfinite parameter.\");\n"
        << "    parameters = values;\n    external_masses = {" << join(p.masses, ", ") << "};\n";
    for (size_t i = 0; i < p.legs.size(); ++i)
        if (p.legs[i].massive && project.spin2[i] > 0)
            out << "    if (external_masses[" << i
                << "] == 0) throw std::invalid_argument(\"Re-export to change the massive spin "
                   "representation.\");\n";
    out << "    for (std::size_t i=0; i<leg_count; ++i) particles[i].set_mass(external_masses[i]);\n}\n\n"
        << "void " << name << "::set_momenta(const Momenta& physical) {\n"
        << "    auto test = hepcat_spinas::validate_momenta(physical, external_masses, incoming_legs());\n"
        << "    if (!test.valid) throw std::invalid_argument(test.message);\n    ready = false;\n    momenta "
           "= physical;\n"
        << "    for (std::size_t i=0; i<leg_count; ++i) particles[i].set_momentum(momenta[i].data());\n"
        << join(p.point_lines, "\n") << "\n"
        << join(p.chain_lines, "\n") << "\n    ready = true;\n}\n\n"
        << join(p.functions, "\n\n") << "\n\n"
        << "template<class Scalar> Scalar " << name
        << "::component(const Spins& spins, const Indices& indices) const {\n"
        << join(p.dispatch, "\n")
        << "\n    throw std::invalid_argument(\"Unavailable helicity in exported family.\");\n}\n\n"
        << "template<class Scalar> Scalar " << name << "::evaluate(const Spins& spins) const {\n"
        << "    if (!ready) throw std::logic_error(\"Call set_momenta before amp.\");\n    Indices "
           "indices{};\n";
    std::string indent = "    ";
    strings vector_legs;
    for (size_t i = 0; i < p.legs.size(); ++i) {
        const auto s = "spins[" + std::to_string(i) + "]";
        int rank = project.spin2[i];
        if (!p.legs[i].massive && rank)
            continue;
        out << "    hepcat_spinas::check_spin(" << rank << ", " << s << ");\n";
        if (rank == 1)
            out << "    indices[" << i << "][0] = " << s << ";\n";
        if (rank > 1)
            vector_legs.push_back(std::to_string(i));
    }
    if (vector_legs.empty())
        out << "    return component<Scalar>(spins, indices);\n";
    else {
        out << "    Scalar result{};\n    ldouble normalization=1;\n";
        for (auto& leg : vector_legs) {
            auto rank = project.spin2[std::stoul(leg)];
            out << "    auto choices" << leg << " = hepcat_spinas::spinor_assignments(" << rank << ", spins["
                << leg << "]);\n";
            out << "    normalization *= hepcat_spinas::spinor_normalization(" << rank << ", spins[" << leg
                << "]);\n";
        }
        for (auto& leg : vector_legs) {
            out << indent << "for (const auto& choice : choices" << leg << ") {\n";
            indent += "    ";
            out << indent << "std::copy(choice.begin(), choice.end(), indices[" << leg << "].begin());\n";
        }
        out << indent << "result += component<Scalar>(spins, indices);\n";
        for (auto& leg : vector_legs) {
            indent.resize(indent.size() - 4);
            out << indent << "}\n";
        }
        out << "    return normalization * result;\n";
    }
    out << "}\n\ncdouble " << name
        << "::amp(const Spins& spins) const { return evaluate<cdouble>(spins); }\n";
    out << "\nldouble " << name << "::amp_squared(const Spins& spins) const {\n";
    if (p.conditioned_components.empty())
        out << "    return std::norm(amp(spins));\n";
    else {
        strings groups;
        for (const auto& group : p.exchangeable)
            groups.push_back(integer_list(group));
        out << "    using tracked = hepcat_spinas::conditioned_complex;\n"
            << "    auto best = evaluate<tracked>(spins);\n"
            << "    auto condition = hepcat_spinas::relative_condition(best);\n"
            << "    const auto threshold = 1/std::sqrt(std::numeric_limits<ldouble>::epsilon());\n"
            << "    if (condition > threshold) {\n"
            << "        static const auto permutations = hepcat_spinas::bose_permutations<leg_count>({"
            << join(groups, ", ") << "});\n"
            << "        auto other = *this;\n"
            << "        for (std::size_t k=1; k<permutations.size(); ++k) {\n"
            << "            Momenta momenta_permuted; Spins spins_permuted;\n"
            << "            for (std::size_t leg=0; leg<leg_count; ++leg) {\n"
            << "                auto source=permutations[k][leg];\n"
            << "                momenta_permuted[leg]=momenta[source]; spins_permuted[leg]=spins[source];\n"
            << "            }\n"
            << "            other.set_momenta(momenta_permuted);\n"
            << "            auto trial=other.evaluate<tracked>(spins_permuted);\n"
            << "            auto error=hepcat_spinas::relative_condition(trial);\n"
            << "            if (error < condition) { best=trial; condition=error; }\n"
            << "            if (condition <= threshold) break;\n"
            << "        }\n    }\n    return std::norm(best.value);\n";
    }
    out << "}\n\nldouble " << name
        << "::spin_sum() const {\n    if (!ready) throw std::logic_error(\"Call set_momenta before "
           "spin_sum.\");\n"
        << "    ldouble result=0;\n    Spins spins{};\n";
    for (size_t i = 0; i < p.legs.size(); ++i) {
        ints allowed = p.legs[i].helicities;
        if (p.legs[i].massive) {
            allowed.clear();
            for (int j = -project.spin2[i]; j <= project.spin2[i]; j += 2)
                allowed.push_back(j);
        }
        out << indent << "for (int h" << i << " : " << integer_list(allowed) << ") {\n";
        indent += "    ";
        out << indent << "spins[" << i << "] = h" << i << ";\n";
    }
    out << indent << "result += amp_squared(spins);\n";
    for (size_t i = 0; i < p.legs.size(); ++i) {
        indent.resize(indent.size() - 4);
        out << indent << "}\n";
    }
    out << "    return result;\n}\n} // namespace hepcat_generated\n";
    return out.str();
}
std::string cmake_path(const fs::path& path) {
    auto text = path.generic_string();
    std::string delim = "=";
    while (text.find("]" + delim + "]") != std::string::npos)
        delim += "=";
    return "[" + delim + "[" + text + "]" + delim + "]";
}
std::string cmake(const std::string& name, const fs::path& spinas) {
    return "cmake_minimum_required(VERSION 3.18)\nproject(" + name +
           " LANGUAGES CXX)\n"
           "if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)\n  set(CMAKE_BUILD_TYPE Release CACHE "
           "STRING \"Build type\" FORCE)\nendif()\n"
           "set(SPINAS_ROOT " +
           cmake_path(spinas) +
           " CACHE PATH \"Path to SPINAS\")\n"
           "find_path(SPINAS_INCLUDE_DIR spinas.h HINTS \"${SPINAS_ROOT}/include\" REQUIRED)\n"
           "find_library(SPINAS_LIBRARY NAMES spinas spinas_shared HINTS \"${SPINAS_ROOT}/build\" "
           "\"${SPINAS_ROOT}/lib\" REQUIRED)\n"
           "set(SPINAS_PRECISION AUTO CACHE STRING \"SPINAS ABI: AUTO, DOUBLE or LONG_DOUBLE\")\n"
           "if(SPINAS_PRECISION STREQUAL AUTO)\n"
           "  if(EXISTS \"${SPINAS_ROOT}/build/CMakeCache.txt\")\n"
           "    file(STRINGS \"${SPINAS_ROOT}/build/CMakeCache.txt\" abi REGEX "
           "\"^PRECISION_TYPE:STRING=(LONG_DOUBLE|DOUBLE)$\")\n"
           "    string(REGEX REPLACE \"^PRECISION_TYPE:STRING=\" \"\" SPINAS_PRECISION \"${abi}\")\n"
           "  endif()\nendif()\n"
           "add_library(" +
           name + "_process STATIC process.cpp)\nset_target_properties(" + name +
           "_process PROPERTIES POSITION_INDEPENDENT_CODE ON)\n"
           "target_include_directories(" +
           name +
           "_process PUBLIC \"${CMAKE_CURRENT_SOURCE_DIR}\" \"${SPINAS_INCLUDE_DIR}\")\n"
           "target_link_libraries(" +
           name +
           "_process PUBLIC \"${SPINAS_LIBRARY}\")\n"
           "target_compile_features(" +
           name +
           "_process PUBLIC cxx_std_17)\n"
           "if(SPINAS_PRECISION STREQUAL LONG_DOUBLE)\n  target_compile_definitions(" +
           name +
           "_process PUBLIC WITH_LONG_DOUBLE)\n"
           "elseif(NOT SPINAS_PRECISION STREQUAL DOUBLE)\n  message(FATAL_ERROR \"Set SPINAS_PRECISION to "
           "the ABI of your SPINAS library.\")\nendif()\n"
           "add_executable(" +
           name + " main.cpp)\ntarget_link_libraries(" + name + " PRIVATE " + name +
           "_process)\n"
           "include(CTest)\nif(BUILD_TESTING)\n  add_test(NAME " +
           name + "_examples COMMAND " + name +
           ")\nendif()\n"
           "include(\"${CMAKE_CURRENT_SOURCE_DIR}/external.cmake\" OPTIONAL)\n";
}
std::string main_example(const std::string& name) {
    return "#include \"process.hpp\"\n#include <iostream>\nint main() {\n    hepcat_generated::" + name +
           " process;\n"
           "    auto incoming=process.incoming_legs();\n    auto "
           "energy=hepcat_spinas::example_center_of_mass_energy(process.masses(), incoming);\n"
           "    for(unsigned i=0; i<3; ++i) {\n        auto "
           "p=hepcat_spinas::make_example_momenta(process.masses(), incoming, energy, i);\n"
           "        process.set_momenta(p);\n        auto value=process.amp2();\n"
           "        if(!std::isfinite(value) || value<0) return 1;\n        std::cout << \"Point \" << i+1 "
           "<< \"  |M|^2 = \" << value << '\\n';\n    }\n}\n";
}
} // namespace
spinas_project write_spinas_process(const fs::path& directory, const model& vertices,
                                    const std::vector<spinas_component>& family,
                                    const spinas_export_options& options) {
    if (family.empty())
        throw std::invalid_argument("no SPINAS components");
    auto& name = options.name;
    if (name.empty() || !std::isalpha(static_cast<unsigned char>(name[0])) ||
        !std::all_of(name.begin(), name.end(), [](unsigned char c) { return std::isalnum(c) || c == '_'; }))
        throw std::invalid_argument("expected a C++ process identifier");
    plan p(vertices, options, family.front().particles);
    compile_family(p, family);
    p.project.directory = fs::absolute(directory);
    const fs::path templates = options.runtime_directory.empty()
                                   ? fs::path(__FILE__).parent_path().parent_path() / "source/internal/export"
                                   : options.runtime_directory;
    auto spinas = options.spinas_directory;
    if (spinas.empty()) {
        if (const char* env = std::getenv("SPINAS_ROOT"))
            spinas = env;
        else
            spinas = templates.parent_path().parent_path().parent_path().parent_path() / "spinas";
    }
    // Form and validate every output before opening any project source.
    std::map<std::string, std::string> files{{"CMakeLists.txt", cmake(name, spinas)},
                                             {"process.cpp", source(p)},
                                             {"process.hpp", header(p)},
                                             {"main.cpp", main_example(name)}};
    strings runtime_files{"spinas-runtime.hpp", "spinas-validation.hpp"};
    if (!p.conditioned_components.empty())
        runtime_files.push_back("spinas-conditioning.hpp");
    for (const auto& file : runtime_files) {
        std::ifstream in(templates / file, std::ios::binary);
        if (!in)
            throw std::runtime_error("cannot read SPINAS runtime: " + (templates / file).string());
        files.emplace(file, std::string(std::istreambuf_iterator<char>(in), {}));
    }
    fs::create_directories(directory);
    for (auto& file : files)
        if (fs::exists(directory / file.first) && !options.overwrite)
            throw std::runtime_error("SPINAS output already exists: " + (directory / file.first).string());
    for (auto& file : files) {
        std::ofstream dest(directory / file.first, std::ios::binary);
        dest.exceptions(std::ios::badbit | std::ios::failbit);
        dest << file.second;
        dest.close();
    }
    return p.project;
}
} // namespace hepcat
