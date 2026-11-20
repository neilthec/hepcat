#include "external_export.hpp"
#include "generation_plan.hpp"
#include "validation_numeric.hpp"

#include <algorithm>
#include <cctype>
#include <chrono>
#include <cmath>
#include <fstream>
#include <functional>
#include <iomanip>
#include <limits>
#include <numeric>
#include <set>
#include <sstream>
#include <stdexcept>
#if defined(__unix__) || defined(__APPLE__)
#include <cerrno>
#include <fcntl.h>
#include <spawn.h>
#include <sys/wait.h>
#include <unistd.h>
extern char **environ;
#endif

namespace hepcat {
namespace fs = std::filesystem;
namespace {
std::string quote(const std::string &s) {
  std::string out = "\"";
  for (unsigned char c : s) {
    if (c < 32)
      throw std::invalid_argument(
          "Control character in path or model metadata");
    if (c == '"' || c == '\\')
      out += '\\';
    out += char(c);
  }
  return out + '"';
}
std::string cmake_string(const std::string &value) {
  std::string equals;
  while (value.find("]" + equals + "]") != std::string::npos)
    equals += '=';
  return "[" + equals + "[" + value + "]" + equals + "]";
}
std::string number(double x) {
  if (!std::isfinite(x))
    throw std::invalid_argument(
        "The reference models require finite real inputs");
  std::ostringstream out;
  out.imbue(std::locale::classic());
  out << std::setprecision(std::numeric_limits<double>::max_digits10) << x;
  return out.str();
}
std::string literal(const std::string &s) { return quote(s); }
std::string literal(double x) { return number(x); }
template <class T> std::string literal(T i) { return std::to_string(i); }
template <class Range, class F>
std::string join(const Range &r, F f, std::string open, std::string close) {
  std::string out = open;
  bool first = true;
  for (const auto &element : r) {
    if (!first)
      out += ", ";
    first = false;
    out += f(element);
  }
  return out + close;
}
template <class Range> std::string list(const Range &r, bool json = false) {
  return join(
      r, [](const auto &x) { return literal(x); }, json ? "[" : "{",
      json ? "]" : "}");
}
template <class Range> std::string lists(const Range &r, bool json = false) {
  return join(
      r, [json](const auto &x) { return list(x, json); }, json ? "[" : "{",
      json ? "]" : "}");
}
template <class Map> std::string object(const Map &m) {
  return join(
      m,
      [](const auto &v) { return quote(v.first) + ": " + literal(v.second); },
      "{", "}");
}
void write(const fs::path &file, const std::string &contents) {
  std::ofstream out(file);
  out << contents;
  out.close();
  if (!out)
    throw std::runtime_error("Cannot write " + file.string());
}
std::string bare(std::string s) {
  if (s.size() >= 2 &&
      (s.substr(s.size() - 2) == ".+" || s.substr(s.size() - 2) == ".-"))
    s.resize(s.size() - 2);
  return s;
}
std::vector<std::size_t> order(const spinas_project &p) {
  std::vector<std::size_t> v{p.incoming[0], p.incoming[1]};
  for (std::size_t i = 0; i < p.pdgs.size(); ++i)
    if (i != v[0] && i != v[1])
      v.push_back(i);
  return v;
}
template <class V>
V reordered(const V &values, const std::vector<std::size_t> &indices) {
  V result;
  for (auto i : indices)
    result.push_back(values.at(i));
  return result;
}
std::string mapped(const std::map<std::string, std::string> &m,
                   const std::string &x) {
  auto i = m.find(x);
  return i == m.end() ? x : i->second;
}

struct model_bindings {
  const model &source;
  validation::parameters numeric;
  std::vector<const model_parameter *> parameters;
  std::map<std::string, int> by_name, by_symbol;
  std::map<Id, int> by_id;
  std::vector<double> defaults;
  std::size_t independent = 0;
  explicit model_bindings(const model &m)
      : source(m), numeric(validation::model_parameters(m)) {
    for (const auto *collection : {&m.variables(), &m.functions()}) {
      for (const auto &p : *collection) {
        const auto symbol = m.algebra().format(p.symbol);
        auto v = numeric.find(symbol);
        if (v == numeric.end() || v->second.imag() != 0)
          continue;
        auto slot = static_cast<int>(parameters.size());
        by_name.emplace(p.name, slot);
        by_symbol.emplace(symbol, slot);
        by_id.emplace(p.symbol, slot);
        parameters.push_back(&p);
        defaults.push_back(static_cast<double>(v->second.real()));
      }
      if (collection == &m.variables())
        independent = parameters.size();
    }
  }
  int role(const std::string &expr) const {
    if (expr == "0")
      return -1;
    auto it = by_symbol.find(expr);
    if (it != by_symbol.end())
      return it->second;
    it = by_name.find(expr);
    if (it != by_name.end())
      return it->second;
    throw std::invalid_argument(
        "CalcHEP mass or width needs a real model binding: " + expr);
  }
  std::string expression(Id id, const std::function<void(int)> &on_slot) const {
    if (auto it = by_id.find(id); it != by_id.end()) {
      on_slot(it->second);
      return "v[" + std::to_string(it->second) + "]";
    }
    const auto &a = source.algebra();
    mpq_class q;
    if (a.number(id, q))
      return number(q.get_d());
    const auto &n = a.node(id);
    if (n.head == "Pi")
      return "std::acos(REAL(-1))";
    std::vector<std::string> args;
    for (auto child : n.args)
      args.push_back(expression(child, on_slot));
    if (n.head == "Plus" || n.head == "Times") {
      std::string out = "(";
      for (std::size_t i = 0; i < args.size(); ++i) {
        if (i)
          out += n.head == "Plus" ? " + " : " * ";
        out += args[i];
      }
      return out + ")";
    }
    static const std::map<std::string, std::string> functions = {
        {"Power", "pow"},   {"Sqrt", "sqrt"},   {"Abs", "abs"},
        {"Sin", "sin"},     {"Cos", "cos"},     {"Tan", "tan"},
        {"ArcSin", "asin"}, {"ArcCos", "acos"}, {"Exp", "exp"},
        {"Log", "log"}};
    if (auto f = functions.find(n.head); f != functions.end())
      return "std::" + f->second +
             join(args, [](const auto &x) { return x; }, "(", ")");
    throw std::invalid_argument("Unsupported CalcHEP model relation: " +
                                a.format(id));
  }
};
int charge(int pdg, const external_options &o) {
  if (auto it = o.charge3.find(pdg); it != o.charge3.end())
    return it->second;
  int c = 0;
  switch (std::abs(pdg)) {
  case 11:
  case 13:
  case 15:
    c = -3;
    break;
  case 1:
  case 3:
  case 5:
    c = -1;
    break;
  case 2:
  case 4:
  case 6:
    c = 2;
    break;
  case 24:
    c = 3;
    break;
  case 12:
  case 14:
  case 16:
  case 21:
  case 22:
  case 23:
  case 25:
    break;
  default:
    throw std::invalid_argument("Specify three times the charge using "
                                "external_options::charge3 for PDG " +
                                std::to_string(pdg));
  }
  return pdg < 0 ? -c : c;
}
void numerical_ch(const spinas_project &p, const model &m,
                  const external_options &o,
                  const std::vector<std::size_t> &external_order,
                  const model_bindings &b) {
  std::ostringstream e, conf;
  e << "int evaluate_model(REAL* v) {\n";
  std::set<int> active, done;
  std::function<void(int)> define = [&](int slot) {
    if (slot < static_cast<int>(b.independent) || done.count(slot))
      return;
    if (!active.insert(slot).second)
      throw std::invalid_argument("Cyclic CalcHEP parameter relation");
    auto expr = b.expression(b.parameters.at(slot)->definition, define);
    e << "  v[" << slot << "] = " << expr << ";\n  if (!std::isfinite(v["
      << slot << "])) return " << slot + 1 << ";\n";
    active.erase(slot);
    done.insert(slot);
  };
  for (std::size_t i = b.independent; i < b.parameters.size(); ++i)
    define(static_cast<int>(i));
  e << "  return 0;\n}\n\n";
  conf << "hepcat_calchep::Configuration numerical_configuration() {\n"
       << "  hepcat_calchep::Configuration c;\n  c.calchep_directory = "
          "HEPCAT_CALCHEP_ROOT;\n"
       << "  c.independent_count = " << b.independent
       << ";\n  c.evaluate = &evaluate_model;\n";
  for (std::size_t i = 0; i < b.parameters.size(); ++i)
    conf << "  c.parameters.push_back({" << quote(b.parameters[i]->name) << ", "
         << number(b.defaults[i]) << "});\n";
  std::map<int, std::size_t> rows;
  for (const auto &particle : m.particles()) {
    if (rows.count(std::abs(particle.pdg)))
      continue;
    auto mass = b.role(particle.mass), width = b.role(particle.width);
    auto mass_name = mass < 0 ? "0" : b.parameters[mass]->name,
         width_name = width < 0 ? "0" : b.parameters[width]->name;
    auto n = bare(particle.name), anti = bare(particle.antiname);
    auto slot = rows.size();
    rows.emplace(std::abs(particle.pdg), slot);
    conf << "  c.particles.push_back({" << quote(n) << ", " << quote(anti)
         << ", " << particle.pdg << ", " << particle.spin2 << ", "
         << particle.color << ", " << charge(particle.pdg, o) << ", "
         << quote(mass_name) << ", " << quote(width_name) << ", "
         << (n == anti ? "true" : "false") << "});\n";
  }
  for (auto i : external_order) {
    if (!rows.count(std::abs(p.pdgs[i])))
      throw std::invalid_argument("Unknown physical PDG in model");
    std::set<int> selected_helicities;
    for (const auto& row : p.helicities) selected_helicities.insert(row.at(i));
    int states = p.mass_defaults[i] > 0 ? p.spin2[i] + 1
                    : static_cast<int>(selected_helicities.size());
    conf << "  c.legs.push_back({" << quote(bare(p.physical_particles.at(i))) << ", " << p.pdgs[i]
         << ", " << p.spin2[i] << ", " << p.color_dimensions.at(i) << ", "
         << (charge(std::abs(p.pdgs[i]), o) == 0 ? "true" : "false") << ", " << states << "});\n";
  }
  std::vector<int> slots;
  for (const auto &name : p.parameter_names) {
    auto index = b.by_name.find(name);
    if (index == b.by_name.end())
      throw std::invalid_argument("CalcHEP model lacks real input " + name);
    slots.push_back(index->second);
  }
  conf << "  c.process_parameter_indices = " << list(slots)
       << ";\n  c.external_order = " << list(external_order) << ";\n";
  if (auto gg = b.by_name.find("GG"); gg != b.by_name.end())
    conf << "  c.running_strong_parameter = " << gg->second << ";\n";
  // Recover phase-space mapping channels from allowed model diagrams. All
  // helicity alternatives for a given cut share a mapping when masses agree.
  std::set<std::tuple<std::vector<int>, int, int, std::size_t>> denominators;
  std::vector<std::size_t> physical_slot(external_order.size());
  for (std::size_t i = 0; i < external_order.size(); ++i)
    physical_slot[external_order[i]] = i + 1;
  for (const auto &hel : p.helicities) {
    std::vector<std::string> analytic;
    for (std::size_t i = 0; i < p.pdgs.size(); ++i) {
      const auto &pr = *std::find_if(
          m.particles().begin(), m.particles().end(), [&](const auto &x) {
            return std::abs(x.pdg) == std::abs(p.pdgs[i]);
          });
      bool crossed = i != p.incoming[0] && i != p.incoming[1];
      std::string name =
          (crossed ? -p.pdgs[i] : p.pdgs[i]) == pr.pdg ? pr.name : pr.antiname;
      if (hel[i] && bare(pr.name) == bare(pr.antiname))
        name = bare(pr.name) + (hel[i] > 0 ? ".+" : ".-");
      analytic.push_back(name);
    }
    for (const auto &plan : generation::diagram_plans(m, analytic))
      for (const auto &term : plan.terms)
        for (const auto &edge : term.edges) {
          const auto &part = m.particle(edge.particle_a);
          std::vector<int> legs;
          for (int l : edge.legs)
            legs.push_back(static_cast<int>(physical_slot.at(l - 1)));
          std::sort(legs.begin(), legs.end());
          std::vector<int> other;
          for (std::size_t j = 1; j <= p.pdgs.size(); ++j)
            if (!std::binary_search(legs.begin(), legs.end(), int(j)))
              other.push_back(int(j));
          if (other.size() < legs.size() ||
              (other.size() == legs.size() && other < legs))
            legs = other;
          denominators.emplace(std::move(legs), b.role(part.mass),
                               b.role(part.width), rows.at(std::abs(part.pdg)));
        }
  }
  for (const auto &[legs, mass, width, particle] : denominators) {
    conf << "  c.denominators.push_back({\"";
    for (int leg : legs)
      conf << "\\" << std::setfill('0') << std::setw(3) << std::oct << leg
           << std::dec;
    conf << "\", " << mass << ", " << width << ", " << particle << "});\n";
  }
  conf << "  return c;\n}\n";
  write(p.directory / "calchep-model.inc", e.str() + conf.str());
}
fs::path runtime_path(const external_options &o) {
  if (!o.runtime_directory.empty())
    return fs::absolute(o.runtime_directory);
  return fs::path(__FILE__).parent_path().parent_path() /
         "source/internal/export";
}
void copy_to(const fs::path &source, const fs::path &to) {
  if (fs::exists(to) && fs::equivalent(source, to))
    return;
  fs::copy_file(source, to, fs::copy_options::overwrite_existing);
}
void check(const spinas_project &p, const external_options &o) {
  if (o.points < 10000)
    throw std::invalid_argument(
        "External validation requires at least 10000 shared points");
  if (p.name.empty() ||
      !std::all_of(
          p.name.begin(), p.name.end(),
          [](unsigned char c) { return std::isalnum(c) || c == '_'; }) ||
      std::isdigit(static_cast<unsigned char>(p.name[0])))
    throw std::invalid_argument("Invalid generated C++ class name");
  std::size_t n = p.pdgs.size();
  if (n < 4 || p.spin2.size() != n || p.mass_defaults.size() != n ||
      p.physical_particles.size() != n ||
      p.parameter_names.size() != p.parameter_defaults.size() ||
      p.incoming[0] >= n || p.incoming[1] >= n ||
      p.incoming[0] == p.incoming[1] || p.helicities.empty() ||
      !(p.normalization > 0 && p.symmetry_factor > 0))
    throw std::invalid_argument(
        "Incomplete generated HEPCAT/SPINAS process metadata");
  for (auto &hs : p.helicities)
    if (hs.size() != n)
      throw std::invalid_argument("Invalid external helicity row");
  for (auto i : p.width_parameters)
    if (i >= p.parameter_names.size())
      throw std::invalid_argument("Invalid width parameter");
  if (o.calchep_directory.empty() != o.calchep_model_directory.empty() ||
      (!o.calchep_directory.empty() && !o.calchep_model_number))
    throw std::invalid_argument("Specify a CalcHEP installation, model "
                                "directory and positive model number");
  if (o.madgraph_directory.empty() != o.madgraph_model.empty())
    throw std::invalid_argument(
        "Specify a MadGraph installation and model together");
  if (std::any_of(o.allowed_pdgs.begin(), o.allowed_pdgs.end(),
                  [](int i) { return i <= 0; }))
    throw std::invalid_argument(
        "Restrictions require positive absolute PDG codes");
  if (!o.allowed_pdgs.empty())
    for (int id : p.pdgs)
      if (std::find(o.allowed_pdgs.begin(), o.allowed_pdgs.end(),
                    std::abs(id)) == o.allowed_pdgs.end())
        throw std::invalid_argument(
            "Particle restriction excludes an external leg");
}
} // namespace

// The conventional SM UFO takes inverse alpha, G_F and M_Z. HEPCAT's SM
// instead takes e, sin(theta_W) and M_W. Check both tree-level relations at
// independently varied inputs before translating this parameter scheme.
static std::map<std::string, double> madgraph_inputs(const model &m, const external_options &o) {
  auto inputs = o.madgraph_parameter_rules;
  auto selected = fs::path(o.madgraph_model).filename().string();
  if (selected != "sm" && selected != "sm-full")
    return inputs;
  std::map<std::string, std::string> keys;
  for (const auto *rows : {&m.variables(), &m.functions()})
    for (const auto &p : *rows)
      keys[p.name] = m.algebra().format(p.symbol);
  for (auto name : {"EE", "SW", "MW", "MZ", "GF"})
    if (!keys.count(name))
      return inputs;
  std::map<std::string, double> defaults;
  for (const auto &point :
       std::vector<validation::parameters>{{},
                                           {{"EE", 0.29L}, {"SW", 0.41L}, {"MW", 77.L}},
                                           {{"EE", 0.38L}, {"SW", 0.55L}, {"MW", 92.L}}}) {
    auto v = validation::model_parameters(m, point);
    auto get = [&](const char *key) { return v.at(keys.at(key)).real(); };
    for (auto name : {"EE", "SW", "MW", "MZ", "GF"})
      if (!v.count(keys[name]) || v.at(keys[name]).imag() != 0)
        return inputs;
    auto e = get("EE"), w = get("SW"), mw = get("MW"), mz = get("MZ"), gf = get("GF");
    if (!(e > 0 && w > 0 && w < 1 && mw > 0 && mz > 0 && gf > 0) ||
        std::abs(mz * std::sqrt(1 - w * w) / mw - 1) > 1e-12L ||
        std::abs(gf * 4 * w * w * mw * mw * std::sqrt(2.L) / (e * e) - 1) > 1e-12L)
      return inputs;
    if (point.empty())
      defaults = {{"aEWM1", 4 * std::acos(-1.L) / (e * e)}, {"Gf", gf}, {"MZ", mz}};
  }
  auto lower = [](std::string s) {
    for (auto &c : s)
      c = std::tolower(static_cast<unsigned char>(c));
    return s;
  };
  for (const auto &entry : defaults) {
    const auto key = lower(entry.first);
    if (std::none_of(inputs.begin(), inputs.end(),
                     [&](const auto &p) { return lower(p.first) == key; }))
      inputs.insert(entry);
  }
  return inputs;
}

void write_external_project(const spinas_project &p, const model &m,
                            const external_options &o) {
  check(p, o);
  const auto mg_inputs = madgraph_inputs(m, o);
  if (!fs::is_regular_file(p.directory / "CMakeLists.txt") ||
      !fs::is_regular_file(p.directory / "process.hpp"))
    throw std::invalid_argument(
        "Export the SPINAS process before attaching numerical backends");
  const auto runtime = runtime_path(o);
  const auto templates =
      o.template_directory.empty()
          ? fs::path(__FILE__).parent_path() / "external_templates"
          : o.template_directory;
  const auto ord = order(p);
  auto pdgs = reordered(p.pdgs, ord);
  const bool colored = std::any_of(p.color_dimensions.begin(), p.color_dimensions.end(),
                                   [](int d) { return d != 1; });
  std::vector<std::string> names, mass_names, massless;
  for (const auto &n : p.parameter_names)
    names.push_back(mapped(o.calchep_parameter_map, n));
  for (const auto &param : m.variables())
    if (p.mass_parameter_pdgs.count(param.name))
      mass_names.push_back(mapped(o.calchep_parameter_map, param.name));
  for (auto mass : p.mass_defaults)
    massless.push_back(mass == 0 ? "true" : "false");
  auto abspath = [](const fs::path &f) {
    return f.empty() ? std::string() : fs::absolute(f).string();
  };
  // The preprocessor and complex-number typedefs from CalcHEP stay outside
  // ordinary SPINAS project headers and are used only by its dedicated
  // executable.
  std::ostringstream h;
  h << "#pragma once\n#include \"process.hpp\"\n#include <string>\n#include "
       "<vector>\nnamespace native_export {\n"
    << "using Process = hepcat_generated::" << p.name << ";\n"
    << "constexpr std::size_t default_points = " << o.points << ";\n"
    << "constexpr bool complete_helicities = "
    << (p.complete_helicities ? "true" : "false") << ";\n"
    << "static const char* const calchep_model_directory = "
    << quote(abspath(o.calchep_model_directory)) << ";\n"
    << "constexpr unsigned calchep_model_number = " << o.calchep_model_number
    << ";\n"
    << "inline std::array<std::size_t,Process::leg_count> leg_order(){return {"
    << list(ord) << "};}\n"
    << "inline std::vector<int> physical_pdgs(){return " << list(pdgs) << ";}\n"
    << "inline std::vector<int> spin2(){return " << list(p.spin2) << ";}\n"
    << "inline std::vector<int> allowed_pdgs(){return " << list(o.allowed_pdgs)
    << ";}\n"
    << "inline std::vector<int> width_indices(){return "
    << list(p.width_parameters) << ";}\n"
    << "inline std::vector<std::string> independent_mass_names(){return "
    << list(mass_names) << ";}\n"
    << "inline std::vector<std::string> calchep_names(){return " << list(names)
    << ";}\n"
    << "inline std::vector<bool> massless(){return "
    << join(
           massless, [](const auto &v) { return v; }, "{", "}")
    << ";}\n"
    << "inline std::vector<std::vector<int>> helicities(){return "
    << lists(p.helicities) << ";}\n"
    << "}\n";
  std::ostringstream mg, js;
  mg << "config.madgraph_directory = " << quote(abspath(o.madgraph_directory)) << ";\n"
     << "config.model = " << quote(o.madgraph_model) << ";\n"
     << "config.python_executable = " << quote(o.python_executable) << ";\n"
     << "config.physical_pdgs = native_export::physical_pdgs();\n"
     << "config.spin2 = native_export::spin2();\nconfig.helicities = "
        "native_export::helicities();\n"
     << "config.full_color = " << (colored ? "true" : "false") << ";\n"
     << "config.color_representations = " << list(p.color_dimensions) << ";\n"
     << "config.allowed_particle_pdgs = native_export::allowed_pdgs();\n"
     << "config.leg_order = " << list(ord)
     << ";\nconfig.normalization = " << number(p.normalization) << ";\n"
     << "config.parameter_names = " << list(p.parameter_names) << ";\n"
     << "config.exact_zero_helicities = " << lists(p.exact_zero_helicities) << ";\n"
     << "config.width_parameter_indices = " << list(p.width_parameters) << ";\n";
  for (auto &[key, v] : p.mass_parameter_pdgs)
    mg << "config.mass_parameter_pdgs[" << quote(key) << "] = {" << std::abs(v)
       << "};\n";
  for (auto &[key, v] : o.madgraph_parameter_map)
    mg << "config.parameter_map[" << quote(key) << "] = " << quote(v) << ";\n";
  for (auto &[key, v] : mg_inputs)
    mg << "config.parameter_rules[" << quote(key) << "] = " << number(v)
       << ";\n";
  std::vector<size_t> one = ord;
  for (auto &leg : one)
    ++leg;
  js << "{\n  \"MadGraphDirectory\": " << quote(abspath(o.madgraph_directory))
     << ",\n  \"MadGraphModel\": " << quote(o.madgraph_model)
     << ",\n  \"PythonExecutable\": " << quote(o.python_executable)
     << ",\n  \"ProcessDirectory\": \".\",\n  \"SessionDirectoryBase\": "
        "\"BuildDirectory\",\n  \"SessionDirectory\": "
     << quote("PROC_" + p.name) << ",\n  \"ProcessName\": " << quote(p.name)
     << ",\n  \"BridgeLinkFile\": \"build/madevent-link.json\""
     << ",\n  \"PhysicalPDGs\": " << list(pdgs, true) << ",\n  \"LegOrder\": " << list(one, true)
     << ",\n  \"AllowedParticlePDGs\": " << list(o.allowed_pdgs, true)
     << ",\n  \"Spin2\": " << list(p.spin2, true)
     << ",\n  \"HelicityConfigurations\": " << lists(p.helicities, true)
     << ",\n  \"Masses\": " << list(p.mass_defaults, true)
     << ",\n  \"InitialSpinAverage\": " << number(p.symmetry_factor / p.normalization)
     << ",\n  \"SymmetryFactor\": " << number(p.symmetry_factor)
     << ",\n  \"ParameterNames\": " << list(p.parameter_names, true)
     << ",\n  \"ParameterDefaults\": " << list(p.parameter_defaults, true)
     << ",\n  \"ParameterMap\": " << object(o.madgraph_parameter_map)
     << ",\n  \"ParameterRules\": " << object(mg_inputs) << ",\n  \"MassParameterPDGs\": "
     << join(
            p.mass_parameter_pdgs,
            [](const auto &pair) {
              return quote(pair.first) + ": [" +
                     std::to_string(std::abs(pair.second)) + "]";
            },
            "{", "}")
     << "\n}\n";
  if (!o.calchep_directory.empty()) {
    std::ostringstream replay;
    replay << "{\n  \"Class\": " << quote(p.name)
           << ",\n  \"Masses\": " << list(p.mass_defaults, true) << ",\n  \"IncomingLegs\": ["
           << p.incoming[0] + 1 << "," << p.incoming[1] + 1 << "]"
           << ",\n  \"ParameterOrder\": " << list(p.parameter_names, true)
           << ",\n  \"ParameterDefaults\": "
           << join(
                  p.parameter_defaults, [](double v) { return "{\"Real\":" + number(v) + "}"; },
                  "[", "]")
           << ",\n  \"WidthIndices\": " << list(p.width_parameters, true)
           << ",\n  \"CalcHEPValidation\": {\"Status\":\"AVAILABLE\",\"Supported\":"
           << (p.complete_helicities ? "true" : "false") << ",\"PhysicalPDGs\":" << list(pdgs, true)
           << ",\"LegOrder\":" << list(one, true)
           << ",\"ParameterNames\":" << list(p.parameter_names, true)
           << ",\"ParameterMap\":" << object(o.calchep_parameter_map)
           << ",\"CalcHEPDirectory\":" << quote(abspath(o.calchep_directory))
           << ",\"ModelDirectory\":" << quote(abspath(o.calchep_model_directory))
           << ",\"ModelNumber\":" << o.calchep_model_number
           << ",\"AllowedParticlePDGs\":" << list(o.allowed_pdgs, true)
           << ",\"IndependentMassParameters\":" << list(mass_names, true) << "}}\n";
    write(p.directory / "calchep-replay.json", replay.str());
    copy_to(runtime / "calchep-precision.py", p.directory / "calchep-precision.py");
    copy_to(runtime / "calchep_chart.py", p.directory / "calchep_chart.py");
    copy_to(runtime / "spinas-quad.hpp", p.directory / "spinas-quad.hpp");
  }
  write(p.directory / "external-model.hpp", h.str());
  write(p.directory / "madevent-config.json", js.str());
  write(p.directory / "madgraph-model.inc", mg.str());
  std::string numerical_status;
  if (!o.calchep_directory.empty()) {
    try {
      if (colored)
        throw std::invalid_argument("Colored event generation requires external color-flow tags.");
      numerical_ch(p, m, o, ord, model_bindings(m));
    } catch (const std::invalid_argument &error) {
      // Numerical integration requires editable model relations and charge
      // metadata. Independent matrix-element validation only needs the values.
      numerical_status = error.what();
    }
  }
  for (const auto *name : {"calchep-runtime.hpp", "calchep-spinas-runtime.hpp",
                           "madgraph-runtime.hpp", "madgraph-runtime.py",
                           "madevent-runtime.py", "spinas-validation.hpp"})
    copy_to(runtime / name, p.directory / name);
  for (const auto *name :
       {"calchep-main.cpp", "madgraph-main.cpp", "calchep-spinas.cpp",
        "madevent-spinas.cpp", "backends.cmake"})
    copy_to(templates / name, p.directory / name);
  write(p.directory / "external.cmake",
        "set(HEPCAT_CALCHEP_NUMERICAL_ERROR " + cmake_string(numerical_status) + ")\n" +
            "set(HEPCAT_EXTERNAL_COLOR " + (colored ? "ON" : "OFF") + ")\nset(HEPCAT_PROCESS " +
            p.name + ")\nset(HEPCAT_CALCHEP " + cmake_string(abspath(o.calchep_directory)) +
            ")\nset(HEPCAT_MADGRAPH " + cmake_string(abspath(o.madgraph_directory)) +
            ")\nset(HEPCAT_PYTHON " + cmake_string(o.python_executable) +
            ")\ninclude(\"${CMAKE_CURRENT_LIST_DIR}/backends.cmake\")\n");
}

int run_external_command(const std::vector<std::string> &arguments,
                         const fs::path &log) {
  if (arguments.empty())
    throw std::invalid_argument("Expected an executable");
#if defined(__unix__) || defined(__APPLE__)
  std::vector<char *> argv;
  for (auto &x : arguments)
    argv.push_back(const_cast<char *>(x.c_str()));
  argv.push_back(nullptr);
  posix_spawn_file_actions_t actions;
  if (int e = posix_spawn_file_actions_init(&actions))
    throw std::system_error(e, std::generic_category(), "spawn actions");
  int error = posix_spawn_file_actions_addopen(
      &actions, STDOUT_FILENO, log.c_str(), O_WRONLY | O_TRUNC | O_CREAT, 0600);
  if (!error)
    error = posix_spawn_file_actions_adddup2(&actions, STDOUT_FILENO,
                                             STDERR_FILENO);
  pid_t pid;
  if (!error)
    error =
        posix_spawnp(&pid, argv[0], &actions, nullptr, argv.data(), environ);
  posix_spawn_file_actions_destroy(&actions);
  if (error)
    throw std::system_error(error, std::generic_category(), arguments[0]);
  int status;
  while (waitpid(pid, &status, 0) < 0)
    if (errno != EINTR)
      throw std::system_error(errno, std::generic_category(),
                              "waiting for " + arguments[0]);
  return WIFEXITED(status) ? WEXITSTATUS(status) : 128 + WTERMSIG(status);
#else
  throw std::runtime_error(
      "Launching the external HEP packages requires a POSIX host.");
#endif
}
external_report validate_external_project(const spinas_project &p,
                                          const external_options &o) {
  check(p, o);
  if (!o.run_calchep && !o.run_madgraph)
    throw std::invalid_argument("Select an independent validation backend");
  if ((o.run_calchep && o.calchep_directory.empty()) ||
      (o.run_madgraph && o.madgraph_directory.empty()))
    throw std::invalid_argument(
        "Configure the installation and model for each requested validation");

  auto build = fs::absolute(p.directory / "build");
  fs::create_directories(build);
  external_report result{
      o.seed.value_or(static_cast<std::uint64_t>(
          std::chrono::system_clock::now().time_since_epoch().count())),
      o.points};
  result.requested_calchep = o.run_calchep;
  result.requested_madgraph = o.run_madgraph;
  if (o.run_calchep)
    result.calchep_report = build / "calchep-comparison.log";
  if (o.run_madgraph)
    result.madgraph_report = build / "madgraph-comparison.log";
  // Invalidate previous receipts before configuring. A failed rebuild must
  // never leave a passing report that appears to belong to this invocation.
  const auto pending = "Validation pending for seed " + std::to_string(result.seed) + "\n";
  if (o.run_calchep) write(result.calchep_report,pending);
  if (o.run_madgraph) {
    write(result.madgraph_report,pending);
    write(build / "madgraph-comparison.json",
          "{\"Passed\":false,\"Status\":\"Pending\",\"Seed\":" + std::to_string(result.seed) + "}\n");
  }
  result.configuration = run_external_command(
      {o.cmake_executable, "-S", fs::absolute(p.directory).string(), "-B",
       build.string()},
      build / "external-configure.log");
  if (result.configuration != 0)
    return result;
  for (const auto &backend :
       {std::string("calchep"), std::string("madgraph")}) {
    bool calc = backend == "calchep";
    if (calc ? !o.run_calchep : !o.run_madgraph)
      continue;
    const auto exe = p.name + "_validate_" + backend;
    int &status = calc ? result.calchep : result.madgraph;
    status = run_external_command(
        {o.cmake_executable, "--build", build.string(), "--target", exe},
        build / ("external-build-" + backend + ".log"));
    if (status)
      continue;
    std::vector<std::string> args = {(build / exe).string(), "--points",
                                     std::to_string(o.points), "--seed",
                                     std::to_string(result.seed)};
    if (calc) {
      args.push_back("--outliers");
      args.push_back((build / "calchep-outliers.csv").string());
    } else {
      args.push_back("--report");
      args.push_back((build / "madgraph-comparison.json").string());
    }
    status = run_external_command(args, calc ? result.calchep_report
                                             : result.madgraph_report);
    if (calc && status == 3) {
      const auto job =
          build / ("precision-" +
                   std::to_string(std::chrono::steady_clock::now().time_since_epoch().count()));
      auto replay_status = run_external_command(
          {o.python_executable, (p.directory / "calchep-precision.py").string(),
           p.directory.string(), "--require-captured-values", "--points-csv",
           (build / "calchep-outliers.csv").string(), "--seed", std::to_string(result.seed),
           "--output", job.string()},
          build / "calchep-precision.log");
      if (fs::is_regular_file(job / "precision-summary.json"))
        fs::copy_file(job / "precision-summary.json", build / "calchep-precision.json",
                      fs::copy_options::overwrite_existing);
      if (fs::is_regular_file(job / "calchep-chart.csv"))
        fs::copy_file(job / "calchep-chart.csv", build / "calchep-chart.csv",
                      fs::copy_options::overwrite_existing);
      std::ofstream log(result.calchep_report, std::ios::app);
      log << "CalcHEP binary128 replay of every saved raw outlier: "
          << (replay_status == 0 ? "PASS" : "FAIL") << "\n";
      if (replay_status == 0)
        status = 0;
    }
  }
  return result;
}
} // namespace hepcat
