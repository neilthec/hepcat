// Use the opposite photon helicity as a second way to write a QED amplitude.
// From the repository root, build the native library and this example:
//   cmake -S csource -B tests/results/native-reduce-build -DCMAKE_BUILD_TYPE=Release
//   cmake --build tests/results/native-reduce-build --target hepcat-qed-helicity-partners
//   tests/results/native-reduce-build/hepcat-qed-helicity-partners \
//       tests/results/spinas-output/RUN/physical/eeeeAAA
// The input is a completed family from hepcat-qed. Use the spurious directory to study that policy.
// The optional flags are --root DIR, --vertices N, --cuts N, --feynman N and --workers N.
// The source formulas remain in place. Any shorter signed matches, their checks, and a manifest
// are written to parity-alternatives/ after the entire family has been examined. Repeated runs
// retain their own timestamped directories.

#include "amplitude.hpp"
#include "reduce.hpp"
#include "../construction-progress.hpp"
#include <algorithm>
#include <chrono>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <limits>
#include <iterator>
#include <map>
#include <numeric>
#include <set>
#include <sstream>

namespace fs = std::filesystem;
using namespace hepcat;
namespace {
constexpr std::size_t feynman_points = 100000;
constexpr std::uint64_t sign_seed = 21734681, identity_seed = 37651911, feynman_seed = 43021791;


struct settings {
    fs::path directory, root = ".";
    unsigned vertices = 3, cuts = 0, feynman = 2, workers = 2;
};
unsigned positive(const std::string& value) {
    if (value.empty() || value.find_first_not_of("0123456789") != value.npos)
        throw std::invalid_argument("Expected a positive integer: " + value);
    auto result = std::stoul(value);
    if (!result || result > std::numeric_limits<unsigned>::max())
        throw std::invalid_argument("Integer outside the supported range: " + value);
    return static_cast<unsigned>(result);
}
settings arguments(int argc, char** argv) {
    if (argc < 2 || (argc % 2) != 0)
        throw std::invalid_argument("Usage: hepcat-qed-helicity-partners RUN/POLICY/FAMILY [--root DIR] "
                                    "[--vertices N] [--cuts N] [--feynman N] [--workers N]");
    settings s;
    s.directory = fs::canonical(argv[1]);
    for (int i = 2; i < argc; i += 2) {
        const std::string flag = argv[i], value = argv[i+1];
        if (flag == "--root") s.root = value;
        else if (flag == "--vertices") s.vertices = positive(value);
        else if (flag == "--cuts") s.cuts = positive(value);
        else if (flag == "--feynman") s.feynman = positive(value);
        else if (flag == "--workers") s.workers = positive(value);
        else throw std::invalid_argument("Unknown option " + flag);
    }
    return s;
}

std::string read(const fs::path& filename) {
    std::ifstream file(filename);
    if (!file) throw std::runtime_error("Missing input " + filename.string());
    return {std::istreambuf_iterator<char>(file), {}};
}

struct family {
    std::vector<std::string> charged;
    std::size_t photons;
};

// The notebook uses different all-incoming orderings for four and six charged legs.
// The suffix contains one A per photon. For example, eeeeAAA has three photons.
family parse_family(const std::string& name) {
    const auto count = name.find_first_not_of('e');
    if (count == name.npos || name.find_first_not_of('A', count) != name.npos)
        throw std::invalid_argument("Expected a charged-fermion prefix and a nonempty photon suffix: " + name);
    std::vector<std::string> charged;
    if (count == 2) charged = {"e", "E"};
    else if (count == 4) charged = {"e", "E", "E", "e"};
    else if (count == 6) charged = {"e", "E", "e", "E", "e", "E"};
    else throw std::invalid_argument("Expected two, four or six charged fermions: " + name);
    return {charged, name.size() - count};
}

std::vector<std::string> external_legs(std::vector<std::string> legs, const std::string& helicities) {
    for (char h : helicities) legs.push_back(h == '+' ? "A.+" : "A.-");
    return legs;
}

// Seven-point runs normally save one Bose representative per photon count. Older
// notebooks can also save separate orderings, for example both +- and -+.
std::map<std::string, std::string> partners(const fs::path& directory, std::size_t photons) {
    std::set<std::string> states;
    std::map<std::size_t, std::string> first_in_class;
    for (const auto& file : fs::directory_iterator(directory)) {
        if (!file.is_regular_file() || file.path().extension() != ".expr") continue;
        const auto name = file.path().stem().string();
        if (name.size() == photons && name.find_first_not_of("+-") == name.npos) states.insert(name);
    }
    for (const auto& state : states) {
        first_in_class.emplace(std::count(state.begin(), state.end(), '+'), state);
    }
    if (first_in_class.size() != photons + 1)
        throw std::runtime_error("Incomplete family: expected a representative for each possible number of positive-helicity photons");
    std::map<std::string, std::string> selected;
    for (const auto& state : states) {
        auto flipped = state;
        for (char& h : flipped) h = h == '+' ? '-' : '+';
        const auto plus = std::count(flipped.begin(), flipped.end(), '+');
        selected[state] = states.count(flipped) ? flipped : first_in_class.at(plus);
    }
    return selected;
}

// Keep all fermion labels. The kth photon of each helicity maps to the kth photon of the
// opposite helicity in the target ordering. Reordering identical photons has positive sign.
std::vector<int> parity_labels(const std::string& from, const std::string& to, std::size_t nf) {
    std::vector<int> labels(1 + nf + from.size());
    std::iota(labels.begin(), labels.end(), 0);
    for (char helicity : {'+', '-'}) {
        std::vector<int> source, target;
        for (std::size_t i = 0; i < from.size(); ++i) {
            const int leg = static_cast<int>(nf + i + 1);
            if (from[i] == helicity) source.push_back(leg);
            if (to[i] != helicity) target.push_back(leg);
        }
        if (source.size() != target.size()) throw std::invalid_argument("Helicities are not parity partners");
        for (std::size_t i = 0; i < source.size(); ++i) labels[source[i]] = target[i];
    }
    return labels;
}

// Walk typed nodes. Relabeling every integer would change exponents, rational coefficients or masses.
Id exchange_chirality(Algebra& algebra, Id root, const std::vector<int>& labels,
                      const std::vector<std::string>& legs, const model& vertices) {
    std::map<Id, Id> cache;
    auto read_label = [&](Id id) {
        const auto i = algebra.leg(id);
        if (i < 1 || static_cast<std::size_t>(i) >= labels.size())
            throw std::invalid_argument("Invalid external label in parity transformation");
        return i;
    };
    auto visit = [&](auto& visit, Id id) -> Id {
        if (auto it = cache.find(id); it != cache.end()) return it->second;
        const auto n = algebra.node(id);
        auto w = n.args;
        if (!n.compound) {
            mpq_class number;
            if (n.head != "I" && !algebra.number(id, number))
                throw std::invalid_argument("Unsupported scalar atom " + n.head);
            return id;
        }
        if (n.head == "ModelParameter" && w.size() == 3) {
            if (algebra.node(w[1]).head != '"' + vertices.parameter_scope() + '"' ||
                algebra.node(w[2]).head != "\"Real\"")
                throw std::invalid_argument("Expected a real model parameter from the chosen vertex model");
            return id;
        }
        if (n.head == "Spinor" && w.size() == 3) {
            const int leg = read_label(w[2]);
            const auto& part = vertices.particle(legs.at(leg - 1));
            const auto& chirality = algebra.node(w[1]).head;
            if ((chirality != "\"Angle\"" && chirality != "\"Square\"") ||
                algebra.node(w[0]).head != (part.mass == "0" ? "\"Helicity\"" : "\"Spin\""))
                throw std::invalid_argument("Unexpected spinor on leg " + std::to_string(leg));
            w[1] = algebra.atom(chirality == "\"Angle\"" ? "\"Square\"" : "\"Angle\"");
            w[2] = algebra.integer(labels[leg]);
        } else if (n.head == "Multiparticle" && !w.empty()) {
            std::set<int> momenta;
            for (Id k : w) momenta.insert(labels[read_label(k)]);
            if (momenta.size() != w.size()) throw std::invalid_argument("Repeated label in channel momentum");
            w.clear();
            for (int k : momenta) w.push_back(algebra.integer(k));
        } else if (n.head == "Mom" && w.size() == 1) {
            if (algebra.node(w[0]).head == "Multiparticle") w[0] = visit(visit, w[0]);
            else w[0] = algebra.integer(labels[read_label(w[0])]);
        } else if (n.head == "MomProd" && w.size() == 2) {
            for (auto& leg : w) leg = algebra.integer(labels[read_label(leg)]);
        } else if ((n.head == "SpinorChain" && w.size() >= 2) || n.head == "Times" || n.head == "Plus" ||
                   ((n.head == "PropDen" || n.head == "Power" || n.head == "Rational") && w.size() == 2)) {
            for (auto& item : w) item = visit(visit, item);
        } else throw std::invalid_argument("Unsupported expression head or arity: " + n.head);
        return cache[id] = algebra.raw(n.head, std::move(w), true);
    };
    return visit(visit, root);
}

std::string label_list(const std::vector<int>& labels) {
    std::ostringstream os;
    for (std::size_t i = 1; i < labels.size(); ++i) {
        if (i > 1) os << ',';
        os << labels[i];
    }
    return os.str();
}
void check_partner(const settings& s, const model& vertices, const model& cuts, const model& feynman,
                   const std::vector<std::string>& charged, bool spurious, const std::string& source,
                   const std::string& target, const fs::path& output, std::ostream& manifest) {
    const auto legs = external_legs(charged, target);
    amplitude process(legs, vertices, cuts, feynman, spurious ? generation_references::cyclic : generation_references::physical);
    const auto& masses = process.external_masses();
    const auto labels = parity_labels(source, target, charged.size());
    amp_expression automatic(read(s.directory / (target + ".expr")), masses);
    Algebra algebra;
    auto id = exchange_chirality(algebra, algebra.parse(read(s.directory / (source + ".expr"))),
                                 labels, external_legs(charged, source), vertices);
    // Both possible signs are compared to the signed automatic result at the same exact points.
    std::optional<amp_expression> matched;
    int sign = 0;
    for (int hypothesis : {1, -1}) {
        const auto value = hypothesis == 1 ? id : algebra.mul({algebra.integer(-1), id});
        amp_expression expression(algebra.format(value), masses);
        auto trial = automatic.check_identity(expression, {3, sign_seed});
        if (trial.status == identity_check_status::unresolved) throw std::runtime_error(trial.detail);
        if (trial.passed()) {
            if (matched) throw std::runtime_error("Both phases agreed for " + target);
            sign = hypothesis;
            matched = std::move(expression);
        }
    }
    std::cout << source << " -> " << target << "   " << automatic.leaf_count() << " leaves" << std::endl;
    manifest << source << '\t' << target << '\t' << label_list(labels) << '\t' << sign << '\t'
             << sign_seed << '\t' << identity_seed << '\t' << feynman_seed << '\t'
             << automatic.complexity() << '\t' << automatic.leaf_count() << '\t';
    if (!matched) {
        manifest << "\t\tNO_SIGNED_MATCH\t0\t0\n";
        return;
    }
    manifest << matched->complexity() << '\t' << matched->leaf_count() << '\t';
    if (matched->complexity() > automatic.complexity() || matched->leaf_count() >= automatic.leaf_count()) {
        manifest << "NO_REDUCTION\t0\t0\n";
        return;
    }
    auto identity = automatic.check_identity(*matched, {32, identity_seed});
    if (!identity.passed() || identity.compared != 32) throw std::runtime_error(identity.detail);
    validation_options options;
    options.seed = feynman_seed;
    options.points = feynman_points;
    options.workers = s.workers;
    std::cout << "    Checking signed Feynman values and every factorization channel" << std::endl;
    auto validated = process.validate(*matched, options);
    std::ofstream receipt(output / (target + ".checks.txt"));
    notebook_output::print_validation(validated, receipt);
    receipt.close();
    if (!receipt) throw std::runtime_error("Could not save the validation report");
    const auto has_feynman = std::any_of(validated.numerical.begin(), validated.numerical.end(),
        [](const auto& row) {
            return row.kind == "Feynman" && row.status == check_outcome::pass && row.compared == feynman_points;
        });
    std::set<std::string> kinds;
    for (const auto& check : validated.checks) kinds.insert(check.kind);
    if (!validated.passed() || !has_feynman || !kinds.count("Physical") || !kinds.count("Reference") ||
        !kinds.count("HighEnergy") || !kinds.count("Spurious"))
        throw std::runtime_error(target + " failed direct validation or has an incomplete check inventory");
    notebook_output::write_expression(output / target, *matched);
    manifest << "SHORTER_VALIDATED\t" << identity.compared << '\t' << feynman_points << '\n';
}

fs::path make_stage(const fs::path& directory, const std::string& stamp) {
    for (unsigned attempt = 0; ; ++attempt) {
        auto name = "parity-alternatives.partial-" + stamp + "-" + std::to_string(attempt);
        const auto stage = directory / name;
        if (fs::create_directory(stage)) return stage;
    }
}

fs::path publish(const fs::path& stage, const fs::path& directory, const std::string& stamp) {
    for (unsigned attempt = 0; ; ++attempt) {
        std::string name = "parity-alternatives";
        if (attempt) name += "-" + stamp + "-" + std::to_string(attempt);
        const auto destination = directory / name;
        if (fs::exists(destination)) continue;

        // A published directory always contains manifest.tsv. An atomic rename of a
        // directory cannot replace a nonempty directory, even if a concurrent publisher
        // wins between the existence check and rename. Use a fresh name in that case.
        std::error_code error;
        fs::rename(stage, destination, error);
        if (!error) return destination;
        if (!fs::is_directory(destination))
            throw fs::filesystem_error("Could not publish parity study", stage, destination, error);
    }
}

}

int main(int argc, char** argv) {
    try {
        const auto s = arguments(argc, argv);
        const auto family = parse_family(s.directory.filename().string());
        const auto policy = s.directory.parent_path().filename().string();
        if (policy != "physical" && policy != "spurious") throw std::invalid_argument("Expected physical or spurious directory");
        const bool spurious = policy == "spurious";
        const model vertices(s.root / "models", s.vertices);
        const model cuts(s.root / "models", s.cuts ? s.cuts : (spurious ? 5 : 4));
        const model feynman(s.root / "tests/reference-models/calchep", s.feynman);
        const auto& electron = vertices.particle("e");
        const auto& photon = vertices.particle("A.+");
        if (electron.pdg != 11 || electron.spin2 != 1 || electron.mass == "0" || electron.antiname != "E" ||
            photon.pdg != 22 || photon.spin2 != 2 || photon.mass != "0" || photon.antiname != "A.-" ||
            vertices.particle("E").mass != electron.mass || vertices.particle("A.-").mass != "0")
            throw std::invalid_argument("Expected a QED model with a massive electron and massless photons");
        const auto helicity_partners = partners(s.directory, family.photons);
        const auto timestamp = std::chrono::steady_clock::now().time_since_epoch().count();
        const auto stamp = std::to_string(timestamp);
        const auto stage = make_stage(s.directory, stamp);
        std::ofstream manifest(stage / "manifest.tsv");
        manifest << "source\ttarget\tsource_to_target_labels\toverall_sign\tsign_seed\tidentity_seed\tfeynman_seed\tautomatic_weight\tautomatic_leaves\t"
                    "candidate_weight\tcandidate_leaves\tstatus\tsigned_identity_samples\tsigned_feynman_points\n";
        for (const auto& [target, source] : helicity_partners) {
            check_partner(s, vertices, cuts, feynman, family.charged, spurious, source, target,
                          stage, manifest);
            manifest.flush();
        }
        manifest.close();
        if (!manifest) throw std::runtime_error("Could not save the parity manifest");
        const auto output = publish(stage, s.directory, stamp);
        std::cout << "Results in " << output << '\n';
    } catch (const std::exception& e) {
        std::cerr << e.what() << '\n';
        return 1;
    }
}
