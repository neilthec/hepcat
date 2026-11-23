#include "generation_plan.hpp"
#include "generation_feynman.hpp"
#include "model.hpp"
#include <cctype>
#include <tuple>

#include <algorithm>
#include <cstdint>
#include <map>
#include <set>
#include <stdexcept>
#include <unordered_map>

namespace hepcat::generation {
namespace {
using mask = std::uint64_t;
using species = std::size_t;
using signature = std::vector<species>;

// Labels stay attached to leaves. A subset contains the legs on the side
// away from the distinguished root leg. This identifies each edge without
// allocating graph objects or performing graph isomorphism comparisons.
unsigned first_bit(mask x) {
#if defined(__GNUC__) || defined(__clang__)
    return static_cast<unsigned>(__builtin_ctzll(x));
#else
    unsigned i = 0;
    while (!(x & 1)) {
        ++i;
        x >>= 1;
    }
    return i;
#endif
}
unsigned degree(mask x) {
#if defined(__GNUC__) || defined(__clang__)
    return static_cast<unsigned>(__builtin_popcountll(x));
#else
    unsigned n = 0;
    while (x) {
        ++n;
        x &= x - 1;
    }
    return n;
#endif
}
std::vector<int> labels(mask x) {
    std::vector<int> out;
    while (x) {
        out.push_back(1 + static_cast<int>(first_bit(x)));
        x &= x - 1;
    }
    return out;
}
mask full_set(std::size_t n) {
    if (n > 64) throw std::length_error("native generation supports at most 64 external labels");
    return n == 64 ? ~mask(0) : (mask(1) << n) - 1;
}
struct candidate {
    std::size_t row;
    species port;
};
struct incidence {
    // External and fused legs offered to one model vertex in tagged order.
    species type;
    int slot;
};
class inventory {
  public:
    const model& m;
    std::vector<std::string> names;
    std::map<std::string, species> id;
    std::vector<species> anti;
    std::vector<bool> fermion, dirac, antiparticle;
    std::vector<species> internal_kind, lexical_rank;
    std::vector<signature> ports;

    std::map<signature, std::vector<candidate>> openings;
    std::set<unsigned> arities;
    signature ext;

    inventory(const model& input, const std::vector<std::string>& particles, bool diagrams) : m(input) {
        for (const auto& p : m.particles()) {
            for (const auto* name : {&p.name, &p.antiname}) {
                if (id.emplace(*name, names.size()).second) names.push_back(*name);
            }
        }
        auto unphysical = [](const std::string& p) {
            return p.size() > 2 && p[p.size() - 2] == '.' &&
                   std::string("fcC").find(p.back()) != std::string::npos;
        };
        if (m.is_feynman())
            for (const auto& v : m.vertices())
                for (const auto& p : v.legs)
                    if (p.size() > 2 && p.compare(p.size() - 2, 2, ".t") == 0 &&
                        id.emplace(p, names.size()).second)
                        names.push_back(p);
        for (const auto& name : names) {
            const auto& p = m.particle(name);
            auto conjugate = id.at(m.antiparticle(name));
            anti.push_back(conjugate);
            fermion.push_back(p.spin2 % 2 != 0);
            dirac.push_back(p.spin2 == 1);
            antiparticle.push_back(p.antiname == name);
            // A massless vector's two helicities share a diagram topology.
            // External leaves always carry their original, exact helicity.
            bool helicity_pair =
                name.size() > 2 && name[name.size() - 2] == '.' && (name.back() == '+' || name.back() == '-') &&
                p.mass == "0" && p.spin2 == 2 && id.find(name.substr(0, name.size() - 2)) == id.end() &&
                names[conjugate] == name.substr(0, name.size() - 1) + (name.back() == '+' ? "-" : "+");
            internal_kind.push_back(helicity_pair ? std::min(id.at(name), conjugate) : id.at(name));
        }
        // Wolfram canonical string order for the usual particle labels sorts
        // case-insensitively and breaks ties with lowercase first.
        auto compare_names = [](const std::string& a, const std::string& b) {
            auto collate = [](unsigned char c) {
                if (c == '-') return int('+');
                if (c == '+') return int('-');
                return std::tolower(c);
            };
            auto precedes = [&](char x, char y) { return collate(x) < collate(y); };
            if (std::lexicographical_compare(a.begin(), a.end(), b.begin(), b.end(), precedes)) return true;
            if (std::lexicographical_compare(b.begin(), b.end(), a.begin(), a.end(), precedes)) return false;
            return a > b;
        };
        auto sorted_names = names;
        std::sort(sorted_names.begin(), sorted_names.end(), compare_names);
        lexical_rank.resize(names.size());
        for (std::size_t i = 0; i < sorted_names.size(); ++i)
            lexical_rank[id.at(sorted_names[i])] = i;
        for (const auto& p : particles) {
            auto it = id.find(m.is_feynman() ? feynman_external(m, p) : p);
            if (it == id.end()) throw std::invalid_argument("unknown external particle: " + p);
            ext.push_back(it->second);
        }
        std::map<signature, std::vector<std::size_t>> rows;
        std::set<std::tuple<signature, std::uint32_t, std::uint32_t>> seen;
        for (const auto& row : m.vertices()) {
            if (m.is_feynman() && std::any_of(row.legs.begin(), row.legs.end(), unphysical)) {
                ports.emplace_back();
                continue;
            }
            signature ps;
            for (const auto& p : row.legs)
                ps.push_back(id.at(p));
            auto sorted = ps;
            std::sort(sorted.begin(), sorted.end());
            if (!m.is_feynman())
                rows[sorted] = {ports.size()}; // last matching constructive amplitude
            else if (seen.emplace(ps, row.factor, row.expression).second)
                rows[sorted].push_back(ports.size());
            ports.push_back(std::move(ps));
        }

        for (const auto& [key, candidates] : rows) {
            if (key.size() < 3 || key.size() > ext.size() || (!diagrams && key.size() == ext.size())) continue;
            arities.insert(static_cast<unsigned>(key.size() - 1));
            for (std::size_t i = 0; i < key.size(); ++i) {
                if (i && key[i] == key[i - 1]) continue;
                auto consumed = key;
                consumed.erase(consumed.begin() + i);
                for (auto row : candidates)
                    openings[consumed].push_back({row, key[i]});
            }
        }
    }

    vertex attach(std::size_t row, const std::vector<incidence>& incoming) const {
        vertex placed{row, {}};
        std::vector<bool> taken(incoming.size());
        for (auto p : ports[row]) {
            auto i = std::size_t(0);
            while (i < incoming.size() && (taken[i] || incoming[i].type != p))
                ++i;
            if (i == incoming.size()) throw std::logic_error("incomplete model vertex assignment");
            placed.slots.push_back(incoming[i].slot);
            taken[i] = true;
        }
        return placed;
    }
};

template <class T> int parity(const std::vector<T>& word) {
    int s = 1;
    for (std::size_t j = 0; j < word.size(); ++j)
        for (std::size_t i = 0; i < j; ++i)
            if (word[j] < word[i]) s = -s;
    return s;
}
using ordered_port = std::pair<int, int>; // particle/antiparticle role and signed leg label

int channel_sign(const inventory& md, const term& t) {
    std::vector<ordered_port> word;
    int pairs[2]{};
    for (int v = 0; v < 2; ++v) {
        const auto& vert = t.vertices[v];
        int count[2]{};
        const auto& ps = md.ports[vert.row];
        for (int role = 0; role < 2; ++role)
            for (std::size_t j = 0; j < ps.size(); ++j) {
                auto p = ps[j];
                if (!md.dirac[p] || md.antiparticle[p] != bool(role)) continue;
                if (md.anti[p] == p)
                    throw std::invalid_argument("factorization requires unambiguous Dirac ports");
                ++count[role];
                word.emplace_back(role, vert.slots[j]);
            }
        if (count[0] != count[1]) throw std::invalid_argument("factorization requires balanced Dirac ports");
        pairs[v] = count[0];
    }
    std::vector<std::size_t> internal;
    std::vector<ordered_port> survivors;
    for (std::size_t i = 0; i < word.size(); ++i)
        if (word[i].second < 0)
            internal.push_back(i);
        else
            survivors.push_back(word[i]);
    int sign = parity(survivors);
    if (!internal.empty()) {
        if (internal.size() != 2) throw std::logic_error("unpaired internal fermion");
        if ((internal[1] - internal[0] - 1) % 2) sign = -sign;
        if (word[internal[0]].first == 0) sign = -sign;
        --pairs[0];
        --pairs[1];
    }
    return pairs[0] % 2 && pairs[1] % 2 ? -sign : sign;
}

int diagram_sign(const inventory& md, const term& t) {
    std::set<species> used;
    bool repeats = false;
    for (auto p : md.ext)
        if (md.fermion[p] && !used.insert(p).second) repeats = true;
    if (!repeats) return 1;

    // The side of a fermion line is fixed by the ordered ports of its model
    // vertex, before a vector exchange is transformed into a local tensor.
    std::vector<std::vector<int>> ports(t.vertices.size());
    std::vector<std::size_t> owner(md.ext.size() + 1);
    for (std::size_t i = 0; i < t.vertices.size(); ++i) {
        auto& v = t.vertices[i];
        for (std::size_t j = 0; j < v.slots.size(); ++j)
            if (md.fermion[md.ports[v.row][j]]) {
                ports[i].push_back(v.slots[j]);
                if (v.slots[j] > 0) owner[v.slots[j]] = i;
            }
        if (ports[i].size() != 0 && ports[i].size() != 2)
            throw std::invalid_argument("identical external fermions require bilinear diagram vertices");
    }
    std::vector<std::pair<int, int>> lines;
    for (int l = 1; std::size_t(l) <= md.ext.size(); ++l) {
        if (!md.fermion[md.ext[l - 1]]) continue;
        auto v = owner[l];
        bool forward = ports[v].front() == l;
        int previous = l, next;
        for (;;) {
            const auto& legs = ports[v];
            next = legs[legs.front() == previous ? 1 : 0];
            if (next > 0) break;
            const auto& e = t.edges[-1 - next];
            v = v == e.a ? e.b : e.a;
            previous = next;
        }
        if (next > l) lines.emplace_back(forward ? l : next, forward ? next : l);
    }
    // Only permutations within identical species produce a sign. The
    // relative order of two different species groups cancels in the parity.
    std::sort(lines.begin(), lines.end(), [&](auto x, auto y) {
        return std::make_tuple(md.lexical_rank[md.ext[x.first - 1]], md.lexical_rank[md.ext[x.second - 1]],
                               std::min(x.first, x.second)) <
               std::make_tuple(md.lexical_rank[md.ext[y.first - 1]], md.lexical_rank[md.ext[y.second - 1]],
                               std::min(y.first, y.second));
    });
    std::map<species, std::vector<int>> sequences;
    for (auto line : lines)
        for (int l : {line.first, line.second})
            sequences[md.ext[l - 1]].push_back(l);
    int sign = 1;
    for (const auto& [p, word] : sequences)
        sign *= parity(word);
    return sign;
}

struct current {
    mask legs;
    species type; // visible at the parent, conjugate of the open port at this node
    std::size_t row;
    std::vector<const current*> children;
};

class diagram_search {
    const inventory& md;
    std::unordered_map<mask, std::vector<current>> cache;
    using group_key = std::vector<std::pair<mask, species>>;
    std::map<group_key, std::size_t> groups;
    std::vector<plan> plans;

    template <class F>
    static void partitions(mask remaining, unsigned count, std::vector<mask>& blocks, F& emit) {
        if (count == 1) {
            blocks.push_back(remaining);
            emit(blocks);
            blocks.pop_back();
            return;
        }
        if (count == degree(remaining)) {
            auto length = blocks.size();
            while (remaining) {
                auto singleton = remaining & -remaining;
                blocks.push_back(singleton);
                remaining ^= singleton;
            }
            emit(blocks);
            blocks.resize(length);
            return;
        }
        // Put the least remaining label in the next block. This enumerates
        // unordered set partitions once, independently of model slot order.
        mask first = remaining & -remaining, others = remaining ^ first;
        mask subset = others;
        for (;;) {
            auto rest = others ^ subset;
            if (degree(rest) >= count - 1) {
                blocks.push_back(first | subset);
                partitions(rest, count - 1, blocks, emit);
                blocks.pop_back();
            }
            if (!subset) break;
            subset = (subset - 1) & others;
        }
    }

    const std::vector<current>& currents(mask legs) {
        auto [it, new_key] = cache.try_emplace(legs);
        auto& out = it->second;
        if (!new_key) return out;
        auto count = degree(legs);
        if (count == 1) {
            out.push_back({legs, md.ext[first_bit(legs)], 0, {}});
            return out;
        }
        std::vector<mask> blocks;
        auto emit = [&](const std::vector<mask>& groups) {
            std::vector<const std::vector<current>*> choices;
            for (auto block : groups) {
                const auto& available = currents(block);
                if (available.empty()) return;
                choices.push_back(&available);
            }
            std::vector<const current*> selected;
            auto product = [&](auto& self, std::size_t k) -> void {
                if (k < choices.size()) {
                    for (const auto& item : *choices[k]) {
                        selected.push_back(&item);
                        self(self, k + 1);
                        selected.pop_back();
                    }
                    return;
                }
                signature sig;
                for (const auto* c : selected)
                    sig.push_back(c->type);
                std::sort(sig.begin(), sig.end());
                auto matches = md.openings.find(sig);
                if (matches == md.openings.end()) return;
                for (auto c : matches->second)
                    out.push_back({legs, md.anti[c.port], c.row, selected});
            };
            product(product, 0);
        };
        for (auto arity : md.arities)
            if (arity <= count) partitions(legs, arity, blocks, emit);
        return out;
    }

    void place(const current& c, std::size_t v, int open, term& built, group_key& key) {
        std::vector<incidence> legs{{md.anti[c.type], open}};
        for (const auto* child : c.children) {
            if (child->children.empty()) {
                legs.push_back({child->type, 1 + static_cast<int>(first_bit(child->legs))});
                continue;
            }
            auto index = built.edges.size();
            auto node = built.vertices.size();
            int slot = -1 - static_cast<int>(index);
            auto p = md.anti[child->type], q = child->type;
            key.emplace_back(child->legs, md.internal_kind[p]);
            // For self-conjugate internal legs either orientation gives the
            // same spin sum. Use the child (built before its parent in the
            // corresponding current recursion) as the particle endpoint.
            built.edges.push_back({labels(child->legs), node, v, md.names[p], md.names[q], p <= q ? node : v});
            built.vertices.emplace_back();
            legs.push_back({q, slot});
            place(*child, node, slot, built, key);
        }
        // Last matching permutation in DiagramAmplitudes. External labels
        // and internal source IDs are ordered independently of species.
        auto source = [&](int leg) {
            if (leg > 0) return static_cast<std::size_t>(leg);
            const auto& e = built.edges[-1 - leg];
            return 1 + md.ext.size() + (v == e.a ? e.b : e.a);
        };
        std::sort(legs.begin(), legs.end(), [&](auto a, auto b) {
            return md.m.is_feynman() ? source(a.slot) < source(b.slot) : source(a.slot) > source(b.slot);
        });
        built.vertices[v] = md.attach(c.row, legs);
    }

  public:
    explicit diagram_search(const inventory& m) : md(m) {}
    std::vector<plan> run() {
        // Rooting every labeled tree at external leg 1 removes arbitrary
        // fusion orders. Memoized subcurrents share child descriptors; no
        // symbolic expression is allocated until its worker starts sewing.
        auto all = full_set(md.ext.size());
        for (const auto& c : currents(all ^ mask(1))) {
            if (md.anti[c.type] != md.ext.front()) continue;
            term t;
            group_key key;
            t.vertices.emplace_back();
            place(c, 0, 1, t, key);
            std::sort(key.begin(), key.end());
            if (!md.m.is_feynman()) t.sign = diagram_sign(md, t);
            auto [it, added] = groups.emplace(key, plans.size());
            if (added) plans.emplace_back();
            plans[it->second].terms.push_back(std::move(t));
        }
        return std::move(plans);
    }
};

struct tagged_side {
    std::vector<incidence> legs;
    const std::vector<candidate>* matches = nullptr;
    tagged_side(const inventory& md, mask subset) {
        signature sig;
        // RetreiveAmplitude tags external labels in increasing order for equal
        // particle names. The internal port comes last.
        for (int label : labels(subset)) {
            auto p = md.ext[label - 1];
            legs.push_back({p, label});
            sig.push_back(p);
        }
        std::sort(sig.begin(), sig.end());
        auto found = md.openings.find(sig);
        if (found != md.openings.end()) matches = &found->second;
    }
    vertex attach(const inventory& md, const candidate& c) {
        legs.push_back({c.port, -1});
        auto v = md.attach(c.row, legs);
        legs.pop_back();
        return v;
    }
};

template <class F> void combinations(unsigned n, unsigned k, unsigned start, mask prefix, const F& f) {
    if (!k) {
        f(prefix);
        return;
    }
    for (unsigned i = start; i + k <= n; ++i)
        combinations(n, k - 1, i + 1, prefix | (mask(1) << i), f);
}
} // namespace

std::vector<plan> diagram_plans(const model& m, const std::vector<std::string>& process) {
    full_set(process.size());
    inventory md(m, process, true);
    if (process.size() < 3) return {};
    return diagram_search(md).run();
}

std::vector<plan> channel_plans(const model& m, const std::vector<std::string>& process) {
    if (m.is_feynman()) throw std::invalid_argument("factorization requires on-shell model amplitudes");
    mask full = full_set(process.size());
    inventory md(m, process, false);
    std::vector<std::size_t> species_row(md.names.size());
    for (std::size_t i = 0; i < m.particles().size(); ++i) {
        const auto& p = m.particles()[i];
        species_row[md.id.at(p.name)] = i;
        species_row[md.id.at(p.antiname)] = i;
    }
    std::vector<std::vector<plan>> by_particle(m.particles().size());
    // Intersect the lower-amplitude signatures on the two sides. A cut
    // with no allowed state incurs two lookups and no symbolic work.
    for (unsigned k = 2; k <= process.size() / 2; ++k)
        combinations(static_cast<unsigned>(process.size()), k, 0, 0, [&](mask left) {
            if (k * 2 == process.size() && !(left & 1)) return;
            tagged_side l(md, left);
            if (!l.matches) return;
            tagged_side r(md, full ^ left);
            if (!r.matches) return;
            std::map<std::size_t, plan> cut;
            for (const auto& lc : *l.matches) {
                const auto q = md.anti[lc.port];
                auto rc = std::find_if(r.matches->begin(), r.matches->end(),
                                       [&](const candidate& c) { return c.port == q; });
                if (rc == r.matches->end()) continue;
                auto row = species_row[lc.port];
                const auto& name = md.names[lc.port];
                std::size_t upper = name == m.particles()[row].name ? 0 : 1;
                term t;
                t.vertices = {l.attach(md, lc), r.attach(md, *rc)};
                t.edges.push_back({labels(left), 0, 1, name, md.names[q], upper});
                t.sign = channel_sign(md, t);
                cut[row].terms.push_back(std::move(t));
            }
            for (auto& [row, p] : cut) {
                if (p.terms.size() == 2 && p.terms.front().edges.front().upper != 0)
                    std::swap(p.terms[0], p.terms[1]);
                by_particle[row].push_back(std::move(p));
            }
        });
    // Particle-file order, then subset size and lexicographic leg indices,
    // agrees with FactorizationAmplitudeChannels.
    std::vector<plan> result;
    for (auto& cuts : by_particle)
        for (auto& c : cuts)
            result.push_back(std::move(c));
    return result;
}
} // namespace hepcat::generation
