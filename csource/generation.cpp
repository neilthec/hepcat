#include "amplitude.hpp"
#include "generation_plan.hpp"
#include "generation_sew.hpp"
#include "generation_current.hpp"
#include "generation_feynman.hpp"
#include "native_fit.hpp"

#include <algorithm>
#include <numeric>
#include <thread>

namespace hepcat {

amplitude::amplitude(std::vector<std::string> particles, model vertices, generation_references references)
    : particles_(std::move(particles)), vertices_model_(std::move(vertices)), references_(references) {
    if (particles_.size() < 3)
        throw std::invalid_argument("native tree generation requires at least three external legs");
    for (const auto& name : particles_) {
        auto vertex_name =
            vertices_model_->is_feynman() ? generation::feynman_external(*vertices_model_, name) : name;
        masses_.push_back(vertices_model_->particle(vertex_name).mass);
    }
}
amplitude::amplitude(std::vector<std::string> particles, model vertices, model channels,
                     generation_references references)
    : amplitude(std::move(particles), std::move(vertices), references) {
    if (channels.is_feynman())
        throw std::invalid_argument("factorization requires complete on-shell amplitudes from ampsN.mdl");
    for (std::size_t i = 0; i < particles_.size(); ++i) {
        const auto& name = particles_[i];
        auto vertex_name =
            vertices_model_->is_feynman() ? generation::feynman_external(*vertices_model_, name) : name;
        const auto& v = vertices_model_->particle(vertex_name);
        const auto& c = channels.particle(name);
        if (v.mass != c.mass || v.spin2 != c.spin2 || v.color != c.color ||
            (!vertices_model_->is_feynman() &&
             vertices_model_->antiparticle(name) != channels.antiparticle(name)))
            throw std::invalid_argument("incompatible particle definitions between generation models for " +
                                        name +
                                        "; independent Feynman diagrams can use the vertex-only constructor");
    }
    channels_model_ = std::move(channels);
}

amplitude::amplitude(std::vector<std::string> particles, model vertices, model channels,
                     model feynman, generation_references references)
    : amplitude(std::move(particles), std::move(vertices), std::move(channels), references) {
    set_feynman_model(std::move(feynman));
}

amplitude& amplitude::set_feynman_model(model rules) {
    if (!vertices_model_ || vertices_model_->is_feynman() || !rules.is_feynman())
        throw std::invalid_argument("attach a CalcHEP Feynman model to a constructive process");
    for (const auto& name : particles_) {
        const auto& v = vertices_model_->particle(name);
        const auto mapped = generation::feynman_external(rules, name);
        const auto conjugate = generation::feynman_external(rules, vertices_model_->antiparticle(name));
        const auto& f = rules.particle(mapped);
        const bool opposite_orientation = f.name != f.antiname && (name == v.name) != (mapped == f.name);
        const bool chiral_fermion = v.spin2 == 1 && v.mass == "0";
        if (v.spin2 != f.spin2 || v.color != f.color || (v.mass == "0") != (f.mass == "0") ||
            rules.antiparticle(mapped) != conjugate || opposite_orientation ||
            (v.pdg && f.pdg && v.pdg != f.pdg) || (chiral_fermion && v.auxiliary != f.auxiliary))
            throw std::invalid_argument("incompatible Feynman particle definition for " + name);
    }
    feynman_model_ = std::move(rules);
    construction_.reset();
    feynman_rows_.clear();
    feynman_generated_ = false;
    return *this;
}

amplitude& amplitude::generate_diagrams(std::size_t workers) {
    return generate_rows(true, false, false, workers);
}
amplitude& amplitude::generate_factorization_products(std::size_t workers) {
    return generate_rows(false, true, false, workers);
}
amplitude& amplitude::generate_feynman_diagrams(std::size_t workers) {
    return generate_rows(false, false, true, workers);
}
amplitude& amplitude::generate(std::size_t workers) {
    return generate_rows(true, channels_model_.has_value(), feynman_model_.has_value(), workers);
}

amplitude& amplitude::generate_rows(bool diagrams, bool products, bool feynman, std::size_t workers) {
    if (!vertices_model_) throw std::logic_error("supply particles and a model to generate amplitude rows");
    if (products && !channels_model_)
        throw std::logic_error("supply a factorization model to generate channels");
    if (feynman && !feynman_model_) throw std::logic_error("attach a Feynman model first");
    feynman = feynman && !feynman_generated_;
    diagrams = diagrams && !diagrams_generated_;
    products = products && !channels_generated_;
    if (!diagrams && !products && !feynman) {
        last_workers_ = 0;
        return *this;
    }
    auto dp =
        diagrams ? generation::diagram_plans(*vertices_model_, particles_) : std::vector<generation::plan>{};
    if (diagrams && !vertices_model_->is_feynman())
        generation::resolve_diagram_colors(dp, *vertices_model_, particles_);
    auto cp =
        products ? generation::channel_plans(*channels_model_, particles_) : std::vector<generation::plan>{};
    auto fp = feynman ? generation::diagram_plans(*feynman_model_, particles_) : std::vector<generation::plan>{};
    const auto first_feynman = dp.size() + cp.size();
    std::vector<std::string> feynman_masses;
    if (feynman)
        for (const auto& name : particles_)
            feynman_masses.push_back(feynman_model_->particle(generation::feynman_external(*feynman_model_, name)).mass);
    std::vector<std::optional<amplitude_item>> generated(first_feynman + fp.size());
    const auto count = generated.size();
    const auto threads =
        std::min(count, workers ? workers : std::max<std::size_t>(1, std::thread::hardware_concurrency()));
    // Largest lower amplitudes first. Output slots retain planner order.
    std::vector<std::size_t> order(count), costs(count);
    std::iota(order.begin(), order.end(), 0);
    auto visit = [&](std::size_t i, auto f) {
        if (i < dp.size()) return f(*vertices_model_, dp[i], false);
        if (i < first_feynman) return f(*channels_model_, cp[i - dp.size()], true);
        return f(*feynman_model_, fp[i - first_feynman], false);
    };
    if (threads > 1) {
        for (auto i : order)
            visit(i, [&](const model& m, const generation::plan& p, bool) {
                for (const auto& t : p.terms)
                    for (const auto& v : t.vertices)
                        costs[i] += m.algebra().cost(m.vertices()[v.row].expression);
            });
        std::stable_sort(order.begin(), order.end(), [&](auto a, auto b) { return costs[a] > costs[b]; });
    }
    fit_detail::parallel_indices(count, threads, [&](std::size_t position) {
        const auto i = order[position];
        visit(i, [&](const model& m, const generation::plan& p, bool channel) {
            try {
                if (p.terms.empty()) throw std::logic_error("empty diagram or channel plan");
                Algebra tree(&m.algebra());
                auto root = m.is_feynman() ? generation::sew_feynman(tree, m, particles_, p)
                                           : generation::sew(tree, m, particles_, p, channel,
                                                             references_ != generation_references::keep,
                                                             references_ == generation_references::physical);
                if (channel && root == tree.integer(0)) return;
                std::vector<numerator_shell> shells;
                if (!m.is_feynman())
                    for (const auto& edge : p.terms.front().edges) {
                        auto cut = edge.legs;
                        if (!channel && !std::binary_search(cut.begin(), cut.end(), 1)) {
                            cut.clear();
                            for (int k = 1; k <= static_cast<int>(particles_.size()); ++k)
                                if (!std::binary_search(edge.legs.begin(), edge.legs.end(), k))
                                    cut.push_back(k);
                        }
                        shells.push_back({std::move(cut), m.particle(edge.particle_a).mass});
                    }
                amplitude_item item("0", i >= first_feynman ? feynman_masses : masses_, std::move(shells));
                if (m.is_feynman())
                    item.expression.set_from(tree, root);
                else
                    item.expression.reduce_from(tree, root);
                item.reduced_ = true;
                item.generated_ = true;
                generated[i] = std::move(item);
            } catch (const std::exception& e) {
                const auto label = channel ? "factorization channel " : (m.is_feynman() ? "Feynman diagram " : "diagram ");
                const auto offset = i >= first_feynman ? first_feynman : (channel ? dp.size() : 0);
                throw std::runtime_error(std::string(label) + std::to_string(i - offset + 1) + ": " + e.what());
            }
        });
    });
    // Reserve both before publishing either one, including on retry after an
    // allocation or worker failure. Hand-added rows retain their positions.
    diagrams_.reserve(diagrams_.size() + dp.size());
    products_.reserve(products_.size() + cp.size());
    feynman_rows_.reserve(feynman_rows_.size() + fp.size());
    for (std::size_t i = 0; i < count; ++i) {
        if (!generated[i]) continue;
        auto& rows = i < dp.size() ? diagrams_ : (i < first_feynman ? products_ : feynman_rows_);
        rows.push_back(std::move(*generated[i]));
    }
    construction_.reset();
    diagrams_generated_ |= diagrams;
    channels_generated_ |= products;
    feynman_generated_ |= feynman;
    if (diagrams || products) {
        completed_.reset();
        high_energy_result_.reset();
        before_final_reduction_.reset();
    }
    last_workers_ = threads;
    return *this;
}
} // namespace hepcat
