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

amplitude& amplitude::generate_diagrams(std::size_t workers) { return generate_rows(true, false, workers); }
amplitude& amplitude::generate_factorization_products(std::size_t workers) {
    return generate_rows(false, true, workers);
}
amplitude& amplitude::generate(std::size_t workers) { return generate_rows(true, channels_model_.has_value(), workers); }

amplitude& amplitude::generate_rows(bool diagrams, bool products, std::size_t workers) {
    if (!vertices_model_) throw std::logic_error("supply particles and a model to generate amplitude rows");
    if (products && !channels_model_)
        throw std::logic_error("supply a factorization model to generate channels");
    diagrams = diagrams && !diagrams_generated_;
    products = products && !channels_generated_;
    if (!diagrams && !products) {
        last_workers_ = 0;
        return *this;
    }
    auto dp =
        diagrams ? generation::diagram_plans(*vertices_model_, particles_) : std::vector<generation::plan>{};
    if (diagrams && !vertices_model_->is_feynman())
        generation::resolve_diagram_colors(dp, *vertices_model_, particles_);
    auto cp =
        products ? generation::channel_plans(*channels_model_, particles_) : std::vector<generation::plan>{};
    std::vector<std::optional<amplitude_item>> generated(dp.size() + cp.size());
    const auto count = generated.size();
    const auto threads =
        std::min(count, workers ? workers : std::max<std::size_t>(1, std::thread::hardware_concurrency()));
    // Largest lower amplitudes first. Output slots retain planner order.
    std::vector<std::size_t> order(count), costs(count);
    std::iota(order.begin(), order.end(), 0);
    auto visit = [&](std::size_t i, auto f) {
        if (i < dp.size()) return f(*vertices_model_, dp[i], false);
        return f(*channels_model_, cp[i - dp.size()], true);
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
                amplitude_item item("0", masses_, std::move(shells));
                if (m.is_feynman())
                    item.expression.set_from(tree, root);
                else
                    item.expression.reduce_from(tree, root);
                item.reduced_ = true;
                generated[i] = std::move(item);
            } catch (const std::exception& e) {
                throw std::runtime_error(std::string(channel ? "factorization channel " : "diagram ") +
                                         std::to_string(channel ? i - dp.size() + 1 : i + 1) + ": " + e.what());
            }
        });
    });
    // Reserve both before publishing either one, including on retry after an
    // allocation or worker failure. Hand-added rows retain their positions.
    diagrams_.reserve(diagrams_.size() + dp.size());
    products_.reserve(products_.size() + cp.size());
    for (std::size_t i = 0; i < count; ++i)
        if (generated[i]) (i < dp.size() ? diagrams_ : products_).push_back(std::move(*generated[i]));
    diagrams_generated_ |= diagrams;
    channels_generated_ |= products;
    completed_.reset();
    high_energy_result_.reset();
    last_workers_ = threads;
    return *this;
}
} // namespace hepcat
