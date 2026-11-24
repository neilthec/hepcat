// Cut a minimal single-flavor Abelian tree at the center of its charged leaves.
// Masks are in external-leg order. Each wave is tabulated only for its local spins.
#pragma once
#include <array>
#include <complex>
#include <cstddef>
#include <memory>
#include <utility>
#include <vector>
#include <map>
#include <string>

namespace hepcat {
class model;
namespace validation { struct point; }
class charged_tree_sewing {
public:
    using mask = unsigned;
    using real = long double;
    using complex = std::complex<real>;
    using point = validation::point;
    enum class edge { photon, fermion };
    struct cut { mask left, right; edge propagator; bool balanced; };
    // The factor of each of the two cubic helicity vertices must be -sqrt(2) e.
    // The returned expression specifies e in the model's own parameter scope.
    static std::shared_ptr<const charged_tree_sewing> from_model(
        const model&, const std::vector<std::string>& legs, bool physical);
    const std::string& coupling() const { return coupling_; }
    const std::vector<cut>& sewings() const { return cuts_; }
    std::size_t diagrams() const { return diagrams_; }
    std::string description() const;
    std::string latex() const;
    bool physical() const { return physical_; }
    std::shared_ptr<const charged_tree_sewing> alternate_gauge() const;
    complex amplitude(const point& p, real e) const { return evaluate(p,e,false); }
    real spin_sum(const point& p, real e) const { return evaluate(p,e,true).real(); }

private:
    using wave = std::array<complex, 4>;
    enum class end { dressed, amputated, exchanged, center };
    struct join { unsigned a,b; int sign; };
    struct node {
        mask legs;
        int charge;
        end boundary;
        unsigned spins;
        std::vector<unsigned short> slots;
        std::vector<join> terms;
        std::size_t graphs;
    };
    charged_tree_sewing(std::vector<int>, bool, std::string);
    std::string format_formula(bool tex) const;
    unsigned build(mask, end);
    complex evaluate(const point&,real,bool) const;
    std::vector<int> types_, references_;
    bool physical_;
    std::string coupling_;
    mask charged_ = 0;
    unsigned pairs_ = 0;
    int orientation_ = 1, partner_ = -1;
    std::size_t diagrams_ = 0;
    std::vector<int> charge_;
    std::map<std::pair<mask,end>,unsigned> indices_;
    std::vector<node> nodes_;
    std::vector<cut> cuts_;
    std::vector<join> roots_;
};
}
