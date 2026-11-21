// Check the apparent pole w=2 k.Q for one central radiating fermion routing.
// From the repo root, generate the positive-helicity --piece center files
// with seven-point-six-fermions.py, then pass them as PHYSICAL.expr SPURIOUS.expr.
#include "model.hpp"
#include "validation_numeric.hpp"
#include "validation_precision.hpp"

#include <algorithm>
#include <array>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <sstream>

using namespace hepcat;
using hepcat::validation::expression_evaluator;
namespace hp = hepcat::validation::precision;

namespace {
using hp::number;
using hp::scalar;
using labels = std::array<int,7>;

scalar decimal(const char* s) { return scalar(s,hp::bits,10); }

std::string read(const char* path) {
    std::ifstream input(path);
    if (!input) throw std::runtime_error(std::string("Cannot read ") + path);
    std::ostringstream text;
    text << input.rdbuf();
    return text.str();
}

hp::phase_point relabel(const hp::phase_point& source, const labels& order) {
    auto p = source;
    for (size_t i=0; i<order.size(); ++i) {
        const auto j = order[i];
        p.momenta[i] = source.momenta[j];
        p.masses[i] = source.masses[j];
        p.angles[i] = source.angles[j];
        p.squares[i] = source.squares[j];
        p.angle_basis[i] = source.angle_basis[j];
        p.square_basis[i] = source.square_basis[j];
        for (size_t n=0; n<order.size(); ++n)
            p.scalar_products[7*i+n] = source.scalar_products[7*j+order[n]];
    }
    return p;
}

// For each massive leg use p^- = E-p_z = u and p^+ = (m^2+p_T^2)/u.
// The six transverse momenta and u's sum to zero. A null seventh momentum
// along z then enforces conservation. The first three u's sum to delta.
hp::phase_point trajectory(const scalar& delta, const scalar& mass, unsigned spin_seed) {
    std::array<scalar,6> u{decimal("1"),decimal("-0.3"),decimal("-0.7")+delta,
                           decimal("-0.4"),decimal("-0.7")-delta,decimal("1.1")};
    const char* px[] = {"2",".1","-.1",".3","-.5","-1.8"};
    const char* py[] = {"0",".2",".4","-.6","-.3",".3"};
    std::vector<hp::four> momenta(7);
    scalar photon_energy(0,hp::bits);
    for (size_t i=0; i<6; ++i) {
        const auto x = decimal(px[i]), y = decimal(py[i]);
        const scalar v = (mass*mass+x*x+y*y)/u[i];
        momenta[i] = {(v+u[i])/2,x,y,(v-u[i])/2};
        photon_energy -= v/2;
    }
    momenta[6] = {photon_energy,decimal("0"),decimal("0"),photon_energy};
    // The spinor chart expects the two positive-energy fermions first.
    const labels beam_order{0,5,2,3,4,1,6};
    std::swap(momenta[1],momenta[5]);
    return relabel(hp::from_momenta(momenta,{mass,mass,mass,mass,mass,mass,decimal("0")},spin_seed),
                   beam_order);
}

// Separate the numerator multiplying 1/w before evaluating on the pole.
// The transcription is affine in 1/w. A repeated pole would need a separate test.
struct affine { Id regular, residue; };
affine separate_pole(Algebra& a, Id term, Id inverse) {
    const auto zero = a.integer(0), one = a.integer(1);
    if (term==inverse) return {zero,one};
    const auto& node = a.node(term);
    if (node.head!="Plus" && node.head!="Times") return {term,zero};
    if (node.head=="Plus") {
        std::vector<Id> regular, residual;
        for (Id c : node.args) {
            const auto part = separate_pole(a,c,inverse);
            regular.push_back(part.regular);
            residual.push_back(part.residue);
        }
        return {a.add(regular),a.add(residual)};
    }
    affine value{one,zero};
    for (Id c : node.args) {
        const auto part = separate_pole(a,c,inverse);
        if (value.residue!=zero && part.residue!=zero)
            throw std::runtime_error("Unexpected double pole");
        value = {a.mul({value.regular,part.regular}),
                 a.add({a.mul({value.regular,part.residue}),a.mul({value.residue,part.regular})})};
    }
    return value;
}

labels image(unsigned mask) {
    labels order{0,1,2,3,4,5,6};
    if (mask & 1) {
        std::swap(order[0],order[5]);
        std::swap(order[1],order[4]);
        std::swap(order[2],order[3]);
    }
    if (mask & 2) std::swap(order[0],order[1]);
    if (mask & 4) std::swap(order[4],order[5]);
    return order;
}

void run(const char* physical_path, const char* spurious_path) {
    model qed("models",3);
    Algebra a(&qed.algebra());
    const auto mass = a.parse(qed.particle("e").mass), zero = a.integer(0);
    a.setKinematics({mass,mass,mass,mass,mass,mass,zero},{});
    const auto inputs = validation::model_parameters(qed);
    const auto m = validation::evaluate_scalar(a,mass,inputs).real();
    const auto wide_mass = hp::wide_real(m);
    const auto w=a.parse("SpinorChain[Spinor[\"Helicity\",\"Square\",7],Mom[Multiparticle[1,2,3]],Spinor[\"Helicity\",\"Angle\",7]]");
    const auto seed = a.parse(read(spurious_path));
    const auto part = separate_pole(a,seed,a.pow(w,-1));
    if (part.residue==zero) throw std::runtime_error("The seed has no target pole");
    const expression_evaluator physical(a,a.parse(read(physical_path)),inputs), spurious(a,seed,inputs);
    const expression_evaluator numerator(a,part.residue,inputs), finite(a,part.regular,inputs), omega(a,w,inputs);
    validation::point metadata;
    metadata.momenta.resize(7);
    metadata.reference_seed = metadata.color_seed = 0;
    auto sum_residue = [&](const hp::phase_point& cut) {
        number total;
        scalar peak(0,hp::bits);
        for (unsigned mask=0; mask<8; ++mask) {
            const auto value=numerator.precise(metadata,relabel(cut,image(mask)));
            // The reversed orientation interchanges Q and -(Q+k), so w changes sign.
            total += (mask & 1) ? -value : value;
            peak = std::max(peak,value.magnitude());
        }
        if (peak <= decimal("1e-30") || total.magnitude() > peak*decimal("1e-35"))
            throw std::runtime_error("The cleared residues do not cancel at w=0");
        return scalar(total.magnitude()/peak,hp::bits);
    };
    std::cout << std::scientific << std::setprecision(6);
    for (unsigned seed=704771; seed<=704774; ++seed) {
        auto cut=trajectory(decimal("0"),wide_mass,seed);
        if (omega.precise(metadata,cut).magnitude()>decimal("1e-40"))
            throw std::runtime_error("The central point missed w=0");
        number reference;
        for (unsigned mask=0; mask<8; ++mask)
            reference += physical.precise(metadata,relabel(cut,image(mask)));
        if (reference.magnitude() < decimal("1e-30"))
            throw std::runtime_error("Degenerate physical reference at w=0");
        std::cout << "Cut spin seed " << seed << ", residue sum / peak " << sum_residue(cut)
                  << ", physical magnitude " << reference.magnitude() << '\n';
    }

    std::cout << "delta\tw\tpeak spurious image\tfinite physical sum\traw relative difference\tshared-w relative difference\n";
    for (const char* size : {"1e-2","1e-6","1e-10","1e-14","1e-20","1e-28"}) {
        for (int sign : {-1,1}) {
            const scalar delta = sign*decimal(size);
            const auto p = trajectory(delta,wide_mass,704771);
            number regular, sum, finite_part, residue;
            scalar peak(0,hp::bits);
            for (unsigned mask=0; mask<8; ++mask) {
                const auto q = relabel(p,image(mask));
                regular += physical.precise(metadata,q);
                const auto value = spurious.precise(metadata,q);
                sum += value;
                peak=std::max(peak,value.magnitude());
                finite_part += finite.precise(metadata,q);
                auto coeff = numerator.precise(metadata,q);
                residue += (mask & 1) ? -coeff : coeff;
            }
            if (regular.magnitude()==0) throw std::runtime_error("Zero reference at finite w");
            const scalar difference = (sum-regular).magnitude()/regular.magnitude();
            const auto evaluated_w = omega.precise(metadata,p);
            // Exact algebra gives one common w up to sign. Accumulating its
            // numerator first avoids computing nearly equal denominators independently.
            const auto shared = finite_part + residue*evaluated_w.reciprocal();
            const scalar common_difference = (shared-regular).magnitude()/regular.magnitude();
            if (common_difference>decimal("1e-24") ||
                (abs(delta)>=decimal("1e-14") && difference>decimal("1e-25")) ||
                (abs(delta)<decimal("1e-25") && peak<regular.magnitude()*decimal("1e20")))
                throw std::runtime_error("The near-pole comparison failed");
            std::cout << delta << '\t' << evaluated_w.x << '\t' << peak << '\t' << regular.magnitude()
                      << '\t' << difference << '\t' << common_difference << '\n';
        }
    }
}
} // namespace

int main(int argc, char** argv) {
    if (argc!=3) {
        std::cerr << "usage: six-fermion-pole PHYSICAL-CENTER.expr SPURIOUS-CENTER.expr\n";
        return 2;
    }
    try { run(argv[1],argv[2]); }
    catch (const std::exception& error) { std::cerr << error.what() << '\n'; return 1; }
}
