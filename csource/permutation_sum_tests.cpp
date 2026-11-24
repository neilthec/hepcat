// Evaluate the returned permutation sum on relabeled phase-space points.
// This exercises the sign, stabilizer and momentum ordering independently
// of the symbolic relabeling used by the reducer.
#include "amp_expression.hpp"
#include "permutation_sum.hpp"
#include "validation_numeric.hpp"

#include <algorithm>
#include <iostream>
#include <numeric>
#include <stdexcept>

using namespace hepcat;
using namespace hepcat::validation;

namespace {
void require(bool ok, const char* reason) {
    if (!ok) throw std::runtime_error(reason);
}

point permute(const point& p, const std::vector<int>& legs, const std::vector<int>& order) {
    auto q = p;
    std::vector<std::size_t> from(p.masses.size());
    std::iota(from.begin(),from.end(),0);
    for (std::size_t i = 0; i < legs.size(); ++i) from[legs[i]-1] = order[i]-1;
    for (std::size_t i = 0; i < from.size(); ++i) {
        const auto j = from[i];
        q.momenta[i] = p.momenta[j];
        q.angles[i] = p.angles[j];
        q.squares[i] = p.squares[j];
        q.angle_basis[i] = p.angle_basis[j];
        q.square_basis[i] = p.square_basis[j];
        for (std::size_t k = 0; k < from.size(); ++k)
            q.scalar_products[from.size()*i+k] = p.scalar_products[from.size()*j+from[k]];
    }
    return q;
}

void test_massive_scalar_legs() {
    Algebra a;
    const auto m = a.parse("m");
    a.setKinematics({a.parse("M1"),a.parse("M2"),m,m,m,m}, {});
    auto inverse_pole = [&](int spectator, int leg) {
        const auto labels = a.call("Multiparticle",{a.integer(spectator),a.integer(leg)});
        return a.pow(a.call("PropDen",{a.call("Mom",{labels}),m}),-1);
    };
    std::vector<Id> ordered;
    for (int i = 3; i <= 6; ++i)
        for (int j = 3; j <= 6; ++j)
            for (int k = 3; k <= 6; ++k)
                if (i != j && i != k && j != k)
                    ordered.push_back(a.mul({inverse_pole(1,i),inverse_pole(2,j),
                                             a.pow(inverse_pole(1,k),2)}));
    const auto sum = compact_permutations(a,a.add(ordered));
    require(sum && sum->legs == std::vector<int>({3,4,5,6}) &&
            sum->orbit_sizes == std::vector<std::size_t>{24} &&
            std::any_of(ordered.begin(),ordered.end(),[&](Id term) {
                return compact_commuting_products(a,term) == sum->kernel;
            }),
            "failed to collect identical massive scalar legs");
}

void test_signed_scalar_kernel() {
    Algebra a;
    a.setKinematics(std::vector<Id>(4,a.integer(0)),{});
    const auto seed = a.parse("Plus[MomProd[1,3],MomProd[1,4],Times[-1,MomProd[2,3]],Times[-1,MomProd[2,4]]]");
    permutation_sum form{{3,4},seed,{}, {},{{1,2}}};
    auto project = [&](permutation_sum s) {
        return a.simplify(expand_permutation_groups(a,expand_signed_operators(a,s),{s.legs}));
    };
    auto equivalent = [&](const permutation_sum& s, Id result) {
        auto check=s;
        check.kernel=result;
        const auto difference=a.add({project(s),a.mul({a.integer(-1),project(check)})});
        const expression_evaluator value(a,difference,{});
        for (unsigned trial=1; trial<=3; ++trial) {
            auto p=sample_point({0,0,0,0},310+trial);
            // Use independent integer products here to avoid accidental
            // four-body momentum-conservation identities in this algebra test.
            for (unsigned i=0;i<4;++i) for (unsigned j=i;j<4;++j) {
                const unsigned n=3+4*i+j;
                p.scalar_products[4*i+j]=p.scalar_products[4*j+i]=n*n+trial*n*n*n;
            }
            if (value(p)!=complex{}) return false;
        }
        return true;
    };
    auto reduced=simplify_signed_kernel(a,seed,form);
    require(a.cost(reduced)<a.cost(seed) && equivalent(form,reduced),"incorrect scalar alternating fold");
    form.kernel=a.parse("Times[MomProd[1,3],Plus[MomProd[1,3],Times[-1,MomProd[2,3]]]]");
    // This joint swap mixes the alternating and symmetric groups. Folding an
    // individual Fermi transposition inside its image would change the result.
    form.joint_exchange=signed_exchange{{{1,3}},-1};
    require(equivalent(form,simplify_signed_kernel(a,form.kernel,form)),
            "failed to protect the spectator or the ordered exchange operators");
}

} // namespace

int main(int argc, char** argv) {
    if (argc != 2) return 2;
    try {
        test_massive_scalar_legs();
        test_signed_scalar_kernel();
        std::cout << "Permutation kernels reconstruct the signed amplitudes.\n";
    } catch (const std::exception& e) {
        std::cerr << e.what() << '\n';
        return 1;
    }
}
