#include "vector_basis.hpp"

#include <iostream>

int main() {
    using namespace hepcat;
    // Combining a+b=c with b+d=e replaces (a-d)/3 by (c-e)/3, canceling
    // the intermediate coefficient of b. The sixth coordinate is a spectator.
    TensorCoordinates source{{1,mpq_class(1,3)},{4,mpq_class(-1,3)},{6,1}};
    std::vector<TensorCoordinates> rows{
        {{1,1},{2,1},{3,-1}}, {{2,1},{4,1},{5,-1}}
    };
    std::map<Id, std::size_t> weights{{1,9},{4,9}};
    TensorCoordinates expected{{3,mpq_class(1,3)},{5,mpq_class(-1,3)},{6,1}};
    if (reduceTensorSupport(source, rows, weights) != expected) {
        std::cerr << "Failed to eliminate both costly coordinates using the two identities.\n";
        return 1;
    }
    // A dependent rescaling can change the proposed basis but cannot change
    // the exact result. An independent spectator has to remain untouched.
    rows.push_back({{1,mpq_class(2,7)},{2,mpq_class(2,7)},{3,mpq_class(-2,7)}});
    if (reduceTensorSupport(source, rows, weights) != expected ||
        reduceTensorSupport({{6,1}}, rows, weights) != TensorCoordinates{{6,1}} ||
        !reduceTensorSupport(rows.back(), rows, weights).empty()) return 1;
}
