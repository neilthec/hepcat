#include "reduce.hpp"
#include <iostream>
#include <stdexcept>

namespace hepcat { Id collectCandidate(Algebra&, Id); }

int main() {
    using namespace hepcat;
    Algebra a;
    const Id mass=a.atom("m");
    a.setKinematics({mass,mass,mass,mass},{});
    auto spin=[&](int i,bool angle) {
        return a.call("Spinor",{a.atom("\"Spin\""),a.atom(angle?"\"Angle\"":"\"Square\""),a.integer(i)});
    };
    auto mom=[&](int i) {return a.call("Mom",{a.integer(i)});};
    auto bracket=[&](int i,int j,bool angle) {
        return a.call("SpinorChain",{spin(i,angle),spin(j,angle)});
    };
    auto sandwich=[&](int i,Id momentum,int j) {
        return a.call("SpinorChain",{spin(i,false),momentum,spin(j,true)});
    };
    Id p34=a.call("Mom",{a.call("Multiparticle",{a.integer(3),a.integer(4)})});
    Id input=a.add({sandwich(1,p34,2),a.mul({mass,bracket(1,2,true)}),
                    a.mul({a.integer(-1),mass,bracket(1,2,false)})});
    if (collectCandidate(a,input)!=a.integer(0))
        throw std::runtime_error("External momentum conservation and Dirac cancellation");
    Id lhs=a.add({a.mul({bracket(1,2,true),sandwich(3,mom(1),4)}),
                  a.mul({a.integer(-1),bracket(1,4,true),sandwich(3,mom(1),2)})});
    Id rhs=a.mul({a.integer(-1),mass,bracket(2,4,true),bracket(1,3,false)});
    Id simplified=collectCandidate(a,lhs);
    if (simplified!=rhs)
        throw std::runtime_error("Schouten with one momentum: "+a.format(simplified));
    for (Id slot : {a.call("UnknownMatrix",{a.integer(1)}), mom(7)}) {
        Id unsupported=a.add({a.mul({bracket(1,2,true),sandwich(3,slot,4)}),
            a.mul({a.integer(-1),bracket(1,4,true),sandwich(3,slot,2)})});
        if (collectCandidate(a,unsupported)!=unsupported)
            throw std::runtime_error("Unknown or unlisted momentum must remain opaque");
    }
    std::cout << "Sparse collection identities passed\n";
}
