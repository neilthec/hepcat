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
    // The external Dirac factors have different powers of two unrelated masses.
    // Their inverse powers must be allowed to cancel within one spectator pole.
    Id mx=a.atom("mx"), my=a.atom("my"), spectator=a.atom("coupling");
    a.setKinematics({mx,my,a.atom("ma"),a.atom("mb")},{});
    Id invX=a.pow(mx,-1), invY=a.pow(my,-1);
    Id unequal=a.add({
        a.mul({spectator,invX,invY,sandwich(1,p34,2)}),
        a.mul({spectator,invY,bracket(1,2,true)}),
        a.mul({a.integer(-1),spectator,invX,bracket(1,2,false)})});
    if (collectCandidate(a,unequal)!=a.integer(0))
        throw std::runtime_error("Unequal external masses must reconnect Dirac terms");

    // Twice one Schouten zero plus a surviving bracket product. The shortest
    // answer requires a rational multiple of the identity, with one spectator.
    Id abcd=a.mul({bracket(1,2,true),bracket(3,4,true)});
    Id acbd=a.mul({bracket(1,3,true),bracket(2,4,true)});
    Id adbc=a.mul({bracket(1,4,true),bracket(2,3,true)});
    auto coeff=[&](int n) {return a.mul({a.exact(mpq_class(n,3)),spectator});};
    Id weighted=a.add({a.mul({coeff(2),abcd}),a.mul({coeff(-1),acbd}),a.mul({coeff(2),adbc})});
    if (collectCandidate(a,weighted)!=a.mul({coeff(1),acbd}))
        throw std::runtime_error("Rational Schouten support cancellation");

    for (Id slot : {a.call("UnknownMatrix",{a.integer(1)}), mom(7)}) {
        Id unsupported=a.add({a.mul({bracket(1,2,true),sandwich(3,slot,4)}),
            a.mul({a.integer(-1),bracket(1,4,true),sandwich(3,slot,2)})});
        if (collectCandidate(a,unsupported)!=unsupported)
            throw std::runtime_error("Unknown or unlisted momentum must remain opaque");
    }
    // Keep the sign and check the exact middle coefficient before folding a
    // trinomial. The two numerical roots must disappear from its prefactor.
    Id x=bracket(1,2,true), y=bracket(3,4,false);
    Id sqrt2=a.call("Power",{a.integer(2),a.exact(mpq_class(1,2))});
    Id core=a.add({a.pow(x,2),a.pow(y,2),a.mul({a.integer(-2),x,y})});
    Id square=compact_commuting_products(a,a.mul({a.pow(sqrt2,2),core}));
    Id opposite=a.add({y,a.mul({a.integer(-1),x})});
    Id direct=a.add({x,a.mul({a.integer(-1),y})});
    if (square!=a.mul({a.integer(2),a.pow(direct,2)}) &&
        square!=a.mul({a.integer(2),a.pow(opposite,2)}))
        throw std::runtime_error("Exact signed binomial square and numeric radical: "+a.format(square));
    Id nonsquare=a.add({a.pow(x,2),a.pow(y,2),a.mul({a.integer(3),x,y})});
    if (a.node(compact_commuting_products(a,nonsquare)).head=="Power")
        throw std::runtime_error("Folded a non-square trinomial");
    Id unknownRoot=a.call("Power",{a.pow(mx,2),a.exact(mpq_class(1,2))});
    if (compact_commuting_products(a,unknownRoot)!=unknownRoot)
        throw std::runtime_error("Removed a symbolic square root without a positivity assumption");

    // Repeated subcurrents can be large on paper and cheap to factor in the
    // shared expression tree. Extract a common factor from only two summands.
    Id current = spectator;
    for (int level = 0; level < 13; ++level)
        current = a.call("Current", {current, current});
    Id large = a.add({a.mul({current,x}), a.mul({current,y}), mx});
    Id grouped = a.add({a.mul({current,a.add({x,y})}), mx});
    if (a.cost(large) <= 24000 || collect_scalar_factors(a,large) != grouped)
        throw std::runtime_error("Repeated subcurrent was missed in a large expression");

    std::cout << "Sparse collection identities passed\n";
}
