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


    // Distinctly factored coupling powers multiply the same spin tensor.
    // The sign of the symbolic square root is immaterial and stays opaque.
    Id positiveRoot=a.call("Power",{a.integer(3),a.exact(mpq_class(1,2))});
    Id gx=a.atom("couplingX"), gy=a.atom("couplingY");
    Id composite=a.pow(a.mul({gx,gy}),-2);
    Id separatedPowers=a.mul({a.pow(gx,-2),a.pow(gy,-2)});
    auto cancellation=a.add({a.mul({a.pow(positiveRoot,2),composite,x}),
                             a.mul({a.integer(-3),separatedPowers,x}), unknownRoot});
    if (collect_parameter_monomials(a,cancellation)!=unknownRoot)
        throw std::runtime_error("Cancellation of exact coupling and radical factors");

    Id rootBlocks=a.add({a.pow(a.mul({gx,positiveRoot}),-1),
                         a.mul({a.integer(-1),a.pow(gx,-1),
                                a.call("Power",{a.integer(3),a.exact(mpq_class(-1,2))})})});
    if (collect_parameter_monomials(a,rootBlocks)!=a.integer(0))
        throw std::runtime_error("Different ways of writing the same inverse radical");

    // Repeated subcurrents can be large on paper and cheap to factor in the
    // shared expression tree. Extract a common factor from only two summands.
    Id current = spectator;
    for (int level = 0; level < 13; ++level)
        current = a.call("Current", {current, current});
    Id large = a.add({a.mul({current,x}), a.mul({current,y}), mx});
    Id grouped = a.add({a.mul({current,a.add({x,y})}), mx});
    if (a.cost(large) <= 24000 || collect_scalar_factors(a,large) != grouped)
        throw std::runtime_error("Repeated subcurrent was missed in a large expression");

    // The first collection creates a sum that already multiplies another
    // current. The next pass can factor the newly shared sum as a whole.
    Id leg_a=a.call("CurrentA", {x,y}), leg_b=a.call("CurrentB", {y,x});
    Id u=a.atom("u"), v=a.atom("v");
    Id xu=a.mul({x,u}), yv=a.mul({y,v}), shared=a.add({xu,yv});
    Id related=a.add({a.mul({leg_a,xu}),a.mul({leg_a,yv}),a.mul({leg_b,shared})});
    if (collect_scalar_factors(a,related,3)!=a.mul({a.add({leg_a,leg_b}),shared}))
        throw std::runtime_error("Newly exposed common subcurrent was missed");

    // Distribution would generate over 500^4 monomials. Factoring the
    // shared polynomial must recover the exact product without expanding it.
    std::vector<Id> polynomials;
    for (int k = 0; k < 4; ++k) {
        std::vector<Id> terms;
        for (int j = 0; j < 520; ++j)
            terms.push_back(a.atom("variable" + std::to_string(520*k+j)));
        polynomials.push_back(a.add(terms));
    }
    auto product = a.mul(polynomials);
    auto separated = a.add({a.mul({x,product}),a.mul({y,product})});
    if (collect_scalar_factors(a,separated) != a.mul({a.add({x,y}),product}))
        throw std::runtime_error("Common Laurent factor lost at the expansion budget");

    // A compound reciprocal can cancel after factoring its numerator.
    // Committing that local cancellation could increase the expanded score
    // once a second term at the original pole has been combined.
    Id padding = a.call("OpaqueCoefficient", std::vector<Id>(4100, x));
    Id rational_pole = a.pow(a.add({u,v}),-1);
    Id local = a.mul({rational_pole,a.add({a.mul({u,padding}),a.mul({v,padding})})});
    Id rational_sum = a.add({local,a.mul({a.integer(-1),u,padding,rational_pole})});
    if (expression_complexity(a,collect_scalar_factors(a,rational_sum)) >
        expression_complexity(a,rational_sum))
        throw std::runtime_error("Factored a composite reciprocal into a more complex sum");

    // Clear one existing simple pole across a nested sum. One regular
    // term acquires d in its numerator and remains present away from the cut.
    Id d = a.parse("PropDen[Mom[Multiparticle[1,2]],m]");
    Id inverse = a.pow(d,-1);
    Id nested = a.add({a.mul({u,a.add({x,a.mul({inverse,y})})}),
                       a.mul({inverse,v,mx}),a.mul({inverse,leg_a,leg_b})});
    Id cleared = a.mul({inverse,a.add({a.mul({u,a.add({a.mul({d,x}),y})}),
                                       a.mul({v,mx}),a.mul({leg_a,leg_b})})});
    if (collect_simple_fractions(a,nested) != cleared || a.cost(cleared) >= a.cost(nested))
        throw std::runtime_error("Lost the regular numerator term when clearing a shared pole");
    for (Id hidden : {a.pow(d,-2), a.pow(a.add({inverse,x}),-1)}) {
        Id mixed = a.add({nested,hidden});
        if (collect_simple_fractions(a,mixed) != mixed)
            throw std::runtime_error("Reorganized an unsupported or higher-order pole");
    }

    // More than 131072 full monomials, sharing one small coefficient over
    // independent poles. Two printed contributions in the first class cancel.
    std::vector<Id> poles, numerators;
    for (int i = 0; i < 1024; ++i)
        poles.push_back(a.pow(a.call("PropDen", {a.integer(i + 1), mass}), -1));
    for (int i = 0; i < 130; ++i) numerators.push_back(a.atom("n" + std::to_string(i)));
    auto coefficient = a.mul({a.add(numerators), a.call("MomProd", {a.integer(1), a.integer(2)})});
    auto many_poles = a.add({a.mul({a.add(poles), coefficient}),
                             a.mul({a.integer(-1), poles.front(), coefficient})});
    if (reported_expression_complexity(a, a.mul({a.pow(sqrt2,6), many_poles})) != 1023 * 130 * 9)
        throw std::runtime_error("Large pole classes did not combine with their correct weights");

    std::cout << "Sparse collection identities passed\n";
}
