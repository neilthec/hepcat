// Regression cases for sign-sensitive local spinor algebra.
#include "reduce.hpp"

#include <algorithm>
#include <iostream>
#include <stdexcept>

using hepcat::Algebra;
using hepcat::Id;
using hepcat::reduceSpinor;

int main() {
    Algebra a;
    Id mass = a.atom("m"), zero = a.integer(0);
    a.setKinematics({mass,mass,mass,mass}, {});
    auto spin = [&](int leg, bool angle, std::string kind = "\"Spin\"") {
        return a.call("Spinor",{a.atom(kind),a.atom(angle ? "\"Angle\"" : "\"Square\""),a.integer(leg)});
    };
    auto indexed = [&](int leg, bool angle, bool upper) {
        return a.call("Spinor",{a.atom("\"Spin\""),a.atom(upper ? "\"Upper\"" : "\"Lower\""),
            a.atom(angle ? "\"Angle\"" : "\"Square\""),a.integer(leg),a.atom("J")});
    };
    auto mom = [&](int i){return a.call("Mom",{a.integer(i)});};
    auto word = [&](std::vector<Id> w){return a.call("SpinorChain",std::move(w));};
    auto neg = [&](Id x){return a.mul({a.integer(-1),x});};
    auto check = [&](const char* description, Id obtained, Id expected) {
        if (obtained != expected) throw std::runtime_error(std::string(description)+": "+
            a.format(obtained)+" != "+a.format(expected));
    };
    for (bool angle : {false,true}) for (bool upper : {false,true}) {
        Id first=indexed(1,angle,upper), last=indexed(4,angle,upper);
        check("indexed left Dirac equation",reduceSpinor(a,word({first,mom(1),spin(4,!angle)})),
              a.mul({mass,word({indexed(1,!angle,upper),spin(4,!angle)})}));
        check("indexed right Dirac equation",reduceSpinor(a,word({spin(1,!angle),mom(4),last})),
              a.mul({a.integer(-1),mass,word({spin(1,!angle),indexed(4,!angle,upper)})}));
        Id reverse=word({spin(4,angle),mom(2),mom(3),spin(1,angle)});
        check("even reversal",reduceSpinor(a,reverse),neg(word({spin(1,angle),mom(3),mom(2),spin(4,angle)})));
    }
    for (long sign : {-1L,1L}) {
        auto upper=a.node(indexed(2,true,true)).args, lower=a.node(indexed(2,false,false)).args;
        upper.back()=lower.back()=a.call("Rational",{a.integer(sign),a.integer(2)});
        auto fixed=a.mul({word({spin(1,true),a.call("Spinor",upper)}),
                          word({a.call("Spinor",lower),spin(4,false)})});
        check("fixed spin projection cannot use completeness",reduceSpinor(a,fixed),fixed);
    }
    Id p2=mom(2), p3=mom(3);
    Id a1=spin(1,true), s1=spin(1,false), a4=spin(4,true), s4=spin(4,false);
    Id ab=word({s1,p2,a4}), ac=word({s1,p3,a4});
    check("repeated Clifford momentum",reduceSpinor(a,word({s1,p2,p3,p2,a4})),
        a.add({a.mul({a.integer(2),a.call("MomProd",{a.integer(2),a.integer(3)}),ab}),
               a.mul({a.integer(-1),a.pow(mass,2),ac})}));
    for (bool upper : {false,true}) for (bool angle : {false,true}) {
        Id first=word({spin(1,angle),indexed(2,angle,upper)});
        for (bool otherAngle : {false,true}) {
            Id second=word({indexed(2,otherAngle,!upper),spin(4,otherAngle)});
            bool same = angle==otherAngle;
            Id aligned=same ? a.mul({mass,word({spin(1,angle),spin(4,angle)})}) :
                            reduceSpinor(a,word({spin(1,angle),p2,spin(4,otherAngle)}));
            Id expected = angle == upper ? aligned : neg(aligned);
            for (bool reverseLeft : {false, true}) for (bool reverseRight : {false, true}) {
                auto left = a.node(first).args;
                auto right = a.node(second).args;
                if (reverseLeft) std::reverse(left.begin(),left.end());
                if (reverseRight) std::reverse(right.begin(),right.end());
                check("massive completeness in all orientations",
                    reduceSpinor(a,a.mul({word(left),word(right)})),
                    reverseLeft == reverseRight ? expected : neg(expected));
            }
        }
    }
    Id unsupported=word({a1,a.atom("UnknownInsertion"),s4});
    check("opaque insertion preserved",reduceSpinor(a,unsupported),unsupported);
    Id ref=spin(1,true,"\"Xi\"");
    check("reference is not a physical Dirac spinor",reduceSpinor(a,word({ref,mom(1),s4})),
              word({s4,mom(1),ref}));
    auto helicity=[&](int i,bool angle){return spin(i,angle,"\"Helicity\"");};
    a.setKinematics({mass,zero,zero,mass},{});
    Id h2a=helicity(2,true), h2s=helicity(2,false), h3a=helicity(3,true), h3s=helicity(3,false);
    check("lightlike insertion",reduceSpinor(a,word({s1,p2,a4})),
        a.mul({word({s1,h2s}),word({h2a,a4})}));
    check("massless Dirac equation",reduceSpinor(a,word({h2a,p2,s4})),zero);
    check("squared helicity contraction sign",reduceSpinor(a,a.mul({a.pow(word({h2a,h3a}),2),a.pow(word({h2s,h3s}),2)})),
        a.pow(a.mul({a.integer(-2),a.call("MomProd",{a.integer(2),a.integer(3)})}),2));
    check("odd powered helicity contraction sign",reduceSpinor(a,a.mul({
        a.pow(word({h2a,h3a}),3),a.pow(word({h2s,h3s}),3)})),
        a.pow(a.mul({a.integer(-2),a.call("MomProd",{a.integer(2),a.integer(3)})}),3));
    auto bracket=[&](int i,int j) {return word({spin(i,true),spin(j,true)});};
    check("Schouten with spectator",reduceSpinor(a,a.add({
        a.mul({mass,bracket(1,2),bracket(3,4)}),a.mul({mass,bracket(1,4),bracket(2,3)})})),
        a.mul({mass,bracket(1,3),bracket(2,4)}));
    std::cout << "Spinor identities passed\n";
}
