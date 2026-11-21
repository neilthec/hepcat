"""Transcribe one oriented radiation term for six massive fermions and one photon."""
import argparse
from itertools import permutations
from pathlib import Path
import re


def call(head, *args):
    return f"{head}[{','.join(map(str, args))}]"


def add(*terms):
    terms = [v for v in terms if v != 0]
    return call("Plus", *terms) if len(terms) > 1 else next(iter(terms), 0)


def mul(*factors):
    if 0 in factors:
        return 0
    factors = [v for v in factors if v != 1]
    return call("Times", *factors) if len(factors) > 1 else next(iter(factors), 1)


def over(v, *poles):
    return mul(v, *(call("Power", pole, -1) for pole in poles))


def momentum(legs):
    if isinstance(legs, int):
        return call("Mom", legs)
    return call("Mom", call("Multiparticle", *sorted(legs)))


def bracket(square, left, right, *insertions):
    if left == right and not insertions:
        return 0

    def spin(sq, t):
        return call("Spinor", '"Helicity"' if t == 7 else '"Spin"', '"Square"' if sq else '"Angle"', t)
    return call("SpinorChain", spin(square, left), *(momentum(p) for p in insertions),
                spin(square ^ bool(len(insertions) % 2), right))


def coupling_factor(charge):
    return mul(-1, call("Power", mul(call("Power", 2, call("Rational", 1, 2)), charge), 5))


def summand(piece, preference, positive, mass, charge, legs=range(1, 8), *, split_spurious=False):
    a, b, x, y, c, d, k = legs

    # Parity interchanges the two spinor brackets, including inserted chains.
    def active(l, r, *momenta):
        return bracket(positive, l, r, *momenta)

    def passive(l, r):
        return bracket(not positive, l, r)

    def tau(*legs):
        return call("PropDen", momentum(legs), mass)

    def s(*legs):
        return call("PropDen", momentum(legs), 0)

    def shift(t):
        return over(active(k, t), tau(t, k))

    def dipole(t, u):
        return over(mul(-1, active(k, k, u, t)), tau(t, k), tau(u, k))

    # H is one orientation of the undressed (c,d) current.
    def H(t, active_end):
        product = active if active_end else passive
        through_pair = active(t, c, (c, d)) if active_end else active(c, t, (c, d))
        return mul(-1, add(mul(product(y, d), through_pair),
                           mul(product(y, t), active(d, c, y))))

    bare = add(mul(active(b, x), H(a, False)), mul(passive(b, x), H(a, True)))
    if piece == "leaf":
        # Both square-spinor shifts on the emitting pair follow by antisymmetry.
        emitted = add(mul(active(k, x), H(b, False)), mul(passive(b, x), H(k, True)))
        source = over(add(mul(dipole(a, b), bare), mul(shift(a), emitted)), s(a, b, k))
    else:
        # The two orientations of the undressed (a,b) current add.
        shifted_x = mul(active(b, k), H(a, False))
        open_spin = mul(active(k, a), passive(b, x), H(k, True))
        dq = tau(a, b, x)
        if preference == "physical":
            orbital = over(add(dipole(x, y), over(active(k, k, y, (c, d)), tau(y, k), dq)), 2)
            spin_factor = over(1, 2, dq)
            contact = over(add(mul(passive(b, x), active(a, k), active(y, d), active(k, c, x)),
                               mul(-1, active(b, x), active(k, a, x), passive(y, d), active(k, c))),
                           2, tau(x, k))
        else:
            orbital_numerator = over(mul(-1, active(k, k, (a, b), x)), tau(x, k))
            if split_spurious:
                # Omit the common outer propagators here. The full expansion
                # sums both charge orientations before dividing by w.
                return mul(shift(x), shifted_x), add(mul(orbital_numerator, bare), mul(-1, open_spin))
            omega = active(k, k, (a, b, x))
            orbital = over(mul(-1, active(k, k, (a, b), x)), tau(x, k), omega)
            spin_factor = over(-1, omega)
            contact = 0
        source = over(add(mul(orbital, bare), mul(shift(x), shifted_x),
                          mul(spin_factor, open_spin), contact), s(a, b))

    return over(mul(coupling_factor(charge), source), s(c, d), tau(c, d, y))


def expanded(preference, positive, mass, charge, *, raw=False):
    """Apply the signed operators, collecting a common spurious divisor before division."""
    group_poles = preference == "spurious" and not raw
    k = 7
    residues = {}

    def den(internal_mass, *labels):
        return call("PropDen", momentum(labels), internal_mass)

    def parity(p):
        return (-1) ** sum(x > y for i, x in enumerate(p) for y in p[i+1:])
    terms = []
    for electrons in permutations((1, 3, 5)):
        for positrons in permutations((2, 4, 6)):
            a, x, c = electrons
            b, y, d = positrons
            sign = parity(electrons) * parity(positrons)
            side_residues = []
            if group_poles:
                same_pair = over(mul(sign, coupling_factor(charge)), den(0, a, b), den(0, c, d))
            for left, right, i, j, u, v in ((a,b,x,y,c,d), (d,c,y,x,b,a)):
                regular, numerator = [], []
                for z, t in ((u,v), (v,u)):
                    for l, r, odd in ((left,right,1), (right,left,-1)):
                        legs = (l,r,i,j,z,t,k)
                        leaf = summand("leaf", preference, positive, mass, charge, legs)
                        if group_poles:
                            terms.append(mul(sign * odd, leaf))
                            shift, residue = summand("center", preference, positive, mass, charge,
                                                     legs, split_spurious=True)
                            regular.append(shift)
                            numerator.append(residue)
                        else:
                            center = summand("center", preference, positive, mass, charge, legs)
                            terms.append(mul(sign, add(mul(odd, leaf), center)))
                if group_poles:
                    central_pole = den(mass, u, v, j)
                    terms.append(mul(same_pair, over(add(*regular), central_pole)))
                    side_residues.append(over(add(*numerator), central_pole))
            if group_poles:
                # Q' on the reversed line is -Q-k. Both local endpoint
                # exchanges leave 2 k.Q unchanged. Only reversal negates w.
                key = tuple(sorted((a,b,x)))
                residue = add(side_residues[0], mul(-1, side_residues[1]))
                residues.setdefault(key, []).append(mul(same_pair, residue))
    for triple, numerators in residues.items():
        omega = bracket(positive, k, k, triple)
        terms.append(over(add(*numerators), omega))
    return add(*terms)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("preference", choices=["physical", "spurious"])
    parser.add_argument("elementary", type=Path, help="source of the model's mass and coupling identifiers")
    mode = parser.add_mutually_exclusive_group(required=True)
    mode.add_argument("--expanded", action="store_true", help="write the complete sum for numerical export")
    mode.add_argument("--raw-expanded", action="store_true",
                      help="write the direct operator sum with each oriented divisor evaluated separately")
    mode.add_argument("--piece", choices=("leaf", "center"),
                        help="emit on an outer pair (odd under a,b exchange) or on the central line (even)")
    parser.add_argument("--helicity", choices=("+", "-"), default="+")
    args = parser.parse_args()
    text = args.elementary.read_text()
    def parameter(name):
        match = re.search(rf'ModelParameter\[\s*"{name}"\s*,[^\]]+\]', text)
        if not match:
            parser.error(f"Missing model parameter {name}")
        return match[0]
    inputs = (args.preference, args.helicity == "+", parameter("Me"), parameter("EE"))
    if args.piece:
        print(summand(args.piece, *inputs))
    else:
        print(expanded(*inputs, raw=args.raw_expanded))
