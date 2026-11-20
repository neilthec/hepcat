"""Write the oriented two-current summand in the QED paper in HEPCAT syntax."""
import argparse
from pathlib import Path
import re


def call(head, *args):
    return f"{head}[{','.join(map(str, args))}]"


def mul(*args):
    return call("Times", *args)


def add(*args):
    return call("Plus", *args)


def frac(top, *bottom):
    return mul(top, *(call("Power", x, -1) for x in bottom))


def neg(x):
    return mul(-1, x)


def square(x):
    return call("Power", x, 2)


def momentum(*legs):
    return call("Mom", legs[0] if len(legs) == 1 else call("Multiparticle", *sorted(legs)))


def bracket(sq, start, end, *momenta):
    def spin(s, i):
        return call("Spinor", '"Spin"' if i <= 4 else '"Helicity"', '"Square"' if s else '"Angle"', i)
    return call("SpinorChain", spin(sq, start), *momenta, spin(sq ^ (len(momenta) % 2), end))


def summand(physical, mass, charge):
    a, b, c, d, i, j, k = 1, 2, 4, 3, 5, 6, 7

    def tau(*legs):
        return call("PropDen", momentum(*legs), mass)

    def soft(t, u, v):
        return frac(neg(bracket(True, v, v, momentum(u), momentum(t))), tau(t, v), tau(u, v))

    def spin(t, v):
        return frac(bracket(True, v, t), tau(t, v))

    def connected(t):
        return frac(neg(square(mul(mass, bracket(True, i, j)))), tau(t, i), tau(t, j), tau(t, i, j))

    def recoil(t):
        return frac(mul(bracket(True, k, i), bracket(True, k, i, momentum(t))), tau(t, k), tau(t, i, j, k))

    ca, cb = connected(a), connected(b)
    fi, fj, fk = (soft(a, b, v) for v in (i, j, k))
    if physical:
        pair = frac(add(mul(fi, fj), ca, cb), 2)
    else:
        cross = frac(bracket(True, i, j, momentum(a), momentum(b)), tau(a, i), tau(b, j))
        diagonal = mul(square(mass), bracket(True, i, j),
                       add(*(frac(1, tau(t, i), tau(t, i, j)) for t in (a, b))))
        pair = frac(add(cross, neg(diagonal)), bracket(False, i, j))

    # X2, X3 are covectors. Each entry carries a coefficient, the end
    # spinor and, for the Dirac recoil, the momentum applied to it.
    shift = frac(neg(mul(ca, bracket(False, i, a))), mass)
    x2 = [(pair, a, None), (mul(fi, spin(a, j)), j, None), (shift, i, None)]
    x3 = [(add(frac(mul(fk, add(pair, ca, cb)), 3), neg(mul(cb, recoil(b)))), a, None),
          (mul(pair, spin(a, k)), k, None), (mul(fk, shift), i, None),
          (frac(mul(ca, recoil(a)), mass), a, momentum(a, i, j, k))]

    def project(x, leg):
        return add(*(mul(weight, bracket(True, end, leg) if p is None
                         else bracket(False, end, leg, p)) for weight, end, p in x))

    bd, bc = bracket(False, b, d), bracket(False, b, c)

    def vacuum(x):
        return add(mul(bd, project(x, c)), mul(bc, project(x, d)))

    right_spin = add(mul(bd, spin(c, k)), neg(mul(bc, spin(d, k))))
    radiating = add(mul(soft(c, d, k), vacuum(x2)), mul(right_spin, project(x2, k)))
    exchange = add(frac(vacuum(x3), call("PropDen", momentum(c, d), 0)),
                   frac(radiating, call("PropDen", momentum(c, d, k), 0)))
    return neg(mul(call("Power", mul(charge, call("Power", 2, call("Rational", 1, 2))), 5), exchange))


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("preference", choices=("physical", "spurious"))
    parser.add_argument("elementary", type=Path, help="saved .expr file giving the model parameter identifiers")
    args = parser.parse_args()
    text = args.elementary.read_text()

    def parameter(name):
        match = re.search(rf'ModelParameter\[\s*"{name}"\s*,[^\]]+\]', text)
        if not match:
            parser.error(f"Missing model parameter {name}")
        return match[0]

    print(summand(args.preference == "physical", parameter("Me"), parameter("EE")))
