"""Transcribe one signed-orbit summand for the four-fermion QED amplitude with ++- photons."""
import argparse
from pathlib import Path
import re


def call(head, *args):
    return f"{head}[{','.join(map(str, args))}]"


def mul(*args):
    factors = [x for x in args if x != 1]
    if len(factors) > 1:
        return call("Times", *factors)
    return factors[0] if factors else 1


def add(*args):
    return call("Plus", *args)


def frac(numerator, *denominators):
    return mul(numerator, *(call("Power", x, -1) for x in denominators))


def neg(x):
    return mul(-1, x)


def sub(x, y):
    return add(x, neg(y))


def square(x):
    return call("Power", x, 2)


def pole(mass, *legs):
    return call("PropDen", call("Mom", call("Multiparticle", *sorted(legs))), mass)


def bracket(is_square, start, end, *momenta):
    def spinor(sq, leg):
        return call("Spinor", '"Spin"' if leg <= 4 else '"Helicity"', '"Square"' if sq else '"Angle"', leg)
    return call("SpinorChain", spinor(is_square, start), *(call("Mom", p) for p in momenta),
                spinor(is_square ^ bool(len(momenta) % 2), end))


def summand(physical, mass, charge):
    a, b, c, d, i, j, k = 1, 2, 4, 3, 5, 6, 7

    def positive(h):
        return h != k

    def tau(t, *photons):
        return pole(mass, t, *photons)

    def dipole(t, u, h):
        return frac(neg(bracket(positive(h), h, h, u, t)), tau(t, h), tau(u, h))

    def spin(t, h):
        return frac(bracket(positive(h), h, t), tau(t, h))

    def insertion(t, h, l):
        chain = bracket(True, h, l, t) if positive(h) else bracket(True, l, h, t)
        return frac(mul(bracket(positive(h), h, l), chain), tau(t, h))

    def connected(t, h, l):
        if positive(h) == positive(l):
            field = mul(mass, bracket(positive(h), h, l))
        else:
            plus, minus = (h, l) if positive(h) else (l, h)
            field = bracket(True, plus, minus, t)
        return frac(neg(square(field)), tau(t, h), tau(t, l), tau(t, h, l))

    def both_ends(h, l):
        return add(connected(a, h, l), connected(b, h, l))

    def recoil(t, h, l):
        if positive(h) == positive(l):
            return frac(neg(mul(connected(t, h, l), bracket(not positive(h), h, t))), mass)
        return frac(mul(insertion(t, l, h), spin(t, h)), tau(t, h, l))

    def ordered(t):
        first = mul(add(insertion(t, i, j), insertion(t, i, k)), connected(t, j, k))
        last = mul(insertion(t, k, i), connected(t, i, j))
        return frac(add(first, last), tau(t, i, j, k))

    def triple_recoil(t):
        return frac(add(mul(add(insertion(t, j, i), insertion(t, j, k)), recoil(t, i, k)),
                        mul(add(insertion(t, k, i), insertion(t, k, j)), recoil(t, i, j)),
                        mul(connected(t, j, k), add(1, frac(tau(t, j, k), tau(t, i))), bracket(True, i, t)),
                        mul(bracket(True, i, j), recoil(t, j, k))), tau(t, i, j, k))

    fi, fj, fk = (dipole(a, b, h) for h in (i, j, k))
    v, w = spin(a, i), spin(b, k)
    if physical:
        pair = frac(add(mul(fi, fj), connected(a, i, j), connected(b, i, j)), 2)
    else:
        cross = frac(bracket(True, i, j, a, b), tau(a, i), tau(b, j))
        diagonal = mul(square(mass), bracket(True, i, j),
                       add(*(frac(1, tau(t, i), tau(t, i, j)) for t in (a, b))))
        pair = frac(sub(cross, diagonal), bracket(False, i, j))
    x_pair = [(pair, a), (add(mul(fj, v), recoil(a, i, j)), i)]

    # Sew an angle spinor y and a square spinor x against the second charged line.
    def sew(y, x, photon):
        vacuum = add(mul(bracket(False, y, d), bracket(True, x, c)),
                     mul(bracket(False, y, c), bracket(True, x, d)))
        if photon is None:
            return vacuum
        if positive(photon):
            endpoint = sub(mul(bracket(False, y, d), spin(c, photon)),
                           mul(bracket(False, y, c), spin(d, photon)))
            shift = mul(bracket(True, x, photon), endpoint)
        else:
            endpoint = sub(mul(bracket(True, x, d), spin(c, photon)),
                           mul(bracket(True, x, c), spin(d, photon)))
            shift = mul(bracket(False, y, photon), endpoint)
        return add(mul(dipole(c, d, photon), vacuum), shift)

    def project(y, square_spinors, photon=None):
        return add(*(mul(coefficient, sew(y, x, photon)) for coefficient, x in square_spinors))

    # The angle endpoint is b or k. The square endpoint is a or i.
    y0 = add(mul(fj, both_ends(i, k)), ordered(a), neg(ordered(b)))
    yi = add(mul(v, connected(b, j, k)), mul(recoil(a, i, k), fj), triple_recoil(a))
    zk = add(mul(fj, recoil(b, k, i)),
             frac(sub(mul(w, connected(b, i, j), pole(0, i, k)),
                      mul(add(insertion(b, j, i), insertion(b, j, k)), recoil(b, k, i))),
                  tau(b, i, j, k)))
    zi = mul(v, recoil(b, k, j))
    triple = add(mul(fk, project(b, x_pair)), neg(mul(w, project(k, x_pair))),
                 project(b, [(y0, a), (yi, i)]), project(k, [(zk, a), (zi, i)]))
    equal_pair = project(b, x_pair, k)
    mixed_pair = add(project(b, [(add(mul(fi, fk), both_ends(i, k)), a),
                                 (add(mul(fk, v), recoil(a, i, k)), i)], j),
                     project(k, [(sub(recoil(b, k, i), mul(w, fi)), a), (neg(mul(w, v)), i)], j))
    exchange = add(frac(triple, pole(0, c, d)), frac(equal_pair, pole(0, c, d, k)),
                   frac(mixed_pair, pole(0, c, d, j)))
    return neg(mul(call("Power", mul(charge, call("Power", 2, call("Rational", 1, 2))), 5), exchange))


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("preference", choices=("physical", "spurious"))
    parser.add_argument("elementary", type=Path, help="saved .expr file giving the two model parameter identifiers")
    args = parser.parse_args()
    text = args.elementary.read_text()

    def parameter(name):
        match = re.search(rf'ModelParameter\[\s*"{name}"\s*,[^\]]+\]', text)
        if not match:
            parser.error(f"Missing model parameter {name}")
        return match[0]

    print(summand(args.preference == "physical", parameter("Me"), parameter("EE")))
