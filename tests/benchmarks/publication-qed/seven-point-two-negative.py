"""Transcribe the printed pair-current formula without using HEPCAT's current generator."""
import re
import sys
from pathlib import Path


def call(name, *args):
    return f"{name}[{','.join(map(str, args))}]"


def times(*args):
    return call("Times", *args)


def plus(*args):
    return call("Plus", *args)


def power(x, n):
    return call("Power", x, n)


def divide(x, *denominators):
    return times(x, *(power(d, -1) for d in denominators))


def bracket(square, i, j, *momenta):
    def spinor(sq, leg):
        return call("Spinor", '"Spin"' if leg < 3 else '"Helicity"',
                    '"Square"' if sq else '"Angle"', leg)
    return call("SpinorChain", spinor(square, i), *(call("Mom", p) for p in momenta),
                spinor(bool(square) ^ bool(len(momenta) % 2), j))


def paper_summand(physical, mass, charge):
    refs = {3: 4, 4: 5, 5: 3, 6: 7, 7: 6}

    def tau(*legs):
        return call("PropDen", call("Mom", call("Multiparticle", *sorted(legs))), mass)

    def norm(i):
        return tau(1, i) if physical else bracket(i >= 6, refs[i], i)

    def x(i, t):
        if physical:
            return 0 if t == 1 else divide(bracket(i < 6, i, i, 1, 2), tau(1, i))
        if i < 6:
            return divide(bracket(True, i, refs[i], t), norm(i))
        return divide(bracket(True, refs[i], i, t), norm(i))

    def field(i, j, t):
        if (i < 6) == (j < 6):
            return power(times(mass, bracket(i < 6, i, j)), 2)
        positive, negative = sorted((i, j))
        return power(bracket(True, positive, negative, t), 2)

    def current(t, i, j, k, square):
        # Contract each basis spinor directly with the indicated central polarization.
        def project(end):
            use_reference = square == (k >= 6)
            if use_reference and physical:
                if t == 1:
                    return times(-1, bracket(square, end, k, 1))
                return bracket(not square, k, end, 1)
            photon = refs[k] if use_reference else k
            return bracket(square, end, photon) if t == 1 else bracket(square, photon, end)

        pole = tau(t, i, j)
        scalar = plus(times(x(i, t), x(j, t)), divide(times(-1, field(i, j, t)), pole))
        terms = [times(scalar, project(t))]
        for a, b in ((i, j), (j, i)):
            if square != (a < 6):
                continue
            ta = bracket(square, t, a)
            if square == (b < 6):
                recoil = times(mass, power(bracket(square, a, b), 2), bracket(not square, a, t))
            else:
                recoil = times(ta, bracket(not square, b, a, t), bracket(not square, a, b))
            terms.append(times(plus(times(ta, x(b, t)), divide(recoil, pole)), project(a)))
        return divide(plus(*terms), tau(t, i), tau(t, j))

    def join(i, j, k, l, m):
        return divide(plus(*(times(current(1, i, j, k, s), current(2, l, m, k, not s))
                             for s in (False, True))), norm(k))

    separated = plus(join(3, 4, 5, 6, 7), join(6, 7, 3, 4, 5))
    negative_center = plus(join(3, 4, 6, 5, 7), join(3, 6, 7, 4, 5))
    mixed = join(3, 6, 4, 5, 7)
    if physical:
        seed = plus(divide(separated, 4), mixed, divide(negative_center, 2))
    else:
        seed = plus(divide(separated, 2), mixed, join(3, 6, 5, 4, 7), negative_center)
    return times(power(times(power(2, call("Rational", 1, 2)), charge), 5), seed)


if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("preference", choices=("physical", "spurious"))
    parser.add_argument("elementary", type=Path, help="saved .expr file, for model parameter identifiers")
    args = parser.parse_args()
    text = args.elementary.read_text()

    def model_parameter(name):
        pattern = rf'ModelParameter\[\s*"{name}"\s*,[^\]]+\]'
        match = re.search(pattern, text)
        if not match:
            sys.exit(f"Cannot find model parameter {name} in {args.elementary}")
        return match[0]

    print(paper_summand(args.preference == "physical", model_parameter("Me"), model_parameter("EE")))
