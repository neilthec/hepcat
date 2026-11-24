"""Expand the numbered spinor formulas in the QED manuscript for signed checks.

Usage: python3 six-point-numbered-formulas.py CHANNEL_SUM.expr OUTPUT_DIRECTORY
The input is the saved on-shell reconstruction. Only its two parameter names
and their model scope are used to write the paper formulas in HEPCAT syntax.
"""
import argparse
from itertools import permutations
from pathlib import Path
import re


def call(head, *args):
    return f"{head}[{','.join(map(str, args))}]"


def product(*factors):
    return call("Times", *factors)


def add(*terms):
    return call("Plus", *terms)


def power(base, exponent):
    return call("Power", base, exponent)


def chain(kinds, legs, massive):
    def endpoint(chirality, leg):
        return call("Spinor", '"Spin"' if leg in massive else '"Helicity"', f'"{chirality}"', leg)
    return call("SpinorChain", endpoint(kinds[0], legs[0]),
                *(call("Mom", leg) for leg in legs[1:-1]), endpoint(kinds[1], legs[-1]))


def pole(mass, *legs):
    return call("PropDen", call("Mom", call("Multiparticle", *sorted(legs))), mass)


def parity(labels):
    return (-1) ** sum(a > b for i, a in enumerate(labels) for b in labels[i + 1:])


def equal_helicity(m, e):
    def bracket(kinds, *legs):
        return chain(kinds, legs, {1, 2})
    angle = ("Angle", "Angle")
    square = ("Square", "Square")
    mixed = ("Angle", "Square")
    physical, spurious = [], []
    for a, b, c, d in permutations((3, 4, 5, 6)):
        correction = product(-1, power(m, 2), power(bracket(square, c, d), 2))
        physical.append(product(
            power(bracket(square, a, b), 2), power(product(pole(m, 2, c), pole(m, 2, d)), -1),
            add(product(bracket(square, c, 1, 2, c), bracket(square, d, 1, 2, d)),
                product(correction, pole(m, 1, c), pole(m, 1, d), power(pole(m, 2, c, d), -1)))))
        spurious.append(product(
            bracket(square, a, b),
            power(product(bracket(angle, a, b), pole(m, 1, c), pole(m, 1, d), pole(m, 2, a)), -1),
            add(product(bracket(mixed, a, 1, c), bracket(mixed, a, 1, d),
                        power(product(bracket(angle, a, c), bracket(angle, a, d)), -1)),
                product(correction, power(pole(m, 1, c, d), -1)))))
    overall = product(power(e, 4), m, bracket(angle, 1, 2))
    return {
        "four-photon-physical": product(overall, add(*physical),
                                        power(product(*(pole(m, 1, i) for i in range(3, 7))), -1)),
        "four-photon-spurious": product(2, overall, add(*spurious)),
    }


def six_fermions(m, e):
    def angle(*legs):
        return chain(("Angle", "Angle"), legs, range(1, 7))

    def square(*legs):
        return chain(("Square", "Square"), legs, range(1, 7))

    def mixed(*legs):
        return chain(("Square", "Angle"), legs, range(1, 7))

    def numerator(a, b, x, y, c, d):
        return add(product(angle(b, x), square(y, d), add(product(m, angle(a, c)), mixed(a, b, c))),
                   product(square(a, x), angle(y, d), add(product(m, square(b, c)), mixed(c, a, b))),
                   product(-1, mixed(a, x, b), add(product(angle(x, c), square(y, d)),
                                                    product(square(x, c), angle(y, d)))))
    terms = []
    for odd in permutations((1, 3, 5)):
        for even in permutations((2, 4, 6)):
            a, x, c = odd
            b, y, d = even
            numerators = [numerator(i, j, x, y, k, l)
                          for i, j in ((a, b), (b, a)) for k, l in ((c, d), (d, c))]
            terms.append(product(parity(odd) * parity(even), add(*numerators),
                                 power(product(pole(0, a, b), pole(0, c, d), pole(m, a, b, x)), -1)))
    return product(-4, power(e, 4), add(*terms))


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("channel_sum", type=Path)
    parser.add_argument("output_directory", type=Path)
    args = parser.parse_args()
    source = args.channel_sum.read_text()
    def parameter(name):
        found = set(re.findall(r'ModelParameter\["' + name + r'","[^"]+","Real"\]', source))
        if len(found) != 1:
            raise ValueError(f"Expected one model scope for {name}")
        return found.pop()
    m, e = parameter("Me"), parameter("EE")
    formulas = equal_helicity(m, e)
    formulas["six-fermion-permutation"] = six_fermions(m, e)
    args.output_directory.mkdir(parents=True, exist_ok=True)
    for name, value in formulas.items():
        (args.output_directory / f"{name}.form").write_text(value + "\n")


if __name__ == "__main__":
    main()
