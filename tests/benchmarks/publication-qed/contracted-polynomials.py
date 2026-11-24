"""Collect fully contracted native spinor products by physical denominator.

Input: a directory produced by the numerator-only constructive driver.
Output: an external-label permutation sum with rational coefficients. The only
special endpoints are the two external photon polarization spinors. Their
reference normalization is applied to the full, relabeled amplitude.
"""
import argparse
import csv
import itertools
import json
import re
from collections import Counter, defaultdict
from fractions import Fraction
from functools import lru_cache
from pathlib import Path


def args(s):
    head, _, inner = s.partition("[")
    if not inner:
        return head, ()
    if s[-1] != "]":
        raise ValueError(s)
    parts, pos, depth = [], 0, 0
    inner = inner[:-1]
    for i, c in enumerate(inner):
        if c == "," and depth == 0:
            parts.append(inner[pos:i])
            pos = i + 1
        else:
            depth += (c == "[") - (c == "]")
    return head, (*parts, inner[pos:])


def parity(p):
    return (-1) ** sum(i > j for k, i in enumerate(p) for j in p[k + 1:])


def permutations(legs):
    base = list(range(len(legs)+1))
    groups = [[i for i, s in enumerate(legs, 1) if s == name] for name in dict.fromkeys(legs)]
    for choices in itertools.product(*(itertools.permutations(g) for g in groups)):
        sign = 1
        for g, chosen in zip(groups, choices):
            if legs[g[0]-1] in ("e", "E"):
                sign *= parity(chosen)
            for i, j in zip(g, chosen):
                base[i] = j
        yield tuple(base), sign


def shorter(group, n):
    g = tuple(sorted(group))
    h = tuple(i for i in range(1, n+1) if i not in g)
    return (g, 1) if (len(g), g) <= (len(h), h) else (h, -1)


def digits(g):
    return "".join(map(str, g))


def momentum(s):
    h, (labels,) = args(s)
    if h != "Mom":
        raise ValueError(s)
    if labels.isdigit():
        return (int(labels),)
    h, group = args(labels)
    if h != "Multiparticle":
        raise ValueError(s)
    return tuple(map(int, group))


def pole(s, p):
    h, (mom, mass) = args(s)
    if h != "PropDen":
        raise ValueError(s)
    g, _ = shorter((p[j] for j in momentum(mom)), len(p)-1)
    if mass != "0" and not mass.startswith('ModelParameter["Me",'):
        raise ValueError(mass)
    return ("s" if mass == "0" else "t") + digits(g)


def chain(s):
    head, elements = args(s)
    def endpoint(t):
        h, a = args(t)
        if h != "Spinor" or len(a) != 3 or a[0] not in ('"Spin"', '"Polarization"') or not a[2].isdigit():
            raise ValueError("Uncontracted or unexpected spinor: " + t)
        chiral = {'"Angle"': "<", '"Square"': "["}
        return chiral[a[1]], int(a[2])
    l, i = endpoint(elements[0])
    r, j = endpoint(elements[-1])
    moms = tuple(map(momentum, elements[1:-1]))
    if (l != r) != bool(len(moms) % 2) or head != "SpinorChain":
        raise ValueError(s)
    return l, i, moms, j, r


def word(t, p):
    l, i, moms, j, r = t
    i, j = p[i], p[j]
    sign, mids = 1, []
    for g in moms:
        a, mult = shorter((p[x] for x in g), len(p)-1)
        sign *= mult
        mids.append(a)
    def fmt(k, a, gs, b, z):
        return k + str(a) + "".join("("+digits(g)+")" for g in gs) + str(b) + (">" if z == "<" else "]")
    first = fmt(l, i, mids, j, r)
    second = fmt(r, j, reversed(mids), i, l)
    if first == second and len(mids) % 2 == 0:
        return first, 0
    if (len(mids) % 2 and l == "<") or (len(mids) % 2 == 0 and second < first):
        return second, sign if len(mids) % 2 else -sign
    return first, sign


@lru_cache(None)
def deword(s):
    return s[0], int(s[1]), tuple(tuple(map(int, g)) for g in re.findall(r"\(([1-9]+)\)", s)), int(s[-2]), "<" if s[-1] == ">" else "["


def terms(s):
    # Native contraction usually returns expanded terms. Also support the
    # unexpanded four-point input and factors independent of a contracted leg.
    if s.startswith("Plus["):
        for t in args(s)[1]:
            yield from terms(t)
    elif s.startswith("Times["):
        for row in itertools.product(*(terms(t) for t in args(s)[1])):
            yield tuple(w for part in row for w in part)
    else:
        yield (s,)


def collect(path, n, k):
    ident = tuple(range(n+1))
    groups = defaultdict(Counter)
    @lru_cache(None)
    def parse(s):
        if s.startswith("SpinorChain["):
            a, v = word(chain(s), ident)
            return "word", a, v
        if re.fullmatch(r"-?[0-9]+",s):
            return "integer", None, int(s)
        if s.startswith("ModelParameter["):
            return "param", args(s)[1][0], 1
        h, ab = args(s)
        if h != "Power":
            raise ValueError(s)
        b, exponent = ab
        if b == "2" and exponent == "Rational[1,2]":
            return "sqrt", None, 1
        if b == "Power[2,Rational[1,2]]":
            return "sqrt", None, int(exponent)
        if b.startswith("PropDen[") and exponent == "-1":
            return "pole", pole(b, ident), 1
        if b.startswith("ModelParameter["):
            return "param", args(b)[1][0], int(exponent)
        if b.startswith("SpinorChain[") and exponent.isdigit():
            a, v = word(chain(b), ident)
            return "power", a, (int(exponent), v)
        raise ValueError(s)
    for term in terms(path.read_text().strip()):
        m = e = sqrt = 0
        c, words, den = 1, [], []
        for t in term:
            tag, a, v = parse(t)
            if tag == "integer":
                c *= v
            elif tag == "word":
                words.append(a)
                c *= v
            elif tag == "power":
                exp, s = v
                words.extend([a]*exp)
                c *= s**exp
            elif tag == "pole":
                den.append(a)
            elif tag == "sqrt":
                sqrt += v
            elif a == '"EE"':
                e += v
            elif a == '"Me"':
                m += v
            else:
                raise ValueError(t)
        if e != n-2 or sqrt != n-2*k:
            raise ValueError(f"Coupling {e}, sqrt(2) power {sqrt} in {path}")
        if c:
            groups[tuple(sorted(den))][tuple(sorted(["m"]*m + words))] += c
    return {d: {mon:v for mon,v in terms.items() if v} for d,terms in groups.items()}


def move_den(den,p):
    return tuple(sorted(t[0]+digits(shorter((p[int(i)] for i in t[1:]),len(p)-1)[0]) for t in den))


def move_poly(poly,p,sgn):
    result = Counter()
    for mon,v in poly.items():
        factors = []
        v *= sgn
        for s in mon:
            if s == "m":
                t, sign = s, 1
            else:
                t, sign = word(deword(s),p)
            v *= sign
            factors.append(t)
        result[tuple(sorted(factors))] += v
    return {k:v for k,v in result.items() if v}


def catalog(path):
    legs = (path/"checked-order.txt").read_text().splitlines()[0].split()
    n, k = len(legs), legs.count("e")
    permutations_all = list(permutations(legs))
    result = []
    count = 0
    with (path/"orbits.tsv").open() as file:
        for row in csv.DictReader(file,delimiter="\t"):
            seed = row["seed"]
            polynomials = collect(path/f"seed-{seed}.expr",n,k)
            # A labeled product of physical denominators fixes the constructive
            # term. Account exactly for permutations fixing the charged cuts.
            charged = tuple(sorted("t"+digits(shorter(map(int,g),n)[0]) for g in re.findall(r"\(([1-9]+)\)",row["charged_cuts"])))
            stabilizers = [(p,s) for p,s in permutations_all if move_den(charged,p) == charged]
            if len(stabilizers) != int(row["stabilizer_order"]):
                raise ValueError("Charged stabilizer")
            seen = set()
            for den, poly in sorted(polynomials.items()):
                if den in seen: continue
                maps = {move_den(den,p) for p,s in stabilizers}
                if not maps <= polynomials.keys():
                    raise ValueError("Missing neutral pole under exchange")
                for p,s in stabilizers:
                    if move_poly(poly,p,s) != polynomials[move_den(den,p)]:
                        raise ValueError(f"Numerators are not related by signed label exchange: {path} {seed} {den}")
                seen.update(maps)
                divisor = len(stabilizers)//len(maps)
                result.append(dict(poles=list(den), divisor=divisor, terms=[[v,*mon] for mon,v in sorted(poly.items())]))
                count += len(permutations_all)//divisor
    return dict(particles=legs, charge_power=n-2, sqrt_two_power=n-2*k,
                permutation_order=len(permutations_all), pole_products=count, fractions=result)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("input",type=Path)
    parser.add_argument("output",type=Path)
    a = parser.parse_args()
    result = catalog(a.input)
    a.output.write_text(json.dumps(result,separators=(",",":"))+"\n")
    print(f"{len(result['fractions'])} fractions before external permutations, {result['pole_products']} labeled pole products")
