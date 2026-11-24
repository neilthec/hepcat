"""Verify the two printed four-pair spinor numerators against a checked model.
Run: python3 tests/benchmarks/publication-qed/check-four-pair-spinors.py models/amps4.mdl
One may supply a Shared[...] expression on its own. Exact integer and rational
arithmetic checks each coefficient after ordinary chain reversal and momentum
conservation. No amplitude benchmark supplies any numerator coefficients.
"""

import collections
import functools
import itertools
import re
import sys
from fractions import Fraction
from pathlib import Path


def canonical_monomial(k, perm, flip=False):
    """Relabel the spinors, choose complementary momenta, and orient each chain."""
    (m, spin) = k
    sign = 1
    ws = []
    for (l, i, mom, j, r) in spin:
        l ^= flip
        r ^= flip
        i = perm.get(i, i)
        j = perm.get(j, j)
        insert = []
        for b in mom:
            labels = tuple(sorted((perm.get(i, i) for i in b)))
            comp = tuple(sorted(set(range(1, 9)) - set(labels)))
            if (len(comp), comp) < (len(labels), labels):
                labels = comp
                sign *= -1
            insert.append(labels)
        seq = tuple(insert)
        if len(seq) % 2 and l or (not len(seq) % 2 and i > j):
            (i, j) = (j, i)
            (l, r) = (r, l)
            seq = seq[::-1]
            if not len(seq) % 2:
                sign *= -1
        ws.append((l, i, seq, j, r))
    return ((m, tuple(sorted(ws))), sign)


def exchanges(pairsets, flip):
    """Enumerate the local label exchanges and the optional parity copy."""
    for changes in itertools.product(range(2), repeat=len(pairsets)):
        perm = {}
        for ((i, j), change) in zip(pairsets, changes):
            if change:
                perm[i] = j
                perm[j] = i
        for rev in range(1 + flip):
            yield (perm, rev)

# Each entry gives a signed monomial after removing the common 8 e^6.
first_numerator = [
    (1, 'm^2 [27] [45] <16> <38>'),
    (1, 'm [27] [38] [5(348)4> <16>'),
    (1, '[27] [4(348)6> [5(127)1> <38>'),
    (1, 'm [27] [38] [5(127)1> <46>'),
]

second_numerator = [
    (1, '[25] [4(347)6> [8(125)1> <37>'),
    (1, 'm [25] [37] [8(125)1> <46>'),
    (1, 'm [25] [4(347)1> [68] <37>'),
    (-1, 'm^2 [25] [37] [68] <14>'),
    (-1, 'm [25] [4(125)1> <37> <68>'),
    (1, '[25] [37] <1(125)(347)4> <68>'),
    (1, 'm^2 [25] [46] <18> <37>'),
    (1, 'm [25] [37] [6(347)4> <18>'),
]


def readword(w):
    (m, ws) = (0, [])
    for part in w.split():
        if part == 'm':
            m = 1
            continue
        if part == 'm^2':
            m = 2
            continue
        ws.append((part[0] == '<', int(part[1]), tuple((tuple(map(int, b)) for b in re.findall('\\(([1-8]+)\\)', part))), int(part[-2]), part[-1] == '>'))
    return (m, tuple(ws))


def bits(s):
    b = sum((1 << i - 1 for i in s))
    return min((b, 255 ^ b), key=lambda n: (bin(n).count('1'), n))


def move(ds, p):
    vals = []
    for d in ds:
        b = d % 256
        vals.append(256 * (d // 256) + bits((p.get(i, i) for i in range(1, 9) if b & 1 << i - 1)))
    return tuple(sorted(vals))


def numerator_terms(s, z, ds):
    local = collections.Counter()
    for (c, text) in s:
        k = readword(text)
        for (p, flip) in exchanges(z, True):
            (r, sign) = canonical_monomial(k, p, flip)
            local[r] += c * sign
    return (ds, local)


def parity(t):
    return (-1) ** sum((a > b for (i, a) in enumerate(t) for b in t[i + 1:]))


def model_terms(filename):
    """Collect the exact stored coefficients by their five physical propagators."""
    with Path(filename).open() as src:
        for line in src:
            if line.startswith('Shared['):
                body = line.strip()
                break
            fields = [v.strip() for v in line.split('|')]
            if tuple((v for v in fields[:-2] if v)) == ('e', 'E') * 4:
                if fields[-2] != '1':
                    raise ValueError('Unexpected external factor')
                body = fields[-1]
                break
        else:
            raise ValueError('No four-pair amplitude in the input')
    if not body.startswith('Shared[') or not body.endswith(']'):
        raise ValueError('No checked shared expression')
    records = [''] + body[7:-1].split(';')

    def args(row):
        return [records[int(i)] for i in row[2:].split(',')]

    @functools.lru_cache(maxsize=None)
    def readchain(row):
        endpoints = re.findall('Spinor\\["Spin","(Angle|Square)",([1-8])\\]', row)
        if len(endpoints) != 2:
            raise ValueError(row)
        moms = [tuple(map(int, content.split(','))) for content in re.findall('Mom\\[Multiparticle\\[([1-8,]+)\\]\\]', row)]
        if len(moms) != row.count('Mom['):
            raise ValueError(row)
        ((l, a), (r, b)) = endpoints
        return (l == 'Angle', int(a), tuple(moms), int(b), r == 'Angle')

    @functools.lru_cache(maxsize=None)
    def readpower(row):
        (base, exp) = args(row)
        if base in ('Me', 'EE'):
            return (base, int(exp))
        pole = re.fullmatch('PropDen\\[Mom\\[Multiparticle\\[([1-8,]+)\\]\\],(0|Me)\\]', base)
        if not pole or exp != '-1':
            raise ValueError(row)
        return ('pole', bits(map(int, pole[1].split(','))) + 256 * (pole[2] == 'Me'))
    groups = collections.defaultdict(collections.Counter)
    if not records[-1].startswith('+:'):
        raise ValueError('Expected expanded sum')
    for row in args(records[-1]):
        if not row.startswith('*:'):
            raise ValueError('Expected one spinor product')
        coefficient = 1
        mass = 0
        charge = 0
        words = []
        dens = []
        for factor in args(row):
            if factor.startswith('SpinorChain['):
                words.append(readchain(factor))
            elif factor.startswith('^:'):
                (role, value) = readpower(factor)
                if role == 'Me':
                    mass += value
                elif role == 'EE':
                    charge += value
                else:
                    dens.append(value)
            elif factor == 'Me':
                mass += 1
            else:
                coefficient *= int(factor)
        if charge != 6 or abs(coefficient) != 8 or len(dens) != 5:
            raise ValueError('Unexpected charge, normalization or pole count')
        (word, sign) = canonical_monomial((mass, words), {})
        groups[tuple(sorted(dens))][word] += sign * coefficient // 8
    return groups


def check_paper_formulas(f):
    """Sum both printed seeds over same-charge permutations, including their signs."""
    d1 = tuple(sorted([bits([1, 2]), bits([3, 4]), bits([5, 6]), 256 + bits([1, 2, 7]), 256 + bits([3, 4, 8])]))
    d2 = tuple(sorted([bits([1, 2]), bits([3, 4]), bits([1, 2, 5, 6]), 256 + bits([1, 2, 5]), 256 + bits([3, 4, 7])]))
    pieces = [(*numerator_terms(first_numerator, [(1, 2), (3, 4), (5, 6)], d1), 1, {})]
    (d, n) = numerator_terms(second_numerator, [(1, 2), (3, 4)], d2)
    for (q, _) in exchanges([(5, 6), (7, 8)], False):
        pieces.append((d, n, Fraction((-1) ** (len(q) // 2), 2), q))
    image = collections.defaultdict(list)
    odds = (1, 3, 5, 7)
    evens = (2, 4, 6, 8)
    for es in itertools.permutations(odds):
        for ps in itertools.permutations(evens):
            pg = dict(zip(odds + evens, es + ps))
            sgn = parity(es) * parity(ps)
            for (ds, num, mult, q) in pieces:
                p = {i: pg[q.get(i, i)] for i in range(1, 9)}
                image[move(ds, p)].append((p, sgn * mult, num))
    matched = 0
    entries = 0

    def check(key, g):
        h = collections.Counter()
        for (p, sgn, n) in image.pop(key):
            for (k, c) in n.items():
                (alt, s) = canonical_monomial(k, p)
                h[alt] += sgn * s * c
        if h != g:
            delta = {k: (x, g[k]) for (k, x) in h.items() if x != g[k]}
            raise ValueError((key, len(delta), next(iter(delta.items()), None)))
    for (key, raw) in model_terms(f).items():
        check(key, raw)
        matched += 1
        entries += sum((abs(v) for v in raw.values()))
    if image:
        raise ValueError('Unmatched propagator products in the printed formulas')
    print(f'Exact signed coefficient comparison: {matched} sets of propagators, {entries} monomials.')

if __name__ == '__main__':
    check_paper_formulas(sys.argv[1])
