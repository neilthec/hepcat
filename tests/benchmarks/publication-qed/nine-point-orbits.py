"""Check the nine-point occupation sums against independently labeled QED trees."""
from collections import Counter
from itertools import combinations, permutations, product
from math import comb, factorial, prod
import sys


def labeled_trees(pairs):
    edges = list(combinations(range(pairs), 2))
    for tree in combinations(edges, pairs - 1):
        visited = {0}
        for _ in range(pairs - 1):
            for a, b in tree:
                if a in visited or b in visited:
                    visited.update((a, b))
        if len(visited) == pairs:
            yield tree


def shape(tree, labels):
    return tuple(sorted(tuple(sorted((labels[a], labels[b]))) for a, b in tree))


# A separate transcription checks the actual denominators printed in the paper.
# The path is ordered from one end to the other, and the star has its hub first.
occupation_table = {
    (2,0): [('abcde|0',120),('abcd|e',24),('abc|de',12)],
    (2,1): [('abcdh|0',24),('abcd|h',24),('abch|d',6),('abc|dh',6),('abh|cd',4)],
    (2,2): [('abchi|0',12),('abc|hi',12),('abch|i',6),('abhi|c',4),('ab|chi',4),('abh|ci',2)],
    (3,0): [('abc|0|0',6),('ab|c|0',2),('ab|0|c',2),('a|bc|0',2),('a|b|c',2),('0|abc|0',12)],
    (3,1): [('abh|0|0',2),('ab|h|0',2),('ab|0|h',2),('a|h|b',2),('h|ab|0',2),
            ('ah|b|0',1),('a|bh|0',1),('a|b|h',1),('ah|0|b',1),('0|abh|0',4)],
    (4,0): [('star:a|0|0|0',6),('star:0|a|0|0',2),('a|0|0|0',1),('0|a|0|0',1)]
}

def check_printed_table(pairs,negatives,expected):
    if pairs==1:
        return
    photons=9-2*pairs
    letters='abcdefg'[:photons-negatives]+'hij'[:negatives]
    observed=Counter()
    for word,divisor in occupation_table[pairs,negatives]:
        star=word.startswith('star:')
        cells=word.split(':')[-1].split('|')
        tree=tuple((0 if star else i-1,i) for i in range(1,pairs))
        assignment=[next(v for v,cell in enumerate(cells) if c in cell) for c in letters]
        copies=Counter()
        for image in permutations(range(pairs)):
            for left in permutations(range(photons-negatives)):
                for right in permutations(range(photons-negatives,photons)):
                    key=shape(tree,image),tuple(image[assignment[k]] for k in left+right)
                    copies[key]+=1
        if set(copies.values())!={divisor}:
            raise ValueError(f'Incorrect divisor {divisor} in printed term {word}')
        observed.update(copies.keys())
    if observed!=Counter({key:1 for key in expected}):
        raise ValueError('Printed terms duplicate or omit an attachment allocation')

def check_central_words(negatives):
    """Expand the two triple currents and the two external Bose sums."""
    positives = 7 - negatives
    observed = Counter()
    kernels = 0
    for center in (0, 1):
        for left in range(4):
            right = negatives - left - center
            if right not in range(4):
                continue
            kernels += 1
            copies = Counter()
            for plus in permutations(range(positives)):
                for minus in permutations(range(positives, 7)):
                    first = plus[:3-left] + minus[:left]
                    middle = minus[left] if center else plus[3-left]
                    last = plus[4-left-center:] + minus[left+center:]
                    for a, b in product(permutations(first), permutations(last)):
                        copies[a+(middle,)+b] += 1
            divisor = prod(factorial(n) for n in (left, 3-left, right, 3-right))
            if set(copies.values()) != {divisor}:
                raise ValueError("Incorrect central-word stabilizer")
            observed.update(copies.keys())
    if observed != Counter(permutations(range(7))):
        raise ValueError("Central sum omits or duplicates a seven-photon word")
    return kernels


def check(pairs, negatives):
    photons = 9 - 2 * pairs
    positives = photons - negatives
    expected = Counter()
    for tree in labeled_trees(pairs):
        degree = Counter(v for edge in tree for v in edge)
        for allocation in product(range(pairs), repeat=photons):
            orders = prod(factorial(degree[v] + allocation.count(v)) for v in range(pairs))
            expected[tree, allocation] = orders

    check_printed_table(pairs,negatives,expected)
    images = list(permutations(range(pairs)))
    plus_orders = list(permutations(range(positives)))
    minus_orders = list(permutations(range(positives, photons)))
    checked = Counter()
    kernels = 0
    for tree in labeled_trees(pairs):
        if tree != min(shape(tree, p) for p in images):
            continue
        autos = [p for p in images if shape(tree, p) == tree]
        degree = Counter(v for edge in tree for v in edge)
        for allocation in product(range(pairs), repeat=photons):
            inside = lambda x: tuple(sorted(x[:positives])) + tuple(sorted(x[positives:]))
            def mapped(p):
                return inside(tuple(p[v] for v in allocation))
            if min(mapped(p) for p in autos) != allocation:
                continue
            kernels += 1
            stabilizer = sum(mapped(p) == allocation for p in autos)
            for block in (allocation[:positives], allocation[positives:]):
                stabilizer *= prod(factorial(k) for k in Counter(block).values())
            orders = prod(factorial(degree[v] + allocation.count(v)) for v in range(pairs))
            occurrences = Counter()
            for p in images:
                for plus, minus in product(plus_orders, minus_orders):
                    key = shape(tree, p), tuple(p[allocation[i]] for i in plus + minus)
                    occurrences[key] += 1
            if set(occurrences.values()) != {stabilizer}:
                raise ValueError("Incorrect orbit divisor")
            for key in occurrences:
                checked[key] += orders
    if checked != expected:
        raise ValueError("Labeled trees differ from the projected sum")
    channels = 2**photons * comb(2*pairs, pairs-1) - 2*pairs
    channels += 2**(photons-1) * (comb(2*pairs, pairs)-2)
    # With one line its central vertex distinguishes a photon and two
    # triples. Count the inequivalent choices in this shorter expression.
    if pairs == 1:
        kernels = check_central_words(negatives)
    return kernels, factorial(pairs) * sum(checked.values()), channels


def ordered_words(pairs):
    bars = (2,3) if pairs == 2 else tuple(range(2, 2*pairs+1, 2))
    kets = (1,4) if pairs == 2 else tuple(range(1, 2*pairs, 2))
    photons = range(2*pairs+1, 10)
    for tree in labeled_trees(pairs):
        exchanged = [[] for _ in bars]
        for a,b in tree:
            exchanged[a].append(-bars[b])
            exchanged[b].append(-bars[a])
        for allocation in product(range(pairs), repeat=len(photons)):
            ports = [row.copy() for row in exchanged]
            for i, v in zip(photons, allocation):
                ports[v].append(i)
            for words in product(*(permutations(row) for row in ports)):
                for matching in permutations(kets):
                    yield tuple(zip(matching, words))


def compare_with_planner(lines):
    actual = Counter()
    for line in lines:
        pairs, *words = line.split()
        parsed = [word.split(':') for word in words]
        key = tuple((int(ket),tuple(map(int, ports.rstrip(',').split(',')))) for ket, ports in parsed)
        actual[int(pairs),key] += 1
    for pairs in range(1,5):
        for key in ordered_words(pairs):
            if actual.pop((pairs,key), 0) != 1:
                raise ValueError(f'Missing or duplicate ordered tree: {key}')
    if actual:
        raise ValueError('Extra trees in planner')
    print('Every ordered word and external fermion matching agrees with the CalcHEP-model tree planner.')


if __name__ == '__main__':
    if sys.argv[1:] == ['--check-trees']:
        compare_with_planner(sys.stdin)
    print('pairs  helicity  kernels  labeled diagrams  channels')
    for p in range(1,5):
        n = 9 - 2*p
        for h in range(1 + n//2):
            print(p, '+'*(n-h) + '-'*h, *check(p,h))
