"""Check the printed nine-point numerator with four explicit mass factors.

Supply directories of frozen +++ or ++- spinor seeds. Select the unique sewing by
its four charged propagators, independently of the representative numbering.
The ordinary chain antisymmetry is sufficient for the printed coefficient.
"""
import csv
import re
import sys
from collections import Counter
from pathlib import Path


def arguments(text):
    """Split the arguments of an ordinary native FullForm expression."""
    head, opening, body = text.partition("[")
    if not opening:
        return head, []
    if not body.endswith("]"):
        raise ValueError(f"Incomplete {head} expression")
    body = body[:-1]
    items, start, depth = [], 0, 0
    for i, char in enumerate(body):
        if char == "[":
            depth += 1
        elif char == "]":
            depth -= 1
        elif char == "," and depth == 0:
            items.append(body[start:i])
            start = i + 1
    return head, items + [body[start:]]


def cut(labels):
    labels = frozenset(labels)
    other = frozenset(range(1, 10)) - labels
    return min((labels, other), key=lambda group: (len(group), sorted(group)))


def bracket(expression, image, all_positive):
    head, ends = arguments(expression)
    if head != "SpinorChain" or len(ends) != 2:
        raise ValueError("Expected two endpoints at the highest mass power")
    labels, chiralities = [], []
    for endpoint in ends:
        kind, (spin, chirality, leg) = arguments(endpoint)
        chiralities.append(chirality)
        if kind != "Spinor":
            raise ValueError(endpoint)
        if spin == '"Xi"':
            if leg != "901":
                raise ValueError("Unexpected reference")
            if all_positive and chirality != '"Angle"':
                raise ValueError("Unexpected square reference in the all-positive amplitude")
            label = "reference" if all_positive else (9 if chirality == '"Angle"' else 7)
        else:
            label = image[int(leg) - 1]
        labels.append(label)
    if len(set(chiralities)) != 1:
        raise ValueError("Unexpected mixed-chirality simple bracket")
    a, b = labels
    if a == b:
        return "", 0
    if "reference" in labels:
        # The printed [9(12)i> is <r i>. An occurrence of <i r>
        # picks up a minus sign before it is collected in the fraction.
        endpoint = b if a == "reference" else a
        return f"[9(12){endpoint}>", 1 if a == "reference" else -1
    h = "<" if chirality == '"Angle"' else "["
    return h + "".join(map(str, sorted(labels))), (a < b) - (b < a)


def check(directory):
    charged = {cut(g) for g in ((4, 8), (5, 9), (1, 2, 3), (1, 2, 3, 7))}
    photons = {cut((2, 3)), cut((5, 6, 9))}
    legs = (directory / "checked-order.txt").read_text().split()[:9]
    if legs[:8] != list("eEeEeE") + ["A.+", "A.+"] or legs[8:] not in (["A.+"], ["A.-"]):
        raise ValueError("Expected a checked +++ or ++- construction")
    all_positive = legs[-1] == "A.+"
    wanted_denominator = ["[9(12)7>", "[9(12)8>", "[9(12)9>"] if all_positive else ["<79", "<89", "[79"]
    matching = []
    with (directory / "orbits.tsv").open() as inventory:
        for row in csv.DictReader(inventory, delimiter="\t"):
            subsets = re.findall(r"\(([1-9]+)\)", row["charged_cuts"])
            prefix = "seed-" + row["seed"]
            for line in (directory / (prefix + ".permutations")).read_text().splitlines():
                sign, *image = map(int, line.split())
                if {cut(image[int(i) - 1] for i in s) for s in subsets} == charged:
                    matching.append((prefix, sign, image))
    if len(matching) != 1:
        raise ValueError(f"Expected one labeled sewing, found {len(matching)}")
    prefix, sign, image = matching[0]
    head, monomials = arguments((directory / (prefix + ".expr")).read_text())
    if head != "Plus":
        raise ValueError("Expected the sum after the internal spin contractions")
    found = Counter()
    mass_four = 'Power[ModelParameter["Me","qed-reconstruction","Real"],4]'
    charge_seven = 'Power[ModelParameter["EE","qed-reconstruction","Real"],7]'
    sqrt_two_cubed = 'Power[Power[2,Rational[1,2]],3]'
    for term in monomials:
        _, factors = arguments(term)
        if mass_four not in factors:
            continue
        if factors.count(charge_seven) != 1 or factors.count(sqrt_two_cubed) != 1 or factors.count(mass_four) != 1:
            raise ValueError("Unexpected coupling")
        charged_poles, photon_poles, numerator, denominator = set(), set(), [], []
        weight = sign
        for f in factors:
            if re.fullmatch(r"-?\d+", f):
                weight *= int(f)
            elif f.startswith("Power[PropDen["):
                match = re.fullmatch(r"Power\[PropDen\[Mom\[Multiparticle\[([1-9,]+)\]\],(0|ModelParameter\[.*?\])\],-1\]", f)
                if not match:
                    raise ValueError(f)
                group = cut(image[int(i) - 1] for i in match[1].split(","))
                (photon_poles if match[2] == "0" else charged_poles).add(group)
            elif f.startswith("SpinorChain["):
                word, orientation = bracket(f, image, all_positive)
                numerator.append(word)
                weight *= orientation
            elif f.startswith("Power[SpinorChain["):
                _, (base, exponent) = arguments(f)
                if exponent != "-1":
                    raise ValueError(f)
                word, orientation = bracket(base, image, all_positive)
                if not orientation:
                    raise ValueError("The chosen reference makes a denominator identically zero")
                denominator.append(word)
                weight *= orientation
            elif f not in (mass_four, charge_seven, sqrt_two_cubed):
                raise ValueError(f)
        if photon_poles == photons and weight:
            if charged_poles != charged or sorted(denominator) != wanted_denominator:
                raise ValueError("Unexpected denominator")
            found[tuple(sorted(numerator))] += weight
    if all_positive:
        common = ["[9(12)5>", "[9(12)6>"]
        terms = [
            ["[9(12)4>", "[89", "<13", "[27"],
            ["[9(12)4>", "[89", "<12", "[37"],
            ["[48", "[79", "[9(12)2>", "[13"],
            ["[48", "[79", "[9(12)3>", "[12"],
        ]
    else:
        common = ["<49", "<59", "<69", "[78"]
        terms = [["<13", "[27"], ["<12", "[37"]]
    expected = {tuple(sorted(common + term)): -4 for term in terms}
    if {k: v for k, v in found.items() if v} != expected:
        raise ValueError("Printed signed coefficients differ from the sewing")
    print(f"{'+++' if all_positive else '++-'}: all {len(expected)} signed monomials "
          "and all denominators agree with the unique labeled sewing.")


if __name__ == "__main__":
    if len(sys.argv) < 2:
        sys.exit("Supply the checked directory for +++, ++-, or both.")
    for directory in sys.argv[1:]:
        check(Path(directory))
