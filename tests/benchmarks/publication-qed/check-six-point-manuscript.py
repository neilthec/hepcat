"""Check the printed six-point helicity formulas against constructive model data.

Run with a path to six-point-photon-helicities.tex, the reconstructed polynomial
directory, and the ordinary HEPCAT models directory. Requires SymPy. The check
reads the typeset brackets, signs, reference spinors and propagator products.
"""

import argparse
import json
import re
from pathlib import Path

import sympy as sp


def atom(word):
    return sp.Symbol(word)


def invariant(kind, labels):
    return atom(kind + "".join(sorted(labels)))


def equation(source, label):
    pattern = r"\\begin\{(?:equation|align)\}(.*?)\\end\{(?:equation|align)\}"
    for block in re.findall(pattern, source, re.S):
        for row in re.split(r"\\\\\s*(?=(?:\\M|N_[123])\s*)", block):
            if r"\label{" + label + "}" in row:
                return row
    raise ValueError(f"Missing printed equation {label}")


def math_body(source):
    source = re.sub(r"\\widehat\{(\d+)\}", r"\1", source)
    text = re.sub(r"\\(?:label|begin|end)\{[^}]+\}", "", source)
    text = re.sub(r"\\(?:nonumber|notag)|\\hspace\{[^}]+\}|\\mathbf\{(\d+)\}|\\mathbf(\d)", lambda m: m[1] or m[2] or "", text)
    text = re.sub(r"\\(?:bigg|Bigg|big|Big|left|right)(?:\\?)([{}()\[\]])",
                  lambda m: "(" if m[1] in "{([" else ")", text)
    return re.sub(r"\\\\|\\[!,;]|[\s&]", "", text)


def read_tex(text, labels=None):
    """Parse the scalar arithmetic actually used in these displayed equations."""
    text = math_body(text).replace("{}", "").rstrip(".,")
    if labels:
        text = re.sub(r"(?<=[{\[0-9ijk])([ijk])(?=[0-9ijk}\]\\])", lambda m: labels[m[1]], text)
        # An index can also follow \\langle (the last character is a letter).
        text = re.sub(r"(\\langle)([ijk])", lambda m: m[1]+labels[m[2]], text)
    stack = {}
    def store(value):
        key = "v" + str(len(stack)) + "z"
        stack[key] = value
        return key
    text = re.sub(r"\\ang\{([^{}]+)\}", lambda m: store(atom("<" + m[1] + ">")), text)
    text = re.sub(r"(\\langle|\[)([0-9()]+)(\\rangle|\])", lambda m:
                  store(atom(("<" if m[1] == r"\langle" else "[") + m[2]
                             + (">" if m[3] == r"\rangle" else "]"))), text)
    text = re.sub(r"(\\tau|s)_\{([0-9]+)\}", lambda m: store(invariant("t" if m[1] == r"\tau" else "s", m[2])), text)
    tokens = re.findall(r"v\d+z|\\frac|\d+|[m()+^{}-]|.", text)
    return arithmetic(tokens, stack)


def arithmetic(tokens, names):
    pos = 0
    def primary():
        nonlocal pos
        token = tokens[pos]
        pos += 1
        if token in ("{", "("):
            value = add()
            closing = "}" if token == "{" else ")"
            if pos >= len(tokens) or tokens[pos] != closing:
                raise ValueError(f"Missing {closing} near {tokens[max(0,pos-3):pos+3]}")
            pos += 1
        elif token == r"\frac":
            numerator = primary()
            value = numerator / primary()
        elif token == "-":
            value = -primary()
        elif token == "m":
            value = atom("m")
        elif token in names:
            value = names[token]
        elif token.isdigit():
            value = sp.Integer(token)
        else:
            raise ValueError(f"Unrecognized token {token} near {tokens[max(0,pos-3):pos+3]}")
        if pos < len(tokens) and tokens[pos] == "^":
            pos += 1
            value = value ** primary()
        return value
    def product():
        nonlocal pos
        value = primary()
        while pos < len(tokens) and tokens[pos] not in ("+", "-", ")", "}"):
            if tokens[pos] == "*":
                pos += 1
            value *= primary()
        return value
    def add():
        nonlocal pos
        value = product()
        while pos < len(tokens) and tokens[pos] in ("+", "-"):
            sign = 1 if tokens[pos] == "+" else -1
            pos += 1
            value += sign * product()
        return value
    value = add()
    if pos != len(tokens):
        raise ValueError(f"Unused input {tokens[pos:]}")
    return value


def read_model_body(file):
    prefix = ["e", "E", "A.+", "A.+", "A.+", "A.-"]
    rows = [[s.strip() for s in line.split("|")] for line in file.read_text().splitlines()]
    row, = [r for r in rows if r[:-2] == prefix]
    if row[-2].replace(" ", "") != "(4*(EE)^(4))":
        raise ValueError("Unexpected model prefactor")
    body = row[-1]
    names = {}
    def store(value):
        key = "v" + str(len(names)) + "z"
        names[key] = value
        return key
    body = re.sub(r"PropDen\[Mom\[Multiparticle\[([\d,]+)\]\],Me\]", lambda m:
                  store(invariant("t", m[1].replace(",", ""))), body)
    body = re.sub(r"s(\d+)-Me\^2", lambda m: store(invariant("t", m[1])), body)
    body = re.sub(r"(?:\[|<)\d+[\]>]", lambda m: store(atom(m[0])), body)
    return arithmetic(re.findall(r"v\d+z|\d+|.", body.replace("Me", "m")), names)


def same(x, y, claim):
    if sp.expand(x-y) != 0:
        raise ValueError(f"Printed expression differs from the constructive expression: {claim}")


def check_permutations(raw, one, two):
    """Compare the signs, ordering of exchanges and full propagator products."""
    def fraction(f, name, coefficient):
        den = "".join((r"\tau" if p[0]=="t" else "s")+"_{"+p[1:]+"}" for p in f["poles"])
        n = "" if coefficient == 1 else str(coefficient)
        partner = r"(1+\mathcalC)" if f["chirality"] == 1 else r"(1-\mathcalC)"
        return r"\frac{"+n+partner+name+"}{"+den+"}"

    def exchanges(f, pairs):
        return "".join(f"(1{'+' if s==1 else '-'}P_{{{pair}}})"
                       for pair,s in zip(pairs, f["exchanges"]))

    f, = one["fractions"]
    if (f["factor"], one["sqrt_two_power"], one["charge_power"]) != ([1,2],4,4):
        raise ValueError("The constructive four-photon normalization has changed")
    # Half of (sqrt(2))^4 is the factor 2 printed in front of B4.
    four = r"\sum_{\pi\inS_{3456}}\pi"+exchanges(f,["12"])+fraction(f,"N",1)
    a,b,c = two["fractions"]
    if (a["exchanges"] != b["exchanges"] or (two["sqrt_two_power"],two["charge_power"]) != (2,4)
            or any(x["factor"][1] != 1 for x in (a,b,c))):
        raise ValueError("The constructive two-pair normalization or exchanges have changed")
    ab = exchanges(a,["12","43"])+"("+fraction(a,"N_1",a["factor"][0])+"+"+fraction(b,"N_2",b["factor"][0])+")"
    last = exchanges(c,["12","43"])+fraction(c,"N_3",c["factor"][0])
    pairs = "(1-P_{14})(1-P_{23})(1+P_{56})("+ab+"+"+last+")"
    for label, want in (("eq:six-four-photon-polynomial",four), ("eq:six-two-pair-polynomial",pairs)):
        expression = equation(raw,label).split("=",1)[1]
        if math_body(expression).replace("{}","").rstrip(".,") != want:
            raise ValueError(f"Permutation, sign or denominator differs: {label}")


def verify(source, directory, model_dir):
    raw = source.read_text()
    one, two = [json.loads((directory/f"6-{k}.short.json").read_text()) for k in (1,2)]
    labels = ("eq:six-four-photon-numerator", "eq:six-two-pair-na",
              "eq:six-two-pair-nb", "eq:six-two-pair-nc")
    for label, f in zip(labels, one["fractions"]+two["fractions"]):
        text = equation(raw, label).split("=", 1)[1].strip().rstrip(".,")
        value = read_tex(text)
        target = sum(c * sp.prod(atom(w) for w in words) for c,*words in f["terms"])
        same(value, target, label)
    check_permutations(raw, one, two)

    cyclic = equation(raw, "eq:six-pppm-physical").split(r"\sum_{\rm cyc(i,j,k)}",1)
    if "4e^4" not in cyclic[0]:
        raise ValueError("Wrong overall normalization in +++-")
    value = sum(read_tex(cyclic[1].replace(r"\end{aligned}", "").strip().rstrip("."),
                         dict(zip("ijk", order))) for order in ("345","453","534"))
    same(value, read_model_body(model_dir/"amps4.mdl"), "physical +++- cyclic sum")

    for family, word, pole in (
            ("pppm","+++-","spurious"), ("ppmm","++--","physical"), ("ppmm","++--","spurious"),
            ("pairs-pp","++","physical"), ("pairs-pp","++","spurious"),
            ("pairs-pm","+-","physical"), ("pairs-pm","+-","spurious")):
        label = "eq:six-"+family+"-"+pole
        eq = equation(raw, label)
        legs = list(range(7-len(word), 7))
        top, bottom, denominators = [], [], []
        for j,(i,h) in enumerate(zip(legs,word)):
            ref = legs[(j+1)%len(legs)]
            if pole=="physical":
                top.append(("qbp" if h=="+" else "qa", i))
                bottom.append(("qb" if h=="+" else "qap", i))
                denominators.append(invariant("t", f"1{i}"))
            else:
                top.append(("qa", ref if h=="+" else i))
                bottom.append(("qb", i if h=="+" else ref))
                denominators.append(atom(f"<{ref}{i}>" if h=="+" else f"[{ref}{i}]"))
        formula = re.sub(r"\s+", "", eq)
        norm = re.search(r"\\frac\{2e\^4\}\{(.*?)\}\\,B_([24])", formula)
        if not norm or int(norm[2]) != len(legs):
            raise ValueError(f"Wrong coupling or polynomial: {label}")
        same(read_tex(norm[1]), sp.prod(denominators), label+" denominator")
        matrix = formula.split(r"\begin{pmatrix}")[1].split(r"\end{pmatrix}")[0]
        printed = [[(name,int(i)) for name,i in re.findall(r"\\(q[ab]p?)\{(\d)\}", row)]
                   for row in matrix.split(r"\\")]
        if printed != [top, bottom]:
            raise ValueError(f"Wrong external spinor or reference: {label}")
    print("All eight helicity and pole choices agree. Checked the cyclic sum, four factored numerators,")
    print("every pole product and permutation sign, and the seven external-spinor projections.")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("section", type=Path)
    parser.add_argument("polynomials", type=Path)
    parser.add_argument("models", type=Path)
    args = parser.parse_args()
    verify(args.section, args.polynomials, args.models)
