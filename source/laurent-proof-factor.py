"""Exact subset factor collection on an existing Laurent DAG, with local witnesses."""
from collections import Counter
from fractions import Fraction
import hashlib
import json
import argparse
import re
from pathlib import Path
import sys
import time

started = time.monotonic()
maximum_seconds = 30
maximum_nodes = 100000
maximum_witness_products = 2000000
verification_products = 0
nodes = [None]
interned = {}
costs = [0]
witnesses = []
factor_cache = {}

def deadline():
    if time.monotonic() - started > maximum_seconds:
        raise RuntimeError("PreprocessorTimeLimit")

def node(kind, arguments):
    deadline()
    arguments = tuple(arguments)
    key = kind, arguments
    if key in interned:
        return interned[key]
    if len(nodes) > maximum_nodes:
        raise RuntimeError("PreprocessorNodeLimit")
    identifier = len(nodes)
    nodes.append(key)
    interned[key] = identifier
    if kind in ("Constant", "Variable"):
        estimate = 1
    elif kind == "Add":
        estimate = sum(costs[child] for child in arguments)
    elif kind == "Multiply":
        estimate = 1
        for child in arguments:
            estimate = min(10**9, estimate * costs[child])
    elif kind == "Power":
        child, exponent = arguments
        estimate = min(10**9, costs[child] ** min(max(0, exponent), 32)) if exponent >= 0 else 1
    else:
        raise RuntimeError("UnknownNodeType")
    costs.append(min(10**9, estimate))
    return identifier

def multiply(children):
    flat = []
    for child in children:
        kind, arguments = nodes[child]
        flat.extend(arguments if kind == "Multiply" else [child])
    flat.sort()
    return flat[0] if len(flat) == 1 else node("Multiply", flat)

def add(children):
    children = tuple(sorted(children))
    return children[0] if len(children) == 1 else node("Add", children)

def factors(identifier):
    kind, arguments = nodes[identifier]
    return tuple(arguments) if kind == "Multiply" else (identifier,)

def multiply_formal(left, right):
    global verification_products
    verification_products += len(left) * len(right)
    if verification_products > maximum_witness_products:
        raise RuntimeError("WitnessProductLimit")
    result = Counter()
    for a, coefficient_a in left.items():
        for b, coefficient_b in right.items():
            result[tuple(sorted(a + b))] += coefficient_a * coefficient_b
    return +result

def expand_formal(plan):
    # This checker uses only independent opaque atoms. It never examines the
    # definitions of the actual kinematic factors selected by the collector.
    kind, arguments = plan
    if kind == "atom":
        return Counter({(arguments,): 1})
    if kind == "plus":
        result = Counter()
        for child in arguments:
            result.update(expand_formal(child))
        return +result
    if kind == "times":
        result = Counter({(): 1})
        for child in arguments:
            result = multiply_formal(result, expand_formal(child))
        return result
    raise RuntimeError("UnknownWitnessPlan")

def plain_term(term):
    return multiply(term), ("times", [("atom", factor) for factor in term])

def collect(terms):
    deadline()
    terms = tuple(sorted(tuple(sorted(term)) for term in terms))
    if terms in factor_cache:
        return factor_cache[terms]
    frequencies = Counter()
    for term in terms:
        frequencies.update(set(term))
    eligible = [factor for factor, count in frequencies.items() if count >= 2]
    if not eligible:
        pieces = [plain_term(term) for term in terms]
        result = add([piece[0] for piece in pieces]), ("plus", [piece[1] for piece in pieces])
        factor_cache[terms] = result
        return result
    selected = max(eligible, key=lambda factor: ((frequencies[factor] - 1) * costs[factor], costs[factor], frequencies[factor], -factor))
    grouped, rest = [], []
    for term in terms:
        if selected in term:
            reduced = list(term)
            reduced.remove(selected)
            grouped.append(tuple(reduced))
        else:
            rest.append(term)
    grouped_node, grouped_plan = collect(grouped)
    piece_node = multiply([selected, grouped_node])
    piece_plan = ("times", [("atom", selected), grouped_plan])
    if rest:
        rest_node, rest_plan = collect(rest)
        output_node = add([piece_node, rest_node])
        output_plan = ("plus", [piece_plan, rest_plan])
    else:
        output_node, output_plan = piece_node, piece_plan
    expected = Counter(terms)
    actual = expand_formal(output_plan)
    if actual != expected:
        raise RuntimeError("OpaqueFactorIdentityFailed")
    witnesses.append({"SelectedFactor": selected, "InputTerms": terms,
                      "OutputPlan": output_plan, "OutputNode": output_node,
                      "FormalExpansionVerified": True, "FormalMonomials": len(actual)})
    result = output_node, output_plan
    factor_cache[terms] = result
    return result

def children(identifier):
    kind, arguments = nodes[identifier]
    return arguments if kind in ("Add", "Multiply") else arguments[:1] if kind == "Power" else ()

def process(source, destination, witness_path):
    if source.stat().st_size > 32000000:
        raise RuntimeError("InputByteLimit")
    original = source.read_bytes().decode("utf-8")
    lines = [line.split() for line in original.splitlines() if line.strip() and not line.lstrip().startswith("#")]
    if lines[0] != ["HEPCAT_LAURENT_DAG", "1"] or lines[-1] != ["End"]:
        raise RuntimeError("InvalidHeaderOrEnd")
    if len(lines[1]) != 2 or lines[1][0] != "Variables":
        raise RuntimeError("InvalidVariables")
    if not re.fullmatch(r"[+-]?[0-9]+", lines[1][1]):
        raise RuntimeError("InvalidVariables")
    variables = int(lines[1][1])
    if not 1 <= variables <= 16:
        raise RuntimeError("InvalidVariables")
    mapping = [None]
    roots = []
    rewritten_adds = 0
    original_add_certificates = []
    for fields in lines[2:-1]:
        deadline()
        if any(not re.fullmatch(r"[+-]?[0-9]+", item) for item in fields[1:]):
            raise RuntimeError("InvalidInteger")
        kind = fields[0]
        if kind == "Root":
            if len(fields) != 2 or len(roots) >= 4096:
                raise RuntimeError("InvalidRoot")
            roots.append(int(fields[1]))
            continue
        if roots or int(fields[1]) != len(mapping):
            raise RuntimeError("NonSequentialNode")
        original_id = int(fields[1])
        if original_id > 250000:
            raise RuntimeError("InputNodeLimit")
        if kind == "Constant":
            if len(fields) != 4 or max(len(fields[2]), len(fields[3])) > 20000:
                raise RuntimeError("InvalidConstant")
            rational = Fraction(int(fields[2]), int(fields[3]))
            result = node(kind, [str(rational.numerator), str(rational.denominator)])
        elif kind == "Variable":
            if len(fields) != 3:
                raise RuntimeError("InvalidVariable")
            index = int(fields[2])
            if not 1 <= index <= variables:
                raise RuntimeError("VariableIndex")
            result = node(kind, [index])
        elif kind in ("Add", "Multiply"):
            if len(fields) < 3:
                raise RuntimeError("InvalidChildren")
            child_ids = list(map(int, fields[3:]))
            if len(child_ids) != int(fields[2]) or any(child < 1 or child >= original_id for child in child_ids):
                raise RuntimeError("InvalidChildren")
            mapped_children = [mapping[child] for child in child_ids]
            if kind == "Multiply":
                result = multiply(mapped_children)
            else:
                input_terms = [factors(child) for child in mapped_children]
                result, plan = collect(input_terms)
                # Verify every original Add independently, including one-child
                # and unfactored cases, as the induction link to the source DAG.
                actual = expand_formal(plan)
                if actual != Counter(input_terms):
                    raise RuntimeError("OriginalAddIdentityFailed")
                original_add_certificates.append({"OriginalNode": original_id, "MappedNode": result,
                    "InputTerms": input_terms, "OutputPlan": plan, "FormalExpansionVerified": True})
                rewritten_adds += nodes[result][0] != "Add" or nodes[result][1] != tuple(sorted(mapped_children))
        elif kind == "Power":
            if len(fields) != 4:
                raise RuntimeError("InvalidPower")
            child, exponent = map(int, fields[2:])
            if child < 1 or child >= original_id or abs(exponent) > 1000000:
                raise RuntimeError("InvalidPower")
            result = node(kind, [mapping[child], exponent])
        else:
            raise RuntimeError("UnknownNodeType")
        mapping.append(result)
    if not roots or any(root < 1 or root >= len(mapping) for root in roots):
        raise RuntimeError("InvalidRoots")
    live = set()
    def visit(identifier):
        if identifier in live:
            return
        live.add(identifier)
        for child in children(identifier):
            visit(child)
    for root in roots:
        visit(mapping[root])
    identifiers = sorted(live)
    renumber = {old: new for new, old in enumerate(identifiers, 1)}
    emitted = ["HEPCAT_LAURENT_DAG 1", f"Variables {variables}"]
    for old in identifiers:
        kind, arguments = nodes[old]
        identifier = renumber[old]
        if kind in ("Constant", "Variable"):
            values = arguments
        elif kind in ("Add", "Multiply"):
            values = [len(arguments)] + [renumber[child] for child in arguments]
        else:
            values = [renumber[arguments[0]], arguments[1]]
        emitted.append(" ".join(map(str, [kind, identifier, *values])))
    for root in roots:
        emitted.append(f"Root {renumber[mapping[root]]}")
    emitted.append("End")
    output = "\n".join(emitted) + "\n"
    destination.write_text(output)
    report = {"Status": "COMPLETE", "Operation": "ExactSubsetLiteralFactorCollection",
              "SourceSHA256": hashlib.sha256(original.encode()).hexdigest(),
              "OutputSHA256": hashlib.sha256(output.encode()).hexdigest(),
              "Variables": variables, "OriginalNodes": len(mapping) - 1,
              "OutputNodes": len(identifiers), "RewrittenAdds": rewritten_adds,
              "OriginalRoots": roots, "OutputRoots": [renumber[mapping[root]] for root in roots],
              "OriginalNodeToIntermediate": mapping[1:], "RetainedIntermediateToOutput": renumber,
              "IntermediateDefinitions": nodes[1:], "LocalWitnesses": witnesses,
              "OriginalAddCertificates": original_add_certificates,
              "WitnessProducts": verification_products, "Seconds": time.monotonic() - started,
              "NoKinematicExpansion": True, "AllFormalExpansionsVerified": True}
    encoded = json.dumps(report, separators=(",", ":"))
    if len(encoded) > 128000000:
        raise RuntimeError("WitnessByteLimit")
    witness_path.write_text(encoded)
    print(json.dumps({key: value for key, value in report.items() if key not in
                      ("OriginalNodeToIntermediate", "RetainedIntermediateToOutput", "IntermediateDefinitions", "LocalWitnesses", "OriginalAddCertificates")}, indent=2))

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("input", type=Path)
    parser.add_argument("output", type=Path)
    parser.add_argument("witnesses", type=Path)
    parser.add_argument("--seconds", type=float, default=30)
    arguments = parser.parse_args()
    if not 0 < arguments.seconds <= 3600:
        raise SystemExit("Invalid time allowance")
    maximum_seconds = arguments.seconds
    process(arguments.input.resolve(), arguments.output.resolve(), arguments.witnesses.resolve())
