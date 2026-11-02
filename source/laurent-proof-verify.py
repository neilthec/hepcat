"""Read-only verifier of a factor-dag.py certificate and its two DAG files."""
from collections import Counter
from fractions import Fraction
import hashlib
import json
from pathlib import Path
import sys

if not __debug__:
    raise RuntimeError("Exact factor verification requires enabled assertions")

source_path, output_path, certificate_path = map(Path, sys.argv[1:])
if max(source_path.stat().st_size, output_path.stat().st_size) > 32000000 or certificate_path.stat().st_size > 128000000:
    raise RuntimeError("VerificationByteLimit")
source_text = source_path.read_bytes().decode("utf-8")
output_text = output_path.read_bytes().decode("utf-8")
certificate = json.loads(certificate_path.read_text())
assert certificate["Status"] == "COMPLETE"
assert hashlib.sha256(source_text.encode()).hexdigest() == certificate["SourceSHA256"]
assert hashlib.sha256(output_text.encode()).hexdigest() == certificate["OutputSHA256"]

def parse(text):
    definitions = [None]
    roots = []
    for fields in map(str.split, text.splitlines()):
        if not fields or fields[0].startswith("#"):
            continue
        if fields[0] == "Root":
            roots.append(int(fields[1]))
        elif fields[0] in ("Constant", "Variable", "Add", "Multiply", "Power"):
            assert int(fields[1]) == len(definitions)
            kind = fields[0]
            if kind == "Constant":
                value = Fraction(int(fields[2]), int(fields[3]))
                arguments = (str(value.numerator), str(value.denominator))
            elif kind in ("Add", "Multiply"):
                arguments = tuple(map(int, fields[3:]))
                assert int(fields[2]) == len(arguments)
            else:
                arguments = tuple(map(int, fields[2:]))
            definitions.append((kind, arguments))
    return definitions, roots

source, source_roots = parse(source_text)
output, output_roots = parse(output_text)
middle = [None] + [(kind, tuple(arguments)) for kind, arguments in certificate["IntermediateDefinitions"]]
mapping = [None] + certificate["OriginalNodeToIntermediate"]
renumber = {int(key): value for key, value in certificate["RetainedIntermediateToOutput"].items()}
lookup = {definition: identifier for identifier, definition in enumerate(middle) if identifier}
assert len(lookup) == len(middle) - 1
assert len(mapping) == len(source)

def factor_list(identifier):
    kind, arguments = middle[identifier]
    return arguments if kind == "Multiply" else (identifier,)

def plan_node(plan):
    kind, arguments = plan
    if kind == "atom":
        assert 1 <= arguments < len(middle)
        return arguments
    children = [plan_node(child) for child in arguments]
    if kind == "times":
        flat = sorted(factor for child in children for factor in factor_list(child))
        return flat[0] if len(flat) == 1 else lookup[("Multiply", tuple(flat))]
    assert kind == "plus"
    return children[0] if len(children) == 1 else lookup[("Add", tuple(sorted(children)))]

def expand(plan):
    kind, arguments = plan
    if kind == "atom":
        return {(arguments,): 1}
    parts = [expand(child) for child in arguments]
    if kind == "plus":
        answer = Counter()
        for part in parts:
            answer.update(part)
        return dict(answer)
    assert kind == "times"
    answer = {(): 1}
    for part in parts:
        product = Counter()
        for left, a in answer.items():
            for right, b in part.items():
                product[tuple(sorted(left + right))] += a * b
        answer = dict(product)
    return answer

for witness in certificate["LocalWitnesses"]:
    terms = [tuple(term) for term in witness["InputTerms"]]
    assert expand(witness["OutputPlan"]) == dict(Counter(terms))
    assert plan_node(witness["OutputPlan"]) == witness["OutputNode"]

add_certificates = {item["OriginalNode"]: item for item in certificate["OriginalAddCertificates"]}
for identifier, definition in enumerate(source[1:], 1):
    kind, arguments = definition
    mapped = mapping[identifier]
    if kind in ("Constant", "Variable"):
        assert middle[mapped] == definition
    elif kind == "Power":
        assert middle[mapped] == (kind, (mapping[arguments[0]], arguments[1]))
    elif kind == "Multiply":
        expected = sorted(factor for child in arguments for factor in factor_list(mapping[child]))
        assert tuple(expected) == factor_list(mapped)
    else:
        assert kind == "Add"
        witness = add_certificates.pop(identifier)
        expected = Counter(factor_list(mapping[child]) for child in arguments)
        assert Counter(tuple(term) for term in witness["InputTerms"]) == expected
        assert expand(witness["OutputPlan"]) == dict(expected)
        assert plan_node(witness["OutputPlan"]) == mapped == witness["MappedNode"]
assert not add_certificates

assert set(renumber.values()) == set(range(1, len(output)))
for old, new in renumber.items():
    kind, arguments = middle[old]
    if kind in ("Add", "Multiply"):
        expected = kind, tuple(renumber[child] for child in arguments)
    elif kind == "Power":
        expected = kind, (renumber[arguments[0]], arguments[1])
    else:
        expected = kind, arguments
    assert output[new] == expected
assert source_roots == certificate["OriginalRoots"]
assert output_roots == certificate["OutputRoots"] == [renumber[mapping[root]] for root in source_roots]
print(f"Factor witnesses PASS: {len(source)-1} original nodes, {len(output)-1} output nodes, "
      f"{len(certificate['LocalWitnesses'])} local distributive identities, {len(source_roots)} roots")
