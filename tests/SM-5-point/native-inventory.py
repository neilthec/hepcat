"""Embed the notebook's five-point process order and names in the C++ example."""
import itertools
import json
from pathlib import Path
import sys


def generate(source, destination):
    inventory = json.loads(source.read_text())
    families = {f["ID"]: f for f in inventory["Families"]}
    cases = {c["ID"]: c for c in inventory["Cases"]}
    if len(families) != len(inventory["Families"]) or len(cases) != len(inventory["Cases"]):
        raise ValueError("Duplicate family or case identifier")
    rows = []
    emitted = set()
    for name, family in families.items():
        assignments = family["HelicityAssignments"]
        expected = set(itertools.product(*[(leg + ".+", leg + ".-") if leg in {"A", "G"} else (leg,)
                                           for leg in family["Particles"]]))
        if (len(assignments) != len(expected) or len(assignments) != family["LabeledHelicityAssignmentCount"]
                or {tuple(a["Particles"]) for a in assignments} != expected):
            raise ValueError(f"Repeated or missing helicity assignment in {name}")
        if {a["CaseID"] for a in assignments} != set(family["CaseIDs"]):
            raise ValueError(f"Missing helicity representative in {name}")
        for a in assignments:
            c = cases[a["CaseID"]]
            mapping = a["SourcePermutation"]
            if sorted(mapping) != [1, 2, 3, 4, 5] or any(c["Particles"][i] != a["Particles"][j - 1]
                                                          for i, j in enumerate(mapping)):
                raise ValueError(f"Incorrect helicity permutation for {a['CaseID']}")
            if sorted(a["Particles"]) != sorted(c["Particles"]) or c["FamilyID"] != name:
                raise ValueError(f"Incorrect assignment of {a['CaseID']}")
        for cid in family["CaseIDs"]:
            if cid in emitted:
                raise ValueError(f"Repeated case {cid}")
            emitted.add(cid)
            c = cases[cid]
            if c["Incoming"] != family["Incoming"] or c["Outgoing"] != family["Outgoing"]:
                raise ValueError(f"Crossing mismatch in {cid}")
            fields = [cid, name, family["ExportName"], c["Sector"], ",".join(c["Particles"]),
                      ",".join(c["Incoming"]), ",".join(c["Outgoing"])]
            rows.append("{" + ", ".join(map(json.dumps, fields)) + ", " +
                        str(c["HasDiagrams"]).lower() + ", " + str(len(assignments)) + "},\n")
    if emitted != set(cases):
        raise ValueError("Every case must belong to exactly one family")
    destination.write_text("// Generated from the five-point Mathematica inventory.\n" + "".join(rows))


if __name__ == "__main__":
    generate(*map(Path, sys.argv[1:]))
