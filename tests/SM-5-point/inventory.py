#!/usr/bin/env python3
"""Enumerate model-1 five-leg trees and complete each numerical helicity family.

This is a graph/particle inventory, not a claim that an amplitude has been
constructed or validated. Three trivalent vertices form a path; cutting either
end gives a four-leg current sewn to a trivalent vertex. The other possible
five-leg tree has one trivalent and one four-valent vertex.
"""
from __future__ import annotations
import argparse
import ast
import math
import operator
from collections import Counter, defaultdict, deque
import hashlib
import itertools
import json
from pathlib import Path
import re

SCHEMA_VERSION = 1
SECTOR_ORDER = ["PureHiggs", "MassiveElectroweak", "ElectroweakPhoton", "PureGluon",
                "TwoFermionHiggs", "TwoFermionElectroweak", "TwoFermionQCD",
                "FourFermionHiggs", "FourFermionElectroweak", "FourFermionPhoton", "FourFermionGluon"]
QUARK_GENERATIONS = (("u", "d"), ("c", "s"), ("t", "b"))
LEPTON_GENERATIONS = (("ne", "e"), ("nm", "m"), ("nl", "l"))


def stable_id(particles, prefix="sm5_"):
    return prefix + hashlib.sha256("|".join(sorted(particles)).encode()).hexdigest()[:12]


def base_particle(particle):
    return particle[0] if particle in {"A.+", "A.-", "G.+", "G.-"} else particle


def load_model(root):
    directory = root / "models"
    particles = {}
    for line in (directory / "prtcls1.mdl").read_text().splitlines():
        row = [part.strip() for part in line.split("|")]
        if len(row) > 10 and row[4].isdigit():
            for name, anti in ((row[1], row[2]), (row[2], row[1])):
                particles[name] = {"Antiparticle": anti, "Spin2": int(row[4]),
                                   "Mass": row[5], "ColorDimension": int(row[7]), "Chirality": row[8]}
    vertices = []
    for line in (directory / "amps1.mdl").read_text().splitlines():
        row = [part.strip() for part in line.split("|")]
        if len(row) >= 6 and row[0] in particles:
            vertices.append({"Particles": tuple(part for part in row[:4] if part),
                             "Factor": re.sub(r"\s+", "", row[4]),
                             "Expression": re.sub(r"\s+", "", row[5])})
    maps_path = root / "tests/checks/fixtures/sm-factorization-catalog.json"
    maps = json.loads(maps_path.read_text())["FlavorMaps"]
    return particles, vertices, maps, maps_path



def numerical_masses(root, particles):
    """Read literal parameters and arithmetic mass dependencies without eval."""
    parameters = {}
    pending = {}
    for filename in ("vars1.mdl", "func1.mdl"):
        for line in (root / "models" / filename).read_text().splitlines():
            row = [part.strip() for part in line.split("|")]
            if len(row) >= 2 and re.fullmatch(r"[A-Za-z][A-Za-z0-9]*", row[0]):
                pending[row[0]] = row[1].split("%", 1)[0].strip()
    binary = {ast.Add: operator.add, ast.Sub: operator.sub, ast.Mult: operator.mul,
              ast.Div: operator.truediv, ast.Pow: operator.pow}
    def evaluate(node):
        if isinstance(node, ast.Constant) and isinstance(node.value, (int, float)):
            return node.value
        if isinstance(node, ast.Name):
            return parameters[node.id]
        if isinstance(node, ast.BinOp) and type(node.op) in binary:
            return binary[type(node.op)](evaluate(node.left), evaluate(node.right))
        if isinstance(node, ast.UnaryOp) and isinstance(node.op, (ast.UAdd, ast.USub)):
            return evaluate(node.operand) * (-1 if isinstance(node.op, ast.USub) else 1)
        if (isinstance(node, ast.Call) and isinstance(node.func, ast.Name)
                and node.func.id == "Sqrt" and len(node.args) == 1 and not node.keywords):
            return math.sqrt(evaluate(node.args[0]))
        raise ValueError("Unsupported arithmetic parameter expression")
    while pending:
        resolved = []
        for name, expression in pending.items():
            try:
                value = evaluate(ast.parse(expression.replace("^", "**").replace("[", "(").replace("]", ")"), mode="eval").body)
            except (SyntaxError, ValueError, KeyError, ZeroDivisionError):
                continue
            if math.isfinite(value):
                parameters[name] = float(value)
                resolved.append(name)
        if not resolved:
            break
        for name in resolved:
            del pending[name]
    result = {}
    for name, data in particles.items():
        mass = data["Mass"]
        result[name] = 0.0 if mass == "0" else parameters[mass]
    return result

def certify_generation_maps(particles, vertices, maps):
    """Replay the existing bijective maps on every elementary row, without widths."""
    by_order = {vertex["Particles"]: vertex for vertex in vertices}
    certificates = []
    for index, mapping in enumerate(maps):
        names, masses = mapping["ParticleMap"], mapping["MassMap"]
        if len(set(names.values())) != len(names):
            raise ValueError("Generation map is not bijective")
        def mass_replace(text):
            return re.sub(r"[A-Za-z][A-Za-z0-9]*", lambda m: masses.get(m[0], m[0]), text)
        for vertex in vertices:
            key = tuple(names.get(p, p) for p in vertex["Particles"])
            target = by_order[key]
            if any(mass_replace(vertex[field]) != target[field] for field in ("Factor", "Expression")):
                raise ValueError(f"Generation map {index} changes an elementary vertex")
        for name, data in particles.items():
            target = particles[names.get(name, name)]
            for key in ("Spin2", "ColorDimension", "Chirality"):
                if target[key] != data[key]:
                    raise ValueError(f"Generation map {index} changes particle {key}")
            if target["Mass"] != masses.get(data["Mass"], data["Mass"]):
                raise ValueError(f"Generation map {index} changes particle mass role")
            if target["Antiparticle"] != names.get(data["Antiparticle"], data["Antiparticle"]):
                raise ValueError(f"Generation map {index} changes antiparticle role")
        certificates.append({"Map": index, "ElementaryVertices": len(vertices),
                             "ParticleRoles": len(particles), "Status": "EXACT_ROW_MATCH"})
    return certificates


def tree_signatures(particles, vertices):
    """Return supported multisets and topology provenance, without diagram algebra."""
    cubic_remainders = defaultdict(set)
    cubics = [v["Particles"] for v in vertices if len(v["Particles"]) == 3]
    quartics = [v["Particles"] for v in vertices if len(v["Particles"]) == 4]
    for vertex in cubics:
        for position, particle in enumerate(vertex):
            cubic_remainders[particle].add(vertex[:position] + vertex[position + 1:])
    four = set()
    for vertex in cubics:
        for position, particle in enumerate(vertex):
            for rest in cubic_remainders[particles[particle]["Antiparticle"]]:
                four.add(tuple(sorted(vertex[:position] + vertex[position + 1:] + rest)))
    five = defaultdict(set)
    for source, topology in ((four, "ThreeCubics"), (quartics, "CubicQuartic")):
        for vertex in source:
            for position, particle in enumerate(vertex):
                for rest in cubic_remainders[particles[particle]["Antiparticle"]]:
                    five[tuple(sorted(vertex[:position] + vertex[position + 1:] + rest))].add(topology)
    return five, four


def export_tokens(root):
    """Readable C++ names use the model's particle/antiparticle orientation."""
    tokens = {"W+": "Wp", "W-": "Wm", "A": "A", "G": "G", "h": "h", "Z": "Z"}
    for line in (root / "models/prtcls1.mdl").read_text().splitlines():
        row = [part.strip() for part in line.split("|")]
        if len(row) > 10 and row[4].isdigit() and int(row[4]) % 2:
            tokens[row[1]] = row[1]
            tokens[row[2]] = row[1] + "bar"
    return tokens


def generation_rank(signature, particles):
    rank = {}
    for generations in (QUARK_GENERATIONS, LEPTON_GENERATIONS):
        for generation, doublet in enumerate(generations):
            for particle in doublet:
                rank[particle] = generation
                rank[particles[particle]["Antiparticle"]] = generation
    # Favor first-generation representatives while preserving every equality pattern.
    return (sum(rank.get(p, 0) for p in signature),
            tuple(sorted(rank.get(p, 0) for p in signature)), signature)


def sector(signature, particles):
    fermions = sum(particles[p]["Spin2"] % 2 for p in signature if p in particles)
    bosons = Counter(base_particle(p) for p in signature
                     if p not in particles or particles[p]["Spin2"] % 2 == 0)
    if fermions == 0:
        name = ("PureHiggs" if set(bosons) == {"h"} else "PureGluon" if "G" in bosons
                else "ElectroweakPhoton" if "A" in bosons else "MassiveElectroweak")
    elif fermions == 2:
        name = ("TwoFermionQCD" if "G" in bosons else "TwoFermionHiggs" if "h" in bosons
                else "TwoFermionElectroweak")
    elif fermions == 4:
        name = ("FourFermionGluon" if "G" in bosons else "FourFermionPhoton" if "A" in bosons
                else "FourFermionHiggs" if "h" in bosons else "FourFermionElectroweak")
    else:
        raise ValueError(f"Unexpected fermion sector {fermions}")
    return name, fermions, dict(sorted(bosons.items()))


def family_order(signature, particles):
    """Choose a deterministic physical crossing; all incoming particles stay on shell."""
    values = list(signature)
    anti = lambda p: particles[p]["Antiparticle"] if p in particles else p
    fermion = lambda p: p in particles and particles[p]["Spin2"] % 2 == 1
    rank = {p: index for index, p in enumerate(
        ["e", "E", "ne", "Ne", "u", "U", "d", "D", "m", "M", "nm", "Nm",
         "c", "C", "s", "S", "l", "L", "nl", "Nl", "t", "T", "b", "B", "A", "G", "W+", "W-", "Z", "h"])}
    def pair_rank(pair):
        left, right = (values[i] for i in pair)
        fcount = int(fermion(left)) + int(fermion(right))
        category = (0 if fcount == 2 and anti(left) == right else 1 if fcount == 2 else
                    2 if left == right and left in {"A", "G"} else 3 if fcount else
                    4 if anti(left) == right else 5)
        return category, sorted((rank[left], rank[right])), pair
    pair = min(itertools.combinations(range(5), 2), key=pair_rank)
    incoming = sorted((values[i] for i in pair), key=rank.get)
    remaining = sorted((p for i, p in enumerate(values) if i not in pair), key=rank.get)
    return incoming + remaining


def slot_permutation(source, target):
    """One-based source-slot to target-slot bijection, including repeated particles."""
    positions = defaultdict(deque)
    for index, particle in enumerate(target, 1):
        positions[particle].append(index)
    result = [positions[particle].popleft() for particle in source]
    if sorted(result) != list(range(1, len(source) + 1)):
        raise ValueError("Invalid particle permutation")
    return result


def orbit_representatives(signatures, maps, particles):
    unassigned = set(signatures)
    result = {}
    for representative in sorted(signatures, key=lambda s: generation_rank(s, particles)):
        if representative not in unassigned:
            continue
        for index, mapping in enumerate(maps):
            target = tuple(sorted(mapping["ParticleMap"].get(p, p) for p in representative))
            if target in unassigned:
                result[target] = (representative, index)
                unassigned.remove(target)
    if unassigned:
        raise ValueError("Generation closure did not cover inventory")
    return result


def build_inventory(root):
    particles, vertices, maps, maps_path = load_model(root)
    certificates = certify_generation_maps(particles, vertices, maps)
    masses = numerical_masses(root, particles)
    name_tokens = export_tokens(root)
    trees, four = tree_signatures(particles, vertices)
    family_signatures = {tuple(sorted(base_particle(p) for p in signature)) for signature in trees}
    all_signatures = set()
    for family in family_signatures:
        choices = [(p + ".+", p + ".-") if p in {"A", "G"} else (p,) for p in family]
        all_signatures.update(tuple(sorted(assignment)) for assignment in itertools.product(*choices))
    case_reps = orbit_representatives(all_signatures, maps, particles)
    family_reps = orbit_representatives(family_signatures, maps, particles)
    family_orders = {s: family_order(s, particles) for s in family_signatures}
    families = []
    cases_by_signature = {}
    for family in family_signatures:
        family_id = stable_id(family, "sm5f_")
        order = family_orders[family]
        family_sector, fermions, bosons = sector(family, particles)
        slots = [i for i, p in enumerate(order, 1) if p in {"A", "G"}]
        assignments = []
        family_cases = {}
        for signs in itertools.product(("+", "-"), repeat=len(slots)):
            choice = {leg: sign for leg, sign in zip(slots, signs)}
            assigned = [p + "." + choice[i] if i in choice else p for i, p in enumerate(order, 1)]
            signature = tuple(sorted(assigned))
            case_id = stable_id(signature)
            family_cases.setdefault(signature, assigned)
            assignments.append({"Particles": assigned, "CaseID": case_id})
        incoming = order[:2]
        outgoing = [particles[p]["Antiparticle"] if p in particles else p for p in order[2:]]
        incoming_threshold = sum(masses.get(p, 0.0) for p in incoming)
        outgoing_threshold = sum(masses.get(p, 0.0) for p in outgoing)
        energy = max(1000.0, 2 * max(incoming_threshold, outgoing_threshold))
        kinematics = {"IncomingThresholdGeV": incoming_threshold,
                      "OutgoingThresholdGeV": outgoing_threshold,
                      "SuggestedSqrtSGeV": math.ceil(energy / 100.0) * 100.0}
        rep, map_index = family_reps[family]
        for signature, assigned in family_cases.items():
            representative, index = case_reps[signature]
            cases_by_signature[signature] = {
                "ID": stable_id(signature), "FamilyID": family_id, "Particles": assigned,
                "Signature": list(signature), "Sector": family_sector, "FermionCount": fermions,
                "BosonContent": bosons, "Incoming": incoming, "Outgoing": outgoing,
                "HasDiagrams": signature in trees, "Topologies": sorted(trees.get(signature, [])),
                "RepresentativeID": stable_id(representative), "FlavorMap": index,
                "IsRepresentative": signature == representative,
                "Kinematics": kinematics,
                "RequiresConstruction": signature in trees,
            }
        family_record = {"ID": family_id, "ExportName": "".join(name_tokens[p] for p in order),
                         "Signature": list(family), "Particles": order,
                         "CaseIDs": [stable_id(s) for s in sorted(family_cases)],
                         "Sector": family_sector, "FermionCount": fermions, "BosonContent": bosons,
                         "Incoming": incoming, "Outgoing": outgoing, "Kinematics": kinematics,
                         "MasslessBosonLegs": slots, "LabeledHelicityAssignmentCount": len(assignments),
                         "RepresentativeFamilyID": stable_id(rep, "sm5f_"), "FlavorMap": map_index,
                         "IsRepresentative": family == rep, "HelicityAssignments": assignments}
        families.append(family_record)
    export_names = [family["ExportName"] for family in families]
    if len(set(name.lower() for name in export_names)) != len(export_names):
        raise ValueError("Export names collide on a case-insensitive filesystem")
    if not all(re.fullmatch(r"[A-Za-z][A-Za-z0-9]*", name) for name in export_names):
        raise ValueError("Export name is not a portable C++ identifier")
    cases_by_id = {record["ID"]: record for record in cases_by_signature.values()}
    for record in cases_by_signature.values():
        representative = cases_by_id[record["RepresentativeID"]]
        mapping = maps[record["FlavorMap"]]["ParticleMap"]
        mapped = [mapping.get(p, p) for p in representative["Particles"]]
        record["MappedRepresentativeParticles"] = mapped
        record["RepresentativePermutation"] = slot_permutation(mapped, record["Particles"])
    for family in families:
        for assignment in family["HelicityAssignments"]:
            source = cases_by_signature[tuple(sorted(assignment["Particles"]))]
            assignment["SourcePermutation"] = slot_permutation(source["Particles"], assignment["Particles"])
    def priority_family(family):
        return (SECTOR_ORDER.index(family["Sector"]), not family["IsRepresentative"],
                -family["BosonContent"].get("h", 0), generation_rank(tuple(family["Signature"]), particles))
    families.sort(key=priority_family)
    order_index = {f["ID"]: index for index, f in enumerate(families)}
    cases = sorted(cases_by_signature.values(), key=lambda c: (order_index[c["FamilyID"]], c["Signature"]))
    summary = {"CubicVertices": sum(len(v["Particles"]) == 3 for v in vertices),
               "QuarticVertices": sum(len(v["Particles"]) == 4 for v in vertices),
               "FourLegCubicTreeMultisets": len(four), "TreeSupportedCases": len(trees),
               "Cases": len(cases), "Families": len(families),
               "NoDiagramHelicityCompletionCases": sum(not c["HasDiagrams"] for c in cases),
               "RepresentativeCases": sum(c["IsRepresentative"] for c in cases),
               "RepresentativeFamilies": sum(f["IsRepresentative"] for f in families),
               "LabeledHelicityAssignments": sum(f["LabeledHelicityAssignmentCount"] for f in families),
               "CasesByFermionCount": dict(sorted(Counter(str(c["FermionCount"]) for c in cases).items())),
               "FamiliesByFermionCount": dict(sorted(Counter(str(f["FermionCount"]) for f in families).items()))}
    sectors = [{"Name": name, "Cases": sum(c["Sector"] == name for c in cases),
                "Families": sum(f["Sector"] == name for f in families),
                "RepresentativeCases": sum(c["Sector"] == name and c["IsRepresentative"] for c in cases),
                "RepresentativeFamilies": sum(f["Sector"] == name and f["IsRepresentative"] for f in families)}
               for name in SECTOR_ORDER]
    files = ["models/amps1.mdl", "models/prtcls1.mdl", "models/vars1.mdl", "models/func1.mdl",
             str(maps_path.relative_to(root))]
    return {"SchemaVersion": SCHEMA_VERSION, "ModelNumber": 1, "FactorizationModelNumber": 2,
            "Scope": "Five external legs, model-1 tree interactions, diagonal generation mixing and massless neutrinos.",
            "Meaning": "An exhaustive progress inventory, not constructed amplitudes or completed validations.",
            "CrossingConvention": "All Particles are incoming; the first two define physical Incoming and the last three cross to antiparticles in Outgoing.",
            "PermutationConvention": "One-based source-slot to target-slot bijections. Apply HEPCAT typed relabeling, including fermion conventions; no numerical phase fitting or parity/conjugation reduction is used.",
            "FlavorConvention": "Only the 36 certified simultaneous bijective quark/lepton generation maps are used. Representative relations do not mark a case or numerical validation complete.",
            "Files": {name: hashlib.sha256((root / name).read_bytes()).hexdigest() for name in files},
            "Summary": summary, "Particles": particles, "MassesGeV": masses, "FlavorMaps": maps,
            "FlavorCertificates": certificates, "Sectors": sectors, "Cases": cases, "Families": families}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", type=Path, default=Path(__file__).resolve().parents[2])
    parser.add_argument("--output", type=Path, default=Path(__file__).resolve().with_name("inventory.json"))
    args = parser.parse_args()
    inventory = build_inventory(args.root.resolve())
    args.output.parent.mkdir(parents=True, exist_ok=True)
    args.output.write_text(json.dumps(inventory, indent=2) + "\n")
    print(json.dumps(inventory["Summary"], indent=2))
    print("Inventory:", args.output)

if __name__ == "__main__":
    main()
