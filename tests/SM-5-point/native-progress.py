"""Summarize the retained native five-point calculations across run directories."""
import argparse
import collections
import csv
import itertools
import json
import re
from pathlib import Path


def native_pass(directory, case, points):
    if not (directory / (case + ".expr")).is_file():
        return False
    checks = directory / (case + ".checks.tsv")
    if not checks.is_file():
        return False
    lines = checks.read_text().splitlines()
    if not lines or not re.fullmatch(r"seed \d+, points \d+", lines[0]):
        return False
    requested = int(lines[0].split()[-1])
    if requested < points:
        return False
    numeric = set()
    intrinsic = set()
    for line in lines[1:]:
        fields = line.split("\t")
        if fields[0] in ("FinalReduction", "Feynman"):
            if len(fields) < 4 or fields[1:4] != ["PASS", str(requested), "0"]:
                return False
            numeric.add(fields[0])
        elif len(fields) < 3 or fields[2] != "PASS":
            return False
        else:
            intrinsic.add(fields[0])
    return "Feynman" in numeric and {"Reference", "HighEnergy", "Spurious"} <= intrinsic


def external_status(directory, family, points):
    mg = json.loads((directory / "madgraph-comparison.json").read_text())
    rows = mg.get("Rows", [])
    fixed = [tuple(r["Helicities"]) for r in rows if r.get("Kind") == "FixedHelicity"]
    variable = family["MasslessBosonLegs"]  # One-based locations of the photons and gluons.
    expected = set(itertools.product((-2, 2), repeat=len(variable)))
    if any(len(h) != 5 for h in fixed):
        return None
    projections = {tuple(h[i - 1] for i in variable) for h in fixed}
    if (not mg.get("Passed") or projections != expected or len(fixed) != len(expected)
            or len(rows) != len(fixed) + 1 or len(fixed) != len(set(fixed))
            or sum(r.get("Kind") == "PolarizationSum" for r in rows) != 1):
        return None
    if any(not r.get("Passed") or r["ComparedPoints"] < points
           or r["ComparedPoints"] != r["RequestedPoints"] or r["Mismatches"] or r["EvaluationErrors"]
           for r in rows):
        return None
    ch = (directory / "calchep-comparison.log").read_text()
    counts = re.search(r"^Points requested/generated/compared: (\d+)/(\d+)/(\d+)$", ch, re.M)
    errors = re.search(r"^Mismatches/errors: (\d+)/(\d+)$", ch, re.M)
    if (not counts or len(set(counts.groups())) != 1 or int(counts[1]) < points
            or not errors or errors[2] != "0"):
        return None
    if errors[1] == "0" and "Amplitude validation: PASS" in ch:
        return "PASS"
    replay = json.loads((directory / "calchep-precision.json").read_text())
    wide = replay["Results"]["GCC128"]
    if (replay["Status"] == "PRECISION_REPLAY_PASS" and replay["NativeControlReproducesCapture"]
            and wide["Status"] == "PASS" and wide["Points"] == int(errors[1]) > 0
            and not wide["Errors"] and not wide["Mismatches"]):
        return "PASS_AFTER_REPLAY"
    return None


def collect(results, families, points):
    native, full = set(), {}
    for ledger in sorted(results.glob("run-*/results.tsv")):
        with ledger.open() as stream:
            rows = list(csv.DictReader(stream, delimiter="\t"))
        by_name = collections.defaultdict(set)
        for row in rows:
            if row.get("status") == "NATIVE_PASS":
                by_name[row["family"]].add(row["id"])
        for name, cases in by_name.items():
            family = families[name]
            directory = ledger.parent / family["ID"]
            retained = {c for c in cases if native_pass(directory, c, points)}
            native.update(retained)
            if retained != set(family["CaseIDs"]) or not any(
                    r.get("status") == "EXTERNAL_PASS" and r["id"] == family["ID"]
                    and r["calchep_exit"] == r["madgraph_exit"] == "0" for r in rows):
                continue
            try:
                status = external_status(directory, family, points)
            except (OSError, KeyError, ValueError):
                continue
            if status:
                full[name] = (status, directory)
    return native, full


def main():
    here = Path(__file__).resolve().parent
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--results", type=Path, default=here.parent / "results/spinas-output/SM-5-point")
    parser.add_argument("--points", type=int, default=100000)
    parser.add_argument("--pending", action="store_true", help="List the outstanding export families")
    args = parser.parse_args()
    if args.points < 10000:
        parser.error("Use at least 10,000 points")
    inventory = json.loads((here / "inventory.json").read_text())
    families = {f["ExportName"]: f for f in inventory["Families"]}
    native, full = collect(args.results, families, args.points)
    if args.pending:
        for name, family in families.items():
            if name not in full:
                print(family["Sector"], name, family["ID"], sep="\t")
        return
    print(f"Retained native SM five-point results with at least {args.points:,} numerical points")
    print(f"{'Sector':26s} {'Signed':>7s} {'All checks':>11s} {'Cases':>6s} {'Families passed/total':>23s}")
    sectors = collections.defaultdict(list)
    for f in families.values():
        sectors[f["Sector"]].append(f)
    for sector, group in [*sectors.items(), ("Total", list(families.values()))]:
        cases = {c for f in group for c in f["CaseIDs"]}
        verified = [f for f in group if f["ExportName"] in full]
        both = sum(len(f["CaseIDs"]) for f in verified)
        print(f"{sector:26s} {len(cases & native):7d} {both:11d} {len(cases):6d}"
              f" {len(verified):16d}/{len(group)}")
    qualified = [name for name, (status, _) in full.items() if status == "PASS_AFTER_REPLAY"]
    if qualified:
        print("CalcHEP outliers passed a binary128 replay for " + ", ".join(sorted(qualified)))


if __name__ == "__main__":
    main()
