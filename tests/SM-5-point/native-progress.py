"""Summarize the retained native five-point calculations across run directories."""
import argparse
import collections
import csv
import itertools
import json
import queue
import re
import subprocess
import threading
import time
from pathlib import Path


def native_pass(directory, case, points):
    expression = directory / (case + ".expr")
    if not expression.is_file() or not expression.stat().st_size:
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
    return "Feynman" in numeric and {"Reference", "Physical", "HighEnergy", "Spurious"} <= intrinsic


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
            if name not in families:
                continue
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


def run_family(command, stage_seconds):
    if stage_seconds is None:
        return subprocess.run(command).returncode
    stages = {"Generate and reduce", "Complete factorization", "Complete high energy and reduce"}
    lines = queue.Queue()
    with subprocess.Popen(command, stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
                          text=True, bufsize=1) as process:
        def read_lines():
            try:
                for line in process.stdout:
                    lines.put(line)
            finally:
                lines.put(None)

        reader = threading.Thread(target=read_lines, daemon=True)
        reader.start()
        deadline = None
        try:
            while True:
                try:
                    timeout = None if deadline is None else max(0, deadline - time.monotonic())
                    line = lines.get(timeout=timeout)
                except queue.Empty:
                    process.terminate()
                    print(f"Construction time limit reached during {stage}. Family remains open.", flush=True)
                    return 124
                if line is None:
                    return process.wait()
                print(line, end="", flush=True)
                if line.strip() in stages:
                    stage = line.strip()
                    deadline = time.monotonic() + stage_seconds
                elif line.strip() == "Validate":
                    deadline = None
        finally:
            if process.poll() is None:
                process.terminate()
            try:
                process.wait(timeout=5)
            except subprocess.TimeoutExpired:
                process.kill()
                process.wait()
            reader.join()


def main():
    here = Path(__file__).resolve().parent
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--results", type=Path, default=here.parent / "results/spinas-output/SM-5-point")
    parser.add_argument("--points", type=int, default=100000)
    action = parser.add_mutually_exclusive_group()
    action.add_argument("--pending", action="store_true", help="List the outstanding export families")
    action.add_argument("--run-pending", type=Path, metavar="EXECUTABLE",
                        help="Run the C++ constructor on each outstanding family, one at a time")
    parser.add_argument("--sector", help="Select one notebook sector")
    parser.add_argument("--stage-seconds", type=int, metavar="SECONDS",
                        help="Move on if one construction stage exceeds this time. Validation runs to completion.")
    parser.add_argument("--seed", type=int, help="Use a reproducible seed for new calculations")
    parser.add_argument("--cmake", type=Path, help="CMake executable for the external checks")
    parser.add_argument("--calchep", type=Path, help="CalcHEP installation")
    parser.add_argument("--madgraph", type=Path, help="MadGraph installation")
    parser.add_argument("--spinas", type=Path, help="SPINAS checkout")
    args = parser.parse_args()
    if args.points < 10000:
        parser.error("Use at least 10,000 points")
    inventory = json.loads((here / "inventory.json").read_text())
    families = {f["ExportName"]: f for f in inventory["Families"]}
    if args.sector:
        families = {n: f for n, f in families.items() if f["Sector"] == args.sector}
        if not families:
            parser.error("Unknown sector " + args.sector)
    native, full = collect(args.results, families, args.points)
    pending = [f for n, f in families.items() if n not in full]
    if args.stage_seconds is not None and (not args.run_pending or args.stage_seconds < 1):
        parser.error("--stage-seconds requires --run-pending and a positive number of seconds")
    if args.run_pending:
        runner = args.run_pending.resolve(strict=True)
        command = [str(runner), "--root", str(here.parents[1]), "--output", str(args.results.resolve()),
                   "--workers", "1", "--points", str(args.points)]
        for key in ("seed", "cmake", "calchep", "madgraph", "spinas"):
            value = getattr(args, key)
            if value is not None:
                command.extend(["--" + key, str(value)])
        failed = 0
        for index, family in enumerate(pending, 1):
            print(f"\nFamily {index}/{len(pending)}   {family['Sector']}   {family['ExportName']}", flush=True)
            code = run_family([*command, "--family", family["ID"]], args.stage_seconds)
            if code:
                failed += 1
                print(f"Family remains open (exit {code}).", flush=True)
        print(f"{len(pending)} families attempted, {failed} remain open from this sweep.")
        return int(failed != 0)
    if args.pending:
        for family in pending:
            print(family["Sector"], family["ExportName"], family["ID"], sep="\t")
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
    raise SystemExit(main())
