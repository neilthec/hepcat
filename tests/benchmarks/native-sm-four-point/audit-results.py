#!/usr/bin/env python3
"""Match signed native SM checks to independent CalcHEP and MadGraph family runs."""
import argparse
import csv
import hashlib
import json
import math
import os
import re
import sys
from collections import defaultdict
from decimal import Decimal, InvalidOperation, localcontext
from functools import lru_cache
from pathlib import Path

HERE = Path(__file__).resolve().parent
FIELDS = ("case", "family", "status", "weight", "notebook_weight", "leaves", "notebook_leaves",
          "wolfram_weight", "wolfram_leaves", "wolfram_version",
          "expression_sha256", "commutative_sha256", "external_sha256", "external_match", "native_seed", "notebook_points", "feynman_points", "final_reduction_points",
          "feynman_precision_replays", "external_seed", "calchep_points", "calchep_quad_points", "calchep_chart_points", "calchep_chart_delta",
          "calchep_unprojected_quad_max_rel", "calchep_chart_max_rel", "calchep_spinas_refinement_max_rel", "mg_helicity",
          "mg_fixed_points", "mg_sum_points", "mg_precision_bits", "mg_exact_zero_chart_delta",
          "mg_zero_raw_max_abs", "mg_zero_unprojected_quad_max_abs", "mg_zero_final_max_abs", "external_run")
INVALID = (OSError, ValueError, KeyError, TypeError, InvalidOperation)


def require(ok, message):
    if not ok:
        raise ValueError(message)


def table(path):
    with path.open() as stream:
        return list(csv.DictReader(stream, delimiter="\t", quoting=csv.QUOTE_NONE))


def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


# This is a parser for the generated, unspaced native FullForm wire format.
# Preserve every token, including string escapes, numeric spellings and empty
# argument lists. Only children of the two explicitly commutative heads sort.
def canonical_expression(raw):
    require(raw.endswith(b"\n") and not raw.endswith(b"\n\n"), "invalid expression terminator")
    source = raw[:-1]
    token = re.compile(rb'"(?:\\.|[^"\\])*"|[^\s\[\],"]+|[\[\],]')
    position = 0
    def item():
        nonlocal position
        match = token.match(source, position)
        require(match is not None and match[0] not in (b"[", b"]", b","), "invalid expression atom")
        head = match[0]
        position = match.end()
        if source[position:position + 1] != b"[":
            return head
        position += 1
        children = []
        if source[position:position + 1] != b"]":
            children.append(item())
            while source[position:position + 1] == b",":
                position += 1
                children.append(item())
        require(source[position:position + 1] == b"]", "unbalanced expression")
        position += 1
        if head in (b"Plus", b"Times"):
            children.sort()
        return head + b"[" + b",".join(children) + b"]"
    normalized = item()
    require(position == len(source), "trailing expression input")
    return normalized + b"\n"


@lru_cache(maxsize=1024)
def fingerprints(path):
    raw = path.read_bytes()
    return hashlib.sha256(raw).hexdigest(), hashlib.sha256(canonical_expression(raw)).hexdigest()


def complete_counts(numbers):
    require(len(set(numbers)) == 1 and numbers[0] >= 10000, f"incomplete point counts {numbers}")
    return numbers[0]


def model_particles():
    """Read physical PDG codes and all-incoming doubled helicities from elementary model 1."""
    pdgs, helicities = {}, {}
    for line in (HERE.parents[2] / "models/prtcls1.mdl").read_text().splitlines():
        cells = [c.strip() for c in line.split("|")]
        if len(cells) < 9 or not cells[3].isdigit():
            continue
        particle, anti = cells[1:3]
        pdg, spin = map(int, cells[3:5])
        for tag, sign in ((particle, 1), (anti, -1)):
            species = tag.split(".")[0]
            pdgs[species] = -pdg if sign < 0 and species != particle.split(".")[0] else pdg
            if tag.endswith((".+", ".-")):
                helicities[tag] = spin if tag.endswith("+") else -spin
            elif cells[8] in ("L", "R") and cells[5] == "0":
                helicities[tag] = sign * spin * (-1 if cells[8] == "L" else 1)
            else:
                helicities[tag] = 0
    return pdgs, helicities


def physical_helicity(case, helicities):
    legs = [helicities[tag] for tag in case["particles"].split(",")]
    return tuple(-h if i >= 2 and h % 2 else h for i, h in enumerate(legs))


def indexed(rows, key):
    items = {key(row): row for row in rows}
    require(len(items) == len(rows), "duplicate result rows")
    return items


def case_blocks(path, expected):
    blocks = {}
    current = None
    for line in path.read_text().splitlines():
        if line in expected:
            require(line not in blocks, f"duplicate native checks for {line}")
            current = blocks[line] = []
        elif current is not None and line:
            current.append(line)
        else:
            require(not line, f"unexpected native record {line[:90]}")
    require(set(blocks) == expected, "missing native case checks")
    return blocks


def native_case(row, case, lines):
    require(row["status"] == "PASS", "native construction or complexity gate failed")
    require(row["notebook_weight"] == case["notebook_weight"] and row["notebook_leaves"] == case["notebook_leaves"]
            and 0 <= int(row["weight"]) <= int(case["notebook_weight"]) and 0 < int(row["leaves"]) <= int(case["notebook_leaves"]),
            "native complexity disagrees with the catalog")
    match = re.fullmatch(r"seed (\d+)  requested (\d+)", lines[0]) if lines else None
    require(match and int(match[2]) >= 10000, "missing native seed or insufficient points")
    seed, requested = map(int, match.groups())
    kinds, counts = set(), {}
    for line in lines[1:]:
        if line.startswith("External configure "):
            continue
        parts = line.split("\t")
        require(len(parts) >= 3 and parts[0] == "PASS", f"native check failed or missing: {line[:80]}")
        kind = parts[1]
        kinds.add(kind)
        if kind in ("ReferenceAmplitude", "Feynman", "FinalReduction"):
            match = re.fullmatch(r"(\d+) points, (\d+) mismatches, .*?, precision replays (\d+)(?:, .*?)?", parts[2])
            require(match and kind not in counts, f"invalid signed {kind} check")
            points, failures, replays = map(int, match.groups())
            require(points == requested and not failures and replays <= points, f"incomplete {kind} check")
            counts[kind] = (points, replays)
    require({"Reference", "Physical", "Spurious", "HighEnergy", "ReferenceAmplitude", "Feynman"} <= kinds,
            "missing mandatory built-in or signed checks")
    return dict(native_seed=seed, notebook_points=counts["ReferenceAmplitude"][0], feynman_points=counts["Feynman"][0],
                feynman_precision_replays=counts["Feynman"][1], final_reduction_points=counts.get("FinalReduction", ("",))[0])


def wolfram_score(cross, case, native, fingerprint):
    require(cross["expression_sha256"] == fingerprint and cross["family"] == case["family"]
            and cross["native_weight"] == native["weight"] and cross["native_leaves"] == native["leaves"]
            and cross["notebook_weight"] == case["notebook_weight"] and cross["notebook_leaves"] == case["notebook_leaves"],
            "Wolfram cross-score does not match this native expression and notebook reference")
    require(cross["wolfram_version"] and cross["wolfram_weight"] == native["weight"]
            and 0 < int(cross["wolfram_leaves"]) <= int(case["notebook_leaves"]),
            "Wolfram complexity cross-check failed")
    return {key: cross[key] for key in ("wolfram_weight", "wolfram_leaves", "wolfram_version")}


def csv_records(path):
    with path.open() as stream:
        return indexed(list(csv.DictReader(stream)), lambda row: int(row["point"]))


def finite_decimal(value):
    number = Decimal(str(value))
    require(number.is_finite(), "nonfinite precision comparison")
    return number


def calchep_chart(directory, replay, original, seed):
    chart = replay["SameChartReplay"]
    wide = replay["Results"]["GCC128"]
    path = directory / "calchep-chart.csv"
    expected_hash = digest(path)
    require(chart["Status"] == "PASS" and chart["Seed"] == seed and chart["Mismatches"] == chart["Errors"] == 0
            and chart["SPINASMantissaBits"] == chart["CalcHEPMantissaBits"] == 113
            and chart["RequestedPoints"] == chart["ComparedPoints"] == len(original)
            and chart["SourceOutliersSHA256"] == replay["PointsSHA256"]
            and expected_hash == chart["InputSHA256"] == chart["SPINASInputSHA256"] == chart["CalcHEPInputSHA256"],
            "missing or one-sided CalcHEP common-input precision replay")
    require(wide["Status"] == "FAIL" and 0 < wide["Mismatches"] <= len(original), "missing failed unprojected binary128 control")
    inputs = csv_records(path)
    rows = indexed(chart["Rows"], lambda r: int(r["Point"]))
    require(inputs.keys() == original.keys() == rows.keys(), "CalcHEP chart replay lost or substituted an outlier")
    tolerance = finite_decimal(replay["RelativeTolerance"])
    limit = finite_decimal(chart["ComponentChangeLimitOverECM"])
    require(tolerance == finite_decimal(chart["RelativeTolerance"]) and 0 < tolerance <= Decimal("1e-7")
            and 0 < limit <= Decimal(str(16 * sys.float_info.epsilon)), "invalid CalcHEP chart tolerance")
    observed, shifts, unprojected, generated_changes = [], [], [], []
    masses = [Decimal.from_float(float(x)) for x in chart["Masses"]]
    require(len(masses) == 4 and all(m.is_finite() and m >= 0 for m in masses), "invalid CalcHEP chart masses")
    with localcontext() as ctx:
        ctx.prec = 64
        for index, row in rows.items():
            raw = original[index]
            require(float(row["RawSPINAS"]) == float(raw["hepcat"]) and float(row["RawCalcHEP"]) == float(raw["calchep"]),
                    "CalcHEP chart control changed the original amplitudes")
            old_reference = float(row["UnprojectedCalcHEP"])
            old_value = float(raw["hepcat"])
            require(math.isfinite(old_reference) and old_reference >= 0 and math.isfinite(old_value) and old_value >= 0,
                    "invalid unprojected CalcHEP control")
            scale_before = max(old_reference, old_value)
            unprojected.append(abs(old_reference - old_value) / scale_before if scale_before else 0)
            a, b = finite_decimal(row["SPINAS"]), finite_decimal(row["CalcHEP"])
            require(min(a, b) >= 0, "invalid squared amplitude")
            error = abs(a-b) / max(a, b) if max(a, b) else Decimal(0)
            require(error <= tolerance and math.isclose(error, finite_decimal(row["RelativeError"]), rel_tol=1e-8, abs_tol=1e-32),
                    "common-input CalcHEP amplitudes disagree")
            observed.append(error)
            original_result = finite_decimal(row["RawSPINAS"])
            scale_result = max(original_result, a)
            generated_change = abs(a - original_result) / scale_result if scale_result else Decimal(0)
            require(generated_change <= tolerance and math.isclose(generated_change, finite_decimal(row["RawToRefinedSPINASRelativeChange"]),
                    rel_tol=1e-8, abs_tol=1e-32), "SPINAS result changed beyond tolerance after refinement")
            generated_changes.append(generated_change)
            points = [[finite_decimal(inputs[index][f"p{i}_{j}"]) for j in range(4)] for i in range(1,5)]
            scale = points[0][0] + points[1][0]
            require(scale > 0 and all(abs(points[0][j]+points[1][j]-points[2][j]-points[3][j]) <= Decimal("1e-32")*scale
                                      for j in range(4)), "common CalcHEP input violates momentum conservation")
            require(all(abs(p[0]**2 - sum(x**2 for x in p[1:]) - m**2) <= Decimal("1e-32") * scale**2
                        for p, m in zip(points, masses)), "common CalcHEP input violates an external mass shell")
            shift = max(abs(p[j] - Decimal.from_float(float(raw[f"p{i}_{j}"])))
                        for i, p in enumerate(points, 1) for j in range(4)) / scale
            require(shift <= limit and math.isclose(shift, finite_decimal(row["ComponentChangeOverECM"]), rel_tol=1e-10, abs_tol=1e-32),
                    "excessive or misreported CalcHEP kinematic displacement")
            shifts.append(shift)
    require(sum(e > float(tolerance) for e in unprojected) == wide["Mismatches"]
            and math.isclose(max(unprojected), wide["MaximumRelativeError"], rel_tol=1e-8, abs_tol=2e-15)
            and math.isclose(max(observed), finite_decimal(chart["MaximumRelativeError"]), rel_tol=1e-8, abs_tol=1e-32)
            and math.isclose(max(generated_changes), finite_decimal(chart["MaximumSPINASRelativeChange"]), rel_tol=1e-8, abs_tol=1e-32)
            and math.isclose(max(shifts), finite_decimal(chart["MaximumComponentChangeOverECM"]), rel_tol=1e-10, abs_tol=1e-32),
            "CalcHEP common-input summary conflicts with individual rows")
    return dict(calchep_chart_points=len(original), calchep_chart_delta=chart["MaximumComponentChangeOverECM"],
                calchep_unprojected_quad_max_rel=wide["MaximumRelativeError"], calchep_chart_max_rel=chart["MaximumRelativeError"],
                calchep_spinas_refinement_max_rel=chart["MaximumSPINASRelativeChange"])


def calchep(directory, case):
    log = (directory / "calchep-comparison.log").read_text()
    def field(pattern):
        values = re.findall(pattern, log, re.MULTILINE)
        require(len(values) == 1, f"missing or duplicate CalcHEP field {pattern}")
        return values[0]
    require(field(r"^PROCESS: (.+)$") == case["incoming"] + "->" + case["outgoing"], "wrong CalcHEP process")
    seed = int(field(r"^Seed: (\d+)$"))
    counts = field(r"^Points requested/generated/compared: (\d+)/(\d+)/(\d+)$")
    n = complete_counts(tuple(map(int, counts)))
    mismatches, errors = map(int, field(r"^Mismatches/errors: (\d+)/(\d+)$"))
    status = field(r"^Amplitude validation: (\w+)$")
    require(not errors, "CalcHEP evaluation errors")
    if mismatches == 0:
        require(status == "PASS", "raw CalcHEP did not pass")
        return seed, n, 0, {}
    require(status == "FAIL", "raw CalcHEP status conflicts with its mismatch count")
    replay = json.loads((directory / "calchep-precision.json").read_text())
    native, wide = replay["Results"]["Native"], replay["Results"]["GCC128"]
    capture = directory / "calchep-outliers.csv"
    require(replay["Status"] == "PRECISION_REPLAY_PASS" and replay["NativeControlReproducesCapture"] is True
            and replay["Seed"] == seed and replay["ExternalCount"] == 4 and replay["PointsSHA256"] == digest(capture),
            "CalcHEP replay does not match the raw capture")
    samples = csv_records(capture)
    require(len(samples) == mismatches and all(1 <= p <= n for p in samples)
            and replay["Points"] == native["Points"] == native["OriginalBitMatches"] == native["Mismatches"] == wide["Points"] == mismatches
            and native["Status"] == "FAIL" and native["Errors"] == wide["Errors"] == 0
            and wide["MantissaBits"] == 113 and wide["REALBytes"] == 16, "incomplete CalcHEP binary128 replay")
    details = {}
    if "SameChartReplay" in replay:
        details = calchep_chart(directory, replay, samples, seed)
    else:
        require(wide["Status"] == "PASS" and wide["Mismatches"] == 0, "CalcHEP binary128 outliers still disagree")
    return seed, n, mismatches, details


def mg_rows(report, expected, passing):
    rows = indexed(report["Rows"], lambda r: (r["Kind"], tuple(r["Helicities"])))
    require(set(rows) == {("FixedHelicity", h) for h in expected} | {("PolarizationSum", ())},
            "missing or unexpected MadGraph fixed helicities or sum")
    counts = {}
    for key, row in rows.items():
        counts[key] = complete_counts([row[k] for k in ("RequestedPoints", "GeneratedPoints", "ComparedPoints", "Points")])
        require(row["EvaluationErrors"] == 0, f"MadGraph evaluation errors for {key}")
        if passing:
            require(row["Passed"] is True and row["Status"] == "PASS" and row["Mismatches"] == 0, f"MadGraph mismatch for {key}")
    return counts


def audit_chart(report, expected, counts, seed, build, precise):
    previous = report["UnprojectedBinary128Comparison"]
    change = report["MaximumChartComponentChangeOverBeamEnergy"]
    require(previous["Seed"] == seed and previous["Status"] == "FAIL" and previous["Passed"] is False
            and mg_rows(previous, expected, False) == counts and build["Masses"] == [0] * 4
            and precise["MasslessTwoBodyChartLibrary"] and math.isfinite(change)
            and 0 <= change <= 16 * sys.float_info.epsilon, "invalid MadGraph mass-shell chart replay")
    require(any(old["Mismatches"] > 0 for old in previous["Rows"]), "unprojected replay lacks a mismatch")
    for old, new in zip(previous["Rows"], report["Rows"]):
        require((old["Kind"], old["Helicities"], old["ExpectedExactZero"]) ==
                (new["Kind"], new["Helicities"], new["ExpectedExactZero"])
                and (new["ExpectedExactZero"] or old == new), "MadGraph chart changed a nonzero comparison")
    return change


def madgraph(directory, expected, zeros, case, pdgs, seed, n):
    report = json.loads((directory / "madgraph-comparison.json").read_text())
    require(report["Seed"] == seed and report["Status"] == "PASS" and report["Passed"] is True, "MadGraph run failed")
    build = json.loads(report["ReferenceBuildJSON"])
    expected_pdgs = [pdgs[tag] for tag in (case["incoming"] + "," + case["outgoing"]).split(",")]
    require(build["Status"] == "READY" and build["PhysicalPDGs"] == expected_pdgs and build["ExternalCount"] == 4,
            "wrong MadGraph process")
    require(build["Normalization"].startswith("Raw fixed-helicity, color-summed squared amplitude")
            and build["SpinSum"].startswith("Exhaustive generated helicity table"), "MadGraph polarization interface missing")
    counts = mg_rows(report, expected, True)
    for row in report["Rows"]:
        zero = expected == zeros if row["Kind"] == "PolarizationSum" else tuple(row["Helicities"]) in zeros
        require(row["ExpectedExactZero"] is zero and (not zero or 0 <= row["ZeroAbsoluteTolerance"] <= 1e-22),
                "MadGraph zero policy differs from the submitted expressions")
    require(set(counts.values()) == {n}, "CH/MG point counts differ")
    def residuals(source):
        return {tuple(r["Helicities"]): r["MaximumAbsoluteError"] for r in source.get("Rows", ())
                if r["Kind"] == "FixedHelicity" and r["ExpectedExactZero"]}
    unprojected = report.get("UnprojectedBinary128Comparison", {})
    errors = (residuals(report.get("NativePrecisionComparison", report)), residuals(unprojected), residuals(report))
    for stage in errors:
        require(all(math.isfinite(value) and value >= 0 for value in stage.values()), "invalid exact-zero residual")
    require(all(r["MaximumAbsoluteError"] <= r["ZeroAbsoluteTolerance"] for r in report["Rows"]
                if r["ExpectedExactZero"]), "MadGraph residual exceeds the exact-zero bound")
    if "NativePrecisionComparison" not in report and "PrecisionBuildJSON" not in report:
        require("UnprojectedBinary128Comparison" not in report and "MaximumChartComponentChangeOverBeamEnergy" not in report,
                "MadGraph chart replay lacks a precision build")
        return 53, None, errors
    raw = report["NativePrecisionComparison"]
    require(raw["Status"] == "FAIL" and raw["Passed"] is False and raw["Seed"] == seed and
            mg_rows(raw, expected, False) == counts, "MadGraph replay did not cover the original comparison")
    precise = json.loads(report["PrecisionBuildJSON"])
    require(precise["Status"] == "READY" and precise["MantissaBits"] == 113 and precise["Process"] == build["Process"],
            "invalid MadGraph precision build")
    if "UnprojectedBinary128Comparison" not in report and "MaximumChartComponentChangeOverBeamEnergy" not in report:
        return 113, None, errors
    return 113, audit_chart(report, expected, counts, seed, build, precise), errors


def find_runs(roots):
    seen = set()
    for root in roots:
        require(root.is_dir(), f"missing external search root {root}")
        for name, subdirs, files in os.walk(root):
            if "scores.tsv" in files:
                subdirs.clear()
                run = Path(name).resolve()
                if run not in seen:
                    seen.add(run)
                    yield run


def audit(args):
    catalog = table(HERE / "catalog.tsv")
    if not args.notebook_only:
        catalog += table(HERE / "flavor-closure.tsv")
    families = defaultdict(list)
    for case in catalog:
        if not args.family or case["family"] in args.family:
            families[case["family"]].append(case)
    require(not set(args.family) - families.keys() and families, "unknown or empty family selection")
    pdgs, helicities = model_particles()
    native = args.native_run.resolve()
    scores = indexed([r for r in table(native / "scores.tsv") if r["case"] != "external"], lambda r: (r["family"], r["case"]))
    cross = indexed(table(args.wolfram_scores), lambda r: r["case"]) if args.wolfram_scores else None
    external = defaultdict(list)
    for run in find_runs([native, *args.external_root]):
        for row in table(run / "scores.tsv"):
            if row["case"] == "external" and row["family"] in families and row["status"] == "PASS":
                external[row["family"]].append(run)
    output, missing = [], []
    for family, cases in families.items():
        ids = {c["case_id"] for c in cases}
        rows = {c["case_id"]: dict(case=c["case_id"], family=family, notebook_weight=c["notebook_weight"],
                                   notebook_leaves=c["notebook_leaves"], status="NATIVE_INCOMPLETE") for c in cases}
        try:
            folder = native / family
            require({p.stem for p in folder.glob("*.expr")} == ids, "native expression inventory incomplete")
            blocks = case_blocks(folder / "checks.txt", ids)
            for c in cases:
                i = c["case_id"]
                row = scores[(family, i)]
                raw, canonical = fingerprints(folder / (i + ".expr"))
                rows[i].update(native_case(row, c, blocks[i]), weight=row["weight"], leaves=row["leaves"],
                               expression_sha256=raw, commutative_sha256=canonical)
                if cross is not None:
                    rows[i].update(wolfram_score(cross[i], c, row, raw))
            for row in rows.values():
                row["status"] = "EXTERNAL_INCOMPLETE"
            physical = {c["case_id"]: physical_helicity(c, helicities) for c in cases}
            require(len(set(physical.values())) == len(ids), "overlapping catalog helicity assignments")
            zeros = {i for i in ids if (folder / (i + ".expr")).read_bytes() == b"0\n"}
            zero_helicities = {physical[i] for i in zeros}
            reasons = []
            for run in sorted(set(external[family]), key=lambda p: (p.name, str(p)), reverse=True):
                try:
                    directory = run / family
                    require({p.stem for p in directory.glob("*.expr")} == ids, "external expression inventory differs")
                    compared = {i: fingerprints(directory / (i + ".expr")) for i in ids}
                    different = [i for i in sorted(ids) if compared[i][1] != rows[i]["commutative_sha256"]]
                    require(not different, "different expression(s): " + ",".join(different))
                    seed, points, replayed, ch_chart = calchep(directory, cases[0])
                    bits, change, zero_errors = madgraph(directory, set(physical.values()), zero_helicities, cases[0], pdgs, seed, points)
                    for i, helicity in physical.items():
                        identical = compared[i][0] == rows[i]["expression_sha256"]
                        rows[i].update(status="PASS", external_sha256=compared[i][0],
                                       external_match="identical" if identical else "commutative-order", external_seed=seed, calchep_points=points, calchep_quad_points=replayed,
                                       mg_helicity=",".join(map(str, helicity)), mg_fixed_points=points,
                                       mg_sum_points=points, mg_precision_bits=bits,
                                       mg_exact_zero_chart_delta=change if i in zeros else None,
                                       mg_zero_raw_max_abs=zero_errors[0].get(helicity),
                                       mg_zero_unprojected_quad_max_abs=zero_errors[1].get(helicity),
                                       mg_zero_final_max_abs=zero_errors[2].get(helicity), external_run=run.name, **ch_chart)
                    break
                except INVALID as error:
                    reasons.append(f"{run.name}: {error}")
            else:
                raise ValueError("; ".join(reasons[:3]) or "no passing external family receipt")
        except INVALID as error:
            missing.append(family)
            print(f"{family}: {error}", file=sys.stderr)
        output.extend(rows.values())
    with args.receipt.open("w") as stream:
        writer = csv.DictWriter(stream, FIELDS, delimiter="\t", lineterminator="\n")
        writer.writeheader()
        writer.writerows(output)
    if args.missing_families:
        args.missing_families.write_text("".join(f + "\n" for f in missing))
    passed = sum(r["status"] == "PASS" for r in output)
    print(f"{passed}/{len(output)} cases, {len(families)-len(missing)}/{len(families)} families. {args.receipt}")
    return int(bool(missing))


if __name__ == "__main__":
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument("--native-run", required=True, type=Path, help="final native run containing scores.tsv")
    p.add_argument("--external-root", action="append", default=[], type=Path, help="search recursively for backend runs")
    p.add_argument("--receipt", "--output", required=True, type=Path, help="write one TSV row per construction")
    p.add_argument("--wolfram-scores", type=Path, help="require and record independent cross-score.wls measurements")
    p.add_argument("--missing-families", type=Path, help="write family names needing new evidence")
    p.add_argument("--notebook-only", action="store_true")
    p.add_argument("--family", action="append", default=[], help="audit one family (repeatable)")
    try:
        sys.exit(audit(p.parse_args()))
    except INVALID as error:
        p.exit(2, f"audit: {error}\n")
