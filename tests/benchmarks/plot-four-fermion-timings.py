#!/usr/bin/env python3
"""Plot archived four-fermion timings without rerunning any timed work.

Example:
  python3 tests/benchmarks/plot-four-fermion-timings.py

Matplotlib is imported only when drawing. --analysis-only needs Python's
standard library. Error bars are repetition quartiles divided, on the right,
by that backend's fixed N=0 median. They are not confidence intervals.
"""

# HEPCAT - High Energy Physics Constructive Amplitude Tool
#
# Copyright (C) 2026 Neil Christensen
#
# This file is part of HEPCAT.
#
# HEPCAT is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# HEPCAT is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with HEPCAT.  If not, see <https://www.gnu.org/licenses/>.

from __future__ import annotations

import argparse
import csv
import hashlib
import json
import math
from pathlib import Path
import statistics


BACKENDS = (
    ("physical", "HEPCAT / SPINAS, AllPhysical", "#0072B2", "o", "-"),
    ("spurious", "HEPCAT / SPINAS, AllSpurious", "#D55E00", "s", "--"),
    ("calchep", "CalcHEP", "#009E73", "^", "-."),
    ("madgraph", "MadGraph", "#8E5EA2", "D", ":"),
)
TARGET_POINTS = 10_000


def quantile(values, fraction):
    ordered = sorted(values)
    position = fraction * (len(ordered) - 1)
    lower, upper = math.floor(position), math.ceil(position)
    return ordered[lower] + (position - lower) * (ordered[upper] - ordered[lower])


def verify_repetitions(rows: list[dict], path: Path) -> None:
    """Recompute every plotted median and quartile from the actual timed sweeps."""
    with path.open(newline="", encoding="utf-8") as handle:
        raw = list(csv.DictReader(handle))
    expected = sum(row["repetitions"] * len(BACKENDS) for row in rows)
    if len(raw) != expected:
        raise ValueError(f"Unexpected number of timing sweeps in {path}")
    for row in rows:
        for backend, *_ in BACKENDS:
            group = [item for item in raw if int(item["photons"]) == row["photons"]
                     and item["backend"] == backend]
            if len(group) != row["repetitions"] or sorted(int(item["repetition"]) for item in group) != \
                    list(range(1, row["repetitions"] + 1)) or any(
                        int(item["points"]) != row["points"] for item in group):
                raise ValueError(f"Incomplete sweeps for {backend}, N={row['photons']}")
            values = [float(item["seconds"]) * TARGET_POINTS / row["points"] for item in group]
            if not all(math.isfinite(value) and value > 0 for value in values):
                raise ValueError(f"Invalid sweep time in {path}")
            expected_values = (statistics.median(values), min(values), max(values),
                               quantile(values, .25), quantile(values, .75))
            for suffix, expected_value in zip(
                    ("seconds", "min_seconds", "max_seconds", "q25_seconds", "q75_seconds"),
                    expected_values):
                if not math.isclose(row[f"{backend}_{suffix}"], expected_value,
                                    rel_tol=5e-10, abs_tol=1e-14):
                    raise ValueError(f"Summary disagrees with raw sweeps for {backend}, N={row['photons']}")


def read_timings(path: Path) -> list[dict]:
    """Reject partial or inconsistent CSVs rather than plotting partial runs."""
    with path.open(newline="", encoding="utf-8") as handle:
        raw = list(csv.DictReader(handle))
    rows = []
    for source in raw:
        row = {key: int(source[key]) for key in ("photons", "points", "repetitions")}
        if row["points"] <= 0 or row["repetitions"] <= 0:
            raise ValueError(f"Invalid point/repetition count in {path}")
        scale = TARGET_POINTS / row["points"]
        for backend, *_ in BACKENDS:
            for suffix in ("seconds", "min_seconds", "max_seconds", "q25_seconds", "q75_seconds"):
                key = f"{backend}_{suffix}"
                value = float(source[key]) * scale
                if not math.isfinite(value) or value <= 0:
                    raise ValueError(f"Invalid positive time {key} in {path}")
                row[key] = value
            times = [row[f"{backend}_{suffix}"] for suffix in
                     ("min_seconds", "q25_seconds", "seconds", "q75_seconds", "max_seconds")]
            if times != sorted(times):
                raise ValueError(f"Unordered quantiles for {backend}, N={row['photons']}")
        rows.append(row)
    rows.sort(key=lambda row: row["photons"])
    if [row["photons"] for row in rows] != [0, 1, 2]:
        raise ValueError(f"Expected completed N=0,1,2 rows in {path}")
    return rows


def analyze(rows: list[dict], repeat_rows: list[dict] | None) -> dict:
    result = {
        "Process": "e- e+ -> e- e+ + N photons",
        "Photons": [0, 1, 2],
        "TimeUnit": "seconds per 10000 common phase-space points",
        "ErrorBars": "25th to 75th percentile of repeated timings, not confidence intervals",
        "NormalizedErrorBars": "Each time/quantile divided by that backend's fixed N=0 median; no baseline uncertainty propagated",
        "SpeedupDefinition": "reference time / HEPCAT time; greater than one means HEPCAT is faster",
        "Rows": [],
    }
    for row in rows:
        record = {
            "Photons": row["photons"],
            "PointsActuallyTimed": row["points"],
            "Repetitions": row["repetitions"],
            "BackendTimes": {},
            "Speedups": {},
        }
        for backend, *_ in BACKENDS:
            median = row[f"{backend}_seconds"]
            q25, q75 = [row[f"{backend}_{quantile}_seconds"] for quantile in ("q25", "q75")]
            record["BackendTimes"][backend] = {
                "MedianSeconds": median,
                "Q25Seconds": q25,
                "Q75Seconds": q75,
                "RelativeIQR": (q75 - q25) / median,
                "GrowthFromOwnN0": median / rows[0][f"{backend}_seconds"],
            }
        for hepcat in ("physical", "spurious"):
            for reference in ("calchep", "madgraph"):
                record["Speedups"][f"{hepcat}_vs_{reference}"] = (
                    row[f"{reference}_seconds"] / row[f"{hepcat}_seconds"])
        record["SpuriousTimeOverPhysicalTime"] = row["spurious_seconds"] / row["physical_seconds"]
        if repeat_rows is not None:
            repeat = next(item for item in repeat_rows if item["photons"] == row["photons"])
            comparisons = {}
            for backend, *_ in BACKENDS:
                first = row[f"{backend}_seconds"]
                second = repeat[f"{backend}_seconds"]
                first_q25 = row[f"{backend}_q25_seconds"]
                first_q75 = row[f"{backend}_q75_seconds"]
                second_q25 = repeat[f"{backend}_q25_seconds"]
                second_q75 = repeat[f"{backend}_q75_seconds"]
                comparisons[backend] = {
                    "RepeatMedianSeconds": second,
                    "RelativeMedianChange": second / first - 1,
                    "Central50PercentIntervalsOverlap": (
                        max(first_q25, second_q25) <= min(first_q75, second_q75)),
                    "RepeatGrowthFromOwnN0": second / repeat_rows[0][f"{backend}_seconds"],
                }
            record["RepeatedRunComparison"] = comparisons
        result["Rows"].append(record)
    return result


def write_tables(rows: list[dict], analysis: dict, directory: Path) -> None:
    with (directory / "speed-ratios.csv").open("w", newline="", encoding="utf-8") as handle:
        writer = csv.writer(handle)
        writer.writerow(("photons", "physical_speedup_vs_calchep", "physical_speedup_vs_madgraph",
                         "spurious_speedup_vs_calchep", "spurious_speedup_vs_madgraph",
                         "spurious_time_over_physical_time"))
        for row in analysis["Rows"]:
            writer.writerow((row["Photons"], *row["Speedups"].values(), row["SpuriousTimeOverPhysicalTime"]))
    with (directory / "timing-spread.csv").open("w", newline="", encoding="utf-8") as handle:
        writer = csv.writer(handle)
        writer.writerow(("photons", "backend", "median_seconds_per_10000", "q25_seconds_per_10000",
                         "q75_seconds_per_10000", "iqr_over_median", "growth_from_own_N0"))
        for row in analysis["Rows"]:
            for backend, data in row["BackendTimes"].items():
                writer.writerow((row["Photons"], backend, *data.values()))
    if "RepeatedRunComparison" in analysis["Rows"][0]:
        with (directory / "repeated-run-comparison.csv").open("w", newline="", encoding="utf-8") as handle:
            writer = csv.writer(handle)
            writer.writerow(("photons", "backend", "repeat_median_seconds_per_10000", "relative_median_change",
                             "central_50_percent_intervals_overlap", "repeat_growth_from_own_N0"))
            for row in analysis["Rows"]:
                for backend, data in row["RepeatedRunComparison"].items():
                    writer.writerow((row["Photons"], backend, *data.values()))


def choose_scale(rows: list[dict], normalized: bool, requested: str) -> str:
    if requested != "auto":
        return requested
    values = []
    for backend, *_ in BACKENDS:
        divisor = rows[0][f"{backend}_seconds"] if normalized else 1
        values.extend(row[f"{backend}_seconds"] / divisor for row in rows)
    return "log" if max(values) / min(values) >= 50 else "linear"


def draw(rows: list[dict], directory: Path, stem: str, requested_scale: str) -> dict:
    import matplotlib
    matplotlib.use("Agg")
    import matplotlib.pyplot as plt
    from matplotlib.ticker import MaxNLocator

    plt.rcParams.update({
        "font.family": "DejaVu Sans", "font.size": 9.5, "axes.labelsize": 9.5,
        "axes.titlesize": 10, "legend.fontsize": 8.5, "pdf.fonttype": 42,
        "ps.fonttype": 42, "axes.spines.top": False, "axes.spines.right": False,
        "axes.axisbelow": True, "savefig.facecolor": "white",
    })
    fig, axes = plt.subplots(1, 2, figsize=(8.8, 3.7))
    fig.subplots_adjust(left=.085, right=.985, bottom=.22, top=.77, wspace=.34)
    scales = []
    photons = [row["photons"] for row in rows]
    for ax, normalized in zip(axes, (False, True)):
        for backend, label, color, marker, linestyle in BACKENDS:
            divisor = rows[0][f"{backend}_seconds"] if normalized else 1
            medians = [row[f"{backend}_seconds"] / divisor for row in rows]
            low = [median - row[f"{backend}_q25_seconds"] / divisor
                   for row, median in zip(rows, medians)]
            high = [row[f"{backend}_q75_seconds"] / divisor - median
                    for row, median in zip(rows, medians)]
            ax.errorbar(photons, medians, yerr=[low, high], label=label,
                        color=color, marker=marker, linestyle=linestyle,
                        linewidth=1.65, markersize=5, capsize=3,
                        elinewidth=1.1, capthick=1, zorder=3)
        scale = choose_scale(rows, normalized, requested_scale)
        scales.append(scale)
        ax.set_yscale(scale)
        if scale == "linear":
            ax.set_ylim(bottom=0)
            ax.yaxis.set_major_locator(MaxNLocator(nbins=5, min_n_ticks=4))
        ax.set_xticks(photons)
        ax.set_xlim(-.12, 2.12)
        ax.set_xlabel(r"Number of outgoing photons, $N$")
        ax.grid(axis="y", color="#D9DDE1", linewidth=.65)
        ax.set_title("(b) Growth from each method's baseline" if normalized
                     else "(a) Absolute evaluation time", loc="left", pad=8)
        ax.set_ylabel(r"Time / own $N=0$ median" if normalized
                      else "Seconds per 10,000 points")
    handles, labels = axes[0].get_legend_handles_labels()
    fig.legend(handles, labels, loc="upper center", bbox_to_anchor=(.5, .91),
               ncol=4, frameon=False, columnspacing=1.2, handlelength=2.2)
    fig.suptitle(r"$e^-e^+\to e^-e^+ + N\gamma$: all spins and photon helicities summed",
                 y=.992, fontsize=11)
    repetitions = sorted({row["repetitions"] for row in rows})
    repeated = str(repetitions[0]) if len(repetitions) == 1 else ", ".join(map(str, repetitions))
    fig.text(.5, .065, f"Medians of {repeated} repetitions; bars show the central 50% of timings.",
             ha="center", fontsize=8.2, color="#41474D")
    point_counts = sorted({row["points"] for row in rows})
    if point_counts != [TARGET_POINTS]:
        counts = ', '.join(map(str, point_counts))
        fig.text(.5, .12, f"Actual points per timing row: {counts}; times scaled to 10,000 points.",
                 ha="center", fontsize=8, color="#41474D")
    fig.text(.5, .022, r"Normalized bars use each method's fixed $N=0$ median; baseline uncertainty is not propagated.",
             ha="center", fontsize=8, color="#41474D")
    png = directory / f"{stem}.png"
    pdf = directory / f"{stem}.pdf"
    fig.savefig(png, dpi=300)
    fig.savefig(pdf, metadata={"Title": "Four-fermion QED amplitude evaluation times",
                              "Subject": "Repeated runtime medians and interquartile ranges"})
    plt.close(fig)
    return {"FigurePNG": str(png), "FigurePDF": str(pdf), "YScales": scales}


def main() -> None:
    benchmark = Path(__file__).resolve().parent
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--input", type=Path, default=benchmark / "data/timings/two-pair/timings.csv")
    parser.add_argument("--repetitions", type=Path,
                        help="Raw sweep CSV; defaults to INPUT stem plus -repetitions.csv")
    parser.add_argument("--repeat-run", type=Path)
    parser.add_argument("--output-directory", type=Path)
    parser.add_argument("--stem", default="four-fermion-timings")
    parser.add_argument("--y-scale", choices=("auto", "linear", "log"), default="auto")
    parser.add_argument("--analysis-only", action="store_true")
    args = parser.parse_args()
    rows = read_timings(args.input)
    repetitions = args.repetitions or args.input.with_name(args.input.stem + "-repetitions.csv")
    verify_repetitions(rows, repetitions)
    repeat_rows = read_timings(args.repeat_run) if args.repeat_run else None
    if args.repeat_run:
        verify_repetitions(repeat_rows, args.repeat_run.with_name(args.repeat_run.stem + "-repetitions.csv"))
    directory = args.output_directory or benchmark.parents[0] / "results/reproduction/figures"
    directory.mkdir(parents=True, exist_ok=True)
    analysis = analyze(rows, repeat_rows)
    analysis["InputCSV"] = str(args.input.resolve())
    analysis["InputSHA256"] = hashlib.sha256(args.input.read_bytes()).hexdigest()
    analysis["RepetitionCSV"] = str(repetitions.resolve())
    analysis["RepetitionSHA256"] = hashlib.sha256(repetitions.read_bytes()).hexdigest()
    analysis["RawSweepsVerified"] = True
    if args.repeat_run:
        analysis["RepeatedRunCSV"] = str(args.repeat_run.resolve())
        analysis["RepeatedRunSHA256"] = hashlib.sha256(args.repeat_run.read_bytes()).hexdigest()
    write_tables(rows, analysis, directory)
    if not args.analysis_only:
        analysis.update(draw(rows, directory, args.stem, args.y_scale))
    output = directory / "timing-analysis.json"
    output.write_text(json.dumps(analysis, indent=2) + "\n", encoding="utf-8")
    for row in analysis["Rows"]:
        ratios = row["Speedups"]
        print(f"N={row['Photons']}: Physical speedup vs CH={ratios['physical_vs_calchep']:.4g}, "
              f"vs MG={ratios['physical_vs_madgraph']:.4g}; Spurious vs CH="
              f"{ratios['spurious_vs_calchep']:.4g}, vs MG={ratios['spurious_vs_madgraph']:.4g}")
    print(output)


if __name__ == "__main__":
    main()
