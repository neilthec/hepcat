#!/usr/bin/env python3
"""Verify timed sweeps and redraw the one-pair timing figure without rerunning physics.

The default inputs are the published compact timing records beside this tool.
Fresh paired benchmark runs can be selected with --input-directory. Their
fourway.csv must have physical/ and spurious/ raw sweep CSVs beside it.
--analysis-only uses the Python standard library; drawing requires Matplotlib.
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

import argparse
import csv
import hashlib
import json
import math
from pathlib import Path
import statistics


TARGET_POINTS = 10000
STYLES = (
    ('HEPCAT_SPINAS_AllPhysical', 'HEPCAT / SPINAS, AllPhysical', '#0072B2', 'o', '-', 'physical', 'SPINAS'),
    ('HEPCAT_SPINAS_AllSpurious', 'HEPCAT / SPINAS, AllSpurious', '#D55E00', 's', '--', 'spurious', 'SPINAS'),
    ('CalcHEP', 'CalcHEP', '#009E73', '^', '-.', 'physical', 'CalcHEP'),
    ('MadGraph', 'MadGraph', '#8E5EA2', 'D', ':', 'physical', 'MadGraph'),
)


def read_csv(path):
    with path.open(newline='', encoding='utf-8') as stream:
        return list(csv.DictReader(stream))


def quantile(values, fraction):
    ordered = sorted(values)
    position = fraction * (len(ordered) - 1)
    low, high = math.floor(position), math.ceil(position)
    return ordered[low] + (position - low) * (ordered[high] - ordered[low])


def read_timings(directory):
    summary_path = directory / 'fourway.csv'
    rows = sorted(read_csv(summary_path), key=lambda row: int(row['photons']))
    if [int(row['photons']) for row in rows] != [2, 3, 4]:
        raise ValueError('Expected exactly one completed row for N=2,3,4')
    point_counts = {int(row['points']) for row in rows}
    if len(point_counts) != 1 or min(point_counts) <= 0:
        raise ValueError('All timing rows need the same positive point count')
    points = point_counts.pop()
    scale = TARGET_POINTS / points
    files = [summary_path]
    repetitions, series = set(), {}
    for key, label, color, marker, line, folder, backend in STYLES:
        path = directory / folder / 'timings-repetitions.csv'
        files.append(path)
        raw = read_csv(path)
        if any(int(item['photons']) not in (2, 3, 4) or
               item['backend'] not in ('SPINAS', 'CalcHEP', 'MadGraph') for item in raw):
            raise ValueError(f'Unexpected timing sweep in {path}')
        # Check the whole inventory, including the second run's retained
        # reference timings, even though the figure uses the physical run.
        for photons in (2, 3, 4):
            for name in ('SPINAS', 'CalcHEP', 'MadGraph'):
                group = [item for item in raw if int(item['photons']) == photons and item['backend'] == name]
                if not group or sorted(int(item['repetition']) for item in group) != list(range(1, len(group) + 1)) \
                        or any(int(item['points']) != points for item in group):
                    raise ValueError(f'Incomplete or duplicate timing sweeps in {path}')
                if any(not math.isfinite(float(item['seconds'])) or float(item['seconds']) <= 0 for item in group):
                    raise ValueError(f'Invalid positive timing in {path}')
                repetitions.add(len(group))
        medians, low, high = [], [], []
        for row in rows:
            photons = int(row['photons'])
            values = [float(item['seconds']) for item in raw
                      if int(item['photons']) == photons and item['backend'] == backend]
            median = float(row[key + '_seconds'])
            if not math.isfinite(median) or median <= 0 or not math.isclose(
                    median, statistics.median(values), rel_tol=5e-10, abs_tol=1e-14):
                raise ValueError(f'Summary disagrees with raw sweeps for {key}, N={photons}')
            relative = row.get(key + '_relative_N2')
            if relative is not None and not math.isclose(float(relative), median / float(rows[0][key + '_seconds']),
                                                          rel_tol=5e-10, abs_tol=1e-14):
                raise ValueError(f'Incorrect baseline normalization for {key}, N={photons}')
            medians.append(median * scale)
            low.append(quantile(values, .25) * scale)
            high.append(quantile(values, .75) * scale)
        series[key] = {'medians': medians, 'q25': low, 'q75': high}
    if len(repetitions) != 1:
        raise ValueError('All backends and multiplicities need the same repetition count')
    return rows, series, files, points, repetitions.pop()


def draw(series, out, repetitions, points=TARGET_POINTS):
    import matplotlib
    matplotlib.use('Agg')
    import matplotlib.pyplot as plt

    plt.rcParams.update({'font.family': 'DejaVu Sans', 'font.size': 9,
                         'axes.spines.top': False, 'axes.spines.right': False, 'pdf.fonttype': 42})
    fig, axes = plt.subplots(1, 2, figsize=(8.8, 3.7))
    fig.subplots_adjust(left=.085, right=.98, bottom=.23, top=.77, wspace=.34)
    for key, label, color, marker, line, *_ in STYLES:
        values = series[key]
        y, q25, q75 = values['medians'], values['q25'], values['q75']
        for ax, divisor in zip(axes, (1, y[0])):
            ax.errorbar([2, 3, 4], [value / divisor for value in y],
                        yerr=[[max(0, value - lower) / divisor for value, lower in zip(y, q25)],
                              [max(0, upper - value) / divisor for value, upper in zip(y, q75)]],
                        label=label, color=color, marker=marker, linestyle=line, linewidth=1.6, capsize=3)
    for i, ax in enumerate(axes):
        ax.set_yscale('log')
        ax.set_xticks([2, 3, 4])
        ax.set_xlabel('Number of outgoing photons, $N$')
        ax.grid(axis='y', color='#D9DDE1', linewidth=.6)
        ax.set_xlim(1.88, 4.12)
        ax.set_ylabel('Seconds per 10,000 points' if i == 0 else 'Time / own $N=2$ median')
        ax.set_title('(a) Absolute evaluation time' if i == 0 else "(b) Growth from each method's baseline",
                     loc='left', fontsize=10)
    fig.suptitle(r'$e^-e^+\to N\gamma$: all spins and photon helicities summed', y=.99, fontsize=11)
    fig.legend(*axes[0].get_legend_handles_labels(), loc='upper center', bbox_to_anchor=(.5, .91),
               ncol=4, frameon=False, fontsize=8.3, columnspacing=1.1)
    repeat_label = 'nine' if repetitions == 9 else str(repetitions)
    fig.text(.5, .065, f'Medians of {repeat_label} repetitions; bars show the central 50% of timings.',
             ha='center', fontsize=8)
    if points != TARGET_POINTS:
        fig.text(.5, .12, f'{points} points actually timed; displayed times scaled to 10,000 points.',
                 ha='center', fontsize=8)
    fig.text(.5, .022, "Normalized bars use each method's fixed $N=2$ median; baseline uncertainty is not propagated.",
             ha='center', fontsize=8)
    fig.savefig(out / 'photon-timings.pdf')
    fig.savefig(out / 'photon-timings.png', dpi=170)
    plt.close(fig)


def main():
    benchmark = Path(__file__).resolve().parent
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--input-directory', type=Path, default=benchmark / 'data/timings/one-pair')
    parser.add_argument('--output-directory', type=Path,
                        default=benchmark.parent / 'results/reproduction/figures')
    parser.add_argument('--analysis-only', action='store_true')
    args = parser.parse_args()
    rows, series, files, points, repetitions = read_timings(args.input_directory)
    args.output_directory.mkdir(parents=True, exist_ok=True)
    report = {'TimingDataStatus': 'VERIFIED', 'RawSweepsVerified': True,
              'Scope': 'Replay of recorded timings; no amplitude validation or new timing run.',
              'PointsActuallyTimed': points, 'Repetitions': repetitions,
              'PlottedTimeUnit': 'seconds per 10,000 points', 'Rows': rows, 'Series': series,
              'sources': [{'path': str(path), 'sha256': hashlib.sha256(path.read_bytes()).hexdigest()}
                          for path in dict.fromkeys(files)]}
    if not args.analysis_only:
        draw(series, args.output_directory, repetitions, points)
    destination = args.output_directory / 'photon-timings-provenance.json'
    destination.write_text(json.dumps(report, indent=2) + '\n')
    print(f'Verified one-pair timing medians and raw sweeps: {points} points, {repetitions} repetitions.')
    print(destination)


if __name__ == '__main__':
    main()
