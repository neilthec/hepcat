# Four fermions with zero, one or two photons

This benchmark compares the complete physical process
`e- e+ -> e- e+ + N photons`, for `N = 0, 1, 2`, in four implementations:

1. HEPCAT/SPINAS using the saved AllPhysical expressions.
2. HEPCAT/SPINAS using the saved AllSpurious expressions.
3. CalcHEP's independently generated matrix element.
4. MadGraph's independently generated matrix element.

All four receive the same 10,000 saved physical phase-space points at each
multiplicity. Every massive spin and photon helicity is included. The
AllSpurious label describes the saved representation preference; a sector
can coincide with the physical form after ordinary cancellations.

## Prepare the exports

Run from the HEPCAT repository root, with a working Wolfram kernel:

```sh
study="$PWD/tests/benchmarks/build-four-fermion-double"
/Applications/Wolfram.app/Contents/MacOS/WolframKernel \
  -script tests/benchmarks/export-four-fermions.wl --output "$study/exports"
```

The exporter reads complete amplitudes from models 3 and 4. It does not run
construction or completion and does not alter the model files. It records
their hashes and the exact supplied helicity sectors. It writes six distinct
double-precision optimized process classes, named `ffffPhysical0` through
`ffffPhysical2` and `ffffSpurious0` through `ffffSpurious2`.

The existing `tests/results/spinas-output/eeee/manifest.json` supplies the explicitly
selected independent CalcHEP and MadGraph models. Use `--configuration FILE`
to select another configured export. Both references must be available and
explicitly restrict internal particle content to electrons and photons,
absolute PDGs `{11,22}`. `--source DIR`, `--models DIR` and `--spinas DIR`
support frozen inputs or another installation. The installed MadGraph Python
interpreter must be runnable from the Wolfram kernel too.

These vectorlike tree-level QED models have real couplings. The exporter
explicitly declares `HelicitySymmetry -> Conjugation` for the complete
massive-spin sum. Every individual complex helicity amplitude remains
available. The numerical benchmark checks the shortened sum against all
`16 * 2^N` public complex-amplitude components at every point, outside timing.

## Build and run

Use a **double-precision** SPINAS library. An arm64 `long double` library is
not ABI-compatible merely because it has the same mantissa width. CMake
checks a linked SPINAS call before building the benchmark.

```sh
cmake -S tests/benchmarks/four-fermion -B "$study/build" \
  -DCMAKE_BUILD_TYPE=Release \
  -DSPINAS_ROOT=/path/to/spinas \
  -DSPINAS_LIBRARY=/path/to/double/libspinas.a \
  -DCALCHEP_ROOT=/path/to/CalcHEP \
  "-DHEPCAT_PHYSICAL_PROCESS_DIRECTORIES=$study/exports/physical/ffffPhysical0;$study/exports/physical/ffffPhysical1;$study/exports/physical/ffffPhysical2" \
  "-DHEPCAT_SPURIOUS_PROCESS_DIRECTORIES=$study/exports/spurious/ffffSpurious0;$study/exports/spurious/ffffSpurious1;$study/exports/spurious/ffffSpurious2"
cmake --build "$study/build" --parallel
"$study/build/qed_four_fermion_benchmark" \
  --points 10000 --repeats 9 --warmup 100 --energy 100 \
  --output "$study/timings"
```

Replace the installation paths. Default masses and couplings are taken from
the supplied exports. The seed defaults to the time and is saved in the
metadata; `--seed INTEGER` replays the same point streams. `--energy` is the
fixed center-of-mass energy in GeV. `--tolerance` defaults to `1e-7`.

The phase-space generator treats both outgoing fermions as massive. Its
sequential isotropic points are valid test kinematics, not a weighted
cross-section integration. Points are quantized to the common double inputs
before being sent to any backend. There are no selection cuts or discarded
validation points.

Every point must agree across all four methods before a timing row is
accepted. Both optimized SPINAS sums must also agree with exhaustive public
amplitude enumeration. A failure leaves the saved comparisons for diagnosis
and stops without accepting that multiplicity's timing row.

## What is timed

Each backend runs serially over the complete point set, with the backend
order rotated between sweeps. Timing includes momentum setup and the complete
squared-amplitude evaluation. Nine sweeps give medians, minima, maxima and
interquartile ranges. Generation, compilation, initialization, phase-space
generation, validation, warmup and file output are excluded. All evaluated
values are consumed and checked after the timer stops.

The common weight includes incoming averaging `1/4` and the final identical
photon factor `1/N!`. The outgoing electron and positron are distinguishable,
so there is no extra identical-fermion factor. CalcHEP already applies these
factors; the raw MadGraph spin sum is multiplied by `1/(4 N!)` exactly once.

SPINAS uses its optimized, explicitly symmetry-reduced QED sum. MadGraph uses
the native exhaustive spin/helicity sum without a cached good-helicity mask;
CalcHEP uses its own symbolic spin sum. The comparison therefore measures
these specified implementations, not a universal optimum for each package.
All reference builds use private zero-width models and optimization level 3.
No installed model, production width or native event session is changed.

## Saved results

- `timings.csv`: absolute median seconds per full point set, each backend's
  growth relative to its own `N=0` time, spread and maximum validation errors.
- `timings-repetitions.csv`: every measured sweep.
- `timings-points-N*.csv`: every common physical four-momentum.
- `timings-values-N*.csv`: both SPINAS representations, both exhaustive
  controls, CalcHEP, MadGraph and all relative differences, for every point.
- `timings-madgraph-N*.json`: independent reference provenance.
- `timings-metadata.json`: the resolved seed, normalization, precision,
  parameter values, reference policy, machine information and build hashes.

Keep the full output directory for reproduction, including the export
sidecars and build information. Repeat a frozen benchmark on a quiet machine
before using millisecond timings as publication-level performance claims.
The source and the compact result summary are versioned; generated process
trees, libraries and full point inventories remain outside Git.

## Recorded comparison, 6 November 2026

Double-precision optimized exports on the native Apple M3 development Mac,
Apple Clang 21, Release `-O3`, `Me = 0.0005 GeV`, `EE = 0.31333`, and
`sqrt(s) = 100 GeV`. Each entry is the median of nine serial sweeps over
10,000 common points. The resolved time seed was `1794010385397612000`.

| Photons | AllPhysical (s) | AllSpurious (s) | CalcHEP (s) | MadGraph (s) |
| ---: | ---: | ---: | ---: | ---: |
| 0 | 0.007618 | 0.007630 | 0.001690 | 0.011208 |
| 1 | 0.022855 | 0.015436 | 0.006519 | 0.077667 |
| 2 | 0.131677 | 0.133619 | 0.193038 | 0.576800 |

Relative to each backend's own zero-photon baseline:

| Photons | AllPhysical | AllSpurious | CalcHEP | MadGraph |
| ---: | ---: | ---: | ---: | ---: |
| 0 | 1.00 | 1.00 | 1.00 | 1.00 |
| 1 | 3.00 | 2.02 | 3.86 | 6.93 |
| 2 | 17.28 | 17.51 | 114.25 | 51.46 |

All 30,000 comparisons passed at `1e-7`. The largest relative differences
against AllPhysical were `5.04e-13` for AllSpurious, `3.52e-11` for CalcHEP
and `1.80e-13` for MadGraph. Both optimized SPINAS sums also passed exhaustive
enumeration of all public complex amplitudes at every point. The compact
CSV and JSON summary are stored beside these instructions; full point files,
values, raw sweeps, input snapshots and build provenance were retained in
the study's output directory.

CalcHEP is fastest at zero and one photon. At two photons AllPhysical is
about 1.47 times faster than CalcHEP and 4.38 times faster than this MadGraph
reference. Physical and spurious forms are close there. At one photon the
exports have identical complexity scores of 160, but use 36 and 20 native
products, respectively, in the symmetry-reduced branch. That difference in
cached work helps explain their different evaluation times.

These are local desktop measurements, not a universal performance ranking.
The recorded interquartile ranges are small, but a frozen publication run
should still be repeated on a quiet machine. Keep absolute times beside
normalized growth because their zero-photon baselines differ substantially.
