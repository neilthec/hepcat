# Reproduce the paper results

This directory contains the timing programs, saved measurements, plotting
scripts and verification tools used for the QED and HEPCAT papers. Run the
commands below from the HEPCAT checkout root. Generated work goes under
`tests/results/`, outside the maintained inputs.

Choose the task you want:

| Task | Start here | External programs needed |
| --- | --- | --- |
| Recompute published tables from saved records | [Evidence replay](reproduction/README.md) | Python 3.9 or later; no Wolfram, SPINAS, CalcHEP or MadGraph |
| Redraw the two timing figures | The saved-data commands below | Python and Matplotlib |
| Rerun the timing measurements | The notebook and terminal workflow below | Wolfram, SPINAS, CMake, C/C++ and Fortran compilers, CalcHEP and MadGraph |
| Independently construct and validate the amplitudes | `tests/QED-6-point-construction.nb` and `tests/SM-4-point-construction.nb` | The installations selected in each notebook's Setup |
| Test package components | [Regression tests](../checks/README.md) | Dependencies described for the selected tests |

A replay of saved measurements checks the transcription and statistics. A new
numerical run tests the amplitudes at freshly generated points. A notebook
construction tests the symbolic construction procedure. Each has its own
purpose; none is labeled as another kind of test.

## Check and plot the published timing data

The maintained inputs are in `data/timings/one-pair/` and
`data/timings/two-pair/`. Both studies used 10,000 common phase-space points
per multiplicity and nine serial timing sweeps. The plotting tools verify the
medians against the individual sweeps before drawing the figures.

For a standard-library-only check, without installing plotting packages:

```sh
python3 tests/benchmarks/plot-photon-timings.py --analysis-only
python3 tests/benchmarks/plot-four-fermion-timings.py --analysis-only
```

To create the figures as PDF and PNG files:

```sh
python3 -m venv .venv
.venv/bin/python -m pip install -r tests/benchmarks/requirements.txt
.venv/bin/python tests/benchmarks/plot-photon-timings.py
.venv/bin/python tests/benchmarks/plot-four-fermion-timings.py
```

The figures and verification reports are written to
`tests/results/reproduction/figures/`, including `photon-timings.pdf` and
`four-fermion-timings.pdf`. The PNG versions are convenient for inspection.
The recorded inputs remain unchanged.

Use `.venv/Scripts/python.exe` on Windows. The numerical reference tools need
an operating system supported by their installations. The plotting tools
accept explicit input and output paths, so they also work on another machine
without the numerical programs.

## Prepare the notebook configuration once

Open `tests/QED-6-point-construction.nb` and evaluate Setup, adjusting the
HEPCAT, SPINAS, CalcHEP and MadGraph locations for your machine. Select the
supplied massive-electron QED CalcHEP model and MadGraph model indicated there.
Both independent calculations restrict particle content to electrons and
photons, absolute PDGs `{11,22}`.

For the minimum setup needed by the timing runner, evaluate the four-point
`e,E,A,A` helicity calculations and **Export eeAA to C++**. This writes
`tests/results/spinas-output/eeAA/manifest.json`, including your explicit
reference-model selections. Both reference configurations must be available.
An export only needs to be repeated when these settings change.

To reproduce the amplitude-construction claims, continue through the desired
QED sections, both pole preferences and their independent validation tables.
Use the separate SM notebook for the 74 SM families. Full high-multiplicity
validations can take much longer than the timing check. Their point counts
are chosen in the notebook; an interrupted run is not a completed validation.

The timing exporter reads the unchanged complete formulas from models 4 and 5,
which are the physical- and spurious-pole catalogs. It does not use whatever
amplitude variables happen to be present in an interactive kernel, and it does
not reconstruct or simplify an amplitude during timing. This makes the formulas
being measured explicit and reproducible.

## Try the complete timing workflow

Start with a short run:

```sh
.venv/bin/python tests/benchmarks/run-timings.py --smoke \
  --configuration tests/results/spinas-output/eeAA/manifest.json \
  --spinas /path/to/spinas
```

The runner builds a private double-precision SPINAS library, exports both
catalogs, builds the numerical drivers, compares every point with CalcHEP and
MadGraph, combines the paired photon runs and draws the figures. It runs the
two-fermion and four-fermion studies serially. It preserves the notebook
exports and installed libraries. `--smoke` uses ten points per multiplicity,
one timing sweep and one warmup point. These short timings verify the workflow;
they are not substitutes for the paper's measurements.

Use `--family photons` or `--family four-fermions` to run just one study.
`--kernel /path/to/WolframKernel` selects an installation not discovered
locally. `--no-plots` skips the optional plotting step. Use `--dry-run` to see
the commands before starting. The runner prints its output directory and
retains logs, source hashes, parameters, seeds, pointwise comparisons, timing
sweeps and the combined tables. An existing output is retained unless an
explicit replacement is requested.

For the paper-sized measurements, omit `--smoke`:

```sh
.venv/bin/python tests/benchmarks/run-timings.py \
  --configuration tests/results/spinas-output/eeAA/manifest.json \
  --spinas /path/to/spinas
```

The defaults are 10,000 points, nine sweeps, 100 warmup points and a
center-of-mass energy of 100 GeV. The saved one-pair seed is
`1793918752098091000`; the four-fermion seed is `1794010385397612000`.
The runner selects these study seeds unless overridden. Use a quiet machine
and run studies serially. New wall times depend on hardware, compiler,
selected formulas and system load; exact agreement with the published times
is not expected.

## What is timed and validated

The one-pair study is `e- e+ -> N photons`, with `N=2,3,4`. The two-pair study
is `e- e+ -> e- e+ + N photons`, with `N=0,1,2`; its detailed description is
in [four-fermion/README.md](four-fermion/README.md).

All implementations receive the same saved binary64 momenta and matched
masses and charge. Every spin and photon helicity is included. The independent
references use private zero-width configurations. The exported SPINAS sum is
also checked against exhaustive evaluation of its public complex amplitudes.
A failed comparison prevents that timing row from being accepted.

The timers include momentum setup and the complete squared-amplitude sum.
They exclude generation, compilation, initialization, phase-space generation,
validation, warmup and output. The figures show the median of nine sweeps and
its central 50 percent range. Each normalized curve uses that method's own
lowest-multiplicity time. CalcHEP and MadGraph photon timings are taken from
the designated physical run, rather than selecting whichever reference run
happened to be faster. The separate spurious-run reference measurements are
retained as a repeatability check.

The data identify the historical source revisions used in the papers. A new
run records the current source and library hashes. Rebuilding current code
is a reproducible new measurement, not a claim to have rerun the historical
compiler or hardware. See the saved-data provenance and the evidence-replay
README for the distinction.

## Manual builds and specialized comparisons

The sections below document individual exporters, numerical settings and
other studies for readers who want control of the separate stages. The
runner above performs the ordinary complete workflow.

## All-physical expressions in double precision

`export-physical.wl` reads the saved amplitudes from QED factorization model 4.
It checks that every kinematic denominator is a physical electron propagator,
then exports the complete helicity families for two, three and four photons.
It does not reconstruct or simplify a formula. The `benchmark-amplitudes.json`
sidecar records the model files, expressions, complexity scores and pole cuts.
The timing metadata includes this provenance, so a physical-pole calculation
cannot be confused with an unclassified earlier export.

The script uses an existing export manifest for the explicitly selected CalcHEP
and MadGraph installations and reference models. Its default is
`tests/results/spinas-output/eeAA/manifest.json`; use `--configuration /path/to/manifest.json`
to select another. Optional `--calchep` and `--madgraph` override installation
paths; HEPCAT selects a compatible Python for the chosen MadGraph installation.
Source formulas come from model 4 by default. `--representation AllSpurious`
selects the saved formulas in model 5 instead, without applying further identities.
Both selections record every inverse factor, including composite spinor
denominators, in their amplitude sidecars. Only `AllPhysical` requires every
kinematic inverse factor to be a physical electron propagator.

Build a private double-precision SPINAS library and a separate benchmark:

```sh
benchmark_dir="$PWD/tests/benchmarks/build-physical-double"
cmake -S "$HOME/code/spinas" -B "$benchmark_dir/spinas" \
  -DCMAKE_BUILD_TYPE=Release -DPRECISION_TYPE=DOUBLE -DBUILD_TESTING=OFF
cmake --build "$benchmark_dir/spinas" --target spinas_static --parallel
/Applications/Wolfram.app/Contents/MacOS/WolframKernel \
  -script tests/benchmarks/export-physical.wl
cmake -S tests/benchmarks -B "$benchmark_dir/optimized" \
  -DCALCHEP_ROOT="$HOME/code/CalcHEP/calchep_3.9.2" \
  -DSPINAS_ROOT="$HOME/code/spinas" \
  -DSPINAS_LIBRARY="$benchmark_dir/spinas/libspinas.a" \
  "-DHEPCAT_PROCESS_DIRECTORIES=$benchmark_dir/exports/eeAA;$benchmark_dir/exports/eeAAA;$benchmark_dir/exports/eeAAAA"
cmake --build "$benchmark_dir/optimized" --parallel
ctest --test-dir "$benchmark_dir/optimized" --output-on-failure
cd "$benchmark_dir/optimized"
./qed_photon_benchmark --output physical-double
```

Adjust the Wolfram executable and installation paths for another computer.
This leaves the existing SPINAS library and notebook exports unchanged. Both
the emitted C++ scalar type and the linked library must be double precision;
CMake checks their actual link compatibility. Equal mantissa widths alone are
insufficient to identify the C++ type on platforms such as macOS arm64.

The exporter defaults to `"CodeStyle" -> "Optimized"`. It analyzes the supplied
expressions, precomputes required spinor-product components once per momentum
point, and shares repeated arithmetic at the appropriate parameter, point or
spin level. The public amplitude interface still validates its arguments; the
generated spin sum uses a private kernel with its already validated spin states.
This avoids repeating accessor and validation work without changing the formulas.

Choose `"CodeStyle" -> "Readable"` in `WriteSPINASProcess` to retain direct
formula output. Both modes cache numerical constants and parameter-only factors,
and preserve complex amplitudes, phases and spin projections. Optimization is
bounded: oversized arithmetic graphs or canonical-dispatch projects retain a
supported evaluator, with the applied work and fallback reason recorded in the
manifest. Neither mode uses unsafe floating-point reassociation or fast-math.

To compare the modes, export the same saved formulas into separate directories:

```sh
/Applications/Wolfram.app/Contents/MacOS/WolframKernel \
  -script tests/benchmarks/export-physical.wl \
  --output "$benchmark_dir/readable-exports" --code-style Readable
/Applications/Wolfram.app/Contents/MacOS/WolframKernel \
  -script tests/benchmarks/export-physical.wl \
  --output "$benchmark_dir/optimized-exports" --code-style Optimized
```

Configure a separate benchmark build for each set, selecting its three export
directories with `HEPCAT_PROCESS_DIRECTORIES` as above. Run them serially with
the same explicit seed, energy, point count and repetitions. The metadata records
`CodeStyle`, `CodeLayout`, optimization diagnostics and the formula hashes.
Compare the total setting-momenta plus spin-sum time: precomputing all spin
components can be a tradeoff when only one massive-spin configuration is needed.
The timing results, rather than the mode name, determine the benefit for a process.

## Compare physical and spurious formulas with both reference generators

Export the complete saved families into separate directories:

```sh
/Applications/Wolfram.app/Contents/MacOS/WolframKernel \
  -script tests/benchmarks/export-physical.wl --representation AllPhysical \
  --output "$benchmark_dir/physical-exports"
/Applications/Wolfram.app/Contents/MacOS/WolframKernel \
  -script tests/benchmarks/export-physical.wl --representation AllSpurious \
  --output "$benchmark_dir/spurious-exports"
```

Configure and build a separate benchmark for each export set using the preceding
CMake recipe. Run the two benchmarks serially with the same explicit seed,
`--points 10000 --repeats 9 --warmup 100 --energy 100`. Both exports default to
optimized double-precision SPINAS code. Each run independently checks every point
against CalcHEP and MadGraph before accepting a timing row.

Before making a four-column table, require matching point-file hashes, model
parameters and precision, and compare the two saved SPINAS value columns directly.
Use the physical and spurious SPINAS medians and the reference medians from one
designated run. Keep the second run's reference times as repeatability evidence;
do not select whichever reference timing is faster. Preserve both raw result sets
and metadata with the combined table. Each curve is normalized to its own N=2
time. The `AllSpurious` label describes the saved representation preference;
individual sectors can still have only physical poles after cancellation.

The 2026-11-05 run with demand-driven caches, shared momentum chains and
guarded real denominators on the Apple M3 development Mac, with Apple Clang 21
Release `-O3`, gave these seconds for 10,000 points
(median of nine serial full sweeps). The center-of-mass energy was 100 GeV and
the saved seed was `1793918752098091000`. Model defaults were Me = 0.0005 GeV
and EE = 0.31333. All points and source formulas are unchanged from the earlier
exhaustive-summation benchmark.

| Photons | HEPCAT/SPINAS AllPhysical (s) | HEPCAT/SPINAS AllSpurious (s) | CalcHEP (s) | MadGraph (s) |
| --- | ---: | ---: | ---: | ---: |
| 2 | 0.002972 | 0.002549 | 0.002974 | 0.013884 |
| 3 | 0.008849 | 0.016775 | 0.020658 | 0.072378 |
| 4 | 0.078147 | 0.062394 | 0.249972 | 0.446684 |

The saved models are tree-level vectorlike QED, so this benchmark explicitly
uses `"HelicitySymmetry" -> "Conjugation"`. This is a declared theory symmetry,
not a symmetry fitted to samples. It evaluates 2, 4 and 8 representative photon
configurations while retaining all 4, 8 and 16 complex helicity amplitudes.
The ordinary exporter defaults to `Automatic`, which only reuses pairs proved
by bounded exact algebra. Use `None` for an exhaustive evaluator; the export
script accepts `--helicity-symmetry None` to reproduce that comparison.
Nonreal runtime parameters restore exhaustive sums. Fixed selections lacking
the reversed helicities also remain exhaustive.

Every one of the 30,000 points agreed across all four methods; the largest
relative difference was 2.62e-10. Each shortened SPINAS sum was independently
checked against the exhaustive sum of its public complex amplitudes outside
timing; their largest relative difference was 1.47e-12. Point-file and
source-amplitude hashes match the earlier run. CalcHEP
and MadGraph table entries use the physical run's nine sweeps; separately
repeated reference statistics are retained as a stability check. Timing includes
momentum setup and complete spin/helicity sums, excluding generation,
compilation, initialization and validation. MadGraph uses the exhaustive native
MATRIX interface described below, without helicity sampling or recycling.
These are matrix-element evaluation timings, not event throughput.

Compared with the preceding conjugate-helicity benchmark, the additional
numerical setup changes gave these speedups:

| Photons | AllPhysical speedup | AllSpurious speedup |
| --- | ---: | ---: |
| 2 | 1.97 | 2.00 |
| 3 | 2.00 | 1.24 |
| 4 | 1.60 | 1.74 |

For four photons, AllPhysical fell from 0.124803 s to 0.078147 s, and AllSpurious
from 0.108535 s to 0.062394 s. These gains include point preparation. The
shortened sum prepares only its required product components and arithmetic;
individual complex amplitudes remain available and fill any missing caches
lazily. SPINAS obtains phases directly from Cartesian momenta and can share a
repeated middle-momentum matrix across brackets. Generated code also works
with older SPINAS headers through the ordinary per-product fallback, although
that fallback does not share the new middle-matrix helper.

Whole real denominator products cache their reciprocal only under exact runtime
reality checks. Width-bearing propagators remain complex, including symbolic
widths whose default happens to be zero. Singular, subnormal, nonfinite and
unsafe multiplication cases retain the original complex division. Independent
CalcHEP and MadGraph validation uses private zero-width models and process
copies; normal numerical simulations retain their configured widths.

The local combined table and complete provenance are
`build-numerical-setup-double/fourway.csv` and
`build-numerical-setup-double/fourway-summary.json`. The CSV also gives each
method's growth relative to its own N=2 baseline. Raw repetitions, identical
points, exhaustive comparisons and `setup-speedup.csv` remain in that build
directory. The preceding conjugate-helicity run is retained in
`build-fourway-helicity-double`, and the earlier exhaustive run in
`build-fourway-double`. These generated artifacts are excluded from Git.

## Readable versus optimized export timings

On the development Mac (arm64, Apple Clang 21, double precision, `-O3`), the
2026-11-05 comparison gave the following medians for 10,000 points and nine
repetitions, using seed `202611051342` at 100 GeV. Both modes used the same
all-physical formulas and identical saved momenta. Every point agreed with the
independent CalcHEP and MadGraph implementations within the benchmark tolerance.

| Photons | Readable SPINAS (s) | Optimized SPINAS (s) | Speedup |
| --- | ---: | ---: | ---: |
| 2 | 0.006643 | 0.006263 | 1.06 |
| 3 | 0.030851 | 0.022522 | 1.37 |
| 4 | 0.764442 | 0.237815 | 3.21 |

For four photons the same optimized run measured 0.253515 s for CalcHEP and
0.452404 s for the independent MadGraph interface described below. These are
observations for these formulas and interfaces, not universal backend rankings.
The local raw files are under `build-physical-double/readable-mode/readable*`
and `build-physical-double/optimized-mode/optimized*`; the paired table is
`build-physical-double/code-style-comparison.csv`. These generated files are
excluded from Git and can be recreated with the workflow above.

## Benchmark other existing exports

First export a complete helicity family for each photon count with
`WriteSPINASProcess`. Configure both validation backends in those exports:
`CalcHEPDirectory`, `CalcHEPModel`, `MadGraphDirectory`, and `MadGraphModel`.
Explicitly choose `{11,22}` for both `CalcHEPAllowedParticles` and
`MadGraphAllowedParticles`, so every calculation is the same electron/photon QED.
Reference model selection and parameter matching are taken from those manifests.

From the HEPCAT repository:

```sh
cmake -S benchmarks -B tests/benchmarks/build \
  -DCALCHEP_ROOT="$HOME/code/CalcHEP/calchep_3.9.2" \
  -DSPINAS_ROOT="$HOME/code/spinas"
cmake --build tests/benchmarks/build --parallel
ctest --test-dir tests/benchmarks/build --output-on-failure
cd tests/benchmarks/build
./qed_photon_benchmark
```

The generic CMake defaults use `tests/results/spinas-output/eeAA`, `eeAAA`, and `eeAAAA`:
N=2,3,4. Their representation is whatever the user last exported. The original
timing run used spurious-pole formulas. Use the separate workflow above for the
explicitly verified all-physical set. The newer N=5 and N=6 all-positive
formulas alone are **not** complete helicity sums. Add those multiplicities
when all their independent helicity sectors have been constructed and exported.
The program rejects incomplete families rather than comparing them with the
fully summed reference generators.

To supply other export directories, configure with a semicolon-separated list:

```sh
cmake -S benchmarks -B tests/benchmarks/build \
  -DCALCHEP_ROOT=/path/to/CalcHEP \
  '-DHEPCAT_PROCESS_DIRECTORIES=/path/to/eeAA;/path/to/eeAAA;/path/to/eeAAAA'
```

Use the same SPINAS precision for all exports and the linked SPINAS library.
`Release` compilation is required. The benchmark privately rebuilds the
independent CalcHEP and MadGraph matrix elements with `-O3`. It leaves their
installations, model files, normal validation defaults, and event sessions alone.

Defaults are 10,000 points per N, five complete timing repetitions, 100 warmup
calls, and fixed center-of-mass energy 100 GeV. The seed comes from the time and
is printed and saved. Override settings when needed:

```sh
./qed_photon_benchmark --points 10000 --repeats 5 --energy 100 \
  --seed 12345 --output photon-timings
```

Run on an otherwise idle machine and repeat on a different occasion before
using the timings in a paper. Timed backends run serially, never competitively
in parallel. Their order rotates between repetitions. The same fixed mass and
coupling values must be used at every N; values come from the exported model.

## Files to plot

- `photon-timings.csv`: one row per N. `spinas_seconds`, `calchep_seconds`, and
  `madgraph_seconds` are median elapsed times for one complete 10,000-point
  sweep. The three `*_relative_N2` columns are the requested three curves.
- `photon-timings-repetitions.csv`: every elapsed-time measurement, including
  seconds per point. Use these to show timing spread or error bars.
- `photon-timings-points-N*.csv`: the actual common physical momenta, in GeV.
- `photon-timings-values-N*.csv`: all three squared amplitudes and relative
  differences at every point. Disagreement stops the benchmark and rejects that
  multiplicity's timing row; no failing point is silently discarded.
- `photon-timings-metadata.json`: settings, seed, numeric precision, normalization,
  machine, requested/completed multiplicities, model values, compiler settings,
  source/model/library hashes, and the repository revision.
- `photon-timings-madgraph-N*.json`: independent MadGraph generation provenance,
  including version, parameters, graph/helicity counts, compiler and flags.

For example, Mathematica can read the plot table with
`Import["photon-timings.csv", "CSV"]`, or use the CSV
reader in any plotting program. The normalized values for N=2 are exactly 1.
Earlier validated rows remain in the CSV if a later multiplicity fails;
metadata then says `PARTIAL` and lists which counts completed.

## What the measurement means

The timed operation is setting each new momentum point and evaluating the
**fully summed squared amplitude**, with incoming spin average 1/4 and final
identical-photon factor 1/N!. Each backend applies these exactly once. It is
not the square of a sum over different helicities.

The independent MadGraph implementation uses its generated standalone Fortran
`MATRIX` and exhaustively loops over the full generated helicity inventory.
It does not use the HEPCAT-replaced `PROC_*` MadEvent matrix, helicity sampling,
or an optional helicity-recycling implementation. CalcHEP uses its independently
generated symbolically spin-summed matrix element. These are specific numerical
interfaces, not a claim about the fastest possible configuration of either tool.
Native adapter overhead is included: CalcHEP packs its input buffer on each call;
MadGraph's flat input buffers are prepared in advance. SPINAS updates its
particles and cached products before summing the selected spin configurations.

Phase-space generation uses massless RAMBO at fixed energy, without cuts, and
is outside all timings. Momenta are rounded to double once before being sent
to any backend. Every point is checked for shell and conservation constraints.
Code generation, compilation, initialization, numerical agreement checks,
warmup, file output and checksums are also outside the timed regions. This is
an amplitude-throughput measurement, not cross-section integration or event
unweighting throughput. No phase-space weights are required for this timing.

CalcHEP and MadGraph use double precision. SPINAS uses the linked build's scalar
type; its actual mantissa bits and exported precision are recorded. On some
platforms `long double` has the same precision as `double`; on others it does
not. Report this in the paper or repeat using matching double SPINAS exports.
Build dependencies trigger CMake reconfiguration when measured sources, models,
or libraries change. Rebuild before recording publication results.


## Direct all-positive formula checks above six photons

`all-positive-formulas.hpp` evaluates the previously fixed physical and spurious
all-positive QED recurrences directly, using memoized photon subsets.
`all-positive-validation.cpp` compares both with an independent MadGraph
matrix element. It does not load Mathematica, generate HEPCAT diagrams, run
preparation/completion, or expand the formulas into long symbolic expressions.
The argument N counts photons: N=7 and N=8 mean nine and ten external particles.

Both formulas take physical positive-energy momenta for incoming electron and
positron and outgoing positive-helicity photons. The evaluator performs the
all-ingoing crossing once, including photon angle-spinor signs. It retains
all four massive spin amplitudes. The comparison uses the raw sum of their
squared moduli on each side, with every photon fixed positive; it applies no
initial average or identical-particle phase-space factor. An overall phase of
the amplitude therefore cannot cause a false mismatch.

Build against a matching SPINAS library. For this checkout's LongDouble build:

```sh
validation_dir="$PWD/tests/benchmarks/build-all-positive-high-multiplicity"
mkdir -p "$validation_dir"
clang++ -O2 -std=c++11 -DWITH_LONG_DOUBLE \
  -I"$HOME/code/spinas/include" -I"$HOME/code/spinas" -Isource/internal/export \
  tests/benchmarks/all-positive-validation.cpp "$HOME/code/spinas/build/libspinas.a" \
  -o "$validation_dir/all-positive-validation"
"$validation_dir/all-positive-validation" --self-test
```

For a Double SPINAS library, omit `-DWITH_LONG_DOUBLE`. The controls compare
both recurrences with the established two-, three- and four-photon formulas
at two masses and charges and every massive spin. They also compare ordered
and determinant-collected scalar currents.

Generate a private MadGraph reference with the existing helper. For eight
photons, save a JSON configuration such as:

```json
{
  "MadGraphDirectory": "/path/to/MadGraph",
  "MadGraphModel": "sm-full",
  "PhysicalPDGs": [11, -11, 22, 22, 22, 22, 22, 22, 22, 22],
  "AllowedParticlePDGs": [11, 22],
  "Masses": [1.5, 1.5, 0, 0, 0, 0, 0, 0, 0, 0],
  "Parameters": [{"Name": "Me", "Value": 1.5},
                 {"Name": "EE", "Value": 0.3333333333333333}],
  "ParameterRules": {},
  "ParameterMap": {},
  "JampSplit": 128,
  "TimeConstraint": 7200
}
```

Use a Python compatible with the selected MadGraph installation:

```sh
python3 source/internal/export/madgraph-runtime.py --config /path/to/config.json \
  --output "$validation_dir/reference-N8"
"$validation_dir/all-positive-validation" --photons 8 --points 1000 \
  --mass 1.5 --reference "$validation_dir/reference-N8/libhepcat_madgraph.dylib" \
  --output "$validation_dir/N8"
```

Use the `.so` library suffix on Linux. For seven photons remove one photon and
one zero mass from the configuration, then select `--photons 7`. Mass and charge
must match the reference card; rebuilding with new configuration values makes
those inputs explicit. Energy bounds default to 10 through 100 in the selected
mass units. The default seed comes from the time and is printed and saved;
`--seed INTEGER` replays a run. The report and CSV files retain every point and
comparison, without retrying or discarding an inconvenient point. The default
relative tolerance is 1e-7 with zero absolute tolerance.

`JampSplit` is an optional internal reference-build setting, not a
`ConstructAmplitude` option. It invokes MadGraph's own `get_JAMP_lines` split
argument and disables its color-code reoptimization for that export. This
keeps the native diagrams, coefficients and helicity states while writing
bounded final-sum statements. A private bootstrap starts in MadGraph's normal
optimized Python mode so its startup cannot bypass that setting. Installed
MadGraph files are unchanged. Omit the setting, or use zero/null, to keep the
ordinary reference generation unchanged. It does not alter MadEvent defaults.
The helper records this policy and the bootstrap's source/hash in `result.json`.


The 2026-11-05 direct tests passed all 1000 requested points per multiplicity:

| Photons | External particles | Mass | AllPhysical max. relative difference | AllSpurious max. relative difference |
| --- | --- | --- | ---: | ---: |
| 6 (control) | 8 | 1 | 8.21e-12 | 8.21e-12 |
| 7 | 9 | 1 | 4.77e-12 | 3.29e-11 |
| 8 | 10 | 1.5 | 1.23e-11 | 1.45e-8 |

Charge is 1/3, with logarithmically sampled center-of-mass energies from 10 to
100. The spurious representation has larger numerical deviations in this
sample, particularly at eight photons. Its worst original point is retained
in the raw results; no relabeling was used to replace a value in the comparison.
These are finite numerical tests, not an analytic all-N proof.

MadGraph supplied 720, 5040 and 40320 native diagrams respectively. The
seven-photon split-reference control agreed exactly with the original native
output at every one of 4000 massive-spin evaluations. The split references
have 40 and 315 final-sum statements at N=7 and N=8. Their matrix files are
byte-identical to the original outside `GET_JAMP`. The original eight-photon
compile was interrupted because of its single enormous sum, not because of
a numerical discrepancy; it is not counted as a completed comparison.

The local `build-all-positive-high-multiplicity/validation-summary.csv` and
`validation-summary.json` retain all results, seeds, input/source hashes and
reference metadata. The `N6-*`, `N7-*`, `N8-*` CSVs retain all common momenta
and individual comparisons; `native-split-control.json` records the independent
formatting control. The tested reference directories are `reference-N6`,
`reference-N7`, and `reference-N8-split-v2`. Their configurations, generated
sources, parameter cards and build logs are preserved. Generated artifacts
remain outside Git; the evaluator, comparison program and replay instructions
are versioned.

## Independent published-formula checks

These research controls use separately transcribed published amplitudes, not
answers supplied to the constructive algorithm. Their equation and convention
dictionaries distinguish known amplitude families from possible new ways of
organizing the same answer.

- [All-positive one-pair comparison](literature-all-positive/README.md):
  Ochirov's all-multiplicity ordered formula, fully Abelianized, tested against
  both subset formulas at two through six photons. Includes a separate literal
  five-point Das-Zeng QED formula and rejected incomplete-permutation controls.
- [One-negative one-pair comparison](literature-one-minus/README.md):
  published primitives plus the complete BCJ permutation reconstruction at
  two and three photons. Records polarization phases and massive-spinor
  conventions explicitly, with independent primitive and BCJ controls.
- [Four-fermion comparison](four-fermion-literature/README.md):
  exact low-point published-primitive Abelianization, with both identical
  fermion pairings retained. Its checked scope is stated separately from the
  higher-multiplicity MadGraph validation of the current formula.

The all-positive and one-negative numerical checks share SPINAS's low-level
spinors with the tested code. They independently validate amplitude algebra
and ordering, rather than providing a separate implementation of the spinor
backend. Finite tests do not prove the general representation identities.
