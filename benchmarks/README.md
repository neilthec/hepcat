# QED photon timing benchmark

This program measures `e- e+ -> N photons` at the **same 10,000 saved phase-space
points** in HEPCAT/SPINAS, independent CalcHEP, and independent MadGraph. Every
point includes both photon helicities and both electron/positron spin states.
Each backend is normalized to its own two-photon elapsed time for plotting.

The calculation and timers are C++: [qed-photons.cpp](qed-photons.cpp) and
[benchmark.hpp](benchmark.hpp). A small Python configuration step reads the
existing export manifests and writes build adapters. It neither changes the
amplitudes nor regenerates or overwrites the user's exported projects.

## All-physical expressions in double precision

`export-physical.wl` reads the saved amplitudes from QED factorization model 3.
It checks that every kinematic denominator is a physical electron propagator,
then exports the complete helicity families for two, three and four photons.
It does not reconstruct or simplify a formula. The `benchmark-amplitudes.json`
sidecar records the model files, expressions, complexity scores and pole cuts.
The timing metadata includes this provenance, so a physical-pole calculation
cannot be confused with an unclassified earlier export.

The script uses an existing export manifest for the explicitly selected CalcHEP
and MadGraph installations and reference models. Its default is
`tests/spinas-output/eeAA/manifest.json`; use `--configuration /path/to/manifest.json`
to select another. Optional `--calchep` and `--madgraph` override installation
paths; HEPCAT selects a compatible Python for the chosen MadGraph installation.
Source formulas come from model 3 by default. `--representation AllSpurious`
selects the saved formulas in model 4 instead, without applying further identities.
Both selections record every inverse factor, including composite spinor
denominators, in their amplitude sidecars. Only `AllPhysical` requires every
kinematic inverse factor to be a physical electron propagator.

Build a private double-precision SPINAS library and a separate benchmark:

```sh
benchmark_dir="$PWD/benchmarks/build-physical-double"
cmake -S "$HOME/code/spinas" -B "$benchmark_dir/spinas" \
  -DCMAKE_BUILD_TYPE=Release -DPRECISION_TYPE=DOUBLE -DBUILD_TESTING=OFF
cmake --build "$benchmark_dir/spinas" --target spinas_static --parallel
/Applications/Wolfram.app/Contents/MacOS/WolframKernel \
  -script benchmarks/export-physical.wl
cmake -S benchmarks -B "$benchmark_dir/optimized" \
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
  -script benchmarks/export-physical.wl \
  --output "$benchmark_dir/readable-exports" --code-style Readable
/Applications/Wolfram.app/Contents/MacOS/WolframKernel \
  -script benchmarks/export-physical.wl \
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
  -script benchmarks/export-physical.wl --representation AllPhysical \
  --output "$benchmark_dir/physical-exports"
/Applications/Wolfram.app/Contents/MacOS/WolframKernel \
  -script benchmarks/export-physical.wl --representation AllSpurious \
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

The 2026-11-05 four-way run on the Apple M3 development Mac, with Apple Clang
21 Release `-O3`, gave these seconds for 10,000 points (median of nine serial
full sweeps). The center-of-mass energy was 100 GeV and the time-derived saved
seed was `1793918752098091000`. Model defaults were Me = 0.0005 GeV and EE = 0.31333.

| Photons | HEPCAT/SPINAS AllPhysical (s) | HEPCAT/SPINAS AllSpurious (s) | CalcHEP (s) | MadGraph (s) |
| --- | ---: | ---: | ---: | ---: |
| 2 | 0.006137 | 0.005338 | 0.002948 | 0.013862 |
| 3 | 0.022125 | 0.027845 | 0.020787 | 0.072682 |
| 4 | 0.235735 | 0.176184 | 0.250278 | 0.446573 |

Every one of the 30,000 points agreed across all four methods; the largest
relative difference was 2.62e-10. Point-file hashes were identical between
the paired runs. CalcHEP and MadGraph table entries use the physical run's nine
sweeps, with the separately repeated reference statistics retained as a stability
check. Timing includes momentum setup and complete spin/helicity sums, excluding
all generation, compilation, initialization and validation. MadGraph uses the
exhaustive native MATRIX interface described below, with no helicity sampling or
recycling. This comparison measures matrix-element evaluation, not event throughput.

The local combined table and complete provenance are
`build-fourway-double/fourway.csv` and `build-fourway-double/fourway-summary.json`.
The CSV also gives each method's growth relative to its own N=2 baseline. Raw
repetitions, identical point sets and pointwise comparisons remain in that build
directory. These generated artifacts are excluded from Git.

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
cmake -S benchmarks -B benchmarks/build \
  -DCALCHEP_ROOT="$HOME/code/CalcHEP/calchep_3.9.2" \
  -DSPINAS_ROOT="$HOME/code/spinas"
cmake --build benchmarks/build --parallel
ctest --test-dir benchmarks/build --output-on-failure
cd benchmarks/build
./qed_photon_benchmark
```

The generic CMake defaults use `tests/spinas-output/eeAA`, `eeAAA`, and `eeAAAA`:
N=2,3,4. Their representation is whatever the user last exported. The original
timing run used spurious-pole formulas. Use the separate workflow above for the
explicitly verified all-physical set. The newer N=5 and N=6 all-positive
formulas alone are **not** complete helicity sums. Add those multiplicities
when all their independent helicity sectors have been constructed and exported.
The program rejects incomplete families rather than comparing them with the
fully summed reference generators.

To supply other export directories, configure with a semicolon-separated list:

```sh
cmake -S benchmarks -B benchmarks/build \
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
