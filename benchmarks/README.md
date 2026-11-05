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
Source formulas always come from model 3 in this checkout.

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

The generated exporter caches fixed numerical constants once and refreshes
model-only factors when masses or couplings change. Spinor and momentum
dependence is retained on its ordinary per-point/per-spin update path.

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
