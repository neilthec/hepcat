# Verify the paper results

These tools let a reader check the numerical records and amplitude catalog
used in the QED and HEPCAT papers. They also provide a numerical replay of
the precision study. The inputs are curated in one `paper-evidence.zip`;
the full research directory and generated build trees are not required.
The bundle contains data and frozen evaluator source, not external binaries.

Run commands from the HEPCAT checkout root. All defaults are relative to the
checkout, not a particular user's home directory. Generated results go under
`tests/results/reproduction/`, which is ignored by Git.

First unpack the optional tools and data from the publication bundle:

```sh
python3 tests/run-checks.py --unpack-only
```

This verifies and expands `tests/benchmarks/benchmarks.zip` to the paths used
below. Expanded files are ignored by Git. The compact checkout retains this
guide and the bundle; generated results remain under `tests/results/`.

## Check the saved evidence

Python 3.9 or later is sufficient. This command needs no Wolfram installation,
SPINAS library, CalcHEP, MadGraph or plotting packages:

```sh
python3 tests/benchmarks/reproduction/verify.py
```

It verifies input hashes and then:

- Regenerates the 38 catalog representatives from models 3 and 4, tests
  exact rational formatter roundtrips and checks the long TeX expansions.
- Checks the original SM comparison records: 74 CalcHEP and MadGraph family
  passes and 132 MadGraph helicity rows with 1,000 comparisons each. The
  saved construction summary records 132 Feynman passes; captured component
  outputs and their coverage are reported separately.
- Checks the completed general-formula and literature-validation records,
  including the saved million-point seven-photon results. An interrupted
  eight-photon high-statistics run is not counted as completed.
- Rechecks the four numerical implementations on all 60,000 saved timing
  points, using the retained squared-amplitude values.
- Recomputes the precision statistics from all 48 pointwise CSVs, including
  each labeled photon-helicity sector. Extended values are read as decimals
  rather than first rounded to Python's binary64 float.
- Checks all 72 printed precision entries, including the separately labeled
  eight-photon input-rounding experiment.

The final `evidence-verification.json` is written in
`tests/results/reproduction/`. The regenerated catalog and precision tables
are retained there too. The input bundle and model files are left unchanged.
A checksum mismatch, incomplete inventory or disagreeing published value
stops the command; it does not produce a successful report.

To redraw the HEPCAT paper's construction-workflow diagram, install the
optional [plotting requirements](../requirements.txt), then run:

```sh
python3 tests/benchmarks/reproduction/draw-workflow.py
```

This writes `workflow.pdf` and `workflow.png` under
`tests/results/reproduction/figures/`. Use `--output-directory DIR` for another
destination. It illustrates the method and does not run a physics calculation.

This verifies the retained evidence and its transcription. It does not
perform a fresh symbolic construction or run a new independent generator.
The exact rational catalog checks establish transcription identities, not
the physical correctness of every amplitude. The original numerical checks
remain finite sampled evidence, rather than all-kinematics proofs.

## Recompute the precision statistics separately

To regenerate just the precision summaries from the unchanged raw results:

```sh
python3 tests/benchmarks/reproduction/analyze-precision.py
```

This writes `summary.csv`, `summary.json` and `helicity-summary.csv` under
`tests/results/reproduction/precision/`. The statistics include medians,
percentiles, maxima, threshold counts and worst-point identities. The seven
catalog families use 61,000 common point configurations in total. The
high-multiplicity one-pair study uses a separate 1,000 points at each of five
through eight photons. These samples and their scopes remain distinct.

Use `--output-directory DIR` for another destination. `--data-directory DIR`
reads a matching result tree from a new complete numerical replay;
`--data-archive FILE` accepts a ZIP containing the unchanged precision CSVs.
Use `--totals-only` when fixed-helicity reanalysis is not needed.

## Re-evaluate the numerical amplitudes

This is a different operation from reading saved rates. It compiles the
frozen C++ evaluators with a private SPINAS copy and recomputes their
amplitudes at the retained momenta. It compares with the original saved
MadGraph rates. It does not claim that MadGraph was run again.

Start with one family in double precision:

```sh
python3 tests/benchmarks/reproduction/replay-precision.py \
  --spinas /path/to/spinas --families eeAA --modes double \
  --output-directory tests/results/precision-smoke
```

The `eeAA` sample contains 10,000 momenta and all four photon-helicity
choices. The command writes build logs, new rates, measured precision and
`numerical-replay.json` in the selected directory. Use a new output directory
for a different experiment, so earlier result files cannot be mistaken for
newly completed work.

The complete original study uses these family groups:
`eeAA`, `eeAAA`, `eeAAAA`, `four-fermion`, `six-fermion`, `high-n` and
`rounding`. With no `--families` or `--modes` selection, the driver runs all
supported groups in float, double and extended precision. The rounding
experiment has only double and extended records.

Choose a target that provides the required 64-bit significand before running
the complete study. Native Apple Silicon `long double` has only 53 bits;
there, add `--target x86_64` with Apple's compiler and Rosetta, as shown below.
The following native command is for a platform with a true extended type.

```sh
python3 tests/benchmarks/reproduction/replay-precision.py \
  --spinas /path/to/spinas \
  --output-directory tests/results/precision-full
python3 tests/benchmarks/reproduction/analyze-precision.py \
  --data-directory tests/results/precision-full \
  --output-directory tests/results/precision-full/analysis
```

A compatible C++17 compiler and SPINAS source checkout are needed. The
recorded SPINAS revision is `c8c69ff330d0845f1e5ea1116d049093683e92bf`.
The installed source and library are never edited. The private study builds
select their scalar type and retain the recorded external-momentum admission
policy. Float is an isolated study configuration, not a production precision
option of HEPCAT's exporter.

The tool measures the actual significand widths and requires 24, 53 and 64
bits for the three modes. Native Apple Silicon `long double` has only 53
bits. To reproduce the 64-bit experiment there, use Apple's compiler and
Rosetta with `--target x86_64`; otherwise use a native platform providing
the required extended type. For example:

```sh
python3 tests/benchmarks/reproduction/replay-precision.py \
  --spinas /path/to/spinas --target x86_64 \
  --output-directory tests/results/precision-x86
```

The frozen input policies are retained. One-pair and high-multiplicity
drivers parse coordinates through binary64, then convert to the working
type. Four- and six-fermion drivers parse the same decimal records directly
at the working precision. Their extended inputs therefore need not contain
exactly the original binary64 bits. Float also quantizes the inputs. These
comparisons include the specified input and arithmetic effects; the
MadGraph references remain binary64. The saved and separately projected
eight-photon inputs are explicitly distinguished.

## Perform fresh independent checks

For new symbolic constructions, physical residue checks and independent
Feynman, CalcHEP and MadGraph comparisons, use:

- `tests/QED-6-point-construction.nb` for the QED catalog and general formulas;
- `tests/SM-4-point-construction.nb` for the SM four-point families.

Set the installation paths and models in each notebook's Setup. Keep model
parameters, particle restrictions, widths, crossing, spin sums and symmetry
factors matched. The notebooks use public HEPCAT functions. The general
formula tests use the formulas written explicitly in the notebook, not an
automatic construction or a replacement by a reference answer.

High-statistics comparisons are optional and can take much longer. Select
the point count in the call being run and keep its actual completed report.
The recorded seven-photon million-point passes and the separately completed
eight-photon 1,000-point comparisons must not be conflated with the stopped
eight-photon million-point attempt.

For fresh four-way timing runs and plotting, follow
[the benchmark README](../README.md). For component regression tests, follow
[the check README](../../checks/README.md).

## New 100,000-point formula checks and same-input precision replay

The 11 November 2026 campaign made four new independent MadGraph comparisons,
with 100,000 points per call. These are separate from the historical 1,000-point
and million-point records described above. The supplied formulas were evaluated
through `ValidateMadGraph`, the same public tool used in the QED notebook.

| Photons | Form | Seed | Compared points | Mismatches at relative tolerance 10^-7 | Maximum relative difference |
| --- | --- | --- | ---: | ---: | ---: |
|7|AllPhysical|1794446122171025000|100,000|0|1.533317406473041e-9|
|7|AllSpurious|1794446506139815000|100,000|0|7.706445651083013e-9|
|8|AllPhysical|1794447136623380000|100,000|0|1.1287517761254384e-8|
|8|AllSpurious|1794448410592767000|100,000|16|4.9282386699895785e-6|

All four calls completed every requested comparison, with zero evaluation
errors. The final row is **FAIL**, and remains FAIL. The settings were
Me=0.0005, EE=0.31333, center-of-mass energies 1.5 through 6, zero widths,
relative tolerance 10^-7 and absolute tolerance 0. Incoming electron/positron
spins were summed with an overall 1/4 average; all photon helicities were fixed
positive in HEPCAT's all-ingoing convention. No final photon factorial was
included in this comparison. The MadGraph reference restricted intermediate
particles to electrons and photons. Native Apple Silicon arithmetic provided
53 significand bits even though the type was named `long double`.

To repeat one complete call, evaluate Setup and the general-formula definitions
in `tests/QED-6-point-construction.nb`, then call the existing validator. For
example, after defining `qedFormulaSpuriousPPPPPPPP`:

```wolfram
ValidateMadGraph[
  Join[{"e", "E"}, ConstantArray["A.+",8]],
  qedFormulaSpuriousPPPPPPPP, diagramModel,
  "MadGraphPoints" ->100000,
  "MadGraphSeed" ->1794448410592767000,
  "MinimumEnergy" ->1.5, "MaximumEnergy" ->6,
  "RelativeTolerance" ->10^-7, "AbsoluteTolerance" ->0,
  "Output" ->"Details",
  Sequence @@ madGraphValidationOptions]
```

Replace the form/photon count and seed using the table for the other calls.
Explicit options precede the shared options, so they override shared defaults.
Keep the returned details report, its actual point counts and its resolved seed.
This performs a new MadGraph calculation and can take substantially longer than
the selected-point replay below.

The spurious failure motivated a controlled arithmetic diagnostic. The retained
fixture contains 1,600 points in the 100-point error-reporting bins containing
all 16 original failures. This is a **conditional subset**, not a new random
sample or an extended-precision 100,000-point validation. It retains the literal
HEPCAT-generated arithmetic graphs, their matching runtime headers, original
binary64 momenta and parameters, and the separately recomputed MadGraph rates
at those same selected points. The first recovered independent rate agrees
exactly with the original saved rate; native replay recovers all 16 failures
and the original worst discrepancy. No compensating recurrence or different
amplitude representation is substituted.

Check the compact fixture without compiling anything:

```sh
python3 tests/benchmarks/reproduction/replay-generated-precision.py --verify-only
```

The fixture also includes the four complete compressed details reports, their
original compressed/uncompressed hashes, source and notebook hashes, seeds,
formula identities and the recorded environment. The WXF report values are
untouched. To inspect them, unpack and decompress them without compiling:

```sh
python3 tests/benchmarks/reproduction/replay-generated-precision.py \
  --extract-only \
  --output-directory tests/results/generated-precision-inspect
```

Then, in ordinary Mathematica, set the path to your checkout and import a report:

```wolfram
reportDirectory = "/path/to/hepcat/tests/results/generated-precision-inspect/fixture/reports";
report = Import[
  FileNameJoin[{reportDirectory, "call-004-ValidateMadGraph.wxf"}], "WXF"];
report["Status"]
report["Rows"]
```

Reports 001 through 004 correspond to the four table rows in order. The extraction
uses Python's standard-library gzip support and verifies every decompressed
byte; Mathematica needs no additional Python library to read WXF. Historical
absolute paths inside the unmodified reports identify the original run and are
not required paths on your machine. The eight-photon generated graphs are
included for the arithmetic replay. Seven-photon graphs can be regenerated
from the explicit notebook formulas using the public call above.

To recompute both generated graphs on all 1,600 unchanged points, build private
53-bit and 64-bit SPINAS copies. On Apple Silicon, select the Intel target with
Rosetta to obtain a true 64-bit significand:

```sh
python3 tests/benchmarks/reproduction/replay-generated-precision.py \
  --spinas /path/to/spinas --target x86_64 \
  --output-directory tests/results/generated-precision-x86
```

On a native x86 platform with true extended `long double`, omit `--target`.
Use `--modes double` for 53-bit arithmetic alone. `--selection failures` replays
only the 16 original failing points; `--selection first` replays point 2437.
Compiling these large frozen graphs can take several minutes per target; the
16-point selection reduces evaluation time but does not shorten compilation.
The tool measures actual precision, verifies all retained input bit patterns,
and refuses to overwrite a nonempty output directory. It uses the isolated
library builder described above; the installed SPINAS checkout stays unchanged.
This replay compares saved MadGraph rates and does not rerun MadGraph.

The original diagnostic found:

| Form/arithmetic | Selected points | Mismatches at unchanged tolerance | Maximum relative difference |
| --- | ---: | ---: | ---: |
|AllPhysical, native ARM53|1,600|0|7.630177579219658e-12|
|AllSpurious, native ARM53|1,600|16|4.928238669979738e-6|
|AllPhysical, Intel64|1,600|0|1.863108212539769e-11|
|AllSpurious, Intel64|1,600|1|1.592686254325058e-7|

The remaining 64-bit mismatch is point 77413. More arithmetic precision therefore
helps substantially but does not make every unchanged rounded input agree at
this tolerance. A separate control changes the input momenta to satisfy the
shell and conservation relations at 64-bit precision. To reproduce that control:

```sh
python3 tests/benchmarks/reproduction/replay-generated-precision.py \
  --spinas /path/to/spinas --target x86_64 --modes extended \
  --selection failures --project-shells \
  --output-directory tests/results/generated-precision-projected
```

This last command is explicitly a changed-kinematics, 16-point comparison of the
two HEPCAT forms. It does **not** compare projected points with the old MadGraph
rates and does not replace the original FAIL. The recorded maximum relative
form-to-form difference was 4.309267831159759e-9; the largest momentum-component
change divided by the total energy was 1.522242974148612e-16. The controlled
projection procedure and both projected input arrays are retained in the
replay output. These tests distinguish arithmetic sensitivity from sensitivity
to the small shell/conservation residuals already present in binary64 inputs.

## Provenance and interpretation

The bundle retains measurement revisions, seeds, model hashes, evaluator
sources, reference rates and the relevant original validation reports.
Historical absolute paths in records are provenance, not paths required on
the reader's machine. The tools resolve the supplied data locally.

Catalog/model changes should be checked deliberately: the default verification
compares the present models 3 and 4 with the frozen paper catalog. A later
model extension may require a new snapshot rather than changing historical
results to make the test pass. New wall times, numerical last bits and compiler
details can vary across machines; keep the new metadata with a rerun.
