# HEPCAT

High Energy Physics Constructive Amplitude Tool

HEPCAT constructs, simplifies and checks scattering amplitudes in Wolfram
Language. It also exports numerical SPINAS processes and compares them with
independent CalcHEP and MadGraph calculations.

## Start here

With the Wolfram working directory set to this checkout root, evaluate:

```wolfram
Get[FileNameJoin[{"source", "HEPCAT.wl"}]]
```

Search Mathematica Help for **HEPCAT** for the package guide, public function
reference pages and worked examples. The complete Help is included in
`Documentation/HEPCAT-Documentation.zip`. Loading the package makes it available
automatically from a writable user cache, including when the checkout itself
is read-only. There is no separate Help installation step.

Open either notebook in `tests/` and evaluate its Setup:

- `QED-6-point-construction.nb` constructs and checks the QED examples.
- `SM-4-point-construction.nb` constructs and checks the SM four-point examples.

Each notebook finds this checkout from its own location. Set the external
SPINAS, CalcHEP and MadGraph paths in Setup for your installations. The saved
outputs record earlier calculations; rerunning generates new results and
actual random seeds. Numerical projects and their build files go in
`tests/results/`, which is ignored by Git.

The Wolfram workflows were tested with version 15.0. Numerical export also
needs SPINAS, CMake and compatible compilers. Independent reference tests need
the selected CalcHEP and MadGraph installations. The model and testing guides
explain the particular input conventions and optional dependencies.

## Directory guide

| Directory | Contents |
| --- | --- |
| `source/` | Package loader, spinor algebra and amplitude construction. Supporting implementations and numerical adapters are under `internal/`. |
| `models/` | Four constructive models, described below. |
| `Documentation/` | Full Mathematica Help bundle and its maintenance instructions. |
| `tests/` | The two construction notebooks and the test runner. |
| `tests/checks/` | Complete regression-test bundle, a guide and one notebook helper. |
| `tests/benchmarks/` | Timing and publication-evidence bundle with reproduction guides. |
| `tests/reference-models/` | Matched CalcHEP and MadGraph Feynman-rule models, with upstream attribution. |

Model 1 is the constructive SM. Model 2 contains elementary QED interactions.
Model 3 adds known QED amplitudes with physical poles; model 4 contains their
reference-based pole forms. Models 3 and 4 provide complete lower-point input
for higher-multiplicity factorization. Their known expressions remain in the
model tables, separate from the general construction algorithms.

This publication branch excludes old notebooks, migration records and research
scratch files. They remain on the
[development branch](https://github.com/neilthec/hepcat/tree/factorization).
The optional bundles keep the initial checkout small; unpacking makes their
complete source, tests and inputs inspectable. Expanded files are local and
ignored by Git.

## Run the checks

From this directory:

```sh
python3 tests/run-checks.py
```

This safely unpacks the included tools, runs the Python support tests and then
runs the standard Wolfram suites in independent kernels. The Wolfram timeout
is 900 seconds per suite. Use `--kernel /path/to/WolframKernel` for another
installation, `--python-only` or `--wolfram-only` for a focused run, and `--list`
to inspect the suites. Expensive full constructions and external integrations
are selected explicitly with repeated `--suite` arguments. See the
[check guide](tests/checks/README.md) for scope and dependencies.

To unpack the tools without starting any calculations:

```sh
python3 tests/run-checks.py --unpack-only
```

The original directory structure is restored for direct use of the individual
runners. Existing identical files are retained; a changed local file is
reported rather than overwritten.

## Reproduce the paper results

After unpacking, follow the
[benchmark guide](tests/benchmarks/README.md) for fresh timing runs and plotting.
The [evidence replay guide](tests/benchmarks/reproduction/README.md) explains
how to regenerate the catalog and precision statistics from the included saved
records. These saved-data checks need only Python's standard library. Plots
use the optional Matplotlib dependency described in the guide.

Saved-data verification, a new numerical run and a new symbolic construction
are distinct operations. The guides state what each command establishes.
Reference-model inputs, widths, crossing and normalization must match for
independent comparisons.

For AI-assisted derivations or code changes, have your AI read
[AGENTS.md](AGENTS.md). Documentation maintenance is described in
[Documentation/README.md](Documentation/README.md).

## License

Copyright (C) 2026 Neil Christensen.

HEPCAT is distributed under the GNU General Public License, version 3 or
any later version. The full text in [LICENSE](LICENSE) is copied from
[GNU's official GPL v3 text](https://www.gnu.org/licenses/gpl-3.0.txt).
The source notices use the template in
[GNU's licensing instructions](https://www.gnu.org/licenses/gpl-howto.html).
Imported reference models retain their upstream notices and terms.
