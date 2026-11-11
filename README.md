# HEPCAT

High Energy Physics Constructive Amplitude Tool

HEPCAT constructs, simplifies and checks scattering amplitudes in Wolfram
Language. It also exports numerical SPINAS processes and compares them with
independent CalcHEP and MadGraph calculations.

## Load the package

With the Wolfram working directory set to the HEPCAT checkout root, evaluate:

```wolfram
Get[FileNameJoin[{"source", "HEPCAT.wl"}]]
```

Search Mathematica Help for **HEPCAT** for the package guide, public function
reference pages and examples. `PacletInfo.wl` describes the Help package.

## Directory guide

| Directory | Contents |
| --- | --- |
| `source/` | Loader, spinor algebra and model/construction implementation. Additional modules, numerical adapters and exact-proof helpers are grouped under `internal/`. |
| `models/` | Constructive SM model 1, elementary QED model 2, and QED factorization models 3 and 4 with physical and spurious pole representations. |
| `Documentation/` | Native Mathematica Help, its reviewed source and build instructions. |
| `tests/` | The final QED and SM construction notebooks. |
| `tests/checks/` | Automated function and integration tests, with known expressions and model variants under `fixtures/`. |
| `tests/benchmarks/` | Timing studies, general-formula tests and comparisons with published expressions. |
| `tests/reference-models/` | Matched CalcHEP and MadGraph Feynman-rule models used for independent validation. |
| `tests/results/` | Local generated numerical projects and build output. |
| `tests/history/` | Earlier notebooks and manual calculations retained for development. |
| `tests/development/` | Detailed development notes, migration records and preserved earlier inputs. |

The local `papers/` directory holds development references. It is excluded by
Git. Experimental models 5 and 6 are retained in
`tests/checks/fixtures/models/` for development and regression tests.

## Run the automated checks

From this directory:

```sh
python3 tests/checks/run-wolfram-tests.py --timeout 900
python3 -m unittest discover -s tests/checks -p 'test_*.py' -v
```

The first command runs the standard suites in independent Wolfram kernels.
The timeout applies separately to each suite. Use `--kernel /path/to/WolframKernel`
when the executable is elsewhere. Additional full constructions and external
program integrations are selected explicitly with `--suite`; see the
[check guide](tests/checks/README.md). The Python tests exercise the support
runners and adapters.

For numerical timing and formula comparisons, start with the
[benchmark guide](tests/benchmarks/README.md). External integrations use the
reader's SPINAS, CalcHEP and MadGraph installations and explicitly selected
models. Each reference model's README records its inputs and conventions.

For AI-assisted derivations or code changes, have your AI read
[AGENTS.md](AGENTS.md). Documentation build instructions are in
[Documentation/README.md](Documentation/README.md).

## License

Copyright (C) 2026 Neil Christensen.

HEPCAT is distributed under the GNU General Public License, version 3 or
any later version. The full text in [LICENSE](LICENSE) is copied from
[GNU's official GPL v3 text](https://www.gnu.org/licenses/gpl-3.0.txt).
The source notices use the template in
[GNU's licensing instructions](https://www.gnu.org/licenses/gpl-howto.html).
Imported reference models retain their upstream notices and terms.
