# Tested environment and reproduction

This records the publication-validation environment on 11 November 2026.
It describes a tested configuration, not a requirement to use the same computer.
Set installation paths in notebook Setup or the documented command options;
no author-specific home directory is required.

## Software and hardware

| Component | Tested version or configuration |
| --- | --- |
| Computer | Apple M3, 8 physical/logical CPU cores, 16 GiB memory; Mac15,13 |
| Operating system | macOS 26.6.2, build 25G83; Darwin 25.6.0; native ARM64 |
| Wolfram | 15.0.0 for Mac OS X ARM (64-bit), release dated 19 May 2026; `MacOSX-ARM64` |
| C/C++ | Apple Clang 21.0.0, build `clang-2100.3.34.2`; macOS SDK 27.0 |
| Fortran | GNU Fortran 16.1.0, Homebrew GCC; target `aarch64-apple-darwin25` |
| Build tools | CMake 3.31.0; GNU Make 3.81 |
| Python | 3.9.6; ordinary runner and saved-data analysis use its standard library |
| Plotting | Matplotlib 3.9.4; NumPy 2.0.2 |
| Optional exact-proof headers | Boost 1.86.0, header-only Multiprecision `cpp_int`; not a SPINAS dependency |
| SPINAS | Git revision `c8c69ff330d0845f1e5ea1116d049093683e92bf`, available on upstream `factorization` |
| Independent references | CalcHEP 3.9.2; MadGraph5_aMC@NLO 3.8.1, version file dated 9 October 2026 |

Use the exact SPINAS revision above for this numerical replay. It is a later
revision than the separately released `v1.0.3`; that release tag alone does not
identify the library tested here. For a separate SPINAS clone, select it with:

```sh
git -C /path/to/spinas fetch origin factorization
git -C /path/to/spinas checkout --detach c8c69ff330d0845f1e5ea1116d049093683e92bf
```

Use a separate clone if your SPINAS checkout contains work in progress.
The numerical runners make private build copies and leave installed sources
and libraries unchanged. SPINAS itself has no Boost dependency. HEPCAT's
optional native exact Laurent-proof backend additionally uses the header-only
Boost.Multiprecision `cpp_int` library and a C++17 compiler; its runtime records
the discovered header and compiler hashes. It does not link a Boost library.
For headers outside the compiler's search path, set `BOOST_ROOT=/path/to/boost`
(the directory containing `include/boost`) before starting the kernel or runner.
The tested kernels inherited that variable and the C++ include path. In a GUI-started kernel, `SetEnvironment["BOOST_ROOT" -> "/path/to/boost"]` supplies the same location for subsequently launched proof helpers.
Without that optional backend, an unavailable proof must remain unproved.

The publication validation started at HEPCAT commit
`749a4c9d6becb727cf1a46dbe053ddf9d0ac3df4`. Runtime repairs made during
validation are identified by the source hashes below; the publication freeze
commit will identify their committed version. The detailed environment report
retains hashes for all package sources, model inputs and reference
models, rather than relying on a branch name that can later change.

- `source/HEPCAT-Base.wl`: SHA256 `21e53c34902eb67ed144bf407d68aa43ebde806608834d5fb91dffa6b338ebf6`.
- `source/HEPCAT-Model.wl`: SHA256 `80f719c833510f002028316eb74bf25a2e4aa3d8deac07e1c4bfbb52a67d1ade`.

## Builds and scalar precision

The private SPINAS component-test build used CMake `Release`,
`PRECISION_TYPE=LONG_DOUBLE`, and `-O3 -DNDEBUG`. Both `test_spinas` and `test_SM`
passed. Native numerical literature comparisons used an isolated
`PRECISION_TYPE=DOUBLE` library and C++11 `-O2` drivers. The full precision
replay used private scalar-type builds with C++17, `-O2 -ffp-contract=off`.
The Intel-compatible replay adds `-arch x86_64` and runs through Rosetta on
this Apple Silicon machine. These flags describe different experiments;
they are not a single universal optimization setting.

| Type | Native ARM64 size / significand bits | Intel-compatible size / significand bits |
| --- | --- | --- |
| `float` | 4 bytes / 24 bits | 4 bytes / 24 bits |
| `double` | 8 bytes / 53 bits | 8 bytes / 53 bits |
| `long double` | 8 bytes / 53 bits | 16 bytes / 64 bits |

The replay measures these widths and rejects a purported extended build with
only 53 bits. Float is an isolated precision experiment, not a production
export setting. Do not mix SPINAS headers and libraries built for different
scalar types or architectures: both are part of the C++ ABI. The tested
MadGraph references use the recorded Fortran compiler; high-multiplicity
reference generation can use split source files and lower optimization to
bound compile time, as recorded in each generated reference manifest.

## Physics configuration

- Use the bundled constructive and matched Feynman reference models. The SM
  notebook uses constructive model 1 and the supplied massive-first-generation
  SM Feynman models, including their Higgs couplings. Its MadGraph parameter
  rule `"lamWS" -> 0` disables CKM flavor mixing to match the constructive model.
- QED independent references restrict particle content to electrons and
  photons, absolute PDGs `{11,22}`. Models 3 and 4 hold the physical- and
  reference-based-pole lower-point catalogs. The literature formula tests use
  their separately recorded masses, charges, seeds and fixed helicities.
- Bare tree comparisons disable **all widths**, including dependent width
  parameters, in private reference-model copies. A nonzero width is not part
  of these agreement claims.
- HEPCAT formulas use all momenta incoming. Numerical exports cross the
  explicitly outgoing particles exactly once, including spinor crossing
  signs. Reference processes use the corresponding physical incoming and
  outgoing states. Individual complex amplitudes may differ by an overall
  convention phase; validation compares the documented squared quantities
  or a fixed, declared phase convention.
- Colored processes retain coherent color structures, their interference,
  and the appropriate incoming color averages. Spin/helicity sums, initial
  averages and identical-final-particle symmetry factors must match. The standalone literature-formula harness uses raw spin/color sums without
  initial averages or a final symmetry factor. The notebook public-API
  all-positive formula checks instead average the four incoming massive-spin
  states by `1/4`, with all photons fixed to positive helicity and no final
  `1/N!`. Keep the normalization attached to each report.
- The precision replay preserves the documented input parsing policy. The
  high-multiplicity references are archived binary64 MG rates. Restoring
  external mass shells and momentum conservation is a separate experiment;
  its adjusted momenta are not compared to old MG values as if unchanged.

## What has been checked

These scopes must be kept separate. A saved-data replay is not a fresh
symbolic construction or a fresh independent-reference calculation.

| Validation | Status at this environment snapshot |
| --- | --- |
| Python component regressions | 207 passed, including the reader and evaluation-exhaustion regressions |
| Private SPINAS component and SM tests | 2 of 2 passed |
| Fresh published-formula numerical comparisons | 5,000 all-positive and 12,000 one-negative points passed, including complex components and negative controls |
| Fresh formula/MG comparisons with new private references | 5,200 one-negative and 6,000 two-pair all-positive points passed, with the original counts and seeds |
| Exact supplementary symbolic comparisons | 32 one-negative and 258 two-pair tests passed; the literature comparison passed 576 shell/conservation/component checks and two negative controls |
| Fresh precision replay | All 20 Intel-compatible groups completed at 24, 53 and 64 bits, plus the native rounding control; all 72 printed precision values reproduced at their stated rounding |
| Fresh notebook formula/MG checks | 100,000 points each at seven and eight positive-helicity photons, in both pole forms; three pass at unchanged relative tolerance `1e-7`, while the native eight-photon spurious form has 16 discrepancies, maximum `4.93e-6`, and no evaluation errors |
| Larger-sample diagnostic replay | All 16 original failures recovered in a 1,600-point subset; unchanged binary64 inputs promoted to true 64-bit-significand arithmetic reduce 15 below tolerance, with one remaining at `1.59e-7`. A separate changed-input, formula-to-formula control is not a replacement MG validation |
| Saved catalog, tables and timing data | Catalog transcription checks, all 72 precision entries, and 36 printed timing entries from 24 unique medians verified |
| Focused completion-input regressions | 102 passed, including 31 conservative timeout-fallback safeguards |
| Focused four-leg preparation regressions | 50 passed, including final-only scheduling, exact-identity safeguards, rational-proof normalization, and bounded fallback; this is not an end-to-end construction pass |
| Fresh public SM vector integration | 19 passed, including both automatic constructors, unchanged compactness bounds and final checks at three parameter assignments |
| Full 117-suite default and 15-suite integration campaigns | In progress; a complete release pass has not yet been established |
| Actual QED and SM notebook campaigns | In progress, with unresolved construction and simplification regressions under investigation; archived passes are not substituted for fresh results |

The repaired fixed-seed public vector integration passes all 19 checks.
`Z,Z,W-,W+` completes with complexity 53, below the maintained bound of 63;
`W+,W-,W-,W+` completes with complexity 213, below the maintained bound of 216.
Both selected representatives pass all mandatory final checks and all three
independent parameter assignments, with physical poles and unchanged models.
The complete fresh notebook campaigns remain pending. These two end-to-end
results are not a full release pass.

The deliberately low-precision float discrepancies are measured
conditioning results, not failed high-precision agreement tests. One newly
compiled two-pair MG comparison has a maximum discrepancy of `2.86e-11`
rather than the historical `2.72e-11`; both pass the declared tolerance.
Historical tables remain the results of their documented original builds.

## Run it on another machine

From the HEPCAT checkout root:

```sh
python3 tests/run-checks.py
python3 tests/run-checks.py --list
python3 tests/run-checks.py --unpack-only
```

To include the compiled adapter tests after moving the checkout, give the
SPINAS location explicitly:

```sh
HEPCAT_SPINAS_DIRECTORY=/path/to/spinas python3 tests/run-checks.py --python-only
```

This clean-checkout command ran all 207 Python checks without skips in the
tested configuration. Without SPINAS, its optional adapter class is skipped;
that is not evidence that those native checks passed.

Use `--kernel /path/to/WolframKernel` if discovery does not find your licensed
kernel. Full construction/integration suites are selected explicitly as
explained in [the check guide](checks/README.md). Evaluate Setup and the
calculations in `QED-6-point-construction.nb` and
`SM-4-point-construction.nb` for the notebook workflows.

For saved evidence, fresh timing runs and the three-precision replay, follow
[the benchmark guide](benchmarks/README.md) and
[the evidence replay guide](benchmarks/reproduction/README.md). Plotting is
optional; the statistics checks need no plotting packages. Timing comparisons
must run serially on a quiet machine. The papers' nine-sweep timing medians
are historical measurements, not wall times from the concurrent validation
campaign, and different machines need not reproduce the same seconds.

Generated exports, private reference models, build caches, logs and reports
belong under `tests/results/` and are ignored by Git. This validation's detailed
records are under `tests/results/publication-validation/`: `environment/`
records platform, tool versions and input hashes; `paper-audit/` records
commands, seeds, pointwise outputs, source/library SHA256 values and the paper
claim comparisons. These files are local audit evidence, not required files
in a fresh checkout. The maintained reproduction inputs are in the bundled
benchmark archive, and its extraction is checksum-verified.
