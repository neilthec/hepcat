# HEPCAT Regression Tests

Run the complete Wolfram suite from the repository root:

```sh
python3 tests/checks/run-wolfram-tests.py --timeout 900
```

Each suite runs in a fresh kernel. The timeout is a hard per-suite limit;
the runner terminates the kernel process group, including descendants, on a
timeout or interruption. A syntax-validation wrapper checks the entire source
before any test executes, and the runner requires an explicit completion
marker as well as a zero exit status. Wolfram's zero exit status alone is not
sufficient. It does not use the server or the user's notebook kernel.
Individual assertions in the legacy suites also have short limits.
An end-of-suite count guards against a recursion error silently skipping a
test loop.

Run only the suite relevant to a change by selecting it explicitly:

```sh
python3 tests/checks/run-wolfram-tests.py --suite legacy-algebra-tests.wl
```

Repeat `--suite` to select several. `--kernel /absolute/path/to/WolframKernel`
selects a different licensed kernel. Run the Python runner's own tests with:

```sh
python3 -m unittest discover -s tests/checks -p test_runner.py -v
```

These include a live descendant-process timeout test. Three syntax-wrapper
integration tests also run when a local Wolfram kernel is available; otherwise
those three are explicitly skipped.

## Original Functionality

These suites exercise the original tools directly, not through the newer
identity-candidate or physical-residue checker implementations.

| Suite | Coverage |
| --- | --- |
| `legacy-package-tests.wl` | Public definitions and protection, loading and reloading, rejection of unavailable source files without erasing working definitions, context restoration, preservation of unrelated user data, and the five repaired legacy-bug probes. Interface-presence tests are not claims of mathematical coverage. |
| `legacy-algebra-tests.wl` | Massive and massless spin contractions, explicit upper/lower indices, powers and spectator factors, epsilon tensors, ordinary and hatted momenta, external Dirac identities, Schouten identities, composite momenta, traces, scalar products, Mandelstam conversion, Zeta identities, legacy simplifier options, x-factor sewing, termination and idempotence. |
| `legacy-transform-tests.wl` | Crossing and permutations, spin-index selection and symmetrization through spin 2, conjugation, squared amplitudes, explicit spinors and momentum matrices, helicity/Zeta conversion, complex shifts, SpinorZ, amplitude-string parsing, display/TeX/plain-text/SPINAS output, SPINAS declarations and updates, and SU(3) color algebra. |
| `legacy-model-tests.wl` | The actual on-disk model readers, explicit failures for missing or unterminated files, particle/antiparticle masses, amplitude notation conversion, opposite helicities, partitions through eight particles, amplitude lookup and typed relabeling, spin-two sewing, original three-/four-point diagram APIs, compact factorization output, diagram-local `SimplifyFactorizationChannels` reductions with propagators kept off shell, and shifted-momentum/residue helpers. Temporary reader fixtures are removed after testing. |

Expected results use explicit identities, matrices, strings and small
hand-computable models. Fifty algebra checks evaluate 25 multi-chain Schouten
cases at two deterministic sets of exact two-component spinors and consistent
massless momentum matrices. Other checks test composition, invariance under
relabeling, or idempotence; those supplement the fixed-result checks rather
than replacing them. No test requires internal on-shell conditions inside
`ReduceSpinorProducts`.

Focused spinor-identity tests also cover reference-spinor Schouten cycles:
an auxiliary Xi identifier may coincide with a physical momentum label without
being its physical spinor. Weighted products, powers, reciprocal factors and
distinct Xi identifiers must terminate and preserve independent exact complex
two-component values. The physical-endpoint orientations that expose ordinary
Dirac reductions remain enabled. Existing-kernel reload tests recreate the
five obsolete product definitions and verify their targeted removal, repeated
reload stability, and preservation of unrelated user definitions on `Times`.

Automatic-reduction regressions also compare long and indexed chains,
multilinear momentum slots and chiral traces with independent exact Weyl
matrices at unconserved momenta. Preparation tests separately cover consistent
whole-numerator conservation, nonconsecutive external labels, strict improving
steps, disabled conservation and protected reciprocal factors.
Preparation also tests ordinary whole-expression reduction of reciprocal
spinor chains and bracket factors, independently of numerator-only internal
on-shell substitutions.
`PrepareAmplitudes` accepts both diagram pairs and compact factorization rows.
Its channel tests check preserved row metadata and propagators, numerator-only
selected-cut shell conditions, and protection of unrelated internal channels.
Completion can retain originals with the opt-in `"AlternativeChannels"` fallback,
certified on the selected cuts and considered only after the prepared-first fit.
Exact coefficient-refinement tests include heavily scaled rows, algebraic
right-hand sides, retained free coefficients and late contradictions.

The original ten suites remain enabled. They cover the newer generalized
identity API, diagram generation and helicity sewing, curated Standard Model
paths, worked five-point QED derivations, physical/spurious poles, high-energy
behavior, reference independence, amplitude construction and model parameters.

## Amplitude Text and Model Export

`plain-text-tests.wl` checks readable display output and the separate
`PlainText[expression, "Model"]` serialization mode. It covers exact powers,
complex coefficients, compact brackets, general momentum chains, labels above
9, composite momenta, physical propagators and explicit massive-spin indices.
Model syntax rejects unsupported objects and unsafe row delimiters.

`model-export-tests.wl` checks `AppendModelAmplitudes`: safe model-local
parameters, complete batch validation, reader round trips, duplicate conflicts,
idempotence, original row/newline preservation and atomic higher-multiplicity
appends. Parser regressions keep legacy bracket notation separate from typed
function arguments and reject malformed quotation marks.

`qed-model-export-tests.wl` checks all six saved nonzero five-point amplitudes
in the electron/photon QED model. It exercises all eight ordered three-photon
helicities and both four-fermion radiation helicities, independent Feynman
amplitudes with all massive-spin components, other fermion orderings and
nonunit masses/couplings. Six-point channel inventories verify that the newly
saved amplitudes are actually available for three-by-five sewing. The cubic
diagram model is unchanged.

## Model Isolation

`model-isolation-tests.wl` exercises both model readers with same-named
parameters and different values, preserved user variables, local dependency
resolution and overrides, stable parameter identities and serialization.
Snapshots of conjugation and export definitions verify that reading models
installs no per-parameter global rules. Real-domain metadata is explicit;
undeclared-complex parameters keep their conjugation. Independent benchmark
models supply their own numerical definitions, so different couplings are
reported as disagreement rather than silently identified by name.

After updating from the older readers, restart an interactive kernel once to
clear the per-parameter metadata those readers previously installed. The new
readers store parameter data in each model and do not install such definitions.
The regression runner already starts each suite in a fresh kernel.

## Feynman Benchmark

`feynman-tests.wl` covers the independent CalcHEP backend in
`source/HEPCAT-Feynman.wl`. It imports all 139 local SM rules, including the
fourteen added light-fermion Higgs/Goldstone rules, checks safe
ordered-gamma parsing and ordinary amplitude-pair output, and verifies the
unitary-gauge longitudinal numerator and exclusion of Goldstone/ghost exchange.
The four-gluon auxiliary represents a contact interaction, never a pole.

Independent four-component matrices check Dirac words, Fierz contractions,
traces and full QED diagram numerators without internal on-shell assumptions.
Separate external-shell fixtures calibrate both photon phases, the symmetrized
hZZ vertex and massive-vector normalization, transversality and completeness.
Whole-amplitude QED and nonabelian Ward checks exercise the vertex, propagator
and momentum conventions together. Malformed tensor slots and unsupported
structures must return explicit failures.

This is not a complete numerical benchmark grid for every SM process. The local
SM restores electron, muon, up and down masses together with their Higgs and
Goldstone couplings. Tests compare the four massive Higgs vertices with the
constructive model and check massive-electron photon Ward identities.
Running-quark-mass functions for other flavors are imported symbolically.

`CheckAmplitudes` is the canonical combined checker; `CheckPhysicalResidues`
remains a compatibility name. The opt-in `"FeynmanAmplitudes"` comparison uses
independently generated spinor amplitudes at external-shell points away from
all poles, without fitting a sign or scale. `PrepareAmplitudes` converts raw
Feynman diagram pairs automatically and does not apply their internal-shell
mass rules. The construction notebook shows both the Feynman calculation and
the comparison for the all-plus and mixed-helicity five-point QED examples,
and the identical-four-fermion single-photon process. Rational complex off-cut
samples avoid costly radicals with four massive external fermions; tests verify
every coherent-spin coefficient, external shells, momentum conservation,
nonzero physical propagators and rejection of an incorrect relative pairing
sign. A process without an eligible cut retains real center-of-mass sampling.

## Factorization Completion

The completion suite includes the notebook's actual five-point QED
`AllSpurious` diagram preparation followed by automatic completion, without
supplying a known answer as a correction basis. It also completes the
four-fermion/photon process, including recorded preparation references and an
independent full Feynman comparison. It checks shared-chart residue evaluation
without skipping component regularity, selected-cut
certification of alternative sewn-channel reference choices, the final
physical and spurious checks, bounded validation feedback on saved charts,
fresh subsequent validation seeds, and preservation of genuine residue-free
directions. Additional unit checks cover provably zero algebraic directions
and invalid `ChannelReferences` or `MaxRefinements` options. Same-cut,
same-mass contributions must be summed across exchanged species. New
continuations with uncanceled higher-order physical propagator powers are
recorded as unsupported by the simple-pole fitter, without dropping raw
sources or declaring such powers forbidden by the theory.
The opt-in `RefinementHistory` and `ChannelContinuations` reports make these
steps inspectable. Passing sampled constraints is not a generic uniqueness
theorem; correction-space and time limits must still report unresolved work.

`six-point-completion-tests.wl` covers the reference-free four-by-four sewn
channels that first occur in six-point electron/photon QED. They must enter
the compact correction space even though they need no reference replacement.
The suite also checks generic scalar channels, correction-term limits,
higher-order-pole rejection, selected-channel-only numerator reductions,
additional cyclic reference choices and concise failure reasons. The physical-residue
suite separately verifies that skipping an optional formal coefficient leaves
the mapped residue and kinematic validation unchanged.
Focused completion tests also cover opt-in Bose projection, the compact/literal
union, selected-cut atomic sources and exact formal-dependence pruning without changing the available span.
The bounded pruning pass has a 60-second allowance; a timeout keeps every
direction. The final correction cap applies after projection and pruning.
When the full automatic Bose space fits, its complete span is retained. If it
does not, an explicitly named grouped-input alternative keeps whole input
diagrams and all available channel atoms. Both exact sizes and limit decisions
are recorded, never silently truncated. Small scalar regressions verify the
alternative at two mass scales and reject it when it still exceeds the cap.
Cheap mixed-helicity tests check the fourteen-channel inventory and the
six-element permutation group of three identical positive-helicity photons,
leaving the negative-helicity photon fixed. Equal masses never infer a
helicity grouping. The default without declared bosonic groups is unchanged.
Automatic-chart simple-pole evaluation is compared with full mapped residues.
Automatic affine components are extracted in one pass without changing their
exact algebra, variable order or explicit zero directions. Opaque custom maps
retain the legacy coefficient forms, and nonlinear inputs keep the conservative
extraction path.
Hidden inverse-bracket and invariant poles, higher-order cancellations, implicit
x-factor context, zero-coefficient singular directions and custom maps exercise
the conservative fallback paths rather than bypassing regularity checks.
Large exact linear systems test the certified modular-block solver against
every original row and every free direction, with rational denominators,
redundant rectangular systems, many right-hand sides, algebraic scaling,
unlucky primes, inconsistent rows and conservative native fallback.
Exact block contradictions require a nonzero constant residual with every
free coefficient zero. Rank-preserving but nonleading modular pivot profiles
still fall back to the native exact solver rather than assert inconsistency.
Optional cleanup must not invalidate a validated result, and dense affine families
must not be expanded before sparse validation.

The full six-point diagram-to-completion regression is deliberately opt-in.
It generates the twenty-four electron/four-positive-photon diagrams, performs
ordinary `AllSpurious` preparation, and derives corrections from the model's
fourteen channels without a supplied final amplitude or correction basis.
Completion must pass every held-out channel at both the model and doubled mass
values, including the remaining affine family. Allow time for preparation in
addition to the bounded 1200-second completion stage:

```sh
HEPCAT_TEST_FULL_SIX_POINT=1 python3 tests/checks/run-wolfram-tests.py \
  --suite six-point-completion-tests.wl --timeout 1800
```

## Repaired Legacy Bugs

The five formerly expected-failure probes now run as ordinary regression
tests. A recurrence fails the normal run, including when no optional flags
are supplied. Focused suites also cover adjacent powers, repeated occurrences,
spin assignments and rational-expression layouts:

1. `ReduceSpinContractions` preserves the reversal sign for powered left-left
   massless completeness patterns. Its result must equal the contraction of
   one copy times the remaining spectator factor.
2. `ReverseMomentum` reverses every requested occurrence within a chain
   before restoring its temporary markers. Separate probes cover two angle
   endpoints and two repeated momentum insertions; both require two sign changes.
3. `SymmetrizeSpin` averages the distinct assignments returned by
   `Permutations`, retaining the multiplicities of coinciding spin labels.
   Symmetrizing distinct formal indices and then making
   them equal must agree with supplying the equal indices directly.
4. `SumResiduesAlt` cancels removable rational factors before substituting
   either simple pole. Partial fractions, common-denominator and expanded
   forms give the same result, including regular spectator terms.

Every failed or unevaluated assertion, unexpected message, or timeout fails
its test. There are no expected-failure exemptions for these repaired bugs.
The runner still accepts `--strict-known-issues` for command-line compatibility;
it is no longer needed for these probes.

The subsequent code audit added regressions for:

- Typed particle permutations and diagram-vertex attachment, preserving numeric
  little-group indices and auxiliary reference labels while relabeling momenta,
  masses and composite particles.
- Crossing traces, composite momenta, weighted propagator sums and indexed
  spinors, including agreement between crossing before and after reduction.
- Interlaced repeated-momentum chains, using a closed contraction that shortens
  every resulting word instead of permitting anticommutation cycles. Exact
  independent Weyl matrices check the signs without momentum conservation.
- The fourth spin index in spin-two diagram construction and amplitude lookup.
- Failed package-load preflight and missing or unterminated model files, with
  working definitions and open-stream cleanup checked explicitly.
- Contradictory diagram mass declarations and misspelled public checker options.
  Equivalent repeated mass rules remain valid; assumptions belonging to separate
  diagrams are never merged.
- Internal-shell scope inside symbolic powers: an unknown-sign exponent might
  represent a denominator, so its base is protected from internal-shell edits.
  Ordinary external on-shell spinor reduction remains available there.

## Remaining Limits

This is broad representative coverage, not exhaustive testing of every
pattern, spin configuration or historical notebook calculation. In particular:

- The tests do not reevaluate or modify the original research notebooks, or
  supply a complete numerical reference grid for every Standard Model process.
- The complex-shift and spin-to-helicity tests cover representative branches,
  not every multi-shift expression or every vector-helicity conversion.
- Export checks validate symbolic display objects and generated strings, not
  front-end appearance, a rendered PDF or a compiled SPINAS program. Diagram
  drawing functions have interface checks, not visual rendering checks.
- Reader tests cover missing files, missing final separators and blank records,
  but do not exhaustively validate malformed Wolfram expressions or every
  historical model-file layout. Loader preflight checks source-file availability,
  not transactional recovery from arbitrary source syntax or evaluation errors.
- Signed symbolic scalar-product arguments are covered, but negative integer
  momentum labels are not currently reduced consistently by the old dot-product
  rules. This remains a documented input-handling gap.
- Several names in the loader are declarations without implementations in the
  current source, including `ReadVertices`, `ConvertVertex`, `ReduceXFactorsP`,
  `ReduceXFactorsM`, `rSpinor` and the older random-amplitude/genetic helpers.
  `lSPINAS` definitions are commented out. These are not advertised as working
  capabilities or protected by tests that bless an unevaluated result.

When adding an identity, include a hand-computable example, nontrivial spectator
and power cases, appropriate external mass assumptions, a relabeling, and an
idempotence or bounded-termination case. Independent exact matrix checks are
especially valuable for signs. Preserve off-shell propagators and keep internal
numerator shell assumptions explicit and diagram-local.
