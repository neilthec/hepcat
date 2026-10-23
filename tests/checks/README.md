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

## Feynman Benchmark

`feynman-tests.wl` covers the independent CalcHEP backend in
`source/HEPCAT-Feynman.wl`. It imports all 125 stock SM rules, checks safe
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

This is not a complete numerical benchmark grid for every SM process. The raw
model has zero electron, muon, up and down masses. Restoring those masses needs
their Yukawa sector as well; the tests do not bless changing masses alone in
a full SM model. Running-quark-mass functions are imported symbolically.

## Factorization Completion

The completion suite includes the notebook's actual five-point QED
`AllSpurious` diagram preparation followed by automatic completion, without
supplying a known answer as a correction basis. It checks selected-cut
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
