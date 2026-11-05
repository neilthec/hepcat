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

Ordinary HEPCAT checks and generated C++ validation use time-based seeds by
default. Regression cases that depend on replay or a particular kinematic
fixture supply an explicit seed. Automatic-seed tests check that the resolved
seed is recorded once, can be reused to replay the samples, and does not alter
the caller's Wolfram random state.
`random-seed-tests.wl` covers all public checking and construction defaults,
parallel replay, nested stage overrides and the recorded seed in final output.

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

`final-amplitude-simplification-tests.wl` covers bounded, factor-preserving
reductions of completed amplitudes. A self-contained twelve-term QED six-point
fixture checks exact rational regrouping, unchanged physical poles and scoped
parameters, typed relabeling, explicit spin indices and idempotence. Independent
two-component determinants check ordinary Schouten identities, and large
factored products verify that expansion guards retain endpoint Dirac reductions
without distributing exponentially many terms. Completed amplitude preparation
uses external masses only; an existing propagator never supplies an internal
numerator shell rule. Exact virtuality cancellation and Lorentz scalar squares
are tested separately from numerator on-shell conditions. The saved mixed
five-point representative is checked on two rational off-cut charts and all
four coherent massive-spin components; no saved notebook or temporary cache
is loaded by the suite.

`numerical-spinor-stability-tests.wl` covers declared external on-shell masses
in explicit energy-angle evaluation, including symbolic construction followed
by numerical replacement. Highly boosted small spinor components, momentum
matrix diagonals and scalar products are checked alongside rest, massless,
negative-energy, complex and off-shell controls. Exact chart formulas remain
unchanged; inexact real direction bases avoid near-axis cancellation.

`high-energy-power-counting-tests.wl` covers conservative all-spin upper bounds,
complete massive basis scaling, generic propagator invariants, strict numeric
coefficients, and the exact Laurent fallback for hidden denominators, references
and genuine bad growth. Bound-only samples never invent observed leading powers;
unresolved directions prevent an overall bound claim. No internal shell is used.

`automatic-resource-tests.wl` also checks in-place admission of a complete
exchange correction union: construction occurs once, every column is retained,
the automatic ceiling and original clock remain binding, and explicit caps or
supplied bases never acquire implicit permission to grow.
The later literal fallback also uses the construction allowance implied by the
admitted effective cap, rather than rejecting valid columns against a stale
pre-admission allowance.

`feynman-coefficient-comparison-tests.wl` compares the scalar-coefficient route
with the former whole-polynomial route on small cases. It covers every monomial
of 64- and 729-component tensors, opposite signs, tiny exact mixed coefficients
below the numerical tolerance, removable spin denominators and genuinely
unsupported spin-dependent denominators. No coefficient or sample is dropped.
Its recorded six-point seed regression rejects an accidental spectator pole,
then validates all 27 original propagator obligations and complete external
kinematics on a fresh rational geometry. Incorrect amplitudes and damaged spin
bases still fail; sampling is never selected by comparison agreement.

`completion-structured-source-tests.wl` covers source-derived correction blocks,
conservative zero residues of structurally regular columns, and the original
fallbacks. A zero shortcut must preserve every matrix coordinate and verify all
other inverse factors on the actual cut chart. Unsupported inverses, coincident
poles and custom maps use the full evaluator.

`ordinary-identity-normalization-tests.wl` covers bounded sparse Schouten and
external-Dirac normalization. Numerical optimization proposes coordinates only;
accepted candidates require an exact ordinary-identity replay and a strict rank
improvement. Preparation supplies momentum-conservation relations from its
external leg set. Standalone reduction receives no implicit conservation rule.

`dirac-schouten-reduction-tests.wl` checks bounded endpoint sum contractions
using ordinary external identities. Its controls cover both bracket chiralities,
unequal masses, explicit spin-index variance, literal positive powers and
protected inverse factors. Conservation-dependent preparation is tested
separately from the process-independent reducer and requires complete external
metadata. The opt-in `five-point-construction-integration-tests.wl` constructs
all six independent five-point QED helicity sectors through the notebook API,
requiring full physical checks, independent Feynman agreement and matched
complexity ceilings for opposite helicities. The four-fermion defaults retain
the seeds from the saved positive result and negative timeout. Set
`HEPCAT_FIVE_POINT_INTEGRATION_CASES=FourFermionP,FourFermionM` to run just
that pair. `HEPCAT_FIVE_POINT_INTEGRATION_SEEDS` accepts up to eight comma-separated
integer seeds for a reproducible sweep. Result-size ceilings also guard the
compact paired-current grouping, so equally complicated outputs cannot pass
merely because their helicities match.

`five-point-identity-proof-tests.wl` checks both the ordinary dense proof
portfolio and the exact single-null native route, including the failed-seed
family, relabeling, full coherent spins, independent parameter scopes,
reciprocal radicals, fraction-free reconstruction and nonzero denominator
witnesses. A deliberately nonzero tensor must remain unproved. The native
backend is optional; an unavailable tool must retain the original direction.

`shared-matrix-cofactor-tests.wl` verifies the general two-by-two shared-matrix
diagonal identity, both chiral word orientations, explicit spin variance,
literal spectator witnesses, virtuality preservation and bounded whole-amplitude
cofactor normalization. Each accepted complete checkpoint must improve the
existing rank; diagram/channel degree-partition sources remain unchanged.

`native-laurent-identity-tests.wl` covers the late serial generic identity
certificate: dense external rest/null coordinates, typed mass/coupling
normalization, reversible algebra export, full coherent coefficient coverage,
nonzero denominator domain and runtime/root/hash/deadline checks. The optional
hard fixture runs with `HEPCAT_TEST_NATIVE_FULL_DIRECTION=1`. The Python suite
`test_laurent_proof_runtime.py` independently tests exact Laurent arithmetic,
factor witnesses, tool availability, all declared roots, caps and process cleanup.
Missing optional native tools never turn an unresolved direction into a proof.
`native-rest-null-dual-tests.wl` separately verifies both angle- and square-axis
charts, their exact external identities, and the typed expression-based selector.

`numerator-spectator-tests.wl` covers denominator-preserving local collections
that expose small ordinary identity coefficients. Trials retain complete inverse
inventories, external mass scope and atomic timeout checkpoints.

`high-energy-compact-retry-tests.wl` retains two real identity-zero directions
that were expensive to prove in their original presentation. It checks the
ordinary compaction route followed by a generic symbolic certificate, with no
sampled-zero acceptance or internal shells. A genuine small deformation, short
deadline, unsupported input and existing smaller workflows guard the fallback.

`coherent-polynomial-rules-tests.wl` verifies exact recursive spin-polynomial
arithmetic with opaque scalar coefficients. It checks complete coefficient and
constant-term coverage, higher spin, tiny genuine coefficients, unsupported
spin-dependent denominators and bounded failure without partial acceptance.
`high-energy-coefficient-series-tests.wl` compares scalar-coefficient and whole
spin-polynomial Laurent expansions exactly. `high-energy-dispatch-tests.wl`
checks real parallel sample dispatch against serial results, fixed sample order
and seeds, complete spin coverage and one shared clock including preconditioning.
The scalar-Series suite also requires exact certificates for CAS zero-test
assumptions, rejects tiny nonzero assumptions, captures repeated assumptions
under caller Quiet/Off and checks that system Message definitions are restored
after success and interruption. Finite but under-expanded series remain incomplete.

The expensive `fermion-construction-integration-tests.wl` constructs the
four-fermion/two-photon PP, PM and MM sectors and the six-fermion process from
fresh models. It uses the public `AllPhysical` workflow, ordinary automatic
resource settings, fixed replay seeds and an independent Feynman benchmark.
No saved final amplitude or process-specific correction basis is supplied.
Select one case while developing, for example:

```sh
HEPCAT_FERMION_INTEGRATION_CASES=PM python3 tests/checks/run-wolfram-tests.py --suite fermion-construction-integration-tests.wl --timeout 3600
```

Omit the environment selection to run all four cases sequentially, with a
larger external runner timeout. Report each case's actual result and timings;
passing the component suites alone does not establish that a full construction
finishes.

## Test Tiers and Cross-Multiplicity Changes

A passing helper suite is evidence for that helper, not a statement that every
process can be constructed. In particular, the default six-point suite includes
one real compact all-positive construction and independent checks. Its assertion
count is not a count of successful six-point processes. The balanced-helicity
worked example uses explicit lower-point recursion, and the optional older
mixed-helicity probes have their own resource settings. Keep these results
separate from fresh public `ConstructAmplitude` runs.

Choose tests according to what a change can affect:

| Tier | When | Required coverage |
| --- | --- | --- |
| Focused identities | While developing a local identity or proof | Exact equality, nonzero controls, both chiralities, relabeling, mass scope, powers and bounded failure. |
| Shared algorithm sentinels | Before accepting changes to reduction, preparation, completion or checker scheduling | Four-point mixed photons and four fermions; all six five-point QED sectors; six-point all-positive plus a mixed-helicity case. |
| Full construction matrix | Before claiming a multiplicity or process family is working | Exact public calls for every claimed sector, with all mandatory checks and independent Feynman comparison. At six points the notebook has five electron/four-photon representatives, three four-fermion/two-photon representatives and six fermions. |
| Reproducibility and performance | For the affected cases after a shared change | Original failing seed, an independent fixed seed and an Automatic run; source/model hashes, stage timings and final expression metrics. |

Physics correctness, efficiency and expression quality are separate results.
A complete physics result must check every physical channel, spurious-pole
cancellation, the high-energy bound, reference independence and the requested
independent benchmark. No check may be skipped to report success. A valid but
large expression remains a simplification issue; a timeout is not evidence that
the amplitude is wrong or the theory needs a new interaction. `AllPhysical`
representation and known achievable complexity ceilings should be reported
explicitly even when the physics checks pass.

Run parity partners in both orders when comparing speed, recording whether the
worker pool was cold or warm. Use one source snapshot without competing heavy
kernels. Runtime measurements should be compared as repeated broad ranges,
not an assertion that opposite helicities take exactly the same number of
seconds. Keep a hard external timeout to clean up an unfinished process, but
do not silently extend the user's calculation limit or lower sample counts.
Promote each reported failure into a focused regression and retain enough of
its actual affine family to replay the failing proof without rerunning the
entire construction. Those retained reports are diagnostic inputs to focused
tests, never answers supplied to the fresh-construction integration tests.

`construct-amplitude-tests.wl` also distinguishes validating one fixed point
from certifying its entire affine family. Controlled stage reports retain real
four-point amplitudes, while the actual mandatory and independent Feynman checks
run without substitutions. The tests cover early factorization-point selection,
late unresolved/underdetermined high-energy candidates, complete parameter
validation, retained unproved directions, wrong-sign rejection and benchmark
reuse. Without an explicit Feynman model the strict workflow still fails on an
unfinished high-energy stage. A separate genuine scalar contact-family probe
requires standalone `CompleteHighEnergy` to remain `UNDERDETERMINED`. Successful
point selection explicitly disclaims benchmark fitting and family uniqueness.

## Full Process Construction

Construction resource defaults are automatic. Focused tests must distinguish the
public `Automatic` request from its numeric `ResourcePolicy` resolution, retain
explicit override and first-rule precedence, and require the same independent
physical/spurious/high-energy/reference checks. Resource caps may grow only as
complete source-space proposals within the shared clock; no test may pass by
silently dropping correction directions or reducing validation quality.
Preferred physical proposals use channel-scaled shared clocks and inspect the
actual denominators of sources from every origin, including auxiliary reference
continuations. Exact identity pruning may dispatch independent directions in
parallel within a shared deadline and a ninety-second per-job ceiling.
Proof tests must retain the original expression before preparation, exercise
original-form grouping/Schouten reductions and the direct symbolic-chart path,
and preserve the later ordinary-preparation fallback. Generic null-axis choices
may use opposite homogeneous little-group weights, but must retain validated
external shells, conservation, symbolic masses and all coherent spin components.
No internal virtuality may be constrained. Missing results, timeouts and zero
numerical samples must preserve unproved freedom. Regression tests should check
these scopes explicitly.
For large final-expression compaction, require an exact replay of every grouped
numerator Schouten edit, a strict improvement in the existing complexity rank,
unchanged physical denominators, and independent amplitude checks. Atomic
checkpoints must retain a consistent amplitude, rank and replay route even on
interruption. The generic identity tests must not rely on a known process
formula or internal shells.

`compact-physical-source-tests.wl` checks the smaller physical source proposal,
fixed regular terms, unchanged residue obligations, physical/simple-pole guards,
complete-space cap rejection and retained own-cut certificates. Its correlated
multi-term continuation must remain one leading coordinate, rather than being
silently split into independently weighted monomials. Deferring optional
invariant trials must not claim that those trials were performed.
`retained-affine-coefficients-tests.wl` checks exact retained-coefficient reuse,
the structural reconstruction guard, mismatched metadata, nonlinear families,
proof-budget exhaustion and removal only after an exact zero certificate.

The expensive fresh six-point parity construction suite is opt-in:

```sh
HEPCAT_PHYSICAL_INTEGRATION_CASES=PPPM,PMMM python3 tests/checks/run-wolfram-tests.py --suite physical-construction-integration-tests.wl --timeout 3600
```

`PPPM` and `PMMM` are the notebook's `e,E,+++-` and `e,E,+---` calls,
with `AllPhysical` and the independent Feynman model. The suite adds only a
replay seed and diagnostic output, not numerical resource overrides. Its
default seeds reproduce the saved positive result and opposite-helicity
high-energy failure. Select either case individually while developing. No prior
amplitude, prepared cache, example helper or supplied correction basis enters
construction.

Every physical channel and mandatory final check must be present and pass,
including the independent Feynman comparison. The suite separately reports
whether the result has only physical inverse factors and achieves the known
parity complexity ceiling of 327. Representation-quality misses are visible
as `QUALITY REGRESSION`, rather than being described as failed physics.
Set `HEPCAT_PHYSICAL_INTEGRATION_STRICT_QUALITY=1` when accepting a simplification
change to make these quality conditions mandatory for the suite's exit status.
Success at sampled kinematics does not establish uniqueness of a retained
affine family or prove an unfinished identity.

`HEPCAT_PHYSICAL_INTEGRATION_SEEDS` accepts up to four comma-separated integer
seeds, or `Automatic`. For example, use `Automatic` for the ordinary time-based
seed path. Results record the resolved seed, source and model hashes, total and
stage timings, complexity, leaves and check summary. Failure reports retain the
actual factorization and high-energy families in a temporary diagnostic file,
without unrelated sample histories. Preserve the printed paths when reporting
a regression. These opt-in results must be reported separately from the
worked-example fixture tests; normal constructor defaults are unchanged.

The verified fresh ordinary call on 2026-10-30 took 272.251 seconds and produced seven
groups with complexity 327 and 1,575 leaves, all denominators physical. The
integration suite passed all seven assertions; its independently regenerated
amplitude report had 17 passing rows, including Feynman agreement. These are
recorded measurements, not promises for a different workload or machine.
This execution includes the certified-identity retry that resolves the former
high-energy proof timeout without changing the notebook call or its defaults.

`automatic-resource-tests.wl` covers lightweight workload estimates, finite
clock/cap resolution, coherent versus fixed-spin sample estimates, unchanged
explicit overrides, public defaults, nested sample precedence and final-check
deadlines. Its fresh four- and five-point constructors receive only the three
required arguments; a forbidden six-point process must still validate as zero.
The larger nonzero six-point default construction is available with
`HEPCAT_TEST_FULL_AUTOMATIC_CONSTRUCTION=1`; it uses the same option-free call
and must pass independent checks. The mixed six-point benchmark also retains
its complete affine family through high-energy completion: an earlier
160-column source fit left 67 directions, all subsequently removed only by
exact ordinary or symbolic external-kinematics identity certificates. The
recorded eight-worker high-energy stage completes in 61.4 seconds. These
measurements are process benchmarks, not general runtime guarantees.
Final external preparation takes another 29.4 seconds and gives 4,147 leaves
in 18 groups with complexity 1,128; the 3.3-second final check reports 17 passes,
including the independent Feynman comparison. Those are recorded measurements
from that earlier source construction, not guarantees for a newly generated
correction space. A lower leaf count alone does not outrank a lower weighted
complexity.

`reverse-normalization-tests.wl`, `determinant-pair-tests.wl`,
`ordinary-pole-compaction-tests.wl` and `scalar-pole-tests.wl` cover the new
ordinary-identity reduction routes. They test arbitrary momentum labels,
unequal external masses, exact polynomial certificates, numerator-only scope,
unchanged inverse factors, interrupted-search checkpoints and retained regular
remainders. The complete six-point `+++-` fixture reduces from complexity 1128
to 327 using the general compactor, with a formal equality check against the
independently derived seven-group expression. A single factored numerator must
receive the same complete pole-block reduction as several separate fractions.
This fixture test is distinct from a fresh full construction, which may choose
a different equivalent representative.

`qed-worked-example-tests.wl` materializes the compact six-point `+++-` and
physical-pole `++--` references directly from the readable example definitions.
It checks model-local parameter binding, unchanged models, ordinary displayed
identities and the complete physical-pole catalogs, then runs all sixteen
non-Feynman checks with the default four samples. It reads no temporary caches
and does not use either example as an input to production completion. The
balanced reference is derived independently from off-shell Dirac currents;
it is not a claim that constructive completion has found that representation.

The slower complete physical mixed-helicity constructor regression is enabled
separately. It chooses `AllPhysical`, requests diagnostic output and supplies an
independent Feynman model, but sets no fitting samples, correction caps, search
steps or stage clocks:

```sh
HEPCAT_TEST_FULL_PHYSICAL_MIXED_CONSTRUCTION=1 python3 tests/checks/run-wolfram-tests.py --suite automatic-resource-tests.wl --timeout 1800
```


`physical-pole-completion-tests.wl` checks inherited and explicit physical-pole
preferences, the original channel catalog, complementary cuts, inverse-factor
and higher-pole guards, contour-normalized channel sources, exact exchange
projection, fixed regular input, correction-space caps and honest fallback.
It also checks that high-energy completion retains the representation request.
A fresh mixed-helicity five-point QED construction must pass all checks,
including an independent Feynman comparison on three samples, retain only
physical denominators, and stay below a compactness bound that rejects the
former oversized result. No saved notebook output or temporary cache is used.


`construct-amplitude-tests.wl` runs real four- and five-point QED workflows,
including an independent Feynman benchmark and a genuinely zero forbidden
process. It verifies original-channel checks, retained preparation history,
external-only final reductions, affine-family transport, fresh validation
seeds, simple outputs, stage/total timings, progress events, final print order,
option precedence, deadlines, model immutability and rejection of opposite-sign
or unresolved final checks. It does not modify or execute the user's notebooks.

`high-energy-family-tests.wl` distinguishes identity-zero correction directions
from real undetermined interactions. It checks the generic symbolic external
chart, momentum conservation, chirality and little-group safety, fixed-spin
and branch exclusions, proof deadlines and retained scalar contact freedom.
The routine suite has 83 assertions, including 29 generic null-pivot checks:
valid and invalid axis choices, opposite homogeneous-weight selection,
conserved external mass shells and all 14 physical internal invariants remaining
generically off shell. These checks use symbolic masses and no sampled
phase-space assignments.
Additional checks cover alternative dependent external shells, a retained
six-point identity, truthful unfinished-proof diagnostics, and symbolic
coherent-spin coefficient proofs. Independent polynomial degree verification,
constant and mixed components, deliberately nonzero additions, higher spin
powers, spin-dependent denominators and expired deadlines are tested. The
plain fixture supplies a zero direction, never a complete amplitude answer.
Certified-span retries additionally verify exact reconstruction, prerequisite
certification, preservation of nonzero remainders and shared deadlines. The
parallel suite checks the same retry through a real warm worker pool.
Six resource-policy checks verify that proof effort scales with the largest
direction as well as the family size, remains inside an explicit stage clock,
and never changes identity certification or retention of unproved directions.
The slower end-to-end regressions reproduce both formerly failing five-point
notebook cases, their working counterparts and independent Feynman checks:

```sh
HEPCAT_TEST_FULL_HELICITY_CONSTRUCTION=1 python3 tests/checks/run-wolfram-tests.py --suite high-energy-family-tests.wl --timeout 900
```

The exact default notebook calls for four-fermion photon emission can also be
replayed with their original failing seeds. They retain the normal algorithm
settings and add no cached amplitude or special correction basis:

```sh
HEPCAT_TEST_NOTEBOOK_FOUR_FERMION=1 python3 tests/checks/run-wolfram-tests.py --suite high-energy-family-tests.wl --timeout 900
```

This mode runs 87 assertions, including independent Feynman checks and exact
external-identity certificate checks. An optional existing
`HEPCAT_NOTEBOOK_VALIDATION_DIRECTORY` receives the newly constructed amplitudes
and compact reports for separate numerical validation.

The spurious-pole suite includes the balanced six-point expression's 24
compound factors. Exact conserved-curve polynomial checks verify their
cancellation in the full sum; an injected double pole at each factor must
fail. Grouped serial/parallel tests preserve every original pole obligation,
result order and unresolved worker failure.
Negative tests include a Clifford sum hiding a massive-invariant pole, which
massless-only deformations would miss. Compound curves vary that invariant,
reject chart-boundary singularities and conservatively decline unsupported
mass patterns or nonhomogeneous helicity factors.
The suite also verifies restored physical masses and spinor-basis determinants
after choosing sampling units, coherent Dirac identities, exact primitive-cache
equivalence, and one shared time budget even for a large compound-pole group.

`null-pair-shift-tests.wl` checks the typed two-null-leg deformation against
independently shifted two-component momentum matrices at five and eight legs.
It verifies conservation, all external shells, both null Dirac equations,
multimomentum chains of either chirality, indexed massive spinors, scalar
invariants, propagator widths and rational powers. Unsupported shifted spinor
forms, composite cut spinors and hatted structures must fail explicitly.
These are algebraic deformation tests, not a boundary-at-infinity assumption
or an assertion that a sum of channel continuations is the full amplitude.

`null-channel-completion-tests.wl` exercises model-derived continuations in
the simultaneous residue fitter: selected-cut preservation, all original
channels, independent parameter assignments, complete affine families,
support and budget guards, and preservation of an existing local term.
The constructor receives no known final expression or Feynman input to fit.

`spin-contraction-guard-tests.wl` verifies that `ReduceSpinContractions` retains
the exact factored input when no supported contraction is possible. Generated
fixtures exercise every massive, bare and indexed helicity, trace and epsilon
replacement family, including hatted spinors, positive and negative powers,
denominators and sums. The existing legacy algebra suite separately checks
their exact signs and reduced expressions.

## Parallel Execution

The four parallel suites require a licensed kernel that can launch and
communicate with at least two local subkernels:

```sh
python3 tests/checks/run-wolfram-tests.py --timeout 600 \
  --suite parallel-execution-tests.wl \
  --suite parallel-preparation-tests.wl \
  --suite parallel-amplitude-check-tests.wl \
  --suite parallel-completion-tests.wl
```

They verify actual worker IDs rather than merely the presence of parallel
options. Scheduler tests deliberately put a slow job first and verify that
later jobs and progress callbacks finish before it, without changing returned
input order. They also cover more than two real workers when available,
processor/job/license limits, worker loss and exact-once serial retry.
Coverage includes ordered results, worker caps, nested serial fallback,
retry after worker failure, caller random-state preservation, disjoint temporary
coordinates, explicit model closures without user-global distribution, and
preservation of an unrelated existing worker. Preparation compares real raw
Feynman diagrams with one shared reference assignment and checks diagram/channel
mass-rule scope. Checker tests compare exact seeded residues and every report
category, including failed and opposite-sign examples. Completion compares exact
fit equations, coherent-spin held-out samples, feedback and final amplitudes;
expired deadlines remain unresolved. Coordinate names are compared up to bound
variable renaming; numerical values, spin-polynomial coefficients, sample seeds,
physical obligations and statuses are not weakened.

The worker pool is local to each test's main kernel. The tests do not use the
server or the interactive notebook kernel. A process that forbids local worker
communication cannot run these parallel suites, even if serial Wolfram tests
work. Ordinary package calls fall back to serial when startup is unavailable.

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
`thesis-preparation-tests.wl` independently checks the same-chirality
diagonal-sandwich contraction with exact two-component matrices, reversed
chains, explicit spin labels, powers, termination and growth guards. Its
six-point all-positive QED examples reproduce the one-term diagram form of
Eq. (101) in the thesis proposal, including particle-label permutations and
the coupling normalization derived from the model. It separately checks
reference-generated pole cancellation while original diagram propagators
remain off shell and protected.
The five-point examples also exercise Eq. (90), both physical-reference
choices, and ordinary chain reduction before multiplying spectator factors.
Mixed-helicity examples additionally check the opposite-chirality sandwich
contraction, authorized per-chain numerator cuts, exact diagram-local spin
components, and retention of an already simpler completed external expression.
Massive-endpoint bracket contractions are checked independently with exact
two-component matrices, both chiralities, explicit spin indices, powers and
protected propagators. Four-fermion radiation tests separately check external
massless-momentum factorization and the actual eight prepared diagrams on
their own electron and photon cuts for every massive-spin component. These
diagram-local checks do not assert equality of the unfinished off-cut sum.
`PrepareAmplitudes` accepts both diagram pairs and compact factorization rows.
Its channel tests check preserved row metadata and propagators, numerator-only
selected-cut shell conditions, and protection of unrelated internal channels.
Passing preparation reports for both inputs to `CompleteFactorization`
retains the simplified expressions, grouped discarded remainders and reference
continuations explicitly. Original channel expressions remain the authoritative
residue targets. The solver first tries the retained preparation directions,
with the same independent all-channel and parameter validation as its other
correction spaces. Plain amplitude/channel lists remain supported, but do not
carry preparation history. Their explicit `"AlternativeChannels"` fallback is
still available. No hidden cache or metadata is attached to ordinary expressions.
`preparation-remainders-tests.wl` covers exact source recomposition, separate
reference changes, partial records, own-cut-only channel assumptions and inverse
protection. A cubic scalar tree demonstrates recovery of another physical pole
discarded during preparation. Additional tests preserve original and zero-residue
targets, reject malformed reports, and ensure consistent, timed-out or inexact
preflights cannot certify success. A genuine five-point QED common-reference
fallback checks full channel validation, original-input and Bose metadata.
`completed-amplitude-preparation-tests.wl` checks the completion pair format,
direct re-preparation with external mass rules only, and reference choices from
physical propagators without inferring internal shells. It preserves literal
shared-Xi correlations and compares a real five-point QED sum before and after
preparation at independent exact off-cut points.
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

## SPINAS Process Export

`spinas-native-project-tests.wl` checks native persistent product caches and
direct helicity formulas against independent complex-amplitude calculations.
Its compiled projects cover changing momenta and parameters, the mass setter,
copying and assignment with pointer rebinding, vector relocation, composite
and long-chain matrix evaluation, upper/lower massive-spin slots, reserved
names, and simultaneous particle permutations of scalar invariants. It also
checks that generated CalcHEP wrappers show named parameters and omit redundant
copies for an unchanged external order.

`spinas-calchep-tests.wl` is part of the normal suite and does not require a
CalcHEP installation. It checks the generated API and unavailable-backend
behavior, physical PDGs, external order, model selection, fixed-helicity/color
guards and a compiled unavailable stub. It also compiles the standalone random
phase-space/comparison tests in both SPINAS precision modes. Those tests cover
mass shells and conservation through six points, seeded replay, common numeric
inputs, 1,000-point pass and failure controls, tiny wrong amplitudes and explicit
evaluation errors.

`spinas-calchep-numerical-tests.wl` covers a different use of CalcHEP: its
numerical integration executable evaluates the supplied SPINAS amplitude,
without generating a symbolic counterpart. It builds and starts the genuine
`n_calchep` in a private session with `CalcHEPModel -> None` and a trap symbolic
executable. It tests current masses, dependency-ordered constraints, selected
normalization, arbitrary incoming order, independent worker caches, running
couplings, unsupported-input errors and zero-parameter models. The runtime
probes compile in both SPINAS precision modes. Unused unsupported model
definitions are reported; a required missing dependency remains unavailable.

Enable genuine VEGAS integrals as well as the ordinary callback checks with:

```sh
HEPCAT_TEST_CALCHEP_NUMERICAL_INTEGRATION=1 python3 tests/checks/run-wolfram-tests.py --suite spinas-calchep-numerical-tests.wl --timeout 900
```

The full run has 29 assertions. It compares the integrated `eeAA` cross section
with an independent SPINAS angular quadrature, repeats after coupling and mass
changes without rebuilding, and compares a constant five-scalar amplitude with
the analytic three-body phase volume and identical-particle factor. All 29
passed on 2026-10-30; no symbolic-generator call was recorded. The three-body
result was 333.8268 +/- 0.1224 pb, versus 333.7078 pb analytically. The `eeAA`
result was 65181.0918 +/- 0.0179 pb, versus 65181.0853 pb by quadrature. These
are finite Monte Carlo validation runs, not analytic equality certificates.

The real external integration is opt-in:

```sh
python3 tests/checks/run-wolfram-tests.py --suite spinas-calchep-integration-tests.wl --timeout 1800
```

It uses the local CalcHEP dynamic C interface and native SPINAS library to
generate and compare `eeAA` and `eeAAA` at 1,000 shared random points each. It
also verifies seed replay, a deliberately doubled amplitude, external-mass
mismatch diagnostics and preservation of the original model tables. Set
`HEPCAT_CALCHEP_DIRECTORY` or use the sibling installation. The five-point
external test can be omitted with `HEPCAT_TEST_CALCHEP_FIVE_POINT=0` when
investigating a four-point build issue; a skipped case is not a pass.
CalcHEP's input model is staged privately, independent of the HEPCAT expression.
The normalization follows the dynamic-interface manual, section 10 and
appendix D.3, and the installed `sqme` implementation. No empirical rescaling
or amplitude-driven sample rejection is permitted.

The polarization suite checks explicit incoming/outgoing processes, fixed
incoming or outgoing massless helicities, antiparticle crossing, selected-sector
coverage, incoming averaging and labeled outgoing symmetry factors. Its C++
test compares complex amplitudes and restricted spin sums at three phase-space
points, including recombination into the full helicity sum:

```sh
python3 tests/checks/run-wolfram-tests.py --suite spinas-polarization-tests.wl --timeout 300
```

The build-tool regression suite checks automatic and explicit CMake lookup,
invalid tools before project creation, source-only export without CMake, and
macOS GUI-style PATH builds for both fixed helicities and complete families.
It also exercises a custom executable path containing spaces:

```sh
python3 tests/checks/run-wolfram-tests.py --suite spinas-build-tools-tests.wl --timeout 180
```

The grouped-helicity integration suite is:

```sh
python3 tests/checks/run-wolfram-tests.py --suite spinas-helicity-family-tests.wl --timeout 420
```

It compiles one project per particle family and independently enumerates every
external spin and helicity at three physical phase-space points. Native
`eeAA` complex amplitudes and its full spin-averaged result are compared with
the generated code. Incoming photons exercise helicity averaging, three final
photons exercise the `1/3!` factor, and six massive fermions exercise the
`1/(2! 2!)` factor. Other fixtures test shared parameter indices and complex
parameter updates across branches, signed identical massless-fermion
permutations, explicit zero sectors, incomplete-family rejection and overwrite
protection. No parity relation or missing zero amplitude is inferred. This
suite needs the built SPINAS library and native `eeAA`/`AAee` fixtures for its
compiled comparisons; source-only guards still run if those are unavailable.

Run the complete C++ export integration suite with:

```sh
python3 tests/checks/run-wolfram-tests.py --suite spinas-export-tests.wl --timeout 900
```

`spinas-export-tests.wl` uses the public `WriteSPINASProcess` API to write,
configure, compile, link and run complete four-, five- and six-point projects.
It finds the SPINAS checkout beside HEPCAT, or uses
`HEPCAT_SPINAS_DIRECTORY`. Native checks require a C++11 compiler, CMake,
SPINAS headers and its built static library, plus `SM/eeAA.cpp`. Missing
compiler or native SPINAS fixtures produce an explicit skip while source-only
checks still run; missing CMake is reported as a build failure. Builds use the
installed library's precision setting.

The independent C++ reference helper checks complex amplitudes at three
conserved physical phase-space points for each fixture, including every
massive-spin component. Coverage includes all four two-photon helicities,
the agreeing five-point H and S2 forms, the twelve-term all-positive six-point
form, massive-vector normalization, outgoing massive spinors, off-shell
composite momentum insertions, chiral traces, reordered incoming legs,
complex model parameters and HEPCAT's `+i m width` convention. Separate
spin-average comparisons check that `amp2` adds no identical-particle factor
or sum over other fixed massless helicities. The suite also checks zero
amplitudes, scoped parameters despite same-named user globals, legacy
formatting, overwrite protection and rejection before partial files are written.

The mixed-helicity four-point entry in `models/amps3.mdl` has an explicit
`-2 EE^2` coefficient, whereas native `SM/eeAA.cpp` uses `+2 e^2`.
The exporter preserves the supplied amplitude's sign. Tests assert this
algebraically identified relative minus for the imported model, and separately
check direct equality when the input uses the native convention. No phase or
normalization is fitted, and neither model nor exporter signs are changed to
force agreement. These numerical regressions validate translation, not the
physical correctness of every model entry.

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

`completion-input-fit-tests.wl` covers the automatic one-coefficient input
normalization and shared-contribution proposals. It uses real scalar poles
to distinguish an overcounted shared term from legitimate multiplicity,
preserves complete signed coefficients and explicit regular input, and checks
fallbacks, opt-outs, parameter validation and interrupted attempts. The real
five-point QED example verifies six prepared AllPhysical diagrams have three
exact pairs, with sum `2 H`, and recover `H` without channel subtraction terms.
These tests treat overlap removal as a residue-validated candidate, never as
an arithmetic identity for adding diagrams.

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
The default suite also regenerates all 24 all-positive physical-reference
diagrams and completes them through the source-derived companion stage,
without supplying a known answer or correction basis. It checks the six
four-copy input groups, four-coefficient fit, compact 12-term result, every
physical channel at separate parameter values, high-energy behavior and a
fresh full Feynman comparison. New purely regular companion directions are
rejected. The input-fit suite additionally tests exact compact-point selection,
protected contact coefficients, affine rebasing and bounded interruptions.
It also covers the explicit balanced-helicity QED recursion example: fresh
lower-point channels, all eight shifted products, reference assignment,
propagator shifts, bounded failures, a fresh coherent-spin Feynman comparison
and representative two- and three-particle residues. This example does not
assert success of the separate automatic diagram-completion ansatz or
silently certify unsupported spurious factors.
`fermion-symmetry-tests.wl` covers signed identical-fermion exchange and its
combined use with Bose symmetry. It checks species/antiparticle separation,
helicities, fixed-spin and color safeguards, parity for unordered groups,
normalization and idempotence. The six-point `e,E,E,e,A.+,A.+` example uses the
full order-eight signed action, including every fermion and photon group.
Larger-group tests verify the shared work cap and explicit subgroup metadata.
Known four- and five-point four-fermion amplitudes are checked before and after
projection against fresh physical cuts and independent Feynman amplitudes.
A missing pairing is repaired through the public completion API without
changing original targets. High-energy directions inherit the combined action,
including when the completed expression is zero. These are 50 focused checks;
the six-point projector test does not claim a new six-point reconstruction.

`bose-symmetry-tests.wl` checks automatic identical-boson state inference from
the model and homogeneous external spinor weights, including separate photon
helicities, fermion exclusion, species/charge degeneracies and conservative
color handling. Explicit ordered process labels can resolve ambiguous species;
they are retained only in requested preparation reports without changing plain
amplitude pairs or channel rows. Large exchange groups use a bounded exact
subgroup. Coefficient-space tests that intentionally retain asymmetric
directions opt out with both symmetry options set to explicit empty lists.
Focused completion tests also cover explicit Bose projection, the compact/literal
union, selected-cut atomic sources and exact formal-dependence pruning without changing the available span.
The bounded pruning pass has a 60-second allowance. Exact Gaussian-rational
columns use incremental sparse elimination; other exact numeric fields keep
the bounded legacy fallback. Tests cover typed left-to-right selection,
complex and algebraic coefficients, infinite budgets, rational denominator
normalization and atomic timeout fallback. Inexact input and unfinished
pruning retain every original direction. The final correction cap applies
after projection and pruning.
When the full automatic Bose space fits, its complete span is retained. If it
does not, an explicitly named grouped-input alternative keeps whole input
diagrams and all available channel atoms. Both exact sizes and limit decisions
are recorded, never silently truncated. Small scalar regressions verify the
alternative at two mass scales and reject it when it still exceeds the cap.
Cheap mixed-helicity tests check the fourteen-channel inventory and the
six-element permutation group of three identical positive-helicity photons,
leaving the negative-helicity photon fixed. Equal masses alone never infer a
helicity grouping. Automatic inference requires compatible external states.
Automatic-chart simple-pole evaluation is compared with full mapped residues.
Automatic affine components are extracted in one pass without changing their
exact algebra, variable order or explicit zero directions. Opaque custom maps
retain the legacy coefficient forms, and nonlinear inputs keep the conservative
extraction path.
Hidden inverse-bracket and invariant poles, higher-order cancellations, implicit
x-factor context, zero-coefficient singular directions and custom maps exercise
the conservative fallback paths rather than bypassing regularity checks.
Large exact linear systems also test bounded CRT reconstruction with exact
original-row and full-affine-family certificates. Manufactured cases cover
coefficients beyond one prime's reconstruction bound, independent unused-prime
checks, unlucky pivot profiles, original-coordinate signs and scales, zero/free
columns, full-rank systems and unresolved fallback. Neither a modular rank nor
a reconstructed candidate alone is accepted as a solution.
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

The mixed `e,E,A.+,A.+,A.+,A.-` case has separate inexpensive channel-layout
checks and a separate optional full regression. Only photon legs 3, 4 and 5
are Bose averaged. Its automatic correction space is larger, so allow for
preparation, the 7200-second completion budget and independent Feynman checks:

```sh
HEPCAT_TEST_FULL_MIXED_SIX_POINT=1 python3 tests/checks/run-wolfram-tests.py \
  --suite six-point-completion-tests.wl --timeout 9000
```

The actual mixed fit was verified from exact cut constraints collected in
batches, with independent full-family residue validation at two mass scales,
three off-cut Feynman comparisons, twelve automatically discovered spurious
pole checks and an `E^-2` high-energy check. The fresh
serial optional regression is a reproducibility test, not a claim that this
long command has already been timed end to end. No cached rows or stored
answer are used by the optional test.

## Five-Point Spurious-Pole Construction

This opt-in suite replays the QED notebook calls with both same-helicity
three-photon cases, both mixed-helicity cases and both single-photon
four-fermion cases. The three-photon results must retain their apparent
bracket poles. Both single-photon results must use physical denominators.
All results must be Xi-free, stay within the recorded complexity ceilings,
and pass the physical and spurious pole, high-energy, reference and
independent Feynman checks. The suite supplies no final expressions.

```sh
python3 tests/checks/run-wolfram-tests.py \
  --suite spurious-construction-integration-tests.wl --timeout 720
```

Set `HEPCAT_SPURIOUS_INTEGRATION_CASES=P,M` to replay only the single-photon
cases. The other case names are `PPP`, `MMM`, `PPM` and `PMM`.

Select `PPPP6` to replay the six-point, four-positive-photon notebook call
with factorization model 4. It requires the general compound-pole orbit
reduction, complexity at most 120, and all 23 checks including the independent
Feynman comparison.

```sh
HEPCAT_SPURIOUS_INTEGRATION_CASES=PPPP6 python3 tests/checks/run-wolfram-tests.py \
  --suite spurious-construction-integration-tests.wl --timeout 720
```

Select `PPPM6,PMMM6` to replay both six-point mixed-helicity notebook calls
with three photons of one helicity and one of the other. The tests use
factorization model 4 and require a common chiral vector current with
complexity at most 528, no Xi, 14 physical residues, four apparent pole
cancellations and all three remaining checks.

```sh
HEPCAT_SPURIOUS_INTEGRATION_CASES=PPPM6,PMMM6 python3 tests/checks/run-wolfram-tests.py \
  --suite spurious-construction-integration-tests.wl --timeout 720
```

Select `PPMM6` to replay the balanced six-point Max Spurious calculation
with factorization model 4. This case requires the off-shell vector-line
route, four apparent pole cancellations and all 21 checks. The automatic
result has no complexity ceiling. The same test independently checks the
separate hard-coded worked form and its complexity ceiling of 1044.

```sh
HEPCAT_SPURIOUS_INTEGRATION_CASES=PPMM6 python3 tests/checks/run-wolfram-tests.py \
  --suite spurious-construction-integration-tests.wl --timeout 720
```

Select `FourFermionsPP6,FourFermionsPM6,FourFermionsMM6` for the three
Max Spurious four-fermion, two-photon cases with factorization model 4.
Each replay requires all 20 physical residues and the independent Feynman,
spurious-pole, reference and high-energy checks to pass. It also validates
the separately worked current formulas, whose complexity ceilings are
1360, 1664 and 1360 respectively. The constructor is never given those formulas.

```sh
HEPCAT_SPURIOUS_INTEGRATION_CASES=FourFermionsPP6,FourFermionsPM6,FourFermionsMM6 \
  python3 tests/checks/run-wolfram-tests.py \
  --suite spurious-construction-integration-tests.wl --timeout 1800
```

Select `SixF6` to replay the six-fermion Max Spurious notebook call with
factorization model 4. The result must have only physical denominators, a
complexity of at most 3168, and all 21 checks passing, including 18 physical
residues and the independent Feynman comparison. The test requires the
grouped off-shell exchange route to derive this form from the diagram model.

```sh
HEPCAT_SPURIOUS_INTEGRATION_CASES=SixF6 python3 tests/checks/run-wolfram-tests.py \
  --suite spurious-construction-integration-tests.wl --timeout 720
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
- Legacy display checks validate symbolic objects and strings, not front-end
  appearance or a rendered PDF. The separate SPINAS integration suite does
  compile and run generated programs when its native prerequisites are present.
  Diagram drawing functions have interface checks, not visual rendering checks.
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

## Independent MadGraph Validation

`spinas-madgraph-tests.wl` covers the public Mathematica entry point and exported
project surface without requiring MadGraph. It checks unconfigured installation
and model defaults, explicit theory and particle-sector selection, per-helicity
support, unavailable-backend metadata, source-only export, replay-seed and
point-count guards, and preservation of the input model. Missing
benchmark software cannot become a passing numerical check.

Full MadGraph integration additionally requires the local generator, GNU
Fortran, a C++ compiler and the installed SPINAS library. The validator generates
an independent unpolarized reference process and selects fixed massless helicity
vectors from that process, summing massive spins. This avoids comparing different
massive-spin bases and avoids changing identical-particle normalization by
creating separately polarized subprocesses. The numerical report records the
actual seed and first failing common momenta for replay.

Select the full independent numerical test explicitly:

```sh
python3 tests/checks/run-wolfram-tests.py \
  --suite spinas-madgraph-integration-tests.wl --timeout 900
```

It matches an adjustable massive-electron QED model, checks 1000 common points
for each of the four photon-helicity combinations, replays the seed, rejects a
deliberately doubled single-helicity amplitude, retains the failing momenta,
accepts an overall imaginary amplitude phase, and checks a fixed outgoing
photon helicity. It also checks four-fermion QED scattering against an explicitly
selected photon/electron reference sector, guarding against accidentally
including Z or Higgs exchange from a full SM model. The installation, model and
particle restriction appear in the test calls, including the direct Mathematica
shortcut. The test fixture selects HEPCAT_MADGRAPH_DIRECTORY or a sibling
MadGraph directory explicitly; the package does not infer that location. Failed
test projects are retained at the printed temporary path for diagnosis;
successful projects are removed.

The builder and C++ protocol also have lightweight tests that do not require a
MadGraph installation:

```sh
python3 -m unittest discover -s tests/checks -p 'test_madgraph_*.py' -v
```

The runtime tests compile a small fake reference backend in a temporary
workspace and skip when a suitable C++ compiler or SPINAS headers are absent.
They cover timeouts, replayable seeds, phase invariance, parameter updates,
particle filters and explicit failures. The builder tests cover model matching,
width prescriptions and safe parameter-card serialization.

`fermion-sewing-grading-tests.wl` verifies the common fermion-reference
normalization for boson and fermion sewing, including mixed-seam composition
and unchanged cubic phases. Three distinct Dirac flavors test an exact nonzero
photon/fermion double residue and all nine physical cuts against an independent
six-diagram Feynman amplitude. Each cut checks all 64 massive-spin components;
an interchange of distinct antifermions checks the external reference order.
Complete lower amplitudes are generated from cubic vertices and independent
off-shell Feynman diagrams, with no supplied six-point answer.
An explicit negative case checks that unpaired Dirac lower amplitudes are
reported as unsupported before any invalid sign enters an amplitude.

`high-energy-laurent-rows-tests.wl` checks large affine-family coefficient
expansion without splitting coupled unknown equations. It covers exact tiny
coefficients, full spin tensors, nonlinear or singular unknowns, unsupported
metadata, deadlines and the unchanged exact fallback.
