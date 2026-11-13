# Standard Model five-point inventory

`inventory.json` records every five-leg particle/helicity multiset supported by
the elementary interactions in ordinary model 1, and the remaining helicity
slots needed to export each physical process. It is a work list. Presence in
this file does not mean an amplitude has been constructed or independently
validated. Construction and numerical-validation progress belongs to the
runner's `progress.json`, with captured records in `tests/results/SM-5-point/`.

The model uses diagonal generation mixing, massless neutrinos, and the masses
and elementary vertices declared in `models/`. There are 1,420 multisets with
allowed trees. Completing each photon/gluon helicity family adds the two
five-gluon all-same-helicity slots, giving 1,422 cases in 869 export families.
The two extra slots have no allowed model-1 diagrams. A supported tree does
not imply a nonzero full amplitude: cancellations must still be checked.
No graviton case occurs because model 1 declares no graviton interactions.

| Sector | Cases | Export families | Generation representative cases | Representative families |
| --- | ---: | ---: | ---: | ---: |
| Pure Higgs | 1 | 1 | 1 | 1 |
| Massive electroweak bosons | 8 | 8 | 8 | 8 |
| Electroweak bosons with photons | 18 | 7 | 18 | 7 |
| Pure gluons | 6 | 1 | 6 | 1 |
| Two fermions with Higgs bosons | 156 | 108 | 52 | 36 |
| Two fermions, other electroweak | 213 | 111 | 71 | 37 |
| Two fermions with gluons | 294 | 90 | 98 | 30 |
| Four fermions with a Higgs | 108 | 108 | 20 | 20 |
| Four fermions with a Z or W | 252 | 252 | 44 | 44 |
| Four fermions with a photon | 204 | 102 | 36 | 18 |
| Four fermions with a gluon | 162 | 81 | 26 | 13 |
| Total | 1,422 | 869 | 380 | 215 |

The detailed `BosonContent` field records the mixture within each broad sector.
The number of fermions is always explicit. Massive spins and fixed chiral
neutrinos are retained in the amplitude; only photon/gluon helicities are
separate construction cases.

## Enumeration and generation relations

A five-leg tree can have three cubic vertices or one cubic and one quartic
vertex. The generator sews the declared particle/antiparticle legs for both
possibilities and retains the topology provenance for each multiset. These
are particle graphs, so this inventory does not perform spinor algebra or
introduce new interaction rules. Internal photons/gluons use the helicity
partners specified by the model. Their implicit color tensors do not change
the external particle inventory.

The 36 simultaneous bijective quark/lepton generation maps are copied from
the four-point factorization catalog. Every rebuild checks each map against
all 74 ordered elementary vertex expressions and every particle's mass,
spin, color, chirality and antiparticle role. Widths are not included in this
bare-tree equivalence. First-generation representatives are preferred, with
the equality and distinct-flavor patterns preserved. No parity or charge
conjugation relation is used to reduce the work list, and no numerical phase
is fitted. These relations identify 380 representative cases in 215 families.
They do not automatically mark any new construction or comparison complete.

## Stable records and crossings

Case IDs start with `sm5_`; family IDs start with `sm5f_`. The suffix is the
first 12 hexadecimal digits of SHA256 of the ASCII-sorted particle names
joined by `|`. A family replaces `A.+` and `A.-` by `A`, and similarly for
`G`. IDs do not depend on scheduling or the chosen physical crossing.
Python/ASCII and Wolfram string sorts differ, so native Wolfram comparisons
should sort both saved signatures and queried particles.

Each family also has a human-readable `ExportName` for its directory and C++
class. Fermion particle names follow the model's particle column; its
antiparticle column adds `bar` (`E` becomes `ebar`, `U` becomes `ubar`, and
`Ne` becomes `nebar`). The other tokens are `Wp`, `Wm`, `A`, `G`, `h`, and
`Z`, concatenated in the family's declared leg order. For example,
`e, E, h, h, h` exports as `eebarhhh`. The generator verifies that all 869
names are distinct even on a case-insensitive filesystem and that each is
a portable C++ identifier. Immutable hashed IDs remain the checkpoint keys.

`Cases` includes `ID`, `FamilyID`, `Signature`, ordered `Particles`, `Sector`,
`HasDiagrams`, `Topologies`, `RepresentativeID`, `FlavorMap`, and permutation
metadata. `FlavorMap` is a zero-based index into `FlavorMaps`.
`RepresentativePermutation` maps each source leg of the generation-mapped
representative to a target leg in this case, using one-based positions.
Use HEPCAT's typed relabeling for expressions and fermion ordering conventions.
The inventory does not claim that a plain permutation has positive sign.

Each family's `Particles` is ordered with two physical incoming particles
first. The remaining three all-incoming legs cross to their antiparticles in
`Outgoing`. A conjugate fermion pair is preferred in the initial state when
available. The `Kinematics` thresholds use the declared numerical masses and
supply a suggested center-of-mass energy above both thresholds. The present
inventory suggests 1 TeV for every family; actual random phase-space points
are generated by the validation code, not by this inventory.

`CaseIDs` lists the unique helicity multisets required by that family.
`HelicityAssignments` lists every labeled photon/gluon helicity choice and its
source case and leg permutation. For example, three identical photons need
four distinct multiset constructions but eight labeled helicity assignments
in an export. There are 1,586 labeled assignments across all 869 families.
The numerical exporter can use its established identical-particle permutation
closure; these assignments provide a completeness check.

## Rebuild and check

From the repository root:

```sh
python3 tests/SM-5-point/inventory.py
python3 -m unittest discover -s tests/checks -p test_sm_five_point_inventory.py -v
```

The inventory saves hashes of its model and four-point flavor-catalog inputs.
`diagram-crosschecks.json` contains 16 actual `CreateDiagrams` support checks,
including Higgs, electroweak, colored, four-fermion and forbidden controls.
All currently pass. These checks test graph support only. They do not stand
in for residue, high-energy, reference, Feynman-amplitude, CalcHEP or MadGraph
validation. To repeat the native graph checks, use the installed licensed
Wolfram kernel:

```sh
WolframKernel -script tests/SM-5-point/check-inventory.wl
```

## Construct, validate and resume

Open `tests/SM-5-point-construction.nb` and evaluate Setup. Each displayed
family has ordinary `ConstructAmplitude` calls followed by a grouped
`WriteSPINASProcess` call. The diagram input is SM model 1; the factorization
input is model 2. The separate matched Feynman model is used for the native
comparison. The export uses the full SM in CalcHEP and MadGraph, with diagonal
flavor mixing and widths disabled by the normal validation adapters.

The notebook begins with the starter families and adds each attempted family
when refreshed. The inventory retains the complete work list, including the
sectors not yet attempted. A missing output or a failed construction is never
shown as a completed validation.

For an unattended resumable batch from the repository root:

```sh
python3 tests/SM-5-point/run.py --representatives --notebook
```

This begins with one family per certified generation orbit. It does not count
the other generations as numerically validated. Omit `--representatives` to
work through the exact full inventory. `--sector PureHiggs` or repeated
`--family FAMILY_ID` arguments select a smaller batch. `--limit 5` bounds the
number of families attempted. The default comparisons use 1,000 phase-space
points. External installation paths can be changed with `--spinas`,
`--calchep` and `--madgraph`; the notebook exposes the same paths in Setup.

Every native construction and every independent export is checkpointed before
the next job. `progress.json` is the resumable ledger. The exact expressions,
full native diagnostics, random seeds, native check rows and numerical reports
are saved as compressed WXF records in `tests/results/SM-5-point/`. Live stage
logs and short JSON summaries are in that directory too. Generated numerical
projects go to `tests/results/spinas-output/SM-5-point/` with readable names.
The worked notebook retains the actual stage milestones, check tables and expressions. Repeated working heartbeats are omitted from its saved preview; the complete prints remain in the WXF records and live logs.

An amplitude counts as fully validated only when its native construction
passes all required checks, including the Feynman comparison, and its complete
helicity family passes both CalcHEP and MadGraph at the requested point count.
Export checkpoints are bound to the exact input amplitude files. Package,
model, reference-model, export-template and numerical-library fingerprints
prevent stale results from being silently reused. The queue stops if its
inputs change during a run. Previous attempts are archived before retries.

Failed and timed-out cases remain explicitly incomplete. The next ordinary
batch continues other pending families. Use `--retry-failed` after diagnosing
the failure or selecting a justified larger external `--timeout` (seconds).
The constructor's own automatic resource limits remain in force. A timeout
is never promoted to a pass. Ctrl-C stops the owned worker and saves the
ledger; completed siblings are reused on the next run.

To refresh the notebook from saved actual records without doing new physics:

```sh
python3 tests/SM-5-point/run.py --refresh-only --notebook
```

Notebook regeneration is explicit. Preserve a separately edited copy before
regenerating if you have added manual derivations or comments.


## Precision and retained-expression checks

`FullyValidatedCases` counts families passing the ordinary native, CalcHEP
and MadGraph runs. `PrecisionQualifiedCases` is separate. A precision
qualification retains the raw CalcHEP failure and requires a current,
checksum-bound replay at genuine 113-bit precision on at least 1,000 common
points, the same 1e-7 relative tolerance, zero mismatches/errors, and a passing
MadGraph comparison. The replay also verifies that its native 53-bit control
reproduces the captured ordinary values. The notebook shows both results.

`replay-calchep-precision.py` derives the process from an existing numerical
export. It supports GCC/libquadmath with a built Unix-Makefiles export and
two incoming legs first. It reports evidence separately and never changes a
raw result to PASS. `precision_evidence.py` checks the optional evidence link
against the current amplitude capsules, export sources, libraries and saved
points before the ledger or notebook recognizes a qualification.

`revalidate.wl` can check a previously completed expression after a package
change using fresh physical/spurious/high-energy/reference and native Feynman
checks. It preserves the original constructor expression, details, outputs,
time and seed and records the new validation separately. Failed rechecks do
not replace a valid archived construction. After a successful recheck,
external comparisons are repeated for the new checksum-bound capsule.

The external construction watchdog defaults to two hours so that it does not
cut off the larger automatic five-point budgets. The package's own finite
Automatic limits and the worker's memory limit remain active. An explicit
`--timeout` overrides the external guard.

## Regression audit

The baseline audit executed all 119 standard Wolfram suites against package
source fingerprint `208ed946f731511ac2a2d85036d0727d4bda163f083514dd53a9d63ebbf4051a`.
There were 111 initial passes and eight initially failing suites. Five strict
historical-test migrations then passed their complete focused replays: color
and W-field conventions, two obsolete score expectations, the worked-reference
momentum-degree profile, and a renamed notebook section checked by its actual
formula and MadGraph-validation inputs. The resulting status is 116 passing
suites and three unresolved bounded-quality suites. The Python support tests
passed 282 of 282.

The remaining failures are recorded in `paired-spectator-tests.wl`,
`native-laurent-identity-tests.wl`, and `thesis-preparation-tests.wl`. They concern
bounded simplification or exact-proof preparation. Their original strict
assertions remain in place. The original logs, pre-policy controls, elevated
replays and successful test migrations are retained separately rather than
presenting this audit as entirely green.

The local machine-readable report is
`tests/results/SM-5-point/regressions/summary.json`; its `PackageSourceFiles`
records exact file hashes, and its `Artifacts` entries locate the full log,
per-suite progress, repaired replays and diagnostic details. Generated files
under `tests/results/` are local audit artifacts. This baseline applies to the
recorded source bytes; later production changes require their own delta checks.
Use the standard commands in the repository README and the testing tiers in
`tests/checks/README.md` to repeat checks for a later revision.

## Frozen C5 recorded coverage

The recorded C5 checkpoint uses source context
`b672a8945605ad07e3f24e47a783f071a54c9b057e44b65048267ec057d55afd`.
Its ledger contains 200 completed native case records. Of these, 195 have
ordinary full-family CalcHEP and MadGraph passes and two have separately
verified CalcHEP precision qualifications, giving 197 validated cases.
The historical records admitted by the finite compatibility proofs keep their
original contexts and timings; these totals do not claim 200 fresh C5 runs.
There is no outstanding supplemental mass-counterexample exclusion in this
checkpoint. Earlier counterexamples and rejected candidates remain archived.

All six pure charged-lepton/four-fermion/Higgs families now pass their native
and 1,000-point CalcHEP/MadGraph comparisons, including identical fermions.
The electron, muon and tau pair with two Z bosons and one Higgs are also
validated. The four-Z/Higgs ordinary call completed through the existing
factorization/high-energy fallback after one optional source check reached its
high-energy clock; its final native checks and both 1,000-point comparisons pass.

| Newly recorded process | Native construction time | Final complexity | CalcHEP / MadGraph points |
| --- | ---: | ---: | ---: |
| Z, Z, Z, Z, h | 860.1 s | 249 | 1,000 / 1,000, both PASS |
| m, M, Z, Z, h | 226.4 s | 1,094 | 1,000 / 1,000, both PASS |
| l, L, Z, Z, h | 226.8 s | 1,094 | 1,000 / 1,000, both PASS |

The remaining three completed native cases belong to the unfinished
five-gluon family: `+++++` and `++++-` calculate to zero, while `+++--`
returns a nonzero result. The latter actual default constructor call also has
a separate fixed-helicity MadGraph comparison. These partial-family checks
are not counted as a completed full-helicity-family CalcHEP/MadGraph export.
The `++---` case remains failed in C5; `+----` and `-----` have not been
completed in that checkpoint. Thus the five-gluon family is still incomplete.

All nine charged-fermion-pair/three-photon families are covered: three charged
leptons and six quarks, each with four helicity multisets and all eight labeled
helicity assignments in its MadGraph export. Every export uses 1,000 common
points for each requested comparison. These are the recorded parameter and
kinematic checks, not proofs of identities at arbitrary parameters.

This section describes frozen C5 evidence. Later experimental source changes
or prototype successes are not new validated campaign records unless their
actual inputs, checks and context have been admitted and checkpointed.

## Recorded C6 coverage

The C6 checkpoint uses source context
`47026bedc2844622616e46a05069c2682cc4306872799bbe8e02d4a01291cc04`.
It contains 215 completed native case records. Of these, 207 have ordinary
full-family CalcHEP and MadGraph passes. Two further cases have verified
CalcHEP precision qualifications, and the six five-gluon helicity multisets
have a separate MadGraph precision qualification. This gives 215 validated
cases in 142 ordinary PASS families and three precision-qualified families.
These totals include checksum-bound historical records admitted by the finite
reviewed compatibility chain; they do not claim 215 fresh C6 constructions.

C6 extends the source-certified current path to declared massless chiral
fermion endpoints. The endpoint state comes from the model metadata, and the
used vertices, ordered graphs and normal independent checks remain mandatory.
It also permits a model-certified helicity partner as a construction proposal:
the exact declared vertex transformation is checked before transport, and the
target still receives its own factorization, high-energy, reference and
Feynman checks. Neither relation marks an unrun process validated.

The completed twelve-family chiral cohort includes charged lepton/neutrino
pairs with a W and two Higgs bosons, in both charge choices and all three
generations, and neutrino pairs with either Z/h/h or Z/Z/h. Its summary separates
six constructor dispatches, two already-current C6 public-call records and four
prior compatible passes. Eight exports were freshly run at C6. Every family
has both 1,000-point numerical comparisons passing. The muon and tau pair with
one photon and two Higgs bosons also complete for both helicities and pass both
1,000-point family comparisons. Exact capsule, context and timing bindings are
in `cohort-chiral-C6-summary.json` and `cohort-muon-tau-Ahh-C6-summary.json` under
`tests/results/SM-5-point/`.

All six five-gluon helicity multisets have completed native constructions.
The grouped export passes CalcHEP at 1,000 points. Its original exhaustive
MadGraph comparison retains a raw FAIL: six of the 32 labeled helicity
comparisons have one mismatch, all at point 719 of the same 1,000-point stream.
Four analytically zero components have residuals about `3.08e-22`, and two small
nonzero components of order `2.99e-11` differ by about `3.42e-7` relatively.
The original values, point, seed and tolerances remain unchanged.

A separately reviewed 113-bit replay passes all 32,000 point/helicity
comparisons on the uncorrected binary64 inputs at the original tolerances.
The independent verifier checks every retained file, current generated source
and linked library, all saved failed-point momentum bits and native values,
and every recomputed comparison. It does not claim that the original run
saved every native value. Its maximum nonzero relative discrepancy is about
`1.36e-8`; its maximum residual in proven-zero channels is about `2.25e-23`.
The raw export stays FAIL, with a separate `PRECISION_QUALIFIED` record.
Evidence and replay instructions are in
`tests/results/SM-5-point/precision/GGGGG-MadGraph/`. The notebook offers a
read-only recomputation of the archived comparisons. A complete reference
rebuild is a separate procedure described in that packet's instructions.

The optional MadGraph verifier is separate from the established CalcHEP
precision path. Both reject stale evidence. Historical qualifications retain
their actual source contexts and require explicit finite compatibility for
later reviewed source changes; model or numerical-runtime changes are not
silently admitted. No claim is made that the complete 1,422-case SM five-point
inventory is finished.

## Construction and validation bottlenecks

The source changes made during this campaign retain the distinction between a
construction source and an independent benchmark. Feynman expressions never
supply a fitted correction. The failure-only ordinary-source retry redoes only
reductions that actually timed out, within the original finite source-building
deadline. Both electron/photon/two-Higgs helicities needed this retry and then
passed the normal construction and both external comparisons. Their final
complexity scores are both 310.

Default independent Feynman preparation may time out while trying to simplify
a diagram. The constructor now retains that partial report and can instead
convert the complete original off-shell diagram list. Every original mass-rule
pair must survive. Explicit preparation controls are honored, and the resulting
benchmark still has to pass all normal amplitude comparisons. This recovered
the W-pair/three-photon benchmark. Its conversion can dominate the total run
although the constructive factorization stage is quick.

An earlier checker-budget limitation was recorded for that W-pair family. Its
independent converted benchmark has 2,591,628 leaves, whereas its constructive
candidates have only hundreds. The automatic checker metric currently sizes its
clock using the candidate and channels. Two default attempts therefore retained
Feynman UNRESOLVED rows despite passing the other native checks. The original
failures, unchanged candidates, complete benchmarks, and controlled checks with
larger finite limits are saved under `diagnostics/WWAAA/`. A separate successful
candidate check is not labeled as a successful default constructor. Actual
public constructor retries with explicit limits have their own scripts and
output records.

The early electron/Z/two-Higgs completion failed before the source-certified
repair. Its original 415-direction solve timed out. Controlled exact replays of the
retained 325-direction physical space, its full 508-direction affine union,
and a 758-direction selected-source extension found contradictions across three
model-consistent parameter assignments. These exclude a single constant
coefficient vector in those particular finite spaces, not a physical amplitude.
The independent complete off-shell Feynman expression agrees with all ten
factorization cuts at all three assignments: 120 valid charts and 1,440 exact
coherent-spin comparisons. The targets are therefore supported at those points.

A further diagnostic grouped the saved 325-direction equations by their actual
parameter metadata. Each assignment separately fits with rank 194 and 131 free
coordinates, including all 1,080 rows at the initial assignment and 480 rows at
each other assignment. All 2,040 equations were reproduced exactly. This points
to investigating the model-parameter factors carried by the source tensors
before expanding the kinematic space again. It is sampled evidence about this
space, not a validated new expression or a proof at arbitrary parameters.
Those matrix diagnostics were not promoted into an amplitude. The later
source-certified construction and ordinary native/CalcHEP/MadGraph validation
resolved this process. The earlier scripts, matrices and certificates remain
in `diagnostics/eEZhh/`, including `physical325/`, `affine-union/`,
`selected-source-union-preflight758/`, and `physical325-parameter-blocks/`.

The early identical-four-electron/one-Higgs pilot also stopped in factorization
completion, both at default inputs and in a controlled rational-parameter retry. A replay of the saved 843-direction physical space found an exact
all-column contradiction in 1,792 equations across three assignments. The
independent complete 28-diagram Feynman expression agrees with every one of its
16 cuts at all three assignments, with 3,072 exact coherent-spin comparisons.
Those early attempts did not produce a validated result. The source-certified
repair subsequently completed this case and all its pure charged-lepton/Higgs
siblings, with ordinary external comparisons. The failed attempts remain under
`diagnostics/pure-charged-lepton-higgs/`.

An additional parameter test exposed a limitation in the previously accepted
distinct-electron/muon/Higgs expression. With the expression, channels and
fresh Feynman benchmark unchanged, the public override `"Mm" -> 1/2000`
(equal to the declared electron mass) gives exact failures at two Z cuts and
in the full Feynman comparison. The original model values still pass. The
source had genuinely passed all three recorded, distinct completion and
Feynman parameter assignments, so this is not an ignored-option diagnosis.
Its original validations remain evidence at those recorded inputs. That
superseded expression is not a certified identity in arbitrary masses and was
replaced by the source-derived, symbolically coupled form.

The proposed identical-flavor assembly has exact diagram coverage and sign
certificates in both backends: two disjoint sets of 14 diagrams with relative
minus sign cover the 28 target diagrams. Its failed validation is already
present in the mass-degenerate distinct-flavor source, before antisymmetrizing.
Neither of those early assembly candidates was accepted or exported. Fresh
target Feynman conversion was exactly identical to the earlier symbolic cache.
Evidence and exact counterexamples remain in
`fermion-flow-reuse-20261112T195125/` beneath the four-fermion/Higgs diagnostic
directory. The subsequent source-certified method keeps the masses and
couplings symbolic, passes the additional equal-mass probe, and supplies the
validated current expressions. The rejected numeric-fit source is not reused.

The initial five-gluon bottleneck was color projection. A bounded stage probe
measured quick graph generation and an 83-second unreduced conversion, then a
timeout in the original color projection. It found 3,960 tensor occurrences
but only 45 literal distinct tensors. C4 implemented exact duplicate sharing
and sparse tensor-network contraction, preserving every scalar contribution,
original coordinate row and sign. C5 added certified graded identical-leg
transport between color coefficients, with fresh amplitude checks. These are
production changes, not merely proposed optimizations. The original timing
record remains at `diagnostics/five-gluon-stage-timing/findings.json`.

The C5 actual default `+++--` call completes in 180.3 s with recorded complexity
14. Its canonical import preserves the actual expression, prints, options,
seed and timing rather than rerunning or replacing the amplitude. The
`++---` default attempt remains incomplete and its full failed report is
preserved. No opposite-helicity or unattempted sibling is marked validated
from the successful result alone.

An independent C5 source proposal for ZZZZh passed all ten physical cuts,
reference independence and the full Feynman comparison at all three parameter
assignments. Its third high-energy check completed three of four exact Laurent
directions, all passing, before reaching its shared finite deadline. The
constructor correctly rejected that unfinished proposal and completed the
ordinary fallback. This explains most of the 860.1 s run without indicating
an amplitude mismatch. Metadata and exact retained timings are under
`diagnostics/ZZZZh-C5-source-validation/`. No check was disabled or relaxed.

These timings are concurrent campaign logs, not a controlled performance
benchmark. The tracked progress ledger is consolidated at batch boundaries;
individual workers also save their own checksum-bound records immediately.


## Notebook display cache

The notebook builder keeps a local display-only cache under
`tests/results/SM-5-point/notebook-display-cache/`. It verifies the actual
calculation capsule, builder version and cached payload checksums before reuse.
Exact amplitudes, captured validation tables and explicit controls are retained;
solver diagnostics remain in the original capsules. Failed or unresolved runs
show a compact actual status, stage, reason and exact WXF path. An optional,
disabled Import command can be copied into an input cell to inspect the full
report. Oversized failed diagnostic previews, including Datasets over 1 MiB,
are omitted with an explicit note; their original data are not changed.
Successful formulas and validation tables retain their normal exact display.
This cache can be deleted safely. It never supplies validation evidence or a missing calculation result.


## One exporter-only compatibility checkpoint

The reviewed helicity-closure update changes only
`source/internal/HEPCAT-SPINAS.wl`. A finite compatibility record preserves
native calculations already saved before that update. It checks the archived
full source/model/reference/runtime manifest, including all tracked paths and
program locations, and permits exactly the pinned old/new exporter hashes.
Every other byte must match. Each admitted old native or export record must
also match its prepatch summary and capsule hashes. Newly written old-context
records cannot enter through this exception.

An old native record keeps its original `ContextSHA256`; compatibility does
not claim that its checks ran again. An old export remains a historical
validation report from the old exporter. Its preserved report and capsule do
not, by themselves, identify today's mutable generated C++ files or binaries.
Existing extended-precision evidence continues to verify its own exact
project, library, point-stream and raw-report hashes at the report's original
context. A new export records the current `ExporterContextSHA256`, each
input's actual `NativeCaseContexts`, and their unchanged capsule hashes.
These bindings are checked before and after exporting.

That exporter transition is already sealed. The original sealing command was
the following, with the reviewed checksum substituted; it is not a way to
approve a later unrelated source change:

```sh
python3 tests/SM-5-point/stage_context.py --seal-exporter-sha REVIEWED_SHA256
```

Sealing refuses any second changed file or a second transition. It archives
the exact postpatch manifest and pins its checksum. Later source, model,
reference, runtime or tracked-path changes close the bridge and require fresh
validation. The ordinary runner records the new full context when starting a
batch. Neither sealing nor metadata admission calculates an amplitude or
runs an external validation.

## Certified generation transport candidates

`generation-transport.wl` is a campaign helper for reusing a known amplitude
under the inventory's bijective generation maps. It verifies the map against
every elementary vertex and particle role, checks the exact model source files,
and records their checksums with the complete map. Typed leg permutations move
spinors, momenta, mass rules and color indices together. The fermion reference
sign follows the separately ordered particle and antiparticle blocks.

Its result is deliberately `CANDIDATE`. It does not construct a new amplitude,
run checks or mark another process validated. A campaign worker must bind the
saved source capsule, then run fresh target factorization, high-energy,
reference and independent Feynman checks, followed by CalcHEP and MadGraph
comparisons. The notebook must show that transport and its actual checks rather
than attach reused output to a default `ConstructAmplitude` call. The focused
`sm-generation-transport-tests.wl` suite checks all 36 elementary maps and five
strict-phase four-point examples without relying on saved five-point capsules.


The optional `supplemental-probes.json` registry binds additional parameter checks
to the exact source amplitude, probe capsule, JSON summary and control scripts.
The notebook preserves the original results and shows the actual extra table
beside a literal input for repeating the check. Missing or changed evidence
produces an unavailable note and no supplemental result. A recorded failure is
a limitation of that saved formula at the additional inputs; the probe never
overwrites its construction or export records. `build-notebook.wl --probes FILE`
selects another registry for a separate notebook.


The historical `covariant-current-sources.wl` campaign prototype is retained
for isolated derivation and regression work on five legs
containing four massive color-singlet Dirac fermions and one scalar. It matches
every used elementary vertex symbolically to a minimal covariant current,
preserves model-owned masses and couplings, and checks ordered vertex ports,
physical propagators and fermion endpoint flow against every original diagram.
Internal lines remain off shell. Used contact or nonminimal vertices are rejected;
no independent reference model, saved amplitude or numerical coefficient fit is
used to generate the candidate.

The result is `UNVALIDATED_CANDIDATE`, with its external mass rules and certificates.
It still requires the ordinary independent factorization, high-energy, reference
and Feynman checks, then the requested CalcHEP and MadGraph comparisons. A local
vertex certificate alone does not establish a complete amplitude or update the
campaign ledger. The opt-in `covariant-current-source-tests.wl` suite exercises
synthetic scalar/pseudoscalar models, model and namespace isolation, particle and
coupling renames, unsupported vertices, and fresh distinct/identical fermion
diagram coverage without importing saved amplitudes.


## Source-certified constructor proposals

The frozen C5 package uses the certified-current method as an optional early
stage of ordinary `ConstructAmplitude` calls with an independent Feynman model.
Its supported five-leg classes are four massive color-singlet Dirac fermions
with one scalar; two such fermions with either one massive vector and two
identical scalars or two massive vectors and one scalar; and four massive
color-singlet vectors with one scalar. Used cubic vertices
must match the minimal covariant templates exactly.
Every ordered graph, physical propagator and fermion endpoint must be covered;
internal momenta remain off shell throughout the conversion.

For a supported metric-current class, an existing metric quartic continuation is fixed from the
complete known four-point amplitude in the factorization model after subtracting
the chosen source-derived cubic exchanges. This is a symbolic reconstruction of
a declared operator, including all nine coherent vector-spin components. It does
not fit sampled coefficients, introduce an independent contact interaction or
read the independent Feynman benchmark to construct the candidate.

Candidates receive external-only preparation and all the normal checks at the
requested parameter assignments. Default inputs with distinct independent Dirac
mass parameters additionally test an equal-mass assignment. A rejected or
unsupported proposal falls through to the existing completion path. These
restrictions matter: the frozen C5 source extension does not certify chiral
massless fermion currents, three-vector currents or colored currents. Later
prototype work is outside the C5 coverage claimed here.

The actual public-call pilots and canonical workers are separate records. Pilot
timings are not substituted for notebook timings. The canonical worker writes
compressed WXF; new integration captures do the same. Both preserve the exact
expressions, check reports, options and seeds. The additional equal-mass probe
has now been rerun on the repaired distinct-lepton expression, with fresh full
Feynman diagrams, and all 13 rows pass. Its superseded failure remains in the
supplemental registry's history and the original artifacts are unchanged.

## Finite constructor-history compatibility

The initial C3 `constructor-context-bridge.json` retains a fixed set of earlier
validation records across that reviewed constructor-only update. Two independent cold
loads before and after compare all ten definition fields of every existing
package symbol. Mathematical definitions outside `ConstructAmplitude` are
unchanged; its usage message, the exactly recomputed loader hash cache and
empty temporary local symbols are explicitly accounted for. New private source
modules were separately reviewed for side effects and model isolation.

Each admitted historical summary and capsule must still match its frozen
checksum. It keeps its actual source context, options, seed and timing. This
admission neither reruns the constructor nor asserts that today's constructor
reproduces the historical result. Existing counterexample guards and the strict
precision-evidence checks remain mandatory. New canonical constructions and
exports record the current context and actual input capsules normally. Any
unreviewed source, model or runtime change closes this finite exception.


The subsequent C4 transition additionally reviews three private colour helpers.
Primitive delta, generator and structure-constant tensors can be contracted as
exact sparse arrays. Literal repeated tensors share that work, while every
original scalar contribution and coordinate row is retained. Unsupported index
incidence uses the original enumerator. Only projection diagnostics omit the
large repeated Gram matrix; the unique Gram and occurrence map reconstruct it.
The finite-sum argument, 65 exact controls,45-tensor rank-six certificate and 360
production checks are bound to the C4 review. This retains historical evidence;
it does not claim a fresh full five-gluon construction.

The ordinary default electron-pair/ZZ/Higgs call now passes all standard native
parameter assignments and 1000-point CalcHEP and MadGraph comparisons. Its actual
public-call record is imported with exact expression/prints/seed/timing equality
and explicit provenance; the constructor was not rerun merely to change its
storage location. The canonical numerical export is a fresh normal writer run
in the standard output directory. A separate same-stream CH precision study
regenerated the original binary64 stream, reproduced the original native
aggregate output, and verified every regenerated native value bit-for-bit.
On those same 1000 momenta, the maximum relative difference fell from 5.68e-8
at native precision to 2.20e-13 at 113 bits. Raw results and tolerance are retained;
this optional study is not used to relabel a raw comparison or construct an
amplitude.


The C5 transition is separately sealed in `constructor-C4-C5-context-bridge.json`.
It reviews retained fitted-point checkpoints and certified graded color
transport, together with the metadata-only four-vector/scalar admission.
A recorded sparse fit is not itself a validated amplitude: a particular point
is accepted only after ordinary external preparation and every mandatory
independent check. Known failed held-out point rows cannot be overridden by
new favorable samples. Unfinished affine-family classification does not become
a claim of uniqueness. The exact C5 dependency manifest binds its cold
captures, code review, controls and prior proof chain; later development is
not admitted through that sealed transition automatically.


## Recorded C7 coverage

The C7 checkpoint uses source context
`4165f24c7d5501c40c03ea91f850a5cde77389a1236e3af397fb6d7e2344a167`.
It records **245 native-complete cases**: **237** with strict ordinary
CalcHEP/MadGraph validation and **8** with the separately reviewed precision
qualification. The latter retain their raw comparison results. This is partial
coverage of the 1,422-case inventory, not a claim that every SM five-point
amplitude has been completed.

C7 adds 24 quark families, containing 30 explicit amplitudes: six quark-pair
Zhh families, six quark-pair ZZh families, six photon/hh families with both
photon helicities constructed independently, and six charged-vector/hh
families. Every new case uses the current source, retains all three normal
parameter validation groups and passes physical residues, automatic spurious
poles, high energy, reference independence and the independent full Feynman
comparison. Each family also passes 1,000 phase-space points against both
CalcHEP and MadGraph in the normal canonical export.

There were 29 fresh ordinary worker constructions and one exact adoption of
an already completed actual public default uUZhh call. The adoption preserves
its expression, reports, prints and original timing; no constructor rerun is
claimed. Earlier C6 records remain historical evidence admitted through the
reviewed finite compatibility chain. They are not relabeled as fresh C7 runs.
The frozen cohort summary and per-record hashes are in
`tests/results/SM-5-point/diagnostics/C7-spectator-cohort-20261113/summary.json`.


## Recorded C8 coverage

The C8 checkpoint uses source context
`48d82ad2adaf29f9a338c9fb6b9cf3da37ed7570f6de5852ee50c798fd0d8363`.
It records **251 native-complete cases**: **243** with strict ordinary CalcHEP
and MadGraph comparisons and **8** with separate precision qualification.
Their raw comparison results are retained. There are 172 ordinary validated
families and three precision-qualified families; most of the 869-family work
list remains pending.

C8 adds all six combinations of two massive charged-lepton pairs and one Z.
The source proposal matches every used elementary current symbolically and
preserves the ordered diagrams, physical poles and fermion endpoints. Its
massive-vector tensor is encoded in the same symmetric spin polynomial as
other vector amplitudes, with the raw tensor retained in Details. Four-fermion
photon processes keep their previous construction path. No new public option
or numerical coefficient fit is introduced.

Two cases preserve byte-for-byte capsules from actual public calls using the
reviewed isolated source tree; the canonical import does not rerun construction
or native validation. The other four use the ordinary campaign constructor.
All six retain their normal three validation assignments, plus the automatic
equal-mass assignment for distinct independent fermion masses, and every final
native/Feynman row passes. Every actual canonical output was then exported and
compared afresh with both CalcHEP and MadGraph at 1,000 common points. The
separate prototype backend comparisons are not substituted for these runs.
The exact records and hashes are in
`tests/results/SM-5-point/cohort-C8-four-fermion-Z-summary.json`.

Earlier records retain their actual source contexts and timings through the
finite, independently reviewed C7-to-C8 compatibility proof. The checkers,
models, Feynman converter and numerical exporter are unchanged. This preserves
historical validation evidence without claiming that old constructors were
rerun under C8. The additional equal-mass parameter probe was rerun with fresh
independent Feynman diagrams and passed all 13 rows under C8.
