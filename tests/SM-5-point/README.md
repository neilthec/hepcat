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

A separate checker-budget limitation is recorded for that W-pair family. Its
independent converted benchmark has 2,591,628 leaves, whereas its constructive
candidates have only hundreds. The automatic checker metric currently sizes its
clock using the candidate and channels. Two default attempts therefore retained
Feynman UNRESOLVED rows despite passing the other native checks. The original
failures, unchanged candidates, complete benchmarks, and controlled checks with
larger finite limits are saved under `diagnostics/WWAAA/`. A separate successful
candidate check is not labeled as a successful default constructor. Actual
public constructor retries with explicit limits have their own scripts and
output records.

The electron/Z/two-Higgs process remains unresolved in factorization completion.
Its last retained fit did generate all 600 equations for 415 unknowns; the first
exact linear solve timed out. The coefficient field has degree four. Neither
distinct-coefficient simplification nor conversion to one common number field
finished the solve cheaply. A rational-parameter diagnostic also found an exact
inconsistency in this particular final fallback space across its three test
assignments. This does not establish that the separate physical-only proposal
is insufficient. The relevant stored four-point electron/Z/Higgs, two-electron/
two-Higgs and two-Z/two-Higgs amplitudes all pass independent native checks at
an order-one, model-consistent input set. No model entry was changed. The
scripts, matrices, certificates and qualified conclusions are in
`diagnostics/eEZhh/`.

The five-gluon attempt is separately incomplete. A bounded stage probe measured
quick graph generation and an 83-second unreduced conversion, then a timeout
in color projection. There are 3,960 tensor occurrences but only 45 literal
distinct tensors. Exact deduplication with an occurrence map is a candidate
improvement; it must preserve every original scalar coefficient and sign.
The color implementation has not been changed. See
`diagnostics/five-gluon-stage-timing/findings.json`.

These timings are concurrent campaign logs, not a controlled performance
benchmark. The tracked progress ledger is consolidated at batch boundaries;
individual workers also save their own checksum-bound records immediately.


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

The bridge starts pending. After reviewing and freezing the exporter, seal
this one transition with the exact reviewed file checksum:

```sh
python3 tests/SM-5-point/stage_context.py --seal-exporter-sha REVIEWED_SHA256
```

Sealing refuses any second changed file or a second transition. It archives
the exact postpatch manifest and pins its checksum. Later source, model,
reference, runtime or tracked-path changes close the bridge and require fresh
validation. The ordinary runner records the new full context when starting a
batch. Neither sealing nor metadata admission calculates an amplitude or
runs an external validation.
