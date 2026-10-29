# Working With HEPCAT

Use this package to carry out reproducible symbolic calculations. It is not
merely a checker for whether two supplied expressions are equal. Tasks include:

- Simplifying one expression or diagram using identities, without a known target.
- Deriving and combining diagram amplitudes into a compact final amplitude.
- Exploring diagram-local numerator representatives to find a simple form that
  satisfies physical-factorization and spurious-pole checks.
- Comparing proposed formulas when an equality question actually arises.

Read and execute the tools, preserve the reasoning and intermediate expressions,
and state each step's assumptions. Do not use em dashes.
Show ordinary Mathematica expressions. Do not add `// nice` or `nice[...]`
formatting unless the user explicitly requests it.

This file is in `source/`. For tasks started elsewhere in the repository,
explicitly read `source/AGENTS.md` before using this workflow. All paths below
are relative to the repository root, not to this file.

## Start Here

- `tests/SIMPLIFICATION.md`: practical guide to identities, on-shell scope,
  worked derivations, and the current checker API. Read this first.
- `source/HEPCAT.wl`: package loader; loads the source modules below.
- `source/HEPCAT-Parameters.wl`: inert model-local parameters and functions,
  parameter lookup, local numerical rules and generic parameter formatting.
- `source/HEPCAT-Base.wl`: spinor algebra, `ReduceSpinContractions`,
  `ReduceSpinorProducts`, `SimplifySpinorProducts`, `applyToNumerator`, and the
  section **Symbolic Spinor Identities** containing explicit identity edits.
- `source/HEPCAT-Model.wl`: model loading, diagram construction, factorization,
  and **Physical Residue Diagnostics**, including spurious-pole, high-energy,
  and reference-independence checks; separate bounded preparation and
  amplitude-completion stages are also defined here.
- `source/HEPCAT-Feynman.wl`: CalcHEP model parsing, unitary-gauge tree
  amplitudes and exact Feynman-to-spinor conversion. See
  `model_feynman/README.md` for the imported SM's scope and usage.
- `source/HEPCAT-SPINAS.wl` and `source/spinas-runtime.hpp`: complete C++
  process export, SPINAS linking, spin-state projection and physical example
  momenta. The older `SPINAS` expression-formatting helpers remain available.
- `tests/simplification-paths.wl`: curated four-point identity sequences.
- `tests/SM-4-point-simplification-test.nb`, `tests/QED-5-point.nb`, and
  `tests/Factorization-checks.nb`: worked examples and user-facing usage.
- `tests/QED-5-point-residues.wl`: explicit five-point QED routes and fixtures.
  These helpers are process-specific examples, not general simplifiers.
- `papers/`: physics conventions and identity references. Consult the relevant
  papers, especially `SM_Vertices.pdf`, `SM_4_Point_Amplitudes.pdf`, and
  `QED-Challenges.pdf`; do not assume familiar conventions from another package.

The active symbolic workflow does not require neural models or training. The
earlier machine-learning simplifier is preserved on the `ml-simplify` branch.

## Load and Run

Determine the absolute repository root and assign it to `hepcatDirectory`.
Use a working, licensed Wolfram kernel, then load the package as follows:

```wolfram
Global`$HEPCATpath = FileNameJoin[{hepcatDirectory, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
```

Evaluate package loading before parsing code that uses its symbols. In a
notebook, use a separate setup cell; in a script, load at top level before
defining calculation modules. Wrapping loading and all usage in one initially
parsed `Module` can create conflicting `Global` symbols and shadowing messages.

Run symbolic calculations in a separate kernel/script when practical. On macOS
the usual direct executable is
`/Applications/Wolfram.app/Contents/MacOS/WolframKernel -script script.wl`.
Check local availability; do not change licensing or terminate the user's
interactive Mathematica kernel to make a test run.

For example, the existing five-point electron/photon QED setup is:

```wolfram
particles = {"e", "E", "A.+", "A.+", "A.+"};
diagramModel = ReadModel[FileNameJoin[{hepcatDirectory, "models"}], 2];
factorizationModel = ReadModel[FileNameJoin[{hepcatDirectory, "models"}], 3];
diagrams = CreateDiagrams[particles, diagramModel];
pairs = diagramAmplitudes[diagrams];
```

`pairs` contains `{massRules, expression}` for each diagram. Use the plural
`diagramAmplitudes` public entry point for complete diagram calculations.
`CreateArrowDiagrams[diagrams]` displays the diagrams.

## Independent Feynman Benchmark

`ReadModel` also recognizes a CalcHEP `lgrng*.mdl` model and returns
`FeynmanModel[particles, vertices, variables, functions]`, not the constructive
model's four-element list. The four parts keep their positional organization.
Use the same `CreateDiagrams` and `diagramAmplitudes` entry points; the explicit
model head selects Feynman tensors and ordered Dirac chains. For example:

```wolfram
feynmanModel = ReadModel[FileNameJoin[{hepcatDirectory, "model_feynman"}], 1];
feynmanDiagrams = CreateDiagrams[{"e", "E", "A.+", "A.-"}, feynmanModel];
feynmanPairs = diagramAmplitudes[feynmanDiagrams];
spinorPairs = FeynmanToSpinorProducts[feynmanPairs,
  "TimeConstraint" -> 60];
preparedFeynman = PrepareAmplitudes[feynmanPairs, feynmanModel,
  "ReferenceSpinors" -> "IndependentXi"];
```

Keep this benchmark separate from diagram-local constructive numerator choices.
All momenta are ingoing; internal Feynman numerators and denominators remain
off shell. Conversion uses the paper's external-spinor conventions and ordinary
algebra only. Do not add internal-shell substitutions or adjust a sign to force
agreement. Massless Dirac external fermions require explicit helicities.
Massive vectors retain two symmetric `VectorSpinIndex[leg, slot]` labels.
Explicit color tensors use repeated-index sums and are not automatically summed
over external colors. The `G.t` tensor auxiliary has a constant propagator
encoding the four-gluon contact interaction; it is not a physical pole.

The copied SM files preserve all raw rules, including unphysical rows. Physical
tree generation uses the user's chosen unitary W/Z gauge, with the appropriate
longitudinal propagator numerator, and excludes Goldstones and ghosts. It does
not discard Goldstone exchange while retaining a Feynman-gauge massive-vector
propagator. Majorana and unsupported higher-spin cases return `Failure`.
The local SM restores e, muon, u and d masses together with their Higgs and
Goldstone couplings, matching the existing massive-generation rules.
The light-quark defaults are fixed MSbar inputs at 2 GeV, shared with the
constructive model files, not automatic running or pole-mass measurements.
Neutrinos remain massless. External running-mass
functions such as `MbEff` are imported symbolically, not implemented numerically.

Raw Feynman vertices are not complete lower-point amplitudes. Continue to use
the constructive model containing known lower-point amplitudes for
`FactorizationAmplitudeChannels` and the checker. The Feynman result is an
independent off-cut benchmark, not replacement factorization data.
`PrepareAmplitudes` also accepts a `FeynmanModel` and converts raw tensors before
ordinary spinor reduction. Feynman pairs contain only external mass rules, and
preparation never infers internal shell assumptions from their propagators.
For a Feynman model the effective default is `"MaxSteps" -> 0`: conversion and
ordinary reductions still run, while expensive identity-route enumeration is
opt-in. Explicit positive `"MaxSteps"` enables that search; its candidate
generation is eager and may exceed the useful budget on long chains.
All diagrams must use the same reference choice for each external gauge leg:
individual Feynman diagrams are gauge dependent. The default uses a shared
fixed massless-particle cycle where possible, not independent per-diagram
reference optimization. `AllPhysical` uses a deterministic momentum-insertion
reference per external leg, chosen from the shared process pole inventory and
preferring massive external targets. Its reference poles may coincide with a
diagram's propagators: no internal-shell identity is used. Shared `Mixed`
references remain unimplemented and report `PARTIAL` in details. An unavailable
requested reference choice returns `Failure` in ordinary output rather than
silently retaining the Xi spinors.

## Complete Process Workflow

Use `ConstructAmplitude[process, diagramModel, factorizationModel]` when the
user wants the complete construction. It generates and prepares diagrams and
channels, completes factorization and high-energy behavior, prepares the
completed expressions with external identities only, then runs all mandatory
final checks against the original channels. Optional `"FeynmanModel"` adds an
independent benchmark. Default output is the validated bare expression;
`"Output" -> "Amplitude"` keeps external masses and `"Details"` retains stage
results, timings and the complete checker report. Failures and unresolved
checks must remain explicit. See `tests/SIMPLIFICATION.md` for stage options.

The wrapper displays live stage and total times, then prints total time and
the test table before returning. Preserve prepared-channel provenance and
factorization-family freedom when modifying this workflow. Do not replace its
original residue targets with prepared continuations, apply internal shell
conditions after combining diagrams, disable a failing final check, or inject
a process-specific known expression to make the wrapper appear successful.
`tests/QED-5-point-construction.nb` uses one wrapper call per process; earlier
manual calculations remain in collapsed groups.

## Parallel Calculations

`PrepareAmplitudes`, `CompleteFactorization`, `CompleteHighEnergy`, and
`CheckAmplitudes` accept `"Parallelization" -> Automatic` and
`"MaxKernels" -> Automatic`. Automatic preparation dispatches independent
items; small checker jobs stay serial, and short completion sample stages use
parallelism after the package worker pool is warm. `"Parallelization" -> True`
requests workers explicitly, `False` keeps the whole operation serial, and
a positive `"MaxKernels"` reduces worker count. One worker means serial
execution. The default uses Mathematica's `$ProcessorCount`, the number of
jobs and available license slots, accounting for unrelated user kernels.
There is no fixed two-worker limit or expression-size memory estimate that
silently reduces that count.

Independent jobs use a dynamic queue: a worker receives its next item as soon
as it finishes, before the main kernel invokes its progress callback. A slow
item must not hold up other workers behind a wave barrier. Returned results
retain input order; callbacks arrive in completion order with their original
item indices and monotonically increasing completed counts. Transfer immutable
closures once per worker and only the next job payload thereafter. Retry
unfinished jobs on the main kernel after the parallel queue drains; do not
stall healthy workers behind a serial retry.

Workers load the saved local package sources and receive explicit, call-local
model data. They do not inherit arbitrary definitions from `Global``. Custom
kinematic maps stay on the calling kernel. Progress callbacks also run on the
calling kernel. Existing user workers are left separate from the package pool.
Use a separate test process that permits local worker communication; startup
can fail in a restricted process even when the main Wolfram kernel is licensed.
Failures to launch fall back to serial work and are recorded in details. If
saved sources have changed since loading, reload HEPCAT before using workers;
the package keeps the current call serial to avoid mixing code versions.

Feynman references are chosen once for the complete input before dispatch and
reused by every diagram. Never optimize Feynman references independently on
workers. Constructive diagram and factorization-channel numerator conditions
remain local to each item. Completion keeps the global simultaneous solve
serial and parallelizes independent exact cut samples and held-out validation;
this preserves sample seeds, all physical obligations, and the shared deadline.

Ordinary amplitude and table outputs stay unchanged. Request preparation
`"ReturnDetails" -> True` or checker `"Output" -> "Details"` to inspect the
`"Execution"` entry, including `"Scheduler" -> "DynamicQueue"` for parallel
work. Completion details retain execution data in the sample
constraint and validation reports. Cold worker startup adds several seconds;
benchmark both cold and warm calls before claiming a speedup. Wall-clock-limited
heuristic searches can stop at different valid intermediate forms under
different loads; compare identical work with sufficient per-item budgets.

## Simplify, Explore, and Derive

1. Establish the actual objective and supplied input: an expression, one diagram,
   or a complete process. A known final formula is not required. For a process,
   confirm external ordering/helicities, models, and momentum conventions.
   Preserve the original expressions, diagrams, and amplitude pairs.
2. Keep each diagram's mass rules attached to that diagram. Use
   `ReduceSpinContractions` where appropriate and record any preparation.
3. Search for useful identity moves using mathematical reasoning. Enumerate supported
   edits with `SpinorIdentityCandidates` and apply them with
   `ApplySpinorIdentity`, using the same options in both calls.
4. Record the identity name, assumptions, before/after expressions, and the
   verification result. Regenerate candidates after changing an expression.
5. Explore alternative routes when a sequence stalls; useful cancellations may
   require expansion or a temporarily more complicated intermediate expression.
   Compare and retain the best justified forms rather than following only the
   first available move or stopping at the first local minimum.
6. For a full process, combine diagrams after retaining their individual
   derivations. Where necessary, explore explicitly justified diagram-local
   numerator choices and use the physical checks to reject invalid final forms.
   Investigate reference dependence and spurious-pole cancellation. Do not
   assume a locally allowed numerator choice is the correct full continuation.
7. Match verification to the task. An isolated symbolic simplification needs
   correctly scoped identities, not an invented factorization model. A complete
   physical amplitude with known lower-point data should also pass the channel
   and spurious-pole checks below.
8. Deliver executable Wolfram code and a readable derivation using angle and
   square brackets. Report unresolved steps honestly; do not substitute a known
   final answer without deriving the connection or claim a globally simplest
   form without evidence.

The basic identity API is:

```wolfram
pair = First[pairs];
edits = SpinorIdentityCandidates[pair];
If[edits =!= {},
  selectedEdit = First[edits]; (* Example only, not a recommendation of the best move. *)
  nextExpression = ApplySpinorIdentity[pair, selectedEdit];
  If[nextExpression =!= $Failed,
    nextPair = {First[pair], nextExpression}]];
```

For an isolated expression, start with its external mass rules instead:

```wolfram
expressionPair = {externalMassRules, expression};
edits = SpinorIdentityCandidates[expressionPair, "OnShellChannels" -> {}];
```

Use the same `"OnShellChannels" -> {}` when applying these edits.

`ApplySpinorIdentity` returns an expression, not a new pair. It checks that an
edit belongs to the implemented legal moves for the current context. It is not
an independent theorem prover for arbitrary before/after expressions.
`SchoutenRewrite` and `SchoutenCandidates` expose generalized Schouten moves;
`SpinorExpressionComplexity` is a heuristic measure, not a proof of minimality.
Use it alongside term count, momentum insertions, pole structure, and readability.
Removing momentum insertions can be useful even when the term count temporarily
increases. Explain which form is better and why; do not equate one numerical
score or a passing check with the globally simplest expression.
If a required identity is missing, derive and independently verify it before
using it. Ask about uncertain conventions; do not silently invent a rule.

## Physics and Scope Rules

- Analytic amplitudes use all momenta ingoing. Crossing to numerical conventions
  must change spinors as well as momenta. Use `ReverseMomentum` and the papers;
  reversing only momentum insertions is insufficient.
- Internal on-shell identities are important and allowed in an individual
  diagram's numerator. `"OnShellChannels" -> Automatic` obtains these conditions
  from that diagram's internal mass rules. Never merge different diagrams'
  internal shell assumptions into a shared rule list.
- Keep `PropDen[Mom[Multiparticle[...]], mass]` denominators off shell. An internal
  numerator shell condition does not license setting a physical denominator to
  zero or cancelling it against a term discarded using that condition.
- For a summed amplitude or a generic off-cut equality comparison, use only
  external mass rules and pass `"OnShellChannels" -> {}` to both identity calls.
  Momentum-conservation identities are enabled by default; the option
  `"MomentumConservation" -> False` disables them independently.
- `ReduceSpinorProducts` itself has no momentum-conservation assumption. Its
  multilinearity, long-chain reversal, Clifford and trace reductions use
  ordinary spinor algebra and external mass identities. Preserve explicit spin
  indices and chiral trace ordering; do not introduce inverse rewrite pairs.
- A standalone scalar `Mom[label]^2` means the Lorentz invariant, evaluated
  as the determinant of the two-by-two momentum matrix, not its componentwise
  square. During completion's external reduction, expand composite momenta
  only in typed chain and trace insertion positions. Do not globally turn a
  scalar composite square into a commuting polynomial in bare `Mom` objects.
- A numerator change that vanishes on a diagram's cuts can change its off-cut
  continuation and the single-pole residues of the full sum. Record that change
  and any off-cut remainder explicitly. Do not label it an ordinary off-cut
  identity; validate the resulting full amplitude in all channels.
- `applyToNumerator[expr, rules]` acts on the numerator of each existing additive
  term without `Together` or `Expand`. Preserve this behavior. The older
  reduction/simplification routines apply many identities internally; calling
  one is not, by itself, a transparent elementary derivation.
- Keep reference-spinor choices explicit and consistent when summing diagrams.
  Distinct Xi labels are evaluated literally by the checker. Do not silently
  identify them or assume that changing reference representatives preserves an
  unfinished off-cut continuation.
- Relabel particle indices only in their typed kinematic positions, including
  multiparticles. Never globally replace integers: that also changes numerical
  coefficients, powers, spin indices, and auxiliary reference labels.

## Models and Final Checks

Use a vertex model for diagrams and a model containing complete known
lower-point amplitudes for factorization. Do not treat a known four-point
amplitude as a new diagram vertex. In the current QED examples, model 2 is the
cubic diagram model and suffices for four-point factorization; model 3 includes
the complete four- and five-point electron/photon amplitudes needed for
five- and six-point factorization. For other
processes, inspect the model rather than assuming these example choices apply.

Model parameters are inert data stored in the model, not globally assigned
symbols. Use `ModelParameter[model, "Me"]` to retrieve a parameter,
`ModelParameterName[parameter]` for its name, and `ModelParameterRules[model]`
for local numerical substitutions and resolved dependent functions. Preserve
these scoped objects in derived expressions. Do not create symbols or install
per-parameter formatting definitions, assign global masses/couplings, or identify
two parameters solely because they display the same name. For local studies,
string overrides such as `"ParameterRules" -> {"Me" -> 1}` are interpreted only
against the explicitly supplied model. Keep each model alongside its amplitudes.

The factorization model is authoritative: amplitudes absent from it count as
zero. Ensure the user supplied the intended lower-point data. Do not fabricate
zero rows, insert fitted amplitudes, or add independent QED contact interactions
to force a comparison to pass. Respect the specified theory's vertex content.

Save a verified amplitude with `AppendModelAmplitudes[directory, number,
{{particles, amplitude}}]`. A batch may also use
`{particles, factor, amplitudeOverFactor}` entries. The function reads the
target model, validates its particles and model-local parameters, checks text
round trips, and appends atomically before the final separator. Identical
entries are skipped; conflicting particle multisets fail without overwriting.
Reload `ReadModel` afterward. This is serialization, not a physics validation:
run `CheckAmplitudes` first. Do not add the saved amplitudes to the cubic
diagram model as new interactions.

`PlainText[expression]` is human-readable display text.
`PlainText[expression, "Model"]` is the unambiguous model-file form, retaining
compact brackets where possible and using typed notation for general labels
or explicit spin indices. Unsupported objects fail rather than lose data.
The worked `qedFivePointModelAmplitudes[]` helper in
`tests/QED-5-point-residues.wl` reproduces the six independent nonzero QED
five-point rows; `tests/checks/qed-model-export-tests.wl` checks the reloaded
entries, all eight ordered three-photon helicities, massive spins, independent
Feynman amplitudes and availability of six-point sewing channels.

Export a final spinor amplitude with
`WriteSPINASProcess[outputDirectory, particles, amplitude, model]`. Add
`"Build" -> True, "RunExamples" -> True` to compile, link the existing local
SPINAS library and run three conserved physical 2-to-N example points.
`"SPINASDirectory"` selects its checkout; automatic lookup uses `SPINAS_ROOT`
or the repository's sibling `spinas` directory. Model values stay local.
Inputs use all-ingoing conventions; outgoing momenta and angle-spinor signs
are crossed together. Never apply internal-shell identities in the exporter,
alter supplied signs to fit a native example, or replace HEPCAT's explicit
positive imaginary width with SPINAS's negative-width convention.
`amp2` averages incoming massive spins at fixed massless helicities; it does
not sum other helicity processes or include color or phase-space factors.
Resolve auxiliary spinors, shifts and explicit color tensors first.
Unsupported input returns `Failure` before writing project files.
The `spinas-export-tests.wl` suite compiles generated 4-, 5- and 6-point
projects and checks complex values and spin sums against independent native
SPINAS expressions. A mixed-helicity eeAA sign difference between the current
model and native example is tested explicitly, not silently corrected.

```wolfram
channels = FactorizationAmplitudeChannels[particles, factorizationModel];
report = CheckAmplitudes[finalAmplitude, channels, factorizationModel,
  "Output" -> "Details"];
report["Table"]
```

To compare against an independent Feynman result as well, add
`"FeynmanAmplitudes" -> preparedFeynman` and `"FeynmanModel" -> feynmanModel`.
Each expression obtains numerical parameters from its own model.
The same table then includes a full
off-cut comparison, not just agreement of residues. `"Output" -> "Feynman"`
shows amplitude samples, and `report["FeynmanCheck"]` retains the diagnostic data.
No sign or normalization is fitted. Match external masses, ordering, helicities
and parameter conventions between models. Only external shells and conservation
are used. Identically printed model parameters remain model-local data; matching
names do not identify parameters or overwrite their values. Use explicitly
consistent model files for the comparison, never fit or rename parameters to
force agreement. Every evaluated
internal propagator stays nonzero. Samples compare all
implicit massive-spin coefficients and independently chosen explicit Xi references
for the two full sums. `PASS` is sampled agreement, not an analytic equality proof.
The rational-tree full comparison prefers exact rational complex external
momenta, falling back to real center-of-mass samples when unavailable. All
external shells, momentum conservation and nonzero physical propagators are
still required. The separate high-energy test retains real scattering
kinematics. Details record each chart's `SamplingConstruction`.
`CheckPhysicalResidues` remains a compatibility alias for `CheckAmplitudes`.

Numerical checks and completion use the supplied model's parameter values and
derived functions by default (`"ParameterRules" -> Automatic`). The symbolic
amplitude and model remain unchanged. For an optional parameter study, explicit
rules such as `"ParameterRules" -> {"Me" -> 1, "EE" -> 1}` override the model
defaults locally and dependent model functions are recomputed. Use
theory-consistent values; do not assign model symbols numerical values globally.
The same checker also accepts
diagram pairs directly. Its default table combines physical-residue checks,
automatically discovered spurious poles, high-energy growth and explicit Xi
reference independence; no user pole list is required. Request `"Residues"`
for visible pole samples, `"HighEnergy"` for energy samples, `"References"`
for reference comparisons, `"Summary"` for counts, or `"Details"` for the full report.

Automatic denominator discovery removes duplicate exact obligations before
factoring and classification. At integer denominator powers it can expose
bracket factors of chains built only from explicitly massless external momenta,
and reduce their two-momentum traces. This affects only the inventory: the
Laurent checks use the original summed amplitude. Do not add internal-shell
assumptions or discard zero, unsupported or branch-sensitive denominators.

- `PASS` means agreement or finiteness at the recorded samples, not a general
  analytic proof. Vary reproducible samples and parameters as supplementary
  evidence, and distinguish such tests from symbolic proofs.
- `Opp Sign` is a diagnostic, not permission to flip a result. Trace crossing,
  fermion ordering, model signs, and normalization. The checker already applies
  the sewing factor `-1` from `(i A_L)(i A_R)(i/PropDen)/i`; do not apply it twice.
- `FAIL` and `UNRESOLVED` must remain visible. Unsupported denominator forms,
  singular charts, unspecified parameters, and timeouts are not zero results.
- No discovered spurious denominator does not prove regularity everywhere.
  Check equivalence of reference choices using external identities, without
  internal shell assumptions on the complete amplitude.
- The default high-energy bound `E^(4-n)` is the renormalizable-tree hypothesis,
  not an inference about the model. Change `"HighEnergyBound"` for other theory
  assumptions or use `"HighEnergyCheck" -> False`. The exact energy expansion
  uses fixed masses, conserved real momenta and physical massive spin bases,
  including longitudinal states, at sampled generic kinematic shapes. It does
  not rescale masses, reuse arbitrary residue spin projections, or impose
  internal on-shell conditions. Unsupported cases and timeouts are UNRESOLVED.
  Inspect `report["HighEnergyCheck"]` for scope and series coefficients. Passing
  an upper growth bound does not determine acceptable-growth contact couplings
  or replace a Ward identity or reference-independence check.
- Reference independence varies all literal Xi labels simultaneously at
  conserved external on-shell kinematics away from physical cuts, with momenta
  and external physical spin states fixed. It uses symbolic reference
  coordinates at sampled momenta, not only uniform rescalings. Inspect
  `report["ReferenceCheck"]` for shared-reference scope and unsupported cases;
  use `"ReferenceCheck" -> False` to disable it. No explicit Xi gives scope
  `"NoExplicitReferences"`, not a Ward-identity proof. A shared Xi tests the
  shared choice only; never merge or relabel references to force agreement.
  Unexpanded x factors and unresolved calculations must not become PASS.
- `PhysicalPoleResidue` without a coherent kinematic map extracts only a
  `FORMAL` coefficient. Automatic simultaneous-cut charts are not implemented;
  multiple-pole checks require a suitable custom map. Read the guide first.
  Its optional `"StructuralCoefficient" -> False` skips the formal opaque-pole
  coefficient while retaining the mapped physical residue and kinematic checks.
  Mapped checker and completion callers use this to avoid unnecessary symbolic
  preprocessing; the direct public default remains `True`.
- These diagnostics do not automatically repair an amplitude or establish
  uniqueness. Do not modify the checker, model, or signs merely to obtain PASS.

## Bounded Amplitude Construction

The checker is still read-only. Use the three separate construction functions
when amplitude completion, rather than diagnosis alone, is requested:

```wolfram
diagramPreparation = PrepareAmplitudes[pairs, diagramModel, "ReturnDetails" -> True];
prepared = diagramPreparation["PreparedAmplitudes"];
channels = FactorizationAmplitudeChannels[particles, factorizationModel];
channelPreparation = PrepareAmplitudes[channels, factorizationModel, "ReturnDetails" -> True];
preparedChannels = channelPreparation["PreparedAmplitudes"];
factorizedAmplitude = CompleteFactorization[diagramPreparation, channelPreparation,
  factorizationModel];
completedAmplitude = If[FailureQ[factorizedAmplitude] || factorizedAmplitude === $Failed,
  factorizedAmplitude,
  CompleteHighEnergy[factorizedAmplitude, channels, factorizationModel]];
```

Both completion stages normally return one `{externalMassRules, expression}`
pair. Only external `Mass[leg]` replacements are retained, never internal shell
assumptions. A successful pair can go directly into `PrepareAmplitudes` for
further reduction, or into the checker and next completion stage. Preparation
of a single external-only pair keeps that single-pair form and disables internal
shells; propagators supply reference choices only. `"Output" -> "Expression"`
requests the bare expression from either completion stage. Detailed reports
retain separate `Amplitude` and `ExternalMassRules` fields as before.
For a whole amplitude, keep repeated literal Xi references correlated. Never
assign one shared Xi different values in different terms or for different
particles. Automatic preparation may retain a shared Xi when a safe uniform
replacement is unavailable; explicit incompatible choices must fail visibly.

Run the final stage only after inspecting a successful factorization stage.
`PrepareAmplitudes` detects each element: diagram `{massRules, expression}`
pairs or compact factorization rows `{m1, ..., mn, Multiparticle[cut], mass, expression}`.
It preserves the input format and metadata by default. Channel preparation
derives only external mass rules and its declared cut, never shells for other
propagators inside the lower-point amplitudes. The channel's contour factor
and sewing sign remain unchanged. Cut-only reductions preserve its selected
residue, not its off-cut continuation. Pass both preparation reports to completion:
the original channel rows remain authoritative residue targets, while simplified
channels and retained differences supply correction candidates. Plain pairs and
rows remain valid but carry no hidden history. The legacy `"AlternativeChannels"`
option is available when only plain prepared rows were supplied; its selected-cut
checks and remaining time/term limits are unchanged. Mixed formats require consistent
external masses. `PrepareDiagramAmplitudes` is a thin compatibility alias,
not another implementation. Use `"ReturnDetails" -> True` to request the original inputs,
reference assignments and reduction history; this option defaults to `False`.
The report's `RemainderData` separates the exact fixed-reference before/after
difference from reference changes, and retains completed stage differences,
tested reference alternatives and common-Xi sources. Never call these ordinary
off-cut identities or impose a diagram's internal shells on the assembled sum.
`PreparationRemainders -> Automatic` tries grouped retained corrections after
cheap input rescaling and before broader channel spaces, within one fifth of
the remaining stage clock capped at 12 seconds. Construction has at most four
seconds with a smaller internal deadline, and retains partial diagnostics.
Exact early compatibility equations can reject a space but never certify success.
All original targets, zero obligations and coherent-spin/parameter validation
remain mandatory. Ordinary pair-virtuality lifting keeps every propagator off
shell; regular pieces stay tied to complete source groups. A common-reference
sum is a final bounded fallback, requiring the same full validation. None
disables these attempts; explicit bases and literal-channel mode bypass them.
Preserve remainder and fallback diagnostics even when a later space succeeds.
It applies `ReduceSpinContractions`, selects `"ReferenceSpinors"`, then calls
`ReduceSpinorProducts` on the whole diagram expression while protecting physical
propagators. Ordinary external spinor identities may simplify auxiliary
denominators. Only external mass replacements are global; internal mass and
on-shell replacements remain numerator-only. Reference selection defaults to
`Automatic`: optimize eligible other-massless-particle assignments where available,
with physical momentum-insertion fallback for each remaining reference outside
that diagram's own cuts. `"AllSpurious"` and `"AllPhysical"` optimize only their
respective eligible assignments; `"Mixed"` pools both kinds per external reference
and may select a mixture or only one kind. `"Cyclic"` and `"ReverseCyclic"` retain
fixed, reproducible choices without assignment optimization. `"Xi"` (one common
generic Xi0), `"IndependentXi"` (one per external particle), and `None` (unchanged)
make the other choices explicit. The details record assignments and unavailable
choices are PARTIAL, not silently accepted. Supply the complete diagram set when choosing
physical references, so alternative physical channels can be identified.
Optimized modes rank reduced complete diagram expressions by complexity, term
count and expression size. The default `"MaxReferenceChoices" -> 64` bounds tested
assignments per diagram. `"ReferenceTimeConstraint" -> Automatic` uses the smaller
of five seconds and one third of the remaining per-diagram time. Explicit positive
values or `Infinity` remain subject to the shared diagram deadline. Progress shows
choices tried, the budgeted total and the best complexity. The optional reference
details retain counts, `"Exhaustive"` and `"StopReason"` within `"Search"`, sorted
`"Alternatives"` with assignments, reduced expressions and ranks, and the winning
`"ReducedExpression"`/`"ReducedRank"` alongside its raw `"Expression"`.
A choice cap or reference subbudget may return a valid best-tested winner without
establishing a global minimum; an overall diagram timeout remains PARTIAL.
Preparation does not use factorization to select references, and the smallest
prepared seed is not guaranteed to complete fastest.
With `"InternalOnShell" -> True`, preparation directly converts known
two-particle numerator scalar products to masses before and after `ReduceSpinorProducts`,
using only that diagram's mass rules. Two-leg complements may be used when
momentum conservation is enabled. Unknown pair invariants and denominator
factors remain unchanged. This direct reduction runs even at `"MaxSteps" -> 0`.
The additional bounded, strictly improving identity search also
uses only that diagram's internal mass rules. This additional search is
distinct from `ReduceSpinorProducts` itself. Preserve the original pairs and
the preparation details when studying numerator and reference choices. Use
`"ReferenceSpinors" -> None` to preserve literal references for comparisons;
the checker itself never merges them. `"InternalOnShell" -> False` disables
both the direct invariant reduction and the extra shell edits. `"MaxSteps" -> 0`
disables only the identity search, retaining reference selection, the basic
reductions and the direct invariant reduction. The default
preparation limits are 12 identity steps, 64 inspected candidates per step
and 20 seconds per diagram. Partial preparation retains the last completed
expression and its stop reason.
Preparation's conservation search tries consistent whole-numerator momentum
eliminations and composite complements, using only the declared external mass
labels. Reduce a complete trial before comparing its full-expression rank;
temporary expansion is allowed, but every accepted step must strictly improve
the rank. These trials share the existing limits with local edits and are disabled
by `"MomentumConservation" -> False`. Conservation and internal-shell substitutions
protect all inverse factors; the subsequent ordinary whole-expression spinor
reduction may simplify auxiliary denominators. Do not move this process-dependent search into
`ReduceSpinorProducts` or install persistent momentum-conservation rules.

For an already verified final amplitude, pass a single external-mass pair to
`PrepareAmplitudes`, normally with `"ReferenceSpinors" -> None`. `"MaxSteps" -> 0`
retains the fast ordinary cleanup and bounded final compaction while skipping
the broad candidate search. Compare complexity, expanded terms and expression
size separately. A factored representation can be substantially smaller without
changing the expanded complexity score. Preserve a useful existing form if a
normalization route makes it worse; do not claim a global minimum.

Final cleanup can expose an existing physical denominator through its exact
virtuality identity, retaining its nonzero propagator contribution. This is an
ordinary off-cut identity, not an internal-shell substitution. Sparse Laurent
content grouping and scalar coefficient collection may regroup or cancel those
factors. No new pole or interaction is inferred. Single-insertion conservation
trials change only one-momentum spinor sandwiches, using the declared external
labels and null external masses; they leave longer words and inverse factors
untouched. Normalize scalar momentum squares as Lorentz products before global
conservation substitutions. The original and best completed snapshots remain
available through the final cleanup. Ranking caches are bounded and local to
one preparation call, never a process/model cache.

The ordinary reducer has prospective distribution guards and a factored route
for rational amplitudes. Preserve its established bare-contraction normal forms
and explicit off-cut sandwich remainders. Do not replace resource guard failures
with an expanded partial expression, a numerical fit, or an internal-shell rule.
For a new simplification, prove its identity with independent spinors or exact
external-only off-cut charts, then recheck physical/spurious/high-energy/reference
conditions and an independent Feynman sum. Unresolved checker categories remain
unresolved; the balanced six-point recursion is a resource stress case, not an
all-checks-pass final formula merely because Feynman samples agree.

Automatic completion first tries bounded input-derived proposals when the
original seed fails: fit one constant on its complete pole-bearing part, then
on a union of exact complete signed terms shared across separate diagram
records. These are corrections to test, not new identities for summing diagrams.
Preserve explicit no-PropDen additive terms and every sign, coefficient and
physical target. Accept only a unique constant passing fresh coherent-spin
validation at every validation parameter set. Never change diagram generation
or fit a normalization inside CheckAmplitudes/Feynman comparisons. An already
passing seed is retained. `"InputReweighting" -> None`, explicit CorrectionBasis
and ChannelReferences -> None skip the attempts. Both fits share at most one
fifth of the remaining stage time, capped at 30 seconds; failures must leave
the original seed and later solver intact. Details retain InputFitAttempts.

Before the larger channel spaces, `"ChannelCompanions" -> Automatic` tries
a small space derived from accepted closed physical-reference channel sources.
Eligible two-leg cuts have one declared null external leg and a massive
anchor with the same exchanged mass. Normalize each source using external
identities and only its own cut, verifying that normalization. Group terms by
chain/trace momentum-insertion count, then peel the emitted null label from
typed lower-amplitude composites while protecting the selected propagator.
This is a correction proposal, not an ordinary identity or an assertion that
the selected residue is unchanged. Keep complete groups, coefficients and
regular input. Reject new explicit pole-free directions, undeclared physical
propagators and higher physical pole powers. Fit every original target and
zero-residue obligation independently at coherent spin and parameter samples.
Construction is capped at 15 seconds; construction plus fit share one third
of the remaining stage time, capped at 60 seconds. Caps/partial construction
must remain visible. If this space fails, retain the existing full fallback.
None disables it; explicit bases and literal-channel mode bypass it.

`CompleteFactorization` builds a bounded correction space from actual input
and sewn-channel expressions, or an explicit `"CorrectionBasis"`, and solves all
physical channels simultaneously. `"ChannelReferences" -> Automatic` also
tries recorded preparation assignments, when supplied in preparation details,
and cyclic and physical-reference alternatives for sewn channels. The prepared
diagram expressions are not changed. Additional channel continuations enter
the automatic space only after exact sampled checks preserve their selected
cut residue; this is not an off-cut equality claim or a generic proof.
Automatic completion first tries a compact physical-reference channel space,
including complete sewn channels that already contain no Xi spinors:
apply ordinary external-shell and conservation reductions to each whole accepted
channel continuation before splitting into terms. Verify the reduced whole
expression on its selected cut, then retain both raw and reduced terms. Add
only closed reference-free sources: unexpanded x factors and surviving spinors
for internal cut particles do not qualify merely because no Xi is explicit.
Those original target residues are retained unchanged. Add
selected-cut numerator reductions only as grouped off-cut differences, using
that channel's one internal mass rule, never the other source propagators'
shell conditions. Protect inverse factors and verify the difference's selected
residue before admission. Intermediate cyclic reference rotations are bounded
and individually checked; forward and reverse choices alone do not exhaust
the possibilities once there are more massless reference particles. Add
one grouped pole-bearing input expression per diagram. Explicit additive input
terms without a physical `PropDen` denominator remain in the seed, not in these
cancellation directions; do not claim this separates hidden contact content.
No known final amplitude or independent contact interaction enters the space.
If unavailable, or unsuccessful with time remaining, try the existing literal
termwise space. Attempts share the stage deadline and correction-term cap;
do not silently discard excess directions. `"ChannelReferences" -> None`
retains the literal-only behavior, and an explicit `"CorrectionBasis"` retains
the supplied-space behavior after normalization.
`"BoseSymmetryGroups" -> Automatic` and `"FermionSymmetryGroups" -> Automatic`
are the defaults. Identify all identical external groups from the model's
twice-spin, species, masses and external spinor weights. Even twice-spin uses
symmetric exchange; odd twice-spin uses the permutation parity. Keep particles
and antiparticles separate. Retain the ordered process with
`"ExternalParticles" -> particles` in requested preparation reports;
completion inherits it from input or channel reports. Plain input may provide
the same option directly. Do not infer species from equal masses alone or
merge opposite helicities, different fixed spin states or color-stripped
colored states automatically. Without process names, project only unique
model-and-spinor matches. Ordinary pairs and channel rows remain unchanged;
no global process cache is created.
Apply one normalized signed projector to the seed and correction directions,
combining all detected Bose and Fermi groups. For `e,E,E,e,A.+,A.+`, use
`(1 + P56)(1 - P14)(1 - P23)/8`, not separate attempts for each group.
Typed permutations preserve coefficients, Xi identifiers and explicit
numerical spin labels. Do not change existing diagram Wick or sewing signs.
Keep every original residue target for independent validation.
Automatic mode uses a bounded exact subgroup if the full combined order
exceeds 120, reserving exchanges for different groups where possible.
Details retain detected/used groups, requested/used order and `FullGroupUsed`;
never claim full symmetry when a subgroup was used. Explicit groups have the
same combined cap. Each empty option disables only its own statistic; both
must be `{}` to disable all exchange projection. This is an additional
constraint, never a replacement for residue validation. Preserve exchange
metadata through detailed factorization reports into `CompleteHighEnergy`,
and project newly generated automatic and explicitly supplied high-energy
correction directions with the same signed action. Do not re-infer spin states
from a completed expression that may be zero. Already projected recursive fits must disable both options to avoid
reapplying inference to their intermediate inputs.
With automatic correction directions and declared exchange symmetry, combine the
available compact and literal spaces before projection. Also try the individual
terms of each accepted selected-cut numerator reduction, including spurious
reference choices. Certify the whole reduced source on its own cut before
splitting it; never apply the other internal propagators' shell conditions.
This step is bounded by the construction cap and a 60-second budget. Exact formal-monomial
linear dependencies may be removed without changing that projected span.
This pruning retains sparse matrices and has a 60-second budget; an unfinished
pass retains all directions.
With both symmetry options explicitly `{}`, completion keeps compact-then-literal behavior. Intermediate
individual source-space construction grows with the permutation count, up to
4096 directions unless the requested correction cap is already larger. The
compact, literal and selected-cut spaces may merge into a larger union, bounded
by the sum of their source-space limits. The final cap applies after projection
and pruning; neither a term cap nor the shared clock is a hard memory limit.
If that full automatic exchange space still exceeds the requested cap, a named
budget fallback may retain whole pole-bearing input diagrams plus every
available channel atom, without splitting each input diagram into independent
corrections. The full space remains unchanged whenever it fits. Report both
sizes in `"CorrectionSpaceCandidates"`; do not truncate arbitrary directions
or relax any target or affine-family validation. The fallback is
`"BoseGroupedInputChannelUnion"` for Bose-only projection, with `Fermion` or
`Exchange` prefixes for fermion-only or combined projection. It remains
unresolved if it too exceeds the cap or cannot satisfy the constraints.
New continuations with uncanceled higher-order physical propagator powers are
recorded but not admitted: the current correction fitter supports regular
simple-pole residues. This is a solver limit, not a restriction on the theory;
raw input and channel sources are retained. Contributions from different
exchanged species at the same cut and mass are summed before fitting, with
incomplete channel data still reported as incomplete.
Do not independently add each discrepancy divided by its pole: shared poles
can be double counted. Preserve the
ordinary-amplitude default: `"ReturnDetails" -> False`. Request
`"ReturnDetails" -> True` only when a full report is useful; the legacy
`"Output" -> "Details"` remains supported. A completed factorization stage
does not assert that spurious, high-energy or reference checks have passed.

Failed validation comparisons supply additional exact equations on the same
saved cut charts, with coherent massive-spin components. The simultaneous
system is refitted, then checked with fresh seeds. `"MaxRefinements" -> 3`
bounds these retries after the initial fit; zero disables feedback. Evaluate
feedback on the original sparse correction components before substituting the
saved fit, rather than expanding and reevaluating its dense affine family.
For internally generated coherent cut charts only, a simple typed-pole fast
path may evaluate supported rational primitives directly on the cut after
checking every primitive is finite and every inverse base is nonzero.
Hidden singular denominators, higher poles, unsupported structures and cut-only
spinors fall back to the full mapped restriction. Full-map additive caching
must also fall back to the whole component when singular addends can cancel.
Implicit x factors retain the whole component's cut-spinor inventory; arbitrary
custom maps retain the original whole-expression path. Validate the kinematics
once per chart, but check every component even if its fitted coefficient is zero.
These are evaluation optimizations, not new on-shell identities or weaker tests.
Extract affine components in one coefficient-array pass rather than rescanning
the trial once per unknown. Discover correction-space pole support from its
typed propagator inventory; all original channel targets and zero-residue
obligations remain, and every component is still restricted on each cut.
Progress callbacks include individual successful factorization samples as well
as completed channels, so a large channel does not appear idle for minutes.
Already polynomial coherent-spin constraints keep their exact rational
coefficients instead of combining every unknown into one denominator first.
Rational dependence on spin or fitting variables still uses the full
normalization path; the resulting linear matrix is always solved exactly.
Refit all accumulated raw equations in the original unknowns each round;
composing dense intermediate solution rules can cause severe fraction growth.
Construct the fitted amplitude and free directions only after sparse validation
succeeds; keep their linear combinations factored instead of expanding a dense
family before checking it. Optional representative selection and final external
reduction have one shared two-second budget after validation. Selection tries
bounded exact formal coefficient cancellations in several deterministic orders,
keeps only a strict whole-expression improvement, and retains every free
direction. Preserve coefficients of explicit regular monomials, including
zero coefficients; do not erase contacts merely to shrink an expression.
Rebase FamilyAmplitude around the new center with unchanged directions/free
names, and record the original center and coordinate offset rather than
misrepresenting old linear-solution coordinates. RepresentativeSelection -> None
disables this choice. Cleanup must compare complete ranks and retain the
original compact form if ordinary reduction expands it into a worse result.
Interrupted work commits only a completed point and cannot turn validated
success into failure.
Exact common algebraic units may be removed from matrix columns and the right-hand
side for rational elimination; restore the original coefficient and free-variable
scales in solution rules. Large rational systems first try a bounded, 60-second
CRT reconstruction using its bounded prime inventory (25 primes by default)
and a separate unused-prime check. Accept only after an exact integer-scaled certificate verifies every
original equation for the particular solution and all independent free directions;
the modular rank lower bound then certifies completeness. Failed reconstruction,
bad primes or timeouts fall through to the existing exact solver, never to an
approximate fit or an uncertified modular verdict. Full coefficient-column rank
and a larger augmented rank modulo a valid prime certify exact inconsistency;
a rank-deficient modular gap does not. Large rational systems can select a
square block using modular arithmetic, but must solve it exactly and certify every original row
for a consistent particular solution and the complete independent free family.
An original row with an exact nonzero constant residual and zero dependence
on every free coordinate instead certifies inconsistency of the selected block
and that original row. Repair
failed right-hand-side solves or fall back to ordinary exact row reduction;
modular rank alone never proves consistency or inconsistency. Do not replace
this with an approximate fit. Ordinary
external-shell and conservation identities remove only provably zero
correction directions. Retain every unproved or genuinely residue-invisible
direction for later constraints. Typed `PropDen` poles are checked throughout
the correction family, including poles hidden by setting free parameters to
zero. Established zero-residue obligations remain checked after a pole leaves
the selected candidate; internal validation rows retain those obligations
without changing the user's channels, model or vertex content.
The optional details expose
`"RefinementHistory"` and `"ChannelContinuations"`, including their verification
scope. `"CorrectionSpace"` is `"ReducedChannels"`, `"LiteralTerms"`,
`"InputRescaling"`, `"InputOverlap"`, `"PeeledChannels"`,
`"PreparationRemainders"`, `"RetainedReferences"`,
`"BoseChannelUnion"`, `"BoseSelectedChannelUnion"`, `"BoseGroupedInputChannelUnion"`, `"SuppliedTerms"` or
`"NotRequired"` when the input already passes.
`"ReducedChannelSpace"` retains the compact sources, basis
and selected-cut checks; `"SelectedChannelSpace"` retains the selected-cut
atomic sources and their checks. `"CorrectionSpaceAttempts"` records a compact attempt
followed by literal fallback when that occurs. Neither retries nor alternative
references make the bounded space universal; unresolved limits and inconsistent
equations remain visible.

For difficult cases, explicitly request the factorization report and pass it
to `CompleteHighEnergy` after checking its `"Status"`. Its `"FamilyAmplitude"`,
`"RemainingBasis"` and `"FreeParameters"` retain the full fitted correction
freedom. An ordinary amplitude is one representative; the next stage can
search directions from its surviving terms but cannot generally recover
discarded directions. There is no hidden cache or expression metadata.

`CompleteHighEnergy` constrains the retained family, or supplied bounded
directions, using high-energy growth, physical residues, spurious poles and
explicit reference independence together. It rechecks physical constraints
rather than assuming that an added correction is residue preserving. No
unrestricted contact basis or new independent QED interaction is inferred.
Respect the specified theory and report insufficient correction spaces or
unfixed directions honestly. The default bound remains the renormalizable
tree hypothesis `E^(4-n)` and can be overridden.

Before interpreting free coefficients as missing physics, distinguish true
correction freedom from identity-zero expressions. High-energy completion
first tries ordinary external preparation, then a bounded symbolic rational
chart covering generic conserved external kinematics. The latter retains
symbolic parameters and coherent spin components and constrains no internal
line. Only an exact zero certificate removes a direction; unsupported forms
and proof timeouts retain it. Details expose `"FamilyIdentityReduction"`.
Preserve the typed Lorentz-scalar, chirality, little-group and branch guards;
neither finite zero samples nor a special-frame component proves an identity.
Genuine contact freedom remains `UNDERDETERMINED`.

Compound spurious poles can be checked together on exact conserved rational
curves. Polynomial coprimality checks all roots on each curve while rejecting
roots shared with physical poles. This is sampled finiteness evidence, not
a global identity proof. Keep every original pole obligation and its status
visible when grouping work; unsupported or unfinished checks stay unresolved.
The new compound mode currently requires at least three massless and at most
two massive external legs, typed covariant factors and homogeneous massless
weights. Its curve varies the massive-pair invariant as well as the massless
spinors, so a hidden massive-invariant pole cannot be frozen away. Ordinary
bracket and invariant checks retain their broader support.
Compound poles share one checker `TimeConstraint` budget regardless of their
count; unfinished rows remain explicit. Dimensionless chart generation must
restore the original physical masses, momenta and spinor bases without
rescaling an amplitude or fitting a sign. Preserve determinant and Dirac
identities for every coherent massive-spin component. Exact primitive reuse
is call-local and falls back to the whole evaluator for unsupported structures.

Both completion stages default to 64 correction terms and a 60-second stage
limit. They fit exact equations on sampled kinematics and validate independently;
they are not generic symbolic reconstruction or uniqueness proofs. Use
`"ValidationParameterRules" -> {rules1, rules2, ...}` for additional
theory-consistent parameter assignments. Automatic mass-rescaled validation
is used only when the model has no declared parameter relations; otherwise
only the supplied assignment is checked. Mass and coupling dependence is
not established merely by testing one numeric assignment.

With ordinary output, `CompleteFactorization` returns the amplitude on success
or a compact `Failure["IncompleteAmplitude", ...]` with a short failure reason
when unresolved, without embedding the candidate or report.
Request `"ReturnDetails" -> True` to inspect
its candidates, equations, diagnostics and validation. `CompleteHighEnergy`
also accepts the channel preparation report, retaining original channel targets.
Its ordinary incomplete failure retains the candidate and report,
and `"Output" -> "Details"` returns that report directly. Never turn a retained
unresolved candidate into a claimed physical result. All three stages
support `"ProgressReporting" -> False` and a `"ProgressFunction"` callback;
callbacks receive associations when progress reporting is enabled. An
all-in-one `ConstructAmplitude` wrapper is described above. See
`tests/SIMPLIFICATION.md` for the full staged example and scope limitations.

## Preserve Work and Verify Changes

Preserve user notebook cells, saved outputs, and prior derivations. Append named
sections when notebook edits are requested; do not regenerate a notebook merely
to change a few cells. Do not edit package code during a derivation unless the
task authorizes it or a necessary change has been discussed.

Keep source changes general across processes and multiplicities and follow the
existing section structure. Add focused regressions for new identities or fixes.
For source changes, run the regression suites from the repository root:

```sh
python3 tests/checks/run-wolfram-tests.py --timeout 300
```

The runner accepts `--kernel /absolute/path/to/WolframKernel`. Report the checks
actually executed, failures, and limitations. Do not claim verification based
only on reading code. Do not start training, use the server, or switch branches
unless requested.

The user has requested that every completed improvement be committed and pushed.
After verifying a change, review the pending diff, commit the relevant work on
the current branch, and push to its configured upstream. Preserve unrelated
user edits; never force-push. Report the commit and any verification or push
limitations. This standing instruction does not authorize publishing private
files, credentials, research papers or unrelated artifacts.

The default includes four legacy suites covering the original package and
algebra/model/export tools. Read `tests/checks/README.md` for their inventory
and known pre-existing issues. Known failures are printed separately, not
counted as passes; `--strict-known-issues` makes them fail the run. Never add a
new regression to the known list just to obtain a successful run. Use repeated
`--suite` options for focused verification. The Python runner tests are
`python3 -m unittest discover -s tests/checks -p test_runner.py`.
