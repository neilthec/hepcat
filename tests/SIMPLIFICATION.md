# Symbolic Simplification

The `factorization` branch keeps the symbolic identities and worked
simplification paths. The neural models, training pipeline, and their original
notebooks are preserved on the `ml-simplify` branch at commit `54f6920`.

## Files

- `source/HEPCAT-Base.wl`: the identity tools, loaded by `HEPCAT.wl`.
- `tests/simplification-paths.wl`: known reference amplitudes and curated
  step-by-step simplification paths.
- `tests/SM-4-point-simplification-test.nb`: the retained four-point reference
  cases and worked examples, without a neural benchmark.
- `tests/QED-5-point.nb`: the five-point calculations, unchanged.
- `tests/checks/`: symbolic regression suites and their timeout runner.

## Identity Tools

After loading HEPCAT, enumerate exact edits and apply a selected edit:

```wolfram
pair = First[diagramAmplitudes[diagrams]];
edits = SpinorIdentityCandidates[pair];
If[edits =!= {}, next = ApplySpinorIdentity[pair, First[edits]]];
SpinorExpressionComplexity[pair[[2]]]
```

Edits record the identity name and the expressions to subtract and insert.
Candidate generation canonicalizes spinor chains and uses their linearity;
it does not score edits with a model or promise that the first edit is best.
`ApplySpinorIdentity` checks that the edit is legal in the supplied context.
Use the same options when generating and applying edits.

`SchoutenRewrite` and `SchoutenCandidates` expose generalized Schouten
identities. `ComplicateAmplitude` remains a model-free way to exercise legal
identities with a reproducible random seed.

The existing `applyToNumerator[expr, rules]` applies rules to the numerator of
each existing term without combining or expanding the terms.

## On-Shell Scope

For one diagram, `"OnShellChannels" -> Automatic` infers its internal numerator
conditions from its mass rules. These rules never act inside `PropDen`.
For a sum of diagrams or an off-cut comparison, disable internal shells:

```wolfram
edits = SpinorIdentityCandidates[{externalMassRules, expression},
  "OnShellChannels" -> {}];
If[edits =!= {},
  next = ApplySpinorIdentity[{externalMassRules, expression}, First[edits],
    "OnShellChannels" -> {}]];
```

`"MomentumConservation" -> False` independently disables conservation edits.
Preserve each diagram's mass rules separately instead of merging incompatible
internal on-shell conditions into a shared list.

## Checks

```sh
python3 tests/checks/run-wolfram-tests.py --timeout 300
```

The suites cover symbolic identities, diagram construction, curated
simplification paths, physical residues, and spurious-pole diagnostics. They do not load model
weights or start training.

## Physical-Residue Diagnostics

`tests/Factorization-checks.nb` is a short, self-contained walkthrough with
four- and five-point QED. Each example uses the ordinary diagram amplitudes,
the original compact factorization channels and the model:

```wolfram
diagrams = CreateDiagrams[particles, diagramModel];
amplitudes = diagramAmplitudes[diagrams];
channels = FactorizationAmplitudeChannels[particles, factorizationModel];
CheckPhysicalResidues[amplitudes, channels, factorizationModel,
  "ParameterRules" -> {Me -> 1, EE -> 1}]
```

The normal checker output is a compact table containing the cut, exchanged
mass and `PASS`, `Opp Sign`, `FAIL` or `UNRESOLVED`. `Opp Sign` means the
nonzero candidate residues consistently agree only with the negatives of the
expected physical residues, after the channel-to-residue conversion described
below. Neither supplied input is changed. Samples where both residues vanish
do not determine the sign.
Mixed same-sign and opposite-sign agreement at nonzero samples is `FAIL`.
The checker accepts the diagram pairs
directly, applies `ReduceSpinContractions` and each diagram's mass replacements,
then compares the summed amplitude. A prepared expression can be supplied
instead. It does not call `ReduceSpinorProducts` or impose internal on-shell
conditions on the candidate. The supplied factorization channels are used
directly; the checker does not sew a second set of lower-point amplitudes.

The notebook explicitly simplifies the five-point diagrams before checking
them. It contracts spin indices, replaces composite momenta inside spinor
chains by the shorter momentum-conservation sum, and reduces those chains.
Raw and simplified expressions remain in separate variables; propagators are
unchanged. This is a partial reduction, not automatic elimination of Xi.
Separate sections check the hard-coded physical-pole form H, the equivalent
spurious-pole form S2, and the old permutation formula. None is substituted
for the computed diagram amplitude or inserted into the factorization model.

The supplied model is authoritative: every amplitude absent from it is zero,
at any multiplicity. No explicit zero rows or completeness options are needed.
The user supplies the amplitudes needed for the intended theory. The notebook
uses the existing QED three-point model (model 2) for all diagrams and four-point
factorization, and the existing QED three- and four-point model (model 3) for
five-point factorization. No amplitude rows are constructed or added by the
notebook. The four-point amplitudes are not diagram vertices. An omitted
amplitude gives a zero expected contribution, so
a nonzero candidate residue can produce `FAIL`. Candidate denominator poles
absent from the channel list are checked for full cancellation as apparent
spurious poles, rather than added to the physical-channel list.
Malformed inputs are rejected with an explanatory message. Unsupported
calculations, singular samples and timeouts remain `UNRESOLVED`, not zero.

`FactorizationAmplitudeChannels[particles, model]` keeps its familiar rows:
external masses, the momentum combination defining the channel, the exchanged
mass and the lower-amplitude product divided by the propagator and the existing
recursion `z` factor. These public rows are unchanged. The checker multiplies
the channel expression by `-z PropDen` for the selected propagator to obtain
the expected physical residue. The minus is the sewing convention used by the
diagram code: `(i A_L)(i A_R)(i/PropDen)/i = -A_L A_R/PropDen`. This conversion
is applied exactly once to the internal comparison target, not to the supplied
channel rows or candidate amplitude. Users do not build metadata records or
manually adjust their amplitudes' signs.

For each cut, the checker generates four reproducible exact rational,
generally complex kinematic samples. All external shells and momentum
conservation hold while the selected propagator varies through zero; other
listed poles remain generic. The construction has no four- or five-leg limit
and visits both complex three-point massless branch choices.

Options include `"Samples" -> 4`, `"RandomSeed" -> 1729`,
`"ParameterRules" -> {Me -> 1, EE -> 1}` and `"TimeConstraint" -> 30`
(per pole, and separately for discovery). Parameter replacements are optional; without them masses and
couplings remain symbolic, which can be more expensive. Supply values consistent
with the theory. Automatic `PASS` means agreement at the recorded samples, not
a general analytic proof or a test restricted to real physical phase space.
Massive external spinors are sampled coherently, not exhaustively enumerated.
Reference spinors are evaluated literally and distinctly by their Xi labels.
When adding diagrams, use consistent reference choices for each external
particle. The checker does not silently reinterpret diagram-local Xi counters.

### More Information on Request

Use `"Output"` to choose how much to display:

```wolfram
CheckPhysicalResidues[amplitudes, channels, factorizationModel,
  "ParameterRules" -> {Me -> 1, EE -> 1}, "Output" -> "Residues"]
```

`"Residues"` shows one row per pole and sample in a single table. Physical
rows show calculated and expected residues; spurious rows show the leading
divergent coefficient (zero for a regular result), expected zero, pole order
and finite limit. All values are visible scalars rather than collapsed lists.
All rows and columns are displayed; exact values are retained rather than
replaced with rounded numbers. The status remains the channel's overall
result. This avoids the collapsed lists previously shown inside each cell.
`"Summary"` returns result counts. `"Details"` returns the full diagnostic
report, including channel records, exact sample kinematics and
`"OppositeSignDifference"` (the sum of the calculated and expected residues).
An `Opp Sign` result, like `PASS`, requires all requested samples to complete;
it is not an analytic proof. These
working records are not the normal output of either public function. No
amplitude correction or new interaction is added.

### Advanced Checks

The lower-level `PhysicalPoleResidue[expression, cut]` also accepts a list of cuts. By default it
returns only a `FORMAL` coefficient of opaque `PropDen` factors, not a physical
on-shell proof. To evaluate a physical residue, supply `"KinematicMap" -> f`,
where `f[expression, cuts_List, vars_List]` maps the whole expression coherently
to rational near-cut variables. This includes momenta, dot products,
propagators, reference spinors and external spinors. Each `Mom[i]` must map to
a two-by-two momentum matrix. The checker independently compares its determinant
with the mapped propagators and checks external shells, momentum conservation,
and each selected cut and its complement. The spinor realization is supplied
by the map. The low-level `"UncutChannels"` option accepts explicit cut
descriptors for poles that must remain regular. A finite simple-pole limit produces a physical residue;
invalid maps, unsupported singularities and higher-order poles remain
`UNRESOLVED`.

Custom maps also work with the checker. Set
`"KinematicKind" -> "Sample"` for a phase-space sample or `"Symbolic"` for a
symbolic chart, and optionally `"TimeConstraint" -> 30`. A passing comparison
applies to the supplied sample or chart and its tested spin components, not
automatically to every branch or every kinematic configuration. Request
`"Output" -> "Details"` to inspect the sampled calculations. Automatic
multiple-cut chart generation is not included; use a
coherent custom map with `PhysicalPoleResidue` for simultaneous cuts. These APIs
diagnose the candidate; they do not repair it, add vertices, or establish that
no spurious poles or contact ambiguities remain.

The five-point QED pilot in `tests/QED-5-point-residues.wl` and the regression
suite retain H, S2, the old permutation formula and multiple-cut checks without
adding that machinery to the example notebook. H, S2 and the common-reference
diagrams agree with the physical residues from model 3 at all six electron
poles, using its corrected four-point signs and the sewing conversion above.
The four-point model-2 cubic diagrams likewise agree with their channel
residues. Deliberately opposite-sign candidates are reported as `Opp Sign`;
the old permutation formula still gives three `PASS` and three `FAIL` rows. The
distinct-reference raw `"ActualDiagrams"` also exposes a continuation
discrepancy. These comparisons do not add a QED interaction or repair an
amplitude automatically.

The known four-point amplitudes come from QED model 3; diagrams use the
three-point vertices of QED model 2. Scalar and fermion exchange have
independent sewing regressions. Massive-vector sewing reuses the existing
symmetric spin projector but does not yet have a new independent numerical
calibration in this suite.

### Five-Point QED Coverage

The example notebook covers the two nonzero five-point tree families in the
existing electron/photon QED model: `e,E,A,A,A` (eight photon-helicity choices)
and `e,E,e,E,A` (two photon-helicity choices). Other charged flavors require
their own model data; they are not silently added to the electron-only model.

For the three-photon family, the notebook retains both the prepared diagram
expressions and the independently completed compact amplitudes from
`QED-5-point.nb`. Photon relabeling and parity generate the other helicities.
All eight completed expressions pass all six physical channels. The raw
mixed-helicity diagram sums still give two `PASS` and four `FAIL` rows each:
they retain a reference-dependent continuation and are not silently replaced
by the completed forms.

Fermion exchange signs now follow the open fermion lines in each diagram's
graph, through the model's ordered fermion ports. Photon attachments do not
change the endpoint pairing. Identical external species receive their
permutation parity once, independently of multiplicity or S/T/U labels.
Factorization sewing additionally counts the fermion ports between the two
contracted internal fermions before removing those labels. This restores the
same convention on both sides without changing model amplitudes.

The unfinished four-fermion/photon representatives pass all four electron
cuts but fail their four photon cuts. The explicit diagram-local identity
route below now produces reference-free expressions that pass all eight
channels, for either photon helicity. The unfinished expressions remain
available separately rather than being fitted away with a sign.
The numerical map assigns
internal photon spinors only to the selected massless physical cut and
evaluates its remaining x factors with the incoming-momentum orientation.
It never assigns spinors to a generic off-shell composite momentum.

### Worked Five-Point Identity Routes

`tests/QED-5-point-residues.wl` also contains the process-specific worked
routes used by both QED notebooks. Loading it does not run tests or change
the package, the model, or the checker:

```wolfram
Get[FileNameJoin[{hepcatDirectory, "tests", "QED-5-point-residues.wl"}]];
diagrams = CreateDiagrams[{"e", "E", "E", "e", "A.+"}, qedDiagramModel];
route = q5FourFermionPhotonRoute[diagrams];
route["Final"] // nice
```

The four-fermion route retains each actual diagram, its exact Dirac
reduction, the explicitly removed numerator multiple of its own electron
on-shell condition, and the nonzero off-cut remainder. It does not set
either propagator denominator to zero. Pairing the two photon attachments
on a fermion line then cancels Xi by an exact two-component Schouten
identity. Four radiating-current blocks give the final expression with
physical propagators only. The negative-photon form follows by swapping
angle and square spinors, and is checked independently.

For `e,E,A.+,A.+,A.-`, use:

```wolfram
pairs = diagramAmplitudes[CreateDiagrams[
  {"e", "E", "A.+", "A.+", "A.-"}, qedDiagramModel]];
mixedRoute = qedFivePointMixedRoute[pairs];
mixedRoute["Final"] // nice
```

This route exposes the six local seed numerators, the additional local
numerator multiples and the completed numerators. Each change vanishes
on the relevant diagram's internal shells. Their completed sum reduces
to the three-term, reference-free expression by external-shell and
momentum-conservation identities alone. The helpers
`qedFivePointMixedLocalChecks[mixedRoute]` and
`qedFivePointMixedEqualityChecks[]` retain the local comparisons and exact
polynomial cancellation checks.

Neither route claims that the old raw off-cut sum is unchanged. The
diagram-local numerator representative is selected explicitly, then its
summed physical amplitude is checked against all channels. No independent
QED contact interaction is introduced. These are worked five-point routes,
not a new automatic simplifier for arbitrary processes, and no claim of
an absolute shortest formula is made.

### Automatic Physical and Spurious Checks

The ordinary call checks physical residues and discovers apparent spurious
denominator poles automatically. No pole list is required:

```wolfram
CheckPhysicalResidues[amplitude, channels, factorizationModel,
  "ParameterRules" -> {Me -> 1, EE -> 1}]
```

The compact table has `Type`, `Pole` and `Status` columns for both kinds of
check. Only the supplied factorization channels identify physical poles.
Equivalent scalar invariants are recognized using external mass shells and
momentum conservation, including complementary cuts, mass-sign conventions,
and nonzero rescaling. A massless angle or square bracket can represent a
physical zero-mass channel and is not automatically labeled spurious.

Discovery examines reciprocal factors in the given expression and its
individual diagram inputs without first combining their denominators. It
ignores coupling-only and mass-only constants, factors compound denominators,
deduplicates repeated poles and retains explicit denominator powers. The
actual surviving pole order is determined by the Laurent check on the sum.
Supported spurious factors include massless external brackets, external/Xi
brackets and quadratic momentum invariants. Generic momentum chains,
unclassified factors, noninteger powers and unavailable charts appear as
`UNRESOLVED`, not as a guessed pass. This is not a complete symbolic
classification of every possible denominator representation.

The full summed amplitude is tested for finiteness, not for a zero value.
Exact rational deformations preserve external shells and momentum
conservation while all known physical propagators remain nonzero. A
surviving negative Laurent power gives `FAIL`, including higher-order poles.
Angle and square factors are tested on their respective complex collinear
branches. Scalar massless invariants cover both branches; the requested sample
count is raised if needed to cover every branch. Xi poles instead deform the
reference spinor at generic kinematics. No internal on-shell condition is
imposed on the full amplitude.

`"Output" -> "Residues"` displays one combined sample table. `"Details"`
retains `"PoleDiscovery"`, `"Checks"`, `"SpuriousChecks"`, sample charts and
leading Laurent coefficients, as well as separate `"PhysicalSummary"` and
`"SpuriousSummary"` counts. The ordinary `"Summary"` counts both kinds of
check. No spurious rows means no apparent nonphysical denominator was found
in the supplied form; it is not an independent proof of regularity everywhere.
A sampled `PASS` is not an analytic proof of cancellation.

For an advanced override, `"SpuriousPoles" -> {factor1, factor2}` replaces
automatic discovery with the specified factors. `"SpuriousPoles" -> {}`
requests the legacy physical-only report. Neither is needed in ordinary use.

The H and S2 pilot formulas stay finite on both angle-34 and square-34
branches. The old permutation formula fails the angle-34 test. Regression
tests also cover reference poles, higher-order poles, six-point kinematics,
coincident physical poles and timeouts.
