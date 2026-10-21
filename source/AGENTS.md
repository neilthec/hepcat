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

This file is in `source/`. For tasks started elsewhere in the repository,
explicitly read `source/AGENTS.md` before using this workflow. All paths below
are relative to the repository root, not to this file.

## Start Here

- `tests/SIMPLIFICATION.md`: practical guide to identities, on-shell scope,
  worked derivations, and the current checker API. Read this first.
- `source/HEPCAT.wl`: package loader; loads both source files below.
- `source/HEPCAT-Base.wl`: spinor algebra, `ReduceSpinContractions`,
  `ReduceSpinorProducts`, `SimplifySpinorProducts`, `applyToNumerator`, and the
  section **Symbolic Spinor Identities** containing explicit identity edits.
- `source/HEPCAT-Model.wl`: model loading, diagram construction, factorization,
  and **Physical Residue Diagnostics**, including automatic spurious-pole checks.
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
the four-point amplitudes needed for five-point factorization. For other
processes, inspect the model rather than assuming these example choices apply.

The factorization model is authoritative: amplitudes absent from it count as
zero. Ensure the user supplied the intended lower-point data. Do not fabricate
zero rows, insert fitted amplitudes, or add independent QED contact interactions
to force a comparison to pass. Respect the specified theory's vertex content.

```wolfram
channels = FactorizationAmplitudeChannels[particles, factorizationModel];
report = CheckPhysicalResidues[finalAmplitude, channels, factorizationModel,
  "ParameterRules" -> {Me -> 1, EE -> 1}, "Output" -> "Details"];
report["Table"]
```

Use appropriate parameter values for the theory. The same checker also accepts
diagram pairs directly. Its default table combines physical-residue checks and
automatically discovered spurious-pole checks; no user pole list is required.
Request `"Residues"` for visible sample values, `"Summary"` for counts, or
`"Details"` for assumptions, charts, denominator discovery, and diagnostics.

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
- `PhysicalPoleResidue` without a coherent kinematic map extracts only a
  `FORMAL` coefficient. Automatic simultaneous-cut charts are not implemented;
  multiple-pole checks require a suitable custom map. Read the guide first.
- These diagnostics do not automatically repair an amplitude or establish
  uniqueness. Do not modify the checker, model, or signs merely to obtain PASS.

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
only on reading code. Do not start training, use the server, switch branches,
commit, or push unless requested.
