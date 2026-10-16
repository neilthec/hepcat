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
- `tests/checks/`: three symbolic regression suites and their timeout runner.

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

The suites cover symbolic identities, diagram construction, and the curated
simplification paths. They do not load model weights or start training.
