# Working With HEPCAT

Use HEPCAT to simplify expressions, derive and combine diagram amplitudes,
find compact forms that satisfy physical checks, and compare expressions when
the task calls for equality. Work through the algebra with the package and
retain reproducible intermediate steps and their assumptions.

Read this guide before a derivation or code change. Do not use em dashes.
Show ordinary Mathematica expressions. Avoid adding `// nice` or `nice[...]`
unless the user explicitly requests that formatting.

## Find the implementation

Paths in this guide are relative to the repository root.

- `source/HEPCAT.wl` loads the package.
- `source/HEPCAT-Base.wl` implements spinor algebra and explicit identities.
- `source/HEPCAT-Model.wl` implements models, diagrams, factorization,
  preparation, completion, and amplitude checks.
- `source/internal/` contains the model-parameter, color, Feynman,
  numerical-export, optimization, MadGraph, and Help modules.
- `source/internal/export/` contains the C++ adapters and Python generators
  used by SPINAS, CalcHEP, and MadGraph exports.
- `source/internal/proofs/` contains the optional exact Laurent proof tools.
- `models/` contains the ordinary constructive models. Model 1 is the SM,
  model 2 is elementary QED, and models 3 and 4 add known QED amplitudes with
  physical and reference-based pole representations, respectively.
- `tests/reference-models/` contains the matched CalcHEP and MadGraph models.
- `tests/` contains the final QED six-point and SM four-point notebooks.
- `tests/checks/` contains regression suites and `fixtures/` contains their
  known expressions and process-specific derivation helpers.
- `tests/benchmarks/` contains timing studies and direct-formula comparisons.
- `tests/history/` preserves earlier notebooks.

Start with the native Help guide in
`Documentation/English/Guides/HEPCAT.nb` and the relevant function pages.
For detailed development examples, see `tests/development/SIMPLIFICATION.md`.
The original full guidance is preserved in
`tests/development/AGENTS-full-history.md`. Treat historical outcomes as records
at their stated revisions. Consult the relevant papers under `papers/` for
the author's conventions when available.

## Load and use the package

Use a licensed Wolfram kernel and load the package before parsing expressions
that use its symbols. A separate setup cell prevents accidental `Global`
symbols from being created before the package context exists.

```wolfram
Get["~/code/hepcat/source/HEPCAT.wl"]
```

Adjust the checkout path for the local machine. For scripts, the usual macOS
kernel is `/Applications/Wolfram.app/Contents/MacOS/WolframKernel` with
`-script script.wl`. Use an independent kernel for tests and derivations.
Preserve the user's interactive kernel and active calculations.

The normal construction entry point is

```wolfram
ConstructAmplitude[process, diagramModel, factorizationModel]
```

The diagram model supplies elementary interactions. The factorization model
must supply complete lower-point amplitudes for the requested channels.
Use `CreateDiagrams` and the plural `DiagramAmplitudes` for staged work.
`CreateArrowDiagrams` displays the diagrams. Preparation and completion can
be inspected with `PrepareAmplitudes`, `FactorizationAmplitudeChannels`,
`CompleteFactorization`, `CompleteHighEnergy`, and `CheckAmplitudes`.

Keep examples simple and rely on automatic effort defaults. Specify a model,
pole preference, or numerical-reference installation when the task needs it.
`ConstructAmplitude` defaults to `"PolePreference" -> "AllPhysical"`.
Preserve an explicit supported preference throughout the calculation.

## Apply identities with their assumptions

All analytic external momenta are incoming. Particle labels identify positions
in the process list. Respect HEPCAT's angle/square and crossing conventions.
For an outgoing massless leg, reversing its momentum changes its angle spinor
sign and every momentum insertion sign while retaining its square spinor.

`ReduceSpinContractions` contracts internal spin indices.
`ReduceSpinorProducts` applies ordinary identities and external on-shell
relations. Use `ApplySpinorIdentity` and `SpinorIdentityCandidates` when a
derivation needs explicit, inspectable edits. `ApplyToNumerator` applies an
operation to each numerator in the expression's existing term structure.
Preserve the user's factored form unless expansion or combination is needed.

Diagram outputs retain `{massRules, expression}`. Each diagram may use its
own internal on-shell conditions in its numerator. Retain those assumptions
with the diagram and keep its propagator denominators off shell. A prepared
factorization channel may use the shell of that channel alone.
Feynman-rule diagram inputs retain off-shell internal momenta in both
numerators and denominators.

Preparation reports can retain discarded remainders and original targets.
Use `"ReturnDetails" -> True` when subsequent completion needs that history.
Keep the report intact while examining its prepared expressions. A final
completed amplitude carries external mass information so it can be prepared
again using ordinary identities, without diagram-local internal shells.

After combining diagrams, use external on-shell identities and momentum
conservation to compare complete amplitudes. An internal-shell equivalence
of individual diagrams does not establish equality of the complete forms.
Preserve regular remainders when rearranging numerators and denominators.

## Keep the algorithm general

Select identities and computational effort from the expression, declared
masses and spins, channel structure, and available resources. Keep known
QED/SM formulas in examples, models, or test fixtures. Never insert a saved
answer into the general constructor or use a process-name shortcut to pass
a benchmark. Coefficient fitting must use the stated physical constraints.

Model parameters and dependent functions belong to their returned model.
Use `ModelParameter` and `ModelParameterRules` and allow multiple models with
the same parameter names. Missing model amplitudes are zero according to
the model interface. Preserve complete lower amplitudes when sewing.
Keep identical-fermion relative signs and coherent color interference.

The Laurent helpers verify whether a supplied expression is identically
zero after a certified symbolic external-kinematic parametrization. They
use exact polynomial arithmetic, factor witnesses, and all coherent spin
coefficients. They contain no table of QED or SM amplitude answers.
Their native C++ backend optionally uses Boost for arbitrary-size integers
and rational arithmetic. SPINAS itself remains Boost-free.

This optional proof path has supported-chart and resource limits, including
a bound of 16 algebra variables. Denominator checks establish that clearing
factors are not identically zero. They preserve the physical pole loci.
Unsupported input, missing dependencies, nonzero remainders, or a deadline
must retain the original correction direction with an honest status.
Accept a zero identity only after every required certificate passes.

## Validate and maintain the package

Use focused exact identity tests while developing. For shared preparation,
completion, or checking changes, add the affected public call and a small
cross-multiplicity set with opposite helicities. Run the broad component
suite once the change settles. See `tests/checks/README.md` for the current
suite selection and integration settings.

```sh
python3 tests/checks/run-wolfram-tests.py --timeout 900
python3 -m unittest discover -s tests/checks -p 'test_*.py' -v
```

The first timeout is per suite. Select expensive integrations explicitly
with repeated `--suite` arguments. Record failures, skips and timeouts.
Use the matching SPINAS revision and CTest for numerical-library changes.
Keep timing sweeps separate from concurrent heavy calculations.

Check physical residues, spurious poles, reference independence, high-energy
behavior, and the requested independent Feynman comparison. For CalcHEP or
MadGraph comparisons, match parameters, zero widths, particle content,
crossing, color treatment, initial averages, and final-state factors.
Square the modulus for each external spin/helicity state before summing.
An overall phase is allowed in squared comparisons. Retain a failed point
and its seed. Distinguish exact identities from finite sampled evidence.

Update `Documentation/Source` whenever a public function, option, format or
workflow changes. Public functions use full CamelCase names. Internal helpers
have internal visibility. Keep examples runnable and update their verified
output cache. Follow `Documentation/README.md` to rebuild and check Help.
Avoid restoring retired lowercase public spellings or adding formatting
wrappers to user workflows.

Report the expression obtained, assumptions, identities or algorithm changes,
checks actually completed, and remaining limitations. Preserve the user's
work and keep long derivation histories in the development references.
