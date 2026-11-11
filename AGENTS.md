Ask your AI to read this file before working with HEPCAT.

# Using HEPCAT for Amplitude Derivations

This guide is for a user who asks an AI to use HEPCAT to support a physics
calculation. Help derive new amplitudes, simplify expressions, combine
diagrams, discover useful representations and check proposed results. The AI
should develop and explain the mathematical reasoning, then use the package
in a Wolfram kernel to apply identities and test its ideas.

The automatic constructor is one useful tool. A derivation may also require
choosing reference spinors, exploring identities, comparing alternative forms
or working through a difficult step explicitly. Keep the user's objective in
view and retain reproducible intermediate steps and their assumptions.

Read the relevant source and examples as the calculation requires. This guide
does not replace the implementation or guarantee that every requested
simplification is known. A user can begin by asking their AI to read
`AGENTS.md` and describing the calculation they want to investigate.

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
- `tests/checks/` contains the complete regression suite in `checks.zip`.
- `tests/benchmarks/` contains the timing and evidence tools in `benchmarks.zip`.

The publication checkout keeps optional material in three readable ZIP
bundles. Loading HEPCAT automatically makes its full Mathematica Help
available from a user cache. To inspect documentation source, regression
tests or reproduction tools, first run this from the checkout root:

```sh
python3 tests/run-checks.py --unpack-only
```

Then read `Documentation/English/Guides/HEPCAT.nb` and the relevant function
pages, or use Mathematica Help. The tutorial
`Documentation/English/Tutorials/ApplyingSpinorIdentities.nb` gives a small
runnable example of the explicit identity workflow. Historical notebooks
and development notes remain on the `factorization` development branch;
they are not dependencies of this checkout. Consult the author's papers for
conventions when supplied by the user.

## Load and use the package

Locate the actual checkout from the workspace or Git root. Do not assume a
particular account, installation directory, or checkout name. All paths below
are relative to that root. From a Wolfram working directory set to the root:

```wolfram
Get[FileNameJoin[{"source", "HEPCAT.wl"}]]
```

Use a licensed Wolfram kernel and load the package before parsing expressions
that use its symbols. A separate setup cell prevents accidental `Global`
symbols from being created before the package context exists.

Locate the licensed Wolfram kernel using the local configuration or installed
executables. Its path depends on the operating system and installation. For
scripts, invoke that kernel with `-script script.wl`. Use an independent kernel
for tests and derivations. Preserve the user's interactive kernel and active
calculations.

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

## Work through a derivation

1. Establish the requested task: simplify an expression, derive an amplitude,
   compare two forms, or reconstruct a form satisfying physical constraints.
   Record the starting expression, external particles, mass rules and allowed
   assumptions. A simplification task need not have a known target expression.
2. Read the relevant functions' `::usage`, `Options` and implementations in
   `HEPCAT-Base.wl` or `HEPCAT-Model.wl`, and the applicable focused tests.
   Start with the required identities and tools rather than reading every
   implementation file. The tutorial
   `Documentation/English/Tutorials/ApplyingSpinorIdentities.nb` gives a small
   runnable example of the explicit identity workflow.
3. Use `SpinorIdentityCandidates[{massRules, expression}, opts]` to inspect
   legal edits and `ApplySpinorIdentity` to apply a selected edit with the same
   string-named options. The latter returns an expression, so rebuild
   `{massRules, expression}` for the next step. Regenerate candidates after
   each change; reject `$Failed` and stale edits.
4. Keep a reproducible record of each substantive before/after expression,
   identity and assumption. For a manually proposed rewrite, test the
   difference between its before/after expressions using the applicable
   symbolic identities.
   Sampled kinematic comparisons help detect errors but are not analytic proofs.
   Preserve an unresolved step as unresolved.
5. Track `SpinorExpressionComplexity` when searching for a simpler form, while
   retaining the best verified expression and respecting the requested pole
   preference. Mark completion corrections as reconstruction steps and record
   the physical constraints that determine them.
6. For a complete amplitude, use `CheckAmplitudes` with the process's actual
   factorization channels and model. Inspect failed or unresolved rows and run
   the requested independent Feynman, CalcHEP or MadGraph comparisons. Report
   both what was established symbolically and what was tested numerically.

If an automatic routine stalls or returns a complicated form, investigate the
remaining algebra directly. Propose another identity, reference choice or
organization of terms and check it with the tools. A known expression may be
used as a comparison, with its source stated. When no target is known, retain
the best verified form and explain the evidence and remaining uncertainties.
The package is a means of checking the reasoning, not a substitute for it.

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
For explicit identity edits on a combined or final amplitude, pass
`"OnShellChannels" -> {}` to disable internal-shell assumptions. Enable
momentum conservation only with the complete declared external particle set.
Preserve regular remainders when rearranging numerators and denominators.

## Respect the model and theory

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
This optional proof path has supported-input and resource limits. Inspect its
status and certificates before accepting an exact zero; a timeout or an
unsupported expression is unresolved. The source headers explain the backend
and its limitations when this route is relevant to the calculation.

## Check results and maintain reproducibility

For a derivation, run the focused symbolic and numerical checks that establish
its steps and final result. The package regression suites are useful examples
of how to test an identity. If the task also changes package code, add a
regression for that change and follow `tests/checks/README.md` for focused and
broader component testing. Shared preparation and completion changes need
cross-multiplicity checks with opposite helicities.

```sh
python3 tests/run-checks.py
```

The runner unpacks the complete tools, runs the Python tests, then the
standard Wolfram suites. `--timeout 900` is per Wolfram suite. Select expensive
integrations explicitly with repeated `--suite` arguments; use
`--python-only` or `--wolfram-only` for focused work. Record failures, skips
and timeouts.
Use the matching SPINAS revision and CTest for numerical-library changes.
Keep timing sweeps separate from concurrent heavy calculations.
Run wall-clock-limited construction notebooks and integration campaigns one
at a time when verifying automatic effort defaults. The active calculation
can use its internal parallel workers. Pause between calculations rather than
suspending an active `TimeConstrained` call. Parallel reading and code review
can continue while the scientific workflow runs.

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
