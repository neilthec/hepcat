# Symbolic Simplification

The `factorization` branch keeps the symbolic identities and worked
simplification paths. The neural models, training pipeline, and their original
notebooks are preserved on the `ml-simplify` branch at commit `54f6920`.

## Files

- `source/HEPCAT-Base.wl`: the identity tools, loaded by `HEPCAT.wl`.
- `source/HEPCAT-SPINAS.wl`: complete C++ process export; its accompanying
  `source/spinas-runtime.hpp` supplies spin projection and example phase space.
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

## Automatic Reduction

`ReduceSpinorProducts[expression]` performs process-independent spinor algebra.
Momentum slots are linear, including scalar coefficients, sums and nested
`Multiparticle` sums. Long chains use the same reversal convention as short
ones, retaining explicit spin-index labels and variance. Existing Clifford
contractions and external Dirac identities shorten chains; trace reductions
preserve their chiral ordering, including contractions across the trace boundary.
Physical `PropDen` factors remain opaque.
Shared scalar content is extracted from polynomial sums before applying the
existing Schouten rules, so preexpanded coefficient-weighted identities are
recognized too. This adds no inverse Schouten rewrite or denominator combination.

A directed diagonal-sandwich contraction also recognizes
`[a|P Q|a] [b|P p_a|b]` and its angle-bracket counterpart, with a massless
helicity spinor on leg `a`. In HEPCAT's conventions the ordinary identity is

```text
[a|P Q|a] [b|P p_a|b]
  = 2 P^2 (p_a.Q) [ab]^2 + 2 (p_a.P) [ab] [b|P Q|a].
```

The opposite-chirality product has the corresponding ordinary reduction

```text
[a|P Q|a] <b|P p_a|b>
  = 2 (p_a.Q) <b|P|a]^2 - 2 (p_a.P) <b|P|a] <b|Q|a].
```

Its angle/square dual is supported too. This exposes useful contractions in
mixed-helicity expressions while retaining the off-cut scalar-product term.

The second term is retained: `ReduceSpinorProducts` does not set `p_a.P` to
zero. Reversed diagonal momentum orders and positive integer powers are
supported. The contraction reduces the number of chain momentum insertions
and has no inverse automatic rule. Step and prospective expansion limits
retain the original product when the new pass would grow excessively.

A massive Dirac--Schouten sum contraction also applies when the other
endpoints are massive. For an external leg `a`,

```text
<a b> <r|p_a|s] - m_a <r b> [a s]
  = <a r> <b|p_a|s].
```

Apply the two-dimensional Schouten identity to the angle rows,
`<a b><r| = <r b><a| + <a r><b|`, then use the external Dirac equation
`<a|p_a = m_a [a|`. The square/angle dual follows in the same way.
There is no momentum-conservation or internal-shell assumption. Fixed
explicit spin indices are retained.

The reducer contracts matching sums in this direction, including bounded
batches with a common mass term. Each accepted result strictly reduces both
the completed term count and complexity. Inverse factors and physical
propagators remain protected, and finite term, step and expansion limits
bound the additional pass. Standalone reduction uses literal `Mass[a]`;
preparation can match a model's mass parameter using the supplied external
mass rules. A parameter from another model or an undeclared mass is never
identified just because it has the same printed name. Preparation compares
the complete candidate with its previous result and retains common scalar
and denominator factors.

Massive-endpoint bracket contractions expose the corresponding external
Dirac remainder before a diagram's internal shell is used. For a massless
leg `k` and an arbitrary reference spinor `r`, the ordinary identity is

```text
<a k> <r|p_a|k]
  = m_a <r k> [a k] + 2 (p_a.p_k) <a r>.
```

Apply Schouten to the two angle rows first:

```text
<a k> <r|p_a|k]
  = <r k> <a|p_a|k] + <a r> <k|p_a|k].
```

The external Dirac equation replaces the first chain by `m_a [a k]`;
the massless diagonal sandwich in the second is `2 (p_a.p_k)`.
The reference may be an independent Xi. Its momentum-row counterpart is

```text
<a k> [k|P p_a|k]
  = 2 m_a (P.p_k) [a k] - 2 (p_a.p_k) <a|P|k].
```

Both identities have angle/square duals and preserve a fixed explicit massive
spin index. They require no momentum conservation or internal shell condition.
For the diagram with its own electron cut `(p_a+p_k)^2 = m_a^2`, preparation
may then set the remainder proportional to `2 (p_a.p_k)` to zero in that
diagram's numerator. The original propagator remains off shell. When a
physical reference supplies `P = p_b`, its factor `2 (p_b.p_k)` may cancel
only a recorded auxiliary reference propagator, never an original diagram
propagator. The ordinary reducer retains both terms.

Rank-one factorization, such as `[a|p_k|r> = [a k]<k r>`, can remove another
momentum insertion and expose cancellation of a reference bracket. The ordinary
reducer uses a physical `Helicity` spinor in a neighboring bracket as an
explicit massless witness; a Xi identifier does not supply this information.
It also reduces `<k|P|k]` and `[k|P|k>` to `2 (p_k.P)` for the same massless
physical leg. Preparation can additionally use a declared external integer
leg's zero mass rule when no witness is present. An internal massless composite
does not qualify: preparation does not invent helicity spinors for that
off-shell line. The resulting complete candidate competes with the unsplit
route, with physical propagators still protected.

A common vector can also be contracted before multilinearity expands it.
For a physical massless leg `b`, the ordinary two-component determinant
identity gives

```text
[a|Q|b> <c|Q|b] = Q^2 [a b] <c b> + 2 (Q.p_b) <c|Q|a].
```

The determinant contribution is `Q^2 [a b] <c b>`. The remaining contraction
contains the diagonal sandwich `<b|Q|b] = 2 (Q.p_b)`. This holds for a
composite `Q`, both angle/square choices, and explicit massive spin indices.
Adding a physical endpoint's own null momentum to a one-momentum sandwich
does not change it; an Xi with the same integer identifier supplies no such
permission. All scalar remainders are retained in `ReduceSpinorProducts`.

The corresponding adjacent-vector contraction is useful on longer charged
chains. With `F = [a|Q|b>` and `R = alpha Q + lambda p_b`, ordinary Clifford
algebra gives

```text
F <c|Q R L|d>
  = Q^2 (alpha F <c|L|d> + lambda [a b] <c b> <b|L|d>)
    + 2 lambda (Q.p_b) <c|Q|a] <b|L|d>.
```

Here `alpha` is `1` or `-1`, and `L` is the untouched trailing momentum
chain, possibly empty. Every branch has fewer momentum insertions. A bounded
prepass finds the shared vectors before per-chain expansion obscures them;
the existing expansion guard remains in force. This is an ordinary off-cut
identity, not a license to discard either scalar remainder.

In a particular diagram, two declared adjacent cuts can subsequently simplify
these scalar factors. For example, if `Q = p_1+p_a`, `p_b^2 = 0`, and the
mass list explicitly states `Q^2 = (Q+p_b)^2 = m^2`, their difference gives
`Q.p_b = MomProd[1,b]+MomProd[a,b] = 0`. Preparation solves the linear
scalar-product equations from all cuts of that one diagram together. External
momentum conservation contributes additional equations only when enabled.
No division by symbolic mass parameters is needed. Inconsistent exact cut
data are reported; approximate mass data do not certify extra exact rules.
These rules act only on protected numerators, and compete with the existing
pairwise route instead of forcing one scalar-product parametrization.

This function neither infers the external-particle list nor assumes momentum
conservation. Expanding a composite momentum does not put it on an internal
mass shell. Keep process-dependent conservation and diagram-local internal
shell reductions in `PrepareAmplitudes` instead.

During preparation, composite-momentum complements can be reduced inside
individual numerator chains before expanding the complete product. Their
quality is compared after ordinary external-shell reduction and the scalar
shell rules explicitly enabled for that element's numerator. Thus an equal-size
complement can still be useful when endpoint terms or allowed cut remainders
vanish. Disabling internal shells supplies no such cut rules. This remains
process-dependent preparation and is disabled with
`"MomentumConservation" -> False`.
Literal linear momentum slots proportional to the complete declared external
sum are reduced to zero before massless splitting can hide that sum inside
brackets. The declared labels are used directly, including sparse labels;
unknown legs and protected inverse factors are not erased.
The conserved and unmodified routes are compared after reducing the whole
product. A locally shorter chain does not replace a better complete expression,
and the main preparation stage retains the winning reduced reference trial.
Ordinary reductions of individual chains are retained while the product is
still factored, even when no conservation alternative improves that chain.
This can expose a sandwich-pair contraction before expansion obscures it.
Completed reference trials also compare a factored route: reduce each chain,
apply only the enabled numerator dot rules, optionally contract the directed
sandwich pairs, and cancel recorded auxiliary reference factors. The surrounding
product need not undergo the older whole-product Schouten expansion. Ordinary
and conserved routes compete after completion, so a factored candidate is kept
only when the whole expression has a better rank.
One competing route ranks conservation alternatives for an open massive chain
after factoring insertions of external momenta whose declared masses are zero.
Two apparent three-term alternatives can have very different final costs: a
complement
containing the emitter's own momentum and the external photons can reduce to
bare brackets, while the original complement retains momentum insertions.
This uses the external mass rules and only the explicitly enabled numerator
dot rules. It neither licenses an internal massless spinor nor adds momentum
conservation to `ReduceSpinorProducts`.
The earlier conserved normal form remains a separate whole-expression
candidate. In particular, reference sandwiches retain their paired-contraction
opportunity before massless splitting is tried. A better individual-chain rank
does not justify discarding a better whole-product route.
Another bounded competitor keeps charged chains intact while replacing a
composite by its negative external complement. It then tries the shared-vector
contractions before expanding the chain. For example, six-particle conservation
turns the adjacent momenta `p4+p6, p1+p2+p3` into `Q, -Q-p5`, where `Q = p4+p6`.
The existing adjacent-vector identity can recognize the latter representation.
The raw complement is ranked only after the full contracted candidate is
finished; at most eight such alternatives are inspected. Conservation remains
optional, and original propagators and other inverse factors remain protected.
One bounded alternative distributes numerator sums with inverse factors held
opaque, then applies the directed contractions before the older whole-product
rearrangements. This exposes useful bracket factors while allowing recorded
auxiliary reference poles to cancel separately in each summand. The expansion
guard and the comparison of completed expressions keep this an alternative,
rather than forcing expansion of every input.

Reference trials finish the cheap factored routes before attempting expensive
whole-product reductions. Each completed finite candidate is checkpointed.
If an optional later route exceeds its slice, the finished candidate remains
eligible; the interrupted exploration is still recorded as incomplete. The
ordinary preparation clock and reference-choice limits are unchanged.
For an even number of external massless reference legs, selection also tries
a bounded set of admissible reciprocal assignments, such as `a -> b, b -> a`.
They often expose a shared-vector contraction. The original first tuple is
retained, so `"MaxReferenceChoices" -> 1` still selects the same references.
The matching search is capped and does not materialize the Cartesian product.
These are general ordering heuristics, not process-specific helicity rules.

The five-point all-positive electron/three-photon example in the thesis proposal,
Eq. (90), exercises this ordering. Both admissible physical reference choices
reduce to one term per diagram, with one two-momentum sandwich remaining. The
particular reference choice in Eq. (90) is checked against its explicit formula,
including photon-label permutations and the coupling normalization read from
the model. These are diagram-local numerator representatives, not a claim that
their sum already passes every factorization channel.

The mixed six-point cases `e,E,A.+,A.+,A.+,A.-` and
`e,E,A.+,A.+,A.-,A.-` have 24 photon-order diagrams each. The new shared-vector
and adjacent-vector identities, joint cut equations, and completed-route
ordering are general reductions; the production code does not substitute
these processes' known answers. With `"MaxSteps" -> 0`, the unchanged
five-second reference budget and 20-second element budget, the representative
AllSpurious trials gave:

| Helicity order along the charged line | Complexity | Expanded terms |
| --- | ---: | ---: |
| `+++-` | 14 | 2 |
| `++-+` | 42 | 6 |
| `+-++` | 14 | 2 |
| `-+++` | 14 | 2 |
| `++--` | 3 | 3 |
| `+-+-` | 52 | 4 |
| `+--+` | 28 | 12 |
| `-++-` | 28 | 12 |
| `-+-+` | 52 | 4 |
| `--++` | 3 | 3 |

The two end-block balanced cases contain no numerator momentum insertions.
The corresponding manually derived enclosed-helicity forms reach complexity
22 with six expanded terms, so the automatic 28 is not the best known form.
Finite reference selection and algebraic normal forms can also yield different
quality after photon relabeling; the table is a representative comparison,
not a uniform bound or a minimum theorem. Same-reference comparisons must use
each diagram's own three cuts while preserving its auxiliary reference
denominators. These local simplifications do not establish that the summed
off-cut amplitude already passes completion or independent Feynman checks.

A separate full run with default `Automatic` references and `"MaxSteps" -> 0`
prepared all 48 diagrams, about two minutes per process. All 384 exact
same-reference comparisons passed: two rational mass/kinematic charts and all
four massive-spin components per diagram. Each original propagator remained
present and each reference was assigned. Across all photon orderings, the
observed complexities ranged from 14 to 76 for one negative helicity and from
3 to 68 for two negative helicities. The wider ranges, compared with the
representative table, reflect the bounded selection and reduction ordering.

Physical references can introduce additional `PropDen` factors. Preparation
records which factors came from those references and may cancel them against
their ordinary external pair invariants. The diagram's original physical
propagators, equivalent complementary cuts, and unrelated inverse factors
remain protected. An internal-shell condition never licenses canceling an
original diagram denominator. The default output remains the prepared
amplitude pairs; reference provenance and reduction records are optional details.
Known composite masses are looked up independently of particle-label order,
without dropping repeated labels or inventing values for unknown masses. These
diagram-local replacements remain restricted to protected numerator paths.

Preparation retains its best completed candidate when a later normalization
expands it into a more complex equivalent expression. An already-contracted,
reference-free expression with ordinary external spinors can also compete as
an input candidate, after contraction and reference stages have succeeded.
Raw internal spin sums, unresolved references, auxiliary spinors and Feynman
tensors cannot bypass those stages. Restoring a simpler candidate is recorded
as `RetainedBestCompletedCandidate` in optional details; it does not change a
partial stage into a success or assert a new shell identity.

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
simplification paths, physical residues, spurious poles, high-energy behavior
and reference independence, and bounded amplitude preparation and completion.
They do not load model weights or start training.

The default run also includes the original algebra, crossing, spin-index,
energy, conjugation, color, model-reader, factorization and export workflows.
See [the regression coverage guide](checks/README.md) for the inventory and
the repaired legacy-bug regressions. The five former known-failure probes
are ordinary assertions now and fail the normal run if a bug returns.
Use `--suite legacy-algebra-tests.wl` for a focused run, and run the runner's
own tests with `python3 -m unittest discover -s tests/checks -p test_runner.py`.

## Export a Runnable SPINAS Process

Give the physical incoming and outgoing particles, then the related
all-ingoing particle lists and amplitudes. For example, after calculating
the three canonical two-photon cases in the notebook:

```wolfram
outputDirectory = FileNameJoin[{$HomeDirectory, "spinas-output", "eeAA"}];
WriteSPINASProcess[outputDirectory,
  {"e", "E"} -> {"A", "A"},
  {
    {{"e", "E", "A.+", "A.+"}, qed4PP},
    {{"e", "E", "A.+", "A.-"}, qed4PM},
    {{"e", "E", "A.-", "A.-"}, qed4MM}
  }, factorizationModel,
  "ProcessName" -> "eeAA", "Build" -> True, "RunExamples" -> True]
```

The arrow separates the two incoming particles from the outgoing particles.
Their concatenated order fixes the expression's numbered legs: no particle
relabeling is done. Supply the original all-ingoing expressions, without
crossing them yourself. The exporter applies `ReverseMomentum` to the outgoing
legs, including their angle-spinor signs, exactly once. For charged outgoing
particles it checks the corresponding antiparticle in the all-ingoing list.
For example, physical `e,A -> e,A` uses all-ingoing `e,A,E,A`.

The mixed-helicity permutation is obtained by relabeling the identical photons.
Each supplied formula is emitted once and reused for the equivalent
arrangements. Only permutations within the incoming or outgoing legs are used;
the exporter does not assume parity or invent missing zero amplitudes.
An incomplete family returns `Failure` before creating project files.

A generic massless name such as `"A"` requests both helicities. A name such as
`"A.+"` fixes that leg's helicity, using the existing SPINAS convention rather
than reversing the sign in the process description. This works on either side
of the arrow. For example, polarized Compton scattering needs only these two
expressions:

```wolfram
WriteSPINASProcess[polarizedDirectory,
  {"e", "A.+"} -> {"e", "A"},
  {
    {{"e", "A.+", "E", "A.+"}, comptonPP},
    {{"e", "A.+", "E", "A.-"}, comptonPM}
  }, factorizationModel]
```

Alternatively, `"FixedHelicities" -> {leg -> doubledHelicity}` selects fixed
massless helicities with either grouped calling form. For example,
`"FixedHelicities" -> {4 -> -2}` fixes the fourth leg to negative photon
helicity. Omitted legs are summed. Supplied expressions must cover every
requested configuration; an absent configuration is not assumed to vanish.

The grouped class provides `amp(spins)` and `amp(ds1, ds2, ...)`, using doubled
spins/helicities as in native SPINAS. `spin_sum()` returns the raw sum of
`std::norm(amp(...))` over all external massive spins and the requested massless
helicities. `amp2()` additionally
divides by the incoming state counts and multiplies by the final-state symmetry
factor. Fixed incoming helicities are not averaged. For each identical
outgoing species the symmetry factor is `1/n!`, counting
all helicities together and including identical fermions. There is no incoming
identical-particle factorial. Thus `eeAA` has `amp2() = spin_sum()/8`: four
incoming spin states and two identical final photons. Three or four final
photons contribute `1/6` or `1/24`, respectively. Colored families are currently
rejected because the exporter has no general color-sum implementation.

Fixing an outgoing helicity gives a labeled partial contribution with the same
identical-species factor. Sum all labeled helicity sectors to recover the full
unpolarized result. The complex `amp(...)` values retain phases and spin labels
for future spin-correlated production and decay calculations. Coherent
production-decay contraction is not automated; multiplying separately summed
`amp2()` results would discard the correlations.

The four-argument amplitude form below remains available for a single fixed
massless-helicity expression, with its original normalization conventions.
After calculating and checking `finalAmplitude`, write a complete project with
its ordered particle list and the model supplying its particle and parameter
data. For example, for the all-positive two-photon amplitude:

```wolfram
outputDirectory = FileNameJoin[{$HomeDirectory, "spinas-output", "eeAA"}];
WriteSPINASProcess[outputDirectory, {"e", "E", "A.+", "A.+"},
  finalAmplitude, factorizationModel,
  "SPINASDirectory" -> "/path/to/spinas",
  "Build" -> True, "RunExamples" -> True]
```

The ordinary result is the project directory. It contains `process.hpp`,
`process.cpp`, `main.cpp`, `spinas-runtime.hpp`, `CMakeLists.txt`, `README.md`
and `manifest.json`. Compilation and execution are opt-in; both default to
`False`, and `"RunExamples" -> True` also builds. The example executable prints
three on-shell, momentum-conserving physical points and amplitude values.
The existing SPINAS library must already be built; it is linked rather than
copied or rebuilt. `"SPINASDirectory" -> Automatic` checks `SPINAS_ROOT` and
the sibling `spinas` checkout. `"Precision" -> Automatic` reads its build
configuration. Existing project files are protected unless
`"Overwrite" -> True`; `"ReturnDetails" -> True` adds file paths and build/run
reports.

Generated `process.cpp` files use the familiar SPINAS product names: `a12a`
for an angle bracket, `s34s` for a square bracket and `s314a` for a momentum
chain. Comments display the corresponding bracket expression. The formulas
use model parameter aliases such as `Me` and `EE`, named physical denominators
such as `t13`, and separate, wrapped numerator and denominator expressions.
Repeated scalar products and propagators are calculated once per component
evaluation. Small powers of named primitives use multiplication and inverse
factors use division.
Names are disambiguated when spin slots, masses or model names coincide.

The `helicity_pp`, `helicity_pm`, etc. methods contain the canonical formulas.
The separate `amp` and `spin_sum` methods handle leg permutations and normalized
spin projections. This keeps one compiled formula per supplied helicity sector
without expanding every identical-particle permutation into another formula.
The generic spin machinery is retained for arbitrary multiplicity and spin;
the generated style is not claimed to outperform a hand-optimized native
SPINAS process. Unlike native `sproduct` members, the local primitive aliases
are recomputed for a component evaluation. The `ldouble` casts preserve
compatibility with both SPINAS precision builds. Formatting changes do not
simplify or otherwise alter the supplied amplitude.

When building from desktop Mathematica, CMake is found on `PATH` or in standard
installation locations, including the macOS CMake application and Homebrew.
No shell setup is required. For a custom installation, set
`"CMakeExecutable" -> "/full/path/to/cmake"`. A missing or unusable CMake is
reported before files are generated; source-only export does not require it.
The detailed build report records the executable used.

The generated class is `hepcat_generated::hepcat_process`; set another class
and executable name with `"ProcessName"`. Call `set_momenta`, then `amp` with
a doubled-spin array. Massless helicities are fixed by the exported process;
massive states use normalized symmetric spinor tensors. `spin_sum` sums
massive external spins and `amp2` also averages incoming massive spins.
Neither includes an identical-particle phase-space factor, a sum over other
massless helicities, or external color sums or averages. Colored processes
require an explicitly color-stripped amplitude.

Supply the analytic amplitude in HEPCAT's all-ingoing convention. C++ momenta
have positive energies. The physical arrow form selects the incoming and
outgoing legs; older calls use `"IncomingLegs" -> {1, 2}` by default. The exporter
reverses outgoing momenta and angle spinors, leaving square spinors unchanged.
Internal momenta and physical propagators remain off shell. It preserves the
supplied coefficients and HEPCAT's explicit `+i m width` denominator sign.
It does not choose a normalization or repair the amplitude during export.

Parameter defaults are a resolved snapshot of the supplied model, with local
`"ParameterRules"` overrides if wanted. No global parameter variables are
created. The generated `Parameters` array supports complex couplings; its
order is recorded in the manifest and README. When changing its
values, keep dependent parameters consistent. Complete spinor expressions,
amplitude mass-rule pairs and successful preparation/completion reports are
accepted. Resolve Xi/Zeta spinors, x factors, uncontracted spin/color tensors
and unsupported model functions first. Unresolved or unsupported input returns
`Failure` before creating a partial project. Export supports massive spins
through two and definite massless helicities, with two incoming legs and at
least four external legs for the example generator. These demonstrations do
not replace `CheckAmplitudes`.

The focused regression suite compiles and runs four-, five- and six-point
projects against independent native SPINAS formulas. See
[the export coverage and convention notes](checks/README.md#spinas-process-export).

## Independent CalcHEP Check of Generated C++

`WriteSPINASProcess` also writes a `validate_calchep` method and a standalone
`processName_validate_calchep` executable. Enable the build and immediate check
with one option on an ordinary complete-helicity export:

```wolfram
WriteSPINASProcess[outputDirectory,
  {"e", "E"} -> {"A", "A"},
  {
    {{"e", "E", "A.+", "A.+"}, qed4PP},
    {{"e", "E", "A.+", "A.-"}, qed4PM},
    {{"e", "E", "A.-", "A.-"}, qed4MM}
  }, factorizationModel,
  "RunCalcHEPValidation" -> True]
```

The default is 1,000 points and seed 12345. `"CalcHEPPoints"` and
`"CalcHEPSeed"` override them. No new build option is needed: requesting this
check builds the project. Existing files still require `"Overwrite" -> True`.
For a previously generated project, run its validator independently:

```sh
./build/eeAA_validate_calchep --points 1000 --seed 12345
```

In C++, link the generated `eeAA_calchep_validation` library and call
`process.validate_calchep(options)`, where `options` is a
`hepcat_spinas::ValidationOptions`. The report exposes the pass status, counts,
largest errors and first failing momentum point. The validator uses a copy of
the process, preserving the caller's current momenta.

The CalcHEP C interface creates and compiles the counterpart process at runtime
using `getMEcode`, then evaluates `sqme` at each common point. Particle names
are obtained by PDG, including outgoing antiparticles and self-conjugate
photons. Current independent parameters are assigned, constraints are evaluated
and required derived values and external masses are checked. Unknown or
incompatible values produce an error rather than a mismatched comparison.
CalcHEP's `sqme` includes the initial-state average and outgoing
identical-particle factor, so the complete family uses `amp2()` directly.
The legacy complete-massive-spin overload receives its missing final-state
factor from the physical particle inventory. No numerical normalization is fit.

`"CalcHEPDirectory" -> Automatic` finds a local installation, including a
versioned directory below the sibling `CalcHEP` folder. The generated CMake
project also accepts `-DCALCHEP_ROOT=/path/to/calchep`. The bundled constructive
QED models are paired automatically with `model_feynman` model 2, which has a
massive electron. Other theories require
`"CalcHEPModel" -> {modelDirectory, modelNumber}`. External PDGs alone cannot
distinguish QED from the full electroweak theory. `None` disables model pairing.
No model tables in the installation or repository are modified. Temporary
private copies adapt the compact HEPCAT table headers to CalcHEP's layout.

This backend compares complete, unpolarized, colorless processes. A partial or
fixed-helicity family, color-stripped result or unsupported model reports
`UNAVAILABLE`; it cannot produce a passing validation. Ordinary SPINAS projects
remain usable without CalcHEP. Link the separate backend when using its method.

Random sequential decays vary the total energy and decay directions while
maintaining external masses and momentum conservation. These are diagnostic
points, not uniformly weighted phase-space integration events. Both evaluators
receive the same double-valued momentum components. The default relative
tolerance is `1e-7` with zero absolute tolerance, preventing a wrong tiny result
from passing through an absolute floor. The C++ options or executable tolerance
arguments allow an explicit override. Disagreements and nonfinite evaluations
are counted, never discarded. The first failing point is retained for replay.
The CalcHEP backend uses process-global C state and runs serially in a private
workspace; concurrent CalcHEP sessions fail explicitly.

## Amplitude Checks

`CheckAmplitudes` is the general checker. `CheckPhysicalResidues` remains a
compatibility name with the same options and behavior.

`tests/Factorization-checks.nb` is a short, self-contained walkthrough with
four- and five-point QED. Each example uses the ordinary diagram amplitudes,
the original compact factorization channels and the model:

```wolfram
diagrams = CreateDiagrams[particles, diagramModel];
amplitudes = diagramAmplitudes[diagrams];
channels = FactorizationAmplitudeChannels[particles, factorizationModel];
CheckAmplitudes[amplitudes, channels, factorizationModel]
```

The normal checker output combines physical poles, apparent spurious poles,
a high-energy row showing observed growth and its expected limit, and a
reference-independence row. Results are `PASS`, `Opp Sign`, `FAIL` or
`UNRESOLVED`. `Opp Sign` means the
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

### Independent Feynman Comparison

Supply a separately calculated, spinor-converted Feynman amplitude to add an
off-cut equality test to the same report:

```wolfram
feynmanPairs = diagramAmplitudes[CreateDiagrams[particles, qedFeynmanModel]];
preparedFeynman = PrepareAmplitudes[feynmanPairs, qedFeynmanModel,
  "PolePreference" -> "IndependentXi", "MaxSteps" -> 0];
CheckAmplitudes[finalAmplitude, channels, factorizationModel,
  "FeynmanAmplitudes" -> preparedFeynman,
  "FeynmanModel" -> qedFeynmanModel]
```

The candidate and Feynman amplitude must use the same particles, external
ordering, helicities, masses and couplings. This comparison uses external
on-shell kinematics away from physical and spurious poles, not the
factorization cuts. No additional overall sign or normalization is fitted.
Each external photon or gluon must use the same reference in every Feynman
diagram; individual diagrams are not gauge invariant. `"IndependentXi"` retains
one generic reference per external particle, fixed across the entire sum.
`"MaxSteps" -> 0` keeps exact conversion and ordinary spinor reduction but skips
the optional identity search: an independently checkable benchmark need not be
individually minimized diagram by diagram.
`"Output" -> "Feynman"` requests its numerical comparison details.
`"FeynmanSamples" -> Automatic` uses `"Samples"`; `"FeynmanTimeConstraint" -> Automatic`
uses the ordinary `"TimeConstraint"`.
Without `"FeynmanAmplitudes"`, no Feynman benchmark is generated or run.

For rational tree amplitudes, the comparison prefers exact rational complex
momenta to avoid expensive square-root arithmetic, especially with several
massive external particles. Every external mass shell and momentum conservation
still holds, and every known physical propagator must be nonzero. If that
construction is unavailable, real center-of-mass samples remain the fallback.
The detailed sample chart records `"SamplingConstruction"`. These are sampled
analytic comparisons, not physical phase-space probabilities; the separate
high-energy test continues to use real scattering kinematics.

`tests/QED-5-point-construction.nb` constructs this benchmark independently for
the all-plus and mixed-helicity three-photon cases and the identical-four-fermion
single-photon case. It reads the separate
massive-QED Feynman model 2. Each model retains its own mass and charge
definitions, with matching numerical values for this example; the checker
uses the supplied `"FeynmanModel"` for the benchmark's parameters rather than
identifying the two models' parameter objects or changing either model.
The local SM Feynman model 1 also restores electron, muon, up- and down-quark
masses with their matching Higgs/Goldstone rules. Its light-quark defaults
match constructive models 1, 4 and 5; see `model_feynman/README.md` for their
PDG source and mass-scheme convention. Match the supplied numerical parameters
before comparing amplitudes from different models.

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

Options include `"Samples" -> 4`, `"RandomSeed" -> 1729` and
`"TimeConstraint" -> 30` (per pole, and separately for discovery).
`"ParameterRules" -> Automatic` is the default: numerical checks and completion
resolve the parameter values and derived functions from the supplied model.
Neither the symbolic amplitude nor the model is assigned numerical values
globally. For an optional parameter study, override only the desired values:

```wolfram
CheckAmplitudes[amplitudes, channels, factorizationModel,
  "ParameterRules" -> {"Me" -> 1, "EE" -> 1}]
```

Readers keep parameters as inert model-local data, not per-name global symbols.
`ModelParameter[model, "Me"]` retrieves a symbolic parameter, and
`ModelParameterName[parameter]` gives its display name. `ModelParameterRules[model]`
returns the model's numerical values and resolved dependent functions without
assigning anything globally. For example:

```wolfram
electronMass = ModelParameter[diagramModel, "Me"];
electronMass /. ModelParameterRules[diagramModel]
```

The parameter still displays as `Me` inside amplitudes. Models with different
parameter data retain distinct parameter objects even when their printed names
coincide. Keep the model with its expressions; do not merge parameters by name
or assign `Me = ...` or `EE = ...` to configure a model.

Explicit rules overlay the model defaults locally, and dependent model
functions are recomputed. Choose values consistent with the theory. Missing
or unresolved numerical data remains visible as `UNRESOLVED`; it is not
silently replaced with unit parameters. Automatic `PASS` means agreement at the recorded samples, not
a general analytic proof or a test restricted to real physical phase space.
Massive external spinors are sampled coherently, not exhaustively enumerated.
Reference spinors are evaluated literally and distinctly by their Xi labels.
When adding diagrams, use consistent reference choices for each external
particle. The checker does not silently reinterpret diagram-local Xi counters.

### More Information on Request

Use `"Output"` to choose how much to display:

```wolfram
CheckAmplitudes[amplitudes, channels, factorizationModel,
  "Output" -> "Residues"]
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

### High-Energy Behavior

The ordinary checker call also tests the full summed amplitude against the
fixed-angle renormalizable-tree bound `E^(4-n)`. Thus four-point amplitudes
may approach a constant; five-point amplitudes must fall at least as `1/E`.
This is an explicit theory assumption, not something inferred from the model
file. Override it with `"HighEnergyBound" -> exponent`, or disable this test
with `"HighEnergyCheck" -> False`. This diagnostic does not determine an
allowed constant contact coupling such as the four-Higgs coupling.

```wolfram
report = CheckAmplitudes[amplitudes, channels, factorizationModel,
  "Output" -> "Details"];
report["Table"]
report["HighEnergyTable"]
report["HighEnergyCheck"]
```

`"Output" -> "HighEnergy"` directly displays the high-energy sample table.
`"HighEnergySamples" -> 3` controls the number of reproducible kinematic
shapes. `"HighEnergyTimeConstraint"` defaults to the ordinary
`"TimeConstraint"`, applied separately to the complete energy diagnostic.
Numeric, real, nonnegative external masses are required and are resolved from
the supplied model by default. `"ParameterRules"` optionally overrides them.
Masses and couplings remain fixed while
the energy grows. Unspecified couplings, unsupported structures, singular
charts and timeouts produce `UNRESOLVED`, never an assumed zero.

The energy test uses real, momentum-conserving `2 -> n-2` kinematics for
`n >= 4`, with external shells enforced exactly. Outgoing spatial momenta
scale together; their energies and the incoming two-body kinematics are
recomputed with the same fixed masses. Outgoing momenta and angle spinors
are crossed to the package's all-ingoing convention. Internal propagators
remain off shell. A small-parameter Laurent expansion of the whole summed
amplitude captures cancellations rather than merely counting individual
terms or fitting values at a few large energies.

For implicit massive spinors, coherent spin polynomials retain all symmetric
external spin components, including longitudinal vector states. Exact powers
are obtained at each sampled kinematic shape. `PASS` is evidence on those
shapes, not a proof for every scattering direction or a complete gauge or
reference-independence test. A vanishing truncated expansion is reported as
an upper bound on growth, not as an identically vanishing amplitude.

Set `"SpuriousPoles" -> {}`, `"HighEnergyCheck" -> False` and
`"ReferenceCheck" -> False` to obtain the previous physical-only report.
`"Output" -> "Residues"` remains the
physical/spurious-pole sample view; energy samples have their own view above.
No correction to the amplitude or new model interaction is introduced.

### Reference Independence

The default report also tests dependence on explicit `Spinor["Xi", ...]`
reference spinors, such as the arbitrary photon and gluon polarization
references. It varies every distinct Xi label simultaneously, including both
angle and square spinors, while holding all momenta and physical external
spin states fixed. The full summed amplitude is tested away from physical
poles, not on the individual factorization cuts. No internal on-shell
identities are imposed.

```wolfram
report = CheckAmplitudes[amplitudes, channels, factorizationModel,
  "Output" -> "Details"];
report["ReferenceTable"]
report["ReferenceCheck"]
```

`"Output" -> "References"` displays the reference comparisons directly.
`"ReferenceSamples" -> 3` selects the number of reproducible exact external
kinematic points. At each point, symbolic reference coordinates test changes
of direction, not just a uniform rescaling. Angle and square reference spinors
are linked to a null reference momentum. Implicit massive external spins are
retained as coherent polynomials, so changing Xi does not change the physical
external states. `PASS` proves the tested reference cancellation at the
recorded kinematic points, not at all possible momenta.

Distinct Xi labels are treated literally, without silently identifying
diagram-local labels. A shared Xi used for several external particles remains
a shared reference: its test covers that common choice, not independently
chosen polarizations for those particles. Keep reference assignments
consistent when summing diagrams. The detailed report records the scope.

An expression without explicit Xi receives `PASS` with scope
`"NoExplicitReferences"`; this records the absence of explicit reference
dependence, not a Ward-identity proof. Unexpanded x factors, unsupported
structures, singular reference choices, unresolved coefficients and timeouts
produce `UNRESOLVED`. Reference independence is distinct from comparing
different off-cut continuations produced by internal numerator shell choices.

`"ReferenceTimeConstraint"` defaults to `"TimeConstraint"`, applied separately
to the entire reference diagnostic. Automatic reference-bearing comparisons
currently use real `2 -> n-2` kinematics for `n >= 4` and require numerical,
real, nonnegative external masses. Unsupported kinematics remain `UNRESOLVED`.
Use `"ReferenceCheck" -> False` to disable it. The checker never changes an
amplitude or its reference assignments to obtain agreement.

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

`"StructuralCoefficient" -> False` skips the additional formal calculation
with opaque propagators. This does not change the physical residue or its
kinematic checks. The low-level default remains `True`; mapped checker and
completion calls skip this unused diagnostic to avoid expensive symbolic
algebra before evaluating a large amplitude on its cut.

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

### Worked QED Identity Routes

`tests/QED-5-point-residues.wl` also contains the process-specific worked
routes used by both QED notebooks. Loading it does not run tests or change
the package, the model, or the checker:

```wolfram
Get[FileNameJoin[{hepcatDirectory, "tests", "QED-5-point-residues.wl"}]];
diagrams = CreateDiagrams[{"e", "E", "E", "e", "A.+"}, qedDiagramModel];
route = q5FourFermionPhotonRoute[diagrams];
route["Final"]
```

The four-fermion route retains each actual diagram, its exact Dirac
reduction, the explicitly removed numerator multiple of its own electron
on-shell condition, and the nonzero off-cut remainder. It does not set
either propagator denominator to zero. Pairing the two photon attachments
on a fermion line then cancels Xi by an exact two-component Schouten
identity. Four radiating-current blocks give the final expression with
physical propagators only. The negative-photon form follows by swapping
angle and square spinors, and is checked independently.

The general reducer and preparation now reproduce the useful local steps
without calling this process-specific route. For `e,E,E,e,A.+`, fast
preparation (`"MaxSteps" -> 0`) keeps six expanded terms per diagram:

| Reference choice | Previous complexity | Improved complexity |
| --- | ---: | ---: |
| Common Xi, all eight diagrams | 30 | 22 |
| Physical, first four diagrams | 38 | 38 |
| Physical, remaining four diagrams | 44 | 38 |

The physical-reference cases previously retained eight terms in the last four
diagrams. Rank-one photon-momentum factorization, the directed bracket
contractions, and the bounded distributed alternative remove the extra terms.
Only the current diagram's numerator shell conditions are used. Pairing and
reference cancellation on a common-Xi sum remain distinct from simplifying
each diagram. Independently optimized physical reference choices can change
the off-cut continuation, so the prepared sum still needs the completion and
full-amplitude checks.

For the completed radiation amplitude, the existing four-current expression
remains the best found in the tested forms: 32 expanded terms, complexity 160.
Whole-expression ordinary reduction gives 38 terms and complexity 190;
factor collection and the five whole-numerator momentum eliminations tested
did not produce a smaller result. Preparation retains the compact completed
form. These comparisons do not establish a global minimum.

The six-point extensions `e,E,E,e,A.+,A.+` and `e,E,E,e,A.+,A.-` each have
40 diagrams, including both identical-fermion pairings. Their radiation
reductions can be organized by where the two photons attach. Applying the
ordinary spinor identities and only the diagram's own numerator cuts gives
the following compact local forms:

| Photon attachment | `++`: complexity / expanded terms | `+-`: complexity / expanded terms |
| --- | ---: | ---: |
| Same endpoint of one fermion line | 8 / 8 | 56 / 8 |
| Opposite endpoints of one fermion line | 56 / 8 | 58 / 10 |
| Different fermion lines | 57 / 9 | 57 / 9 |

These current-level forms were derived and checked against every actual
diagram on two exact rational own-cut charts. The complete polynomial in
four massive-spin doublets was compared, covering all 16 spin components
without selecting one physical polarization. The sequential `++` form has
no numerator momentum insertions. The other forms retain momentum dependence
in compact sandwiches. This is a comparison of useful forms, not a global
minimum proof or a claim that preparation always reaches every table entry.

The general preparation improvement is the reduction order described above:
normalize helicity reference chains while keeping charged chains intact,
retain raw conserved complements until paired contractions have been tried,
and rank open charged-chain complements after permitted external massless
factorization. The earlier whole-expression route remains a competitor.
No process-specific current formula is substituted by the package.

With `"PolePreference" -> Automatic` and `"MaxSteps" -> 0`, the final
automatic run prepared all 80 diagrams in about two minutes per process.
All 160 exact polynomial comparisons passed, covering the 16 spin components
at both mass/kinematic assignments, with all three original propagators kept.
Every mixed-helicity diagram reached the corresponding table entry: 16
same-endpoint diagrams at 56, eight opposite-endpoint diagrams at 58, and 16
split-line diagrams at 57. For `++`, all 16 sequential diagrams reached
complexity 8 and the eight opposite-endpoint diagrams reached 56. The split-line
diagrams retained complexity 59 or 80 with 11 or 12 expanded terms; the
derived nine-term form at 57 remains a useful target for further general
search improvements. These comparisons validate diagram-local numerator
reductions; full amplitude completion remains a separate stage.

For `e,E,A.+,A.+,A.-`, use:

```wolfram
pairs = diagramAmplitudes[CreateDiagrams[
  {"e", "E", "A.+", "A.+", "A.-"}, qedDiagramModel]];
mixedRoute = qedFivePointMixedRoute[pairs];
mixedRoute["Final"]
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

The completed mixed amplitude was also compared with alternative ordinary
identity forms, using no internal shell conditions on the sum:

| Form | Complexity | Expanded monomials |
| --- | ---: | ---: |
| Existing three physical blocks | 71 | 7 |
| Two regrouped physical blocks | 72 | 8 |
| Momentum-free inner brackets | 89 | 9 |
| Grouped completed spurious form | 186 | 18 |

The three-block expression remains the best found by this comparison. The
two-block alternative follows by substituting the two external identities

```text
<5|p1 p2|5> = -<35><5|p1|3] - <45><5|p1|4],
t15 + t25 = <35>[35] + <45>[45].
```

They turn the charged-spin tensor in the third physical block into

```text
<12><5|p1 p2|5> + <15><25>(t15+t25)
  = -<35>(<12><5|p1|3] - <15><25>[35])
    -<45>(<12><5|p1|4] - <15><25>[45]).
```

Substitute this expression and collect the two parenthesized tensors in the
first two blocks. No pole has been put on shell: the steps use momentum
conservation, the massless external photon equations and ordinary spinor
linearity. `qedFivePointMixedEqualityChecks[]["FinalRegroupingIdentities"]`
checks the two identities by exact polynomial reduction on the external
equal-mass kinematic variety. The two-block formula has fewer displayed
groups but more expanded monomials, so it does not replace the default final
expression. This bounded comparison is not a proof of global minimality.

### Six-Point Six-Fermion Preparation

For `e,E,e,E,e,E`, the cubic QED model generates 36 diagrams: six fermion
matchings, three possible central fermion lines, and two attachment orders.
Each diagram has two internal photon propagators and one internal electron
propagator. There are no external photon references to choose.

The direct contraction of the two sewn four-fermion currents gives 16 terms:
eight mass terms and eight chains containing the composite internal momentum.
Those composites hide additional momentum branches. The ordinary massive
Dirac--Schouten sum contraction above removes matching mass contributions
after external momentum reduction, using the correct leg's declared mass.

| Form | Complexity | Expanded terms |
| --- | ---: | ---: |
| Spin contractions only, composite momentum retained | 128 | 16 |
| Previous automatic preparation | 96 | 32 |
| Improved automatic preparation | 88 | 24 |

The improvement requires neither internal on-shell conditions nor momentum
conservation. Preparation still supports those operations separately when
enabled. The new sum contraction keeps fixed spin indices, model-local mass
parameters and all three original propagators. It introduces no additional
denominators. Common scalar and denominator factors are retained in the
displayed result, so the smaller numerator does not duplicate a long common
coefficient in every term. This is the best form found in these comparisons,
not a proof that no shorter equivalent expression exists.

A full automatic `"MaxSteps" -> 0` run prepared all 36 diagrams at 88/24
in about 90 seconds, including all six identical-fermion matchings and their
exchange signs. All three original propagators remained intact. Verification
compared complete polynomials in six massive-spin doublets: 72 comparisons on
the diagrams' own cuts and 72 independent ordinary-algebra comparisons away
from the cuts, where total momentum was not constrained. Each comparison
covers all 64 spin components, with two exact rational mass assignments.
The off-cut tests kept every physical propagator nonzero. These checks
validate the reduction; completion and validation of the summed amplitude
remain separate steps.

### Automatic Physical and Spurious Checks

The ordinary call checks physical residues and discovers apparent spurious
denominator poles automatically. No pole list is required:

```wolfram
CheckAmplitudes[amplitude, channels, factorizationModel]
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
deduplicates exact denominator obligations before factoring and classification,
and retains distinct forms and explicit denominator powers. The
actual surviving pole order is determined by the Laurent check on the sum.
Supported spurious factors include massless external brackets, external/Xi
brackets and quadratic momentum invariants. At integer denominator powers,
chains containing only explicitly massless external momentum insertions are
factored into alternating angle/square brackets, and two-momentum traces of
those legs are reduced to scalar products. This changes the denominator
inventory only: limits still evaluate the original full amplitude. It uses
no internal-shell assumptions. Unsupported chirality, massive or composite
insertions, identically zero denominators and branch-sensitive powers remain
visible rather than being discarded. Generic momentum chains,
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

Compound rational denominator factors also have a generic checking route.
An exact massless-spinor mixing preserves every external shell and total
momentum while varying a curve parameter. The checker reduces the full sum
on each curve, then checks that its denominator is coprime to the suspected
pole polynomial. Every root of that polynomial is covered, including its
separate components. Roots shared with supplied physical propagators are
rejected rather than counted as spurious passes. Several compound poles share
one whole-amplitude reduction per sample. The result remains sampled evidence;
it does not establish a global analytic identity. Constant, unsupported or
unfinished pole maps remain unresolved.
All compound poles share one `"TimeConstraint"` budget, not that budget
multiplied by their count. Details record
`"TimeConstraintScope" -> "SharedCompoundPoleGroup"`; ordinary pole checks
retain their per-pole clocks. A stopped group preserves every unresolved row.
This compound mode currently requires at least three massless and at most two
massive external particles and homogeneous, typed Lorentz-covariant factors.
Its auxiliary channel coordinate varies together with the massless spinors;
holding the massive pair fixed would miss hidden invariant factors. The older
bracket and scalar-invariant checks retain their existing scope.
Sampling first chooses units from a nonzero external mass, where available,
then restores the original physical momenta and spinor normalization. This
changes only the sampled kinematics, never the amplitude, model masses or
couplings. Details retain `"KinematicScale"` and the physical external masses.
Each distinct typed kinematic primitive is reduced exactly before insertion
into the complete rational sum. This call-local reuse prevents repeated large
polynomial expansion; unsupported primitives retain the original evaluation
path rather than receiving an assumed value.

## Preparing and Completing an Amplitude

`ConstructAmplitude` coordinates the full process workflow. The individual
construction stages remain available, and `CheckAmplitudes` still only checks
its input; it never changes an amplitude.

### Complete Process Workflow

```wolfram
amplitude = ConstructAmplitude[
  {"e", "E", "A.+", "A.+", "A.+"},
  qedDiagramModel, qedFactorizationModel]
```

The wrapper generates the diagrams and their amplitudes, prepares them,
generates and prepares the factorization channels, completes factorization,
prepares that result, completes high-energy behavior, prepares once more,
and runs the final checks. Ordinary output is the validated expression.
The notebook display shows the current stage, its elapsed time and total
elapsed time, refreshing once per second. At the end it prints total time,
then the test table, before returning the expression. In a terminal, stage
start/end messages and throttled child updates include both elapsed times.

The original factorization rows remain the physical-residue targets, while
prepared channels and discarded remainders help completion.
A preparation search that reaches its per-item limit can continue from its
finite last completed expression when the requested references and conversion
are established and denominators remain off shell. Rejected reductions,
inconsistent invariants and conversion/reference failures stop construction.
This bounded-search policy is the same for every output mode. The factorization
family is transported through ordinary preparation without dropping its free
directions. Both post-completion preparation steps use external identities
only, with internal on-shell conditions disabled and existing references left
unchanged. High-energy completion and final validation use fresh default
sample seeds, distinct from the factorization fit. Explicit stage seeds can
still be supplied for reproducible investigations.

An independent Feynman comparison is optional because it needs its own model:

```wolfram
amplitude = ConstructAmplitude[particles, diagramModel, factorizationModel,
  "FeynmanModel" -> qedFeynmanModel]
```

Feynman diagram references are shared across their full sum, and their internal
numerators stay off shell. The main workflow uses the constructive diagram model; the
factorization model must contain constructive lower-point amplitudes. Particle
helicity aliases such as `A.+` are resolved against the underlying Feynman
particle declarations without adding model globals.

`"Samples"`, `"ValidationSamples"`, `"ParameterRules"`, `"HighEnergyBound"`,
`"RandomSeed"`, `"Parallelization"` and `"MaxKernels"` provide common settings.
`"MaxKernels" -> Automatic` uses Mathematica's processor count, available
license slots and the number of independent jobs. There is no two-worker cap.
For example, `"MaxKernels" -> 4` reduces the worker count when desired; `1`
keeps execution serial. Independent jobs use a dynamic queue, so a free worker
can start another diagram or sample without waiting for a slower item. Results
retain their input order and progress reflects completed work. Completion's
simultaneous linear solve remains serial. Startup and serial stages can still
limit the speedup of a short whole calculation.

A controlled local preparation comparison used the 24 diagrams of
`e,E,A.+,A.+,A.+,A.+`, model 2, `"AllPhysical"`,
`"MaxReferenceChoices" -> 1`, `"MaxSteps" -> 0` and a 120-second per-item limit.
Mathematica reported eight processors. Exact prepared expressions agreed
between serial, parallel and both source versions:

| Preparation mode | Workers | Elapsed time |
| --- | ---: | ---: |
| Serial | 0 | 121.7 s |
| Explicit two-worker cap, warm pool | 2 | 60.9 s |
| Earlier automatic memory estimate, warm pool | 5 | 25.5 s |
| Dynamic queue with automatic processor count, warm pool | 8 | 15.4 s |

The new cold call took 22.1 seconds, including 6.7 seconds of startup. These
measurements concern preparation at the stated search limits, not the whole
construction or an exhaustive simplification claim. A separate uneven-duration
queue test at a fixed two-worker count fell from 1.78 to 1.01 seconds, isolating
the gain from removing wave barriers. Returned job values remained identical.

The resource and effort defaults are `Automatic`; a four-, five- or six-point
calculation needs no tuning options. Preparation begins with reference selection
and the mandatory ordinary reductions, including its correctly scoped numerator
shell conditions. Its automatic extra identity-search allowance is zero, avoiding
eager candidate enumeration before testing whether the compact input suffices.
A positive `"MaxSteps"` still requests additional strictly improving identity edits.
Automatic reference selection compares supported spurious and physical choices
using the reduced expression. If neither construction nor its preparation
reports requests an explicit pole preference, completion can first try a compact
physical-only correction space, then fall back to the inferred reference choices
within its remaining budget. An explicit preference is preserved.

Finite stage clocks and correction allowances depend on lightweight measurements:
external multiplicity, diagram/channel count, expression size and retained family
size. No expansion or kinematic assumption is used merely to choose resources.
Factorization starts with 128 correction directions below six points and 256 at
six points. A supplied correction list contributes its actual length to this
initial automatic choice. If a complete physical union has already been built,
its projected independent count can raise an automatic cap immediately, reusing
that union before trying smaller fallback spaces. Both this adjustment and any
later capacity retry share the original stage deadline and automatic ceiling.
A large raw catalog does not trigger another capacity retry when its complete
projected space already fits. Directions are never silently truncated. Explicit
numeric limits are never increased, including in nested calls.
Completion details distinguish `"RequestedPolePreference"` from
`"AchievedPhysicalDenominators"`. A validated fallback may remain `"COMPLETE"`
while displaying denominator factors outside the physical-propagator inventory.
Its `"PreferenceFallbackReason"` and progress message make that distinction
explicit; passing spurious-pole checks does not mean the factors disappeared
from the displayed expression.
The preferred-pole proposal has a channel-scaled sub-budget: at most
`Max[90, 30 channelCount]` seconds and three quarters of the remaining stage
clock. Multiple-particle cuts reserve two thirds of that proposal budget for
source construction; simpler cuts reserve one third. These are shared deadlines,
not separate clocks restarted for every source.

Automatic fit samples start at four and increase with the actual unknown count
relative to the available channel/coherent-spin equations. Independent validation
still uses four samples by default. Validation is not weakened to make a larger
problem finish. Explicit sample counts override these choices. Exact samples and
held-out checks retain their existing verification scope.

The overall clock is finite too. Its baseline allowance is ten minutes at four
points, thirty minutes at five points and two hours at six points, with a small
work allowance; successful stages return immediately. These are resource limits,
not predicted run times or guarantees that an arbitrary correction space closes.
Stage baselines likewise scale with workload, and final checks receive their own
automatic budget within the remaining overall time. Detailed output retains
`"ResourcePolicy"` with requested and resolved values and factorization
`"AdaptiveAttempts"` explaining any capacity retry.

Override a setting only when a different budget or representation is wanted:

```wolfram
amplitude = ConstructAmplitude[particles, diagramModel, factorizationModel,
  "FactorizationOptions" -> {"TimeConstraint" -> 1200,
    "MaxCorrectionTerms" -> 256},
  "CheckOptions" -> {"Samples" -> 3}]
```

The other bags are `"PreparationOptions"`, `"ChannelPreparationOptions"`,
`"FinalPreparationOptions"`, `"HighEnergyOptions"`, and
`"FeynmanPreparationOptions"`. Stage-specific values override common values;
within an option list the first duplicate rule wins, as in ordinary Wolfram
option handling. The wrapper controls intermediate output/progress formats,
post-completion on-shell scope, and the required final checks.

Request `"Output" -> "Amplitude"` for `{externalMassRules, expression}`, ready
for `PrepareAmplitudes` or `CheckAmplitudes`. `"Output" -> "Details"` retains
`"Stages"`, `"StageTimings"`, `"Checks"`, `"TotalTime"`, and the amplitude pair.
It also records `"RequestedPolePreference"`, `"AchievedPhysicalDenominators"`
and `"PreferenceFallbackReason"` for the final expression. A passing physical
amplitude and a preferred denominator representation are separate results.
If `AllPhysical` was requested but not established, the ordinary final report
prints that limitation before the time and checks. Detailed output retains it
even when printing is disabled; `COMPLETE` alone is not a claim that the
denominator preference was achieved.
`"ProgressReporting" -> False` suppresses ongoing progress;
`"PrintReport" -> False` suppresses the final time and test table. A custom
`"ProgressFunction"` receives `"StageStarted"`, `"Progress"`, and
`"StageFinished"` events with stage and total elapsed seconds.

Every required physical, automatically discovered spurious, high-energy,
reference and optional Feynman row must pass. Failure, opposite-sign,
unresolved or underdetermined outcomes return a compact `Failure` instead of
an uncertified expression. Detailed output preserves the stage diagnostics
and available candidate explicitly. If construction stops before final
validation, the displayed test row says `NOT RUN`. Automatic resource escalation
can enlarge a source-derived correction allowance; it never changes the original
physical targets, permits a new independent interaction, or relaxes a check to
force agreement.

### 1. Prepare the Diagrams and Channels

```wolfram
particles = {"e", "E", "A.+", "A.+", "A.+"};
qedDiagramModel = ReadModel[FileNameJoin[{hepcatDirectory, "models"}], 2];
qedFactorizationModel = ReadModel[FileNameJoin[{hepcatDirectory, "models"}], 3];
pairs = diagramAmplitudes[CreateDiagrams[particles, qedDiagramModel]];
diagramPreparation = PrepareAmplitudes[pairs, qedDiagramModel,
  "ReturnDetails" -> True];
prepared = diagramPreparation["PreparedAmplitudes"];
channels = FactorizationAmplitudeChannels[particles, qedFactorizationModel];
channelPreparation = PrepareAmplitudes[channels, qedFactorizationModel,
  "ReturnDetails" -> True];
preparedChannels = channelPreparation["PreparedAmplitudes"];
```

`PrepareAmplitudes` recognizes each element's format: diagram pairs
`{massRules, expression}`, or compact factorization rows
`{mass1, ..., massN, Multiparticle[...], internalMass, expression}`.
It returns the same format, preserving the metadata and replacing only the
expression. Original inputs and reduction history are omitted unless
`"ReturnDetails" -> True` is requested. `PrepareDiagramAmplitudes` remains
available as a compatibility name.

For a `FeynmanModel`, diagram pairs can be passed directly: preparation first
converts their tensors and ordered Dirac chains to spinor products. Internal
Feynman numerators and denominators remain off shell throughout; only external
shell identities and ordinary algebra are used. No diagram-local internal-shell
substitutions are made merely because the pairs list propagator masses.
Prepared Feynman pairs carry external mass rules only, avoiding accidental
reuse of internal propagator masses as constructive shell assumptions.
Both Feynman and constructive inputs use `"MaxSteps" -> Automatic`, which
initially resolves to zero extra edits. A positive value opts into additional
identity search; conversion and ordinary reductions still run at zero.
Feynman reference choices are shared across the entire diagram list.
`Automatic`, `"AllSpurious"`, `"Cyclic"` and `"ReverseCyclic"` use a fixed
consistent massless-particle cycle rather than optimizing each diagram
independently. `"Xi"`, `"IndependentXi"` and `None` are also supported.
`"AllPhysical"` chooses deterministic momentum-insertion references from the
shared process pole inventory, preferring massive external targets. Each
external gauge leg uses the same target in every diagram, even if its reference
pole already occurs in a particular diagram. No internal-shell assumption is
introduced. `"Mixed"` remains explicitly `"PARTIAL"` in details until a safe
whole-amplitude reference search is available. Unavailable requested references
return `Failure` in ordinary output instead of silently retaining Xi spinors.
After changing a reference choice, reevaluate both preparation and the checker.
Previously computed amplitudes and reports do not update when the choice or
package definitions change.

For constructive diagrams with just one massless external leg, `Automatic`
falls back to momentum-insertion references. Explicit `"AllSpurious"` cannot
use that leg as its own reference and returns `Failure` rather than silently
retaining Xi spinors. Request `"ReturnDetails" -> True` to inspect the retained
partial expressions and the reason a reference choice is unavailable.

For a factorization row, only its declared cut is an internal on-shell
condition. Its momentum-conservation complement describes the same cut, not
an additional assumption. Other propagators inside the lower-point amplitudes
stay off shell. Physical denominators, the sewing sign and `z` are preserved.
Preparation preserves the selected-cut residue; it need not preserve the
channel's continuation away from that pole. Keep the two preparation reports
and pass them to completion, as below. The original channel rows remain the
residue targets; their simplified forms and retained differences supply
correction candidates. Ordinary pairs and rows remain supported, but they
carry no hidden history. For legacy plain prepared-channel rows, originals
can still be supplied with `"AlternativeChannels" -> channels`.

It applies `ReduceSpinContractions`, makes the selected
reference-spinor replacements, then calls `ReduceSpinorProducts` with physical
propagators protected. With `"InternalOnShell" -> True`, it replaces
numerator scalar products for known two-particle invariants using
`(Mass[Multiparticle[i,j]]^2 - Mass[i]^2 - Mass[j]^2)/2` and applies that
diagram's mass rules before and after `ReduceSpinorProducts`. This direct
reduction also runs with `"MaxSteps" -> 0` and is repeated after later identity edits.
Momentum conservation permits using the mass of a complementary internal
channel when its complement has exactly two external particles. Unknown pair
invariants and denominator factors are not replaced. A larger internal
momentum sum does not fix its individual pair products.
An explicitly requested positive `"MaxSteps"` enables the optional bounded
identity search. It can use that diagram's own internal mass rules and accepts
only strictly improving edits; `Automatic` initially uses zero extra edits. No diagram's internal conditions are passed to
another diagram or imposed on the final sum. Reference assignments are recorded
in the preparation details; the checker does not change them.

Choose the references with `"PolePreference"`:

| Value | Choice |
| --- | --- |
| `Automatic` | Compare eligible spurious and physical momentum-insertion references after ordinary reduction, retaining the best completed supported choice. This is the default. |
| `"AllSpurious"` | Optimize over assignments using only eligible other massless external spinors. |
| `"AllPhysical"` | Optimize over momentum-insertion assignments giving physical channels outside that diagram's own propagators. |
| `"Mixed"` | Optimize over both kinds for each reference. The best tested assignment may mix them or use only one kind. |
| `"Cyclic"` | Use a fixed massless-particle cycle, without optimizing the assignment. |
| `"ReverseCyclic"` | Use the reverse fixed cycle, without optimizing the assignment. |
| `"Xi"` | Use the same generic `Xi0` for all external reference spinors. |
| `"IndependentXi"` | Use a separate generic reference for each external particle, consistently labeled across diagrams. |
| `None` | Keep the input reference spinors unchanged. |

For example:

```wolfram
preparationDetails = PrepareAmplitudes[pairs, qedDiagramModel,
  "PolePreference" -> "AllPhysical", "ReturnDetails" -> True];
preparationDetails["DiagramReports"][[1]]["PolePreference"]
```

The optimized modes reduce each tested complete diagram expression before
ranking it by complexity, term count and expression size. They do not choose
references merely by the apparent size of the reference denominator. The search
is finite and bounded: `"MaxReferenceChoices" -> Automatic` currently resolves
to 64 assignments per diagram. `"ReferenceTimeConstraint" -> Automatic` allows the smaller
of five seconds and one third of that diagram's remaining time. A positive
number or `Infinity` can be supplied instead, but the overall per-diagram
`"TimeConstraint"` always applies. The progress display reports choices tried,
the budgeted total and the best complexity found.

With `"ReturnDetails" -> True`, the reference report's `"Search"` records
available, tried, completed and rejected counts, whether the search was
exhaustive, and its stop reason. `"Alternatives"` contains the tested completed
assignments, their reduced expressions and ranks, sorted by rank. The winning
`"ReducedExpression"` and `"ReducedRank"` are retained alongside its unreduced
`"Expression"`. Reaching the choice cap or reference time limit can still return
a valid best-tested assignment; it does not establish a global minimum.
Reference selection does not run factorization checks, and the smallest prepared
diagram need not be the fastest seed for later completion.

For a massless external particle `a`, the physical choice is
`<Xi_a| = [a| p_j` or `[Xi_a| = <a| p_j`. The auxiliary denominator becomes
`PropDen[Mom[Multiparticle[a,j]], mass_j]`. The original diagram propagators
are never put on shell or replaced. The selection excludes each diagram's
own channels, including their momentum-conservation complements, and uses
physical channels represented by the supplied diagram set. Supply the full
`diagramAmplitudes` output so those alternatives are available.

The massless-spinor choices exclude self-reference and massless-pair channels
already known to be physical, which matters for gluons. `Automatic` falls back
to physical references for each external reference with no suitable massless
partner, including a process with only one external massless particle.
If a required choice cannot be established, preparation reports `"PARTIAL"`
rather than inserting a zero
reference denominator or inventing a physical channel. The mode names describe
the reference construction, not other denominators already present in the
expression. These choices do not themselves guarantee that the complete
amplitude passes the checks.
`ReduceSpinorProducts` runs after successful reference replacement, including
when `"MaxSteps" -> 0` disables the later identity search. It acts on the whole
diagram expression, including auxiliary denominators: ordinary spinor identities
can normalize bracket signs and cancel matching spinor factors. Physical
`PropDen` factors remain opaque. External mass replacements apply globally
outside `PropDen`; internal mass replacements and on-shell substitutions remain
numerator-only.

`"InternalOnShell" -> True` remains the ordinary preparation policy.
`"MaxSteps"`, `"MaxCandidates"` and `"TimeConstraint"` default to `Automatic`:
initially zero extra edits, 64 inspected candidates if a search is requested,
and a finite clock based on multiplicity and item size. Each diagram has its
own resolved time limit, including reference selection. Set
`"InternalOnShell" -> False` to disable both the direct invariant reduction and
additional internal-shell edits.
`"MaxSteps" -> 0` retains reference selection, the two basic reductions and the
direct numerator invariant reduction, but disables the identity search.
The later identity search is bounded and greedy, not a promise
of the globally simplest expression. `"Output" -> "Expression"` returns
the prepared sum instead of pairs. `"ReturnDetails" -> True` retains the original and prepared
pairs, per-diagram reductions, accepted identity steps, declared internal
conditions and stop reasons. A timed-out diagram retains its last completed
expression and is reported as `"PARTIAL"` in these details. `"ReturnDetails"`
defaults to `False`; `True` takes precedence over `"Output"`. The explicit
`"Output" -> "Details"` option remains available for existing code.

With `"MomentumConservation" -> True`, preparation also tries eliminating an
external momentum consistently throughout all numerator terms, and replacing
composite momenta by their conservation complements. The external labels come
only from the supplied mass rules, not from a guessed particle count. A trial
may expand temporarily: automatic spinor reduction and the permitted local
shell reductions run before the whole result is ranked. Only a strictly lower
complexity, term-count and expression-size rank is accepted, preventing cycles.
These alternatives share the existing `"MaxCandidates"`, `"MaxSteps"` and
per-diagram time limits with the local identity edits. All inverse factors,
including reciprocal expressions inside a numerator sum, are protected during
conservation and internal-shell substitutions, not during ordinary whole-expression
spinor reduction. Set `"MomentumConservation" -> False` to
disable both local conservation edits and these whole-numerator alternatives.

### 2. Complete the Physical Residues

```wolfram
factorizedAmplitude = CompleteFactorization[
  diagramPreparation, channelPreparation, qedFactorizationModel]
```

The default result is `{externalMassRules, expression}`, not a report. Both
completion stages retain only `Mass[externalLeg] -> mass` replacements in this
pair. Internal numerator shell assumptions are not carried into the completed
amplitude. `"Output" -> "Expression"` returns the bare expression when needed.
`"ReturnDetails" -> False` is the default; `True` requests the full report.
The legacy `"Output" -> "Details"` also remains available. An unresolved
ordinary call returns a compact `Failure` with guidance, without embedding
the candidate or report. Its short `"Reason"` also states the actual failure,
such as exceeding the correction-term cap, rather than implying a timeout.

The completed pair can be prepared directly:

```wolfram
finalAmplitude = PrepareAmplitudes[factorizedAmplitude, qedFactorizationModel];
CheckAmplitudes[finalAmplitude, channels, qedFactorizationModel]
```

A single pair is returned as a single pair. External-only pairs represent a
complete amplitude, so preparation applies external spinor identities,
conservation, reference handling and its bounded simplification search, with
internal on-shell conditions disabled. The physical propagators can supply a
reference-choice inventory; they never supply extra shell assumptions. Lists
of diagram pairs keep their existing list format and diagram-local behavior.
Repeated literal Xi references remain correlated throughout a complete sum;
preparation cannot split a shared reference into different per-particle values.
If a requested physical-reference assignment cannot preserve that correlation,
it remains unavailable rather than changing the amplitude's continuation.
The ordinary automatic mode can retain the shared reference while still
performing the other reductions.
The same pair passes directly through `CompleteHighEnergy` and can then be
prepared again. `CheckAmplitudes` accepts either the pair or its bare expression;
with a bare expression it obtains the external masses from the supplied channels.

### Simplifying a Verified Final Amplitude

Keep only the external mass rules attached to a completed amplitude. For fast
ordinary cleanup without changing its reference choice, use:

```wolfram
simplified = PrepareAmplitudes[completedAmplitude, factorizationModel,
  "PolePreference" -> None, "MaxSteps" -> 0];
CheckAmplitudes[simplified, channels, factorizationModel,
  "FeynmanAmplitudes" -> preparedFeynman, "FeynmanModel" -> feynmanModel]
```

`MaxSteps -> 0` disables the broad identity-candidate search. It still permits
ordinary reductions, the bounded conservation trials, and algebraic compaction.
An external-only pair always has internal shells disabled, even if an internal
propagator occurs in its expression. The original and best completed snapshots
remain competitors throughout preparation.

The ordinary reducer now normalizes repeated contractions once within a call
and compares a factored rational-amplitude route with its distributed route.
Before distribution, structural estimates bound both the number of terms and
the expanded expression size. Large powers and long composite products retain
their completed factored reduction instead of entering an uncontrolled expand.
Bare spinor contractions retain the established canonical rules, including
external Dirac equations and the explicit scalar remainders of sandwich
identities. A bounded inverse-protected factor pass extracts shared spinor
content. None of these operations uses momentum conservation or an internal
shell in `ReduceSpinorProducts`.

Final-amplitude preparation additionally tries sparse grouping by the physical
propagators that terms share and collection of rational coefficients of spinor
atoms. Propagators are independent off-shell quantities in this algebra. It may
use their exact definitions, for example
`2 MomProd[i,j] = PropDen[Mom[Multiparticle[i,j]],M] + M^2 - m_i^2 - m_j^2`,
to cancel a denominator. This retains the propagator term; it is not the
substitution `PropDen -> 0`. Inverse spinor factors also have a protected
competing collection route. Prospective size limits, memory and time budgets
retain a previously completed candidate when an optional route becomes costly.

When conservation is enabled, another bounded trial changes only one-momentum
spinor sandwiches, then uses declared external massless momenta to factor them.
Longer momentum words, scalar invariants and inverse factors are preserved.
This can expose cancellations missed by replacing a momentum everywhere at
once. Scalar `Mom[... ]^2` factors are treated as Lorentz dot products before
any whole-numerator conservation substitution; they never become commuting
products of vector symbols. Complementary physical denominators are normalized
only when conservation is enabled. The final compaction also examines the best
completed checkpoint, so a later expansion cannot hide an earlier useful form.

The rank still compares weighted expanded complexity, displayed additive
blocks and `LeafCount`. Factoring can reduce storage and repeated work while
leaving the weighted expanded complexity unchanged. These are separate gains;
none of the bounded searches establishes a globally shortest formula. Local
ranking results are reused for at most sixteen snapshots of limited size and
are discarded when that preparation call ends.

The following comparisons use the saved notebook completions and the verified
six-point completion outputs as inputs, without replacing them by model formulas.
Times measure preparation only
on the development laptop and exclude package loading, Feynman generation and
checking; they are measurements rather than runtime guarantees.

| Process | Weighted complexity | Expression size (`LeafCount`) | Preparation |
| --- | ---: | ---: | ---: |
| `e,E,+,+,+` | 27 → 27 | 286 → 168 | 0.15 s |
| `e,E,+,+,-` | 148 → 70 | 1687 → 591 | 2.38 s |
| `e,E,E,e,+` | 296 → 232 | 2945 → 1981 | 2.35 s |
| `e,E,+,+,+,+` | 108 → 108 | 1347 → 732 | 0.38 s |
| `e,E,+,+,+,-` | 13953 → 13944 | 184111 → 80489 | 17.30 s |

The five-point results passed fresh independent Feynman comparisons and all
9, 9 and 11 checks, respectively. They also equal their saved inputs at two
exact off-cut points covering every coherent massive-spin coefficient. The
six-point forms passed 17 and 26 checks, respectively, against fresh sums of
24 Feynman diagrams; the latter includes nine spurious-pole checks. All
preparation internal-shell lists are empty. The mixed five-point route also
has a symbolic proof using only external shells and total momentum conservation.

The four QED four-point representatives and the six compact canonical
five-point model expressions retain their original ranks. The all-positive
spurious-pole form and its parity counterpart remain exactly unchanged at
complexity 30 and 417 leaves. Valid inverse brackets are eligible incumbents;
raw endpoint contractions and elementary inverse traces still require reduction.
The existing mixed formula has complexity 71 but seven expanded
terms, whereas the new complexity-70 form has twelve. The known radiation
formula at complexity 160 is still smaller than the automatically obtained
232 form. These are useful comparisons, not evidence of a global minimum.
The balanced six-point recursion is a separate resource stress case. Its
compound spurious factors now have conserved-curve checks; this does not
establish success of the automatic diagram-completion algorithm.

`"PolePreference"` is the common selection option for `PrepareAmplitudes`,
`CompleteFactorization`, `CompleteHighEnergy` and `ConstructAmplitude`. It replaces
the earlier selection-option name. The low-level `FeynmanToSpinorProducts`
`"ReferenceSpinors"` option still accepts explicit spinor replacement rules;
that mapping interface is separate from this high-level representation choice.

Preparation uses the choice to replace the external Xi spinors. Factorization
completion uses the same choice to favor matching correction sources before
trying shifted/general continuations. `Automatic` inherits a requested choice
from detailed preparation reports; with ordinary inputs, pass the choice
explicitly. The constructor forwards its common setting through the stages.

| Choice | Preferred continuation sources |
| --- | --- |
| `"AllPhysical"` | Only the original supplied physical propagators in kinematic denominators. |
| `"AllSpurious"` | References to other massless external legs, allowing auxiliary bracket poles. |
| `"Cyclic"`, `"ReverseCyclic"` | The corresponding fixed massless-reference orientation. |
| `"Mixed"` | Both physical and spurious reference choices. |
| `"Xi"`, `"IndependentXi"` | Common or independent external reference spinors, respectively. |
| `None` | Keep the input reference choice and skip a preferred-source completion attempt. |

Physical propagators are retained in every mode. `AllSpurious` refers to the
auxiliary reference denominators; it does not remove genuine physical poles
or prevent ordinary algebra from canceling unnecessary factors. This is a bias,
not a guarantee of a unique or globally simplest formula.

The physical space uses prepared sources and reduced physical continuations.
An augmented expected-zero residue obligation does not make a new propagator
physical. Prepared channel rows use the normal contour removal and sewing sign.
Grouped selected-cut differences remain proposed off-cut corrections, not
ordinary identities of the full amplitude. Unsupported higher-order physical
propagators are excluded from the current simple-residue attempt.

Every preferred-space fit still enforces all original channels, expected zero
residues, coherent massive-spin components and independent validation parameter
assignments. Explicit regular input stays fixed; no independent contact
interaction is invented. The complete fitted affine family passes to high-energy
completion, which retains the preference. At most half the remaining clock,
capped at 90 seconds, is reserved for the attempt, with at most 15 seconds for
construction. Insufficient or timed-out spaces leave the existing fallbacks
available. Details expose `"PolePreference"`, `"PreferredPoleSpace"` and
`"PreferredPoleFit"` so any fallback is visible.
If the full physical union exceeds the term cap, a separately named proposal
uses the complete reduced physical-channel space before returning to general
fallbacks. This preserves that smaller space's entire affine freedom rather
than truncating arbitrary columns of the oversized union.

With preparation reports, `"PreparationRemainders" -> Automatic` tries
the prepared structures and grouped remainders after the inexpensive input
normalization and any requested physical-pole attempt. Each item's `"RemainderData"` retains the exact
fixed-reference difference `FixedReferenceExpression - PreparedExpression`,
completed stage differences and the particular internal conditions used.
Reference changes are recorded separately: neither changing Xi nor reducing a
diagram numerator on its own cuts is asserted to preserve its full continuation.
The tested reference alternatives retain their unreduced and reduced expressions.
Common-Xi and unambiguous per-leg Xi sources remain available without a cache.

The first correction space groups the supplied diagram and channel differences
by momentum-insertion count. Ordinary pair-invariant identities expose a
virtuality, for example `2 MomProd[i,j] = PropDen[Mom[Multiparticle[i,j]],M] +
M^2 - Mass[i]^2 - Mass[j]^2`; they never set that propagator to zero.
Complete groups with regular terms remain tied rather than becoming arbitrary
independent contact directions. Original physical targets and zero obligations
remain unchanged, and all fits retain coherent-spin and parameter validation.
An exact compatibility probe may reject an inadequate space early; a consistent
or timed-out probe is never a successful completion.

This optional attempt uses at most one fifth of the remaining stage clock,
capped at 12 seconds. Construction uses at most four seconds, with a smaller
internal deadline to retain completed partial groups. An oversized space is
reported rather than silently truncated. The existing channel solvers remain
available if these remainders are insufficient. A retained common-reference
sum may be tried as a final bounded fallback. Set `"PreparationRemainders" ->
None` to compare the earlier route on the same inputs. Explicit correction
bases and literal-channel mode bypass these automatic attempts. Details expose
`"PreparationRemainders"`, `"PreparationRemainderFit"`,
`"PreparationReferenceAttempts"` and `"ChannelPreparation"`.

`CompleteFactorization` solves all supported physical-channel constraints
simultaneously. It does not independently add one residue discrepancy divided
by each propagator, which could double count shared pole contributions.
Before constructing the channel correction space, an input that does not
already pass gets two inexpensive proposals with `"InputReweighting" -> Automatic`:

1. Fit one constant multiplying its complete pole-bearing part. For example,
   residues twice their expected values determine a coefficient of `1/2`.
   This is one simultaneous linear solve, not a search over guessed numbers
   or a division of potentially zero residue components.
2. If needed, collect exact signed terms across separate input diagrams and
   fit one constant multiplying their union. Thus the records `A+B` and `B+C`
   can propose `A+B+C`, while the original input remains `A+2B+C`.

Identical terms can carry a genuine physical multiplicity. Neither proposal
is an automatic identity or a change to how `diagramAmplitudes` adds diagrams.
All original physical-channel targets, including expected zero residues, must
hold on fresh exact sampled charts, in all coherent massive-spin components
and for every validation parameter set. A seed that already passes is retained.
If the proposals fail, the existing general correction solver runs with the
remaining time. Ordinary output still returns only the resulting amplitude.

The overlap proposal compares complete terms, including their signs,
coefficients and propagators. It does not identify `B` with `2 B`, discard
opposite signs, or infer separate diagrams from an already summed expression.
Preparation reports and diagram pairs retain the separate-record information.
Explicit additive terms without `PropDen` denominators keep their original
summed multiplicity in both attempts. This structural separation does not
identify hidden pole-free content inside a pole-bearing numerator.

Use `"InputReweighting" -> None` to disable these attempts. They also leave
explicit `"CorrectionBasis"` choices, including `{}`, and the literal
`"ChannelReferences" -> None` mode unchanged. Input collection has a 4096-term
bound and a small clock limit; both fits together use at most one fifth of
the remaining completion time, capped at 30 seconds. With details requested,
`report["InputFitAttempts"]` records the attempted modes, normalization factors,
overlap counts, equations and held-out validation. Successful correction spaces
are named `"InputRescaling"` or `"InputOverlap"`. The detailed `ConstraintData`
records `FitSeed`, so its fitted coefficient can be interpreted together with
the complete direction in `CorrectionBasis`; for these attempts the seed is
the preserved regular part, rather than the original amplitude.

The five-point all-positive QED example prepares six one-term physical-reference
diagrams with three exact pairs. Their sum is twice `H`; one copy from each pair
is exactly `H`. Both statements can be checked by ordinary rational algebra.
The rescaling attempt recovers this form without channel subtraction terms.
Factorization success still needs the usual spurious, high-energy, reference
and available Feynman checks. Those checks never fit an extra normalization.

`"ChannelCompanions" -> Automatic` then tries a small correction space made
from the supplied lower-point channel expressions. A two-leg cut is eligible
when it joins a declared null external leg to a massive anchor with the same
exchanged mass. Ordinary momentum expansion and declared-massless chain
factorization expose grouped momentum structures; normalization is checked on
that channel's cut. Group by chain/trace insertion count, then remove the
emitted leg from typed lower-amplitude composite labels. Keep the selected
propagator intact. Every completed group is a proposed correction direction,
not a claimed identity or a guaranteed continuation of the original residue.
No known final amplitude, process-specific numerator or new interaction enters.

All propagators must belong to the original physical target catalog, with
simple physical orders. Newly pole-free directions are excluded and explicit
regular input is retained. The subsequent simultaneous fit checks all original
channels, including expected zero residues, with fresh coherent-spin and
parameter validation. A failed compact space leaves the broad fallback intact.
Use `"ChannelCompanions" -> None` to disable it. Explicit correction bases and
literal-channel mode bypass it. Construction is capped at 15 seconds; the
construction and fit share at most one third of the remaining stage clock,
capped at 60 seconds. Partial construction and size/time limits are recorded
in details, not hidden by truncation. `MomentumDegree` in companion mappings
counts chain/trace insertions, not the full high-energy degree of a term.

Its automatic correction directions come from the actual input and sewn
channel expressions, subject to `"MaxCorrectionTerms" -> 64`.
The opt-in `"AlternativeChannels"` list supplies additional continuations only
after the prepared-channel solve fails. Each alternative's grouped selected-cut
residue must agree before its correction directions are admitted; the prepared
channels remain authoritative. Both attempts share the stage's correction-term
and time limits. For plain row inputs the default `{}` does not retain or
reintroduce original inputs. A channel preparation report instead explicitly
retains the original rows as targets and supplies its preparation history.
`"ChannelReferences" -> Automatic` supplements the raw channel expressions
with bounded alternative reference choices: recorded preparation assignments
when available, cyclic choices and physical momentum-insertion choices.
Cyclic choices include intermediate simultaneous rotations when more than two
massless reference particles are available, not just forward and reverse order.
This search has a per-channel rotation limit of 16, with truncation recorded
in the optional continuation diagnostics.
It leaves the user's prepared diagrams unchanged. Preparation details are
needed to recover the actual per-diagram assignments; ordinary amplitude
pairs do not carry that history. Different choices are candidate continuations
away from a channel's pole, not assumed off-cut identities. Each additional
continuation must preserve its selected-cut residue at exact sampled
kinematics before its terms are admitted. This is a bounded correction space,
not an enumeration of all possible amplitudes or a proof of reference equivalence.

`"ShiftedChannelContinuations" -> Automatic` also offers a compact family
derived directly from the original factorization numerators. A deformation of
two null external legs conserves total momentum and their mass shells. For
each channel affected by that deformation, the code chooses the parameter
that puts that channel on shell, evaluates its known lower-point product
there, and divides by its original propagator. This gives a different
continuation of the same selected residue. Each admitted direction must pass
its own cut check before entering the simultaneous fit.

The sum of these directions is a proposal, not an assumed recursion formula:
no vanishing boundary at infinity is assumed. Every original physical channel
and zero-residue obligation remains in the equations. Explicit regular input
stays fixed; its pole-bearing part can be reweighted when `"InputReweighting"`
allows that. Compact signed exchange averages preserve all declared boson and
fermion symmetries. Failed independent validation adds equations before a
refit, and the complete affine family is checked at every requested validation
parameter assignment. The usual high-energy, spurious-pole, reference and
optional Feynman checks remain separate requirements for construction.

Automatic selects opposite null spin weights or identified null scalars where
the current compound-pole checker applies: at least three null and at most two
massive external legs. `True` allows broader generic attempts; `False` or
`None` disables this proposal. Other inputs retain the established routes.
The attempt uses at most four ordered pairs and half the remaining stage
clock, capped at 90 seconds, while respecting `"MaxCorrectionTerms"`.
Detailed reports retain the admitted source checks and attempted fits.

Already contracted rational expressions are kept factored by
`ReduceSpinContractions` when no contraction rule can apply. Its guard checks
both explicit spin-index sums and ordinary massless angle/square contractions;
real contractions still use the established rules. Preparation also retains
an eligible complete input before trying reductions, so a timeout cannot
discard that compact incumbent in favor of a larger partial expansion.
For complete external amplitudes, preparation can apply contraction rules
without global distribution, and checker normalization retains equivalent
bare-helicity products for direct evaluation. Actual summed indices still
require contraction. Ranking small expressions uses the usual complexity;
ranking large factored expressions uses representation size when expanding
them would exceed the reducer's existing distribution budget. The public
complexity function is unchanged.

With automatic channel references, completion first tries a compact space
from accepted physical-reference continuations and complete sewn channels
that already contain no reference spinors. In particular, a four-point-by-four-point
channel at six points must not be omitted merely because no Xi replacement
was needed. Unexpanded x factors and surviving spinors for a cut particle
still require a continuation choice, even without an explicit Xi; they do not
qualify as reference-free sources. Their target residues remain unchanged.
It reduces each whole channel
expression before splitting it into additive terms, using only external
shell identities and momentum conservation. This can expose correction
directions hidden inside momentum chains. Each reduced expression is checked
against its source on that channel's cut before both its raw and reduced terms
are admitted. It also tries numerator-only reduction using that channel's
selected internal mass shell. The off-cut difference is kept as one grouped
correction direction only after checking that its selected residue vanishes.
Other internal lines stay off shell, and all inverse factors remain protected
during the selected-shell substitutions. The input also contributes one grouped pole-bearing expression
per diagram, rather than every individual input term. Explicit input terms
without a physical `PropDen` denominator remain in the seed and are not added
as cancellation directions in this compact space. This is a structural
distinction, not a decomposition of contact pieces hidden inside pole terms.
No independent contact interactions or known final amplitude are inserted.

For a physical-pole request at higher multiplicity, an earlier finite proposal
keeps selected-cut-prepared whole channel expressions as leading fit
coordinates. Their correlated numerators receive one coefficient each, rather
than an independent coefficient for every expanded monomial. The remaining
coordinates come from ordinary reduced physical channel atoms. Exact formal
dependency pruning preserves this selected proposal's span and its ordering
preference for whole sources. Explicit regular input remains fixed.

Each selected source is checked on its own cut before use. Only that channel's
numerator shell may enter its preparation; the full sum is not put on shell.
Every original channel and the complete fitted affine family still undergo
independent validation. This is a smaller reconstruction proposal, not a
truncation of the larger correction space. If it is unavailable or does not
fit, the broader physical union, including additional invariant continuations,
remains a fallback within the same stage deadline. Detailed reports name this
proposal `GroupedSelectedPhysicalSources` and retain its source verification.
No additional user options are required.

If this compact space is unavailable, or fails with time remaining, completion
tries the existing literal termwise input-and-channel space. Both attempts share
the stage's time limit and respect the same correction-term cap; there is no
silent truncation of an oversized space. `"ChannelReferences" -> None` omits
the additional continuations and compact-space attempt. An explicit
`"CorrectionBasis"` still uses exactly the supplied directions, after ordinary
normalization.

`"BoseSymmetryGroups" -> Automatic` and
`"FermionSymmetryGroups" -> Automatic` use the model's twice-spin together with
species and external spinor state to identify all identical-particle groups.
Even twice-spin gives symmetric exchange; odd twice-spin gives antisymmetric
exchange. Four positive-helicity photons on legs 3 through 6 are grouped
automatically. Electrons and positrons form separate groups, and opposite
helicities remain separate. Equal masses alone are insufficient. Compact amplitude pairs and channel rows
retain masses but not particle names. When preparing them, retain the already available process list:

```wolfram
preparationDetails = PrepareAmplitudes[rawAmplitudes, diagramModel,
  "ExternalParticles" -> particles, "ReturnDetails" -> True];
channelPreparationDetails = PrepareAmplitudes[channels, factorizationModel,
  "ExternalParticles" -> particles, "ReturnDetails" -> True];
factorization = CompleteFactorization[preparationDetails,
  channelPreparationDetails, factorizationModel];
```

This adds process metadata only to detailed reports; ordinary pairs and rows
keep their simple form. The ordered names are checked against the model and
external masses. For a plain input, completion can also receive
`"ExternalParticles" -> particles` directly. If names are absent, only unique
matches from the model and homogeneous external spinor weights are used.
Ambiguous species, different fixed massive-spin states, and color-stripped
colored states are left unprojected; details explain why. This avoids guessing
that a photon and gluon, or charged particles with equal masses, are identical.

Averaging acts on the seed and correction directions through typed particle
permutations. The original input remains in details and every original channel
remains an independent validation obligation. For
`{"e", "E", "E", "e", "A.+", "A.+"}`, the identical groups are electrons
`{1,4}`, positrons `{2,3}` and photons `{5,6}`. The combined projector is
`(1 + P56) (1 - P14) (1 - P23) / 8`: all eight combinations are used, with
negative signs for odd fermion exchanges and positive signs for photon
exchanges. It preserves an already correctly symmetric amplitude, without
changing diagram Wick signs or factorization sewing signs.

Each empty option disables only its own statistic. Set both
`"BoseSymmetryGroups" -> {}` and `"FermionSymmetryGroups" -> {}` to disable all
exchange projection. Explicit groups are supported, with the caller
responsible for their physical meaning. Groups must be disjoint, have equal
declared masses and a combined permutation order at most 120; explicit
fermion groups must describe compatible odd-spin states. For larger automatic
groups a bounded exact subgroup is used, reserving exchanges for different
species when possible. This need not enforce the full permutation group at
arbitrary multiplicity. `"ExchangeSymmetry"` details show detected and used
groups, `"RequestedOrder"`, used `"Order"`, `"MaximumOrder"`,
`"FullGroupUsed"`, source and diagnostics. Separate `"BoseSymmetry"` and
`"FermionSymmetry"` summaries show each statistic's groups and order; automatic
summaries also explain state inference. Explicit Bose summaries retain their
previous compact form.

Pass the detailed factorization report to `CompleteHighEnergy` to retain its
combined exchange constraints. Newly generated automatic directions and any
explicit `"CorrectionBasis"` supplied to that stage are projected into the
inherited symmetric/antisymmetric subspace. Ordinary amplitude pairs contain no
hidden symmetry metadata.
Union correction-space names use the prefix `Bose`, `Fermion` or `Exchange`
according to the statistics present. `"MaxCorrectionTerms"` applies after
projection; individual source-space construction may grow by the permutation count, up to
4096 directions unless a larger term limit was requested. The merged union
may be larger; these limits are not a hard memory bound.
With automatic directions, this option first combines the available compact
and literal channel spaces. It also tries individual terms from numerator
reductions of accepted continuations on their selected cut, including
spurious-reference choices. Each whole reduced source must preserve its own
cut residue before being split. Only that channel's internal shell is used;
other internal lines and inverse factors stay protected. Construction has a
60-second budget and the same intermediate term cap. For a cut with three or
more particles, a second bounded pass also uses
`Sum[MomProd[i,j], i<j in cut] == (cutMass^2 - Sum[Mass[i]^2, i in cut])/2`.
It tries each applicable pair as the numerator pivot, with arbitrary declared
external and exchanged masses. Inverse factors and all propagators stay
unchanged. Every whole alternative must preserve its selected residue before
its atoms enter the correction space; no such identity is imposed globally on
the final off-cut amplitude. Original certified reduced sources are retained.
The second pass uses only the time remaining after their construction, with
two-second per-source reduction limits. A construction timeout or an optional
continuation exceeding the intermediate term cap retains only
already certified directions and records `"ConstructionStatus" -> "PARTIAL"`,
`"OriginalSourcesComplete"` and `"InvariantContinuationsComplete"` in
`"SelectedChannelSpace"`; it does not drop any residue target or certify a fit.
Exact formal linear
dependencies are removed by a bounded 60-second pruning pass, preserving the
full span rather than only sampled residues. Gaussian-rational coefficients
use incremental sparse column elimination instead of a bulk row reduction.
The original direction order and typed expressions are retained. Other exact
numeric coefficient fields keep the existing bounded reduction as a fallback;
inexact input is never pruned. A timeout retains every original direction,
not a partially selected subset. The correction-term cap is checked afterward.
Without declared groups, compact-then-literal behavior is unchanged.

When that full automatic exchange space exceeds the requested term cap, a
budget fallback keeps each whole pole-bearing input diagram as a correction,
alongside all available channel atoms, instead of splitting the inputs into
individual terms. It never changes a full space that already fits. Details
record both sizes in `"CorrectionSpaceCandidates"` and name the fallback
`"BoseGroupedInputChannelUnion"` for Bose-only projection, with `Fermion` or
`Exchange` prefixes when appropriate. This is a different bounded space, not an
assertion that all discarded input atoms were dependent. Every original
channel and free direction still has to pass fresh validation. An oversized
or inconsistent fallback remains unresolved.

### Retained-Preparation Comparison

Completion-only measurements on identical saved prepared expressions:

| Process | Earlier plain prepared-channel input | Reports, remainders off | Reports, remainders on | Terms / complexity with reports |
| --- | --- | --- | --- | --- |
| Five-point `e,E,A.+,A.+,A.+` | 0.70 s | 0.79 s | 0.67 s | 3 / 27 |
| Five-point `e,E,A.+,A.+,A.-` | Unresolved | 3.83 s | 6.59 s | 14 / 130 |
| Six-point `e,E,A.+,A.+,A.+,A.+` | 62.23 s, 36 terms / 516 | 20.25 s | 28.24 s | 12 / 108 |

Preserving original channel targets and continuations makes prepared channels
usable without degrading the final form. The retained-remainder attempt itself
does not improve these QED results yet: the mixed five-point space is exactly
inconsistent, and the bounded partial six-point space is rejected by exact
compatibility equations. Existing channel fallbacks supply the successful
compact results. This is bounded extra work, not a claimed universal speedup.
The all-positive five-point example finishes by inexpensive rescaling first.

These runs used physical, spurious and physical reference preparation,
respectively; `MaxSteps -> 0` retained the ordinary, conservation and internal
numerator reductions. Fitting samples were 3, 3 and 12, with one held-out sample,
cap 256, three refinements and 120/120/180-second stage limits. Only the six-point
case declared Bose group `{{3,4,5,6}}`. Preparation times were about 6.85, 30.19
and 85.81 seconds, with channel preparation another 0.73, 6.41 and 9.78 seconds;
these are excluded from the table. Other diagnostics were running, so times
are measurements, not guarantees.

All three results passed fresh physical and sampled high-energy checks and
agreed with independently regenerated 6/6/24-diagram Feynman sums at three
fresh exact rational off-cut samples, including every coherent massive-spin
component at `m=2, EE=3`. Internal Feynman lines remained off shell and no
normalization was fitted. The unchanged regression tests cover the other
five- and six-point QED examples; these three cases are the timed comparison.

### Compact All-Positive Six-Point Result

For `e,E,A.+,A.+,A.+,A.+`, physical-reference preparation gives 24 one-term
diagrams. Ordinary canonicalization reveals six distinct terms, each repeated
four times. Counting each once gets all six middle-line channels right, but
fails all eight outer-line channels. An exact independent-weight fit on these
six input groups is inconsistent: coefficient rank six, augmented rank seven.
Thus neither a common factor nor different constant weights can finish it.

The outer channels sew a three-point vertex to the known five-point amplitude.
Writing the sewn momentum as `p1+pa` exposes products
`[a|p1 p2|a] [i|p1 p2|i]` already present in those channel sources. Keeping
these structures grouped and trying the typed companion construction above
gives four directions. Their uniquely fixed coefficients recover a compact
result containing the six-term diagram core and six additional pair-pole terms.

With all momenta ingoing, define
`t_(1r)=(p1+pr)^2-m^2`, `t_(2r)=(p2+pr)^2-m^2`, and
`t_(1kl)=(p1+pk+pl)^2-m^2`. One convenient regrouping is

```text
M = 4 EE^4 m <12> / Product[t_(1r), r=3..6] *
    Sum[ [kl]^2 / (t_(2i) t_(2j)) *
         ( [i|p1 p2|i] [j|p1 p2|j]
           - m^2 [ij]^2 t_(1i) t_(1j) / t_(1kl) ),
         {i,j} any two-element subset of {3,4,5,6} ].
```

Here `{k,l}` is the complementary photon pair. Expanding this display gives
12 terms and complexity 108. The term proportional to `m^2` is the six-term
unique diagram core; the momentum-chain products supply the missing outer
residues without changing the middle residues. The production algorithm
discovers its numerator groups from the channel sources and fits their
coefficients; it does not insert this displayed formula.

On the same prepared input and the notebook's 12 fitting samples per channel,
one held-out sample, 256-direction cap and Bose declaration, the previous
completion took 178.5 seconds and returned 120 expanded terms with complexity
1080, using 90 coefficients. The compact route took 18.4 seconds and returned
12 terms with complexity 108, using four uniquely fixed coefficients. These
times exclude the shared preparation and are measurements, not guarantees.

The returned public result passed all 14 physical channels, exact sampled
high-energy tests and three independent full off-cut comparisons against
freshly generated 24-diagram Feynman amplitudes at `m=2, EE=3`. Internal
Feynman lines remained off shell and no sign or normalization was fitted.
There are no spurious denominators or explicit Xi spinors in this form.
These checks establish the recorded sample agreement; they are not a proof
of a globally shortest representation.

The six-point `e,E,A.+,A.+,A.+,A.+` section of
`tests/QED-5-point-construction.nb` now uses the automatic defaults. Earlier
benchmarks used explicit sample, correction-cap and clock settings. A historical broad-space
run from 24 `AllSpurious` prepared diagrams completed in about 14 minutes,
with 161 independent correction directions. All 14 physical channels and the retained affine
family passed fresh exact coherent-spin checks at both the model mass and
twice that mass. No answer or supplied correction basis entered the solve.
The returned representative also agreed with an independent sum of all 24
Feynman diagrams at three fresh off-cut samples, including every coherent
massive-spin component. These sampled comparisons are not an analytic proof;
automatic spurious-pole and high-energy diagnostics remain separate checks.

The following notebook chapter treats `e,E,A.+,A.+,A.+,A.-`. Only legs
`{3,4,5}` are identical positive-helicity photons; leg 6 must not be averaged
with them. The current example uses the automatic resource defaults rather
than a long list of manual sample, cap and clock settings.

The current integrated `AllPhysical` factorization benchmark fits 160
source-derived coefficient directions and retains 67 affine directions. Its
source construction uses complete selected-cut-certified continuations and
filters every source origin by the actual resulting denominators, rather than
excluding an origin merely because its reference choice was called spurious.
The raw completed representative has 9,844 leaves. Ordinary external preparation,
including bounded propagator-grouped numerator identities, reduces it to 4,045
leaves in 18 groups, with physical propagators only. No known final amplitude or
Feynman answer enters the fit or the simplification.

That measured run took 272 seconds for factorization, 35.6 seconds for final
preparation and 14.4 seconds for fresh checks. All 17 report rows passed:
14 physical channels, high-energy growth, the explicit-reference diagnostic and
an independent off-cut Feynman comparison. There are no apparent spurious
poles or explicit Xi spinors in this representative. The reference row therefore
records absence of explicit Xi, not a separate Ward-identity proof. These are
sampled amplitude checks, not a global-minimality or analytic uniqueness proof.

The integrated high-energy stage now completes in 61.4 seconds with no free
coefficients remaining. It proves all 67 retained directions to be ordinary
external identities: 64 by a dense symbolic external chart and three by ordinary
algebraic reductions. The eight-worker proof stage took 54.06 seconds. Mass
parameters and coherent massive-spin variables remain symbolic; no numerical
mass specialization, previously saved certificate or known final formula is
used. Every direction was retained until its exact certificate succeeded.
Final external-only preparation then takes 29.4 seconds and returns 18 groups
with 4,147 leaves and weighted complexity 1,128. Fresh final checks take 3.3
seconds, with all 17 rows passing, including the independent Feynman comparison.
These are measured stage timings on the recorded prepared inputs, not a
promise for every cold construction or machine.

These staged measurements do not establish what a newly generated source
catalog will produce in a fresh construction. The opt-in
`physical-construction-integration-tests.wl` regression runs the exact ordinary
notebook call from raw diagrams with `AllPhysical` and the independent Feynman
model. It supplies no saved amplitude, correction basis or resource overrides.
It requires a validated result with only physical denominators and complexity
at most 327, then rebuilds the Feynman benchmark for an independent check.
Run it separately with:

```sh
python3 tests/checks/run-wolfram-tests.py --suite physical-construction-integration-tests.wl --timeout 2400
```

The normal constructor options and resource defaults are unchanged. A compact
worked-example test is not a substitute for this complete fresh-call test.
The verified fresh ordinary call completed in 291.978 seconds and returned
seven groups, 1,575 leaves and complexity 327, with only physical denominator
factors. The separately regenerated validation reported 17 passes, including
the Feynman comparison. These are measured results for that execution, not a
runtime or global-minimality guarantee. The earlier saved ordinary call took
about 663 seconds and retained apparent spurious denominators.

The proof ordering matters. Cheap scalar grouping and bounded numerator
Schouten edits first operate on the original direction. A modest compact form
can then go directly to a generic symbolic chart, before display-oriented
preparation introduces a harder rational representation. A later external-only
preparation remains available as a fallback. Generic null-axis choices may use
opposite homogeneous little-group weights without constraining any internal
virtuality. Timeouts and unsupported directions would still remain in the
family rather than being declared zero from sampled agreement.

Leaf count alone does not define the preferred final form. In this example the
4,045-leaf representative has complexity 1,129, while an equivalent 4,147-leaf
form reaches complexity 1,128 and therefore ranks better. These are bounded
simplification results, not a proof of a globally simplest representation.

The balanced-helicity chapter, `e,E,A.+,A.+,A.-,A.-`, now also completes with
the general `ConstructAmplitude` wrapper. Model-derived shifted channel
continuations supply the missing correction directions; the original
fourteen channels remain the targets. With optional identity-route enumeration
disabled, the original ordering and a reordered helicity assignment both
completed in about 7.7 minutes, including independent Feynman comparison.
Each final report passed 14 physical, 104 apparent spurious, high-energy,
reference and Feynman checks. The reference row records the absence of
explicit Xi, not a separate Ward-identity proof. Preparation of the completed
expression took about six seconds while retaining its compact rational form.

The earlier independent worked example remains available.
`qedBalancedSixPointAmplitude[factorizationModel]`, defined in
`tests/QED-5-point-residues.wl`, regenerates the known lower-point
channels and constructs eight shifted sewn products. This is a worked
minimal-QED recursion, not a general replacement for `CompleteFactorization`.
The notebook explains the `[5,3>` shift and its vanishing boundary term.
The helper returns only the amplitude by default; `"ReturnDetails" -> True`
retains the eight roots and intermediate expressions. It needs the complete
lower-point model, not the cubic diagram model, and uses no Feynman input.

The resulting Xi-free expression passed all 14 physical channels at two fresh
exact coherent-spin samples each, three fresh independent-Xi Feynman
comparisons, two exact high-energy samples with `E^-2` growth, and both
supported bracket-pole checks. The checker also identifies its 28 compound
denominator factors and tests their cancellation together on exact conserved
sample curves. Every requested pole remains visible in the report, and
unsupported or unfinished checks remain `UNRESOLVED`. Sampled finiteness
does not establish a global analytic cancellation or a simplest expression.

The generator also records and rejects a new continuation whose physical
propagators retain powers greater than one after ordinary cancellation. Its
verification reason is `"UnsupportedHigherOrderPhysicalPoleContinuation"`:
the present residue fitter requires regular simple-pole directions. This is
an implementation limit, not a physical restriction or a reason to discard
the raw input and channel sources. Factorization contributions from different
exchanged species sharing one cut and mass are summed, not treated as
alternative targets; incomplete lower-point data remain incomplete.
Physical-only correction proposals inspect the actual denominators of every
source origin, including prepared diagrams, prepared channels, reduced channel
directions and selected-cut-certified auxiliary reference continuations. A
source is not excluded merely because its original reference choice was called
spurious; eligible resulting terms must contain only catalogued physical
propagators, no unresolved references and no unsupported higher physical pole.
Every proposed space must still satisfy all original channels and independent
family validation. Filtering a proposal does not discard the original inputs or
assert that a rejected continuation is an ordinary zero identity.
An explicit `"CorrectionBasis" -> {term1, term2, ...}` supplies a different
bounded space when the automatic one is insufficient. Physical propagators
remain off shell except when extracting the residue for the selected cut.

The automatic fit starts with four exact cut configurations and increases its
sample count from the actual unknown count and coherent-spin equations. It
rechecks the result at four independent configurations by default. Explicit
`"Samples"` and `"ValidationSamples"` values override those automatic choices.
Implicit massive spin components are kept coherent rather than fitting one
spin projection. If independent validation fails, the function converts those
same saved cut charts into further exact equations, refits the simultaneous
system and validates with fresh seeds. `"MaxRefinements" -> Automatic` starts
with three retries and increases conservatively with the measured correction
space; an explicit zero disables feedback. Feedback
evaluates the original sparse correction directions before applying fitted
coefficient rules, avoiding unnecessary expansion of a dense fitted family.
Held-out validation also restricts each original component before inserting
the fitted coefficients, then checks both the selected amplitude and every
surviving free direction. It reuses those same exact restrictions for feedback
instead of mapping a large common-denominator candidate a second time.
On internally generated coherent cut charts, supported simple typed-pole terms
can use direct cut evaluation after checking finite primitives and nonzero
inverse bases. Hidden singular brackets or invariants, higher poles, unsupported
structures and cut-only spinors fall back to the full mapped restriction.
Additive full-map caching also falls back to the whole component when singular
terms can cancel. Implicit x factors keep their whole-component spinor inventory,
and custom maps keep whole-expression evaluation. Every correction component
is still checked, including those assigned zero coefficients; these optimizations
do not change the residue equations or bypass kinematic validation.
Affine components are extracted in one coefficient-array pass. Physical-pole
support is collected from the trial's typed propagators, without first reducing
the whole affine spinor expression. Missing-channel zero-residue obligations
and unknown exchanged masses receive the same treatment as before.
Already polynomial coherent-spin equations retain exact rational coefficients
instead of first combining all fitting coefficients into a common denominator.
Rational spin dependence still uses the full cancellation path, and matrix
elimination and validation remain exact.
Each refit solves all accumulated original equations together. It does not
substitute a large intermediate solution into the new rows, which can cause
severe exact-fraction growth at higher multiplicity. The progress display
separately identifies equation construction, exact solving and validation.
Shared exact algebraic units in matrix columns and the right-hand side can
be scaled out during linear elimination and restored afterward. Neither
optimization changes the exact equations or the meaning of free parameters.
For large rational systems, a bounded CRT reconstruction is tried before the
native exact solve. It uses a bounded inventory of compatible primes (25 by
default) and a separate unused prime, then certifies every original equation for both the particular solution
and the complete independent free family using exact integer arithmetic. Its
attempt receives the time remaining in `CompleteFactorization`, capped at
900 seconds; an infinite outer limit still uses that finite cap. Other private
solver calls keep the 60-second default. Exhausted budgets remain unresolved.
Failed reconstructions and unlucky primes leave the existing exact solver as
the fallback, still within the overall completion clock. This is not an
approximate fit or an uncertified modular test. A valid prime giving full
coefficient-column rank and a larger augmented rank certifies exact
inconsistency; a rank-deficient modular gap alone does not.
For large rational systems, modular arithmetic can select a smaller square
block. The solve remains exact: every original equation must certify both
the consistent particular solution and the full independent free family.
Alternatively, an original row whose exact residual is a nonzero constant
independent of every free coordinate certifies inconsistency with the selected
block. This is an exact contradiction, not a modular-rank test. Failed block
certificates fall back to ordinary exact row reduction. Modular rank alone
does not establish consistency or inconsistency.
The symbolic amplitude and free directions are constructed only after sparse
validation succeeds, retaining factored linear combinations rather than first
expanding a large affine family.
Algebraic cleanup uses external mass shells and momentum conservation only, removing
directions only when they reduce exactly to zero. Unproved or genuinely
residue-invisible directions remain in the affine family for the next stage.
With `"RepresentativeSelection" -> Automatic`, a validated free family also
gets a bounded search for a simpler particular point. Several deterministic
orders try exact cancellations of formal expression coefficients. Each completed
point is compared as a whole; a strict rank improvement is required. This is
a heuristic, not a globally minimal sparse solve. All free directions remain
available for the next stage, and explicit regular coefficients, including
zero coefficients, are preserved rather than deleting contacts for brevity.
`"RepresentativeSelection" -> None` leaves the original particular point.
Details record the original center and the offset in the old free coordinates;
`FamilyAmplitude` is rebased around the chosen amplitude with the same directions.

Representative selection and final cleanup use a bounded post-validation
allowance within the stage clock. Cleanup tries scalar regrouping and ordinary external
reduction, retaining the better whole form. In particular, a compact product of
momentum chains must not be replaced by a more complicated expansion merely
because the ordinary reducer returns it. Interrupted work retains only a
completed improved point and the already validated family.
Large final expressions can also be compacted by grouping existing physical
propagator products and simplifying each numerator. A bounded pass first factors
ordinary scalar content, then tries generalized Schouten edits in the largest
sum numerators. Every accepted edit is an exact identity with external mass
rules only; the complete result must improve the existing complexity rank.
Completed improvements are checkpointed within a fifteen-second total allowance,
with size guards on the input and prospective expansion. The amplitude, rank
and replay data are committed together in one checkpoint record, so a timeout
cannot pair a new amplitude with an older route. Detailed routes retain the
original numerator, selected edits and resulting numerator for replay. This
changes only the representative's algebraic form and never removes affine
directions or applies an internal-line shell to a final sum.
A second bounded ordinary pass works across different physical denominators.
It recovers factors hidden by forward product normalization, using only products
of existing typed spinor chains. Every reverse lift must reproduce the original
polynomial through the forward reducer with the declared external masses.
The determinant-pair identity and generated scalar Schouten identities can then
reduce a small block sharing a higher-particle propagator. Exact propagator
definitions expose that propagator in the numerator. Polynomial division keeps
both the pole contribution and the complete regular remainder; it never puts an
internal line on shell. Complementary momentum sets require momentum
conservation to be enabled.

The regular pieces are collected over their existing pair propagators while
preserving useful numerator factors. Each completed improvement stores its
expression and replay route together. Preparation and factorization completion
share this general cleanup; the latter records it in `"OrdinaryCompaction"` and
recenters `"FamilyAmplitude"` without deleting any `"RemainingBasis"` directions.
The post-fit cleanup leaves the fit dimension and original channel obligations
unchanged. Large physical correction catalogs can also normalize each existing
column as a whole before exact dependency pruning. This preserves their complete
functional span; a reduced sum is never split into new fitting coordinates.
Failed or interrupted normalizations and the untouched tail remain literal
inputs. In the six-point example this reduced 650 columns to 258 independent
columns, with no sampled identity tests or discarded unproved directions.

Preparation also tries a bounded alternative on the original rational
fractions, before they lose useful numerator context through normalization.
It factors out common spectators, applies generalized Schouten moves that
expose external endpoint Dirac contractions, and reduces with external masses
only. Physical inverse factors are protected. The usual reduction and this
alternative compete by the same whole-expression complexity rank; a slower or
larger alternative cannot replace the best completed result. The retained
`OriginalFractionSpinorIdentities` route records the ordinary edits and their
denominators. It applies no internal-shell identities to a completed amplitude.

Completion uses a private forward-product normalization policy for coefficient
construction, while public preparation retains useful factored polynomials.
A presentation preference must not implicitly redefine the proposal coordinates.
The private policy is dynamically scoped and restores the caller's state.

The worked six-point process `e,E,A.+,A.+,A.+,A.-` illustrates the reduction in
`QED-5-point-construction.nb`. Example-only formulas and the local identity route
are in `QED-5-point-residues.wl`, through
`QEDSixPointPPPMPreviousExample[model]`, `QEDSixPointPPPMCompactExample[model]`,
and `QEDSixPointPPPMExampleProof[model]`. Production code never calls these
process-specific helpers. The general simplifier reduces the previous form's
complexity from 1128 to 327, with seven groups instead of eighteen. The
nonzero regular remainder is essential; dropping it would give an incorrect
six-term amplitude. Generic proofs and regressions cover arbitrary labels,
unequal masses, both chiralities, scoped parameters, budget limits and explicit
quotient/remainder replay. No internal-shell identity enters this final reduction.

The same example file provides
`QEDSixPointPPMMPhysicalCurrentExample[model]` for the balanced-helicity
six-point process. It is a readable independent off-shell-current derivation,
with shared physical references and ordinary Clifford, external Dirac, null
momentum and virtuality identities. It has 38 factored groups, only the fourteen
physical propagators, and weighted complexity 3224 instead of 41028 for the
eight-term spurious-pole reference. Both formulas pass independent checks, but
the physical-current reference is not injected into constructive completion.
The notebook labels this distinction. A fresh 504-direction physical proposal
was insufficient for this process: its exact sampled simultaneous system was
inconsistent. Increasing its time allowance cannot repair that particular
proposal space; further source continuations are still needed.
The reference also exposes an important current solver limitation: apparent
double physical-pole factors cancel in complete numerator blocks, whereas
source admission currently requires formally simple powers. Do not merely
remove that guard. A future extension must preserve whole blocks and establish
their regular residue restriction, or constrain every higher Laurent
coefficient to vanish along with fitting the simple residue. The existing
complete-source and final checks remain required.

Typed `PropDen` poles are checked across that family, including poles hidden
in its zero-parameter representative. Established zero-residue obligations
remain checked even after the selected candidate loses the pole. Internal
validation rows preserve these obligations without changing the supplied
channel rows, model or vertex content.
`"TimeConstraint" -> 60` bounds this stage. Incomplete
constraints, inconsistent equations, exceeded correction-space limits and
timeouts remain `"UNRESOLVED"`; they are not successful reconstructions.

When requested, the details retain `"Amplitude"`, `"Correction"`,
`"CorrectionBasis"`, `"FamilyAmplitude"`, `"RemainingBasis"`, `"FreeParameters"`,
fit equations and validation reports. `"RefinementHistory"` records the fit and
validation rounds; `"ChannelContinuations"` records proposed references and
their selected-cut verification. `"CorrectionSpace"` identifies
`"ReducedChannels"`, `"LiteralTerms"`, `"BoseChannelUnion"`,
`"BoseSelectedChannelUnion"`, `"BoseGroupedInputChannelUnion"` or `"SuppliedTerms"`, with `"NotRequired"`
when the input already passes without a correction space;
`"ReducedChannelSpace"` retains the compact sources, directions and selected-cut
checks. `"SelectedChannelSpace"` retains the selected-cut atomic sources and
their certifications. `"CorrectionSpaceAttempts"` records the attempts when a compact fit
falls back to the literal space, including each attempt's refinement history
so an inconsistent system is distinguishable from a later fallback timeout.
These are opt-in details, not additional
ordinary output. A `"COMPLETE"` factorization stage means its selected
representative passed the physical checks at the recorded samples, not that
the full amplitude already satisfies all four final tests.

### 3. Complete and Validate the Remaining Conditions

```wolfram
finalAmplitude = If[FailureQ[factorizedAmplitude] || factorizedAmplitude === $Failed,
  factorizedAmplitude,
  CompleteHighEnergy[
    factorizedAmplitude, channels, qedFactorizationModel]];
If[FailureQ[finalAmplitude] || finalAmplitude === $Failed, finalAmplitude,
  CheckAmplitudes[finalAmplitude, channels, qedFactorizationModel]]
```

Run this stage after factorization completion succeeds. It uses bounded
directions from the supplied expression, an explicitly retained affine
family, or an explicit correction basis to
enforce the high-energy bound together with physical residues, spurious-pole
cancellation and explicit Xi reference independence. Physical constraints
are imposed again, so a high-energy correction cannot knowingly spoil the
fitted factorization conditions. The default `"HighEnergyBound" -> Automatic`
uses `E^(4-n)`; change it when the theory requires a different bound.

Resource defaults are `Automatic`, using the finite workload policy described
above. Fit sampling starts at four, independent validation remains four, and
explicit numeric settings are preserved. The retained family, rather than an
unrelated original source count, supplies the high-energy unknown count.
With no inherited family, the automatic directions are limited to supported
terms already present in the expression. It does not invent an unrestricted
contact-operator basis or introduce independent QED interactions. A needed
local completion must be consistent with the specified theory. If the bounded
space cannot satisfy the constraints, the report retains the failure rather
than guessing extra physics. Nonzero undetermined directions give
`"UNDERDETERMINED"`, not a unique final answer.

A free coefficient can also multiply an expression that is identically zero.
When a completion report already carries exact affine coefficients in
`RemainingBasis`, high-energy preparation reuses them only after checking
their number, coordinate independence and literal reconstruction of the
complete family. This avoids expanding a large family merely to extract the
same coefficients again. Missing or inconsistent metadata takes the ordinary
coefficient-extraction path; nonlinear families and unproved directions remain
explicit. Reusing coefficients is not itself an identity-zero certificate.
Unrecognized identity-zero directions previously caused false `UNDERDETERMINED` results for some five-point
helicity orderings. Completion now removes only directions with an exact
external-identity certificate. The proof retains the original expression,
first trying cheap grouping by physical propagators and bounded Schouten edits
on its numerators. A modest compact result can be tested immediately on a
symbolic rational parametrization of generic conserved, on-shell external
momenta. Ordinary external preparation is a later fallback: a representation
ranked best for display can be considerably harder to prove equal to zero.
Model parameters and all coherent massive-spin
components remain symbolic. No internal line is put on shell, no known final
amplitude is supplied, and numerical zero samples do not remove directions.
Proof timeouts and unsupported expressions retain the original freedom.
When parallel execution is enabled, independent identity proofs use the same
worker pool as the other stages. Smaller directions are dispatched first, with
a shared deadline and an allowance of at most ninety seconds for each
direction. Cheap scalar grouping precedes the dense-chart size guard. Ordinary
external reductions and primitive-first symbolic evaluation of each distinct
typed primitive share the proof allowance. The chart may place massless legs
with opposite homogeneous little-group weights on its coordinate axes. This
changes only the rational coordinates, not the allowed kinematics: the axes
must be distinct declared null legs, external shells and momentum conservation
are validated, and internal virtualities remain free. Each worker must return
its exact certificate; a missing, interrupted or unsuccessful worker leaves
that direction in the family. Finite zero samples
never replace these proofs.
`"Output" -> "Details"` records the certificates in
`"FamilyIdentityReduction"`. A genuine nonzero local ambiguity still remains
`"UNDERDETERMINED"`.

For difficult cases, retaining the factorization family's remaining freedom
can help. This is an explicit optional workflow:

```wolfram
factorizationDetails = CompleteFactorization[
  prepared, channels, qedFactorizationModel, "ReturnDetails" -> True];
If[factorizationDetails["Status"] === "COMPLETE",
  CompleteHighEnergy[factorizationDetails, channels, qedFactorizationModel],
  factorizationDetails["Diagnostics"]]
```

An ordinary factorized amplitude is one representative. The next stage can
search corrections made from its surviving terms, but cannot generally
recover directions discarded from the full family. Passing the optional
report preserves them; no hidden cache or expression metadata is used.

Both completion stages accept `"ValidationParameterRules"` as a list of
parameter-rule lists, for example:

```wolfram
"ValidationParameterRules" -> {
  {"Me" -> 2, "EE" -> 1}, {"Me" -> 3, "EE" -> 2}}
```

The fitting assignment is always included. With `Automatic`, an additional
mass-rescaled assignment is used only when the model has no declared
parameter relations; otherwise validation uses the supplied assignment.
Any additional assignments must obey the theory's parameter relations.
Passing at these exact sampled kinematics and parameter values is not a
generic analytic proof, a complete reconstruction theorem or a uniqueness
claim. Inspect the returned diagnostics and independently vary samples and
parameters before relying on a fitted correction.

Ordinary successful calls return `{externalMassRules, expression}`; use
`"Output" -> "Expression"` for just the expression. An unresolved
`CompleteFactorization` returns a compact `Failure["IncompleteAmplitude", ...]`;
request `"ReturnDetails" -> True` for its full diagnostics and retained candidate.
`CompleteHighEnergy` accepts either the original channel list or
`channelPreparationDetails`, just like factorization completion. In both cases,
the original channel expressions are the residue targets. If a prepared final
amplitude is used, test that same value for `Failure` before passing it onward.
An immediate `InvalidCompletionArguments` means the calculation did not start;
its compact argument-position diagnostics identify an invalid channel or model
argument, rather than reporting a failed high-energy test.

For `CompleteHighEnergy`, an unresolved or underdetermined ordinary
call returns a failure retaining the candidate and report, while
`"Output" -> "Details"` returns the report directly. Invalid inputs are rejected
separately. Do not present an incomplete candidate as an established amplitude.

### Progress and Inspection

All three functions report their current stage by default. Use
`"ProgressReporting" -> False` to silence the display, or supply
`"ProgressFunction" -> (yourFunction[#] &)` to receive progress associations
instead of the default display. Keep progress reporting enabled when using a
callback. Preparation reports the diagram, operation, complexity and term
count; completion reports individual factorization samples, completed channels,
solving and validation. Sample progress includes the current cut and total
number of successful sampled charts, before the channel is finished.
`Validating sparse factorization` is the component-wise implementation of the
same physical residue check, not a different factorization law or a subset of
channels. Its correction-coefficient counter reports the number of remaining
free coefficients. It is zero during an initial check with no correction
variables, and can also be zero after a nonzero correction has been fully
determined. Channel agreement is established by the validation statuses, not
by that counter.
Details retain the intermediate information without requiring bulky default
output. These stages aim for a compact justified result in bounded time;
neither a passing checker nor a local complexity minimum establishes that
no simpler equivalent expression exists.

## Saving Known Amplitudes

`PlainText[amplitude]` produces display text with angle/square brackets and
momentum insertions. Use `PlainText[amplitude, "Model"]` for reversible model
syntax. The latter preserves exact arithmetic, model-local parameters,
composite momenta, propagators and explicit spin indices. Ambiguous or
unsupported objects return `Failure`; it does not turn arbitrary symbols into
model parameters or silently round inexact numbers.

After checking a complete amplitude, append it to the model containing the
known lower-point amplitudes:

```wolfram
AppendModelAmplitudes[modelDirectory, 3, {{particles, finalAmplitude}}]
factorizationModel = ReadModel[modelDirectory, 3];
```

For several amplitudes, supply several entries. Optional triples
`{particles, factor, amplitudeOverFactor}` preserve the model's separate factor
column. The writer checks every entry before making a single atomic update,
keeps existing rows, adds a matching particle-column header for higher
multiplicity, and writes before the final separator. Exact existing entries
are skipped. A conflicting expression or ordering for an existing particle
multiset returns `Failure` without changing the file. Model parameters must
belong to the target model, not just have the same printed names. Reload the
model after writing; an already loaded list does not update automatically.
Writing an expression does not certify its physics or simplify it further.

The current electron/photon QED model 3 includes all six independent nonzero
five-point amplitudes: `e,E,+++`, `e,E,++-`, `e,E,+--`, `e,E,---`, and
`e,E,E,e,A.+/-`. Photon permutations cover all eight ordered three-photon
helicity assignments. Massive fermion spin dependence is retained, not fixed
to one spin projection. Pure-photon and charge-violating five-point trees
vanish and remain absent. This inventory is for the particles in the QED
model, not additional lepton or quark flavors from the Standard Model.

The checked diagram derivations can be reproduced and written as follows:

```wolfram
Get[FileNameJoin[{hepcatDirectory, "tests", "QED-5-point-residues.wl"}]];
entries = qedFivePointModelAmplitudes[];
AppendModelAmplitudes[modelDirectory, 3, entries]
```

That helper does not write files by itself. It uses the existing all-plus,
mixed-helicity and four-fermion diagram derivations and their checked parity
transforms. The regression suite compares the saved rows with independent
Feynman calculations, other particle orderings and nonunit mass/coupling
values. Model 2 stays cubic for diagram generation. Model 3 now supplies both
three-by-five and four-by-four sewings at six points. The all-plus six-point
notebook section exercises their simultaneous completion as described above;
this does not establish every other six-point process or helicity assignment.
