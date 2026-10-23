# CalcHEP Standard Model

The four `.mdl` files are unchanged copies of stock model 1, named `SM`, from
`/Users/neil/code/CalcHEP/calchep_3.9.0/models/`. They contain particles,
Lagrangian vertices, parameters and dependent functions. Models 2 and 3 in
that installation add effective Higgs interactions and are not this baseline.

These are raw CalcHEP Feynman-rule files, not HEPCAT constructive amplitude
tables. `ReadModel` recognizes `lgrng1.mdl` and returns
`FeynmanModel[particles, vertices, variables, functions]`. The four parts retain
the usual positional organization, but the explicit head distinguishes
Feynman rules from constructive amplitudes. Existing `amps*.mdl` models still
return the original four-element list.

## Use

After loading HEPCAT, a simple massless-electron example is:

```wolfram
model = ReadModel[FileNameJoin[{hepcatDirectory, "model_feynman"}], 1];
diagrams = CreateDiagrams[{"e", "E", "A.+", "A.-"}, model];
feynmanPairs = diagramAmplitudes[diagrams];
spinorPairs = FeynmanToSpinorProducts[feynmanPairs,
  "FermionHelicities" -> {1 -> -1/2, 2 -> 1/2}];
amplitude = Total[Last /@ spinorPairs];
```

Both amplitude outputs retain `{massRules, expression}` for each diagram.
The first contains ordered `DiracChain` expressions, Lorentz tensors and vector
wavefunctions; the second contains HEPCAT spinor chains. Massless Dirac external
fermions require explicit helicities. Massive vector wavefunctions retain two
symmetric little-group labels, `VectorSpinIndex[leg, 1]` and
`VectorSpinIndex[leg, 2]`, rather than losing their spin information.
Automatic photon/gluon references remain explicit independent `Xi` spinors.
Use `"ReferenceSpinors" -> {3 -> 4, 4 -> 3}` to choose other massless legs.
The converter has a 60-second default time limit; `"TimeConstraint" -> Infinity`
removes it. `"ReduceSpinorProducts" -> False` retains the unreduced exact chains.

Color tensors remain explicit as `ColorDelta`, `ColorGenerator` and
`ColorStructure`, with repeated internal indices summed. Conversion does not
perform a color sum. The gluon tensor auxiliary `G.t` represents the four-gluon
contact interaction with a constant propagator, not an additional physical pole.

## Light Fermions

The imported model has zero electron, muon, up and down masses. Local working
copies with `Me` and `Mm` were also found, but their vertex tables do not include
the corresponding Yukawa sector. They must not be used as a complete massive SM.

When restoring these masses, add all of the following together:

- Particle mass entries and numerical parameter definitions.
- Higgs Yukawa couplings for every newly massive fermion.
- Neutral and charged Goldstone couplings when using Feynman gauge.
- Tests relating the masses, Yukawa couplings and gauge-current divergences.

Do not change only the particle masses. Prefer generating the full vertex set
from a consistent Lagrangian rather than selectively patching individual rules.
Running-quark-mass functions in `func1.mdl` are external CalcHEP functions;
symbolic import does not provide their numerical implementation.

## Gauge and Scope

The copied source uses Feynman gauge. All 125 raw vertices, including Goldstone
and ghost rows, remain unchanged in these files. The HEPCAT backend uses the
requested **unitary gauge** for massive W and Z lines, including the longitudinal
propagator numerator. It excludes Goldstone and ghost fields from physical tree
generation. This is not Feynman-gauge propagation with missing scalar diagrams.

The Feynman backend provides an independent benchmark for final constructive
amplitudes. Internal momenta stay off shell in both propagator numerators and
denominators. `FeynmanToSpinorProducts` is an exact algebraic conversion, not an
application of diagram-local internal on-shell conditions. Majorana fermions
and unsupported higher physical spins return `Failure`; unsupported structures
must not be silently dropped or replaced by a guessed expression.
Tree propagators have zero widths; the source width declarations are retained
as model information, not inserted into their denominators. Massive vectors
currently convert as spin-index tensors, not selected helicity eigenstates.

CalcHEP's vertex momentum slots point outward. The adapter reverses those
slots once to HEPCAT's all-ingoing convention; it does not reverse the separate
fermion-propagator momentum flow. This matters for odd-derivative interactions
such as the three-gluon vertex.
Raw scalar momentum products use inert `LorentzScalarProduct` until attachment.
Composite internal momenta are expanded into external momenta before ordinary
`MomProd` identities are allowed; internal momentum squares are never replaced
by the mass of the exchanged particle.

Use a model of complete known lower-point amplitudes for factorization checks,
not these raw Feynman vertices. The two calculations serve different purposes.

## Provenance

The imported rules are distributed with CalcHEP 3.9.0. Its local `CITE` file
credits A. Belyaev, N. Christensen and A. Pukhov, *CalcHEP 3.4 for collider
physics within and beyond the Standard Model*, Computer Physics Communications
184 (2013), 1729-1769, doi:10.1016/j.cpc.2013.01.014,
arXiv:1207.6082. The supplied model source is generated with LanHEP.

CalcHEP's local `help/s_mdl_3.txt`, `help/s_mdl_4.txt` and
`model_src/sm.inc` document the particle fields, ordered fermion rules, implicit
color structures and gauge choices. Preserve this attribution and the raw model
files. No explicit license file was found in this local installation; confirm
redistribution terms before publishing the imported files as a separate bundle.
