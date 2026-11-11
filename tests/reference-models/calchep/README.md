# CalcHEP Models

The four `*1.mdl` files originate from stock model 1, named `SM`, from
the `models/` directory of CalcHEP 3.9.0. They contain particles,
Lagrangian vertices, parameters and dependent functions. The local version
restores electron, muon, up- and down-quark masses and their Higgs/Goldstone
couplings, and uses fixed charm, bottom and strange masses matching the
constructive SM benchmark, as described below. Models 2 and 3 in that installation add effective
Higgs interactions and are not this baseline.

These are raw CalcHEP Feynman-rule files, not HEPCAT constructive amplitude
tables. `ReadModel` recognizes `lgrng1.mdl` and returns
`FeynmanModel[particles, vertices, variables, functions]`. The four parts retain
the usual positional organization, but the explicit head distinguishes
Feynman rules from constructive amplitudes. Existing `amps*.mdl` models still
return the original four-element list.

The four `*2.mdl` files define a separate, minimal QED benchmark containing a
massive electron, its antiparticle, a photon and the electron-photon vertex.
Its defaults are `Me = 0.0005` and `EE = 0.31333`. This is pure QED, not a
mass-patched Standard Model with missing Higgs couplings. Load it with
`ReadModel[FileNameJoin[{hepcatDirectory, "tests", "reference-models", "calchep"}], 2]`.

Parameters and derived functions belong to the returned model. Importing does
not create global symbols or assign numerical values to names such as `EE`.
Use `ModelParameter[model, "EE"]` to obtain its symbolic reference and
`ModelParameterRules[model]` for locally resolved numerical rules. Independent
models can have the same parameter names with different values.

## Use

After loading HEPCAT, a simple massive-electron example is:

```wolfram
model = ReadModel[FileNameJoin[{hepcatDirectory, "tests", "reference-models", "calchep"}], 1];
diagrams = CreateDiagrams[{"e", "E", "A.+", "A.-"}, model];
feynmanPairs = DiagramAmplitudes[diagrams];
spinorPairs = FeynmanToSpinorProducts[feynmanPairs];
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

The local SM uses the following fixed defaults, in GeV:

| Parameter | Value | Convention |
| --- | --- | --- |
| `Me` | `0.0005` | Existing constructive-model electron value. |
| `Mm` | `0.105` | Existing constructive-model muon value. |
| `Mu` | `0.00216` | MSbar mass at 2 GeV. |
| `Md` | `0.00470` | MSbar mass at 2 GeV. |

The quark defaults are the central values in the
[PDG 2026 quark summary](https://pdg.lbl.gov/2026/tables/rpp2026-sum-quarks.pdf).
The same `Mu`/`Md` values are present in constructive SM model 1, whose
particle tables and Higgs amplitudes already treated these quarks as massive
but previously omitted their numerical definitions. These are fixed input
defaults, not pole-mass measurements or automatic QCD evolution with `Q`.

For each restored charged fermion, the scalar Higgs rule is
`-EE*mass/(2*MW*SW)`, with Lorentz part `1`, exactly as in the existing massive
generations. Matching neutral and charged Goldstone rules are retained in the
raw tables for consistency; unitary-gauge tree generation excludes them.
Neutrinos remain massless, matching the constructive models.
Tests cover the mass definitions, Yukawa relations, the massive three-point
vertices, Higgs exchange and electromagnetic/color Ward identities.
For the independent four-point SM comparison, this local model uses fixed
`Mc = 1.3`, `Mb = 4.2` and `Ms = 0.095` GeV, exactly as constructive model 1.
Their Higgs and Goldstone vertices still depend on the same mass parameters;
only the numerical input convention changes. The stock `McEff`, `MbEff` and
`MqEff` constraints were removed from this local copy and replaced by those
explicit variables. This makes both the Wolfram comparison and the native
CalcHEP reference evaluate the same declared tree-level model, without silently
borrowing unresolved masses from the amplitude being checked. The installed
CalcHEP model is unchanged. The remaining `LamQCD` initializer is retained as
source model data; it does not enter these tree vertices. Other imported
external functions remain symbolic unless an implementation or explicit
model-local input is supplied.

## Gauge and Scope

The original source uses Feynman gauge. Its 125 raw vertices remain, and the
local massive-light-fermion extension adds fourteen Higgs/Goldstone rows,
giving 139. The HEPCAT backend uses the
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

CalcHEP vertex momentum slots follow the incoming field arguments used by the
adapter. The attached momenta already carry their field-flow signs; the adapter
does not reverse them again. It also preserves the separate fermion-propagator
momentum flow. The automatic three-adjoint tensor is `-I ColorStructure`, as in
the CalcHEP manual. Charged electroweak photon Ward identities and nonabelian
Ward identities test these relative signs independently of a supplied amplitude.
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
color structures and gauge choices. Preserve this attribution and document
local model changes. Although no separate license file was found in the local
installation, the [official CalcHEP download page](https://theory.sinp.msu.ru/~pukhov/calchep.html)
identifies its license as GPL-3 and links the GNU GPL version 3 text. Preserve
that upstream attribution and license when distributing these stock-model
derivatives. The full GNU GPL version 3 text is included in the repository's
root `LICENSE`; HEPCAT's own code notices allow version 3 or any later version.

## Shared SM convention

The CalcHEP vertex table is retained in its source field basis. The constructive
SM tables `models/amps1.mdl` and `tests/checks/fixtures/models/amps6.mdl` use the same charged-current
sign: `-EE/(MW*SW)` for both W charges and every quark and lepton generation.
Exact cubic comparisons test this agreement, with neutral and bosonic controls.
The generic Feynman importer does not infer a process-dependent phase or alter
this model's vertex factors to fit a constructive expression.

All lower-point amplitudes used for factorization must follow this same model
convention. Changing the sign of both charged W fields at every constructive
vertex multiplies each complete tree by the product of its external W phases:
every internal W contributes `(-1)*(-1) = +1`. This also holds for products of
lower amplitudes on each channel, at any multiplicity. A sign correction belongs
at the vertices and must be propagated by recalculation; it must not be fitted
separately to final amplitudes. `Opp Sign` remains a diagnostic that prevents a
validated `ConstructAmplitude` result until the convention is resolved.
