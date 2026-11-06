<|"ConstructiveDiagrams`Spinor" -> "Spinor[\"Helicity\", type, i] represents \
a massless external spinor of chirality \"Angle\" or \
\"Square\".\nSpinor[\"Spin\", type, i] represents a massive spinor with its \
little-group index implicit.\nSpinor[\"Spin\", variance, type, i, index] \
makes the \"Upper\" or \"Lower\" little-group index explicit.\nSpinor[\"Xi\", \
type, i] represents an auxiliary reference spinor; its label need not be an \
external particle.\nRepresents an angle or square spinor belonging to an \
external particle or an auxiliary reference.",
 "ConstructiveDiagrams`SpinorChain" -> "SpinorChain[left, right] represents a \
bracket with two spinor endpoints.\nSpinorChain[left, Mom[i1], Mom[i2], ..., \
right] represents a momentum chain with its given order.\nRepresents a spinor \
bracket or an ordered spinor chain with momentum insertions.",
 "ConstructiveDiagrams`SpinorTrace" -> "SpinorTrace[Mom[i1], Mom[i2], ...] \
represents a momentum-matrix trace.\nRepresents an ordered chiral trace of \
momentum matrices.", "ConstructiveDiagrams`Mom" -> "Mom[i] denotes the \
four-momentum of particle i.\nRepresents a momentum in a spinor chain or a \
propagator.", "ConstructiveDiagrams`Mass" -> "Mass[i] denotes the on-shell \
mass assigned to external particle i.\nRepresents an external-particle mass \
or the assigned mass of an internal channel.",
 "ConstructiveDiagrams`Multiparticle" -> "Multiparticle[i1, i2, ...] \
identifies the sum p_i1+p_i2+... as a typed channel label.\nLabels the sum of \
a specified set of external momenta.", "ConstructiveDiagrams`MomHat" -> "MomH\
at[i] MomHat[i] is the marked momentum used while deriving a supported \
complex deformation.\nRepresents a complex-deformed momentum insertion.",
 "ConstructiveDiagrams`En" -> "En[i] denotes the energy of external particle \
i.\nLabels the energy of an external particle in explicit spinor formulas.",
 "ConstructiveDiagrams`MomMagnitude" -> "MomMagnitude[i] denotes the \
magnitude of the three-momentum of external particle i.\nLabels the magnitude \
of a particle's spatial momentum.", "ConstructiveDiagrams`Theta" -> "Theta[i] \
denotes the polar angle measured from the positive z axis of external \
particle i.\nLabels the polar angle used for an explicit momentum or spinor."\
, "ConstructiveDiagrams`Phi" -> "Phi[i] denotes the azimuthal angle in the \
x-y plane of external particle i.\nLabels the azimuthal angle used for an \
explicit momentum or spinor.", "ConstructiveDiagrams`SpinorEps" -> "SpinorEps\
[variance, i, j] represents the antisymmetric little-group tensor with the \
given index variance.\nRepresents the antisymmetric tensor used to raise and \
lower massive little-group indices.", "ConstructiveDiagrams`Mandelstahm" -> "\
Mandelstahm[i, j] denotes the invariant (p_i+p_j)^2.\nRepresents the \
two-particle invariant (p_i+p_j)^2.",
 "ConstructiveDiagrams`MandelstahmHat" -> "MandelstahmHat[i, j] This is the \
invariant built from the correspondingly deformed momenta.\nRepresents the \
complex-deformed version of a two-particle invariant.",
 "ConstructiveDiagrams`ConstructiveAmplitude" -> "ConstructiveAmplitude[coupl\
ing, numerator, denominator] represents coupling \
numerator/denominator.\nStores an earlier amplitude representation as \
coupling, numerator and denominator.", "ConstructiveDiagrams`PropDen" -> "Pro\
pDen[Mom[channel], m] represents p_channel^2-m^2.\nPropDen[Mom[channel], m, \
width] represents p_channel^2-m^2+I m width.\nRepresents a physical \
propagator denominator without putting its momentum on shell.",
 "ConstructiveDiagrams`MomProd" -> "MomProd[i, j] represents \
p_i.p_j.\nMomProd[Multiparticle[i1, ...], j] expands the scalar product over \
a momentum sum.\nRepresents the Lorentz scalar product of two external or \
composite momenta.", "ConstructiveDiagrams`MomProdHat1" -> "MomProdHat1[i, j] \
MomProdHat1[i,j] marks p_i dot p_j when only the first momentum is \
deformed.\nRepresents a scalar product involving complex-deformed momenta.",
 "ConstructiveDiagrams`MomProdHat2" -> "MomProdHat2[i, j] MomProdHat2[i,j] \
marks p_i dot p_j when only the second momentum is deformed.\nRepresents a \
scalar product involving complex-deformed momenta.",
 "ConstructiveDiagrams`MomProdHat12" -> "MomProdHat12[i, j] MomProdHat12[i,j] \
marks a scalar product in which both momenta belong to the \
deformation.\nRepresents a scalar product involving complex-deformed \
momenta.", "ConstructiveDiagrams`SpinorHat" -> "SpinorHat[kind, type, i] \
marks a helicity, massive or reference spinor as \
shifted.\nSpinorHat[\"Spin\", variance, type, i, index] retains an explicit \
massive little-group index on a shifted spinor.\nRepresents a spinor whose \
momentum has been complex-deformed.", "ConstructiveDiagrams`xFactor" -> "xFac\
tor[massiveLegOrComposite, masslessLegOrComposite] records the ordered \
massive-pair and massless-vector labels of the three-point building \
block.\nRepresents a constructive three-point x-factor.",
 "ConstructiveDiagrams`xTildeFactor" -> "xTildeFactor[massiveLegOrComposite, \
masslessLegOrComposite] records the ordered massive-pair and massless-vector \
labels of the three-point building block.\nRepresents a constructive \
three-point x-factor.", "ConstructiveDiagrams`ApplyToNumerator" -> "ApplyToNu\
merator[expr, rules] applies one replacement rule or a list of rules to each \
term's numerator.\nApplies replacement rules to each existing additive term's \
numerator while preserving its denominator.",
 "ConstructiveDiagrams`ReduceSpinContractions" -> "ReduceSpinContractions[exp\
r] applies supported spin-sum and little-group contraction \
identities.\nContracts repeated spin indices and joins the associated spinor \
factors into momentum chains.",
 "ConstructiveDiagrams`ReduceSpinorProducts" -> "ReduceSpinorProducts[expr] \
returns a reduced expression using bounded, directed algebraic \
passes.\nSimplifies spinor chains, traces and products using ordinary \
identities and external-particle on-shell conditions.",
 "ConstructiveDiagrams`SimplifySpinorProducts" -> "SimplifySpinorProducts[exp\
r, opts] runs a directed iterative simplification with declared momenta and \
mass rules.\nRuns the earlier iterative simplifier with explicitly declared \
external momenta and optional internal-channel information.",
 "ConstructiveDiagrams`ExtractMandelstahm" -> "ExtractMandelstahm[expr] uses \
the historical four-point convention.\nExtractMandelstahm[expr, n] uses \
n-point kinematics, applying complementary-pair identifications only when n \
is 4.\nRewrites supported scalar momentum products and two-particle \
propagators in terms of s invariants.",
 "ConstructiveDiagrams`ReducexFactors" -> "ReducexFactors[expr, opts] applies \
the supported xFactorReduced and xTildeFactorReduced rules.\nReduces the \
supported legacy four-point x-factor formulas for an explicitly specified \
channel.", "ConstructiveDiagrams`ReduceColor" -> "ReduceColor[expr] reduces \
generator contractions, structure constants and traces in the canonical SU(3) \
representation.\nReduceColor[expr, \"IndexDimensions\" -> {i -> 3}] also \
supplies a dimension for a dummy index whose tensor context does not \
determine it.\nReduces contracted SU(3) color tensors and ordered generator \
traces.", "ConstructiveDiagrams`SchoutenRewrite" -> "SchoutenRewrite[c1, c2, \
{m, n}] returns an equivalent two-product expression when the split \
chiralities agree.\nRewrites a product of two spinor chains using a chosen \
generalized Schouten identity.", "ConstructiveDiagrams`SchoutenCandidates" ->
  "SchoutenCandidates[expr] returns edit associations for eligible pairs of \
chains and splits.\nFinds the generalized Schouten replacements that can be \
applied to products of chains in an expression.",
 "ConstructiveDiagrams`SpinorIdentityCandidates" -> "SpinorIdentityCandidates\
[{massRules, expr}, opts] returns available exact edit associations.\nLists \
the identity steps available for a spinor expression under its declared mass \
and momentum assumptions.", "ConstructiveDiagrams`ApplySpinorIdentity" -> "Ap\
plySpinorIdentity[{massRules, expr}, edit, opts] validates and applies an \
edit returned by SpinorIdentityCandidates.\nApplies one validated identity \
step to a spinor expression.",
 "ConstructiveDiagrams`SpinorExpressionComplexity" -> "SpinorExpressionComple\
xity[expr] counts terms, momentum insertions and scalar momentum products \
with the package's simplification weights.\nScores the size of a spinor \
expression, with extra weight for momentum insertions and scalar momentum \
products.", "ConstructiveDiagrams`ComplicateAmplitude" -> "ComplicateAmplitud\
e[{massRules, expr}, n] applies up to n eligible random edits; n defaults to \
5.\nComplicateAmplitude[{massRules, expr}, n, RandomSeed -> seed, \
\"RecordSteps\" -> True] records a reproducible trajectory.\nGenerates an \
equivalent expression by applying a random sequence of reversible identity \
steps.", "ConstructiveDiagrams`ReverseMomentum" -> "ReverseMomentum[expr, i] \
reverses external momentum i.\nReverseMomentum[expr, {i1, i2, ...}] crosses \
the listed legs in sequence.\nCrosses selected external momenta, including \
their spinor signs, in a complete amplitude expression.",
 "ConstructiveDiagrams`PermuteParticles" -> "PermuteParticles[expr, {i1, i2, \
..., in}] maps i1 to i2, i2 to i3, and the last label back to i1.\nCyclically \
relabels selected external particles in typed spinor and momentum \
expressions.", "ConstructiveDiagrams`ChooseSpinIndex" -> "ChooseSpinIndex[exp\
r, i, projection] inserts the explicit spin-index combination for particle \
i.\nSelects one physical spin projection of a massive external particle.",
 "ConstructiveDiagrams`ChooseSpinIndices" -> "ChooseSpinIndices[expr, \
projection1, projection2, ...] applies ChooseSpinIndex to legs 1,2,... with \
the corresponding projections.\nSelects physical spin projections for \
successive massive external particles.",
 "ConstructiveDiagrams`MakeIndicesExplicit" -> "MakeIndicesExplicit[expr, i, \
{indices}] assigns the listed indices to particle \
i.\nMakeIndicesExplicit[expr, indexLists] assigns the per-particle list at \
position i to particle i.\nInserts specified upper little-group indices into \
the implicit massive spinors of an expression.",
 "ConstructiveDiagrams`MakeIndicesImplicit" -> "MakeIndicesImplicit[expr] \
replaces explicit upper-index massive Spinor occurrences with their implicit \
form.\nRemoves the supported explicit upper massive-spin indices from spinor \
chains.", "ConstructiveDiagrams`SymmetrizeSpin" -> "SymmetrizeSpin[expr, i, \
{indices}] averages the distinct index permutations for particle i.\nAssigns \
and symmetrizes the little-group indices of one massive external particle.",
 "ConstructiveDiagrams`SymmetrizeSpins" -> "SymmetrizeSpins[expr, indexLists] \
applies SymmetrizeSpin to each particle's index list.\nSymmetrizes massive \
little-group indices for a list of external particles.",
 "ConstructiveDiagrams`ExpandSummation" -> "ExpandSummation[expr] writes \
eligible repeated symbolic indices as a two-component sum.\nWrites supported \
contracted massive-spin indices as explicit two-component sums.",
 "ConstructiveDiagrams`SquareAmplitude" -> "SquareAmplitude[expr, spins] \
constructs the conjugate product using the per-particle spins.\nConstructs an \
amplitude times its Hermitian conjugate, retaining the massive-spin \
contractions to be reduced.", "ConstructiveDiagrams`ExtractEnergy" -> "Extrac\
tEnergy[expr, particleSpins] substitutes momentum matrices and two-component \
spinors for the listed particles.\nExtractEnergy[expr, particleSpins, \
\"MassRules\" -> {Mass[i] -> mass, ...}] supplies known external on-shell \
masses for stable numerical evaluation.\nEvaluates supported spinor \
expressions in an explicit energy, momentum-angle and spin basis.",
 "ConstructiveDiagrams`SpinToHelicitySpinors" -> "SpinToHelicitySpinors[expr, \
{particle, spin, helicity}] uses the supported fixed-spin helicity \
expansion.\nSpinToHelicitySpinors[expr, specification1, specification2, ...] \
applies up to four specifications in sequence.\nApplies legacy \
spin-to-helicity expansions to selected massive particle spinors.",
 "ConstructiveDiagrams`ConvertStringAmplitude" -> "ConvertStringAmplitude[tex\
t, masses] uses the per-particle mass list to distinguish massless and \
massive spinors.\nConvertStringAmplitude[text, masses, InternalMasses -> \
rules] also supplies mass rules for composite endpoint labels.\nReads a \
compact spinor-bracket string and constructs a typed HEPCAT expression.",
 "ConstructiveDiagrams`ConvertStringHattedAmplitude" -> "ConvertStringHattedA\
mplitude[text, masses, opts] calls ConvertStringAmplitude and replaces Spinor \
and Mom by their hatted forms.\nParses a legacy bracket expression and marks \
its spinors and momenta as shifted.",
 "ConstructiveDiagrams`ComplexifyMomenta" -> "ComplexifyMomenta[expr] marks \
all supported Spinor, Mom and scalar-product \
occurrences.\nComplexifyMomenta[expr, i, j] marks the square spinor of leg i, \
angle spinor of leg j, and the supported shifted momentum factors.\nMarks \
supported momenta and spinors for a legacy complex deformation.",
 "ConstructiveDiagrams`UnhatMomenta" -> "UnhatMomenta[expr] replaces \
SpinorHat, MomHat and MomProdHat12 by Spinor, Mom and MomProd.\nRemoves the \
standard complex-deformation markers.",
 "ConstructiveDiagrams`ExpandComplexMomenta" -> "ExpandComplexMomenta[expr, \
{i, mi}, k, {j, mj}, l, mik] applies the implemented shift identities for the \
chosen mass pattern.\nExpands supported legacy complex-shift formulas with \
specified masses and companion legs.",
 "ConstructiveDiagrams`ReplaceSpinorZ" -> "ReplaceSpinorZ[expr, {i, mi}, {j, \
mj}, k] rewrites SpinorZ[\"Both\",I,J] when both specified masses are \
nonzero.\nReplaces a two-index auxiliary z object in a supported massive \
shift.", "ConstructiveDiagrams`TeX" -> "TeX[expr] returns LaTeX source text \
for the supported expression.\nConverts supported HEPCAT spinor expressions \
to a LaTeX string.", "ConstructiveDiagrams`SPINAS" -> "SPINAS[expr] returns a \
C++ expression using the older named-product convention.\nConverts supported \
chains and algebra to the earlier SPINAS C++ expression notation.",
 "ConstructiveDiagrams`PlainText" -> "PlainText[expr] returns compact \
human-readable text.\nPlainText[expr, \"Model\"] returns explicit text \
suitable for storing a supported exact expression in a HEPCAT \
model.\nConverts a spinor expression to readable bracket text or exact \
model-file syntax.", "ConstructiveDiagrams`UnHatSpinors" -> "UnHatSpinors[exp\
r] replaces each SpinorHat by Spinor.\nRemoves SpinorHat markers while \
leaving shifted momenta unchanged.", "ConstructiveDiagrams`ReadModel" -> "Rea\
dModel[directory, number] Read the numbered model files from a \
directory.\nLoad the particle content, amplitudes and parameters of a model \
for use throughout HEPCAT.", "ConstructiveDiagrams`ParticleMass" -> "Particle\
Mass[name, particles] Return the mass declared for a particle name.\nFind the \
mass declared for a particle or antiparticle.",
 "ConstructiveDiagrams`AppendModelAmplitudes" -> "AppendModelAmplitudes[direc\
tory, number, entries] Append {particles, amplitude} or {particles, factor, \
amplitudeOverFactor} entries to ampsN.mdl.\nAdd established amplitude \
expressions to a constructive model for later factorization calculations.",
 "ConstructiveDiagrams`ModelParameter" -> "ModelParameter[model, name] Find a \
declared parameter by its string name.\nModelParameter[name, scope, domain] \
Retain a parameter as inert model-local data.\nRefer to a mass, coupling or \
other parameter belonging to a particular model.",
 "ConstructiveDiagrams`ModelFunction" -> "ModelFunction[name, scope, \
arguments] Represent an external function belonging to a model.\nKeep a \
model-supplied external function as symbolic data.",
 "ConstructiveDiagrams`ModelParameterName" -> "ModelParameterName[parameter] \
Extract the name from a ModelParameter reference.\nReturn the string name of \
a model-local parameter.", "ConstructiveDiagrams`ModelParameterRules" -> "Mod\
elParameterRules[model] Resolve model variables and supported dependent \
functions.\nModelParameterRules[model, overrides] Apply local overrides keyed \
by ModelParameter references or string names.\nObtain numerical parameter \
substitutions while keeping model definitions local.",
 "ConstructiveDiagrams`LookupAmplitude" -> "LookupAmplitude[taggedParticles, \
model, rightSide, spinIndices] Match particle names, multiply the stored \
factor, and relabel typed particle slots.\nRetrieve a stored lower-point \
amplitude with a specified particle numbering.",
 "ConstructiveDiagrams`FactorizationAmplitudeChannels" -> "FactorizationAmpli\
tudeChannels[particles, model] Return the nonzero channel products for the \
specified all-ingoing process.\nBuild the known lower-point amplitude \
products that a full amplitude must reproduce at its physical poles.",
 "ConstructiveDiagrams`CreateDiagrams" -> "CreateDiagrams[{p1, p2, ...}, \
model] Generate trees for an ordered all-ingoing external particle \
list.\nCreateDiagrams[p1, p2, ..., model] Use the equivalent \
separate-particle argument form.\nGenerate the tree diagrams allowed by a \
model for an ordered external process.",
 "ConstructiveDiagrams`CreateArrowDiagram" -> "CreateArrowDiagram[diagram] \
Draw one diagram returned by CreateDiagrams.\nCreateArrowDiagram[diagram, \
options] Optionally display the vertex and propagator expressions.\nDraw one \
tree diagram and optionally inspect its vertex and propagator expressions.",
 "ConstructiveDiagrams`CreateArrowDiagrams" -> "CreateArrowDiagrams[diagrams] \
Draw a list returned by CreateDiagrams.\nCreateArrowDiagrams[diagrams, \
options] Apply diagram display options to every tree.\nDraw all the diagrams \
generated for a process.", "ConstructiveDiagrams`DiagramAmplitudes" -> "Diagr\
amAmplitudes[diagrams] Return {{massRules, expression}, ...} for the \
generated diagrams.\nObtain a symbolic amplitude expression and its mass \
replacements for each generated diagram.",
 "ConstructiveDiagrams`DiagramAmplitudeRecords" -> "DiagramAmplitudeRecords[d\
iagrams] Build a record for each generated diagram without adding an \
automatic shell reduction.\nDiagramAmplitudeRecords[diagrams, options] Apply \
a supplied pair-preserving simplification and record any declared shell \
usage.\nInspect the expression, propagators and assumptions associated with \
each diagram.", "ConstructiveDiagrams`PhysicalPoleResidue" -> "PhysicalPoleRe\
sidue[expression, channel] Extract the coefficient at one declared physical \
pole.\nPhysicalPoleResidue[expression, {channel1, channel2, ...}] Extract a \
compatible multiple-pole coefficient.\nPhysicalPoleResidue[expression, \
channels, \"KinematicMap\" -> map] Restrict the complete expression on a \
coherent physical cut chart.\nExtract the coefficient of one physical \
propagator pole, or of compatible simultaneous poles.",
 "ConstructiveDiagrams`CheckAmplitudes" -> "CheckAmplitudes[amplitude, \
channels, model] Run the combined checks and return a compact status \
table.\nCheckAmplitudes[amplitudePairs, channels, model, options] Check the \
sum while retaining external mass information.\nCheckAmplitudes[amplitude, \
channels, model, \"Output\" -> \"Details\"] Retain samples, differences, \
diagnostics and the run seed.\nCheck a proposed amplitude against physical \
factorization, spurious-pole, high-energy and reference-spinor conditions.",
 "ConstructiveDiagrams`PrepareAmplitudes" -> "PrepareAmplitudes[items, model] \
Prepare a mass-rule pair, a list of diagram pairs or compact \
factorization-channel rows.\nPrepareAmplitudes[items, model, \
\"PolePreference\" -> preference] Choose and simplify a bounded set of \
allowed reference-spinor assignments.\nPrepareAmplitudes[items, model, \
\"ReturnDetails\" -> True] Retain original inputs, selected references, \
discarded remainders and preparation diagnostics.\nSimplify diagram \
expressions, factorization channels or a completed amplitude before the next \
calculation stage.", "ConstructiveDiagrams`CompleteFactorization" -> "Complet\
eFactorization[amplitudes, channels, model] Fit corrections against all known \
physical channels simultaneously.\nCompleteFactorization[preparationReport, \
channelsOrReport, model] Start from prepared inputs while retaining discarded \
expressions and original residue targets.\nCompleteFactorization[input, \
channels, model, \"ReturnDetails\" -> True] Keep the candidate, affine \
freedom, correction directions and validation diagnostics.\nAdd corrections \
that make a candidate amplitude reproduce all supplied physical factorization \
channels together.", "ConstructiveDiagrams`CompleteHighEnergy" -> "CompleteHi\
ghEnergy[input, channels, model] Constrain a completed amplitude and its \
retained correction freedom.\nCompleteHighEnergy[input, channels, model, \
\"Output\" -> \"Details\"] Inspect the fit equations, affine family and \
independent validation.\nUse high-energy and other full-amplitude conditions \
to fix freedom left after physical factorization.",
 "ConstructiveDiagrams`ConstructAmplitude" -> "ConstructAmplitude[process, \
diagramModel, factorizationModel] Run the full workflow and return the \
validated bare expression.\nConstructAmplitude[process, diagramModel, \
factorizationModel, \"Output\" -> \"Amplitude\"] Retain the external mass \
rules with the expression.\nConstructAmplitude[process, diagramModel, \
factorizationModel, \"Output\" -> \"Details\"] Retain stage outputs, timings, \
checks and the chosen run seed.\nConstruct and check an amplitude from its \
external process and the models supplying its interactions and known \
lower-point amplitudes.", "ConstructiveDiagrams`FeynmanModel" -> "FeynmanMode\
l[particles, vertices, variables, functions] stores the four model tables \
used by the Feynman backend.\nRepresents a model of particles and \
Feynman-rule vertices read from CalcHEP model tables.",
 "ConstructiveDiagrams`DiracWord" -> "DiracWord[operator1, operator2, ...] \
represents an ordered product of Dirac matrices and slashed momenta.\nKeeps \
the order of gamma matrices and momentum slashes along a fermion line.",
 "ConstructiveDiagrams`DiracGamma" -> "DiracGamma[index] represents a Dirac \
gamma matrix carrying a Lorentz index.\nRepresents a gamma matrix with a \
Lorentz index in an ordered fermion-line numerator.",
 "ConstructiveDiagrams`DiracGamma5" -> "DiracGamma5 represents the chirality \
matrix in an ordered Dirac word.\nRepresents the chirality matrix acting at a \
specified position in a Dirac word.", "ConstructiveDiagrams`DiracSlash" -> "D\
iracSlash[Mom[label]] represents a momentum contracted with a Dirac gamma \
matrix.\nRepresents a four-momentum contracted with gamma matrices inside a \
Dirac word.", "ConstructiveDiagrams`LorentzIndex" -> "LorentzIndex[slot] \
labels a Lorentz slot in a Feynman tensor.\nLabels a Lorentz index in a \
Feynman vertex or diagram expression.",
 "ConstructiveDiagrams`LorentzMetric" -> "LorentzMetric[index1, index2] \
represents the Minkowski metric with two Lorentz indices.\nRepresents the \
Minkowski metric used to contract Lorentz indices in a Feynman expression.",
 "ConstructiveDiagrams`LorentzMomentum" -> "LorentzMomentum[momentum, index] \
represents a four-momentum carrying a Lorentz index.\nRepresents a momentum \
with one Lorentz index available for contraction.",
 "ConstructiveDiagrams`LorentzScalarProduct" -> "LorentzScalarProduct[p, q] \
retains an off-shell scalar product in a Feynman rule.\nRepresents a \
contracted momentum dot product before Feynman-rule conversion.",
 "ConstructiveDiagrams`FermionBar" -> "FermionBar[leg, mass, chirality] \
represents the left antifermion endpoint of an ordered Dirac chain.\nRecords \
the left antifermion wavefunction of an all-incoming Dirac chain.",
 "ConstructiveDiagrams`FermionKet" -> "FermionKet[leg, mass, chirality] \
represents the right fermion endpoint of an ordered Dirac chain.\nRecords the \
right fermion wavefunction of an all-incoming Dirac chain.",
 "ConstructiveDiagrams`DiracChain" -> "DiracChain[bar, word, ket] joins \
FermionBar and FermionKet endpoints through one DiracWord.\nRepresents a \
fermion-line numerator between specified external fermion endpoints.",
 "ConstructiveDiagrams`PolarizationVector" -> "PolarizationVector[leg, index, \
mass, helicity] represents an external vector wavefunction in the Feynman \
backend.\nRecords an external vector wavefunction and its Lorentz index in a \
Feynman diagram.", "ConstructiveDiagrams`VectorSpinIndex" -> "VectorSpinIndex\
[leg, slot] labels one of the two symmetric little-group slots of a massive \
vector.\nLabels either of the two symmetric spinor indices describing one \
massive-vector state.", "ConstructiveDiagrams`ColorIndex" -> "ColorIndex[leg] \
labels an external color state.\nIdentifies the external leg associated with \
a symbolic color index.", "ConstructiveDiagrams`ColorDelta" -> "ColorDelta[in\
dex1, index2] represents a contracted color Kronecker tensor.\nRepresents a \
Kronecker delta connecting two symbolic color indices.",
 "ConstructiveDiagrams`ColorGenerator" -> "ColorGenerator[antifundamental, \
fundamental, adjoint] represents a fundamental SU(3) generator.\nRepresents a \
fundamental SU(3) generator with ordered quark and gluon color slots.",
 "ConstructiveDiagrams`ColorStructure" -> "ColorStructure[a, b, c] represents \
an SU(3) structure constant.\nRepresents the SU(3) structure constant with \
three ordered adjoint color indices.", "ConstructiveDiagrams`ColorTrace" -> "\
ColorTrace[a, b, ...] represents Tr(T^a T^b ...) in the written \
order.\nColorTrace[] represents the trace of the fundamental \
identity.\nRepresents an ordered fundamental SU(3) generator trace.",
 "ConstructiveDiagrams`FeynmanToSpinorProducts" -> "FeynmanToSpinorProducts[e\
xpression] converts a contracted Feynman tensor \
expression.\nFeynmanToSpinorProducts[{massRules, expression}] preserves the \
external mass-rule pair.\nFeynmanToSpinorProducts[amplitudes] converts a list \
of amplitude pairs.\nRewrites a contracted Feynman amplitude in HEPCAT \
angle/square spinor notation while retaining internal virtualities.",
 "ConstructiveDiagrams`WriteSPINASProcess" -> "WriteSPINASProcess[directory, \
particles, amplitude, model] exports one all-incoming amplitude at its fixed \
massless helicities.\nWriteSPINASProcess[directory, entries, model] combines \
helicity-related {particles, amplitude} entries into one \
class.\nWriteSPINASProcess[directory, incoming -> outgoing, entries, model] \
declares the physical process and crosses its outgoing legs \
automatically.\nWrites a runnable SPINAS C++ process from a spinor amplitude \
or a complete set of its helicity amplitudes.",
 "ConstructiveDiagrams`ValidateMadGraph" -> "ValidateMadGraph[directory] \
builds and runs the MadGraph validator of a SPINAS project exported by the \
current WriteSPINASProcess.\nValidateMadGraph[incoming -> outgoing, entries, \
model] exports and validates the supplied all-incoming amplitude entries for \
the declared physical process.\nValidateMadGraph[particles, amplitude, model] \
validates one all-incoming amplitude at its specified massless helicities, \
with IncomingLegs selecting the physical incoming \
pair.\nValidateMadGraph[input, ..., \"Output\" -> \"Details\"] returns the \
full report, including per-helicity failures and the actual random \
seed.\nChecks a HEPCAT/SPINAS amplitude against an independently generated \
MadGraph result at common random physical momenta."|>