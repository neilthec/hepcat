<|"ConstructiveDiagrams`Spinor" -> "Spinor[\"Helicity\", type, i] represents \
a massless external spinor of chirality \"Angle\" or \
\"Square\".\nSpinor[\"Spin\", type, i] represents a massive spinor with its \
little-group index implicit.\nSpinor[\"Spin\", variance, type, i, index] \
makes the \"Upper\" or \"Lower\" little-group index explicit.\nSpinor[\"Xi\", \
type, i] represents an auxiliary reference spinor; its label need not be an \
external particle.\nRepresents a massless helicity spinor, a massive spin \
spinor, or an auxiliary reference spinor.",
 "ConstructiveDiagrams`SpinorChain" -> "SpinorChain[left, right] represents a \
bracket with two spinor endpoints.\nSpinorChain[left, Mom[i1], Mom[i2], ..., \
right] represents a momentum chain with its given order.\nRepresents an angle \
or square spinor bracket, optionally with an ordered sequence of momentum \
insertions.", "ConstructiveDiagrams`SpinorTrace" -> "SpinorTrace[Mom[i1], \
Mom[i2], ...] represents a momentum-matrix trace.\nRepresents the ordered \
chiral trace of momentum matrices.", "ConstructiveDiagrams`Mom" -> "Mom[i] \
constructs the corresponding symbolic object.\nRepresents a momentum \
insertion in HEPCAT spinor algebra.", "ConstructiveDiagrams`Mass" -> "Mass[i] \
constructs the corresponding symbolic object.\nRepresents the symbolic mass \
assigned to a particle or a channel.",
 "ConstructiveDiagrams`Multiparticle" -> "Multiparticle[i1, i2, ...] \
constructs the corresponding symbolic object.\nLabels a sum of external \
particle momenta.", "ConstructiveDiagrams`MultiparticleDiff" -> "Multiparticl\
eDiff[i, j] constructs the corresponding symbolic object.\nRepresents a \
legacy composite label for a difference of particle momenta.",
 "ConstructiveDiagrams`Particle" -> "Particle[i] constructs the corresponding \
symbolic object.\nWraps a particle label for older display and export \
helpers.", "ConstructiveDiagrams`MomHat" -> "MomHat[i] constructs the \
corresponding symbolic object.\nRepresents a complex-deformed momentum \
insertion.", "ConstructiveDiagrams`MomQ" -> "MomQ[i] constructs the \
corresponding symbolic object.\nRepresents an auxiliary q-momentum label in \
symbolic output.", "ConstructiveDiagrams`En" -> "En[i] constructs the \
corresponding symbolic object.\nLabels the energy of an external particle in \
explicit spinor formulas.", "ConstructiveDiagrams`MomMagnitude" -> "MomMagnit\
ude[i] constructs the corresponding symbolic object.\nLabels the magnitude of \
a particle's spatial momentum.", "ConstructiveDiagrams`Theta" -> "Theta[i] \
constructs the corresponding symbolic object.\nLabels the polar angle used \
for an explicit momentum or spinor.", "ConstructiveDiagrams`Phi" -> "Phi[i] \
constructs the corresponding symbolic object.\nLabels the azimuthal angle \
used for an explicit momentum or spinor.", "ConstructiveDiagrams`Coupling" ->
  "Coupling[i] constructs the corresponding symbolic object.\nRepresents a \
symbolic indexed coupling in legacy amplitude expressions.",
 "ConstructiveDiagrams`rr" -> "rr[i, j] constructs the corresponding symbolic \
object.\nRepresents a legacy indexed r label used in formatted expressions.",
 "ConstructiveDiagrams`sigma" -> "sigma[i] constructs the corresponding \
symbolic object.\nRepresents a symbolic Pauli-matrix label for legacy \
display.", "ConstructiveDiagrams`delta" -> "delta constructs the \
corresponding symbolic object.\nIs a legacy symbolic delta display marker.",
 "ConstructiveDiagrams`SpinorEps" -> "SpinorEps[variance, i, j] constructs \
the corresponding symbolic object.\nRepresents an explicit little-group \
epsilon tensor.", "ConstructiveDiagrams`SpinorZ" -> "SpinorZ[\"Both\", i, j] \
constructs the corresponding symbolic object.\nRepresents an indexed \
auxiliary z object in legacy complex shifts.",
 "ConstructiveDiagrams`Mandelstahm" -> "Mandelstahm[i, j] constructs the \
corresponding symbolic object.\nRepresents the scalar invariant (p_i+p_j)^2."\
, "ConstructiveDiagrams`MandelstahmHat" -> "MandelstahmHat[i, j] constructs \
the corresponding symbolic object.\nRepresents the complex-deformed version \
of a two-particle invariant.", "ConstructiveDiagrams`zPlus" -> "zPlus[channel\
] constructs the corresponding symbolic object.\nLabels the plus root of a \
complex-shift pole equation.", "ConstructiveDiagrams`zMinus" -> "zMinus[chann\
el] constructs the corresponding symbolic object.\nLabels the minus root of a \
complex-shift pole equation.",
 "ConstructiveDiagrams`ConstructiveAmplitude" -> "ConstructiveAmplitude[coupl\
ing, numerator, denominator] represents coupling \
numerator/denominator.\nStores a legacy amplitude as a coupling, numerator \
and denominator.", "ConstructiveDiagrams`PropDen" -> "PropDen[Mom[channel], \
m] represents p_channel^2-m^2.\nPropDen[Mom[channel], m, width] represents \
p_channel^2-m^2+I m width.\nRepresents an off-shell physical propagator \
denominator.", "ConstructiveDiagrams`MomProd" -> "MomProd[i, j] represents \
p_i.p_j.\nMomProd[Multiparticle[i1, ...], j] expands the scalar product over \
a momentum sum.\nRepresents a Lorentz scalar product of particle or composite \
momenta.", "ConstructiveDiagrams`MomProdHat1" -> "MomProdHat1[i, j] \
represents a scalar product with the first momentum shifted and the second \
unshifted.\nRepresents a scalar product involving complex-deformed momenta.",
 "ConstructiveDiagrams`MomProdHat2" -> "MomProdHat2[i, j] represents a scalar \
product with the first momentum unshifted and the second shifted.\nRepresents \
a scalar product involving complex-deformed momenta.",
 "ConstructiveDiagrams`MomProdHat12" -> "MomProdHat12[i, j] represents a \
scalar product with both momenta shifted.\nRepresents a scalar product \
involving complex-deformed momenta.", "ConstructiveDiagrams`SpinorHat" -> "Sp\
inorHat[kind, type, i] marks a helicity, massive or reference spinor as \
shifted.\nSpinorHat[\"Spin\", variance, type, i, index] retains an explicit \
massive little-group index on a shifted spinor.\nRepresents a spinor whose \
momentum has been complex-deformed.",
 "ConstructiveDiagrams`SpinorChainReduced" -> "SpinorChainReduced[args] \
exposes the automatic elementary reduction rules.\nIs the rule-bearing \
reduction head used internally for SpinorChain.",
 "ConstructiveDiagrams`SpinorTraceReduced" -> "SpinorTraceReduced[args] \
exposes the automatic elementary reduction rules.\nIs the rule-bearing \
reduction head used internally for SpinorTrace.",
 "ConstructiveDiagrams`xFactor" -> "xFactor[massiveLegOrComposite, \
masslessLegOrComposite] records the ordered x-factor labels.\nRepresents a \
constructive three-point x-factor.", "ConstructiveDiagrams`xTildeFactor" -> "\
xTildeFactor[massiveLegOrComposite, masslessLegOrComposite] records the \
ordered x-factor labels.\nRepresents a constructive three-point x-factor.",
 "ConstructiveDiagrams`applyToNumerator" -> "applyToNumerator[expr, rules] \
applies one replacement rule or a list of rules to each term's \
numerator.\nApplies rules or a function to the numerator of each additive \
term in its existing form.", "ConstructiveDiagrams`ReduceSpinContractions" ->
  "ReduceSpinContractions[expr] applies supported spin-sum and little-group \
contraction identities.\nSews contracted massive or massless spinor indices \
into shorter momentum chains.",
 "ConstructiveDiagrams`ReduceSpinorProducts" -> "ReduceSpinorProducts[expr] \
returns a reduced expression using bounded, directed algebraic \
passes.\nReduces spinor chains and traces using ordinary identities and \
external on-shell conditions.",
 "ConstructiveDiagrams`SimplifySpinorProducts" -> "SimplifySpinorProducts[exp\
r, opts] runs a directed iterative simplification with declared momenta and \
mass rules.\nRuns the older iterative momentum-conservation and spinor \
simplifier.", "ConstructiveDiagrams`ExtractMandelstahm" -> "ExtractMandelstah\
m[expr] uses the historical four-point convention.\nExtractMandelstahm[expr, \
n] uses n-point kinematics, applying complementary-pair identifications only \
when n is 4.\nRewrites scalar momentum products and supported two-particle \
propagators using s invariants.", "ConstructiveDiagrams`ReducexFactors" -> "R\
educexFactors[expr, opts] applies the supported xFactorReduced and \
xTildeFactorReduced rules.\nReduces legacy x-factor expressions using an \
explicitly declared four-point channel.",
 "ConstructiveDiagrams`ReduceColor" -> "ReduceColor[expr] applies supported \
fundamental-color trace and completeness identities.\nReduces legacy SU(3) \
generator and structure-constant expressions to traces.",
 "ConstructiveDiagrams`SchoutenRewrite" -> "SchoutenRewrite[c1, c2, {m, n}] \
returns an equivalent two-product expression when the split chiralities \
agree.\nConstructs the generalized Schouten identity for two chains at \
specified momentum splits.", "ConstructiveDiagrams`SchoutenCandidates" -> "Sc\
houtenCandidates[expr] returns edit associations for eligible pairs of chains \
and splits.\nLists exact generalized Schouten edits available in an \
expression.", "ConstructiveDiagrams`SpinorIdentityCandidates" -> "SpinorIdent\
ityCandidates[{massRules, expr}, opts] returns available exact edit \
associations.\nEnumerates legal symbolic identity edits for a diagram or \
amplitude expression.", "ConstructiveDiagrams`ApplySpinorIdentity" -> "ApplyS\
pinorIdentity[{massRules, expr}, edit, opts] validates and applies an edit \
returned by SpinorIdentityCandidates.\nApplies a currently legal identity \
edit and returns the new expression.",
 "ConstructiveDiagrams`SpinorExpressionComplexity" -> "SpinorExpressionComple\
xity[expr] counts terms, momentum insertions and scalar momentum products \
with the package's simplification weights.\nReturns a scalar complexity score \
for a spinor expression.", "ConstructiveDiagrams`ComplicateAmplitude" -> "Com\
plicateAmplitude[{massRules, expr}, n] applies up to n eligible random edits; \
n defaults to 5.\nComplicateAmplitude[{massRules, expr}, n, RandomSeed -> \
seed, \"RecordSteps\" -> True] records a reproducible trajectory.\nGenerates \
a model-free random sequence of reversible symbolic identities.",
 "ConstructiveDiagrams`ReverseMomentum" -> "ReverseMomentum[expr, i] reverses \
external momentum i.\nReverseMomentum[expr, {i1, i2, ...}] crosses the listed \
legs in sequence.\nCrosses selected external legs in complete spinor \
amplitude expressions.", "ConstructiveDiagrams`PermuteParticles" -> "PermuteP\
articles[expr, {i1, i2, ..., in}] maps i1 to i2, i2 to i3, and the last label \
back to i1.\nCyclically relabels the specified external particles in typed \
amplitude expressions.", "ConstructiveDiagrams`ReverseSequence" -> "ReverseSe\
quence[a1, a2, ..., an] returns the arguments in reverse sequence \
order.\nReverses a sequence of arguments for use inside another expression.",
 "ConstructiveDiagrams`ChooseSpinIndex" -> "ChooseSpinIndex[expr, i, \
projection] inserts the explicit spin-index combination for particle \
i.\nSelects a physical spin projection for one external massive particle.",
 "ConstructiveDiagrams`ChooseSpinIndices" -> "ChooseSpinIndices[expr, \
projection1, projection2, ...] applies ChooseSpinIndex to legs 1,2,... with \
the corresponding projections.\nSelects spin projections in external-particle \
order.", "ConstructiveDiagrams`MakeIndicesExplicit" -> "MakeIndicesExplicit[e\
xpr, i, {indices}] assigns the listed indices to particle \
i.\nMakeIndicesExplicit[expr, indexLists] assigns the per-particle list at \
position i to particle i.\nReplaces implicit massive spin indices with \
specified explicit upper indices.",
 "ConstructiveDiagrams`MakeIndicesImplicit" -> "MakeIndicesImplicit[expr] \
replaces explicit upper-index massive Spinor occurrences with their implicit \
form.\nRemoves supported explicit upper massive spin indices from chains.",
 "ConstructiveDiagrams`SymmetrizeSpin" -> "SymmetrizeSpin[expr, i, {indices}] \
averages the distinct index permutations for particle i.\nInserts and \
symmetrizes the little-group indices of one massive particle.",
 "ConstructiveDiagrams`SymmetrizeSpins" -> "SymmetrizeSpins[expr, indexLists] \
applies SymmetrizeSpin to each particle's index list.\nSymmetrizes massive \
little-group indices using a list in external-particle order.",
 "ConstructiveDiagrams`ExpandSummation" -> "ExpandSummation[expr] writes \
eligible repeated symbolic indices as a two-component sum.\nExpands supported \
contracted massive spin indices into explicit plus/minus half components.",
 "ConstructiveDiagrams`SquareAmplitude" -> "SquareAmplitude[expr, spins] \
constructs the conjugate product using the per-particle spins.\nForms the \
amplitude times its Hermitian conjugate with explicit massive indices.",
 "ConstructiveDiagrams`ExtractEnergy" -> "ExtractEnergy[expr, particleSpins] \
substitutes momentum matrices and two-component spinors for the listed \
particles.\nExtractEnergy[expr, particleSpins, \"MassRules\" -> {Mass[i] -> \
mass, ...}] supplies known external on-shell masses for stable numerical \
evaluation.\nEvaluates supported spinor chains as explicit energy-angle \
matrix expressions.", "ConstructiveDiagrams`ExtractEnergy1" -> "ExtractEnergy\
1[expr, i, energy, momentumMagnitude, theta, phi] replaces supported momentum \
occurrences for particle i.\nExtractEnergy1[expr, i, energy, \
momentumMagnitude, theta, phi, mass] also supplies the known external \
on-shell mass for stable numerical components.\nSubstitutes one particle's \
momentum matrices and explicit four-vector components.",
 "ConstructiveDiagrams`ExtractEnergy2" -> "ExtractEnergy2[expr, i, energy, \
momentumMagnitude, theta, phi] replaces supported helicity and indexed \
massive spinor endpoints on particle i.\nExtractEnergy2[expr, i, energy, \
momentumMagnitude, theta, phi, mass] also supplies the known external \
on-shell mass for stable numerical components.\nSubstitutes explicit \
two-component spinor rows and columns for one particle.",
 "ConstructiveDiagrams`MomProdExplicit" -> "MomProdExplicit[upperFourVector, \
lowerFourVector] represents the intermediate Dot product before final \
evaluation.\nStores the intermediate explicit four-vector scalar product used \
by ExtractEnergy.", "ConstructiveDiagrams`SpinToHelicitySpinors" -> "SpinToHe\
licitySpinors[expr, {particle, spin, helicity}] uses the supported fixed-spin \
helicity expansion.\nSpinToHelicitySpinors[expr, specification1, \
specification2, ...] applies up to four specifications in sequence.\nApplies \
legacy spin-to-helicity expansions to selected massive particle spinors.",
 "ConstructiveDiagrams`ConvertStringAmplitude" -> "ConvertStringAmplitude[tex\
t, masses] uses the per-particle mass list to distinguish massless and \
massive spinors.\nConvertStringAmplitude[text, masses, InternalMasses -> \
rules] also supplies mass rules for composite endpoint labels.\nParses legacy \
compact bracket notation into typed HEPCAT spinor expressions.",
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
shift.", "ConstructiveDiagrams`nice" -> "nice[expr] returns the corresponding \
formatted output.\nFormats legacy HEPCAT objects in readable notebook \
notation.", "ConstructiveDiagrams`lnice" -> "lnice[spinor] returns the \
corresponding formatted output.\nFormats a single spinor as the left endpoint \
of a displayed chain.", "ConstructiveDiagrams`TeX" -> "TeX[expr] returns the \
corresponding formatted output.\nConverts supported HEPCAT expressions into a \
LaTeX string.", "ConstructiveDiagrams`lTeX" -> "lTeX[spinor] returns the \
corresponding formatted output.\nReturns the LaTeX string for a spinor at the \
left end of a chain.", "ConstructiveDiagrams`lPlainText" -> "lPlainText[spino\
r] returns the corresponding formatted output.\nReturns compact plain text \
for the left endpoint of a chain.", "ConstructiveDiagrams`SPINAS" -> "SPINAS[\
expr] returns the corresponding formatted output.\nFormats a supported \
expression as a legacy SPINAS C++ expression string.",
 "ConstructiveDiagrams`lSPINAS" -> "lSPINAS[spinor] returns the corresponding \
formatted output.\nReturns the legacy SPINAS name prefix for a chain's left \
endpoint.", "ConstructiveDiagrams`PlainText" -> "PlainText[expr] returns \
compact human-readable text.\nPlainText[expr, \"Model\"] returns explicit \
text suitable for storing a supported exact expression in a HEPCAT \
model.\nWrites readable bracket notation or explicit model-readable amplitude \
text.", "ConstructiveDiagrams`CreateSPName" -> "CreateSPName[chain] returns \
C++ source text.\nConstructs the legacy SPINAS member name for one chain.",
 "ConstructiveDiagrams`CreateSPInitializationStatement" -> "CreateSPInitializ\
ationStatement[chain] returns C++ source text.\nWrites a legacy SPINAS \
sproduct initialization statement.",
 "ConstructiveDiagrams`CreateSPUpdateStatement" -> "CreateSPUpdateStatement[c\
hain] returns C++ source text.\nWrites a legacy SPINAS spinor-product update \
statement.", "ConstructiveDiagrams`InitializeSPINASSpinorProducts" -> "Initia\
lizeSPINASSpinorProducts[expr] returns C++ source text.\nWrites \
initialization statements for distinct spinor chains in an expression.",
 "ConstructiveDiagrams`UpdateSPINASSpinorProducts" -> "UpdateSPINASSpinorProd\
ucts[expr] returns C++ source text.\nWrites update statements for distinct \
spinor products in an expression.",
 "ConstructiveDiagrams`DeclareSPINASSpinorProducts" -> "DeclareSPINASSpinorPr\
oducts[expr] returns C++ source text.\nWrites a legacy sproduct member \
declaration for chains in an expression.",
 "ConstructiveDiagrams`HelicitySpinorHat" -> "HelicitySpinorHat identifies \
the retained name; it is not a supported executable routine in this \
checkout.\nIs a retained protected compatibility symbol without an active \
public implementation.", "ConstructiveDiagrams`SpinSpinorHat" -> "SpinSpinorH\
at identifies the retained name; it is not a supported executable routine in \
this checkout.\nIs a retained protected compatibility symbol without an \
active public implementation.", "ConstructiveDiagrams`rSpinor" -> "rSpinor \
identifies the retained name; it is not a supported executable routine in \
this checkout.\nIs a retained protected compatibility symbol without an \
active public implementation.", "ConstructiveDiagrams`MomProdL" -> "MomProdL \
identifies the retained name; it is not a supported executable routine in \
this checkout.\nIs a retained protected compatibility symbol without an \
active public implementation.", "ConstructiveDiagrams`MomProdR" -> "MomProdR \
identifies the retained name; it is not a supported executable routine in \
this checkout.\nIs a retained protected compatibility symbol without an \
active public implementation.", "ConstructiveDiagrams`ReduceXFactorsP" -> "Re\
duceXFactorsP identifies the retained name; it is not a supported executable \
routine in this checkout.\nIs a retained protected compatibility symbol \
without an active public implementation.",
 "ConstructiveDiagrams`ReduceXFactorsM" -> "ReduceXFactorsM identifies the \
retained name; it is not a supported executable routine in this checkout.\nIs \
a retained protected compatibility symbol without an active public \
implementation.", "ConstructiveDiagrams`Output" -> "Output[ConstructiveDiagra\
ms`zp] formats the historical z_p marker.\nIs a legacy display marker with a \
single specialized formatting definition.",
 "ConstructiveDiagrams`ExpandMomHatAlt" -> "ExpandMomHatAlt[expr] rewrites \
shifted composite propagators and chain momentum insertions.\nExpands \
supported all-line-shift momentum and propagator markers.",
 "ConstructiveDiagrams`UnHatSpinors" -> "UnHatSpinors[expr] replaces each \
SpinorHat by Spinor.\nRemoves SpinorHat markers while leaving shifted momenta \
unchanged.", "ConstructiveDiagrams`xFactorReduced" -> "xFactorReduced[i, \
Multiparticle[j, k], opts] uses the declared momenta, channel and channel \
mass.\nEvaluates a supported legacy four-point composite x-factor formula.",
 "ConstructiveDiagrams`xTildeFactorReduced" -> "xTildeFactorReduced[i, \
Multiparticle[j, k], opts] uses the declared momenta, channel and channel \
mass.\nEvaluates a supported legacy four-point composite x-factor formula.",
 "ConstructiveDiagrams`colorT" -> "colorT[args] forms the symbolic color \
object.\nRepresents a fundamental SU(3) generator in the legacy color \
reducer.", "ConstructiveDiagrams`colorf" -> "colorf[args] forms the symbolic \
color object.\nRepresents a structure-constant object in the legacy color \
trace convention.", "ConstructiveDiagrams`colorTrace" -> "colorTrace[args] \
forms the symbolic color object.\nRepresents a trace of legacy colorT \
generators.", "ConstructiveDiagrams`ReadModel" -> "ReadModel[directory, \
number] Read the numbered model files from a directory.\nRead a constructive \
amplitude model or an imported CalcHEP Feynman model.",
 "ConstructiveDiagrams`ReadVariables" -> "ReadVariables[file] Return the \
variable rows in a numbered varsN.mdl file.\nRead a model variable table \
without assigning global parameter values.",
 "ConstructiveDiagrams`ReadFunctions" -> "ReadFunctions[file] Return the rows \
of a funcN.mdl table.\nRead the dependent parameter expressions in a model \
function table.", "ConstructiveDiagrams`ReadParticles" -> "ReadParticles[file\
] Read the eleven-column prtclsN.mdl table.\nRead particle and antiparticle \
declarations from a model table.", "ConstructiveDiagrams`ParticleMass" -> "Pa\
rticleMass[name, particles] Return the mass declared for a particle \
name.\nLook up a particle or antiparticle mass in a particle table.",
 "ConstructiveDiagrams`ReadAmplitudes" -> "ReadAmplitudes[file, particles] \
Read an ampsN.mdl table using the particle declarations for spin and mass \
information.\nRead complete constructive amplitudes and their separate \
overall factors.", "ConstructiveDiagrams`ConvertAmplitude" -> "ConvertAmplitu\
de[row, particles] Convert the final amplitude string using the row's \
external particles.\nConvertAmplitude[row, particles, scope] Use an explicit \
parameter scope for model data.\nConvert a constructive model row from text \
into typed spinor notation.", "ConstructiveDiagrams`ConvertAmplitudeOld" -> "\
ConvertAmplitudeOld[string, particles] Use the older string-replacement \
converter; new code should use ConvertAmplitude.\nAccess the historical \
string converter retained for compatibility.",
 "ConstructiveDiagrams`AddOppositeHelicityAmplitudes" -> "AddOppositeHelicity\
Amplitudes[rows] Retain each row and add its helicity-reversed bracket \
partner when applicable.\nMechanically add angle-square conjugate rows with \
reversed massless helicities.",
 "ConstructiveDiagrams`AppendModelAmplitudes" -> "AppendModelAmplitudes[direc\
tory, number, entries] Append {particles, amplitude} or {particles, factor, \
amplitudeOverFactor} entries to ampsN.mdl.\nAppend validated text \
representations of supplied amplitudes to a constructive model file.",
 "ConstructiveDiagrams`ModelParameter" -> "ModelParameter[model, name] Find a \
declared parameter by its string name.\nModelParameter[name, scope, domain] \
Retain a parameter as inert model-local data.\nRepresent and retrieve a \
parameter belonging to one model.", "ConstructiveDiagrams`ModelFunction" -> "\
ModelFunction[name, scope, arguments] Represent an external function \
belonging to a model.\nRetain an external model function call as inert data."\
, "ConstructiveDiagrams`ModelParameterName" -> "ModelParameterName[parameter] \
Extract the name from a ModelParameter reference.\nGet the string name of a \
model-local parameter.", "ConstructiveDiagrams`ModelParameterRules" -> "Model\
ParameterRules[model] Resolve model variables and supported dependent \
functions.\nModelParameterRules[model, overrides] Apply local overrides keyed \
by ModelParameter references or string names.\nResolve model parameters \
numerically without changing the symbolic amplitude or global session.",
 "ConstructiveDiagrams`FactorizationPartitions" -> "FactorizationPartitions[p\
articles] Partition numbered external particles into two sides containing at \
least two external legs each.\nEnumerate distinct complementary external-leg \
partitions for tree factorization.",
 "ConstructiveDiagrams`LookupAmplitude" -> "LookupAmplitude[taggedParticles, \
model, rightSide, spinIndices] Match particle names, multiply the stored \
factor, and relabel typed particle slots.\nRetrieve a complete model \
amplitude with its external labels reassigned.",
 "ConstructiveDiagrams`FactorizationAmplitudeChannels" -> "FactorizationAmpli\
tudeChannels[particles, model] Return the nonzero channel products for the \
specified all-ingoing process.\nSew known lower-point amplitudes into the \
physical factorization channels.", "ConstructiveDiagrams`CreateDiagrams" -> "\
CreateDiagrams[{p1, p2, ...}, model] Generate trees for an ordered \
all-ingoing external particle list.\nCreateDiagrams[p1, p2, ..., model] Use \
the equivalent separate-particle argument form.\nGenerate tree diagrams from \
a supplied constructive or Feynman model.",
 "ConstructiveDiagrams`CreateArrowDiagram" -> "CreateArrowDiagram[diagram] \
Draw one diagram returned by CreateDiagrams.\nCreateArrowDiagram[diagram, \
options] Optionally display the vertex and propagator expressions.\nDraw one \
generated tree with external labels and directed particle lines.",
 "ConstructiveDiagrams`CreateArrowDiagrams" -> "CreateArrowDiagrams[diagrams] \
Draw a list returned by CreateDiagrams.\nCreateArrowDiagrams[diagrams, \
options] Apply diagram display options to every tree.\nDraw the generated \
trees for a complete process.", "ConstructiveDiagrams`diagramAmplitude" -> "d\
iagramAmplitude[diagram] Return one diagram's bare amplitude \
expression.\nEvaluate the expression associated with one generated diagram.",
 "ConstructiveDiagrams`diagramAmplitudes" -> "diagramAmplitudes[diagrams] \
Return {{massRules, expression}, ...} for the generated diagrams.\nEvaluate \
every tree while retaining its separate mass and internal-cut declarations.",
 "ConstructiveDiagrams`DiagramAmplitudeRecords" -> "DiagramAmplitudeRecords[d\
iagrams] Build a record for each generated diagram without adding an \
automatic shell reduction.\nDiagramAmplitudeRecords[diagrams, options] Apply \
a supplied pair-preserving simplification and record any declared shell \
usage.\nRetain explicit diagram expressions, physical propagators and \
numerator assumptions for diagnostics.",
 "ConstructiveDiagrams`ExpandXFactors" -> "ExpandXFactors[expression, \
particleCount] Apply the supported x-factor rewrites for labels 1 through \
particleCount.\nApply the retained x-factor identities and external \
reference-spinor expansion.", "ConstructiveDiagrams`ConstructiveDiagram" -> "\
ConstructiveDiagram[particles, model] Match one three-point constructive \
amplitude.\nConstructiveDiagram[particles, internalParticle, model, channel] \
Evaluate the selected four-point exchange with channel labels such as \
{1,2}.\nEvaluate an older three-point vertex or a selected four-point \
exchange.", "ConstructiveDiagrams`SChannelDiagram" -> "SChannelDiagram[p1, \
p2, p3, p4, internalParticle, model] Call ConstructiveDiagram for channel {1, \
2}.\nEvaluate a selected four-point s-channel exchange with the historical \
constructive helper.", "ConstructiveDiagrams`TChannelDiagram" -> "TChannelDia\
gram[p1, p2, p3, p4, internalParticle, model] Call ConstructiveDiagram for \
channel {1, 3}.\nEvaluate a selected four-point t-channel exchange with the \
historical constructive helper.", "ConstructiveDiagrams`UChannelDiagram" -> "\
UChannelDiagram[p1, p2, p3, p4, internalParticle, model] Call \
ConstructiveDiagram for channel {1, 4}.\nEvaluate a selected four-point \
u-channel exchange with the historical constructive helper.",
 "ConstructiveDiagrams`SimplifyFactorizationChannels" -> "SimplifyFactorizati\
onChannels[channels] Retain each channel row and simplify its last expression \
using its selected cut.\nApply the historical spinor simplifier separately to \
compact channel rows.", "ConstructiveDiagrams`SumResiduesAlt" -> "SumResidues\
Alt[channels] Cancel removable factors and evaluate the zPlus and zMinus \
poles of each shifted channel.\nSum the two finite-pole residues in a \
previously shifted channel representation.",
 "ConstructiveDiagrams`PhysicalPoleResidue" -> "PhysicalPoleResidue[expressio\
n, channel] Extract the coefficient at one declared physical \
pole.\nPhysicalPoleResidue[expression, {channel1, channel2, ...}] Extract a \
compatible multiple-pole coefficient.\nPhysicalPoleResidue[expression, \
channels, \"KinematicMap\" -> map] Restrict the complete expression on a \
coherent physical cut chart.\nExtract a single- or compatible multiple-pole \
coefficient with explicit kinematic scope.",
 "ConstructiveDiagrams`CheckAmplitudes" -> "CheckAmplitudes[amplitude, \
channels, model] Run the combined checks and return a compact status \
table.\nCheckAmplitudes[amplitudePairs, channels, model, options] Check the \
sum while retaining external mass information.\nCheckAmplitudes[amplitude, \
channels, model, \"Output\" -> \"Details\"] Retain samples, differences, \
diagnostics and the run seed.\nCheck physical residues, apparent spurious \
poles, high-energy growth and reference dependence.",
 "ConstructiveDiagrams`CheckPhysicalResidues" -> "CheckPhysicalResidues[ampli\
tude, channels, model, options] Call CheckAmplitudes with the same arguments \
and options.\nUse the earlier name for the combined amplitude checker.",
 "ConstructiveDiagrams`PrepareAmplitudes" -> "PrepareAmplitudes[items, model] \
Prepare a mass-rule pair, a list of diagram pairs or compact \
factorization-channel rows.\nPrepareAmplitudes[items, model, \
\"PolePreference\" -> preference] Choose and simplify a bounded set of \
allowed reference-spinor assignments.\nPrepareAmplitudes[items, model, \
\"ReturnDetails\" -> True] Retain original inputs, selected references, \
discarded remainders and preparation diagnostics.\nSimplify diagram pairs or \
factorization rows while retaining their physical assumptions.",
 "ConstructiveDiagrams`PrepareDiagramAmplitudes" -> "PrepareDiagramAmplitudes\
[items, model, options] Prepare the same input formats with the same options \
as PrepareAmplitudes.\nUse the compatibility name for PrepareAmplitudes.",
 "ConstructiveDiagrams`CompleteFactorization" -> "CompleteFactorization[ampli\
tudes, channels, model] Fit corrections against all known physical channels \
simultaneously.\nCompleteFactorization[preparationReport, channelsOrReport, \
model] Start from prepared inputs while retaining discarded expressions and \
original residue targets.\nCompleteFactorization[input, channels, model, \
\"ReturnDetails\" -> True] Keep the candidate, affine freedom, correction \
directions and validation diagnostics.\nRepair physical residue deficits with \
a bounded simultaneous correction fit.",
 "ConstructiveDiagrams`CompleteHighEnergy" -> "CompleteHighEnergy[input, \
channels, model] Constrain a completed amplitude and its retained correction \
freedom.\nCompleteHighEnergy[input, channels, model, \"Output\" -> \
\"Details\"] Inspect the fit equations, affine family and independent \
validation.\nFit retained residue-preserving freedom to high-energy, \
spurious-pole and reference constraints.",
 "ConstructiveDiagrams`ConstructAmplitude" -> "ConstructAmplitude[process, \
diagramModel, factorizationModel] Run the full workflow and return the \
validated bare expression.\nConstructAmplitude[process, diagramModel, \
factorizationModel, \"Output\" -> \"Amplitude\"] Retain the external mass \
rules with the expression.\nConstructAmplitude[process, diagramModel, \
factorizationModel, \"Output\" -> \"Details\"] Retain stage outputs, timings, \
checks and the chosen run seed.\nConstruct, complete, simplify and validate \
an amplitude from its process and two models.",
 "ConstructiveDiagrams`ReadVertices" -> "ReadVertices Reserved compatibility \
symbol; there is no callable implementation on the current factorization \
branch.\nReserved historical vertex-reader name.",
 "ConstructiveDiagrams`ConvertVertex" -> "ConvertVertex Reserved \
compatibility symbol; there is no callable implementation on the current \
factorization branch.\nReserved historical vertex-converter name.",
 "ConstructiveDiagrams`CompareAmpToSquaredAmp" -> "CompareAmpToSquaredAmp \
Reserved compatibility symbol; there is no callable implementation on the \
current factorization branch.\nReserved historical amplitude-comparison \
name.", "ConstructiveDiagrams`DeleteRandomConstructiveAmplitude" -> "DeleteRa\
ndomConstructiveAmplitude Reserved compatibility symbol; there is no callable \
implementation on the current factorization branch.\nReserved historical \
random amplitude-deletion name.",
 "ConstructiveDiagrams`CheckLengthConstructive" -> "CheckLengthConstructive \
Reserved compatibility symbol; there is no callable implementation on the \
current factorization branch.\nReserved historical random-search length-check \
name.", "ConstructiveDiagrams`CheckNumberOfSpinors" -> "CheckNumberOfSpinors \
Reserved compatibility symbol; there is no callable implementation on the \
current factorization branch.\nReserved historical random-search spinor-count \
name.", "ConstructiveDiagrams`CreateRandomConstructiveSpinorProduct" -> "Crea\
teRandomConstructiveSpinorProduct Reserved compatibility symbol; there is no \
callable implementation on the current factorization branch.\nReserved \
historical random spinor-product generator name.",
 "ConstructiveDiagrams`CreateRandomMassFactor" -> "CreateRandomMassFactor \
Reserved compatibility symbol; there is no callable implementation on the \
current factorization branch.\nReserved historical random mass-factor \
generator name.",
 "ConstructiveDiagrams`CreateRandomConstructiveAmplitudeTerm" -> "CreateRando\
mConstructiveAmplitudeTerm Reserved compatibility symbol; there is no \
callable implementation on the current factorization branch.\nReserved \
historical random term-generator name.",
 "ConstructiveDiagrams`AddRandomConstructiveAmplitudeTerm" -> "AddRandomConst\
ructiveAmplitudeTerm Reserved compatibility symbol; there is no callable \
implementation on the current factorization branch.\nReserved historical \
random term-insertion name.",
 "ConstructiveDiagrams`CreateInitialRandomPopulationConstructiveAmplitudes" \
-> "CreateInitialRandomPopulationConstructiveAmplitudes Reserved \
compatibility symbol; there is no callable implementation on the current \
factorization branch.\nReserved historical population-search initializer \
name.", "ConstructiveDiagrams`RandomDeleteSpinorProduct" -> "RandomDeleteSpin\
orProduct Reserved compatibility symbol; there is no callable implementation \
on the current factorization branch.\nReserved historical random \
spinor-deletion name.", "ConstructiveDiagrams`RandomAddSpinorProduct" -> "Ran\
domAddSpinorProduct Reserved compatibility symbol; there is no callable \
implementation on the current factorization branch.\nReserved historical \
random spinor-insertion name.",
 "ConstructiveDiagrams`RandomChangeCoefficientSpinorProduct" -> "RandomChange\
CoefficientSpinorProduct Reserved compatibility symbol; there is no callable \
implementation on the current factorization branch.\nReserved historical \
random coefficient-mutation name.",
 "ConstructiveDiagrams`MateConstructiveAmplitudes" -> "MateConstructiveAmplit\
udes Reserved compatibility symbol; there is no callable implementation on \
the current factorization branch.\nReserved historical amplitude-population \
mating name.", "ConstructiveDiagrams`MakeNRandomChanges" -> "MakeNRandomChang\
es Reserved compatibility symbol; there is no callable implementation on the \
current factorization branch.\nReserved historical random-mutation driver \
name.", "ConstructiveDiagrams`FindAmp" -> "FindAmp Reserved compatibility \
symbol; there is no callable implementation on the current factorization \
branch.\nReserved historical amplitude-search name.",
 "ConstructiveDiagrams`FeynmanModel" -> "FeynmanModel[particles, vertices, \
variables, functions] stores the four model tables used by the Feynman \
backend.\nRepresents a model specified by off-shell Feynman rules.",
 "ConstructiveDiagrams`DiracWord" -> "DiracWord[operator1, operator2, ...] \
represents an ordered product of Dirac matrices and slashed \
momenta.\nRepresents an ordered product of Dirac matrices and slashed \
momenta.", "ConstructiveDiagrams`DiracGamma" -> "DiracGamma[index] represents \
a Dirac gamma matrix carrying a Lorentz index.\nRepresents a Dirac gamma \
matrix carrying a Lorentz index.", "ConstructiveDiagrams`DiracGamma5" -> "Dir\
acGamma5 represents the chirality matrix in an ordered Dirac \
word.\nRepresents the chirality matrix in an ordered Dirac word.",
 "ConstructiveDiagrams`DiracSlash" -> "DiracSlash[Mom[label]] represents a \
momentum contracted with a Dirac gamma matrix.\nRepresents a momentum \
contracted with a Dirac gamma matrix.",
 "ConstructiveDiagrams`LorentzIndex" -> "LorentzIndex[slot] labels a Lorentz \
slot in a Feynman tensor.\nLabels a Lorentz slot in a Feynman tensor.",
 "ConstructiveDiagrams`LorentzMetric" -> "LorentzMetric[index1, index2] \
represents the Minkowski metric with two Lorentz indices.\nRepresents the \
Minkowski metric with two Lorentz indices.",
 "ConstructiveDiagrams`LorentzMomentum" -> "LorentzMomentum[momentum, index] \
represents a four-momentum carrying a Lorentz index.\nRepresents a \
four-momentum carrying a Lorentz index.",
 "ConstructiveDiagrams`LorentzScalarProduct" -> "LorentzScalarProduct[p, q] \
retains an off-shell scalar product in a Feynman rule.\nRetains an off-shell \
scalar product in a Feynman rule.", "ConstructiveDiagrams`FermionBar" -> "Fer\
mionBar[leg, mass, chirality] represents the left antifermion endpoint of an \
ordered Dirac chain.\nRepresents the left antifermion endpoint of an ordered \
Dirac chain.", "ConstructiveDiagrams`FermionKet" -> "FermionKet[leg, mass, \
chirality] represents the right fermion endpoint of an ordered Dirac \
chain.\nRepresents the right fermion endpoint of an ordered Dirac chain.",
 "ConstructiveDiagrams`DiracChain" -> "DiracChain[bar, word, ket] joins \
FermionBar and FermionKet endpoints through one DiracWord.\nRepresents an \
ordered external-fermion bilinear.",
 "ConstructiveDiagrams`PolarizationVector" -> "PolarizationVector[leg, index, \
mass, helicity] represents an external vector wavefunction in the Feynman \
backend.\nRepresents an external vector wavefunction in the Feynman backend."\
, "ConstructiveDiagrams`VectorSpinIndex" -> "VectorSpinIndex[leg, slot] \
labels one of the two symmetric little-group slots of a massive \
vector.\nLabels one of the two symmetric little-group slots of a massive \
vector.", "ConstructiveDiagrams`ColorIndex" -> "ColorIndex[leg] labels an \
external color state.\nLabels an external color state.",
 "ConstructiveDiagrams`ColorDelta" -> "ColorDelta[index1, index2] represents \
a contracted color Kronecker tensor.\nRepresents a contracted color Kronecker \
tensor.", "ConstructiveDiagrams`ColorGenerator" -> "ColorGenerator[antifundam\
ental, fundamental, adjoint] represents a fundamental SU(3) \
generator.\nRepresents a fundamental SU(3) generator.",
 "ConstructiveDiagrams`ColorStructure" -> "ColorStructure[a, b, c] represents \
an SU(3) structure constant.\nRepresents an SU(3) structure constant.",
 "ConstructiveDiagrams`FeynmanToSpinorProducts" -> "FeynmanToSpinorProducts[e\
xpression] converts a contracted Feynman tensor \
expression.\nFeynmanToSpinorProducts[{massRules, expression}] preserves the \
external mass-rule pair.\nFeynmanToSpinorProducts[amplitudes] converts a list \
of amplitude pairs.\nConverts contracted Feynman amplitudes to spinor \
products without imposing internal mass shells.",
 "ConstructiveDiagrams`WriteSPINASProcess" -> "WriteSPINASProcess[directory, \
particles, amplitude, model] exports one all-incoming amplitude at its fixed \
massless helicities.\nWriteSPINASProcess[directory, entries, model] combines \
helicity-related {particles, amplitude} entries into one \
class.\nWriteSPINASProcess[directory, incoming -> outgoing, entries, model] \
declares the physical process and crosses its outgoing legs \
automatically.\nWrites a complete, readable C++ process using SPINAS, with \
optional CalcHEP and MadGraph validation and numerical integration.",
 "ConstructiveDiagrams`ValidateMadGraph" -> "ValidateMadGraph[directory] \
builds and runs the MadGraph validator of a SPINAS project exported by the \
current WriteSPINASProcess.\nValidateMadGraph[incoming -> outgoing, entries, \
model] exports and validates the supplied all-incoming amplitude entries for \
the declared physical process.\nValidateMadGraph[particles, amplitude, model] \
validates one all-incoming amplitude at its specified massless helicities, \
with IncomingLegs selecting the physical incoming \
pair.\nValidateMadGraph[input, ..., \"Output\" -> \"Details\"] returns the \
full report, including per-helicity failures and the actual random \
seed.\nCompares exported HEPCAT/SPINAS amplitudes with independent MadGraph \
matrix elements on common random phase-space points, separately for each \
massless-helicity combination."|>