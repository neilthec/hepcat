(* Polynomial factors hidden by ordinary external Dirac/Schouten reductions. *)
root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Global`$HEPCATpath = FileNameJoin[{root, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
$HistoryLength = 0;
checks = 0; failures = {};
SetAttributes[check, HoldRest];
check[name_, value_] := (checks++; If[!TrueQ[value], AppendTo[failures, name]; Print["FAIL: ", name]]);

(* The two massive labels and their masses are independent. The two Weyl
   labels have no relation to one another or to a momentum-conservation sum. *)
rfFixture[labels : {a_Integer, b_Integer, j_Integer, k_Integer}, {ma_, mb_}] := Module[
 {ang, sq, B, C, D, U, V, E, X, H, W, F, G, Z1, Z2, x, y, n, factored, relations, epsilon, adj,
  p11, p12, p21, q11, q12, q21, p, q, za, zb, ja, jb, ka, kb, ks1, ks2,
  basisA, basisB, angles, squares, chart},
 ang[i_] := Spinor[If[MemberQ[{a, b}, i], "Spin", "Helicity"], "Angle", i];
 sq[i_] := Spinor[If[MemberQ[{a, b}, i], "Spin", "Helicity"], "Square", i];
 B = SpinorChain[ang[k], ang[a]]; C = SpinorChain[ang[k], ang[b]];
 D = SpinorChain[ang[a], ang[b]];
 U = SpinorChain[ang[k], Mom[a], sq[j]]; V = SpinorChain[ang[k], Mom[b], sq[j]];
 E = SpinorChain[ang[k], Mom[a], Mom[b], ang[k]];
 X = SpinorChain[sq[j], Mom[a], Mom[b], sq[j]]; H = SpinorChain[sq[j], sq[k]];
 W = SpinorChain[ang[k], Mom[a], sq[b]];
 F = SpinorChain[sq[j], sq[a]]; G = SpinorChain[sq[j], sq[b]];
 Z1 = SpinorChain[ang[b], Mom[a], sq[j]]; Z2 = SpinorChain[ang[a], Mom[b], sq[j]];
 x = 2 MomProd[a, b]; y = 2 MomProd[a, k];
 n = D (mb^2 U^2 + ma^2 V^2) + x ma C V F - x mb B U G -
   (x + y) B C U H - mb B W U H + ma^2 B C V H + ma C E F H + (x + y) B C X;
 factored = (y B C + E D) (X - H U);
 relations = {ma C F + D U + B Z1, mb B G - D V + C Z2,
   -B Z1 V + C Z2 U - D U V + B C X,
   E Z1 + mb W U - C (ma^2 V - x U),
   E X - ma^2 V^2 - mb^2 U^2 + x U V};
 epsilon = {{0, 1}, {-1, 0}};
 adj[value_] := {{value[[2, 2]], -value[[1, 2]]}, {-value[[2, 1]], value[[1, 1]]}};
 p = {{p11, p12}, {p21, (ma^2 + p12 p21)/p11}};
 q = {{q11, q12}, {q21, (mb^2 + q12 q21)/q11}};
 basisA = {{ma, 0}, {0, 1}}; basisB = {{mb, 0}, {0, 1}};
 angles = AssociationThread[labels, {basisA.{1, za}, basisB.{1, zb}, {ja, jb}, {ka, kb}}];
 squares = AssociationThread[labels, {epsilon.adj[p].basisA.{1, za}/ma,
   epsilon.adj[q].basisB.{1, zb}/mb, {0, 1}, {ks1, ks2}}];
 chart = <|"Momenta" -> AssociationThread[labels, {p, q,
     Outer[Times, angles[j], squares[j]], Outer[Times, angles[k], squares[k]]}],
   "Angle" -> angles, "Square" -> squares,
   "Masses" -> AssociationThread[labels, {ma, mb, 0, 0}],
   "AngleBasis" -> AssociationThread[{a, b}, {basisA, basisB}],
   "SquareBasis" -> AssociationThread[{a, b},
     {epsilon.adj[p].basisA/ma, epsilon.adj[q].basisB/mb}],
   "SpinVariables" -> {za, zb}, "PoleVariable" -> Unique["unusedFixturePole"],
   "ReferenceSeed" -> 0, "CoherentVectorSpinIndices" -> True|>;
 <|"Original" -> Expand[n], "Factored" -> factored, "Chart" -> chart,
   "MassRules" -> Thread[(Mass /@ labels) -> {ma, mb, 0, 0}],
   "Relations" -> relations,
   "FormalWitness" -> Expand[n - factored - ((x V + E H) relations[[1]] -
     x U relations[[2]] + x relations[[3]] - B H relations[[4]] - D relations[[5]])]|>
];
rfForward[expr_, rules_] := Expand[ConstructiveDiagrams`Private`canonicalChains[
 First[ConstructiveDiagrams`Private`rspProductReductionCandidates[Expand[expr], False]] /. rules]];
rfReplay[expr_, route_] := Fold[Factor[Expand[ConstructiveDiagrams`Private`canonicalChains[
  #1 - #2["NormalizedBlock"] + #2["Cofactor"] #2["Product"]]]] &, expr, route];
rfMatrixZero[expr_, fixture_] := Cancel[Together[ConstructiveDiagrams`Private`spuriousEvaluateCurve[
  expr, fixture["Chart"], {}]]] === 0;

rfFixtures = {
 rfFixture[{1, 2, 3, 6}, {rfMass, rfMass}],
 rfFixture[{7, 11, 4, 19}, {rfMassA, rfMassB}],
 rfFixture[{23, 5, 17, 2}, {ModelParameter["ma", "reverse-factor-a", "Real"],
   ModelParameter["mb", "reverse-factor-b", "Real"]}]
};
Do[
 rfResult = ConstructiveDiagrams`Private`rspReverseNormalizationFactor[
   fixture["Original"], fixture["MassRules"], 5];
 check["general factor recovery " <> ToString[index],
   rfResult["Status"] === "IMPROVED" && First[rfResult["Rank"]] <= 52 &&
   Length[rfResult["Route"]] <= 3 && rfResult["ForwardNormalizationCertificate"] &&
   !rfResult["InverseKinematicsIntroduced"] && rfResult["InternalOnShellConditions"] === {}];
 check["exact forward normalization and replay " <> ToString[index],
   rfForward[rfResult["Amplitude"], fixture["MassRules"]] ===
     rfForward[fixture["Original"], fixture["MassRules"]] &&
   rfReplay[fixture["Original"], rfResult["Route"]] === rfResult["Amplitude"] &&
   AllTrue[rfResult["Route"], #["NormalizedBlock"] ===
     rfForward[#["Cofactor"] #["Product"], fixture["MassRules"]] &]];
 check["ordinary derivation has an exact independent polynomial witness " <> ToString[index],
   fixture["FormalWitness"] === 0 && AllTrue[fixture["Relations"], rfMatrixZero[#, fixture] &]];
 rfOnce = ReduceSpinorProducts[fixture["Factored"]];
 check["factored polynomial is retained and idempotent " <> ToString[index],
   SpinorExpressionComplexity[rfOnce] <= 52 && ReduceSpinorProducts[rfOnce] === rfOnce],
 {index, Length[rfFixtures]}, {fixture, {rfFixtures[[index]]}}];

rfBase = First[rfFixtures];
rfBasisFlagBefore = OwnValues[ConstructiveDiagrams`Private`$rspCanonicalProductBasis];
rfCanonicalPolynomial = Block[{ConstructiveDiagrams`Private`$rspCanonicalProductBasis = True},
 ReduceSpinorProducts[rfBase["Factored"]]];
check["a private coordinate-basis scope requests forward canonical product reduction",
 SpinorExpressionComplexity[rfCanonicalPolynomial] === 103 &&
 rfForward[rfCanonicalPolynomial, rfBase["MassRules"]] ===
   rfForward[rfBase["Original"], rfBase["MassRules"]]];
check["the private basis scope does not leak into ordinary polynomial preparation",
 OwnValues[ConstructiveDiagrams`Private`$rspCanonicalProductBasis] === rfBasisFlagBefore &&
 SpinorExpressionComplexity[ReduceSpinorProducts[rfBase["Factored"]]] <= 52 &&
 OwnValues[ConstructiveDiagrams`Private`$rspCanonicalProductBasis] === rfBasisFlagBefore];
rfStopped = ConstructiveDiagrams`Private`rspReverseNormalizationFactor[
 rfBase["Original"], rfBase["MassRules"], 0];
check["zero budget retains the complete original checkpoint",
 rfStopped["Amplitude"] === rfBase["Original"] && rfStopped["Route"] === {} &&
 rfStopped["ForwardNormalizationCertificate"] && rfStopped["Generated"] === 0];
rfInterrupted = ConstructiveDiagrams`Private`rspReverseNormalizationFactor[
 rfBase["Original"], rfBase["MassRules"], 0.001];
check["an interrupted search retains its complete expression route and certificate",
 rfInterrupted["ForwardNormalizationCertificate"] &&
 rfReplay[rfBase["Original"], rfInterrupted["Route"]] === rfInterrupted["Amplitude"] &&
 rfForward[rfInterrupted["Amplitude"], rfBase["MassRules"]] ===
   rfForward[rfBase["Original"], rfBase["MassRules"]]];
rfLimited = ConstructiveDiagrams`Private`rspReverseNormalizationFactor[
 rfBase["Original"], rfBase["MassRules"], 5, 1];
check["a bounded product catalog retains only exact completed improvements",
 rfLimited["PairCount"] <= 1 && rfLimited["ForwardNormalizationCertificate"] &&
 rfForward[rfLimited["Amplitude"], rfBase["MassRules"]] ===
   rfForward[rfBase["Original"], rfBase["MassRules"]]];
rfInternalRule = Mass[Multiparticle[1, 3]] -> rfMass;
rfExternalOnly = ConstructiveDiagrams`Private`rspReverseNormalizationFactor[
 rfBase["Original"], Append[rfBase["MassRules"], rfInternalRule], 5];
check["only explicit external mass data enter the forward identity certificate",
 rfExternalOnly["ExternalMassRules"] === rfBase["MassRules"] &&
 rfExternalOnly["InternalOnShellConditions"] === {}];
rfInverse = rfBase["Original"]/SpinorChain[Spinor["Helicity", "Angle", 3], Spinor["Helicity", "Angle", 6]];
check["inverse spinor factors are outside polynomial reverse lifting",
 ConstructiveDiagrams`Private`rspReverseNormalizationFactor[rfInverse,
   rfBase["MassRules"], 5]["Amplitude"] === rfInverse];
rfPropagator = PropDen[Mom[Multiparticle[1, 3]], rfMass];
check["the numerator helper does not rewrite physical propagators",
 ConstructiveDiagrams`Private`rspReverseNormalizationFactor[rfBase["Original"]/rfPropagator,
   rfBase["MassRules"], 5]["Amplitude"] === rfBase["Original"]/rfPropagator];

rfHuge = (SpinorChain[Spinor["Helicity", "Angle", 3], Spinor["Helicity", "Angle", 6]] +
  SpinorChain[Spinor["Helicity", "Angle", 3], Spinor["Helicity", "Angle", 4]])^1000000;
rfHugeResult = TimeConstrained[ConstructiveDiagrams`Private`rspReverseNormalizationFactor[
  rfHuge, rfBase["MassRules"], 1], 2, $TimedOut];
check["huge factored powers are guarded before expansion-based ranking",
 AssociationQ[rfHugeResult] && rfHugeResult["Amplitude"] === rfHuge &&
 rfHugeResult["Generated"] === 0 && rfHugeResult["RankMethod"] === "StructuralFallback"];
rfConflicting = ConstructiveDiagrams`Private`rspReverseNormalizationFactor[
 rfBase["Original"], Append[rfBase["MassRules"], Mass[1] -> rfOtherMass], 5];
check["conflicting external masses skip factor lifting rather than selecting a value",
 rfConflicting["Amplitude"] === rfBase["Original"] && !rfConflicting["MassRulesValid"] &&
 rfConflicting["Route"] === {}];
rfNonscalar = ConstructiveDiagrams`Private`rspReverseNormalizationFactor[
 rfBase["Original"], Append[Rest[rfBase["MassRules"]], Mass[1] -> {rfMass, rfOtherMass}], 5];
check["nonscalar external mass values cannot enter a certificate",
 rfNonscalar["Amplitude"] === rfBase["Original"] && !rfNonscalar["MassRulesValid"]];
rfDuplicate = ConstructiveDiagrams`Private`rspReverseNormalizationFactor[
 rfBase["Original"], Append[rfBase["MassRules"], First[rfBase["MassRules"]]], 0];
check["identical repeated external mass entries are unambiguous",
 rfDuplicate["MassRulesValid"] && rfDuplicate["ExternalMassRules"] === rfBase["MassRules"]];

rfUnknownMass = ConstructiveDiagrams`Private`rspReverseNormalizationFactor[
 rfBase["Original"], Append[Rest[rfBase["MassRules"]], Mass[1] -> Missing["UnknownMass"]], 1];
check["unknown mass markers are not scalar external mass definitions",
 !rfUnknownMass["MassRulesValid"] && rfUnknownMass["Amplitude"] === rfBase["Original"] &&
 rfUnknownMass["Route"] === {}];
rfFailedInput = ConstructiveDiagrams`Private`rspReverseNormalizationFactor[$TimedOut,
 rfBase["MassRules"], 1];
check["an unfinished input is retained without a fabricated reduction",
 rfFailedInput["Amplitude"] === $TimedOut && rfFailedInput["Generated"] === 0 &&
 rfFailedInput["Route"] === {}];

rfA = Spinor["Spin", "Angle", 1]; rfJ = Spinor["Helicity", "Square", 3];
rfDirac = (SpinorChain[rfA, Mom[1], rfJ] + SpinorChain[rfA, Mom[2], rfJ])^2;
rfDiracReduced = ReduceSpinorProducts[rfDirac];
check["retaining a polynomial factor cannot retain a reducible endpoint momentum",
 FreeQ[rfDiracReduced, SpinorChain[rfA, Mom[1], rfJ]] &&
 rfForward[rfDiracReduced - rfDirac, {}] === 0 &&
 ReduceSpinorProducts[rfDiracReduced] === rfDiracReduced];
rfClifford = (SpinorChain[Spinor["Helicity", "Angle", 3], Mom[4], Mom[4],
  Spinor["Helicity", "Angle", 6]] + SpinorChain[Spinor["Helicity", "Angle", 3],
  Spinor["Helicity", "Angle", 6]])^2;
check["retaining a polynomial factor cannot retain adjacent equal momenta",
 FreeQ[ReduceSpinorProducts[rfClifford], SpinorChain[_, Mom[4], Mom[4], _]] &&
 rfForward[ReduceSpinorProducts[rfClifford] - rfClifford, {}] === 0];

rfLinear = (SpinorChain[Spinor["Helicity", "Angle", 3], 0,
  Spinor["Helicity", "Square", 6]] + SpinorChain[Spinor["Helicity", "Angle", 3],
  2 Mom[4] + 3 Mom[5], Spinor["Helicity", "Square", 6]])^2;
rfLinearReduced = ReduceSpinorProducts[rfLinear];
check["retained polynomial alternatives still normalize small scalar sum and zero slots",
 FreeQ[rfLinearReduced, SpinorChain[___, _Plus | _Times | 0, ___]] &&
 rfForward[rfLinearReduced - rfLinear, {}] === 0 &&
 ReduceSpinorProducts[rfLinearReduced] === rfLinearReduced];

(* These determinant/Schouten checks use unrestricted symbolic matrices,
   independent of either on-shell spinor implementation or factor search. *)
rfEpsilon = {{0, 1}, {-1, 0}};
rfAdj[p_] := {{p[[2, 2]], -p[[1, 2]]}, {-p[[2, 1]], p[[1, 1]]}};
rfP = {{rfP11, rfP12}, {rfP21, rfP22}}; rfQ = {{rfQ11, rfQ12}, {rfQ21, rfQ22}};
rfR = {rfR1, rfR2}; rfS = {rfS1, rfS2}; rfK = {rfK1, rfK2};
rfU = rfR.rfEpsilon.rfP.rfS; rfV = rfR.rfEpsilon.rfQ.rfS;
rfE = rfR.rfEpsilon.rfP.rfAdj[rfQ].rfR;
rfX = rfS.(-rfEpsilon).rfAdj[rfP].rfQ.rfS;
rfH = rfS.(-rfEpsilon).rfK; rfCP = rfR.rfEpsilon.rfP.rfK; rfCQ = rfR.rfEpsilon.rfQ.rfK;
rfSX = rfS.(-rfEpsilon).rfAdj[rfQ].rfP.rfK;
check["unrestricted determinant identity",
 Expand[rfE rfX - Det[rfP] rfV^2 - Det[rfQ] rfU^2 + Tr[rfP.rfAdj[rfQ]] rfU rfV] === 0];
check["unrestricted Weyl Schouten identity", Expand[rfH rfE - rfCP rfV + rfCQ rfU] === 0];
check["unrestricted Clifford Schouten identity", Expand[rfSX rfU - Det[rfP] rfH rfV + rfCP rfX] === 0];

If[checks =!= 32, AppendTo[failures, "Not every reverse-normalization assertion executed."]];
Print[<|"Checks" -> checks, "Failed" -> Length[failures], "Failures" -> failures|>];
If[failures === {}, Print["HEPCAT_TEST_COMPLETED:reverse-normalization-tests.wl"]];
Quit[If[failures === {}, 0, 1]];
