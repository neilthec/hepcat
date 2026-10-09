(* Higher-point compact edits retain every particle and ordered momentum. *)
root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Global`$HEPCATpath = FileNameJoin[{root, "source"}];
Block[{Print}, Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
  Get[FileNameJoin[{root, "tests", "unscrambling.wl"}]]];
checks = 0; failures = {};
check[name_, value_] := (checks++; If[!TrueQ[value], AppendTo[failures, name]]);
ang[i_, j_] := SpinorChain[Spinor["Spin", "Angle", i], Spinor["Spin", "Angle", j]];
rules = Table[Mass[i] -> (i + 1), {i, 5}];
amp = ang[1, 2] SpinorChain[Spinor["Spin", "Angle", 3], Mom[5], Mom[5], Spinor["Spin", "Angle", 4]];
candidates = UnscrambleCandidates[{rules, amp}];
features = Unscrambling`Private`compactParticleFeatures[amp, rules, candidates];
check["five-point edit tokens generated", Length[Dimensions[features]] === 3 &&
  First[Dimensions[features]] === Length[candidates] && Last[Dimensions[features]] === 166];
check["five-point tokens packed and finite", Developer`PackedArrayQ[features] &&
  AllTrue[Flatten[features], NumberQ[#] && TrueQ[Im[#] == 0 && Abs[#] < Infinity] &]];
chainIndex = First[Select[Range[Length[candidates]],
  candidates[[#]]["Name"] === "ChainSquare" &&
    Lookup[candidates[[#]], "Direction", None] === "Contract" &]];
chainTokens = Select[features[[chainIndex]], #[[149]] == 1. && #[[156]] == 1. &];
check["particle five appears in both repeated momentum tokens", Length[chainTokens] === 2 &&
  chainTokens[[All, 162]] === {5., 5.} && Sort[chainTokens[[All, 164]]] === {1., 2.}];
check["five-point repeated-momentum reduction is legal", Unscrambling`Private`samePoly[
  Unscrambling`Private`applyCandidate[amp, candidates[[chainIndex]]], 36 ang[1, 2] ang[3, 4]]];
ids = AssociationThread[Range[7], Range[7]];
Do[
  subset = If[members === {}, None, Multiparticle @@ members];
  tokens = Unscrambling`Private`compactParticleLegTokens[subset, "LegRole", 2, 0, 0, ids];
  check["exact subset members " <> ToString[members],
    tokens[[All, 162]] === N[If[members === {}, {0}, members]] &&
    tokens[[All, 163]] === ConstantArray[N[Length[members]], Max[1, Length[members]]]],
  {members, Subsets[Range[7]]}];
check["no external particle ceiling", Unscrambling`Private`compactParticleFeatureSchema["MaximumExternalLegs"] === Infinity];
Do[
  extendedRules = Table[Mass[i] -> (i + 1), {i, n}];
  extendedAmp = ang[1, 2] SpinorChain[Spinor["Spin", "Angle", 3], Mom[n], Mom[n], Spinor["Spin", "Angle", 4]];
  extendedCandidates = UnscrambleCandidates[{extendedRules, extendedAmp}];
  extendedFeatures = Unscrambling`Private`compactParticleFeatures[extendedAmp, extendedRules, extendedCandidates];
  check["descriptors support " <> ToString[n] <> " particles", Length[Dimensions[extendedFeatures]] === 3 &&
    Last[Dimensions[extendedFeatures]] === 166 && MemberQ[Flatten[extendedFeatures[[All, All, 162]]], N[n]]],
  {n, {6, 9}}];
sparse = AssociationThread[Range[5], {11, 21, 35, 49, 87}];
sparsePair = Unscrambling`Private`relabelLegs[{rules, amp}, sparse];
sparseCandidates = UnscrambleCandidates[sparsePair];
check["sparse particle names normalize identically", Unscrambling`Private`compactParticleFeatures[
  sparsePair[[2]], sparsePair[[1]], sparseCandidates] === features];
check["legal extension has a distinct runtime fingerprint", StringQ[Unscrambling`Private`$compactSourceFingerprint] &&
  Unscrambling`Private`$compactSourceFingerprint =!=
    "cf001b379916e10aab582c97e2f8d08d566466163658a4009b4998de57b483a7"];
sumAmp = ang[1, 2] SpinorChain[Spinor["Spin", "Angle", 3], Mom[Multiparticle[4, 5]],
  Spinor["Spin", "Square", 4]];
sumCandidates = UnscrambleCandidates[{rules, sumAmp}, "MomentumConservation" -> False];
sumFeatures = Unscrambling`Private`compactParticleFeatures[sumAmp, rules, sumCandidates];
check["new momentum edits have finite five-point descriptors", Length[Dimensions[sumFeatures]] === 3 &&
  First[Dimensions[sumFeatures]] === Length[sumCandidates] && Last[Dimensions[sumFeatures]] === 166 &&
  AllTrue[Flatten[sumFeatures], NumberQ[#] && TrueQ[Abs[#] < Infinity] &]];
check["composite splitting offered without conservation", AnyTrue[sumCandidates,
  #["Name"] === "MomentumSum" && Lookup[#, "Identity", None] === "Split" &]];
sumIndex = First[Select[Range[Length[sumCandidates]], sumCandidates[[#]]["Name"] === "MomentumSum" &]];
sumTokens = Select[sumFeatures[[sumIndex]], #[[149]] == 1. && #[[156]] == 1. &];
check["composite descriptor retains every subset member", Sort[sumTokens[[All, 162]]] === {4., 5.}];
sumAfter = Unscrambling`Private`applyCandidate[sumAmp, sumCandidates[[sumIndex]]];
collectCandidates = UnscrambleCandidates[{rules, sumAfter}, "MomentumConservation" -> False];
collectFeatures = Unscrambling`Private`compactParticleFeatures[sumAfter, rules, collectCandidates];
check["collection descriptors are supported", Length[Dimensions[collectFeatures]] === 3 &&
  Last[Dimensions[collectFeatures]] === 166];
fourRules = Take[rules, 4];
fourAmp = SpinorChain[Spinor["Spin", "Angle", 1], Mom[Multiparticle[2, 3]], Spinor["Spin", "Square", 4]];
fourCandidates = UnscrambleCandidates[{fourRules, fourAmp}, "MomentumConservation" -> False];
check["legacy compact descriptor width retained for new edits", Dimensions[
  Unscrambling`Private`compactActionFeatures[fourAmp, fourRules, fourCandidates]] === {Length[fourCandidates], 146}];

(* Normalization and new legal identities share both compact encodings. *)
Do[
  zeroBracket = SpinorChain[endpoint, endpoint];
  check["identical endpoint is zero " <> ToString[endpoint, InputForm],
    Unscrambling`Private`canonicalChains[zeroBracket] === 0],
  {endpoint, {Spinor["Helicity", "Angle", 1], Spinor["Spin", "Square", 1],
    Spinor["Spin", "Angle", 1, 2], Spinor["Spin", 2, "Angle", 1]}}];
indexedBracket = SpinorChain[Spinor["Spin", "Angle", 1, 1], Spinor["Spin", "Angle", 1, 2]];
check["distinct little-group indices are not zero", Unscrambling`Private`canonicalChains[indexedBracket] =!= 0];
check["Schouten normalizes newly identical endpoints", Unscrambling`Private`samePoly[
  Unscrambling`Private`canonicalChains[SchoutenRewrite[ang[1, 2], ang[1, 3], {0, 0}]],
  ang[1, 2] ang[1, 3]]];
protectedDenominator = PropDen[Mom[Multiparticle[1, 2]], Mass[1],
  ang[1, 1], SpinorTrace[Mom[1], Mom[1]], MomProd[1, 2]];
check["normalization never descends into PropDen", Unscrambling`Private`canonicalAmplitude[
  protectedDenominator, rules] === protectedDenominator];
check["empty chiral trace is two", Unscrambling`Private`canonicalChains[SpinorTrace[]] === 2];
oddTrace = SpinorTrace[Mom[1], Mom[2], Mom[3]];
check["odd trace is left unsupported", Unscrambling`Private`canonicalChains[oddTrace] === oddTrace &&
  UnscrambleCandidates[{rules, oddTrace}, "MomentumConservation" -> False] ===
    {Unscrambling`Private`stopMove[]}];
check["explicit trace momentum slots distribute", Unscrambling`Private`samePoly[
  Unscrambling`Private`canonicalChains[SpinorTrace[2 Mom[1] - Mom[2], Mom[3]]],
  2 SpinorTrace[Mom[1], Mom[3]] - SpinorTrace[Mom[2], Mom[3]]]];
tracePair = SpinorTrace[Mom[1], Mom[2]];
tracePairCandidates = UnscrambleCandidates[{rules, tracePair}, "MomentumConservation" -> False];
check["trace pair contracts to twice its scalar product", AnyTrue[tracePairCandidates,
  Lookup[#, "Identity", None] === "TracePair" && Unscrambling`Private`samePoly[
    Unscrambling`Private`applyCandidate[tracePair, #], 2 MomProd[1, 2]] &]];
check["scalar product has a reverse trace label", AnyTrue[
  UnscrambleCandidates[{rules, 2 MomProd[1, 2]}, "MomentumConservation" -> False],
  Lookup[#, "Identity", None] === "TracePair" && Unscrambling`Private`samePoly[
    Unscrambling`Private`applyCandidate[2 MomProd[1, 2], #], tracePair] &]];
traceSum = SpinorTrace[Mom[Multiparticle[1, 2]], Mom[3]];
traceSumCandidates = UnscrambleCandidates[{rules, traceSum}, "MomentumConservation" -> False];
traceSplit = SelectFirst[traceSumCandidates, Lookup[#, "Identity", None] === "Split" &, Missing["Split"]];
traceAfterSplit = If[AssociationQ[traceSplit], Unscrambling`Private`applyCandidate[traceSum, traceSplit], $Failed];
check["composite trace split has an exact reverse", traceAfterSplit =!= $Failed && AnyTrue[
  UnscrambleCandidates[{rules, traceAfterSplit}, "MomentumConservation" -> False],
  Lookup[#, "Identity", None] === "Collect" && Unscrambling`Private`samePoly[
    Unscrambling`Private`applyCandidate[traceAfterSplit, #], traceSum] &]];
traceOverlap = SpinorTrace[Mom[Multiparticle[1, 2]], Mom[Multiparticle[1, 3]], Mom[4], Mom[5]];
traceOverlapCandidates = UnscrambleCandidates[{rules, traceOverlap}, "MomentumConservation" -> False];
check["overlapping composite trace contractions apply numerator masses", AllTrue[
  Select[traceOverlapCandidates, #["Name"] === "Anticommutation" &], FreeQ[#["Insert"], _Mass] &]];
subsetRules = Append[rules, Mass[Multiparticle[1, 2, 3]] -> 9];
traceSubset = SpinorTrace[Mom[Multiparticle[1, 2, 3]], Mom[Multiparticle[1, 2, 3]], Mom[4], Mom[5]];
traceSubsetCandidates = UnscrambleCandidates[{subsetRules, traceSubset}, "MomentumConservation" -> False];
check["three-leg on-shell trace square contracts", AnyTrue[traceSubsetCandidates,
  Lookup[#, "Identity", None] === "TraceSquare" && Lookup[#, "Direction", None] === "Contract" &&
    Unscrambling`Private`samePoly[Unscrambling`Private`applyCandidate[traceSubset, #],
      81 SpinorTrace[Mom[4], Mom[5]]] &]];
traceOffShell = UnscrambleCandidates[{subsetRules, traceSubset}, "MomentumConservation" -> False,
  "OnShellChannels" -> {}];
check["three-leg trace square respects channel filter", AnyTrue[traceOffShell,
  Lookup[#, "Identity", None] === "TraceSquare" && Unscrambling`Private`samePoly[
    Unscrambling`Private`applyCandidate[traceSubset, #],
    Mom[Multiparticle[1, 2, 3]]^2 SpinorTrace[Mom[4], Mom[5]]] &]];
Do[
  normalized = Unscrambling`Private`canonicalAmplitude[board, subsetRules];
  boardCandidates = UnscrambleCandidates[{subsetRules, board}];
  particleDescriptors = Unscrambling`Private`compactParticleFeatures[normalized, subsetRules, boardCandidates];
  check["finite trace/scalar particle descriptors " <> ToString[board, InputForm],
    Length[Dimensions[particleDescriptors]] === 3 && Last[Dimensions[particleDescriptors]] === 166 &&
    AllTrue[Flatten[particleDescriptors], NumberQ[#] && TrueQ[Im[#] == 0 && Abs[#] < Infinity] &]];
  If[FreeQ[board, Mom[5]],
    overviewRules = Append[fourRules, Mass[Multiparticle[1, 2, 3]] -> 9];
    overviewCandidates = UnscrambleCandidates[{overviewRules, board}];
    overviewDescriptors = Unscrambling`Private`compactActionFeatures[normalized, overviewRules, overviewCandidates];
    check["legacy four-point overview width retained " <> ToString[board, InputForm],
      Dimensions[overviewDescriptors] === {Length[overviewCandidates], 146} &&
      AllTrue[Flatten[overviewDescriptors], NumberQ[#] && TrueQ[Abs[#] < Infinity] &]]];
  check["new identities preserve opaque propagators " <> ToString[board, InputForm], AllTrue[
    UnscrambleCandidates[{subsetRules, board/protectedDenominator}],
    DeleteDuplicates[Cases[{Unscrambling`Private`applyCandidate[
      Unscrambling`Private`canonicalAmplitude[board/protectedDenominator, subsetRules], #]}, _PropDen, Infinity]] ===
        {protectedDenominator} &]],
  {board, {tracePair, traceSum, traceAfterSplit, traceOverlap, traceSubset,
    SpinorTrace[Mom[1], Mom[2], Mom[3], Mom[1]], MomProd[1, 2],
    Mom[Multiparticle[1, 2, 3]]^2}}];

BlockRandom[
  SeedRandom[51];
  portableArrays = <|"Embedding" -> RandomReal[{-1., 1.}, {256, 24}],
    "Conv1Weights" -> RandomReal[{-0.1, 0.1}, {64, 24, 5}],
    "Conv1Biases" -> RandomReal[{0.1, 0.2}, 64],
    "Conv2Weights" -> RandomReal[{-0.1, 0.1}, {64, 64, 5}],
    "Conv2Biases" -> RandomReal[{0.1, 0.2}, 64],
    "TokenWeights" -> RandomReal[{-0.1, 0.1}, {64, 166}],
    "TokenBiases" -> RandomReal[{0.1, 0.2}, 64],
    "TokenMaskWeights" -> Unscrambling`Private`compactParticleMaskWeights[],
    "TokenMaskBiases" -> ConstantArray[0., 64],
    "HiddenWeights" -> RandomReal[{-0.1, 0.1}, {64, 256}],
    "HiddenBiases" -> RandomReal[{0.1, 0.2}, 64],
    "OutputWeights" -> RandomReal[{-0.1, 0.1}, {1, 64}], "OutputBiases" -> {0.2}|>;
  portableCandidates = ConstantArray[0., {3, 4, 166}];
  Do[portableCandidates[[i, j, 147]] = 1.;
    portableCandidates[[i, j, 1 ;; 146]] = RandomReal[{-1., 1.}, 146], {i, 3}, {j, i}];
];
portableState = Range[30];
portableScores[tokens_] := Unscrambling`Private`compactParticlePolicyScores[portableArrays,
  <|"State" -> portableState, "Candidates" -> tokens|>];
baseScores = portableScores[portableCandidates];
check["portable particle scorer returns every action", VectorQ[baseScores, NumberQ] && Length[baseScores] === 3];
Do[
  extraScores = portableScores[pad[portableCandidates, {3, 9, 166}, 0.]];
  check["portable particle padding ignores nonzero biases " <> ToString[pad],
    VectorQ[extraScores, NumberQ] && Max[Abs[baseScores - extraScores]] < 1.*^-12],
  {pad, {PadLeft, PadRight}}];
extraScores = portableScores[Join[portableCandidates, {First[portableCandidates]}]];
check["portable action count does not affect existing scores", Length[extraScores] === 4 &&
  Max[Abs[baseScores - Take[extraScores, 3]]] < 1.*^-12 &&
  Abs[First[extraScores] - Last[extraScores]] < 1.*^-12];
singleScores = portableScores[Take[portableCandidates, 1]];
check["portable scorer supports one action", Length[singleScores] === 1 &&
  Abs[First[singleScores] - First[baseScores]] < 1.*^-12];
stateValues = portableArrays["Embedding"][[portableState]];
stateValues = Unscrambling`Private`compactPortableConvolution[stateValues,
  portableArrays["Conv1Weights"], portableArrays["Conv1Biases"]];
stateValues = Unscrambling`Private`compactPortableConvolution[stateValues,
  portableArrays["Conv2Weights"], portableArrays["Conv2Biases"]];
stateParts = {Mean[stateValues], Max /@ Transpose[stateValues]};
stateVector = Flatten[#/Sqrt[Total[#^2] + 1.*^-12] & /@ stateParts];
expectedPaddingScore = First[portableArrays["OutputWeights"]] . Ramp[
  portableArrays["HiddenWeights"][[All, 1 ;; 128]] . stateVector + portableArrays["HiddenBiases"]] +
  First[portableArrays["OutputBiases"]];
paddingScores = portableScores[ConstantArray[0., {1, 6, 166}]];
check["all padded tokens encode as zero despite token biases", VectorQ[paddingScores, NumberQ] &&
  Length[paddingScores] === 1 && Abs[First[paddingScores] - expectedPaddingScore] < 1.*^-12];
Do[
  invalidTokens = portableCandidates; invalidTokens[[1, 1, 147]] = presence;
  check["portable scorer rejects invalid presence " <> ToString[presence], portableScores[invalidTokens] === $Failed],
  {presence, {-1., 0.5, 2.}}];
check["portable scorer rejects wrong token width", portableScores[portableCandidates[[All, All, 1 ;; 165]]] === $Failed];
invalidTokens = portableCandidates; invalidTokens[[1, 1, 1]] = Infinity;
check["portable scorer rejects nonfinite tokens", portableScores[invalidTokens] === $Failed];
metadata = <|"Encoding" -> "StateUTF8AndNumericActions", "Architecture" -> "CompactParticleActions",
  "DescriptorEncoding" -> "ParticleRoleTokens", "DescriptorFeatureWidth" -> 166,
  "CompactFeatureSchema" -> Unscrambling`Private`compactParticleFeatureSchema,
  "Objective" -> "LegalMoveCrossEntropy", "ReadoutNormalization" -> "SeparateMeanMaxL2",
  "ReadoutSquaredNormEpsilon" -> 1.*^-12,
  "NativeTokenCount" -> 14, "RuntimeEvaluation" -> "PortableVariableParticleTokens",
  "RuntimeCoreSourceEncoding" -> "SectionedLegalMathAndParticleTokens",
  "RuntimeCoreSourceSHA256" -> Unscrambling`Private`$compactParticleSourceFingerprint|>;
check["particle metadata accepted", Unscrambling`Private`compactParticleModelCompatibleQ[metadata]];
check["known earlier particle model remains compatible", Unscrambling`Private`compactParticleModelCompatibleQ[
  Join[metadata, <|"RuntimeCoreSourceSHA256" ->
    "62ed48a7d3049905b07fd3cc4a28b2c05dc8d89d1a820440ca8e4bae4cde773a"|>]]];
check["particle metadata rejects old descriptor", !Unscrambling`Private`compactParticleModelCompatibleQ[
  Join[metadata, <|"DescriptorEncoding" -> "CompactStructuredActions"|>]]];
check["particle metadata rejects changed core", !Unscrambling`Private`compactParticleModelCompatibleQ[
  Join[metadata, <|"RuntimeCoreSourceSHA256" -> "NotTheRuntime"|>]]];
check["particle metadata rejects missing native tensor width", !Unscrambling`Private`compactParticleModelCompatibleQ[
  KeyDrop[metadata, "NativeTokenCount"]]];
check["particle metadata rejects a native-only runtime", !Unscrambling`Private`compactParticleModelCompatibleQ[
  Join[metadata, <|"RuntimeEvaluation" -> "FixedNativeTokens"|>]]];
If[FileExistsQ[FileNameJoin[{root, "tests", "unscramble-compact.m"}]],
  legacy = Get[FileNameJoin[{root, "tests", "unscramble-compact.m"}]];
  check["existing compact metadata still accepted", If[Lookup[legacy, "DescriptorEncoding", None] === "ParticleRoleTokens",
    Unscrambling`Private`compactParticleModelCompatibleQ[legacy], Unscrambling`Private`compactModelCompatibleQ[legacy]]]];
If[AllTrue[FileNameJoin[{root, "tests", #}] & /@ {"unscramble-compact.m", "unscramble-compact.wlnet"}, FileExistsQ],
  Block[{Unscrambling`Private`$policyMethod = "CompactActions"},
    installedLoaded = TimeConstrained[Unscrambling`Private`loadUnscrambleModel[
      FileNameJoin[{root, "tests"}]], 15, $Failed];
    check["installed compact weights load without retraining", TrueQ[installedLoaded]];
    If[TrueQ[installedLoaded] && Lookup[legacy, "DescriptorEncoding", None] === "ParticleRoleTokens",
      installedBoard = UnscrambleCandidates[{subsetRules, traceSubset}];
      installedScores = TimeConstrained[Unscrambling`Private`scoreCandidates[
        traceSubset, subsetRules, installedBoard], 15, $Failed];
      check["installed portable scorer accepts new legal trace features", VectorQ[installedScores, NumberQ] &&
        Length[installedScores] === Length[installedBoard] &&
        AllTrue[installedScores, TrueQ[Im[#] == 0 && Abs[#] < Infinity] &]]]]];

(* Independent Weyl matrices catch chirality errors that the legacy reducer shares. *)
Module[{traceMatrixVectors, traceMatrixVector, traceMatrixDot, traceMatrixSigma,
  traceMatrixTrace, traceMatrixValue, traceMatrixRules, traceMatrixExamples,
  traceMatrixBoards, traceMatrixFeatures, traceMatrixAudit, traceMatrixCyclic},
  traceMatrixVectors = <|1 -> {2, 1, 0, 0}, 2 -> {3, 0, 1, 0}, 3 -> {4, 0, 0, 1},
    4 -> {5, 1, 1, 0}, 5 -> {7, 2, 0, 1}|>;
  traceMatrixVector[i_Integer] := traceMatrixVectors[i];
  traceMatrixVector[Multiparticle[ii__Integer]] := Total[traceMatrixVector /@ {ii}];
  traceMatrixDot[a_, b_] := With[{v = traceMatrixVector[a], w = traceMatrixVector[b]},
    2 v[[1]] w[[1]] - v.w];
  traceMatrixSigma[leg_, sign_] := With[{v = traceMatrixVector[leg]},
    {{v[[1]] + sign v[[4]], sign (v[[2]] - I v[[3]])},
      {sign (v[[2]] + I v[[3]]), v[[1]] - sign v[[4]]}}];
  traceMatrixTrace[{}] := 2;
  traceMatrixTrace[legs_List] := Tr[Dot @@ MapIndexed[
    traceMatrixSigma[#1, (-1)^(First[#2] - 1)] &, legs]];
  traceMatrixValue[expr_] := Simplify[expr /. {
    t_SpinorTrace :> traceMatrixTrace[First /@ (List @@ t)],
    MomProd[a_, b_] :> traceMatrixDot[a, b],
    HoldPattern[Mom[leg_]^2] :> traceMatrixDot[leg, leg]}];
  traceMatrixRules = Join[Mass[#] -> Sqrt[traceMatrixDot[#, #]] & /@ Range[5],
    {Mass[Multiparticle[1, 2]] ->
      Sqrt[traceMatrixDot[Multiparticle[1, 2], Multiparticle[1, 2]]]}];
  traceMatrixExamples = {
    SpinorTrace[Mom[1], Mom[2], Mom[3], Mom[4], Mom[5], Mom[1]],
    SpinorTrace[Mom[1], Mom[1], Mom[2], Mom[3]],
    SpinorTrace[Mom[Multiparticle[1, 2]], Mom[3], Mom[4], Mom[5]],
    SpinorTrace[Mom[1], Mom[2]], MomProd[1, 2]};
  traceMatrixBoards = UnscrambleCandidates[{traceMatrixRules, #},
    "MomentumConservation" -> False] & /@ traceMatrixExamples;
  traceMatrixFeatures = MapThread[Unscrambling`Private`compactParticleFeatures[
    #1, traceMatrixRules, #2] &, {traceMatrixExamples, traceMatrixBoards}];
  check["trace boards retain finite particle descriptor width", AllTrue[traceMatrixFeatures,
    ArrayQ[#, 3, NumberQ] && Last[Dimensions[#]] === 166 &&
      AllTrue[Flatten[#], TrueQ[Im[#] == 0 && Abs[#] < Infinity] &] &]];
  traceMatrixAudit = Flatten[MapThread[Function[{start, board},
    Map[Function[move, If[MemberQ[
        {"ChainSquare", "Anticommutation", "MomentumSum", "MomentumSquare", "Stop"}, move["Name"]],
      {move["Name"], Lookup[move, "Identity", None], traceMatrixValue[
        start - Unscrambling`Private`applyCandidate[start, move]]}, Nothing]], board]],
    {traceMatrixExamples, traceMatrixBoards}], 1];
  check["trace independent audit covers broad legal boards", Length[traceMatrixAudit] >= 90 &&
    ContainsAll[traceMatrixAudit[[All, 2]],
      {"TraceSquare", "TraceAnticommutation", "TraceCyclicSquare", "TracePair", "Split", "Introduce"}]];
  check["trace identities agree exactly with independent matrices", AllTrue[traceMatrixAudit, Last[#] === 0 &]];
  traceMatrixCyclic = Select[First[traceMatrixBoards],
    Lookup[#, "Identity", None] === "TraceCyclicSquare" &];
  check["six-momentum boundary contraction retains chirality", Length[traceMatrixCyclic] === 1 &&
    traceMatrixValue[First[traceMatrixExamples]] === 2628 - 6 I &&
    traceMatrixValue[First[traceMatrixCyclic]["Insert"]] === 2628 - 6 I &&
    3 traceMatrixTrace[{2, 3, 4, 5}] === 2628 + 6 I];
];
Print["Particle compact checks: ", checks, "; failures: ", failures];
Quit[If[failures === {}, 0, 1]];
