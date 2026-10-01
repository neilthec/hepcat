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
check["legacy core encoding fingerprint preserved", Unscrambling`Private`$compactSourceFingerprint ===
  "cf001b379916e10aab582c97e2f8d08d566466163658a4009b4998de57b483a7"];
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
  If[Lookup[legacy, "DescriptorEncoding", None] === "CompactStructuredActions",
    check["existing compact metadata still accepted", Unscrambling`Private`compactModelCompatibleQ[legacy]]]];
Print["Particle compact checks: ", checks, "; failures: ", failures];
Quit[If[failures === {}, 0, 1]];
