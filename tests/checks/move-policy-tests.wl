(* Reverse-path supervision and state-only neural execution. *)
root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Global`$HEPCATpath = FileNameJoin[{root, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
Get[FileNameJoin[{root, "tests", "unscrambling.wl"}]];
checks = 0; failures = {};
check[name_, value_] := (checks++; If[!TrueQ[value], AppendTo[failures, name]]);
ang[i_, j_] := SpinorChain[Spinor["Helicity", "Angle", i], Spinor["Helicity", "Angle", j]];
rules = Table[Mass[i] -> 0, {i, 4}];
amp = ang[1, 2] ang[3, 4];
pair = {rules, amp};
dir = CreateDirectory[];
check["default named model is automatic", ("Model" /. Options[UnscrambleSpinorAmplitudes]) === Automatic &&
  ("Model" /. Options[UnscrambleTrace]) === Automatic];
mainOptions = Options[UnscrambleSpinorAmplitudes]; traceOptions = Options[UnscrambleTrace];
Block[{Unscrambling`Private`loadUnscrambleModel, loadedSelections = {}},
  Unscrambling`Private`loadUnscrambleModel[directory_] := (
    AppendTo[loadedSelections, {Unscrambling`Private`$policyMethod, directory}]; True);
  selected = UnscrambleTrace[pair, "MaxSteps" -> 0];
  check["automatic named model preserves candidate scorer policy", selected["PolicyMethod"] === "CandidateScorer"];
  selected = UnscrambleTrace[pair, "Model" -> "CompactActions", "MaxSteps" -> 0];
  check["named compact model selects compact policy", selected["PolicyMethod"] === "CompactActions"];
  selected = UnscrambleTrace[pair, "Model" -> "CandidateScorer", "PolicyMethod" -> "ReverseMoves", "MaxSteps" -> 0];
  check["named candidate scorer overrides legacy policy", selected["PolicyMethod"] === "CandidateScorer"];
  selected = UnscrambleTrace[pair, "Model" -> "Current", "MaxSteps" -> 0];
  check["old selector spelling remains compatible", selected["PolicyMethod"] === "CandidateScorer"];
  selected = UnscrambleTrace[pair, "Model" -> "CompactActions", "PolicyMethod" -> "CandidateScorer", "MaxSteps" -> 0];
  check["named compact model overrides scorer policy", selected["PolicyMethod"] === "CompactActions"];
  selected = UnscrambleTrace[pair, "Model" -> Automatic, "PolicyMethod" -> "ReverseMoves", "MaxSteps" -> 0];
  check["automatic preserves explicit reverse method", selected["PolicyMethod"] === "ReverseMoves"];
  selected = UnscrambleTrace[pair, "Model" -> "CompactActions", "ModelDirectory" -> dir, "MaxSteps" -> 0];
  check["named model preserves explicit directory", Last[loadedSelections] === {"CompactActions", ExpandFileName[dir]}];
  beforeInvalid = Length[loadedSelections];
  check["unknown named model rejected", Quiet[UnscrambleTrace[pair, "Model" -> "Unknown", "MaxSteps" -> 0]] === $Failed];
  check["unknown model does not load another net", Length[loadedSelections] === beforeInvalid];
  check["invalid model directory rejected", Quiet[UnscrambleTrace[pair, "ModelDirectory" -> 123, "MaxSteps" -> 0]] === $Failed];
  SetOptions[UnscrambleSpinorAmplitudes, "Model" -> "CompactActions"];
  SetOptions[UnscrambleTrace, "Model" -> "CandidateScorer"];
  selected = UnscrambleSpinorAmplitudes[pair, "MaxSteps" -> 0];
  check["main independent compact SetOptions", Last[loadedSelections][[1]] === "CompactActions" && MatchQ[selected, {_List, _}]];
  selected = UnscrambleTrace[pair, "MaxSteps" -> 0];
  check["trace independent candidate scorer SetOptions", Last[loadedSelections][[1]] === "CandidateScorer"];
  SetOptions[UnscrambleSpinorAmplitudes, "Model" -> "CandidateScorer"];
  SetOptions[UnscrambleTrace, "Model" -> "CompactActions"];
  selected = UnscrambleSpinorAmplitudes[pair, "MaxSteps" -> 0];
  check["main independent candidate scorer SetOptions", Last[loadedSelections][[1]] === "CandidateScorer" && MatchQ[selected, {_List, _}]];
  selected = UnscrambleTrace[pair, "MaxSteps" -> 0];
  check["trace independent compact SetOptions", Last[loadedSelections][[1]] === "CompactActions"];
];
SetOptions[UnscrambleSpinorAmplitudes, mainOptions]; SetOptions[UnscrambleTrace, traceOptions];
compactMetadata = <|"Encoding" -> "StateUTF8AndNumericActions", "Architecture" -> "CompactReverseMoveActions",
  "DescriptorEncoding" -> "CompactStructuredActions", "DescriptorFeatureWidth" -> 146,
  "CompactFeatureSchema" -> Unscrambling`Private`compactActionFeatureSchema,
  "Objective" -> "LegalMoveCrossEntropy", "ReadoutNormalization" -> "SeparateMeanMaxL2",
  "ReadoutSquaredNormEpsilon" -> 1.*^-12, "RuntimeCoreSourceEncoding" -> "SectionedLegalMathAndCompactActions",
  "RuntimeCoreSourceSHA256" -> Unscrambling`Private`$compactSourceFingerprint|>;
check["compatible compact metadata accepted", Unscrambling`Private`compactModelCompatibleQ[compactMetadata]];
Do[check["compact metadata rejects " <> First[replacement],
    !Unscrambling`Private`compactModelCompatibleQ[Join[compactMetadata, Association[replacement]]]],
  {replacement, {"DescriptorFeatureWidth" -> 145, "CompactFeatureSchema" -> <||>,
    "RuntimeCoreSourceSHA256" -> "WrongCoreHash", "Objective" -> "SquaredError",
    "ReadoutSquaredNormEpsilon" -> 0.}}];
compactFile = FileNameJoin[{root, "tests", "unscramble-compact.wlnet"}];
If[FileExistsQ[compactFile],
  compactMetadataFile = FileNameJoin[{root, "tests", "unscramble-compact.m"}];
  compactArtifactMetadata = If[FileExistsQ[compactMetadataFile], Get[compactMetadataFile], <||>];
  compactDescriptorEncoding = Lookup[compactArtifactMetadata, "DescriptorEncoding", None];
  check["installed compact descriptor encoding supported",
    MemberQ[{"CompactStructuredActions", "ParticleRoleTokens"}, compactDescriptorEncoding]];
  If[MemberQ[{"CompactStructuredActions", "ParticleRoleTokens"}, compactDescriptorEncoding],
  particleCompactArtifact = compactDescriptorEncoding === "ParticleRoleTokens";
  compactArrayExtractor = If[particleCompactArtifact,
    Unscrambling`Private`compactParticlePolicyArrays, Unscrambling`Private`compactPolicyArrays];
  compactFeatureEncoder = If[particleCompactArtifact,
    Unscrambling`Private`compactParticleFeatures, Unscrambling`Private`compactActionFeatures];
  compactPortableScorer = If[particleCompactArtifact,
    Unscrambling`Private`compactParticlePolicyScores, Unscrambling`Private`compactPolicyScores];
  compactWeights = compactArrayExtractor[Import[compactFile]];
  check["portable compact arrays have expected shapes", AssociationQ[compactWeights]];
  compactCandidates = UnscrambleCandidates[pair];
  compactState = Unscrambling`Private`stateFeature[amp, rules, Unscrambling`Private`stateLegIDs[amp, rules]];
  compactFeatures = compactFeatureEncoder[amp, rules, compactCandidates];
  check["compact numeric actions match installed encoding", If[particleCompactArtifact,
    Length[Dimensions[compactFeatures]] === 3 && First[Dimensions[compactFeatures]] === Length[compactCandidates] &&
      Last[Dimensions[compactFeatures]] === 166,
    Dimensions[compactFeatures] === {Length[compactCandidates], 146}]];
  Do[
    compactInput = <|"State" -> compactState, "Candidates" -> ConstantArray[Last[compactFeatures], count]|>;
    portableScores = compactPortableScorer[compactWeights, compactInput];
    check["portable compact scores count " <> ToString[count], Length[portableScores] === count &&
      VectorQ[portableScores, NumberQ] && AllTrue[portableScores, TrueQ[Im[#] == 0 && Abs[#] < Infinity] &] &&
      Max[portableScores] - Min[portableScores] < 10^-10],
    {count, {2, 1025}}];
  check["portable compact rejects wrong action width", compactPortableScorer[compactWeights,
    <|"State" -> compactState, "Candidates" -> ConstantArray[0.,
      If[particleCompactArtifact, {1, 1, 165}, {1, 145}]]|>] === $Failed];
  check["portable compact rejects zero state byte", compactPortableScorer[compactWeights,
    <|"State" -> ReplacePart[compactState, 1 -> 0], "Candidates" -> compactFeatures|>] === $Failed];
  check["portable compact rejects nonfinite features", compactPortableScorer[compactWeights,
    <|"State" -> compactState, "Candidates" -> ReplacePart[compactFeatures,
      If[particleCompactArtifact, {1, 1, 1}, {1, 1}] -> Indeterminate]|>] === $Failed];
  sparseIDs = AssociationThread[Range[4], {31, 47, 59, 83}];
  sparseAmp = amp /. Spinor[type_, chirality_, leg_Integer] :> Spinor[type, chirality, sparseIDs[leg]];
  sparseRules = rules /. Mass[leg_Integer] :> Mass[sparseIDs[leg]];
  sparseCandidates = UnscrambleCandidates[{sparseRules, sparseAmp}];
  sparseState = Unscrambling`Private`stateFeature[sparseAmp, sparseRules,
    Unscrambling`Private`stateLegIDs[sparseAmp, sparseRules]];
  sparseFeatures = compactFeatureEncoder[sparseAmp, sparseRules, sparseCandidates];
  check["compact states normalize sparse particle labels", sparseState === compactState];
  check["compact descriptors normalize sparse particle labels", sparseFeatures === compactFeatures];
  check["portable scores preserve sparse relabeling", Max[Abs[
    compactPortableScorer[compactWeights, <|"State" -> sparseState, "Candidates" -> sparseFeatures|>] -
    compactPortableScorer[compactWeights, <|"State" -> compactState, "Candidates" -> compactFeatures|>]]]
      < 10^-10];
  ];
];
Block[{Unscrambling`Private`$policyMethod = "ReverseMoves",
    Unscrambling`Private`$trainingBatchSize = 1, Unscrambling`Private`$trainingTargetDevice = "CPU"},
  scr = ComplicateAmplitude[pair, 2, RandomSeed -> 1, "RecordSteps" -> True];
  rows = Flatten[Unscrambling`Private`trainingRows[#["After"], rules, #["Before"]] & /@ Reverse[scr["Trajectory"]], 1];
  check["reverse path produces labels", Length[rows] > 0];
  check["no resulting expression encodings", AllTrue[rows, !KeyExistsQ[#, "Candidates"] &]];
  check["classification targets are normalized vectors", AllTrue[rows,
    Dimensions[#["Target"]] === {512} && Abs[Total[#["Target"]] - 1.] < 10^-6 &]];
  check["legal masks are vectors", AllTrue[rows, Dimensions[#["LegalMask"]] === {512} &&
    Total[#["LegalMask"]] === N[#["ActionCount"]] &]];
  check["reverse rows do not use regression weights", AllTrue[rows, !KeyExistsQ[#, "Weights"] &]];
  Do[
    cands = UnscrambleCandidates[{rules, step["After"]}];
    row = Unscrambling`Private`trainingRows[step["After"], rules, step["Before"]];
    If[row =!= {},
      positives = Flatten[Position[First[row]["Target"], _?Positive]];
      check["positive actions undo recorded step", AllTrue[positives,
        Unscrambling`Private`samePoly[Unscrambling`Private`applyCandidate[step["After"], cands[[#]]], step["Before"]] &]]],
    {step, scr["Trajectory"]}];
  stop = First[Unscrambling`Private`trainingRows[amp, rules, amp]];
  check["stop label", stop["Target"][[stop["ActionCount"]]] > 0];
  check["unused slots have no target", Total[Drop[stop["Target"], stop["ActionCount"]]] == 0];
  check["unused slots are masked", Total[Drop[stop["LegalMask"], stop["ActionCount"]]] == 0];
  Block[{Unscrambling`Private`$moveSlots = 1},
    check["overflow fails instead of truncating", Quiet[Unscrambling`Private`trainingRows[amp, rules, amp]] === $Failed]];
  net = Unscrambling`Private`trainCandidateNetwork[Append[rows, stop], 1];
  check["state-only network trains", Head[net] === NetGraph];
  check["no candidate encoder", FreeQ[Information[net, "ArraysPositionList"], "CandidateEncoder"]];
  check["move policy returns raw logit vector", Dimensions[net[KeyTake[stop, {"State"}]]] === {512}];

  encoder = NetInitialize[Unscrambling`Private`makeMoveEncoder[], RandomSeeding -> 1];
  base = ConstantArray[97, 800];
  baseVector = encoder[base];
  check["move encoder has finite 128-vector output", Dimensions[baseVector] === {128} && VectorQ[baseVector, NumericQ]];
  Do[check["move encoder observes position " <> ToString[position],
    Max[Abs[encoder[ReplacePart[base, position -> 98]] - baseVector]] > 10^-5],
    {position, {1, 400, 800}}];
  raggedInputs = {base, ReplacePart[Take[base, 500], 250 -> 98]};
  check["ragged encoder batching preserves per-state results",
    Max[Abs[Flatten[encoder[raggedInputs] - (encoder[#] & /@ raggedInputs)]]] < 10^-6];

  trainer = NetInitialize[Unscrambling`Private`makeMoveTrainingPolicy[], RandomSeeding -> 1];
  trainer = NetReplacePart[trainer, {
    {"Policy", "Actions", 3, "Weights"} -> ConstantArray[0., {512, 128}],
    {"Policy", "Actions", 3, "Biases"} -> PadRight[{0., 1., 2.}, 512, 10.^25]}];
  lossInput = <|"State" -> base, "LegalMask" -> PadRight[{1., 1., 1.}, 512],
    "Target" -> N[UnitVector[512, 3]]|>;
  (* Native CPU cross-entropy arithmetic differs by about 2e-6 in this three-class case. *)
  ceTolerance = 5.*^-6;
  probabilities = trainer[lossInput, NetPort[{"Probabilities", "Output"}]];
  gradient = trainer[lossInput, NetPortGradient[{"Policy", "Actions", 3, "Biases"}]];
  check["masked probabilities sum to one", Abs[Total[probabilities] - 1.] < 10^-6];
  check["giant invalid logits have zero probability", Total[Drop[probabilities, 3]] == 0];
  check["giant invalid logits have zero gradient", Total[Abs[Drop[gradient, 3]]] == 0];
  check["classification gradient matches probability minus target",
    Max[Abs[gradient - (probabilities - lossInput["Target"])]] < ceTolerance];
  check["one-positive cross-entropy value", Abs[trainer[lossInput] - (Log[1 + Exp[1] + Exp[2]] - 2)] < ceTolerance];
  multiple = Join[lossInput, <|"Target" -> N[(UnitVector[512, 1] + UnitVector[512, 3])/2]|>];
  multipleGradient = trainer[multiple, NetPortGradient[{"Policy", "Actions", 3, "Biases"}]];
  check["multiple positive actions have normalized cross-entropy",
    Abs[trainer[multiple] - (Log[1 + Exp[1] + Exp[2]] - 1)] < ceTolerance];
  check["multiple positive gradient matches normalized target",
    Max[Abs[multipleGradient - (probabilities - multiple["Target"])]] < ceTolerance];
  check["multiple positive invalid gradients remain zero", Total[Abs[Drop[multipleGradient, 3]]] == 0];
  strongWrong = NetReplacePart[trainer, {"Policy", "Actions", 3, "Biases"} ->
    PadRight[{1000., 1., 2.}, 512, 10.^25]];
  wrongGradient = strongWrong[lossInput, NetPortGradient[{"Policy", "Actions", 3, "Biases"}]];
  check["confident wrong legal action has finite loss", Abs[strongWrong[lossInput] - 998.] < 10^-5];
  check["confident wrong action retains corrective gradient",
    Max[Abs[wrongGradient - (UnitVector[512, 1] - UnitVector[512, 3])]] < 10^-6];
  accuracy = Unscrambling`Private`moveTrainingAccuracy[NetExtract[trainer, "Policy"],
    {Join[multiple, <|"ActionCount" -> 3|>]}];
  check["training accuracy accepts normalized positive labels", accuracy["Correct"] === 1 && accuracy["Accuracy"] == 1.];
  Block[{Unscrambling`Private`$trainingBatchSize = 2},
    batchedNet = Unscrambling`Private`trainCandidateNetwork[Append[rows, stop], 1];
    check["mixed-length state-only network trains in batches", Head[batchedNet] === NetGraph &&
      AllTrue[Append[rows, stop], Dimensions[batchedNet[KeyTake[#, {"State"}]]] === {512} &]];
    check["batched trained encoder preserves per-state inference",
      Max[Abs[Flatten[batchedNet[<|"State" -> Lookup[Append[rows, stop], "State"]|>] -
        (batchedNet[KeyTake[#, {"State"}]] & /@ Append[rows, stop])]]] < 10^-6]];
  Unscrambling`Private`$modelNet = net;
  cands = UnscrambleCandidates[pair];
  scores = Unscrambling`Private`scoreCandidates[amp, rules, cands];
  check["only legal slots scored", Length[scores] === Length[cands] && VectorQ[scores, NumericQ]];
  giantPolicy = NetExtract[NetReplacePart[trainer, {"Policy", "Actions", 3, "Biases"} ->
    PadRight[N[Range[Length[cands]]/10], 512, 10.^25]], "Policy"];
  Block[{Unscrambling`Private`$modelNet = giantPolicy},
    giantScores = Unscrambling`Private`scoreCandidates[amp, rules, cands];
    check["inference excludes giant invalid logits", Length[giantScores] === Length[cands] &&
      Max[Abs[giantScores - N[Range[Length[cands]]/10]]] < 10^-6];
    chosen = Unscrambling`Private`selectCandidate[amp, rules, False];
    check["greedy inference only selects legal slots", chosen["Index"] === Length[cands]]];
  paths = Unscrambling`Private`modelPaths[dir];
  Export[paths["Net"], net];
  Put[<|"Encoding" -> "SplitFullFormUTF8", "Architecture" -> Unscrambling`Private`policyArchitecture[], "MoveSlots" -> 512,
    "Objective" -> "LegalMoveCrossEntropy",
    "MoveSourceHash" -> FileHash[FileNameJoin[{root, "tests", "unscrambling.wl"}], "SHA256"]|>, paths["Metadata"]];
  Unscrambling`Private`$loadedModelDirectory = None;
  check["move model reloads", Unscrambling`Private`loadUnscrambleModel[dir]];
  check["reload scores stable", Max[Abs[scores - Unscrambling`Private`scoreCandidates[amp, rules, cands]]] < 10^-6];
  Block[{Unscrambling`Private`$policyMethod = "CompactActions",
      Unscrambling`Private`$loadedModelDirectory = None},
    compactPaths = Unscrambling`Private`modelPaths[dir];
    Export[compactPaths["Net"], net]; Put[compactMetadata, compactPaths["Metadata"]];
    check["compact loader rejects state-only ports", !Unscrambling`Private`loadUnscrambleModel[dir]]];
  metadata = Get[paths["Metadata"]];
  Put[KeyDrop[metadata, "Objective"], paths["Metadata"]];
  Unscrambling`Private`$loadedModelDirectory = None;
  check["old regression objective model is rejected", !Unscrambling`Private`loadUnscrambleModel[dir]];
  Put[metadata, paths["Metadata"]];
  trace = UnscrambleTrace[pair, "PolicyMethod" -> "ReverseMoves", "ModelDirectory" -> dir, "MaxSteps" -> 2];
  check["public move inference", TrueQ[trace["ModelLoaded"]] && !TrueQ[trace["ScoringFailed"]]];
  Block[{Unscrambling`Private`$policyMethod = "CandidateScorer"},
    scorerEncoder = NetExtract[Unscrambling`Private`makePolicy[], "StateEncoder"];
    check["scorer recurrent encoder is unchanged", Head[NetExtract[scorerEncoder, 2]] === GatedRecurrentLayer &&
      Head[NetExtract[scorerEncoder, 3]] === SequenceLastLayer];
    scorerRow = First[Unscrambling`Private`trainingRows[amp, rules, amp]];
    check["scorer regression rows are unchanged", KeyExistsQ[scorerRow, "Candidates"] &&
      KeyExistsQ[scorerRow, "Weights"] && Dimensions[scorerRow["Target"]] === {Length[cands], 1}];
    check["separate files", Unscrambling`Private`modelPaths[dir] =!= paths];
    check["no fallback to wrong method", !Unscrambling`Private`loadUnscrambleModel[dir]]];
];
check["default unchanged", ("PolicyMethod" /. Options[TrainUnscrambleNet]) === "CandidateScorer"];
check["invalid method rejected", Quiet[TrainUnscrambleNet[{pair}, "PolicyMethod" -> "Unknown"]] === $Failed];
report = TrainUnscrambleNet[{pair}, "PolicyMethod" -> "ReverseMoves", Steps -> 2,
  Scrambles -> 1, HoldOut -> 1, MaxTrainingRounds -> 1, EpisodeLength -> 2,
  Kernels -> 1, "ModelDirectory" -> dir];
check["public training pipeline", AssociationQ[report] && report["PolicyMethod"] === "ReverseMoves" &&
  report["ReverseSteps"] > 0 && report["HoldoutScrambles"] === 1];
check["scorer model not overwritten", !FileExistsQ[FileNameJoin[{dir, "unscramble.wlnet"}]]];
check["separate validation report", FileExistsQ[FileNameJoin[{dir, "move-validation-results.m"}]]];
progress = Get[FileNameJoin[{dir, "move-training-progress.m"}]];
check["progress includes completed batches and loss", AssociationQ[progress] &&
  progress["AbsoluteBatch"] > 0 && NumericQ[progress["BatchLoss"]] &&
  progress["AbsoluteBatch"] === progress["TotalBatches"]];
check["progress snapshot excludes large arrays", FreeQ[Keys[progress], "Net" | "Weights" | "BatchData"]];
DeleteDirectory[dir, DeleteContents -> True];
Print[<|"Checks" -> checks, "Failed" -> Length[failures], "Failures" -> failures|>];
Quit[If[failures === {}, 0, 1]];
