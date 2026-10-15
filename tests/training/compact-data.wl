(* ::Section::Closed:: *)
(*Compact Dataset Configuration and Splitting*)
(* Loaded in Unscrambling`Private`; staged as compact-scale-data.wl. *)
compactScaleDefaults = <|"Scrambles" -> 12, "HoldoutScrambles" -> 2,
  "Depths" -> {3, 4, 5, 6}, "Seed" -> 20260930, "MaxActions" -> 1024,
  "TrainingCandidateBudget" -> 256, "MaxTrainingRowBytes" -> Infinity,
  "MaxStateBytes" -> 100000, "MaxExpressionLeaves" -> 20000,
  "JobSeconds" -> 120, "RowSeconds" -> 15, "RouteJobSeconds" -> 600,
  "SimplificationRoutes" -> {}, "RoutePermutations" -> 4, "CandidateStorage" -> "Packed",
  "RouteProgressDirectory" -> None, "ModelGuidanceEnabled" -> False,
  "FullBoardGuidanceStatesPerRoute" -> 4, "FullBoardAuditMaxStates" -> 48, "HardNegativeCount" -> 32,
  "RecoveryAttempts" -> 2, "RecoveryRolloutSteps" -> 2, "RecoveryDepth" -> 2,
  "RecoveryWidth" -> 4, "RecoveryNodeLimit" -> 32, "RecoveryMaxJobs" -> 48,
  "RecoveryTotalSeconds" -> 1800, "GuidanceSeconds" -> 120,
  "OptimizeSimplificationRoutes" -> False, "RouteOptimizationSeconds" -> 180,
  "RouteOptimizationTotalSeconds" -> 1800|>;
$compactGuidanceWeights = None;
compactScaleConfig[config_Association] := Join[compactScaleDefaults, config];
compactScalePacked[value_] := Developer`ToPackedArray[value];
(* Sparse storage preserves the tensor; only individual neural inputs become dense. *)
compactScaleCandidateStorageQ[value_] := Developer`PackedArrayQ[value] ||
  (Head[value] === SparseArray && TrueQ[value["ImplicitValue"] == 0] &&
    VectorQ[value["NonzeroValues"], Head[#] === Real && MachineNumberQ[#] && TrueQ[Abs[#] < Infinity] &]);
compactScaleDenseCandidates[value_] := If[Head[value] === SparseArray,
  compactScalePacked[Normal[value]], value];
compactScaleStoreCandidates[features_, storage_] := Switch[storage,
  "Packed", compactScalePacked[N[features]],
  "Sparse", SparseArray[N[features]],
  _, $Failed];
compactScaleCandidatesEqualQ[first_, second_] := If[Head[first] === Head[second],
  first === second, SparseArray[first] === SparseArray[second]];
compactScaleStateKey[state_] := IntegerString[Hash[state, "SHA256"], 16, 64];
compactScaleProcessNames[record_] := DeleteDuplicates[Cases[
  StringSplit[#, " / "] & /@ Lookup[record, "Sources", {}],
  {"SM-4-point-test.nb", process_String, _String} :> process]];
compactScaleMassKey[record_] := IntegerString[Hash[Sort[Cases[record["Masses"],
  HoldPattern[Mass[_Integer] -> mass_] :> mass]], "SHA256"], 16, 64];

compactScaleSplit[catalog_List, seed_Integer, holdFraction_?NumericQ] := Module[
  {indices, massGroups, named, processGroups, edges, groups, order, reachable, previous,
    target, best, heldGroups, held, training, groupRecords},
  If[Length[catalog] < 2 || !(0. < holdFraction < 1.), Return[$Failed]];
  indices = Range[Length[catalog]];
  massGroups = GatherBy[indices, compactScaleMassKey[catalog[[#]]] &];
  named = Flatten[Table[{name, index}, {index, indices},
    {name, compactScaleProcessNames[catalog[[index]]]}], 1];
  processGroups = Last /@ # & /@ GatherBy[named, First];
  edges = Flatten[(UndirectedEdge @@@ Partition[#, 2, 1] &) /@
    Join[massGroups, processGroups]];
  groups = Sort[Sort /@ ConnectedComponents[Graph[indices, edges]]];
  If[Length[groups] < 2, Return[$Failed]];
  order = BlockRandom[SeedRandom[seed]; RandomSample[Range[Length[groups]]]];
  target = Round[holdFraction Length[catalog]]; reachable = <|0 -> {}|>;
  Do[previous = Normal[reachable];
    Do[With[{sum = First[entry] + Length[groups[[group]]]},
      If[!KeyExistsQ[reachable, sum], reachable[sum] = Append[Last[entry], group]]],
      {entry, previous}], {group, order}];
  best = First[SortBy[Select[Keys[reachable], 0 < # < Length[catalog] &],
    {Abs[# - target], Boole[# > target], #} &]];
  heldGroups = Sort[reachable[best]]; held = Sort[Flatten[groups[[heldGroups]]]];
  training = Complement[indices, held];
  groupRecords = MapIndexed[Function[{members, position},
    <|"Group" -> First[position], "Indices" -> members,
      "Split" -> If[MemberQ[heldGroups, First[position]], "Heldout", "Training"],
      "ExternalMassKeys" -> DeleteDuplicates[compactScaleMassKey /@ catalog[[members]]],
      "KnownProcessNames" -> DeleteDuplicates[Flatten[compactScaleProcessNames /@ catalog[[members]]]],
      "ReferenceNames" -> Lookup[catalog[[members]], "Name"]|>], groups];
  <|"TrainingIndices" -> training, "HeldoutIndices" -> held, "Groups" -> groupRecords,
    "Seed" -> seed, "RequestedHoldoutFraction" -> N[holdFraction],
    "ActualHoldoutFraction" -> N[Length[held]/Length[catalog]],
    "CatalogReferences" -> Length[catalog], "TrainingReferences" -> Length[training],
    "HeldoutReferences" -> Length[held], "FamilyGroups" -> Length[groups],
    "SplitRule" -> "Connected external-mass multisets and known source process names",
    "Interpretation" -> "Heldout external-mass/process families; references include channels and sums"|>
];

compactScaleJobs[catalog_List, indices_List, config_Association, kind_String] := Module[
  {settings = compactScaleConfig[config], training, count, depths, jobs},
  training = MemberQ[{"Training", "Train"}, kind];
  If[!training && !MemberQ[{"Heldout", "Holdout", "Validation"}, kind], Return[$Failed]];
  count = If[training, settings["Scrambles"], settings["HoldoutScrambles"]]; depths = settings["Depths"];
  If[!IntegerQ[count] || count < 1 || !VectorQ[depths, IntegerQ] || depths === {} || Min[depths] < 1,
    Return[$Failed]];
  jobs = Flatten[Table[Module[{pair = Lookup[catalog[[index]], {"Masses", "Amplitude"}], seed, ids},
    seed = If[sequence === 0, 0, settings["Seed"] + sequence + If[training, 0, 1000000]];
    ids = trainingLegPermutation[pair, index, seed];
    <|"Kind" -> kind, "Mode" -> If[sequence === 0, "Original", "Scramble"],
      "AmplitudeIndex" -> index, "ReferenceName" -> catalog[[index]]["Name"],
      "Sequence" -> sequence, "Seed" -> seed,
      "Steps" -> If[sequence === 0, 0, depths[[1 + Mod[sequence + index - 2, Length[depths]]]]],
      "LegPermutation" -> ids, "Pair" -> relabelLegs[pair, ids]|>],
    {index, indices}, {sequence, If[training, 0, 1], count}], 1];
  MapIndexed[Join[#1, <|"Order" -> First[#2]|>] &, jobs]
];

compactScaleRoutePermutationCount[count_] := Replace[count, "All" -> All];
compactScaleRouteJobs[config_Association] := Module[{settings = compactScaleConfig[config], jobs, count},
  count = compactScaleRoutePermutationCount[settings["RoutePermutations"]];
  If[!validRoutePermutationCountQ[count],
    Message[TrainUnscrambleNet::routes]; Return[$Failed]];
  jobs = makeSimplificationRouteJobs[settings["SimplificationRoutes"], count];
  If[jobs === $Failed, Return[$Failed]];
  Join[#, <|"Kind" -> "Training", "ReferenceName" -> #["Name"], "Sequence" -> #["Seed"],
    "ExternalLegCount" -> Length[Keys[stateLegIDs[#["States"], #["Pair"][[1]]]]], "Curated" -> True,
    "RouteIdentities" -> Lookup[settings["SimplificationRoutes"][[ -#["AmplitudeIndex"]]], "Identities", {}]|>] & /@ jobs
];

compactScaleRoutePhase[identities_List, step_Integer?Positive] := Module[{operation, phase},
  operation = If[step <= Length[identities], identities[[step]], "Stop"];
  phase = Length[Split[Take[identities, UpTo[step]]]] + Boole[step > Length[identities]];
  <|"RoutePhase" -> ToString[phase] <> ":" <> operation,
    "RoutePhaseIndex" -> phase, "RouteIdentity" -> operation|>
];

compactScaleProblem[reason_, extras_:<||>] := Join[<|"Reason" -> reason|>, extras];
compactScaleActionKey[candidate_, ids_] := IntegerString[Hash[
  {candidate["Name"], relabelLegs[candidate["Subtract"], ids],
    relabelLegs[candidate["Insert"], ids]}, "SHA256"], 16, 64];
compactScaleBoardSignature[keys_List] := IntegerString[Hash[Sort[keys], "SHA256"], 16, 64];
compactScaleCandidateSignature[candidates_, ids_] := IntegerString[Hash[
  ({#["Name"], relabelLegs[#["Subtract"], ids], relabelLegs[#["Insert"], ids]} &) /@ candidates,
  "SHA256"], 16, 64];
compactScalePositiveSlots[target_List] := Flatten[Position[target, _?Positive]];
(* Rank action identities, not enumeration slots. Mandatory actions never compete for a budget. *)
(* ::Section::Closed:: *)
(*Stable Candidate Sampling and Model Guidance*)
compactScaleSelection[keys_List, positives_List, stop_, stateKey_String, budget_, seed_, hard_List:{}] := Module[
  {mandatory, negative, effective, selected, sampled, retainedHard},
  If[!IntegerQ[budget] || budget < 1 || !IntegerQ[seed] || keys === {} ||
      !DuplicateFreeQ[keys] || !IntegerQ[stop] || !(1 <= stop <= Length[keys]) ||
      !VectorQ[positives, IntegerQ] || positives === {} ||
      !AllTrue[positives, 1 <= # <= Length[keys] &] || !VectorQ[hard, IntegerQ] ||
      !AllTrue[hard, 1 <= # <= Length[keys] &],
    Return[compactScaleProblem["InvalidCandidateSampling"]]];
  mandatory = Union[positives, {stop}]; effective = Max[budget, Length[mandatory]];
  retainedHard = Take[Select[DeleteDuplicates[hard], !MemberQ[mandatory, #] &],
    UpTo[Max[0, effective - Length[mandatory]]]];
  mandatory = Union[mandatory, retainedHard];
  sampled = Length[keys] > effective;
  selected = If[!sampled, Range[Length[keys]],
    negative = Complement[Range[Length[keys]], mandatory];
    negative = SortBy[negative,
      {Hash[{"CompactTrainingNegative", seed, stateKey, keys[[#]]}, "SHA256"], keys[[#]]} &];
    Join[SortBy[DeleteCases[Join[mandatory, Take[negative, UpTo[effective - Length[mandatory]]]], stop],
      keys[[#]] &], {stop}]];
  <|"Indices" -> selected, "Sampling" -> <|"Method" -> If[hard === {}, "StateActionHashRank", "ModelHardNegativesAndStateActionHashRank"],
    "Seed" -> seed, "RequestedBudget" -> budget, "EffectiveBudget" -> effective,
    "FullActionCount" -> Length[keys], "RetainedActionCount" -> Length[selected],
    "FullPositiveCount" -> Length[DeleteDuplicates[positives]], "MandatoryActionCount" -> Length[Union[positives, {stop}]],
    "FullNegativeCount" -> Length[keys] - Length[DeleteDuplicates[positives]],
    "RetainedNegativeCount" -> Length[selected] - Length[DeleteDuplicates[positives]],
    "DroppedNegativeCount" -> Length[keys] - Length[selected], "Sampled" -> sampled,
    "CandidateBudgetExpanded" -> (Length[Union[positives, {stop}]] > budget), "HardNegativeCount" -> Length[retainedHard],
    "RawLegalEnumeration" -> True, "StopRetained" -> MemberQ[selected, stop]|>|>
];
compactScaleGuidanceSteps[job_Association, settings_Association] := Module[{count, nonterminal, phases, groups, ordered, phaseSlots},
  If[!TrueQ[settings["ModelGuidanceEnabled"]], Return[{}]];
  count = settings["FullBoardGuidanceStatesPerRoute"];
  nonterminal = Range[Max[0, Length[job["States"]] - 1]];
  If[nonterminal === {} || count === 0, Return[{}]];
  phases = compactScaleRoutePhase[Lookup[job, "RouteIdentities", {}], #]["RoutePhaseIndex"] & /@ nonterminal;
  groups = GatherBy[nonterminal, phases[[#]] &];
  If[Length[groups] > count,
    phaseSlots = If[count === 1, {1}, DeleteDuplicates[Round[Subdivide[1, Length[groups], count - 1]]]];
    groups = groups[[phaseSlots]]];
  ordered = Flatten[Table[If[index <= Length[group], group[[index]], Nothing],
    {index, Max[Length /@ groups]}, {group, groups}], 1];
  Sort[Take[ordered, UpTo[count]]]
];
compactScaleRecoveryAnchorSteps[job_Association, settings_Association] := Module[{steps, count, indices},
  steps = compactScaleGuidanceSteps[job, settings]; count = settings["RecoveryAttempts"];
  If[Length[steps] <= count, Return[steps]];
  If[count === 0, Return[{}]];
  indices = If[count === 1, {1}, DeleteDuplicates[Round[Subdivide[1, Length[steps], count - 1]]]];
  steps[[indices]]
];
compactScaleHardSlots[keys_List, labels_List, scores_List, count_Integer?NonNegative] :=
  Take[SortBy[Select[Range[Length[keys]], labels[[#]] === 0 && # =!= Length[keys] &],
    {-scores[[#]], keys[[#]]} &], UpTo[count]];
compactScaleLoadGuidanceWeights[settings_Association] := Module[{file, metaFile, metadata, policy, weights},
  $compactGuidanceWeights = None;
  If[!TrueQ[settings["ModelGuidanceEnabled"]], Return[<|"Passed" -> True, "Enabled" -> False|>]];
  file = Lookup[settings, "WarmStartPolicyFile", None]; metaFile = Lookup[settings, "WarmStartMetadataFile", None];
  If[!StringQ[file] || !StringQ[metaFile] || !FileExistsQ[file] || !FileExistsQ[metaFile], Return[$Failed]];
  metadata = Quiet[Check[Get[metaFile], $Failed]];
  If[!AssociationQ[metadata] || !compactParticleModelCompatibleQ[metadata], Return[$Failed]];
  policy = TimeConstrained[Quiet[Check[Import[file], $Failed]], 60, $Failed];
  weights = If[MatchQ[policy, _NetGraph], compactParticlePolicyArrays[policy], $Failed];
  If[!AssociationQ[weights], Return[$Failed]];
  $compactGuidanceWeights = weights;
  <|"Passed" -> True, "Enabled" -> True,
    "PolicySHA256" -> IntegerString[FileHash[file, "SHA256"], 16, 64],
    "MetadataSHA256" -> IntegerString[FileHash[metaFile, "SHA256"], 16, 64]|>
];
compactScaleSamplingReport[rows_List] := Module[{reports},
  reports = Lookup[Select[rows, AssociationQ[Lookup[#, "Sampling", None]] &], "Sampling", <||>];
  <|"RowsWithSamplingMetadata" -> Length[reports],
    "SampledRows" -> Count[Lookup[reports, "Sampled", {}], True],
    "ExpandedBudgetRows" -> Count[Lookup[reports, "CandidateBudgetExpanded", {}], True],
    "RawActionThresholdExceededRows" -> Count[Lookup[reports, "RawActionThresholdExceeded", {}], True],
    "FullLegalActions" -> Total[Lookup[reports, "FullActionCount", 0]],
    "RetainedActions" -> Total[Lookup[reports, "RetainedActionCount", 0]],
    "TeacherPositiveActions" -> Total[Lookup[reports, "FullPositiveCount", 0]],
    "DroppedNegativeActions" -> Total[Lookup[reports, "DroppedNegativeCount", 0]],
    "MaximumFullActionCount" -> Max[Join[{0}, Lookup[reports, "FullActionCount", {}]]],
    "MaximumRetainedActionCount" -> Max[Join[{0}, Lookup[reports, "RetainedActionCount", {}]]],
    "RawLegalEnumeration" -> True|>
];
compactScaleRouteProgress[job_, event_, settings_] := Module[{directory, record, path, pending, result},
  directory = Lookup[settings, "RouteProgressDirectory", None];
  record = Join[KeyTake[job, {"Order", "ReferenceName", "Sequence", "Seed"}],
    <|"RouteName" -> Lookup[job, "Name", Lookup[job, "ReferenceName", "Unknown"]],
      "LegPermutation" -> Values[Lookup[job, "LegPermutation", <||>]],
      "Timestamp" -> DateString["ISODateTime"], "KernelPID" -> $ProcessID|>, KeyDrop[event, "LegPermutation"]];
  If[Lookup[event, "Phase", None] === "RowComplete",
    Print[DateString["ISODateTime"], " ROUTE ROW: ", KeyTake[record,
      {"RouteName", "Sequence", "Step", "Steps", "Seconds"}]]];
  If[directory === None, Return[True]];
  If[!StringQ[directory] || !DirectoryQ[directory], Return[False]];
  path = FileNameJoin[{directory, "route-" <> ToString[Lookup[job, "Order", 0]] <> "-" <>
    ToString[Lookup[job, "Sequence", Lookup[job, "Seed", 0]]] <> ".json"}];
  pending = FileNameJoin[{directory, ".pending-" <> FileNameTake[path] <> "-" <> ToString[$ProcessID]}];
  result = Quiet[Check[Export[pending, record, "RawJSON"], $Failed]];
  If[result === $Failed, Return[False]];
  TrueQ[Quiet[Check[RenameFile[pending, path, OverwriteTarget -> True]; True, False]]]
];
(* ::Section::Closed:: *)
(*Verified Rows and Generation Jobs*)
compactScaleLegal[expr_, rules_, settings_, provided_:Automatic] := Module[{leaves, state, candidates},
  leaves = LeafCount[expr];
  If[leaves > settings["MaxExpressionLeaves"],
    Return[compactScaleProblem["ExpressionLeafLimit", <|"Leaves" -> leaves|>]]];
  state = stateFeature[expr, rules, stateLegIDs[expr, rules]];
  If[Length[state] > settings["MaxStateBytes"],
    Return[compactScaleProblem["StateByteLimit", <|"Bytes" -> Length[state]|>]]];
  candidates = If[provided === Automatic, legalMoves[expr, rules], provided];
  If[candidates === {} || Last[candidates]["Name"] =!= "Stop" ||
      Count[Lookup[candidates, "Name"], "Stop"] =!= 1,
    Return[compactScaleProblem["InvalidLegalMoves"]]];
  <|"State" -> state, "Candidates" -> candidates, "RawLegalEnumeration" -> True,
    "RawActionThresholdExceeded" -> (Length[candidates] > settings["MaxActions"])|>
];

(* This list must come from legalMoves, which excludes unchanged non-Stop edits. *)
compactScaleDeltaLabels[expr_, target_, candidates_List] := Module[{canonicalTarget, delta},
  canonicalTarget = canonicalChains[target];
  If[samePoly[expr, canonicalTarget],
    Return[Boole[#["Name"] === "Stop"] & /@ candidates]];
  delta = Expand[canonicalTarget - canonicalChains[expr]];
  Boole[#["Name"] =!= "Stop" &&
    samePoly[canonicalChains[#["Insert"] - #["Subtract"]], delta]] & /@ candidates
];

compactScaleRow[expr_, rules_, target_, settings_, candidatesProvided_:Automatic, labelsProvided_:Automatic] := Module[
  {config = compactScaleConfig[settings], legal, candidates, features, storedFeatures, labels,
    ids, signature, keys, selection, selected, stateKey, sampling, row, rowBytes,
    guided, scores, hard = {}, hardScores = <||>, guidance = <||>, proof = <||>},
  If[!IntegerQ[config["TrainingCandidateBudget"]] || config["TrainingCandidateBudget"] < 1 ||
      !IntegerQ[config["Seed"]] || !TrueQ[config["MaxTrainingRowBytes"] > 0],
    Return[compactScaleProblem["InvalidCandidateSamplingConfiguration"]]];
  If[!MemberQ[{"Packed", "Sparse"}, Lookup[settings, "CandidateStorage", "Packed"]],
    Return[compactScaleProblem["InvalidCandidateStorage"]]];
  legal = compactScaleLegal[expr, rules, config, candidatesProvided];
  If[KeyExistsQ[legal, "Reason"], Return[legal]];
  candidates = legal["Candidates"];
  labels = If[labelsProvided === Automatic, compactScaleDeltaLabels[expr, target, candidates], labelsProvided];
  If[!ListQ[labels] || Length[labels] =!= Length[candidates] || !AllTrue[labels, # === 0 || # === 1 &],
    Return[compactScaleProblem["InvalidTeacherLabels"]]];
  If[Total[labels] === 0, Return[compactScaleProblem["NoLegalReverse"]]];
  ids = stateLegIDs[expr, rules]; keys = compactScaleActionKey[#, ids] & /@ candidates;
  stateKey = compactScaleStateKey[legal["State"]];
  guided = TrueQ[config["ModelGuidanceEnabled"]] && TrueQ[Lookup[config, "GuidanceState", False]];
  If[guided,
    If[!AssociationQ[$compactGuidanceWeights], Return[compactScaleProblem["GuidanceWeightsUnavailable"]]];
    scores = TimeConstrained[trainingCompactCandidateScores[$compactGuidanceWeights, expr, rules, candidates],
      config["GuidanceSeconds"], $Failed];
    If[!ListQ[scores] || Length[scores] =!= Length[candidates] || !VectorQ[scores,
        NumberQ[#] && TrueQ[Im[#] == 0 && Abs[#] < Infinity] &],
      Return[compactScaleProblem["FullBoardGuidanceScoringFailed"]]];
    guidance = trainingFullBoardAudit[candidates, labels, scores];
    If[!AssociationQ[guidance] || !TrueQ[Lookup[guidance, "Passed", False]],
      Return[compactScaleProblem["FullBoardGuidanceAuditFailed"]]];
    hard = compactScaleHardSlots[keys, labels, scores,
      Min[config["HardNegativeCount"], Max[0, config["TrainingCandidateBudget"] -
        Length[Union[Flatten[Position[labels, 1]], {Length[candidates]}]]]]];
    hardScores = AssociationThread[keys[[hard]], scores[[hard]]];
    proof = <|"Expression" -> expr, "Masses" -> rules, "Target" -> target,
      "OnShellChannels" -> $onShellChannels, "MomentumConservation" -> $useMomentumConservation,
      "StateKey" -> stateKey, "FullCandidateSignature" -> compactScaleBoardSignature[keys],
      "FullActionCount" -> Length[keys], "TeacherKeys" -> keys[[Flatten[Position[labels, 1]]]]|>];
  selection = compactScaleSelection[keys, Flatten[Position[labels, 1]], Length[candidates],
    stateKey, config["TrainingCandidateBudget"], config["Seed"], hard];
  If[KeyExistsQ[selection, "Reason"], Return[selection]];
  selected = selection["Indices"]; sampling = Join[selection["Sampling"],
    <|"RawActionThresholdExceeded" -> legal["RawActionThresholdExceeded"]|>];
  signature = compactScaleCandidateSignature[candidates[[selected]], ids];
  candidates = candidates[[selected]]; labels = labels[[selected]];
  features = compactActionFeatures[expr, rules, candidates];
  If[features === $Failed, Return[compactScaleProblem["InvalidCompactDescriptor"]]];
  storedFeatures = compactScaleStoreCandidates[features, Lookup[settings, "CandidateStorage", "Packed"]];
  If[!compactScaleCandidateStorageQ[storedFeatures],
    Return[compactScaleProblem["InvalidCandidateStorage"]]];
  row = Join[<|"State" -> compactScalePacked[legal["State"]], "StateKey" -> stateKey,
    "Candidates" -> storedFeatures,
    "Target" -> compactScalePacked[N[labels/Total[labels]]],
    "TargetOperations" -> DeleteDuplicates[Lookup[Pick[candidates, labels, 1], "Name"]],
    "LegalMask" -> compactScalePacked[ConstantArray[1., Length[candidates]]],
    "ActionCount" -> Length[candidates], "CandidateSignature" -> signature,
    "CandidateKeys" -> keys[[selected]], "CandidateOperations" -> Lookup[candidates, "Name"],
    "FullCandidateSignature" -> compactScaleBoardSignature[keys],
    "FullActionCount" -> Length[keys], "Sampling" -> sampling,
    "TrainingCandidateBudget" -> config["TrainingCandidateBudget"], "SamplingSeed" -> config["Seed"],
    "Sampled" -> sampling["Sampled"], "MandatoryActionCount" -> sampling["MandatoryActionCount"],
    "CandidateBudgetExpanded" -> sampling["CandidateBudgetExpanded"], "HardNegativeKeys" -> keys[[hard]],
    "HardNegativeScores" -> hardScores|>,
    If[guided, <|"AuditProof" -> proof, "WarmFullBoardAudit" -> guidance|>, <||>]];
  rowBytes = ByteCount[KeyTake[row, {"State", "Candidates", "Target", "LegalMask"}]];
  If[rowBytes > config["MaxTrainingRowBytes"], Return[compactScaleProblem["TrainingRowMemoryLimit",
    <|"RowBytes" -> rowBytes, "ByteLimit" -> config["MaxTrainingRowBytes"], "Sampling" -> sampling|>]]];
  Join[row, <|"RowStorageBytes" -> rowBytes|>]
];

compactScaleGenerationJob[job_Association, config_Association] := Module[
  {settings = compactScaleConfig[config], pair = job["Pair"], rows = {}, skipped = {},
    expr, original, before, next, legal, choices, choice, row, actual = 0, started = AbsoluteTime[], result,
    provenance = KeyTake[job, {"Order", "Kind", "Mode", "AmplitudeIndex", "ReferenceName", "Sequence", "Seed", "Steps"}],
    heldout},
  If[job["Mode"] === "SimplificationRoute", Return[compactScaleRouteGenerationJob[job, config]]];
  result = TimeConstrained[
    BlockRandom[SeedRandom[Hash[{settings["Seed"], job["AmplitudeIndex"], job["Seed"], "CompactScaleScramble"}]];
      original = canonicalAmplitude[pair[[2]], pair[[1]]]; expr = original;
      Do[
        legal = TimeConstrained[compactScaleLegal[expr, pair[[1]], settings], settings["RowSeconds"],
          compactScaleProblem["LegalEnumerationTimeout"]];
        If[KeyExistsQ[legal, "Reason"], AppendTo[skipped, Join[<|"Step" -> step|>, legal]]; Break[]];
        choices = Select[legal["Candidates"],
          #["Name"] =!= "Stop" && TrueQ[Lookup[#, "ScrambleEligible", True]] &];
        If[choices === {}, AppendTo[skipped, compactScaleProblem["NoScrambleMoves", <|"Step" -> step|>]]; Break[]];
        before = expr;
        choice = RandomChoice[choices];
        next = TimeConstrained[applyCandidate[expr, choice], settings["RowSeconds"], $Failed];
        If[next === $Failed, AppendTo[skipped, compactScaleProblem["ApplyTimeout", <|"Step" -> step|>]]; Break[]];
        expr = next; actual++;
        row = TimeConstrained[compactScaleRow[expr, pair[[1]], before, settings], settings["RowSeconds"],
          compactScaleProblem["ReverseRowTimeout"]];
        If[KeyExistsQ[row, "Reason"], AppendTo[skipped, Join[<|"Step" -> step|>, row]],
          AppendTo[rows, Join[row, <|"Provenance" -> Join[provenance, <|"Step" -> step,
            "StopExample" -> False, "ForwardMove" -> choice["Name"]|>]|>]]],
        {step, job["Steps"]}];
      row = TimeConstrained[compactScaleRow[original, pair[[1]], original, settings], settings["RowSeconds"],
        compactScaleProblem["StopRowTimeout"]];
      If[KeyExistsQ[row, "Reason"], AppendTo[skipped, Join[<|"Step" -> 0|>, row]],
        AppendTo[rows, Join[row, <|"Provenance" -> Join[provenance, <|"Step" -> 0, "StopExample" -> True|>]|>]]];
      True], settings["JobSeconds"], AppendTo[skipped, compactScaleProblem["JobTimeout"]]; False];
  If[!TrueQ[result], rows = {}];
  heldout = If[MemberQ[{"Training", "Train"}, job["Kind"]], <||>,
    <|"Pair" -> pair, "LegPermutation" -> job["LegPermutation"],
      "Original" -> If[ValueQ[original], original, None],
      "Expression" -> If[ValueQ[expr], expr, None]|>];
  Join[provenance, heldout, <|"Rows" -> rows, "Skipped" -> skipped, "ActualSteps" -> actual,
    "Completed" -> TrueQ[result] && actual === job["Steps"],
    "Seconds" -> N[AbsoluteTime[] - started], "AcceptedRows" -> Length[rows],
    "SkippedReasons" -> Counts[Lookup[skipped, "Reason", {}]],
    "CandidateSampling" -> compactScaleSamplingReport[rows]|>]
];

compactScaleRouteGenerationJob[job_Association, config_Association] := Block[
  {$onShellChannels = trainingJobChannels[job],
    $useMomentumConservation = Lookup[job, "MomentumConservation", $useMomentumConservation]},
  Module[{settings = compactScaleConfig[config], verified, rows = {}, failure = None,
      failureDetails = <||>, latest = <||>, started = AbsoluteTime[], completed, provenance, progress, guidanceSteps},
    provenance = KeyTake[job, {"Order", "Kind", "Mode", "AmplitudeIndex", "ReferenceName", "Sequence",
      "Seed", "Steps", "ExternalLegCount", "Curated", "ModelRecovery", "RecoveryProof",
      "RecoverySourceStep", "EndpointVerified"}];
    guidanceSteps = compactScaleGuidanceSteps[job, settings];
    progress = Function[event,
      latest = event;
      compactScaleRouteProgress[job, event, settings]];
    completed = TimeConstrained[
      verified = verifiedSimplificationRoute[job, <|
        "StepTimeLimit" -> settings["RowSeconds"],
        "ProgressFunction" -> progress,
        "FailureFunction" -> Function[detail,
          failureDetails = detail; failure = detail["Reason"];
          compactScaleRouteProgress[job, detail, settings]],
        "RowFunction" -> Function[{state, target, candidates, labels, step}, Module[{row},
          row = compactScaleRow[state, job["Pair"][[1]], target,
            Join[settings, <|"GuidanceState" -> MemberQ[guidanceSteps, step]|>], candidates, labels];
          If[KeyExistsQ[row, "Reason"], row,
            Join[row, <|"Provenance" -> Join[provenance,
              <|"Step" -> step, "StopExample" -> (step === Length[job["States"]])|>,
              compactScaleRoutePhase[Lookup[job, "RouteIdentities", {}], step]]|>]]]]|>];
      If[verified === $Failed,
        If[failure === None, failure = "IllegalSimplificationRoute"]; False,
        rows = verified["Rows"]; True],
      settings["RouteJobSeconds"],
      failure = "CuratedJobTimeout";
      failureDetails = Join[<|"RouteName" -> job["Name"],
        "Sequence" -> Lookup[job, "Sequence", Lookup[job, "Seed", 0]],
        "LegPermutation" -> Lookup[job, "LegPermutation", <||>],
        "Steps" -> Length[job["States"]]|>, KeyTake[latest, {"Phase", "Step"}],
        <|"Reason" -> failure, "Seconds" -> N[AbsoluteTime[] - started]|>];
      compactScaleRouteProgress[job, failureDetails, settings]; False];
    If[!TrueQ[completed], rows = {}];
    Join[provenance, <|"Rows" -> rows, "Skipped" -> {}, "ActualSteps" -> Max[0, Length[rows] - 1],
      "Completed" -> TrueQ[completed], "Failure" -> failure, "FailureDetails" -> failureDetails,
      "Seconds" -> N[AbsoluteTime[] - started], "AcceptedRows" -> Length[rows], "SkippedReasons" -> <||>,
      "CandidateSampling" -> compactScaleSamplingReport[rows]|>]
  ]
];

(* ::Section::Closed:: *)
(*Keyed Dataset Merging*)
(* Particle-token boards have row-local zero padding; keys compare the actual token sequence. *)
compactScaleCanonicalFeature[feature_] := Module[{value, last},
  value = compactScaleDenseCandidates[feature];
  If[Length[Dimensions[value]] =!= 2, Return[value]];
  last = Length[value];
  While[last > 1 && MinMax[value[[last]]] === {0., 0.}, last--];
  compactScalePacked[Take[value, last]]
];
compactScaleKeyedRowQ[row_] := Module[{n, keys, operations, target},
  n = Lookup[row, "ActionCount", 0]; keys = Lookup[row, "CandidateKeys", {}];
  operations = Lookup[row, "CandidateOperations", {}]; target = Lookup[row, "Target", {}];
  IntegerQ[n] && n > 0 && Length[keys] === n && DuplicateFreeQ[keys] &&
    AllTrue[keys, StringMatchQ[#, RegularExpression["[0-9a-f]{64}"]] &] &&
    Length[operations] === n && VectorQ[operations, StringQ] &&
    Last[operations] === "Stop" && Count[operations, "Stop"] === 1 &&
    Length[target] === n && VectorQ[target, NumberQ[#] && TrueQ[Im[#] == 0 && Abs[#] < Infinity] &] && Min[target] >= 0. &&
    Abs[Total[target] - 1.] < 0.00001 &&
    row["LegalMask"] === compactScalePacked[ConstantArray[1., n]] &&
    First[Dimensions[row["Candidates"]]] === n && compactScaleCandidateStorageQ[row["Candidates"]] &&
    IntegerQ[Lookup[row, "FullActionCount", 0]] && row["FullActionCount"] >= n &&
    StringQ[Lookup[row, "FullCandidateSignature", None]] &&
    StringMatchQ[row["FullCandidateSignature"], RegularExpression["[0-9a-f]{64}"]] &&
    IntegerQ[Lookup[row, "TrainingCandidateBudget", 0]] && row["TrainingCandidateBudget"] > 0 &&
    IntegerQ[Lookup[row, "SamplingSeed", None]]
];

compactScaleMergeKeyedGroup[group_List, config_Association] := Module[
  {first = First[group], sources = group, curated, curatedMoves, stopRows, stopKey, entries,
    locations, keys, selectedKeys, positiveKeys, keyOperations, feature, reference, candidateFeatures,
    width, tokens, targets, selection, sampling, budget, seed, fullCount, merged, rowBytes,
    observations, hardKeys, hardScores, proofs, warmAudits, mismatch = None, stats = <|"UnionTargetStates" -> 0, "StopPriorityStates" -> 0,
      "DiscardedReverseObservations" -> 0, "CuratedPriorityStates" -> 0, "DiscardedStopObservations" -> 0|>},
  curated = AnyTrue[group, TrueQ[Lookup[Lookup[#, "Provenance", <||>], "Curated", False]] &];
  If[!AllTrue[group, compactScaleKeyedRowQ] || !AllTrue[group,
      #["State"] === first["State"] && #["FullActionCount"] === first["FullActionCount"] &&
      #["FullCandidateSignature"] === first["FullCandidateSignature"] &],
    Return[compactScaleProblem["FullLegalBoardMismatch", <|"Curated" -> curated|>]]];
  stopKey = Last[first["CandidateKeys"]];
  If[!AllTrue[group, Last[#["CandidateKeys"]] === stopKey &],
    Return[compactScaleProblem["StopActionMismatch", <|"Curated" -> curated|>]]];
  If[Length[group] === 1 &&
      Lookup[config, "TrainingCandidateBudget", first["TrainingCandidateBudget"]] === first["TrainingCandidateBudget"] &&
      Lookup[config, "Seed", first["SamplingSeed"]] === first["SamplingSeed"] &&
      Lookup[config, "CandidateStorage", If[Head[first["Candidates"]] === SparseArray, "Sparse", "Packed"]] ===
        If[Head[first["Candidates"]] === SparseArray, "Sparse", "Packed"],
    rowBytes = ByteCount[KeyTake[first, {"State", "Candidates", "Target", "LegalMask"}]];
    If[rowBytes > Lookup[config, "MaxTrainingRowBytes", Infinity],
      Return[compactScaleProblem["TrainingRowMemoryLimit", <|"RowBytes" -> rowBytes,
        "ByteLimit" -> config["MaxTrainingRowBytes"], "Sampling" -> first["Sampling"], "Curated" -> curated|>]]];
    Return[<|"Row" -> Join[first, <|"ObservationCount" -> 1,
      "Provenance" -> {Lookup[first, "Provenance", <||>]}, "RowStorageBytes" -> rowBytes|>], "Stats" -> stats|>]];
  stopRows = Select[group, TrueQ[Last[#["Target"]] > 0.] &];
  curatedMoves = Select[group, TrueQ[Lookup[Lookup[#, "Provenance", <||>], "Curated", False]] &&
    !TrueQ[Lookup[Lookup[#, "Provenance", <||>], "StopExample", False]] &];
  If[stopRows =!= {} && Length[stopRows] < Length[group],
    If[curatedMoves =!= {},
      stats["CuratedPriorityStates"] = 1; stats["DiscardedStopObservations"] = Length[stopRows];
      sources = Select[group, !TrueQ[Last[#["Target"]] > 0.] &],
      stats["StopPriorityStates"] = 1; stats["DiscardedReverseObservations"] = Length[group] - Length[stopRows];
      sources = stopRows]];
  positiveKeys = Union[Flatten[#["CandidateKeys"][[compactScalePositiveSlots[#["Target"]]]] & /@ sources]];
  hardKeys = Complement[Union[Flatten[Lookup[group, "HardNegativeKeys", {}]]], positiveKeys, {stopKey}];
  hardScores = Merge[Lookup[group, "HardNegativeScores", <||>], Max];
  hardKeys = SortBy[hardKeys, {-Lookup[hardScores, #, 0.], #} &];
  If[positiveKeys === {}, Return[compactScaleProblem["NoLegalReverse"]]];
  If[Length[DeleteDuplicates[Sort[#["CandidateKeys"][[compactScalePositiveSlots[#["Target"]]]]] & /@ sources]] > 1,
    stats["UnionTargetStates"] = 1];
  entries = Flatten[MapIndexed[Function[{source, index},
    MapIndexed[Function[{key, slot}, {key, {First[index], First[slot]}}], source["CandidateKeys"]]], group], 1];
  locations = GroupBy[entries, First -> Last]; keys = Keys[locations];
  keyOperations = Association[];
  Do[
    reference = First[locations[key]];
    feature = compactScaleCanonicalFeature[group[[reference[[1]]]]["Candidates"][[reference[[2]]]]];
    keyOperations[key] = group[[reference[[1]]]]["CandidateOperations"][[reference[[2]]]];
    If[!AllTrue[Rest[locations[key]], Function[location,
        group[[location[[1]]]]["CandidateOperations"][[location[[2]]]] === keyOperations[key] &&
          compactScaleCanonicalFeature[group[[location[[1]]]]["Candidates"][[location[[2]]]]] === feature]],
      mismatch = compactScaleProblem["LegalDescriptorMismatch", <|"ActionKey" -> key, "Curated" -> curated|>]; Break[]],
    {key, keys}];
  If[mismatch =!= None, Return[mismatch]];
  budget = Lookup[config, "TrainingCandidateBudget", Max[Lookup[group, "TrainingCandidateBudget"]]];
  hardKeys = Take[hardKeys, UpTo[Min[Lookup[config, "HardNegativeCount", Length[hardKeys]],
    Max[0, budget - Length[Union[positiveKeys, {stopKey}]]]]]];
  seed = Lookup[config, "Seed", first["SamplingSeed"]];
  If[!AllTrue[group, #["SamplingSeed"] === seed &],
    Return[compactScaleProblem["SamplingSeedMismatch", <|"Curated" -> curated|>]]];
  selection = compactScaleSelection[keys, Flatten[Position[MemberQ[positiveKeys, #] & /@ keys, True]],
    First[FirstPosition[keys, stopKey]], first["StateKey"], budget, seed,
    Lookup[AssociationThread[keys, Range[Length[keys]]], hardKeys]];
  If[KeyExistsQ[selection, "Reason"], Return[selection]];
  selectedKeys = keys[[selection["Indices"]]];
  fullCount = first["FullActionCount"];
  sampling = Join[selection["Sampling"], <|"FullActionCount" -> fullCount,
    "AvailableActionCount" -> Length[keys], "FullNegativeCount" -> fullCount - Length[positiveKeys],
    "DroppedNegativeCount" -> fullCount - Length[selectedKeys], "Sampled" -> (Length[selectedKeys] < fullCount),
    "NegativePoolExhausted" -> (Length[keys] < Min[fullCount, Max[budget, Length[Union[positiveKeys, {stopKey}]]]]),
    "RawActionThresholdExceeded" -> AnyTrue[Lookup[group, "Sampling", <||>], TrueQ[Lookup[#, "RawActionThresholdExceeded", False]] &]|>];
  If[TrueQ[sampling["Sampled"]],
    selectedKeys = Append[Sort[DeleteCases[selectedKeys, stopKey]], stopKey]];
  If[Last[selectedKeys] =!= stopKey,
    selectedKeys = Append[DeleteCases[selectedKeys, stopKey], stopKey]];
  candidateFeatures = Map[Function[key,
    reference = First[locations[key]];
    compactScaleCanonicalFeature[group[[reference[[1]]]]["Candidates"][[reference[[2]]]]]], selectedKeys];
  If[Length[Dimensions[First[candidateFeatures]]] === 2,
    tokens = Max[Length /@ candidateFeatures]; width = Last[Dimensions[First[candidateFeatures]]];
    candidateFeatures = PadRight[#, {tokens, width}, 0.] & /@ candidateFeatures];
  candidateFeatures = compactScaleStoreCandidates[candidateFeatures,
    Lookup[config, "CandidateStorage", If[Head[first["Candidates"]] === SparseArray, "Sparse", "Packed"]]];
  targets = compactScalePacked[N[Boole[MemberQ[positiveKeys, #]] & /@ selectedKeys]/Length[positiveKeys]];
  observations = DeleteDuplicates[Lookup[sources, "Provenance", <||>]];
  proofs = DeleteDuplicates[Flatten[Map[Function[source,
    Lookup[source, "AuditProofs", If[KeyExistsQ[source, "AuditProof"], {source["AuditProof"]}, {}]]], sources], 1]];
  warmAudits = DeleteDuplicates[Flatten[Map[Function[source,
    Lookup[source, "WarmFullBoardAudits", If[KeyExistsQ[source, "WarmFullBoardAudit"], {source["WarmFullBoardAudit"]}, {}]]], sources], 1]];
  merged = Join[KeyDrop[first, {"Provenance", "AuditProof", "AuditProofs", "WarmFullBoardAudit", "WarmFullBoardAudits"}],
    <|"Candidates" -> candidateFeatures,
    "CandidateKeys" -> selectedKeys, "CandidateOperations" -> Lookup[keyOperations, selectedKeys],
    "CandidateSignature" -> If[selectedKeys === first["CandidateKeys"], first["CandidateSignature"],
      IntegerString[Hash[{"CompactSelectedActionKeys", selectedKeys}, "SHA256"], 16, 64]],
    "Target" -> targets, "LegalMask" -> compactScalePacked[ConstantArray[1., Length[selectedKeys]]],
    "ActionCount" -> Length[selectedKeys], "TrainingCandidateBudget" -> budget, "SamplingSeed" -> seed,
    "TargetOperations" -> Union[Lookup[keyOperations, positiveKeys]],
    "ObservationCount" -> Length[group], "Provenance" -> observations, "Sampling" -> sampling,
    "Sampled" -> sampling["Sampled"], "MandatoryActionCount" -> sampling["MandatoryActionCount"],
    "CandidateBudgetExpanded" -> sampling["CandidateBudgetExpanded"], "HardNegativeKeys" -> hardKeys,
    "HardNegativeScores" -> KeyTake[hardScores, hardKeys],
    "AuditProofs" -> proofs, "WarmFullBoardAudits" -> warmAudits|>];
  rowBytes = ByteCount[KeyTake[merged, {"State", "Candidates", "Target", "LegalMask"}]];
  If[rowBytes > Lookup[config, "MaxTrainingRowBytes", Infinity],
    Return[compactScaleProblem["TrainingRowMemoryLimit", <|"RowBytes" -> rowBytes,
      "ByteLimit" -> config["MaxTrainingRowBytes"], "Sampling" -> sampling, "Curated" -> curated|>]]];
  <|"Row" -> Join[merged, <|"RowStorageBytes" -> rowBytes|>], "Stats" -> stats|>
];

compactScaleMergeRows[raw_List, config_Association:<||>] := Module[
  {groups, rows = {}, rejected = {}, unionStates = 0, stopPriority = 0, discardedReverse = 0,
    curatedPriority = 0, discardedStops = 0, curated, curatedMoves,
    first, sources, positives, target, collision, merged, stopRows, observations, packedBytes, compactRows,
    keyed, stats},
  groups = GatherBy[raw, #["StateKey"] &];
  Do[
    first = First[group]; sources = group;
    curated = AnyTrue[group, TrueQ[Lookup[Lookup[#, "Provenance", <||>], "Curated", False]] &];
    If[AnyTrue[group, KeyExistsQ[#, "CandidateKeys"] &],
      If[!AllTrue[group, KeyExistsQ[#, "CandidateKeys"] &],
        AppendTo[rejected, <|"StateKey" -> first["StateKey"], "Reason" -> "MixedLegacyActionKeys",
          "Observations" -> Length[group], "Curated" -> curated|>]; Continue[]];
      keyed = compactScaleMergeKeyedGroup[group, config];
      If[KeyExistsQ[keyed, "Reason"],
        AppendTo[rejected, Join[<|"StateKey" -> first["StateKey"], "Observations" -> Length[group]|>, keyed]];
        Continue[]];
      merged = keyed["Row"]; stats = keyed["Stats"];
      collision = compactActionDescriptorCollisions[compactScaleDenseCandidates[merged["Candidates"]], merged["Target"]];
      If[collision === $Failed || collision["MixedTargetGroups"] > 0,
        AppendTo[rejected, <|"StateKey" -> first["StateKey"], "Reason" -> "DescriptorTargetCollision",
          "Observations" -> Length[group], "Collision" -> collision, "Curated" -> curated|>]; Continue[]];
      unionStates += stats["UnionTargetStates"]; stopPriority += stats["StopPriorityStates"];
      discardedReverse += stats["DiscardedReverseObservations"]; curatedPriority += stats["CuratedPriorityStates"];
      discardedStops += stats["DiscardedStopObservations"];
      AppendTo[rows, merged]; Continue[]];
    If[!AllTrue[group, #["State"] === first["State"] &&
        compactScaleCandidatesEqualQ[#["Candidates"], first["Candidates"]] && #["ActionCount"] === first["ActionCount"] &&
        #["CandidateSignature"] === first["CandidateSignature"] && #["LegalMask"] === first["LegalMask"] &],
      AppendTo[rejected, <|"StateKey" -> first["StateKey"], "Reason" -> "LegalDescriptorMismatch",
        "Observations" -> Length[group], "Curated" -> curated|>]; Continue[]];
    stopRows = Select[group, TrueQ[#["Target"][[#["ActionCount"]]] > 0.] &];
    curatedMoves = Select[group, TrueQ[Lookup[Lookup[#, "Provenance", <||>], "Curated", False]] &&
      !TrueQ[Lookup[Lookup[#, "Provenance", <||>], "StopExample", False]] &];
    If[stopRows =!= {} && Length[stopRows] < Length[group],
      If[curatedMoves =!= {},
        curatedPriority++; discardedStops += Length[stopRows]; sources = Complement[group, stopRows],
        stopPriority++; discardedReverse += Length[group] - Length[stopRows]; sources = stopRows]];
    positives = Union[Flatten[Flatten[Position[#["Target"], _?Positive]] & /@ sources]];
    If[Length[DeleteDuplicates[Lookup[sources, "Target"]]] > 1, unionStates++];
    target = ReplacePart[ConstantArray[0., first["ActionCount"]], Thread[positives -> N[1./Length[positives]]]];
    collision = compactActionDescriptorCollisions[compactScaleDenseCandidates[first["Candidates"]], target];
    If[collision === $Failed || collision["MixedTargetGroups"] > 0,
      AppendTo[rejected, <|"StateKey" -> first["StateKey"], "Reason" -> "DescriptorTargetCollision",
        "Observations" -> Length[group], "Collision" -> collision, "Curated" -> curated|>]; Continue[]];
    observations = DeleteDuplicates[Lookup[sources, "Provenance", <||>]];
    merged = Join[KeyDrop[first, "Provenance"], <|"Target" -> compactScalePacked[target],
      "TargetOperations" -> Union[Flatten[Lookup[sources, "TargetOperations", {}]]],
      "ObservationCount" -> Length[group], "Provenance" -> observations|>];
    AppendTo[rows, merged], {group, groups}];
  packedBytes = Total[ByteCount[KeyTake[#, {"State", "Candidates", "Target", "LegalMask"}]] & /@ rows];
  compactRows = AllTrue[rows,
    AllTrue[Values[KeyTake[#, {"State", "Target", "LegalMask"}]], Developer`PackedArrayQ] &&
      compactScaleCandidateStorageQ[#["Candidates"]] &];
  <|"Rows" -> rows, "RawObservations" -> Length[raw], "UniqueStates" -> Length[groups],
    "AcceptedStates" -> Length[rows], "UnionTargetStates" -> unionStates,
    "StopPriorityStates" -> stopPriority, "DiscardedReverseObservations" -> discardedReverse,
    "CuratedPriorityStates" -> curatedPriority, "DiscardedStopObservations" -> discardedStops,
    "RejectedStates" -> Length[rejected], "Rejected" -> rejected, "PackedRowBytes" -> packedBytes,
    "PackedRows" -> AllTrue[rows, AllTrue[Values[KeyTake[#, {"State", "Candidates", "Target", "LegalMask"}]],
      Developer`PackedArrayQ] &],
    "CompactRows" -> compactRows, "ValidCandidateStorageRows" -> compactRows,
    "DenseCandidateRows" -> Count[Lookup[rows, "Candidates"], _?Developer`PackedArrayQ],
    "SparseCandidateRows" -> Count[Lookup[rows, "Candidates"], _SparseArray],
    "RowStorageBytes" -> packedBytes,
    "CandidateSampling" -> compactScaleSamplingReport[rows]|>
];
