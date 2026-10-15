(* ::Section::Closed:: *)
(*Compact Training Startup and Configuration*)
(* Stage this module as compact-scale-run.wl for the watchdog. *)
Global`particleStartupTime = AbsoluteTime[];
particleRun = DirectoryName[$InputFileName];
Put[$ProcessID, FileNameJoin[{particleRun, "kernel.pid"}]];
particleConfig = Import[FileNameJoin[{particleRun, "config.json"}], "RawJSON"];
If[!AssociationQ[particleConfig], Print["Missing training configuration."]; Quit[1]];
particleRoot = ExpandFileName[Lookup[particleConfig, "WorkerRoot", "~/hepcat-move-tests"]];
Global`$HEPCATpath = FileNameJoin[{particleRoot, "source"}];
Block[{Print}, Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
  Get[FileNameJoin[{particleRun, "unscrambling.wl"}]]];

Unscrambling`Private`particleTimeBudgetPlan[configuration_Association] := Module[
  {pipeline, requested, preparation, reserve, grace, required, wall, finitePositive},
  finitePositive = Function[NumberQ[#] && TrueQ[Im[#] == 0] && TrueQ[0 < # < Infinity]];
  pipeline = Lookup[configuration, "PipelineSeconds", 39600];
  requested = Lookup[configuration, "NNTimeSeconds", 36000];
  preparation = Lookup[configuration, "PreparationSeconds", Automatic];
  reserve = Lookup[configuration, "FinalizationReserveSeconds", Automatic];
  wall = Lookup[configuration, "WallSeconds", Lookup[configuration, "HardWallSeconds", 43200]];
  If[!AllTrue[{pipeline, requested}, finitePositive],
    Return[<|"Passed" -> False, "Reason" -> "InvalidPipelineOrNNBudget"|>]];
  If[!finitePositive[wall], Return[<|"Passed" -> False, "Reason" -> "InvalidWatchdogWallBudget"|>]];
  grace = Min[300., Max[30., 0.1 requested]];
  If[preparation === Automatic, preparation = 0.];
  If[reserve === Automatic, reserve = Min[7200., 0.1 pipeline, Max[0., pipeline - requested - grace]]];
  If[!NumberQ[preparation] || !TrueQ[Im[preparation] == 0] || !TrueQ[0 <= preparation < Infinity] ||
      !finitePositive[reserve],
    Return[<|"Passed" -> False, "Reason" -> "InvalidPreparationOrFinalizationBudget"|>]];
  required = preparation + requested + reserve + grace;
  <|"Passed" -> TrueQ[pipeline >= required && wall >= required + 30.],
    "Reason" -> Which[pipeline < required, "InconsistentPipelineBudget",
      wall < required + 30., "InconsistentWatchdogWallBudget", True, "BudgetFits"],
    "PreparationSeconds" -> N[preparation], "RequestedNNTimeSeconds" -> N[requested],
    "FinalizationReserveSeconds" -> N[reserve], "NNTimeoutGraceSeconds" -> N[grace],
    "PipelineSeconds" -> N[pipeline], "RequiredPipelineSeconds" -> N[required],
    "WatchdogWallSeconds" -> N[wall], "RequiredWatchdogWallSeconds" -> N[required + 30.]|>
];

Unscrambling`Private`particleDeadlineAbsoluteTime[value_] := Module[{instant},
  If[MemberQ[{None, Null}, value], Return[Infinity]];
  If[!StringQ[value] || !StringMatchQ[value,
      RegularExpression[".*(?:Z|[+-][0-9]{2}:[0-9]{2})"]], Return[$Failed]];
  instant = Quiet[Check[AbsoluteTime[DateObject[value, TimeZone -> 0]], $Failed]];
  If[NumberQ[instant] && TrueQ[0 < instant < Infinity], N[instant], $Failed]
];

Unscrambling`Private`particleNNBudgetAllowance[plan_Association, started_, now_, deadline_:Infinity, wall_:Automatic] := Module[
  {pipelineRemaining, deadlineRemaining, wallRemaining, wallBudget, available, effective, minimum, requested},
  wallBudget = If[wall === Automatic, Lookup[plan, "WatchdogWallSeconds", Infinity], wall];
  If[!TrueQ[Lookup[plan, "Passed", False]] || !AllTrue[{started, now}, NumberQ] ||
      !TrueQ[started <= now] || !(deadline === Infinity ||
        (NumberQ[deadline] && TrueQ[Im[deadline] == 0] && TrueQ[Abs[deadline] < Infinity])) ||
      !(wallBudget === Infinity || (NumberQ[wallBudget] && TrueQ[Im[wallBudget] == 0] &&
        TrueQ[0 < wallBudget < Infinity])),
    Return[<|"Passed" -> False, "Reason" -> "InvalidTrainingBudgetClock"|>]];
  pipelineRemaining = Max[0., started + plan["PipelineSeconds"] - now];
  (* Match the watchdog's cleanup margin while retaining a separate finalization reserve. *)
  deadlineRemaining = If[deadline === Infinity, Infinity, Max[0., deadline - 30. - now]];
  wallRemaining = If[wallBudget === Infinity, Infinity, Max[0., started + wallBudget - 30. - now]];
  available = Min[pipelineRemaining, deadlineRemaining, wallRemaining];
  requested = plan["RequestedNNTimeSeconds"];
  effective = Min[requested, Max[0., available - plan["FinalizationReserveSeconds"] - plan["NNTimeoutGraceSeconds"]]];
  minimum = Min[requested, 60.];
  Join[plan, <|"Passed" -> TrueQ[effective >= minimum],
    "Reason" -> Which[effective < minimum, "InsufficientNNWindow",
      effective < requested, "ReducedNNBudget", True, "FullNNBudget"],
    "EffectiveNNTimeSeconds" -> N[effective], "NNBudgetLimited" -> TrueQ[effective < requested],
    "PipelineSecondsRemaining" -> N[pipelineRemaining],
    "DeadlineSecondsRemaining" -> If[deadlineRemaining === Infinity, Null, N[deadlineRemaining]],
    "WallSecondsRemaining" -> If[wallRemaining === Infinity, Null, N[wallRemaining]],
    "MinimumNNTimeSeconds" -> N[minimum]|>]
];

particleStartupBudgetPlan = Unscrambling`Private`particleTimeBudgetPlan[particleConfig];
If[!TrueQ[particleStartupBudgetPlan["Passed"]],
  Print["Invalid training time budgets: ", particleStartupBudgetPlan]; Quit[1]];
particleCatalog = Get[FileNameJoin[{particleRun, "training-amplitudes.wl"}]];
If[!ListQ[particleCatalog], Print["Invalid training catalog."]; Quit[1]];
particleRoutes = If[TrueQ[Lookup[particleConfig, "UseSimplificationRoutes", True]], trainingSimplificationRoutes, {}];
If[!Unscrambling`Private`validSimplificationRoutesQ[particleRoutes], Print["Invalid curated simplification routes."]; Quit[1]];
particleRequestedRoutes = Lookup[particleConfig, "SimplificationRouteNames", All];
If[particleRequestedRoutes =!= All,
  If[!ListQ[particleRequestedRoutes] || particleRequestedRoutes === {} ||
      !VectorQ[particleRequestedRoutes, StringQ] || !DuplicateFreeQ[particleRequestedRoutes] ||
      Complement[particleRequestedRoutes, Lookup[particleRoutes, "Name"]] =!= {},
    Print["Invalid curated route selection."]; Quit[1]];
  particleRoutes = Select[particleRoutes, MemberQ[particleRequestedRoutes, #["Name"]] &]];

Begin["Unscrambling`Private`"];
Get[FileNameJoin[{Global`particleRun, "compact-scale-data.wl"}]];
Get[FileNameJoin[{Global`particleRun, "compact-scale-training.wl"}]];
$onShellChannels = Automatic; $useMomentumConservation = True;
particleStarted = Global`particleStartupTime;
particleSettings = compactScaleConfig[Join[<|"Scrambles" -> 12, "HoldoutScrambles" -> 2,
  "Depths" -> {3, 4, 5, 6}, "Seed" -> 20261001, "Kernels" -> 24,
  "MaxActions" -> 1024, "TrainingCandidateBudget" -> 256, "TrainingUpdateCandidateBudget" -> 128,
  "MaxTrainingTensorMiB" -> 256, "MaxTokens" -> 128, "MaxStateBytes" -> 50000,
  "MaxExpressionLeaves" -> 10000, "RowSeconds" -> 10, "JobSeconds" -> 90,
  "GenerationSeconds" -> 2400, "GenerationChunkJobs" -> Automatic, "MaxDatasetGiB" -> 24,
  "NNTimeSeconds" -> 36000, "MaxTrainingRounds" -> 1000000,
  "LearningRate" -> 0.001, "RandomSeed" -> 1, "ProgressEverySeconds" -> 30,
  "CheckEverySeconds" -> 1200, "CheckpointSeconds" -> 180, "FitSampleSize" -> 64,
  "CheckpointFitSampleSize" -> 8, "MinimumReferenceReverseCoverage" -> 1.,
  "MaximumRejectedStateFraction" -> 0.05, "MinimumTrainingStates" -> 100,
  "CaseSeconds" -> 30, "ValidationMaxCases" -> 24, "ValidationMaxSteps" -> 16,
  "FivePointScrambles" -> 12, "SimplificationRoutes" -> Global`particleRoutes, "RoutePermutations" -> 4,
  "CuratedTrainingFraction" -> Automatic,
  "RouteDiagnosticSeconds" -> 1800,
  "PreparationSeconds" -> Automatic, "FinalizationReserveSeconds" -> Automatic,
  "PipelineSeconds" -> 39600|>, Global`particleConfig]];
particleSettings["RoutePermutations"] = compactScaleRoutePermutationCount[particleSettings["RoutePermutations"]];
particleJSON[name_, value_] := compactScaleAtomicExport[FileNameJoin[{Global`particleRun, name}], value, "RawJSON"];
particleWXF[name_, value_] := compactScaleAtomicExport[FileNameJoin[{Global`particleRun, name}], value, "WXF"];
particleStage[name_, detail_:<||>] := Module[{record},
  record = particleCheckpointJSONValue[Join[<|"Stage" -> name, "Timestamp" -> DateString["ISODateTime"],
    "KernelPID" -> $ProcessID, "PipelineSeconds" -> N[AbsoluteTime[] - particleStarted]|>, detail]];
  If[!particleJSON["stage.json", record], Print["Stage checkpoint write failed."]; Abort[]];
  Print[DateString["ISODateTime"], " STAGE: ", record]
];
particleFail[reason_, detail_:<||>] := (
  particleStage["Failed", Join[<|"Reason" -> reason|>, detail]];
  If[Kernels[] =!= {}, CloseKernels[]]; Quit[1]);

particleGenerationChunkSize[requested_, workers_Integer?Positive] :=
  If[requested === Automatic, 2 workers,
    If[IntegerQ[requested] && requested > 0, Min[requested, 2 workers], $Failed]];

particleCollisions[features_, target_] := Module[{groups, mixed},
  groups = GatherBy[Range[Length[features]], features[[#]] &];
  mixed = Select[groups, AnyTrue[target[[#]], Positive] && AnyTrue[target[[#]], # <= 0. &] &];
  <|"Groups" -> Length[groups], "DuplicateGroups" -> Count[Length /@ groups, _?(# > 1 &)],
    "MixedTargetGroups" -> Length[mixed]|>
];
particleMerge[raw_, settings_:<||>] := Block[{compactActionDescriptorCollisions = particleCollisions},
  compactScaleMergeRows[raw, settings]];
particleTrainingTensorBytes[count_Integer?Positive, tokens_Integer?Positive] :=
  8 count (166 tokens + 2);
particleRowQ[row_, count_, tokens_] := Module[{n, d, present, candidates},
  If[!AssociationQ[row], Return[False]];
  n = Lookup[row, "ActionCount", 0]; d = Dimensions[Lookup[row, "Candidates", {}]];
  If[!IntegerQ[n] || n < 1 || n > count || Length[d] =!= 3 || d[[1]] =!= n ||
      d[[2]] < 1 || d[[2]] > tokens || d[[3]] =!= 166, Return[False]];
  If[!compactScaleCandidateStorageQ[row["Candidates"]], Return[False]];
  candidates = compactScaleDenseCandidates[row["Candidates"]];
  present = Total /@ Flatten[candidates[[All, All, 147 ;; 149]], 1];
  AllTrue[Values[KeyTake[row, {"State", "Target", "LegalMask"}]], Developer`PackedArrayQ] &&
    compactScaleByteVectorQ[row["State"]] && Length[row["State"]] <= particleSettings["MaxStateBytes"] &&
    compactScaleFiniteVectorQ[Flatten[candidates]] &&
    AllTrue[present, TrueQ[# == 0. || # == 1.] &] &&
    Length[row["Target"]] === n && compactScaleFiniteVectorQ[row["Target"]] &&
    Min[row["Target"]] >= 0. && Abs[Total[row["Target"]] - 1.] < 0.00001 &&
    Length[row["LegalMask"]] === n && MinMax[row["LegalMask"]] === {1., 1.}
];
particleTrainRow[row_, count_, tokens_] := <|"State" -> row["State"],
  "Candidates" -> Developer`ToPackedArray[PadRight[compactScaleDenseCandidates[row["Candidates"]], {count, tokens, 166}, 0.]],
  "Target" -> Developer`ToPackedArray[PadRight[row["Target"], count, 0.]],
  "LegalMask" -> Developer`ToPackedArray[PadRight[row["LegalMask"], count, 0.]]|>;
(* ::Section::Closed:: *)
(*Fixed Training Views and Sampling*)
(* Every update retains all teachers and Stop; guided alternatives receive
   the remaining budget before random alternatives. *)
particleUpdateRow[row_, budget_Integer?Positive] := Module[{mandatory, alternatives, selected, n},
  n = row["ActionCount"];
  If[n <= budget, Return[row]];
  mandatory = Union[Flatten[Position[row["Target"], _?Positive]], {n}];
  alternatives = If[KeyExistsQ[row, "CandidateKeys"],
    Lookup[AssociationThread[row["CandidateKeys"], Range[n]], Lookup[row, "HardNegativeKeys", {}], Nothing], {}];
  mandatory = Union[mandatory, Take[alternatives, UpTo[Max[0, budget - Length[mandatory]]]]];
  alternatives = Complement[Range[n], mandatory];
  selected = Sort[Join[mandatory, RandomSample[alternatives,
    Min[Length[alternatives], Max[0, budget - Length[mandatory]]]]]];
  Join[row, <|"Candidates" -> row["Candidates"][[selected]],
    "Target" -> compactScalePacked[row["Target"][[selected]]],
    "LegalMask" -> compactScalePacked[row["LegalMask"][[selected]]],
    "ActionCount" -> Length[selected]|>,
    If[KeyExistsQ[row, "CandidateKeys"], <|"CandidateKeys" -> row["CandidateKeys"][[selected]],
      "CandidateOperations" -> row["CandidateOperations"][[selected]],
      "CandidateSignature" -> IntegerString[Hash[{"CompactSelectedActionKeys", row["CandidateKeys"][[selected]]}, "SHA256"], 16, 64],
      "HardNegativeKeys" -> Select[Lookup[row, "HardNegativeKeys", {}], MemberQ[row["CandidateKeys"][[selected]], #] &],
      "HardNegativeScores" -> KeyTake[Lookup[row, "HardNegativeScores", <||>], row["CandidateKeys"][[selected]]]|>, <||>]]
];
particleNativeInput[row_, tokens_] := <|"State" -> row["State"],
  "Candidates" -> Developer`ToPackedArray[PadRight[compactScaleDenseCandidates[row["Candidates"]],
    {row["ActionCount"], tokens, 166}, 0.]]|>;
particlePortableInput[row_] := <|"State" -> row["State"],
  "Candidates" -> compactScaleDenseCandidates[row["Candidates"]]|>;
particleNativePolicy[policy_, tokens_] := variableCompactParticlePolicy[policy, tokens];
particleSample[rows_List, count_Integer?Positive] := If[rows === {}, {},
  If[Length[rows] <= count, rows, If[count === 1, Take[rows, 1],
    rows[[DeleteDuplicates[Round[Subdivide[1, Length[rows], count - 1]]]]]]]];

particleCuratedRowQ[row_Association] := AnyTrue[Lookup[row, "Provenance", {}],
  TrueQ[Lookup[#, "Curated", False]] &];
particleRowLegCount[row_Association] := Module[{provenance, explicit, index},
  provenance = Lookup[row, "Provenance", {}];
  explicit = Select[Lookup[provenance, "ExternalLegCount", {}], IntegerQ[#] && # > 0 &];
  If[explicit =!= {}, Return[First[explicit]]];
  index = Select[Lookup[provenance, "AmplitudeIndex", {}], IntegerQ[#] && # > 0 &];
  If[index === {}, Return[0]];
  Lookup[Global`particleCatalog[[First[index]]], "ExternalLegCount", 4]
];
particleRowSamplingKeys[row_Association] := Module[{curated, category, phases},
  curated = particleCuratedRowQ[row];
  category = If[curated, "Curated", "Broad"] <> ToString[particleRowLegCount[row]] <> "Point";
  phases = DeleteDuplicates[("Route:" <> Lookup[#, "ReferenceName", "Unknown"] <> "/" <>
      Lookup[#, "RoutePhase", "Unknown"]) & /@
    Select[Lookup[row, "Provenance", {}], TrueQ[Lookup[#, "Curated", False]] &]];
  <|"Category" -> category, "Provenance" -> phases|>
];

(* Every retained row appears in each finite epoch. Only the underweighted pool
   repeats, and tensors stay in the original corpus rather than being copied. *)
particleSamplingPlan[rows_List, fraction_:Automatic] := Module[
  {curated, broad, c, b, cq, bq, n = Length[rows], keys},
  If[n === 0 || !(fraction === Automatic || (compactScaleFiniteQ[fraction] && 0. < fraction < 1.)), Return[$Failed]];
  curated = Pick[Range[n], particleCuratedRowQ /@ rows]; broad = Complement[Range[n], curated];
  c = Length[curated]; b = Length[broad]; cq = c; bq = b;
  If[fraction =!= Automatic,
    If[c === 0 || b === 0, Return[$Failed]];
    If[N[c/n] < fraction, cq = Max[c, Ceiling[fraction b/(1. - fraction)]],
      bq = Max[b, Ceiling[(1. - fraction) c/fraction]]]];
  If[cq + bq > 16 n, Return[$Failed]];
  keys = particleRowSamplingKeys /@ rows;
  <|"Method" -> If[fraction === Automatic, "UniformCompleteCorpusEpoch", "CompleteCorpusEpochWithMinorityOversampling"],
    "TrainingRows" -> n, "RoundLength" -> cq + bq, "CuratedDatasetRows" -> c, "BroadDatasetRows" -> b,
    "CuratedRowsPerEpoch" -> cq, "BroadRowsPerEpoch" -> bq,
    "RequestedCuratedTrainingFraction" -> If[fraction === Automatic, Null, fraction],
    "DatasetCuratedFraction" -> N[c/n], "ScheduledCuratedTrainingFraction" -> N[cq/(cq + bq)],
    "EveryRowVisitedPerEpoch" -> True, "MaximumEpochMultiplier" -> 16,
    "CuratedIndices" -> curated, "BroadIndices" -> broad,
    "RowCategories" -> Lookup[keys, "Category"], "RowProvenanceKeys" -> Lookup[keys, "Provenance"]|>
];
particlePublicSamplingPlan[plan_Association] := KeyDrop[plan,
  {"CuratedIndices", "BroadIndices", "RowCategories", "RowProvenanceKeys"}];
particlePoolEpoch[indices_List, count_Integer] := Module[{whole, tail},
  If[count === 0, Return[{}]]; If[indices === {} || count < Length[indices], Return[$Failed]];
  whole = Table[RandomSample[indices], {Quotient[count, Length[indices]]}];
  tail = RandomSample[indices, Mod[count, Length[indices]]]; Join[Flatten[whole], tail]
];
particleEpochIndices[plan_Association, seed_Integer, epoch_Integer?Positive] := BlockRandom[
  SeedRandom[Hash[{seed, epoch, "ParticleTokenBalancedRows"}]];
  RandomSample[Join[particlePoolEpoch[plan["CuratedIndices"], plan["CuratedRowsPerEpoch"]],
    particlePoolEpoch[plan["BroadIndices"], plan["BroadRowsPerEpoch"]]]]
];
particleSamplingCounts[plan_Association, indices_List] := <|
  "Categories" -> Counts[plan["RowCategories"][[indices]]],
  "Provenance" -> Counts[Flatten[plan["RowProvenanceKeys"][[indices]]]]|>;
particleSamplingReport[plan_Association, epochCounts_Association, seed_Integer, batch_Integer?NonNegative] := Module[
  {full, remaining, complete, partial, categories, provenance, curated, length = plan["RoundLength"]},
  full = Quotient[batch, length]; remaining = Mod[batch, length];
  If[!AllTrue[Range[full], KeyExistsQ[epochCounts, #] &], Return[$Failed]];
  complete = Lookup[epochCounts, Range[full], {}];
  partial = particleSamplingCounts[plan, If[remaining === 0, {},
    Take[particleEpochIndices[plan, seed, full + 1], remaining]]];
  categories = Merge[Append[Lookup[complete, "Categories", {}], partial["Categories"]], Total];
  provenance = Merge[Append[Lookup[complete, "Provenance", {}], partial["Provenance"]], Total];
  If[Total[Values[categories]] =!= batch, Return[$Failed]];
  curated = Total[Values[KeySelect[categories, StringStartsQ[#, "Curated"] &]]];
  <|"SamplingPlan" -> particlePublicSamplingPlan[plan], "TrainingRows" -> plan["TrainingRows"],
    "RoundLength" -> length, "CompletedFullRounds" -> full, "SampledUpdates" -> batch,
    "CuratedUpdates" -> curated, "BroadUpdates" -> batch - curated,
    "ActualCuratedTrainingFraction" -> If[batch === 0, Null, N[curated/batch]],
    "SampledCategoryCounts" -> categories, "SampledProvenanceCounts" -> provenance|>
];

(* ::Section::Closed:: *)
(*Teacher Diagnostics on Sampled Pools and Full Legal Boards*)
particleRouteTeacherRecord[scores_List, target_List] := Module[{positive, best, chosen, centered, tolerance},
  If[scores === {} || Length[scores] =!= Length[target] ||
      !compactScaleFiniteVectorQ[scores] || !compactScaleFiniteVectorQ[target] ||
      Min[target] < 0. || Abs[Total[target] - 1.] > 0.00001, Return[$Failed]];
  positive = Flatten[Position[target, _?Positive]];
  best = Max[scores[[positive]]]; chosen = First[Ordering[scores, -1]];
  centered = scores - Max[scores]; tolerance = 1.*^-5 + 2.*^-6 Max[1., Abs[best - Max[scores]]];
  <|"Scope" -> "RetainedCandidatePool", "Correct" -> Boole[target[[chosen]] > 0.], "ChosenIndex" -> chosen,
    "TeacherRank" -> (1 + Count[scores, _?(# > best &)]),
    "TeacherInTopScoreTie" -> (Max[scores] - best <= tolerance),
    "TeacherScoreMargin" -> (best - Max[scores]), "PositiveChoices" -> Length[positive],
    "StopSelected" -> (chosen === Length[scores]),
    "LegalCrossEntropy" -> (Log[Total[Quiet[Exp[centered], General::munfl]]] - Total[target centered])|>
];
particleRouteRecordSummary[records_List] := Module[{scored},
  scored = Select[records, TrueQ[Lookup[#, "Scored", False]] &];
  <|"Observations" -> Length[records], "ScoredObservations" -> Length[scored],
    "Correct" -> Total[Lookup[scored, "Correct", {}]],
    "Accuracy" -> If[scored === {}, Null, N[Mean[Lookup[scored, "Correct"]]]],
    "MeanTeacherRank" -> If[scored === {}, Null, N[Mean[Lookup[scored, "TeacherRank"]]]],
    "MaximumTeacherRank" -> If[scored === {}, Null, Max[Lookup[scored, "TeacherRank"]]],
    "PrematureStops" -> Count[scored, record_ /; TrueQ[record["StopSelected"]] && !TrueQ[record["StopExample"]]],
    "MeanLegalCrossEntropy" -> If[scored === {}, Null, N[Mean[Lookup[scored, "LegalCrossEntropy"]]]]|>
];

particleDiagnosticRoundRobin[groups_List] := If[groups === {}, {},
  Flatten[Table[If[round <= Length[group], group[[round]], Nothing],
    {round, Max[Length /@ groups]}, {group, groups}], 1]];
particleRouteDiagnosticRows[rows_List] := Module[{indices, provenance, routeGroups, orderRoute},
  indices = Select[Range[Length[rows]], particleCuratedRowQ[rows[[#]]] &];
  If[indices === {}, Return[{}]];
  provenance = AssociationThread[indices, First[Select[Lookup[rows[[#]], "Provenance", {}],
      TrueQ[Lookup[#, "Curated", False]] &]] & /@ indices];
  routeGroups = GatherBy[indices, Lookup[provenance[#], "ReferenceName", "Unknown"] &];
  orderRoute[group_] := Module[{permutations, phases},
    permutations = GatherBy[group, Lookup[provenance[#], "Sequence", Lookup[provenance[#], "Seed", 0]] &];
    (* Rotate phases across permutations so even a short prefix covers both. *)
    phases = MapIndexed[Function[{permutation, index}, With[{groups = GatherBy[permutation,
          Lookup[provenance[#], "RoutePhaseIndex", Lookup[provenance[#], "RoutePhase", "Unknown"]] &]},
        particleDiagnosticRoundRobin[RotateLeft[groups, Mod[First[index] - 1, Length[groups]]]]]], permutations];
    particleDiagnosticRoundRobin[phases]
  ];
  rows[[particleDiagnosticRoundRobin[orderRoute /@ routeGroups]]]
];

(* Curated rows already carry the exact legal reverse-route proof. Score each
   unique state once in balanced diagnostic order, retaining all provenance. *)
particleRouteAssessment[policy_, rows_List, seconds_, callback_:None] := Module[
  {curated, arrays, records = {}, processed = 0, started = AbsoluteTime[], result, summarize, observation, emit,
    writesSucceeded = True},
  curated = particleRouteDiagnosticRows[rows];
  arrays = TimeConstrained[compactParticlePolicyArrays[policy], 30, $Failed];
  summarize[complete_, reason_] := <|"Complete" -> complete, "Reason" -> reason,
    "Scope" -> "RetainedCandidatePool", "Ordering" -> "RoundRobinRoutesPermutationsAndPhases",
    "UniqueCuratedRows" -> Length[curated], "ProcessedUniqueRows" -> processed,
    "ProgressWritesSucceeded" -> writesSucceeded,
    "Seconds" -> N[AbsoluteTime[] - started], "Overall" -> particleRouteRecordSummary[records],
    "ByRoute" -> Map[particleRouteRecordSummary, GroupBy[records, #["ReferenceName"] &]],
    "EarlyRoute" -> particleRouteRecordSummary[Select[records,
      #["ReferenceName"] === "e, E, M, m / Z exchange" && #["Step"] < 29 &]],
    "State29Suffix" -> particleRouteRecordSummary[Select[records,
      #["ReferenceName"] === "e, E, M, m / Z exchange" &&
        #["Step"] >= 29 && !TrueQ[#["StopExample"]] &]],
    "ByPhase" -> Map[particleRouteRecordSummary,
      GroupBy[records, #["ReferenceName"] <> "/" <> #["RoutePhase"] &]],
    "ByPermutation" -> Map[particleRouteRecordSummary,
      GroupBy[records, #["ReferenceName"] <> "/" <> ToString[#["Sequence"]] &]],
    "Records" -> records|>;
  emit[value_] := If[callback =!= None && !TrueQ[Quiet[Check[callback[value], False]]], writesSucceeded = False];
  If[arrays === $Failed, result = summarize[False, "PortableArrayExtractionFailed"]; emit[result]; Return[result]];
  result = TimeConstrained[
    Do[
      observation = TimeConstrained[Quiet[Check[particleRouteTeacherRecord[
        compactParticlePolicyScores[arrays, particlePortableInput[row]], row["Target"]], $Failed]], 15, $Failed];
      Do[AppendTo[records, Join[KeyTake[provenance,
          {"ReferenceName", "Sequence", "Seed", "Step", "RoutePhase", "RoutePhaseIndex", "RouteIdentity", "StopExample"}],
        <|"StateKey" -> Lookup[row, "StateKey", Null], "ActionCount" -> row["ActionCount"],
          "TargetOperations" -> Lookup[row, "TargetOperations", {}], "Scored" -> AssociationQ[observation]|>,
        If[AssociationQ[observation], observation, <|"Failure" -> "InvalidScoresOrRowTimeout"|>]]],
        {provenance, Select[row["Provenance"], TrueQ[Lookup[#, "Curated", False]] &]}];
      processed++;
      If[Mod[processed, 50] === 0, emit[summarize[False, "InProgress"]]],
      {row, curated}]; True,
    seconds, $TimedOut];
  result = summarize[TrueQ[result], If[result === $TimedOut, "DiagnosticTimeLimit",
    If[AnyTrue[records, !TrueQ[#["Scored"]] &], "SomeRowsNotScored", "Complete"]]];
  emit[result]; result
];
particleAuditProofs[row_Association] := Module[{proofs},
  proofs = Lookup[row, "AuditProofs", If[AssociationQ[Lookup[row, "AuditProof", None]], {row["AuditProof"]}, {}]];
  If[ListQ[proofs], Select[proofs, AssociationQ], {}]
];
particleFullBoardAuditCases[rows_List, maximum_Integer?Positive] := Module[{available, ordered, selected, proofs},
  available = Select[rows, particleAuditProofs[#] =!= {} &];
  ordered = particleRouteDiagnosticRows[available];
  selected = Take[ordered, UpTo[maximum]];
  proofs[row_] := particleAuditProofs[row];
  <|"Cases" -> Map[Function[row, <|"Row" -> row, "Proof" -> First[proofs[row]]|>], selected],
    "Selection" -> <|"Method" -> "RoundRobinRoutesPermutationsAndPhasesFirstVerifiedProof",
      "MaximumStates" -> maximum, "TotalAvailableStates" -> Length[available],
      "TotalAvailableProofs" -> Total[Length[proofs[#]] & /@ available], "SelectedStates" -> Length[selected],
      "States" -> Map[Function[row, <|"StateKey" -> row["StateKey"],
        "FullCandidateSignature" -> First[proofs[row]]["FullCandidateSignature"],
        "Provenance" -> row["Provenance"]|>], selected]|>|>
];
particleFullBoardAssessment[policy_, rows_List, seconds_, maximum_Integer:48] := Module[
  {arrays, cases, selection, plan, records = {}, started = AbsoluteTime[], result, report, auditTag = Unique[]},
  If[maximum < 1, Return[<|"Passed" -> False, "Reason" -> "InvalidAuditStateBudget", "Records" -> {}|>]];
  arrays = If[AssociationQ[policy], policy, compactParticlePolicyArrays[policy]];
  plan = particleFullBoardAuditCases[rows, maximum]; cases = plan["Cases"]; selection = plan["Selection"];
  If[!AssociationQ[arrays], Return[<|"Passed" -> False, "Scope" -> "FullLegalBoard",
    "Complete" -> False, "Reason" -> "PortableArrayExtractionFailed", "Selection" -> selection, "Records" -> {}|>]];
  result = TimeConstrained[
    Do[report = Catch[With[{row = item["Row"], proof = item["Proof"]},
      Block[{$onShellChannels = proof["OnShellChannels"],
          $useMomentumConservation = proof["MomentumConservation"]},
        Module[{expr = proof["Expression"], rules = proof["Masses"], candidates, keys, labels, scores, audit},
          If[!ListQ[Lookup[row, "Provenance", None]] || row["Provenance"] === {} ||
              !AllTrue[row["Provenance"], MemberQ[{"Training", "Train"}, Lookup[#, "Kind", None]] &],
            Throw[<|"Passed" -> False, "Reason" -> "NonTrainingAuditProof", "StateKey" -> row["StateKey"]|>, auditTag]];
          candidates = legalMoves[expr, rules];
          keys = compactScaleActionKey[#, stateLegIDs[expr, rules]] & /@ candidates;
          If[compactScaleStateKey[stateFeature[expr, rules, stateLegIDs[expr, rules]]] =!= proof["StateKey"] ||
              proof["StateKey"] =!= row["StateKey"] || Length[keys] =!= proof["FullActionCount"] ||
              compactScaleBoardSignature[keys] =!= proof["FullCandidateSignature"],
            Throw[<|"Passed" -> False, "Reason" -> "AuditProofBoardMismatch", "StateKey" -> row["StateKey"]|>, auditTag]];
          labels = Boole[MemberQ[proof["TeacherKeys"], #]] & /@ keys;
          If[Total[labels] === 0 || !AllTrue[Pick[candidates, labels, 1],
              samePoly[applyCandidate[expr, #], canonicalChains[proof["Target"]]] &],
            Throw[<|"Passed" -> False, "Reason" -> "AuditTeacherProofMismatch", "StateKey" -> row["StateKey"]|>, auditTag]];
          scores = trainingCompactCandidateScores[arrays, expr, rules, candidates];
          audit = If[ListQ[scores], trainingFullBoardAudit[candidates, labels, scores], $Failed];
          If[!AssociationQ[audit], Throw[<|"Passed" -> False, "Reason" -> "FullBoardScoringFailed",
            "StateKey" -> row["StateKey"]|>, auditTag]];
          Join[audit, <|"StateKey" -> row["StateKey"], "FullCandidateSignature" -> proof["FullCandidateSignature"],
            "TeacherKeys" -> proof["TeacherKeys"], "Provenance" -> row["Provenance"],
            "RetainedActionCount" -> row["ActionCount"]|>]
        ]]], auditTag];
      AppendTo[records, report], {item, cases}]; True, seconds, $TimedOut];
  <|"Passed" -> (TrueQ[result] && Length[records] === Length[cases] && AllTrue[records, TrueQ[Lookup[#, "Passed", False]] &]),
    "Scope" -> "FullLegalBoard", "Complete" -> TrueQ[result], "ExpectedBoards" -> Length[cases],
    "Selection" -> selection,
    "ScoredBoards" -> Count[records, record_ /; TrueQ[Lookup[record, "Passed", False]]],
    "Reason" -> Which[result === $TimedOut, "FullBoardAuditTimeLimit",
      AnyTrue[records, !TrueQ[Lookup[#, "Passed", False]] &], "FullBoardAuditFailed", True, "Complete"],
    "Seconds" -> N[AbsoluteTime[] - started], "Records" -> records|>
];
particleRecoverySourceJobs[jobs_List, maximum_Integer?Positive] := Module[{eligible, groups, selected},
  eligible = Select[jobs, MemberQ[{"Training", "Train"}, Lookup[#, "Kind", None]] &&
    Lookup[#, "Mode", None] === "SimplificationRoute" && TrueQ[Lookup[#, "Curated", False]] &];
  groups = GatherBy[eligible, Lookup[#, "ReferenceName", Lookup[#, "Name", "Unknown"]] &];
  groups = SortBy[#, {Lookup[#, "Sequence", Lookup[#, "Seed", 0]], Lookup[#, "Order", 0]} &] & /@ groups;
  selected = Take[particleDiagnosticRoundRobin[groups], UpTo[maximum]];
  <|"Jobs" -> selected, "AvailableJobs" -> Length[eligible], "SelectedJobs" -> Length[selected],
    "SelectionMethod" -> "RoundRobinRoutesAndPermutations"|>
];
particleFitSample[rows_List, count_Integer?Positive] := Module[{groups, quota, chosen, remainder},
  If[Length[rows] <= count, Return[rows]];
  groups = GatherBy[rows, Function[row,
    {particleRowLegCount[row], Boole[Last[row["Target"]] > 0.], particleCuratedRowQ[row]}]];
  quota = Max[1, Quotient[count, Length[groups]]];
  chosen = Flatten[particleSample[#, quota] & /@ groups, 1];
  remainder = Select[rows, !MemberQ[Lookup[chosen, "StateKey"], #["StateKey"]] &];
  Take[Join[chosen, particleSample[remainder, Max[1, count - Length[chosen]]]], UpTo[count]]
];
particleValidationCases[jobs_List, count_Integer?Positive] := Module[{groups, ordered, choices},
  groups = GatherBy[Select[jobs, IntegerQ[Lookup[#, "ActualSteps", 0]] && #["ActualSteps"] > 0 &],
    #["AmplitudeIndex"] &];
  If[groups === {}, Return[{}]];
  groups = particleSample[groups, count];
  ordered = MapIndexed[RotateLeft[SortBy[#1, {#["Steps"], #["Sequence"]} &],
      Mod[First[#2] - 1, Length[#1]]] &, groups];
  choices = Flatten[Table[If[round <= Length[group], group[[round]], Nothing],
    {round, Max[Length /@ ordered]}, {group, ordered}], 1];
  Take[choices, UpTo[count]]
];
particleValidationTraceSummary[trace_Association] := Module[
  {history = Lookup[trace, "Trace", {}], progress = Lookup[trace, "Progress", <||>]},
  <|"InferenceSteps" -> Length[history],
    "ChangedEdits" -> Count[Lookup[history, "Changed", {}], True],
    "Workers" -> Lookup[trace, "Workers", 1],
    "SearchMode" -> Lookup[progress, "Mode", "Serial"],
    "SearchStage" -> Lookup[progress, "Stage", "Unknown"]|>
];
(* Compare float32 neural scores with the machine-precision portable evaluator. *)
particleScoreAgreement[native_, portable_, target_:None] := Module[
  {absolute = 1.*^-5, relative = 2.*^-6, difference, tolerances, centeredNative, centeredPortable,
    nativeExponentials, portableExponentials, nativeProbabilities, portableProbabilities,
    probabilityDifference, variation, nativeChoice, portableChoice,
    tieTolerance, nativeLoss = Null, portableLoss = Null, lossDifference = 0., lossTolerance = 1.*^-4, checks},
  If[!ListQ[native] || !ListQ[portable] || native === {} || Length[native] =!= Length[portable] ||
      !compactScaleFiniteVectorQ[native] || !compactScaleFiniteVectorQ[portable],
    Return[<|"Passed" -> False, "Reason" -> "InvalidScoreVectors"|>]];
  If[target =!= None && (!ListQ[target] || Length[target] =!= Length[native] ||
      !compactScaleFiniteVectorQ[target] || Min[target] < 0. || Abs[Total[target] - 1.] > absolute),
    Return[<|"Passed" -> False, "Reason" -> "InvalidTarget"|>]];
  difference = Abs[native - portable];
  tolerances = absolute + relative MapThread[Max[Abs[#1], Abs[#2]] &, {native, portable}];
  centeredNative = native - Max[native]; centeredPortable = portable - Max[portable];
  nativeExponentials = Quiet[Exp[centeredNative], General::munfl];
  portableExponentials = Quiet[Exp[centeredPortable], General::munfl];
  nativeProbabilities = nativeExponentials/Total[nativeExponentials];
  portableProbabilities = portableExponentials/Total[portableExponentials];
  probabilityDifference = Max[Abs[nativeProbabilities - portableProbabilities]];
  variation = Total[Abs[nativeProbabilities - portableProbabilities]]/2.;
  nativeChoice = First[Ordering[native, -1]]; portableChoice = First[Ordering[portable, -1]];
  (* A shared score offset must not enlarge the allowance for a near-tied decision. *)
  tieTolerance = absolute + relative Max[1., Abs[centeredNative[[portableChoice]]],
    Abs[centeredPortable[[nativeChoice]]]];
  If[target =!= None,
    nativeLoss = Log[Total[nativeExponentials]] - Total[target centeredNative];
    portableLoss = Log[Total[portableExponentials]] - Total[target centeredPortable];
    lossDifference = Abs[nativeLoss - portableLoss];
    lossTolerance += relative Max[Abs[nativeLoss], Abs[portableLoss]]];
  checks = <|"RawScores" -> And @@ MapThread[LessEqual, {difference, tolerances}],
    "Probabilities" -> (probabilityDifference <= 2.*^-5), "TotalVariation" -> (variation <= 5.*^-5),
    "TopChoice" -> (nativeChoice === portableChoice ||
      (-centeredNative[[portableChoice]] <= tieTolerance && -centeredPortable[[nativeChoice]] <= tieTolerance)),
    "CrossEntropy" -> (target === None || (compactScaleFiniteQ[nativeLoss] &&
      compactScaleFiniteQ[portableLoss] && lossDifference <= lossTolerance))|>;
  <|"Passed" -> AllTrue[Values[checks], TrueQ], "Reason" -> If[AllTrue[Values[checks], TrueQ], "Agreement", "ScoreAgreement"],
    "FailedChecks" -> Keys[Select[checks, !TrueQ[#] &]], "Checks" -> checks,
    "MaximumRawDifference" -> Max[difference], "MaximumRawTolerance" -> Max[tolerances],
    "MaximumProbabilityDifference" -> probabilityDifference, "TotalVariation" -> variation,
    "NativeChoice" -> nativeChoice, "PortableChoice" -> portableChoice, "TieTolerance" -> tieTolerance,
    "NativeCrossEntropy" -> nativeLoss, "PortableCrossEntropy" -> portableLoss,
    "CrossEntropyDifference" -> lossDifference, "CrossEntropyTolerance" -> lossTolerance|>
];
particleAssessment[policy_, rr_, trainedTokens_] := Module[{width, native, arrays, records},
  If[!ListQ[rr] || rr === {} || !AllTrue[rr, AssociationQ],
    Return[<|"Passed" -> False, "Reason" -> "InvalidAssessmentRows"|>]];
  width = Max[trainedTokens, Max[Dimensions[#["Candidates"]][[2]] & /@ rr]];
  native = If[width === trainedTokens, policy, TimeConstrained[particleNativePolicy[policy, width], 30, $Failed]];
  arrays = compactParticlePolicyArrays[policy];
  If[!MatchQ[native, _NetGraph] || arrays === $Failed,
    Return[<|"Passed" -> False, "Reason" -> If[arrays === $Failed, "PortableArrayExtraction", "NativePolicy"]|>]];
  records = Map[Function[row, Module[{scores, portable, agreement, loss},
    scores = Flatten[native[particleNativeInput[row, width], TargetDevice -> "CPU"]];
    portable = compactParticlePolicyScores[arrays, particlePortableInput[row]];
    If[Length[scores] =!= row["ActionCount"],
      Return[<|"Passed" -> False, "Reason" -> "ActionCountMismatch", "StateKey" -> Lookup[row, "StateKey", Null]|>]];
    agreement = particleScoreAgreement[scores, portable, row["Target"]];
    If[!TrueQ[agreement["Passed"]], Return[<|"Passed" -> False,
      "Reason" -> agreement["Reason"], "StateKey" -> Lookup[row, "StateKey", Null], "Agreement" -> agreement|>]];
    loss = agreement["NativeCrossEntropy"];
    <|"Passed" -> True, "Correct" -> Boole[row["Target"][[First[Ordering[scores, -1]]]] > 0.],
      "Loss" -> loss, "StateKey" -> Lookup[row, "StateKey", Null], "Agreement" -> agreement|>
  ]], rr];
  If[!AllTrue[records, TrueQ[Lookup[#, "Passed", False]] &],
    Return[<|"Passed" -> False, "Reason" -> "AssessmentRowFailed", "Records" -> records|>]];
  <|"Passed" -> True, "Rows" -> Length[rr], "Correct" -> Total[Lookup[records, "Correct"]],
    "Accuracy" -> N[Mean[Lookup[records, "Correct"]]], "MeanLegalCrossEntropy" -> Mean[Lookup[records, "Loss"]],
    "NativeAssessmentTokenCount" -> width, "PortableUnpaddedAgreement" -> True,
    "MaximumPortableDifference" -> Max[Lookup[Lookup[records, "Agreement"], "MaximumRawDifference"]],
    "MaximumProbabilityDifference" -> Max[Lookup[Lookup[records, "Agreement"], "MaximumProbabilityDifference"]]|>
];

SetAttributes[particleFinalAssessment, HoldAll];
particleFinalAssessment[name_String, operation_, seconds_:180] := Module[
  {file, startedAt, elapsed, value, status, record, result},
  file = "final-" <> ToLowerCase[name] <> "-assessment.json";
  startedAt = DateString["ISODateTime"];
  record = <|"Assessment" -> name, "AssessmentStatus" -> "Started",
    "AssessmentStartedAt" -> startedAt, "AssessmentTimeLimitSeconds" -> seconds|>;
  If[!particleJSON[file, record], particleFail["FinalAssessmentReportWriteFailed", record]];
  particleStage["FinalFitAssessment", record];
  {elapsed, value} = AbsoluteTiming[TimeConstrained[operation, seconds, $TimedOut]];
  status = Which[value === $TimedOut, "TimedOut", value === $Failed, "Failed",
    value === $Aborted, "Aborted", AssociationQ[value], "Completed", True, "InvalidResult"];
  record = Join[record, <|"AssessmentStatus" -> status, "AssessmentFinishedAt" -> DateString["ISODateTime"],
    "AssessmentSeconds" -> N[elapsed]|>];
  result = Join[If[AssociationQ[value], value,
    <|"Passed" -> False, "Reason" -> "Assessment" <> status,
      "Result" -> particleCheckpointJSONValue[value]|>], record];
  If[!particleJSON[file, particleCheckpointJSONValue[result]],
    particleFail["FinalAssessmentReportWriteFailed", record]];
  particleStage["FinalFitAssessment", result];
  result
];

particleProgressSnapshot[progress_Association, rowCount_Integer?Positive, started_, budget_] := Module[{keys, snapshot, batch},
  keys = Intersection[Keys[progress], {"AbsoluteBatch", "Round", "BatchLoss", "RoundLoss", "TimeElapsed"}];
  (* NetTrain delayed properties must be evaluated inside its callback. *)
  snapshot = AssociationThread[keys, Replace[Lookup[progress, #], None | _Missing -> Null] & /@ keys];
  batch = Lookup[snapshot, "AbsoluteBatch", None];
  If[!IntegerQ[batch] || batch < 0, Return[$Failed]];
  Join[snapshot, <|"CompletedFullRounds" -> Quotient[batch, rowCount], "TrainingRows" -> rowCount,
    "WallSeconds" -> N[AbsoluteTime[] - started], "BudgetSeconds" -> budget,
    "BudgetSecondsRemaining" -> Max[0., budget - Lookup[snapshot, "TimeElapsed", 0.]],
    "BudgetProgressFraction" -> Min[1., Max[0., Lookup[snapshot, "TimeElapsed", 0.]/budget]],
    "Timestamp" -> DateString["ISODateTime"], "WolframMemoryBytes" -> MemoryInUse[]|>]
];
particleProgressLossesFiniteQ[status_Association] :=
  !AnyTrue[Lookup[status, {"BatchLoss", "RoundLoss"}, Null], # =!= Null && !compactScaleFiniteQ[#] &] &&
  (Lookup[status, "AbsoluteBatch", 0] === 0 || compactScaleFiniteQ[Lookup[status, "BatchLoss", None]]);

particleAudit[trainer_, policy_, samples_, count_, tokens_, gradients_:False] := Module[
  {arrays, biasPath, records, readout, expanded},
  arrays = compactParticlePolicyArrays[policy];
  If[arrays === $Failed, Return[<|"Passed" -> False, "Reason" -> "PortableArrayExtraction"|>]];
  expanded = TimeConstrained[particleNativePolicy[policy, tokens + 3], 30, $Failed];
  If[!MatchQ[expanded, _NetGraph], Return[<|"Passed" -> False, "Reason" -> "ExpandedNativePolicy"|>]];
  biasPath = {"Policy", "Scorer", "Net", "Score", 3, "Biases"};
  records = Map[Function[row, Module[{input, scores, fixed, portable, loss, padded, reordered,
    order, agreements, checks, candidates, expected = Null, lossDifference = Infinity,
    gradient = 0., n = row["ActionCount"]},
    candidates = compactScaleDenseCandidates[row["Candidates"]];
    input = particleTrainRow[row, count, tokens];
    scores = Flatten[policy[particleNativeInput[row, tokens], TargetDevice -> "CPU"]];
    fixed = Flatten[NetExtract[trainer, "Policy"][KeyTake[input, {"State", "Candidates"}], TargetDevice -> "CPU"]];
    portable = compactParticlePolicyScores[arrays, particlePortableInput[row]];
    loss = trainer[input, TargetDevice -> "CPU"];
    padded = Flatten[expanded[<|"State" -> row["State"], "Candidates" ->
      PadRight[candidates, {n + 2, tokens + 3, 166}, 0.]|>, TargetDevice -> "CPU"]];
    order = Reverse[Range[n]];
    reordered = Flatten[policy[<|"State" -> row["State"], "Candidates" ->
      PadRight[candidates[[order]], {n, tokens, 166}, 0.]|>, TargetDevice -> "CPU"]];
    If[TrueQ[gradients], gradient = First[trainer[input, NetPortGradient[biasPath], TargetDevice -> "CPU"]]];
    If[Length[scores] =!= n || Length[fixed] =!= count || Length[padded] =!= n + 2 || Length[reordered] =!= n ||
        !AllTrue[{scores, fixed, padded, reordered}, compactScaleFiniteVectorQ],
      Return[<|"Passed" -> False, "Actions" -> n, "StateKey" -> Lookup[row, "StateKey", Null],
        "Reason" -> "InvalidNativeScoreShapeOrValues",
        "ScoreLengths" -> (Length /@ {scores, fixed, padded, reordered}),
        "ExpectedLengths" -> {n, count, n + 2, n}|>]];
    agreements = <|"Portable" -> particleScoreAgreement[scores, portable, row["Target"]],
      "FixedVariable" -> particleScoreAgreement[scores, Take[fixed, n], row["Target"]],
      "Padding" -> particleScoreAgreement[scores, Take[padded, n], row["Target"]],
      "CandidateOrder" -> particleScoreAgreement[scores[[order]], reordered, row["Target"][[order]]]|>;
    expected = agreements["FixedVariable"]["NativeCrossEntropy"];
    If[compactScaleFiniteQ[loss] && compactScaleFiniteQ[expected], lossDifference = Abs[loss - expected]];
    checks = Join[AssociationMap[TrueQ[agreements[#]["Passed"]] &, Keys[agreements]],
      <|"TrainerCrossEntropy" -> (compactScaleFiniteQ[loss] && compactScaleFiniteQ[expected] &&
          lossDifference <= 1.*^-4 + 2.*^-6 Max[Abs[loss], Abs[expected]]),
        "SharedBiasGradient" -> (compactScaleFiniteQ[gradient] && Abs[gradient] < 1.*^-5)|>];
    <|"Actions" -> n, "Tokens" -> Dimensions[row["Candidates"]][[2]], "StateKey" -> Lookup[row, "StateKey", Null], "Loss" -> loss,
      "ExpectedLoss" -> expected, "CrossEntropyDifference" -> lossDifference,
      "SharedBiasGradient" -> gradient,
      "PortableDifference" -> Lookup[agreements["Portable"], "MaximumRawDifference", Infinity],
      "FixedVariableDifference" -> Lookup[agreements["FixedVariable"], "MaximumRawDifference", Infinity],
      "PaddingDifference" -> Lookup[agreements["Padding"], "MaximumRawDifference", Infinity],
      "CandidateOrderDifference" -> Lookup[agreements["CandidateOrder"], "MaximumRawDifference", Infinity],
      "Passed" -> AllTrue[Values[checks], TrueQ], "FailedChecks" -> Keys[Select[checks, !TrueQ[#] &]],
      "Checks" -> checks, "Agreements" -> agreements|>
  ]], samples];
  readout = compactScaleReadoutAudit[policy, samples];
  <|"Passed" -> (AllTrue[records, TrueQ[#["Passed"]] &] && TrueQ[readout["Passed"]]),
    "FailedChecks" -> Join[If[AllTrue[records, TrueQ[#["Passed"]] &], {}, {"RowAudit"}],
      If[TrueQ[readout["Passed"]], {}, {"UnitReadout"}]],
    "Records" -> records, "ReadoutAudit" -> readout|>
];

particleWarmStart[trainer_, config_] := Module[{file, metaFile, metadata, policy, paths, targetPaths, result, ports},
  file = Lookup[config, "WarmStartPolicyFile", None]; metaFile = Lookup[config, "WarmStartMetadataFile", None];
  If[file === None && metaFile === None, Return[<|"Trainer" -> trainer, "Initialization" -> "Fresh",
    "OptimizerStateReset" -> True|>]];
  If[!StringQ[file] || !StringQ[metaFile] || !FileExistsQ[file] || !FileExistsQ[metaFile], Return[$Failed]];
  metadata = Quiet[Check[Get[metaFile], $Failed]];
  If[!AssociationQ[metadata] || !compactParticleModelCompatibleQ[metadata], Return[$Failed]];
  policy = TimeConstrained[Quiet[Check[Import[file], $Failed]], 60, $Failed];
  If[!MatchQ[policy, _NetGraph] || !compactScaleFiniteNetQ[policy] || compactParticlePolicyArrays[policy] === $Failed,
    Return[$Failed]];
  ports = Information[policy, "InputPorts"];
  If[Lookup[ports, "Candidates", None] =!= {"Varying", metadata["NativeTokenCount"], 166}, Return[$Failed]];
  paths = Information[policy, "ArraysPositionList"]; targetPaths = Prepend[#, "Policy"] & /@ paths;
  If[Sort[targetPaths] =!= Sort[Information[trainer, "ArraysPositionList"]] ||
      !And @@ MapThread[Dimensions[NetExtract[policy, #1]] === Dimensions[NetExtract[trainer, #2]] &,
        {paths, targetPaths}], Return[$Failed]];
  result = NetReplacePart[trainer, MapThread[#1 -> NetExtract[policy, #2] &, {targetPaths, paths}]];
  If[!compactScaleFiniteNetQ[result] || compactScaleArraysHash[NetExtract[result, "Policy"]] =!=
      compactScaleArraysHash[policy], Return[$Failed]];
  <|"Trainer" -> result, "Initialization" -> "CompatibleParticlePolicyWeights", "OptimizerStateReset" -> True,
    "WarmStartPolicySHA256" -> compactScaleFileHash[file], "WarmStartMetadataSHA256" -> compactScaleFileHash[metaFile],
    "WarmStartPolicyArraysSHA256" -> compactScaleArraysHash[policy]|>
];

(* Named gates preserve the reason for rejection without publishing rejected weights. *)
SetAttributes[particleCheckpointGate, HoldAll];
particleCheckpointGate[state_Symbol, name_, seconds_, operation_, validator_, failureClass_] := Module[
  {elapsed, value, accepted, messages},
  state["Gate"] = name;
  Block[{$MessageList = {}},
    {elapsed, value} = AbsoluteTiming[TimeConstrained[Quiet[Check[operation, $Failed]], seconds, $TimedOut]];
    messages = ToString[#, InputForm] & /@ $MessageList];
  state["GateTimings"] = Append[Lookup[state, "GateTimings", <||>], name -> N[elapsed]];
  If[messages =!= {}, state["GateMessages"] = Append[Lookup[state, "GateMessages", <||>], name -> messages]];
  If[AssociationQ[value], state["GateResults"] = Append[Lookup[state, "GateResults", <||>], name ->
    If[MemberQ[{"NativePortableAudit", "FitAssessment"}, name], value,
      <|"Keys" -> Keys[value], "ValueDimensions" -> AssociationMap[Dimensions[value[#]] &, Keys[value]]|>]]];
  accepted = value =!= $TimedOut && TrueQ[validator[value]];
  If[!accepted,
    state["FailureClass"] = If[value === $TimedOut, "Timeout", failureClass];
    state["ResultHead"] = ToString[Head[value], InputForm];
    Throw[False, particleCheckpointRejected]];
  value
];
particleCheckpointDecision[outcome_Association, consecutiveFailures_Integer] := Which[
  TrueQ[Lookup[outcome, "Passed", False]], "Continue",
  MemberQ[{"TransientIO", "Timeout"}, Lookup[outcome, "FailureClass", "Unknown"]] && consecutiveFailures < 2, "Retry",
  True, "Abort"
];
particleCheckpointJSONValue[value_] := Which[
  AssociationQ[value], Association[KeyValueMap[
    If[StringQ[#1], #1, ToString[#1, InputForm]] -> particleCheckpointJSONValue[#2] &, value]],
  ListQ[value], particleCheckpointJSONValue /@ value,
  StringQ[value] || IntegerQ[value] || value === True || value === False || value === Null, value,
  MatchQ[value, _Real] && TrueQ[Abs[value] < Infinity], value,
  MatchQ[value, _Rational], particleCheckpointJSONValue[N[value]],
  MatchQ[value, _DirectedInfinity], ToString[value, InputForm],
  value === Indeterminate, "Indeterminate", value === $Failed, "Failed",
  value === $TimedOut, "TimedOut", value === $Aborted, "Aborted", True, ToString[value, InputForm]
];
particleCheckpointPublish[run_String, manifest_Association] := Module[{fields, files},
  fields = {"Policy", "Metadata", "Trainer", "History"};
  files = Table[{Lookup[manifest, field <> "File", None], Lookup[manifest, field <> "SHA256", None]}, {field, fields}];
  If[!AllTrue[files, StringQ[First[#]] && StringQ[Last[#]] && FileExistsQ[FileNameJoin[{run, First[#]}]] &&
      Quiet[Check[compactScaleFileHash[FileNameJoin[{run, First[#]}]] === Last[#], False]] &], Return[False]];
  compactScaleAtomicExport[FileNameJoin[{run, "checkpoint-manifest.json"}], manifest, "RawJSON"]
];
particleCheckpointForensics[run_String, net_, diagnostic_Association] := Module[
  {directory, stem, path, result, bytes, snapshot = <|"ForensicSnapshotSaved" -> False|>},
  directory = FileNameJoin[{run, "checkpoint-rejections"}];
  If[!DirectoryQ[directory], Quiet[Check[CreateDirectory[directory], Null]]];
  stem = "rejected-" <> IntegerString[Lookup[diagnostic, "AbsoluteBatch", 0], 10, 9] <> "-" <> CreateUUID[];
  bytes = ByteCount[net];
  If[DirectoryQ[directory] && bytes <= 256 1024^2 && Length[FileNames["rejected-*.wxf", directory]] < 2,
    path = FileNameJoin[{directory, stem <> ".wxf"}];
    result = TimeConstrained[Quiet[Check[compactScaleAtomicExport[path, net, "WXF"], False]], 15, False];
    snapshot = <|"ForensicSnapshotSaved" -> TrueQ[result], "ForensicSnapshotBytes" -> bytes,
      "ForensicTrainerFile" -> If[TrueQ[result], FileNameJoin[{"checkpoint-rejections", stem <> ".wxf"}], Null]|>];
  result = Join[diagnostic, snapshot];
  TimeConstrained[Quiet[Check[compactScaleAtomicExport[FileNameJoin[{run, "checkpoint-failure.json"}],
      particleCheckpointJSONValue[result], "RawJSON"], False]], 10, False];
  If[DirectoryQ[directory], TimeConstrained[Quiet[Check[compactScaleAtomicExport[
    FileNameJoin[{directory, stem <> ".json"}], particleCheckpointJSONValue[result], "RawJSON"], False]], 10, False]];
  Print[DateString["ISODateTime"], " CHECKPOINT REJECTED: ", particleCheckpointJSONValue[result]];
  result
];
particleCheckpointAttempt[net_, status_Association, final_, settings_Association] := Module[
  {run = settings["Run"], state, result, current, arrays, audit, fit, entry, savedMetadata, directory,
    relativeDirectory, trainerFile, policyFile, metadataFile, historyFile, loaded, loadedTrainer, loadedMetadata,
    sourceHash, manifest, checkpointStarted = AbsoluteTime[]},
  state = <|"Passed" -> False, "AbsoluteBatch" -> Lookup[status, "AbsoluteBatch", 0],
    "FinalCheckpoint" -> TrueQ[final], "Timestamp" -> DateString["ISODateTime"],
    "Gate" -> "Starting", "GateTimings" -> <||>, "GateResults" -> <||>|>;
  result = TimeConstrained[Catch[
    particleCheckpointGate[state, "FiniteTrainerArrays", 15, compactScaleFiniteNetQ[net], TrueQ, "InvalidArrays"];
    current = particleCheckpointGate[state, "VariablePolicyConstruction", 30,
      variableCompactParticlePolicy[NetExtract[net, "Policy"]], MatchQ[#, _NetGraph] &, "InvalidPolicy"];
    arrays = particleCheckpointGate[state, "PortableArrayExtraction", 15,
      compactParticlePolicyArrays[current], AssociationQ, "InvalidArrays"];
    audit = particleCheckpointGate[state, "NativePortableAudit", 60,
      particleAudit[net, current, Take[settings["Samples"], UpTo[2]], settings["Count"], settings["Tokens"]],
      AssociationQ[#] && TrueQ[Lookup[#, "Passed", False]] &, "NumericalAudit"];
    fit = particleCheckpointGate[state, "FitAssessment", 45,
      particleAssessment[current, settings["FitRows"], settings["Tokens"]],
      AssociationQ[#] && TrueQ[Lookup[#, "Passed", False]] &, "NumericalAssessment"];
    entry = Join[status, <|"FitSample" -> fit, "ReadoutAuditPassed" -> True,
      "NativePortableAuditPassed" -> True, "FinalCheckpoint" -> TrueQ[final], "OptimizerStateResetWithinRun" -> False|>];
    savedMetadata = Join[settings["BaseMetadata"], settings["Initialization"], <|"NativeTokenCount" -> settings["Tokens"],
      "TrainingActionCount" -> settings["Count"], "TrainingTokenCount" -> settings["Tokens"], "TrainingProgress" -> entry,
      "TrainingBatchSize" -> 1, "LearningRate" -> settings["LearningRate"], "Optimizer" -> "ADAM",
      "OptimizerContinuity" -> "SingleNetTrainSession",
      "CheckpointResumeSemantics" -> "WeightsOnly; restarting NetTrain resets ADAM state",
      "RandomSeed" -> settings["Seed"]|>];
    particleCheckpointGate[state, "MetadataCompatibility", 10, compactParticleModelCompatibleQ[savedMetadata], TrueQ, "InvalidMetadata"];
    relativeDirectory = FileNameJoin[{"checkpoints", "batch-" <> IntegerString[state["AbsoluteBatch"], 10, 9] <> "-" <> CreateUUID[]}];
    directory = FileNameJoin[{run, relativeDirectory}];
    state["StagingDirectory"] = relativeDirectory;
    particleCheckpointGate[state, "StagingDirectory", 10, CreateDirectory[directory, CreateIntermediateDirectories -> True], StringQ, "TransientIO"];
    trainerFile = FileNameJoin[{directory, "training-checkpoint.wxf"}];
    policyFile = FileNameJoin[{directory, "unscramble-compact.wlnet"}];
    metadataFile = FileNameJoin[{directory, "unscramble-compact.m"}];
    historyFile = FileNameJoin[{directory, "fit-history.json"}];
    particleCheckpointGate[state, "TrainerExport", 30, compactScaleAtomicExport[trainerFile, net, "WXF"], TrueQ, "TransientIO"];
    particleCheckpointGate[state, "PolicyExport", 30, compactScaleAtomicExport[policyFile, current], TrueQ, "TransientIO"];
    particleCheckpointGate[state, "MetadataExport", 15, compactScaleAtomicPut[metadataFile, savedMetadata], TrueQ, "TransientIO"];
    particleCheckpointGate[state, "HistoryExport", 15,
      compactScaleAtomicExport[historyFile, Append[settings["History"], entry], "RawJSON"], TrueQ, "TransientIO"];
    loaded = particleCheckpointGate[state, "PolicyReimport", 30, Import[policyFile], MatchQ[#, _NetGraph] &, "TransientIO"];
    loadedTrainer = particleCheckpointGate[state, "TrainerReimport", 30, Import[trainerFile, "WXF"], MatchQ[#, _NetGraph] &, "TransientIO"];
    loadedMetadata = particleCheckpointGate[state, "MetadataReimport", 15, Get[metadataFile], AssociationQ, "TransientIO"];
    sourceHash = particleCheckpointGate[state, "PolicyArraysHash", 15, compactScaleArraysHash[current], StringQ, "InvalidArrays"];
    particleCheckpointGate[state, "PolicyRoundTrip", 15,
      compactScaleFiniteNetQ[loaded] && compactScaleArraysHash[loaded] === sourceHash && compactParticlePolicyArrays[loaded] =!= $Failed,
      TrueQ, "ArtifactIntegrity"];
    particleCheckpointGate[state, "TrainerRoundTrip", 15,
      compactScaleFiniteNetQ[loadedTrainer] && compactScaleArraysHash[loadedTrainer] === compactScaleArraysHash[net],
      TrueQ, "ArtifactIntegrity"];
    particleCheckpointGate[state, "MetadataRoundTrip", 10, loadedMetadata === savedMetadata, TrueQ, "ArtifactIntegrity"];
    manifest = <|"AbsoluteBatch" -> state["AbsoluteBatch"], "Timestamp" -> DateString["ISODateTime"],
      "PolicyFile" -> FileNameJoin[{relativeDirectory, "unscramble-compact.wlnet"}], "PolicySHA256" -> compactScaleFileHash[policyFile],
      "MetadataFile" -> FileNameJoin[{relativeDirectory, "unscramble-compact.m"}], "MetadataSHA256" -> compactScaleFileHash[metadataFile],
      "TrainerFile" -> FileNameJoin[{relativeDirectory, "training-checkpoint.wxf"}], "TrainerSHA256" -> compactScaleFileHash[trainerFile],
      "SnapshotFile" -> FileNameJoin[{relativeDirectory, "training-checkpoint.wxf"}], "SnapshotSHA256" -> compactScaleFileHash[trainerFile],
      "HistoryFile" -> FileNameJoin[{relativeDirectory, "fit-history.json"}], "HistorySHA256" -> compactScaleFileHash[historyFile],
      "PolicyArraysSHA256" -> sourceHash, "TrainerArraysSHA256" -> compactScaleArraysHash[net],
      "ResumeOptimizerStatePreserved" -> False, "Initialization" -> settings["Initialization"]|>;
    state["StagedManifest"] = manifest;
    (* One atomic manifest switch publishes an immutable, fully verified model pair. *)
    particleCheckpointGate[state, "ManifestPublication", 15, particleCheckpointPublish[run, manifest], TrueQ, "TransientIO"];
    state["Passed"] = True; state["FailureClass"] = None;
    state["Entry"] = entry; state["Manifest"] = manifest; state["ModelDirectory"] = directory;
    True,
    particleCheckpointRejected], settings["CheckpointSeconds"], $TimedOut];
  state["CheckpointSeconds"] = N[AbsoluteTime[] - checkpointStarted];
  If[result === $TimedOut, state["FailureClass"] = "Timeout"; state["OverallDeadlineExceeded"] = True];
  If[!TrueQ[result], state["Passed"] = False; Return[particleCheckpointForensics[run, net, state]]];
  Print[DateString["ISODateTime"], " CHECKPOINT: ", entry];
  state
];

(* ::Section::Closed:: *)
(*Single-Session Particle Optimization and Checkpoints*)
particleTrain[rr_, held_, config_, baseMetadata_] := Module[
  {corpus = rr, rowCount, count, tokens, trainer, policy, warm, initialization, order = {}, cursor = 1, epoch = 0,
    seed, seconds, maxRounds, generator, guard, progress, checkpoint, started, observed = <|"AbsoluteBatch" -> 0|>,
    invalidLoss = False, invalidProgress = False, progressFailed = False, checkpointFailed = False, diverged = False,
    result, properties, elapsed, reason, stats, audit, samples, fitRows, heldFitRows, checkpointFitRows,
    fit, heldFit, history = {}, lastCheckpointTime = 0., lastCheckpointBatch = -1, checkpointDirectory,
    lastCheckpointAttempt = <||>, lastCheckpointAttemptTime = 0., consecutiveCheckpointFailures = 0,
    publishedModelDirectory = None, checkpointDecision, samplingPlan, epochLength, epochCounts = <||>,
    samplingReport, routeBefore, routeAfter, fullBefore, fullAfter, trainingBudget},
  rowCount = Length[corpus]; count = Max[Lookup[corpus, "ActionCount"]];
  tokens = Max[Dimensions[#["Candidates"]][[2]] & /@ corpus];
  seed = config["RandomSeed"]; seconds = config["NNTimeSeconds"]; maxRounds = config["MaxTrainingRounds"];
  If[!AllTrue[corpus, particleRowQ[#, count, config["MaxTokens"]] &],
    Return[<|"Status" -> "InvalidTrainingRows"|>]];
  If[particleTrainingTensorBytes[count, tokens] > config["MaxTrainingTensorMiB"] 1024^2,
    Return[<|"Status" -> "TrainingTensorMemoryBudget", "Actions" -> count, "Tokens" -> tokens,
      "EstimatedInputBytes" -> particleTrainingTensorBytes[count, tokens],
      "MaximumInputMiB" -> config["MaxTrainingTensorMiB"]|>]];
  samplingPlan = particleSamplingPlan[corpus, Lookup[config, "CuratedTrainingFraction", Automatic]];
  If[!AssociationQ[samplingPlan], Return[<|"Status" -> "InvalidCuratedSamplingPlan"|>]];
  epochLength = samplingPlan["RoundLength"];
  particleStage["NativeConstructor", <|"FixedActions" -> count, "FixedTokens" -> tokens,
    "EstimatedInputBytes" -> particleTrainingTensorBytes[count, tokens],
    "TrainingCandidateBudget" -> config["TrainingCandidateBudget"],
    "FullLegalActionCountRange" -> MinMax[Lookup[#, "FullActionCount", #["ActionCount"]] & /@ corpus],
    "SampledStates" -> Count[Lookup[corpus, "Sampled", False], True]|>];
  trainer = TimeConstrained[NetInitialize[compactParticleTrainer[count, tokens], RandomSeeding -> seed], 90, $Failed];
  If[!compactScaleFiniteNetQ[trainer], Return[<|"Status" -> "NativeTrainerInitializationFailed"|>]];
  warm = TimeConstrained[particleWarmStart[trainer, config], 90, $Failed];
  If[!AssociationQ[warm], Return[<|"Status" -> "IncompatibleWarmStart"|>]];
  trainer = warm["Trainer"]; initialization = Join[KeyDrop[warm, "Trainer"],
    <|"SamplingPlan" -> particlePublicSamplingPlan[samplingPlan],
      "TrainingCandidatePoolBudget" -> config["TrainingCandidateBudget"],
      "TrainingUpdateCandidateBudget" -> config["TrainingUpdateCandidateBudget"],
      "CandidateSampling" -> If[TrueQ[config["ModelGuidanceEnabled"]],
        "AllTeachersAndStopWithFullBoardHardNegativesAndRotatingAlternatives",
        "AllTeacherPositivesAndStopWithRotatingPoolAlternatives"]|>];
  policy = TimeConstrained[variableCompactParticlePolicy[NetExtract[trainer, "Policy"]], 60, $Failed];
  If[!MatchQ[policy, _NetGraph], Return[<|"Status" -> "VariableActionFixedTokenPolicyFailed"|>]];
  samples = DeleteDuplicatesBy[{First[corpus], First[SortBy[corpus, #["ActionCount"] &]],
    Last[SortBy[corpus, #["ActionCount"] &]], First[MaximalBy[corpus, Dimensions[#["Candidates"]][[2]] &]]}, #["StateKey"] &];
  audit = TimeConstrained[particleAudit[trainer, policy, samples, count, tokens, True], 120, $Failed];
  If[!particleJSON["initial-audit.json", audit] || !AssociationQ[audit] || !TrueQ[audit["Passed"]],
    Return[<|"Status" -> "InitialNativePortableAuditFailed"|>]];
  fitRows = particleFitSample[corpus, config["FitSampleSize"]]; heldFitRows = particleFitSample[held, config["FitSampleSize"]];
  checkpointFitRows = particleFitSample[corpus, config["CheckpointFitSampleSize"]];
  checkpointDirectory = FileNameJoin[{Global`particleRun, "checkpoints"}];
  If[!DirectoryQ[checkpointDirectory], CreateDirectory[checkpointDirectory]];
  checkpoint[net_, status_, final_:False] := Module[{exposure},
    exposure = particleSamplingReport[samplingPlan, epochCounts, seed, Lookup[status, "AbsoluteBatch", 0]];
    If[!AssociationQ[exposure], lastCheckpointAttempt = <|"Passed" -> False,
      "Gate" -> "SamplingProgress", "FailureClass" -> "InvalidSamplingCounters"|>; Return[False]];
    lastCheckpointAttemptTime = AbsoluteTime[];
    lastCheckpointAttempt = particleCheckpointAttempt[net, Join[status, exposure], final, <|"Run" -> Global`particleRun,
      "Count" -> count, "Tokens" -> tokens, "Samples" -> samples, "FitRows" -> checkpointFitRows,
      "BaseMetadata" -> baseMetadata, "Initialization" -> initialization, "LearningRate" -> config["LearningRate"],
      "Seed" -> seed, "History" -> history, "CheckpointSeconds" -> config["CheckpointSeconds"]|>];
    If[!TrueQ[Lookup[lastCheckpointAttempt, "Passed", False]], Return[False]];
    AppendTo[history, lastCheckpointAttempt["Entry"]]; lastCheckpointBatch = Lookup[status, "AbsoluteBatch", 0];
    lastCheckpointTime = AbsoluteTime[]; publishedModelDirectory = lastCheckpointAttempt["ModelDirectory"];
    consecutiveCheckpointFailures = 0; True
  ];
  If[!checkpoint[trainer, <|"AbsoluteBatch" -> 0, "CompletedFullRounds" -> 0,
      "Timestamp" -> DateString["ISODateTime"], "WallSeconds" -> 0., "TrainingRows" -> rowCount|>],
    Return[<|"Status" -> "InitialCheckpointFailed", "CheckpointFailure" -> lastCheckpointAttempt|>]];
  particleStage["CuratedTeacherBefore", <|"DiagnosticSeconds" -> config["RouteDiagnosticSeconds"]|>];
  routeBefore = particleRouteAssessment[policy, corpus, config["RouteDiagnosticSeconds"],
    Function[value, particleJSON["curated-teacher-before.json", value]]];
  fullBefore = If[TrueQ[config["ModelGuidanceEnabled"]],
    particleFullBoardAssessment[policy, corpus, config["RouteDiagnosticSeconds"], config["FullBoardAuditMaxStates"]],
    <|"Passed" -> True, "Enabled" -> False, "Scope" -> "FullLegalBoard", "Records" -> {}|>];
  If[!particleJSON["full-board-teacher-before.json", fullBefore] || !TrueQ[Lookup[fullBefore, "Passed", False]],
    Return[<|"Status" -> "InitialFullBoardAuditFailed", "FullBoardAudit" -> fullBefore|>]];
  trainingBudget = particleNNBudgetAllowance[particleTimeBudgetPlan[config], particleStarted, AbsoluteTime[],
    particleDeadlineAbsoluteTime[Lookup[config, "DeadlineUTC", None]]];
  If[!particleJSON["training-budget.json", trainingBudget],
    Return[<|"Status" -> "TrainingBudgetReportWriteFailed"|>]];
  If[!TrueQ[trainingBudget["Passed"]],
    Return[<|"Status" -> trainingBudget["Reason"], "TrainingBudget" -> trainingBudget|>]];
  seconds = trainingBudget["EffectiveNNTimeSeconds"];
  particleStage["TrainingBudget", KeyDrop[trainingBudget, "PipelineSeconds"]];
  started = AbsoluteTime[];
  generator = Function[info, Module[{view, rowIndex},
    If[info["BatchSize"] =!= 1, invalidLoss = True; Abort[]];
    If[cursor > Length[order], epoch++;
      order = particleEpochIndices[samplingPlan, seed, epoch];
      epochCounts[epoch] = particleSamplingCounts[samplingPlan, order]; cursor = 1];
    rowIndex = order[[cursor++]];
    view = BlockRandom[
      SeedRandom[Hash[{seed, epoch, cursor, rowIndex, "TrainingAlternatives"}]];
      particleTrainRow[particleUpdateRow[corpus[[rowIndex]], config["TrainingUpdateCandidateBudget"]], count, tokens]];
    AssociationThread[Keys[view], List /@ Values[view]]
  ]];
  guard = Function[p,
    observed = particleProgressSnapshot[p, epochLength, started, seconds];
    If[!AssociationQ[observed], invalidProgress = True; observed = <|"AbsoluteBatch" -> 0|>; Abort[]];
    If[!particleProgressLossesFiniteQ[observed], invalidLoss = True; Abort[]]
  ];
  progress = Function[p,
    observed = particleProgressSnapshot[p, epochLength, started, seconds];
    If[!AssociationQ[observed], invalidProgress = True; observed = <|"AbsoluteBatch" -> 0|>; Abort[]];
    If[!particleProgressLossesFiniteQ[observed], invalidLoss = True; Abort[]];
    samplingReport = particleSamplingReport[samplingPlan, epochCounts, seed, observed["AbsoluteBatch"]];
    If[!AssociationQ[samplingReport], invalidProgress = True; Abort[]];
    observed = Join[observed, samplingReport,
      KeyTake[trainingBudget, {"RequestedNNTimeSeconds", "EffectiveNNTimeSeconds", "NNBudgetLimited", "FinalizationReserveSeconds"}]];
    If[!particleJSON["training-progress.json", observed], progressFailed = True; Abort[]];
    Print[DateString["ISODateTime"], " NN PROGRESS: ", observed];
    If[AbsoluteTime[] - lastCheckpointTime >= config["CheckEverySeconds"] &&
        AbsoluteTime[] - lastCheckpointAttemptTime >= Max[30, config["ProgressEverySeconds"]] &&
        observed["AbsoluteBatch"] > lastCheckpointBatch,
      If[!checkpoint[p["Net"], observed],
        consecutiveCheckpointFailures++;
        checkpointDecision = particleCheckpointDecision[lastCheckpointAttempt, consecutiveCheckpointFailures];
        If[checkpointDecision === "Abort", checkpointFailed = True; Abort[]];
        Print[DateString["ISODateTime"], " CHECKPOINT RETRY PENDING: ",
          KeyTake[lastCheckpointAttempt, {"AbsoluteBatch", "Gate", "FailureClass", "CheckpointSeconds"}]]]]
  ];
  particleStage["NNTraining", Join[<|"Rows" -> rowCount, "FixedActions" -> count, "FixedTokens" -> tokens,
    "RoundLength" -> epochLength,
    "DatasetBytes" -> ByteCount[corpus], "GeneratorExpressionBytes" -> ByteCount[generator],
    "PaddedCandidateBytesPerBatch" -> 8 count tokens 166, "NNTimeSeconds" -> seconds,
    "MaxRounds" -> maxRounds, "CheckpointEverySeconds" -> config["CheckEverySeconds"]|>,
    initialization, KeyDrop[trainingBudget, "PipelineSeconds"]]];
  properties = {"TrainedNet", "TotalRounds", "TotalBatches", "ReasonTrainingStopped", "TotalTrainingTime", "RoundLoss"};
  {elapsed, result} = AbsoluteTiming[TimeConstrained[CheckAbort[Check[
    NetTrain[trainer, {generator, "RoundLength" -> epochLength}, properties,
      BatchSize -> 1, MaxTrainingRounds -> maxRounds, TimeGoal -> seconds, TargetDevice -> "CPU",
      TrainingProgressReporting -> None, TrainingProgressMeasurements -> None,
      TrainingProgressFunction -> {{guard, "Interval" -> Quantity[1, "Batches"]},
        {progress, "Interval" -> Quantity[config["ProgressEverySeconds"], "Seconds"]}},
      Method -> {"ADAM", "LearningRate" -> config["LearningRate"]}],
    diverged = True; $Failed, {NetTrain::arrdiv}], $Aborted], seconds + trainingBudget["NNTimeoutGraceSeconds"], $Aborted]];
  If[diverged || invalidLoss || invalidProgress || progressFailed || checkpointFailed || !ListQ[result] || Length[result] =!= 6,
    Return[<|"Status" -> Which[diverged, "ArrayDivergence", invalidLoss, "NonfiniteProgressLoss",
      invalidProgress, "InvalidProgressCounters", progressFailed, "ProgressCheckpointWriteFailed",
      checkpointFailed, "SafeCheckpointFailed", True, "TrainingTimeoutOrMissingResults"],
      "Diverged" -> diverged, "NonfiniteLoss" -> invalidLoss, "ObservedUpdates" -> Lookup[observed, "AbsoluteBatch", 0],
      "CheckpointFailure" -> If[checkpointFailed, lastCheckpointAttempt, Null]|>]];
  trainer = First[result]; reason = If[StringQ[result[[4]]], result[[4]], ToString[result[[4]], InputForm]];
  If[!compactScaleFiniteNetQ[trainer] || !IntegerQ[result[[3]]] || result[[3]] < 1 ||
      !TrueQ[result[[2]] == Quotient[result[[3]], epochLength]] || !compactScaleFiniteQ[result[[5]]] ||
      !(compactScaleFiniteQ[result[[6]]] || (result[[2]] == 0 && result[[6]] === None)) ||
      !MemberQ[{"TimeGoal", "MaxTrainingRounds"}, reason], Return[<|"Status" -> "InvalidFinalTrainingState"|>]];
  policy = TimeConstrained[variableCompactParticlePolicy[NetExtract[trainer, "Policy"]], 60, $Failed];
  audit = TimeConstrained[particleAudit[trainer, policy, samples, count, tokens], 120, $Failed];
  If[!particleJSON["final-audit.json", audit] || !AssociationQ[audit] || !TrueQ[audit["Passed"]],
    Return[<|"Status" -> "FinalNativePortableAuditFailed"|>]];
  fit = particleFinalAssessment["Training", particleAssessment[policy, fitRows, tokens]];
  heldFit = particleFinalAssessment["Heldout", particleAssessment[policy, heldFitRows, tokens]];
  If[!AssociationQ[fit] || !TrueQ[Lookup[fit, "Passed", False]] ||
      !AssociationQ[heldFit] || !TrueQ[Lookup[heldFit, "Passed", False]],
    Return[<|"Status" -> "FinalFitAssessmentFailed", "TrainingAssessment" -> fit, "HeldoutAssessment" -> heldFit|>]];
  stats = Join[<|"Status" -> "Complete", "ActualRounds" -> result[[2]], "ActualUpdates" -> result[[3]],
    "CompletedFullRounds" -> Quotient[result[[3]], epochLength], "ReasonTrainingStopped" -> reason,
    "TrainingSeconds" -> result[[5]], "WallSeconds" -> elapsed, "TrainingRows" -> rowCount,
    "TrainingActionCount" -> count, "TrainingTokenCount" -> tokens, "NNTimeSeconds" -> seconds,
    "FinalRoundLoss" -> Replace[result[[6]], None -> Null], "OptimizerContinuity" -> "SingleNetTrainSession",
    "CheckpointResumeSemantics" -> "WeightsOnly; restarting NetTrain resets ADAM state",
    "FitSample" -> fit, "HeldoutTeacherFitSample" -> heldFit, "TrainingBudget" -> trainingBudget|>, initialization,
    particleSamplingReport[samplingPlan, epochCounts, seed, result[[3]]]];
  If[!checkpoint[trainer, Join[stats, <|"AbsoluteBatch" -> result[[3]], "Timestamp" -> DateString["ISODateTime"]|>], True],
    Return[<|"Status" -> "FinalCheckpointFailed", "CheckpointFailure" -> lastCheckpointAttempt|>]];
  particleStage["CuratedTeacherAfter", <|"DiagnosticSeconds" -> config["RouteDiagnosticSeconds"]|>];
  routeAfter = particleRouteAssessment[policy, corpus, config["RouteDiagnosticSeconds"],
    Function[value, particleJSON["curated-teacher-after.json", value]]];
  fullAfter = If[TrueQ[config["ModelGuidanceEnabled"]],
    particleFullBoardAssessment[policy, corpus, config["RouteDiagnosticSeconds"], config["FullBoardAuditMaxStates"]],
    <|"Passed" -> True, "Enabled" -> False, "Scope" -> "FullLegalBoard", "Records" -> {}|>];
  If[!particleJSON["full-board-teacher-after.json", fullAfter] || !TrueQ[Lookup[fullAfter, "Passed", False]],
    Return[<|"Status" -> "FinalFullBoardAuditFailed", "FullBoardAudit" -> fullAfter|>]];
  stats = Join[stats, <|"CuratedTeacherBefore" -> KeyDrop[routeBefore, "Records"],
    "CuratedTeacherAfter" -> KeyDrop[routeAfter, "Records"],
    "FullBoardTeacherBefore" -> KeyDrop[fullBefore, "Records"],
    "FullBoardTeacherAfter" -> KeyDrop[fullAfter, "Records"],
    "CuratedDiagnosticsDoNotGateModelPublication" -> True|>];
  If[!particleJSON["training-summary.json", stats], Return[<|"Status" -> "TrainingSummaryCheckpointFailed"|>]];
  <|"Status" -> "Complete", "Policy" -> policy, "Summary" -> stats, "ModelDirectory" -> publishedModelDirectory|>
];

(* ::Section::Closed:: *)
(*Dataset Generation, Verified Guidance, and Final Validation*)
particleMain[] := Module[
  {catalog = Global`particleCatalog, trainIndices, heldIndices, replayIndices, indicesValid, numericKeys, integerKeys,
    split, sourcePaths, hashes, workers, ready, generationStarted, generate, payloadBytes = 0,
    trainingJobs, heldJobs, replayJobs, rawTrain, rawHeld, rawReplay, merge, heldMerge, rows, heldRows,
    overlap, coverage, reverseCoverage, dataset, trained, baseMetadata, evaluations, evaluate, validationJobs,
    expectedCases, selected, summary, startingKeys, selectedTrainFamilies, selectedHeldFamilies,
    selectedTrainTemplates, selectedHeldTemplates, maximumRejected, trainingFour, trainingFive,
    originalCounts, eligibleIndices, eligibleCoverage, heldCaseLimit, replayCaseLimit, generationRows,
    validationDepths, validationSettings, selectionSummary, routeJobs, routeHeldOverlap, curatedRejected, routeMassFamilyKey,
    guidance, optimized, scorer, recovery, recoveryReports = {}, recoveryJobs = {}, recoveryRows = {},
    recoveryOptions, remaining, recovered, recoveryOrder, guidanceStarted, generationConfig,
    recoveryPlan, recoveryRemaining, attemptedRecoveryJobs = 0},
  trainIndices = Lookup[particleSettings, "TrainingIndices", {}]; heldIndices = Lookup[particleSettings, "HeldoutIndices", {}];
  replayIndices = Lookup[particleSettings, "ReplayIndices", {}];
  heldCaseLimit = Lookup[particleSettings, "ValidationHeldoutMaxCases", Max[1, Quotient[particleSettings["ValidationMaxCases"], 2]]];
  replayCaseLimit = Lookup[particleSettings, "ValidationReplayMaxCases", Max[1, Quotient[particleSettings["ValidationMaxCases"], 2]]];
  validationDepths = Lookup[particleSettings, "ValidationDepths", particleSettings["Depths"]];
  indicesValid[xx_] := ListQ[xx] && xx =!= {} && VectorQ[xx, IntegerQ] &&
    Min[xx] >= 1 && Max[xx] <= Length[catalog] && Length[DeleteDuplicates[xx]] === Length[xx];
  numericKeys = {"RowSeconds", "JobSeconds", "RouteJobSeconds", "GenerationSeconds", "MaxDatasetGiB", "MaxTrainingTensorMiB", "NNTimeSeconds", "LearningRate",
    "ProgressEverySeconds", "CheckEverySeconds", "CheckpointSeconds", "RouteDiagnosticSeconds", "CaseSeconds", "PipelineSeconds",
    "GuidanceSeconds", "RouteOptimizationSeconds", "RouteOptimizationTotalSeconds", "RecoveryTotalSeconds"};
  integerKeys = {"Scrambles", "FivePointScrambles", "HoldoutScrambles", "Kernels", "MaxActions", "TrainingCandidateBudget", "TrainingUpdateCandidateBudget", "MaxTokens", "MaxStateBytes",
    "MaxExpressionLeaves", "MaxTrainingRounds", "FitSampleSize", "CheckpointFitSampleSize", "MinimumTrainingStates",
    "ValidationMaxCases", "ValidationMaxSteps", "FullBoardAuditMaxStates", "RecoveryMaxJobs"};
  If[!AllTrue[Lookup[particleSettings, numericKeys], compactScaleFiniteQ[#] && # > 0. &] ||
      !AllTrue[Lookup[particleSettings, integerKeys], IntegerQ[#] && # > 0 &] ||
      particleGenerationChunkSize[particleSettings["GenerationChunkJobs"],
        Max[1, particleSettings["Kernels"]]] === $Failed ||
      !MemberQ[{"Packed", "Sparse"}, particleSettings["CandidateStorage"]] ||
      !AllTrue[{heldCaseLimit, replayCaseLimit}, IntegerQ[#] && # > 0 &] || particleSettings["CaseSeconds"] <= 2 ||
      !MemberQ[{True, False}, particleSettings["ModelGuidanceEnabled"]] ||
      !MemberQ[{True, False}, particleSettings["OptimizeSimplificationRoutes"]] ||
      !AllTrue[Lookup[particleSettings, {"FullBoardGuidanceStatesPerRoute", "HardNegativeCount", "RecoveryAttempts"}],
        IntegerQ[#] && # >= 0 &] ||
      !AllTrue[Lookup[particleSettings, {"RecoveryRolloutSteps", "RecoveryDepth", "RecoveryWidth", "RecoveryNodeLimit"}],
        IntegerQ[#] && # > 0 &] ||
      !IntegerQ[particleSettings["RandomSeed"]] || !IntegerQ[particleSettings["Seed"]] ||
      !indicesValid[trainIndices] || !indicesValid[heldIndices] || !indicesValid[replayIndices] ||
      Intersection[trainIndices, heldIndices] =!= {} || Complement[replayIndices, trainIndices] =!= {} ||
      !VectorQ[particleSettings["Depths"], IntegerQ[#] && # > 0 &] || particleSettings["Depths"] === {} ||
      !VectorQ[validationDepths, IntegerQ[#] && # > 0 &] || validationDepths === {} ||
      !TrueQ[0. <= particleSettings["MaximumRejectedStateFraction"] <= 0.2] ||
      !TrueQ[0. < particleSettings["MinimumReferenceReverseCoverage"] <= 1.] || compactParticleFeatureWidth =!= 166 ||
      !validRoutePermutationCountQ[particleSettings["RoutePermutations"]] ||
      !(particleSettings["CuratedTrainingFraction"] === Automatic ||
        (compactScaleFiniteQ[particleSettings["CuratedTrainingFraction"]] &&
          0. < particleSettings["CuratedTrainingFraction"] < 1.)),
    particleFail["InvalidConfigurationOrReferenceSelection"]];
  particleSettings["RouteProgressDirectory"] = FileNameJoin[{Global`particleRun, "route-progress"}];
  If[!DirectoryQ[particleSettings["RouteProgressDirectory"]],
    Quiet[CreateDirectory[particleSettings["RouteProgressDirectory"]]]];
  If[!DirectoryQ[particleSettings["RouteProgressDirectory"]], particleFail["RouteProgressDirectoryFailed"]];
  guidance = compactScaleLoadGuidanceWeights[particleSettings];
  If[!AssociationQ[guidance] || !TrueQ[Lookup[guidance, "Passed", False]], particleFail["IncompatibleGuidanceWarmStart"]];
  If[!particleJSON["model-guidance.json", guidance], particleFail["GuidanceReportWriteFailed"]];
  If[TrueQ[particleSettings["OptimizeSimplificationRoutes"]],
    particleStage["RouteOptimization"];
    optimized = trainingOptimizeSimplificationRoutes[particleSettings["SimplificationRoutes"],
      <|"Seconds" -> particleSettings["RouteOptimizationSeconds"],
        "TotalSeconds" -> particleSettings["RouteOptimizationTotalSeconds"], "PreserveDirections" -> True|>];
    If[!AssociationQ[optimized] || !validSimplificationRoutesQ[Lookup[optimized, "Routes", $Failed]],
      particleFail["InvalidOptimizedSimplificationRoutes"]];
    If[!particleJSON["route-optimization.json", KeyDrop[optimized, "Routes"]], particleFail["RouteOptimizationReportWriteFailed"]];
    particleSettings["SimplificationRoutes"] = optimized["Routes"]];
  routeJobs = compactScaleRouteJobs[particleSettings];
  If[routeJobs === $Failed, particleFail["InvalidSimplificationRoutes"]];
  If[!particleWXF["curated-route-jobs.wxf", routeJobs], particleFail["CuratedRouteCheckpointFailed"]];
  routeMassFamilyKey[record_] := IntegerString[Hash[Sort[Cases[record["Masses"],
    HoldPattern[Mass[_Integer] -> mass_] :> mass]], "SHA256"], 16, 64];
  routeHeldOverlap = Intersection[routeMassFamilyKey /@ particleSettings["SimplificationRoutes"],
    routeMassFamilyKey /@ catalog[[heldIndices]]];
  If[routeHeldOverlap =!= {}, particleFail["CuratedRoutesOverlapHeldoutMassFamilies",
    <|"MassFamilyKeys" -> routeHeldOverlap,
      "RouteNames" -> Lookup[particleSettings["SimplificationRoutes"], "Name", {}]|>]];
  selectedTrainFamilies = DeleteDuplicates[Lookup[Select[catalog[[trainIndices]], TrueQ[Lookup[#, "Synthetic", False]] &], "Family", {}]];
  selectedHeldFamilies = DeleteDuplicates[Lookup[Select[catalog[[heldIndices]], TrueQ[Lookup[#, "Synthetic", False]] &], "Family", {}]];
  selectedTrainTemplates = DeleteDuplicates[Lookup[Select[catalog[[trainIndices]], TrueQ[Lookup[#, "Synthetic", False]] &], "Template", {}]];
  selectedHeldTemplates = DeleteDuplicates[Lookup[Select[catalog[[heldIndices]], TrueQ[Lookup[#, "Synthetic", False]] &], "Template", {}]];
  split = <|"TrainingIndices" -> trainIndices, "HeldoutIndices" -> heldIndices, "ReplayIndices" -> replayIndices,
    "TrainingReferences" -> Length[trainIndices], "HeldoutReferences" -> Length[heldIndices],
    "SyntheticTrainingFamilies" -> selectedTrainFamilies, "SyntheticHeldoutFamilies" -> selectedHeldFamilies,
    "FamilyDisjoint" -> (Intersection[selectedTrainFamilies, selectedHeldFamilies] === {}),
    "TemplateDisjoint" -> (Intersection[selectedTrainTemplates, selectedHeldTemplates] === {}),
    "ReferenceDisjoint" -> True, "PermutationAugmentation" -> True,
    "CuratedRouteNames" -> Lookup[particleSettings["SimplificationRoutes"], "Name", {}],
    "CuratedRouteJobs" -> Length[routeJobs], "CuratedHeldoutMassFamiliesDisjoint" -> True,
    "RoutePermutations" -> If[particleSettings["RoutePermutations"] === All, "All", particleSettings["RoutePermutations"]],
    "RequestedCuratedTrainingFraction" -> Replace[particleSettings["CuratedTrainingFraction"], Automatic -> Null],
    "SyntheticInterpretation" -> "Algebraic fixtures, not physical five-point amplitudes",
    "SelectedReferences" -> KeyTake[#, {"Name", "Family", "Template", "Synthetic", "ExternalLegCount"}] & /@
      catalog[[Join[trainIndices, heldIndices]]]|>;
  If[!particleJSON["split.json", split], particleFail["ReferenceSplitCheckpointFailed"]];
  sourcePaths = Join[FileNames["*.wl", FileNameJoin[{Global`particleRoot, "source"}]],
    FileNameJoin[{Global`particleRun, #}] & /@ {"unscrambling.wl", "training-amplitudes.wl",
      "compact-scale-data.wl", "compact-scale-training.wl", "compact-scale-run.wl"}];
  hashes = AssociationThread[FileNameTake /@ sourcePaths, compactScaleFileHash /@ sourcePaths];
  If[!particleJSON["source-hashes.json", hashes], particleFail["SourceHashCheckpointFailed"]];
  particleStage["WorkerSetup", <|"TrainingReferences" -> Length[trainIndices], "HeldoutReferences" -> Length[heldIndices],
    "RequestedWorkers" -> particleSettings["Kernels"]|>];
  workers = TimeConstrained[LaunchKernels[particleSettings["Kernels"]], 90, $Failed];
  If[!ListQ[workers] || Length[workers] =!= particleSettings["Kernels"], particleFail["WorkerLaunchFailure"]];
  ready = With[{root = Global`particleRoot, run = Global`particleRun, expected = hashes["unscrambling.wl"], config = particleSettings},
    TimeConstrained[ParallelEvaluate[
      SetEnvironment[{"OMP_NUM_THREADS" -> "1", "OMP_DYNAMIC" -> "FALSE", "OPENBLAS_NUM_THREADS" -> "1",
        "MKL_NUM_THREADS" -> "1", "MXNET_CPU_WORKER_NTHREADS" -> "1"}];
      Global`$HEPCATpath = FileNameJoin[{root, "source"}];
      Block[{Print}, Get[FileNameJoin[{Global`$HEPCATpath, "HEPCAT.wl"}]];
        Get[FileNameJoin[{run, "unscrambling.wl"}]]];
      Begin["Unscrambling`Private`"];
      Get[FileNameJoin[{run, "compact-scale-data.wl"}]]; End[];
      Unscrambling`Private`$onShellChannels = Automatic; Unscrambling`Private`$useMomentumConservation = True;
      IntegerString[FileHash[FileNameJoin[{run, "unscrambling.wl"}], "SHA256"], 16, 64] === expected &&
        TrueQ[Lookup[Unscrambling`Private`compactScaleLoadGuidanceWeights[config], "Passed", False]],
      workers, DistributedContexts -> None], 90, $Failed]];
  If[ready =!= ConstantArray[True, Length[workers]], particleFail["WorkerInitializationFailed"]];
  generationStarted = AbsoluteTime[];
  particleStage["OriginalLegalMoveAudit", <|"References" -> Length[trainIndices]|>];
  originalCounts = With[{references = catalog[[trainIndices]], seconds = particleSettings["RowSeconds"]},
    TimeConstrained[ParallelMap[Function[record, TimeConstrained[
      Length[Unscrambling`Private`legalMoves[
        Unscrambling`Private`canonicalAmplitude[record["Amplitude"], record["Masses"]], record["Masses"]]],
      seconds, -1]], references, Method -> "FinestGrained", DistributedContexts -> None],
      particleSettings["GenerationSeconds"], $Failed]];
  If[!VectorQ[originalCounts, IntegerQ[#] && # >= 1 &] || Length[originalCounts] =!= Length[trainIndices],
    particleFail["OriginalLegalMoveAuditFailed"]];
  eligibleIndices = Pick[trainIndices, # > 1 & /@ originalCounts];
  If[eligibleIndices === {}, particleFail["NoReferencesWithScrambleMoves"]];
  generate[jobs_, kind_] := Module[{results = {}, got, remaining, delta},
    Do[
      remaining = particleSettings["GenerationSeconds"] - (AbsoluteTime[] - generationStarted);
      If[remaining <= 0, particleFail["GenerationDeadline"]];
      got = With[{input = chunk, config = particleSettings, maxTokens = particleSettings["MaxTokens"]}, TimeConstrained[
        ParallelMap[Function[job, Block[
          {Unscrambling`Private`compactActionFeatures = Function[{expression, rules, candidates}, Module[{f},
            f = Unscrambling`Private`compactParticleFeatures[expression, rules, candidates];
            If[Length[Dimensions[f]] =!= 3 || Dimensions[f][[2]] > maxTokens, $Failed, f]]],
            Unscrambling`Private`compactActionFeatureWidth = 166},
          Unscrambling`Private`compactScaleGenerationJob[job, config]]], input,
          Method -> "FinestGrained", DistributedContexts -> None], remaining, $Failed]];
      If[!ListQ[got] || Length[got] =!= Length[chunk] || !AllTrue[got, AssociationQ], particleFail["ParallelGenerationFailed"]];
      If[AnyTrue[got, TrueQ[Lookup[#, "Curated", False]] && !TrueQ[Lookup[#, "Completed", False]] &],
        particleFail["CuratedRouteGenerationFailed", <|"Jobs" -> KeyTake[#,
          {"ReferenceName", "Sequence", "Failure", "FailureDetails", "Completed", "Seconds"}] & /@
          Select[got, TrueQ[Lookup[#, "Curated", False]] && !TrueQ[Lookup[#, "Completed", False]] &]|>]];
      delta = Total[ByteCount[Lookup[#, "Rows", {}]] & /@ got]; payloadBytes += delta;
      If[payloadBytes > particleSettings["MaxDatasetGiB"] 1024^3, particleFail["DatasetMemoryBudget", <|"RowPayloadBytes" -> payloadBytes|>]];
      results = Join[results, got];
      particleStage["DataGeneration", <|"Kind" -> kind, "CompletedJobs" -> Length[results], "TotalJobs" -> Length[jobs],
        "GeneratedRows" -> Total[Lookup[results, "AcceptedRows", 0]], "RowPayloadBytes" -> payloadBytes,
        "MomentumSumTargetRows" -> Count[Flatten[Lookup[results, "Rows", {}], 1],
          row_ /; MemberQ[Lookup[row, "TargetOperations", {}], "MomentumSum"]]|>],
      {chunk, Partition[jobs, UpTo[particleGenerationChunkSize[
        particleSettings["GenerationChunkJobs"], Length[workers]]]]}]; results
  ];
  trainingFive = Select[trainIndices, Lookup[catalog[[#]], "ExternalLegCount", 4] === 5 &];
  trainingFour = Complement[trainIndices, trainingFive];
  trainingJobs = Join[routeJobs, compactScaleJobs[catalog, trainingFour, particleSettings, "Training"],
    compactScaleJobs[catalog, trainingFive, Join[particleSettings, <|"Scrambles" -> particleSettings["FivePointScrambles"]|>], "Training"]];
  trainingJobs = MapIndexed[Join[#1, <|"Order" -> First[#2]|>] &, trainingJobs];
  validationSettings = Join[particleSettings, <|"Depths" -> validationDepths|>];
  heldJobs = compactScaleJobs[catalog, heldIndices, validationSettings, "Heldout"];
  replayJobs = compactScaleJobs[catalog, replayIndices, validationSettings, "Validation"];
  rawTrain = generate[trainingJobs, "Training"]; rawHeld = generate[heldJobs, "Heldout"];
  rawReplay = generate[replayJobs, "FourPointReplay"]; CloseKernels[workers]; workers = {};
  If[TrueQ[particleSettings["ModelGuidanceEnabled"]] && particleSettings["RecoveryAttempts"] > 0,
    scorer = Function[{state, rules, candidates}, trainingCompactCandidateScores[$compactGuidanceWeights, state, rules, candidates]];
    recoveryOrder = Max[Join[{0}, Lookup[trainingJobs, "Order", {}]]]; guidanceStarted = AbsoluteTime[];
    recoveryPlan = particleRecoverySourceJobs[routeJobs, particleSettings["RecoveryMaxJobs"]];
    Do[
      recoveryRemaining = particleSettings["RecoveryTotalSeconds"] - (AbsoluteTime[] - guidanceStarted);
      remaining = Min[recoveryRemaining, particleSettings["GenerationSeconds"] - (AbsoluteTime[] - generationStarted)];
      If[remaining <= 0,
        AppendTo[recoveryReports, <|"Passed" -> False, "Reason" -> "RecoveryPreparationTimeLimit", "NoPartialRows" -> True,
          "ReferenceName" -> job["ReferenceName"], "Sequence" -> job["Sequence"]|>]; Break[]];
      particleStage["VerifiedLearnerRecovery", <|"ReferenceName" -> job["ReferenceName"], "Sequence" -> job["Sequence"],
        "AcceptedRecoveryJobs" -> Length[recoveryJobs], "RemainingGenerationSeconds" -> remaining|>];
      recoveryOptions = <|"MaxAttempts" -> particleSettings["RecoveryAttempts"],
        "RolloutSteps" -> particleSettings["RecoveryRolloutSteps"], "MaxDepth" -> particleSettings["RecoveryDepth"],
        "BeamWidth" -> particleSettings["RecoveryWidth"], "MaxNodes" -> particleSettings["RecoveryNodeLimit"],
        "Seconds" -> particleSettings["GuidanceSeconds"], "StepTimeLimit" -> particleSettings["RowSeconds"],
        "MaxExpressionLeaves" -> particleSettings["MaxExpressionLeaves"],
        "AnchorSteps" -> compactScaleRecoveryAnchorSteps[job, particleSettings]|>;
      attemptedRecoveryJobs++;
      recovery = TimeConstrained[trainingLearnerRecoveryExamples[job, scorer, recoveryOptions], remaining, $TimedOut];
      If[!AssociationQ[recovery] || !ListQ[Lookup[recovery, "Routes", None]] || !ListQ[Lookup[recovery, "Reports", None]],
        AppendTo[recoveryReports, <|"Passed" -> False, "Reason" -> If[recovery === $TimedOut,
          "RecoveryPreparationTimeLimit", "InvalidRecoveryResult"], "ReferenceName" -> job["ReferenceName"],
          "Sequence" -> job["Sequence"], "NoPartialRows" -> True|>]; Continue[]];
      recoveryReports = Join[recoveryReports, recovery["Reports"]];
      Do[
        remaining = Min[particleSettings["RecoveryTotalSeconds"] - (AbsoluteTime[] - guidanceStarted),
          particleSettings["GenerationSeconds"] - (AbsoluteTime[] - generationStarted)];
        If[remaining <= 0,
          AppendTo[recoveryReports, <|"Passed" -> False, "Reason" -> "RecoveryEncodingTimeLimit",
            "ReferenceName" -> job["ReferenceName"], "NoPartialRows" -> True|>]; Break[]];
        recovered = Join[job, candidateJob, <|"Order" -> ++recoveryOrder, "Kind" -> "Training",
          "Mode" -> "SimplificationRoute", "Curated" -> True, "ModelRecovery" -> True|>];
        generationConfig = Join[particleSettings, <|"RouteJobSeconds" -> Min[remaining, particleSettings["RouteJobSeconds"]]|>];
        recovered = Block[{compactActionFeatures = Function[{expression, rules, candidates}, Module[{features},
            features = compactParticleFeatures[expression, rules, candidates];
            If[Length[Dimensions[features]] =!= 3 || Dimensions[features][[2]] > particleSettings["MaxTokens"], $Failed, features]]],
            compactActionFeatureWidth = 166}, compactScaleRouteGenerationJob[recovered, generationConfig]];
        If[!TrueQ[Lookup[recovered, "Completed", False]],
          AppendTo[recoveryReports, Join[<|"Passed" -> False, "Reason" -> "RecoveryContinuationRejected"|>,
            KeyTake[recovered, {"ReferenceName", "Sequence", "Failure", "FailureDetails"}]]]; Continue[]];
        AppendTo[recoveryJobs, Join[candidateJob, <|"Order" -> recoveryOrder, "Kind" -> "Training",
          "Mode" -> "SimplificationRoute", "Curated" -> True, "ModelRecovery" -> True|>]];
        AppendTo[recoveryRows, recovered], {candidateJob, recovery["Routes"]}],
      {job, recoveryPlan["Jobs"]}];
    payloadBytes += Total[ByteCount[Lookup[#, "Rows", {}]] & /@ recoveryRows];
    If[payloadBytes > particleSettings["MaxDatasetGiB"] 1024^3, particleFail["DatasetMemoryBudget", <|"RowPayloadBytes" -> payloadBytes|>]];
    rawTrain = Join[rawTrain, recoveryRows]; trainingJobs = Join[trainingJobs, recoveryJobs];
    If[!particleJSON["learner-recovery.json", particleCheckpointJSONValue[<|"Enabled" -> True,
        "AvailableJobs" -> recoveryPlan["AvailableJobs"], "SelectedJobs" -> recoveryPlan["SelectedJobs"],
        "AttemptedJobs" -> attemptedRecoveryJobs, "DeferredJobs" -> recoveryPlan["AvailableJobs"] - attemptedRecoveryJobs,
        "UnselectedJobs" -> recoveryPlan["AvailableJobs"] - recoveryPlan["SelectedJobs"],
        "UnattemptedSelectedJobs" -> recoveryPlan["SelectedJobs"] - attemptedRecoveryJobs,
        "SelectionMethod" -> recoveryPlan["SelectionMethod"], "NoPartialRows" -> True,
        "AugmentationBounds" -> KeyTake[particleSettings, {"RecoveryMaxJobs", "RecoveryTotalSeconds", "RecoveryAttempts",
          "RecoveryRolloutSteps", "RecoveryDepth", "RecoveryWidth", "RecoveryNodeLimit", "GuidanceSeconds"}],
        "AcceptedJobs" -> Length[recoveryJobs], "AcceptedRows" -> Total[Lookup[recoveryRows, "AcceptedRows", 0]],
        "Seconds" -> N[AbsoluteTime[] - guidanceStarted], "ElapsedSeconds" -> N[AbsoluteTime[] - guidanceStarted],
        "Reports" -> recoveryReports|>]] ||
        !particleWXF["learner-recovery-jobs.wxf", recoveryJobs], particleFail["RecoveryReportWriteFailed"]]];
  particleStage["DatasetMerge", <|"TrainingJobs" -> Length[rawTrain], "GeneratedRowPayloadBytes" -> payloadBytes|>];
  merge = particleMerge[Flatten[Lookup[rawTrain, "Rows"], 1], particleSettings];
  heldMerge = particleMerge[Flatten[Lookup[rawHeld, "Rows"], 1], particleSettings];
  curatedRejected = Select[merge["Rejected"], TrueQ[Lookup[#, "Curated", False]] &];
  If[curatedRejected =!= {}, particleFail["CuratedTrainingRowsRejected", <|"Rejected" -> curatedRejected|>]];
  rows = merge["Rows"]; heldRows = heldMerge["Rows"];
  overlap = Intersection[Lookup[rows, "StateKey"], Lookup[heldRows, "StateKey"]];
  coverage = MapIndexed[Function[{index, position}, Module[{accepted},
    accepted = Select[rows, AnyTrue[#["Provenance"], #["AmplitudeIndex"] === index &] &];
    <|"AmplitudeIndex" -> index, "Name" -> catalog[[index]]["Name"], "AcceptedStates" -> Length[accepted],
      "OriginalLegalMoves" -> originalCounts[[First[position]]], "OriginallyStopOnly" -> (originalCounts[[First[position]]] === 1),
      "OriginalLegalAuditTimedOut" -> (originalCounts[[First[position]]] === -1),
      "ReverseStates" -> Count[accepted, row_ /; AnyTrue[row["Provenance"],
        #["AmplitudeIndex"] === index && !TrueQ[#["StopExample"]] &]]|>
  ]], trainIndices];
  reverseCoverage = N[Count[Lookup[coverage, "ReverseStates"], _?Positive]/Length[coverage]];
  eligibleCoverage = N[Count[Lookup[Select[coverage, MemberQ[eligibleIndices, #["AmplitudeIndex"]] &], "ReverseStates"], _?Positive]/Length[eligibleIndices]];
  maximumRejected = Max[N[merge["RejectedStates"]/Max[1, merge["UniqueStates"]]],
    N[heldMerge["RejectedStates"]/Max[1, heldMerge["UniqueStates"]]]];
  dataset = <|"TrainingStates" -> Length[rows], "HeldoutStates" -> Length[heldRows], "Coverage" -> coverage,
    "CuratedRouteJobs" -> Length[routeJobs],
    "CuratedTrainingStates" -> Count[rows, row_ /; AnyTrue[row["Provenance"], TrueQ[Lookup[#, "Curated", False]] &]],
    "ModelGuidanceEnabled" -> particleSettings["ModelGuidanceEnabled"],
    "FullBoardGuidanceStates" -> Count[rows, row_ /; particleAuditProofs[row] =!= {}],
    "HardNegativeActions" -> Total[Length[Lookup[#, "HardNegativeKeys", {}]] & /@ rows],
    "VerifiedRecoveryJobs" -> Length[recoveryJobs],
    "VerifiedRecoveryStates" -> Count[rows, row_ /; AnyTrue[row["Provenance"], TrueQ[Lookup[#, "ModelRecovery", False]] &]],
    "RequestedCuratedTrainingFraction" -> Replace[particleSettings["CuratedTrainingFraction"], Automatic -> Null],
    "ReferenceReverseCoverageFraction" -> reverseCoverage, "RejectedStateFraction" -> maximumRejected,
    "EligibleReferences" -> Length[eligibleIndices], "OriginallyStopOnlyReferences" -> Count[originalCounts, 1],
    "OriginalLegalAuditTimeouts" -> Count[originalCounts, -1], "EligibleReferenceReverseCoverageFraction" -> eligibleCoverage,
    "TrainingMerge" -> KeyDrop[merge, "Rows"], "HeldoutMerge" -> KeyDrop[heldMerge, "Rows"],
    "StateOverlap" -> Length[overlap], "SkippedReasons" -> Counts[Lookup[Flatten[Lookup[Join[rawTrain, rawHeld], "Skipped"], 1], "Reason", {}]],
    "CompletedTrainingJobs" -> Count[Lookup[rawTrain, "Completed"], True], "TrainingJobs" -> Length[rawTrain],
    "TargetOperationCounts" -> Counts[Flatten[Lookup[rows, "TargetOperations", {}]]],
    "CandidateStorage" -> particleSettings["CandidateStorage"],
    "MomentumSumTargetRows" -> Count[rows, row_ /; MemberQ[Lookup[row, "TargetOperations", {}], "MomentumSum"]],
    "UnpaddedDatasetBytes" -> ByteCount[rows], "GenerationSeconds" -> N[AbsoluteTime[] - generationStarted],
    "ActionCountRange" -> MinMax[Lookup[rows, "ActionCount", 0]],
    "FullLegalActionCountRange" -> MinMax[Lookup[#, "FullActionCount", #["ActionCount"]] & /@ rows],
    "TrainingCandidateBudget" -> particleSettings["TrainingCandidateBudget"],
    "TrainingUpdateCandidateBudget" -> particleSettings["TrainingUpdateCandidateBudget"],
    "SampledTrainingStates" -> Count[Lookup[rows, "Sampled", False], True],
    "CandidateBudgetExpandedStates" -> Count[Lookup[rows, "CandidateBudgetExpanded", False], True],
    "TrainingCandidateObjective" -> "SampledLegalMoveCrossEntropy",
    "TokenCountRange" -> MinMax[Dimensions[#["Candidates"]][[2]] & /@ rows]|>;
  If[!particleJSON["dataset.json", dataset] || !particleWXF["training-rows.wxf", rows] ||
      !particleWXF["curated-training-rows.wxf", Select[rows, particleCuratedRowQ]] ||
      !particleWXF["validation-cases.wxf", KeyDrop[#, "Rows"] & /@ Join[rawHeld, rawReplay]], particleFail["DatasetCheckpointFailed"]];
  If[Length[rows] < particleSettings["MinimumTrainingStates"] || heldRows === {} || overlap =!= {} ||
      dataset["MomentumSumTargetRows"] < Lookup[particleSettings, "MinimumMomentumSumTargetRows", 0] ||
      maximumRejected > particleSettings["MaximumRejectedStateFraction"] || !TrueQ[merge["CompactRows"]] ||
      !AllTrue[Lookup[coverage, "AcceptedStates"], Positive] ||
      eligibleCoverage < particleSettings["MinimumReferenceReverseCoverage"] ||
      !AllTrue[rows, particleRowQ[#, Max[Lookup[rows, "ActionCount"]], particleSettings["MaxTokens"]] &] ||
      !AllTrue[heldRows, particleRowQ[#, Max[Lookup[heldRows, "ActionCount"]], particleSettings["MaxTokens"]] &],
    particleFail["InsufficientOrInvalidDataset", KeyTake[dataset,
      {"TrainingStates", "HeldoutStates", "StateOverlap", "EligibleReferenceReverseCoverageFraction", "RejectedStateFraction"}]]];
  startingKeys = Lookup[rows, "StateKey"];
  validationJobs = KeyDrop[#, "Rows"] & /@ Join[
    particleValidationCases[rawHeld, heldCaseLimit], particleValidationCases[rawReplay, replayCaseLimit]];
  expectedCases = Length[validationJobs];
  If[expectedCases < 2 || !AllTrue[{"Heldout", "Validation"}, MemberQ[Lookup[validationJobs, "Kind"], #] &],
    particleFail["MissingHeldoutOrReplayValidationCases"]];
  selectionSummary = <|"ValidationDepths" -> validationDepths,
    "SelectionRule" -> "Round-robin reference quotas with rotated scramble depths",
    "HeldoutCaseLimit" -> heldCaseLimit, "ReplayCaseLimit" -> replayCaseLimit,
    "SelectedCases" -> KeyTake[#, {"AmplitudeIndex", "Kind", "Sequence", "Steps", "ActualSteps", "ReferenceName"}] & /@ validationJobs,
    "SelectedHeldoutReferences" -> DeleteDuplicates[Lookup[Select[validationJobs, #["Kind"] === "Heldout" &], "AmplitudeIndex"]],
    "SelectedReplayReferences" -> DeleteDuplicates[Lookup[Select[validationJobs, #["Kind"] === "Validation" &], "AmplitudeIndex"]]|>;
  If[!particleJSON["validation-selection.json", selectionSummary], particleFail["ValidationSelectionCheckpointFailed"]];
  baseMetadata = <|"Encoding" -> "StateUTF8AndNumericActions", "Architecture" -> "CompactParticleActions",
    "DescriptorEncoding" -> "ParticleRoleTokens", "DescriptorFeatureWidth" -> 166,
    "RuntimeEvaluation" -> "PortableVariableParticleTokens", "CompactFeatureSchema" -> compactParticleFeatureSchema,
    "Objective" -> "SampledLegalMoveCrossEntropy", "TrainingCandidateBudget" -> particleSettings["TrainingCandidateBudget"],
    "TrainingUpdateCandidateBudget" -> particleSettings["TrainingUpdateCandidateBudget"],
    "CandidateSampling" -> If[TrueQ[particleSettings["ModelGuidanceEnabled"]],
      "AllTeachersAndStopWithFullBoardHardNegativesAndRotatingAlternatives",
      "AllTeacherPositivesAndStopWithRotatingPoolAlternatives"],
    "ModelGuidance" -> guidance, "HardNegativeCount" -> particleSettings["HardNegativeCount"],
    "VerifiedRecoveryJobs" -> Length[recoveryJobs],
    "InferenceCandidates" -> "AllLegalMovesGloballyRanked", "ReadoutNormalization" -> "SeparateMeanMaxL2",
    "ReadoutSquaredNormEpsilon" -> 1.*^-12, "RuntimeCoreSourceEncoding" -> "SectionedLegalMathAndParticleTokens",
    "RuntimeCoreSourceSHA256" -> $compactParticleSourceFingerprint, "OnShellChannels" -> Automatic,
    "MomentumConservation" -> True, "SourceHashes" -> hashes, "DatasetHashEncoding" -> "SHA256File",
    "DatasetHash" -> compactScaleFileHash[FileNameJoin[{Global`particleRun, "training-rows.wxf"}]], "ReferenceSplit" -> split|>;
  rawTrain = {}; rawHeld = {}; rawReplay = {}; trainingJobs = {}; heldJobs = {}; replayJobs = {};
  merge = KeyDrop[merge, "Rows"]; heldMerge = KeyDrop[heldMerge, "Rows"];
  trained = particleTrain[rows, heldRows, particleSettings, baseMetadata];
  If[trained["Status"] =!= "Complete", particleFail["TrainingFailed", KeyDrop[trained, {"Policy"}]]];
  particleStage["FinalValidation", <|"ExpectedCases" -> expectedCases|>];
  evaluate[job_] := TimeConstrained[Module[{trace, seconds, expr = job["Expression"], original = job["Original"], pair = job["Pair"], key},
    key = compactScaleStateKey[stateFeature[expr, pair[[1]], stateLegIDs[expr, pair[[1]]]]];
    {seconds, trace} = AbsoluteTiming[UnscrambleTrace[{pair[[1]], expr}, "PolicyMethod" -> "CompactActions",
      "ModelDirectory" -> trained["ModelDirectory"], "OnShellChannels" -> Automatic, "MomentumConservation" -> True,
      "MaxSteps" -> particleSettings["ValidationMaxSteps"], "Attempts" -> 1, "MaxStagnantSteps" -> 4,
      "Kernels" -> 1, "ProgressReporting" -> False, "TimeLimit" -> particleSettings["CaseSeconds"] - 2]];
    If[!AssociationQ[trace], Return[<|"AmplitudeIndex" -> job["AmplitudeIndex"], "Kind" -> job["Kind"],
      "Evaluated" -> False, "Reason" -> "InvalidTrace"|>]];
    Join[<|"AmplitudeIndex" -> job["AmplitudeIndex"], "Kind" -> job["Kind"], "Evaluated" -> True,
      "StartingStateSeen" -> MemberQ[startingKeys, key], "ActualSteps" -> job["ActualSteps"],
      "Recovered" -> Boole[samePoly[trace["Result"][[2]], original]],
      "Simpler" -> Boole[expressionComplexity[trace["Result"][[2]]] < expressionComplexity[expr]],
      "ModelLoaded" -> Lookup[trace, "ModelLoaded", False], "TerminationReason" -> Lookup[trace, "TerminationReason", "Unknown"],
      "ScoringFailed" -> Lookup[trace, "ScoringFailed", True], "Seconds" -> seconds|>, particleValidationTraceSummary[trace]]
  ], particleSettings["CaseSeconds"], <|"AmplitudeIndex" -> job["AmplitudeIndex"], "Kind" -> job["Kind"],
    "Evaluated" -> False, "Reason" -> "CaseTimeout"|>];
  evaluations = {};
  Do[AppendTo[evaluations, evaluate[job]];
    If[!particleJSON["validation-results.json", evaluations], particleFail["ValidationCheckpointFailed"]];
    Print[DateString["ISODateTime"], " VALIDATION: ", Last[evaluations]], {job, validationJobs}];
  selected = Select[evaluations, TrueQ[#["Evaluated"]] &];
  summary = <|"Status" -> "Complete", "Training" -> trained["Summary"], "Dataset" -> dataset,
    "ExpectedCases" -> expectedCases, "EvaluatedCases" -> Length[selected], "Recovered" -> Total[Lookup[selected, "Recovered", {}]],
    "Simpler" -> Total[Lookup[selected, "Simpler", {}]],
    "ExcludedReasons" -> Counts[Lookup[Select[evaluations, !TrueQ[#["Evaluated"]] &], "Reason", {}]],
    "ScoringFailures" -> Count[Lookup[selected, "ScoringFailed", {}], True],
    "ModelLoadFailures" -> Count[Lookup[selected, "ModelLoaded", {}], Except[True]],
    "HeldoutStartingStateSeen" -> Count[Lookup[Select[selected, #["Kind"] === "Heldout" &], "StartingStateSeen", {}], True],
    "Interpretation" -> "Recorded reverse-move training; disjoint reference holdouts; synthetic five-point fixtures",
    "ModelInstalledIntoProduction" -> False, "PipelineSeconds" -> N[AbsoluteTime[] - particleStarted]|>;
  If[Length[selected] =!= expectedCases || summary["ScoringFailures"] > 0 || summary["ModelLoadFailures"] > 0 ||
      summary["HeldoutStartingStateSeen"] > 0, summary["Status"] = "ValidationGateFailed";
    particleJSON["summary.json", summary]; particleFail["PublicLoaderOrValidationGateFailed", KeyTake[summary,
      {"EvaluatedCases", "ScoringFailures", "ModelLoadFailures", "HeldoutStartingStateSeen", "ExcludedReasons"}]]];
  If[!particleJSON["summary.json", summary], particleFail["SummaryCheckpointFailed"]];
  particleStage["Complete", KeyTake[summary, {"EvaluatedCases", "Recovered", "Simpler", "ScoringFailures"}]];
  True
];

particlePipelineRemaining = particleSettings["PipelineSeconds"] - (AbsoluteTime[] - particleStarted);
If[particlePipelineRemaining <= 0, particleFail["PipelineDeadlineBeforePreparation"]];
particleCompleted = TimeConstrained[CheckAbort[particleMain[], $Aborted], particlePipelineRemaining, $Aborted];
If[!TrueQ[particleCompleted], particleFail["PipelineDeadlineOrAbort"]];
End[];
Quit[If[TrueQ[Unscrambling`Private`particleCompleted], 0, 1]];
