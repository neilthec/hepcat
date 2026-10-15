root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Global`$HEPCATpath = FileNameJoin[{root, "source"}];
Block[{Print}, Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
  Get[FileNameJoin[{root, "tests", "unscrambling.wl"}]]];
checks = 0; failures = {};
check[name_, condition_] := (checks++; If[!TrueQ[condition], AppendTo[failures, name]; Print["FAIL: ", name]]);
Begin["Unscrambling`Private`"];
runtimeDirectory = FileNameJoin[{Global`root, "tests", "training"}];
Get[FileNameJoin[{runtimeDirectory, "compact-data.wl"}]];
Get[FileNameJoin[{runtimeDirectory, "compact-fit.wl"}]];
runtimeRunner = Quiet[Check[ReadList[FileNameJoin[{runtimeDirectory, "compact-run.wl"}], Hold[Expression]], $Failed]];
Global`check["checked-in compact runner parses", ListQ[runtimeRunner] && runtimeRunner =!= {}];
runtimeName[expression_] := Replace[expression,
  Hold[SetDelayed[(head_)[___], _]] :> ToString[head, InputForm]];
runtimeNames = {"particleUpdateRow", "particleCollisions", "particleMerge", "particleFullBoardAssessment",
  "particleFullBoardAuditCases", "particleRouteDiagnosticRows", "particleDiagnosticRoundRobin", "particleCuratedRowQ"};
runtimeNames = Join[runtimeNames, {"particleRecoverySourceJobs", "particleAuditProofs"}];
runtimeDefinitions = Cases[runtimeRunner,
  Verbatim[SetDelayed][lhs_, rhs_] :> Hold[SetDelayed[lhs, rhs]], Infinity];
runtimeSelected = Select[runtimeDefinitions, MemberQ[runtimeNames, runtimeName[#]] &];
Global`check["exact runtime helpers isolated", Length[runtimeSelected] === Length[runtimeNames]];
ReleaseHold /@ runtimeSelected;
Global`check["training entry point remains unevaluated", !ValueQ[Global`particleRun] && !ValueQ[particleSettings] && Kernels[] === {}];
runtimeSettings = compactScaleConfig[<|"TrainingCandidateBudget" -> 5, "Seed" -> 71|>];
Global`check["model guidance opt-in", runtimeSettings["ModelGuidanceEnabled"] === False];
Global`check["disabled guidance needs no warm model", compactScaleLoadGuidanceWeights[runtimeSettings] ===
  <|"Passed" -> True, "Enabled" -> False|> && $compactGuidanceWeights === None];
Global`check["enabled guidance rejects missing warm paths", compactScaleLoadGuidanceWeights[
  Join[runtimeSettings, <|"ModelGuidanceEnabled" -> True|>]] === $Failed];
runtimeStart = runtimeA; runtimeTarget = runtimeB; runtimeOffPath = runtimeC; runtimeEndpoint = runtimeD;
runtimeBoard = Join[{
  <|"Name" -> "Mass", "Subtract" -> runtimeStart, "Insert" -> runtimeTarget|>,
  <|"Name" -> "Schouten", "Subtract" -> 2 runtimeStart, "Insert" -> runtimeStart + runtimeTarget|>},
  Table[<|"Name" -> "OnShell", "Subtract" -> runtimeStart, "Insert" -> runtimeAlternative[index]|>, {index, 18}], {stopMove[]}];
runtimeBoardFor[expression_] := Which[
  expression === runtimeStart, runtimeBoard,
  expression === runtimeOffPath, {<|"Name" -> "Mass", "Subtract" -> runtimeOffPath, "Insert" -> runtimeTarget|>, stopMove[]},
  expression === runtimeTarget, {<|"Name" -> "Mass", "Subtract" -> runtimeTarget, "Insert" -> runtimeEndpoint|>, stopMove[]},
  expression === runtimeEndpoint, {stopMove[]}, True, {stopMove[]}];
runtimeFeatures[expression_, rules_, candidates_] := compactScalePacked[Map[Function[candidate,
  {ReplacePart[ConstantArray[0., 166], {1 -> N[Mod[Hash[{candidate["Name"], candidate["Insert"]}], 10^6]], 147 -> 1.}]}], candidates]];
runtimeScore[candidates_] := Map[Function[candidate, Which[
  candidate === runtimeHardCandidate, 20., MemberQ[Take[runtimeBoard, 2], candidate], 5., True, 0.]], candidates];
Block[{legalMoves = Function[{expression, rules}, runtimeBoardFor[expression]],
    compactActionFeatures = Function[{expression, rules, candidates}, runtimeFeatures[expression, rules, candidates]],
    trainingCompactCandidateScores = Function[{weights, expression, rules, candidates}, runtimeScore[candidates]], Print},
  runtimePlain = compactScaleRow[runtimeStart, {}, runtimeTarget, runtimeSettings];
  runtimeFullKeys = compactScaleActionKey[#, <||>] & /@ runtimeBoard;
  runtimeMissing = First[Select[Range[3, Length[runtimeBoard] - 1],
    !MemberQ[runtimePlain["CandidateKeys"], runtimeFullKeys[[#]]] &]];
  runtimeHardCandidate = runtimeBoard[[runtimeMissing]]; runtimeHardKey = runtimeFullKeys[[runtimeMissing]];
  $compactGuidanceWeights = <||>;
  runtimeGuidedSettings = Join[runtimeSettings, <|"ModelGuidanceEnabled" -> True, "GuidanceState" -> True|>];
  runtimeGuided = compactScaleRow[runtimeStart, {}, runtimeTarget, runtimeGuidedSettings];
  Global`check["full-board mining finds omitted high scorer", MemberQ[runtimeGuided["CandidateKeys"], runtimeHardKey] &&
    !MemberQ[runtimePlain["CandidateKeys"], runtimeHardKey]];
  Global`check["full-board audit uses every legal action", runtimeGuided["WarmFullBoardAudit"]["Scope"] === "FullLegalBoard" &&
    runtimeGuided["WarmFullBoardAudit"]["FullActionCount"] === Length[runtimeBoard] &&
    runtimeGuided["WarmFullBoardAudit"]["TeacherRank"] === 2];
  Global`check["hard negatives respect remaining pool capacity", runtimeGuided["ActionCount"] === 5 &&
    Length[runtimeGuided["HardNegativeKeys"]] === 2 && !TrueQ[runtimeGuided["CandidateBudgetExpanded"]]];
  Global`check["teacher equivalents and Stop survive mining", ContainsAll[runtimeGuided["CandidateKeys"], runtimeFullKeys[[{1, 2, -1}]]] &&
    Last[runtimeGuided["CandidateOperations"]] === "Stop" && Total[runtimeGuided["Target"]] === 1.];
  runtimeNoSpace = compactScaleRow[runtimeStart, {}, runtimeTarget,
    Join[runtimeGuidedSettings, <|"TrainingCandidateBudget" -> 2|>]];
  Global`check["only teacher and Stop overflow expands budget", runtimeNoSpace["ActionCount"] === 3 &&
    runtimeNoSpace["HardNegativeKeys"] === {} && TrueQ[runtimeNoSpace["CandidateBudgetExpanded"]]];
  runtimeObservation = Join[runtimeGuided, <|"Provenance" -> <|"Kind" -> "Training", "Curated" -> True, "ReferenceName" -> "Runtime route",
    "Sequence" -> 0, "Step" -> 1, "StopExample" -> False|>|>];
  runtimeMerged = First[particleMerge[{runtimeObservation}, runtimeGuidedSettings]["Rows"]];
  runtimeStopWithProof = Join[compactScaleRow[runtimeStart, {}, runtimeStart, runtimeGuidedSettings],
    <|"Provenance" -> <|"Kind" -> "Training", "Curated" -> False, "StopExample" -> True|>|>];
  runtimePlainCurated = Join[runtimePlain, <|"Provenance" -> <|"Kind" -> "Training", "Curated" -> True,
    "ReferenceName" -> "Runtime route", "Sequence" -> 0, "Step" -> 1, "StopExample" -> False|>|>];
  runtimeCuratedPriority = particleMerge[{runtimeStopWithProof, runtimePlainCurated}, runtimeGuidedSettings];
  runtimeNoProofMove = First[runtimeCuratedPriority["Rows"]];
  Global`check["curated priority discards stale Stop proof and warm audit", runtimeCuratedPriority["CuratedPriorityStates"] === 1 &&
    !KeyExistsQ[runtimeNoProofMove, "AuditProof"] && !KeyExistsQ[runtimeNoProofMove, "WarmFullBoardAudit"] &&
    runtimeNoProofMove["AuditProofs"] === {} && runtimeNoProofMove["WarmFullBoardAudits"] === {} &&
    Last[runtimeNoProofMove["Target"]] === 0.];
  runtimeNoProofCases = particleFullBoardAuditCases[{runtimeNoProofMove}, 48];
  Global`check["retained row without proof is never selected for auditing", runtimeNoProofCases["Cases"] === {} &&
    runtimeNoProofCases["Selection"]["TotalAvailableStates"] === 0];
  runtimeLegacyGhost = Join[runtimeNoProofMove, <|"AuditProof" -> runtimeStopWithProof["AuditProof"]|>];
  Global`check["explicit empty proof list overrides stale scalar proof", particleAuditProofs[runtimeLegacyGhost] === {} &&
    particleFullBoardAuditCases[{runtimeLegacyGhost}, 48]["Cases"] === {}];
  runtimeReverseWithProof = Join[runtimeGuided, <|"Provenance" -> <|"Kind" -> "Training", "Curated" -> False,
    "StopExample" -> False|>|>];
  runtimeStopWithoutProof = KeyDrop[runtimeStopWithProof, {"AuditProof", "WarmFullBoardAudit"}];
  runtimeStopPriority = particleMerge[{runtimeReverseWithProof, runtimeStopWithoutProof}, runtimeGuidedSettings];
  runtimeNoProofStop = First[runtimeStopPriority["Rows"]];
  Global`check["Stop priority discards stale reverse proof and teacher labels", runtimeStopPriority["StopPriorityStates"] === 1 &&
    !KeyExistsQ[runtimeNoProofStop, "AuditProof"] && runtimeNoProofStop["AuditProofs"] === {} &&
    runtimeNoProofStop["WarmFullBoardAudits"] === {} && Last[runtimeNoProofStop["Target"]] === 1. &&
    particleFullBoardAuditCases[{runtimeNoProofStop}, 48]["Cases"] === {}];
  runtimeView = BlockRandom[SeedRandom[19]; particleUpdateRow[runtimeMerged, 4]];
  runtimeSelectedSlots = Lookup[AssociationThread[runtimeMerged["CandidateKeys"], Range[runtimeMerged["ActionCount"]]], runtimeView["CandidateKeys"]];
  Global`check["updates preserve hardest alternative", MemberQ[runtimeView["CandidateKeys"], runtimeHardKey]];
  Global`check["updates align keys operations tensors target and mask", runtimeView["Candidates"] === runtimeMerged["Candidates"][[runtimeSelectedSlots]] &&
    runtimeView["CandidateOperations"] === runtimeMerged["CandidateOperations"][[runtimeSelectedSlots]] &&
    runtimeView["Target"] === runtimeMerged["Target"][[runtimeSelectedSlots]] &&
    runtimeView["LegalMask"] === runtimeMerged["LegalMask"][[runtimeSelectedSlots]] && runtimeView["ActionCount"] === 4];
  Global`check["updates refresh selected signature", runtimeView["CandidateSignature"] =!= runtimeMerged["CandidateSignature"] &&
    runtimeView["CandidateSignature"] === IntegerString[Hash[{"CompactSelectedActionKeys", runtimeView["CandidateKeys"]}, "SHA256"], 16, 64]];
  runtimeAuditBefore = particleFullBoardAssessment[<||>, {runtimeMerged}, 5];
  runtimeAuditAfter = particleFullBoardAssessment[<||>, {runtimeMerged}, 5];
  Global`check["before and after audit same complete board", TrueQ[runtimeAuditBefore["Passed"]] && TrueQ[runtimeAuditAfter["Passed"]] &&
    runtimeAuditBefore["Records"] === runtimeAuditAfter["Records"] &&
    runtimeAuditBefore["Selection"] === runtimeAuditAfter["Selection"] &&
    First[runtimeAuditBefore["Records"]]["FullActionCount"] === Length[runtimeBoard]];
  runtimeManyProofRows = Table[Join[runtimeMerged, <|"StateKey" -> "state" <> ToString[index],
    "Provenance" -> {Join[First[runtimeMerged["Provenance"]], <|"ReferenceName" -> If[OddQ[index], "route1", "route2"],
      "Sequence" -> Mod[index, 3], "RoutePhaseIndex" -> Mod[index, 2]|>]}|>], {index, 10}];
  runtimeCasePlan = particleFullBoardAuditCases[runtimeManyProofRows, 4];
  Global`check["representative full-board audit bounds repeated states", runtimeCasePlan["Selection"]["TotalAvailableStates"] === 10 &&
    runtimeCasePlan["Selection"]["SelectedStates"] === 4 && Length[runtimeCasePlan["Cases"]] === 4 &&
    Length[DeleteDuplicates[Lookup[First[#["Provenance"]], "ReferenceName"] & /@ Lookup[runtimeCasePlan["Cases"], "Row"]]] === 2];
  runtimeHeldProof = Join[runtimeMerged, <|"Provenance" -> {Join[First[runtimeMerged["Provenance"]], <|"Kind" -> "Heldout"|>]}|>];
  runtimeHeldAudit = particleFullBoardAssessment[<||>, {runtimeHeldProof}, 5];
  Global`check["guidance audit rejects heldout provenance", !TrueQ[runtimeHeldAudit["Passed"]] &&
    First[runtimeHeldAudit["Records"]]["Reason"] === "NonTrainingAuditProof"];
  Block[{legalMoves = Function[{expression, rules}, Delete[runtimeBoardFor[expression], 3]]},
    runtimeMismatch = particleFullBoardAssessment[<||>, {runtimeMerged}, 5];
    Global`check["full-board audit rejects changed legal board", !TrueQ[runtimeMismatch["Passed"]] &&
      First[runtimeMismatch["Records"]]["Reason"] === "AuditProofBoardMismatch"]];
  runtimeSparse = Join[runtimeMerged, <|"Candidates" -> SparseArray[runtimeMerged["Candidates"]]|>];
  runtimeSparseView = BlockRandom[SeedRandom[19]; particleUpdateRow[runtimeSparse, 4]];
  Global`check["hard update equivalent for sparse storage", Head[runtimeSparseView["Candidates"]] === SparseArray &&
    compactScaleDenseCandidates[runtimeSparseView["Candidates"]] === runtimeView["Candidates"] &&
    KeyDrop[runtimeSparseView, "Candidates"] === KeyDrop[runtimeView, "Candidates"]];
  runtimeRecoveryJob = <|"Order" -> 1, "Name" -> "Runtime verified recovery", "ReferenceName" -> "Runtime route",
    "Kind" -> "Training", "Mode" -> "SimplificationRoute", "Curated" -> True, "ModelRecovery" -> True,
    "RecoverySourceStep" -> 1, "EndpointVerified" -> True,
    "AmplitudeIndex" -> -1, "Sequence" -> 0, "Seed" -> 0, "Steps" -> 2, "ExternalLegCount" -> 4,
    "Pair" -> {{}, runtimeOffPath}, "States" -> {runtimeOffPath, runtimeTarget, runtimeEndpoint},
    "RouteIdentities" -> {"Mass", "Mass"}, "LegPermutation" -> <||>|>;
  runtimeRecovery = compactScaleRouteGenerationJob[runtimeRecoveryJob, runtimeSettings];
  Global`check["verified recovery reaches actual training rows", TrueQ[runtimeRecovery["Completed"]] && Length[runtimeRecovery["Rows"]] === 3 &&
    AllTrue[runtimeRecovery["Rows"], TrueQ[#["Provenance"]["ModelRecovery"]] &] &&
    AllTrue[runtimeRecovery["Rows"], #["Provenance"]["RecoverySourceStep"] === 1 && TrueQ[#["Provenance"]["EndpointVerified"]] &] &&
    Lookup[Lookup[runtimeRecovery["Rows"], "Provenance"], "StopExample"] === {False, False, True}];
  runtimeIllegal = Quiet[compactScaleRouteGenerationJob[Join[runtimeRecoveryJob,
    <|"States" -> {runtimeOffPath, runtimeTarget, runtimeUnreachable}|>], runtimeSettings]];
  Global`check["illegal recovery suffix yields no partial rows", !TrueQ[runtimeIllegal["Completed"]] && runtimeIllegal["Rows"] === {}];
];
runtimeSelection = compactScaleSelection[Table[ToString[index], {index, 20}], {1, 2}, 20, "runtime", 5, 7, Range[3, 18]];
Global`check["hard mandatory list cannot expand pool budget", Length[runtimeSelection["Indices"]] === 5 &&
  runtimeSelection["Sampling"]["HardNegativeCount"] === 2 && !TrueQ[runtimeSelection["Sampling"]["CandidateBudgetExpanded"]]];
runtimeStepsJob = <|"States" -> Range[10], "RouteIdentities" -> {"Mass", "Mass", "Mass", "Schouten", "Schouten", "OnShell", "OnShell", "Mass", "Mass"}|>;
Global`check["guidance states cover phases deterministically", compactScaleGuidanceSteps[runtimeStepsJob,
  Join[runtimeSettings, <|"ModelGuidanceEnabled" -> True|>]] === {1, 4, 6, 8}];
Global`check["disabled guidance selects no route states", compactScaleGuidanceSteps[runtimeStepsJob, runtimeSettings] === {}];
runtimeLongPhaseJob = <|"States" -> Range[28], "RouteIdentities" -> Flatten[ConstantArray["phase" <> ToString[#], 3] & /@ Range[9]]|>;
runtimeSpanningSettings = Join[runtimeSettings, <|"ModelGuidanceEnabled" -> True|>];
Global`check["guidance sampling reaches late route phases", compactScaleGuidanceSteps[runtimeLongPhaseJob, runtimeSpanningSettings] === {1, 10, 16, 25}];
Global`check["recovery anchors span opening and suffix", compactScaleRecoveryAnchorSteps[runtimeLongPhaseJob, runtimeSpanningSettings] === {1, 25}];
runtimeRecoverySources = Flatten[Table[<|"Kind" -> "Training", "Mode" -> "SimplificationRoute", "Curated" -> True,
  "ReferenceName" -> "route" <> ToString[route], "Sequence" -> permutation|>, {route, 2}, {permutation, 4}], 1];
runtimeRecoveryPlan = particleRecoverySourceJobs[Append[runtimeRecoverySources,
  Join[First[runtimeRecoverySources], <|"Kind" -> "Heldout"|>]], 3];
Global`check["recovery budget preserves complete source dataset", runtimeRecoveryPlan["AvailableJobs"] === 8 &&
  runtimeRecoveryPlan["SelectedJobs"] === 3 && Length[runtimeRecoverySources] === 8];
Global`check["recovery balances routes and excludes heldout jobs", Lookup[runtimeRecoveryPlan["Jobs"], "ReferenceName"] ===
  {"route1", "route2", "route1"} && AllTrue[runtimeRecoveryPlan["Jobs"], #["Kind"] === "Training" &]];
End[];
Print["COMPACT RUNTIME TESTS: ", checks, " checks; failures: ", failures];
Quit[If[failures === {}, 0, 1]];
