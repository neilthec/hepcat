(* Loaded in Unscrambling`Private`. One shared scorer evaluates every legal edit.
   UTF-8 serialization preserves the complete ordered expression tree. *)
$policyVersion = 3;
$modelNet = None;
$loadedModelDirectory = None;
$onShellChannels = Automatic;
$useMomentumConservation = True;
$maxStagnantSteps = 3;
$timeLimit = 60;
$trainingBatchSize = 1;
$trainingTargetDevice = "CPU";

normalizeOnShellChannels[Automatic] := Automatic;
normalizeOnShellChannels[channels_List] := Sort /@ channels;

Options[UnscrambleCandidates] = {"OnShellChannels" -> Automatic, "MomentumConservation" -> True};
Options[ComplicateAmplitude] = Join[{RandomSeed -> Automatic, "RecordSteps" -> False}, Options[UnscrambleCandidates]];
Options[UnscrambleSpinorAmplitudes] = Join[
  {"MaxSteps" -> 12, "Attempts" -> 5, "MaxStagnantSteps" -> 3,
    "TimeLimit" -> 60, "ModelDirectory" -> Automatic}, Options[UnscrambleCandidates]];
Options[UnscrambleTrace] = Options[UnscrambleSpinorAmplitudes];

stopMove[] := <|"Name" -> "Stop", "Subtract" -> 0, "Insert" -> 0|>;

(* Relate both endpoints to their own momenta without changing the other
   endpoint's massive/massless kind or little-group indices. *)
flipSpinor[s_Spinor] := s /. {"Angle" -> "Square", "Square" -> "Angle"};
chainMoves[expr_, massRules_] := Module[{moves = {}, mons, chains, c, xs, ps, m, rhs, others, t, k, side, j},
  mons = monomialsOf[expr];
  Do[
    chains = Select[chainsIn[mons[[t]]], validChainQ];
    Do[
      c = chains[[k]]; xs = List @@ c; ps = Drop[Rest[xs], -1];
      Do[
        m = massValue[massRules, spinorLeg[xs[[side]]]];
        If[!MissingQ[m] && m =!= 0,
          rhs = If[side === 1,
            (SpinorChain @@ Join[{flipSpinor[First[xs]], Mom[spinorLeg[First[xs]]]}, ps, {Last[xs]}])/m,
            -(SpinorChain @@ Join[{First[xs]}, ps, {Mom[spinorLeg[Last[xs]]], flipSpinor[Last[xs]]}])/m];
          AppendTo[moves, <|"Name" -> "Mass", "Subtract" -> mons[[t]],
            "Insert" -> Expand[mons[[t]]/c*rhs], "Site" -> {t, k, side}, "Direction" -> "Expand"|>]
        ];
        If[ps === {}, Continue[]];
        If[If[side === 1, First[ps], Last[ps]] === Mom[spinorLeg[xs[[side]]]],
          m = massValue[massRules, spinorLeg[xs[[side]]]];
          If[!MissingQ[m],
            rhs = If[side === 1,
              m*(SpinorChain @@ Join[{flipSpinor[First[xs]]}, Rest[ps], {Last[xs]}]),
              -m*(SpinorChain @@ Join[{First[xs]}, Most[ps], {flipSpinor[Last[xs]]}])];
            AppendTo[moves, <|"Name" -> "Mass", "Subtract" -> mons[[t]],
              "Insert" -> Expand[mons[[t]]/c*rhs], "Site" -> {t, k, side}, "Direction" -> "Contract"|>]
          ]
        ], {side, {1, -1}}
      ];
      If[TrueQ[$useMomentumConservation],
        Do[
          If[!MemberQ[externalIndices[massRules], ps[[j, 1]]], Continue[]];
          others = DeleteCases[externalIndices[massRules], ps[[j, 1]]];
          If[others === {}, Continue[]];
          rhs = -Total[ReplacePart[c, j + 1 -> Mom[#]] & /@ others];
          AppendTo[moves, <|"Name" -> "MomentumConservation", "Subtract" -> mons[[t]],
            "Insert" -> Expand[mons[[t]]/c*rhs], "Site" -> {t, k, j}|>],
          {j, Length[ps]}
        ]
      ], {k, Length[chains]}
    ], {t, Length[mons]}
  ];
  moves
];

applyCandidate[expr_, cand_Association] :=
  If[cand["Name"] === "Stop", expr, canonicalChains[expr - cand["Subtract"] + cand["Insert"]]];

legalMoves[expr_, massRules_List] := Module[{legacy, moves},
  legacy = Select[kinematicMoves[expr, massRules],
    MemberQ[If[TrueQ[$useMomentumConservation],
      {"MomentumConservation", "MomentumSquare", "OnShell"},
      {"MomentumSquare", "OnShell"}], #["Name"]] &];
  moves = Join[chainMoves[expr, massRules], SchoutenCandidates[expr], legacy];
  moves = Select[moves, !samePoly[applyCandidate[expr, #], expr] &];
  Append[DeleteDuplicatesBy[moves, {#["Name"], #["Subtract"], #["Insert"]} &], stopMove[]]
];

UnscrambleCandidates[{massRules_List, amplitude_}, OptionsPattern[]] :=
  Block[{$onShellChannels = normalizeOnShellChannels[OptionValue["OnShellChannels"]],
      $useMomentumConservation = OptionValue["MomentumConservation"]},
    legalMoves[canonicalChains[amplitude /. massRules], massRules]
  ];

(* Labels are renumbered only where they denote particles, never in coefficients,
   exponents, spin indices, or split positions. Sparse and high labels are legal. *)
relabelLegs[expr_, ids_Association] := Module[{idx},
  idx[i_Integer] := Lookup[ids, i, i];
  idx[Multiparticle[ii__]] := Multiparticle @@ (idx /@ {ii});
  idx[x_] := x;
  expr /. {
    Spinor[k_, t: ("Angle" | "Square"), i_, rest___] :> Spinor[k, t, idx[i], rest],
    Spinor[k_, ud_, t: ("Angle" | "Square"), i_, rest___] :> Spinor[k, ud, t, idx[i], rest],
    Mom[i_] :> Mom[idx[i]], Mass[i_] :> Mass[idx[i]],
    MomProd[i_, j_] :> MomProd[idx[i], idx[j]],
    Mandelstahm[i_, j_] :> Mandelstahm[idx[i], idx[j]]
  }
];

stateLegIDs[expr_, rules_] := Module[{legs},
  legs = Flatten[Join[
    Cases[{expr, rules}, s_Spinor :> spinorLeg[s], Infinity],
    Cases[{expr, rules}, (Mom | Mass)[i_] :> i, Infinity],
    Cases[{expr, rules}, (MomProd | Mandelstahm)[i_, j_] :> {i, j}, Infinity]] /.
      Multiparticle[ii__] :> {ii}];
  legs = Union[Select[legs, IntegerQ]];
  AssociationThread[legs, Range[Length[legs]]]
];

encodePacket[packet_] := 1 + ToCharacterCode[ToString[FullForm[packet], PageWidth -> Infinity], "UTF8"];
stateFeature[expr_, rules_, ids_] := encodePacket[{
  "HEPCATStateV3", {"State", relabelLegs[expr, ids]}, {"Masses", relabelLegs[rules, ids]},
  {"Conditions", $useMomentumConservation,
    Map[Lookup[ids, #, #] &, Sort /@ (First /@ internalPairs[rules]), {2}]}}];
editFeature[cand_, ids_] := encodePacket[{
  "HEPCATEditV3", cand["Name"], relabelLegs[cand["Subtract"], ids],
  relabelLegs[cand["Insert"], ids], Lookup[cand, "Site", {}], Lookup[cand, "Splits", {}],
  relabelLegs[Lookup[cand, "Chains", {}], ids], Lookup[cand, "Direction", ""]}];

candidateFeature[expr_, rules_, cand_] := Module[{ids = stateLegIDs[expr, rules]},
  <|"State" -> stateFeature[expr, rules, ids], "Candidate" -> editFeature[cand, ids]|>
];

(* Left padding keeps the final recurrent position on actual edit content.
   257 is reserved for padding; no expression or edit bytes are dropped. *)
groupFeatures[expr_, rules_, cands_] := Module[{ids = stateLegIDs[expr, rules], edits},
  edits = editFeature[#, ids] & /@ cands;
  <|"State" -> stateFeature[expr, rules, ids],
    "Candidates" -> (PadLeft[#, Max[Length /@ edits], 257] & /@ edits)|>
];

UnscrambleEncoding[pair:{rules_List, amp_}, cand_Association, opts:OptionsPattern[UnscrambleCandidates]] :=
  Block[{$onShellChannels = normalizeOnShellChannels[OptionValue["OnShellChannels"]],
      $useMomentumConservation = OptionValue["MomentumConservation"]},
    candidateFeature[canonicalChains[amp /. rules], rules, cand]
  ];

makeEncoder[classes_, width_:"Varying"] := NetChain[{
  EmbeddingLayer[24, classes], GatedRecurrentLayer[64], SequenceLastLayer[]
}, "Input" -> {width}];

makePolicy[width_:"Varying"] := Module[{head},
  head = NetGraph[<|"Join" -> CatenateLayer[],
    "Score" -> NetChain[{LinearLayer[32], Ramp, LinearLayer[1], LogisticSigmoid}]|>,
    {{NetPort["State"], NetPort["Candidate"]} -> "Join" -> "Score"},
    "State" -> 64, "Candidate" -> 64];
  NetGraph[<|"StateEncoder" -> makeEncoder[256],
    "CandidateEncoder" -> NetMapOperator[makeEncoder[257, width]],
    "Scorer" -> NetMapThreadOperator[head, <|"Candidate" -> 1|>]|>,
    {NetPort["State"] -> "StateEncoder" -> NetPort["Scorer", "State"],
      NetPort["Candidates"] -> "CandidateEncoder" -> NetPort["Scorer", "Candidate"]}]
];

modelPaths[dir_] := <|
  "Net" -> FileNameJoin[{dir, "unscramble-shared-v3.wlnet"}],
  "Metadata" -> FileNameJoin[{dir, "unscramble-shared-v3.m"}]
|>;
resolveModelDirectory[Automatic] := $modelDirectory;
resolveModelDirectory[dir_String] := ExpandFileName[dir];

loadUnscrambleModel[dir_] := Module[{paths, meta, net},
  If[$loadedModelDirectory === dir && MatchQ[$modelNet, _NetChain | _NetGraph], Return[True]];
  paths = modelPaths[dir];
  If[!AllTrue[Values[paths], FileExistsQ], Return[False]];
  meta = Get[paths["Metadata"]];
  If[!AssociationQ[meta] || Lookup[meta, "Version", None] =!= $policyVersion ||
      Lookup[meta, "Encoding", None] =!= "SplitFullFormUTF8" ||
      Lookup[meta, "Architecture", None] =!= "SharedStateGRU", Return[False]];
  net = Import[paths["Net"]];
  If[!MatchQ[net, _NetChain | _NetGraph], Return[False]];
  $modelNet = net; $loadedModelDirectory = dir;
  True
];

scoreCandidates[expr_, rules_, cands_] := Module[{scores},
  scores = Flatten[$modelNet[groupFeatures[expr, rules, cands], TargetDevice -> "CPU"]];
  If[Length[scores] =!= Length[cands] || !VectorQ[scores, NumericQ], $Failed, scores]
];

selectCandidate[expr_, rules_, sample_] := Module[{cands, scores, probs, idx},
  cands = legalMoves[expr, rules];
  scores = scoreCandidates[expr, rules, cands];
  If[scores === $Failed, Return[$Failed]];
  probs = Exp[(scores - Max[scores])/0.2];
  idx = If[TrueQ[sample], RandomChoice[probs -> Range[Length[cands]]], First[Ordering[scores, -1]]];
  <|"Candidate" -> cands[[idx]], "Index" -> idx, "Count" -> Length[cands], "Score" -> scores[[idx]]|>
];

SpinorExpressionComplexity[expr_] := expressionComplexity[expr];
expressionComplexity[expr_] := Module[{chains},
  chains = Flatten[chainsIn /@ monomialsOf[expr]];
  Length[monomialsOf[expr]] + 4*Total[Max[Length[#] - 2, 0] & /@ chains] +
    4*Count[{expr}, _MomProd, Infinity] +
    2*Count[{expr}, Power[Mom[Multiparticle[__]], 2], Infinity]
];

(* Scrambling and inference share exactly the same candidate vocabulary. *)
scramblePair[{rules_List, amp_}, n_, seed_, record_] := Module[
  {original = canonicalChains[amp /. rules], expr, moves, pick, next, history = {}},
  expr = original;
  BlockRandom[
    If[IntegerQ[seed], SeedRandom[seed]];
    Do[
      moves = Select[legalMoves[expr, rules], #["Name"] =!= "Stop" &];
      If[moves === {}, Break[]];
      pick = RandomChoice[moves]; next = applyCandidate[expr, pick];
      AppendTo[history, <|"Before" -> expr, "After" -> next, "Candidate" -> pick|>];
      expr = next, {n}
    ]
  ];
  Join[<|"MassRules" -> rules, "Original" -> original, "Expression" -> expr, "Steps" -> Length[history]|>,
    If[TrueQ[record], <|"Trajectory" -> history|>, <||>]]
];

ComplicateAmplitude[pair:{_List, _}, nSteps_Integer:5, OptionsPattern[]] :=
  Block[{$onShellChannels = normalizeOnShellChannels[OptionValue["OnShellChannels"]],
      $useMomentumConservation = OptionValue["MomentumConservation"]},
    scramblePair[pair, Max[0, nSteps], OptionValue[RandomSeed], OptionValue["RecordSteps"]]
  ];

(* A reverse scramble is a label only when the inference candidate set can
   actually perform it. Skip unreachable targets rather than invent actions. *)
trainingRows[expr_, rules_, target_] := Module[{cands, labels, canonicalTarget = canonicalChains[target]},
  cands = legalMoves[expr, rules];
  labels = Boole[samePoly[applyCandidate[expr, #], canonicalTarget]] & /@ cands;
  If[!MemberQ[labels, 1], Return[{}]];
  {Join[groupFeatures[expr, rules, cands], <|"Target" -> List /@ N[labels],
    "Weights" -> List /@ balanceWeights[labels]|>]}
];

balanceWeights[labels_] := Module[{positive = Count[labels, 1], negative = Count[labels, 0]},
  If[positive === 0 || negative === 0, ConstantArray[1., Length[labels]],
    N[Sqrt[Length[labels]/(2*If[# === 1, positive, negative])] & /@ labels]]
];

Options[TrainUnscrambleNet] = Join[
  {Steps -> 8, Scrambles -> 20, HoldOut -> 2, MaxTrainingRounds -> 6,
    EpisodeLength -> 12, Kernels -> 1, TargetDevice -> "CPU", "ModelDirectory" -> Automatic,
    "DataCacheDirectory" -> None, "WorkerRoot" -> Automatic,
    "WorkerThreads" -> 1, "TrainingBatchSize" -> 1,
    "JobTimeLimit" -> 300, "ValidationTimeLimit" -> 60}, Options[UnscrambleCandidates]];
TrainUnscrambleNet::kernels = "Kernels must be a positive integer, Automatic, or a nonempty list of connected kernel objects. Worker startup or initialization failed.";
TrainUnscrambleNet::nodata = "No reversible training examples were generated. No model was saved.";
TrainUnscrambleNet::options = "Steps, HoldOut must be nonnegative integers; Scrambles, MaxTrainingRounds, EpisodeLength must be positive integers.";
TrainUnscrambleNet::save = "Could not save the candidate model in `1`.";

makeTrainingPolicy[width_:"Varying"] := NetGraph[<|"Policy" -> makePolicy[width],
  "WeightedPrediction" -> ThreadingLayer[Times], "WeightedTarget" -> ThreadingLayer[Times],
  "Loss" -> MeanSquaredLossLayer[]|>, {
  NetPort["State"] -> NetPort["Policy", "State"],
  NetPort["Candidates"] -> NetPort["Policy", "Candidates"],
  {"Policy", NetPort["Weights"]} -> "WeightedPrediction" -> NetPort["Loss", "Input"],
  {NetPort["Target"], NetPort["Weights"]} -> "WeightedTarget" -> NetPort["Loss", "Target"]}];

prepareTrainingBatch[rows_] := Module[{width, padded, net},
  width = Max[Length /@ Flatten[Lookup[rows, "Candidates"], 1]];
  padded = (Join[#, <|"Candidates" -> (PadLeft[#, width, 257] & /@ #["Candidates"])|>] &) /@ rows;
  net = makeTrainingPolicy[width];
  <|"Net" -> net, "Rows" -> padded, "Width" -> width|>
];

variableWidthPolicy[net_] := Module[{paths = Information[net, "ArraysPositionList"]},
  NetReplacePart[makePolicy[], Map[# -> NetExtract[net, #] &, paths]]
];

trainCandidateNetwork[rows_, rounds_] := Module[{trained, batch = prepareTrainingBatch[rows]},
  (* NetTrain permits only the first input dimension to vary. Fix the byte
     width to this dataset's longest edit, padding only, never truncating. *)
  trained = NetTrain[NetInitialize[batch["Net"], RandomSeeding -> 1], batch["Rows"],
    MaxTrainingRounds -> rounds, BatchSize -> $trainingBatchSize, TargetDevice -> $trainingTargetDevice,
    TrainingProgressReporting -> None, Method -> {"ADAM", "LearningRate" -> 0.001}];
  If[MatchQ[trained, _NetGraph],
    variableWidthPolicy[NetExtract[trained, "Policy"]], $Failed]
];

Get[FileNameJoin[{$modelDirectory, "training-pipeline.wl"}]];

UnscrambleSpinorAmplitudes::nomodel = "No version-3 shared-state model found in `1`. Retrain with TrainUnscrambleNet; older models are incompatible and are not overwritten.";
UnscrambleSpinorAmplitudes::scores = "The model did not return one numeric score per candidate.";
UnscrambleSpinorAmplitudes::options = "MaxSteps must be a nonnegative integer; Attempts and MaxStagnantSteps must be positive integers; TimeLimit must be a positive number of seconds.";

runUnscramble[{rules_List, amplitude_}, record_, maxSteps_, attempts_, dir_] := Module[
  {expr = canonicalChains[amplitude /. rules], checkpoint, trial, before, selection, move, next,
    history = {}, checkpoints = {}, accepted, failed = False, attempt,
    stagnant = 0, reason = "StepLimit", loaded = False, bestScore, nextScore},
  checkpoint = expr; bestScore = expressionComplexity[checkpoint];
  TimeConstrained[
  loaded = loadUnscrambleModel[dir];
  If[!loaded, Message[UnscrambleSpinorAmplitudes::nomodel, dir];
    Return[<|"Result" -> {rules, amplitude}, "Trace" -> {}, "Checkpoints" -> {}, "ModelLoaded" -> False|>]];
  Do[
    before = checkpoint; trial = checkpoint; reason = "StepLimit";
    Do[
      selection = selectCandidate[trial, rules, attempt > 1];
      If[selection === $Failed, Message[UnscrambleSpinorAmplitudes::scores]; failed = True; reason = "ScoringFailed"; Break[]];
      move = selection["Candidate"]; next = applyCandidate[trial, move];
      nextScore = expressionComplexity[next];
      If[nextScore < bestScore,
        checkpoint = next; bestScore = nextScore; stagnant = 0,
        stagnant++];
      If[TrueQ[record], AppendTo[history, Join[selection, <|"Attempt" -> attempt,
        "Before" -> trial, "After" -> next, "Changed" -> !samePoly[trial, next],
        "ComplexityBefore" -> expressionComplexity[trial], "ComplexityAfter" -> expressionComplexity[next]|>]]];
      trial = next;
      If[stagnant >= $maxStagnantSteps, reason = "Stagnation"; Break[]];
      If[move["Name"] === "Stop", reason = "Stop"; Break[]], {maxSteps}
    ];
    accepted = expressionComplexity[checkpoint] < expressionComplexity[before];
    If[TrueQ[record], AppendTo[checkpoints, <|"Attempt" -> attempt, "Before" -> before,
      "Trial" -> trial, "Accepted" -> accepted, "After" -> checkpoint|>]];
    If[failed || stagnant >= $maxStagnantSteps, Break[]], {attempt, attempts}
  ], $timeLimit, reason = "TimeLimit"];
  <|"Result" -> {rules, checkpoint}, "Trace" -> history, "Checkpoints" -> checkpoints,
    "ModelLoaded" -> loaded, "ScoringFailed" -> failed, "TerminationReason" -> reason,
    "StagnantSteps" -> stagnant|>
];

configuredRun[pair:{_List, _}, record_, OptionsPattern[UnscrambleTrace]] := Block[
  {$onShellChannels = normalizeOnShellChannels[OptionValue["OnShellChannels"]],
    $useMomentumConservation = OptionValue["MomentumConservation"],
    $maxStagnantSteps = OptionValue["MaxStagnantSteps"], $timeLimit = OptionValue["TimeLimit"]},
  If[!IntegerQ[OptionValue["MaxSteps"]] || OptionValue["MaxSteps"] < 0 ||
      !IntegerQ[OptionValue["Attempts"]] || OptionValue["Attempts"] < 1 ||
      !IntegerQ[$maxStagnantSteps] || $maxStagnantSteps < 1 ||
      !NumberQ[$timeLimit] || !TrueQ[$timeLimit > 0],
    Message[UnscrambleSpinorAmplitudes::options]; Return[$Failed]];
  runUnscramble[pair, record, OptionValue["MaxSteps"], OptionValue["Attempts"],
    resolveModelDirectory[OptionValue["ModelDirectory"]]]
];
UnscrambleTrace[pair:{_List, _}, opts:OptionsPattern[]] := configuredRun[pair, True, opts];
UnscrambleSpinorAmplitudes[pair:{_List, _}, opts:OptionsPattern[]] := Module[{result},
  result = configuredRun[pair, False, opts];
  If[AssociationQ[result], result["Result"], $Failed]
];
