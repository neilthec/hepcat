(* Deterministic search-control tests, without executing the neural backend. *)
root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Global`$HEPCATpath = FileNameJoin[{root, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
Get[FileNameJoin[{root, "tests", "unscrambling.wl"}]];
checks = 0; failures = {};
check[name_, value_] := (checks++; If[!TrueQ[value], AppendTo[failures, name]]);
check["progress-driven defaults", Lookup[Association[Options[UnscrambleTrace]],
  {"MaxSteps", "MaxStagnantSteps", "ProgressReporting"}] === {Infinity, 10, Automatic}];
check["both public entry points default to available parallel workers",
  Lookup[Association[Options[UnscrambleTrace]], "Kernels"] === Automatic &&
    Lookup[Association[Options[UnscrambleSpinorAmplitudes]], "Kernels"] === Automatic];
SetOptions[UnscrambleTrace, "Kernels" -> 1];
SetOptions[UnscrambleSpinorAmplitudes, "Kernels" -> 1];
Block[{Unscrambling`Private`loadUnscrambleModel, Unscrambling`Private`selectCandidate, calls = 0},
  Unscrambling`Private`loadUnscrambleModel[_] := True;
  Unscrambling`Private`selectCandidate[ex_, _, _] := (
    calls++;
    <|"Candidate" -> <|"Name" -> "Test", "Subtract" -> ex,
      "Insert" -> If[calls === 1, a + b, a + b + c + calls]|>|>
  );
  result = UnscrambleTrace[{{}, a + b + c}, "MaxSteps" -> 100, "Attempts" -> 20,
    "MaxStagnantSteps" -> 3];
  check["stagnation bounds each attempt", calls === 61 && Length[result["Checkpoints"]] === 20 &&
    result["TerminationReason"] === "Stagnation"];
  check["intermediate best retained", result["Result"] === {{}, a + b}];
  check["trace checkpoint records improvement", First[result["Checkpoints"]]["Accepted"]];
  calls = 0;
  compositeInput = SpinorChain[Spinor["Spin", "Angle", 1], Mom[Multiparticle[1, 3]],
    Spinor["Spin", "Square", 4]];
  splitInput = SpinorChain[Spinor["Spin", "Angle", 1], Mom[1], Spinor["Spin", "Square", 4]] +
    SpinorChain[Spinor["Spin", "Angle", 1], Mom[3], Spinor["Spin", "Square", 4]];
  contractedInput = m1 SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 4]] +
    SpinorChain[Spinor["Spin", "Angle", 1], Mom[3], Spinor["Spin", "Square", 4]];
  Unscrambling`Private`selectCandidate[ex_, _, _] := (
    calls++; <|"Candidate" -> <|"Name" -> "Test", "Subtract" -> ex,
      "Insert" -> If[calls === 1, splitInput, contractedInput]|>|>
  );
  result = UnscrambleTrace[{{}, compositeInput}, "MaxSteps" -> 2, "Attempts" -> 1,
    "MaxStagnantSteps" -> 2];
  check["composite split is a neutral search step",
    Lookup[First[result["Trace"]], {"ComplexityBefore", "ComplexityAfter"}] === {10, 10}];
  check["contraction after split retained before reaching old input score",
    result["Result"] === {{}, contractedInput} &&
      Last[result["Trace"]]["ComplexityAfter"] === 6 &&
      First[result["Checkpoints"]]["Accepted"]];
  check["progress counts match the retained checkpoint",
    Lookup[result["Progress"], {"Steps", "CurrentTerms", "BestTerms",
      "CurrentExpandedTerms", "BestExpandedTerms", "CurrentComplexity", "BestComplexity"}] ===
        {2, 2, 2, 2, 2, 6, 6}];
  calls = 0;
  shrinkingInput = Total[Array[term, 30]];
  Unscrambling`Private`selectCandidate[ex_, _, _] := (
    calls++; <|"Candidate" -> If[calls <= 20,
      <|"Name" -> "Test", "Subtract" -> ex, "Insert" -> Total[Array[term, 30 - calls]]|>,
      Unscrambling`Private`stopMove[]]|>
  );
  result = UnscrambleTrace[{{}, shrinkingInput}, "Attempts" -> 1, "ProgressReporting" -> False];
  check["unlimited steps continue while simplifying beyond twelve edits",
    calls === 21 && result["TerminationReason"] === "Stop" &&
      result["Result"] === {{}, Total[Array[term, 10]]} && result["Progress"]["Steps"] === 21];
  check["progress reports the best term count and elapsed time",
    result["Progress"]["BestTerms"] === 10 && result["Progress"]["BestExpandedTerms"] === 10 &&
      NumberQ[result["Progress"]["ElapsedSeconds"]] && result["Progress"]["ElapsedSeconds"] >= 0];
  calls = 0;
  result = UnscrambleTrace[{{}, shrinkingInput}, "MaxSteps" -> 4, "Attempts" -> 1];
  check["explicit finite step caps remain available",
    calls === 4 && result["TerminationReason"] === "StepLimit" && result["Progress"]["BestTerms"] === 26];
  calls = 0;
  result = UnscrambleTrace[{{}, shrinkingInput}, "MaxSteps" -> 0, "Attempts" -> 1];
  check["zero steps remain available", calls === 0 && result["Result"] === {{}, shrinkingInput} &&
    result["Progress"]["Steps"] === 0];
  calls = 0;
  Unscrambling`Private`selectCandidate[ex_, _, _] := (
    calls++; <|"Candidate" -> <|"Name" -> "Test", "Subtract" -> ex, "Insert" -> ex|>|>
  );
  result = UnscrambleTrace[{{}, a}, "Attempts" -> 1];
  check["default stagnation ends an unlimited attempt after ten steps",
    calls === 10 && result["TerminationReason"] === "Stagnation" && result["StagnantSteps"] === 10];
  splitChain[i_, p_] := SpinorChain[Spinor["Spin", "Angle", i], p, Spinor["Spin", "Square", i + 100]];
  splitStates = Table[Total[Table[If[index <= splitStep,
    splitChain[index, Mom[11]] + splitChain[index, Mom[27]],
    splitChain[index, Mom[Multiparticle[11, 27]]]], {index, 12}]], {splitStep, 0, 12}];
  afterSplits = Last[splitStates] /. splitChain[11, Mom[11]] ->
    m11 SpinorChain[Spinor["Spin", "Square", 11], Spinor["Spin", "Square", 111]];
  calls = 0;
  Unscrambling`Private`selectCandidate[ex_, _, _] := (
    calls++; <|"Candidate" -> Which[
      calls <= 12, <|"Name" -> "MomentumSum", "Identity" -> "Split",
        "Subtract" -> ex, "Insert" -> splitStates[[calls + 1]]|>,
      calls === 13, <|"Name" -> "Mass", "Subtract" -> ex, "Insert" -> afterSplits|>,
      True, Unscrambling`Private`stopMove[]]|>
  );
  result = UnscrambleTrace[{{}, First[splitStates]}, "Attempts" -> 1];
  check["twelve neutral splits do not exhaust ten-step patience", calls === 14 &&
    result["TerminationReason"] === "Stop" && result["Progress"]["NeutralSplits"] === 12 &&
    result["Result"] === {{}, afterSplits} && result["Progress"]["BestComplexity"] === 116];
  calls = 0;
  Unscrambling`Private`selectCandidate[ex_, _, _] := (
    calls++; <|"Candidate" -> If[calls === 10,
      <|"Name" -> "MomentumSum", "Identity" -> "Split", "Subtract" -> ex, "Insert" -> splitInput|>,
      <|"Name" -> "Test", "Subtract" -> ex, "Insert" -> ex|>]|>
  );
  result = UnscrambleTrace[{{}, compositeInput}, "Attempts" -> 1];
  check["neutral splits preserve accumulated stagnation", calls === 11 &&
    result["TerminationReason"] === "Stagnation" && result["StagnantSteps"] === 10 &&
    result["Progress"]["NeutralSplits"] === 1];
  calls = 0;
  Unscrambling`Private`selectCandidate[ex_, _, _] := (
    calls++; <|"Candidate" -> <|"Name" -> "MomentumSum", "Identity" -> "Split",
      "Subtract" -> ex, "Insert" -> ex|>|>
  );
  result = UnscrambleTrace[{{}, compositeInput}, "Attempts" -> 1];
  check["no-op split labels still consume patience", calls === 10 &&
    result["TerminationReason"] === "Stagnation" && result["Progress"]["NeutralSplits"] === 0];
  calls = 0;
  Unscrambling`Private`selectCandidate[ex_, _, _] := (
    calls++; <|"Candidate" -> If[OddQ[calls],
      <|"Name" -> "MomentumSum", "Identity" -> "Split", "Subtract" -> ex, "Insert" -> splitInput|>,
      <|"Name" -> "MomentumSum", "Identity" -> "Collect", "Subtract" -> ex, "Insert" -> compositeInput|>]|>
  );
  result = UnscrambleTrace[{{}, compositeInput}, "Attempts" -> 1];
  check["split-collect cycles still exhaust patience", calls === 20 &&
    result["TerminationReason"] === "Stagnation" && result["Progress"]["NeutralSplits"] === 10];
  Block[{Print, Unscrambling`Private`unscrambleProgressPanel, $Notebooks = False,
      prints = 0, panels = 0},
    Print[___] := prints++;
    Unscrambling`Private`unscrambleProgressPanel[___] := panels++;
    Do[UnscrambleTrace[{{}, a}, "Attempts" -> 1, "ProgressReporting" -> setting],
      {setting, {False, Automatic, True}}];
    check["headless progress is silent and does not render panels", prints === 0 && panels === 0];
  ];
  calls = 0;
  sampledFlags = {};
  Unscrambling`Private`selectCandidate[ex_, _, sampled_] := (
    calls++; AppendTo[sampledFlags, sampled];
    <|"Candidate" -> <|"Name" -> "Test", "Subtract" -> ex,
      "Insert" -> If[sampled, a + b, a + b + c + d]|>|>
  );
  result = UnscrambleTrace[{{}, a + b + c}, "MaxSteps" -> 10, "Attempts" -> 3,
    "MaxStagnantSteps" -> 2];
  check["sampled retry improves after stagnation", calls === 7 && result["Result"] === {{}, a + b}];
  check["patience resets for each retry", Lookup[result["Trace"], "Attempt"] === {1, 1, 2, 2, 2, 3, 3}];
  check["first attempt greedy and retries sampled", sampledFlags === {False, False, True, True, True, True, True}];
  check["retry checkpoint preserves best", Lookup[result["Checkpoints"], "Accepted"] === {False, True, False} &&
    Last[result["Checkpoints"]]["Before"] === a + b && Last[result["Checkpoints"]]["After"] === a + b];
  calls = 0;
  Unscrambling`Private`selectCandidate[ex_, _, _] := (
    calls++;
    <|"Candidate" -> <|"Name" -> "Test", "Subtract" -> ex,
      "Insert" -> Switch[calls, 1, a + b + c + d, 2, a + b, 3, a + b + c, 4, a, _, a + b]|>|>
  );
  result = UnscrambleTrace[{{}, a + b + c}, "Attempts" -> 1, "MaxStagnantSteps" -> 2];
  check["improvement resets patience", calls === 6 && result["Result"] === {{}, a}];
  calls = 0;
  Unscrambling`Private`selectCandidate[ex_, _, _] := (
    calls++; If[calls > 1, Pause[2]];
    <|"Candidate" -> <|"Name" -> "Test", "Subtract" -> ex, "Insert" -> a|>|>
  );
  result = UnscrambleTrace[{{}, a + b}, "TimeLimit" -> 0.2];
  check["timeout preserves earlier improvement", result["TerminationReason"] === "TimeLimit" && result["Result"] === {{}, a}];
  calls = 0;
  Unscrambling`Private`selectCandidate[ex_, _, _] := (
    calls++; <|"Candidate" -> Unscrambling`Private`stopMove[]|>
  );
  result = UnscrambleTrace[{{}, a}, "MaxSteps" -> 100, "Attempts" -> 20, "MaxStagnantSteps" -> 2];
  check["stop ends each attempt", calls === 20 && Length[result["Trace"]] === 20 &&
    Length[result["Checkpoints"]] === 20 && result["StagnantSteps"] === 1 &&
    result["TerminationReason"] === "Stop"];
  calls = 0;
  Unscrambling`Private`selectCandidate[_, _, _] := (calls++; $Failed);
  result = Quiet[UnscrambleTrace[{{}, a}, "Attempts" -> 20]];
  check["scoring failure cancels retries", calls === 1 && result["ScoringFailed"] &&
    result["TerminationReason"] === "ScoringFailed"];
  Unscrambling`Private`selectCandidate[ex_, _, _] := (Pause[2]; $Failed);
  result = UnscrambleTrace[{{}, a}, "TimeLimit" -> 0.1];
  check["slow scoring bounded", result["TerminationReason"] === "TimeLimit" && result["Result"] === {{}, a}];
  Unscrambling`Private`loadUnscrambleModel[_] := (Pause[2]; True);
  result = UnscrambleTrace[{{}, a}, "TimeLimit" -> 0.1];
  check["slow model load bounded", result["TerminationReason"] === "TimeLimit" && !result["ModelLoaded"]];
  Unscrambling`Private`loadUnscrambleModel[_] := False;
  calls = 0;
  result = Quiet[UnscrambleTrace[{{}, a + b}, "Attempts" -> 1]];
  check["missing model does not invoke the policy", calls === 0 && !result["ModelLoaded"] &&
    result["TerminationReason"] === "ModelUnavailable" && result["Result"] === {{}, a + b} &&
    result["Progress"]["Steps"] === 0];
];
check["noninteger step cap rejected", Quiet[UnscrambleTrace[{{}, a}, "MaxSteps" -> 1.5]] === $Failed];
check["unknown step cap rejected", Quiet[UnscrambleTrace[{{}, a}, "MaxSteps" -> Automatic]] === $Failed];
check["invalid patience rejected", Quiet[UnscrambleTrace[{{}, a}, "MaxStagnantSteps" -> 0]] === $Failed];
check["invalid timeout rejected", Quiet[UnscrambleTrace[{{}, a}, "TimeLimit" -> -1]] === $Failed];
check["invalid progress setting rejected", Quiet[UnscrambleTrace[{{}, a}, "ProgressReporting" -> "yes"]] === $Failed];
Do[check["invalid inference kernel count rejected: " <> ToString[count, InputForm],
  Quiet[UnscrambleTrace[{{}, a}, "Kernels" -> count]] === $Failed],
  {count, {0, -1, 1.5, {}, {1}, "two"}}];
check["noninteger inference random seed rejected",
  Quiet[UnscrambleTrace[{{}, a}, RandomSeed -> 1.5]] === $Failed];

Block[{Unscrambling`Private`legalMoves, Unscrambling`Private`scoreCandidates, moves},
  moves = {<|"Name" -> "TestA", "Subtract" -> 0, "Insert" -> 0|>,
    <|"Name" -> "TestB", "Subtract" -> 0, "Insert" -> 0|>, Unscrambling`Private`stopMove[]};
  Unscrambling`Private`legalMoves[_, _] := moves;
  Unscrambling`Private`scoreCandidates[_, _, _] := {0.9, 0.8, 1.};
  choices = Table[Unscrambling`Private`selectInferenceCandidate[a, {},
    <|"FirstChoice" -> rank|>]["Index"], {rank, 2}];
  check["parallel first choices partition non-stop model-ranked edits", choices === {1, 2}];
  check["greedy continuation may choose the policy stop action",
    Unscrambling`Private`selectInferenceCandidate[a, {}, <|"Greedy" -> True|>]["Index"] === 3];
  request = <|"Expression" -> a, "Rules" -> {}, "RouteID" -> 2, "Generation" -> 4,
    "RequestID" -> "test", "Seed" -> 7821, "DrawIndex" -> 17, "Seconds" -> 1.|>;
  check["worker sampling is reproducible for a route generation and draw",
    Unscrambling`Private`evaluateInferenceEdit[request]["Selection"]["Index"] ===
      Unscrambling`Private`evaluateInferenceEdit[request]["Selection"]["Index"]];
  BlockRandom[
    SeedRandom[82]; expected = RandomInteger[1000000]; SeedRandom[82];
    Unscrambling`Private`evaluateInferenceEdit[request];
    check["worker exploration does not alter the caller random stream",
      RandomInteger[1000000] === expected]
  ];
];

(* Exploration weights and route-local history do not require numerical inference. *)
Block[{Unscrambling`Private`legalMoves, Unscrambling`Private`scoreCandidates,
    moves, scoreCalls = 0, capturedWeights = {}},
  moves = {<|"Name" -> "TestA", "Subtract" -> a, "Insert" -> b|>,
    <|"Name" -> "TestB", "Subtract" -> a, "Insert" -> c|>, Unscrambling`Private`stopMove[]};
  Unscrambling`Private`legalMoves[_, _] := moves;
  Unscrambling`Private`scoreCandidates[_, _, _] := (scoreCalls++; {1., 0., -10.});
  Block[{System`RandomChoice = Function[choice,
      AppendTo[capturedWeights, choice[[1]]]; First[choice[[2]]]]},
    Unscrambling`Private`selectInferenceCandidate[a, {}, <|"Temperature" -> 0.5|>];
    Unscrambling`Private`selectInferenceCandidate[a, {}, <|"Temperature" -> 2.|>];
    coldWeights = capturedWeights[[1]]; warmWeights = capturedWeights[[2]];
    check["warmer exploration gives lower-ranked moves more relative probability",
      coldWeights[[2]]/coldWeights[[1]] < warmWeights[[2]]/warmWeights[[1]]];
    capturedWeights = {};
    Unscrambling`Private`selectInferenceCandidate[a, {}, <|"Temperature" -> 1.|>];
    unusedWeights = First[capturedWeights];
    Unscrambling`Private`selectInferenceCandidate[a, {}, <|"Temperature" -> 1.,
      "MoveVisits" -> <|Unscrambling`Private`inferenceMoveKey[First[moves]] -> 9|>|>];
    usedWeights = Last[capturedWeights];
    check["shared move usage is a soft probability penalty rather than a ban",
      0 < usedWeights[[1]]/usedWeights[[2]] < unusedWeights[[1]]/unusedWeights[[2]]];
  ];
  scoreCalls = 0;
  chosen = Unscrambling`Private`selectInferenceCandidate[a, {}, <|"Greedy" -> True,
    "VisitedStateKeys" -> (Unscrambling`Private`inferenceStateKey /@ {a, b})|>];
  check["a visited result is rerolled without rescoring the expression",
    scoreCalls === 1 && chosen["Candidate"] === moves[[2]] && chosen["After"] === c &&
      chosen["LoopAvoided"] === 1];
  chosen = Unscrambling`Private`selectInferenceCandidate[a, {}, <|"Greedy" -> True,
    "VisitedStateKeys" -> {Unscrambling`Private`inferenceStateKey[a]}|>];
  check["a state visited on another route is not globally forbidden",
    chosen["After"] === b && chosen["LoopAvoided"] === 0];
  chosen = Unscrambling`Private`selectInferenceCandidate[a, {}, <|"Greedy" -> True,
    "VisitedStateKeys" -> (Unscrambling`Private`inferenceStateKey /@ {a, b, c})|>];
  check["stop remains available after every algebraic choice would repeat a state",
    chosen["Candidate"]["Name"] === "Stop" && chosen["After"] === a && chosen["LoopAvoided"] === 2];
  chosen = Unscrambling`Private`selectInferenceCandidate[a, {}, <|"FirstChoice" -> 1,
    "ExcludedMoveKeys" -> (Unscrambling`Private`inferenceMoveKey /@ Most[moves])|>];
  check["scarce non-stop choices fall back instead of suppressing the only useful route",
    TrueQ[chosen["SharedMoveFallback"]] && chosen["Candidate"]["Name"] =!= "Stop"];
  scoreCalls = 0;
  chosen = Unscrambling`Private`selectInferenceCandidate[a, {}, <|"Greedy" -> True,
    "ExcludedMoveKeys" -> {Unscrambling`Private`inferenceMoveKey[First[moves]]},
    "VisitedStateKeys" -> {Unscrambling`Private`inferenceStateKey[c]}|>];
  check["shared fallback can reopen an excluded move after an unexcluded move would loop",
    chosen["After"] === b && TrueQ[chosen["SharedMoveFallback"]] &&
      chosen["LoopAvoided"] === 1 && scoreCalls === 1];
  scoreCalls = 0;
  packet = Unscrambling`Private`evaluateInferenceEdit[<|"Expression" -> a, "Rules" -> {},
    "RouteID" -> 1, "Generation" -> 0, "RequestID" -> "loop-packet", "Greedy" -> True,
    "Seconds" -> 1., "VisitedStateKeys" -> (Unscrambling`Private`inferenceStateKey /@ {a, b})|>];
  check["worker packets report rerolls and avoid copying the resulting expression into selection",
    packet["Status"] === "Edited" && packet["After"] === c && packet["LoopAvoided"] === 1 &&
      scoreCalls === 1 && !KeyExistsQ[packet["Selection"], "After"]];
  Unscrambling`Private`legalMoves[_, _] := Most[moves];
  Unscrambling`Private`scoreCandidates[_, _, _] := (scoreCalls++; {1., 0.});
  chosen = Unscrambling`Private`selectInferenceCandidate[a, {}, <|"Greedy" -> True,
    "VisitedStateKeys" -> (Unscrambling`Private`inferenceStateKey /@ {a, b, c})|>];
  check["exhausted loop-free choices have an ordinary no-moves outcome", MissingQ[chosen]];
];

check["route temperatures span the candidate-scorer sampling baseline",
  Chop[Unscrambling`Private`inferenceRouteTemperature["CandidateScorer", 1, 3] - 0.1] === 0 &&
    Chop[Unscrambling`Private`inferenceRouteTemperature["CandidateScorer", 2, 3] - 0.2] === 0 &&
    Chop[Unscrambling`Private`inferenceRouteTemperature["CandidateScorer", 3, 3] - 0.4] === 0];
check["route temperatures span the compact-actions sampling baseline",
  Chop[Unscrambling`Private`inferenceRouteTemperature["CompactActions", 1, 3] - 0.5] === 0 &&
    Chop[Unscrambling`Private`inferenceRouteTemperature["CompactActions", 3, 3] - 2.] === 0];

Block[{Unscrambling`Private`inferenceRead, reads = 0, ticket, currentPacket},
  ticket = <|"Worker" -> testWorker1, "RouteID" -> 1, "Generation" -> 2, "RequestID" -> "live"|>;
  currentPacket = <|"Status" -> "Stop", "RouteID" -> 1, "Generation" -> 2,
    "RequestID" -> "live", "After" -> a|>;
  Unscrambling`Private`inferenceRead[_] := (
    reads++;
    If[reads === 1,
      HoldComplete[<|"Status" -> "Edited", "RouteID" -> 1, "Generation" -> 1,
        "RequestID" -> "stale", "After" -> 0|>],
      With[{packet = currentPacket}, HoldComplete[packet]]]
  );
  receivedPacket = Unscrambling`Private`inferenceReceive[{ticket}, 0.1];
  check["pinned channel discards a stale request ID before reading the current result",
    TrueQ[receivedPacket["Ready"]] && receivedPacket["Handle"] === ticket &&
      receivedPacket["Result"] === currentPacket && reads === 2];
  Unscrambling`Private`inferenceRead[_] := HoldComplete[7];
  receivedPacket = Unscrambling`Private`inferenceReceive[{ticket}, 0.1];
  check["malformed pinned worker replies remain failures",
    TrueQ[receivedPacket["Ready"]] && receivedPacket["Result"]["Status"] === "WorkerFailed"];
];

(* Controlled asynchronous replies exercise the coordinator without a neural backend. *)
Block[{Unscrambling`Private`loadUnscrambleModel, Unscrambling`Private`withInferenceWorkers,
    Unscrambling`Private`inferenceSubmit, Unscrambling`Private`inferenceReceive,
    Unscrambling`Private`cancelInference, submitted = {}, cancellations = {}, sequence = 0,
    received = 0, staleTicket = None, sentStale = False, initialGeneration = None},
  Unscrambling`Private`loadUnscrambleModel[_] := True;
  Unscrambling`Private`withInferenceWorkers[_, _, _, _, body_] := body[{testWorker1, testWorker2}];
  Unscrambling`Private`inferenceSubmit[worker_, request_] := Module[{ticket},
    sequence++;
    If[initialGeneration === None, initialGeneration = request["Generation"]];
    AppendTo[submitted, request];
    ticket = <|"Worker" -> worker, "RouteID" -> request["RouteID"],
      "Generation" -> request["Generation"], "RequestID" -> sequence, "Request" -> request|>;
    If[request["Generation"] === initialGeneration && request["RouteID"] === 2, staleTicket = ticket];
    ticket
  ];
  Unscrambling`Private`cancelInference[tickets_, workers_] := (
    AppendTo[cancellations, <|"Tickets" -> tickets, "Workers" -> workers|>]; True);
  Unscrambling`Private`inferenceReceive[tickets_, _] := Module[{ticket, request, before, after, stop, packet},
    received++;
    If[received > 30, Return[<|"Ready" -> False|>]];
    If[staleTicket =!= None && !sentStale &&
        First[tickets]["Generation"] =!= initialGeneration,
      sentStale = True; ticket = staleTicket; after = 0; stop = False,
      ticket = First[tickets]; request = ticket["Request"];
      before = request["Expression"];
      stop = (ticket["Generation"] === initialGeneration && ticket["RouteID"] === 2) ||
        (ticket["Generation"] =!= initialGeneration &&
          !(before === a + b && ticket["RouteID"] === 2));
      after = Which[ticket["Generation"] === initialGeneration && ticket["RouteID"] === 1,
        Switch[request["DrawIndex"], 1, a + b + c + d, 2, a + b + f, _, a + b],
        before === a + b && ticket["RouteID"] === 2, a, True, before]
    ];
    request = ticket["Request"]; before = request["Expression"];
    packet = <|"Status" -> If[stop, "Stop", "Edited"],
      "Selection" -> <|"Candidate" -> If[stop, Unscrambling`Private`stopMove[],
        <|"Name" -> "Test", "Subtract" -> before, "Insert" -> after|>]|>,
      "Before" -> before, "After" -> after,
      "ComplexityBefore" -> Unscrambling`Private`expressionComplexity[before],
      "ComplexityAfter" -> Unscrambling`Private`expressionComplexity[after],
      "Counts" -> Unscrambling`Private`expressionTermCounts[after],
      "NeutralSplit" -> False, "Changed" -> before =!= after,
      "RouteID" -> ticket["RouteID"], "Generation" -> ticket["Generation"],
      "WorkerID" -> ticket["Worker"], "ElapsedSeconds" -> 0.|>;
    <|"Ready" -> True, "Handle" -> ticket, "Result" -> packet|>
  ];
  result = UnscrambleTrace[{{}, a + b + c}, "Kernels" -> 2, "Attempts" -> 1,
    "MaxStagnantSteps" -> 3, "TimeLimit" -> 2, "ProgressReporting" -> False];
  requestGroups = GroupBy[submitted, #["Generation"] &];
  check["parallel routes begin together from one checkpoint",
    Lookup[Take[First[Values[requestGroups]], 2], "Expression"] === {a + b + c, a + b + c}];
  check["local improvements that do not beat the global best leave other routes running",
    Count[Lookup[result["Trace"], "Generation"], initialGeneration] === 4];
  check["every global improvement restarts all routes at the shared best",
    Length[requestGroups] === 3 &&
      Lookup[Values[requestGroups][[2]], "Expression"] === {a + b, a + b} &&
      Lookup[Values[requestGroups][[3]], "Expression"] === {a, a}];
  check["parallel restart cancels only the inference workers",
    Length[cancellations] >= 2 && AllTrue[cancellations,
      #["Workers"] === {testWorker1, testWorker2} &]];
  check["late replies from an old generation cannot replace the checkpoint",
    sentStale && result["Result"] === {{}, a} && FreeQ[result["Trace"],
      KeyValuePattern["After" -> 0]]];
  check["parallel routes have distinct initial branch instructions",
    Length[DeleteDuplicates[Lookup[Take[First[Values[requestGroups]], 2], {"Seed", "FirstChoice"}]]] === 2];
  check["parallel progress retains a row for each route",
    ListQ[Lookup[result["Progress"], "Routes", None]] &&
      Length[result["Progress"]["Routes"]] === 2];
  check["parallel requests and progress expose distinct policy temperatures",
    Lookup[Take[First[Values[requestGroups]], 2], "Temperature"] === {0.1, 0.4} &&
      Lookup[result["Routes"], "Temperature"] === {0.1, 0.4}];
  check["a global record resets every route history and the shared move catalog",
    AllTrue[Rest[Values[requestGroups]], Function[group,
      AllTrue[group, #["VisitedStateKeys"] === {Unscrambling`Private`inferenceStateKey[#["Expression"]]} &&
        #["MoveVisits"] === <||> && #["ExcludedMoveKeys"] === {} &]]]];
];

(* A duplicate proposal is retried finitely without consuming accepted-step patience. *)
Block[{Unscrambling`Private`loadUnscrambleModel, Unscrambling`Private`withInferenceWorkers,
    Unscrambling`Private`inferenceSubmit, Unscrambling`Private`inferenceReceive,
    Unscrambling`Private`cancelInference, submitted = {}, sequence = 0, fallback = False,
    initial = a + b + c, grown = a + b + c + d, repeatedMove},
  repeatedMove = <|"Name" -> "Test", "Subtract" -> initial, "Insert" -> grown|>;
  Unscrambling`Private`loadUnscrambleModel[_] := True;
  Unscrambling`Private`withInferenceWorkers[_, _, _, _, body_] := body[{testWorker1, testWorker2}];
  Unscrambling`Private`inferenceSubmit[worker_, request_] := (
    sequence++; AppendTo[submitted, request];
    Join[KeyTake[request, {"RouteID", "Generation"}],
      <|"Worker" -> worker, "RequestID" -> sequence, "Request" -> request|>]);
  Unscrambling`Private`cancelInference[_, _] := True;
  Unscrambling`Private`inferenceReceive[tickets_, _] := Module[
    {ticket = First[tickets], request, before, after, stop, move},
    request = ticket["Request"];
    If[Lookup[request, "FirstChoice", 0] > 2,
      Return[<|"Ready" -> True, "Handle" -> ticket, "Result" -> Join[
        KeyTake[ticket, {"RouteID", "Generation"}], <|"Status" -> "NoMoves"|>]|>]];
    before = request["Expression"]; stop = before === grown;
    after = If[stop, before, grown]; move = If[stop, Unscrambling`Private`stopMove[], repeatedMove];
    <|"Ready" -> True, "Handle" -> ticket, "Result" -> <|"Status" -> If[stop, "Stop", "Edited"],
      "Selection" -> <|"Candidate" -> move, "MoveKey" -> Unscrambling`Private`inferenceMoveKey[move]|>,
      "Before" -> before, "After" -> after,
      "ComplexityBefore" -> Unscrambling`Private`expressionComplexity[before],
      "ComplexityAfter" -> Unscrambling`Private`expressionComplexity[after],
      "Counts" -> Unscrambling`Private`expressionTermCounts[after], "NeutralSplit" -> False,
      "Changed" -> before =!= after, "RouteID" -> request["RouteID"],
      "Generation" -> request["Generation"], "WorkerID" -> ticket["Worker"], "ElapsedSeconds" -> 0.,
      "LoopAvoided" -> If[request["RouteID"] === 1 && !stop, 2, 0],
      "SharedMoveFallback" -> fallback|>|>
  ];
  result = UnscrambleTrace[{{}, initial}, "Kernels" -> 2, "Attempts" -> 1,
    "MaxStagnantSteps" -> 10, "TimeLimit" -> 2, "ProgressReporting" -> False];
  secondRouteRequests = Select[submitted, #["RouteID"] === 2 &];
  check["other-route collisions retry at most three times before permitting a shared prefix",
    result["DuplicateAvoided"] === 3 && result["TerminationReason"] === "RoutesExhausted" &&
      Count[Lookup[result["Trace"], "After"], grown] === 4];
  check["collision retries use a fresh draw without forcing the same initial rank",
    Lookup[secondRouteRequests, "DrawIndex"] === Range[5] &&
      KeyExistsQ[First[secondRouteRequests], "FirstChoice"] &&
      AllTrue[Rest[secondRouteRequests], !KeyExistsQ[#, "FirstChoice"] &]];
  check["collision rerolls leave accepted steps and stagnation counters unchanged",
    result["Progress"]["Steps"] === 4 && Lookup[result["Routes"], "Steps"] === {2, 2} &&
      Lookup[result["Routes"], "StagnantSteps"] === {2, 2} &&
      Lookup[result["Routes"], "DuplicateAvoided"] === {0, 3}];
  check["loop rerolls are reported separately from accepted edits",
    result["LoopAvoided"] === 2 && result["Progress"]["LoopAvoided"] === 2 &&
      Lookup[result["Routes"], "LoopAvoided"] === {2, 0}];
  check["shared edges are penalized without a global ban on other-route states",
    AllTrue[secondRouteRequests[[2 ;; 4]],
      #["VisitedStateKeys"] === {Unscrambling`Private`inferenceStateKey[initial]} &&
        Lookup[#["MoveVisits"], Unscrambling`Private`inferenceMoveKey[repeatedMove], 0] === 1 &&
        MemberQ[#["ExcludedMoveKeys"], Unscrambling`Private`inferenceMoveKey[repeatedMove]] &]];
  fallback = True; submitted = {}; sequence = 0;
  result = UnscrambleTrace[{{}, initial}, "Kernels" -> 2, "Attempts" -> 1,
    "MaxStagnantSteps" -> 10, "TimeLimit" -> 2, "ProgressReporting" -> False];
  check["a worker's scarce-choice fallback bypasses unnecessary coordinator rerolls",
    result["DuplicateAvoided"] === 0 && result["Progress"]["Steps"] === 4 && Length[submitted] === 4];
  submitted = {}; sequence = 0;
  result = UnscrambleTrace[{{}, initial}, "Kernels" -> 2, "Attempts" -> 2,
    "MaxStagnantSteps" -> 10, "TimeLimit" -> 2, "ProgressReporting" -> False];
  nextAttemptRequests = Select[submitted, Lookup[#, "FirstChoice", 0] > 2 &];
  check["new attempts clear local history but retain the generation's shared edge usage",
    Length[nextAttemptRequests] === 2 &&
      AllTrue[nextAttemptRequests, #["VisitedStateKeys"] === {Unscrambling`Private`inferenceStateKey[initial]} &&
        #["Expression"] === initial && !#["Greedy"] && #["ExcludedMoveKeys"] === {} &&
        Lookup[#["MoveVisits"], Unscrambling`Private`inferenceMoveKey[repeatedMove], 0] === 2 &]];
];

Block[{Unscrambling`Private`loadUnscrambleModel, Unscrambling`Private`withInferenceWorkers,
    Unscrambling`Private`inferenceSubmit, Unscrambling`Private`inferenceReceive,
    Unscrambling`Private`cancelInference, submitted = {}, sequence = 0},
  Unscrambling`Private`loadUnscrambleModel[_] := True;
  Unscrambling`Private`withInferenceWorkers[_, _, _, _, body_] := body[{testWorker1, testWorker2}];
  Unscrambling`Private`inferenceSubmit[worker_, request_] := (
    sequence++; AppendTo[submitted, request];
    Join[KeyTake[request, {"RouteID", "Generation"}],
      <|"Worker" -> worker, "RequestID" -> sequence, "Request" -> request|>]);
  Unscrambling`Private`cancelInference[_, _] := True;
  Unscrambling`Private`inferenceReceive[tickets_, _] := With[{ticket = First[tickets]},
    <|"Ready" -> True, "Handle" -> ticket, "Result" -> Join[
      KeyTake[ticket, {"RouteID", "Generation"}], <|"Status" -> "NoMoves"|>]|>];
  result = UnscrambleTrace[{{}, a + b}, "Kernels" -> 2, "Attempts" -> 2,
    "TimeLimit" -> 2, "ProgressReporting" -> False];
  check["no loop-free legal move finishes ordinary attempts rather than failing a worker",
    result["TerminationReason"] === "RoutesExhausted" && !result["ScoringFailed"] &&
      Lookup[result["Routes"], "Status"] === {"NoMoves", "NoMoves"} &&
      Lookup[result["Routes"], "Attempt"] === {2, 2} && result["Progress"]["Steps"] === 0];
  check["new attempts begin from the checkpoint with clean local history and sampled continuation",
    Length[submitted] === 4 && Lookup[Take[submitted, -2], "Greedy"] === {False, False} &&
      AllTrue[submitted, #["VisitedStateKeys"] === {Unscrambling`Private`inferenceStateKey[a + b]} &&
        #["ExcludedMoveKeys"] === {} &]];
];

Block[{Unscrambling`Private`loadUnscrambleModel, Unscrambling`Private`withInferenceWorkers,
    Unscrambling`Private`inferenceSubmit, Unscrambling`Private`inferenceReceive,
    Unscrambling`Private`cancelInference, ticketsMade = 0, receives = 0, cancelled = False},
  Unscrambling`Private`loadUnscrambleModel[_] := True;
  Unscrambling`Private`withInferenceWorkers[_, _, _, _, body_] := body[{testWorker1, testWorker2}];
  Unscrambling`Private`inferenceSubmit[worker_, request_] := (
    ticketsMade++; Join[KeyTake[request, {"RouteID", "Generation"}],
      <|"Worker" -> worker, "RequestID" -> ticketsMade, "Request" -> request|>]);
  Unscrambling`Private`cancelInference[_, _] := (cancelled = True; True);
  Unscrambling`Private`inferenceReceive[tickets_, _] := Module[{ticket, before, after},
    receives++; ticket = First[tickets]; before = ticket["Request"]["Expression"];
    If[receives > 1, Pause[2]];
    after = a;
    <|"Ready" -> True, "Handle" -> ticket, "Result" -> <|"Status" -> "Edited",
      "Selection" -> <|"Candidate" -> <|"Name" -> "Test", "Subtract" -> before, "Insert" -> after|>|>,
      "Before" -> before, "After" -> after,
      "ComplexityBefore" -> Unscrambling`Private`expressionComplexity[before],
      "ComplexityAfter" -> Unscrambling`Private`expressionComplexity[after],
      "Counts" -> Unscrambling`Private`expressionTermCounts[after],
      "NeutralSplit" -> False, "Changed" -> True, "RouteID" -> ticket["RouteID"],
      "Generation" -> ticket["Generation"], "WorkerID" -> ticket["Worker"], "ElapsedSeconds" -> 0.|>|>
  ];
  result = UnscrambleTrace[{{}, a + b}, "Kernels" -> 2, "TimeLimit" -> 0.1,
    "Attempts" -> 1, "ProgressReporting" -> False];
  check["parallel timeout preserves an accepted improvement",
    result["Result"] === {{}, a} && result["TerminationReason"] === "TimeLimit"];
  check["parallel timeout cancels outstanding owned work", cancelled];
  check["timed-out progress rows no longer claim to be running",
    Lookup[result["Progress"]["Routes"], "Status"] === {"TimeLimit", "TimeLimit"}];
  Unscrambling`Private`withInferenceWorkers[_, _, _, _, body_] := (Pause[2]; body[{testWorker1, testWorker2}]);
  result = UnscrambleTrace[{{}, a + b}, "Kernels" -> 2, "TimeLimit" -> 0.1,
    "ProgressReporting" -> False];
  check["parallel startup is inside the overall timeout",
    result["Result"] === {{}, a + b} && result["TerminationReason"] === "TimeLimit" &&
      result["Progress"]["ElapsedSeconds"] < 1.];
  receives = 0; cancelled = False;
  Unscrambling`Private`withInferenceWorkers[_, _, _, _, body_] := body[{testWorker1, testWorker2}];
  Unscrambling`Private`inferenceReceive[tickets_, _] := Module[{ticket, before},
    receives++; If[receives > 1, Abort[]];
    ticket = First[tickets]; before = ticket["Request"]["Expression"];
    <|"Ready" -> True, "Handle" -> ticket, "Result" -> <|"Status" -> "Edited",
      "Selection" -> <|"Candidate" -> <|"Name" -> "Test", "Subtract" -> before, "Insert" -> a|>|>,
      "Before" -> before, "After" -> a,
      "ComplexityBefore" -> Unscrambling`Private`expressionComplexity[before], "ComplexityAfter" -> 1,
      "Counts" -> Unscrambling`Private`expressionTermCounts[a], "NeutralSplit" -> False,
      "Changed" -> True, "RouteID" -> ticket["RouteID"], "Generation" -> ticket["Generation"],
      "WorkerID" -> ticket["Worker"], "ElapsedSeconds" -> 0.|>|>
  ];
  result = CheckAbort[UnscrambleTrace[{{}, a + b}, "Kernels" -> 2, "TimeLimit" -> 2,
    "Attempts" -> 1, "ProgressReporting" -> False], leakedAbort];
  check["user abort returns the best parallel checkpoint and cancels owned work",
    AssociationQ[result] && result["Result"] === {{}, a} &&
      result["TerminationReason"] === "Aborted" && cancelled];
];

Block[{Unscrambling`Private`loadUnscrambleModel, Unscrambling`Private`withInferenceWorkers,
    Unscrambling`Private`inferenceSubmit, Unscrambling`Private`inferenceReceive,
    Unscrambling`Private`cancelInference, sequence = 0, firstGeneration = None},
  Unscrambling`Private`loadUnscrambleModel[_] := True;
  Unscrambling`Private`withInferenceWorkers[_, _, _, _, body_] := body[{testWorker1, testWorker2}];
  Unscrambling`Private`inferenceSubmit[worker_, request_] := (
    sequence++; If[firstGeneration === None, firstGeneration = request["Generation"]];
    Join[KeyTake[request, {"RouteID", "Generation"}],
      <|"Worker" -> worker, "RequestID" -> sequence, "Request" -> request|>]);
  Unscrambling`Private`cancelInference[_, _] := True;
  Unscrambling`Private`inferenceReceive[tickets_, _] := Module[
    {ticket = First[tickets], request, before, after, neutral, stop, edit, move},
    request = ticket["Request"]; before = request["Expression"]; edit = request["DrawIndex"];
    neutral = request["Generation"] === firstGeneration && request["RouteID"] === 1 && edit <= 12;
    stop = request["Generation"] =!= firstGeneration || request["RouteID"] === 2;
    after = Which[neutral, splitStates[[edit + 1]], stop, before, True, afterSplits];
    move = Which[stop, Unscrambling`Private`stopMove[],
      neutral, <|"Name" -> "MomentumSum", "Identity" -> "Split", "Subtract" -> before, "Insert" -> after|>,
      True, <|"Name" -> "Mass", "Subtract" -> before, "Insert" -> after|>];
    <|"Ready" -> True, "Handle" -> ticket, "Result" -> <|"Status" -> If[stop, "Stop", "Edited"],
      "Selection" -> <|"Candidate" -> move|>, "Before" -> before, "After" -> after,
      "ComplexityBefore" -> Unscrambling`Private`expressionComplexity[before],
      "ComplexityAfter" -> Unscrambling`Private`expressionComplexity[after],
      "Counts" -> Unscrambling`Private`expressionTermCounts[after],
      "NeutralSplit" -> neutral, "Changed" -> before =!= after,
      "RouteID" -> request["RouteID"], "Generation" -> request["Generation"],
      "WorkerID" -> ticket["Worker"], "ElapsedSeconds" -> 0.|>|>
  ];
  result = UnscrambleTrace[{{}, First[splitStates]}, "Kernels" -> 2, "Attempts" -> 1,
    "MaxStagnantSteps" -> 2, "TimeLimit" -> 2, "ProgressReporting" -> False];
  check["parallel neutral splits do not consume route patience",
    result["Result"] === {{}, afterSplits} && result["Progress"]["NeutralSplits"] === 12];
  check["only strict global improvements restart the group", result["Restarts"] === 1];
];

Block[{Unscrambling`Private`inferenceKernelSnapshot, Unscrambling`Private`launchInferenceKernels,
    Unscrambling`Private`closeInferenceKernels, Unscrambling`Private`initializeInferenceWorkers,
    Unscrambling`Private`inferenceProcessorCapacity,
    connected = {callerWorker}, closed = {}, initialized = True, bodyCalls = 0},
  Unscrambling`Private`inferenceProcessorCapacity[] := 4;
  Unscrambling`Private`inferenceKernelSnapshot[] := connected;
  Unscrambling`Private`launchInferenceKernels[n_] := (
    connected = Join[connected, Array[ownedWorker, n]]; Take[connected, -n]);
  Unscrambling`Private`closeInferenceKernels[workers_] := (
    closed = Join[closed, workers]; connected = Complement[connected, workers]; Null);
  Unscrambling`Private`initializeInferenceWorkers[_, _, _, _] := initialized;
  result = Unscrambling`Private`withInferenceWorkers[2, "test-model", <||>, 1,
    Function[workers, bodyCalls++; workers]];
  check["parallel inference closes only its own worker pool",
    result === {ownedWorker[1], ownedWorker[2]} && connected === {callerWorker} &&
      Sort[closed] === {ownedWorker[1], ownedWorker[2]} && bodyCalls === 1];
  closed = {};
  result = Unscrambling`Private`withInferenceWorkers[Automatic, "test-model", <||>, 1,
    Function[workers, Length[workers]]];
  check["automatic inference requests all available processor workers",
    result === 4 && Length[closed] === 4 && connected === {callerWorker}];
  closed = {};
  result = Quiet[Unscrambling`Private`withInferenceWorkers[8, "test-model", <||>, 1,
    Function[workers, Length[workers]]]];
  check["explicit inference worker requests cannot exceed processor capacity",
    result === 4 && Length[closed] === 4 && connected === {callerWorker}];
  initialized = False; closed = {}; bodyCalls = 0;
  result = Quiet[Unscrambling`Private`withInferenceWorkers[2, "test-model", <||>, 1,
    Function[workers, bodyCalls++; True]]];
  check["failed worker initialization closes its pool without entering the search",
    result === $Failed && connected === {callerWorker} && bodyCalls === 0 && Length[closed] === 2];
  initialized = True; closed = {};
  result = CheckAbort[Unscrambling`Private`withInferenceWorkers[2, "test-model", <||>, 1,
    Function[workers, Abort[]]], testAbort];
  check["user abort closes owned inference workers and leaves caller workers connected",
    result === testAbort && connected === {callerWorker} && Length[closed] === 2];
  closed = {}; bodyCalls = 0;
  Unscrambling`Private`launchInferenceKernels[n_] := (
    connected = Join[connected, Array[ownedWorker, n]]; Pause[2]; Take[connected, -n]);
  result = TimeConstrained[Unscrambling`Private`withInferenceWorkers[2, "test-model", <||>, 1,
    Function[workers, bodyCalls++; True]], 0.05, $TimedOut];
  check["overall startup timeout closes partially launched owned workers",
    result === $TimedOut && connected === {callerWorker} && Length[closed] === 2 && bodyCalls === 0];
  Unscrambling`Private`launchInferenceKernels[n_] := (
    System`AbortKernels[]; connected = Join[connected, Array[ownedWorker, n]]; Take[connected, -n]);
  Block[{callerAborts = 0, savedAbortOwnValues},
    Block[{System`AbortKernels = Function[callerAborts++]},
      savedAbortOwnValues = OwnValues[System`AbortKernels];
      result = Unscrambling`Private`withInferenceWorkers[2, "test-model", <||>, 1,
        Function[workers, Length[workers]]];
      check["bootstrap abort hook cannot reset a caller worker pool",
        result === 2 && callerAborts === 0 && connected === {callerWorker}];
      check["caller abort hook own values are restored after worker startup",
        OwnValues[System`AbortKernels] === savedAbortOwnValues];
      System`AbortKernels[];
      check["restored caller abort hook remains callable", callerAborts === 1]
    ];
  ];
];
Print[<|"Checks" -> checks, "Failed" -> Length[failures], "Failures" -> failures|>];
Quit[If[failures === {}, 0, 1]];
