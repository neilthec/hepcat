(* Deterministic search-control tests, without executing the neural backend. *)
root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Global`$HEPCATpath = FileNameJoin[{root, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
Get[FileNameJoin[{root, "tests", "unscrambling.wl"}]];
checks = 0; failures = {};
check[name_, value_] := (checks++; If[!TrueQ[value], AppendTo[failures, name]]);
Block[{Unscrambling`Private`loadUnscrambleModel, Unscrambling`Private`selectCandidate, calls = 0},
  Unscrambling`Private`loadUnscrambleModel[_] := True;
  Unscrambling`Private`selectCandidate[ex_, _, _] := (
    calls++;
    <|"Candidate" -> <|"Name" -> "Test", "Subtract" -> ex,
      "Insert" -> If[calls === 1, a + b, a + b + c + calls]|>|>
  );
  result = UnscrambleTrace[{{}, a + b + c}, "MaxSteps" -> 100, "Attempts" -> 20];
  check["stagnation bounds each attempt", calls === 61 && Length[result["Checkpoints"]] === 20 &&
    result["TerminationReason"] === "Stagnation"];
  check["intermediate best retained", result["Result"] === {{}, a + b}];
  check["trace checkpoint records improvement", First[result["Checkpoints"]]["Accepted"]];
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
];
check["invalid patience rejected", Quiet[UnscrambleTrace[{{}, a}, "MaxStagnantSteps" -> 0]] === $Failed];
check["invalid timeout rejected", Quiet[UnscrambleTrace[{{}, a}, "TimeLimit" -> -1]] === $Failed];
Print[<|"Checks" -> checks, "Failed" -> Length[failures], "Failures" -> failures|>];
Quit[If[failures === {}, 0, 1]];
