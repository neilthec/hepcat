(* Deterministic search-control tests, without executing the neural backend. *)
root = DirectoryName[DirectoryName[$InputFileName]];
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
  check["stagnation stops across attempts", calls === 4 && result["TerminationReason"] === "Stagnation"];
  check["intermediate best retained", result["Result"] === {{}, a + b}];
  check["trace checkpoint records improvement", First[result["Checkpoints"]]["Accepted"]];
  calls = 0;
  Unscrambling`Private`selectCandidate[ex_, _, _] := (
    calls++;
    <|"Candidate" -> <|"Name" -> "Test", "Subtract" -> ex,
      "Insert" -> Switch[calls, 1, a + b + c + d, 2, a + b, 3, a + b + c, 4, a, _, a + b]|>|>
  );
  result = UnscrambleTrace[{{}, a + b + c}, "MaxStagnantSteps" -> 2];
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
  check["repeated stop bounded", calls === 2 && result["StagnantSteps"] === 2];
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
