(* Graph, encoding, and call-count tests without native numerical execution. *)
root = DirectoryName[DirectoryName[$InputFileName]];
Global`$HEPCATpath = FileNameJoin[{root, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
Get[FileNameJoin[{root, "tests", "unscrambling.wl"}]];
checks = 0; failures = {};
check[name_, value_] := (checks++; If[!TrueQ[value], AppendTo[failures, name]]);
ang[i_, j_] := SpinorChain[Spinor["Helicity", "Angle", i], Spinor["Helicity", "Angle", j]];
rules = Table[Mass[i] -> 0, {i, 6}];
amp = ang[1, 2]*ang[3, 4]*ang[5, 6];
cands = UnscrambleCandidates[{rules, amp}];
features = Unscrambling`Private`groupFeatures[amp, rules, cands];
check["one state and complete candidate set", Keys[features] === {"State", "Candidates"} && Length[features["Candidates"]] === Length[cands]];
check["rectangular candidate matrix", MatrixQ[features["Candidates"], IntegerQ]];
separate = UnscrambleEncoding[{rules, amp}, #] & /@ cands;
check["state independent of edit", AllTrue[separate, #["State"] === features["State"] &]];
check["padding preserves complete edits", Map[DeleteCases[#, 257] &, features["Candidates"]] === Lookup[separate, "Candidate"]];
check["state not included in edit bytes", FreeQ[
  StringContainsQ[FromCharacterCode[#["Candidate"] - 1, "UTF8"], "HEPCATStateV3"] & /@ separate, True]];
check["momentum-only labels normalized", Unscrambling`Private`stateLegIDs[Mom[900]*MomProd[800, 700], {Mass[600] -> 0}] ===
  <|600 -> 1, 700 -> 2, 800 -> 3, 900 -> 4|>];
Block[{Unscrambling`Private`$modelNet, calls = 0, input},
  Unscrambling`Private`$modelNet[data_, ___] := (calls++; input = data; List /@ N[Range[Length[data["Candidates"]]]]);
  scores = Unscrambling`Private`scoreCandidates[amp, rules, cands];
  check["one network evaluation for all candidates", calls === 1 && input === features];
  check["candidate score order retained", scores === N[Range[Length[cands]]]];
  Unscrambling`Private`$modelNet[data_, ___] := {{0.}};
  check["wrong score count rejected", Unscrambling`Private`scoreCandidates[amp, rules, cands] === $Failed];
];
labels = {1, 0, 0, 0}; weights = Unscrambling`Private`balanceWeights[labels];
check["balanced class contribution without duplication", Abs[First[weights]^2 - Total[Rest[weights]^2]] < 10^-12];
rows = Unscrambling`Private`trainingRows[amp, rules, amp];
check["training groups all edits under one state", Length[rows] === 1 && First[rows]["State"] === features["State"] &&
  Length[First[rows]["Target"]] === Length[cands] && Length[First[rows]["Weights"]] === Length[cands]];
policy = NetInitialize[Unscrambling`Private`makePolicy[], RandomSeeding -> 1];
training = NetInitialize[Unscrambling`Private`makeTrainingPolicy[], RandomSeeding -> 1];
check["shared graph initializes", Head[policy] === NetGraph];
check["weighted training graph initializes", Head[training] === NetGraph];
check["state encoder outside candidate mapping", Head[NetExtract[policy, "StateEncoder"]] === NetChain &&
  Head[NetExtract[policy, "CandidateEncoder"]] === NetMapOperator && Head[NetExtract[policy, "Scorer"]] === NetMapThreadOperator];
check["trained policy extractable", Head[NetExtract[training, "Policy"]] === NetGraph];
shortRow = <|"State" -> {1, 2}, "Candidates" -> {{1, 2}, {257, 3}},
  "Target" -> {{1.}, {0.}}, "Weights" -> {{1.}, {1.}}|>;
longRow = <|"State" -> {1, 2, 3}, "Candidates" -> {{1, 2, 3, 4, 5}},
  "Target" -> {{1.}}, "Weights" -> {{1.}}|>;
Print["Preparing fixed-width training graph"];
batch = Unscrambling`Private`prepareTrainingBatch[{shortRow, longRow}];
check["training width covers longest edit", batch["Width"] === 5];
check["training padding retains bytes", Map[DeleteCases[#, 257] &, Flatten[Lookup[batch["Rows"], "Candidates"], 1]] ===
  {{1, 2}, {3}, {1, 2, 3, 4, 5}}];
Print["Candidate port: ", NetExtract[batch["Net"], "Candidates"]];
check["training has only leading variable dimension", NetExtract[batch["Net"], "Candidates"] === {"Varying", 5, Restricted["Integer", 257]}];
Print["Initializing fixed-width graph"];
fixedTraining = NetInitialize[batch["Net"], RandomSeeding -> 1];
check["fixed width training initializes", Head[fixedTraining] === NetGraph];
restored = Unscrambling`Private`variableWidthPolicy[NetExtract[fixedTraining, "Policy"]];
check["inference width restored", NetExtract[restored, "Candidates"] === {"Varying", "Varying", Restricted["Integer", 257]}];
check["restoration preserves learned arrays", Information[restored, "Arrays"] === Information[NetExtract[fixedTraining, "Policy"], "Arrays"]];
Print[<|"Checks" -> checks, "Failed" -> Length[failures], "Failures" -> failures|>];
Quit[If[failures === {}, 0, 1]];
