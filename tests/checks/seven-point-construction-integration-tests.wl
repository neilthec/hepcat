(* Opt-in complete public seven-point call, with no supplied answer or basis. *)
root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Get[FileNameJoin[{root, "source", "HEPCAT.wl"}]];
$HistoryLength = 0;
diagrams = ReadModel[FileNameJoin[{root, "models"}], 2];
model = ReadModel[FileNameJoin[{root, "models"}], 3];
benchmark = ReadModel[FileNameJoin[{root, "model_feynman"}], 2];
before = {diagrams, model, benchmark};
process = Join[{"e", "E"}, ConstantArray["A.+", 5]];
report = ConstructAmplitude[process, diagrams, model, "FeynmanModel" -> benchmark,
  "RandomSeed" -> 2492414739354637420, "Output" -> "Details", "PrintReport" -> False];
checks = 0; failures = {};
SetAttributes[check, HoldRest];
check[name_, test_] := (checks++; If[!TrueQ[test], AppendTo[failures, name]; Print["FAIL: ", name]]);
check["ordinary automatic construction completes", AssociationQ[report] && report["Status"] === "COMPLETE"];
If[AssociationQ[report] && report["Status"] === "COMPLETE",
  check["all thirty physical channels and every final check pass", report["Checks"]["Summary"] === <|"PASS" -> 33|> &&
    Length[report["Checks"]["Channels"]] === 30];
  check["the completed amplitude has only physical propagators and no Xi",
    TrueQ[report["AchievedPhysicalDenominators"]] && report["Checks"]["SpuriousChecks"] === {} &&
      FreeQ[report["Amplitude"], (Spinor | SpinorHat)["Xi", ___]]];
  check["the current reaches the verified compactness benchmark", report["Complexity"] <= 820];
  selection = report["RepresentativeSelection"];
  check["both independent parameter assignments pass without using the benchmark for construction",
    selection["BenchmarkUsedForConstruction"] === False && Length[selection["ValidationReports"]] === 2 &&
    AllTrue[selection["ValidationReports"], #["Report"]["Summary"] === <|"PASS" -> 33|> &]];
  check["the topology-derived current is the selected route",
    report["Stages"]["Physical chiral current"]["Method"] === "PhysicalChiralSubsetCurrent"];
  Print["SEVEN-POINT RESULT ", KeyTake[report, {"Status", "Complexity", "TotalTime", "AchievedPhysicalDenominators"}]]
];
check["the caller's models retain their original data", before === {diagrams, model, benchmark}];
Print["CHECKS ", checks, "; FAILURES ", Length[failures]];
If[failures === {}, Print["HEPCAT_TEST_COMPLETED:seven-point-construction-integration-tests.wl"], Print[failures]];
Quit[If[failures === {}, 0, 1]];
