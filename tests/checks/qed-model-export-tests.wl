(* The saved QED model must supply every independent five-point tree amplitude.
   Known diagram derivations are compared with the reloaded text and independent
   Feynman diagrams, not used as a substitute for a failed numerical check. *)
root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Global`$HEPCATpath = FileNameJoin[{root, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
Get[FileNameJoin[{root, "tests", "QED-5-point-residues.wl"}]];
checks = 0; failures = {};
check[name_, value_] := (checks++; If[!TrueQ[value],
  AppendTo[failures, name]; Print["FAIL: ", name]]);
qmeModel = ReadModel[FileNameJoin[{root, "models"}], 3];
qmeFeynmanModel = ReadModel[FileNameJoin[{root, "model_feynman"}], 2];
qmeEntries = qedFivePointModelAmplitudes[];
qmeParts = First /@ qmeEntries;
qmePossibleParts = DeleteDuplicates[Sort /@ Tuples[{"e", "E", "A.+", "A.-"}, 5]];
qmeInventory = Select[qmePossibleParts,
  Count[#, "e"] === Count[#, "E"] && MemberQ[{1, 2}, Count[#, "e"]] &];
qmeRows = Select[qmeModel[[2]], Length[DeleteCases[Drop[#, -2], ""]] === 5 &];
check["the electron/photon tree inventory has six independent nonzero five-point processes",
  Length[qmeEntries] === 6 && Sort[Sort /@ qmeParts] === Sort[qmeInventory]];
check["exhaustive diagram generation finds no missing five-point particle multisets",
  Sort[Select[qmePossibleParts, CreateDiagrams[#, prModel] =!= {} &]] === Sort[qmeInventory]];
check["the factorization model stores every five-point multiset exactly once",
  Length[qmeRows] === 6 && Sort[Sort[Drop[#, -2]] & /@ qmeRows] === Sort[qmeInventory]];
qmeLookup[particles_List] := LookupAmplitude[
  MapIndexed[{#1, First[#2]} &, particles], qmeModel, False,
  {qmeSpin1, qmeSpin2, qmeSpin3, qmeSpin4}];
Do[
  qmeExpected = entry[[2]] entry[[3]];
  qmeActual = qmeLookup[First[entry]];
  check["the saved text roundtrips the diagram derivation " <> ToString[First[entry], InputForm],
    Cancel[Together[qmeActual - qmeExpected]] === 0];
  check["saved amplitudes have no reference spinors or unexpanded x factors " <>
      ToString[First[entry], InputForm],
    FreeQ[qmeActual, Spinor["Xi", ___] | _xFactor | _xTildeFactor]],
  {entry, qmeEntries}];

qmeProcesses = Join[
  (Join[{"e", "E"}, #] & /@ Tuples[{"A.+", "A.-"}, 3]),
  {{"e", "E", "E", "e", "A.+"}, {"e", "E", "E", "e", "A.-"}}];
qmeAllPass[report_] := AssociationQ[report] &&
  AllTrue[report["Checks"], #["Status"] === "PASS" &];
qmeVerify[particles_List, overrides_, seed_Integer] := Module[
  {amplitude = qmeLookup[particles], channels, feynman, report},
  channels = FactorizationAmplitudeChannels[particles, qmeModel];
  feynman = PrepareAmplitudes[diagramAmplitudes[
      CreateDiagrams[particles, qmeFeynmanModel]], qmeFeynmanModel,
    "ReferenceSpinors" -> "IndependentXi", "MaxSteps" -> 0,
    "ProgressReporting" -> False];
  report = CheckAmplitudes[amplitude, channels, qmeModel,
    "FeynmanAmplitudes" -> feynman, "FeynmanModel" -> qmeFeynmanModel,
    "ParameterRules" -> overrides, "Samples" -> 2,
    "HighEnergySamples" -> 1, "ReferenceSamples" -> 1,
    "RandomSeed" -> seed, "TimeConstraint" -> 90, "Output" -> "Details"];
  check["all physical, high-energy, reference and Feynman checks pass " <>
      ToString[particles, InputForm], qmeAllPass[report]];
  check["both independent Feynman samples and all massive spin components were compared " <>
      ToString[particles, InputForm],
    report["FeynmanCheck"]["CompletedSamples"] === 2 &&
      AllTrue[report["FeynmanCheck"]["Samples"],
        Length[#["SpinVariables"]] === Count[particles, "e" | "E"] &]];
  report
];
Print["Checking all eight ordered three-photon helicities and both radiation helicities."];
qmeReports = MapIndexed[qmeVerify[#1, Automatic, 1729 + 41 First[#2]] &, qmeProcesses];
qmePermutedReports = MapIndexed[qmeVerify[#1, {"Me" -> 3/7, "EE" -> 2},
    312791 + 41 First[#2]] &,
  {{"A.-", "E", "e", "E", "e"}, {"e", "e", "E", "E", "A.+"}}];
check["a missing pure-photon tree amplitude remains zero",
  qmeLookup[{"A.+", "A.-", "A.+", "A.-", "A.+"}] === 0];

(* This checks availability and sewing, not a completed six-point amplitude. *)
qmeSixProcesses = {
  {"e", "E", "A.+", "A.+", "A.+", "A.+"},
  {"e", "E", "A.+", "A.-", "A.+", "A.-"},
  {"e", "E", "e", "E", "A.+", "A.-"},
  {"e", "E", "e", "E", "e", "E"}};
qmeSixChannels = FactorizationAmplitudeChannels[#, qmeModel] & /@ qmeSixProcesses;
check["six-point sewing includes the new three-by-five and existing four-by-four channels",
  Length /@ qmeSixChannels === {14, 14, 20, 18} &&
    FreeQ[qmeSixChannels, _Failure | $Failed | _LookupAmplitude]];
check["the cubic diagram model has not acquired the new factorization amplitudes",
  AllTrue[prModel[[2]], Length[DeleteCases[Drop[#, -2], ""]] === 3 &]];

Print[checks, " QED model-export checks; ", Length[failures], " failures."];
If[failures =!= {}, Print[failures]; Quit[1]];
Print["HEPCAT_TEST_COMPLETED:qed-model-export-tests.wl"];
Quit[0];
