(* Independent checker jobs preserve every physical obligation and exact sample. *)
root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Global`$HEPCATpath = FileNameJoin[{root, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
Get[FileNameJoin[{root, "tests", "QED-5-point-residues.wl"}]];
checks = 0; failures = {};
check[name_, value_] := (checks++; If[!TrueQ[value],
  AppendTo[failures, name]; Print["FAIL: ", name]]);
paCheckOptions = {"Output" -> "Details", "Samples" -> 1,
  "HighEnergySamples" -> 1, "ReferenceSamples" -> 1,
  "TimeConstraint" -> 60, "RandomSeed" -> 891031,
  "ParameterRules" -> {prElectronMass -> 1, prElectricCharge -> 1}};
(* Dummy Laurent/spin/chart variables are local bound coordinates, so their
   generated symbol suffixes carry no physical or sample information. *)
paAlpha[value_] := Module[{symbols},
  symbols = DeleteDuplicates[Cases[value, symbol_Symbol /;
      StringMatchQ[SymbolName[Unevaluated[symbol]],
        ("physicalPole" | "automaticCut" | "referencePole" | "inverseEnergyRoot" |
          "externalSpin" | "completionSpin" | "referenceCoordinate" |
          "unusedReferencePole" | "unusedBenchmarkPole") ~~ DigitCharacter ..] :>
        symbol, Infinity, Heads -> True]];
  (* Creation order for a given coordinate kind is retained in both serial
     and worker charts even when algebraic term sorting changes traversal. *)
  symbols = SortBy[symbols, Function[symbol, With[{name = SymbolName[symbol]},
    {StringReplace[name, RegularExpression["[0-9]+$"] -> ""],
      FromDigits[Last[StringCases[name, DigitCharacter ..]]]}]]];
  (* Chart construction can retain exact numeric arithmetic until transport.
     Reevaluation compares those same rational values without dropping fields. *)
  MapAll[Identity, value /. Thread[symbols -> Table[
    Symbol["ConstructiveDiagrams`Private`parallelCheckCoordinate" <> ToString[i]],
    {i, Length[symbols]}]]]
];
paSemantic[report_Association] := Association[KeyValueMap[Function[{key, value},
  key -> Switch[key,
    "Channels" | "SpuriousChecks" | "Checks", paAlpha /@ value,
    "Table" | "SpuriousTable" | "HighEnergyTable" | "ReferenceTable" |
      "FeynmanTable", paAlpha /@ Normal[value],
    _, paAlpha[value]]], KeyDrop[report, "Execution"]]];
check["parallel controls are exposed by the checker and compatibility alias",
  AllTrue[{CheckAmplitudes, CheckPhysicalResidues},
    ("Parallelization" /. Options[#]) === Automatic &&
      ("MaxKernels" /. Options[#]) === Automatic &]];
paSerial = CheckAmplitudes[prH, prChannels, prFixtureModel,
  Sequence @@ paCheckOptions, "FeynmanAmplitudes" -> prH,
  "Parallelization" -> False, "MaxKernels" -> 2];
SeedRandom[891040]; paCallerRandomState = $RandomState;
paParallel = CheckAmplitudes[prH, prChannels, prFixtureModel,
  Sequence @@ paCheckOptions, "FeynmanAmplitudes" -> prH,
  "Parallelization" -> True, "MaxKernels" -> 2];
check["parallel worker startup and checks preserve the caller random state",
  $RandomState === paCallerRandomState];
check["parallel five-point full report preserves all nine obligations",
  paSerial["Summary"] === <|"PASS" -> 9|> &&
    paParallel["Summary"] === paSerial["Summary"]];
check["serial and parallel detailed reports agree apart from coordinate names and execution",
  paSemantic[paSerial] === paSemantic[paParallel]];
check["physical channel order is independent of worker completion order",
  Lookup[paParallel["Channels"], "Cut"] === Lookup[paSerial["Channels"], "Cut"]];
check["physical sample choices and exact residue values remain identical",
  Lookup[paParallel["Channels"], "CalculatedResidue"] ===
    Lookup[paSerial["Channels"], "CalculatedResidue"] &&
  Lookup[paParallel["Channels"], "ExpectedResidue"] ===
    Lookup[paSerial["Channels"], "ExpectedResidue"]];
check["the independent Feynman comparison remains a full amplitude check",
  paParallel["FeynmanCheck"]["Status"] === "PASS" &&
    Length[paParallel["FeynmanCheck"]["Samples"]] === 1];
check["check execution information appears only in detailed reports",
  KeyExistsQ[paParallel, "Execution"] &&
    NoneTrue[Normal[paParallel["Table"]], KeyExistsQ[#, "Execution"] &]];
paSpuriousSerial = CheckAmplitudes[prS2, prChannels, prFixtureModel,
  Sequence @@ paCheckOptions, "Parallelization" -> False];
paSpuriousParallel = CheckAmplitudes[prS2, prChannels, prFixtureModel,
  Sequence @@ paCheckOptions, "Parallelization" -> True, "MaxKernels" -> 2];
check["automatic spurious-pole discovery remains on the unchanged full expression",
  paSpuriousParallel["PoleDiscovery"] === paSpuriousSerial["PoleDiscovery"] &&
    Length[paSpuriousParallel["SpuriousChecks"]] > 0];
check["parallel spurious, high-energy and reference rows preserve serial semantics",
  paSemantic[paSpuriousParallel] === paSemantic[paSpuriousSerial]];
check["spurious pole order and sampled Laurent differences are preserved",
  Lookup[paSpuriousParallel["SpuriousChecks"], "Pole"] ===
    Lookup[paSpuriousSerial["SpuriousChecks"], "Pole"] &&
  Lookup[paSpuriousParallel["SpuriousChecks"], "Status"] ===
    Lookup[paSpuriousSerial["SpuriousChecks"], "Status"]];
paOldSerial = CheckAmplitudes[prOldPermutation, prChannels, prFixtureModel,
  Sequence @@ paCheckOptions, "SpuriousPoles" -> {},
  "HighEnergyCheck" -> False, "ReferenceCheck" -> False,
  "Parallelization" -> False];
paOldParallel = CheckAmplitudes[prOldPermutation, prChannels, prFixtureModel,
  Sequence @@ paCheckOptions, "SpuriousPoles" -> {},
  "HighEnergyCheck" -> False, "ReferenceCheck" -> False,
  "Parallelization" -> True, "MaxKernels" -> 2];
check["the known failed old continuation remains failed in parallel",
  Lookup[paOldSerial["Summary"], "FAIL", 0] > 0 &&
    paSemantic[paOldSerial] === paSemantic[paOldParallel]];
paOpposite = CheckAmplitudes[-prH, prChannels, prFixtureModel,
  Sequence @@ paCheckOptions, "SpuriousPoles" -> {},
  "HighEnergyCheck" -> False, "ReferenceCheck" -> False,
  "Parallelization" -> True, "MaxKernels" -> 2];
check["parallel checks retain useful opposite-sign diagnostics",
  paOpposite["Summary"] === <|"Opp Sign" -> 6|> &&
    AllTrue[paOpposite["Channels"], #["MatchingSigns"] === {-1} &]];
paCustom = CheckAmplitudes[prFourPointAmplitude, prFourPointChannels, prModel,
  Sequence @@ paCheckOptions, "KinematicMap" -> prFourPointMap[1, 2],
  "SpuriousPoles" -> {}, "HighEnergyCheck" -> False, "ReferenceCheck" -> False,
  "Parallelization" -> True, "MaxKernels" -> 2];
check["custom user maps are evaluated locally without distributing caller definitions",
  paCustom["Summary"] === <|"PASS" -> 2|> &&
    StringContainsQ[paCustom["Execution"]["SchedulingReason"], "calling kernel"]];
paAutomatic = CheckAmplitudes[prH, prChannels, prFixtureModel,
  Sequence @@ paCheckOptions, "Parallelization" -> Automatic];
check["automatic scheduling keeps small checks local",
  StringContainsQ[paAutomatic["Execution"]["SchedulingReason"], "small"] &&
    paAutomatic["Summary"] === <|"PASS" -> 8|>];
paFailureJob = <|"Group" -> "Physical", "Kind" -> "AutomaticResidue",
  "FailureBase" -> <|"Cut" -> {1, 3}, "Mass" -> prElectronMass|>|>;
paFailure = ConstructiveDiagrams`Private`amplitudeCheckJobResult[
  paFailureJob, Failure["WorkerAborted", <||>]];
check["an interrupted job remains an explicit unresolved obligation",
  paFailure["Status"] === "UNRESOLVED" && paFailure["Cut"] === {1, 3} &&
    !FreeQ[paFailure["Diagnostics"], Failure["WorkerAborted", _]]];
paSpuriousFailure = ConstructiveDiagrams`Private`amplitudeCheckJobResult[
  <|"Group" -> "Spurious", "FailureBase" -> <|"Pole" -> prAngle[3, 4]|>|>, $Failed];
check["failed spurious workers retain their pole rather than disappearing",
  paSpuriousFailure["Status"] === "UNRESOLVED" &&
    paSpuriousFailure["Pole"] === prAngle[3, 4]];
Do[check["invalid parallel option is rejected " <> ToString[bad, InputForm],
  Quiet[CheckAmplitudes[prH, prChannels, prFixtureModel, bad]] === $Failed],
  {bad, {"Parallelization" -> "Sometimes", "Parallelization" -> 2,
    "MaxKernels" -> 0, "MaxKernels" -> -1, "MaxKernels" -> 3/2,
    "MaxKernels" -> Infinity}}];
Print[<|"Checks" -> checks, "Failed" -> Length[failures], "Failures" -> failures|>];
If[failures =!= {}, Quit[1]];
Print["HEPCAT_TEST_COMPLETED:parallel-amplitude-check-tests.wl"];
Quit[0];
