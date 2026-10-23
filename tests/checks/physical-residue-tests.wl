(* Exact QED five-point single-pole checks, not simultaneous diagram cuts. *)
root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Global`$HEPCATpath = FileNameJoin[{root, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
SetOptions[CheckPhysicalResidues, "HighEnergyCheck" -> False, "ReferenceCheck" -> False];
If[DownValues[PhysicalPoleResidue] === {} || DownValues[CheckPhysicalResidues] === {},
  Print["Physical-residue APIs did not load."]; Quit[1]];
Get[FileNameJoin[{root, "tests", "QED-5-point-residues.wl"}]];
checks = 0; failures = {};
check[name_, value_] := (checks++; If[!TrueQ[value], AppendTo[failures, name];
  Print["FAIL: ", name]]);
prAdaptedTargets[particles_, model_] := ConstructiveDiagrams`Private`residueSimpleInputs[
  0, FactorizationAmplitudeChannels[particles, model], model]["Targets"];
prDetailedReport[expression_, targets_List, opts : OptionsPattern[CheckPhysicalResidues]] :=
  ConstructiveDiagrams`Private`residueCheckReport[expression, targets, {}, opts];
prDetailedReport[expression_, records_List, targets_List,
    opts : OptionsPattern[CheckPhysicalResidues]] :=
  ConstructiveDiagrams`Private`residueCheckReport[expression, targets, records, opts];

check["diagram vertices come from the existing cubic QED model",
  prModel === ReadModel[FileNameJoin[{root, "models"}], 2]];
check["five-point factorization uses the existing QED model with four-point amplitudes",
  prFixtureModel === ReadModel[FileNameJoin[{root, "models"}], 3] &&
    MemberQ[prFixtureModel[[2]], prA4Row]];
check["four-point factorization uses only the cubic QED model",
  prFourPointChannels === FactorizationAmplitudeChannels[
    {"e", "E", "A.+", "A.+"}, prModel]];
check["six independently sewn electron channels are available",
  Length[prElectronTargets] === 6];
check["electron targets have complete lower amplitudes",
  AllTrue[prElectronTargets, TrueQ[#["Complete"]] &]];
prCubicOnlyChannels = FactorizationAmplitudeChannels[prParticles, prModel];
check["absent higher-point amplitudes contribute no original factorization rows",
  prCubicOnlyChannels === {}];
prCubicOnlyReport = CheckPhysicalResidues[
  {{Thread[(Mass /@ Range[5]) -> {Me, Me, 0, 0, 0}], prH}},
  prCubicOnlyChannels, prModel,
  "Output" -> "Details", "Samples" -> 1,
  "ParameterRules" -> {Me -> 1, EE -> 1}, "TimeConstraint" -> 20];
check["poles absent from supplied factorization data are exposed as spurious failures",
  prCubicOnlyReport["Channels"] === {} &&
    prCubicOnlyReport["PhysicalSummary"] === <||> &&
    prCubicOnlyReport["SpuriousSummary"] === <|"FAIL" -> 6|>];
check["the original factorization output remains compact numerical-mass rows",
  Length[prChannels] === 6 && AllTrue[prChannels,
    ListQ[#] && Length[#] === 8 && MatchQ[#[[-3]], _Multiparticle] &&
      !AssociationQ[#] && !FreeQ[Last[#], _PropDen] &]];
check["the user's factorization function has no alternate public output mode",
  Options[FactorizationAmplitudeChannels] === {}];

check["two four-point electron channels have complete A3 times A3 targets",
  Length[prFourPointTargets] === 2 &&
    AllTrue[prFourPointTargets, TrueQ[#["Complete"]] &]];
Do[
  prFourPointResidue = PhysicalPoleResidue[prFourPointAmplitude, target,
    "KinematicMap" -> prFourPointMap[i, j], "KinematicKind" -> "Sample",
    "UncutChannels" -> Select[prFourPointTargets, #["Cut"] =!= target["Cut"] &],
    "TimeConstraint" -> 15];
  prFourPointExpected = Module[{q}, Cancel[Together[
      prMapOnChart[target["StateSum"], prFourPointChart[target["Cut"], q, i, j]]]] /. q -> 0];
  check["independent QED4 sewing phase and normalization " <>
    ToString[{target["Cut"], i, j}, InputForm],
    prFourPointResidue["Status"] === "PASS" &&
      prZero[prFourPointResidue["PhysicalResidue"] - prFourPointExpected]],
  {target, prFourPointTargets}, {i, 2}, {j, 2}
];

(* Stored A4 rows and diagrams are separate inputs to the same cubic-channel
   test. Agreement here fixes each model-row sign without replacing its formula. *)
prStoredFourPointRows = Select[prFixtureModel[[2]],
  Length[DeleteCases[Drop[#, -2], ""]] === 4 &];
check["all four stored QED four-point amplitudes are covered",
  Length[prStoredFourPointRows] === 4];
Do[
  prStoredParticles = DeleteCases[Drop[row, -2], ""];
  prStoredChannels = FactorizationAmplitudeChannels[prStoredParticles, prModel];
  prStoredAmplitude = Times @@ Take[row, -2];
  prStoredDiagramRecords = DiagramAmplitudeRecords[
    CreateDiagrams[prStoredParticles, prModel]];
  prStoredDiagramAmplitude = prPreparedDiagramSum[prStoredDiagramRecords];
  prStoredReport = CheckPhysicalResidues[prStoredAmplitude,
    prStoredChannels, prModel, "Output" -> "Details", "Samples" -> 2,
    "ParameterRules" -> {Me -> 1, EE -> 1}, "TimeConstraint" -> 30];
  prStoredDiagramReport = CheckPhysicalResidues[prStoredDiagramAmplitude,
    prStoredChannels, prModel, "Output" -> "Details", "Samples" -> 2,
    "ParameterRules" -> {Me -> 1, EE -> 1}, "TimeConstraint" -> 30];
  prStoredLabel = ToString[prStoredParticles, InputForm];
  check["stored QED4 amplitude agrees with cubic sewing " <> prStoredLabel,
    Length[prStoredChannels] === 2 && prStoredReport["PhysicalSummary"] === <|"PASS" -> 2|>];
  check["independent cubic diagrams agree with the same QED4 sewing " <> prStoredLabel,
    Length[prStoredDiagramRecords] === 2 &&
      prStoredDiagramReport["PhysicalSummary"] === <|"PASS" -> 2|>];
  check["stored and diagram QED4 sampled residues agree " <> prStoredLabel,
    Lookup[Flatten[Lookup[prStoredReport["Channels"], "Samples"]], "CalculatedResidue"] ===
      Lookup[Flatten[Lookup[prStoredDiagramReport["Channels"], "Samples"]],
        "CalculatedResidue"] &&
      AnyTrue[Flatten[Lookup[prStoredReport["Channels"], "Samples"]],
        #["ExpectedResidue"] =!= 0 &]],
  {row, prStoredFourPointRows}
];

prSavedChannels = prChannels;
prPublicDetails = CheckPhysicalResidues[prH, prChannels, prFixtureModel,
  "Output" -> "Details", "KinematicMap" -> prMap[1, 2],
  "KinematicKind" -> "Sample", "TimeConstraint" -> 15];
check["the simple three-input public checker passes all six QED poles",
  prPublicDetails["Summary"] === <|"PASS" -> 6|>];
prPublicTable = CheckPhysicalResidues[prH, prChannels, prFixtureModel,
  "KinematicMap" -> prMap[1, 2], "TimeConstraint" -> 15];
check["the default public result is a three-column table rather than nested diagnostics",
  Head[prPublicTable] === Dataset && Length[Normal[prPublicTable]] === 6 &&
    AllTrue[Normal[prPublicTable],
      Sort[Keys[#]] === Sort[{"Type", "Pole", "Status"}] && #["Type"] === "Physical" &]];
prPublicResidues = CheckPhysicalResidues[prH, prChannels, prFixtureModel,
  "Output" -> "Residues", "KinematicMap" -> prMap[1, 2], "TimeConstraint" -> 15];
check["residue values appear only when the user asks for them",
  Head[prPublicResidues] === Dataset && AllTrue[Normal[prPublicResidues],
    ContainsAll[Keys[#], {"Type", "Pole", "Calculated", "Expected", "Difference"}] &]];
check["supplied-map residue display retains one numbered row per channel",
  Length[Normal[prPublicResidues]] === 6 &&
    Lookup[Normal[prPublicResidues], "Sample"] === ConstantArray[1, 6]];
prResidueValueKeys = {"Calculated", "Expected", "Difference"};
prUnifiedResidueValues[row_Association] := AssociationThread[prResidueValueKeys,
  Lookup[row, {"CalculatedResidue", "ExpectedResidue", "Difference"}]];
check["supplied-map displayed residue values remain exact and match the details",
  (KeyTake[#, prResidueValueKeys] & /@ Normal[prPublicResidues]) ===
    (prUnifiedResidueValues /@ prPublicDetails["Channels"])];
prAutomaticResidueDetails = CheckPhysicalResidues[prH, prChannels, prFixtureModel,
  "Output" -> "Details", "Samples" -> 2, "RandomSeed" -> 1729,
  "ParameterRules" -> {Me -> 1, EE -> 1}, "TimeConstraint" -> 20];
prAutomaticResidueDisplay = CheckPhysicalResidues[prH, prChannels, prFixtureModel,
  "Output" -> "Residues", "Samples" -> 2, "RandomSeed" -> 1729,
  "ParameterRules" -> {Me -> 1, EE -> 1}, "TimeConstraint" -> 20];
prAutomaticResidueRows = Normal[prAutomaticResidueDisplay];
check["automatic residue display shows every cut sample as a separate row",
  Head[prAutomaticResidueDisplay] === Dataset && Length[prAutomaticResidueRows] === 12 &&
    AllTrue[GatherBy[prAutomaticResidueRows, {#["Type"], #["Pole"]} &],
      Lookup[#, "Sample"] === {1, 2} &]];
check["automatic residue display has scalar arithmetic cells rather than hidden sample lists",
  AllTrue[prAutomaticResidueRows,
    AllTrue[Lookup[#, prResidueValueKeys], !ListQ[#] &] &]];
check["automatic displayed residue values are the unrounded exact details samples",
  (KeyTake[#, prResidueValueKeys] & /@ prAutomaticResidueRows) ===
    (prUnifiedResidueValues /@
      Flatten[Lookup[prAutomaticResidueDetails["Channels"], "Samples"], 1]) &&
    FreeQ[Lookup[prAutomaticResidueRows, prResidueValueKeys], _Real] &&
    AnyTrue[Lookup[prAutomaticResidueRows, "Expected"], # =!= 0 &]];
check["expanded residue display shows all rows and columns by default",
  MemberQ[Options[prAutomaticResidueDisplay], MaxItems -> {All, All}] &&
    MemberQ[Options[prPublicResidues], MaxItems -> {All, All}]];
prPhysicalOnlyTable = CheckPhysicalResidues[prH, prChannels, prFixtureModel,
  "SpuriousPoles" -> {}, "KinematicMap" -> prMap[1, 2], "TimeConstraint" -> 15];
prPhysicalOnlyResidues = CheckPhysicalResidues[prH, prChannels, prFixtureModel,
  "SpuriousPoles" -> {}, "Output" -> "Residues",
  "KinematicMap" -> prMap[1, 2], "TimeConstraint" -> 15];
check["the explicit physical-only option preserves the legacy compact columns",
  Head[prPhysicalOnlyTable] === Dataset && AllTrue[Normal[prPhysicalOnlyTable],
    Sort[Keys[#]] === Sort[{"Cut", "Mass", "Status"}] &]];
check["the explicit physical-only option preserves legacy residue names",
  Head[prPhysicalOnlyResidues] === Dataset && AllTrue[Normal[prPhysicalOnlyResidues],
    ContainsAll[Keys[#], {"Cut", "Mass", "CalculatedResidue", "ExpectedResidue", "Difference"}] &]];
check["summary output returns only status counts",
  CheckPhysicalResidues[prH, prChannels, prFixtureModel,
    "Output" -> "Summary", "KinematicMap" -> prMap[1, 2], "TimeConstraint" -> 15] ===
      <|"PASS" -> 6|>];
prAmplitudePair = {Thread[(Mass /@ Range[5]) -> {Me, Me, 0, 0, 0}], prH};
prPairsReport = CheckPhysicalResidues[{prAmplitudePair}, prChannels, prFixtureModel,
  "Output" -> "Details", "KinematicMap" -> prMap[1, 2], "TimeConstraint" -> 15];
check["ordinary diagram mass-rule expression pairs can be passed directly",
  prPairsReport["Summary"] === prPublicDetails["Summary"]];
check["the public comparison does not mutate supplied compact channels",
  prChannels === prSavedChannels];
check["the checker removes the contour variable and applies the diagram sewing phase",
  AllTrue[prPublicDetails["FactorizationChannels"],
    #["SewingFactor"] === -1 &&
      Together[#["StateSum"] + ConstructiveDiagrams`z #["Propagator"]
        #["ChannelExpression"]] === 0 &]];
prOppositePublicDetails = CheckPhysicalResidues[-prH, prChannels, prFixtureModel,
  "Output" -> "Details", "KinematicMap" -> prMap[1, 2],
  "KinematicKind" -> "Sample", "TimeConstraint" -> 15];
check["opposite QED residues are reported separately from generic failures",
  prOppositePublicDetails["Summary"] === <|"Opp Sign" -> 6|>];
check["opposite-sign details retain the unmodified expected residue and both differences",
  AllTrue[prOppositePublicDetails["Channels"],
    #["OppositeSignDifference"] === 0 &&
      Together[#["CalculatedResidue"] + #["ExpectedResidue"]] === 0 &&
      Together[#["Difference"] -
        (#["CalculatedResidue"] - #["ExpectedResidue"])] === 0 &] &&
    AnyTrue[prOppositePublicDetails["Channels"], #["Difference"] =!= 0 &]];
check["summary output includes the opposite-sign count",
  CheckPhysicalResidues[-prH, prChannels, prFixtureModel,
    "Output" -> "Summary", "KinematicMap" -> prMap[1, 2],
    "TimeConstraint" -> 15] === <|"Opp Sign" -> 6|>];
prCustomChannels = {ReplacePart[First[prChannels], -1 -> 2 Last[First[prChannels]] ]};
prCustomReport = CheckPhysicalResidues[prH, prCustomChannels, prFixtureModel,
  "Output" -> "Details", "KinematicMap" -> prMap[1, 2], "TimeConstraint" -> 15];
check["the checker uses the user's supplied channel expression rather than rebuilding amplitudes",
  AnyTrue[prCustomReport["Channels"], #["Status"] === "FAIL" &]];
Do[
  check["malformed public input is rejected " <> ToString[bad, InputForm],
    Quiet[CheckPhysicalResidues[prH, bad, prFixtureModel]] === $Failed],
  {bad, {{None}, {<|"Cut" -> {1, 3}, "Mass" -> Me|>},
    {ReplacePart[First[prChannels], -3 -> Multiparticle[1, 99]]},
    {ReplacePart[First[prChannels],
      -1 -> Last[First[prChannels]]/ConstructiveDiagrams`z]}}}
];
check["unknown public detail levels are rejected",
  Quiet[CheckPhysicalResidues[prH, prChannels, prFixtureModel,
    "Output" -> "Everything"]] === $Failed];
prUnknownExchangeReport = CheckPhysicalResidues[
  {{First[prAmplitudePair],
    1/PropDen[Mom[Multiparticle[1, 3]], prUnlistedExchangeMass]}},
  {}, prFixtureModel, "Output" -> "Details", "Samples" -> 1];
check["a candidate propagator with no exchange mass in the model cannot pass as zero",
  prUnknownExchangeReport["Summary"] === <|"UNRESOLVED" -> 1|>];

prRows = {};
Do[
  prExpected = prSewnValue[target, i, j];
  prCompact = prResidue[prH, target, i, j];
  prSpurious = prResidue[prS2, target, i, j];
  prOld = prResidue[prOldPermutation, target, i, j];
  prDoubled = prResidue[2 prH, target, i, j];
  prLabel = ToString[{target["Cut"], i, j}, InputForm];
  check["sewn physical residue is evaluated " <> prLabel,
    prExpected["Status"] === "PASS"];
  check["compact physical residue is evaluated " <> prLabel,
    prCompact["Status"] === "PASS"];
  check["spurious form physical residue is evaluated " <> prLabel,
    prSpurious["Status"] === "PASS"];
  check["H matches independently sewn A3 times A4 " <> prLabel,
    prZero[prResidueValue[prCompact] - prResidueValue[prExpected]]];
  check["S2 matches independently sewn A3 times A4 " <> prLabel,
    prZero[prResidueValue[prSpurious] - prResidueValue[prExpected]]];
  AppendTo[prRows, <|"Cut" -> target["Cut"], "Spins" -> {i, j},
    "Expected" -> prResidueValue[prExpected],
    "H" -> prResidueValue[prCompact], "S2" -> prResidueValue[prSpurious],
    "Old" -> prResidueValue[prOld], "Doubled" -> prResidueValue[prDoubled]|>],
  {target, prElectronTargets}, {i, 2}, {j, 2}
];
check["all six channels and four spin components were tested", Length[prRows] === 24];
check["nonzero residues fix the overall normalization",
  AnyTrue[prRows, !prZero[#["Expected"]] &]];
check["old permutation fails a physical second-endpoint pole",
  AnyTrue[prRows, MemberQ[#["Cut"], 2] && !prZero[#["Old"] - #["Expected"]] &]];
check["old permutation still passes first-endpoint poles",
  AllTrue[Select[prRows, MemberQ[#["Cut"], 1] &],
    prZero[#["Old"] - #["Expected"]] &]];
check["doubling the compact amplitude fails a nonzero physical residue",
  AnyTrue[prRows, !prZero[#["Doubled"] - #["Expected"]] &]];
prDiagrams = TimeConstrained[CreateDiagrams[prParticles, prModel], 20, $Failed];
check["pilot uses the six actual cubic QED diagrams",
  ListQ[prDiagrams] && Length[prDiagrams] === 6];
If[ListQ[prDiagrams] && Length[prDiagrams] === 6,
  prRecords = DiagramAmplitudeRecords[prDiagrams];
  check["diagram records retain original off-shell physical denominators",
    Length[prRecords] === 6 && AllTrue[prRecords,
      TrueQ[#["DenominatorsKeptOffShell"]] &&
        Length[#["PhysicalPropagators"]] === 2 &]];
  prReport = prDetailedReport[prH, prRecords, prElectronTargets,
    "KinematicMap" -> prMap[1, 2], "KinematicKind" -> "Sample",
    "TimeConstraint" -> 15];
  check["detailed engine compares all six H residues on the supplied exact charts",
    Length[prReport["Channels"]] === 6 &&
      AllTrue[prReport["Channels"], #["Status"] === "PASS" &]];
  prTwoInputReport = prDetailedReport[prH, prElectronTargets,
    "KinematicMap" -> prMap[1, 2], "KinematicKind" -> "Sample",
    "TimeConstraint" -> 15];
  check["the detailed engine needs no diagram records",
    Length[prTwoInputReport["Channels"]] === 6 &&
      AllTrue[prTwoInputReport["Channels"], #["Status"] === "PASS" &]];
  check["optional details retain the supplied channel and comparison expressions",
    AllTrue[prTwoInputReport["Channels"], AssociationQ[#] &&
      ContainsAll[Keys[#], {"Cut", "Complement", "Propagator", "CutCondition",
        "FactorizationChannels", "ExpectedStateSum",
        "CalculatedResidue", "ExpectedResidue", "Difference"}] &&
      #["FactorizationChannels"] =!= {} &]];
  prOldReport = prDetailedReport[prOldPermutation, prRecords, prElectronTargets,
    "KinematicMap" -> prMap[1, 2], "KinematicKind" -> "Sample",
    "TimeConstraint" -> 15];
  check["public checker reports the old permutation's genuine residue failure",
    AnyTrue[prOldReport["Channels"], #["Status"] === "FAIL" &]];
  prActualReport = prDetailedReport[prPreparedDiagramSum[prRecords],
    prRecords, prElectronTargets, "KinematicMap" -> prMap[1, 2],
    "KinematicKind" -> "Sample", "TimeConstraint" -> 15];
  check["actual diagrams pass with consistent references, spin contractions and their own mass rules",
    Length[prActualReport["Channels"]] === 6 &&
      AllTrue[prActualReport["Channels"], #["Status"] === "PASS" &]]
];
If[prElectronTargets =!= {},
  prFormal = PhysicalPoleResidue[prH, First[prElectronTargets]];
  check["opaque structural extraction is not a physical pass",
    prFormal["Status"] === "FORMAL"]
];
(* Generic extraction and guard rails, independent of the QED comparison. *)
prTestTarget = First[Select[prElectronTargets, #["Cut"] === {1, 3} &]];
prOtherTarget = First[Select[prElectronTargets, #["Cut"] === {2, 5} &]];
prD13 = prTestTarget["Propagator"];
prD25 = prOtherTarget["Propagator"];
prComplementDen = PropDen[Mom[Multiparticle[2, 4, 5]], Me];
check["complementary physical channels share one formal pole",
  PhysicalPoleResidue[4/prD13 + 5/prComplementDen + 7, prTestTarget][
    "StructuralCoefficient"] === 9];
check["signed momentum sums identify the same pole",
  PhysicalPoleResidue[3/PropDen[-Mom[1] - Mom[3], Me], prTestTarget][
    "StructuralCoefficient"] === 3];
check["different exchange masses are not merged",
  PhysicalPoleResidue[1/PropDen[Mom[Multiparticle[1, 3]], Me + 1], prTestTarget][
    "StructuralCoefficient"] === 0];
check["the full sum is canceled before extracting a residue",
  PhysicalPoleResidue[1/prD13^2 - 1/prComplementDen^2 + 3/prD13,
    prTestTarget]["StructuralCoefficient"] === 3];
check["uncanceled higher-order poles remain unresolved",
  PhysicalPoleResidue[1/prD13^2, prTestTarget,
    "KinematicMap" -> prMap[1, 2]]["Status"] === "UNRESOLVED"];
prCancelHidden = PhysicalPoleResidue[prMixed[3, 1, 3]/prD13^2, prTestTarget,
  "KinematicMap" -> prMap[1, 2]];
check["a numerator cut factor can reduce an apparent double pole",
  MissingQ[prCancelHidden["StructuralCoefficient"]] && prCancelHidden["Status"] === "PASS" &&
    prCancelHidden["PhysicalResidue"] === 1];
check["formal compatible double-pole coefficient is extracted",
  PhysicalPoleResidue[alpha/(prD13 prD25) + beta/prD13,
    {prTestTarget, prOtherTarget}]["StructuralCoefficient"] === alpha];
prHiddenPole = PhysicalPoleResidue[1/prMixed[3, 1, 3], prTestTarget,
  "KinematicMap" -> prMap[1, 2], "UncutChannels" -> prUncutChannels[prTestTarget]];
check["kinematic mapping discovers poles hidden in a spinor denominator",
  prHiddenPole["StructuralCoefficient"] === 0 &&
    prHiddenPole["Status"] === "PASS" && prHiddenPole["PhysicalResidue"] === 1];
check["unmapped Mandelstahm invariants cannot yield a physical pass",
  PhysicalPoleResidue[1/(Mandelstahm[1, 3] - Me^2), prTestTarget,
    "KinematicMap" -> prMap[1, 2]]["Status"] === "UNRESOLVED"];
Do[
  prErrorMap[expr_, cuts_, vars_] := If[expr === prUnknownAmplitude, error,
    prMap[1, 2][expr, cuts, vars]];
  check["kinematic error objects cannot become a zero residue " <> ToString[error, InputForm],
    PhysicalPoleResidue[prUnknownAmplitude, prTestTarget,
      "KinematicMap" -> prErrorMap]["Status"] === "UNRESOLVED"],
  {error, {Missing["NotImplemented"], Failure["NotImplemented", <||>],
    ConditionalExpression[1, prUnknownCondition], $Failed, $TimedOut}}
];
prInconsistentMap[expr_, cuts_, vars_] := If[MatchQ[expr, Mom[_Integer]],
  ConstantArray[0, {2, 2}], prMap[1, 2][expr, cuts, vars]];
check["momentum matrices independently validate callback shell assertions",
  PhysicalPoleResidue[1/prD13, prTestTarget,
    "KinematicMap" -> prInconsistentMap]["Status"] === "UNRESOLVED"];
prExtraCut = First[Select[prElectronTargets, #["Cut"] === {1, 4} &]];
prExtraCutMap[expr_, cuts_, vars_] := If[expr === prExtraCut["Propagator"], 0,
  prMap[1, 2][expr, cuts, vars]];
prOneRecord = Join[First[prRecords], <|"PhysicalPropagators" -> {prTestTarget}|>];
prExtraReport = prDetailedReport[prH, {prOneRecord}, {prTestTarget},
  "KinematicMap" -> prExtraCutMap, "UncutChannels" -> {prExtraCut}];
check["caller-supplied uncut channels are validated by the checker",
  First[prExtraReport["Channels"]]["Status"] === "UNRESOLVED"];
prFormalReport = ConstructiveDiagrams`Private`residueCheckChannel[
  prH, prTestTarget, prRecords, prElectronTargets, prElectronTargets,
  Automatic, "Sample", 15, 10^-10, {}];
check["a low-level formal comparison is never labeled a physical pass",
  AssociationQ[prFormalReport] && prFormalReport["Status"] === "FORMAL"];
prMissingReport = ConstructiveDiagrams`Private`residueCheckReport[prH, {}, prRecords];
check["missing target report rows remain unresolved associations",
  Length[prMissingReport["Channels"]] === 6 && AllTrue[prMissingReport["Channels"],
    AssociationQ[#] && #["Status"] === "UNRESOLVED" &]];
prWrongScope = Join[prTestTarget, <|"ExternalLegs" -> Range[4],
  "Complement" -> {2, 4}, "ExternalMassRules" -> Most[prTestTarget["ExternalMassRules"]]|>];
check["targets from a different number of external legs are rejected",
  Quiet[prDetailedReport[prH, prRecords, {prWrongScope}]] === $Failed];
prWrongMass = Join[prTestTarget, <|"ExternalMassRules" ->
  (prTestTarget["ExternalMassRules"] /. (Mass[1] -> _) -> (Mass[1] -> 2))|>];
check["targets with inconsistent external masses are rejected",
  Quiet[prDetailedReport[prH, prRecords, {prWrongMass}]] === $Failed];
prWrongProcess = Join[prTestTarget, <|"ExternalParticles" -> {"u", "U", "A.+", "A.+", "A.+"}|>];
check["different processes are not matched merely because their masses coincide",
  Quiet[prDetailedReport[prH, prRecords, {prWrongProcess}]] === $Failed];
check["malformed record channels are rejected without indexing failures",
  Quiet[prDetailedReport[prH, {<|"PhysicalPropagators" -> {None}|>},
    {prTestTarget}]] === $Failed];
prSlowMap[expr_, cuts_, vars_] := (Pause[0.1]; prMap[1, 2][expr, cuts, vars]);
check["chart evaluation has a time bound",
  PhysicalPoleResidue[prH, prTestTarget, "KinematicMap" -> prSlowMap,
    "TimeConstraint" -> 0.01]["Status"] === "UNRESOLVED"];
prAuxDen = PropDen[Mom[Multiparticle[3, 4]], 0];
prAugmentedRecord = First[DiagramAmplitudeRecords[{First[prDiagrams]},
  "SimplificationFunction" -> Function[pair, {First[pair], Last[pair]/prAuxDen}]]];
check["auxiliary denominators do not create graph-derived physical channels",
  Length[prAugmentedRecord["PhysicalPropagators"]] === 2 &&
    prAugmentedRecord["OriginalExpression"] === First[prRecords]["OriginalExpression"] &&
    prAugmentedRecord["DeclaredSimplificationOnShellChannels"] === {}];
prProtectedRecord = First[DiagramAmplitudeRecords[{First[prDiagrams]},
  "SimplificationFunction" -> Function[pair,
    {First[pair], Last[pair] /. _PropDen -> 0}]]];
check["diagram simplification callbacks cannot put protected propagators on shell",
  prProtectedRecord["Expression"] === prProtectedRecord["OriginalExpression"]];
prBadRecord = First[DiagramAmplitudeRecords[{First[prDiagrams]},
  "SimplificationFunction" -> Function[pair, {{}, Last[pair]}]]];
check["a simplification cannot silently change mass provenance", FailureQ[prBadRecord]];

prScalarModel = {{{"Scalar", "s", "s", 1, 0, scalarMass, 0, 1, 1, "", ""}},
  {{"s", "s", "s", "", cubicCoupling, 1}}, {}, {}};
prScalarChannels = FactorizationAmplitudeChannels[ConstantArray["s", 4], prScalarModel];
prScalarTargets = ConstructiveDiagrams`Private`residueSimpleInputs[
  0, prScalarChannels, prScalarModel]["Targets"];
prScalarDiagrams = CreateDiagrams[ConstantArray["s", 4], prScalarModel];
check["cubic scalar channels include the negative physical sewing phase",
  Length[prScalarTargets] === 3 && AllTrue[prScalarTargets,
    TrueQ[#["Complete"]] && #["StateSum"] === -cubicCoupling^2 &]];
check["direct scalar diagrams retain their independently generated negative numerator",
  Length[prScalarDiagrams] === 3 && AllTrue[prScalarDiagrams,
    Cancel[diagramAmplitude[#] First[Values[#["Propagators"]]]["Factor"]^-1 I] ===
      -cubicCoupling^2 &]];
prScalarChannelAmplitude = Total[
  #["StateSum"]/#["Propagator"] & /@ prScalarTargets];
prScalarPassReport = CheckPhysicalResidues[prScalarChannelAmplitude,
  prScalarChannels, prScalarModel, "Output" -> "Details", "Samples" -> 3,
  "ParameterRules" -> {cubicCoupling -> 2, scalarMass -> 1}, "TimeConstraint" -> 15];
check["automatic scalar comparisons agree in the physical diagram convention",
  prScalarPassReport["Summary"] === <|"PASS" -> 3|>];
prScalarDiagramAmplitude = Total[
  (ReduceSpinContractions[Last[#]] /. First[#]) & /@
    diagramAmplitudes[prScalarDiagrams]];
prScalarDiagramReport = CheckPhysicalResidues[prScalarDiagramAmplitude,
  prScalarChannels, prScalarModel, "Output" -> "Summary", "Samples" -> 2,
  "ParameterRules" -> {cubicCoupling -> 2, scalarMass -> 1}, "TimeConstraint" -> 15];
check["independently generated scalar diagrams agree with physical sewing",
  prScalarDiagramReport === <|"PASS" -> 3|>];
prScalarSignReport = CheckPhysicalResidues[-prScalarChannelAmplitude,
  prScalarChannels, prScalarModel, "Output" -> "Details", "Samples" -> 3,
  "ParameterRules" -> {cubicCoupling -> 2, scalarMass -> 1}, "TimeConstraint" -> 15];
check["an intentionally negated scalar amplitude reports the opposite sign",
  prScalarSignReport["Summary"] === <|"Opp Sign" -> 3|> &&
    AllTrue[prScalarSignReport["Channels"],
      #["CompletedSamples"] === 3 && AllTrue[#["Samples"],
        #["Status"] === "Opp Sign" && #["OppositeSignDifference"] === 0 &] &]];
prZeroScalarChannels = ReplacePart[#, -1 -> 0] & /@ prScalarChannels;
prZeroScalarReport = CheckPhysicalResidues[0, prZeroScalarChannels, prScalarModel,
  "Output" -> "Details", "Samples" -> 3,
  "ParameterRules" -> {scalarMass -> 1}, "TimeConstraint" -> 15];
check["zero-only sampled residues pass rather than claiming an opposite sign",
  prZeroScalarReport["Summary"] === <|"PASS" -> 3|> &&
    AllTrue[prZeroScalarReport["Channels"], AllTrue[#["Samples"],
      #["CalculatedResidue"] === 0 && #["ExpectedResidue"] === 0 &] &]];

(* Exercise cross-sample sign consistency independently of random chart selection. *)
prSignSample[calculated_, expected_, signs_List] := <|
  "Status" -> Which[MemberQ[signs, 1], "PASS", MemberQ[signs, -1], "Opp Sign",
    True, "FAIL"], "CalculatedResidue" -> calculated, "ExpectedResidue" -> expected,
  "Difference" -> calculated - expected, "OppositeSignDifference" -> calculated + expected,
  "MatchingSigns" -> signs, "Diagnostics" -> {}|>;
prMockAutomaticSignReport[samples_List] := Module[{nextSample = 0},
  Block[{ConstructiveDiagrams`Private`residueAutomaticChart,
      ConstructiveDiagrams`Private`residueCheckChannel},
    ConstructiveDiagrams`Private`residueAutomaticChart[___] := <|
      "Map" -> Identity, "Sample" -> 1, "Chart" -> <||>, "ParameterRules" -> {}|>;
    ConstructiveDiagrams`Private`residueCheckChannel[___] :=
      samples[[Min[++nextSample, Length[samples]]]];
    ConstructiveDiagrams`Private`residueAutomaticComparison[0,
      First[prScalarTargets], {}, prScalarTargets, prScalarTargets,
      15, 10^-10, {}, Length[samples], 1729, {scalarMass -> 1}]
  ]
];
prSameSample = prSignSample[2, 2, {1}];
prOppositeSample = prSignSample[-2, 2, {-1}];
prZeroSample = prSignSample[0, 0, {1, -1}];
prMismatchSample = prSignSample[3, 2, {}];
check["matching nonzero samples plus zero samples pass",
  prMockAutomaticSignReport[{prZeroSample, prSameSample}]["Status"] === "PASS"];
check["opposite nonzero samples plus zero samples retain the opposite-sign result",
  prMockAutomaticSignReport[{prZeroSample, prOppositeSample}]["Status"] === "Opp Sign"];
check["mixed nonzero matching and opposite signs fail rather than claiming a global sign",
  prMockAutomaticSignReport[{prSameSample, prOppositeSample}]["Status"] === "FAIL"];
check["a generic mismatch remains a failure alongside opposite-sign samples",
  prMockAutomaticSignReport[{prOppositeSample, prMismatchSample}]["Status"] === "FAIL"];
check["all-zero samples do not determine a negative overall sign",
  prMockAutomaticSignReport[{prZeroSample, prZeroSample}]["Status"] === "PASS"];
prUnresolvedSample = Join[prOppositeSample,
  <|"Status" -> "UNRESOLVED", "MatchingSigns" -> {},
    "Diagnostics" -> {"A sample could not be resolved."}|>];
check["unresolved samples cannot complete an otherwise opposite-sign comparison",
  prMockAutomaticSignReport[{prOppositeSample, prUnresolvedSample}]["Status"] === "UNRESOLVED"];
prIncompleteSignReport = Block[
  {ConstructiveDiagrams`Private`residueAutomaticChart,
   ConstructiveDiagrams`Private`residueCheckChannel, prSignChartCount = 0},
  ConstructiveDiagrams`Private`residueAutomaticChart[___] := (
    If[++prSignChartCount > 1, Pause[2]];
    <|"Map" -> Identity, "Sample" -> 1, "Chart" -> <||>, "ParameterRules" -> {}|>);
  ConstructiveDiagrams`Private`residueCheckChannel[___] := prOppositeSample;
  ConstructiveDiagrams`Private`residueAutomaticComparison[0,
    First[prScalarTargets], {}, prScalarTargets, prScalarTargets,
    0.2, 10^-10, {}, 2, 1729, {scalarMass -> 1}]
];
check["one opposite-sign sample followed by a sampling timeout remains unresolved",
  prIncompleteSignReport["Status"] === "UNRESOLVED" &&
    prIncompleteSignReport["CompletedSamples"] === 1 &&
    First[prIncompleteSignReport["Samples"]]["Status"] === "Opp Sign"];

prTwoMassModel = {
  {{"Scalar", "s", "s", 1, 0, externalScalarMass, 0, 1, 1, "", ""},
   {"Scalar", "x", "x", 2, 0, firstExchangeMass, 0, 1, 1, "", ""},
   {"Scalar", "y", "y", 3, 0, secondExchangeMass, 0, 1, 1, "", ""}},
  {{"s", "s", "x", "", firstExchangeCoupling, Mass[3]},
   {"s", "s", "y", "", secondExchangeCoupling, Mass[3]}}, {}, {}};
prTwoMassChannels = FactorizationAmplitudeChannels[ConstantArray["s", 4], prTwoMassModel];
prTwoMassTargets = ConstructiveDiagrams`Private`residueSimpleInputs[
  0, prTwoMassChannels, prTwoMassModel]["Targets"];
check["factorization mass substitutions are local to each exchanged species",
  Length[prTwoMassTargets] === 6 && AllTrue[prTwoMassTargets,
    #["StateSum"] === If[#["Mass"] === firstExchangeMass,
      -firstExchangeCoupling^2 firstExchangeMass^2,
      -secondExchangeCoupling^2 secondExchangeMass^2] &]];
prSuppliedTargets = Select[prAdaptedTargets[prParticles, prFixtureModel],
  #["Mass"] === Me && Count[#["Cut"], 1 | 2] === 1 &&
    Count[#["Cut"], 3 | 4 | 5] === 1 &];
check["supplied higher-point amplitudes are trusted without completeness metadata",
  Length[prSuppliedTargets] === 6 &&
    AllTrue[prSuppliedTargets, TrueQ[#["Complete"]] &]];
check["malformed models are rejected rather than interpreted as absent amplitudes",
  Quiet[CheckPhysicalResidues[prH, prChannels,
    {prFixtureModel[[1]], None}]] === $Failed];

(* No QED particle names or fixed four/five-leg indexing enter this fixture.
   The complete higher-point rows are amplitude data, not diagram vertices. *)
prSixScalarModel = {{{"Scalar", "s", "s", 1, 0, 0, 0, 1, 1, "", ""}},
  {{"s", "s", "s", "", scalarCubic, 1},
   {"s", "s", "s", "s", scalarQuartic, 1},
   {"s", "s", "s", "s", "s", scalarQuintic, 1}}, {}, {}};
prSixScalarChannels = FactorizationAmplitudeChannels[ConstantArray["s", 6], prSixScalarModel];
prSixScalarTargets = ConstructiveDiagrams`Private`residueSimpleInputs[
  0, prSixScalarChannels, prSixScalarModel]["Targets"];
check["six-point factorization includes every two-three and three-three partition once",
  ListQ[prSixScalarTargets] && Length[prSixScalarTargets] === 25 &&
    Counts[Length /@ Lookup[prSixScalarTargets, "Cut"]] === <|2 -> 15, 3 -> 10|> &&
    AllTrue[prSixScalarTargets, #["ExternalLegs"] === Range[6] &&
      TrueQ[#["Complete"]] &]];
prSixScalarTarget = SelectFirst[prSixScalarTargets, #["Cut"] === {1, 2} &];
check["six-point complete lower amplitudes are actually sewn",
  prSixScalarTarget["StateSum"] === -scalarCubic scalarQuintic &&
    AllTrue[Select[prSixScalarTargets, Length[#["Cut"]] === 3 &],
      #["StateSum"] === -scalarQuartic^2 &]];
prSixScalarCubicModel = ReplacePart[prSixScalarModel, 2 -> Take[prSixScalarModel[[2]], 1]];
prSixScalarMissing = FactorizationAmplitudeChannels[ConstantArray["s", 6], prSixScalarCubicModel];
check["absent higher-point rows produce no compact six-point channels",
  prSixScalarMissing === {}];
prSixMissingReport = CheckPhysicalResidues[0, prSixScalarMissing,
  prSixScalarCubicModel, "Output" -> "Details", "Samples" -> 1, "TimeConstraint" -> 5];
check["empty factorization data is explicitly reported as no comparisons",
  prSixMissingReport["Channels"] === {} && prSixMissingReport["Summary"] === <||> &&
    prSixMissingReport["Diagnostics"] =!= {}];

(* A generic single-cut API check uses automatic kinematics, not the five-point
   QED chart. A lone pole with its sewn constant numerator has known residue. *)
prSixScalarReport = prDetailedReport[
  prSixScalarTarget["StateSum"]/prSixScalarTarget["Propagator"],
  {prSixScalarTarget}, "Samples" -> 2,
  "ParameterRules" -> {scalarCubic -> 2, scalarQuartic -> 3, scalarQuintic -> 5},
  "TimeConstraint" -> 15];
check["automatic physical residue checks work at six points without diagram records",
  Length[prSixScalarReport["Channels"]] === 1 &&
    First[prSixScalarReport["Channels"]]["Status"] === "PASS"];
prSixScalarAmplitude = Total[
  #["StateSum"]/#["Propagator"] & /@ prSixScalarTargets];
prSixScalarFullReport = prDetailedReport[prSixScalarAmplitude,
  prSixScalarTargets, "Samples" -> 2,
  "ParameterRules" -> {scalarCubic -> 2, scalarQuartic -> 3, scalarQuintic -> 5},
  "TimeConstraint" -> 15];
check["complete six-point report checks all twenty-five physical scalar cuts",
  Length[prSixScalarFullReport["Channels"]] === 25 &&
    AllTrue[prSixScalarFullReport["Channels"], #["Status"] === "PASS" &]];
prSixScalarRepeated = prDetailedReport[
  prSixScalarTarget["StateSum"]/prSixScalarTarget["Propagator"],
  {prSixScalarTarget}, "Samples" -> 2,
  "ParameterRules" -> {scalarCubic -> 2, scalarQuartic -> 3, scalarQuintic -> 5},
  "TimeConstraint" -> 15];
prNormalizedSampleMomenta[report_] := Map[Function[sample,
  With[{chart = sample["Kinematics"]},
    chart["Momenta"] /. chart["PoleVariable"] -> prReproduciblePole]],
  First[report["Channels"]]["Samples"]];
check["automatic sample kinematics are reproducible with the same seed",
  prNormalizedSampleMomenta[prSixScalarReport] ===
    prNormalizedSampleMomenta[prSixScalarRepeated]];
Do[
  check["invalid general checker option is rejected " <> ToString[option, InputForm],
    Quiet[prDetailedReport[0, {prSixScalarTarget}, option]] === $Failed],
  {option, {"Samples" -> 0, "Samples" -> 1/2, "RandomSeed" -> "seed",
    "ParameterRules" -> None, "ParameterRules" -> {Me :> 1}}}
];
check["the public checker rejects an unrecognized option instead of silently ignoring it",
  Quiet[CheckPhysicalResidues[prH, prChannels, prFixtureModel,
    "UnknownDiagnosticOption" -> True]] === $Failed];
check["single-channel residue extraction rejects an unrecognized option",
  Quiet[PhysicalPoleResidue[0, prTestTarget,
    "UnknownDiagnosticOption" -> True]] === $Failed];
check["multi-channel residue extraction rejects an unrecognized option",
  Quiet[PhysicalPoleResidue[0, {prTestTarget},
    "UnknownDiagnosticOption" -> True]] === $Failed];
check["strict option validation retains ordinary nested option-list syntax",
  PhysicalPoleResidue[0, prTestTarget,
    {"KinematicKind" -> "Symbolic", "TimeConstraint" -> 5}]["Status"] === "FORMAL" &&
    CheckPhysicalResidues[{{Thread[(Mass /@ Range[4]) -> ConstantArray[0, 4]], 0}},
      {}, prModel, {"SpuriousPoles" -> {}, "HighEnergyCheck" -> False,
        "ReferenceCheck" -> False, "Output" -> "Summary"}] === <||>];
prUnknownFactorReport = prDetailedReport[prUnknownFactor prH,
  {prTestTarget}, "KinematicMap" -> prMap[1, 2], "TimeConstraint" -> 15];
check["an undecided symbolic normalization difference is not a false sample pass or failure",
  First[prUnknownFactorReport["Channels"]]["Status"] === "UNRESOLVED"];
prTinyExactDifference = prDetailedReport[(1 + 10^-30) prH,
  {prTestTarget}, "KinematicMap" -> prMap[1, 2], "TimeConstraint" -> 15];
check["a tiny exact nonzero residue difference is not hidden by numerical tolerance",
  First[prTinyExactDifference["Channels"]]["Status"] === "FAIL"];
prTinyExactOppositeDifference = prDetailedReport[(-1 + 10^-30) prH,
  {prTestTarget}, "KinematicMap" -> prMap[1, 2], "TimeConstraint" -> 15];
check["a tiny exact nonzero opposite-sign difference is not hidden by numerical tolerance",
  First[prTinyExactOppositeDifference["Channels"]]["Status"] === "FAIL" &&
    First[prTinyExactOppositeDifference["Channels"]]["OppositeSignDifference"] =!= 0];
prSymbolicSignTarget = Join[prTestTarget, <|"StateSum" -> prSymbolicCoefficient|>];
prSymbolicSignReport = prDetailedReport[-prSymbolicCoefficient/prD13,
  {prSymbolicSignTarget}, "KinematicMap" -> prMap[1, 2],
  "KinematicKind" -> "Symbolic", "TimeConstraint" -> 15];
check["symbolic chart comparisons recognize an exact opposite sign without numerical sampling",
  prSymbolicSignReport["Summary"] === <|"Opp Sign" -> 1|> &&
    First[prSymbolicSignReport["Channels"]]["CalculatedResidue"] === -prSymbolicCoefficient &&
    First[prSymbolicSignReport["Channels"]]["ExpectedResidue"] === prSymbolicCoefficient &&
    First[prSymbolicSignReport["Channels"]]["OppositeSignDifference"] === 0];
prOppositeDifferenceTimeout = Block[
  {ConstructiveDiagrams`Private`residueOnChart, prSignTimeoutValue, prSignValueCalls = 0},
  prSignTimeoutValue /: Plus[prSignTimeoutValue, -1] := 1;
  prSignTimeoutValue /: Plus[prSignTimeoutValue, 1] := (Pause[2]; 3);
  ConstructiveDiagrams`Private`residueOnChart[___] :=
    If[++prSignValueCalls === 1, prSignTimeoutValue, 1];
  ConstructiveDiagrams`Private`residueCheckChannel[1/prD13,
    prTestTarget, {}, {Join[prTestTarget, <|"StateSum" -> 1|>]},
    {prTestTarget}, prMap[1, 2], "Sample", 0.2, 10^-10, {}]
];
check["a timeout of only the opposite-sign difference remains unresolved rather than failing",
  prOppositeDifferenceTimeout["Status"] === "UNRESOLVED" &&
    prOppositeDifferenceTimeout["Difference"] === 1 &&
    prOppositeDifferenceTimeout["OppositeSignDifference"] === Missing["Timeout"] &&
    MemberQ[prOppositeDifferenceTimeout["Diagnostics"], Missing["Timeout"]]];

prMasslessScalarModel = ReplacePart[prScalarModel, {1, 1, 6} -> 0];
prMasslessScalarTargets = prAdaptedTargets[ConstantArray["s", 4], prMasslessScalarModel];
prMasslessScalarAmplitude = Total[
  #["StateSum"]/#["Propagator"] & /@ prMasslessScalarTargets];
prMasslessScalarReport = prDetailedReport[prMasslessScalarAmplitude,
  prMasslessScalarTargets, "Samples" -> 4,
  "ParameterRules" -> {cubicCoupling -> 2}, "TimeConstraint" -> 15];
check["automatic four-point massless charts find regular branches for all three scalar cuts",
  Length[prMasslessScalarReport["Channels"]] === 3 &&
    AllTrue[prMasslessScalarReport["Channels"],
      #["Status"] === "PASS" && #["CompletedSamples"] === 4 &]];

prGenericChannel[n_, cut_, masses_, internal_] := <|"Cut" -> cut,
  "Complement" -> Complement[Range[n], cut], "Mass" -> internal,
  "ExternalLegs" -> Range[n],
  "ExternalMassRules" -> Thread[(Mass /@ Range[n]) -> masses],
  "Propagator" -> PropDen[Mom[Multiparticle @@ cut], internal]|>;
prGenericChartCases = {
  prGenericChannel[4, {1, 2}, {0, 0, 0, 0}, 0],
  prGenericChannel[6, {1, 3, 5}, {2, 3, 5, 7, 11, 13}, 17],
  prGenericChannel[7, {1, 2}, {0, 2, 0, 3, 0, 5, 7}, 11],
  prGenericChannel[6, {1, 2, 3},
    {prChartMass1, prChartMass2, 0, prChartMass4, 0, prChartMass6}, prChartInternalMass]};
Do[
  prGenericChart = ConstructiveDiagrams`Private`residueAutomaticChart[
    prGenericChartCases[[case]], sample, 511, {}];
  prGenericValidation = If[FailureQ[prGenericChart], <|"Valid" -> False|>,
    ConstructiveDiagrams`Private`residueKinematicChecks[prGenericChart["Map"],
      {prGenericChartCases[[case]]}, {prGenericPole}, {}]];
  check["general chart preserves external shells, momentum and selected cut " <>
    ToString[{case, sample}], TrueQ[prGenericValidation["Valid"]]],
  {case, Length[prGenericChartCases]}, {sample, 4}
];

prGenericSpinorChart = ConstructiveDiagrams`Private`residueAutomaticChart[
  First[prElectronTargets], 1, 512, {Me -> 1, EE -> 1}];
prGenericSpinorZero[expression_] := ConstructiveDiagrams`Private`residueExactZero[
  ConstructiveDiagrams`Private`residueEvaluateChart[expression,
    prGenericSpinorChart["Chart"], {Me -> 1, EE -> 1}]];
prGenericAngle3 = Spinor["Helicity", "Angle", 3];
prGenericAngle4 = Spinor["Helicity", "Angle", 4];
prGenericSquare3 = Spinor["Helicity", "Square", 3];
check["automatic spinor map preserves two-momentum traces",
  prGenericSpinorZero[SpinorTrace[Mom[1], Mom[2]] - 2 MomProd[1, 2]]];
check["automatic spinor map preserves the empty trace",
  prGenericSpinorZero[SpinorTrace[] - 2]];
check["automatic spinor map evaluates Mandelstahm invariants coherently",
  prGenericSpinorZero[Mandelstahm[1, 2] - Mass[1]^2 - Mass[2]^2 - 2 MomProd[1, 2]]];
(* MomentumSquare edits emit scalar Mom[...]^2. These must be evaluated
   before a bare momentum is mapped to its two-by-two matrix. *)
check["automatic chart evaluates an external momentum square as its mass squared",
  prGenericSpinorZero[Mom[1]^2 - Mass[1]^2]];
check["automatic chart evaluates a massless composite momentum square as a scalar invariant",
  prGenericSpinorZero[Mom[Multiparticle[3, 4]]^2 - 2 MomProd[3, 4]]];
check["automatic chart evaluates higher-multiplicity momentum squares without internal shells",
  prGenericSpinorZero[Mom[Multiparticle[3, 4, 5]]^2 -
    2 (MomProd[3, 4] + MomProd[3, 5] + MomProd[4, 5])]];
Do[
  check["automatic chart preserves an even external momentum power " <>
    ToString[prInvariantPower],
    prGenericSpinorZero[Mom[1]^prInvariantPower - Mass[1]^prInvariantPower]];
  check["automatic chart preserves an even composite momentum power " <>
    ToString[prInvariantPower],
    prGenericSpinorZero[Mom[Multiparticle[3, 4]]^prInvariantPower -
      (2 MomProd[3, 4])^(prInvariantPower/2)]],
  {prInvariantPower, {4, 6, -2, -4}}];
check["scalar momentum powers do not change bare momentum matrix evaluation",
  MatrixQ[ConstructiveDiagrams`Private`residueEvaluateChart[Mom[1],
    prGenericSpinorChart["Chart"], {Me -> 1, EE -> 1}],
    Function[value, FreeQ[value, _Mom]]]];
prUnknownInvariant = ConstructiveDiagrams`Private`residueEvaluateChart[
  Mom[Multiparticle[3, 99]]^2, prGenericSpinorChart["Chart"], {Me -> 1, EE -> 1}];
check["an unknown leg inside a scalar momentum square remains unresolved",
  !ConstructiveDiagrams`Private`residueFiniteQ[prUnknownInvariant] &&
    !FreeQ[prUnknownInvariant, _Missing]];
prInvariantTarget = First[prElectronTargets];
prInvariantResidue = PhysicalPoleResidue[
  Mom[Multiparticle[3, 4]]^2/prInvariantTarget["Propagator"],
  prInvariantTarget, "KinematicMap" -> prGenericSpinorChart["Map"],
  "KinematicKind" -> "Symbolic", "TimeConstraint" -> 15];
prInvariantDotResidue = PhysicalPoleResidue[
  2 MomProd[3, 4]/prInvariantTarget["Propagator"], prInvariantTarget,
  "KinematicMap" -> prGenericSpinorChart["Map"],
  "KinematicKind" -> "Symbolic", "TimeConstraint" -> 15];
check["public residue extraction accepts the scalar momentum-square form of a numerator",
  prInvariantResidue["Status"] === "PASS" &&
    prInvariantDotResidue["Status"] === "PASS" &&
    prZero[prInvariantResidue["PhysicalResidue"] -
      prInvariantDotResidue["PhysicalResidue"]]];
prInvariantReport = CheckPhysicalResidues[
  prH (Mom[1]^2/Me^2 + Mom[Multiparticle[3, 4]]^2/(2 MomProd[3, 4]) - 1),
  prChannels, prFixtureModel, "ParameterRules" -> {Me -> 1, EE -> 1},
  "Samples" -> 1, "SpuriousPoles" -> {}, "TimeConstraint" -> 30,
  "Output" -> "Details"];
check["the public checker accepts equivalent external and composite momentum-square numerators",
  Counts[Lookup[prInvariantReport["Channels"], "Status"]] === <|"PASS" -> 6|>];
prExternalSquareInput = Mom[Multiparticle[1, 3, 4]]^2/
  prInvariantTarget["Propagator"];
prExternalSquareReduced = ConstructiveDiagrams`Private`completionExternalReduce[
  prExternalSquareInput, prInvariantTarget["ExternalMassRules"]];
check["completion cleanup preserves Lorentz squares instead of multiplying bare vector momenta",
  !FreeQ[prExternalSquareReduced, HoldPattern[Power[_Mom, 2]]] &&
    FreeQ[prExternalSquareReduced, HoldPattern[Times[___, _Mom, _Mom, ___]]]];
prExternalSquareChart = prGenericSpinorChart["Chart"] /.
  prGenericSpinorChart["Chart"]["PoleVariable"] -> 7;
prExternalSquareValues = ConstructiveDiagrams`Private`residueEvaluateChart[
    #, prExternalSquareChart, {Me -> 1, EE -> 1}] & /@
  {prExternalSquareInput, prExternalSquareReduced,
    (Mass[2]^2 + 2 MomProd[2, 5])/prInvariantTarget["Propagator"]};
check["completion cleanup preserves the scalar invariant and its conservation complement away from cuts",
  AllTrue[prExternalSquareValues, !ListQ[#] &&
      FreeQ[#, _Missing | Indeterminate | ComplexInfinity | DirectedInfinity[_]] &] &&
    prZero[prExternalSquareValues[[1]] - prExternalSquareValues[[2]]] &&
    prZero[prExternalSquareValues[[1]] - prExternalSquareValues[[3]]] &&
    ConstructiveDiagrams`Private`residueEvaluateChart[
      prInvariantTarget["Propagator"], prExternalSquareChart, {Me -> 1, EE -> 1}] === 7];
check["automatic spinor map preserves momentum anticommutation",
  prGenericSpinorZero[
    SpinorChain[prGenericAngle3, Mom[1], Mom[2], prGenericAngle4] +
    SpinorChain[prGenericAngle3, Mom[2], Mom[1], prGenericAngle4] -
    2 MomProd[1, 2] SpinorChain[prGenericAngle3, prGenericAngle4]]];
check["automatic spinor map preserves trace anticommutation",
  prGenericSpinorZero[SpinorTrace[Mom[1], Mom[2], Mom[3], Mom[4]] +
    SpinorTrace[Mom[2], Mom[1], Mom[3], Mom[4]] -
    2 MomProd[1, 2] SpinorTrace[Mom[3], Mom[4]]]];
check["automatic spinor map preserves the massless Weyl equation",
  prGenericSpinorZero[SpinorChain[prGenericAngle3, Mom[3], prGenericSquare3]]];
check["independent massless little-group scales preserve each momentum matrix",
  AllTrue[Keys[prGenericSpinorChart["Chart"]["MasslessLittleGroupScales"]],
    ConstructiveDiagrams`Private`residueExactZero[
      Outer[Times, prGenericSpinorChart["Chart"]["Angle"][#],
        prGenericSpinorChart["Chart"]["Square"][#]] -
      prGenericSpinorChart["Chart"]["Momenta"][#]] &]];
check["automatic spinors do not impose a false cyclic square-bracket identity",
  !prGenericSpinorZero[prSquare[3, 4] + prSquare[4, 5] + prSquare[5, 3]]];
Do[
  check["automatic massive spinor map preserves the angle Dirac equation " <>
    ToString[{position, spin}], prGenericSpinorZero[
      SpinorChain[Spinor["Spin", position, "Angle", 1, spin], Mom[1], prGenericSquare3] -
      Mass[1] SpinorChain[Spinor["Spin", position, "Square", 1, spin], prGenericSquare3]]];
  check["automatic massive spinor map preserves the square Dirac equation " <>
    ToString[{position, spin}], prGenericSpinorZero[
      SpinorChain[Spinor["Spin", position, "Square", 1, spin], Mom[1], prGenericAngle3] -
      Mass[1] SpinorChain[Spinor["Spin", position, "Angle", 1, spin], prGenericAngle3]]],
  {position, {"Upper", "Lower"}}, {spin, {-1/2, 1/2}}
];
check["distinct explicit reference labels remain distinct in automatic evaluation",
  !prGenericSpinorZero[SpinorChain[Spinor["Xi", "Angle", 10],
    Spinor["Xi", "Angle", 11]]]];

prFullPilot = q5FullResidueReport["Samples" -> 4,
  "ParameterRules" -> {Me -> 1, EE -> 1}, "TimeConstraint" -> 20];
prFullReports = prFullPilot["Reports"];
check["pilot retains the original diagrams and its explicit reference assignments",
  Length[prFullPilot["DiagramRecords"]] === 6 &&
    Length[prFullPilot["ReferenceAssignments"]] === 6 &&
    AllTrue[prFullPilot["ReferenceAssignments"], Length[#["Rules"]] === 3 &] &&
    Length[prFullPilot["CommonReferenceRules"]] === 3];
check["each prepared diagram uses the same reference label for each external photon",
  AllTrue[prFullPilot["DiagramRecords"], With[
    {prepared = #["OriginalExpression"] /. prDiagramReferenceRules[#]},
    AllTrue[Cases[prepared,
      SpinorChain[Spinor["Xi", _, reference_Integer],
        Spinor["Helicity", _, leg_Integer]] :> {reference, leg}, Infinity],
      First[#] === Last[#] &]] &]];
check["full QED report includes every supplied compact factorization channel",
  AllTrue[Values[prFullReports],
    Length[#["Channels"]] === Length[DeleteDuplicates[
      {#["Cut"], #["Mass"]} & /@ prTargets]] &]];
Do[
  check["general automatic QED report passes " <> label,
    AllTrue[prFullReports[label]["Channels"], #["Status"] === "PASS" &]],
  {label, {"H", "S2", "CommonReferenceDiagrams"}}
];
check["distinct-reference raw diagrams expose an electron-pole continuation failure",
  AnyTrue[prFullReports["ActualDiagrams"]["Channels"],
    #["Mass"] === Me && Count[#["Cut"], 1 | 2] === 1 &&
      Count[#["Cut"], 3 | 4 | 5] === 1 && #["Status"] === "FAIL" &]];
check["the passing common-reference diagram expression is an explicit saved substitution",
  prFullPilot["Amplitudes"]["CommonReferenceDiagrams"] ===
    (prFullPilot["Amplitudes"]["ActualDiagrams"] /. prFullPilot["CommonReferenceRules"])];
check["general automatic QED report still rejects the old permutation",
  AnyTrue[prFullReports["OldPermutation"]["Channels"], #["Status"] === "FAIL" &]];
check["full QED reports retain sample details, not just pass counts",
  AllTrue[prFullReports["H"]["Channels"],
    ContainsAll[Keys[#], {"FactorizationChannels", "ExpectedStateSum", "Samples"}] &]];

Do[
  prDouble = q5DoubleResidueReport[i, j];
  prDoubleValues = prDouble /@ {"H", "S2", "OldPermutation",
    "ExpectedFromFirst", "ExpectedFromSecond"};
  check["compatible two-variable residues are evaluated " <> ToString[{i, j}],
    AllTrue[prDoubleValues, #["Status"] === "PASS" &]];
  check["both lower-amplitude sewing orders give the same double residue " <> ToString[{i, j}],
    AllTrue[Rest[prDoubleValues],
      prZero[#["PhysicalResidue"] - First[prDoubleValues]["PhysicalResidue"]] &]],
  {i, 2}, {j, 2}
];
check["the old formula can pass a double pole despite failing single poles",
  prZero[prDouble["H"]["PhysicalResidue"] - prDouble["OldPermutation"]["PhysicalResidue"]] &&
    AnyTrue[prRows, !prZero[#["Old"] - #["Expected"]] &]];

(* Lower-point formulas must embed only particle-label positions. Numerical
   coefficients, exponents, spin labels and auxiliary Xi counters are not legs. *)
prLookupFormula = 7 MomProd[1, 2]^2 + 2 Mandelstahm[2, 3] + 3 Mass[4] +
  SpinorTrace[2 Mom[1], Mom[4]] + PropDen[2 Mom[1] + Mom[3], 2 prLookupMass] +
  SpinorChain[Spinor["Helicity", "Angle", 1], 2 Mom[2], Spinor["Helicity", "Square", 3]] +
  SpinorChain[Spinor["Spin", "Upper", "Angle", 1, -1/2],
    Spinor["Spin", "Lower", "Angle", 2, 1/2]];
prLookupModel = {prScalarModel[[1]],
  {{"s", "s", "s", "s", 1, prLookupFormula}}, {}, {}};
prLookupTags = {{"s", 5}, {"s", 1}, {"s", 3}, {"s", Multiparticle[2, 4, 6]}};
prLookupExpected = 7 MomProd[5, 1]^2 + 2 Mandelstahm[1, 3] +
  3 Mass[Multiparticle[2, 4, 6]] + SpinorTrace[2 Mom[5], Mom[Multiparticle[2, 4, 6]]] +
  PropDen[2 Mom[5] + Mom[3], 2 prLookupMass] +
  SpinorChain[Spinor["Helicity", "Angle", 5], 2 Mom[1], Spinor["Helicity", "Square", 3]] +
  SpinorChain[Spinor["Spin", "Upper", "Angle", 5, -1/2],
    Spinor["Spin", "Lower", "Angle", 1, 1/2]];
check["lower-amplitude lookup preserves coefficients, powers and spin labels while embedding invariants",
  LookupAmplitude[prLookupTags, prLookupModel, False, {prIx1, prIx2, prIx3, prIx4}] ===
    prLookupExpected];
prNestedLookupModel = ReplacePart[prLookupModel, {2, 1, -1} ->
  Mom[Multiparticle[1, Multiparticle[2, Multiparticle[3, 4]], 2]]];
check["nested multiparticles flatten completely after lower-amplitude embedding",
  LookupAmplitude[prLookupTags, prNestedLookupModel, False, {prIx1, prIx2, prIx3, prIx4}] ===
    Mom[Multiparticle[5, 1, 3, 2, 4, 6, 1]]];
prReferenceLookupModel = ReplacePart[prLookupModel, {2} -> {
  Join[ConstantArray["s", 10], {1,
    SpinorChain[Spinor["Xi", "Angle", 10], Spinor["Helicity", "Square", 10]]}]}];
check["auxiliary Xi counters are not relabeled when physical leg ten changes",
  LookupAmplitude[Thread[{ConstantArray["s", 10], Join[Range[9], {12}]}],
    prReferenceLookupModel, False, {prIx1, prIx2, prIx3, prIx4}] ===
      SpinorChain[Spinor["Xi", "Angle", 10], Spinor["Helicity", "Square", 12]]];

prToyAngle[a_, b_] := SpinorChain[Spinor["Spin", "Angle", a], Spinor["Spin", "Angle", b]];
prToySquare[a_, b_] := SpinorChain[Spinor["Spin", "Square", a], Spinor["Spin", "Square", b]];
prSixFermionModel = {{{"Fermion", "e", "E", 1, 1, prFermionMass, 0, 1, 1, "", ""},
    {"Scalar", "s", "s", 2, 0, 0, 0, 1, 1, "", ""}},
  {{"e", "E", "s", "", prF3, prToyAngle[1, 2]},
   {"e", "E", "e", "E", "s", prF5, prToyAngle[1, 3] prToySquare[2, 4]}}, {}, {}};
prSixFermionTargets = prAdaptedTargets[{"e", "E", "e", "E", "e", "E"},
  prSixFermionModel];
prFirstFermionTarget = SelectFirst[prSixFermionTargets,
  #["Cut"] === {1, 2} && #["Mass"] === 0 &];
prPermutedFermionTarget = SelectFirst[prSixFermionTargets,
  #["Cut"] === {2, 3} && #["Mass"] === 0 &];
prSwapFermionLabels[expression_] := expression /. Spinor[type_, kind_, label_] :>
  Spinor[type, kind, label /. {1 -> 3, 3 -> 1}];
check["six-fermion factorization retains identical-species exchange parity",
  prPermutedFermionTarget["StateSum"] ===
      -prSwapFermionLabels[prFirstFermionTarget["StateSum"]]];

prAllFourFermionChannels = FactorizationAmplitudeChannels[{"e", "E", "e", "E"}, prModel];
prAllFourFermionTargets = ConstructiveDiagrams`Private`residueSimpleInputs[
  0, prAllFourFermionChannels, prModel]["Targets"];
prFourFermionTargets = Select[prAllFourFermionTargets,
  #["Mass"] === 0 && TrueQ[#["Complete"]] && #["StateSum"] =!= 0 &];
check["the two internal photon helicities combine before residue evaluation",
  Length[prFourFermionTargets] === 2 && AllTrue[prFourFermionTargets,
    FreeQ[#["StateSum"], _xFactor | _xTildeFactor] &]];
prFourFermionAmplitude = Total[(ReduceSpinContractions[Last[#]] /. First[#]) & /@
  diagramAmplitudes[CreateDiagrams[{"e", "E", "e", "E"}, prModel]]];
prFourFermionReport = prDetailedReport[prFourFermionAmplitude, prFourFermionTargets,
  "Samples" -> 2, "ParameterRules" -> {Me -> 1, EE -> 1}, "TimeConstraint" -> 30];
check["identical-four-fermion diagrams and channels agree with the physical sewing phase",
  prFourFermionReport["Summary"] === <|"PASS" -> 2|> &&
    AllTrue[prFourFermionReport["Channels"],
      AllTrue[#["Samples"], Together[#["CalculatedResidue"] - #["ExpectedResidue"]] === 0 &] &]];
prAllFourFermionReport = prDetailedReport[prFourFermionAmplitude,
  prAllFourFermionTargets, "Samples" -> 1,
  "ParameterRules" -> {Me -> 1, EE -> 1}, "TimeConstraint" -> 30];
check["plain QED model checks its two nonzero four-point channels without explicit zero rows",
  Length[prAllFourFermionTargets] === 2 &&
    prAllFourFermionReport["Summary"] === <|"PASS" -> 2|>];

prMixedModel = ReadModel[FileNameJoin[{root, "models"}], 1];
prMixedTargets = Select[prAdaptedTargets[{"e", "E", "m", "M"}, prMixedModel],
  #["Mass"] === 0 && TrueQ[#["Complete"]] && #["StateSum"] =!= 0 &];
prMixedPhotonDiagrams = Select[CreateDiagrams[{"e", "E", "m", "M"}, prMixedModel],
  AllTrue[Values[#["Propagators"]], #["Mass"] === 0 &] &];
prMixedPhotonAmplitude = Total[(ReduceSpinContractions[Last[#]] /. First[#]) & /@
  diagramAmplitudes[prMixedPhotonDiagrams]];
prMixedPhotonReport = prDetailedReport[prMixedPhotonAmplitude, prMixedTargets,
  "Samples" -> 2, "ParameterRules" -> {Me -> 1, Mm -> 2, EE -> 1}, "TimeConstraint" -> 30];
check["nonidentical massive four-fermion photon residues agree without x factors",
  Length[prMixedTargets] === 1 && FreeQ[First[prMixedTargets]["StateSum"], _xFactor | _xTildeFactor] &&
    prMixedPhotonReport["Summary"] === <|"PASS" -> 1|> &&
    AllTrue[First[prMixedPhotonReport["Channels"]]["Samples"],
      Together[#["CalculatedResidue"] - #["ExpectedResidue"]] === 0 &]];

(* Internal fermion contractions must retain their source-model positions.
   The three intervening ports below are lost if internal labels are removed
   before the Wick sign is calculated. *)
prFermionSewingChannel = {
  {{"e", 1}, {"A.+", 5}, {"E", Multiparticle[2, 3, 4]}},
  {{"E", 2}, {"E", 3}, {"e", 4}, {"e", Multiparticle[1, 5]}}};
check["sewing a cubic vertex to a four-fermion amplitude includes the internal Wick sign",
  ConstructiveDiagrams`Private`factorizationExternalFermionSign[
    prFermionSewingChannel, prFixtureModel] === -1];
prTwoFermionSewingChannel = {
  {{"e", 1}, {"A.+", 3}, {"E", Multiparticle[2, 4, 5]}},
  {{"E", 2}, {"A.+", 4}, {"A.+", 5}, {"e", Multiparticle[1, 3]}}};
check["adjacent internal fermion ports preserve the established two-fermion sewing sign",
  ConstructiveDiagrams`Private`factorizationExternalFermionSign[
    prTwoFermionSewingChannel, prFixtureModel] === 1];
prTwoFermionOuterSewingChannel = {
  {{"E", 2}, {"A.+", 3}, {"e", Multiparticle[1, 4, 5]}},
  {{"e", 1}, {"A.+", 4}, {"A.+", 5}, {"E", Multiparticle[2, 3]}}};
check["two intervening fermion ports preserve the reversed internal-particle orientation",
  ConstructiveDiagrams`Private`factorizationExternalFermionSign[
    prTwoFermionOuterSewingChannel, prFixtureModel] === 1];
Do[
  prFiveFermionChannels = FactorizationAmplitudeChannels[particles, prFixtureModel];
  prFiveFermionTargets = ConstructiveDiagrams`Private`residueSimpleInputs[
    0, prFiveFermionChannels, prFixtureModel]["Targets"];
  prFiveElectronTargets = Select[prFiveFermionTargets, #["Mass"] === Me &];
  prFiveFermionAmplitude = Total[(ReduceSpinContractions[Last[#]] /. First[#]) & /@
    diagramAmplitudes[CreateDiagrams[particles, prModel]]];
  prFiveElectronReport = prDetailedReport[prFiveFermionAmplitude, prFiveElectronTargets,
    "Samples" -> 2, "ParameterRules" -> {Me -> 1, EE -> 1}, "TimeConstraint" -> 30];
  check["five-point four-fermion electron cuts agree after graph and Wick sign repairs " <>
    ToString[particles, InputForm], Length[prFiveElectronTargets] === 4 &&
      prFiveElectronReport["Summary"] === <|"PASS" -> 4|> &&
      AllTrue[Flatten[Lookup[prFiveElectronReport["Channels"], "Samples"]],
        Together[#["CalculatedResidue"] - #["ExpectedResidue"]] === 0 &]],
  {particles, {{"e", "E", "E", "e", "A.+"}, {"e", "E", "e", "E", "A.-"}}}
];

prPhotonTargets = Select[prFiveFermionTargets, #["Mass"] === 0 &];
prPhotonTarget = SelectFirst[prPhotonTargets, #["Cut"] === {1, 2} &];
prPhotonAutomatic = ConstructiveDiagrams`Private`residueAutomaticChart[
  prPhotonTarget, 2, 1729, {Me -> 1, EE -> 1}];
prPhotonChart = prPhotonAutomatic["Chart"];
prPhotonCut = Multiparticle @@ prPhotonTarget["Cut"];
prPhotonComplement = Multiparticle @@ prPhotonTarget["Complement"];
prPhotonPole = prPhotonChart["PoleVariable"];
prCutSpinor[kind_, index_] := ConstructiveDiagrams`Private`residueChartSpinor[
  Spinor["Helicity", kind, index], prPhotonChart];
prCutAngle = prCutSpinor["Angle", prPhotonCut];
prCutSquare = prCutSpinor["Square", prPhotonCut];
prPhotonMomentum = ConstructiveDiagrams`Private`residueChartMomentum[
  prPhotonCut, prPhotonChart] /. prPhotonPole -> 0;
check["selected massless-cut spinors reconstruct the exact rank-one momentum",
  prZero[Outer[Times, prCutAngle, prCutSquare] - prPhotonMomentum]];
check["complementary cut momentum reverses the angle spinor but not the square spinor",
  prZero[prCutSpinor["Angle", prPhotonComplement] + prCutAngle] &&
    prZero[prCutSpinor["Square", prPhotonComplement] - prCutSquare]];
check["unselected composite spinors are not silently put on shell",
  MissingQ[prCutSpinor["Angle", Multiparticle[1, 5]]]];
prMassiveAutomatic = ConstructiveDiagrams`Private`residueAutomaticChart[
  First[prFiveElectronTargets], 2, 1729, {Me -> 1, EE -> 1}];
check["the photon map does not assign massless spinors to a massive internal cut",
  MissingQ[ConstructiveDiagrams`Private`residueChartSpinor[
    Spinor["Helicity", "Angle", Multiparticle @@ First[prFiveElectronTargets]["Cut"]],
    prMassiveAutomatic["Chart"]]]];
prPhotonEndpointFactor = SpinorChain[Spinor["Helicity", "Angle", prPhotonCut],
  Spinor["Helicity", "Angle", 5]]^2;
prCutXValue[head_] := Module[{value, endpoint},
  value = ConstructiveDiagrams`Private`residueEvaluateChart[
    head[1, 2] prPhotonEndpointFactor, prPhotonChart, {Me -> 1, EE -> 1}];
  endpoint = ConstructiveDiagrams`Private`residueEvaluateChart[
    prPhotonEndpointFactor, prPhotonChart, {Me -> 1, EE -> 1}];
  Cancel[Together[value/endpoint]] /. prPhotonPole -> 0
];
prCutX = prCutXValue[xFactor]; prCutXTilde = prCutXValue[xTildeFactor];
prCutSecondMomentum = prPhotonChart["Momenta"][2] /. prPhotonPole -> 0;
prSpinorMetric = {{0, 1}, {-1, 0}};
check["internal x evaluates its defining incoming-photon momentum equation",
  prZero[prCutSecondMomentum.prSpinorMetric.prCutSquare + prCutX prCutAngle]];
check["internal x-tilde evaluates the conjugate incoming-photon momentum equation",
  prZero[ConstructiveDiagrams`Private`residueAdjugate[prCutSecondMomentum].prCutAngle +
    prCutXTilde prSpinorMetric.prCutSquare]];
check["internal x and x-tilde have inverse helicity weights on the selected cut",
  prZero[prCutX prCutXTilde - 1]];
prPhotonReferenceValue[angle_, square_] := Module[{chart},
  chart = Join[prPhotonChart, <|"ReferenceOverrides" -> <|
    "Angle" -> <|"CutPhoton" -> angle|>, "Square" -> <|"CutPhoton" -> square|>|>|>];
  Cancel[Together[ConstructiveDiagrams`Private`residueEvaluateChart[
    prPhotonTarget["StateSum"], chart, {Me -> 1, EE -> 1}]]] /. prPhotonPole -> 0
];
check["sewn internal-photon targets are independent of their auxiliary x references",
  prZero[prPhotonReferenceValue[{1, 2}, {3, 5}] -
    prPhotonReferenceValue[{5, 11}, {7, 13}]]];
prPhotonLittleGroupScaled = prPhotonTarget["StateSum"] /.
  chain_SpinorChain :> prHelicityScale^(
    Count[chain, Spinor["Helicity", "Angle", prPhotonCut], Infinity] -
    Count[chain, Spinor["Helicity", "Square", prPhotonCut], Infinity]) chain;
prPhotonLittleGroupScaled = prPhotonLittleGroupScaled /.
  {xFactor[i_, j_] :> prHelicityScale^-2 xFactor[i, j],
    xTildeFactor[i_, j_] :> prHelicityScale^2 xTildeFactor[i, j]};
check["the sewn photon helicity sum has no internal little-group scale dependence",
  Together[prPhotonLittleGroupScaled - prPhotonTarget["StateSum"]] === 0];

Print[<|"Checks" -> checks, "Failed" -> Length[failures],
  "Failures" -> failures, "ExactSinglePoleComponents" -> Length[prRows],
  "ValidationScope" -> "ExactOnProvidedRationalSampleCharts"|>];
Print["HEPCAT_TEST_COMPLETED:physical-residue-tests.wl"];
Quit[If[failures === {}, 0, 1]];
