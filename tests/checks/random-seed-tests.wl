(* Time-based ordinary runs retain an explicit, fully replayable seed. *)
root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Global`$HEPCATpath = FileNameJoin[{root, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
$HistoryLength = 0;
checks = 0; failures = {};
SetAttributes[check, HoldRest];
check[name_, value_] := (checks++; If[!TrueQ[value], AppendTo[failures, name]; Print["FAIL: ", name]]);
rsSeedQ[value_] := IntegerQ[value] && 0 <= value < 2^63;
check["public checking and construction stages choose time-based seeds by default",
  AllTrue[{CheckAmplitudes, CheckPhysicalResidues, CompleteFactorization,
    CompleteHighEnergy, ConstructAmplitude}, ("RandomSeed" /. Options[#]) === Automatic &]];
rsSeeds = Table[ConstructiveDiagrams`Private`amplitudeResolveRandomSeed[Automatic], 4];
check["successive automatic requests resolve to distinct concrete nonnegative seeds",
  AllTrue[rsSeeds, rsSeedQ] && DuplicateFreeQ[rsSeeds]];
check["explicit integer seeds including zero are retained exactly",
  (ConstructiveDiagrams`Private`amplitudeResolveRandomSeed /@ {0, 17, 1729}) === {0, 17, 1729}];
rsHelperPreservesRandomState = BlockRandom[Module[{expected, actual},
  SeedRandom[20261030]; expected = RandomInteger[10^6, 8];
  SeedRandom[20261030]; ConstructiveDiagrams`Private`amplitudeResolveRandomSeed[Automatic];
  actual = RandomInteger[10^6, 8]; expected === actual]];
check["automatic seed selection does not consume the caller's random stream", rsHelperPreservesRandomState];

rsModel = {{{"Scalar", "s", "s", 1, 0, 0, 0, 1, 1, "", ""}},
  {{"s", "s", "s", "s", 1, 7}}, {}, {}};
rsMassRules = Thread[(Mass /@ Range[4]) -> 0];
rsXi[leg_] := SpinorChain[Spinor["Xi", "Angle", 71], Spinor["Helicity", "Angle", leg]];
(* This deliberately reference-dependent expression exercises every checker
   group. These tests concern reproducible sampling, not a claimed amplitude. *)
rsExpression = 1 + rsXi[1]/rsXi[2];
rsChannels = {{0, 0, 0, 0, Multiparticle[1, 3], 0, 0}};
rsSettings = <|"Samples" -> 1, "HighEnergySamples" -> 1,
  "ReferenceSamples" -> 1, "FeynmanSamples" -> 1,
  "FeynmanAmplitudes" -> rsExpression,
  "SpuriousPoles" -> {PropDen[Mom[Multiparticle[2, 3]], 0]},
  "Output" -> "Details", "Parallelization" -> False,
  "MaxKernels" -> 2, "TimeConstraint" -> 20|>;
rsReport[opts___Rule] := CheckAmplitudes[{rsMassRules, rsExpression}, rsChannels, rsModel,
  Sequence @@ Normal[Join[rsSettings, Association[{opts}]]]];
rsGroups = {"Channels", "SpuriousChecks", "HighEnergyCheck", "ReferenceCheck", "FeynmanCheck"};
rsGeometry[chart_Association] := With[{pole = Lookup[chart, "PoleVariable", None]},
  <|"Momenta" -> Map[Function[matrix, Map[Together, matrix, {2}]],
      chart["Momenta"] /. If[MatchQ[pole, _Symbol], {pole -> rsReplayPole}, {}]],
    "ReferenceSeed" -> Lookup[chart, "ReferenceSeed", None]|>];
rsCharts[value_] := Cases[value, chart_Association /; KeyExistsQ[chart, "Momenta"] :>
  rsGeometry[chart], {0, Infinity}];
rsGroupedCharts[report_Association] := Association[(# -> rsCharts[report[#]]) & /@ rsGroups];
rsAutomatic = rsReport[];
rsSecondAutomatic = rsReport[];
check["ordinary public checker details record a concrete seed for each new run",
  AssociationQ[rsAutomatic] && AssociationQ[rsSecondAutomatic] &&
    rsSeedQ[rsAutomatic["RandomSeed"]] && rsSeedQ[rsSecondAutomatic["RandomSeed"]] &&
    rsAutomatic["RandomSeed"] =!= rsSecondAutomatic["RandomSeed"]];
rsReplay = rsReport["RandomSeed" -> rsAutomatic["RandomSeed"]];
check["physical, spurious, high-energy, reference and Feynman jobs all retain sampled charts",
  AllTrue[Values[rsGroupedCharts[rsAutomatic]], Length[#] > 0 &]];
check["one recorded automatic seed replays every checker group's exact kinematics",
  rsGroupedCharts[rsAutomatic] === rsGroupedCharts[rsReplay]];
check["new automatic checker runs explore different exact physical kinematics",
  rsCharts[rsAutomatic["Channels"]] =!= rsCharts[rsSecondAutomatic["Channels"]]];
rsZero = rsReport["RandomSeed" -> 0];
rsZeroReplay = rsReport["RandomSeed" -> 0];
check["zero is an explicit replay seed rather than an automatic-seed sentinel",
  rsZero["RandomSeed"] === 0 && rsZeroReplay["RandomSeed"] === 0 &&
    rsGroupedCharts[rsZero] === rsGroupedCharts[rsZeroReplay]];
rsLegacy = CheckPhysicalResidues[{rsMassRules, rsExpression}, rsChannels, rsModel,
  Sequence @@ Normal[Join[rsSettings, <|"RandomSeed" -> 0|>]]];
check["the compatibility checker preserves an explicit seed and its complete sampled geometry",
  rsLegacy["RandomSeed"] === 0 && rsGroupedCharts[rsLegacy] === rsGroupedCharts[rsZero]];
rsParallel = rsReport["RandomSeed" -> 0, "Parallelization" -> True];
check["parallel checking uses real workers for independently sampled jobs",
  rsParallel["Execution"]["Mode"] === "Parallel" && rsParallel["Execution"]["KernelCount"] === 2];
check["parallel scheduling does not resample or alter the shared explicit run seed",
  rsParallel["RandomSeed"] === 0 && rsGroupedCharts[rsParallel] === rsGroupedCharts[rsZero]];
If[rsGroupedCharts[rsParallel] =!= rsGroupedCharts[rsZero],
  Print["PARALLEL SEED GEOMETRY: ", Association[(# ->
    If[rsCharts[rsParallel[#]] === rsCharts[rsZero[#]], "SAME",
      <|"Parallel" -> rsCharts[rsParallel[#]], "Serial" -> rsCharts[rsZero[#]]|>]) & /@ rsGroups]]];
rsPublicPreservesRandomState = BlockRandom[Module[{expected, actual},
  SeedRandom[20261030]; expected = RandomInteger[10^6, 8];
  SeedRandom[20261030]; rsReport[];
  actual = RandomInteger[10^6, 8]; expected === actual]];
check["an automatically seeded complete checker run leaves the caller's random stream untouched",
  rsPublicPreservesRandomState];

rsFactor = CompleteFactorization[{rsMassRules, 7}, {}, rsModel,
  "ExternalParticles" -> ConstantArray["s", 4], "Samples" -> 1, "ValidationSamples" -> 1,
  "Parallelization" -> False, "TimeConstraint" -> 20, "ProgressReporting" -> False,
  "ReturnDetails" -> True];
check["standalone factorization completion records its resolved automatic seed",
  AssociationQ[rsFactor] && rsFactor["Status"] === "COMPLETE" && rsSeedQ[rsFactor["RandomSeed"]]];
rsEnergy = CompleteHighEnergy[rsFactor, {}, rsModel,
  "Samples" -> 1, "ValidationSamples" -> 1,
  "Parallelization" -> False, "TimeConstraint" -> 20, "ProgressReporting" -> False,
  "Output" -> "Details"];
check["standalone high-energy completion records a fresh automatic seed",
  AssociationQ[rsEnergy] && rsEnergy["Status"] === "COMPLETE" && rsSeedQ[rsEnergy["RandomSeed"]] &&
    rsEnergy["RandomSeed"] =!= rsFactor["RandomSeed"]];

rsConstructOptions = <|"Output" -> "Details", "ProgressReporting" -> False,
  "PrintReport" -> False, "Parallelization" -> False, "TimeConstraint" -> 60,
  "Samples" -> 1, "ValidationSamples" -> 1,
  "PreparationOptions" -> {"MaxSteps" -> 0, "TimeConstraint" -> 5},
  "ChannelPreparationOptions" -> {"MaxSteps" -> 0, "TimeConstraint" -> 5},
  "FinalPreparationOptions" -> {"MaxSteps" -> 0, "TimeConstraint" -> 5},
  "FactorizationOptions" -> {"RandomSeed" -> Automatic},
  "HighEnergyOptions" -> {"RandomSeed" -> Automatic},
  "CheckOptions" -> {"RandomSeed" -> Automatic, "HighEnergySamples" -> 1,
    "ReferenceSamples" -> 1, "TimeConstraint" -> 10}|>;
rsConstruct[opts___Rule] := ConstructAmplitude[ConstantArray["s", 4], rsModel, rsModel,
  Sequence @@ Normal[Join[rsConstructOptions, Association[{opts}]]]];
rsConstruction = rsConstruct[];
check["a complete ordinary construction records one time-based run seed",
  AssociationQ[rsConstruction] && rsConstruction["Status"] === "COMPLETE" &&
    rsConstruction["Amplitude"] === 7 && rsSeedQ[rsConstruction["RandomSeed"]] &&
    rsConstruction["RequestedOptions"]["RandomSeed"] === Automatic];
rsStageSeeds[report_Association] := With[{stages = report["Stages"]},
  Lookup[stages[#], "RandomSeed", None] & /@
    {"Completing factorization", "Completing high-energy behavior", "Checking final amplitude"}];
check["nested Automatic seed options inherit the constructor's shared seed and independent validation offsets",
  rsStageSeeds[rsConstruction] === rsConstruction["RandomSeed"] + {0, 1000033, 2000003}];
rsZeroConstruction = rsConstruct["RandomSeed" -> 0];
check["explicit constructor seed zero is honored throughout all nested stages",
  rsZeroConstruction["Status"] === "COMPLETE" && rsZeroConstruction["RandomSeed"] === 0 &&
    rsStageSeeds[rsZeroConstruction] === {0, 1000033, 2000003}];
rsOverrideConstruction = rsConstruct["RandomSeed" -> 23,
  "FactorizationOptions" -> {"RandomSeed" -> 101},
  "HighEnergyOptions" -> {"RandomSeed" -> 102},
  "CheckOptions" -> {"RandomSeed" -> 103, "HighEnergySamples" -> 1,
    "ReferenceSamples" -> 1, "TimeConstraint" -> 10}];
check["explicit per-stage seeds override inherited defaults without changing the root seed",
  rsOverrideConstruction["Status"] === "COMPLETE" && rsOverrideConstruction["RandomSeed"] === 23 &&
    rsStageSeeds[rsOverrideConstruction] === {101, 102, 103}];
rsPrinted = {};
rsPrintedConstruction = Block[{Print = (AppendTo[rsPrinted, HoldComplete[##]] &)},
  rsConstruct["RandomSeed" -> 0, "PrintReport" -> True]];
check["ordinary final construction output prints the actual replay seed before timing and tests",
  rsPrintedConstruction["Status"] === "COMPLETE" && Length[rsPrinted] >= 3 &&
    StringContainsQ[ToString[First[rsPrinted], InputForm], "Random seed"] &&
    StringContainsQ[ToString[rsPrinted[[-2]], InputForm], "Total"] &&
    StringContainsQ[ToString[Last[rsPrinted], InputForm], "PASS"]];

Print["RANDOM SEED CHECKS: ", checks, "; FAILURES: ", Length[failures]];
If[failures === {}, Print["HEPCAT_TEST_COMPLETED:random-seed-tests.wl"], Print[failures]];
Quit[If[failures === {}, 0, 1]];
