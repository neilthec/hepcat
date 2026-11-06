(* Tiny Automatic batches do not pay parallel-tool or worker startup costs. *)
root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Global`$HEPCATpath = FileNameJoin[{root, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
$HistoryLength = 0; checks = 0; failures = {};
SetAttributes[check, HoldRest];
check[name_, value_] := (checks++;
  If[!TrueQ[value], AppendTo[failures, name]; Print["FAIL: ", name]]);
apSettings = ConstructiveDiagrams`Private`preparationParallelSettings;
apOptions = <|"Parallelization" -> Automatic, "MaxKernels" -> Automatic|>;
apTiny = Table[{{Mass[1] -> apMass}, index/apDen}, {index, 3}];
apLarge = Table[{{Mass[1] -> apMass}, Sum[apCoefficient[index, k] apChain[k], {k, 200}]}, {index, 24}];
apCold = Block[{ConstructiveDiagrams`Private`$amplitudeParallelKernels = {}},
  apSettings[apTiny, apOptions]];
check["cold tiny Automatic preparation resolves to serial before parallel initialization",
  apCold["Parallelization"] === False && TrueQ[apCold["WarmOnly"]]];
apWarm = Block[{ConstructiveDiagrams`Private`$amplitudeParallelKernels = {apKernel1, apKernel2}},
  apSettings[apTiny, apOptions]];
check["small Automatic work may reuse an already warm pool without enlarging it",
  apWarm["Parallelization"] === Automatic && TrueQ[apWarm["WarmOnly"]]];
apForced = Block[{ConstructiveDiagrams`Private`$amplitudeParallelKernels = {}},
  apSettings[apTiny, Join[apOptions, <|"Parallelization" -> True|>]]];
check["explicit parallel preparation still requests a normal launch-capable pool",
  apForced["Parallelization"] === True && !TrueQ[Lookup[apForced, "WarmOnly", False]] &&
    apForced["MaxKernels"] === Automatic];
apLargeSettings = Block[{ConstructiveDiagrams`Private`$amplitudeParallelKernels = {}},
  apSettings[apLarge, apOptions]];
check["twenty-four substantial diagrams retain Automatic parallel startup",
  apLargeSettings["Parallelization"] === Automatic &&
    !TrueQ[Lookup[apLargeSettings, "WarmOnly", False]] && apLargeSettings["MaxKernels"] === Automatic];
check["explicit serial preparation remains serial at any expression size",
  apSettings[apLarge, <|"Parallelization" -> False, "MaxKernels" -> 7|>] ===
    <|"Parallelization" -> False, "MaxKernels" -> 7|>];
check["an explicit worker cap is preserved by the Automatic policy",
  apSettings[apTiny, <|"Parallelization" -> Automatic, "MaxKernels" -> 1|>]["MaxKernels"] === 1];
(* Probe the actual dispatcher boundary without launching processes. *)
apPoolCalls = {};
apMockResults = Internal`InheritedBlock[
  {ConstructiveDiagrams`Private`amplitudeParallelPool, ConstructiveDiagrams`Private`amplitudeParallelLimit},
  Clear[ConstructiveDiagrams`Private`amplitudeParallelPool,
    ConstructiveDiagrams`Private`amplitudeParallelLimit];
  ConstructiveDiagrams`Private`amplitudeParallelLimit[___] := 3;
  ConstructiveDiagrams`Private`amplitudeParallelPool[limit_, warmOnly_] := (
    AppendTo[apPoolCalls, {limit, warmOnly}];
    <|"Kernels" -> {}, "StartupTime" -> 0., "Diagnostics" -> {"Mock pool"}|>);
  Map[ConstructiveDiagrams`Private`amplitudeParallelMap[Identity, Range[3], #] &,
    {apCold, apWarm, apForced, apLargeSettings}]];
check["cold tiny work never reaches even the pool initialization boundary",
  apPoolCalls === {{3, True}, {3, False}, {3, False}}];
check["scheduling policy leaves each input and its order unchanged",
  AllTrue[apMockResults, #["Results"] === Range[3] &]];
apModel = {{{"Scalar", "s", "s", 1, 0, apMass, 0, 1, 1, "", ""}},
  {{"s", "s", "s", "", apCoupling, 1}}, {}, {}};
apChannels = FactorizationAmplitudeChannels[ConstantArray["s", 4], apModel];
apPoolTouched = False;
apPrepared = Internal`InheritedBlock[{ConstructiveDiagrams`Private`amplitudeParallelPool},
  Clear[ConstructiveDiagrams`Private`amplitudeParallelPool];
  ConstructiveDiagrams`Private`amplitudeParallelPool[___] := (apPoolTouched = True; $Failed);
  Block[{ConstructiveDiagrams`Private`$amplitudeParallelKernels = {}},
    PrepareAmplitudes[apChannels, apModel, "MaxSteps" -> 0,
      "ProgressReporting" -> False, "ReturnDetails" -> True]]];
check["three ordinary scalar channels prepare under Automatic without a launch attempt",
  !apPoolTouched && AssociationQ[apPrepared] && apPrepared["Status"] === "PREPARED" &&
    apPrepared["Execution"]["Mode"] === "Serial"];
check["the detailed options preserve the user's Automatic request",
  apPrepared["RequestedOptions"]["Parallelization"] === Automatic &&
    apPrepared["Options"]["Parallelization"] === Automatic];
Print[checks, " preparation-parallel-policy checks; ", Length[failures], " failures."];
If[failures =!= {}, Print[failures]; Quit[1]];
Print["HEPCAT_TEST_COMPLETED:preparation-parallel-policy-tests.wl"]; Quit[0];
