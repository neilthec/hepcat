(* Independent high-energy directions share one bounded checker deadline. *)
root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Global`$HEPCATpath = FileNameJoin[{root, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
Get[FileNameJoin[{root, "tests", "QED-5-point-residues.wl"}]];
checks = 0; failures = {};
check[name_, value_] := (checks++; If[!TrueQ[value], AppendTo[failures, name]; Print["FAIL: ", name]]);
hdParameters = {prElectronMass -> 1, prElectricCharge -> 1};
hdNormalized = ConstructiveDiagrams`Private`residueSimpleInputs[prH, prChannels, prFixtureModel];
hdMetadata = ConstructiveDiagrams`Private`poleDiscoveryMetadata[hdNormalized["Targets"], hdNormalized["Records"]];
hdPlan = ConstructiveDiagrams`Private`highEnergyCheckPlan[prH, hdMetadata, 4, 192681, hdParameters, 60, Automatic];
check["one explicit job is created for every original high-energy sample",
  Length[hdPlan["Jobs"]] === 4 && Lookup[hdPlan["Jobs"], "Kind"] === ConstantArray["HighEnergySample", 4]];
check["the sample jobs preserve sample indices and random seed",
  (#["Arguments"][[{3, 4}]] & /@ hdPlan["Jobs"]) === Table[{i, 192681}, {i, 4}]];
check["preconditioning and every sample share one unchanged absolute deadline",
  DeleteDuplicates[Last[#["Arguments"]] & /@ hdPlan["Jobs"]] === {hdPlan["Deadline"]} &&
    TrueQ[hdPlan["Deadline"] - hdPlan["Started"] == 60]];
hdExpiredEvaluations = 0;
hdExpired = Internal`InheritedBlock[{ConstructiveDiagrams`Private`highEnergyAutomaticChart},
  Clear[ConstructiveDiagrams`Private`highEnergyAutomaticChart];
  ConstructiveDiagrams`Private`highEnergyAutomaticChart[___] := (hdExpiredEvaluations++; $Failed);
  ConstructiveDiagrams`Private`highEnergyCheckSample[prH, hdMetadata, 3, 192681,
    hdParameters, -1, AbsoluteTime[] - 1]];
check["an already-expired sample never evaluates its chart or reports completion",
  hdExpiredEvaluations === 0 && hdExpired["Status"] === "UNRESOLVED" &&
    hdExpired["Sample"] === 3 && hdExpired["SampleCompleted"] === False];
hdSettings = {"Output" -> "Details", "Samples" -> 1, "SpuriousPoles" -> {},
  "ReferenceCheck" -> False, "HighEnergySamples" -> 4,
  "HighEnergyTimeConstraint" -> 60, "TimeConstraint" -> 60,
  "RandomSeed" -> 192681, "ParameterRules" -> hdParameters, "MaxKernels" -> 2};
hdSerial = CheckAmplitudes[prH, prChannels, prFixtureModel,
  Sequence @@ hdSettings, "Parallelization" -> False];
hdParallel = CheckAmplitudes[prH, prChannels, prFixtureModel,
  Sequence @@ hdSettings, "Parallelization" -> True];
hdAlpha[value_] := Module[{symbols},
  symbols = DeleteDuplicates[Cases[value, symbol_Symbol /;
      StringMatchQ[SymbolName[Unevaluated[symbol]],
        ("inverseEnergyRoot" | "externalSpin" | "completionSpin") ~~ DigitCharacter ..] :>
        symbol, Infinity, Heads -> True]];
  symbols = SortBy[symbols, Function[symbol, With[{name = SymbolName[symbol]},
    {StringReplace[name, RegularExpression["[0-9]+$"] -> ""],
      FromDigits[Last[StringCases[name, DigitCharacter ..]]]}]]];
  MapAll[Identity, value /. Thread[symbols -> Table[
    Symbol["ConstructiveDiagrams`Private`dispatchCoordinate" <> ToString[i]], {i, Length[symbols]}]]]
];
hdSemantic[value_] := hdAlpha[MapAll[Function[item,
  If[AssociationQ[item], KeyDrop[item, {"ElapsedTime", "Execution"}], item]], value]];
(* Each sample owns its chart coordinates. Independent worker Unique counters
   need not have one global creation order across distinct sample scopes. *)
hdReportSemantic[report_Association] := Association[KeyValueMap[Function[{key, value},
  key -> If[key === "Samples", hdSemantic /@ value, hdSemantic[value]]], report]];
check["serial and parallel high-energy reports retain identical exact semantics",
  hdReportSemantic[hdSerial["HighEnergyCheck"]] === hdReportSemantic[hdParallel["HighEnergyCheck"]]];
check["all four sample obligations pass in their original order",
  hdParallel["HighEnergyCheck"]["Status"] === "PASS" &&
    hdParallel["HighEnergyCheck"]["CompletedSamples"] === 4 &&
    Lookup[hdParallel["HighEnergyCheck"]["Samples"], "Sample"] === Range[4] &&
    Lookup[hdParallel["HighEnergyCheck"]["Samples"], "Status"] === ConstantArray["PASS", 4]];
check["the exact Laurent method and coherent-spin coverage remain visible",
  hdParallel["HighEnergyCheck"]["Method"] === "ExactFixedAngleLaurentExpansion" &&
    hdParallel["HighEnergyCheck"]["CoefficientConvention"] === "UnnormalizedCoherentSpinPolynomial" &&
    AllTrue[hdParallel["HighEnergyCheck"]["Samples"],
      Length[#["SpinVariables"]] === 2 &&
      #["CoefficientConvention"] === "UnnormalizedCoherentSpinPolynomial" &]];
check["four independent high-energy jobs really use both requested workers",
  hdParallel["Execution"]["Mode"] === "Parallel" &&
    hdParallel["Execution"]["KernelCount"] === 2 &&
    hdParallel["Execution"]["HighEnergySampleJobs"] === 4 &&
    hdParallel["Execution"]["HighEnergyCompletedSampleJobs"] === 4 &&
    Length[DeleteDuplicates[Lookup[hdParallel["Execution"]["HighEnergySampleExecution"], "KernelID"]]] === 2 &&
    Min[Lookup[hdParallel["Execution"]["HighEnergySampleExecution"], "KernelID"]] > 0];
check["explicit serial checking keeps every high-energy job on the calling kernel",
  hdSerial["Execution"]["Mode"] === "Serial" &&
    Lookup[hdSerial["Execution"]["HighEnergySampleExecution"], "KernelID"] === ConstantArray[0, 4]];
check["execution timing stays in details rather than in each user sample",
  AllTrue[hdParallel["HighEnergyCheck"]["Samples"],
    !KeyExistsQ[#, "SampleCompleted"] && !KeyExistsQ[#, "CheckExecution"] &] &&
    AllTrue[hdParallel["Execution"]["HighEnergySampleExecution"], NumericQ[#["ElapsedTime"]] &]];
hdStandalone = ConstructiveDiagrams`Private`highEnergyCheckReport[prH, hdMetadata,
  4, 192681, hdParameters, 60, Automatic];
check["standalone serial diagnostics use the identical sample plan and aggregation",
  hdReportSemantic[hdStandalone] === hdReportSemantic[hdSerial["HighEnergyCheck"]]];
hdCompleted = (Join[#, <|"SampleCompleted" -> True|>] & /@ hdSerial["HighEnergyCheck"]["Samples"]);
hdAggregatePlan = ConstructiveDiagrams`Private`highEnergyCheckPlan[prH, hdMetadata,
  4, 192681, hdParameters, 60, Automatic];
check["out-of-order worker replies are restored to original sample order",
  hdReportSemantic[ConstructiveDiagrams`Private`highEnergyCheckReportFromPlan[hdAggregatePlan,
    Reverse[hdCompleted]]] === hdReportSemantic[hdStandalone]];
hdIncomplete = ConstructiveDiagrams`Private`highEnergyCheckReportFromPlan[
  hdAggregatePlan, Most[hdCompleted]];
check["three passed samples cannot stand in for four requested samples",
  hdIncomplete["Status"] === "UNRESOLVED" && hdIncomplete["CompletedSamples"] === 3 &&
    MissingQ[hdIncomplete["ObservedPowerUpperBound"]]];
hdDuplicate = ConstructiveDiagrams`Private`highEnergyCheckReportFromPlan[
  hdAggregatePlan, Append[Most[hdCompleted], First[hdCompleted]]];
check["duplicate sample indices cannot falsely satisfy complete coverage",
  hdDuplicate["Status"] === "UNRESOLVED"];
hdTiny = CheckAmplitudes[prH, prChannels, prFixtureModel,
  Sequence @@ DeleteCases[hdSettings, HoldPattern["HighEnergyTimeConstraint" -> _]],
  "HighEnergyTimeConstraint" -> 0.000001, "Parallelization" -> True];
check["a tiny shared public deadline remains unresolved without partial PASS",
  hdTiny["HighEnergyCheck"]["Status"] === "UNRESOLVED" &&
    hdTiny["HighEnergyCheck"]["CompletedSamples"] === 0 &&
    MissingQ[hdTiny["HighEnergyCheck"]["ObservedPowerUpperBound"]] &&
    hdTiny["Execution"]["HighEnergyCompletedSampleJobs"] === 0];
hdShared = Internal`InheritedBlock[{ConstructiveDiagrams`Private`highEnergyOneSample},
  Clear[ConstructiveDiagrams`Private`highEnergyOneSample];
  ConstructiveDiagrams`Private`highEnergyOneSample[___] := (Pause[0.03];
    <|"Status" -> "PASS", "ObservedPower" -> -1, "ObservedPowerUpperBound" -> -1|>);
  ConstructiveDiagrams`Private`highEnergyCheckReport[prH, hdMetadata,
    4, 192681, hdParameters, 0.05, Automatic]];
check["standalone samples do not restart the time budget for each direction",
  hdShared["Status"] === "UNRESOLVED" && hdShared["CompletedSamples"] < 4];
hdPreconditioning = Internal`InheritedBlock[{ConstructiveDiagrams`Private`highEnergyRegroupTerms},
  Clear[ConstructiveDiagrams`Private`highEnergyRegroupTerms];
  ConstructiveDiagrams`Private`highEnergyRegroupTerms[___] := (Pause[0.03]; <|"Expression" -> prH|>);
  ConstructiveDiagrams`Private`highEnergyCheckReport[prH, hdMetadata,
    4, 192681, hdParameters, 0.01, Automatic]];
check["preconditioning consumes the same deadline instead of receiving a separate sample budget",
  hdPreconditioning["Status"] === "UNRESOLVED" && hdPreconditioning["CompletedSamples"] === 0];
check["the high-energy deadline policy is recorded truthfully in details",
  hdParallel["Execution"]["HighEnergyTimeBudgetScope"] === "SharedPreconditioningAndAllSamples"];
CloseKernels[ConstructiveDiagrams`Private`$amplitudeParallelKernels];
Print["Checks: ", checks, "; failures: ", Length[failures]];
If[failures =!= {}, Print[failures]; Quit[1]];
Print["HEPCAT_TEST_COMPLETED:high-energy-dispatch-tests.wl"]; Quit[0];
