(* Parallel exact sample construction leaves simultaneous fitting unchanged. *)
root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Global`$HEPCATpath = FileNameJoin[{root, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
Get[FileNameJoin[{root, "tests", "QED-5-point-residues.wl"}]];
checks = 0; failures = {};
check[name_, value_] := (checks++; If[!TrueQ[value],
  AppendTo[failures, name]; Print["FAIL: ", name]]);
pcRules = ConstructiveDiagrams`Private`residueParameterRules[prFixtureModel,
  {prElectronMass -> 1, prElectricCharge -> 1}];
pcNormalized = ConstructiveDiagrams`Private`completionNormalizeInput[
  prH, prChannels, prFixtureModel];
pcVariables = {Unique["parallelFitA"], Unique["parallelFitB"]};
pcBasis = {prH, First[List @@ Expand[prH]]};
pcTrial = 2 prH + pcVariables.pcBasis;
pcSettings[parallel_] := <|"Parallelization" -> parallel, "MaxKernels" -> 2|>;
pcConstraints[parallel_] := Block[
  {ConstructiveDiagrams`Private`$completionParallelSettings = pcSettings[parallel]},
  ConstructiveDiagrams`Private`completionResidueConstraints[pcTrial, pcVariables,
    pcNormalized, 2, 829017, pcRules, 60, Automatic, False]];
check["both completion APIs expose the same worker controls",
  AllTrue[{CompleteFactorization, CompleteHighEnergy},
    ("Parallelization" /. Options[#]) === Automatic &&
      ("MaxKernels" /. Options[#]) === Automatic &]];
pcSerial = pcConstraints[False];
pcParallel = pcConstraints[True];
check["serial and parallel constraints complete every channel and sample",
  pcSerial["Status"] === "COMPLETE" && pcParallel["Status"] === "COMPLETE" &&
    Length[pcParallel["Samples"]] === 2 Length[pcNormalized["Targets"]]];
check["independent jobs return the identical exact simultaneous equations",
  pcSerial["Equations"] === pcParallel["Equations"]];
check["fit samples preserve the original channel and sample ordering",
  (KeyTake[#, {"Cut", "Sample"}] & /@ pcSerial["Samples"]) ===
    (KeyTake[#, {"Cut", "Sample"}] & /@ pcParallel["Samples"])];
pcChartData[samples_List] := Map[Function[entry, With[{chart = entry["Chart"]},
  KeyTake[chart, {"Momenta", "AngleBasis", "SquareBasis", "References", "Masses",
    "MasslessBranchChoices", "MasslessLittleGroupScales", "Cut", "Complement",
    "CutMass", "ReferenceSeed"}] /. chart["PoleVariable"] -> parallelTestPole]], samples];
check["workers use exactly the original deterministic cut charts",
  pcChartData[pcSerial["Samples"]] === pcChartData[pcParallel["Samples"]]];
check["a requested two-worker fit uses two real workers",
  pcParallel["Execution"]["Mode"] === "Parallel" &&
    pcParallel["Execution"]["KernelCount"] === 2];
pcSolution = <|"SolutionRules" -> Thread[pcVariables -> {-1, 0}], "FreeParameters" -> {}|>;
pcValidate[parallel_, solution_] := Block[
  {ConstructiveDiagrams`Private`$completionParallelSettings = pcSettings[parallel]},
  ConstructiveDiagrams`Private`completionSparseValidation[pcTrial, pcVariables,
    solution, {}, {}, pcNormalized, 2, 1829020, pcRules, 60, Automatic, False]];
pcSerialValidation = pcValidate[False, pcSolution];
pcParallelValidation = pcValidate[True, pcSolution];
check["held-out validation checks all original targets and coherent spins",
  pcParallelValidation["FamilyPass"] === True &&
    pcParallelValidation["Feedback"]["Status"] === "COMPLETE" &&
    Counts[Lookup[pcParallelValidation["Report"]["Channels"], "Status"]] ===
      <|"PASS" -> Length[pcNormalized["Targets"]]|>];
check["held-out feedback retains identical raw equations for later refinements",
  pcSerialValidation["Feedback"]["Equations"] ===
    pcParallelValidation["Feedback"]["Equations"]];
check["held-out charts and sample seeds do not depend on scheduling",
  pcChartData[pcSerialValidation["Feedback"]["Samples"]] ===
    pcChartData[pcParallelValidation["Feedback"]["Samples"]] &&
  Lookup[pcSerialValidation["Feedback"]["Samples"], "RandomSeed"] ===
    Lookup[pcParallelValidation["Feedback"]["Samples"], "RandomSeed"]];
check["zero-fitted directions remain in the raw validation constraints",
  !FreeQ[pcParallelValidation["Feedback"]["Equations"], Last[pcVariables]]];
pcWrong = pcValidate[True, <|"SolutionRules" -> Thread[pcVariables -> {0, 0}],
  "FreeParameters" -> {}|>];
check["parallel validation rejects a wrong amplitude while retaining refinement equations",
  pcWrong["FamilyPass"] === False &&
    MemberQ[Lookup[pcWrong["Report"]["Channels"], "Status"], "FAIL"] &&
    pcWrong["Feedback"]["Status"] === "COMPLETE" &&
    pcWrong["Feedback"]["Equations"] === pcParallelValidation["Feedback"]["Equations"]];
pcPublic[parallel_] := CompleteFactorization[2 prH, prChannels, prFixtureModel,
  "CorrectionBasis" -> {prH}, "ChannelReferences" -> None,
  "BoseSymmetryGroups" -> {}, "Samples" -> 1, "ValidationSamples" -> 1,
  "ParameterRules" -> pcRules, "ValidationParameterRules" -> {pcRules},
  "Parallelization" -> parallel, "MaxKernels" -> 2,
  "TimeConstraint" -> 60, "ProgressReporting" -> False, "ReturnDetails" -> True];
pcSerialPublic = pcPublic[False]; pcParallelPublic = pcPublic[True];
check["public completion preserves the exactly fitted amplitude and all channel checks",
  pcSerialPublic["Status"] === "COMPLETE" && pcParallelPublic["Status"] === "COMPLETE" &&
    Together[pcSerialPublic["Amplitude"] - pcParallelPublic["Amplitude"]] === 0 &&
    Together[pcParallelPublic["Amplitude"] - prH] === 0 &&
    AllTrue[pcParallelPublic["ValidationReport"]["Channels"], #["Status"] === "PASS" &]];
check["disabling public parallelism also keeps internal checks on the master",
  pcSerialPublic["InitialReport"]["Execution"]["Mode"] === "Serial" &&
    pcSerialPublic["ConstraintData"]["Fit"]["Execution"]["Mode"] === "Serial"];
check["invalid factorization worker counts are rejected before fitting",
  Quiet[CompleteFactorization[prH, prChannels, prFixtureModel, "MaxKernels" -> 0]] === $Failed];
check["invalid high-energy worker choices are rejected before checking",
  FailureQ[CompleteHighEnergy[prH, prChannels, prFixtureModel, "Parallelization" -> "invalid"]]];
(* Completion callbacks may arrive in a different order from source jobs. *)
pcProgressEvents = {};
Block[{ConstructiveDiagrams`Private`$completionParallelSettings = pcSettings[True]},
  ConstructiveDiagrams`Private`completionResidueConstraints[pcTrial, pcVariables,
    pcNormalized, 1, 929017, pcRules, 60,
    Function[event, AppendTo[pcProgressEvents, event]], True]];
pcSampleEvents = Select[pcProgressEvents, Lookup[#, "Stage", None] === "Factorization samples" &];
check["parallel fit progress counts completed samples independently of their source indices",
  Lookup[pcSampleEvents, "Completed"] === Range[Length[pcNormalized["Targets"]]] &&
    Sort[Lookup[pcSampleEvents, "Cut"]] === Sort[Lookup[pcNormalized["Targets"], "Cut"]]];
pcProgressEvents = {};
Block[{ConstructiveDiagrams`Private`$completionParallelSettings = pcSettings[True]},
  ConstructiveDiagrams`Private`completionSparseValidation[pcTrial, pcVariables,
    pcSolution, {}, {}, pcNormalized, 1, 1929020, pcRules, 60,
    Function[event, AppendTo[pcProgressEvents, event]], True]];
pcSampleEvents = Select[pcProgressEvents,
  Lookup[#, "Stage", None] === "Validating sparse factorization samples" &];
check["parallel held-out progress also counts completions monotonically",
  Lookup[pcSampleEvents, "Completed"] === Range[Length[pcNormalized["Targets"]]] &&
    Sort[Lookup[pcSampleEvents, "Cut"]] === Sort[Lookup[pcNormalized["Targets"], "Cut"]]];

pcComponents = ConstructiveDiagrams`Private`completionLinearComponents[pcTrial, pcVariables, False];
pcExpired = Quiet[ConstructiveDiagrams`Private`completionConstraintSample[
  pcComponents, pcVariables, ConstructiveDiagrams`Private`completionTargets[pcNormalized["Targets"]],
  {1, 1}, 829017, pcRules, 60, AbsoluteTime[] - 1]];
check["a sample that missed the shared deadline is unresolved rather than accepted",
  AssociationQ[pcExpired] && pcExpired["Status"] === "UNRESOLVED" &&
    !KeyExistsQ[pcExpired, "Equations"]];

(* Independent identity proofs share the same bounded worker queue. These are
   ordinary external conservation identities, with one genuine local direction
   deliberately retained. No sampled zero is accepted as an identity proof. *)
pcIdentityRules = pcNormalized["ExternalMassRules"];
pcIdentityParameters = Table[Unique["parallelIdentity"], {5}];
pcIdentityDirections = Append[Table[Total[MomProd[leg, #] & /@ Range[5]],
  {leg, 4}], 1];
pcIdentityFamily = 7 + pcIdentityParameters.pcIdentityDirections;
pcIdentityResult = Block[
  {ConstructiveDiagrams`Private`$completionParallelSettings = pcSettings[True]},
  ConstructiveDiagrams`Private`completionHEPruneIdentityFamily[
    pcIdentityFamily, pcIdentityParameters, pcIdentityRules, prFixtureModel, 40]];
check["independent identity certificates use real parallel workers",
  pcIdentityResult["Execution"]["Mode"] === "Parallel" &&
    pcIdentityResult["Execution"]["KernelCount"] === 2];
check["only exact conservation identities are removed, in original parameter order",
  pcIdentityResult["RemovedParameters"] === Most[pcIdentityParameters] &&
    pcIdentityResult["FreeParameters"] === {Last[pcIdentityParameters]} &&
    pcIdentityResult["RemainingBasis"] === {1} &&
    pcIdentityResult["FamilyAmplitude"] === 7 + Last[pcIdentityParameters] &&
    Lookup[pcIdentityResult["Certificates"], "Parameter"] === pcIdentityParameters &&
    AllTrue[Most[pcIdentityResult["Certificates"]], #["Status"] === "PROVED_ZERO" &&
      Lookup[#, "NumericalKinematicSamples", 0] === 0 &&
      Lookup[#, "InternalOnShellConditions", {}] === {} &]];
pcTailParameters = Take[pcIdentityParameters, 3];
pcTailDirections = Join[Take[pcIdentityDirections, 2], {1}];
pcTailEvents = {};
pcTailResult = Block[
  {ConstructiveDiagrams`Private`$completionParallelSettings = pcSettings[Automatic]},
  ConstructiveDiagrams`Private`completionHEPruneIdentityFamily[
    7 + pcTailParameters.pcTailDirections, pcTailParameters, pcIdentityRules,
    prFixtureModel, 40, pcTailDirections,
    Function[event, AppendTo[pcTailEvents, event]]]];
check["a short automatic proof tail reuses the warm parallel worker pool",
  pcTailResult["Execution"]["Mode"] === "Parallel" &&
    pcTailResult["Execution"]["KernelCount"] === 2];
check["a short parallel proof tail preserves genuine freedom and exact zero certificates",
  pcTailResult["RemovedParameters"] === Most[pcTailParameters] &&
    pcTailResult["FreeParameters"] === {Last[pcTailParameters]} &&
    pcTailResult["RemainingBasis"] === {1} &&
    pcTailResult["FamilyAmplitude"] === 7 + Last[pcTailParameters]];
check["proof progress counts every completed, certified and retained direction",
  Lookup[pcTailEvents, "Completed"] === Range[3] &&
    Lookup[pcTailEvents, "Total"] === {3, 3, 3} &&
    OrderedQ[Lookup[pcTailEvents, "ProvedZero"]] &&
    AllTrue[pcTailEvents, #["Completed"] === #["ProvedZero"] + #["RetainedDirections"] &] &&
    Last[pcTailEvents]["ProvedZero"] === 2 &&
    Last[pcTailEvents]["RetainedDirections"] === 1];
(* Supply a deterministic first-batch snapshot whose two prerequisites were
   independently proved above. Only that snapshot is simulated: the remainder
   proofs and their second dispatch run on the real warm worker pool. *)
pcSpanParameters = Table[Unique["parallelSpan"], {4}];
pcSpanZeros = Take[pcIdentityDirections, 2];
pcSpanTarget = 2 First[pcSpanZeros] + 3 Last[pcSpanZeros];
pcSpanDirections = Join[pcSpanZeros, {pcSpanTarget, pcSpanTarget + 1}];
pcSpanMapCalls = 0; pcSpanEvents = {};
DownValues[pcRealParallelMap] = DownValues[ConstructiveDiagrams`Private`amplitudeParallelMap] /.
  ConstructiveDiagrams`Private`amplitudeParallelMap -> pcRealParallelMap;
pcSpanResult = Block[
  {ConstructiveDiagrams`Private`$completionParallelSettings = pcSettings[Automatic],
   ConstructiveDiagrams`Private`amplitudeParallelMap =
     Function[{worker, jobs, settings, callback},
       pcSpanMapCalls++;
       If[pcSpanMapCalls === 1,
         pcSpanInitialResults = Table[<|"Index" -> index,
           "Parameter" -> pcSpanParameters[[index]], "Certificate" ->
             If[index <= 2, KeyDrop[pcIdentityResult["Certificates"][[index]], "Parameter"],
               <|"Status" -> "NOT_PROVED", "Method" -> "TimeConstraint"|>]|>, {index, jobs}];
         If[callback =!= None, MapIndexed[
           callback[#1, First[#2], Length[pcSpanInitialResults]] &, pcSpanInitialResults]];
         <|"Results" -> pcSpanInitialResults, "Execution" -> <|"Mode" -> "InitialFixture"|>|>,
         pcRealParallelMap[worker, jobs, settings, callback]]]},
  ConstructiveDiagrams`Private`completionHEPruneIdentityFamily[
    7 + pcSpanParameters.pcSpanDirections, pcSpanParameters, pcIdentityRules,
    prFixtureModel, 30, pcSpanDirections,
    Function[event, AppendTo[pcSpanEvents, event]]]];
check["certified-span remainder proofs reuse the real warm parallel worker pool",
  pcSpanMapCalls === 2 &&
    pcSpanResult["CertifiedZeroSpanRetry"]["Execution"]["Mode"] === "Parallel" &&
    pcSpanResult["CertifiedZeroSpanRetry"]["Execution"]["KernelCount"] === 2];
check["parallel certified-span retries prove zero while preserving a genuine contact direction",
  pcSpanResult["RemovedParameters"] === Most[pcSpanParameters] &&
    pcSpanResult["FreeParameters"] === {Last[pcSpanParameters]} &&
    pcSpanResult["RemainingBasis"] === {Last[pcSpanDirections]} &&
    pcSpanResult["Certificates"][[3]]["Method"] === "CertifiedZeroSpanRemainder" &&
    pcSpanResult["Certificates"][[4]]["Status"] === "NOT_PROVED"];
pcSpanRetryEvents = Select[pcSpanEvents,
  Lookup[#, "Operation", None] === "Reusing certified ordinary identities" &];
check["warm certified-span retry progress reports complete proof and retention counts",
  Lookup[pcSpanRetryEvents, "Completed"] === {1, 2} &&
    Lookup[pcSpanRetryEvents, "Total"] === {2, 2} &&
    Last[pcSpanRetryEvents]["ProvedZero"] === 1 &&
    Last[pcSpanRetryEvents]["RetainedDirections"] === 1];
pcIdentityExpired = Block[
  {ConstructiveDiagrams`Private`$completionParallelSettings = pcSettings[True]},
  ConstructiveDiagrams`Private`completionHEPruneIdentityFamily[
    pcIdentityFamily, pcIdentityParameters, pcIdentityRules, prFixtureModel, 0]];
check["an exhausted parallel proof budget preserves the entire affine family",
  pcIdentityExpired["FreeParameters"] === pcIdentityParameters &&
    pcIdentityExpired["RemovedParameters"] === {} &&
    Expand[pcIdentityExpired["FamilyAmplitude"] - pcIdentityFamily] === 0];
Print["CHECKS: ", checks, "; FAILURES: ", Length[failures]];
If[failures =!= {}, Print[failures]];
CloseKernels[ConstructiveDiagrams`Private`$amplitudeParallelKernels];
If[failures === {}, Print["HEPCAT_TEST_COMPLETED:parallel-completion-tests.wl"]];
Quit[If[failures === {}, 0, 1]];
