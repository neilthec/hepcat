(* A completed amplitude carries external masses, never diagram-local cuts. *)
root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Global`$HEPCATpath = FileNameJoin[{root, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
checks = 0; failures = {};
SetAttributes[check, HoldRest];
check[name_, value_] := (checks++;
  If[!TrueQ[value], AppendTo[failures, name]; Print["FAIL: ", name]]);
cpZero[value_] := Together[value] === 0;
cpModel = {{{"Scalar", "s", "s", 1, 0, 0, 0, 1, 1, "", ""}}, {}, {}, {}};
cpExternal = {Mass[1] -> cpMassA, Mass[2] -> cpMassB, Mass[3] -> 0, Mass[4] -> 0};
cpDen = PropDen[Mom[Multiparticle[1, 3]], cpMassA];
(* The external masses make this exact off-cut definition D13=2 p1.p3.
   It is not the diagram-local shell substitution p1.p3=0. *)
cpExternalZero[value_] := cpZero[value /. MomProd[1, 3] -> cpDen/2];
cpChain = SpinorChain[Spinor["Spin", "Angle", 2], Mom[1], Mom[1],
  Spinor["Helicity", "Angle", 4]];
cpBracket = SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Helicity", "Angle", 4]];
cpPair = {cpExternal, (cpChain + MomProd[1, 3])/cpDen};
cpSaved = cpPair;
cpPrepare[input_, opts___] := PrepareAmplitudes[input, cpModel, opts,
  "MaxSteps" -> 0, "ProgressReporting" -> False];
cpPrepared = cpPrepare[cpPair];
cpDetails = cpPrepare[cpPair, "ReturnDetails" -> True];
check["a single amplitude pair returns the same single-pair format",
  MatchQ[cpPrepared, {_List, _}] && First[cpPrepared] === cpExternal &&
    cpExternalZero[Last[cpPrepared] - (cpMassA^2 cpBracket + MomProd[1, 3])/cpDen]];
check["completed amplitude preparation applies ordinary external mass identities",
  FreeQ[Last[cpPrepared], Mom[1]] && !FreeQ[Last[cpPrepared], cpMassA^2] &&
    cpZero[Last[cpPrepared] - cpMassA^2 cpBracket/cpDen - 1/2]];
check["existing physical propagators do not introduce numerator shell rules",
  cpDetails["Options"]["InternalOnShell"] === False &&
    First[cpDetails["ItemReports"]]["InternalOnShellChannels"] === {} &&
    First[cpDetails["ItemReports"]]["InternalOnShellConditions"] === {} &&
    First[cpDetails["RemainderData"]]["MassRules"] === cpExternal &&
    !FreeQ[Last[cpPrepared], cpDen]];
check["detailed single-pair preparation retains normalized reusable item lists",
  cpDetails["InputForm"] === "SinglePair" && cpDetails["InputType"] === "Amplitude" &&
    cpDetails["OriginalAmplitudes"] === {cpPair} &&
    cpDetails["PreparedAmplitudes"] === {cpPrepared} &&
    cpDetails["ExternalMassRules"] === cpExternal];
check["ordinary output and detailed amplitude agree without changing the input",
  cpDetails["Amplitude"] === Last[cpPrepared] && cpPair === cpSaved &&
    cpPrepare[cpPair, "Output" -> "Expression"] === Last[cpPrepared]];
cpPreparedList = cpPrepare[{cpPair}];
check["list-of-pairs output keeps its established outer list",
  MatchQ[cpPreparedList, {{_List, _}}] && First[First[cpPreparedList]] === cpExternal &&
    cpExternalZero[Last[First[cpPreparedList]] - Last[cpPrepared]] && cpPrepare[{}] === {}];
check["the compatibility preparation name accepts a single pair too",
  PrepareDiagramAmplitudes[cpPair, cpModel, "MaxSteps" -> 0,
    "ProgressReporting" -> False] === cpPrepared];
cpRequestedShell = cpPrepare[cpPair, "InternalOnShell" -> True, "ReturnDetails" -> True];
check["an external-only pair cannot acquire an internal shell through an option",
  cpRequestedShell["Options"]["InternalOnShell"] === False &&
    cpRequestedShell["PreparedAmplitudes"] === {cpPrepared}];
cpDiagramRules = Append[cpExternal, Mass[Multiparticle[1, 3]] -> cpMassA];
cpDiagram = cpPrepare[{cpDiagramRules, MomProd[1, 3]/cpDen}, "ReturnDetails" -> True];
check["a single genuine diagram still uses its explicitly supplied internal shell",
  cpDiagram["InputType"] === "Diagrams" && cpDiagram["Options"]["InternalOnShell"] &&
    cpDiagram["Amplitude"] === 0 &&
    First[cpDiagram["ItemReports"]]["InternalOnShellChannels"] === {{1, 3}}];
cpNested = cpPrepare[{cpExternal, MomProd[1, 3]/(cpDen (5 + MomProd[1, 3]))}];
check["ordinary virtuality cancellation keeps the auxiliary inverse off shell",
  cpExternalZero[Last[cpNested] - MomProd[1, 3]/(cpDen (5 + MomProd[1, 3]))] &&
    !FreeQ[Last[cpNested], 1/(5 + MomProd[1, 3])] && Last[cpNested] =!= 0];

cpReference[id_, leg_] :=
  SpinorChain[Spinor["Xi", "Angle", id], Mom[2], Spinor["Helicity", "Square", leg]]/
    SpinorChain[Spinor["Xi", "Angle", id], Spinor["Helicity", "Angle", leg]];
cpPhysical = cpPrepare[{cpExternal, cpReference[71, 3]/cpDen},
  "ReferenceSpinors" -> "AllPhysical", "ReturnDetails" -> True];
check["a completed amplitude can choose an existing physical pole as its reference",
  cpPhysical["Status"] === "PREPARED" &&
    FreeQ[cpPhysical["Amplitude"], Spinor["Xi", ___]] &&
    First[cpPhysical["ItemReports"]]["ReferenceSpinors"]["Assignments"][[1]]["Target"] === 1];
check["physical reference inventory remains separate from actual external mass rules",
  cpPhysical["ExternalMassRules"] === cpExternal &&
    First[cpPhysical["RemainderData"]]["MassRules"] === cpExternal &&
    First[cpPhysical["ItemReports"]]["InternalOnShellChannels"] === {}];
cpRulesFive = Append[cpExternal, Mass[5] -> 0];
cpShared = {cpRulesFive, cpReference[0, 3] cpReference[0, 4]/cpDen};
cpSharedSpurious = cpPrepare[cpShared, "ReferenceSpinors" -> "AllSpurious", "ReturnDetails" -> True];
check["one shared Xi is uniformly replaced by one admissible external spinor",
  cpSharedSpurious["Status"] === "PREPARED" &&
    FreeQ[cpSharedSpurious["Amplitude"], Spinor["Xi", ___]] &&
    DeleteDuplicates[Lookup[First[cpSharedSpurious["ItemReports"]]["ReferenceSpinors"]["Assignments"],
      "Target"]] === {5}];
cpSharedAllPhysical = Quiet[cpPrepare[cpShared, "ReferenceSpinors" -> "AllPhysical"]];
check["an unsafe per-leg physical replacement of a shared Xi is reported as unavailable",
  FailureQ[cpSharedAllPhysical] && First[cpSharedAllPhysical] === "AmplitudeReferences"];
cpAngle[a_, b_] := SpinorChain[a, b];
cpXiOnly = Spinor["Xi", "Angle", 87];
cpA[leg_] := Spinor["Helicity", "Angle", leg];
cpXiSchouten = cpAngle[cpXiOnly, cpA[3]] cpAngle[cpA[4], cpA[5]] -
  cpAngle[cpXiOnly, cpA[4]] cpAngle[cpA[3], cpA[5]] +
  cpAngle[cpXiOnly, cpA[5]] cpAngle[cpA[3], cpA[4]];
cpXiSchoutenPrepared = cpPrepare[{cpRulesFive, cpXiSchouten}, "ReturnDetails" -> True];
check["an Xi without a reciprocal reference bracket does not block ordinary Schouten reduction",
  cpXiSchoutenPrepared["Status"] === "PREPARED" && cpXiSchoutenPrepared["Amplitude"] === 0 &&
    First[cpXiSchoutenPrepared["ItemReports"]]["InternalOnShellChannels"] === {}];
cpUnassigned = cpPrepare[{cpExternal, cpAngle[cpXiOnly, cpA[3]]}, "ReturnDetails" -> True];
check["an unassigned literal Xi is preserved uniformly instead of guessed or split",
  cpUnassigned["Status"] === "PREPARED" &&
    cpUnassigned["Amplitude"] === cpAngle[cpXiOnly, cpA[3]] &&
    First[cpUnassigned["ItemReports"]]["ReferenceSpinors"]["Assignments"][[1]]["Target"] === cpXiOnly];
cpFeynmanModel = FeynmanModel @@ cpModel;
cpFeynmanPair = {cpExternal, cpReference[0, 3] cpReference[0, 4]/cpDen};
cpFeynmanSingle = PrepareAmplitudes[cpFeynmanPair, cpFeynmanModel,
  "MaxSteps" -> 0, "TimeConstraint" -> 3, "ProgressReporting" -> False, "ReturnDetails" -> True];
check["a single external-only amplitude preserves Xi correlations with a Feynman model too",
  First[cpFeynmanSingle["ItemReports"]]["ReferenceSpinors"]["Expression"] === Last[cpFeynmanPair] &&
    DeleteDuplicates[Lookup[First[cpFeynmanSingle["ItemReports"]]["ReferenceSpinors"]["Assignments"],
      "Mode"]] === {"PreservedXi"} && !cpFeynmanSingle["Options"]["InternalOnShell"]];
cpFeynmanUnsafe = Quiet[PrepareAmplitudes[cpFeynmanPair, cpFeynmanModel,
  "ReferenceSpinors" -> "AllPhysical", "MaxSteps" -> 0, "ProgressReporting" -> False]];
check["a Feynman model does not permit splitting a completed amplitude's shared Xi",
  FailureQ[cpFeynmanUnsafe] && First[cpFeynmanUnsafe] === "FeynmanReferences"];

(* Actual cubic diagrams, not a supplied correction basis, supply a completed
   four-point tree. Re-preparation may simplify it but cannot assume its lines
   are on shell merely because their propagators occur in the expression. *)
cpCubicModel = {{{"Scalar", "s", "s", 1, 0, cpMass, 0, 1, 1, "", ""}},
  {{"s", "s", "s", "", cpCoupling, 1}}, {}, {}};
cpCubicPairs = diagramAmplitudes[CreateDiagrams[ConstantArray["s", 4], cpCubicModel]];
cpCubicChannels = FactorizationAmplitudeChannels[ConstantArray["s", 4], cpCubicModel];
cpTree = Total[Last /@ cpCubicPairs];
cpCompleted = CompleteFactorization[cpTree, cpCubicChannels, cpCubicModel,
  "Samples" -> 1, "ValidationSamples" -> 1, "ParameterRules" -> {cpMass -> 1, cpCoupling -> 2},
  "ProgressReporting" -> False, "TimeConstraint" -> 30];
check["factorization returns a directly preparable external-mass pair",
  MatchQ[cpCompleted, {_List, _}] && Length[First[cpCompleted]] === 4 &&
    AllTrue[First[cpCompleted], MatchQ[#, HoldPattern[Mass[_Integer] -> _]] &]];
cpCompletedPrepared = PrepareAmplitudes[cpCompleted, cpCubicModel,
  "MaxSteps" -> 0, "ProgressReporting" -> False, "ReturnDetails" -> True];
cpCompletedCheck = CheckAmplitudes[First[cpCompletedPrepared["PreparedAmplitudes"]],
  cpCubicChannels, cpCubicModel, "Samples" -> 2,
  "ParameterRules" -> {cpMass -> 2, cpCoupling -> 3}, "SpuriousPoles" -> {},
  "HighEnergyCheck" -> False, "ReferenceCheck" -> False, "Output" -> "Details"];
check["re-preparing completion preserves all scalar-tree physical residues",
  cpCompletedPrepared["Options"]["InternalOnShell"] === False &&
    cpCompletedCheck["Summary"] === <|"PASS" -> 3|> &&
    cpZero[cpCompletedPrepared["Amplitude"] - Last[cpCompleted]]];
cpHighEnergy = CompleteHighEnergy[cpCompleted, cpCubicChannels, cpCubicModel,
  "Samples" -> 1, "ValidationSamples" -> 1,
  "ParameterRules" -> {cpMass -> 1, cpCoupling -> 2},
  "ProgressReporting" -> False, "TimeConstraint" -> 30];
check["high-energy completion accepts and returns an external-mass pair",
  MatchQ[cpHighEnergy, {_List, _}] && First[cpHighEnergy] === First[cpCompleted] &&
    AllTrue[First[cpHighEnergy], MatchQ[#, HoldPattern[Mass[_Integer] -> _]] &]];
cpHighEnergyChannels = PrepareAmplitudes[cpCubicChannels, cpCubicModel,
  "ReturnDetails" -> True, "MaxSteps" -> 0, "ProgressReporting" -> False];
cpHighEnergyFromChannels = CompleteHighEnergy[cpCompleted, cpHighEnergyChannels, cpCubicModel,
  "Samples" -> 1, "ValidationSamples" -> 1,
  "ParameterRules" -> {cpMass -> 1, cpCoupling -> 2},
  "ProgressReporting" -> False, "TimeConstraint" -> 30];
check["high-energy completion accepts prepared channel reports as well as ordinary lists",
  MatchQ[cpHighEnergyFromChannels, {_List, _}] &&
    First[cpHighEnergyFromChannels] === First[cpHighEnergy] &&
    cpZero[Last[cpHighEnergyFromChannels] - Last[cpHighEnergy]]];
cpChangedChannelReport = Join[cpHighEnergyChannels, <|"PreparedAmplitudes" ->
  (ReplacePart[#, -1 -> 0] & /@ cpHighEnergyChannels["PreparedAmplitudes"])|>];
cpHighEnergyOriginalTargets = CompleteHighEnergy[cpCompleted, cpChangedChannelReport, cpCubicModel,
  "Samples" -> 1, "ValidationSamples" -> 1,
  "ParameterRules" -> {cpMass -> 1, cpCoupling -> 2},
  "ProgressReporting" -> False, "TimeConstraint" -> 30];
check["high-energy channel preparation keeps original channel expressions as residue targets",
  MatchQ[cpHighEnergyOriginalTargets, {_List, _}] &&
    First[cpHighEnergyOriginalTargets] === First[cpHighEnergy] &&
    cpZero[Last[cpHighEnergyOriginalTargets] - Last[cpHighEnergy]]];
cpInvalidChannelReport = CompleteHighEnergy[cpCompleted,
  <|"PreparedAmplitudes" -> cpCubicChannels|>, cpCubicModel];
check["malformed high-energy channel reports identify the invalid second argument",
  FailureQ[cpInvalidChannelReport] && First[cpInvalidChannelReport] === "InvalidCompletionArguments" &&
    cpInvalidChannelReport[[2]]["ArgumentCount"] === 3 &&
    cpInvalidChannelReport[[2]]["ArgumentHeads"] === {List, Association, List} &&
    cpInvalidChannelReport[[2]]["ArgumentLengths"] === {2, 1, 4} &&
    cpInvalidChannelReport[[2]]["InvalidArgumentPositions"] === {2}];
cpInvalidHighEnergyModel = CompleteHighEnergy[cpCompleted, cpCubicChannels, FeynmanModel @@ cpCubicModel];
check["unsupported high-energy model heads identify the invalid third argument",
  FailureQ[cpInvalidHighEnergyModel] && First[cpInvalidHighEnergyModel] === "InvalidCompletionArguments" &&
    cpInvalidHighEnergyModel[[2]]["ArgumentHeads"] === {List, List, FeynmanModel} &&
    cpInvalidHighEnergyModel[[2]]["InvalidArgumentPositions"] === {3}];
cpHighEnergyPrepared = PrepareAmplitudes[cpHighEnergy, cpCubicModel,
  "MaxSteps" -> 0, "ProgressReporting" -> False];
check["the final high-energy result can be prepared directly without internal shells",
  MatchQ[cpHighEnergyPrepared, {_List, _}] && First[cpHighEnergyPrepared] === First[cpHighEnergy] &&
    cpZero[Last[cpHighEnergyPrepared] - Last[cpHighEnergy]]];
cpFinalChecks = (CheckAmplitudes[#, cpCubicChannels, cpCubicModel,
  "Samples" -> 1, "ParameterRules" -> {cpMass -> 2, cpCoupling -> 3},
  "SpuriousPoles" -> {}, "HighEnergyCheck" -> False, "ReferenceCheck" -> False,
  "Output" -> "Summary"] & /@ {cpHighEnergyPrepared, Last[cpHighEnergyPrepared]});
check["the checker accepts either the final prepared pair or its bare expression",
  cpFinalChecks === {<|"PASS" -> 3|>, <|"PASS" -> 3|>}];

(* A shared-Xi all-positive QED continuation is known to require that literal
   correlation. Re-preparation must not split it into unrelated per-leg Xi's. *)
Get[FileNameJoin[{root, "tests", "QED-5-point-residues.wl"}]];
cpQEDPairs = diagramAmplitudes[CreateDiagrams[prParticles, prModel]];
cpQEDExternal = ConstructiveDiagrams`Private`preparationExternalRules[First[First[cpQEDPairs]]];
cpQEDContracted = Map[Function[pair,
  ConstructiveDiagrams`Private`preparationNumerators[Last[pair], Function[num,
    ConstructiveDiagrams`Private`preparationReplaceMasses[ReduceSpinContractions[num], First[pair]]]]],
  cpQEDPairs];
cpQEDCommon = Total[cpQEDContracted] /. Spinor["Xi", chirality_, _] :> Spinor["Xi", chirality, 0];
cpQEDPair = {cpQEDExternal, cpQEDCommon};
cpQEDPrepared = PrepareAmplitudes[cpQEDPair, prModel,
  "MaxSteps" -> 0, "TimeConstraint" -> 8, "ProgressReporting" -> False, "ReturnDetails" -> True];
check["default post-completion preparation preserves a shared literal QED Xi",
  DeleteDuplicates[Cases[cpQEDPrepared["Amplitude"], Spinor["Xi", _, id_] :> id, Infinity]] === {0} &&
    First[cpQEDPrepared["ItemReports"]]["InternalOnShellChannels"] === {}];
cpQEDRules = ModelParameterRules[prFixtureModel, {"Me" -> 2, "EE" -> 3}];
cpQEDTargets = ConstructiveDiagrams`Private`completionTargets[
  ConstructiveDiagrams`Private`residueSimpleInputs[cpQEDPair, prChannels, prFixtureModel]["Targets"]];
cpQEDChecks = Table[
  cpChart = ConstructiveDiagrams`Private`amplitudeRationalComparisonChart[
    cpQEDTargets, sample, 832901, cpQEDRules];
  cpZero[ConstructiveDiagrams`Private`residueEvaluateChart[#, cpChart, cpQEDRules]] & /@
    {cpQEDPrepared["Amplitude"] - cpQEDCommon, cpQEDPrepared["Amplitude"] - prH}, {sample, 3}];
check["re-prepared common-Xi QED amplitude agrees with its input and known full amplitude off cut",
  And @@ Flatten[cpQEDChecks]];
check["post-completion preparation retains the actual fixed-reference input as a complexity baseline",
  !ConstructiveDiagrams`Private`preparationBetterQ[
    ConstructiveDiagrams`Private`preparationRank[cpQEDCommon],
    ConstructiveDiagrams`Private`preparationRank[cpQEDPrepared["Amplitude"]]]];
cpQEDPhysical = Quiet[PrepareAmplitudes[cpQEDPair, prModel,
  "ReferenceSpinors" -> "AllPhysical", "MaxSteps" -> 0,
  "ProgressReporting" -> False]];
check["explicit physical post-preparation does not silently decorrelate a shared QED Xi",
  FailureQ[cpQEDPhysical]];

Print[checks, " completed-amplitude-preparation checks; ", Length[failures], " failures."];
If[failures =!= {}, Print[failures]; Quit[1]];
Print["HEPCAT_TEST_COMPLETED:completed-amplitude-preparation-tests.wl"];
Quit[0];
