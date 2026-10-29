(* Complete off-cut Feynman comparisons supplement all existing amplitude checks. *)
root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Global`$HEPCATpath = FileNameJoin[{root, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
If[AnyTrue[{CheckAmplitudes, CheckPhysicalResidues, PrepareAmplitudes,
    FeynmanToSpinorProducts}, DownValues[#] === {} &],
  Print["Amplitude comparison APIs did not load."]; Quit[1]];
checks = 0; failures = {};
SetAttributes[check, HoldRest];
check[name_, value_] := (checks++;
  If[Mod[checks, 20] === 0, Print["Completed ", checks, " amplitude-check checks."]];
  If[!TrueQ[value], AppendTo[failures, name]; Print["FAIL: ", name]]);
qaMassRules[n_Integer] := Thread[(Mass /@ Range[n]) -> ConstantArray[0, n]];
qaPair[expression_, n_Integer : 4] := {{qaMassRules[n], expression}};
qaScalarModel = {{{"Scalar", "s", "s", 1, 0, 0, 0, 1, 1, "", ""}},
  {}, {}, {}};
qaSettings[opts___] := Normal[Join[<|"Samples" -> 2, "RandomSeed" -> 1729,
  "TimeConstraint" -> 60, "SpuriousPoles" -> {}, "HighEnergyCheck" -> False,
  "ReferenceCheck" -> False, "ParameterRules" -> {Me -> 1, EE -> 1},
  "Output" -> "Details"|>, Association[{opts}]]];
qaReport[input_, channels_, model_, benchmark_, opts___] := CheckAmplitudes[
  input, channels, model, Sequence @@ qaSettings[
    "FeynmanAmplitudes" -> benchmark, opts]];
qaStatus[report_] := Lookup[Lookup[report, "FeynmanCheck", <||>], "Status", Missing["NoFeynmanCheck"]];
qaInvalid[value_] := value === $Failed || FailureQ[value];
qaAllPhysicalPass[report_] := AssociationQ[report] &&
  AllTrue[report["Channels"], #["Status"] === "PASS" &];

check["the complete checker is canonical and its former name remains compatible",
  Options[CheckAmplitudes] === Options[CheckPhysicalResidues]];
qaSavedLegacyOptions = Options[CheckPhysicalResidues];
qaSavedCanonicalOptions = Options[CheckAmplitudes];
SetOptions[CheckPhysicalResidues, "HighEnergyCheck" -> False, "ReferenceCheck" -> False];
qaLegacyDefaultsReport = CheckPhysicalResidues[qaPair[1], {}, qaScalarModel,
  "SpuriousPoles" -> {}, "Samples" -> 1, "Output" -> "Details"];
qaCanonicalDefaultsReport = CheckAmplitudes[qaPair[1], {}, qaScalarModel,
  "SpuriousPoles" -> {}, "Samples" -> 1, "Output" -> "Details"];
SetOptions[CheckPhysicalResidues, Sequence @@ qaSavedLegacyOptions];
check["legacy SetOptions affect the compatibility checker without changing canonical defaults",
  !KeyExistsQ[qaLegacyDefaultsReport, "HighEnergyCheck"] &&
    !KeyExistsQ[qaLegacyDefaultsReport, "ReferenceCheck"] &&
    qaLegacyDefaultsReport["Summary"] === <||> &&
    KeyExistsQ[qaCanonicalDefaultsReport, "HighEnergyCheck"] &&
    KeyExistsQ[qaCanonicalDefaultsReport, "ReferenceCheck"] &&
    Options[CheckAmplitudes] === qaSavedCanonicalOptions &&
    Options[CheckPhysicalResidues] === qaSavedLegacyOptions];
check["the Feynman benchmark is optional by default",
  ("FeynmanAmplitudes" /. Options[CheckAmplitudes]) === None &&
    ("FeynmanSamples" /. Options[CheckAmplitudes]) === Automatic &&
    ("FeynmanTimeConstraint" /. Options[CheckAmplitudes]) === Automatic];
qaPlain = qaReport[qaPair[1], {}, qaScalarModel, None];
check["omitting a Feynman benchmark does not add a comparison row",
  !KeyExistsQ[qaPlain, "FeynmanCheck"] &&
    FreeQ[Lookup[qaPlain, "Checks", {}], KeyValuePattern["Type" -> "Feynman"]]];
check["the optional Feynman view stays empty without a supplied benchmark",
  Normal[CheckAmplitudes[qaPair[1], {}, qaScalarModel,
    Sequence @@ qaSettings["Output" -> "Feynman"]]] === {}];
qaOne = qaReport[qaPair[1], {}, qaScalarModel, 1];
check["identical complete expressions pass away from every internal cut",
  qaStatus[qaOne] === "PASS" && qaOne["FeynmanCheck"]["CompletedSamples"] === 2];
check["a comparison without physical channels retains the real center-of-mass chart",
  AllTrue[qaOne["FeynmanCheck"]["Samples"],
    #["Chart"]["SamplingConstruction"] === "ExactRealCenterOfMassOffCut" &]];
check["the Feynman comparison participates in the combined table and summary",
  qaOne["Summary"] === <|"PASS" -> 1|> && qaOne["FeynmanSummary"] === <|"PASS" -> 1|> &&
    Count[Lookup[qaOne["Checks"], "Type"], "Feynman"] === 1 &&
    MemberQ[Lookup[Normal[qaOne["Table"]], "Type"], "Feynman"]];
check["the comparison scope distinguishes full off-cut amplitudes from residues",
  qaOne["FeynmanCheck"]["Scope"] ===
    "SampledOffCutMomentaAllImplicitMassiveSpinComponents"];
check["the recorded Feynman sample values are visible ordinary fields",
  AllTrue[qaOne["FeynmanCheck"]["Samples"], ContainsAll[Keys[#],
    {"Sample", "CalculatedAmplitude", "FeynmanAmplitude", "Difference",
      "OppositeSignDifference", "MatchingSigns"}] &]];
qaFeynmanView = CheckAmplitudes[qaPair[1], {}, qaScalarModel,
  Sequence @@ qaSettings["FeynmanAmplitudes" -> 1, "Output" -> "Feynman"]];
check["the dedicated Feynman view returns every requested comparison sample",
  Length[Normal[qaFeynmanView]] === 2 &&
    AllTrue[Normal[qaFeynmanView], KeyExistsQ[#, "Difference"] &]];
check["the compatibility alias includes the same optional Feynman comparison",
  CheckPhysicalResidues[qaPair[1], {}, qaScalarModel,
    Sequence @@ qaSettings["FeynmanAmplitudes" -> 1, "Output" -> "Summary"]] ===
      qaOne["Summary"]];
qaOpposite = qaReport[qaPair[1], {}, qaScalarModel, -1];
check["a uniform minus sign is reported without changing either amplitude",
  qaStatus[qaOpposite] === "Opp Sign" &&
    qaOpposite["Summary"] === <|"Opp Sign" -> 1|> &&
    AllTrue[qaOpposite["FeynmanCheck"]["Samples"],
      #["CalculatedAmplitude"] === 1 && #["FeynmanAmplitude"] === -1 &&
        #["OppositeSignDifference"] === 0 &]];
check["two zero amplitudes pass rather than being called opposite sign",
  qaStatus[qaReport[qaPair[0], {}, qaScalarModel, 0]] === "PASS"];
check["a zero versus nonzero amplitude fails",
  qaStatus[qaReport[qaPair[0], {}, qaScalarModel, 1]] === "FAIL"];
check["a nonunit normalization is not mistaken for a sign convention",
  qaStatus[qaReport[qaPair[1], {}, qaScalarModel, 2]] === "FAIL"];
check["kinematics-dependent disagreement is not mistaken for a constant ratio",
  qaStatus[qaReport[qaPair[1 + MomProd[1, 3]], {}, qaScalarModel, 1]] === "FAIL"];
Do[
  check["complete comparisons work at general multiplicity " <> ToString[n],
    qaStatus[qaReport[qaPair[1, n], {}, qaScalarModel, 1,
      "Samples" -> 1]] === "PASS"], {n, {5, 6}}];
qaFeynmanPairs = qaPair[1]; qaSavedFeynmanPairs = qaFeynmanPairs;
qaSavedInput = qaPair[1]; qaSavedModel = qaScalarModel;
qaPairReport = qaReport[qaSavedInput, {}, qaScalarModel, qaFeynmanPairs];
check["prepared amplitude pairs are accepted without mutating either input or the model",
  qaStatus[qaPairReport] === "PASS" && qaFeynmanPairs === qaSavedFeynmanPairs &&
    qaSavedInput === qaPair[1] && qaScalarModel === qaSavedModel];
qaBadMassPairs = {{ReplacePart[qaMassRules[4], 1 -> (Mass[1] -> 1)], 1}};
qaBadMass = qaReport[qaPair[1], {}, qaScalarModel, qaBadMassPairs];
check["incompatible benchmark external masses remain unresolved rather than being silently replaced",
  qaStatus[qaBadMass] === "UNRESOLVED"];
qaDeclaredCoupling = Symbol["QATestConstructive`coupling"];
qaForeignCoupling = Symbol["QATestFeynman`coupling"];
qaNamespaceModel = ReplacePart[qaScalarModel,
  3 -> {{qaDeclaredCoupling, 3, "declared constructive coupling"}}];
qaUnmatchedNamespace = qaReport[qaPair[qaDeclaredCoupling], {}, qaNamespaceModel,
  qaForeignCoupling, "Samples" -> 1];
check["an unset same-named parameter in another context is not silently identified with a model parameter",
  With[{declared = qaDeclaredCoupling, foreign = qaForeignCoupling},
    SymbolName[declared] === SymbolName[foreign] && Context[declared] =!= Context[foreign]] &&
    qaStatus[qaUnmatchedNamespace] === "UNRESOLVED"];
qaMatchedNamespace = qaReport[qaPair[qaDeclaredCoupling], {}, qaNamespaceModel,
  qaForeignCoupling /. qaForeignCoupling -> qaDeclaredCoupling, "Samples" -> 1];
check["explicitly matching a benchmark parameter to the declared model symbol resolves the comparison",
  qaStatus[qaMatchedNamespace] === "PASS"];
qaTypedCoupling = ModelParameter["coupling", "copied-model-regression", "Real"];
qaTypedModel = ReplacePart[qaScalarModel,
  3 -> {{qaTypedCoupling, 3, "candidate coupling"}}];
qaCopiedDifferentModel = ReplacePart[qaTypedModel,
  3 -> {{qaTypedCoupling, 7, "independent benchmark value"}}];
qaCopiedDifferentReport = qaReport[qaPair[qaTypedCoupling], {}, qaTypedModel,
  qaPair[qaTypedCoupling], "FeynmanModel" -> qaCopiedDifferentModel,
  "ParameterRules" -> Automatic, "Samples" -> 1];
check["copied models with identical parameter references use their own different numerical definitions",
  qaStatus[qaCopiedDifferentReport] === "FAIL" &&
    First[qaCopiedDifferentReport["FeynmanCheck"]["Samples"]]["CalculatedAmplitude"] === 3 &&
    First[qaCopiedDifferentReport["FeynmanCheck"]["Samples"]]["FeynmanAmplitude"] === 7];
qaMissingOwnModel = ReplacePart[qaTypedModel, 3 -> {}];
qaUnresolvedOwnModel = ReplacePart[qaTypedModel,
  3 -> {{qaTypedCoupling, ModelFunction["notImplemented", "regression", {}],
    "unresolved benchmark definition"}}];
check["missing or unresolved benchmark definitions cannot borrow copied candidate values",
  AllTrue[{qaMissingOwnModel, qaUnresolvedOwnModel},
    qaStatus[qaReport[qaPair[qaTypedCoupling], {}, qaTypedModel,
      qaPair[qaTypedCoupling], "FeynmanModel" -> #,
      "ParameterRules" -> Automatic, "Samples" -> 1]] === "UNRESOLVED" &]];
qaForeignTypedCoupling = ModelParameter["coupling", "foreign-model-regression", "Real"];
qaForeignTypedModel = ReplacePart[qaScalarModel,
  3 -> {{qaForeignTypedCoupling, 3, "own benchmark coupling"}}];
check["a foreign parameter reference in a benchmark cannot be supplied by candidate model rules",
  qaStatus[qaReport[qaPair[qaTypedCoupling], {}, qaTypedModel,
    qaPair[qaTypedCoupling], "FeynmanModel" -> qaForeignTypedModel,
    "ParameterRules" -> Automatic, "Samples" -> 1]] === "UNRESOLVED"];
qaUnmapped = qaReport[qaPair[0], {}, qaScalarModel,
  SpinorChain[Spinor["Helicity", "Angle", Multiparticle[1, 2]],
    Spinor["Helicity", "Angle", 3]]];
check["an off-shell composite helicity spinor is unsupported rather than assigned an on-shell value",
  qaStatus[qaUnmapped] === "UNRESOLVED"];
qaInternalMomentum = Mom[Multiparticle[1, 2]];
qaOffShellBenchmark = {{Append[qaMassRules[4], Mass[Multiparticle[1, 2]] -> 1],
  SpinorTrace[qaInternalMomentum, qaInternalMomentum] - 2}};
check["benchmark mass metadata never puts an internal numerator momentum on shell",
  qaStatus[qaReport[qaPair[0], {}, qaScalarModel, qaOffShellBenchmark]] === "FAIL"];
qaOneSample = qaReport[qaPair[1], {}, qaScalarModel, 1,
  "Samples" -> 2, "FeynmanSamples" -> 1];
check["an explicit Feynman sample count overrides the physical-pole sample count",
  qaOneSample["FeynmanCheck"]["RequestedSamples"] === 1 &&
    qaOneSample["FeynmanCheck"]["CompletedSamples"] === 1];
Do[
  check["invalid Feynman comparison controls are rejected " <> ToString[bad, InputForm],
    qaInvalid[Quiet[CheckAmplitudes[qaPair[1], {}, qaScalarModel,
      Sequence @@ qaSettings["FeynmanAmplitudes" -> 1, bad]]]]],
  {bad, {"FeynmanSamples" -> 0, "FeynmanSamples" -> 1.5,
    "FeynmanSamples" -> "All", "FeynmanTimeConstraint" -> 0,
    "FeynmanTimeConstraint" -> "Forever", "UnknownCheckOption" -> True}}];

(* Different reference choices are legal only for reference-independent sums. *)
qaAngle[i_, j_] := SpinorChain[Spinor["Helicity", "Angle", i],
  Spinor["Helicity", "Angle", j]];
qaXi[id_, leg_] := SpinorChain[Spinor["Xi", "Angle", id],
  Spinor["Helicity", "Angle", leg]];
qaSchouten[id_] := qaXi[id, 1] qaAngle[2, 3] +
  qaXi[id, 2] qaAngle[3, 1] + qaXi[id, 3] qaAngle[1, 2];
qaReferenceIdentity[id_] := 1 + qaSchouten[id]/qaXi[id, 4];
qaReferencePass = qaReport[qaPair[qaReferenceIdentity[71]], {}, qaScalarModel,
  qaReferenceIdentity[9001]];
check["equivalent amplitudes may use different literal auxiliary-spinor labels",
  qaStatus[qaReferencePass] === "PASS"];
check["the two amplitudes use independent reference choices rather than merging their labels",
  qaStatus[qaReport[qaPair[qaXi[71, 1]/qaXi[71, 2]], {}, qaScalarModel,
    qaXi[9001, 1]/qaXi[9001, 2]]] === "FAIL"];
check["a shared literal reference label cannot hide reference-dependent disagreement",
  qaStatus[qaReport[qaPair[qaXi[71, 1]/qaXi[71, 2]], {}, qaScalarModel,
    qaXi[71, 1]/qaXi[71, 2]]] === "FAIL"];

(* A spectator makes the hZZ tensor a four-leg kinematic fixture, not a claim
   about a new interaction. Coherent polynomials include longitudinal states. *)
qaVectorMassRules = {Mass[1] -> 0, Mass[2] -> 1, Mass[3] -> 1, Mass[4] -> 0};
qaVectorBenchmark = FeynmanToSpinorProducts[
  LorentzMetric[qaVectorMu, qaVectorNu]
    PolarizationVector[2, qaVectorMu, 1, Automatic]
    PolarizationVector[3, qaVectorNu, 1, Automatic], "ReduceSpinorProducts" -> False];
qaVectorImplicit = -SpinorChain[Spinor["Spin", "Angle", 2], Spinor["Spin", "Angle", 3]] *
  SpinorChain[Spinor["Spin", "Square", 2], Spinor["Spin", "Square", 3]];
qaVectorReport = qaReport[{{qaVectorMassRules, qaVectorImplicit}}, {}, qaScalarModel,
  qaVectorBenchmark, "Samples" -> 1];
check["converted massive-vector tensors match the implicit symmetrized spin polynomial",
  qaStatus[qaVectorReport] === "PASS"];
qaMixedSpinProjection[leg_] :=
  SpinorChain[Spinor["Spin", "Angle", leg],
    Spinor["Spin", "Upper", "Angle", leg, -1/2]] *
  SpinorChain[Spinor["Spin", "Angle", leg],
    Spinor["Spin", "Upper", "Angle", leg, 1/2]];
qaVectorAltered = qaReport[{{qaVectorMassRules,
    qaVectorImplicit + qaMixedSpinProjection[2] qaMixedSpinProjection[3]}},
  {}, qaScalarModel, qaVectorBenchmark, "Samples" -> 1];
check["a wrong longitudinal-longitudinal coefficient fails even when a fixed helicity would agree",
  qaStatus[qaVectorAltered] === "FAIL" &&
    With[{sample = First[qaVectorAltered["FeynmanCheck"]["Samples"]]},
      Length[sample["SpinVariables"]] === 2 &&
        Together[sample["Difference"] /. Thread[sample["SpinVariables"] -> 0]] === 0 &&
        Together[Coefficient[Coefficient[sample["Difference"],
          sample["SpinVariables"][[1]], 1], sample["SpinVariables"][[2]], 1]] =!= 0]];
qaOrdinaryChart = ConstructiveDiagrams`Private`highEnergyAutomaticChart[
  qaVectorImplicit, <|"ExternalLegs" -> Range[4],
    "ExternalMassRules" -> qaVectorMassRules|>, 1, 1729, {}];
check["ordinary chart evaluation does not silently collapse explicit vector indices",
  MissingQ[ConstructiveDiagrams`Private`residueChartSpinor[
    Spinor["Spin", "Upper", "Angle", 2, VectorSpinIndex[2, 1]], qaOrdinaryChart]]];

(* Build independent Feynman QED from vertices before loading known answers. *)
Print["Constructing independent four- and five-point Feynman QED benchmarks."];
qaFeynmanModel = ReadModel[FileNameJoin[{root, "model_feynman"}], 2];
qaElectronRow = First[FirstPosition[qaFeynmanModel[[1, All, 2]], "e"]];
qaProcesses = {{"e", "E", "A.+", "A.+"}, {"e", "E", "A.+", "A.-"},
  {"e", "E", "A.+", "A.+", "A.+"}, {"e", "E", "A.+", "A.+", "A.-"}};
qaRawBenchmarks = diagramAmplitudes[CreateDiagrams[#, qaFeynmanModel]] & /@ qaProcesses;
qaSpinorBenchmarks = FeynmanToSpinorProducts[#,
  "ReduceSpinorProducts" -> False, "TimeConstraint" -> 90] & /@ qaRawBenchmarks;
check["independent benchmarks come from two four-point and six five-point diagrams",
  Length /@ qaRawBenchmarks === {2, 2, 6, 6} &&
    FreeQ[qaSpinorBenchmarks, _DiracChain | _PolarizationVector | _Failure | $Failed]];
Get[FileNameJoin[{root, "tests", "QED-5-point-residues.wl"}]];
qaPreparedRawFeynman = PrepareAmplitudes[First[qaRawBenchmarks], qaFeynmanModel,
  "PolePreference" -> None, "InternalOnShell" -> True, "MaxSteps" -> 0,
  "TimeConstraint" -> 60, "ProgressReporting" -> False, "ReturnDetails" -> True];
check["Feynman-model preparation converts raw tensors without constructive internal-shell assumptions",
  qaPreparedRawFeynman["Status"] === "PREPARED" &&
    qaPreparedRawFeynman["InputType"] === "FeynmanDiagrams" &&
    qaPreparedRawFeynman["Options"]["InternalOnShell"] === False &&
    AllTrue[qaPreparedRawFeynman["ItemReports"], #["InternalOnShellChannels"] === {} &] &&
    FreeQ[qaPreparedRawFeynman["PreparedAmplitudes"], _DiracChain | _PolarizationVector]];
check["Feynman preparation retains original input but outputs external-only mass metadata",
  qaPreparedRawFeynman["OriginalAmplitudes"] === First[qaRawBenchmarks] &&
    AllTrue[First /@ qaPreparedRawFeynman["PreparedAmplitudes"],
      AllTrue[#, MatchQ[#, HoldPattern[Mass[_Integer] -> _]] &] &]];
check["generated raw Feynman amplitudes themselves never declare internal numerator shells",
  AllTrue[Flatten[qaRawBenchmarks, 1], Function[pair,
    AllTrue[First[pair], MatchQ[#, HoldPattern[Mass[_Integer] -> _]] &]]]];
qaPreparedConvertedFeynman = PrepareAmplitudes[First[qaSpinorBenchmarks], qaFeynmanModel,
  "PolePreference" -> None, "MaxSteps" -> 0,
  "TimeConstraint" -> 60, "ProgressReporting" -> False];
check["already converted Feynman pairs use the same preparation interface",
  qaPreparedConvertedFeynman === qaPreparedRawFeynman["PreparedAmplitudes"]];
qaDefaultFeynmanPreparation = PrepareAmplitudes[First[qaRawBenchmarks], qaFeynmanModel,
  "PolePreference" -> None, "ProgressReporting" -> False, "ReturnDetails" -> True];
check["Feynman preparation defaults to exact algebra without entering the constructive identity search",
  qaDefaultFeynmanPreparation["Status"] === "PREPARED" &&
    qaDefaultFeynmanPreparation["Options"]["MaxSteps"] === 0 &&
    AllTrue[qaDefaultFeynmanPreparation["ItemReports"], #["IdentitySteps"] === {} &] &&
    ("MaxSteps" /. Options[PrepareAmplitudes]) === 12];
qaCompositeMomentum = Mom[Multiparticle[1, 3]];
qaCompositeTracePair = {{Mass[1] -> Me, Mass[2] -> Me, Mass[3] -> 0,
    Mass[4] -> 0, Mass[Multiparticle[1, 3]] -> Me},
  SpinorTrace[qaCompositeMomentum, qaCompositeMomentum]/
    PropDen[qaCompositeMomentum, Me]};
qaCompositeTracePrepared = PrepareAmplitudes[{qaCompositeTracePair}, qaFeynmanModel,
  "PolePreference" -> None, "InternalOnShell" -> True, "MaxSteps" -> 0,
  "ProgressReporting" -> False, "ReturnDetails" -> True];
qaCompositeTraceChart = prFourPointChart[{1, 3}, 1, 1, 1];
qaCompositeTraceValue[expression_] := ConstructiveDiagrams`Private`residueEvaluateChart[
  expression, qaCompositeTraceChart, {Me -> 1, EE -> 1}];
check["even explicit InternalOnShell True cannot reduce a Feynman composite numerator to its mass",
  qaCompositeTracePrepared["Options"]["InternalOnShell"] === False &&
    Together[qaCompositeTraceValue[qaCompositeTracePrepared["Amplitude"]] -
      qaCompositeTraceValue[Last[qaCompositeTracePair]]] === 0 &&
    qaCompositeTraceValue[qaCompositeTracePrepared["Amplitude"]] === 4];
qaSharedFeynman = PrepareAmplitudes[qaSpinorBenchmarks[[3]], qaFeynmanModel,
  "PolePreference" -> "AllSpurious", "MaxSteps" -> 0, "TimeConstraint" -> 60,
  "ProgressReporting" -> False, "ReturnDetails" -> True];
qaSharedFeynmanAssignments = Flatten[Lookup[
  Lookup[qaSharedFeynman["ItemReports"], "PolePreference"], "Assignments"], 1];
check["all Feynman diagrams use one common reference assignment for each external photon",
  qaSharedFeynman["Status"] === "PREPARED" &&
    qaSharedFeynmanAssignments =!= {} &&
    AllTrue[GatherBy[qaSharedFeynmanAssignments, #["Leg"] &],
      Length[DeleteDuplicates[Lookup[#, "Target"]]] === 1 &] &&
    FreeQ[qaSharedFeynman["Amplitude"], Spinor["Xi", ___]]];
qaPhysicalProcessIndices = {1, 3, 4};
qaPhysicalFeynman = Table[PrepareAmplitudes[qaSpinorBenchmarks[[i]], qaFeynmanModel,
  "PolePreference" -> "AllPhysical", "MaxSteps" -> 0, "TimeConstraint" -> 90,
  "ProgressReporting" -> False, "ReturnDetails" -> True], {i, qaPhysicalProcessIndices}];
Do[With[{i = qaPhysicalProcessIndices[[j]]},
  qaPhysicalAssignments = Flatten[Lookup[
    Lookup[qaPhysicalFeynman[[j]]["ItemReports"], "PolePreference"], "Assignments"], 1];
  check["shared physical Feynman references replace every Xi for process " <>
      ToString[qaProcesses[[i]], InputForm],
    qaPhysicalFeynman[[j]]["Status"] === "PREPARED" &&
      Sort[DeleteDuplicates[Lookup[qaPhysicalAssignments, "Leg"]]] === Range[3, Length[qaProcesses[[i]]]] &&
      AllTrue[qaPhysicalAssignments, #["Mode"] === "AllPhysical" &] &&
      AllTrue[GatherBy[qaPhysicalAssignments, #["Leg"] &],
        Length[DeleteDuplicates[Lookup[#, "Target"]]] === 1 &] &&
      FreeQ[qaPhysicalFeynman[[j]]["Amplitude"], Spinor["Xi", ___]] &&
      !FreeQ[qaPhysicalFeynman[[j]]["Amplitude"], _PropDen]];
  check["shared physical Feynman references never declare internal shells for process " <>
      ToString[qaProcesses[[i]], InputForm],
    qaPhysicalFeynman[[j]]["Options"]["InternalOnShell"] === False &&
      AllTrue[qaPhysicalFeynman[[j]]["ItemReports"],
        #["InternalOnShellChannels"] === {} &&
          AllTrue[#["Reductions"], !TrueQ[Lookup[#, "InternalOnShellIdentity", False]] &] &] &&
      AllTrue[First /@ qaPhysicalFeynman[[j]]["PreparedAmplitudes"],
        AllTrue[#, MatchQ[#, HoldPattern[Mass[_Integer] -> _]] &] &]]],
  {j, Length[qaPhysicalFeynman]}];
qaUnsupportedFeynmanReferences = PrepareAmplitudes[First[qaSpinorBenchmarks], qaFeynmanModel,
  "PolePreference" -> "Mixed", "MaxSteps" -> 0,
  "ProgressReporting" -> False, "ReturnDetails" -> True];
check["unimplemented shared mixed Feynman references remain explicitly partial",
  qaUnsupportedFeynmanReferences["Status"] === "PARTIAL" &&
    qaUnsupportedFeynmanReferences["Diagnostics"] =!= {}];
check["unsupported Feynman reference modes cannot silently return amplitudes with unchanged Xi",
  FailureQ[PrepareAmplitudes[First[qaSpinorBenchmarks], qaFeynmanModel,
    "PolePreference" -> "Mixed", "MaxSteps" -> 0, "ProgressReporting" -> False]]];
qaIndependentFeynman = PrepareAmplitudes[qaSpinorBenchmarks[[3]], qaFeynmanModel,
  "PolePreference" -> "IndependentXi", "MaxSteps" -> 0, "TimeConstraint" -> 90,
  "ProgressReporting" -> False, "ReturnDetails" -> True];
check["independent Feynman references survive ordinary preparation as distinct photon references",
  qaIndependentFeynman["Status"] === "PREPARED" &&
    Sort[DeleteDuplicates[Cases[qaIndependentFeynman["Amplitude"],
      Spinor["Xi", _, id_] :> id, Infinity]]] === {3, 4, 5}];
qaMasslessFeynmanModel = qaFeynmanModel;
qaMasslessFeynmanModel[[1, qaElectronRow, 6]] = 0;
qaMasslessFeynmanPairs = diagramAmplitudes[CreateDiagrams[
  {"e", "E", "A.+", "A.-"}, qaMasslessFeynmanModel]];
qaMasslessUnspecified = Quiet[PrepareAmplitudes[qaMasslessFeynmanPairs, qaMasslessFeynmanModel,
  "PolePreference" -> None, "MaxSteps" -> 0, "ProgressReporting" -> False]];
check["unspecified massless Dirac helicities cannot return unchecked prepared amplitudes",
  FailureQ[qaMasslessUnspecified]];
qaMasslessSpecified = PrepareAmplitudes[qaMasslessFeynmanPairs, qaMasslessFeynmanModel,
  "FermionHelicities" -> {1 -> -1/2, 2 -> 1/2},
  "PolePreference" -> None, "MaxSteps" -> 0,
  "ProgressReporting" -> False, "ReturnDetails" -> True];
check["explicit massless Dirac helicities allow ordinary Feynman preparation",
  qaMasslessSpecified["Status"] === "PREPARED" &&
    FreeQ[qaMasslessSpecified["PreparedAmplitudes"], _DiracChain | _PolarizationVector]];
qaMasslessRawCheck = qaReport[qaPair[0], {}, qaScalarModel, qaMasslessFeynmanPairs,
  "Samples" -> 1];
check["the checker diagnoses raw Feynman conversion requiring unspecified helicities",
  qaStatus[qaMasslessRawCheck] === "UNRESOLVED"];
qaKnownFour = Table[With[{row = SelectFirst[prFixtureModel[[2]],
      Take[#, 4] === qaProcesses[[i]] &]}, Times @@ row[[-2 ;; -1]]], {i, 2}];
qaConstructiveMixedPairs = diagramAmplitudes[CreateDiagrams[Last[qaProcesses], prModel]];
qaKnownMixed = qedFivePointMixedRoute[qaConstructiveMixedPairs]["Final"];
qaKnownAmplitudes = Join[qaKnownFour, {prH, qaKnownMixed}];
qaKnownChannels = FactorizationAmplitudeChannels[#, If[Length[#] === 4,
    prModel, prFixtureModel]] & /@ qaProcesses;
qaPhysicalBenchmarkReports = Table[With[{i = qaPhysicalProcessIndices[[j]]},
  qaReport[qaKnownAmplitudes[[i]], qaKnownChannels[[i]], prFixtureModel,
    qaPhysicalFeynman[[j]]["PreparedAmplitudes"], "FeynmanModel" -> qaFeynmanModel,
    "Samples" -> 1, "FeynmanSamples" -> 2, "FeynmanTimeConstraint" -> 120,
    "TimeConstraint" -> 90]], {j, {2, 3}}];
Do[With[{i = j + 2},
  check["shared physical references preserve the full five-point QED amplitude " <>
      ToString[qaProcesses[[i]], InputForm],
    qaStatus[qaPhysicalBenchmarkReports[[j]]] === "PASS" &&
      qaAllPhysicalPass[qaPhysicalBenchmarkReports[[j]]]]], {j, 2}];
qaIndependentBenchmarkReports = Table[
  qaReport[qaKnownAmplitudes[[3]], qaKnownChannels[[3]], prFixtureModel,
    qaIndependentFeynman["PreparedAmplitudes"], "FeynmanModel" -> qaFeynmanModel,
    "Samples" -> 1, "FeynmanSamples" -> 2, "FeynmanTimeConstraint" -> 120,
    "TimeConstraint" -> 90, "RandomSeed" -> seed], {seed, {1729, 2026, 2718}}];
Do[
  check["ordinary preparation with independent Xi preserves all-plus QED at seed " <>
      ToString[{1729, 2026, 2718}[[j]]],
    qaStatus[qaIndependentBenchmarkReports[[j]]] === "PASS" &&
      qaAllPhysicalPass[qaIndependentBenchmarkReports[[j]]]],
  {j, Length[qaIndependentBenchmarkReports]}];
qaIndependentDefaultBenchmark = qaReport[qaKnownAmplitudes[[3]], qaKnownChannels[[3]],
  prFixtureModel, qaIndependentFeynman["PreparedAmplitudes"],
  "FeynmanModel" -> qaFeynmanModel, "ParameterRules" -> Automatic,
  "Samples" -> 2, "FeynmanSamples" -> 2, "FeynmanTimeConstraint" -> 120,
  "TimeConstraint" -> 90];
check["prepared independent Xi also preserve all-plus QED at model-default masses and couplings",
  qaStatus[qaIndependentDefaultBenchmark] === "PASS" &&
    qaAllPhysicalPass[qaIndependentDefaultBenchmark]];
qaRawMassiveReport = qaReport[First[qaKnownFour], First[qaKnownChannels], prModel,
  First[qaRawBenchmarks], "FeynmanModel" -> qaFeynmanModel, "Samples" -> 1];
check["supported raw massive Feynman tensors are converted by the full checker itself",
  qaStatus[qaRawMassiveReport] === "PASS" && qaAllPhysicalPass[qaRawMassiveReport]];
qaBenchmarkReports = Table[
  qaReport[qaKnownAmplitudes[[i]], qaKnownChannels[[i]],
    If[i <= 2, prModel, prFixtureModel], qaSpinorBenchmarks[[i]] /.
      Spinor["Xi", kind_, id_Integer] :> Spinor["Xi", kind, id + 1000],
    "FeynmanModel" -> qaFeynmanModel,
    "Samples" -> 1, "FeynmanSamples" -> 2, "FeynmanTimeConstraint" -> 120,
    "TimeConstraint" -> 90], {i, Length[qaProcesses]}];
Do[
  check["the full off-cut Feynman amplitude agrees with constructive QED " <>
      ToString[qaProcesses[[i]], InputForm],
    qaStatus[qaBenchmarkReports[[i]]] === "PASS" &&
      qaAllPhysicalPass[qaBenchmarkReports[[i]]]], {i, Length[qaProcesses]}];
check["the complete QED comparison includes every implicit massive-fermion spin component",
  AllTrue[qaBenchmarkReports, #["FeynmanCheck"]["Scope"] ===
    "SampledOffCutMomentaAllImplicitMassiveSpinComponents" &]];
qaSignBenchmark = qaReport[First[qaKnownFour], First[qaKnownChannels], prModel,
  ({First[#], -Last[#]} &) /@ First[qaSpinorBenchmarks],
  "FeynmanModel" -> qaFeynmanModel, "Samples" -> 1];
check["a sign reversal of a real Feynman benchmark is diagnosed without altering residue checks",
  qaStatus[qaSignBenchmark] === "Opp Sign" && qaAllPhysicalPass[qaSignBenchmark]];
qaRegularDifference = First[qaKnownFour] + Times @@
  DeleteDuplicates[Cases[First[qaKnownFour], _PropDen, Infinity]];
qaRegularDifferenceReport = qaReport[qaRegularDifference, First[qaKnownChannels],
  prModel, First[qaSpinorBenchmarks], "FeynmanModel" -> qaFeynmanModel, "Samples" -> 1];
check["full Feynman comparisons detect a pole-regular change that all residue checks miss",
  qaAllPhysicalPass[qaRegularDifferenceReport] && qaStatus[qaRegularDifferenceReport] === "FAIL"];

qaSingleChartPropagator = PropDen[Mom[Multiparticle[1, 3]], 0];
qaSingleChartChannels = {{0, 0, 0, 0, Multiparticle[1, 3], 0, 0}};
qaSingleChartReport = qaReport[qaPair[qaSingleChartPropagator - 3],
  qaSingleChartChannels, qaScalarModel, 0,
  "Samples" -> 1, "FeynmanSamples" -> 2];
check["off-cut comparison does not mistake agreement on one frozen channel surface for equality",
  qaStatus[qaSingleChartReport] === "FAIL"];
check["repeated off-cut samples for one eligible channel use different rational coordinates",
  Length[DeleteDuplicates[Lookup[
    Lookup[qaSingleChartReport["FeynmanCheck"]["Samples"], "Chart"],
    "SamplingParameter"]]] === 2];
qaSingleChartTargets = ConstructiveDiagrams`Private`residueSimpleInputs[
  qaPair[qaSingleChartPropagator - 3], qaSingleChartChannels, qaScalarModel]["Targets"];
qaChartPreservesRandomState = BlockRandom[Module[{expected, actual, chart},
  SeedRandom[20261026]; expected = RandomInteger[10^6, 6];
  SeedRandom[20261026];
  chart = ConstructiveDiagrams`Private`amplitudeRationalComparisonChart[
    qaSingleChartTargets, 1, 1729, {}];
  actual = RandomInteger[10^6, 6];
  AssociationQ[chart] && expected === actual]];
check["rational off-cut chart sampling preserves the caller's random state",
  qaChartPreservesRandomState];

(* Four massive external fermions make radical-valued numerical spin bases
   unnecessarily costly. The full comparison can use rational complex charts,
   but must still test every coherent spin coefficient away from every pole. *)
Print["Checking independent four-fermion/photon Feynman amplitudes at model defaults."];
qaFourFermionParticles = {"e", "E", "E", "e", "A.+"};
qaFourFermionRaw = diagramAmplitudes[CreateDiagrams[
  qaFourFermionParticles, qaFeynmanModel]];
qaFourFermionReferenceModes = {"IndependentXi", "AllPhysical"};
qaFourFermionPrepared = Table[PrepareAmplitudes[qaFourFermionRaw, qaFeynmanModel,
  "PolePreference" -> mode, "MaxSteps" -> 0, "TimeConstraint" -> 90,
  "ProgressReporting" -> False, "ReturnDetails" -> True],
  {mode, qaFourFermionReferenceModes}];
check["the four-fermion photon benchmark comes from all eight independent Feynman diagrams",
  Length[qaFourFermionRaw] === 8 &&
    AllTrue[qaFourFermionPrepared, #["Status"] === "PREPARED" &&
      Length[#["PreparedAmplitudes"]] === 8 &&
      #["Options"]["InternalOnShell"] === False &]];
check["one external photon retains an independent reference or receives an all-physical reference",
  Sort[DeleteDuplicates[Cases[qaFourFermionPrepared[[1]]["Amplitude"],
      Spinor["Xi", _, id_] :> id, Infinity]]] === {5} &&
    FreeQ[qaFourFermionPrepared[[2]]["Amplitude"], Spinor["Xi", ___]]];
qaFourFermionRoute = q5FourFermionPhotonRoute[CreateDiagrams[
  qaFourFermionParticles, prModel]];
qaFourFermionFinal = qaFourFermionRoute["Final"];
qaFourFermionChannels = FactorizationAmplitudeChannels[
  qaFourFermionParticles, prFixtureModel];
qaFourFermionNormalized = ConstructiveDiagrams`Private`residueSimpleInputs[
  qaFourFermionFinal, qaFourFermionChannels, prFixtureModel];
qaFourFermionRules = ModelParameterRules[prFixtureModel];
qaFourFermionReports = Table[qaReport[qaFourFermionFinal, qaFourFermionChannels,
  prFixtureModel, prepared["PreparedAmplitudes"], "FeynmanModel" -> qaFeynmanModel,
  "ParameterRules" -> Automatic, "Samples" -> 1, "FeynmanSamples" -> 2,
  "FeynmanTimeConstraint" -> 120, "TimeConstraint" -> 90],
  {prepared, qaFourFermionPrepared}];
Do[
  qaFourFermionSamples = qaFourFermionReports[[i]]["FeynmanCheck"]["Samples"];
  check["four-fermion/photon full amplitudes agree at model defaults with " <>
      qaFourFermionReferenceModes[[i]],
    qaStatus[qaFourFermionReports[[i]]] === "PASS" &&
      qaAllPhysicalPass[qaFourFermionReports[[i]]] &&
      Length[qaFourFermionSamples] === 2 &&
      AllTrue[qaFourFermionSamples, #["Status"] === "PASS" &&
        Length[#["SpinVariables"]] === 4 &&
        #["Chart"]["SamplingConstruction"] === "ExactRationalComplexOffCut" &&
        TrueQ[#["Chart"]["CoherentVectorSpinIndices"]] &&
        !TrueQ[#["Chart"]["AllowCutSpinors"]] &]];
  check["rational off-cut comparison conserves momentum and all four external masses with " <>
      qaFourFermionReferenceModes[[i]],
    AllTrue[qaFourFermionSamples, Function[sample, With[{chart = sample["Chart"]},
      AllTrue[Flatten[Total[Values[chart["Momenta"]]]],
        Cancel[Together[#]] === 0 &] &&
      AllTrue[Range[5], Function[leg,
        Cancel[Together[Det[chart["Momenta"][leg]] -
          (Mass[leg]^2 /. qaFourFermionNormalized["Targets"][[1]]["ExternalMassRules"] /.
            Rationalize[qaFourFermionRules, 0])]] === 0]]]]]];
  check["rational comparison leaves every known physical propagator nonzero with " <>
      qaFourFermionReferenceModes[[i]],
    AllTrue[qaFourFermionSamples,
      ConstructiveDiagrams`Private`referencePhysicalGenericQ[#["Chart"],
        qaFourFermionNormalized["Targets"], qaFourFermionRules] &]],
  {i, Length[qaFourFermionReports]}];
qaFourFermionWrongRelative = qaFourFermionFinal -
  2 First[qaFourFermionRoute["PairedCurrents"]]["Final"];
qaFourFermionWrongSample = ConstructiveDiagrams`Private`amplitudeComparisonOneSample[
  qaFourFermionWrongRelative,
  qaFourFermionReports[[1]]["FeynmanCheck"]["FeynmanExpression"],
  qaFourFermionReports[[1]]["FeynmanCheck"]["Samples"][[1]]["Chart"],
  1, 1729, 0, qaFourFermionRules, ModelParameterRules[qaFeynmanModel], 10^-20];
check["the rational four-fermion comparison rejects an incorrect relative fermion-pair sign",
  qaFourFermionWrongSample["Status"] === "FAIL" &&
    qaFourFermionWrongSample["MatchingSigns"] === {}];

Print[checks, " amplitude-check checks; ", Length[failures], " failures."];
If[failures =!= {}, Print[failures]; Quit[1]];
Print["HEPCAT_TEST_COMPLETED:amplitude-check-tests.wl"];
Quit[0];
