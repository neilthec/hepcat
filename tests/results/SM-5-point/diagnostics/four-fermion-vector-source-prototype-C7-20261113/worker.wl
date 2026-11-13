Get[FileNameJoin[{Environment["HEPCAT_4FV_ROOT"], "source", "HEPCAT.wl"}]];
Begin["HEPCATFourFermionVectorPilot`Private`"];
$ContextPath = Prepend[DeleteCases[$ContextPath, "HEPCATFourFermionVectorPilot`Private`"], "HEPCATFourFermionVectorPilot`Private`"];
$HistoryLength = 0;
fvRoot = Environment["HEPCAT_4FV_ROOT"]; fvDirectory = Environment["HEPCAT_4FV_OUTPUT"];
fvPlan = Import[Environment["HEPCAT_4FV_PLAN"], "RawJSON"]; fvProcess = fvPlan["Particles"];
fvPrototype = FileNameJoin[{DirectoryName[fvDirectory], "prototype-source.wl"}];
Get[fvPrototype];
fvSHA[path_String] := IntegerString[FileHash[path, "SHA256"], 16, 64];
fvSave[name_String, data_] := Module[{p = FileNameJoin[{fvDirectory, name}], stream, tmp},
 tmp = p <> "." <> CreateUUID[] <> ".tmp"; stream = OpenWrite[tmp, BinaryFormat -> True];
 BinaryWrite[stream, Normal[BinarySerialize[data, PerformanceGoal -> "Size"]], "Byte"]; Close[stream];
 RenameFile[tmp, p, OverwriteTarget -> True]];
fvJSON[value_Association] := Association[KeyValueMap[(If[StringQ[#1], #1, ToString[#1, InputForm]] -> fvJSON[#2]) &, value]];
fvJSON[value_List] := fvJSON /@ value;
fvJSON[value_] := If[StringQ[value] || IntegerQ[value] || Head[value] === Real || MemberQ[{True, False, Null}, value], value, ToString[value, InputForm]];
fvRecord = <|"Status" -> "STARTED", "Particles" -> fvProcess, "FamilyID" -> fvPlan["FamilyID"],
 "ContextSHA256" -> "4165f24c7d5501c40c03ea91f850a5cde77389a1236e3af397fb6d7e2344a167",
 "PrototypeOnly" -> True, "PublicConstructorSuccessClaimed" -> False, "CanonicalPromotion" -> False,
 "ProductionSourceEdited" -> False, "OnlyMassiveVectorClassGateExtended" -> True,
 "InternalOnShellConditions" -> {}, "NumericalCoefficientFit" -> False, "BenchmarkUsedForConstruction" -> False,
 "ScriptSHA256" -> fvSHA[$InputFileName], "PrototypeSourceSHA256" -> fvSHA[fvPrototype]|>;
fvStart = AbsoluteTime[]; fvReports = {}; fvRows = {}; fvPrints = {};
fvCheckpoint[] := (fvRecord["Seconds"] = N[AbsoluteTime[] - fvStart]; fvRecord["ValidationRecords"] = fvReports;
 fvSave["result.wxf", fvRecord]; Export[FileNameJoin[{fvDirectory, "summary.json.tmp"}],
 fvJSON[KeyDrop[fvRecord, {"Proposal", "PreparedPair", "Preparation", "IndependentFeynmanPairs", "Channels", "ValidationRecords", "ExportResult"}]], "RawJSON"];
 RenameFile[FileNameJoin[{fvDirectory, "summary.json.tmp"}], FileNameJoin[{fvDirectory, "summary.json"}], OverwriteTarget -> True]);
fvWork[] := Catch[Module[{model, lower, proposal, prep, pair, benchmarkModel, raw, benchmark, channels, sets, overrides,
 index, seed, checked, rows, passed, result, partial, ch, mg, config, options, modelBefore, sourceFiles},
 model = ReadModel[FileNameJoin[{fvRoot, "models"}], 1]; lower = ReadModel[FileNameJoin[{fvRoot, "models"}], 2];
 sourceFiles = Join[FileNames["*.wl", FileNameJoin[{fvRoot, "source"}]], FileNames["*.wl", FileNameJoin[{fvRoot, "source", "internal"}]]];
 fvHashes = AssociationMap[fvSHA, sourceFiles]; fvRecord["SourceHashes"] = fvHashes;
 Print["4FV_SOURCE_BEGIN ", fvProcess];
 proposal = HEPCATFourFermionVectorPrototype`Private`covariantCurrentSource[fvProcess, model, "TimeConstraint" -> 60];
 fvRecord["Proposal"] = proposal;
 If[!AssociationQ[proposal] || Lookup[proposal, "Status", None] =!= "UNVALIDATED_CANDIDATE",
  fvRecord["Status"] = "SOURCE_UNRESOLVED"; fvRecord["Reason"] = proposal; fvCheckpoint[]; Throw[False]];
 fvRecord["OriginalDiagramCount"] = proposal["OriginalDiagramCount"]; fvRecord["LiftedDiagramCount"] = proposal["LiftedDiagramCount"];
 fvRecord["RawLeafCount"] = LeafCount[proposal["Expression"]]; fvRecord["SourceSealedBeforeBenchmarkRead"] = True;
 fvSave["proposal.wxf", fvRecord]; fvRecord["ProposalCapsuleSHA256"] = fvSHA[FileNameJoin[{fvDirectory, "proposal.wxf"}]];
 Print["4FV_SOURCE_SEALED ", fvRecord["OriginalDiagramCount"], " diagrams; leaves ", fvRecord["RawLeafCount"]];
 prep = PrepareAmplitudes[proposal["Amplitude"], lower, "InternalOnShell" -> False, "PolePreference" -> None,
   "ExternalParticles" -> fvProcess, "ReturnDetails" -> True, "Output" -> "Amplitudes", "TimeConstraint" -> 60,
   "Parallelization" -> False, "ProgressReporting" -> False]; fvRecord["Preparation"] = prep;
 If[!TrueQ[ConstructiveDiagrams`Private`constructPreparationUsableQ[prep]], fvRecord["Status"] = "PREPARATION_UNRESOLVED"; fvCheckpoint[]; Throw[False]];
 pair = First[prep["PreparedAmplitudes"]]; fvRecord["PreparedPair"] = pair;
 fvRecord["PreparedLeafCount"] = LeafCount[Last[pair]]; fvRecord["Complexity"] = SpinorExpressionComplexity[Last[pair]];
 If[!FreeQ[pair, (Spinor | SpinorHat)["Xi", ___]], fvRecord["Status"] = "REFERENCE_UNRESOLVED"; fvCheckpoint[]; Throw[False]];
 channels = FactorizationAmplitudeChannels[fvProcess, lower]; fvRecord["Channels"] = channels;
 (* The independent benchmark is first read after the source-only proposal has been sealed. *)
 benchmarkModel = ReadModel[FileNameJoin[{fvRoot, "tests", "reference-models", "calchep"}], 1];
 raw = DiagramAmplitudes[CreateDiagrams[fvProcess, benchmarkModel]];
 benchmark = FeynmanToSpinorProducts[raw, "ReferenceSpinors" -> Automatic, "ReduceSpinorProducts" -> False, "TimeConstraint" -> 120];
 If[!TrueQ[ConstructiveDiagrams`Private`constructBenchmarkConversionCompleteQ[raw, benchmark]],
  fvRecord["Status"] = "BENCHMARK_UNRESOLVED"; fvCheckpoint[]; Throw[False]];
 fvRecord["IndependentFeynmanPairs"] = benchmark; fvRecord["IndependentFeynmanDiagramCount"] = Length[raw];
 sets = ConstructiveDiagrams`Private`completionValidationRuleSets[lower, Automatic, Automatic];
 If[Length[sets] =!= 3, fvRecord["Status"] = "PARAMETER_POLICY_UNRESOLVED"; fvCheckpoint[]; Throw[False]];
 Do[overrides = ConstructiveDiagrams`Private`constructValidationOverrides[lower, Automatic, sets[[index]]];
  seed = Mod[Hash[{AbsoluteTime[], fvProcess, index}], 2^31 - 2] + 1;
  Print["4FV_VALIDATION_BEGIN ", index, "/3 seed=", seed];
  checked = CheckAmplitudes[pair, channels, lower, "ParameterRules" -> overrides, "FeynmanAmplitudes" -> benchmark,
    "FeynmanModel" -> benchmarkModel, "Samples" -> 4, "RandomSeed" -> seed,
    "TimeConstraint" -> 600, "FeynmanTimeConstraint" -> 600, "Output" -> "Details", "Parallelization" -> False];
  rows = If[AssociationQ[checked] && MatchQ[Lookup[checked, "Table", None], _Dataset], Normal[checked["Table"]], {}];
  passed = TrueQ[ConstructiveDiagrams`Private`constructAllChecksPassQ[checked, pair, channels, lower, benchmark]];
  AppendTo[fvReports, <|"ParameterSet" -> sets[[index]], "ParameterOverrides" -> overrides, "RandomSeed" -> seed,
    "Samples" -> 4, "CheckTimeConstraint" -> 600, "Report" -> checked, "Rows" -> rows, "AllChecksPass" -> passed|>];
  fvRecord["ParameterGroupsCompleted"] = Length[fvReports]; fvRecord["CheckStatusCounts"] = Counts[Flatten[Lookup[#, "Status", {}] & /@ Lookup[fvReports, "Rows", {}]]];
  fvRecord["Status"] = If[passed, "NATIVE_IN_PROGRESS", "NATIVE_NOT_ALL_PASS"]; fvCheckpoint[];
  Print["4FV_VALIDATION_END ", index, " pass=", passed]; If[!passed, Throw[False]], {index, 3}];
 fvRecord["Status"] = "ALL_THREE_NATIVE_GROUPS_PASS"; fvCheckpoint[];
 config = Import[FileNameJoin[{fvRoot, "tests", "results", "SM-5-point", "configuration.json"}], "RawJSON"];
 options = {"ProcessName" -> fvPlan["Class"], "SPINASDirectory" -> config["SPINASDirectory"],
  "Build" -> True, "RunExamples" -> True, "Overwrite" -> False, "ReturnDetails" -> True,
  "CalcHEPDirectory" -> config["CalcHEPDirectory"], "CalcHEPModel" -> {FileNameJoin[{fvRoot, "tests", "reference-models", "calchep"}], 1},
  "CalcHEPPoints" -> 1000, "RunCalcHEPValidation" -> True, "MadGraphDirectory" -> config["MadGraphDirectory"],
  "MadGraphModel" -> FileNameJoin[{fvRoot, "tests", "reference-models", "madgraph", "sm"}],
  "MadGraphParameterRules" -> {"lamWS" -> 0}, "MadGraphPoints" -> 1000, "RunMadGraphValidation" -> True};
 result = WriteSPINASProcess[FileNameJoin[{fvDirectory, "spinas-output"}], fvPlan["Incoming"] -> fvPlan["Outgoing"],
  {{fvProcess, pair}}, model, Sequence @@ options]; fvRecord["ExportResult"] = result;
 partial = If[FailureQ[result], Lookup[result[[2]], "ExportResult", <||>], result];
 ch = If[AssociationQ[partial], Lookup[partial, "CalcHEPValidation", <||>], <||>];
 mg = If[AssociationQ[partial], Lookup[partial, "MadGraphValidation", <||>], <||>];
 fvRecord["CalcHEPStatus"] = If[AssociationQ[ch], Lookup[ch, "Status", "UNRESOLVED"], "FAIL"];
 fvRecord["MadGraphStatus"] = If[AssociationQ[mg], Lookup[mg, "Status", "UNRESOLVED"], "FAIL"];
 fvRecord["Status"] = If[fvRecord["CalcHEPStatus"] === "PASS" && fvRecord["MadGraphStatus"] === "PASS", "PROTOTYPE_NATIVE_AND_EXTERNAL_PASS", "EXTERNAL_NOT_ALL_PASS"];
 fvCheckpoint[]; True]];
fvOutcome = MemoryConstrained[TimeConstrained[Block[{ConstructiveDiagrams`Private`$amplitudeParallelActive = True}, fvWork[]],
  2350, Missing["PrototypeTimeConstraint"]], 4 2^30, Missing["PrototypeMemoryConstraint"]];
If[!MemberQ[{"PROTOTYPE_NATIVE_AND_EXTERNAL_PASS", "SOURCE_UNRESOLVED", "PREPARATION_UNRESOLVED", "REFERENCE_UNRESOLVED", "BENCHMARK_UNRESOLVED", "PARAMETER_POLICY_UNRESOLVED", "NATIVE_NOT_ALL_PASS", "EXTERNAL_NOT_ALL_PASS"}, fvRecord["Status"]],
 fvRecord["Status"] = "UNRESOLVED"; fvRecord["Reason"] = fvOutcome];
fvRecord["ProductionSourceHashesUnchanged"] = AssociationMap[fvSHA, Keys[fvHashes]] === fvHashes; fvCheckpoint[];
Print["4FV_DONE ", fvRecord["Status"]];
End[]; Quit[];
