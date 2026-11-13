Get[FileNameJoin[{Environment["HEPCAT_4FV_SHADOW"], "source", "HEPCAT.wl"}]];
Begin["HEPCATFourFermionVectorPublicShadow`Private`"];
$ContextPath = Prepend[DeleteCases[$ContextPath, "HEPCATFourFermionVectorPublicShadow`Private`"], "HEPCATFourFermionVectorPublicShadow`Private`"];
$HistoryLength = 0;
root = Environment["HEPCAT_4FV_ROOT"]; output = Environment["HEPCAT_4FV_OUTPUT"];
plan = Import[Environment["HEPCAT_4FV_PLAN"], "RawJSON"]; process = plan["Particles"];
model = ReadModel[FileNameJoin[{root, "models"}], 1]; lower = ReadModel[FileNameJoin[{root, "models"}], 2];
feynman = ReadModel[FileNameJoin[{root, "tests", "reference-models", "calchep"}], 1];
printed = {}; started = AbsoluteTime[]; live = OpenWrite[FileNameJoin[{output, "worker.live.txt"}]];
SetAttributes[capture, HoldAll];
capture[body_] := Internal`InheritedBlock[{Print}, Unprotect[Print]; Print[] := Null;
 Print[args__] := (AppendTo[printed, If[Length[{args}] === 1, First[{args}], SequenceForm[args]]];
 WriteString[live, DateString[{"Hour", ":", "Minute", ":", "Second"}], " ", ToString[Last[printed], OutputForm], "\n"]; Null);
 Protect[Print]; body];
details = capture[MemoryConstrained[TimeConstrained[
 Block[{ConstructiveDiagrams`Private`$amplitudeParallelActive = True},
 ConstructAmplitude[process, model, lower, "FeynmanModel" -> feynman, "Output" -> "Details"]],
 7100, Failure["ShadowOuterTimeConstraint", <||>]], 4 2^30, Failure["ShadowMemoryConstraint", <||>]]];
Close[live]; tables = Cases[printed, _Dataset, {1}]; rows = Flatten[Normal /@ tables, 1];
expression = If[AssociationQ[details], Lookup[details, "Amplitude", Missing["NotValidated"]], details];
ok = AssociationQ[details] && Lookup[details, "Status", None] === "COMPLETE" && !MissingQ[expression] && !FailureQ[expression] &&
 rows =!= {} && AllTrue[rows, Lookup[#, "Status", None] === "PASS" &] && AnyTrue[rows, Lookup[#, "Type", None] === "Feynman" &];
record = <|"Status" -> If[ok, "COMPLETE", "NOT_COMPLETE"], "Particles" -> process,
 "FamilyID" -> plan["FamilyID"], "Expression" -> expression, "Details" -> details, "Prints" -> printed, "Rows" -> rows,
 "Seconds" -> N[AbsoluteTime[] - started], "ShadowSourceContextSHA256" -> Environment["HEPCAT_4FV_SHADOW_CONTEXT"],
 "OriginalProductionContextSHA256" -> "4165f24c7d5501c40c03ea91f850a5cde77389a1236e3af397fb6d7e2344a167",
 "PublicCall" -> "ConstructAmplitude[process, model1, model2, \"FeynmanModel\" -> reference1, \"Output\" -> \"Details\"]",
 "PublicOptions" -> {"FeynmanModel", "Output"}, "AllOtherOptionsDefault" -> True, "RandomSeedChoice" -> "Automatic",
 "ProductionSourceEdited" -> False, "CanonicalPromotion" -> False, "ActualPublicConstructorExecuted" -> True|>;
stream = OpenWrite[FileNameJoin[{output, "result.wxf.tmp"}], BinaryFormat -> True];
BinaryWrite[stream, Normal[BinarySerialize[record, PerformanceGoal -> "Size"]], "Byte"]; Close[stream];
RenameFile[FileNameJoin[{output, "result.wxf.tmp"}], FileNameJoin[{output, "result.wxf"}], OverwriteTarget -> True];
summary = KeyDrop[record, {"Expression", "Details", "Prints", "Rows"}];
AssociateTo[summary, {"Rows" -> Length[rows], "RowStatuses" -> Counts[Lookup[rows, "Status", {}]],
 "FeynmanRows" -> Count[Lookup[rows, "Type", {}], "Feynman"],
 "Complexity" -> If[AssociationQ[details], Lookup[details, "Complexity", Null], Null]}];
Export[FileNameJoin[{output, "summary.json"}], summary, "RawJSON"];
Print[summary]; Quit[If[ok, 0, 1]];
