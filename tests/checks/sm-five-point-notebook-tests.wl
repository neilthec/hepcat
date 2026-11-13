(* Structural checks for the staged SM five-point notebook and captured-output
   replay. No scattering amplitude, compiler or reference generator is run. *)

(*
HEPCAT - High Energy Physics Constructive Amplitude Tool

Copyright (C) 2026 Neil Christensen

This file is part of HEPCAT.

HEPCAT is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

HEPCAT is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with HEPCAT.  If not, see <https://www.gnu.org/licenses/>.
*)

sm5ntRoot = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Get[FileNameJoin[{sm5ntRoot, "source", "HEPCAT.wl"}]];
sm5ntChecks = 0; sm5ntFailures = {};
SetAttributes[sm5ntCheck, HoldRest];
sm5ntCheck[name_, assertion_] := Module[{value},
  sm5ntChecks++;
  value = Quiet[Check[assertion, $Failed]];
  If[!TrueQ[value], AppendTo[sm5ntFailures, name]; Print["FAIL: ", name]]
];
sm5ntContains[expression_, fragment_String] := !FreeQ[expression, text_String /; StringContainsQ[text, fragment]];
sm5ntRead[path_] := DeleteCases[
  Quiet[Check[ToExpression[Import[path, "Text"], InputForm, HoldComplete], $Failed]], Null, {1}];
sm5ntPath = FileNameJoin[{sm5ntRoot, "tests", "SM-5-point-construction.nb"}];
sm5ntNotebook = sm5ntRead[sm5ntPath];
sm5ntInventory = Import[FileNameJoin[{sm5ntRoot, "tests", "SM-5-point", "inventory.json"}], "RawJSON"];
sm5ntInputs = Cases[sm5ntNotebook, Cell[text_String, "Input", ___] :> text, Infinity];
sm5ntParsed = ToExpression[#, InputForm, HoldComplete] & /@ sm5ntInputs;
sm5ntOrdinaryInputs = Cases[sm5ntNotebook, Cell[text_String, "Input", opts___] /;
  FreeQ[{opts}, HoldPattern[CellTags -> tag_String] /;
    StringStartsQ[tag, "sm5-supplemental-probe-"]] :> text, Infinity];
sm5ntOrdinaryParsed = ToExpression[#, InputForm, HoldComplete] & /@ sm5ntOrdinaryInputs;
sm5ntConstructors = Flatten[Cases[#, HoldPattern[ConstructAmplitude[p_, d_, f_, opts___]] :>
  HoldComplete[p, d, f, {opts}], Infinity] & /@ sm5ntParsed];
sm5ntExports = Flatten[Cases[#, HoldPattern[WriteSPINASProcess[args___]] :>
  HoldComplete[args], Infinity] & /@ sm5ntParsed];
sm5ntTags = Cases[sm5ntNotebook, HoldPattern[CellTags -> value_String] :> value, Infinity];
sm5ntFamilyTags = Select[sm5ntTags, StringStartsQ[#, "sm5-family-"] &];
sm5ntInputTags = Flatten[Cases[sm5ntNotebook, Cell[_, "Input", options___] :>
  Cases[{options}, HoldPattern[CellTags -> value_String] :> value], Infinity]];
sm5ntConstructionTags = Select[sm5ntInputTags, StringStartsQ[#, "sm5-construction-"] &];
sm5ntCheck["the worked file is one held native notebook", MatchQ[sm5ntNotebook, HoldComplete[_Notebook]]];
sm5ntCheck["all cell groups contain cells", FreeQ[sm5ntNotebook,
  HoldPattern[CellGroupData[children_List, _]] /; !MatchQ[children, {___Cell}]]];
sm5ntCheck["all input cells parse without evaluating them", FreeQ[sm5ntParsed, $Failed | _Syntax]];
sm5ntCheck["Setup and sector overview precede process cells",
  Take[Cases[sm5ntNotebook, Cell[text_String, "Section", ___] :> text, Infinity], 2] ===
    {"Setup", "Sector overview"}];
sm5ntCheck["elementary and complete lower-point SM model roles are separate",
  !FreeQ[sm5ntParsed, HoldPattern[Set[sm, ReadModel[FileNameJoin[{hepcatDirectory, "models"}], 1]]]] &&
  !FreeQ[sm5ntParsed, HoldPattern[Set[smFactorization,
    ReadModel[FileNameJoin[{hepcatDirectory, "models"}], 2]]]] &&
  !FreeQ[sm5ntParsed, HoldPattern[Set[smFeynman,
    ReadModel[FileNameJoin[{hepcatDirectory, "tests", "reference-models", "calchep"}], 1]]]]];
sm5ntCheck["each calculation directly requests the independent Feynman model",
  Length[sm5ntConstructors] >= 4 && AllTrue[sm5ntConstructors,
    MatchQ[#, HoldComplete[_Symbol, sm, smFactorization, {"FeynmanModel" -> smFeynman, ___Rule}]] &&
      FreeQ[#, HoldPattern[("RandomSeed" | "Output") -> _]] &]];
sm5ntCheck["each visible family has its own explicit physical numerical export",
  Length[sm5ntExports] === Length[sm5ntFamilyTags] &&
    AllTrue[sm5ntExports, MatchQ[#, HoldComplete[_, _Rule, _List, sm, ___]] &]];
sm5ntCheck["the case and export tags map to the complete inventory",
  ContainsAll[Lookup[sm5ntInventory["Cases"], "ID"],
    StringDrop[#, StringLength["sm5-construction-"]] & /@ sm5ntConstructionTags] &&
  ContainsAll[Lookup[sm5ntInventory["Families"], "ID"],
    StringDrop[#, StringLength["sm5-family-"]] & /@ sm5ntFamilyTags] &&
  DuplicateFreeQ[sm5ntConstructionTags] && DuplicateFreeQ[sm5ntFamilyTags]];
sm5ntCheck["both numerical comparisons request 1000 points from the shared setting",
  !FreeQ[sm5ntParsed, HoldPattern[Set[validationPoints, 1000]]] &&
  !FreeQ[sm5ntParsed, HoldPattern["CalcHEPPoints" -> validationPoints]] &&
  !FreeQ[sm5ntParsed, HoldPattern["MadGraphPoints" -> validationPoints]] &&
  !FreeQ[sm5ntParsed, HoldPattern["RunCalcHEPValidation" -> True]] &&
  !FreeQ[sm5ntParsed, HoldPattern["RunMadGraphValidation" -> True]]];
sm5ntCheck["full SM reference content and diagonal flavor mixing are retained",
  !FreeQ[sm5ntParsed, HoldPattern["lamWS" -> 0]] &&
    FreeQ[sm5ntParsed, HoldPattern[("CalcHEPAllowedParticles" | "MadGraphAllowedParticles") -> {11, 22}]]];
sm5ntCheck["the output stays below tests/results/spinas-output",
  !FreeQ[sm5ntParsed, HoldPattern[Set[spinasOutputDirectory,
    FileNameJoin[{hepcatDirectory, "tests", "results", "spinas-output", "SM-5-point"}]]]]];
sm5ntCheck["the notebook exposes normal constructors without formatting or stage wrappers",
  FreeQ[sm5ntOrdinaryParsed, _nice | _CreateDiagrams | _PrepareAmplitudes | _CheckAmplitudes] &&
    !AnyTrue[sm5ntInputs, StringContainsQ[#, "// nice" | "//nice"] &]];
sm5ntCheck["user-facing symbols are readable particle names rather than inventory hashes",
  AnyTrue[sm5ntInputs, StringContainsQ[#, "sm5hhhhhAmplitude"] &] &&
    !AnyTrue[sm5ntInputs,
      StringContainsQ[#, RegularExpression["sm5[0-9a-f]{12}(Particles|Amplitude|Export)"]] &]];
sm5ntCheck["export filenames remain distinct on case-insensitive filesystems",
  DuplicateFreeQ[ToLowerCase /@ Lookup[sm5ntInventory["Families"], "ExportName"]]];
sm5ntCheck["the remaining work is stated without a synthetic validation table",
  !FreeQ[sm5ntNotebook, text_String /; StringContainsQ[text, "tests/SM-5-point/inventory.json"]] &&
    FreeQ[sm5ntParsed, _Grid] &&
    Count[sm5ntParsed, _Dataset, Infinity] === Count[sm5ntTags, tag_String /;
      StringStartsQ[tag, "sm5-precision-replay-"]]];

(* Test replay with small synthetic payloads, never substituting them for a
   physics result. The builder only renders cells and never calls constructors. *)
sm5ntTemporary = CreateDirectory[];
sm5ntBuilder = FileNameJoin[{sm5ntRoot, "tests", "SM-5-point", "build-notebook.wl"}];
sm5ntSampleFamily = SelectFirst[sm5ntInventory["Families"], Sort[#["Particles"]] === ConstantArray["h", 5] &];
sm5ntSampleCases = Select[sm5ntInventory["Cases"], MemberQ[sm5ntSampleFamily["CaseIDs"], #["ID"]] &];
sm5ntSampleID = First[sm5ntSampleCases]["ID"];
sm5ntSampleFamilyID = sm5ntSampleFamily["ID"];
sm5ntFixtureInventory = FileNameJoin[{sm5ntTemporary, "inventory.json"}];
sm5ntFixtureNotebook = FileNameJoin[{sm5ntTemporary, "replayed.nb"}];
Export[sm5ntFixtureInventory, <|"Cases" -> sm5ntSampleCases, "Families" -> {sm5ntSampleFamily}|>, "RawJSON"];
Export[FileNameJoin[{sm5ntTemporary, sm5ntSampleID <> ".wxf"}],
  <|"Prints" -> {SequenceForm["synthetic capture ", 17]}, "Expression" -> 3/7|>, "WXF"];
Export[FileNameJoin[{sm5ntTemporary, sm5ntSampleFamilyID <> "-export.wxf"}],
  <|"Prints" -> {SequenceForm["synthetic export ", 23]},
    "Result" -> <|"Directory" -> "synthetic-export-directory"|>|>, "WXF"];
sm5ntBuild[] := Block[{$ScriptCommandLine = {sm5ntBuilder,
    "--inventory", sm5ntFixtureInventory, "--results", sm5ntTemporary,
    "--output", sm5ntFixtureNotebook}}, Get[sm5ntBuilder]];
sm5ntBuild[];
sm5ntReplay = sm5ntRead[sm5ntFixtureNotebook];
sm5ntCheck["captured prints and the exact amplitude output are rendered",
  !FreeQ[sm5ntReplay, Cell[BoxData[ToBoxes[3/7, StandardForm]], "Output", ___]] &&
    !FreeQ[sm5ntReplay, "synthetic capture "]];
sm5ntCheck["successful export output is its actual directory rather than a diagnostic dump",
  !FreeQ[sm5ntReplay, Cell[BoxData[ToBoxes["synthetic-export-directory", StandardForm]], "Output", ___]] &&
    Count[sm5ntReplay, Cell[_, "Print", ___], Infinity] === 2];
(* A local rendering cache must reproduce actual saved content, not become
   an alternate source of amplitudes or validation status. *)
sm5ntCacheCapsule = FileNameJoin[{sm5ntTemporary, sm5ntSampleID <> ".wxf"}];
sm5ntCacheDirectory = FileNameJoin[{sm5ntTemporary, "notebook-display-cache"}];
sm5ntCachePayload = FileNameJoin[{sm5ntCacheDirectory, sm5ntSampleID <> ".wxf"}];
sm5ntCacheManifest = FileNameJoin[{sm5ntCacheDirectory, sm5ntSampleID <> ".json"}];
sm5ntCacheTable = Dataset[{<|"Type" -> "Physical", "Status" -> "PASS", "Value" -> 5/13|>}];
sm5ntCacheRecord = <|"ID" -> sm5ntSampleID, "Status" -> "COMPLETE", "Seconds" -> 7/2,
  "Expression" -> {{Mass[1] -> 7}, 3/7 + SpinorChain["Angle", 1, "Angle", 2]},
  "Prints" -> {SequenceForm["actual fixture milestone ", 17], sm5ntCacheTable},
  "NotebookCells" -> {Cell["actual fixture captured cell", "Print"]},
  "RevalidationPrints" -> {"actual fixture recheck", sm5ntCacheTable},
  "RevalidationSeconds" -> 3/5, "RevalidationCheckTimeConstraint" -> 600,
  "CacheMissWitness" -> "original capsule only",
  "Details" -> <|"RequestedOptions" -> <|"TimeConstraint" -> 3600,
    "CheckOptions" -> {"FeynmanTimeConstraint" -> 600}|>,
    "UnusedSolverDiagnostics" -> Range[4096]|>|>;
Export[sm5ntCacheCapsule, sm5ntCacheRecord, "WXF"];
sm5ntUncached = sm5NotebookRecord[sm5ntCacheCapsule];
sm5ntCached = sm5NotebookRecord[sm5ntCacheCapsule];
sm5ntCheck["display cache preserves the exact amplitude and actual Dataset print objects",
  sm5ntCached["Expression"] === sm5ntCacheRecord["Expression"] &&
  sm5ntCached["Prints"] === sm5ntCacheRecord["Prints"] &&
  sm5ntCached["NotebookCells"] === sm5ntCacheRecord["NotebookCells"] &&
  sm5ntCached["RevalidationPrints"] === sm5ntCacheRecord["RevalidationPrints"] &&
  sm5ntCached["RevalidationSeconds"] === 3/5 &&
  sm5ntCached["RevalidationCheckTimeConstraint"] === 600];
sm5ntCheck["cached rendering and explicit controls equal the uncached saved record",
  sm5NotebookCaptured[sm5ntCached, "Construction"] === sm5NotebookCaptured[sm5ntUncached, "Construction"] &&
  sm5NotebookConstructionControls[sm5ntCached] === sm5NotebookConstructionControls[sm5ntUncached] &&
  sm5ntCached["Status"] === sm5ntCacheRecord["Status"]];
sm5ntCheck["display projection omits unconsumed solver diagnostics and binds both file checksums",
  !KeyExistsQ[sm5ntCached, "CacheMissWitness"] &&
  Keys[sm5ntCached["Details"]] === {"RequestedOptions"} &&
  With[{metadata = Import[sm5ntCacheManifest, "RawJSON"]},
    metadata["CapsuleSHA256"] === sm5NotebookDisplayHash[sm5ntCacheCapsule] &&
    metadata["PayloadSHA256"] === sm5NotebookDisplayHash[sm5ntCachePayload] &&
    metadata["DisplaySourceSHA256"] === sm5NotebookDisplaySourceSHA256]];
sm5ntChangedCacheRecord = Join[sm5ntCacheRecord, <|"Expression" -> 11/13|>];
Export[sm5ntCacheCapsule, sm5ntChangedCacheRecord, "WXF"];
sm5ntChangedCapsule = sm5NotebookRecord[sm5ntCacheCapsule];
sm5ntCheck["changing the real capsule rejects an otherwise intact old display cache",
  sm5ntChangedCapsule["Expression"] === 11/13 && KeyExistsQ[sm5ntChangedCapsule, "CacheMissWitness"]];
sm5ntChangedSource = Block[{sm5NotebookDisplaySourceSHA256 = "different builder source"},
  sm5NotebookRecord[sm5ntCacheCapsule]];
sm5ntCheck["changing the display source fingerprint reloads the original capsule",
  KeyExistsQ[sm5ntChangedSource, "CacheMissWitness"] && sm5ntChangedSource["Expression"] === 11/13];
sm5ntChangedSchema = Block[{sm5NotebookDisplayCacheSchema = sm5NotebookDisplayCacheSchema + 1}, sm5NotebookRecord[sm5ntCacheCapsule]];
sm5ntCheck["changing the display schema reloads the original capsule",
  KeyExistsQ[sm5ntChangedSchema, "CacheMissWitness"] && sm5ntChangedSchema["Expression"] === 11/13];
(* Populate the real schema again, then corrupt only the cached payload. *)
sm5NotebookRecord[sm5ntCacheCapsule];
Export[sm5ntCachePayload, <|"Status" -> "PASS", "Expression" -> "NOT_A_VALIDATED_AMPLITUDE"|>, "WXF"];
sm5ntCorruptCache = sm5NotebookRecord[sm5ntCacheCapsule];
sm5ntCheck["a corrupt payload cannot replace the real amplitude or status",
  sm5ntCorruptCache["Status"] === "COMPLETE" && sm5ntCorruptCache["Expression"] === 11/13 &&
  KeyExistsQ[sm5ntCorruptCache, "CacheMissWitness"]];
DeleteFile[sm5ntCacheManifest];
sm5ntMissingCacheManifest = sm5NotebookRecord[sm5ntCacheCapsule];
sm5ntCheck["a payload without its checksum manifest cannot supply output",
  KeyExistsQ[sm5ntMissingCacheManifest, "CacheMissWitness"] &&
  sm5ntMissingCacheManifest["Expression"] === 11/13];
DeleteFile[sm5ntCacheCapsule];
sm5ntCheck["a display cache cannot supply an amplitude when the actual capsule is absent",
  sm5NotebookRecord[sm5ntCacheCapsule] === <||>];
sm5ntFailedCacheRecord = <|"Status" -> "FAIL", "Expression" -> Missing["NoAmplitude"],
  "Prints" -> {"actual fixture failure"}, "Details" -> Failure["ActualFixtureFailure",
    <|"MessageTemplate" -> "actual fixture failure", "Diagnostics" -> Range[4096]|>]|>;
Export[sm5ntCacheCapsule, sm5ntFailedCacheRecord, "WXF"];
sm5ntFailedCached = sm5NotebookRecord[sm5ntCacheCapsule];
sm5ntFailedWarm = sm5NotebookRecord[sm5ntCacheCapsule];
sm5ntCheck["failed display cache preserves status and reason while the original full report stays intact",
  sm5ntFailedCached === sm5ntFailedWarm && sm5ntFailedWarm["Status"] === "FAIL" &&
  sm5ntFailedWarm["FailurePreview"]["Reason"] === "actual fixture failure" &&
  FreeQ[sm5ntFailedWarm, _Failure] && !KeyExistsQ[sm5ntFailedWarm, "Expression"] &&
  Import[sm5ntCacheCapsule, "WXF"] === sm5ntFailedCacheRecord];
sm5ntExportProjection = sm5NotebookDisplayProjection[<|"Status" -> "PASS",
  "Prints" -> {"actual fixture export"}, "Result" -> <|"Directory" -> "actual-fixture-directory",
    "UnusedBackendDiagnostics" -> Range[4096]|>|>];
sm5ntCheck["successful export projection preserves its real displayed directory and prints",
  sm5ntExportProjection === <|"Status" -> "PASS", "Prints" -> {"actual fixture export"},
    "Result" -> <|"Directory" -> "actual-fixture-directory"|>|> &&
  sm5NotebookDisplayProjection[<|"Status" -> "FAIL", "Result" ->
    Failure["ActualFixtureExportFailure", <||>]|>]["FailurePreview"]["Status"] === "FAIL"];

(* A failed constructor can retain enormous affine fits and previous color
   coefficients. Display never embeds them, even in captured native cells. *)
sm5ntFullDataWitness = "DO_NOT_EMBED_FULL_FAILED_DIAGNOSTICS";
sm5ntLargeFailure = Failure["ActualLargeFixtureFailure", <|
  "MessageTemplate" -> "actual coefficient remained unresolved", "Stage" -> "Completing factorization",
  "ColorCoefficientReports" -> ConstantArray[sm5ntFullDataWitness, 20000],
  "FittedPointCheckpoint" -> Range[20000]|>];
sm5ntFailureTable = Dataset[{<|"Type" -> "Physical", "Status" -> "FAIL", "Test" -> "actual failed fixture cut"|>}];
sm5ntLargeRecord = <|"ID" -> sm5ntSampleID, "Status" -> "UNRESOLVED", "Seconds" -> 23,
 "Expression" -> sm5ntLargeFailure, "Details" -> sm5ntLargeFailure,
 "Prints" -> {"actual unresolved fixture milestone", sm5ntFailureTable,
   <|"FullData" -> ConstantArray[sm5ntFullDataWitness, 5000]|>},
 "NotebookCells" -> {Cell[BoxData[RowBox[{sm5ntFullDataWitness}]], "Output"]}|>;
Export[sm5ntCacheCapsule, sm5ntLargeRecord, "WXF"];
sm5ntOriginalFailureHash = sm5NotebookDisplayHash[sm5ntCacheCapsule];
sm5ntLargeCold = sm5NotebookRecord[sm5ntCacheCapsule];sm5ntLargeWarm = sm5NotebookRecord[sm5ntCacheCapsule];
sm5ntFailureCells = sm5NotebookCaptured[sm5ntLargeWarm, "Construction"];
sm5ntCheck["cold and warm failed records never embed full affine diagnostics or captured failure boxes",
 sm5ntLargeCold === sm5ntLargeWarm && ByteCount[sm5ntLargeWarm] < 30000 &&
 FreeQ[sm5ntLargeWarm, sm5ntFullDataWitness | _Failure] &&
 FreeQ[sm5ntFailureCells, sm5ntFullDataWitness | _Failure] && ByteCount[sm5ntFailureCells] < 100000];
sm5ntCheck["the actual unresolved status stage reason and exact capsule path remain visible",
 !FreeQ[sm5ntFailureCells, text_String /; StringContainsQ[text, "UNRESOLVED"] &&
   StringContainsQ[text, "Completing factorization"] && StringContainsQ[text, "actual coefficient remained unresolved"] &&
   StringContainsQ[text, ExpandFileName[sm5ntCacheCapsule]]]];
sm5ntInspectionCells = Cases[sm5ntFailureCells, Cell[text_String, "Input", opts___] :> {text, {opts}}, Infinity];
sm5ntCheck["the full report is available through an explicitly disabled valid Import command",
 Length[sm5ntInspectionCells] === 1 && MemberQ[sm5ntInspectionCells[[1, 2]], Evaluatable -> False] &&
 !FreeQ[ToExpression[sm5ntInspectionCells[[1, 1]], InputForm, HoldComplete], HoldPattern[Import[_, "WXF"]]]];
sm5ntCheck["actual failed validation tables and milestones are retained without a synthetic pass",
 MemberQ[sm5ntLargeWarm["Prints"], sm5ntFailureTable] &&
 MemberQ[sm5ntLargeWarm["Prints"], "actual unresolved fixture milestone"] &&
 sm5ntContains[sm5ntFailureCells, "actual unresolved fixture milestone"] &&
 sm5ntContains[sm5ntFailureCells, "actual failed fixture cut"] &&
 Lookup[Normal[sm5ntFailureTable], "Status"] === {"FAIL"} &&
 sm5ntLargeWarm["FailurePreview"]["Status"] === "UNRESOLVED"];
sm5ntCheck["omitted diagnostic payloads are explicitly identified and original proof bytes do not change",
 !FreeQ[sm5ntFailureCells, text_String /; StringContainsQ[text, "saved report"] && StringContainsQ[text, "not been removed"]] &&
 Import[sm5ntCacheCapsule, "WXF"] === sm5ntLargeRecord &&
 sm5NotebookDisplayHash[sm5ntCacheCapsule] === sm5ntOriginalFailureHash];
sm5ntHugeDatasetWitness = "OVERSIZED_FAILED_DATASET_REMAINS_IN_WXF";
sm5ntHugeFailureTable = Dataset[{<|"Type" -> "Physical", "Status" -> "UNRESOLVED",
 "Test" -> sm5ntHugeDatasetWitness, "ResidualData" -> Range[400000]|>}];
sm5ntHugeTableRecord = Join[sm5ntLargeRecord, <|"Prints" -> {
 "actual unresolved fixture milestone", sm5ntFailureTable, sm5ntHugeFailureTable}|>];
Export[sm5ntCacheCapsule, sm5ntHugeTableRecord, "WXF"];
sm5ntHugeTableHash = sm5NotebookDisplayHash[sm5ntCacheCapsule];
sm5ntHugeTablePreview = sm5NotebookRecord[sm5ntCacheCapsule];
sm5ntHugeTableCells = sm5NotebookCaptured[sm5ntHugeTablePreview, "Construction"];
sm5ntCheck["an oversized failed Dataset is omitted explicitly without changing its original table",
 ByteCount[sm5ntHugeFailureTable] > 2^20 &&
 MemberQ[sm5ntHugeTablePreview["Prints"], sm5ntFailureTable] &&
 !MemberQ[sm5ntHugeTablePreview["Prints"], sm5ntHugeFailureTable] &&
 FreeQ[sm5ntHugeTableCells, sm5ntHugeDatasetWitness] &&
 sm5ntContains[sm5ntHugeTableCells, "not been removed"] &&
 Import[sm5ntCacheCapsule, "WXF"] === sm5ntHugeTableRecord &&
 sm5NotebookDisplayHash[sm5ntCacheCapsule] === sm5ntHugeTableHash];
sm5ntCheck["completed-record projections retain full Dataset data irrespective of failure preview limits",
 !sm5NotebookSmallDiagnosticQ[sm5ntHugeFailureTable] &&
 sm5NotebookDisplayProjection[<|"Status" -> "COMPLETE", "Prints" -> {sm5ntHugeFailureTable},
   "Expression" -> 3/7|>]["Prints"] === {sm5ntHugeFailureTable}];
sm5ntLargeRecord2 = Join[sm5ntLargeRecord, <|"Status" -> "FAIL"|>];
Export[sm5ntCacheCapsule, sm5ntLargeRecord2, "WXF"];
sm5ntCheck["a changed real failed capsule invalidates its old compact preview",
 sm5NotebookRecord[sm5ntCacheCapsule]["FailurePreview"]["Status"] === "FAIL"];
sm5ntCheck["successful formulas and successful captured cells retain their existing exact output",
 !FreeQ[sm5NotebookCaptured[<|"Status" -> "COMPLETE", "Expression" -> 3/7,
   "Prints" -> {sm5ntCacheTable}|>, "Construction"], Cell[BoxData[ToBoxes[3/7, StandardForm]], "Output", ___]] &&
 sm5NotebookCaptured[<|"Status" -> "COMPLETE", "NotebookCells" -> {Cell["exact successful captured cell", "Print"]}|>, "Construction"] === {Cell["exact successful captured cell", "Print"]}];

(* Explicit controls come from RequestedOptions, never resolved resource policy. *)
sm5ntDefaultControlRecord = <|"Expression" -> 3/7, "Prints" -> {}, "Details" -> <|
  "RequestedOptions" -> <|"TimeConstraint" -> Automatic, "CheckOptions" -> {},
    "RandomSeed" -> 12345, "Output" -> "Details"|>,
  "Options" -> <|"TimeConstraint" -> 3600|>|>|>;
sm5ntCheck["automatic construction effort and internal output/seed remain omitted",
  sm5NotebookConstructionControls[sm5ntDefaultControlRecord] === {}];
sm5ntExplicitRecord = <|"Expression" -> 3/7, "Prints" -> {"synthetic controlled run"}, "Details" -> <|
  "RequestedOptions" -> <|"TimeConstraint" -> 3600,
    "CheckOptions" -> {"FeynmanTimeConstraint" -> 600, "RandomSeed" -> 12345,
      "Output" -> "Details", "HighEnergyTimeConstraint" -> Automatic},
    "RandomSeed" -> 12345, "Output" -> "Details"|>|>|>;
Export[FileNameJoin[{sm5ntTemporary, sm5ntSampleID <> ".wxf"}], sm5ntExplicitRecord, "WXF"];
sm5ntBuild[]; sm5ntExplicitReplay = sm5ntRead[sm5ntFixtureNotebook];
sm5ntExplicitInputs = ToExpression[#, InputForm, HoldComplete] & /@
  Cases[sm5ntExplicitReplay, Cell[text_String, "Input", ___] :> text, Infinity];
sm5ntCheck["saved explicit construction and Feynman allowances appear in the literal input",
  !FreeQ[sm5ntExplicitInputs, HoldPattern[ConstructAmplitude[_, sm, smFactorization,
    "FeynmanModel" -> smFeynman, "TimeConstraint" -> 3600,
    "CheckOptions" -> {"FeynmanTimeConstraint" -> 600}]]]];
sm5ntCheck["diagnostic RandomSeed and Output never leak into the generated construction input",
  FreeQ[sm5ntExplicitInputs, HoldPattern[("RandomSeed" | "Output") -> _]]];
sm5ntCheck["explicit controls add a short replaceable provenance note",
  !FreeQ[sm5ntExplicitReplay, Cell[_, "Text", GeneratedCell -> True, CellAutoOverwrite -> True,
    CellTags -> "sm5-construction-controls-" <> sm5ntSampleID]]];
Export[FileNameJoin[{sm5ntTemporary, sm5ntSampleID <> ".wxf"}], sm5ntDefaultControlRecord, "WXF"];
sm5ntBuild[];sm5ntDefaultReplay = sm5ntRead[sm5ntFixtureNotebook];
sm5ntCheck["ordinary default records retain simple input and no explicit-control note",
  FreeQ[sm5ntDefaultReplay, "sm5-construction-controls-" <> sm5ntSampleID] &&
    FreeQ[Cases[sm5ntDefaultReplay, Cell[txt_String, "Input", ___] :> txt, Infinity],
      txt_String /; StringContainsQ[txt, "CheckOptions" | "TimeConstraint"]]];
sm5ntCheck["checker associations retain first-rule semantics without internal options",
  sm5NotebookConstructionControls[<|"Details" -> <|"RequestedOptions" -> <|"CheckOptions" ->
    <|"FeynmanTimeConstraint" -> 600, "Output" -> "Details", "RandomSeed" -> 123|>|>|>|>] ===
      {"CheckOptions" -> {"FeynmanTimeConstraint" -> 600}}];
sm5ntCheck["duplicate checker options preserve the package's first-rule precedence",
  sm5NotebookConstructionControls[<|"Details" -> <|"RequestedOptions" -> <|"CheckOptions" ->
    {"FeynmanTimeConstraint" -> 600, "FeynmanTimeConstraint" -> 1200}|>|>|>] ===
      {"CheckOptions" -> {"FeynmanTimeConstraint" -> 600}}];
sm5ntCheck["meaningful empty-list checker values are retained rather than treated as an empty bag",
  sm5NotebookConstructionControls[<|"Details" -> <|"RequestedOptions" -> <|"CheckOptions" ->
    {"SpuriousPoles" -> {}, "ParameterRules" -> Automatic}|>|>|>] ===
      {"CheckOptions" -> {"SpuriousPoles" -> {}}}];
sm5ntCheck["retained-check allowances are not misrepresented as original constructor options",
  sm5NotebookConstructionControls[<|"RevalidationCheckTimeConstraint" -> 600|>] === {}];
sm5ntCheck["failure details are not misread as requested options",
  Quiet[Check[sm5NotebookConstructionControls[<|"Details" -> Failure["Synthetic", <||>]|>], $Failed]] === {}];
sm5ntCheck["a checker bag containing only internal modes adds no control",
  sm5NotebookConstructionControls[<|"Details" -> <|"RequestedOptions" -> <|"CheckOptions" ->
    {"RandomSeed" -> 123, "Output" -> "Details"}|>|>|>] === {}];
sm5ntCheck["an all-Automatic checker bag adds no control",
  sm5NotebookConstructionControls[<|"Details" -> <|"RequestedOptions" -> <|"CheckOptions" ->
    {"FeynmanTimeConstraint" -> Automatic, "TimeConstraint" -> Automatic}|>|>|>] === {}];
sm5ntRetainedCell = Cell["synthetic preserved output", "Print"];
Export[FileNameJoin[{sm5ntTemporary, sm5ntSampleID <> ".wxf"}],
  <|"NotebookCells" -> {sm5ntRetainedCell}, "Prints" -> {"unused fallback"}, "Expression" -> 99|>, "WXF"];
sm5ntBuild[];
sm5ntReplayPreferred = sm5ntRead[sm5ntFixtureNotebook];
sm5ntCheck["captured native cells take priority without inventing another output",
  !FreeQ[sm5ntReplayPreferred, sm5ntRetainedCell] &&
    FreeQ[sm5ntReplayPreferred, "unused fallback"] &&
    FreeQ[sm5ntReplayPreferred, Cell[BoxData["99"], "Output", ___]]];
Export[FileNameJoin[{sm5ntTemporary, sm5ntSampleID <> ".wxf"}],
  <|"Prints" -> {}, "Status" -> "TIMEOUT", "Seconds" -> 42,
    "Expression" -> Missing["NoAmplitude"],
    "Details" -> Failure["SyntheticCapturedFailure", <|"MessageTemplate" -> "synthetic solver failure"|>]|>, "WXF"];
sm5ntBuild[];
sm5ntReplayFailure = sm5ntRead[sm5ntFixtureNotebook];
sm5ntCheck["a missing amplitude retains the actual recorded failure details",
  sm5ntContains[sm5ntReplayFailure, "synthetic solver failure"]];
Export[FileNameJoin[{sm5ntTemporary, sm5ntSampleID <> ".wxf"}],
  <|"Prints" -> {}, "Status" -> "TIMEOUT", "Seconds" -> 42,
    "Expression" -> Missing["NoAmplitude"]|>, "WXF"];
sm5ntBuild[];
sm5ntReplayTimeout = sm5ntRead[sm5ntFixtureNotebook];
sm5ntCheck["an interrupted run without detailed output displays its recorded status",
  sm5ntContains[sm5ntReplayTimeout, "RecordedSMFivePointRun"] && sm5ntContains[sm5ntReplayTimeout, "TIMEOUT"]];
Export[FileNameJoin[{sm5ntTemporary, sm5ntSampleID <> ".wxf"}],
  <|"ID" -> sm5ntSampleID, "Prints" -> {"synthetic original construction"}, "Expression" -> 3/7,
    "RevalidationPrints" -> {"synthetic current native checks"}, "RevalidationSeconds" -> 1.25|>, "WXF"];
sm5ntBuild[];
sm5ntReplayRevalidation = sm5ntRead[sm5ntFixtureNotebook];
sm5ntRecheckGroups = Cases[sm5ntReplayRevalidation,
  HoldPattern[CellGroupData[{Cell[_, "Input", options___], rest___}, _]] /;
      MemberQ[{options}, CellTags -> "sm5-construction-" <> sm5ntSampleID] :> {rest}, Infinity];
sm5ntCheck["actual revalidation prints follow construction prints and precede the amplitude",
  Length[sm5ntRecheckGroups] === 1 &&
    (Cases[First[sm5ntRecheckGroups], Cell[_, style_String, ___] :> style, {1}] ===
      {"Print", "Text", "Print", "Output"}) &&
    !FreeQ[sm5ntReplayRevalidation, text_String /; StringContainsQ[text, "unchanged saved amplitude"]]];
sm5ntCheck["fresh input evaluation can replace the generated revalidation provenance",
  !FreeQ[sm5ntReplayRevalidation, Cell[_, "Text", GeneratedCell -> True, CellAutoOverwrite -> True,
    CellTags -> "sm5-revalidation-" <> sm5ntSampleID]]];
Export[FileNameJoin[{sm5ntTemporary, sm5ntSampleID <> ".wxf"}],
  <|"ID" -> sm5ntSampleID, "Prints" -> {"synthetic construction"}, "Expression" -> 3/7,
    "RevalidationPrints" -> {"synthetic native checks"}, "RevalidationSeconds" -> 1.25,
    "RevalidationCheckTimeConstraint" -> 600|>, "WXF"];
sm5ntBuild[];
sm5ntCheck["an explicit retained-check allowance is visible without changing construction timing",
  !FreeQ[sm5ntRead[sm5ntFixtureNotebook], text_String /;
    StringContainsQ[text, "explicit 600 s checker limit"]]];
sm5ntRevalidator = sm5ntRead[FileNameJoin[{sm5ntRoot, "tests", "SM-5-point", "revalidate.wl"}]];
sm5ntCheck["the retained-amplitude revalidator parses and never invokes construction",
  MatchQ[sm5ntRevalidator, _HoldComplete] && FreeQ[sm5ntRevalidator, _Syntax | _ConstructAmplitude]];
sm5ntCheck["revalidation retains native default mandatory checks",
  !FreeQ[sm5ntRevalidator, HoldPattern[CheckAmplitudes[expression, channels, factorizationModel,
      "FeynmanAmplitudes" -> benchmark, "FeynmanModel" -> feynmanModel,
      "Output" -> "Details", "RandomSeed" -> seed, "TimeConstraint" -> nativeCheckSeconds]]] &&
    FreeQ[sm5ntRevalidator, HoldPattern[(("HighEnergyCheck" | "ReferenceCheck") -> False) |
      ("SpuriousPoles" -> {})]]];
sm5ntPrecisionSample = <|"Results" -> <|
  "Native" -> <|"MantissaBits" -> 53, "Points" -> 1000, "Mismatches" -> 17, "Errors" -> 0,
    "MaximumRelativeError" -> 4.*^-7, "Status" -> "FAIL"|>,
  "GCC53" -> <|"MantissaBits" -> 53, "Points" -> 1000, "Mismatches" -> 18, "Errors" -> 0,
    "MaximumRelativeError" -> 5.*^-7, "Status" -> "FAIL"|>,
  "GCC128" -> <|"MantissaBits" -> 113, "Points" -> 1000, "Mismatches" -> 0, "Errors" -> 0,
    "MaximumRelativeError" -> 2.*^-8, "Status" -> "PASS"|>|>|>;
sm5ntPrecisionRows = sm5NotebookPrecisionRows[sm5ntPrecisionSample];
sm5ntCheck["precision replay presentation preserves raw default failures and actual numerical fields",
  Lookup[sm5ntPrecisionRows, "Status"] === {"FAIL", "FAIL", "PASS"} &&
    Lookup[sm5ntPrecisionRows, "Mismatches"] === {17, 18, 0} &&
    Lookup[sm5ntPrecisionRows, "MantissaBits"] === {53, 53, 113}];
sm5ntCheck["repeated notebook replacement leaves a complete notebook and no temporary output",
  MatchQ[sm5ntRead[sm5ntFixtureNotebook], HoldComplete[_Notebook]] &&
    FileNames[".*.tmp", sm5ntTemporary] === {}];
sm5ntCheck["syntax whitespace cleanup preserves spaces inside multiline string literals",
  sm5NotebookTrimSyntaxWhitespace["Notebook[{  \nCell[\"kept  \nkept\\\"  \n\", \"Text\"]  \n}]  \n"] ===
    "Notebook[{\nCell[\"kept  \nkept\\\"  \n\", \"Text\"]\n}]\n"];
sm5ntCheck["closing quotes do not hide syntax whitespace before the next string",
  sm5NotebookTrimSyntaxWhitespace["Cell[\"first\",  \n\"Text\"]  \nCell[\"second\",  \n\"Output\"]  \n"] ===
    "Cell[\"first\",\n\"Text\"]\nCell[\"second\",\n\"Output\"]\n"];
sm5ntHeartbeat = Row[{"Preparing diagrams", ": working; stage ", 1.25, " s; total ", 2.5}];
sm5ntMilestone = Row[{"Total time: ", 2.5}];
sm5ntVisible = sm5NotebookVisiblePrints[{sm5ntHeartbeat, sm5ntMilestone,
  "Completing factorization: UNRESOLVED", Dataset[{<|"Status" -> "FAIL"|>}]}];
sm5ntCheck["saved previews omit only routine heartbeats and retain milestones and failures",
  Length[sm5ntVisible] === 3 && FreeQ[sm5ntVisible, sm5ntHeartbeat] &&
  MemberQ[sm5ntVisible, sm5ntMilestone] && !FreeQ[sm5ntVisible, _Dataset] &&
  !FreeQ[sm5ntVisible, "Completing factorization: UNRESOLVED"]];
sm5ntCheck["plain and multi-argument heartbeats use the same presentation filter",
  sm5NotebookVisiblePrints[{"Preparing diagrams: working; stage 1 s; total 2 s",
    SequenceForm["Preparing diagrams", ": working; stage ", 1], "actual warning"}] === {"actual warning"}];
(* Summary-only attempts select nonstarter families but never invent output. *)
sm5ntGluonFamily = SelectFirst[sm5ntInventory["Families"], #["Sector"] === "PureGluon" &];
sm5ntGluonCases = Select[sm5ntInventory["Cases"], MemberQ[sm5ntGluonFamily["CaseIDs"], #["ID"]] &];
sm5ntGluonID = First[sm5ntGluonCases]["ID"];
Export[sm5ntFixtureInventory, <|"Cases" -> sm5ntGluonCases, "Families" -> {sm5ntGluonFamily}|>, "RawJSON"];
sm5ntSummaryPath = FileNameJoin[{sm5ntTemporary, sm5ntGluonID <> ".json"}];
Export[sm5ntSummaryPath, <|"ID" -> sm5ntGluonID, "Status" -> "TIMEOUT", "Seconds" -> 42,
  "Message" -> "synthetic external deadline", "Expression" -> "NOT_AN_AMPLITUDE"|>, "RawJSON"];
sm5ntBuild[]; sm5ntSummaryTimeout = sm5ntRead[sm5ntFixtureNotebook];
sm5ntCheck["a JSON-only attempted nonstarter family is included automatically",
  !FreeQ[sm5ntSummaryTimeout, "sm5-family-" <> sm5ntGluonFamily["ID"]] &&
    sm5ntContains[sm5ntSummaryTimeout, "RecordedSMFivePointRun"] && sm5ntContains[sm5ntSummaryTimeout, "TIMEOUT"]];
sm5ntCheck["a summary-only timeout retains its recorded time without an expression or validation",
  sm5NotebookRecord[FileNameJoin[{sm5ntTemporary, sm5ntGluonID <> ".wxf"}]]["Seconds"] === 42 &&
    FreeQ[sm5ntSummaryTimeout, "NOT_AN_AMPLITUDE" | _Dataset]];
Do[Export[sm5ntSummaryPath, <|"ID" -> sm5ntGluonID, "Status" -> status, "Seconds" -> 19|>, "RawJSON"];
  sm5ntBuild[];
  sm5ntCheck["summary-only " <> status <> " retains its actual unsuccessful status",
    sm5ntContains[sm5ntRead[sm5ntFixtureNotebook], status]], {status, {"ERROR", "INTERRUPTED"}}];
Export[sm5ntSummaryPath, <|"ID" -> sm5ntGluonID, "Status" -> "COMPLETE", "Seconds" -> 7,
  "Expression" -> "NOT_AN_AMPLITUDE"|>, "RawJSON"];
sm5ntBuild[];sm5ntMissingComplete = sm5ntRead[sm5ntFixtureNotebook];
sm5ntCheck["a COMPLETE summary without its capsule is not displayed as a completed amplitude",
  sm5ntContains[sm5ntMissingComplete, "MissingSavedSMFivePointCapsule"] &&
    FreeQ[sm5ntMissingComplete, "NOT_AN_AMPLITUDE"]];
Export[FileNameJoin[{sm5ntTemporary, sm5ntGluonFamily["ID"] <> "-export.json"}],
  <|"FamilyID" -> sm5ntGluonFamily["ID"], "Status" -> "PASS", "Seconds" -> 8,
    "Result" -> <|"Directory" -> "NOT_AN_EXPORT"|>|>, "RawJSON"];
sm5ntBuild[];sm5ntMissingExport = sm5ntRead[sm5ntFixtureNotebook];
sm5ntCheck["a PASS summary without its export capsule supplies no successful output",
  sm5ntContains[sm5ntMissingExport, "MissingSavedSMFivePointCapsule"] &&
    FreeQ[sm5ntMissingExport, "NOT_AN_EXPORT"]];
DeleteFile[sm5ntSummaryPath];DeleteFile[FileNameJoin[{sm5ntTemporary, sm5ntGluonFamily["ID"] <> "-export.json"}]];
Export[sm5ntSummaryPath, <|"ID" -> "different-case", "Status" -> "TIMEOUT"|>, "RawJSON"];
sm5ntBuild[];
sm5ntCheck["a mismatched JSON identity is not attributed to a different process family",
  FreeQ[sm5ntRead[sm5ntFixtureNotebook], "sm5-family-" <> sm5ntGluonFamily["ID"]]];
(* Supplemental parameter probes use synthetic, hash-bound captures here.
   No real local capsule or scattering calculation is required by these tests. *)
sm5ntProbeSource = FileNameJoin[{sm5ntTemporary, sm5ntSampleID <> ".wxf"}];
sm5ntProbeDirectory = FileNameJoin[{sm5ntTemporary, "probes"}];
CreateDirectory[sm5ntProbeDirectory];
sm5ntProbeCapsule = FileNameJoin[{sm5ntProbeDirectory, "probe.wxf"}];
sm5ntProbeSummary = FileNameJoin[{sm5ntProbeDirectory, "probe.json"}];
sm5ntProbeScript = FileNameJoin[{sm5ntProbeDirectory, "probe.wl"}];
sm5ntProbeConversion = FileNameJoin[{sm5ntProbeDirectory, "conversion.wl"}];
sm5ntProbeTable = Dataset[{<|"Type" -> "Physical", "Test" -> "synthetic exact cut", "Status" -> "FAIL"|>}];
sm5ntProbeOriginal = <|"ID" -> sm5ntSampleID, "Status" -> "COMPLETE", "Expression" -> 3/7,
  "Prints" -> {Dataset[{<|"Status" -> "PASS", "Test" -> "original declared-input fixture"|>}]}|>;
Export[sm5ntProbeSource, sm5ntProbeOriginal, "WXF"];
sm5ntProbeSourceHash = sm5NotebookDisplayHash[sm5ntProbeSource];
sm5ntProbeRecord = <|"SourceCaseID" -> sm5ntSampleID,
  "SourceCapsuleSHA256" -> sm5ntProbeSourceHash, "ContextSHA256" -> "fixture-context",
  "Status" -> "CHECKS_NOT_ALL_PASS", "StatusCounts" -> <|"FAIL" -> 1|>,
  "SourceAmplitudeUnchanged" -> True, "NoExpressionMassSlotOrFlavorTransport" -> True,
  "ParameterRules" -> {"fixtureMass" -> 1/2000}, "RandomSeed" -> 101,
  "Report" -> <|"Checks" -> {<|"Type" -> "Physical", "Status" -> "FAIL"|>},
    "Table" -> sm5ntProbeTable|>|>;
Export[sm5ntProbeCapsule, sm5ntProbeRecord, "WXF"];
Export[sm5ntProbeSummary, KeyDrop[sm5ntProbeRecord, {"Report", "ParameterRules"}], "RawJSON"];
Export[sm5ntProbeScript, "(* synthetic captured public checker *)", "Text"];
Export[sm5ntProbeConversion, "(* synthetic captured independent conversion *)", "Text"];
sm5ntProbeEntry = <|"ID" -> "fixture-parameter-check", "CaseID" -> sm5ntSampleID,
  "Kind" -> "ParameterCheck", "Title" -> "Additional fixture parameter check",
  "Note" -> "The original fixture result is retained; this supplementary input fails.",
  "InputTemplate" -> "$CASEProbe = CheckAmplitudes[$CASEAmplitude, {}, smFactorization, " <>
    "\"ParameterRules\" -> $PARAMETER_RULES, \"RandomSeed\" -> $RANDOM_SEED, " <>
    "\"TimeConstraint\" -> $CHECK_SECONDS, \"Output\" -> \"Details\"];\n$CASEProbe[\"Table\"]",
  "SourceCapsuleSHA256" -> sm5ntProbeSourceHash, "ProbeContextSHA256" -> "fixture-context",
  "Status" -> "CHECKS_NOT_ALL_PASS", "StatusCounts" -> <|"FAIL" -> 1|>, "CheckTimeConstraint" -> 90,
  "Files" -> AssociationThread[{"Capsule", "Summary", "Script", "ConversionScript"},
    (<|"Path" -> FileNameJoin[{"probes", FileNameTake[#]}], "SHA256" -> sm5NotebookDisplayHash[#]|> & /@
      {sm5ntProbeCapsule, sm5ntProbeSummary, sm5ntProbeScript, sm5ntProbeConversion})]|>;
sm5ntProbeVerified = sm5NotebookVerifiedProbe[sm5ntProbeEntry, sm5ntSampleID];
sm5ntCheck["supplemental fixture admission retains the actual failed checker table",
  AssociationQ[sm5ntProbeVerified] && sm5ntProbeVerified["Table"] === sm5ntProbeTable];
sm5ntProbeCells = Block[{sm5NotebookProbes = {sm5ntProbeEntry},
    sm5NotebookCasesByID = AssociationThread[Lookup[sm5ntSampleCases, "ID"], sm5ntSampleCases]},
  sm5NotebookSupplementalProbeCells[First[sm5ntSampleCases]]];
sm5ntProbeInput = First[Cases[sm5ntProbeCells, Cell[text_String, "Input", ___] :> text, Infinity]];
sm5ntProbeParsed = ToExpression[sm5ntProbeInput, InputForm, HoldComplete];
sm5ntCheck["supplemental literal input uses the same case variable and actual public parameters",
  StringContainsQ[sm5ntProbeInput, "sm5" <> sm5NotebookProcessName[First[sm5ntSampleCases]["Particles"]] <> "Amplitude"] &&
  !StringContainsQ[sm5ntProbeInput, "$CASE" | "$PARAMETER_RULES" | "$RANDOM_SEED" | "$CHECK_SECONDS"] &&
  !FreeQ[sm5ntProbeParsed, HoldPattern["ParameterRules" -> {"fixtureMass" -> 1/2000}]] &&
  !FreeQ[sm5ntProbeParsed, HoldPattern["RandomSeed" -> 101]] &&
  !FreeQ[sm5ntProbeParsed, HoldPattern["TimeConstraint" -> 90]]];
sm5ntCheck["rendering a supplemental input does not run the checker or mutate its source",
  Import[sm5ntProbeSource, "WXF"] === sm5ntProbeOriginal &&
    !FreeQ[sm5ntProbeCells, "synthetic exact cut"] &&
    Count[sm5ntProbeCells, Cell[_, "Output", ___], Infinity] === 1];
sm5ntCheck["supplemental evidence for a different source identity is rejected",
  sm5NotebookVerifiedProbe[Join[sm5ntProbeEntry, <|"CaseID" -> "different-case"|>], sm5ntSampleID] === $Failed];
sm5ntCheck["a changed source capsule rejects the supplemental capture",
  sm5NotebookVerifiedProbe[Join[sm5ntProbeEntry, <|"SourceCapsuleSHA256" -> "wrong"|>], sm5ntSampleID] === $Failed];
sm5ntCheck["a forged raw status count cannot supply a supplemental table",
  sm5NotebookVerifiedProbe[Join[sm5ntProbeEntry, <|"StatusCounts" -> <|"PASS" -> 1|>|>], sm5ntSampleID] === $Failed];
sm5ntProbeBadFile = Join[sm5ntProbeEntry, <|"Files" -> Join[sm5ntProbeEntry["Files"],
  <|"Script" -> <|"Path" -> "../outside.wl", "SHA256" -> "wrong"|>|>]|>];
sm5ntCheck["supplemental artifact paths cannot escape the result directory",
  sm5NotebookVerifiedProbe[sm5ntProbeBadFile, sm5ntSampleID] === $Failed &&
  sm5NotebookProbeFile["/tmp/absolute.wxf"] === $Failed];
Export[sm5ntProbeScript, "(* modified fixture checker *)", "Text"];
sm5ntUnavailable = Block[{sm5NotebookProbes = {sm5ntProbeEntry}},
  sm5NotebookSupplementalProbeCells[First[sm5ntSampleCases]]];
sm5ntCheck["a changed supporting script produces only an unavailable note",
  Length[sm5ntUnavailable] === 1 && FreeQ[sm5ntUnavailable, _Dataset | Cell[_, "Input", ___]] &&
    !FreeQ[sm5ntUnavailable, text_String /; StringContainsQ[text, "unavailable"]]];
Export[sm5ntProbeScript, "(* synthetic captured public checker *)", "Text"];
DeleteFile[sm5ntProbeCapsule];
sm5ntCheck["a missing raw probe capsule never displays its formerly passing or failing table",
  sm5NotebookVerifiedProbe[sm5ntProbeEntry, sm5ntSampleID] === $Failed];
Export[sm5ntProbeCapsule, sm5ntProbeRecord, "WXF"];
sm5ntCheck["an unrelated case gains no supplemental cells",
  Block[{sm5NotebookProbes = {sm5ntProbeEntry}},
    sm5NotebookSupplementalProbeCells[<|"ID" -> "unrelated"|>]] === {}];
sm5ntProbeRegistryPath = FileNameJoin[{sm5ntTemporary, "supplemental-probes.json"}];
Export[sm5ntProbeRegistryPath, <|"SchemaVersion" -> 1, "Probes" -> {sm5ntProbeEntry}|>, "RawJSON"];
Export[sm5ntFixtureInventory, <|"Cases" -> sm5ntSampleCases, "Families" -> {sm5ntSampleFamily}|>, "RawJSON"];
Block[{$ScriptCommandLine = {sm5ntBuilder, "--inventory", sm5ntFixtureInventory,
    "--results", sm5ntTemporary, "--output", sm5ntFixtureNotebook, "--probes", sm5ntProbeRegistryPath}},
  Get[sm5ntBuilder]];
sm5ntProbeNotebook = sm5ntRead[sm5ntFixtureNotebook];
sm5ntCheck["supplemental failed output follows and preserves the original passed construction table",
  !FreeQ[sm5ntProbeNotebook, "original declared-input fixture"] &&
    !FreeQ[sm5ntProbeNotebook, "sm5-supplemental-probe-fixture-parameter-check"] &&
    !FreeQ[sm5ntProbeNotebook, "synthetic exact cut"] &&
    Count[sm5ntProbeNotebook, Cell[_, "Print", ___], Infinity] >= 1 &&
    Count[sm5ntProbeNotebook, Cell[_, "Output", ___], Infinity] >= 2];
sm5ntCheck["supplemental rendering does not add a constructor or numerical export",
  Length[Cases[ToExpression[#, InputForm, HoldComplete] & /@
    Cases[sm5ntProbeNotebook, Cell[text_String, "Input", ___] :> text, Infinity],
      HoldPattern[ConstructAmplitude[args___]] :> HoldComplete[args], Infinity]] === 1 &&
  Import[sm5ntProbeSource, "WXF"] === sm5ntProbeOriginal];

sm5ntProbeRebind[entry_, key_, path_] := Join[entry, <|"Files" -> Join[entry["Files"],
  <|key -> Join[entry["Files"][key], <|"SHA256" -> sm5NotebookDisplayHash[path]|>]|>]|>];
sm5ntBadSummary = Join[KeyDrop[sm5ntProbeRecord, {"Report", "ParameterRules"}],
  <|"ContextSHA256" -> "different-context"|>];
Export[sm5ntProbeSummary, sm5ntBadSummary, "RawJSON"];
sm5ntCheck["a rebound summary with a different context cannot supply probe evidence",
  sm5NotebookVerifiedProbe[sm5ntProbeRebind[sm5ntProbeEntry, "Summary", sm5ntProbeSummary],
    sm5ntSampleID] === $Failed];
Export[sm5ntProbeSummary, KeyDrop[sm5ntProbeRecord, {"Report", "ParameterRules"}], "RawJSON"];
sm5ntBadTableRecord = Join[sm5ntProbeRecord, <|"Report" -> <|
  "Checks" -> sm5ntProbeRecord["Report"]["Checks"],
  "Table" -> Dataset[{<|"Type" -> "Physical", "Status" -> "PASS"|>}]|>|>];
Export[sm5ntProbeCapsule, sm5ntBadTableRecord, "WXF"];
sm5ntCheck["a rebound table with different statuses from the actual checks is rejected",
  sm5NotebookVerifiedProbe[sm5ntProbeRebind[sm5ntProbeEntry, "Capsule", sm5ntProbeCapsule],
    sm5ntSampleID] === $Failed];
Export[sm5ntProbeCapsule, Join[sm5ntProbeRecord,
  <|"NoExpressionMassSlotOrFlavorTransport" -> False|>], "WXF"];
sm5ntCheck["a parameter probe cannot silently transport its captured source expression",
  sm5NotebookVerifiedProbe[sm5ntProbeRebind[sm5ntProbeEntry, "Capsule", sm5ntProbeCapsule],
    sm5ntSampleID] === $Failed];
Export[sm5ntProbeCapsule, sm5ntProbeRecord, "WXF"];
Export[sm5ntProbeSummary, Join[KeyDrop[sm5ntProbeRecord, {"Report", "ParameterRules"}],
  <|"SourceAmplitudeUnchanged" -> False|>], "RawJSON"];
sm5ntCheck["the summary must independently declare the unchanged source too",
  sm5NotebookVerifiedProbe[sm5ntProbeRebind[sm5ntProbeEntry, "Summary", sm5ntProbeSummary],
    sm5ntSampleID] === $Failed];
Export[sm5ntProbeSummary, KeyDrop[sm5ntProbeRecord, {"Report", "ParameterRules"}], "RawJSON"];
sm5ntFamilyScopeNote = Block[{sm5NotebookProbes = {sm5ntProbeEntry}},
  sm5NotebookSupplementalFamilyNote[sm5ntSampleFamily]];
sm5ntCheck["family export scope preserves prior comparisons while stating the extra failure",
  !FreeQ[sm5ntFamilyScopeNote, text_String /; StringContainsQ[text, "CalcHEP and MadGraph"] &&
    StringContainsQ[text, "recorded inputs"] && StringContainsQ[text, "not validated for mass changes"]]];
sm5ntProgressCounts = <|"Constructed" -> 185, "RecordedInputPassCases" -> 185,
  "FullyValidatedCases" -> 182, "PrecisionQualifiedCases" -> 2,
  "ValidatedIncludingPrecisionQualifiedCases" -> 184, "SupplementalParameterProbeFailedCases" -> 1|>;
sm5ntProgressCells = sm5NotebookProgressCells[sm5ntProgressCounts];
sm5ntCheck["recorded input successes and the current validated total are distinguished",
  !FreeQ[sm5ntProgressCells, text_String /; StringContainsQ[text, "Recorded constructions: 185"] &&
    StringContainsQ[text, "current validated total is 184"] &&
    StringContainsQ[text, "1 saved amplitude"] && StringContainsQ[text, "excluded"]]];
sm5ntCheck["an old or internally inconsistent progress file cannot claim a new validation total",
  sm5NotebookProgressCells[KeyDrop[sm5ntProgressCounts, "RecordedInputPassCases"]] === {} &&
  sm5NotebookProgressCells[Join[sm5ntProgressCounts,
    <|"ValidatedIncludingPrecisionQualifiedCases" -> 185|>]] === {}];
sm5ntCheck["a zero-failure progress summary adds no parameter-failure claim",
  FreeQ[sm5NotebookProgressCells[Join[sm5ntProgressCounts,
      <|"SupplementalParameterProbeFailedCases" -> 0|>]],
    text_String /; StringContainsQ[text, "parameter probe failed"]]];
DeleteDirectory[sm5ntTemporary, DeleteContents -> True];
Print["SM FIVE-POINT NOTEBOOK CHECKS: ", sm5ntChecks, "; FAILURES: ", Length[sm5ntFailures]];
If[sm5ntFailures === {}, Print["HEPCAT_TEST_COMPLETED:sm-five-point-notebook-tests.wl"], Print[sm5ntFailures]];
Quit[If[sm5ntFailures === {}, 0, 1]];
