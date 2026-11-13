(* Rebuild the five-point worked notebook from the process inventory and actual
   captured runs. This script never evaluates a construction or validation.
   Run explicitly: WolframKernel -script tests/SM-5-point/build-notebook.wl
   Options: --inventory FILE --results DIRECTORY --output FILE --families IDS
   --max-notebook-bytes N splits large output into linked sector notebooks;
   --probes FILE selects the optional supplemental-check registry;
   --progress FILE selects an optional recorded-count summary. *)

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

$CharacterEncoding = "UTF-8";
sm5NotebookRoot = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
(* Native WolframKernel -script leaves $ScriptCommandLine empty on some
   installations. Its literal argument vector remains in $CommandLine. *)
sm5NotebookScriptArguments[sm5naScript_List, sm5naCommand_List] := Module[{sm5naPosition},
  If[sm5naScript =!= {}, Return[Rest[sm5naScript]]];
  sm5naPosition = FirstPosition[sm5naCommand, "-script", Missing["NotScriptInvocation"]];
  If[MissingQ[sm5naPosition], {}, Drop[sm5naCommand, First[sm5naPosition] + 1]]
];
sm5NotebookArguments = sm5NotebookScriptArguments[$ScriptCommandLine, $CommandLine];
If[OddQ[Length[sm5NotebookArguments]], Print["Expected option/value pairs."]; Quit[1]];
sm5NotebookOptions = Association[Rule @@@ Partition[sm5NotebookArguments, 2]];
If[Complement[Keys[sm5NotebookOptions], {"--inventory", "--results", "--output", "--families", "--probes", "--progress", "--max-notebook-bytes"}] =!= {},
  Print["Unknown notebook option."]; Quit[1]];
sm5NotebookInventoryPath = ExpandFileName[Lookup[sm5NotebookOptions, "--inventory",
  FileNameJoin[{sm5NotebookRoot, "tests", "SM-5-point", "inventory.json"}]]];
sm5NotebookResultsPath = ExpandFileName[Lookup[sm5NotebookOptions, "--results",
  FileNameJoin[{sm5NotebookRoot, "tests", "results", "SM-5-point"}]]];
sm5NotebookOutputPath = ExpandFileName[Lookup[sm5NotebookOptions, "--output",
  FileNameJoin[{sm5NotebookRoot, "tests", "SM-5-point-construction.nb"}]]];
sm5NotebookMaximumBytesText = Lookup[sm5NotebookOptions, "--max-notebook-bytes", "67108864"];
If[!StringQ[sm5NotebookMaximumBytesText] ||
    !StringMatchQ[sm5NotebookMaximumBytesText, DigitCharacter ..] ||
    FromDigits[sm5NotebookMaximumBytesText] < 1,
  Print["The notebook size limit must be a positive integer in bytes."]; Quit[1]];
sm5NotebookMaximumBytes = FromDigits[sm5NotebookMaximumBytesText];
If[!FileExistsQ[sm5NotebookInventoryPath],
  Print["Generate the SM five-point inventory first: ", sm5NotebookInventoryPath]; Quit[1]];
Get[FileNameJoin[{sm5NotebookRoot, "source", "HEPCAT.wl"}]];
sm5NotebookProgressPath = Lookup[sm5NotebookOptions, "--progress",
  If[sm5NotebookResultsPath === FileNameJoin[{sm5NotebookRoot, "tests", "results", "SM-5-point"}] &&
      sm5NotebookInventoryPath === FileNameJoin[{sm5NotebookRoot, "tests", "SM-5-point", "inventory.json"}],
    FileNameJoin[{sm5NotebookRoot, "tests", "SM-5-point", "progress.json"}], None]];
sm5NotebookProgress = If[StringQ[sm5NotebookProgressPath] && FileExistsQ[sm5NotebookProgressPath],
  Quiet[Check[Import[sm5NotebookProgressPath, "RawJSON"], <||>]], <||>];
sm5NotebookProbeRegistryPath = ExpandFileName[Lookup[sm5NotebookOptions, "--probes",
  FileNameJoin[{sm5NotebookRoot, "tests", "SM-5-point", "supplemental-probes.json"}]]];
sm5NotebookProbeRegistry = If[FileExistsQ[sm5NotebookProbeRegistryPath],
  Quiet[Check[Import[sm5NotebookProbeRegistryPath, "RawJSON"], <||>]], <||>];
sm5NotebookProbes = If[AssociationQ[sm5NotebookProbeRegistry] &&
    Lookup[sm5NotebookProbeRegistry, "SchemaVersion", None] === 1 &&
    ListQ[Lookup[sm5NotebookProbeRegistry, "Probes", None]],
  Select[sm5NotebookProbeRegistry["Probes"], AssociationQ], {}];
sm5NotebookInventory = Import[sm5NotebookInventoryPath, "RawJSON"];
sm5NotebookCases = Lookup[sm5NotebookInventory, "Cases", {}];
sm5NotebookFamilies = Lookup[sm5NotebookInventory, "Families", {}];
If[!VectorQ[sm5NotebookCases, AssociationQ] || !VectorQ[sm5NotebookFamilies, AssociationQ] ||
    sm5NotebookCases === {} || sm5NotebookFamilies === {},
  Print["Inventory Cases and Families must be nonempty lists."]; Quit[1]];
sm5NotebookCasesByID = AssociationThread[Lookup[sm5NotebookCases, "ID"], sm5NotebookCases];
sm5NotebookFamilyIDs = Lookup[sm5NotebookFamilies, "ID"];
sm5NotebookFamiliesByID = AssociationThread[sm5NotebookFamilyIDs, sm5NotebookFamilies];
If[!DuplicateFreeQ[Lookup[sm5NotebookCases, "ID"]] || !DuplicateFreeQ[sm5NotebookFamilyIDs],
  Print["Duplicate inventory IDs."]; Quit[1]];
sm5NotebookStarterSignatures = Sort /@ {
  {"h", "h", "h", "h", "h"}, {"h", "h", "h", "Z", "Z"},
  {"h", "h", "h", "W+", "W-"}, {"e", "E", "h", "h", "h"}};
sm5NotebookStarterFamilies = DeleteDuplicates[Lookup[Select[sm5NotebookCases,
  MemberQ[sm5NotebookStarterSignatures, Sort[#["Particles"]]] &], "FamilyID", {}]];
sm5NotebookRecordedAttemptQ[id_String, suffix_: ""] := Module[{base, summary},
  base = FileNameJoin[{sm5NotebookResultsPath, id <> suffix}];
  If[FileExistsQ[base <> ".wxf"], Return[True]];
  summary = If[FileExistsQ[base <> ".json"],
    Quiet[Check[Import[base <> ".json", "RawJSON"], <||>]], <||>];
  AssociationQ[summary] && Lookup[summary, If[suffix === "", "ID", "FamilyID"], None] === id &&
    StringQ[Lookup[summary, "Status", None]]
];
sm5NotebookRecordedFamilies = Select[sm5NotebookFamilies,
  sm5NotebookRecordedAttemptQ[#["ID"], "-export"] ||
    AnyTrue[Lookup[#, "CaseIDs", {}], sm5NotebookRecordedAttemptQ[#] &] &];
sm5NotebookSelectedIDs = If[KeyExistsQ[sm5NotebookOptions, "--families"],
  DeleteDuplicates[StringSplit[sm5NotebookOptions["--families"], ","]],
  DeleteDuplicates[Join[sm5NotebookStarterFamilies, Lookup[sm5NotebookRecordedFamilies, "ID", {}]]]];
If[Complement[sm5NotebookSelectedIDs, sm5NotebookFamilyIDs] =!= {},
  Print["Unknown family IDs: ", Complement[sm5NotebookSelectedIDs, sm5NotebookFamilyIDs]]; Quit[1]];
sm5NotebookSelectedFamilies = Select[sm5NotebookFamilies, MemberQ[sm5NotebookSelectedIDs, #["ID"]] &];
sm5NotebookSectors = DeleteDuplicates[Lookup[sm5NotebookCases, "Sector"]];
sm5NotebookSectorTitles = <|
  "PureHiggs" -> "Higgs amplitudes", "MassiveElectroweak" -> "Higgs and massive electroweak vectors",
  "ElectroweakPhoton" -> "Electroweak amplitudes with photons", "PureGluon" -> "Gluons",
  "TwoFermionHiggs" -> "Two fermions with Higgs bosons",
  "TwoFermionElectroweak" -> "Two fermions and electroweak bosons",
  "TwoFermionQCD" -> "Two fermions and QCD bosons",
  "FourFermionHiggs" -> "Four fermions with a Higgs boson",
  "FourFermionElectroweak" -> "Four fermions with a massive electroweak vector",
  "FourFermionPhoton" -> "Four fermions with a photon",
  "FourFermionGluon" -> "Four fermions with a gluon"|>;

sm5NotebookParticleToken[name_String] := StringReplace[name, {
  "A.+" -> "Aplus", "A.-" -> "Aminus", "G.+" -> "Gplus", "G.-" -> "Gminus",
  "+" -> "p", "-" -> "m", "." -> ""}];
sm5NotebookProcessName[particles_List] := StringJoin[sm5NotebookParticleToken /@ particles];
sm5NotebookVariable[id_String] := Module[{item},
  item = Lookup[sm5NotebookCasesByID, id, Lookup[sm5NotebookFamiliesByID, id, <||>]];
  "sm5" <> sm5NotebookProcessName[item["Particles"]]
];
sm5NotebookSource[expression_] := ToString[expression, InputForm];
(* Put can leave spaces at syntax line breaks. Scan quotes and backslashes
   explicitly so closing quotes cannot start overlapping string matches. *)
sm5NotebookTrimSyntaxWhitespace[text_String] := Module[
  {markers, strings, whitespace, outside, quoted = False, escapedThrough = 0,
   stringStart = 1, position, stringIndex = 1, starts, ends},
  markers = StringPosition[text, "\"" | "\\", Overlaps -> False];
  strings = Flatten[Last[Reap[
    Do[
      position = marker[[1]];
      If[position <= escapedThrough, Continue[]];
      If[StringTake[text, marker] === "\"",
        If[quoted, Sow[{stringStart, position}]; quoted = False,
          stringStart = position; quoted = True],
        If[quoted, escapedThrough = position + 1]],
      {marker, markers}];
    If[quoted, Sow[{stringStart, StringLength[text]}]]]], 1];
  whitespace = StringPosition[text, RegularExpression["(?m)[ \\t]+$"], Overlaps -> False];
  outside = Select[whitespace, Function[span,
    While[stringIndex <= Length[strings] && strings[[stringIndex, 2]] < span[[1]], stringIndex++];
    stringIndex > Length[strings] || span[[2]] < strings[[stringIndex, 1]]]];
  If[outside === {}, Return[text]];
  starts = Prepend[(Last /@ outside) + 1, 1];
  ends = Append[(First /@ outside) - 1, StringLength[text]];
  StringJoin[MapThread[If[#1 <= #2, StringTake[text, {#1, #2}], ""] &, {starts, ends}]]
];

sm5NotebookText[text_String] := Cell[text, "Text"];
sm5NotebookCount[n_Integer, singular_String, plural_String] := ToString[n] <> " " <> If[n === 1, singular, plural];
sm5NotebookInput[text_String, tag_String] := Cell[text, "Input", CellTags -> tag];
sm5NotebookGroup[cells_List] := Cell[CellGroupData[cells, Open]];
(* A display cache contains only data already consumed below. It is never
   validation evidence: the real capsule must exist and match its checksum.
   Bind formatting/projection changes to this whole builder's source as well. *)
sm5NotebookDisplayCacheSchema = 2;
sm5NotebookDisplaySourceSHA256 = IntegerString[FileHash[$InputFileName, "SHA256"], 16, 64];
sm5NotebookDisplayHash[path_String] := Quiet[Check[
  IntegerString[FileHash[path, "SHA256"], 16, 64], $Failed]];
sm5NotebookDisplayProjection[record_Association] := Module[{projection, details, result},
  If[sm5NotebookFailureDisplayQ[record],
    projection = KeyTake[record, {"ID", "FamilyID", "Status", "Seconds", "ElapsedSeconds", "FailureStage"}];
    projection = Join[projection, <|"FailurePreview" -> sm5NotebookFailurePreview[record],
      "Prints" -> sm5NotebookFailurePrints[record],
      "NotebookCells" -> sm5NotebookFailurePrintCells[record],
      "OmittedDiagnosticPrints" -> sm5NotebookOmittedDiagnostics[record]|>];
    details = Lookup[record, "Details", <||>];
    If[AssociationQ[details] && KeyExistsQ[details, "RequestedOptions"],
      AssociateTo[projection, "Details" -> KeyTake[details, {"RequestedOptions"}]]];
    Return[projection]];
  If[!MemberQ[{"COMPLETE", "PASS"}, Lookup[record, "Status", None]], Return[$Failed]];
  projection = KeyTake[record, {"ID", "Status", "Seconds", "Expression", "Prints", "NotebookCells",
    "RevalidationPrints", "RevalidationSeconds", "RevalidationCheckTimeConstraint"}];
  details = Lookup[record, "Details", <||>];
  If[AssociationQ[details] && KeyExistsQ[details, "RequestedOptions"],
    AssociateTo[projection, "Details" -> KeyTake[details, {"RequestedOptions"}]]];
  If[KeyExistsQ[record, "Result"],
    result = record["Result"];
    If[!AssociationQ[result] || !KeyExistsQ[result, "Directory"], Return[$Failed]];
    AssociateTo[projection, "Result" -> KeyTake[result, {"Directory"}]]];
  projection
];
sm5NotebookDisplayWrite[payload_String, manifest_String, projection_Association,
    expected_Association] := Module[{temporary, completed},
  temporary = (# <> "." <> CreateUUID[] <> ".tmp") & /@ {payload, manifest};
  completed = CheckAbort[Catch[Quiet[Check[
    If[!DirectoryQ[DirectoryName[payload]],
      CreateDirectory[DirectoryName[payload], CreateIntermediateDirectories -> True]];
    If[Export[temporary[[1]], projection, "WXF"] =!= temporary[[1]],
      Throw[$Failed, "DisplayCacheWrite"]];
    If[Export[temporary[[2]], Join[expected, <|"PayloadSHA256" ->
        sm5NotebookDisplayHash[temporary[[1]]]|>], "RawJSON"] =!= temporary[[2]],
      Throw[$Failed, "DisplayCacheWrite"]];
    RenameFile[temporary[[1]], payload, OverwriteTarget -> True];
    RenameFile[temporary[[2]], manifest, OverwriteTarget -> True]; True, $Failed]], "DisplayCacheWrite"],
    Scan[If[FileExistsQ[#], Quiet[DeleteFile[#]]] &, temporary]; Abort[]];
  Scan[If[FileExistsQ[#], Quiet[DeleteFile[#]]] &, temporary]; completed
];
sm5NotebookDisplayRecord[path_String] := Module[
  {capsuleHash, directory, payload, manifest, expected, metadata, cached, record, projection},
  If[!FileExistsQ[path], Return[<||>]];
  capsuleHash = sm5NotebookDisplayHash[path];
  directory = FileNameJoin[{DirectoryName[path], "notebook-display-cache"}];
  payload = FileNameJoin[{directory, FileBaseName[path] <> ".wxf"}];
  manifest = FileNameJoin[{directory, FileBaseName[path] <> ".json"}];
  expected = <|"SchemaVersion" -> sm5NotebookDisplayCacheSchema,
    "CapsuleSHA256" -> capsuleHash, "DisplaySourceSHA256" -> sm5NotebookDisplaySourceSHA256|>;
  metadata = If[FileExistsQ[manifest], Quiet[Check[Import[manifest, "RawJSON"], <||>]], <||>];
  If[StringQ[capsuleHash] && AssociationQ[metadata] &&
      KeyTake[metadata, Keys[expected]] === expected &&
      StringQ[Lookup[metadata, "PayloadSHA256", None]] &&
      sm5NotebookDisplayHash[payload] === metadata["PayloadSHA256"],
    cached = Quiet[Check[Import[payload, "WXF"], <||>]];
    If[AssociationQ[cached] && (MemberQ[{"COMPLETE", "PASS"}, Lookup[cached, "Status", None]] ||
          AssociationQ[Lookup[cached, "FailurePreview", None]]) &&
        sm5NotebookDisplayHash[payload] === metadata["PayloadSHA256"] &&
        sm5NotebookDisplayHash[path] === capsuleHash, Return[cached]]];
  record = Quiet[Check[Import[path, "WXF"], <||>]];
  If[AssociationQ[record] && StringQ[capsuleHash],
    projection = sm5NotebookDisplayProjection[record];
    If[AssociationQ[projection] && sm5NotebookDisplayHash[path] === capsuleHash,
      sm5NotebookDisplayWrite[payload, manifest, projection, expected];
      If[sm5NotebookFailureDisplayQ[record], Return[projection]]]];
  record
];
sm5NotebookRecord[path_String] := Module[{record, summaryPath, summary, stem, export, id},
  record = sm5NotebookDisplayRecord[path];
  If[AssociationQ[record] && record =!= <||>, Return[Join[record, <|"CanonicalCapsulePath" -> ExpandFileName[path]|>]]];
  summaryPath = FileNameJoin[{DirectoryName[path], FileBaseName[path] <> ".json"}];
  summary = If[FileExistsQ[summaryPath],
    Quiet[Check[Import[summaryPath, "RawJSON"], <||>]], <||>];
  stem = FileBaseName[path]; export = StringEndsQ[stem, "-export"];
  id = If[export, StringDrop[stem, -StringLength["-export"]], stem];
  If[!AssociationQ[summary] || Lookup[summary, If[export, "FamilyID", "ID"], None] =!= id ||
      !StringQ[Lookup[summary, "Status", None]], Return[<||>]];
  (* A status summary is evidence of an attempted run, never a saved amplitude,
     export directory, captured cell or validation table. *)
  Join[KeyTake[summary, {"ID", "FamilyID", "Status", "Seconds", "ElapsedSeconds", "RandomSeed",
      "ContextSHA256", "Log", "Message", "FailureStage"}],
    <|"SummaryOnly" -> True, "CanonicalCapsulePath" -> ExpandFileName[path],
      "Expression" -> Missing["NoSavedAmplitude"], "Result" -> Missing["NoSavedExport"], "Prints" -> {}|>]
];
sm5NotebookOutput[value_, style_String] := Cell[BoxData[ToBoxes[value, StandardForm]], style,
  GeneratedCell -> True, CellAutoOverwrite -> True];
(* Repeated headless heartbeats belong in the preserved raw log, rather than
   thousands of saved notebook cells. Milestones, diagnostics and check tables
   remain actual captured outputs. *)
sm5NotebookHeartbeatQ[value_] := (StringQ[value] || MatchQ[value, _Row | _SequenceForm]) &&
  StringContainsQ[If[StringQ[value], value, StringJoin[Cases[value, text_String :> text, Infinity]]],
    ": working; stage "];
sm5NotebookVisiblePrints[prints_List] := Select[prints, !sm5NotebookHeartbeatQ[#] &];
(* Keep failed diagnostic objects in their checksum-bound WXF, not in a
   Failure summary box whose hidden Interpretation can contain the whole fit. *)
sm5NotebookFailureDisplayQ[record_Association] := TrueQ[Lookup[record, "SummaryOnly", False]] ||
  With[{status = Lookup[record, "Status", None]}, StringQ[status] &&
    !MemberQ[{"COMPLETE", "PASS"}, status]] || FailureQ[Lookup[record, "Expression", None]] ||
  FailureQ[Lookup[record, "Result", None]];
sm5NotebookShortDiagnostic[text_String] := StringTake[text, UpTo[600]];
sm5NotebookShortDiagnostic[_] := "";
sm5NotebookFailurePreview[record_Association] := Module[
  {saved = Lookup[record, "FailurePreview", None], status, values, groups, next, failures,
   stage, reasons, diagnostics, reason, tag, index},
  If[AssociationQ[saved], Return[saved]];
  status = Lookup[record, "Status", "UNKNOWN"];
  values = Lookup[record, {"Details", "Expression", "Result"}, None]; groups = {record}; failures = {};
  Do[next = {};
    Do[If[FailureQ[value], AppendTo[failures, First[value]];
        If[AssociationQ[value[[2]]], AppendTo[groups, value[[2]]];
          next = Join[next, Lookup[value[[2]], {"Failure", "Report"}, None]]],
      If[AssociationQ[value], AppendTo[groups, value];
        next = Join[next, Lookup[value, {"Failure", "Report"}, None]]]], {value, values}];
    values = DeleteCases[next, None], {index, 3}];
  stage = SelectFirst[Flatten[Lookup[groups, {"FailureStage", "Stage"}, None]],
    StringQ[#] && # =!= "" &, "Unrecorded stage"];
  diagnostics = Flatten[Map[Function[g, With[{d = Lookup[g, "Diagnostics", {}]},
      If[StringQ[d], {d}, If[ListQ[d], Select[d, StringQ], {}]]]], groups], 1];
  reasons = Select[Join[Flatten[Lookup[groups, {"Message", "MessageTemplate"}, None]], diagnostics], StringQ];
  reason = If[TrueQ[Lookup[record, "SummaryOnly", False]],
    If[MemberQ[{"COMPLETE", "PASS"}, status],
      "The status summary has no readable saved capsule. No completed amplitude or export is displayed.",
      "The run ended without a saved amplitude or validation capsule."],
    If[reasons === {}, "No short diagnostic was recorded; inspect the saved report.", First[reasons]]];
  tag = If[failures === {}, "RecordedSMFivePointRun", First[failures]];
  <|"Status" -> status, "Stage" -> stage, "Reason" -> sm5NotebookShortDiagnostic[reason],
    "FailureTag" -> tag, "Seconds" -> Lookup[record, "Seconds", Lookup[record, "ElapsedSeconds", "unrecorded"]],
    "MissingCapsule" -> TrueQ[Lookup[record, "SummaryOnly", False]]|>
];
sm5NotebookSmallDiagnosticQ[value_] :=
  ByteCount[value] <= If[MatchQ[value, _Dataset], 2^20, 65536] && FreeQ[value, _Failure];
sm5NotebookFailurePrints[record_Association] := Select[
  sm5NotebookVisiblePrints[Lookup[record, "Prints", {}]], sm5NotebookSmallDiagnosticQ];
sm5NotebookFailurePrintCells[record_Association] := If[Lookup[record, "Prints", {}] =!= {}, {},
  Select[Lookup[record, "NotebookCells", {}], MatchQ[#, Cell[_, "Print", ___]] &&
    ByteCount[#] <= 2^20 && FreeQ[#, _Failure] &]];
sm5NotebookOmittedDiagnostics[record_Association] := Lookup[record, "OmittedDiagnosticPrints",
  Length[sm5NotebookVisiblePrints[Lookup[record, "Prints", {}]]] - Length[sm5NotebookFailurePrints[record]] +
    Count[Lookup[record, "NotebookCells", {}], Cell[_, "Output", ___]]];
sm5NotebookRecordedFailure[record_Association] := If[!sm5NotebookFailureDisplayQ[record],
  Missing["NoFailure"], sm5NotebookFailurePreview[record]];
sm5NotebookFailureCells[record_Association, kind_String] := Module[
  {preview = sm5NotebookFailurePreview[record], path, label, text, prints, import, id, omitted},
  path = Lookup[record, "CanonicalCapsulePath", None]; id = Lookup[record, "ID", Lookup[record, "FamilyID", "saved"]];
  label = If[TrueQ[preview["MissingCapsule"]] && MemberQ[{"COMPLETE", "PASS"}, preview["Status"]],
    "MissingSavedSMFivePointCapsule", "RecordedSMFivePointRun"];
  text = label <> ": " <> ToString[preview["Status"]] <> "; stage: " <> ToString[preview["Stage"]] <>
    "; time: " <> ToString[preview["Seconds"], InputForm] <> " s.\n" <>
    preview["Reason"] <> "\nDiagnostic: " <> ToString[preview["FailureTag"], InputForm] <>
    If[StringQ[path], "\nFull saved report: " <> path, ""];
  omitted = sm5NotebookOmittedDiagnostics[record];
  If[omitted > 0, text = text <> "\nDetailed diagnostic output is shown only in the saved report; its data have not been removed."];
  prints = Join[sm5NotebookOutput[#, "Print"] & /@ sm5NotebookFailurePrints[record],
    sm5NotebookFailurePrintCells[record]];
  import = If[StringQ[path], {Cell[
      "Optional full diagnostics: copy this disabled inspection command into a new input cell when needed. It is excluded from normal notebook evaluation.", "Text"],
    Cell["Import[" <> sm5NotebookPathSource[path] <> ", \"WXF\"]", "Input", Evaluatable -> False,
      CellTags -> "sm5-saved-report-" <> id]}, {}];
  Join[prints, {Cell[text, "Text", GeneratedCell -> True, CellAutoOverwrite -> True]}, import]
];
sm5NotebookWithRevalidation[cells_List, record_Association] := Module[
  {prints, additions, positions, index, seconds, checkLimit},
  prints = sm5NotebookVisiblePrints[Lookup[record, "RevalidationPrints", {}]];
  If[!ListQ[prints] || prints === {}, Return[cells]];
  seconds = Lookup[record, "RevalidationSeconds", Missing["Unknown"]];
  checkLimit = Lookup[record, "RevalidationCheckTimeConstraint", Automatic];
  additions = Join[{Cell[
    "Revalidation of the retained expression" <>
      If[NumberQ[seconds], ": " <> ToString[NumberForm[seconds, {8, 2}], OutputForm] <> " s", ""] <>
      ". The original construction output and timing remain above; the following checks use the unchanged saved amplitude with the current package." <>
      If[NumberQ[checkLimit], " These revalidation checks used an explicit " <>
        ToString[checkLimit, InputForm] <> " s checker limit.", ""],
    "Text", GeneratedCell -> True, CellAutoOverwrite -> True,
    CellTags -> "sm5-revalidation-" <> Lookup[record, "ID", "retained"]]},
    sm5NotebookOutput[#, "Print"] & /@ prints];
  positions = Position[cells, Cell[_, "Output", ___], {1}, 1];
  index = If[positions === {}, Length[cells] + 1, First[First[positions]]];
  Join[Take[cells, index - 1], additions, Drop[cells, index - 1]]
];
sm5NotebookCaptured[record_Association, kind_String] := Module[{cells, prints, output, failure},
  If[record === <||>, Return[{}]];
  If[sm5NotebookFailureDisplayQ[record], Return[sm5NotebookFailureCells[record, kind]]];
  cells = Lookup[record, "NotebookCells", Missing["NoCapturedCells"]];
  failure = sm5NotebookRecordedFailure[record];
  If[ListQ[cells] && AllTrue[cells, MatchQ[#, _Cell] &],
    Return[sm5NotebookWithRevalidation[
      If[!MissingQ[failure] && FreeQ[cells, Cell[_, "Output", ___]],
        Append[cells, sm5NotebookOutput[failure, "Output"]], cells], record]]];
  prints = sm5NotebookOutput[#, "Print"] & /@ sm5NotebookVisiblePrints[Lookup[record, "Prints", {}]];
  output = If[kind === "Construction", Lookup[record, "Expression", Missing["NoOutput"]],
    With[{result = Lookup[record, "Result", Missing["NoOutput"]]},
      If[AssociationQ[result] && KeyExistsQ[result, "Directory"], result["Directory"], result]]];
  If[MissingQ[output] && !MissingQ[failure], output = failure];
  sm5NotebookWithRevalidation[
    Join[prints, If[MissingQ[output], {}, {sm5NotebookOutput[output, "Output"]}]], record]
];
sm5NotebookCaptured[_, _] := {};
(* Reproduce only explicit public controls recorded for the construction.
   Resolved automatic effort and diagnostic seeds/output modes are not inputs. *)
sm5NotebookConstructionControls[record_] := Module[{details, requested, limit, checks, controls = {}},
  details = If[AssociationQ[record], Lookup[record, "Details", <||>], <||>];
  requested = If[AssociationQ[details], Lookup[details, "RequestedOptions", <||>], <||>];
  If[!AssociationQ[requested], Return[{}]];
  limit = Lookup[requested, "TimeConstraint", Automatic];
  If[limit === Infinity || NumericQ[limit] && TrueQ[0 < limit < Infinity],
    AppendTo[controls, "TimeConstraint" -> limit]];
  checks = Lookup[requested, "CheckOptions", {}];
  If[AssociationQ[checks], checks = Normal[checks]];
  If[!ListQ[checks] || !OptionQ[checks], checks = {}];
  (* The package's option bags use first-rule precedence. Preserve that when
     rendering one literal value for each checker key. *)
  checks = Normal[Association[Reverse[checks]]];
  checks = Select[checks, StringQ[First[#]] &&
    !MemberQ[{"RandomSeed", "Output"}, First[#]] && Last[#] =!= Automatic &];
  If[checks =!= {}, AppendTo[controls, "CheckOptions" -> checks]];
  controls
];
(* Supplemental checks are historical evidence about the same saved formula.
   They never change a construction result, export result, or progress count.
   The input is rendered as text; rebuilding never evaluates that input. *)
sm5NotebookProbeFile[relative_String] := Module[{parts},
  parts = StringSplit[StringReplace[relative, "\\" -> "/"], "/"];
  If[relative === "" || StringStartsQ[relative, "/" | "\\"] ||
      StringContainsQ[relative, ":"] || MemberQ[parts, ".." | "." | ""], Return[$Failed]];
  FileNameJoin[Prepend[parts, sm5NotebookResultsPath]]
];
sm5NotebookProbeFile[_] := $Failed;
sm5NotebookVerifiedProbe[entry_Association, id_String] := Module[
  {source, files, paths, record, summary, report, rows, table, tableRows, counts, valid},
  If[Lookup[entry, "CaseID", None] =!= id ||
      Lookup[entry, "Kind", None] =!= "ParameterCheck" ||
      !StringQ[Lookup[entry, "ID", None]] ||
      !StringMatchQ[entry["ID"], RegularExpression["[A-Za-z0-9][A-Za-z0-9_-]*"]] ||
      !And @@ (StringQ[Lookup[entry, #, None]] & /@ {"Title", "Note", "InputTemplate", "Status"}) ||
      !StringQ[Lookup[entry, "ProbeContextSHA256", None]] ||
      !NumberQ[Lookup[entry, "CheckTimeConstraint", None]] ||
      !TrueQ[entry["CheckTimeConstraint"] > 0], Return[$Failed]];
  source = FileNameJoin[{sm5NotebookResultsPath, id <> ".wxf"}];
  If[sm5NotebookDisplayHash[source] =!= Lookup[entry, "SourceCapsuleSHA256", None], Return[$Failed]];
  files = Lookup[entry, "Files", <||>];
  If[!AssociationQ[files] || !ContainsAll[Keys[files], {"Capsule", "Summary", "Script", "ConversionScript"}] ||
      !AllTrue[Values[files], AssociationQ[#] && StringQ[Lookup[#, "Path", None]] &&
        StringQ[Lookup[#, "SHA256", None]] &], Return[$Failed]];
  paths = Map[sm5NotebookProbeFile[#["Path"]] &, files];
  If[!AllTrue[Values[paths], StringQ] || !And @@ KeyValueMap[
      sm5NotebookDisplayHash[paths[#1]] === #2["SHA256"] &, files], Return[$Failed]];
  record = Quiet[Check[Import[paths["Capsule"], "WXF"], $Failed]];
  summary = Quiet[Check[Import[paths["Summary"], "RawJSON"], $Failed]];
  If[!AssociationQ[record] || !AssociationQ[summary] ||
      Lookup[record, "SourceCaseID", None] =!= id || Lookup[summary, "SourceCaseID", None] =!= id ||
      Lookup[record, "SourceCapsuleSHA256", None] =!= entry["SourceCapsuleSHA256"] ||
      Lookup[summary, "SourceCapsuleSHA256", None] =!= entry["SourceCapsuleSHA256"] ||
      Lookup[record, "Status", None] =!= entry["Status"] ||
      Lookup[summary, "Status", None] =!= entry["Status"] ||
      Lookup[record, "ContextSHA256", None] =!= Lookup[entry, "ProbeContextSHA256", None] ||
      Lookup[summary, "ContextSHA256", None] =!= Lookup[entry, "ProbeContextSHA256", None] ||
      !AllTrue[{record, summary}, TrueQ[Lookup[#, "SourceAmplitudeUnchanged", False]] &&
        TrueQ[Lookup[#, "NoExpressionMassSlotOrFlavorTransport", False]] &] ||
      !ListQ[Lookup[record, "ParameterRules", None]] ||
      !AllTrue[record["ParameterRules"], MatchQ[#, _Rule] &] ||
      !IntegerQ[Lookup[record, "RandomSeed", None]], Return[$Failed]];
  report = Lookup[record, "Report", <||>];
  If[!AssociationQ[report], Return[$Failed]];
  rows = Lookup[report, "Checks", {}]; table = Lookup[report, "Table", Missing["NoTable"]];
  If[!ListQ[rows] || rows === {} || !AllTrue[rows, AssociationQ] || !MatchQ[table, _Dataset], Return[$Failed]];
  tableRows = Quiet[Check[Normal[table], $Failed]];
  If[!ListQ[tableRows] || !AllTrue[tableRows, AssociationQ], Return[$Failed]];
  counts = Counts[Lookup[rows, "Status", {}]];
  If[!AllTrue[Lookup[{record, summary, entry}, "StatusCounts", None], AssociationQ] ||
      !AllTrue[Lookup[{record, summary, entry}, "StatusCounts"], KeySort[#] === KeySort[counts] &] ||
      Length[tableRows] =!= Length[rows] ||
      KeySort[Counts[Lookup[tableRows, "Status", {}]]] =!= KeySort[counts], Return[$Failed]];
  (* Close a changed-file race before publishing any captured table. *)
  valid = sm5NotebookDisplayHash[source] === entry["SourceCapsuleSHA256"] &&
    And @@ KeyValueMap[sm5NotebookDisplayHash[paths[#1]] === #2["SHA256"] &, files];
  If[!TrueQ[valid], Return[$Failed]];
  <|"Record" -> record, "Table" -> table|>
];
sm5NotebookSupplementalProbeCells[case_Association] := Flatten[Map[Function[entry,
  Module[{verified, variable, code, tag},
    tag = "sm5-supplemental-probe-" <> ToString[Lookup[entry, "ID", "unavailable"]];
    verified = sm5NotebookVerifiedProbe[entry, case["ID"]];
    If[!AssociationQ[verified], Return[{Cell[
      "Supplemental parameter check unavailable: its saved record or checksum binding could not be verified. " <>
        "No additional validation result is displayed.", "Text", CellTags -> tag <> "-unavailable"]}]];
    variable = sm5NotebookVariable[case["ID"]];
    code = StringReplace[entry["InputTemplate"], {
      "$CASE" -> variable,
      "$PARAMETER_RULES" -> sm5NotebookSource[verified["Record"]["ParameterRules"]],
      "$RANDOM_SEED" -> ToString[verified["Record"]["RandomSeed"], InputForm],
      "$CHECK_SECONDS" -> ToString[entry["CheckTimeConstraint"], InputForm]}];
    {Cell[entry["Title"], "Subsubsection"], sm5NotebookText[entry["Note"]],
      sm5NotebookGroup[{sm5NotebookInput[code, tag],
        sm5NotebookOutput[verified["Table"], "Output"]}]}
  ]], Select[sm5NotebookProbes, Lookup[#, "CaseID", None] === case["ID"] &]], 1];
sm5NotebookCalculation[case_Association] := Module[{id, variable, code, record, cells, controls},
  id = case["ID"]; variable = sm5NotebookVariable[id];
  record = sm5NotebookRecord[FileNameJoin[{sm5NotebookResultsPath, id <> ".wxf"}]];
  controls = sm5NotebookConstructionControls[record];
  code = variable <> "Particles = " <> sm5NotebookSource[case["Particles"]] <> ";\n" <>
    variable <> "Amplitude = ConstructAmplitude[" <> variable <>
    "Particles, sm, smFactorization,\n  \"FeynmanModel\" -> smFeynman" <>
    If[controls === {}, "", ",\n  " <> StringRiffle[sm5NotebookSource /@ controls, ",\n  "]] <> "]";
  cells = sm5NotebookCaptured[record, "Construction"];
  If[controls =!= {}, cells = Prepend[cells, Cell[
    "This saved run used explicit construction controls, retained in the input above.",
    "Text", GeneratedCell -> True, CellAutoOverwrite -> True,
    CellTags -> "sm5-construction-controls-" <> id]]];
  sm5NotebookGroup[Join[Prepend[cells, sm5NotebookInput[code, "sm5-construction-" <> id]],
    sm5NotebookSupplementalProbeCells[case]]]
];
sm5NotebookPathSource[path_String] := Module[{absolute, parts, rootParts},
  absolute = ExpandFileName[path]; parts = FileNameSplit[absolute]; rootParts = FileNameSplit[sm5NotebookRoot];
  If[Length[parts] >= Length[rootParts] && Take[parts, Length[rootParts]] === rootParts,
    "FileNameJoin[{hepcatDirectory, " <>
      StringRiffle[sm5NotebookSource /@ Drop[parts, Length[rootParts]], ", "] <> "}]",
    sm5NotebookSource[absolute]]
];
sm5NotebookVerifiedPrecisionFamilies = {};
sm5NotebookPrecisionRows[report_Association] := Table[
  Join[<|"Evaluation" -> name|>, KeyTake[report["Results"][name], {
    "MantissaBits", "Points", "Mismatches", "Errors", "MaximumRelativeError", "Status"}]],
  {name, {"Native", "GCC53", "GCC128"}}];
sm5NotebookPrecisionCells[family_Association] := Module[
  {id, link, verification, qualification, reportPath, report, pointsPath, pointsOption,
   variable, exportName, code, table, outputDirectorySource, note},
  id = family["ID"];
  link = FileNameJoin[{sm5NotebookResultsPath, id <> "-precision.json"}];
  If[!FileExistsQ[link], Return[{}]];
  verification = Quiet[Check[RunProcess[{
    "python3", FileNameJoin[{sm5NotebookRoot, "tests", "SM-5-point", "precision_evidence.py"}],
    "--family", id, "--results", sm5NotebookResultsPath,
    "--export-root", FileNameJoin[{sm5NotebookRoot, "tests", "results", "spinas-output", "SM-5-point"}],
    "--inventory", sm5NotebookInventoryPath}], $Failed]];
  qualification = If[AssociationQ[verification] && Lookup[verification, "ExitCode", 1] === 0,
    Quiet[Check[ImportString[verification["StandardOutput"], "RawJSON"], <||>]], <||>];
  If[!AssociationQ[qualification] || Lookup[qualification, "Status", None] =!= "PRECISION_QUALIFIED",
    Print["Precision replay binding is not current: ", id]; Return[{}]];
  reportPath = FileNameJoin[{sm5NotebookResultsPath, qualification["Report"]}];
  If[!FileExistsQ[reportPath] || IntegerString[FileHash[reportPath, "SHA256"], 16, 64] =!=
      Lookup[qualification, "ReportSHA256", None], Return[{}]];
  report = Import[reportPath, "RawJSON"];
  If[!AssociationQ[report] || !AssociationQ[Lookup[report, "Results", None]], Return[{}]];
  pointsPath = FileNameJoin[{DirectoryName[reportPath], "points.csv"}];
  pointsOption = If[FileExistsQ[pointsPath],
    "  \"--points-csv\", " <> sm5NotebookPathSource[pointsPath] <> ",\n",
    "  \"--points\", \"1000\",\n"];
  variable = sm5NotebookVariable[id]; exportName = family["ExportName"];
  outputDirectorySource = "FileNameJoin[{hepcatDirectory, \"tests\", \"results\", \"SM-5-point\", " <>
    "\"precision\", \"" <> exportName <> "-replay\",\n  " <>
    "DateString[{\"Year\", \"Month\", \"Day\", \"-\", \"Hour\", \"Minute\", \"Second\"}]}]";
  code = variable <> "PrecisionDirectory = " <> outputDirectorySource <> ";\n" <>
    variable <> "PrecisionRun = RunProcess[{\n  \"python3\",\n  " <>
    "FileNameJoin[{hepcatDirectory, \"tests\", \"SM-5-point\", \"replay-calchep-precision.py\"}],\n  " <>
    "FileNameJoin[{spinasOutputDirectory, \"" <> exportName <> "\"}],\n" <>
    pointsOption <> "  \"--seed\", \"" <> ToString[report["Seed"], InputForm] <> "\",\n  " <>
    "\"--output\", " <> variable <> "PrecisionDirectory}];\n" <>
    variable <> "PrecisionSummaryFile = FileNameJoin[{" <> variable <> "PrecisionDirectory, \"precision-summary.json\"}];\n" <>
    "If[FileExistsQ[" <> variable <> "PrecisionSummaryFile],\n  " <>
    variable <> "PrecisionReport = Import[" <> variable <> "PrecisionSummaryFile, \"RawJSON\"];\n  " <>
    "Dataset[Table[\n    Join[<|\"Evaluation\" -> name|>, KeyTake[" <> variable <>
    "PrecisionReport[\"Results\"][name],\n      {\"MantissaBits\", \"Points\", \"Mismatches\", \"Errors\", \"MaximumRelativeError\", \"Status\"}]],\n    " <>
    "{name, {\"Native\", \"GCC53\", \"GCC128\"}}]],\n  " <> variable <> "PrecisionRun]";
  note = "The default CalcHEP comparison above retains its raw residuals. This separate replay " <>
    "uses the same saved phase-space points and CalcHEP expressions. With " <>
    ToString[report["Results"]["GCC128"]["MantissaBits"]] <> " mantissa bits, all " <>
    ToString[report["Points"]] <> " points agree at the unchanged tolerance. This behavior is " <>
    "consistent with roundoff in cancellations at ordinary precision. The native and GCC53 controls " <>
    "remain visible below. Run this optional cell for a fresh replay; each run keeps its own output directory.";
  table = Dataset[sm5NotebookPrecisionRows[report], MaxItems -> {All, All}];
  AppendTo[sm5NotebookVerifiedPrecisionFamilies, id];
  {Cell["CalcHEP precision replay (optional)", "Subsubsection"], sm5NotebookText[note],
    sm5NotebookGroup[{sm5NotebookInput[code, "sm5-precision-replay-" <> id],
      sm5NotebookOutput[table, "Output"]}]}
];
(* MadGraph precision evidence is separate from the CalcHEP replay above. *)
sm5NotebookPrecisionDisplayNumber[value_String] := Quiet[Check[ImportString[value, "RawJSON"], value]];
sm5NotebookPrecisionDisplayNumber[value_] := value;
sm5NotebookMadGraphPrecisionRows[qualification_Association, raw_Association] := Module[
  {rows, zero, nonzero, maximum},
  If[Lookup[qualification, "Backend", None] =!= "MadGraph" ||
      Lookup[qualification, "Status", None] =!= "PRECISION_QUALIFIED" ||
      !ListQ[Lookup[raw, "Rows", None]], Return[{}]];
  rows = raw["Rows"];
  zero = Select[rows, TrueQ[Lookup[#, "ExpectedExactZero", False]] &];
  nonzero = Select[rows, !TrueQ[Lookup[#, "ExpectedExactZero", False]] &];
  maximum[data_, key_] := If[data === {}, 0, Max[Lookup[data, key, 0]]];
  {<|"Evaluation" -> "Original MadGraph", "MantissaBits" -> 53,
      "Points" -> qualification["Points"], "Helicity comparisons" -> qualification["ComparedHelicityPoints"],
      "Mismatches" -> Total[Lookup[rows, "Mismatches", 0]],
      "Errors" -> Total[Lookup[rows, "EvaluationErrors", 0]],
      "Max nonzero relative difference" -> maximum[nonzero, "MaximumRelativeError"],
      "Max zero absolute residual" -> maximum[zero, "MaximumAbsoluteError"], "Status" -> raw["Status"]|>,
   <|"Evaluation" -> "113-bit replay (separate qualification)", "MantissaBits" -> qualification["MantissaBits"],
      "Points" -> qualification["Points"], "Helicity comparisons" -> qualification["ComparedHelicityPoints"],
      "Mismatches" -> 0, "Errors" -> 0,
      "Max nonzero relative difference" -> sm5NotebookPrecisionDisplayNumber[qualification["MaximumNonzeroRelativeError"]],
      "Max zero absolute residual" -> sm5NotebookPrecisionDisplayNumber[qualification["MaximumZeroAbsoluteError"]],
      "Status" -> "PASS"|>}
];
sm5NotebookMadGraphReplayInput[variable_String, verifier_String] :=
  variable <> "MadGraphPrecisionReplay = RunProcess[{\"python3\", " <> sm5NotebookPathSource[verifier] <> "}];\n" <>
  "If[" <> variable <> "MadGraphPrecisionReplay[\"ExitCode\"] === 0,\n  ImportString[" <>
    variable <> "MadGraphPrecisionReplay[\"StandardOutput\"], \"RawJSON\"],\n  " <>
    variable <> "MadGraphPrecisionReplay]";
sm5NotebookMadGraphPrecisionCells[family_Association] := Module[
  {id, link, binding, verification, qualification, reportPath, report, rawPath, raw, verifier,
   rows, table, note, variable},
  id = family["ID"]; link = FileNameJoin[{sm5NotebookResultsPath, id <> "-precision.json"}];
  If[!FileExistsQ[link], Return[{}]];
  binding = Quiet[Check[Import[link, "RawJSON"], <||>]];
  If[!AssociationQ[binding] || Lookup[binding, "Backend", None] =!= "MadGraph", Return[{}]];
  verification = Quiet[Check[RunProcess[{
    "python3", FileNameJoin[{sm5NotebookRoot, "tests", "SM-5-point", "precision_evidence.py"}],
    "--family", id, "--results", sm5NotebookResultsPath,
    "--export-root", FileNameJoin[{sm5NotebookRoot, "tests", "results", "spinas-output", "SM-5-point"}],
    "--inventory", sm5NotebookInventoryPath}], $Failed]];
  qualification = If[AssociationQ[verification] && Lookup[verification, "ExitCode", 1] === 0,
    Quiet[Check[ImportString[verification["StandardOutput"], "RawJSON"], <||>]], <||>];
  If[Lookup[qualification, "Backend", None] =!= "MadGraph" ||
      Lookup[qualification, "Status", None] =!= "PRECISION_QUALIFIED", Return[{}]];
  reportPath = FileNameJoin[{sm5NotebookResultsPath, qualification["Report"]}];
  If[!FileExistsQ[reportPath] || sm5NotebookDisplayHash[reportPath] =!= qualification["ReportSHA256"], Return[{}]];
  report = Import[reportPath, "RawJSON"];
  rawPath = FileNameJoin[{DirectoryName[reportPath], report["RawReportFile"]}];
  verifier = FileNameJoin[{DirectoryName[reportPath], "verify-packet.py"}];
  If[sm5NotebookDisplayHash[rawPath] =!= Lookup[report["Files"], report["RawReportFile"], None] ||
      sm5NotebookDisplayHash[verifier] =!= Lookup[report["Files"], "verify-packet.py", None], Return[{}]];
  raw = Import[rawPath, "RawJSON"]; rows = sm5NotebookMadGraphPrecisionRows[qualification, raw];
  If[rows === {}, Return[{}]];
  variable = sm5NotebookVariable[id]; table = Dataset[rows, MaxItems -> {All, All}];
  AppendTo[sm5NotebookVerifiedPrecisionFamilies, id];
  note = "The original MadGraph comparison above keeps its raw FAIL and residuals. " <>
    "A separate verified replay uses the same uncorrected binary64 inputs, the same generated reference expressions, " <>
    "and 113-bit reference arithmetic. All " <> ToString[qualification["ComparedHelicityPoints"]] <>
    " point/helicity comparisons pass the unchanged tolerances. Saved failed-point momenta and native values were checked bit for bit. " <>
    "This does not claim that every native value from the historical run was saved. " <>
    "The optional cell below independently recomputes the archived comparisons; it does not rebuild MadGraph or generate new points. " <>
    "The packet's replay-instructions.txt explains the separate full rebuild procedure.";
  {Cell["MadGraph precision replay (optional)", "Subsubsection",
      CellTags -> "sm5-madgraph-precision-section-" <> id], sm5NotebookText[note],
    sm5NotebookOutput[table, "Output"],
    sm5NotebookGroup[{sm5NotebookInput[sm5NotebookMadGraphReplayInput[variable, verifier],
      "sm5-madgraph-precision-replay-" <> id]}]}
];
sm5NotebookSupplementalFamilyNote[family_Association] := Module[
  {entries, verified, failed, unavailable, note},
  entries = Select[sm5NotebookProbes, MemberQ[Lookup[family, "CaseIDs", {}], Lookup[#, "CaseID", None]] &];
  If[entries === {}, Return[{}]];
  verified = sm5NotebookVerifiedProbe[#, #["CaseID"]] & /@ entries;
  failed = AnyTrue[verified, AssociationQ[#] &&
    AnyTrue[Lookup[#["Record"]["Report"], "Checks", {}],
      MemberQ[{"FAIL", "Opp Sign"}, Lookup[#, "Status", None]] &] &];
  unavailable = AnyTrue[verified, !AssociationQ[#] &];
  note = "The previous CalcHEP and MadGraph tables below retain their results at the recorded inputs. ";
  note = note <> Which[failed,
    "The additional parameter failure above means this saved formula and its export are not validated for mass changes. " <>
      "Repeating the export at its original inputs does not resolve that failure.",
    unavailable,
    "Supplemental parameter evidence is unavailable; these original comparisons do not establish agreement after mass changes.",
    True, "The supplemental checks above cover their stated assignments, not arbitrary mass changes."];
  {Cell[note, "Text", CellTags -> "sm5-supplemental-export-note-" <> family["ID"]]}
];
sm5NotebookFamily[family_Association] := Module[
  {id, variable, exportName, cases, labels, cells, code, entries, record, physical},
  id = family["ID"]; variable = sm5NotebookVariable[id]; exportName = family["ExportName"];
  cases = Lookup[sm5NotebookCasesByID, family["CaseIDs"]];
  If[!VectorQ[cases, AssociationQ], Print["Unknown case in family ", id]; Quit[1]];
  labels = StringRiffle[family["Particles"], ", "];
  physical = StringRiffle[family["Incoming"], ", "] <> " -> " <>
    StringRiffle[family["Outgoing"], ", "];
  cells = {Cell[labels, "Subsection", CellTags -> "sm5-family-" <> id],
    sm5NotebookText["Physical export: " <> physical <>
      ". The construction lists below use all momenta ingoing."]};
  Do[
    If[Length[cases] > 1, AppendTo[cells,
      Cell[StringRiffle[case["Particles"], ", "], "Subsubsection"]]];
    AppendTo[cells, sm5NotebookCalculation[case]], {case, cases}];
  cells = Join[cells, sm5NotebookSupplementalFamilyNote[family]];
  AppendTo[cells, sm5NotebookText["After the amplitudes above complete, run this export. " <>
    "One C++ process contains the helicity family. It builds SPINAS and compares " <>
    "the squared amplitudes with CalcHEP and MadGraph at 1000 phase-space points each. " <>
    "The complete reports and random seeds remain in the Export variable."]];
  entries = StringRiffle[("    {" <> sm5NotebookVariable[#["ID"]] <> "Particles, " <>
      sm5NotebookVariable[#["ID"]] <> "Amplitude}") & /@ cases, ",\n"];
  code = variable <> "Export = WriteSPINASProcess[\n" <>
    "  FileNameJoin[{spinasOutputDirectory, \"" <> exportName <> "\"}],\n  " <>
    sm5NotebookSource[family["Incoming"]] <> " -> " <> sm5NotebookSource[family["Outgoing"]] <>
    ",\n  {\n" <> entries <> "\n  }, sm,\n  \"ProcessName\" -> \"" <>
    "sm5" <> exportName <> "\", Sequence @@ validationOptions];\n" <>
    "If[AssociationQ[" <> variable <> "Export], " <> variable <> "Export[\"Directory\"], " <>
    variable <> "Export]";
  record = sm5NotebookRecord[FileNameJoin[{sm5NotebookResultsPath, id <> "-export.wxf"}]];
  AppendTo[cells, sm5NotebookGroup[Prepend[sm5NotebookCaptured[record, "Export"],
    sm5NotebookInput[code, "sm5-export-" <> id]]]];
  cells = Join[cells, sm5NotebookPrecisionCells[family], sm5NotebookMadGraphPrecisionCells[family]];
  sm5NotebookGroup[cells]
];

sm5NotebookSetup = sm5NotebookGroup[{
  Cell["Setup", "Section"],
  sm5NotebookInput["hepcatDirectory = ExpandFileName[\"~/code/hepcat\"];\n" <>
    "Get[FileNameJoin[{hepcatDirectory, \"source\", \"HEPCAT.wl\"}]]", "sm5-setup-load"],
  sm5NotebookInput["sm = ReadModel[FileNameJoin[{hepcatDirectory, \"models\"}], 1];\n" <>
    "smFactorization = ReadModel[FileNameJoin[{hepcatDirectory, \"models\"}], 2];\n" <>
    "smFeynman = ReadModel[FileNameJoin[{hepcatDirectory, \"tests\", \"reference-models\", \"calchep\"}], 1];",
    "sm5-setup-models"],
  sm5NotebookText["Model 1 supplies elementary SM interactions for the diagrams. Model 2 " <>
    "supplies complete four-point amplitudes for the five-point factorization channels. " <>
    "The Feynman calculation uses the separate SM Feynman-rule model. Change the paths below " <>
    "to match your installation. Independent numerical comparisons match the parameters " <>
    "and turn off widths in their private benchmark copies."],
  sm5NotebookInput[StringRiffle[{
    "spinasDirectory = FileNameJoin[{ParentDirectory[hepcatDirectory], \"spinas\"}];",
    "spinasOutputDirectory = FileNameJoin[{hepcatDirectory, \"tests\", \"results\", \"spinas-output\", \"SM-5-point\"}];",
    "calcHEPDirectory = FileNameJoin[{ParentDirectory[hepcatDirectory], \"CalcHEP\", \"calchep_3.9.2\"}];",
    "calcHEPModel = {FileNameJoin[{hepcatDirectory, \"tests\", \"reference-models\", \"calchep\"}], 1};",
    "madGraphDirectory = FileNameJoin[{ParentDirectory[hepcatDirectory], \"MadGraph\"}];",
    "madGraphModel = FileNameJoin[{hepcatDirectory, \"tests\", \"reference-models\", \"madgraph\", \"sm\"}];",
    "madGraphParameterRules = {\"lamWS\" -> 0};",
    "validationPoints = 1000;",
    "validationOptions = {",
    "  \"SPINASDirectory\" -> spinasDirectory,",
    "  \"Build\" -> True, \"RunExamples\" -> True, \"Overwrite\" -> True,",
    "  \"CalcHEPDirectory\" -> calcHEPDirectory, \"CalcHEPModel\" -> calcHEPModel,",
    "  \"CalcHEPPoints\" -> validationPoints, \"RunCalcHEPValidation\" -> True,",
    "  \"MadGraphDirectory\" -> madGraphDirectory, \"MadGraphModel\" -> madGraphModel,",
    "  \"MadGraphParameterRules\" -> madGraphParameterRules,",
    "  \"MadGraphPoints\" -> validationPoints, \"RunMadGraphValidation\" -> True,",
    "  \"ReturnDetails\" -> True};"}, "\n"], "sm5-setup-validation"],
  sm5NotebookText["Both reference programs retain the full SM. The lamWS setting selects " <>
    "diagonal quark flavor mixing to match the constructive model. The massive-quark " <>
    "reference models include their Higgs couplings. No electron/photon-only restriction is applied."]
}];
sm5NotebookProgressCells[counts_Association] := Module[{required, text, failed, unavailable},
  required = {"Constructed", "RecordedInputPassCases", "FullyValidatedCases", "PrecisionQualifiedCases",
    "ValidatedIncludingPrecisionQualifiedCases", "SupplementalParameterProbeFailedCases"};
  If[!ContainsAll[Keys[counts], required] ||
      !AllTrue[Lookup[counts, required], IntegerQ[#] && # >= 0 &] ||
      counts["ValidatedIncludingPrecisionQualifiedCases"] =!=
        counts["FullyValidatedCases"] + counts["PrecisionQualifiedCases"] ||
      counts["RecordedInputPassCases"] < counts["ValidatedIncludingPrecisionQualifiedCases"], Return[{}]];
  text = "Recorded constructions: " <> ToString[counts["Constructed"]] <>
    ". Tests at their recorded inputs passed for " <> ToString[counts["RecordedInputPassCases"]] <>
    " amplitudes. The current validated total is " <>
    ToString[counts["ValidatedIncludingPrecisionQualifiedCases"]] <> ": " <>
    ToString[counts["FullyValidatedCases"]] <> " with ordinary comparisons and " <>
    ToString[counts["PrecisionQualifiedCases"]] <> " with separately qualified precision replays.";
  failed = counts["SupplementalParameterProbeFailedCases"];
  unavailable = Lookup[counts, "SupplementalParameterProbeUnavailableCases", 0];
  If[failed > 0, text = text <> " An additional parameter probe failed for " <>
    sm5NotebookCount[failed, "saved amplitude", "saved amplitudes"] <>
    ". The original results remain below, but known parameter failures are excluded from the current validated total."];
  If[IntegerQ[unavailable] && unavailable > 0, text = text <> " Supplemental evidence is unavailable for " <>
    sm5NotebookCount[unavailable, "saved amplitude", "saved amplitudes"] <> "."];
  {Cell[text, "Text", CellTags -> "sm5-progress-summary"]}
];
sm5NotebookProgressCells[_] := {};
(* The durable ledger is an index, not a replacement for deleted scratch evidence. *)
sm5NotebookSavedProgressCells[] := Module[
  {counts, native, families, qualified, precision, records, available, unavailable},
  unavailable = {Cell[
    "Saved validation caches are unavailable or incomplete. Run Setup, then the construction and export cells " <>
      "to rebuild them. No cached validation total is claimed.", "Text", CellTags -> "sm5-progress-unavailable"]};
  counts = Lookup[sm5NotebookProgress, "Counts", <||>];
  If[!AssociationQ[counts] || !KeyExistsQ[counts, "Constructed"], Return[{}]];
  native = Lookup[sm5NotebookProgress, "Cases", <||>];
  families = Lookup[sm5NotebookProgress, "Families", <||>];
  If[!AssociationQ[native] || !AssociationQ[families] || !DirectoryQ[sm5NotebookResultsPath], Return[unavailable]];
  native = Select[native, AssociationQ[#] && Lookup[#, "Status", None] === "COMPLETE" &];
  qualified = Select[families, AssociationQ[#] && (Lookup[#, "Status", None] === "PASS" ||
    Lookup[Lookup[#, "PrecisionQualification", <||>], "Status", None] === "PRECISION_QUALIFIED") &];
  precision = Keys[Select[qualified,
    Lookup[Lookup[#, "PrecisionQualification", <||>], "Status", None] === "PRECISION_QUALIFIED" &]];
  records = Join[KeyValueMap[{#1, #2, ""} &, native], KeyValueMap[{#1, #2, "-export"} &, qualified]];
  available = Length[native] === counts["Constructed"] &&
    ContainsAll[sm5NotebookVerifiedPrecisionFamilies, precision] && AllTrue[records,
      Function[item, StringMatchQ[item[[1]], RegularExpression["[A-Za-z][A-Za-z0-9_-]*"]] &&
        StringQ[Lookup[item[[2]], "CapsuleSHA256", None]] &&
        sm5NotebookDisplayHash[FileNameJoin[{sm5NotebookResultsPath, item[[1]] <> item[[3]] <> ".wxf"}]] ===
          item[[2]]["CapsuleSHA256"]]];
  If[TrueQ[available], sm5NotebookProgressCells[counts], unavailable]
];
(* Delay the overview until sector replay has verified any precision tables. *)
sm5NotebookOverview := sm5NotebookGroup[Join[{
  Cell["Sector overview", "Section"],
  sm5NotebookText["The full inventory contains " <> ToString[Length[sm5NotebookCases]] <>
    " construction cases in " <> ToString[Length[sm5NotebookFamilies]] <> " export families. " <>
    "This notebook develops the sectors in stages. The explicit calculations below cover " <>
    ToString[Length[sm5NotebookSelectedFamilies]] <> " families; the remaining families are listed " <>
    "in tests/SM-5-point/inventory.json. Validation results appear after each calculation or comparison runs."],
  sm5NotebookText["The cells retain output only from actual captured runs. Missing output " <>
    "means the cell has not been run here. Evaluate Setup, then an amplitude cell, then its " <>
    "family export to obtain fresh construction and numerical comparison tables."]},
  sm5NotebookSavedProgressCells[],
  Table[sm5NotebookText[Lookup[sm5NotebookSectorTitles, sector, sector] <> ": " <>
    sm5NotebookCount[Count[Lookup[sm5NotebookFamilies, "Sector"], sector], "export family", "export families"] <> ", " <>
    sm5NotebookCount[Count[Lookup[sm5NotebookCases, "Sector"], sector], "construction case", "construction cases"] <> "."],
    {sector, sm5NotebookSectors}]]];
sm5NotebookSectorData = Select[Table[<|"Sector" -> sector,
  "Families" -> Select[sm5NotebookSelectedFamilies, #["Sector"] === sector &]|>,
  {sector, sm5NotebookSectors}], #["Families"] =!= {} &];
sm5NotebookSectorGroup[data_Association] := sm5NotebookGroup[Join[
  {Cell[Lookup[sm5NotebookSectorTitles, data["Sector"], data["Sector"]], "Section",
    CellTags -> "sm5-sector-" <> data["Sector"]]}, sm5NotebookFamily /@ data["Families"]]];
sm5NotebookSectorGroups = sm5NotebookSectorGroup /@ sm5NotebookSectorData;
sm5NotebookIntroduction = {
  Cell["SM Five-Point Construction and Independent Validation", "Title"],
  sm5NotebookText["Construct the five-point SM amplitudes from elementary diagrams and " <>
    "complete lower-point factorization amplitudes. Each construction checks physical poles, " <>
    "spurious-pole cancellation, high-energy behavior, reference independence and independent " <>
    "Feynman diagrams. Each process family has its own SPINAS export with CalcHEP and MadGraph comparisons."],
  sm5NotebookText["All construction momenta are ingoing. The export states a physical " <>
    "two-to-three process and crosses the outgoing particles consistently. Photon and gluon " <>
    "helicities remain explicit; the numerical comparisons perform the required spin and color sums."]};
sm5NotebookMake[cells_List, extraRules_: <||>] := Notebook[{sm5NotebookGroup[cells]},
  WindowTitle -> "SM Five-Point Construction and Validation", StyleDefinitions -> "Default.nb",
  TaggingRules -> Join[<|"GeneratedBy" -> "tests/SM-5-point/build-notebook.wl",
    "Inventory" -> "tests/SM-5-point/inventory.json"|>, extraRules]];
sm5NotebookDocument = sm5NotebookMake[Join[sm5NotebookIntroduction,
  {sm5NotebookSetup, sm5NotebookOverview}, sm5NotebookSectorGroups]];
(* Publish the index last. Every linked sector is a complete, independently
   runnable notebook, retaining the captured cells exactly as in the single file. *)
sm5NotebookStage[document_Notebook, path_String] := Module[{temporary, result},
  If[!DirectoryQ[DirectoryName[path]],
    CreateDirectory[DirectoryName[path], CreateIntermediateDirectories -> True]];
  temporary = FileNameJoin[{DirectoryName[path], "." <> FileNameTake[path] <> "." <> CreateUUID[] <> ".tmp"}];
  result = CheckAbort[Check[
    Put[document, temporary];
    Export[temporary, sm5NotebookTrimSyntaxWhitespace[Import[temporary, "Text"]], "Text"];
    temporary,
    If[FileExistsQ[temporary], DeleteFile[temporary]]; $Failed],
    If[FileExistsQ[temporary], DeleteFile[temporary]]; Abort[]];
  result
];
sm5NotebookFinish[temporary_String, path_String] := Module[{result},
  result = Check[RenameFile[temporary, path, OverwriteTarget -> True]; True, $Failed];
  If[result === $Failed && FileExistsQ[temporary], Quiet[DeleteFile[temporary]]];
  result
];
sm5NotebookPublish[document_Notebook, path_String] := Module[{temporary = sm5NotebookStage[document, path]},
  If[temporary === $Failed, Return[$Failed]];
  sm5NotebookFinish[temporary, path]
];
sm5NotebookTemporaryPath = sm5NotebookStage[sm5NotebookDocument, sm5NotebookOutputPath];
If[sm5NotebookTemporaryPath === $Failed, Print["The notebook replacement did not complete."]; Quit[1]];
sm5NotebookSplitOutput = FileByteCount[sm5NotebookTemporaryPath] > sm5NotebookMaximumBytes;
sm5NotebookSectorFiles = {};
If[sm5NotebookSplitOutput,
  DeleteFile[sm5NotebookTemporaryPath];
  sm5NotebookSectorFiles = (FileBaseName[sm5NotebookOutputPath] <> "-" <> #["Sector"] <> ".nb" &) /@ sm5NotebookSectorData;
  Do[sm5NotebookSectorDocument = sm5NotebookMake[Join[
      {Cell[Lookup[sm5NotebookSectorTitles, sm5NotebookSectorData[[i]]["Sector"]], "Title"],
       sm5NotebookText["This sector contains the same ordinary construction and validation calls as the main catalog. " <>
         "Evaluate Setup before running a calculation. Saved outputs come only from the captured runs."], sm5NotebookSetup},
      {sm5NotebookSectorGroups[[i]]}], <|"Sector" -> sm5NotebookSectorData[[i]]["Sector"]|>];
    If[sm5NotebookPublish[sm5NotebookSectorDocument,
        FileNameJoin[{DirectoryName[sm5NotebookOutputPath], sm5NotebookSectorFiles[[i]]}]] =!= True,
      Print["A sector notebook replacement did not complete."]; Quit[1]],
    {i, Length[sm5NotebookSectorData]}];
  sm5NotebookSectorLinks = MapThread[Function[{data, filename},
    Cell[TextData[{ButtonBox[Lookup[sm5NotebookSectorTitles, data["Sector"], data["Sector"]],
      BaseStyle -> "Hyperlink", ButtonData -> {filename, None}], " (" <>
      ToString[Length[data["Families"]]] <> " recorded families)"}], "Text"]],
    {sm5NotebookSectorData, sm5NotebookSectorFiles}];
  sm5NotebookDocument = sm5NotebookMake[Join[sm5NotebookIntroduction,
    {sm5NotebookSetup, sm5NotebookOverview,
     sm5NotebookGroup[Join[{Cell["Sector notebooks", "Section"],
       sm5NotebookText["The complete calculations and captured outputs are in the linked notebooks below. " <>
         "Each has its own Setup and can be run independently. Keep these files together when moving the catalog."]},
       sm5NotebookSectorLinks]]}], <|"SectorNotebooks" -> sm5NotebookSectorFiles|>];
  sm5NotebookWriteResult = sm5NotebookPublish[sm5NotebookDocument, sm5NotebookOutputPath],
  sm5NotebookWriteResult = sm5NotebookFinish[sm5NotebookTemporaryPath, sm5NotebookOutputPath]
];
If[!TrueQ[sm5NotebookWriteResult], Print["The notebook replacement did not complete."]; Quit[1]];
Print["Wrote ", sm5NotebookOutputPath, " with ", Length[sm5NotebookSelectedFamilies], " families",
  If[sm5NotebookSplitOutput, " in " <> ToString[Length[sm5NotebookSectorFiles]] <> " linked sector notebooks.", "."]];
