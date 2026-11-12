(* Rebuild the five-point worked notebook from the process inventory and actual
   captured runs. This script never evaluates a construction or validation.
   Run explicitly: WolframKernel -script tests/SM-5-point/build-notebook.wl
   Options: --inventory FILE --results DIRECTORY --output FILE --families IDS. *)

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
sm5NotebookArguments = If[$ScriptCommandLine =!= {}, Rest[$ScriptCommandLine], {}];
If[OddQ[Length[sm5NotebookArguments]], Print["Expected option/value pairs."]; Quit[1]];
sm5NotebookOptions = Association[Rule @@@ Partition[sm5NotebookArguments, 2]];
If[Complement[Keys[sm5NotebookOptions], {"--inventory", "--results", "--output", "--families"}] =!= {},
  Print["Unknown notebook option."]; Quit[1]];
sm5NotebookInventoryPath = ExpandFileName[Lookup[sm5NotebookOptions, "--inventory",
  FileNameJoin[{sm5NotebookRoot, "tests", "SM-5-point", "inventory.json"}]]];
sm5NotebookResultsPath = ExpandFileName[Lookup[sm5NotebookOptions, "--results",
  FileNameJoin[{sm5NotebookRoot, "tests", "results", "SM-5-point"}]]];
sm5NotebookOutputPath = ExpandFileName[Lookup[sm5NotebookOptions, "--output",
  FileNameJoin[{sm5NotebookRoot, "tests", "SM-5-point-construction.nb"}]]];
If[!FileExistsQ[sm5NotebookInventoryPath],
  Print["Generate the SM five-point inventory first: ", sm5NotebookInventoryPath]; Quit[1]];
Get[FileNameJoin[{sm5NotebookRoot, "source", "HEPCAT.wl"}]];
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
sm5NotebookDisplayCacheSchema = 1;
sm5NotebookDisplaySourceSHA256 = IntegerString[FileHash[$InputFileName, "SHA256"], 16, 64];
sm5NotebookDisplayHash[path_String] := Quiet[Check[
  IntegerString[FileHash[path, "SHA256"], 16, 64], $Failed]];
sm5NotebookDisplayProjection[record_Association] := Module[{projection, details, result},
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
    If[AssociationQ[cached] && MemberQ[{"COMPLETE", "PASS"}, Lookup[cached, "Status", None]] &&
        sm5NotebookDisplayHash[payload] === metadata["PayloadSHA256"] &&
        sm5NotebookDisplayHash[path] === capsuleHash, Return[cached]]];
  record = Quiet[Check[Import[path, "WXF"], <||>]];
  If[AssociationQ[record] && StringQ[capsuleHash],
    projection = sm5NotebookDisplayProjection[record];
    If[AssociationQ[projection] && sm5NotebookDisplayHash[path] === capsuleHash,
      sm5NotebookDisplayWrite[payload, manifest, projection, expected]]];
  record
];
sm5NotebookRecord[path_String] := Module[{record, summaryPath, summary, stem, export, id},
  record = sm5NotebookDisplayRecord[path];
  If[AssociationQ[record] && record =!= <||>, Return[record]];
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
    <|"SummaryOnly" -> True, "Expression" -> Missing["NoSavedAmplitude"],
      "Result" -> Missing["NoSavedExport"], "Prints" -> {}|>]
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
sm5NotebookRecordedFailure[record_Association] := Module[{failure, status},
  status = Lookup[record, "Status", "UNKNOWN"];
  If[TrueQ[Lookup[record, "SummaryOnly", False]],
    Return[Failure[If[MemberQ[{"COMPLETE", "PASS"}, status],
      "MissingSavedSMFivePointCapsule", "RecordedSMFivePointRun"], <|
      "MessageTemplate" -> If[MemberQ[{"COMPLETE", "PASS"}, status],
        "The saved summary reports `` after `` seconds, but its amplitude or export capsule is missing or unreadable. No completed result is displayed.",
        "The saved run ended with status `` after `` seconds. No amplitude or validation capsule was saved."],
      "MessageParameters" -> {status, Lookup[record, "Seconds", Lookup[record, "ElapsedSeconds", "unrecorded"]]},
      "RecordedSummary" -> KeyDrop[record, {"Expression", "Result", "Prints", "SummaryOnly"}]|>]]];
  If[!MemberQ[{"FAIL", "TIMEOUT", "ERROR", "INTERRUPTED"}, status], Return[Missing["NoFailure"]]];
  failure = FirstCase[record, _Failure, Missing["NoFailureDetails"], Infinity];
  If[!MissingQ[failure], Return[failure]];
  Failure["RecordedSMFivePointRun", <|
    "MessageTemplate" -> "The saved run ended with status `` after `` seconds and did not return a completed result.",
    "MessageParameters" -> {status, Lookup[record, "Seconds", "unknown"]},
    "Status" -> status, "Seconds" -> Lookup[record, "Seconds", Missing["Unknown"]]|>]
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
  sm5NotebookGroup[Prepend[cells, sm5NotebookInput[code, "sm5-construction-" <> id]]]
];
sm5NotebookPathSource[path_String] := Module[{absolute, parts, rootParts},
  absolute = ExpandFileName[path]; parts = FileNameSplit[absolute]; rootParts = FileNameSplit[sm5NotebookRoot];
  If[Length[parts] >= Length[rootParts] && Take[parts, Length[rootParts]] === rootParts,
    "FileNameJoin[{hepcatDirectory, " <>
      StringRiffle[sm5NotebookSource /@ Drop[parts, Length[rootParts]], ", "] <> "}]",
    sm5NotebookSource[absolute]]
];
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
  {Cell["CalcHEP precision replay (optional)", "Subsubsection"], sm5NotebookText[note],
    sm5NotebookGroup[{sm5NotebookInput[code, "sm5-precision-replay-" <> id],
      sm5NotebookOutput[table, "Output"]}]}
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
  cells = Join[cells, sm5NotebookPrecisionCells[family]];
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
sm5NotebookOverview = sm5NotebookGroup[Join[{
  Cell["Sector overview", "Section"],
  sm5NotebookText["The full inventory contains " <> ToString[Length[sm5NotebookCases]] <>
    " construction cases in " <> ToString[Length[sm5NotebookFamilies]] <> " export families. " <>
    "This notebook develops the sectors in stages. The explicit calculations below cover " <>
    ToString[Length[sm5NotebookSelectedFamilies]] <> " families; the remaining families are listed " <>
    "in tests/SM-5-point/inventory.json. Validation results appear after each calculation or comparison runs."],
  sm5NotebookText["The cells retain output only from actual captured runs. Missing output " <>
    "means the cell has not been run here. Evaluate Setup, then an amplitude cell, then its " <>
    "family export to obtain fresh construction and numerical comparison tables."]},
  Table[sm5NotebookText[Lookup[sm5NotebookSectorTitles, sector, sector] <> ": " <>
    sm5NotebookCount[Count[Lookup[sm5NotebookFamilies, "Sector"], sector], "export family", "export families"] <> ", " <>
    sm5NotebookCount[Count[Lookup[sm5NotebookCases, "Sector"], sector], "construction case", "construction cases"] <> "."],
    {sector, sm5NotebookSectors}]]];
sm5NotebookSectorGroups = Table[With[
  {families = Select[sm5NotebookSelectedFamilies, #["Sector"] === sector &]},
  If[families === {}, Nothing, sm5NotebookGroup[Join[
    {Cell[Lookup[sm5NotebookSectorTitles, sector, sector], "Section", CellTags -> "sm5-sector-" <> sector]},
    sm5NotebookFamily /@ families]]]], {sector, sm5NotebookSectors}];
sm5NotebookDocument = Notebook[{sm5NotebookGroup[Join[{
  Cell["SM Five-Point Construction and Independent Validation", "Title"],
  sm5NotebookText["Construct the five-point SM amplitudes from elementary diagrams and " <>
    "complete lower-point factorization amplitudes. Each construction checks physical poles, " <>
    "spurious-pole cancellation, high-energy behavior, reference independence and independent " <>
    "Feynman diagrams. Each process family has its own SPINAS export with CalcHEP and MadGraph comparisons."],
  sm5NotebookText["All construction momenta are ingoing. The export states a physical " <>
    "two-to-three process and crosses the outgoing particles consistently. Photon and gluon " <>
    "helicities remain explicit; the numerical comparisons perform the required spin and color sums."],
  sm5NotebookSetup, sm5NotebookOverview}, sm5NotebookSectorGroups]]},
  WindowTitle -> "SM Five-Point Construction and Validation", StyleDefinitions -> "Default.nb",
  TaggingRules -> <|"GeneratedBy" -> "tests/SM-5-point/build-notebook.wl",
    "Inventory" -> "tests/SM-5-point/inventory.json"|>];
If[!DirectoryQ[DirectoryName[sm5NotebookOutputPath]],
  CreateDirectory[DirectoryName[sm5NotebookOutputPath], CreateIntermediateDirectories -> True]];
sm5NotebookTemporaryPath = FileNameJoin[{DirectoryName[sm5NotebookOutputPath],
  "." <> FileNameTake[sm5NotebookOutputPath] <> "." <> CreateUUID[] <> ".tmp"}];
sm5NotebookWriteResult = CheckAbort[
  Check[
    Put[sm5NotebookDocument, sm5NotebookTemporaryPath];
    Export[sm5NotebookTemporaryPath,
      sm5NotebookTrimSyntaxWhitespace[Import[sm5NotebookTemporaryPath, "Text"]], "Text"];
    RenameFile[sm5NotebookTemporaryPath, sm5NotebookOutputPath, OverwriteTarget -> True]; True,
    If[FileExistsQ[sm5NotebookTemporaryPath], DeleteFile[sm5NotebookTemporaryPath]]; $Failed],
  If[FileExistsQ[sm5NotebookTemporaryPath], DeleteFile[sm5NotebookTemporaryPath]]; Abort[]];
If[!TrueQ[sm5NotebookWriteResult], Print["The notebook replacement did not complete."]; Quit[1]];
Print["Wrote ", sm5NotebookOutputPath, " with ", Length[sm5NotebookSelectedFamilies], " families."];
