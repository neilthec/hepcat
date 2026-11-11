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
sm5ntRead[path_] := DeleteCases[
  Quiet[Check[ToExpression[Import[path, "Text"], InputForm, HoldComplete], $Failed]], Null, {1}];
sm5ntPath = FileNameJoin[{sm5ntRoot, "tests", "SM-5-point-construction.nb"}];
sm5ntNotebook = sm5ntRead[sm5ntPath];
sm5ntInventory = Import[FileNameJoin[{sm5ntRoot, "tests", "SM-5-point", "inventory.json"}], "RawJSON"];
sm5ntInputs = Cases[sm5ntNotebook, Cell[text_String, "Input", ___] :> text, Infinity];
sm5ntParsed = ToExpression[#, InputForm, HoldComplete] & /@ sm5ntInputs;
sm5ntConstructors = Flatten[Cases[#, HoldPattern[ConstructAmplitude[p_, d_, f_, opts___]] :>
  HoldComplete[p, d, f, {opts}], Infinity] & /@ sm5ntParsed];
sm5ntExports = Flatten[Cases[#, HoldPattern[WriteSPINASProcess[args___]] :>
  HoldComplete[args], Infinity] & /@ sm5ntParsed];
sm5ntTags = Cases[sm5ntNotebook, HoldPattern[CellTags -> value_String] :> value, Infinity];
sm5ntFamilyTags = Select[sm5ntTags, StringStartsQ[#, "sm5-family-"] &];
sm5ntConstructionTags = Select[sm5ntTags, StringStartsQ[#, "sm5-construction-"] &];
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
    MatchQ[#, HoldComplete[_Symbol, sm, smFactorization, {"FeynmanModel" -> smFeynman}]] &]];
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
  FreeQ[sm5ntParsed, _nice | _CreateDiagrams | _PrepareAmplitudes | _CheckAmplitudes] &&
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
  !FreeQ[sm5ntReplayFailure, "synthetic solver failure"]];
Export[FileNameJoin[{sm5ntTemporary, sm5ntSampleID <> ".wxf"}],
  <|"Prints" -> {}, "Status" -> "TIMEOUT", "Seconds" -> 42,
    "Expression" -> Missing["NoAmplitude"]|>, "WXF"];
sm5ntBuild[];
sm5ntReplayTimeout = sm5ntRead[sm5ntFixtureNotebook];
sm5ntCheck["an interrupted run without detailed output displays its recorded status",
  !FreeQ[sm5ntReplayTimeout, "RecordedSMFivePointRun"] && !FreeQ[sm5ntReplayTimeout, "TIMEOUT"]];
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
sm5ntRevalidator = sm5ntRead[FileNameJoin[{sm5ntRoot, "tests", "SM-5-point", "revalidate.wl"}]];
sm5ntCheck["the retained-amplitude revalidator parses and never invokes construction",
  MatchQ[sm5ntRevalidator, _HoldComplete] && FreeQ[sm5ntRevalidator, _Syntax | _ConstructAmplitude]];
sm5ntCheck["revalidation retains native default mandatory checks",
  !FreeQ[sm5ntRevalidator, HoldPattern[CheckAmplitudes[expression, channels, factorizationModel,
      "FeynmanAmplitudes" -> benchmark, "FeynmanModel" -> feynmanModel,
      "Output" -> "Details", "RandomSeed" -> seed]]] &&
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
DeleteDirectory[sm5ntTemporary, DeleteContents -> True];
Print["SM FIVE-POINT NOTEBOOK CHECKS: ", sm5ntChecks, "; FAILURES: ", Length[sm5ntFailures]];
If[sm5ntFailures === {}, Print["HEPCAT_TEST_COMPLETED:sm-five-point-notebook-tests.wl"], Print[sm5ntFailures]];
Quit[If[sm5ntFailures === {}, 0, 1]];
