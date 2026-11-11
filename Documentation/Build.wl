(* ::Package:: *)

(* Build deployed Mathematica Help notebooks from the reviewed source catalogs.
   Run Get["/absolute/path/to/hepcat/Documentation/Build.wl"] in a licensed
   kernel. Example cells are tokenized as literal source and never evaluated. *)

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


Global`$HEPCATpath = FileNameJoin[{ParentDirectory[DirectoryName[$InputFileName]], "source"}];
Get[FileNameJoin[{Global`$HEPCATpath, "HEPCAT.wl"}]];
Needs["CodeParser`"];
Begin["HEPCATDocumentationBuild`Private`"];

$BuildResult = Module[{documentationRoot, repositoryRoot, sourceRoot, outputRoot, catalogFiles,
    catalog, allCatalog, byName, guides, allGuides, tutorials, optionDescriptions, exampleResults, guideTitles, tutorialTitles, readJSON, inputBoxes, outputBoxes, normalizeText,
    richText, plainText, publicQ, hiddenNames, staleFiles, detailCells,
    inlineCode, link, linksCell, metadata, anchor, setupCode, setupCells,
    exampleCells, symbolPage, guidePage, tutorialPage, optionsCells,
    notebook, savePage, usage, indexResult, counts, optionDescription},
  documentationRoot = DirectoryName[$InputFileName];
  repositoryRoot = ParentDirectory[documentationRoot];
  sourceRoot = FileNameJoin[{documentationRoot, "Source"}];
  outputRoot = FileNameJoin[{documentationRoot, "English"}];

  readJSON[name_, default_: {}] := With[{file = FileNameJoin[{sourceRoot, name}]},
    If[FileExistsQ[file], Import[file, "RawJSON"], default]];
  catalogFiles = {"base.json", "model.json", "feynman-export.json"};
  allCatalog = Flatten[readJSON /@ catalogFiles, 1];
  publicQ[entry_Association] := Lookup[entry, "visibility", "public"] === "public";
  catalog = Select[allCatalog, publicQ];
  If[catalog === {} || !VectorQ[catalog, AssociationQ],
    Return[Failure["MissingCatalog", <|"MessageTemplate" -> "HEPCAT documentation catalogs are missing."|>]]];
  If[!DuplicateFreeQ[Lookup[catalog, "name"]],
    Return[Failure["DuplicateReferenceNames", <|"MessageTemplate" -> "Reference-page names must be unique."|>]]];
  byName = AssociationThread[Lookup[catalog, "name"], catalog];
  allGuides = readJSON["guides.json"];
  guides = Select[allGuides, publicQ];
  tutorials = readJSON["tutorials.json"];
  optionDescriptions = readJSON["option-descriptions.json", <||>];
  exampleResults = readJSON["example-results.json", <||>];

  guideTitles = AssociationThread[Lookup[guides, "name", {}], Lookup[guides, "title", {}]];
  tutorialTitles = AssociationThread[Lookup[tutorials, "name", {}], Lookup[tutorials, "title", {}]];
  (* Tokenization preserves every source character without interpreting or
     formatting a symbolic expression. In particular, no example is evaluated. *)
  inputBoxes[code_String] := Module[{tokens = CodeParser`CodeTokenize[code]},
    If[FailureQ[tokens], StyleBox[code, FontFamily -> "Source Code Pro"],
      RowBox[(#[[2]] &) /@ tokens]]
  ];
  (* Cached outputs have already been evaluated and verified separately. Only
     their presentation uses StandardForm; raw input source remains untouched. *)
  outputBoxes[code_String] := Module[{boxes},
    If[StringContainsQ[code, "HoldPattern[" | "RuleDelayed[" | ":>" | "Function["] ||
        !StringContainsQ[code, "SpinorChain[" | "Spinor[" | "ModelParameter[" |
          "ModelFunction[" | "ConstructiveAmplitude[" | "PropDen[" | "MomProd[" |
          "Mandelstahm[" | "Mom[" | "Mass[" | "ColorIndex[" | "ColorDelta[" |
          "ColorGenerator[" | "ColorStructure[" | "ColorTrace["], Return[inputBoxes[code]]];
    boxes = CheckAbort[TimeConstrained[
      Quiet[Check[ToExpression["(" <> code <> "\n)", InputForm,
        Function[Null, ToBoxes[Defer[#], StandardForm], HoldAllComplete]], $Failed]],
      1, $TimedOut], $Aborted];
    If[MemberQ[{$Failed, $Aborted, $TimedOut}, boxes] ||
        !FreeQ[boxes, _ToBoxes | _MakeBoxes], inputBoxes[code], boxes]
  ];
  normalizeText[path_String] := Export[path,
    StringReplace[Import[path, "Text"], RegularExpression["(?m)[ \t]+$"] -> ""], "Text"];
  inlineCode[code_String] := Cell[BoxData[inputBoxes[code]], "InlineFormula"];
  link[name_String, kind_: "ref", label_: Automatic] := ButtonBox[
    Replace[label, Automatic :> Switch[kind,
      "guide", Lookup[guideTitles, name, name],
      "tutorial", Lookup[tutorialTitles, name, name], _, name]], BaseStyle -> "Link",
    ButtonData -> "paclet:HEPCAT/" <> kind <> "/" <> name];
  linksCell[names_List, style_, kind_: "ref"] := Cell[
    TextData[Riffle[link[#, kind] & /@ names, "  •  "]], style];
  (* Native Help search displays label separately from type; a missing label
     makes its result-cell renderer reject an otherwise valid search hit. *)
  plainText[text_String] := StringReplace[text, {
    RegularExpression["\\[([^\\]]+)\\]\\([^)]*\\)"] -> "$1", "`" -> ""}];
  richText[text_String] := Module[{positions, pieces = {}, start = 1, token, parts, uri},
    positions = StringPosition[text, RegularExpression[
      "\\[[^\\]]+\\]\\((?:paclet:[^\\s)]+|https?://[^\\s)]+)\\)|`[^`\\n]+`"]];
    Do[
      If[position[[1]] > start, AppendTo[pieces, StringTake[text, {start, position[[1]] - 1}]]];
      token = StringTake[text, position];
      If[StringStartsQ[token, "`"],
        AppendTo[pieces, inlineCode[StringTake[token, {2, -2}]]],
        parts = First[StringCases[token,
          RegularExpression["^\\[([^\\]]+)\\]\\(([^)]+)\\)$"] -> {"$1", "$2"}]];
        uri = parts[[2]];
        If[StringStartsQ[uri, "paclet:HEPCAT/ref/"] &&
            !KeyExistsQ[byName, StringDrop[uri, StringLength["paclet:HEPCAT/ref/"]]],
          AppendTo[pieces, inlineCode[parts[[1]]]],
          AppendTo[pieces, ButtonBox[parts[[1]], BaseStyle -> "Link", ButtonData -> uri]]]
      ];
      start = position[[2]] + 1,
      {position, positions}];
    If[start <= StringLength[text], AppendTo[pieces, StringDrop[text, start - 1]]];
    TextData[pieces]
  ];
  detailCells[sections_List] := Flatten[Map[Function[section,
    Join[{Cell[section["title"], "NotesSection"]},
      Cell[richText[#], "Notes"] & /@ Lookup[section, "paragraphs", {}]]], sections], 1];
  metadata[name_, title_, type_, summary_, kind_] := {
    "Metadata" -> {"context" -> "ConstructiveDiagrams`", "uri" -> "HEPCAT/" <> kind <> "/" <> name,
      "title" -> title, "type" -> type, "label" -> "HEPCAT " <> type,
      "paclet" -> "HEPCAT", "language" -> "en",
      "summary" -> plainText[summary], "keywords" -> {"HEPCAT", name, "spinor amplitudes"},
      "status" -> "None", "index" -> True}};
  anchor[label_, guide_: "HEPCAT"] := Cell[BoxData[GridBox[{{
    Cell["HEPCAT " <> label, "PacletNameCell"],
    Cell[TextData[{link["HEPCAT", "guide", "HEPCAT"], "  •  ",
      link[guide, "guide", "Related Guide"]}], "AnchorBar"]}},
      GridBoxAlignment -> {"Columns" -> {Left, Right}},
      GridBoxItemSize -> {"Columns" -> {Scaled[.42], Scaled[.58]}}]], "AnchorBarGrid"];
  setupCode = "Get[\"~/code/hepcat/source/HEPCAT.wl\"];";
  setupCells[examples_List] := Module[{inputs = StringRiffle[Lookup[examples, "input", {}], "\n"], lines},
    lines = {setupCode};
    If[StringContainsQ[inputs, "hepcatDirectory"], AppendTo[lines, "hepcatDirectory = \"~/code/hepcat\";"]];
    If[StringContainsQ[inputs, "modelDirectory"], AppendTo[lines, "modelDirectory = \"~/code/hepcat/models\";"]];
    If[StringContainsQ[inputs, "feynmanModelDirectory"], AppendTo[lines, "feynmanModelDirectory = \"~/code/hepcat/tests/reference-models/calchep\";"]];
    Cell[CellGroupData[{
      Cell["Load HEPCAT if needed", "ExampleSubsection"],
      Cell["Skip this setup if HEPCAT is already loaded. These examples assume the checkout is in ~/code/hepcat; change the paths if yours is elsewhere. Evaluate loading in its own cell before the calculation.", "ExampleText"],
      Cell[BoxData[inputBoxes[StringRiffle[lines, "\n"]]], "Input", InitializationCell -> True]
    }, Closed]]
  ];
  exampleCells[examples_List] := Flatten[Map[
    Function[example,
      Join[If[KeyExistsQ[example, "section"], {Cell[example["section"], "ExampleSubsection"]}, {}], {
        Cell[richText[Lookup[example, "description", ""]], "ExampleText"],
        Cell[BoxData[inputBoxes[example["input"]]], "Input"]},
        With[{output = Lookup[exampleResults, example["input"], Missing[]]},
          If[StringQ[output] && StringLength[output] <= 2000 && output =!= "$Failed",
            {Cell[BoxData[outputBoxes[output]], "Output"]}, {}]],
        If[!TrueQ[Lookup[example, "evaluate", True]],
          {Cell["This example writes files, starts a longer calculation, or requires an optional external tool. Evaluate it when you want to perform that operation.", "ExampleText"]}, {}]]
    ], examples], 1];
  optionDescription[function_, option_] := Module[{description, functionDescriptions},
    functionDescriptions = Lookup[optionDescriptions, function, <||>];
    description = If[AssociationQ[functionDescriptions], Lookup[functionDescriptions, option, Missing[]], Missing[]];
    If[MissingQ[description], description = Lookup[optionDescriptions, option, Missing[]]];
    If[StringQ[description], description, "See the function's details and examples for this setting."]
  ];
  optionsCells[name_] := Module[{rules, rows},
    rules = ToExpression["ConstructiveDiagrams`" <> name, InputForm,
      Function[symbol, Options[symbol], HoldAllComplete]];
    If[rules === {}, Return[{}]];
    rows = Map[Function[rule,
      With[{optionName = ToString[First[rule], InputForm]},
        {Cell[TextData[{inlineCode[optionName]}], "TableText"],
         Cell[TextData[{inlineCode[ToString[Last[rule], InputForm]]}], "TableText"],
         Cell[richText[optionDescription[name, StringReplace[optionName, "\"" -> ""]]], "TableText"]}
      ]], rules];
    {Cell["Options", "NotesSection"], Cell[BoxData[GridBox[
      Prepend[rows, {Cell["Option", "TableHeader"], Cell["Default", "TableHeader"], Cell["Meaning", "TableHeader"]}],
      GridBoxAlignment -> {"Columns" -> {Left, Left, Left}},
      GridBoxItemSize -> {"Columns" -> {Scaled[.23], Scaled[.19], Scaled[.58]}},
      GridBoxDividers -> {"Rows" -> {False, True, {False}, False}}]], "3ColumnTableMod"]}
  ];
  notebook[cells_List, name_, title_, type_, summary_, kind_] := Notebook[
    cells, WindowTitle -> title <> " - HEPCAT Documentation", Saveable -> False,
    WindowSize -> {1000, 850}, CellContext -> Notebook,
    TaggingRules -> metadata[name, title, type, summary, kind],
    StyleDefinitions -> FrontEnd`FileName[{"Wolfram"}, "Reference.nb"]];
  symbolPage[entry_Association] := Module[{name = entry["name"], cells},
    cells = Join[{
      anchor["SYMBOL", Lookup[entry, "guide", "HEPCAT"]],
      Cell[name, "ObjectName"],
      Cell[richText[entry["summary"]], "Notes"]},
      Map[Cell[TextData[Join[{inlineCode[#["input"]], "  "}, First[richText[#["description"]]]]], "Usage"] &,
        Lookup[entry, "syntaxes", {}]],
      {Cell["Details", "NotesSection"]},
      Cell[richText[#], "Notes"] & /@ Lookup[entry, "details", {}],
      detailCells[Lookup[entry, "detail_sections", {}]],
      optionsCells[name],
      {Cell["Examples", "PrimaryExamplesSection"], setupCells[Lookup[entry, "examples", {}]],
       Cell["Basic Examples", "ExampleSection"]},
      exampleCells[Lookup[entry, "examples", {}]],
      If[Lookup[entry, "related", {}] === {}, {},
        {Cell["See Also", "SeeAlsoSection"], linksCell[entry["related"], "SeeAlso"]}],
      {Cell["Related Guides", "MoreAboutSection"],
       linksCell[DeleteDuplicates[{Lookup[entry, "guide", "HEPCAT"], "HEPCAT"}], "MoreAbout", "guide"]}
    ];
    notebook[cells, name, name, "Symbol", entry["summary"], "ref"]
  ];
  guidePage[entry_Association] := Module[{cells, sections},
    sections = Flatten[Map[Function[section,
      Join[{Cell[section["title"], "GuideFunctionsSubsection"]},
        If[Lookup[section, "description", ""] === "", {}, {Cell[richText[section["description"]], "GuideText"]}],
        Map[Function[name, Cell[TextData[{link[name], ": ",
          Lookup[Lookup[byName, name, <||>], "summary", "Reference documentation."]}], "GuideText"]],
          Lookup[section, "functions", {}]],
        If[Lookup[section, "guides", {}] === {}, {}, {linksCell[section["guides"], "GuideMoreAbout", "guide"]}],
        If[Lookup[section, "tutorials", {}] === {}, {}, {linksCell[section["tutorials"], "GuideTutorial", "tutorial"]}]
      ]], Lookup[entry, "sections", {}]], 1];
    cells = Join[{anchor["GUIDE"], Cell[entry["title"], "GuideTitle"],
      Cell[richText[entry["summary"]], "GuideAbstract"]},
      Cell[richText[#], "GuideAbstract"] & /@ Lookup[entry, "paragraphs", {}], sections,
      If[Lookup[entry, "tutorials", {}] === {}, {},
        Join[{Cell["Tutorials", "GuideTutorialsSection"]},
          {linksCell[entry["tutorials"], "GuideTutorial", "tutorial"]}]]];
    notebook[cells, entry["name"], entry["title"], "Guide", entry["summary"], "guide"]
  ];
  tutorialPage[entry_Association] := Module[{cells, sections},
    sections = Flatten[Map[Function[section,
      Join[{Cell[section["title"], "TechNoteSection"]},
        Cell[richText[#], "TechNoteText"] & /@ Lookup[section, "paragraphs", {}],
        exampleCells[Lookup[section, "examples", {}]]]], Lookup[entry, "sections", {}]], 1];
    cells = Join[{anchor["TUTORIAL"], Cell[entry["title"], "TechNoteTitle"],
      Cell[richText[entry["summary"]], "TechNoteText"]},
      If[entry["name"] === "GettingStarted", {},
        {setupCells[Flatten[Lookup[Lookup[entry, "sections", {}], "examples", {}], 1]]}]
    ];
    cells = Join[cells, sections,
      If[Lookup[entry, "related", {}] === {}, {},
        {Cell["Related Functions", "SeeAlsoSection"], linksCell[entry["related"], "SeeAlso"]}],
      If[Lookup[entry, "guides", {}] === {}, {},
        {Cell["Related Guides", "MoreAboutSection"], linksCell[entry["guides"], "MoreAbout", "guide"]}]];
    notebook[cells, entry["name"], entry["title"], "Tutorial", entry["summary"], "tutorial"]
  ];
  savePage[expression_, directory_, name_] := Module[{path},
    path = FileNameJoin[Prepend[Append[directory, name <> ".nb"], outputRoot]];
    If[!DirectoryQ[DirectoryName[path]], CreateDirectory[DirectoryName[path], CreateIntermediateDirectories -> True]];
    Put[expression, path];
    normalizeText[path]
  ];
  (* Remove only retired generated pages named by the reviewed catalogs.
     Keeping their files would leave internal helpers searchable after rebuild. *)
  hiddenNames = DeleteDuplicates[Join[
    Lookup[Select[allCatalog, !publicQ[#] &], "name", {}],
    Flatten[Lookup[allCatalog, "retiredNames", {}]]]];
  staleFiles = Join[
    FileNameJoin[{outputRoot, "ReferencePages", "Symbols", # <> ".nb"}] & /@ hiddenNames,
    FileNameJoin[{outputRoot, "Guides", # <> ".nb"}] & /@ Lookup[Select[allGuides, !publicQ[#] &], "name", {}]];
  Scan[If[FileExistsQ[#], DeleteFile[#]] &, staleFiles];
  Scan[savePage[symbolPage[#], {"ReferencePages", "Symbols"}, #["name"]] &, catalog];
  Scan[savePage[guidePage[#], {"Guides"}, #["name"]] &, guides];
  Scan[savePage[tutorialPage[#], {"Tutorials"}, #["name"]] &, tutorials];
  usage = Association@Map[Function[entry,
    "ConstructiveDiagrams`" <> entry["name"] -> StringRiffle[Join[
      (#["input"] <> " " <> plainText[#["description"]] &) /@ Lookup[entry, "syntaxes", {}],
      {plainText[entry["summary"]]}], "\n"]], catalog];
  Put[usage, FileNameJoin[{outputRoot, "SymbolUsage.wl"}]];
  normalizeText[FileNameJoin[{outputRoot, "SymbolUsage.wl"}]];
  Get[FileNameJoin[{repositoryRoot, "source", "internal", "HEPCAT-Documentation.wl"}]];
  counts = <|"Symbols" -> Length[catalog], "Guides" -> Length[guides], "Tutorials" -> Length[tutorials]|>;
  Print["Built HEPCAT Help: ", counts];
  indexResult = Get[FileNameJoin[{documentationRoot, "BuildSearchIndex.wl"}]];
  If[FailureQ[indexResult], indexResult, Join[counts, <|"SearchIndex" -> indexResult["SearchIndex"]|>]]
]
End[];
HEPCATDocumentationBuild`Private`$BuildResult
