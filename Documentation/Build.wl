(* ::Package:: *)

(* Build deployed Mathematica Help notebooks from the reviewed source catalogs.
   Run Get["/absolute/path/to/hepcat/Documentation/Build.wl"] in a licensed
   kernel. Example cells are tokenized as literal source and never evaluated. *)
Global`$HEPCATpath = FileNameJoin[{ParentDirectory[DirectoryName[$InputFileName]], "source"}];
Get[FileNameJoin[{Global`$HEPCATpath, "HEPCAT.wl"}]];
Needs["CodeParser`"];
Begin["HEPCATDocumentationBuild`Private`"];

$BuildResult = Module[{documentationRoot, repositoryRoot, sourceRoot, outputRoot, catalogFiles,
    catalog, byName, guides, tutorials, optionDescriptions, exampleResults, guideTitles, tutorialTitles, readJSON, inputBoxes, outputBoxes, normalizeText,
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
  catalog = Flatten[readJSON /@ catalogFiles, 1];
  If[catalog === {} || !VectorQ[catalog, AssociationQ],
    Return[Failure["MissingCatalog", <|"MessageTemplate" -> "HEPCAT documentation catalogs are missing."|>]]];
  If[!DuplicateFreeQ[Lookup[catalog, "name"]],
    Return[Failure["DuplicateReferenceNames", <|"MessageTemplate" -> "Reference-page names must be unique."|>]]];
  byName = AssociationThread[Lookup[catalog, "name"], catalog];
  guides = readJSON["guides.json"];
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
          "Mandelstahm[" | "Mom[" | "Mass["], Return[inputBoxes[code]]];
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
  metadata[name_, title_, type_, summary_, kind_] := {
    "Metadata" -> {"context" -> "ConstructiveDiagrams`", "uri" -> "HEPCAT/" <> kind <> "/" <> name,
      "title" -> title, "type" -> type, "label" -> "HEPCAT " <> type,
      "paclet" -> "HEPCAT", "language" -> "en",
      "summary" -> summary, "keywords" -> {"HEPCAT", name, "spinor amplitudes"},
      "status" -> "None", "index" -> True}};
  anchor[label_, guide_: "HEPCAT"] := Cell[BoxData[GridBox[{{
    Cell["HEPCAT " <> label, "PacletNameCell"],
    Cell[TextData[{link["HEPCAT", "guide", "HEPCAT"], "  •  ",
      link[guide, "guide", "Related Guide"]}], "AnchorBar"]}},
      GridBoxAlignment -> {"Columns" -> {Left, Right}},
      GridBoxItemSize -> {"Columns" -> {Scaled[.42], Scaled[.58]}}]], "AnchorBarGrid"];
  setupCode = StringRiffle[{
    "If[!StringQ[Global`$HEPCATpath] || !DirectoryQ[Global`$HEPCATpath],",
    "  Global`$HEPCATpath = FileNameJoin[{First[PacletFind[\"HEPCAT\"]][\"Location\"], \"source\"}]];",
    "If[!MemberQ[$Packages, \"ConstructiveDiagrams`\"],",
    "  Get[FileNameJoin[{Global`$HEPCATpath, \"HEPCAT.wl\"}]]];",
    "hepcatDirectory = ParentDirectory[Global`$HEPCATpath];",
    "modelDirectory = FileNameJoin[{hepcatDirectory, \"models\"}];",
    "feynmanModelDirectory = FileNameJoin[{hepcatDirectory, \"model_feynman\"}];"
  }, "\n"];
  setupCells[] := Cell[CellGroupData[{
    Cell["Load HEPCAT", "ExampleSubsection"],
    Cell["Evaluate this setup once before the examples. The paths refer to the local HEPCAT checkout; no model parameters are assigned global numerical values.", "ExampleText"],
    Cell[BoxData[inputBoxes[setupCode]], "Input", InitializationCell -> True]
  }, Open]];
  exampleCells[examples_List] := Flatten[Map[
    Function[example,
      Join[{
        Cell[Lookup[example, "description", ""], "ExampleText"],
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
         Cell[optionDescription[name, StringReplace[optionName, "\"" -> ""]], "TableText"]}
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
      Cell[entry["summary"], "Notes"]},
      Map[Cell[TextData[{inlineCode[#["input"]], "  ", #["description"]}], "Usage"] &,
        Lookup[entry, "syntaxes", {}]],
      {Cell["Details", "NotesSection"]},
      Cell[#, "Notes"] & /@ Lookup[entry, "details", {}],
      optionsCells[name],
      {Cell["Examples", "PrimaryExamplesSection"], setupCells[],
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
        If[Lookup[section, "description", ""] === "", {}, {Cell[section["description"], "GuideText"]}],
        Map[Function[name, Cell[TextData[{link[name], ": ",
          Lookup[Lookup[byName, name, <||>], "summary", "Reference documentation."]}], "GuideText"]],
          Lookup[section, "functions", {}]],
        If[Lookup[section, "guides", {}] === {}, {}, {linksCell[section["guides"], "GuideMoreAbout", "guide"]}],
        If[Lookup[section, "tutorials", {}] === {}, {}, {linksCell[section["tutorials"], "GuideTutorial", "tutorial"]}]
      ]], Lookup[entry, "sections", {}]], 1];
    cells = Join[{anchor["GUIDE"], Cell[entry["title"], "GuideTitle"],
      Cell[entry["summary"], "GuideAbstract"]},
      Cell[#, "GuideAbstract"] & /@ Lookup[entry, "paragraphs", {}], sections,
      If[Lookup[entry, "tutorials", {}] === {}, {},
        Join[{Cell["Tutorials", "GuideTutorialsSection"]},
          {linksCell[entry["tutorials"], "GuideTutorial", "tutorial"]}]]];
    notebook[cells, entry["name"], entry["title"], "Guide", entry["summary"], "guide"]
  ];
  tutorialPage[entry_Association] := Module[{cells, sections},
    sections = Flatten[Map[Function[section,
      Join[{Cell[section["title"], "TechNoteSection"]},
        Cell[#, "TechNoteText"] & /@ Lookup[section, "paragraphs", {}],
        exampleCells[Lookup[section, "examples", {}]]]], Lookup[entry, "sections", {}]], 1];
    cells = Join[{anchor["TUTORIAL"], Cell[entry["title"], "TechNoteTitle"],
      Cell[entry["summary"], "TechNoteText"], setupCells[]}, sections,
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
  Scan[savePage[symbolPage[#], {"ReferencePages", "Symbols"}, #["name"]] &, catalog];
  Scan[savePage[guidePage[#], {"Guides"}, #["name"]] &, guides];
  Scan[savePage[tutorialPage[#], {"Tutorials"}, #["name"]] &, tutorials];
  usage = Association@Map[Function[entry,
    "ConstructiveDiagrams`" <> entry["name"] -> StringRiffle[Join[
      (#["input"] <> " " <> #["description"] &) /@ Lookup[entry, "syntaxes", {}],
      {entry["summary"]}], "\n"]], catalog];
  Put[usage, FileNameJoin[{outputRoot, "SymbolUsage.wl"}]];
  normalizeText[FileNameJoin[{outputRoot, "SymbolUsage.wl"}]];
  Get[FileNameJoin[{repositoryRoot, "source", "HEPCAT-Documentation.wl"}]];
  counts = <|"Symbols" -> Length[catalog], "Guides" -> Length[guides], "Tutorials" -> Length[tutorials]|>;
  Print["Built HEPCAT Help: ", counts];
  indexResult = Get[FileNameJoin[{documentationRoot, "BuildSearchIndex.wl"}]];
  If[FailureQ[indexResult], indexResult, Join[counts, <|"SearchIndex" -> indexResult["SearchIndex"]|>]]
]
End[];
HEPCATDocumentationBuild`Private`$BuildResult
