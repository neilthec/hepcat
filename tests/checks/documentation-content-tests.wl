(* Validate reviewed public Help content and its generated native notebooks.
   This suite parses examples while held; it never evaluates example cells. *)
dcRoot = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
dcSourceDirectory = FileNameJoin[{dcRoot, "source"}];
dcCatalogDirectory = FileNameJoin[{dcRoot, "Documentation", "Source"}];
dcDocumentationDirectory = FileNameJoin[{dcRoot, "Documentation", "English"}];
dcReferenceDirectory = FileNameJoin[{dcDocumentationDirectory, "ReferencePages", "Symbols"}];
dcChecks = 0; dcFailures = {};
SetAttributes[dcCheck, HoldRest];
dcCheck[name_, assertion_] := Module[{result},
  dcChecks++;
  result = TimeConstrained[Quiet[Check[assertion, $Failed]], 30, $TimedOut];
  If[!TrueQ[result], AppendTo[dcFailures, name]; Print["FAIL: ", name]]
];
dcNonemptyStringQ[value_] := StringQ[value] && StringLength[StringTrim[value]] > 0;
dcStringListQ[value_] := ListQ[value] && AllTrue[value, dcNonemptyStringQ];
dcPublicQ[entry_Association] := Lookup[entry, "visibility", "public"] === "public";
dcVisibilityQ[entry_Association] := MemberQ[{"public", "internal"}, Lookup[entry, "visibility", "public"]];
(* Parsing in a dedicated context keeps held examples from creating Global
   symbols before HEPCAT is loaded by the separate registration suite. *)
dcHeldParse[text_String] := Block[
  {$Context = "HEPCATDocumentationSyntax`", $ContextPath = {"System`"}},
  Quiet[Check[ToExpression[text, InputForm, HoldComplete], $Failed]]
];
dcHeldParse[_] := $Failed;
dcHeldInputParse[text_String] := dcHeldParse["(" <> text <> "\n)"];
dcHeldInputParse[_] := $Failed;
dcParseableQ[text_] := Head[dcHeldInputParse[text]] === HoldComplete;
dcHeldSymbolName[HoldComplete[s_Symbol]] := If[AtomQ[Unevaluated[s]], SymbolName[Unevaluated[s]], Nothing];
dcShortName[name_String] := Last[StringSplit[name, "`"]];
dcInputSource[HoldComplete[code_String]] := code;
dcInputSource[HoldComplete[RowBox[tokens : {___String}]]] := StringJoin[tokens];
dcInputSource[_] := $Failed;
dcInputSources[notebook_] := Cases[notebook,
  HoldPattern[Cell[BoxData[boxes_], "Input", ___]] :>
    dcInputSource[HoldComplete[boxes]], Infinity];
dcInitializationSources[notebook_] := Cases[notebook,
  HoldPattern[Cell[BoxData[boxes_], "Input", ___, InitializationCell -> True, ___]] :>
    dcInputSource[HoldComplete[boxes]], Infinity];
dcAssignedNames[held_] := DeleteDuplicates[dcHeldSymbolName /@ Cases[held,
  HoldPattern[Set[s_Symbol, _]] :> HoldComplete[s], Infinity]];
dcRichStrings[text_String] := {text};
dcRichStrings[TextData[parts_List]] := Flatten[dcRichStrings /@ parts];
dcRichStrings[_] := {};
dcProseStrings[notebook_] := Flatten[Cases[notebook,
  HoldPattern[Cell[content_, style_String, ___]] /;
      MemberQ[{"Notes", "Usage", "ExampleText", "GuideAbstract", "GuideText", "TechNoteText", "TableText"}, style] :>
    dcRichStrings[content], Infinity]];
dcProseLinks[notebook_] := DeleteDuplicates[Flatten[Cases[notebook,
  HoldPattern[Cell[content_, style_String, ___]] /;
      MemberQ[{"Notes", "Usage", "ExampleText", "GuideAbstract", "GuideText", "TechNoteText", "TableText"}, style] :>
    Cases[HoldComplete[content], HoldPattern[ButtonData -> uri_String] :> uri, Infinity], Infinity]]];
dcAuthoredLinks[value_] := DeleteDuplicates[Flatten[StringCases[#,
    RegularExpression["\\[[^\\]]+\\]\\((paclet:HEPCAT/[^\\s)]+)\\)"] -> "$1"] & /@
  Cases[value, _String, Infinity]]];

dcAuthoredFiles = Sort[FileNames["*.json", dcCatalogDirectory]];
dcCatalogFiles = Select[dcAuthoredFiles,
  !MemberQ[{"guides", "tutorials", "option-descriptions", "example-results"}, FileBaseName[#]] &];
dcCheck["reference catalogs are present", dcCatalogFiles =!= {}];
dcCatalogs = Quiet[Check[Import[#, "RawJSON"], $Failed]] & /@ dcCatalogFiles;
dcCheck["reference catalogs are nonempty lists of page associations",
  AllTrue[dcCatalogs, ListQ[#] && # =!= {} && AllTrue[#, AssociationQ] &]];
If[!AllTrue[dcCatalogs, ListQ[#] && AllTrue[#, AssociationQ] &], Exit[1]];
dcAllPages = Join @@ dcCatalogs;
dcPages = Select[dcAllPages, dcPublicQ];
dcAllPageNames = Lookup[dcAllPages, "name", Missing["Name"]];
dcPageNames = Lookup[dcPages, "name", Missing["Name"]];
dcRetiredNames = Flatten[Lookup[dcAllPages, "retiredNames", {}]];
dcHiddenNames = DeleteDuplicates[Join[
  Lookup[Select[dcAllPages, !dcPublicQ[#] &], "name", {}], dcRetiredNames]];
dcCoveredNames = DeleteDuplicates[Join[dcAllPageNames, dcRetiredNames]];
dcCheck["all catalog names are nonempty and unique",
  dcStringListQ[dcAllPageNames] && DuplicateFreeQ[dcAllPageNames]];
dcCheck["visibility is public or internal, with public as the default",
  AllTrue[dcAllPages, dcVisibilityQ] && dcPublicQ[<|"name" -> "fixture"|>]];
dcCheck["retired names are explicitly classified and never public pages",
  AllTrue[dcAllPages, dcStringListQ[Lookup[#, "retiredNames", {}]] &] &&
    DuplicateFreeQ[dcRetiredNames] && Intersection[dcRetiredNames, dcPageNames] === {}];
dcCoreNames = {"ReadModel", "CreateDiagrams", "DiagramAmplitudes", "CreateArrowDiagrams",
  "Spinor", "SpinorChain", "Mom", "Mass", "MomProd", "PropDen", "ReduceSpinContractions",
  "ReduceSpinorProducts", "ApplyToNumerator", "PrepareAmplitudes", "CompleteFactorization",
  "CompleteHighEnergy", "ConstructAmplitude", "CheckAmplitudes", "PlainText", "TeX", "SPINAS",
  "WriteSPINASProcess", "ValidateMadGraph"};
dcCheck["the core user workflow retains public reference pages", Complement[dcCoreNames, dcPageNames] === {}];
dcCheck["renamed user functions record the two retired spellings",
  MemberQ[dcRetiredNames, "diagramAmplitudes"] && MemberQ[dcRetiredNames, "applyToNumerator"]];

Do[With[{pageName = page["name"]},
  dcCheck["public summary and syntax: " <> pageName,
    dcNonemptyStringQ[Lookup[page, "summary", ""]] &&
      With[{syntaxes = Lookup[page, "syntaxes", {}]}, ListQ[syntaxes] && syntaxes =!= {} &&
        AllTrue[syntaxes, AssociationQ[#] && dcNonemptyStringQ[Lookup[#, "input", ""]] &&
          dcNonemptyStringQ[Lookup[#, "description", ""]] &]]];
  dcCheck["public explanatory details: " <> pageName,
    With[{details = Lookup[page, "details", {}], sections = Lookup[page, "detail_sections", {}]},
      dcStringListQ[details] && (details =!= {} || sections =!= {}) && ListQ[sections] &&
        AllTrue[sections, AssociationQ[#] && dcNonemptyStringQ[Lookup[#, "title", ""]] &&
          dcStringListQ[Lookup[#, "paragraphs", None]] && Lookup[#, "paragraphs", {}] =!= {} &]]];
  dcCheck["public examples and optional section headings: " <> pageName,
    With[{examples = Lookup[page, "examples", {}]}, ListQ[examples] && examples =!= {} &&
      AllTrue[examples, AssociationQ[#] && dcNonemptyStringQ[Lookup[#, "input", ""]] &&
        dcNonemptyStringQ[Lookup[#, "description", ""]] &&
        MemberQ[{True, False}, Lookup[#, "evaluate", None]] &&
        (!KeyExistsQ[#, "section"] || dcNonemptyStringQ[#["section"]]) &]]];
  dcCheck["held public example syntax: " <> pageName,
    AllTrue[Lookup[Lookup[page, "examples", {}], "input", {}], dcParseableQ]];
  dcCheck["public related references: " <> pageName,
    dcStringListQ[Lookup[page, "related", None]] &&
      Complement[Lookup[page, "related", {}], dcPageNames] === {} &&
      dcNonemptyStringQ[Lookup[page, "guide", ""]]];
  dcCheck["generated public reference: " <> pageName,
    FileExistsQ[FileNameJoin[{dcReferenceDirectory, pageName <> ".nb"}]]]
], {page, dcPages}];
dcCheck["hidden and retired reference pages are not deployed",
  Intersection[dcHiddenNames, FileBaseName /@ FileNames["*.nb", dcReferenceDirectory]] === {}];
dcCheck["the deployed reference set is exactly the reviewed public catalog",
  Sort[FileBaseName /@ FileNames["*.nb", dcReferenceDirectory]] === Sort[dcPageNames]];

(* A classified internal symbol still counts as reviewed API inventory. A
   declaration must not silently disappear merely because it is protected. *)
dcLoaderHeld = dcHeldParse[Import[FileNameJoin[{dcSourceDirectory, "HEPCAT.wl"}], "Text"]];
dcProtectedLists = Cases[dcLoaderHeld,
  HoldPattern[Apply[Protect, Map[ToExpression, names : {__String}]]] :> names, Infinity];
dcCheck["the protected API inventory can be inspected", dcProtectedLists =!= {}];
dcProtectedNames = DeleteDuplicates[dcShortName /@ Flatten[dcProtectedLists]];
dcCheck["every protected name is documented or explicitly classified",
  Complement[dcProtectedNames, dcCoveredNames] === {}];
dcSourceFiles = Sort[FileNames["*.wl", dcSourceDirectory]];
dcSourceHeld = dcHeldParse[Import[#, "Text"]] & /@ dcSourceFiles;
dcCheck["all source declarations can be parsed while held", AllTrue[dcSourceHeld, Head[#] === HoldComplete &]];
dcUsageSymbols = Flatten[Cases[#,
  HoldPattern[Set[MessageName[s_Symbol, "usage"], _String]] :> HoldComplete[s], Infinity] & /@ dcSourceHeld];
dcUsageNames = DeleteDuplicates[dcHeldSymbolName /@ dcUsageSymbols];
dcCheck["usage declarations exist and are all reviewed",
  dcUsageNames =!= {} && Complement[dcUsageNames, dcCoveredNames] === {}];
dcSourceSymbolNames = DeleteDuplicates[dcHeldSymbolName /@ Flatten[
  Cases[#, s_Symbol :> HoldComplete[s], Infinity, Heads -> True] & /@ dcSourceHeld]];
dcCheck["every public catalog symbol occurs in the actual package sources",
  Complement[dcPageNames, dcSourceSymbolNames] === {}];

dcAllGuides = Import[FileNameJoin[{dcCatalogDirectory, "guides.json"}], "RawJSON"];
dcTutorials = Import[FileNameJoin[{dcCatalogDirectory, "tutorials.json"}], "RawJSON"];
dcCheck["guide and tutorial catalogs are nonempty lists of associations",
  AllTrue[{dcAllGuides, dcTutorials}, ListQ[#] && # =!= {} && AllTrue[#, AssociationQ] &]];
If[!ListQ[dcAllGuides] || !ListQ[dcTutorials], Exit[1]];
dcGuides = Select[dcAllGuides, dcPublicQ];
dcGuideNames = Lookup[dcGuides, "name", {}];
dcHiddenGuideNames = Lookup[Select[dcAllGuides, !dcPublicQ[#] &], "name", {}];
dcTutorialNames = Lookup[dcTutorials, "name", {}];
dcCheck["guide names and visibility are explicit and valid",
  dcStringListQ[Lookup[dcAllGuides, "name", {}]] && DuplicateFreeQ[Lookup[dcAllGuides, "name", {}]] &&
    AllTrue[dcAllGuides, dcVisibilityQ] && dcStringListQ[dcTutorialNames] && DuplicateFreeQ[dcTutorialNames]];
dcCheck["public pages use public guides", Complement[Lookup[dcPages, "guide", {}], dcGuideNames] === {}];
dcCheck["the main guide and reference index remain available",
  MemberQ[dcGuideNames, "HEPCAT"] && MemberQ[dcGuideNames, "ReferenceIndex"]];
dcCheck["hidden guide pages are not deployed",
  AllTrue[dcHiddenGuideNames, !FileExistsQ[FileNameJoin[{dcDocumentationDirectory, "Guides", # <> ".nb"}]] &]];
dcCheck["the deployed guide set matches public visibility",
  Sort[FileBaseName /@ FileNames["*.nb", FileNameJoin[{dcDocumentationDirectory, "Guides"}]]] === Sort[dcGuideNames]];
Do[With[{name = guide["name"]},
  dcCheck["public guide introduction and links: " <> name,
    dcNonemptyStringQ[Lookup[guide, "title", ""]] && dcNonemptyStringQ[Lookup[guide, "summary", ""]] &&
      dcStringListQ[Lookup[guide, "paragraphs", None]] &&
      AllTrue[Lookup[guide, "sections", {}], AssociationQ[#] &&
        dcNonemptyStringQ[Lookup[#, "title", ""]] &&
        Complement[Lookup[#, "functions", {}], dcPageNames] === {} &&
        Complement[Lookup[#, "guides", {}], dcGuideNames] === {} &&
        Complement[Lookup[#, "tutorials", {}], dcTutorialNames] === {} &] &&
      Complement[Lookup[guide, "tutorials", {}], dcTutorialNames] === {}]
], {guide, dcGuides}];
Do[With[{name = tutorial["name"]},
  dcCheck["tutorial prose, examples and links: " <> name,
    dcNonemptyStringQ[Lookup[tutorial, "title", ""]] && dcNonemptyStringQ[Lookup[tutorial, "summary", ""]] &&
      ListQ[Lookup[tutorial, "sections", None]] && Lookup[tutorial, "sections", {}] =!= {} &&
      AllTrue[Flatten[Lookup[Lookup[tutorial, "sections", {}], "examples", {}]],
        AssociationQ[#] && dcNonemptyStringQ[Lookup[#, "description", ""]] &&
          dcParseableQ[Lookup[#, "input", None]] &] &&
      Complement[Lookup[tutorial, "related", {}], dcPageNames] === {} &&
      Complement[Lookup[tutorial, "guides", {}], dcGuideNames] === {}];
  dcCheck["generated tutorial: " <> name,
    FileExistsQ[FileNameJoin[{dcDocumentationDirectory, "Tutorials", name <> ".nb"}]]]
], {tutorial, dcTutorials}];
dcUsageRegistryHeld = dcHeldParse[Import[FileNameJoin[{dcDocumentationDirectory, "SymbolUsage.wl"}], "Text"]];
dcRegistryNames = dcShortName /@ Cases[dcUsageRegistryHeld,
  HoldPattern[Rule[name_String, _String]] :> name, Infinity];
dcCheck["the usage registry contains exactly public names",
  Sort[dcRegistryNames] === Sort[dcPageNames] && Intersection[dcRegistryNames, dcHiddenNames] === {}];
dcAuthoredContent = Quiet[Check[Import[#, "RawJSON"], $Failed]] & /@ dcAuthoredFiles;
dcCheck["all authored content is valid JSON", FreeQ[dcAuthoredContent, $Failed]];
dcCheck["authored documentation has no em dashes",
  AllTrue[Cases[dcAuthoredContent, _String, Infinity], !StringContainsQ[#, FromCharacterCode[8212]] &]];

dcNotebookFiles = Sort[FileNames["*.nb", dcDocumentationDirectory, Infinity]];
dcCheck["the native Help notebooks exist", dcNotebookFiles =!= {}];
dcNotebooks = dcHeldParse[Import[#, "Text"]] & /@ dcNotebookFiles;
dcCheck["all generated Help notebooks parse while held", AllTrue[dcNotebooks, MatchQ[#, HoldComplete[_Notebook]] &]];
dcNotebookByPath = AssociationThread[dcNotebookFiles, dcNotebooks];
dcCheckInputRoundtrip[notebook_, examples_List, label_String, commonSetup_: True] := Module[
  {generated, authored, generatedExamples, initialization, expectedVariables, initializationHeld},
  generated = dcInputSources[notebook]; authored = Lookup[examples, "input", {}];
  generatedExamples = If[Length[authored] === 0, {}, If[Length[generated] >= Length[authored], Take[generated, -Length[authored]], {}]];
  dcCheck["authored Input source survives unchanged: " <> label, generatedExamples === authored];
  dcCheck["held Input source roundtrip: " <> label,
    AllTrue[generated, dcNonemptyStringQ] && AllTrue[generatedExamples, dcParseableQ] &&
      And @@ MapThread[SameQ[dcHeldInputParse[#1], dcHeldInputParse[#2]] &, {generatedExamples, authored}]];
  initialization = dcInitializationSources[notebook];
  If[TrueQ[commonSetup],
    expectedVariables = Select[{"hepcatDirectory", "modelDirectory", "feynmanModelDirectory"},
      StringContainsQ[StringRiffle[authored, "\n"], #] &];
    initializationHeld = If[Length[initialization] === 1, dcHeldInputParse[First[initialization]], $Failed];
    dcCheck["short loading and only required directory variables: " <> label,
      Length[initialization] === 1 &&
        !FreeQ[initializationHeld, HoldPattern[Get["~/code/hepcat/source/HEPCAT.wl"]]] &&
        Sort[dcAssignedNames[initializationHeld]] === Sort[expectedVariables]];
    dcCheck["optional common setup is collapsed: " <> label,
      !FreeQ[notebook, HoldPattern[CellGroupData[
        {___, Cell[_, "Input", ___, InitializationCell -> True, ___], ___}, Closed]]]],
    dcCheck["Getting Started does not duplicate the common loading panel: " <> label, initialization === {}]
  ]
];
Do[With[{notebook = Lookup[dcNotebookByPath, FileNameJoin[{dcReferenceDirectory, page["name"] <> ".nb"}], $Failed]},
  dcCheckInputRoundtrip[notebook, Lookup[page, "examples", {}], "reference " <> page["name"]];
  dcCheck["detail subsections survive generation: " <> page["name"],
    AllTrue[Lookup[page, "detail_sections", {}], !FreeQ[notebook, Cell[#["title"], "NotesSection"]] &]];
  dcCheck["example subsections survive generation: " <> page["name"],
    AllTrue[Select[Lookup[page, "examples", {}], KeyExistsQ[#, "section"] &],
      !FreeQ[notebook, Cell[#["section"], "ExampleSubsection"]] &]];
  dcCheck["inline public function links are native links in prose: " <> page["name"],
    Complement[Select[dcAuthoredLinks[KeyTake[page, {"summary", "details", "detail_sections", "examples", "syntaxes"}]],
      StringStartsQ[#, "paclet:HEPCAT/ref/"] && MemberQ[dcPageNames, Last[StringSplit[#, "/"]]] &],
      dcProseLinks[notebook]] === {}]
], {page, dcPages}];
Do[dcCheckInputRoundtrip[
  Lookup[dcNotebookByPath, FileNameJoin[{dcDocumentationDirectory, "Tutorials", tutorial["name"] <> ".nb"}], $Failed],
  Flatten[Lookup[Lookup[tutorial, "sections", {}], "examples", {}]], "tutorial " <> tutorial["name"],
  tutorial["name"] =!= "GettingStarted"], {tutorial, dcTutorials}];
dcCheck["major workflow explanations contain native inline function links",
  AllTrue[{"ReadModel", "PrepareAmplitudes", "CompleteFactorization", "WriteSPINASProcess"},
    dcProseLinks[Lookup[dcNotebookByPath, FileNameJoin[{dcReferenceDirectory, # <> ".nb"}], $Failed]] =!= {} &]];
dcCheck["rendered prose has no unprocessed Markdown links or code delimiters",
  AllTrue[Flatten[dcProseStrings /@ dcNotebooks],
    !StringContainsQ[#, RegularExpression["\\[[^\\]]+\\]\\((?:paclet:|https?://)"] | RegularExpression["`[^`\\n]+`"]] &]];

(* Native Wolfram and external links resolve outside this checkout. HEPCAT's
   own links must lead only to deployed public pages. *)
dcLocalLinks = DeleteDuplicates[Cases[dcNotebooks,
  link_String /; StringStartsQ[link, "paclet:HEPCAT"] :> link, Infinity]];
dcLinkTarget[link_String] := Module[{relative, parts},
  relative = StringDrop[link, StringLength["paclet:HEPCAT"]];
  If[relative === "", Return[FileNameJoin[{dcDocumentationDirectory, "Guides", "HEPCAT.nb"}]]];
  relative = First[StringSplit[relative, {"#", "?"}]];
  parts = DeleteCases[StringSplit[relative, "/"], ""];
  If[Length[parts] =!= 2, Return[$Failed]];
  Switch[First[parts],
    "ref", FileNameJoin[{dcReferenceDirectory, Last[parts] <> ".nb"}],
    "guide", FileNameJoin[{dcDocumentationDirectory, "Guides", Last[parts] <> ".nb"}],
    "tutorial", FileNameJoin[{dcDocumentationDirectory, "Tutorials", Last[parts] <> ".nb"}], _, $Failed]
];
dcCheck["the generated pages contain documentation links", dcLocalLinks =!= {}];
Do[With[{target = dcLinkTarget[link]},
  dcCheck["local documentation target exists: " <> link, StringQ[target] && FileExistsQ[target]]
], {link, dcLocalLinks}];
Print["Documentation content: ", dcChecks - Length[dcFailures], " / ", dcChecks, " checks passed."];
If[dcFailures =!= {}, Print["Failures: ", dcFailures]; Exit[1]];
Print["HEPCAT_TEST_COMPLETED:documentation-content-tests.wl"];
Exit[0];
