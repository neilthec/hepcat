(* Check the authored Help catalog and generated notebooks without evaluating
   examples or opening a front end. The separate registration suite checks
   package loading, F1 links and the local Documentation Center search index. *)
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
(* Parsing in an isolated context prevents names in an example from becoming
   Global symbols before the package is loaded. HoldComplete prevents input
   cells, Notebook expressions and source declarations from executing. *)
dcHeldParse[text_String] := Block[
  {$Context = "HEPCATDocumentationSyntax`", $ContextPath = {"System`"}},
  Quiet[Check[ToExpression[text, InputForm, HoldComplete], $Failed]]
];
dcHeldParse[_] := $Failed;
(* A multiline Input cell is one expression. Parentheses retain its complete
   CompoundExpression, instead of a held argument sequence whose first item
   could accidentally be selected by a box-building callback. *)
dcHeldInputParse[text_String] := dcHeldParse["(" <> text <> "\n)"];
dcHeldInputParse[_] := $Failed;
dcParseableQ[text_] := Head[dcHeldInputParse[text]] === HoldComplete;
dcHeldSymbolName[HoldComplete[s_Symbol]] := SymbolName[Unevaluated[s]];
dcShortName[name_String] := Last[StringSplit[name, "`"]];
(* Generated Input boxes are literal source strings or flat native token
   RowBoxes. Neither representation evaluates a HEPCAT symbol's MakeBoxes. *)
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

dcAuthoredFiles = Sort[FileNames["*.json", dcCatalogDirectory]];
dcCatalogFiles = Select[dcAuthoredFiles,
  !MemberQ[{"guides", "tutorials", "option-descriptions", "example-results"}, FileBaseName[#]] &];
dcCheck["reference catalogs are present", dcCatalogFiles =!= {}];
dcCatalogs = Table[Quiet[Check[Import[file, "RawJSON"], $Failed]], {file, dcCatalogFiles}];
dcCheck["each reference catalog is a nonempty list of page associations",
  AllTrue[dcCatalogs, ListQ[#] && # =!= {} && AllTrue[#, AssociationQ] &]];
If[!AllTrue[dcCatalogs, ListQ[#] && AllTrue[#, AssociationQ] &],
  Print["Cannot validate malformed reference catalogs."]; Exit[1]];
dcPages = Join @@ dcCatalogs;
dcPageNames = Lookup[dcPages, "name", Missing["Name"]];
dcCheck["every reference page has a nonempty unique symbol name",
  dcStringListQ[dcPageNames] && DuplicateFreeQ[dcPageNames]];

Do[
  With[{pageName = Lookup[page, "name", "<unnamed>"]},
    dcCheck["summary for " <> pageName,
      dcNonemptyStringQ[Lookup[page, "summary", Missing["Summary"]]]];
    dcCheck["syntax and descriptions for " <> pageName,
      With[{syntaxes = Lookup[page, "syntaxes", {}]},
        ListQ[syntaxes] && syntaxes =!= {} && AllTrue[syntaxes,
          AssociationQ[#] && dcNonemptyStringQ[Lookup[#, "input", ""]] &&
            dcNonemptyStringQ[Lookup[#, "description", ""]] &]]];
    dcCheck["details for " <> pageName,
      With[{details = Lookup[page, "details", {}]}, details =!= {} && dcStringListQ[details]]];
    dcCheck["examples and descriptions for " <> pageName,
      With[{examples = Lookup[page, "examples", {}]},
        ListQ[examples] && examples =!= {} && AllTrue[examples,
          AssociationQ[#] && dcNonemptyStringQ[Lookup[#, "input", ""]] &&
            dcNonemptyStringQ[Lookup[#, "description", ""]] &&
            MemberQ[{True, False}, Lookup[#, "evaluate", Missing["Evaluate"]]] &]]];
    dcCheck["held example syntax for " <> pageName,
      AllTrue[Lookup[Lookup[page, "examples", {}], "input", {}], dcParseableQ]];
    dcCheck["related references and guide for " <> pageName,
      dcStringListQ[Lookup[page, "related", Missing["Related"]]] &&
        dcNonemptyStringQ[Lookup[page, "guide", ""]]];
    dcCheck["generated reference notebook for " <> pageName,
      FileExistsQ[FileNameJoin[{dcReferenceDirectory, pageName <> ".nb"}]]]
  ],
  {page, dcPages}
];

(* Read the literal protected-name list while held. Do not evaluate symbol
   values to discover API names: a symbol may have its own definition. *)
dcLoaderSource = Quiet[Check[Import[FileNameJoin[{dcSourceDirectory, "HEPCAT.wl"}], "Text"], $Failed]];
dcLoaderHeld = dcHeldParse[dcLoaderSource];
dcProtectedLists = Cases[dcLoaderHeld,
  HoldPattern[Apply[Protect, Map[ToExpression, names : {__String}]]] :> names, Infinity];
dcCheck["the package's protected public API declaration is found", dcProtectedLists =!= {}];
dcProtectedNames = DeleteDuplicates[dcShortName /@ Flatten[dcProtectedLists]];
dcCheck["all protected public APIs have reference pages",
  Complement[dcProtectedNames, dcPageNames] === {}];
If[Complement[dcProtectedNames, dcPageNames] =!= {},
  Print["Undocumented protected APIs: ", Complement[dcProtectedNames, dcPageNames]]];

dcSourceFiles = Sort[FileNames["*.wl", dcSourceDirectory]];
dcSourceHeld = dcHeldParse[Quiet[Check[Import[#, "Text"], $Failed]]] & /@ dcSourceFiles;
dcCheck["all package source declarations can be inspected while held",
  AllTrue[dcSourceHeld, Head[#] === HoldComplete &]];
dcUsageSymbols = Flatten[Cases[#,
    HoldPattern[Set[MessageName[s_Symbol, "usage"], _String]] :> HoldComplete[s], Infinity] & /@ dcSourceHeld];
dcUsageNames = DeleteDuplicates[dcHeldSymbolName /@ dcUsageSymbols];
dcCheck["explicit public usage declarations are found", dcUsageNames =!= {}];
dcCheck["all explicitly documented public APIs have reference pages",
  Complement[dcUsageNames, dcPageNames] === {}];
If[Complement[dcUsageNames, dcPageNames] =!= {},
  Print["Undocumented usage declarations: ", Complement[dcUsageNames, dcPageNames]]];

(* These active older helpers predate ::usage and the protected-name inventory.
   Keep their Help available while avoiding exposure of formatter building
   blocks, matching predicates and private implementation details. *)
dcLegacyBaseFunctions = {
  "PlainText", "lPlainText", "SPINAS", "lSPINAS", "CreateSPName",
  "CreateSPInitializationStatement", "CreateSPUpdateStatement",
  "InitializeSPINASSpinorProducts", "UpdateSPINASSpinorProducts", "DeclareSPINASSpinorProducts",
  "ReducexFactors", "xFactorReduced", "xTildeFactorReduced", "ExtractMandelstahm", "ReduceColor",
  "SpinToHelicitySpinors", "MomProdExplicit", "MakeIndicesImplicit", "PermuteParticles",
  "ConvertStringHattedAmplitude", "ComplexifyMomenta", "ExpandComplexMomenta",
  "ReplaceSpinorZ", "UnhatMomenta"
};
dcCheck["retained active legacy Base APIs have reference pages",
  Complement[dcLegacyBaseFunctions, dcPageNames] === {}];

dcGuides = Quiet[Check[Import[FileNameJoin[{dcCatalogDirectory, "guides.json"}], "RawJSON"], $Failed]];
dcTutorials = Quiet[Check[Import[FileNameJoin[{dcCatalogDirectory, "tutorials.json"}], "RawJSON"], $Failed]];
dcCheck["guide and tutorial catalogs are nonempty lists of associations",
  AllTrue[{dcGuides, dcTutorials}, ListQ[#] && # =!= {} && AllTrue[#, AssociationQ] &]];
If[!ListQ[dcGuides] || !ListQ[dcTutorials],
  Print["Cannot validate malformed guide or tutorial catalogs."]; Exit[1]];
dcGuideNames = Lookup[dcGuides, "name", Missing["Name"]];
dcTutorialNames = Lookup[dcTutorials, "name", Missing["Name"]];
dcCheck["guide and tutorial names are nonempty and unique",
  dcStringListQ[dcGuideNames] && DuplicateFreeQ[dcGuideNames] &&
    dcStringListQ[dcTutorialNames] && DuplicateFreeQ[dcTutorialNames]];
dcCheck["every reference page links to a known guide",
  Complement[Lookup[dcPages, "guide"], dcGuideNames] === {}];
dcCheck["the main guide and drill-down symbol index are present",
  MemberQ[dcGuideNames, "HEPCAT"] && MemberQ[dcGuideNames, "ReferenceIndex"]];
Do[
  With[{name = Lookup[guide, "name", "<unnamed>"]},
    dcCheck["guide title, summary and introduction: " <> name,
      dcNonemptyStringQ[Lookup[guide, "title", ""]] &&
        dcNonemptyStringQ[Lookup[guide, "summary", ""]] &&
        dcStringListQ[Lookup[guide, "paragraphs", {}]]];
    dcCheck["generated guide notebook: " <> name,
      FileExistsQ[FileNameJoin[{dcDocumentationDirectory, "Guides", name <> ".nb"}]]];
    dcCheck["guide symbol and topic links: " <> name,
      AllTrue[Lookup[guide, "sections", {}],
        AssociationQ[#] && dcNonemptyStringQ[Lookup[#, "title", ""]] &&
          Complement[Lookup[#, "functions", {}], dcPageNames] === {} &&
          Complement[Lookup[#, "guides", {}], dcGuideNames] === {} &&
          Complement[Lookup[#, "tutorials", {}], dcTutorialNames] === {} &] &&
        Complement[Lookup[guide, "tutorials", {}], dcTutorialNames] === {}]
  ], {guide, dcGuides}];
Do[
  With[{name = Lookup[tutorial, "name", "<unnamed>"]},
    dcCheck["tutorial title, summary and sections: " <> name,
      dcNonemptyStringQ[Lookup[tutorial, "title", ""]] &&
        dcNonemptyStringQ[Lookup[tutorial, "summary", ""]] &&
        ListQ[Lookup[tutorial, "sections", None]] && Lookup[tutorial, "sections", {}] =!= {}];
    dcCheck["generated tutorial notebook: " <> name,
      FileExistsQ[FileNameJoin[{dcDocumentationDirectory, "Tutorials", name <> ".nb"}]]];
    dcCheck["tutorial held example syntax: " <> name,
      AllTrue[Flatten[Lookup[Lookup[tutorial, "sections", {}], "examples", {}]],
        AssociationQ[#] && dcNonemptyStringQ[Lookup[#, "description", ""]] &&
          dcParseableQ[Lookup[#, "input", Missing["Input"]]] &]];
    dcCheck["tutorial symbol and guide links: " <> name,
      Complement[Lookup[tutorial, "related", {}], dcPageNames] === {} &&
        Complement[Lookup[tutorial, "guides", {}], dcGuideNames] === {}]
  ], {tutorial, dcTutorials}];
dcAuthoredContent = Quiet[Check[Import[#, "RawJSON"], $Failed]] & /@ dcAuthoredFiles;
dcCheck["all authored content is valid JSON", FreeQ[dcAuthoredContent, $Failed]];
dcCatalogStrings = Cases[dcAuthoredContent, _String, Infinity];
dcCheck["authored documentation content contains no em dashes",
  AllTrue[dcCatalogStrings, !StringContainsQ[#, FromCharacterCode[8212]] &]];

dcNotebookFiles = Sort[FileNames["*.nb", dcDocumentationDirectory, Infinity]];
dcCheck["the generated Help notebook collection exists", dcNotebookFiles =!= {}];
dcNotebooks = Table[
  dcHeldParse[Quiet[Check[Import[file, "Text"], $Failed]]], {file, dcNotebookFiles}];
Do[
  With[{path = dcNotebookFiles[[i]], notebook = dcNotebooks[[i]]},
    dcCheck["valid held notebook: " <> FileNameDrop[path, FileNameDepth[dcRoot]],
      MatchQ[notebook, HoldComplete[_Notebook]]]
  ], {i, Length[dcNotebookFiles]}];

dcNotebookByPath = AssociationThread[dcNotebookFiles, dcNotebooks];
dcCheckInputRoundtrip[notebook_, examples_List, label_String] := Module[
  {generated, authored, generatedExamples, initialization},
  generated = dcInputSources[notebook];
  authored = Lookup[examples, "input", {}];
  generatedExamples = If[Length[generated] >= Length[authored],
    Take[generated, -Length[authored]], {}];
  dcCheck["literal generated Input sources: " <> label,
    generated =!= {} && AllTrue[generated, dcNonemptyStringQ]];
  dcCheck["authored examples survive unchanged: " <> label,
    generatedExamples === authored];
  dcCheck["held generated Input roundtrip: " <> label,
    Length[generatedExamples] === Length[authored] &&
      AllTrue[generatedExamples, dcParseableQ] &&
      And @@ MapThread[SameQ[dcHeldInputParse[#1], dcHeldInputParse[#2]] &,
        {generatedExamples, authored}]];
  initialization = dcInitializationSources[notebook];
  dcCheck["complete multiline common setup: " <> label,
    initialization =!= {} && AllTrue[initialization, dcNonemptyStringQ] &&
      Complement[{"hepcatDirectory", "modelDirectory", "feynmanModelDirectory"},
        dcAssignedNames[dcHeldInputParse[First[initialization]]]] === {}]
];
Do[
  dcCheckInputRoundtrip[
    Lookup[dcNotebookByPath, FileNameJoin[{dcReferenceDirectory, page["name"] <> ".nb"}], $Failed],
    Lookup[page, "examples", {}], "reference " <> page["name"]],
  {page, dcPages}];
Do[
  dcCheckInputRoundtrip[
    Lookup[dcNotebookByPath,
      FileNameJoin[{dcDocumentationDirectory, "Tutorials", tutorial["name"] <> ".nb"}], $Failed],
    Flatten[Lookup[Lookup[tutorial, "sections", {}], "examples", {}]],
    "tutorial " <> tutorial["name"]],
  {tutorial, dcTutorials}];

(* Only local HEPCAT paclet links belong to this package. Wolfram's own ref
   links and external web links are resolved by Mathematica, not this test. *)
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
    "tutorial", FileNameJoin[{dcDocumentationDirectory, "Tutorials", Last[parts] <> ".nb"}],
    _, $Failed]
];
dcCheck["the generated pages contain local documentation links", dcLocalLinks =!= {}];
Do[
  With[{target = dcLinkTarget[link]},
    dcCheck["local paclet link exists: " <> link, StringQ[target] && FileExistsQ[target]]
  ], {link, dcLocalLinks}];

Print["Documentation content: ", dcChecks - Length[dcFailures], " / ", dcChecks, " checks passed."];
If[dcFailures =!= {}, Print["Failures: ", dcFailures]; Exit[1]];
Print["HEPCAT_TEST_COMPLETED:documentation-content-tests.wl"];
Exit[0];
