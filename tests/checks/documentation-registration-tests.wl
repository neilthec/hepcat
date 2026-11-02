(* Native Help registration, direct symbol help, local search and safe loading. *)
drRoot = FileNameJoin[FileNameSplit[DirectoryName[DirectoryName[DirectoryName[$InputFileName]]]]];
drSource = FileNameJoin[{drRoot, "source"}];
drLoader = FileNameJoin[{drSource, "HEPCAT.wl"}];
(* Check the simple documented Get in a fresh kernel, without a global path.
   An explicitly invalid path must still fail before package initialization. *)
drMissingSource = FileNameJoin[{$TemporaryDirectory, "hepcat-missing-source-" <> CreateUUID[]}];
drBadFresh = Block[{Global`$HEPCATpath = drMissingSource}, Quiet[Get[drLoader]]];
drFreshInitialized = MemberQ[$Packages, "ConstructiveDiagrams`"];
Clear[Global`$HEPCATpath];
Block[{$MessageList = {}}, Get[drLoader]; drLoadMessages = $MessageList];
drChecks = 0; drFailures = {};
SetAttributes[drCheck, HoldRest];
drCheck[name_, assertion_] := Module[{result},
  drChecks++;
  result = TimeConstrained[Check[assertion, $Failed], 30, $TimedOut];
  If[!TrueQ[result], AppendTo[drFailures, name]; Print["FAIL: ", name]]
];

drDocumentation = FileNameJoin[{drRoot, "Documentation", "English"}];
drCheck["an explicit bad source path fails before fresh initialization",
  drBadFresh === $Failed && !drFreshInitialized];
drCheck["direct Get infers its source directory with no global setup",
  FileNameSplit[Global`$HEPCATpath] === FileNameSplit[drSource] && drLoadMessages === {} &&
    MemberQ[$ContextPath, "ConstructiveDiagrams`"] && $Context === "Global`"];
drCheck["the CamelCase numerator function changes only existing numerators",
  ApplyToNumerator[drNumerator/drDenominator + (drNumerator + 1)/drOtherDenominator,
    drNumerator -> 2] === 2/drDenominator + 3/drOtherDenominator];
drQED = ReadModel[FileNameJoin[{drRoot, "models"}], 2];
drDiagramPairs = DiagramAmplitudes[CreateDiagrams[{"e", "E", "A.+", "A.-"}, drQED]];
drCheck["the CamelCase diagram function computes a real small QED amplitude",
  ListQ[drDiagramPairs] && Length[drDiagramPairs] === 2 &&
    AllTrue[drDiagramPairs, MatchQ[#, {_List, _}] && Last[#] =!= 0 &] &&
    FreeQ[drDiagramPairs, _DiagramAmplitudes | _Failure]];
drDefinitionCount[name_String] := ToExpression["ConstructiveDiagrams`" <> name,
  InputForm, Function[symbol, Length[DownValues[symbol]] + Length[SubValues[symbol]], HoldAllComplete]];
drCheck["retired mixed-case spellings have no callable definitions",
  drDefinitionCount["diagramAmplitudes"] === 0 && drDefinitionCount["applyToNumerator"] === 0];

(* A missing, incomplete or non-string override must not clear working APIs. *)
drDefinitionsBeforeBadPath = DownValues[ApplyToNumerator];
drContextBeforeBadPath = {$Context, $ContextPath};
drBadReload = Block[{Global`$HEPCATpath = drMissingSource}, Quiet[Get[drLoader]]];
drCheck["a missing explicit source override preserves a loaded package",
  drBadReload === $Failed &&
    DownValues[ApplyToNumerator] === drDefinitionsBeforeBadPath &&
    {$Context, $ContextPath} === drContextBeforeBadPath];
drPartialSource = CreateDirectory[];
CopyFile[FileNameJoin[{drSource, "HEPCAT-Base.wl"}], FileNameJoin[{drPartialSource, "HEPCAT-Base.wl"}]];
drPartialReload = Block[{Global`$HEPCATpath = drPartialSource}, Quiet[Get[drLoader]]];
drCheck["an incomplete override is rejected before the first source is loaded",
  drPartialReload === $Failed &&
    DownValues[ApplyToNumerator] === drDefinitionsBeforeBadPath &&
    {$Context, $ContextPath} === drContextBeforeBadPath];
DeleteDirectory[drPartialSource, DeleteContents -> True];
drBadPathType = Block[{Global`$HEPCATpath = 17}, Quiet[Get[drLoader]]];
drCheck["a non-string source override fails without clearing definitions",
  drBadPathType === $Failed &&
    DownValues[ApplyToNumerator] === drDefinitionsBeforeBadPath];

drCheck["legacy symbolic heads receive useful question-mark help",
  StringQ[Mom::usage] && StringContainsQ[Mom::usage, "Mom["] &&
    StringQ[Spinor::usage] && StringContainsQ[Spinor::usage, "Spinor["]];
drCheck["placeholder usage is replaced with useful function help",
  StringQ[SquareAmplitude::usage] && StringLength[SquareAmplitude::usage] > 25 &&
    StringTrim[SquareAmplitude::usage] =!= "Check this out!"];
drProtectedBefore = AssociationMap[ToExpression[#, InputForm, Function[s, Attributes[s], HoldAllComplete]] &,
  {"ConstructiveDiagrams`Mom", "ConstructiveDiagrams`Spinor", "ConstructiveDiagrams`SquareAmplitude"}];
drCheck["loading registers the local documentation paclet",
  AnyTrue[PacletFind["HEPCAT"], ExpandFileName[#["Location"]] === ExpandFileName[drRoot] &]];
drCheck["the bare paclet link resolves to its main guide",
  Documentation`ResolveLink["paclet:HEPCAT", "English", False] ===
    FileNameJoin[{drDocumentation, "Guides", "HEPCAT.nb"}]];
drCheck["the package guide resolves inside this checkout",
  Documentation`ResolveLink["paclet:HEPCAT/guide/HEPCAT", "English", False] ===
    FileNameJoin[{drDocumentation, "Guides", "HEPCAT.nb"}]];
Do[
  With[{symbol = symbolName},
    drCheck["reference page resolves for " <> symbol,
      Documentation`ResolveLink["paclet:HEPCAT/ref/" <> symbol, "English", False] ===
        FileNameJoin[{drDocumentation, "ReferencePages", "Symbols", symbol <> ".nb"}]];
    drCheck["F1/message help links to " <> symbol,
      Documentation`CreateMessageLink["ConstructiveDiagrams`", symbol,
        "usage", "English", False] === "paclet:HEPCAT/ref/" <> symbol]
  ],
  {symbolName, {"ReadModel", "SpinorChain", "DiagramAmplitudes", "ApplyToNumerator", "PrepareAmplitudes", "ConstructAmplitude", "WriteSPINASProcess", "ValidateMadGraph"}}
];

Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
drCheck["reloading registers one local paclet without duplicates",
  Length[Select[PacletFind["HEPCAT"],
    ExpandFileName[#["Location"]] === ExpandFileName[drRoot] &]] === 1];
drCheck["headless loading leaves the front end absent", $FrontEnd === Null];
drCheck["usage registration preserves symbol protection across reload",
  drProtectedBefore === AssociationMap[ToExpression[#, InputForm, Function[s, Attributes[s], HoldAllComplete]] &,
    {"ConstructiveDiagrams`Mom", "ConstructiveDiagrams`Spinor", "ConstructiveDiagrams`SquareAmplitude"}]];

(* Registration is optional for a checkout that includes only source files. *)
drTemporaryDirectory = CreateDirectory[];
CreateDirectory[FileNameJoin[{drTemporaryDirectory, "source"}]];
CopyFile[FileNameJoin[{$HEPCATpath, "HEPCAT-Documentation.wl"}],
  FileNameJoin[{drTemporaryDirectory, "source", "HEPCAT-Documentation.wl"}]];
drCheck["missing documentation is a quiet optional condition",
  Get[FileNameJoin[{drTemporaryDirectory, "source", "HEPCAT-Documentation.wl"}]] === Null];
(* The optional registry supplies strings for existing symbols only. *)
CreateDirectory[FileNameJoin[{drTemporaryDirectory, "Documentation", "English"}],
  CreateIntermediateDirectories -> True];
ConstructiveDiagrams`documentationExistingDescription::usage = "Existing useful description.";
ConstructiveDiagrams`documentationPlaceholder::usage = "Check this out!";
Protect[ConstructiveDiagrams`documentationPlaceholder];
ConstructiveDiagrams`documentationMissingUsage[x_] := x;
Put[<|
  "ConstructiveDiagrams`documentationExistingDescription" -> "Do not replace existing help.",
  "ConstructiveDiagrams`documentationPlaceholder" -> "Replacement useful description.",
  "ConstructiveDiagrams`documentationMissingUsage" -> "New useful description.",
  "ConstructiveDiagrams`documentationUnknownSymbol" -> "Do not create this symbol."|>,
  FileNameJoin[{drTemporaryDirectory, "Documentation", "English", "SymbolUsage.wl"}]];
Get[FileNameJoin[{drTemporaryDirectory, "source", "HEPCAT-Documentation.wl"}]];
drCheck["an existing rich usage message is preserved",
  ConstructiveDiagrams`documentationExistingDescription::usage === "Existing useful description."];
drCheck["a protected placeholder usage is replaced without unprotecting the API",
  ConstructiveDiagrams`documentationPlaceholder::usage === "Replacement useful description." &&
    MemberQ[Attributes[ConstructiveDiagrams`documentationPlaceholder], Protected]];
drCheck["missing usage is supplied for an existing symbol",
  ConstructiveDiagrams`documentationMissingUsage::usage === "New useful description."];
drCheck["the registry never creates unknown API symbols",
  Names["ConstructiveDiagrams`documentationUnknownSymbol"] === {}];
Unprotect[ConstructiveDiagrams`documentationPlaceholder];
Remove["ConstructiveDiagrams`documentationExistingDescription",
  "ConstructiveDiagrams`documentationPlaceholder", "ConstructiveDiagrams`documentationMissingUsage"];
DeleteDirectory[drTemporaryDirectory, DeleteContents -> True];

Needs["DocumentationSearch`"];
drIndexDirectories = Select[DocumentationSearch`DocumentationIndexes[],
  StringStartsQ[ExpandFileName[#], ExpandFileName[drDocumentation] <> $PathnameSeparator] &];
drCheck["the native Help search discovers the local index", drIndexDirectories =!= {}];
drCheck["native Help search finds the main HEPCAT guide",
  !FreeQ[DocumentationSearch`SearchDocumentation[drIndexDirectories, {}, "HEPCAT", "Limit" -> 10],
    "HEPCAT/guide/HEPCAT"]];
(* Search hits must also render as the real Documentation Center result cells.
   Checking only titles and URIs misses absent display metadata. No notebook
   is opened by ExportSearchResults. *)
drCheckRenderedSearch[query_String, uri_String] := Module[{results, matches, notebook},
  results = DocumentationSearch`SearchDocumentation[drIndexDirectories, {}, query, "Limit" -> 10];
  matches = "Matches" /. results;
  drCheck["native search returns complete display fields for " <> query,
    ListQ[matches] && matches =!= {} && AllTrue[matches,
      MatchQ[#, {_String, _String, _String, _String, _String, _String}] &]];
  notebook = Check[DocumentationSearch`ExportSearchResults[results, "Notebook"], $Failed];
  drCheck["native Help search renders clickable results for " <> query,
    MatchQ[notebook, _Notebook] &&
      Length[Cases[notebook, Cell[_, "SearchResultCell", ___], Infinity]] === Length[matches] &&
      !FreeQ[notebook, "paclet:" <> uri]]
];
drCheckRenderedSearch["HEPCAT", "HEPCAT/guide/HEPCAT"];
drCheckRenderedSearch["hepcat", "HEPCAT/guide/HEPCAT"];
drCheckRenderedSearch["ReadModel", "HEPCAT/ref/ReadModel"];
drCheckRenderedSearch["Getting Started", "HEPCAT/tutorial/GettingStarted"];

drAllSearchResults = DocumentationSearch`SearchDocumentation[drIndexDirectories, {}, "HEPCAT", "Limit" -> 1000];
drAllSearchMatches = "Matches" /. drAllSearchResults;
(* Reviewed internals and retired names remain in the source inventory, but
   must not leak into Help pages, native search or the question-mark registry. *)
drCatalogDirectory = FileNameJoin[{drRoot, "Documentation", "Source"}];
drPublicQ[entry_Association] := Lookup[entry, "visibility", "public"] === "public";
drAllCatalog = Join @@ (Import[FileNameJoin[{drCatalogDirectory, #}], "RawJSON"] & /@
  {"base.json", "model.json", "feynman-export.json"});
drPublicNames = Lookup[Select[drAllCatalog, drPublicQ], "name", {}];
drHiddenNames = DeleteDuplicates[Join[
  Lookup[Select[drAllCatalog, !drPublicQ[#] &], "name", {}],
  Flatten[Lookup[drAllCatalog, "retiredNames", {}]]]];
drAllGuides = Import[FileNameJoin[{drCatalogDirectory, "guides.json"}], "RawJSON"];
drHiddenGuides = Lookup[Select[drAllGuides, !drPublicQ[#] &], "name", {}];
drHiddenURIs = Join[("HEPCAT/ref/" <> # &) /@ drHiddenNames,
  ("HEPCAT/guide/" <> # &) /@ drHiddenGuides];
drIndexedURIs = If[ListQ[drAllSearchMatches], Cases[drAllSearchMatches, {_, _, _, uri_String, _, _} :> uri], {}];
drCheck["nice and ExpandMomHatAlt are excluded from public Help registration",
  ContainsAll[drHiddenNames, {"nice", "ExpandMomHatAlt"}] &&
    Intersection[drPublicNames, {"nice", "ExpandMomHatAlt"}] === {}];
drCheck["internal and retired pages are absent from native search",
  Intersection[drHiddenURIs, drIndexedURIs] === {}];
drCheck["every public symbol is present in native search",
  Complement[("HEPCAT/ref/" <> # &) /@ drPublicNames, drIndexedURIs] === {}];
drUsageRegistry = Get[FileNameJoin[{drDocumentation, "SymbolUsage.wl"}]];
drCheck["only public catalog names are registered for usage help",
  AssociationQ[drUsageRegistry] &&
    Sort[Keys[drUsageRegistry]] === Sort[("ConstructiveDiagrams`" <> # &) /@ drPublicNames] &&
    Intersection[Keys[drUsageRegistry], ("ConstructiveDiagrams`" <> # &) /@ drHiddenNames] === {}];
drCheck["no internal or retired reference notebook remains on disk",
  Intersection[drHiddenNames, FileBaseName /@ FileNames["*.nb", FileNameJoin[{drDocumentation, "ReferencePages", "Symbols"}]]] === {} &&
    AllTrue[drHiddenGuides, !FileExistsQ[FileNameJoin[{drDocumentation, "Guides", # <> ".nb"}]] &]];

drCheck["every Help page is indexed with complete display metadata",
  ListQ[drAllSearchMatches] &&
    Length[drAllSearchMatches] === Length[FileNames["*.nb", drDocumentation, Infinity]] &&
    AllTrue[drAllSearchMatches, MatchQ[#, {_String, _String, _String, _String, _String, _String}] &]];
drCheck["every indexed Help page renders in native search without messages",
  Module[{notebook = DocumentationSearch`ExportSearchResults[drAllSearchResults, "Notebook"]},
    MatchQ[notebook, _Notebook] &&
      Length[Cases[notebook, Cell[_, "SearchResultCell", ___], Infinity]] === Length[drAllSearchMatches]]];

(* The Documentation Center searches the combined system/paclet index, not
   only the HEPCAT index. Exercise that public route as well. *)
drGlobalSearchResults = DocumentationSearch`SearchDocumentation["hepcat", "Limit" -> 10];
drGlobalSearchMatches = "Matches" /. drGlobalSearchResults;
drCheck["combined system Help search exposes the HEPCAT guide with its display label",
  ListQ[drGlobalSearchMatches] && AllTrue[drGlobalSearchMatches,
    MatchQ[#, {_String, _String, _String, _String, _String, _String}] &] &&
    MemberQ[drGlobalSearchMatches,
      {_String, "Guide", _String, "HEPCAT/guide/HEPCAT", "HEPCAT Guide", _String}]];
drCheck["combined system Help search renders the clickable HEPCAT results without messages",
  Module[{notebook = DocumentationSearch`ExportSearchResults[drGlobalSearchResults, "Notebook"]},
    MatchQ[notebook, _Notebook] &&
      Length[Cases[notebook, Cell[_, "SearchResultCell", ___], Infinity]] === Length[drGlobalSearchMatches] &&
      !FreeQ[notebook, "paclet:HEPCAT/guide/HEPCAT"]]];

drCheck["native exact symbol search finds ReadModel",
  DocumentationSearch`DirectHitSearch[drIndexDirectories, "ReadModel"] ===
    {"ReadModel", "HEPCAT/ref/ReadModel"}];

drCheck["native exact symbol search finds the MadGraph validation reference",
  DocumentationSearch`DirectHitSearch[drIndexDirectories, "ValidateMadGraph"] ===
    {"ValidateMadGraph", "HEPCAT/ref/ValidateMadGraph"}];

Print["Documentation registration: ", drChecks - Length[drFailures], " / ", drChecks, " checks passed."];
If[drFailures =!= {}, Print["Failures: ", drFailures]; Exit[1]];
Print["HEPCAT_TEST_COMPLETED:documentation-registration-tests.wl"];
Exit[0];
