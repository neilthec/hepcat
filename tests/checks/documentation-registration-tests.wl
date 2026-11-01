(* Native Help registration, direct symbol help, local search and safe loading. *)
drRoot = FileNameJoin[FileNameSplit[DirectoryName[DirectoryName[DirectoryName[$InputFileName]]]]];
Global`$HEPCATpath = FileNameJoin[{drRoot, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
drChecks = 0; drFailures = {};
SetAttributes[drCheck, HoldRest];
drCheck[name_, assertion_] := Module[{result},
  drChecks++;
  result = TimeConstrained[Check[assertion, $Failed], 30, $TimedOut];
  If[!TrueQ[result], AppendTo[drFailures, name]; Print["FAIL: ", name]]
];

drDocumentation = FileNameJoin[{drRoot, "Documentation", "English"}];
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
  {symbolName, {"ReadModel", "SpinorChain", "PrepareAmplitudes", "ConstructAmplitude", "WriteSPINASProcess", "ValidateMadGraph"}}
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
