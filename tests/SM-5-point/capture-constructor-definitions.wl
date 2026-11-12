(* Exact, metadata-only cold capture. Load once; do not read models or calculate amplitudes. *)
Get[Environment["HEPCAT_FINGERPRINT_PACKAGE"]];
Begin["HEPCATColdDefinitionFingerprint`Private`"];
$HistoryLength = 0;
fpDirectory = Environment["HEPCAT_FINGERPRINT_DIRECTORY"];
fpPass = Environment["HEPCAT_FINGERPRINT_PASS"];
fpStarted = AbsoluteTime[];
fpPatterns = {"ConstructiveDiagrams`*", "ConstructiveDiagrams`Private`*",
  "HEPCATCovariantCurrentSources`*", "HEPCATCovariantCurrentSources`Private`*",
  "HEPCATCovariantContactCalibration`*", "HEPCATCovariantContactCalibration`Private`*"};
fpNames = Sort[DeleteDuplicates[Flatten[Names /@ fpPatterns]]];
fpText[expr_] := ToString[expr, InputForm, PageWidth -> Infinity];
fpEntries = Table[ToExpression[name, InputForm, Function[s,
  {Context[Unevaluated[s]] <> SymbolName[Unevaluated[s]], <|
    "DownValues" -> fpText[DownValues[Unevaluated[s]]],
    "UpValues" -> fpText[UpValues[Unevaluated[s]]],
    "SubValues" -> fpText[SubValues[Unevaluated[s]]],
    "NValues" -> fpText[NValues[Unevaluated[s]]],
    "OwnValues" -> fpText[OwnValues[Unevaluated[s]]],
    "DefaultValues" -> fpText[DefaultValues[Unevaluated[s]]],
    "FormatValues" -> fpText[FormatValues[Unevaluated[s]]],
    "Options" -> fpText[Options[Unevaluated[s]]],
    "Attributes" -> fpText[Attributes[Unevaluated[s]]],
    "Messages" -> fpText[Messages[Unevaluated[s]]]|>}, HoldAllComplete]], {name, fpNames}];
fpData = Association[Rule @@@ fpEntries];
If[Length[fpData] =!= Length[fpNames], Print["AMBIGUOUS_SYMBOL_NAMES"]; Quit[3]];
fpRecord = <|"Scope" -> "ColdLoadedConstructorCompatibilityDefinitions",
  "CapturedNamePatterns" -> fpPatterns, "WolframVersion" -> $Version, "SystemID" -> $SystemID,
  "KernelID" -> $KernelID, "PackageLoadedOnce" -> True, "ModelsRead" -> {},
  "PhysicsCalculationsPerformed" -> False, "SymbolCount" -> Length[fpData],
  "Definitions" -> fpData, "Seconds" -> N[AbsoluteTime[] - fpStarted]|>;
fpFinal = FileNameJoin[{fpDirectory, "definitions-" <> fpPass <> "-raw.json"}];
fpTemporary = fpFinal <> ".tmp";
If[FileExistsQ[fpFinal] || FileExistsQ[fpTemporary], Print["CAPTURE_ALREADY_EXISTS"]; Quit[4]];
If[!StringQ[Quiet[Check[Export[fpTemporary, fpRecord, "RawJSON"], $Failed]]], Quit[5]];
If[!StringQ[Quiet[Check[RenameFile[fpTemporary, fpFinal], $Failed]]], Quit[6]];
Print["COLD_DEFINITIONS_CAPTURED ", Length[fpData], " ", fpRecord["Seconds"]];
End[];
Quit[];
