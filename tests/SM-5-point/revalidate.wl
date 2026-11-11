(* HEPCAT - High Energy Physics Constructive Amplitude Tool
   Copyright (C) 2026 Neil Christensen
   SPDX-License-Identifier: GPL-3.0-or-later *)

(* Recheck a retained COMPLETE expression after a package-only policy change.
   Uses the same HEPCAT_SM5_CONFIGURATION, HEPCAT_SM5_ID and HEPCAT_SM5_SEED
   environment variables as evaluate.wl. Construction is never called here.
   The original capsule and JSON are verified before an immutable archive copy
   is made. Failed checks are saved separately and never replace the source. *)

Begin["SMFivePointRevalidation`Private`"];
root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Get[FileNameJoin[{root, "source", "HEPCAT.wl"}]];
$HistoryLength = 0;
sha[file_String] := IntegerString[FileHash[file, "SHA256"], 16, 64];
fail[tag_String, message_String] := (Print[Failure[tag, <|"MessageTemplate" -> message|>]]; Quit[2]);
configurationPath = Environment["HEPCAT_SM5_CONFIGURATION"];
id = Environment["HEPCAT_SM5_ID"];
seedText = Environment["HEPCAT_SM5_SEED"];
If[!StringQ[configurationPath] || !FileExistsQ[configurationPath],
  fail["MissingConfiguration", "Supply HEPCAT_SM5_CONFIGURATION with an existing runner configuration."]];
If[!StringQ[id] || !StringQ[seedText] || !StringMatchQ[seedText, DigitCharacter ..],
  fail["MissingRevalidationIdentity", "Supply the case ID and an explicit integer HEPCAT_SM5_SEED."]];
seed = FromDigits[seedText];
configuration = Import[configurationPath, "RawJSON"];
inventoryPath = FileNameJoin[{root, "tests", "SM-5-point", "inventory.json"}];
inventory = Import[inventoryPath, "RawJSON"];
If[!AssociationQ[configuration] || !StringQ[Lookup[configuration, "ContextSHA256", None]] ||
    !AssociationQ[inventory] || !ListQ[Lookup[inventory, "Cases", None]],
  fail["InvalidRevalidationInputs", "The configuration and process inventory must be valid runner records."]];
entry = SelectFirst[inventory["Cases"], Lookup[#, "ID", None] === id &, Missing["UnknownCase"]];
If[MissingQ[entry], fail["UnknownRevalidationCase", "The requested case is absent from the process inventory."]];
outputDirectory = FileNameJoin[{root, "tests", "results", "SM-5-point"}];
sourcePath = FileNameJoin[{outputDirectory, id <> ".wxf"}];
sourceSummaryPath = FileNameJoin[{outputDirectory, id <> ".json"}];
If[!FileExistsQ[sourcePath] || !FileExistsQ[sourceSummaryPath],
  fail["MissingSourceCapsule", "A retained amplitude capsule and its checksum summary are required."]];
sourceHash = sha[sourcePath];
sourceSummaryHash = sha[sourceSummaryPath];
sourceSummary = Import[sourceSummaryPath, "RawJSON"];
If[!AssociationQ[sourceSummary] || Lookup[sourceSummary, "CapsuleSHA256", None] =!= sourceHash ||
    Lookup[sourceSummary, "ID", None] =!= id || Lookup[sourceSummary, "Status", None] =!= "COMPLETE",
  fail["InvalidSourceChecksum", "The retained COMPLETE capsule must match its exact JSON checksum and case identity."]];
source = Quiet[Check[Import[sourcePath, "WXF"], $Failed]];
If[!AssociationQ[source] || Lookup[source, "ID", None] =!= id ||
    Lookup[source, "Status", None] =!= "COMPLETE" ||
    Lookup[source, "Particles", None] =!= entry["Particles"] ||
    Lookup[source, "ContextSHA256", None] =!= Lookup[sourceSummary, "ContextSHA256", Missing["UnknownContext"]] ||
    !KeyExistsQ[source, "Expression"] || MissingQ[source["Expression"]] || FailureQ[source["Expression"]],
  fail["InvalidSourceAmplitude", "The checksum-verified capsule must contain the COMPLETE amplitude for this process."]];
process = source["Particles"];
expression = source["Expression"];
previousContext = source["ContextSHA256"];
configurationHash = sha[configurationPath];
inventoryHash = sha[inventoryPath];
model = ReadModel[FileNameJoin[{root, "models"}], 1];
factorizationModel = ReadModel[FileNameJoin[{root, "models"}], 2];
feynmanModel = ReadModel[FileNameJoin[{root, "tests", "reference-models", "calchep"}], 1];

writeRecord[file_String, value_] := Module[{temporary = file <> ".tmp", stream, bytes},
  bytes = BinarySerialize[value, PerformanceGoal -> "Size"];
  stream = OpenWrite[temporary, BinaryFormat -> True];
  BinaryWrite[stream, Normal[bytes], "Byte"]; Close[stream];
  RenameFile[temporary, file, OverwriteTarget -> True]
];
writeSummary[file_String, value_Association] := Module[{temporary = file <> ".tmp"},
  Export[temporary, value /. _Missing -> Null, "RawJSON"];
  RenameFile[temporary, file, OverwriteTarget -> True]
];
archiveDirectory = FileNameJoin[{outputDirectory, "revalidation-history"}];
If[!DirectoryQ[archiveDirectory], CreateDirectory[archiveDirectory]];
archiveBase = FileNameJoin[{archiveDirectory, id <> "-" <> sourceHash}];
Do[
  With[{from = pair[[1]], to = pair[[2]], expected = pair[[3]]},
    If[FileExistsQ[to],
      If[sha[to] =!= expected, fail["ChangedRevalidationArchive", "A retained source archive has changed."]],
      CopyFile[from, to]]],
  {pair, {{sourcePath, archiveBase <> ".wxf", sourceHash},
    {sourceSummaryPath, archiveBase <> ".json", sourceSummaryHash}}}];

printed = {}; started = AbsoluteTime[];
live = OpenWrite[FileNameJoin[{outputDirectory, id <> "-revalidation.live.txt"}]];
SetAttributes[capture, HoldAll];
capture[body_] := Internal`InheritedBlock[{Print},
  Unprotect[Print]; Print[] := Null;
  Print[args__] := (AppendTo[printed, If[Length[{args}] === 1, First[{args}], SequenceForm[args]]];
    WriteString[live, DateString[{"Hour", ":", "Minute", ":", "Second"}], " ",
      ToString[Last[printed], OutputForm], "\n"]; Null);
  Protect[Print]; body];
report = capture[MemoryConstrained[
  Print["Revalidating retained amplitude: ", StringRiffle[process, ", "]];
  channels = FactorizationAmplitudeChannels[process, factorizationModel];
  rawFeynman = DiagramAmplitudes[CreateDiagrams[process, feynmanModel]];
  benchmark = PrepareAmplitudes[rawFeynman, feynmanModel,
    "PolePreference" -> "IndependentXi", "MaxSteps" -> 0, "InternalOnShell" -> False];
  If[FailureQ[channels] || FailureQ[benchmark] || channels === $Failed || benchmark === $Failed,
    Failure["RevalidationInputs", <|"MessageTemplate" -> "The native channels or independent Feynman preparation failed.",
      "Channels" -> channels, "FeynmanAmplitudes" -> benchmark|>],
    CheckAmplitudes[expression, channels, factorizationModel,
      "FeynmanAmplitudes" -> benchmark, "FeynmanModel" -> feynmanModel,
      "Output" -> "Details", "RandomSeed" -> seed]],
  Lookup[configuration, "MemoryGiB", 4] 2^30,
  Failure["RevalidationMemoryLimit", <|"MessageTemplate" -> "The retained-amplitude recheck exceeded its memory limit."|>]]];
seconds = N[AbsoluteTime[] - started];
rows = If[AssociationQ[report] && MatchQ[Lookup[report, "Table", None], _Dataset],
  Normal[report["Table"]], {}];
requiredTypes = {"Physical", "HighEnergy", "Reference", "Feynman"};
ok = AssociationQ[report] && !FailureQ[report] && ListQ[rows] && rows =!= {} &&
  AllTrue[rows, AssociationQ[#] && Lookup[#, "Status", None] === "PASS" &] &&
  ContainsAll[Lookup[rows, "Type", {}], requiredTypes];
capture[Print["Retained-amplitude revalidation time: ", seconds, " s"];
  If[AssociationQ[report] && KeyExistsQ[report, "Table"], Print[report["Table"]], Print[report]]];
Close[live];
If[sha[sourcePath] =!= sourceHash || sha[sourceSummaryPath] =!= sourceSummaryHash ||
    sha[configurationPath] =!= configurationHash || sha[inventoryPath] =!= inventoryHash,
  ok = False;
  report = Failure["RevalidationInputChanged", <|
    "MessageTemplate" -> "A source capsule, configuration or inventory changed during the recheck.",
    "CheckReport" -> report|>]];
record = Join[source, <|
  "Status" -> If[ok, "COMPLETE", "FAIL"],
  "ContextSHA256" -> configuration["ContextSHA256"],
  "PreviousContextSHA256" -> previousContext,
  "ConstructionContextSHA256" -> Lookup[source, "ConstructionContextSHA256", previousContext],
  "ConstructionRows" -> Lookup[source, "ConstructionRows", Lookup[source, "Rows", {}]],
  "Rows" -> rows,
  "RevalidationStatus" -> If[ok, "PASS", "FAIL"],
  "RevalidationDetails" -> report, "RevalidationPrints" -> printed,
  "RevalidationSeconds" -> seconds, "RevalidationRandomSeed" -> seed,
  "SourceCapsuleSHA256" -> sourceHash, "SourceSummarySHA256" -> sourceSummaryHash,
  "SourceCapsuleArchive" -> FileNameJoin[{"revalidation-history", id <> "-" <> sourceHash <> ".wxf"}],
  "RevalidationConfigurationSHA256" -> configurationHash,
  "RevalidationInventorySHA256" -> inventoryHash|>];
target = If[ok, sourcePath, FileNameJoin[{outputDirectory,
  id <> "-revalidation-" <> StringTake[configuration["ContextSHA256"], UpTo[12]] <> "-" <> ToString[seed] <> ".wxf"}]];
writeRecord[target, record];
summary = KeyTake[record, {"ID", "Particles", "Status", "Seconds", "Complexity", "RandomSeed",
  "ContextSHA256", "PreviousContextSHA256", "ConstructionContextSHA256", "RevalidationStatus",
  "RevalidationSeconds", "RevalidationRandomSeed", "SourceCapsuleSHA256", "SourceSummarySHA256",
  "SourceCapsuleArchive", "RevalidationConfigurationSHA256", "RevalidationInventorySHA256"}];
AssociateTo[summary, {"Rows" -> Length[rows], "FeynmanRows" -> Count[Lookup[rows, "Type", {}], "Feynman"],
  "CheckStatuses" -> Counts[Lookup[rows, "Status", {}]],
  "FailureStage" -> If[ok, Null, "Revalidating retained amplitude"],
  "CapsuleSHA256" -> sha[target]}];
writeSummary[StringReplace[target, ".wxf" -> ".json"], summary];
Print[summary];
End[];
Quit[If[TrueQ[SMFivePointRevalidation`Private`ok], 0, 1]];
