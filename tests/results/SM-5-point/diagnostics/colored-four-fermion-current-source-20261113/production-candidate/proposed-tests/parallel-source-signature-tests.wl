(* Metadata-only worker-source signature controls. No subkernel or amplitude job is started. *)
psRepository = Environment["HEPCAT_TEST_REPOSITORY"];
If[!StringQ[psRepository] || psRepository === "",
 psRepository = DirectoryName[ExpandFileName[$InputFileName]];
 While[!FileExistsQ[FileNameJoin[{psRepository, "source", "HEPCAT.wl"}]] && ParentDirectory[psRepository] =!= psRepository,
  psRepository = ParentDirectory[psRepository]]];
psEntrypoint = Environment["HEPCAT_TEST_PACKAGE_ENTRYPOINT"];
If[!StringQ[psEntrypoint] || psEntrypoint === "", psEntrypoint = FileNameJoin[{psRepository, "source", "HEPCAT.wl"}]];
Get[psEntrypoint];
Begin["HEPCATParallelSourceSignatureTests`Private`"];
$HistoryLength = 0; checks = 0; failures = {};
SetAttributes[check, HoldRest];
check[name_, test_] := (checks++; If[!TrueQ[test], AppendTo[failures, name]; Print["FAIL: ", name]]);
sourceRoot = DirectoryName[ExpandFileName[Global`psEntrypoint]];
required = {"HEPCAT.wl", "HEPCAT-Base.wl", "internal/HEPCAT-Parameters.wl", "HEPCAT-Model.wl",
 "internal/HEPCAT-Feynman.wl", "internal/HEPCAT-Color.wl", "internal/HEPCAT-SPINAS.wl",
 "internal/HEPCAT-MadGraph.wl", "internal/HEPCAT-CovariantSources.wl",
 "internal/HEPCAT-CovariantContacts.wl", "internal/HEPCAT-ColoredCurrents.wl",
 "internal/HEPCAT-SPINAS-Optimize.wl", "internal/export/spinas-runtime.hpp"};
newRequired = {"internal/HEPCAT-MadGraph.wl", "internal/HEPCAT-CovariantSources.wl",
 "internal/HEPCAT-CovariantContacts.wl", "internal/HEPCAT-ColoredCurrents.wl"};
optional = {"internal/proofs/laurent-proof-runtime.cpp", "internal/proofs/laurent-proof-runtime.py",
 "internal/proofs/laurent-proof-factor.py", "internal/proofs/laurent-proof-verify.py"};
check["all declared required modules exist", AllTrue[FileNameJoin[{sourceRoot, #}] & /@ required, FileExistsQ]];
If[failures =!= {}, Print["Required source fixture is incomplete"]; Quit[1]];
copyRoot = CreateDirectory[FileNameJoin[{$TemporaryDirectory, "hepcat-source-signature-" <> CreateUUID[]}]];
originalDirectory = ConstructiveDiagrams`Private`$amplitudeParallelSourceDirectory;
originalLoadedSignature = ConstructiveDiagrams`Private`$amplitudeParallelLoadedSignature;
Internal`WithLocalSettings[Null,
 Do[path = FileNameJoin[{copyRoot, name}]; If[!DirectoryQ[DirectoryName[path]], CreateDirectory[DirectoryName[path], CreateIntermediateDirectories -> True]];
  If[FileExistsQ[FileNameJoin[{sourceRoot, name}]], CopyFile[FileNameJoin[{sourceRoot, name}], path],
   Export[path, "optional signature fixture\n", "Text"]], {name, Join[required, optional]}];
 baselineFiles = AssociationMap[FileHash[FileNameJoin[{copyRoot, #}], "SHA256"] &, Join[required, optional]];
 Block[{ConstructiveDiagrams`Private`$amplitudeParallelSourceDirectory = copyRoot},
  baseline = ConstructiveDiagrams`Private`amplitudeParallelSignature[];
  check["the complete source signature is finite", IntegerQ[baseline]];
  expected = Hash[Join[FileHash /@ (FileNameJoin[{copyRoot, #}] & /@ required),
   Function[name, With[{p = FileNameJoin[{copyRoot, name}]},
    If[FileExistsQ[p], FileHash[p], Missing["OptionalProofSourceAbsent", name]]]] /@ optional]];
  check["the existing ordered hash formula and optional policy are unchanged", baseline === expected];
  Do[path = FileNameJoin[{copyRoot, name}]; stream = OpenAppend[path]; WriteString[stream, "\n(* signature mutation fixture *)\n"]; Close[stream];
   changed = ConstructiveDiagrams`Private`amplitudeParallelSignature[];
   CopyFile[FileNameJoin[{sourceRoot, name}], path, OverwriteTarget -> True];
   restored = ConstructiveDiagrams`Private`amplitudeParallelSignature[];
   check["editing required source changes its signature and restores: " <> name,
    IntegerQ[changed] && changed =!= baseline && restored === baseline], {name, newRequired}];
  Do[path = FileNameJoin[{copyRoot, name}]; bytes = BinaryReadList[path, "Byte"]; DeleteFile[path];
   absent = ConstructiveDiagrams`Private`amplitudeParallelSignature[];
   stream = OpenWrite[path, BinaryFormat -> True]; BinaryWrite[stream, bytes, "Byte"]; Close[stream];
   restored = ConstructiveDiagrams`Private`amplitudeParallelSignature[];
   check["missing required source fails closed and restores: " <> name,
    absent === $Failed && restored === baseline], {name, newRequired}];
  Do[path = FileNameJoin[{copyRoot, name}]; bytes = BinaryReadList[path, "Byte"]; DeleteFile[path];
   absent = ConstructiveDiagrams`Private`amplitudeParallelSignature[];
   stream = OpenWrite[path, BinaryFormat -> True]; BinaryWrite[stream, bytes, "Byte"]; Close[stream];
   restored = ConstructiveDiagrams`Private`amplitudeParallelSignature[];
   check["missing optional proof source stays finite and distinct: " <> name,
    IntegerQ[absent] && absent =!= baseline && restored === baseline], {name, optional}];
  check["every temporary source byte is restored",
   baselineFiles === AssociationMap[FileHash[FileNameJoin[{copyRoot, #}], "SHA256"] &, Join[required, optional]]]
 ],
 If[DirectoryQ[copyRoot], DeleteDirectory[copyRoot, DeleteContents -> True]]];
check["the loaded source directory and cached signature are untouched",
 ConstructiveDiagrams`Private`$amplitudeParallelSourceDirectory === originalDirectory &&
 ConstructiveDiagrams`Private`$amplitudeParallelLoadedSignature === originalLoadedSignature];
If[checks =!= 17, AppendTo[failures, "Not every source-signature control executed"]];
Print[ToString[checks - Length[failures]] <> "/" <> ToString[checks] <> " parallel source-signature checks passed"];
If[failures =!= {}, Print[failures]];
If[failures === {}, Print["HEPCAT_TEST_COMPLETED:parallel-source-signature-tests.wl"]];
Quit[If[failures === {}, 0, 1]];
