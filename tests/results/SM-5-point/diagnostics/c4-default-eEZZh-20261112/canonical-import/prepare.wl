(* Metadata-only import of an already completed actual public default run. *)
Get["/Users/neil/code/hepcat/source/HEPCAT.wl"];
Begin["HEPCATC4CanonicalImport`Private`"];
$ContextPath=Prepend[DeleteCases[$ContextPath,"HEPCATC4CanonicalImport`Private`"],"HEPCATC4CanonicalImport`Private`"];
$HistoryLength=0;
root="/Users/neil/code/hepcat";
sourceDirectory=FileNameJoin[{root,"tests","results","SM-5-point","diagnostics","c4-default-eEZZh-20261112"}];
outputDirectory=FileNameJoin[{sourceDirectory,"canonical-import"}];
sourceFile=FileNameJoin[{sourceDirectory,"construction.wxf"}];
sha[file_]:=IntegerString[FileHash[file,"SHA256"],16,64];
assert[condition_,name_]:=If[!TrueQ[condition],Print["IMPORT_REJECTED ",name];Quit[2]];
heldSHA[expression_]:=With[{value=expression},IntegerString[Hash[HoldComplete[value],"SHA256"],16,64]];
sourceSHA="859a8e655aa871ea34cbb6e62a4648a5f9f4917bf665992ba846f7f7ac53bddd";
assert[sha[sourceFile]===sourceSHA,"original capsule hash"];
source=Import[sourceFile,"WXF"];
inventory=Import[FileNameJoin[{root,"tests","SM-5-point","inventory.json"}],"RawJSON"];
entry=SelectFirst[inventory["Cases"],#["ID"]==="sm5_f13b4237bc07"&];
assert[AssociationQ[source]&&source["Status"]==="COMPLETE"&&source["CallType"]==="ActualPublicDefaultC4"&&source["CanonicalRecord"]===False,"actual default record"];
assert[source["Particles"]===entry["Particles"]&&source["ContextSHA256"]==="a91edec8580c2721ccbe8d5304442e9e506f9fd89dc911c250a550c31723a238","inventory and context"];
details=source["Details"];
assert[AssociationQ[details]&&details["Status"]==="COMPLETE"&&details["Process"]===entry["Particles"],"complete constructor details"];
assert[source["PublicOptionsSupplied"]==={"FeynmanModel","Output"},"actual supplied options"];
assert[KeyDrop[details["RequestedOptions"],{"FeynmanModel","Output"}]===KeyDrop[Association[Options[ConstructAmplitude]],{"FeynmanModel","Output"}],"remaining options are defaults"];
assert[details["RequestedOptions"]["RandomSeed"]===Automatic&&details["RandomSeed"]===6782590582154204676,"original automatic seed"];
assert[details["Complexity"]===1094&&heldSHA[source["Expression"]]==="57e939b87ab5277e4dca64fd658aca2b8fcde7100642a4c34b9a4a0565015fd3","original exact expression"];
assert[SameQ[source["Expression"],details["Amplitude"]]&&SameQ[source["AmplitudePair"],details["AmplitudePair"]],"original amplitude binding"];
rows=source["Rows"];
assert[Length[rows]===13&&AllTrue[rows,Lookup[#,"Status",None]==="PASS"&]&&Counts[Lookup[rows,"Type"]]===<|"Physical"->10,"HighEnergy"->1,"Reference"->1,"Feynman"->1|>,"actual final native rows"];
reports=details["RepresentativeSelection"]["ValidationReports"];
assert[ListQ[reports]&&Length[reports]===3&&Length[DeleteDuplicates[Lookup[reports,"ParameterRules"]]]===3,"three distinct parameter groups"];
assert[AllTrue[reports,Function[item,With[{report=item["Report"]},
 AssociationQ[report]&&Length[report["Checks"]]===13&&AllTrue[report["Checks"],Lookup[#,"Status",None]==="PASS"&]&&
 Counts[Lookup[report["Checks"],"Type"]]===<|"Physical"->10,"HighEnergy"->1,"Reference"->1,"Feynman"->1|>&&
 report["FeynmanCheck"]["Status"]==="PASS"&&report["FeynmanCheck"]["InternalOnShell"]===False&&
 report["FeynmanCheck"]["NormalizationFitted"]===False&&report["FeynmanCheck"]["CompletedSamples"]>=report["FeynmanCheck"]["RequestedSamples"]]]],"all actual full parameter validations"];
provenance=<|"Kind"->"ImportOfActualPublicDefaultRun","OriginalCapsule"->"tests/results/SM-5-point/diagnostics/c4-default-eEZZh-20261112/construction.wxf",
 "OriginalCapsuleSHA256"->sourceSHA,"OriginalSummarySHA256"->sha[FileNameJoin[{sourceDirectory,"construction.json"}]],
 "ActualRunAudit"->"tests/results/SM-5-point/diagnostics/c4-default-eEZZh-20261112/actual-default-audit.json",
 "ActualRunAuditSHA256"->sha[FileNameJoin[{sourceDirectory,"actual-default-audit.json"}]],
 "ImportPreconditions"->"tests/results/SM-5-point/diagnostics/c4-default-eEZZh-20261112/canonical-import/preconditions.json",
 "ImportPreconditionsSHA256"->sha[FileNameJoin[{outputDirectory,"preconditions.json"}]],
 "ImportedActualDefaultRun"->True,"ConstructorRerunPerformed"->False,"ExpressionUnchanged"->True,
 "OriginalPrintsDetailsRowsAndTimingPreserved"->True,"ActualPublicOptionsSupplied"->source["PublicOptionsSupplied"],
 "OriginalRandomSeedOption"->"Automatic","ActualResolvedRandomSeed"->details["RandomSeed"],
 "OriginalFinalExpressionHeldSHA256"->heldSHA[source["Expression"]],"OriginalSeconds"->source["Seconds"],
 "FullParameterValidationGroups"->Length[reports],"FullParameterValidationRows"->Total[Length[#["Report"]["Checks"]]&/@reports]|>;
record=Join[source,<|"ID"->entry["ID"],"CanonicalRecord"->True,"RandomSeed"->details["RandomSeed"],"Complexity"->details["Complexity"],
 "ImportedActualDefaultRun"->True,"ConstructorRerunPerformed"->False,"ExpressionUnchanged"->True,"Provenance"->provenance|>];
outputFile=FileNameJoin[{outputDirectory,"canonical.wxf"}];
stream=OpenWrite[outputFile<>".tmp",BinaryFormat->True];BinaryWrite[stream,Normal[BinarySerialize[record,PerformanceGoal->"Size"]],"Byte"];Close[stream];RenameFile[outputFile<>".tmp",outputFile,OverwriteTarget->True];
readback=Import[outputFile,"WXF"];
preserved={"Expression","AmplitudePair","Prints","Details","Rows","Seconds","Particles","ContextSHA256"};
assert[And@@(SameQ[source[#],readback[#]]&/@preserved),"exact source field preservation after serialization"];
assert[heldSHA[readback["Expression"]]===provenance["OriginalFinalExpressionHeldSHA256"],"readback expression hash"];
summary=Join[KeyTake[record,{"ID","Particles","Status","Seconds","Complexity","RandomSeed","ContextSHA256","CallType","ImportedActualDefaultRun","ConstructorRerunPerformed","ExpressionUnchanged","Provenance"}],
 <|"Rows"->Length[rows],"FeynmanRows"->Count[Lookup[rows,"Type"],"Feynman"],"CheckStatuses"->Counts[Lookup[rows,"Status"]],"FailureStage"->Null,"CapsuleSHA256"->sha[outputFile]|>];
Export[FileNameJoin[{outputDirectory,"canonical.json"}],summary,"RawJSON"];
Export[FileNameJoin[{outputDirectory,"preparation-audit.json"}],<|"Status"->"VERIFIED_READY_FOR_CANONICAL_IMPORT","ID"->entry["ID"],"ContextSHA256"->record["ContextSHA256"],
 "SourceCapsuleSHA256"->sourceSHA,"PreparedCapsuleSHA256"->sha[outputFile],"PreparedSummarySHA256"->sha[FileNameJoin[{outputDirectory,"canonical.json"}]],
 "PreservedSourceFields"->preserved,"ExactSourceFieldPreservation"->True,"OriginalExpressionHeldSHA256"->heldSHA[source["Expression"]],
 "ReadbackExpressionHeldSHA256"->heldSHA[readback["Expression"]],"ActualParameterGroupPassRows"->(Length[#["Report"]["Checks"]]&/@reports),
 "ConstructorRerunPerformed"->False,"CanonicalWritten"->False|>,"RawJSON"];
Print["VERIFIED_READY_FOR_CANONICAL_IMPORT ",entry["ID"]," ",sha[outputFile]];
End[];Quit[];
