Begin["ZZZZhSourceMetadata`Private`"]; $ContextPath={"System`"};$HistoryLength=0;
root="/Users/neil/code/hepcat";dir=FileNameJoin[{root,"tests","results","SM-5-point","diagnostics","ZZZZh-C5-source-validation"}];
path=FileNameJoin[{root,"tests","results","SM-5-point","sm5_f6c76904211c.wxf"}];
MemoryConstrained[TimeConstrained[
 record=Import[path,"WXF"];details=record["Details"];stages=details["Stages"];
 attempts=details["RepresentativeSelectionAttempts"];
 pointAttempt=SelectFirst[attempts,Lookup[#,"SourceStage",""]==="Certified covariant source"&,<||>];
 reports=Lookup[pointAttempt,"ValidationReports",{}];
 rowDetails[row_]:=Join[KeyTake[row,{"Type","Status","Method","Diagnostics","CompletedSamples","RequestedSamples","ObservedPower","ExpectedPower","ElapsedTime","CheckExecution"}],
 <|"SampleKeys"->(If[AssociationQ[#],Keys[#],{}]&/@Lookup[row,"Samples",{}]),"SampleDiagnostics"->(KeyTake[#,{"Status","Diagnostics","MatchingSigns","Method","ElapsedTime","KinematicFailure"}]&/@Select[Lookup[row,"Samples",{}],AssociationQ])|>];
 summarized=Map[Function[item,With[{report=item["Report"]},<|"Summary"->Lookup[report,"Summary",<||>],
 "PerTypeStatusCounts"->GroupBy[report["Checks"],Lookup[#,"Type",""]&,Counts[Lookup[#,"Status"]]&],
 "Checks"->(rowDetails/@report["Checks"]),"Execution"->Lookup[report,"Execution",<||>],
 "FeynmanKeys"->Keys[Lookup[report,"FeynmanCheck",<||>]],"HighEnergyKeys"->Keys[Lookup[report,"HighEnergyCheck",<||>]]|>]],reports];
 proposal=stages["Covariant source proposal"];preparation=stages["Preparing certified covariant source representative details"];
 benchmark=stages["Preparing Feynman amplitudes"];
 summary=<|"Status"->"METADATA_ONLY","SourceContext"->record["ContextSHA256"],"CapsuleSHA256"->IntegerString[FileHash[path,"SHA256"],16,64],
 "NativeStatus"->record["Status"],"FinalExpressionLeaves"->LeafCount[record["Expression"]],"ConstructionSeconds"->record["Seconds"],
 "SourceProposalKeys"->Keys[proposal],"RawSourceLeaves"->If[KeyExistsQ[proposal,"RawExpression"],LeafCount[proposal["RawExpression"]],Missing["NotSupplied"]],
 "SourceExpressionLeaves"->LeafCount[proposal["Expression"]],"PreparedSourceLeaves"->LeafCount[Last[First[preparation["PreparedAmplitudes"]]]],
 "PreparationInputLeaves"->LeafCount[preparation["OriginalAmplitudes"]],"PreparationReportKeys"->Keys[preparation],
 "PreparedSourceOptions"->KeyTake[preparation["Options"],{"InternalOnShell","PolePreference","MaxSteps","TimeConstraint"}],
 "RetainedBenchmarkNote"->"The stage key is overwritten when the ordinary fallback prepares its benchmark again; timings retain both calls.",
 "FeynmanInputDiagrams"->Length[benchmark["OriginalAmplitudes"]],"FeynmanInputLeaves"->LeafCount[benchmark["OriginalAmplitudes"]],
 "FeynmanPreparedLeaves"->LeafCount[benchmark["PreparedAmplitudes"]],
 "StageTimings"->details["StageTimings"],"SourceSelected"->Lookup[pointAttempt,"Selected",None],"ValidationReports"->summarized|>;
 output=FileNameJoin[{dir,"metadata.wxf"}];stream=OpenWrite[output,BinaryFormat->True];BinaryWrite[stream,Normal[BinarySerialize[summary,PerformanceGoal->"Size"]],"Byte"];Close[stream];
 json=summary/. x_Missing:>ToString[x,InputForm]/. x_Failure:>ToString[x,InputForm];
 Export[FileNameJoin[{dir,"metadata.json"}],json,"RawJSON"];Print[InputForm[KeyTake[summary,{"NativeStatus","SourceSelected","RawSourceLeaves","SourceExpressionLeaves","PreparedSourceLeaves","FeynmanInputLeaves","FeynmanPreparedLeaves"}]]];
 Print["CHECK GROUPS ",InputForm[KeyTake[#,{"Summary","PerTypeStatusCounts"}]&/@summarized]],30,Print["METADATA TIMEOUT"];Quit[124]],1024^3,Print["METADATA MEMORY LIMIT"];Quit[137]];Quit[];
