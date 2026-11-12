(* HEPCAT - High Energy Physics Constructive Amplitude Tool
   Copyright (C) 2026 Neil Christensen
   SPDX-License-Identifier: GPL-3.0-or-later *)

(* Execute one checkpointed SM five-point construction or numerical export. *)
Begin["SMFivePointRun`Private`"];
root=DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
configuration=Import[Environment["HEPCAT_SM5_CONFIGURATION"],"RawJSON"];
inventory=Import[FileNameJoin[{root,"tests","SM-5-point","inventory.json"}],"RawJSON"];
outputDirectory=FileNameJoin[{root,"tests","results","SM-5-point"}];
kind=Environment["HEPCAT_SM5_KIND"];id=Environment["HEPCAT_SM5_ID"];
seed=FromDigits[Environment["HEPCAT_SM5_SEED"]];
  admissionCommand={"python3",FileNameJoin[{root,"tests","SM-5-point","stage_context.py"}],
    "--export-inputs",id,"--configuration",Environment["HEPCAT_SM5_CONFIGURATION"]};
  readAdmission[]:=Module[{command=RunProcess[admissionCommand],value},
    value=Quiet[Check[ImportString[command["StandardOutput"],"RawJSON"],$Failed]];
    If[command["ExitCode"]===0&&AssociationQ[value]&&Lookup[value,"Status",None]==="APPROVED",value,$Failed]];
launchAdmission=If[kind==="export",readAdmission[],None];
If[kind==="export"&&!AssociationQ[launchAdmission],Print["Export startup admission failed ",id];Quit[2]];
Get[FileNameJoin[{root,"source","HEPCAT.wl"}]];
(* Keep startup controls ahead of package helper names on the search path. *)
$ContextPath=Prepend[DeleteCases[$ContextPath,"SMFivePointRun`Private`"],"SMFivePointRun`Private`"];
$HistoryLength=0;
model=ReadModel[FileNameJoin[{root,"models"}],1];
factorizationModel=ReadModel[FileNameJoin[{root,"models"}],2];
feynmanModel=ReadModel[FileNameJoin[{root,"tests","reference-models","calchep"}],1];
writeRecord[file_,value_]:=Module[{temporary=file<>".tmp",stream,bytes},
 bytes=BinarySerialize[value,PerformanceGoal->"Size"];
 stream=OpenWrite[temporary,BinaryFormat->True];BinaryWrite[stream,Normal[bytes],"Byte"];Close[stream];
 RenameFile[temporary,file,OverwriteTarget->True]];
printed={};started=AbsoluteTime[];
liveFile=FileNameJoin[{outputDirectory,id<>If[kind==="export","-export",""]<>".live.txt"}];
live=OpenWrite[liveFile];
capture[body_]:=Internal`InheritedBlock[{Print},
 Unprotect[Print];Print[]:=Null;
 Print[args__]:=(AppendTo[printed,If[Length[{args}]===1,First[{args}],SequenceForm[args]]];
   WriteString[live,DateString[{"Hour",":","Minute",":","Second"}]," ",
    ToString[Last[printed],OutputForm],"\n"];Null);
 Protect[Print];body];
SetAttributes[capture,HoldAll];
If[kind==="construct",
 entry=SelectFirst[inventory["Cases"],#["ID"]===id&];
 process=entry["Particles"];
 details=capture[MemoryConstrained[
  Block[{ConstructiveDiagrams`Private`$amplitudeParallelActive=True},
   ConstructAmplitude[process,model,factorizationModel,"FeynmanModel"->feynmanModel,
     "RandomSeed"->seed,"Output"->"Details"]],
  configuration["MemoryGiB"]2^30,Failure["ConstructionMemoryLimit",<||>]]];
 tables=Cases[printed,_Dataset,{1}];rows=Flatten[Normal/@tables,1];
 expression=If[AssociationQ[details],Lookup[details,"Amplitude",Missing["NotValidated"]],details];
 ok=AssociationQ[details]&&Lookup[details,"Status",None]==="COMPLETE"&&
   !MissingQ[expression]&&!FailureQ[expression]&&
   rows=!={}&&AllTrue[rows,Lookup[#,"Status",None]==="PASS"&]&&
   AnyTrue[rows,Lookup[#,"Type",None]==="Feynman"&];
 record=<|"ContextSHA256"->configuration["ContextSHA256"],"ID"->id,"Particles"->process,"Expression"->expression,"Prints"->printed,"Rows"->rows,
   "Status"->If[ok,"COMPLETE","FAIL"],"Seconds"->N[AbsoluteTime[]-started],
   "Details"->details,"RandomSeed"->seed,
   "Complexity"->If[AssociationQ[details],Lookup[details,"Complexity",Missing["NotMeasured"]],Missing["NotMeasured"]]|>;
 target=FileNameJoin[{outputDirectory,id<>".wxf"}],
 If[kind==="export",
  family=SelectFirst[inventory["Families"],#["ID"]===id&];
  admission=readAdmission[];
  If[!AssociationQ[admission]||admission=!=launchAdmission,Print["Native export inputs changed during package load ",id];Quit[2]];
  entries=Import[FileNameJoin[{outputDirectory,#<>".wxf"}],"WXF"]&/@family["CaseIDs"];
  If[!AllTrue[entries,Lookup[#,"Status",None]==="COMPLETE"&],Print["Incomplete family ",id];Quit[2]];
  exportName=Lookup[family,"ExportName",StringJoin[StringReplace[family["Particles"],{"W+"->"Wp","W-"->"Wm"}]]];
  result=capture[WriteSPINASProcess[
   FileNameJoin[{root,"tests","results","spinas-output","SM-5-point",exportName}],
   family["Incoming"]->family["Outgoing"],
   ({#["Particles"],#["Expression"]}&/@entries),model,
   "ProcessName"->("sm5"<>exportName),
   "SPINASDirectory"->configuration["SPINASDirectory"],
   "Build"->True,"RunExamples"->True,"Overwrite"->True,
   "CalcHEPDirectory"->configuration["CalcHEPDirectory"],
   "CalcHEPModel"->{FileNameJoin[{root,"tests","reference-models","calchep"}],1},
   "CalcHEPPoints"->configuration["Points"],"RunCalcHEPValidation"->True,
   "MadGraphDirectory"->configuration["MadGraphDirectory"],
   "MadGraphModel"->FileNameJoin[{root,"tests","reference-models","madgraph","sm"}],
   "MadGraphParameterRules"->{"lamWS"->0},
   "MadGraphPoints"->configuration["Points"],"RunMadGraphValidation"->True,"ReturnDetails"->True]];
  If[readAdmission[]=!=admission,Print["Export context or native inputs changed ",id];Quit[2]];
  ok=AssociationQ[result]&&Lookup[Lookup[result,"CalcHEPValidation",<||>],"Status",None]==="PASS"&&
    Lookup[Lookup[result,"MadGraphValidation",<||>],"Status",None]==="PASS";
  record=<|"ContextSHA256"->configuration["ContextSHA256"],"FamilyID"->id,"Incoming"->family["Incoming"],"Outgoing"->family["Outgoing"],
   "Prints"->printed,"Result"->result,"Status"->If[ok,"PASS","FAIL"],
   "Seconds"->N[AbsoluteTime[]-started],"Points"->configuration["Points"],
   "ExporterContextSHA256"->admission["ExporterContextSHA256"],
   "NativeCaseContexts"->admission["NativeCaseContexts"],
   "CaseIDs"->admission["CaseIDs"],"CaseCapsulesSHA256"->admission["CaseCapsulesSHA256"],
   "NativeContextCompatibility"->Lookup[admission,"NativeContextCompatibility",Null]|>;
  target=FileNameJoin[{outputDirectory,id<>"-export.wxf"}],
  Print["Unknown work kind ",kind];Quit[2]]];
Close[live];
writeRecord[target,record];
summary=KeyTake[record,{"ID","FamilyID","Particles","Status","Seconds","Complexity","RandomSeed","Points","ContextSHA256","ExporterContextSHA256","NativeCaseContexts","NativeContextCompatibility"}];
If[kind==="construct",AssociateTo[summary,{"Rows"->Length[rows],"FeynmanRows"->Count[Lookup[rows,"Type",{}],"Feynman"],"CheckStatuses"->Counts[Lookup[rows,"Status",{}]],
 "FailureStage"->If[AssociationQ[details]&&FailureQ[Lookup[details,"Failure",None]],Lookup[details["Failure"][[2]],"Stage",Null],Null]}]];
If[kind==="export",
 partial=If[FailureQ[result],Lookup[result[[2]],"ExportResult",<||>],result];
 backendStatus[value_]:=If[FailureQ[value],"FAIL",If[AssociationQ[value],Lookup[value,"Status","NOT RUN"],"NOT RUN"]];
 AssociateTo[summary,{"CaseIDs"->family["CaseIDs"],
 "CaseCapsulesSHA256"->admission["CaseCapsulesSHA256"],
 "CalcHEPStatus"->backendStatus[If[AssociationQ[partial],Lookup[partial,"CalcHEPValidation",None],None]],
 "MadGraphStatus"->backendStatus[If[AssociationQ[partial],Lookup[partial,"MadGraphValidation",None],None]]}]];
AssociateTo[summary,"CapsuleSHA256"->IntegerString[FileHash[target,"SHA256"],16,64]];
summary=summary/._Missing->Null;
summaryFile=StringReplace[target,".wxf"->".json"];
summaryWrite=Quiet[Check[Export[summaryFile<>".tmp",summary,"RawJSON"],$Failed]];
If[!StringQ[summaryWrite]||!FileExistsQ[summaryFile<>".tmp"],
 Print["Checkpoint summary JSON could not be written; preserved WXF: ",target];Quit[2]];
summaryCommit=Quiet[Check[RenameFile[summaryFile<>".tmp",summaryFile,OverwriteTarget->True],$Failed]];
If[!StringQ[summaryCommit]||!FileExistsQ[summaryFile],
 Print["Checkpoint summary JSON could not be committed; preserved WXF: ",target];Quit[2]];
Print[summary];
End[];
Quit[If[TrueQ[SMFivePointRun`Private`ok],0,1]];
