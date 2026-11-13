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
(* Public-call captures may spell the exact all-incoming list Process.  Keep
   the source WXF byte-for-byte and verify both spellings against inventory. *)
recordParticlesForExport[checkpoint_,expected_]:=Module[{hasParticles,hasProcess,particles,process},
 If[!AssociationQ[checkpoint]||!ListQ[expected]||expected==={}||!AllTrue[expected,StringQ],Return[$Failed]];
 hasParticles=KeyExistsQ[checkpoint,"Particles"];hasProcess=KeyExistsQ[checkpoint,"Process"];
 If[!hasParticles&&!hasProcess,Return[$Failed]];
 particles=Lookup[checkpoint,"Particles",Missing["Absent"]];process=Lookup[checkpoint,"Process",Missing["Absent"]];
 If[hasParticles&&(!ListQ[particles]||particles=!=expected),Return[$Failed]];
 If[hasProcess&&(!ListQ[process]||process=!=expected),Return[$Failed]];
 If[hasParticles,particles,process]];
(* Project exact export inputs before WXF Import; saved solver diagnostics are never loaded. *)
exportInputProjections={};
projectNativeForExport[caseID_String,expected_List,approved_Association]:=Module[
 {source,expectedSHA,expectedContext,parent,directory,command,response,metadata,payload,payloadSHA,checkpoint,before,after},
 source=FileNameJoin[{outputDirectory,caseID<>".wxf"}];
 expectedSHA=Lookup[Lookup[approved,"CaseCapsulesSHA256",<||>],caseID,None];
 expectedContext=Lookup[Lookup[approved,"NativeCaseContexts",<||>],caseID,None];
 before=Quiet[Check[IntegerString[FileHash[source,"SHA256"],16,64],$Failed]];
 If[!StringQ[expectedSHA]||before=!=expectedSHA,Return[$Failed]];
 parent=FileNameJoin[{outputDirectory,"temporary","export-inputs"}];
 If[!DirectoryQ[parent],CreateDirectory[parent,CreateIntermediateDirectories->True]];
 directory=FileNameJoin[{parent,caseID<>"-"<>CreateUUID[]}];
 Internal`WithLocalSettings[Null,Catch[
 command={"python3",FileNameJoin[{root,"tests","SM-5-point","project-export-inputs.py"}],
  "--capsule",source,"--output",directory,"--expected-sha",expectedSHA,"--case-id",caseID,
  "--particles-json",ExportString[expected,"RawJSON"]};
 If[StringQ[expectedContext],command=Join[command,{"--context",expectedContext}]];
 response=RunProcess[command];
 metadata=Quiet[Check[ImportString[Lookup[response,"StandardOutput",""],"RawJSON"],$Failed]];
 If[!AssociationQ[response]||Lookup[response,"ExitCode",1]=!=0||!AssociationQ[metadata]||
   Lookup[metadata,"Status",None]=!="PROJECTED"||Lookup[metadata,"CaseID",None]=!=caseID||
   Lookup[metadata,"SourceSHA256",None]=!=expectedSHA||Lookup[metadata,"Particles",None]=!=expected||
   !TrueQ[Lookup[metadata,"SourceUnchanged",False]]||
   Lookup[metadata,"FullNativeCheckpointImported",True]=!=False||
   Lookup[metadata,"ExpressionsEvaluated",True]=!=False,
  Print["Native export input projection unavailable ",caseID," ",metadata];Throw[$Failed,"ExportInputProjection"]];
 payload=FileNameJoin[{directory,"input.wxf"}];payloadSHA=Lookup[metadata,"PayloadSHA256",None];
 If[Lookup[metadata,"Projection",None]=!=payload||!StringQ[payloadSHA]||
   Quiet[Check[IntegerString[FileHash[payload,"SHA256"],16,64],$Failed]]=!=payloadSHA,Throw[$Failed,"ExportInputProjection"]];
 checkpoint=Quiet[Check[Import[payload,"WXF"],$Failed]];
 after=Quiet[Check[IntegerString[FileHash[source,"SHA256"],16,64],$Failed]];
 If[after=!=expectedSHA||Quiet[Check[IntegerString[FileHash[payload,"SHA256"],16,64],$Failed]]=!=payloadSHA||
   !AssociationQ[checkpoint]||Lookup[checkpoint,"Status",None]=!="COMPLETE"||
   !KeyExistsQ[checkpoint,"Expression"]||recordParticlesForExport[checkpoint,expected]=!=expected,Throw[$Failed,"ExportInputProjection"]];
 AppendTo[exportInputProjections,KeyTake[metadata,{"CaseID","SourceSHA256","PayloadSHA256","CopiedFields",
  "ProjectorSHA256","ParserSourceSHA256","Limits","FullNativeCheckpointImported","ExpressionsEvaluated"}]];
 checkpoint,"ExportInputProjection"],
  If[DirectoryQ[directory],Quiet[DeleteDirectory[directory,DeleteContents->True]]]]];
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
  expectedProcesses=Function[caseID,Lookup[SelectFirst[inventory["Cases"],Lookup[#,"ID",None]===caseID&,<||>],"Particles",Missing["AbsentInventoryCase"]]]/@family["CaseIDs"];
  If[!AllTrue[expectedProcesses,ListQ],Print["Missing inventory external particles ",id];Quit[2]];
  entries=MapThread[projectNativeForExport[#1,#2,admission]&,{family["CaseIDs"],expectedProcesses}];
  If[!AllTrue[entries,AssociationQ[#]&&Lookup[#,"Status",None]==="COMPLETE"&],Print["Native record external particles disagree with inventory or projection unavailable ",id];Quit[2]];
  If[readAdmission[]=!=admission,Print["Native export inputs changed during bounded projection ",id];Quit[2]];
  entryProcesses=MapThread[recordParticlesForExport,{entries,expectedProcesses}];
  If[!AllTrue[entryProcesses,ListQ],Print["Native record external particles disagree with inventory ",id];Quit[2]];
  exportName=Lookup[family,"ExportName",StringJoin[StringReplace[family["Particles"],{"W+"->"Wp","W-"->"Wm"}]]];
  result=capture[WriteSPINASProcess[
   FileNameJoin[{root,"tests","results","spinas-output","SM-5-point",exportName}],
   family["Incoming"]->family["Outgoing"],
   MapThread[{#1,#2["Expression"]}&,{entryProcesses,entries}],model,
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
   "NativeContextCompatibility"->Lookup[admission,"NativeContextCompatibility",Null],
   "ExportInputProjections"->exportInputProjections|>;
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
