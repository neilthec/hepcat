root="/Users/neil/code/hepcat";base=root<>"/tests/results/SM-5-point";
inventory=Import[root<>"/tests/SM-5-point/inventory.json","RawJSON"];
expectedContext="47026bedc2844622616e46a05069c2682cc4306872799bbe8e02d4a01291cc04";
work={{"sm5_756b05ecded5","c6-default-neutral-chiral-20261113"},{"sm5_268bfd2334c3","c6-default-charged-chiral-20261113"}};
audits={};
Do[
 id=First[item];d=base<>"/diagnostics/"<>Last[item];input=d<>"/construction.wxf";
 r=Import[input,"WXF"];detail=r["Details"];entry=SelectFirst[inventory["Cases"],#["ID"]===id&];
 stages=Lookup[detail,"StageTimings",{}];
 validationStages=Select[stages,StringStartsQ[Lookup[#,"Stage",""],"Validating certified covariant source representative ("]&];
 If[r["Status"]=!="COMPLETE"||r["CallType"]=!="ActualPublicDefaultC6"||r["Particles"]=!=entry["Particles"]||
   r["ContextSHA256"]=!=expectedContext||r["Expression"]=!=detail["Amplitude"]||r["AmplitudePair"]=!=detail["AmplitudePair"]||
   Length[validationStages]=!=3||!AllTrue[validationStages,#["Status"]==="COMPLETE"&]||
   r["Rows"]==={}||!AllTrue[r["Rows"],Lookup[#,"Status",None]==="PASS"&]||!IntegerQ[detail["RandomSeed"]]||
   !AnyTrue[r["Rows"],Lookup[#,"Type",None]==="Feynman"&],Print["IMPORT_REJECTED ",id];Quit[2]];
 If[FileExistsQ[base<>"/"<>id<>".json"]||FileExistsQ[base<>"/"<>id<>".wxf"],Print["REFUSING_EXISTING ",id];Quit[2]];
 record=Join[r,<|"ID"->id,"RandomSeed"->detail["RandomSeed"],"Complexity"->detail["Complexity"],
 "CanonicalImport"-><|"Status"->"EXACT_DEFAULT_CONSTRUCTION_ADOPTED","ConstructorRerun"->False,
  "OriginalDiagnosticCapsule"->"tests/results/SM-5-point/diagnostics/"<>Last[item]<>"/construction.wxf",
  "OriginalDiagnosticCapsuleSHA256"->IntegerString[FileHash[input,"SHA256"],16,64],
  "OriginalCallRandomSeedWasAutomatic"->True,"PublicOptionsSupplied"->r["PublicOptionsSupplied"]|>|>];
 target=base<>"/"<>id<>".wxf";temporary=target<>".tmp";
 stream=OpenWrite[temporary,BinaryFormat->True];BinaryWrite[stream,Normal[BinarySerialize[record,PerformanceGoal->"Size"]],"Byte"];Close[stream];
 saved=Import[temporary,"WXF"];proof=Association[Map[#->SameQ[saved[#],r[#]]&,{"Expression","AmplitudePair","Details","Prints","Rows","Seconds","ContextSHA256","ResolvedRandomSeed"}]];
 If[!And@@Values[proof],Print[proof];Quit[2]];RenameFile[temporary,target,OverwriteTarget->True];
 summary=KeyTake[record,{"ID","Particles","Status","Seconds","Complexity","RandomSeed","ContextSHA256","CallType","PublicOptionsSupplied","CanonicalImport"}];
 AssociateTo[summary,{"Rows"->Length[record["Rows"]],"FeynmanRows"->Count[Lookup[record["Rows"],"Type"],"Feynman"],"CheckStatuses"->Counts[Lookup[record["Rows"],"Status"]],"FailureStage"->Null,"CapsuleSHA256"->IntegerString[FileHash[target,"SHA256"],16,64]}];
 Export[base<>"/"<>id<>".json.tmp",summary,"RawJSON"];RenameFile[base<>"/"<>id<>".json.tmp",base<>"/"<>id<>".json",OverwriteTarget->True];
 CopyFile[d<>"/construct.log",base<>"/"<>id<>".log"];CopyFile[d<>"/construct.live.txt",base<>"/"<>id<>".live.txt"];
 AppendTo[audits,<|"Status"->"EXACT_TYPED_IMPORT_VERIFIED","ID"->id,"ConstructorRerun"->False,"SameQFields"->proof,"OriginalCapsuleSHA256"->IntegerString[FileHash[input,"SHA256"],16,64],"CanonicalCapsuleSHA256"->summary["CapsuleSHA256"],"RandomSeed"->record["RandomSeed"],"ContextSHA256"->expectedContext|>],{item,work}];
 Export[root<>"/tests/results/SM-5-point/diagnostics/c6-public-chiral-canonical-import-20261113/audit.json",audits,"RawJSON"];Print[audits];Quit[];
