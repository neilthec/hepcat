Global`$HEPCATpath="/Users/neil/code/hepcat/tests/results/SM-5-point/diagnostics/colored-four-fermion-current-source-20261113/production-candidate/shadow/source";
Get[FileNameJoin[{Global`$HEPCATpath,"HEPCAT.wl"}]];
Begin["HEPCATColoredCurrentPublicShadow`Private`"];$ContextPath=Prepend[DeleteCases[$ContextPath,"HEPCATColoredCurrentPublicShadow`Private`"],"HEPCATColoredCurrentPublicShadow`Private`"];
$HistoryLength=0;psRoot="/Users/neil/code/hepcat";psD=DirectoryName[$InputFileName];psIndex=FromDigits[Environment["HEPCAT_COLOR_SHADOW_INDEX"]];psOut=psD<>"/public-"<>ToString[psIndex];If[!DirectoryQ[psOut],CreateDirectory[psOut]];
psProcess=Switch[psIndex,1,{"u","U","c","C","h"},2,{"u","U","u","U","h"},3,{"e","E","u","U","h"},4,{"u","D","C","s","h"},_,Quit[2]];
psModel=ReadModel[psRoot<>"/models",1];psLower=ReadModel[psRoot<>"/models",2];psFeynman=ReadModel[psRoot<>"/tests/reference-models/calchep",1];psStarted=AbsoluteTime[];psPrints={};psLive=OpenWrite[psOut<>"/live.txt"];
SetAttributes[psCapture,HoldAll];psCapture[body_]:=Internal`InheritedBlock[{Print},Unprotect[Print];Print[]:=Null;Print[args__]:=(AppendTo[psPrints,If[Length[{args}]===1,First[{args}],SequenceForm[args]]];WriteString[psLive,DateString[{"Hour",":","Minute",":","Second"}]," ",ToString[Last[psPrints],OutputForm],"\n"];FlushWrite[psLive];Null);Protect[Print];body];
psDetails=psCapture[MemoryConstrained[TimeConstrained[Block[{ConstructiveDiagrams`Private`$amplitudeParallelActive=True},ConstructAmplitude[psProcess,psModel,psLower,"FeynmanModel"->psFeynman,"Output"->"Details"]],1200,Failure["ExternalShadowDeadline",<||>]],4 2^30,Failure["ExternalShadowMemory",<||>]]];Close[psLive];
psRows=If[AssociationQ[psDetails],Lookup[Lookup[psDetails,"Checks",<||>],"Checks",{}],{}];psAttempt=If[AssociationQ[psDetails],Lookup[psDetails,"ColorCurrentAttempt",<||>],<||>];psValidation=Lookup[psAttempt,"Validation",<||>];psReports=Lookup[psValidation,"ValidationReports",{}];
psRank=If[AssociationQ[psDetails],Length[Lookup[psDetails,"ColorBasis",{}]],0];
psComplete=AssociationQ[psDetails]&&Lookup[psDetails,"Status",None]==="COMPLETE"&&psRows=!={}&&AllTrue[psRows,Lookup[#,"Status",None]==="PASS"&]&&Count[Lookup[psRows,"Type"],"Feynman"]===psRank&&
 Lookup[psAttempt,"Selected",False]===True&&Lookup[psValidation,"Status",None]==="COMPLETE"&&Length[psReports]===3psRank&&Length[DeleteDuplicates[Lookup[psReports,"ParameterRules"]]]===3&&
 AllTrue[psReports,AssociationQ[Lookup[#,"Report",None]]&&AllTrue[#["Report"]["Checks"],Lookup[#,"Status",None]==="PASS"&]&];
psRecord=<|"Status"->If[psComplete,"COMPLETE","NOT_COMPLETE"],"CallType"->"ActualPublicDefaultFullShadow","Process"->psProcess,"Details"->psDetails,"Rows"->psRows,"Prints"->psPrints,
 "AmplitudePair"->If[psComplete,psDetails["AmplitudePair"],Missing["NotValidated"]],"Expression"->If[psComplete,psDetails["Amplitude"],Missing["NotValidated"]],
 "Seconds"->N[AbsoluteTime[]-psStarted],"PublicOptionsSupplied"->{"FeynmanModel","Output"},"PublicRandomSeedRequested"->"Automatic","RecordedAmplitudeInputs"->False,
 "SingleKernelScopedExecutionEnvironment"->True,"InstalledProductionChanged"->False,"InstalledProductionContextSHA256"->"48d82ad2adaf29f9a338c9fb6b9cf3da37ed7570f6de5852ee50c798fd0d8363",
 "ProspectiveShadowContextSHA256"->Environment["HEPCAT_COLOR_SHADOW_CONTEXT"],"CanonicalWritten"->False|>;
psFile=psOut<>"/result.wxf";psStream=OpenWrite[psFile,BinaryFormat->True];BinaryWrite[psStream,Normal[BinarySerialize[psRecord,PerformanceGoal->"Size"]],"Byte"];Close[psStream];
psSummary=Join[KeyDrop[psRecord,{"Details","Rows","Prints","AmplitudePair","Expression"}],<|"Rows"->Length[psRows],"StatusCounts"->Counts[Lookup[psRows,"Status",{}]],"Rank"->psRank,"ParameterGroupReports"->Length[psReports],"SourceSelected"->Lookup[psAttempt,"Selected",False],"Complexity"->If[AssociationQ[psDetails],Lookup[psDetails,"Complexity",Null],Null],"CapsuleSHA256"->IntegerString[FileHash[psFile,"SHA256"],16,64]|>];
Export[psOut<>"/result.json",psSummary/.None->Null/._Missing->Null,"RawJSON"];Print[psSummary];End[];Quit[If[psComplete,0,1]];
