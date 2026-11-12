(* HEPCAT - High Energy Physics Constructive Amplitude Tool
   Copyright (C) 2026 Neil Christensen
   SPDX-License-Identifier: GPL-3.0-or-later *)

(* Focused campaign-prototype controls only. No amplitude or ledger promotion. *)
gtcRoot=DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
gtcSHA[path_String]:=IntegerString[FileHash[path,"SHA256"],16,64];
gtcGuardFiles=Join[Sort[FileNames["*.wl",gtcRoot<>"/source",Infinity]],
 Flatten[Table[gtcRoot<>"/models/"<>stem<>ToString[number]<>".mdl",
   {number,{1,2}},{stem,{"amps","prtcls","vars","func","extlib"}}]],
 Sort[FileNames["*1.mdl",gtcRoot<>"/tests/reference-models/calchep"]],
 {gtcRoot<>"/tests/SM-5-point/generation-transport.wl"}];
gtcObservedHashes[]:=Association[(StringDrop[#,StringLength[gtcRoot]+1]->gtcSHA[#])&/@gtcGuardFiles];
gtcBeforeHashes=gtcObservedHashes[];
Get[FileNameJoin[{gtcRoot,"source","HEPCAT.wl"}]];
Get[FileNameJoin[{gtcRoot,"tests","SM-5-point","generation-transport.wl"}]];
gtcOut=CreateDirectory[];
$HistoryLength=0;gtcStart=AbsoluteTime[];gtcRows={};gtcNative={};
gtcSHA[path_String]:=IntegerString[FileHash[path,"SHA256"],16,64];
gtcCheck[name_String,condition_]:=(AppendTo[gtcRows,<|"Test"->name,"Status"->If[TrueQ[condition],"PASS","FAIL"]|>];Print[Last[gtcRows]]);
gtcSave[]:=Module[{record,summary},
 record=<|"Scope"->"CampaignPrototypeStrictPhaseTransportControlsNotCampaignValidation",
  "SourceModelSHA256"->gtcSHA[gtcRoot<>"/source/HEPCAT-Model.wl"],
  "ElementaryModelSHA256"->gtcSHA[gtcRoot<>"/models/amps1.mdl"],
  "CompleteModelSHA256"->gtcSHA[gtcRoot<>"/models/amps2.mdl"],
  "PrototypeSHA256"->gtcSHA[gtcRoot<>"/tests/SM-5-point/generation-transport.wl"],
  "ScriptSHA256"->gtcSHA[$InputFileName],"Checks"->gtcRows,"NativeControls"->gtcNative,
  "Seconds"->N[AbsoluteTime[]-gtcStart],
  "ObservedFileHashesBefore"->gtcBeforeHashes,"ObservedFileHashesAfter"->gtcObservedHashes[],
  "SourceOrModelChanged"->(gtcBeforeHashes=!=gtcObservedHashes[]),
  "AmplitudePromoted"->False,"NumericalPhaseFitted"->False|>;
 Export[gtcOut<>"/control.wxf",record,"WXF"];
 summary=KeyDrop[record,{"NativeControls"}];
 summary["NativeControls"]=Map[KeyTake[#, {"SourceParticles","TargetParticles","FermionReferenceSign","Status","Seconds","Rows","FeynmanMatchingSigns","FeynmanCompletedSamples"}]&,gtcNative];
 Export[gtcOut<>"/summary.json",summary,"RawJSON"]];
gtcElementary=ReadModel[gtcRoot<>"/models",1];gtcFull=ReadModel[gtcRoot<>"/models",2];
gtcFeynman=ReadModel[gtcRoot<>"/tests/reference-models/calchep",1];
gtcInventory=Import[gtcRoot<>"/tests/SM-5-point/inventory.json","RawJSON"];
gtcMaps=gtcInventory["FlavorMaps"];
gtcCertificates=SMFivePointGenerationTransport`Private`generationMapCertificate[gtcElementary,#,{gtcRoot<>"/models",1}]&/@gtcMaps;
gtcCheck["all36symbolic elementary generation maps certify exact vertices and roles",
 Length[gtcCertificates]===36&&AllTrue[gtcCertificates,AssociationQ[#]&&#["Status"]==="EXACT_ROW_MATCH"&]];
gtcIdentity=SelectFirst[gtcMaps,Lookup[#["ParticleMap"],{"e","u"}]==={"e","u"}&&
 Lookup[#["ParticleMap"],{"m","c"}]==={"m","c"}&];
gtcBothSwap=SelectFirst[gtcMaps,Lookup[#["ParticleMap"],{"e","u"}]==={"m","c"}&&
 Lookup[#["ParticleMap"],{"m","c"}]==={"e","u"}&];
gtcSource={"e","E","m","M"};gtcImage={2,3,4,1};gtcTarget=ConstantArray["",4];gtcTarget[[gtcImage]]=gtcSource;
gtcToy={{Mass[1]->ModelParameter[gtcFull,"Me"],Mass[2]->ModelParameter[gtcFull,"Me"],
 Mass[3]->ModelParameter[gtcFull,"Mm"],Mass[4]->ModelParameter[gtcFull,"Mm"]},
 7 ColorGenerator[ColorIndex[1],ColorIndex[4],ConstructiveDiagrams`Private`colorInternalIndex[{8,9},8]]
 SpinorChain[Spinor["Spin","Upper","Angle",1,VectorSpinIndex[1,1]],Mom[Multiparticle[2,4]],
 Spinor["Spin","Square",3]]/PropDen[Mom[Multiparticle[1,2]],ModelParameter[gtcFull,"Me"]]};
gtcToyForward=SMFivePointGenerationTransport`Private`generationTransportCandidate[gtcToy,gtcSource,gtcTarget,gtcFull,gtcIdentity,gtcImage,gtcElementary,{gtcRoot<>"/models",1}];
gtcToyBack=SMFivePointGenerationTransport`Private`generationTransportCandidate[gtcToyForward["Amplitude"],gtcTarget,gtcSource,gtcFull,gtcIdentity,Ordering[gtcImage],gtcElementary,{gtcRoot<>"/models",1}];
gtcCheck["typed noninvolutive transport preserves masses spin slots color dummies and coefficients on inverse",
 gtcToyBack["Amplitude"]===gtcToy&&gtcToyForward["NativeValidationPerformed"]===False&&gtcToyForward["Status"]==="CANDIDATE"];
gtcCheck["invalid external slot maps are rejected without numerical phase fitting",
 FailureQ[SMFivePointGenerationTransport`Private`generationTransportCandidate[gtcToy,gtcSource,gtcTarget,gtcFull,gtcIdentity,{1,1,3,4},gtcElementary,{gtcRoot<>"/models",1}]]];
gtcCheck["an omitted certificate source cannot return a transported candidate",
 FailureQ[SMFivePointGenerationTransport`Private`generationTransportCandidate[gtcToy,gtcSource,gtcTarget,gtcFull,gtcIdentity,gtcImage]]];
gtcCorruptMap=Join[gtcIdentity,<|"MassMap"->Join[gtcIdentity["MassMap"],<|"Me"->"Mm"|>]|>];
gtcCheck["a corrupted mass map cannot use the original exact vertex certificate",
 FailureQ[SMFivePointGenerationTransport`Private`generationTransportCandidate[gtcToy,gtcSource,gtcTarget,gtcFull,gtcCorruptMap,gtcImage,gtcElementary,{gtcRoot<>"/models",1}]]];
gtcCheck["candidate provenance binds the full map and exact elementary model sources",
 AssociationQ[gtcToyForward["GenerationCertificate"]] &&
 gtcToyForward["GenerationCertificate"]["FullGenerationMapSHA256"]===IntegerString[Hash[gtcIdentity,"SHA256"],16,64] &&
 Length[gtcToyForward["GenerationCertificate"]["ElementarySourceFilesSHA256"]]===5 &&
 StringQ[gtcToyForward["GenerationCertificate"]["ElementarySourceSHA256"]]];
gtcRules={"SW"->3/5,"EE"->1/3,"GG"->2/5,"MW"->5,"Mh"->7,"Me"->1,"Mm"->2,"Mu"->3,"Mc"->4,"Md"->6,"Ms"->8};
gtcCases={
 <|"Source"->{"u","U","C","c"},"Map"->gtcIdentity,"Image"->{1,3,2,4},"ExpectedPhase"->-1|>,
 <|"Source"->{"u","U","C","c"},"Map"->gtcBothSwap,"Image"->{2,1,3,4},"ExpectedPhase"->1|>,
 <|"Source"->{"e","E","Z","h"},"Map"->gtcBothSwap,"Image"->{2,1,3,4},"ExpectedPhase"->1|>,
 <|"Source"->{"u","D","e","Ne"},"Map"->gtcBothSwap,"Image"->{2,3,1,4},"ExpectedPhase"->-1|>,
 <|"Source"->{"e","E","E","e"},"Map"->gtcBothSwap,"Image"->{4,2,3,1},"ExpectedPhase"->-1|>};
gtcRun[]:=Do[Module[{source=one["Source"],image=one["Image"],mapping=one["Map"],target,value,masses,candidate,
 channels,raw,benchmark,report,table,rows,feynmanRows,status,started,record},
 started=AbsoluteTime[];target=ConstantArray["",4];target[[image]]=source/.Normal[mapping["ParticleMap"]];
 value=LookupAmplitude[MapIndexed[{#1,First[#2]}&,source],gtcFull,False,{i1,i2,i3,i4}];
 (* Build external masses without allowing nested pure-function slots to alias. *)
 masses=MapIndexed[Function[{name,pos},Mass[First[pos]]->SelectFirst[gtcFull[[1]],MemberQ[#[[{2,3}]],name]&][[6]]],source];
 candidate=SMFivePointGenerationTransport`Private`generationTransportCandidate[{masses,value},source,target,gtcFull,mapping,image,gtcElementary,{gtcRoot<>"/models",1}];
 If[!AssociationQ[candidate],AppendTo[gtcNative,<|"SourceParticles"->source,"TargetParticles"->target,"Status"->"SETUPFAIL","Failure"->candidate|>];gtcSave[];Break[]];
 gtcCheck["declared role-block parity "<>ToString[source,InputForm]<>" -> "<>ToString[target,InputForm],candidate["FermionReferenceSign"]===one["ExpectedPhase"]];
 channels=FactorizationAmplitudeChannels[target,gtcElementary];
 raw=DiagramAmplitudes[CreateDiagrams[target,gtcFeynman]];
 benchmark=FeynmanToSpinorProducts[raw,"ReduceSpinorProducts"->False,"TimeConstraint"->30];
 report=TimeConstrained[CheckAmplitudes[candidate["Amplitude"],channels,gtcFull,
  "ParameterRules"->gtcRules,"Samples"->2,"FeynmanSamples"->2,"HighEnergySamples"->2,"ReferenceSamples"->2,
  "FeynmanAmplitudes"->benchmark,"FeynmanModel"->gtcFeynman,"RandomSeed"->(1729+Length[gtcNative]),
  "Parallelization"->False,"Output"->"Details","TimeConstraint"->90],120,Missing["ControlTimeConstraint"]];
 table=If[AssociationQ[report],Lookup[report,"Table",None],None];rows=If[MatchQ[table,_Dataset],Normal[table],{}];
 feynmanRows=If[AssociationQ[report],Cases[report,a_Association/;Lookup[a,"Type",None]==="Feynman"&&KeyExistsQ[a,"MatchingSigns"],Infinity],{}];
 status=If[rows=!={}&&AllTrue[rows,Lookup[#,"Status",None]==="PASS"&]&&AnyTrue[rows,Lookup[#,"Type",None]==="Feynman"&],"PASS","NOT_ALL_PASS"];
 record=<|"SourceParticles"->source,"TargetParticles"->target,"FermionReferenceSign"->candidate["FermionReferenceSign"],
  "Status"->status,"Rows"->Map[KeyTake[#,{"Color","Type","Status"}]&,rows],
  "FeynmanMatchingSigns"->Lookup[feynmanRows,"MatchingSigns",{}],
  "FeynmanCompletedSamples"->Lookup[feynmanRows,"CompletedSamples",{}],
  "Seconds"->N[AbsoluteTime[]-started],"Candidate"->candidate,"Report"->report,
  "RawIndependentFeynmanPairs"->raw,"IndependentFeynmanPairs"->benchmark|>;
 AppendTo[gtcNative,record];
 gtcCheck["fresh strict native Feynman control "<>ToString[target,InputForm],status==="PASS"];
 gtcSave[];Print["NATIVE_CONTROL ",source," -> ",target," ",status];
 If[status=!= "PASS",Break[]]],{one,gtcCases}];
gtcOutcome=MemoryConstrained[TimeConstrained[gtcRun[],300,Missing["TotalTimeConstraint"]],2*2^30,Missing["MemoryLimit"]];
gtcCheck["all five strict-phase native controls completed",Length[gtcNative]===5&&AllTrue[gtcNative,#["Status"]==="PASS"&]];
gtcCheck["all observed package model and helper files remain unchanged",gtcBeforeHashes===gtcObservedHashes[]];
gtcSave[];
Print["SM GENERATION TRANSPORT CHECKS: ",Length[gtcRows],"; FAILURES: ",Count[Lookup[gtcRows,"Status",{}],"FAIL"]];
gtcPassed=Length[gtcNative]===5&&AllTrue[gtcNative,#["Status"]==="PASS"&]&&AllTrue[gtcRows,#["Status"]==="PASS"&];
DeleteDirectory[gtcOut,DeleteContents->True];
If[gtcPassed,Print["HEPCAT_TEST_COMPLETED:sm-generation-transport-tests.wl"]];
Quit[If[gtcPassed,0,1]];
