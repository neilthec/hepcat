(* The input fixture stores an independently validated construction result,
   never a compact answer. This suite checks the new grouping proof around
   the ordinary tensor identities already audited by their own suite. *)
pgRoot = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Get[FileNameJoin[{pgRoot,"source","HEPCAT.wl"}]];
$HistoryLength=0;pgChecks=0;pgFailures={};
SetAttributes[pgCheck,HoldRest];
pgCheck[name_,assertion_]:=(pgChecks++;If[!TrueQ[assertion],AppendTo[pgFailures,name];Print["FAIL: ",name]]);
pgModel=ReadModel[FileNameJoin[{pgRoot,"models"}],1];
pgInput=Get[FileNameJoin[{pgRoot,"tests","checks","fixtures","massive-four-vector-completed-input.wl"}]][ModelParameter[pgModel,#]&];
pgRules=pgInput["ExternalMassRules"];pgAmplitude=pgInput["Amplitude"];
pgReduce=ConstructiveDiagrams`Private`rspFourLegTensorReduction;
pgEligible=ConstructiveDiagrams`Private`rspFourLegTensorPoleGroupCandidateQ;
pgRelabel=ConstructiveDiagrams`Private`relabelLegs;
pgInverses[value_]:=DeleteDuplicates[Cases[{value},p:Power[base_,n_]/;!TrueQ[n>=0]&&!FreeQ[base,_PropDen|_SpinorChain|_MomProd|_Mom|_Spinor]:>p,Infinity]];
pgFrameReplay[route_List,before_,after_]:=Module[{previous=before,valid},
 valid=AllTrue[route,Function[frame,
 frame["Operation"]==="TypedFrameFourLegTensorIdentities"&&frame["Before"]===previous&&
 pgRelabel[frame["Before"],frame["ForwardLabelMap"]]===frame["RenamedBefore"]&&
 pgRelabel[frame["RenamedBefore"],frame["InverseLabelMap"]]===frame["Before"]&&
 pgRelabel[frame["RenamedAfter"],frame["InverseLabelMap"]]===frame["After"]&&
 frame["CoreRoute"]=!={}&&First[frame["CoreRoute"]]["Before"]===frame["RenamedBefore"]&&
 Last[frame["CoreRoute"]]["After"]===frame["RenamedAfter"]&&
 AllTrue[frame["CoreRoute"],TrueQ[#["ExactFormalReplay"]]&&#["InternalOnShellConditions"]==={}&]&&
 frame["InternalOnShellConditions"]==={}&&(previous=frame["After"];True)]];
 valid&&previous===after];
pgOuterReplay[report_]:=Module[{outer,restore,original,updated,certificates},
 If[Lookup[report,"Status",None]=!="IMPROVED"||Length[report["Route"]]=!=1,Return[False]];
 outer=First[report["Route"]];
 If[outer["Operation"]=!="ExactInverseSpectatorFourLegTensorGroups",Return[False]];
 restore=Reverse /@ outer["ProtectedInverseMap"];
 original=outer["OriginalGroups"];updated=original;certificates=outer["GroupCertificates"];
 If[!DuplicateFreeQ[Lookup[certificates,"GroupIndex"]],Return[False]];
 If[!AllTrue[certificates,Function[certificate,
  With[{index=certificate["GroupIndex"]},certificate["OriginalGroup"]===original[[index]]&&
   pgFrameReplay[certificate["IdentityRoute"],original[[index]],certificate["Amplitude"]]&&
   (updated=ReplacePart[updated,index->certificate["Amplitude"]];True)]]],Return[False]];
 outer["Before"]===pgAmplitude&&
 (outer["Before"]/.outer["ProtectedInverseMap"])===outer["ProtectedExpression"]&&
 (outer["ProtectedExpression"]/.restore)===outer["Before"]&&
 Expand[outer["ProtectedExpression"]-Total[outer["ProtectedGroups"]]]===0&&
 (outer["ProtectedGroups"]/.restore)===original&&
 updated===outer["UpdatedGroups"]&&Total[updated]===outer["After"]&&
 outer["After"]===report["Amplitude"]&&outer["InternalOnShellConditions"]==={}];
pgCheck["portable input retains its recorded numerator-momentum score",
 SpinorExpressionComplexity[pgAmplitude]===pgInput["SourceBaselineScore"]];
pgCheck["only a guarded multi-inverse four-massive-leg input is eligible",
 pgEligible[pgAmplitude,pgRules]&&LeafCount[pgAmplitude]>28000];
pgCheck["higher multiplicity never enters the grouping fallback",
 !pgEligible[pgAmplitude,Append[pgRules,Mass[5]->ModelParameter[pgModel,"MW"]]]];
pgCheck["a massless external leg keeps the ordinary path",
 !pgEligible[pgAmplitude,ReplacePart[pgRules,1->(Mass[1]->0)]]];
pgCheck["distinct numerical masses retain the safe abstraction decline",
 !pgEligible[pgAmplitude,Thread[Mass/@Range[4]->{2,3,2,3}]]];
pgCheck["unsupported noncommutative input is declined before grouping",
 !pgEligible[pgAmplitude+NonCommutativeMultiply[pgX,pgY],pgRules]];
pgCheckpoint=None;pgMessages={};
{pgSeconds,pgReport}=AbsoluteTiming[Block[{$MessageList={}},
 pgResult=pgReduce[pgAmplitude,pgRules,True,10,Function[record,pgCheckpoint=record]];
 pgMessages=$MessageList;pgResult]];
pgCheck["the bounded grouping portfolio finds a substantially smaller endpoint",
 pgReport["Status"]==="IMPROVED"&&TrueQ[pgReport["PoleGroupFallback"]]&&
 SpinorExpressionComplexity[pgReport["Amplitude"]]<pgInput["SourceBaselineScore"]/4];
pgCheck["the outer proof exactly reconstructs every group and its nested identity route",pgOuterReplay[pgReport]];
pgCheck["no new inverse base or exponent is introduced",
 AllTrue[pgInverses[pgReport["Amplitude"]],MemberQ[pgInverses[pgAmplitude],#]&]];
pgCheck["a checkpoint publishes the complete matching amplitude and route",
 AssociationQ[pgCheckpoint]&&pgCheckpoint["Amplitude"]===pgReport["Amplitude"]&&
 pgCheckpoint["Route"]===pgReport["Route"]&&pgOuterReplay[pgCheckpoint]];
pgCheck["private callback formal names do not produce evaluation messages",pgMessages==={}];
pgCheck["the entire grouped search respects one caller deadline",pgSeconds<11];
{pgTinySeconds,pgTiny}=AbsoluteTiming[pgReduce[pgAmplitude,pgRules,True,.001,None]];
pgCheck["an exhausted deadline retains the original endpoint without a partial route",
 pgTiny["Amplitude"]===pgAmplitude&&pgTiny["Route"]==={}&&pgTinySeconds<.8];
pgA[i_,j_]:=SpinorChain[Spinor["Spin","Angle",i],Spinor["Spin","Angle",j]];
pgS[i_,j_]:=SpinorChain[Spinor["Spin","Square",i],Spinor["Spin","Square",j]];
pgToy=(pgA[1,2]pgA[3,4]+pgA[1,4]pgA[2,3])pgS[1,3]pgS[2,4]/PropDen[Mom[Multiparticle[1,3]],pgMass,pgWidth];
pgToyReport=pgReduce[pgToy,Thread[Mass/@Range[4]->2],True,1,None];
pgCheck["small whole packets retain the existing fast path and finite-width inverse",
 pgToyReport["Status"]==="IMPROVED"&&!TrueQ[Lookup[pgToyReport,"PoleGroupFallback",False]]&&
 First[pgToyReport["Route"]]["Operation"]==="TypedFrameFourLegTensorIdentities"&&
 pgInverses[pgToyReport["Amplitude"]]===pgInverses[pgToy]];
(* Isolate restoration from the quotient solver: unchanged subgroup calls
   must still receive the literal composite inverse with its original order. *)
pgAuxiliary=1/(5+MomProd[1,2]+SpinorChain[
 Spinor["Spin","Angle",1],Mom[2],Spinor["Spin","Square",3]]);
pgFiller=Total[Array[pgOpaqueCoefficient,520]]pgA[1,3]pgA[2,4]pgS[1,3]pgS[2,4]/
 PropDen[Mom[Multiparticle[1,4]],pgOtherMass,pgOtherWidth];
pgAuxiliaryInput=pgAuxiliary(pgToy+pgFiller);pgObservedGroups={};
pgAuxiliaryReport=Internal`InheritedBlock[{ConstructiveDiagrams`Private`rspFourLegTensorReductionFrames},
 Clear[ConstructiveDiagrams`Private`rspFourLegTensorReductionFrames];
 ConstructiveDiagrams`Private`rspFourLegTensorReductionFrames[source_,arguments___]:=(
  AppendTo[pgObservedGroups,source];<|"Amplitude"->source,"Status"->"UNCHANGED",
   "Route"->{},"StopReason"->"InputDeclined","TimedOut"->False|>);
 pgReduce[pgAuxiliaryInput,Thread[Mass/@Range[4]->2],True,2,None]];
pgCheck["composite auxiliary inverse restoration preserves every nested argument",
 TrueQ[pgAuxiliaryReport["PoleGroupFallback"]]&&Length[pgObservedGroups]>=3&&
 AllTrue[Rest[pgObservedGroups],!FreeQ[#,pgAuxiliary]&]&&
 pgAuxiliaryReport["StopReason"]==="Complete"&&
 pgAuxiliaryReport["Amplitude"]===pgAuxiliaryInput&&pgAuxiliaryReport["Route"]==={}];
Print["Tensor pole groups: ",pgChecks-Length[pgFailures],"/",pgChecks," passed."];
Print["HEPCAT_TEST_COMPLETED:four-leg-tensor-pole-group-tests.wl"];
If[pgFailures=!={},Print[pgFailures];Quit[1]];Quit[0];
