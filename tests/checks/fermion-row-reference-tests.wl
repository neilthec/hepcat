(* Stable fermion role blocks ignore cross-role row interleavings. *)
(* HEPCAT - High Energy Physics Constructive Amplitude Tool
   Copyright (C) 2026 Neil Christensen
   SPDX-License-Identifier: GPL-3.0-or-later *)

Begin["FermionRowReferenceTests`Private`"];
root=DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Get[FileNameJoin[{root,"source","HEPCAT.wl"}]];
$HistoryLength=0;checks=0;failures={};
SetAttributes[check,HoldRest];
check[name_,body_]:=Module[{value},checks++;
 value=Quiet[Check[TimeConstrained[body,90,$TimedOut],$Failed]];
 If[!TrueQ[value],AppendTo[failures,name];Print["FAIL: ",name]]];
model=ReadModel[FileNameJoin[{root,"models"}],2];
feynman=ReadModel[FileNameJoin[{root,"tests","reference-models","calchep"}],1];
process={"e","Ne","W+","h","h"};lowerProcess={"e","Ne","W+","h"};
rowIndex=SelectFirst[Range[Length[model[[2]]]],
 Sort[DeleteCases[Drop[model[[2,#]],-2],""]]===Sort[lowerProcess]&];
check["the regression uses the complete lower tensor with opposite stored fermion order",
 DeleteCases[Drop[model[[2,rowIndex]],-2],""]==={"Ne","e","W+","h"}];
reindexRow[base_,order_List]:=ReplacePart[base,{2,rowIndex}->Join[order,{1,
 LookupAmplitude[MapIndexed[{#1,First[#2]}&,order],base,False,{i1,i2,i3,i4}]}]];
variants=reindexRow[model,#]&/@{
 {"e","Ne","W+","h"},{"h","W+","e","Ne"},{"W+","Ne","h","e"}};
cutTags={
 {{{"h",4},{"h",5},{"h",Multiparticle[1,2,3]}},
  {{"e",1},{"Ne",2},{"W+",3},{"h",Multiparticle[4,5]}}},
 {{{"e",1},{"h",4},{"E",Multiparticle[2,3,5]}},
  {{"Ne",2},{"W+",3},{"h",5},{"e",Multiparticle[1,4]}}}
};
Do[check["single-pair row reindexing preserves "<>If[k===1,"boson","fermion"]<>
 " sewing with lower block on side "<>ToString[side]<>" variant "<>ToString[v],
 With[{channel=If[side===1,Reverse[cutTags[[k]]],cutTags[[k]]]},
 ConstructiveDiagrams`Private`factorizationExternalFermionSign[channel,model]===
 ConstructiveDiagrams`Private`factorizationExternalFermionSign[channel,variants[[v]]]]],
 {k,2},{side,2},{v,Length[variants]}];
metadataVariant=ReplacePart[model,1->Reverse[model[[1]]]];
Do[check["single-pair sewing is invariant under particle declaration order "<>ToString[k],
 ConstructiveDiagrams`Private`factorizationExternalFermionSign[cutTags[[k]],model]===
 ConstructiveDiagrams`Private`factorizationExternalFermionSign[cutTags[[k]],metadataVariant]],{k,2}];
(* This mixed-species multi-pair word is intentionally retained, not sorted by
   the arbitrary order in which particles happen to be declared in the model. *)
multiChannel={{{"e",3},{"h",5},{"E",Multiparticle[1,2,4]}},
 {{"u",1},{"D",2},{"Ne",4},{"e",Multiparticle[3,5]}}};
check["semileptonic multi-pair sewing does not use particle metadata ranks",
 ConstructiveDiagrams`Private`factorizationExternalFermionSign[multiChannel,model]===
 ConstructiveDiagrams`Private`factorizationExternalFermionSign[multiChannel,metadataVariant]];
channels=FactorizationAmplitudeChannels[process,model];
Do[check["complete channel tensors are invariant under stable lower-row reindexing "<>ToString[v],
 Module[{other=FactorizationAmplitudeChannels[process,variants[[v]]]},
 Length[channels]===Length[other]&&AllTrue[channels,Function[row,
 With[{match=SelectFirst[other,#[[-3]]===row[[-3]]&&#[[-2]]===row[[-2]]&]},
 Cancel[Together[Last[row]-Last[match]]]===0]]]]],{v,Length[variants]}];


(* Stable interleavings retain occurrence order inside each role, including
   repeated identical names. A same-role exchange is tested separately below. *)
reindexAt[base_,index_,order_List]:=ReplacePart[base,{2,index}->Join[order,{1,
 LookupAmplitude[MapIndexed[{#1,First[#2]}&,order],base,False,{i1,i2,i3,i4}]}]];
semileptonicIndex=SelectFirst[Range[Length[model[[2]]]],
 Sort[DeleteCases[Drop[model[[2,#]],-2],""]]===Sort[{"u","D","e","Ne"}]&];
semileptonicVariants=reindexAt[model,semileptonicIndex,#]&/@{
 {"u","e","D","Ne"},{"D","u","Ne","e"},{"D","Ne","u","e"}};
Do[check["multi-pair cross-role interleaving preserves sewing on side "<>ToString[side]<>
 " variant "<>ToString[v],With[{channel=If[side===1,Reverse[multiChannel],multiChannel]},
 ConstructiveDiagrams`Private`factorizationExternalFermionSign[channel,model]===
 ConstructiveDiagrams`Private`factorizationExternalFermionSign[channel,semileptonicVariants[[v]]]]],
 {side,2},{v,Length[semileptonicVariants]}];
qed=ReadModel[FileNameJoin[{root,"models"}],4];
Do[
 qIndex=SelectFirst[Range[Length[qed[[2]]]],
 Sort[DeleteCases[Drop[qed[[2,#]],-2],""]]===Sort[Join[ConstantArray["e",pairCount],ConstantArray["E",pairCount]]]&];
 qOrder=Join[ConstantArray["e",pairCount],ConstantArray["E",pairCount]];
 qVariant=reindexAt[qed,qIndex,qOrder];
 qWord=MapIndexed[{#1,If[First[#2]===2pairCount,Multiparticle[2pairCount,2pairCount+1],First[#2]]}&,
 DeleteCases[Drop[qed[[2,qIndex]],-2],""]];
 qInternal=Last[qWord][[1]];
 qOther=If[qInternal==="e","E","e"];
 qChannel={qWord,{{qOther,Multiparticle@@Range[2pairCount-1]},
   {qInternal,2pairCount},{"A.+",2pairCount+1}}};
 Do[check["identical "<>ToString[pairCount]<>"-pair lower row retains its reference under stable interleaving side "<>ToString[side],
 With[{channel=If[side===1,Reverse[qChannel],qChannel]},
 ConstructiveDiagrams`Private`factorizationExternalFermionSign[channel,qed]===
 ConstructiveDiagrams`Private`factorizationExternalFermionSign[channel,qVariant]]],{side,2}],
 {pairCount,{2,3}}];
(* This is the adapter's actual Wick-pair state convention. Swapping ket or
   bar endpoints changes the reference sign; swapping both restores it. *)
check["same-role fermion exchanges retain their signed state-reference transport",
 ConstructiveDiagrams`Private`feynmanFermionPairSign[{{2,1},{4,3}}]===1&&
 ConstructiveDiagrams`Private`feynmanFermionPairSign[{{2,3},{4,1}}]===-1&&
 ConstructiveDiagrams`Private`feynmanFermionPairSign[{{4,1},{2,3}}]===-1&&
 ConstructiveDiagrams`Private`feynmanFermionPairSign[{{4,3},{2,1}}]===1];

(* A real independent amplitude is required, not just parity helper identities.
   The antineutrino helicity follows the reference model's chiral field row. *)
helicities=Flatten[Table[Module[{name=process[[i]],part,hand,sign},
 part=SelectFirst[feynman[[1]],MemberQ[#[[{2,3}]],name]&];hand=part[[9]];
 If[part[[6]]===0&&Abs[part[[5]]]===1&&MemberQ[{"L","R"},hand],
  sign=If[name===part[[2]],-1,1]If[hand==="L",1,-1];{i->sign/2},{}]],
 {i,Length[process]}]];
pairs=PrepareAmplitudes[DiagramAmplitudes[CreateDiagrams[process,feynman]],feynman,
 "PolePreference"->"IndependentXi","MaxSteps"->0,"InternalOnShell"->False,
 "ExternalParticles"->process,"FermionHelicities"->helicities,"ProgressReporting"->False];
parameters=(#->ModelParameter[model,ModelParameterName[#]]&)/@
 DeleteDuplicates[Cases[pairs,_ModelParameter,Infinity]];
amplitude=Total[Last/@pairs]/.parameters;
rules=ConstructiveDiagrams`Private`residueParameterRules[model,Automatic];
targets=ConstructiveDiagrams`Private`residueSimpleInputs[0,channels,model]["Targets"];
physical=DeleteDuplicatesBy[ConstructiveDiagrams`Private`residueNormalizeChannel/@targets,
 ConstructiveDiagrams`Private`residueChannelKey];
check["the charged-current regression has seven physical channels",Length[physical]===7];
Do[check["native Feynman residue agrees on cut "<>ToString[channel["Cut"],InputForm]<>
 " coherent sample "<>ToString[sample],Module[{generated,chart,pole,map,result},
 generated=ConstructiveDiagrams`Private`completionCutChart[channel,sample,2122997127,rules];
 If[FailureQ[generated],False,
 chart=Join[generated["Chart"],<|"CoherentVectorSpinIndices"->True|>];pole=chart["PoleVariable"];
 map=With[{frozen=chart,originalPole=pole,parameterRules=rules},
 Function[{expression,cuts,variables},ConstructiveDiagrams`Private`residueEvaluateChart[
 expression,frozen/.originalPole->First[variables],parameterRules]]];
 result=ConstructiveDiagrams`Private`residueCheckChannel[amplitude,channel,{},targets,physical,
 map,"Symbolic",30,10^-10,{}];
 Lookup[result,"Status",None]==="PASS"&&Lookup[result,"Difference",Missing[]]===0]]],
 {channel,physical},{sample,4}];

semiProcess={"u","D","e","Ne","h"};
semiChannels=FactorizationAmplitudeChannels[semiProcess,model];
semiPairs=PrepareAmplitudes[DiagramAmplitudes[CreateDiagrams[semiProcess,feynman]],feynman,
 "PolePreference"->"IndependentXi","MaxSteps"->0,"InternalOnShell"->False,
 "ExternalParticles"->semiProcess,"FermionHelicities"->{4->1/2},"ProgressReporting"->False];
semiRebase=(#->ModelParameter[model,ModelParameterName[#]]&)/@
 DeleteDuplicates[Cases[semiPairs,_ModelParameter,Infinity]];
semiAmplitude=Total[Last/@semiPairs]/.semiRebase;
Do[check["all five native full-color semileptonic cuts agree at coherent sample "<>ToString[sample],
 Module[{cache=<||>,map,report},
 map=Function[{expression,cuts,variables},Module[{key,generated,chart,pole},
 key=ConstructiveDiagrams`Private`residueChannelKey[First[cuts]];
 If[!KeyExistsQ[cache,key],generated=ConstructiveDiagrams`Private`completionCutChart[
 First[cuts],sample,2122997127,rules];If[FailureQ[generated],Return[Missing["ChartUnavailable"]]];
 AssociateTo[cache,key->generated["Chart"]]];
 chart=cache[key];pole=chart["PoleVariable"];
 ConstructiveDiagrams`Private`residueEvaluateChart[expression,chart/.pole->First[variables],rules]]];
 report=CheckAmplitudes[semiAmplitude,semiChannels,model,"KinematicMap"->map,"KinematicKind"->"Symbolic",
 "HighEnergyCheck"->False,"ReferenceCheck"->False,"SpuriousPoles"->{},"TimeConstraint"->30,
 "Parallelization"->False,"Output"->"Details"];
 AssociationQ[report]&&Lookup[report,"Summary",<||>]===<|"PASS"->5|>]],{sample,4}];

Print["FERMION ROW REFERENCE CHECKS: ",checks,"; FAILURES: ",Length[failures]];
If[failures==={},Print["HEPCAT_TEST_COMPLETED:fermion-row-reference-tests.wl"],Print[failures]];
End[];
Quit[If[FermionRowReferenceTests`Private`failures==={},0,1]];
