(* A Schouten sum is a tensor identity, including a nonzero massive endpoint.
   Test independent two-component columns as well as the completion regression. *)
root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Global`$HEPCATpath = FileNameJoin[{root,"source"}];
Get[FileNameJoin[{$HEPCATpath,"HEPCAT.wl"}]];
checks=0;failures={};SetAttributes[check,HoldRest];
check[name_,value_] := (checks++; If[!TrueQ[value],AppendTo[failures,name];Print["FAIL: ",name]]);
ssSquare[i_] := Spinor["Xi","Square",i];
ssAngle[i_] := Spinor["Xi","Angle",i];
ssBracket[i_,j_] := SpinorChain[ssSquare[i],ssSquare[j]];
ssOne[i_] := SpinorChain[ssSquare[i],Mom[1],ssAngle[2]];
ssInput=ssBracket[1,3] ssOne[4] - ssBracket[1,4] ssOne[3];
ssExpected=-ssBracket[3,4] ssOne[1];
ssEpsilon={{0,1},{-1,0}};
ssAdj[p_] := {{p[[2,2]],-p[[1,2]]},{-p[[2,1]],p[[1,1]]}};
ssVector[spinor_] := If[spinor[[2]]==="Square",ssSquares[spinor[[3]]],ssAngles[spinor[[3]]]];
ssChain[chain_SpinorChain] := Module[{parts=List@@chain,row,type},
 type=parts[[1,2]];row=ssVector[First[parts]].ssEpsilon If[type==="Square",-1,1];
 Do[row=row.If[type==="Square",ssAdj[ssMomentum],ssMomentum];type=If[type==="Square","Angle","Square"],
 {momentum,Rest[Most[parts]]}];row.ssVector[Last[parts]]];
ssValue[expression_] := Expand[expression /. chain_SpinorChain :> ssChain[chain]];
Do[
 BlockRandom[SeedRandom[seed];
  ssSquares=AssociationThread[Range[4],Table[RandomInteger[{-7,7},2],{4}]];
  ssAngles=AssociationThread[Range[4],Table[RandomInteger[{-7,7},2],{4}]];
  ssMomentum=RandomInteger[{-7,7},{2,2}]];
 check[{"independent Schouten sign",seed},ssValue[ssInput]===ssValue[ssExpected]];
 check[{"public reducer preserves Schouten sum",seed},ssValue[ReduceSpinorProducts[ssInput]]===ssValue[ssInput]];
 check[{"outer scalar and bracket do not reverse the identity",seed},
   ssValue[ReduceSpinorProducts[7ssBracket[3,4] ssInput]]===7ssValue[ssBracket[3,4] ssInput]],
 {seed,{173,991,3251}}];
ssPole=PropDen[Mom[Multiparticle[2,3]],ssMass];
ssRational=ReduceSpinorProducts[7ssInput/ssPole];
check["physical denominator stays intact",DeleteDuplicates[Cases[ssRational,_PropDen,Infinity]]==={ssPole}];
check["rational reduction preserves numerator Schouten sign",ssValue[ssRational ssPole]===7ssValue[ssInput]];
(* All other spinors can be massless, but leg1 must be massive to expose the
   old wrong sign: the old rule's error vanished in massless-only probes. *)
ssRules={Mass[1]->ssMass,Mass[2]->ssMass,Mass[3]->0,Mass[4]->0,Mass[5]->0};
ssPhysicalSquare[i_] := Spinor[If[i<=2,"Spin","Helicity"],"Square",i];
ssPhysicalAngle[i_] := Spinor[If[i<=2,"Spin","Helicity"],"Angle",i];
ssPhysicalBracket[i_,j_] := SpinorChain[ssPhysicalSquare[i],ssPhysicalSquare[j]];
ssPhysicalOne[i_] := SpinorChain[ssPhysicalSquare[i],Mom[1],ssPhysicalAngle[5]];
ssMassiveInput=ssPhysicalBracket[1,3] ssPhysicalOne[4] - ssPhysicalBracket[1,4] ssPhysicalOne[3];
ssMassiveExpected=-ssPhysicalBracket[3,4] SpinorChain[ssPhysicalSquare[1],Mom[1],ssPhysicalAngle[5]];
ssChart=ConstructiveDiagrams`Private`completionHESymbolicExternalChart[ssRules];
ssMap[value_] := Cancel[Together[ConstructiveDiagrams`Private`residueEvaluateChart[value,ssChart,{}]]];
check["massive endpoint Schouten agrees on exact generic external chart",ssMap[ssMassiveInput-ssMassiveExpected]===0];
check["massive endpoint public reduction preserves the full tensor",ssMap[(ReduceSpinorProducts[ssMassiveInput]/.ssRules)-ssMassiveInput]===0];
Do[
 ssIndexedInput=ssMassiveInput/.Spinor["Spin",type_,1]:>Spinor["Spin",variance,type,1,spin];
 ssIndexedExpected=ssMassiveExpected/.Spinor["Spin",type_,1]:>Spinor["Spin",variance,type,1,spin];
 check[{"indexed massive Schouten tensor",variance,spin},ssMap[ssIndexedInput-ssIndexedExpected]===0];
 check[{"indexed reducer preserves its spin component",variance,spin},
  ssMap[(ReduceSpinorProducts[ssIndexedInput]/.ssRules)-ssIndexedInput]===0],
 {variance,{"Upper","Lower"}},{spin,{-1/2,1/2}}];
ssZero=ssPhysicalBracket[3,4] SpinorChain[ssPhysicalSquare[1],Mom[2],ssPhysicalAngle[5]] +
 ssPhysicalBracket[1,3] SpinorChain[ssPhysicalSquare[4],Mom[2],ssPhysicalAngle[5]] -
 ssPhysicalBracket[1,4] SpinorChain[ssPhysicalSquare[3],Mom[2],ssPhysicalAngle[5]];
check["completion null direction is a genuine external identity",ssMap[ssZero]===0];
ssModel=ReadModel[FileNameJoin[{root,"models"}],2];
ssPrepared=PrepareAmplitudes[{ssRules,ssZero/(ssPole PropDen[Mom[Multiparticle[2,4]],ssMass]
 PropDen[Mom[Multiparticle[2,5]],ssMass])},ssModel,
 "InternalOnShell"->False,"PolePreference"->None,"MaxSteps"->0,
 "TimeConstraint"->20,"ProgressReporting"->False,"Parallelization"->False,"ReturnDetails"->True];
check["completed-amplitude preparation preserves the null direction",ssMap[ssPrepared["Amplitude"]]===0];
check["the regression invokes no internal shell condition",ssPrepared["ItemReports"][[1]]["InternalOnShellChannels"]==={}];
ssProof=ConstructiveDiagrams`Private`completionHEZeroDirection[ssZero,ssRules,ssModel,20];
check["high-energy completion certifies rather than corrupts the identity direction",ssProof["Status"]==="PROVED_ZERO"];
(* A shared clock bounds proof work. There is no fixed sixteen-direction
   cutoff that can leave a later ordinary identity artificially undetermined. *)
ssFamilyParameters=Table[Unique["schoutenIdentityCoefficient"],{17}];
ssIdentityFamily=1+ssZero Total[MapIndexed[First[#2] #1&,ssFamilyParameters]];
ssPrunedFamily=ConstructiveDiagrams`Private`completionHEPruneIdentityFamily[
 ssIdentityFamily,ssFamilyParameters,ssRules,ssModel,30];
check["more than sixteen ordinary identity directions can all be certified",
 ssPrunedFamily["FreeParameters"]==={} && ssPrunedFamily["FamilyAmplitude"]===1 &&
 Length[ssPrunedFamily["RemovedParameters"]]===17 &&
 AllTrue[ssPrunedFamily["Certificates"],#["Status"]==="PROVED_ZERO"&]];
ssBudgetFamily=ConstructiveDiagrams`Private`completionHEPruneIdentityFamily[
 ssIdentityFamily,ssFamilyParameters,ssRules,ssModel,0];
check["exhausted identity-proof clock preserves every later direction",
 ssBudgetFamily["FreeParameters"]===ssFamilyParameters &&
 ssBudgetFamily["RemovedParameters"]==={} && Length[ssBudgetFamily["RemainingBasis"]]===17];
If[checks=!=27,AppendTo[failures,{"Expected27Checks",checks}]];
Print[checks," Schouten-sum sign checks; ",Length[failures]," failures."];
If[failures=!={},Print[failures];Quit[1]];
Print["HEPCAT_TEST_COMPLETED:schouten-sum-sign-tests.wl"];
Quit[0];
