(* A short ordinary identity proof may remove a retained direction only when
   its fully reconstructed endpoint is literally zero. *)

(*
HEPCAT - High Energy Physics Constructive Amplitude Tool

Copyright (C) 2026 Neil Christensen

This file is part of HEPCAT.

HEPCAT is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

HEPCAT is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with HEPCAT.  If not, see <https://www.gnu.org/licenses/>.
*)


root=DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Get[FileNameJoin[{root,"source","HEPCAT.wl"}]];
$HistoryLength=0;checks=0;failures={};SetAttributes[check,HoldRest];
check[name_,assertion_]:=(checks++;If[!TrueQ[assertion],AppendTo[failures,name];Print["FAIL: ",name]]);
htzProof=ConstructiveDiagrams`Private`completionHEZeroDirection;
htzModel=ReadModel[FileNameJoin[{root,"models"}],2];
htzMass=ModelParameter[htzModel,"Me"];
htzRules=Thread[(Mass/@Range[4])->ConstantArray[htzMass,4]];
htzA[i_,j_]:=SpinorChain[Spinor["Spin","Angle",i],Spinor["Spin","Angle",j]];
htzS[i_,j_]:=SpinorChain[Spinor["Spin","Square",i],Spinor["Spin","Square",j]];
htzLeft=htzA[1,2]htzA[3,4];
htzRight=SchoutenRewrite[htzA[1,2],htzA[3,4],{0,0}];
htzPole=PropDen[Mom[Multiparticle[1,3]],htzMass,htzWidth];
htzZero=(htzLeft-htzRight)htzS[1,2]htzS[3,4]/htzPole;
check["the fixture is a genuine spinor identity beyond scalar cancellation",
  htzRight=!=$Failed&&htzZero=!=0&&
    !ConstructiveDiagrams`Private`residueExactZero[htzZero]];
htzReport=htzProof[htzZero,htzRules,htzModel,5];
check["an eight-slot Schouten zero is proved by the short ordinary tensor route",
  AssociationQ[htzReport]&&htzReport["Status"]==="PROVED_ZERO"&&
    htzReport["Method"]==="OrdinaryExternalIdentities"&&
    htzReport["ProofInput"]==="ExactFourLegTensorQuotient"];
check["the original direction and complete zero endpoint are preserved",
  htzReport["OriginalDirection"]===htzZero&&
    htzReport["OrdinaryTensorIdentityProof"]["Amplitude"]===0&&
    htzReport["OrdinaryTensorIdentityProof"]["Route"]=!={}];
check["the proof uses no internal shell or sampled model or kinematic data",
  htzReport["InternalOnShellConditions"]==={}&&
    htzReport["NumericalParameterAssignments"]==={}&&
    htzReport["NumericalKinematicSamples"]===0&&
    htzReport["OrdinaryTensorIdentityProof"]["InternalOnShellConditions"]==={}];
check["the width-bearing physical propagator remains in the certificate",
  MemberQ[Cases[htzReport["OrdinaryTensorIdentityProof"]["Route"],
    HoldPattern[Rule[p_PropDen,_]]:>p,Infinity],htzPole]];

(* Reuse the existing exact certificate checker without running its input
   benchmarks. It recomputes typed relabeling, reversible mass scaling,
   coefficient-row replay and numerator assembly for every accepted step. *)
htzCheckerSource=Import[FileNameJoin[{root,"tests","checks",
  "four-leg-tensor-reduction-tests.wl"}],"Text"];
htzBegin=First[First[StringPosition[htzCheckerSource,"ftInverses[expression_] :="]]];
htzEnd=First[First[StringPosition[htzCheckerSource,"ftMassRoundTrip[report_] :="]]]-1;
ToExpression[StringTake[htzCheckerSource,{htzBegin,htzEnd}]];
check["every nested ordinary tensor identity certificate replays exactly",
  ftReplay[htzReport["OrdinaryTensorIdentityProof"]]];
check["the proof route retains symbolic independent scalar parameters",
  !KeyExistsQ[htzReport,"ModelDefinitionExpansion"]&&
    MemberQ[Cases[htzReport["OriginalDirection"],_ModelParameter,Infinity],htzMass]];

htzExpansionCalls=0;
htzNoExpansion=Internal`InheritedBlock[
  {ConstructiveDiagrams`Private`modelExpandParameterDefinitions},
  Clear[ConstructiveDiagrams`Private`modelExpandParameterDefinitions];
  ConstructiveDiagrams`Private`modelExpandParameterDefinitions[___]:=
    (htzExpansionCalls++;Failure["UnexpectedModelExpansion",<||>]);
  htzProof[htzZero,htzRules,htzModel,5]];
check["ordinary tensor zeros are established before dependent model expansion",
  htzExpansionCalls===0&&htzNoExpansion["Status"]==="PROVED_ZERO"&&
    htzNoExpansion["ProofInput"]==="ExactFourLegTensorQuotient"];

htzNonzero=htzLeft htzS[1,2]htzS[3,4]/htzPole;
htzNotProved=TimeConstrained[htzProof[htzNonzero,htzRules,htzModel,0.15],2,$TimedOut];
check["a nonzero tensor cannot acquire a zero certificate from lower complexity",
  AssociationQ[htzNotProved]&&htzNotProved["Status"]==="NOT_PROVED"];
htzLowBudget=htzProof[htzZero,htzRules,htzModel,0];
check["an unavailable proof budget retains the unproved direction",
  htzLowBudget["Status"]==="NOT_PROVED"&&
    !KeyExistsQ[htzLowBudget,"OrdinaryTensorIdentityProof"]];

htzTensorCalls=0;
htzOldRoutes=Internal`InheritedBlock[
  {ConstructiveDiagrams`Private`rspFourLegTensorReduction},
  Clear[ConstructiveDiagrams`Private`rspFourLegTensorReduction];
  ConstructiveDiagrams`Private`rspFourLegTensorReduction[___]:=
    (htzTensorCalls++;Failure["UnexpectedFourLegTensorCall",<||>]);
  Table[htzProof[htzNonzero,Thread[(Mass/@Range[n])->ConstantArray[htzMass,n]],
    htzModel,0],{n,{5,6}}]];
check["five- and six-leg inputs keep their existing proof route",
  htzTensorCalls===0&&AllTrue[htzOldRoutes,#["Status"]==="NOT_PROVED"&&
    !KeyExistsQ[#,"OrdinaryTensorIdentityProof"]&]];
check["ordinary rational-zero handling still precedes the tensor portfolio",
  htzProof[0,htzRules,htzModel,0]["Method"]==="ExactRationalAlgebra"];

Print["CHECKS: ",checks,"; FAILURES: ",Length[failures]];
If[failures=!={},Print[failures]];
Print["HEPCAT_TEST_COMPLETED:high-energy-tensor-zero-tests.wl"];
Quit[If[failures==={},0,1]];
