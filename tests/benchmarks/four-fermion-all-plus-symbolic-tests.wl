(* Focused research-formula regression. No construction/completion runs.
   The matrix identities are generic symbolic identities; model/fixture and
   symmetry checks use exact rational external conserving/on-shell charts
   with all 16 coherent massive-spin components and no internal cut. *)

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


root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Get[FileNameJoin[{root,"source","HEPCAT.wl"}]];
Get[FileNameJoin[{root,"tests", "checks", "fixtures", "QED-5-point-residues.wl"}]];
Get[FileNameJoin[{root,"tests", "checks", "fixtures", "QED-four-fermion-all-plus-formula.wl"}]];
$HistoryLength=0;
checks=0;failures={};
SetAttributes[check,HoldRest];
check[name_,expression_]:=(checks++;If[!TrueQ[expression],AppendTo[failures,name];Print["FAIL: ",name]]);
(* Generic symbolic 2x2 proofs. No numerical or on-shell internal sampling. *)
ClearAll[ffP, ffAdj, ffVector, ffAngle, ffSquare, ffPauli, ffDot, ffZero];
ffSigma = {IdentityMatrix[2], {{0,1},{1,0}}, {{0,-I},{I,0}}, {{1,0},{0,-1}}};
ffBarSigma = ({{#[[2,2]],-#[[1,2]]},{-#[[2,1]],#[[1,1]]}}&) /@ ffSigma;
ffJ = {{0,-1},{1,0}}; ffL = -ffJ;
ffP[v_] := Total[MapThread[Times,{v,ffSigma}]];
ffAdj[p_] := {{p[[2,2]],-p[[1,2]]},{-p[[2,1]],p[[1,1]]}};
ffVector[p_] := Tr[p.#]/2 & /@ ffSigma;
ffAngle[a_,b_] := a.ffJ.b;
ffSquare[a_,b_] := a.ffL.b;
ffPauli[a_,b_,k_] := (a.ffJ.#.ffAdj[ffP[k]].b)& /@ ffBarSigma;
ffDot[a_,b_] := a.DiagonalMatrix[{1,-1,-1,-1}].b;
ffZero[x_] := AllTrue[Flatten[{x}],Together[#]===0&];
ffA={a1,a2};ffB={b1,b2};ffC={c1,c2};ffD={d1,d2};
ffK={k0,k1,k2,k3};ffH={h0,h1,h2,h3};ffO={o0,o1,o2,o3};
ffChecks = <|
  "VectorPauliContraction" -> ffZero[
    ffDot[ffO,ffPauli[ffC,ffD,ffH]] - ffC.ffJ.ffP[ffO].ffAdj[ffP[ffH]].ffD],
  "PauliPauliFierz" -> ffZero[
    ffDot[ffPauli[ffA,ffB,ffK],ffPauli[ffC,ffD,ffH]] -
    2 ffAngle[ffA,ffC] ffB.ffJ.ffP[ffK].ffAdj[ffP[ffH]].ffD],
  "OffshellMatrixWard" -> ffZero[
    Total[MapThread[Times, {ffK*{1,-1,-1,-1},ffBarSigma}]].ffAdj[ffP[ffK]] -
    ffDot[ffK,ffK] IdentityMatrix[2]]|>;
ffPa={{pa,pb},{pc,(m^2+pb pc)/pa}};
ffPb={{qa,qb},{qc,(m^2+qb qc)/qa}};
ffVa=ffVector[ffPa];ffVb=ffVector[ffPb];
ffB0=Table[ffA.ffJ.(2 ffVa[[mu]] IdentityMatrix[2]+ffBarSigma[[mu]].ffAdj[-ffPa-ffPb]).ffB,{mu,4}];
ffTildeA=ffL.ffAdj[ffPa].ffA/m;ffTildeB=ffL.ffAdj[ffPb].ffB/m;
ffG=Outer[Times,ffA,ffTildeB]+Outer[Times,ffB,ffTildeA];
AssociateTo[ffChecks,"GenericExternalGordonCurrent" -> ffZero[ffP[ffB0]-2m ffG]];
ffTildeC={s1,s2};ffTildeD={t1,t2};
ffTildeA={u1,u2};ffTildeB={v1,v2};
ffG=Outer[Times,ffA,ffTildeB]+Outer[Times,ffB,ffTildeA];
ffG2=Outer[Times,ffC,ffTildeD]+Outer[Times,ffD,ffTildeC];
ffQuartet=ffAngle[ffA,ffC]ffSquare[ffTildeB,ffTildeD]+
 ffAngle[ffA,ffD]ffSquare[ffTildeB,ffTildeC]+
 ffAngle[ffB,ffC]ffSquare[ffTildeA,ffTildeD]+
 ffAngle[ffB,ffD]ffSquare[ffTildeA,ffTildeC];
AssociateTo[ffChecks,"GordonQuartetSewingSign" -> ffZero[4ffDot[ffVector[ffG],ffVector[ffG2]]+2ffQuartet]];
ffReference={r1,r2};ffPhotonSquare={i1,i2};ffOtherSquare={j1,j2};
ffEpsilon=ffVector[Sqrt[2]Outer[Times,ffReference,ffPhotonSquare]/referenceDenominator];
ffOtherEpsilon=ffVector[Sqrt[2]Outer[Times,ffReference,ffOtherSquare]/otherReferenceDenominator];
AssociateTo[ffChecks,"CommonAngleSeagullSewingVanishes" -> ffZero[ffDot[ffEpsilon,ffOtherEpsilon]]];
AssociateTo[ffChecks,"SeagullPauliBracketConversion" -> ffZero[
 ffDot[ffEpsilon,ffPauli[ffC,ffD,ffH]] -
 Sqrt[2]ffAngle[ffC,ffReference](ffPhotonSquare.ffAdj[ffP[ffH]].ffD)/referenceDenominator]];

KeyValueMap[Function[{name,result},check["generic "<>name,result]],ffChecks];
qfMomenta[0]=<|1->{5/4,0,0,3/4},2->{5/4,0,0,-3/4},
 3->{-5/4,-3/5,0,-9/20},4->{-5/4,3/5,0,9/20}|>;
qfMomenta[1]=<|1->{5/4,0,0,3/4},2->{5/4,0,0,-3/4},
 3->{-9/8,-1/2,0,1/8},4->{-9/8,1/2,0,1/8},5->{-1/4,0,0,-1/4}|>;
qfMomenta[2]=<|1->{5/3,0,0,4/3},2->{5/3,0,0,-4/3},
 3->{-5/4,0,0,-3/4},4->{-5/4,0,0,3/4},
 5->{-5/12,-5/12,0,0},6->{-5/12,5/12,0,0}|>;
qfPhotonAngles[0]=<||>;qfPhotonSquares[0]=<||>;
qfPhotonAngles[1]=<|5->{1,0}|>;qfPhotonSquares[1]=<|5->{-1/2,0}|>;
qfPhotonAngles[2]=<|5->{1,1},6->{1,-1}|>;
qfPhotonSquares[2]=<|5->{-5/12,-5/12},6->{-5/12,5/12}|>;
qfChart[n_,spins_List]:=Module[{momenta,angles,squares},
 momenta=ffP /@ qfMomenta[n];
 angles=Join[AssociationThread[Range[4],UnitVector[2,#]& /@ spins],qfPhotonAngles[n]];
 squares=Join[Association@Table[i->prL.prAdj[momenta[i]].angles[i],{i,4}],qfPhotonSquares[n]];
 <|"Momenta"->momenta,"Angle"->angles,"Square"->squares|>];
qfSwapChart[chart_,permutation_Association]:=Association@Table[
 key->Association@Table[i->chart[key][Lookup[permutation,i,i]],{i,Keys[chart[key]]}],
 {key,{"Momenta","Angle","Square"}}];
qfKnown[0]=Times@@Take[SelectFirst[prFixtureModel[[2]],
 Drop[#,-2]==={"e","E","E","e"}&],-2];
qfKnown[1]=Times@@Take[SelectFirst[prFixtureModel[[2]],
 Drop[#,-2]==={"e","E","E","e","A.+"}&],-2];
qfKnown[2]=Last[QEDSixPointFourFermionSpuriousExample[prFixtureModel,"PP"]];

(* Independently evaluated vector currents test off-shell Ward sums. *)
qfWardValues[n_,chart_]:=Module[{photons=Range[5,n+4],mom=qfMomenta[n],
 matrices=chart["Momenta"],angles=chart["Angle"],squares=chart["Square"],q={2,3},
 insertion,bracket,pole,j,c,v,epsilon,line},
 insertion[p_,i_]:=q.prJ.matrices[p].prL.squares[i];
 bracket[i_]:=q.prJ.angles[i];
 pole[a_,labels_List]:=Det[matrices[a]+Total[matrices /@ labels]]-1;
 j[a_,{}]=1;
 j[a_,labels_List]:=j[a,labels]=Total[Function[i,With[{previous=DeleteCases[labels,i]},
  j[a,previous](insertion[a,i]+Total[insertion[#,i]& /@ previous])/bracket[i]]]/@labels]/pole[a,labels];
 c[a_,b_,labels_List]:=c[a,b,labels]=Total[Function[left,With[{right=Complement[labels,left]},
  (-1)^Length[right]j[a,left]j[b,right]]]/@Subsets[labels]];
 v[a_,b_,labels_List]:=v[a,b,labels]=Total[Function[left,With[{right=Complement[labels,left]},
  (-1)^Length[right]j[a,left]j[b,right](mom[a]+If[left==={},ConstantArray[0,4],Total[mom /@ left]])]]/@Subsets[labels]];
 epsilon[i_]:=ffVector[Sqrt[2]Outer[Times,q,squares[i]]/bracket[i]];
 line[a_,b_,labels_List]:=Module[{k,orbital},
  k=-mom[a]-mom[b]-If[labels==={},ConstantArray[0,4],Total[mom /@ labels]];
  orbital=2v[a,b,labels]-If[labels==={},ConstantArray[0,4],
   Sqrt[2]Total[c[a,b,DeleteCases[labels,#]]epsilon[#]& /@ labels]];
  (angles[a].prJ.angles[b])orbital+c[a,b,labels]
   Table[angles[a].prJ.ffBarSigma[[mu]].prAdj[ffP[k]].angles[b],{mu,4}]];
 Flatten[Table[With[{a=pair[[1]],b=pair[[2]]},With[
  {k=-mom[pair[[1]]]-mom[pair[[2]]]-If[labels==={},ConstantArray[0,4],Total[mom /@ labels]]},
  Together[ffDot[k,line[a,b,labels]]]]],
  {pair,{{1,2},{4,3},{1,3},{4,2}}},{labels,Subsets[photons]}]]];

Do[
 qfPair=QEDFourFermionAllPlusFormula[n,prElectronMass,prElectricCharge];
 qfAmplitude=Last[qfPair];
 qfReferenceAmplitude=Last[QEDFourFermionAllPlusFormula[n,prElectronMass,prElectricCharge,
  "AngleReference"->Spinor["Xi","Angle",0]]];
 check["no default Xi at N="<>ToString[n],FreeQ[qfPair,Spinor["Xi",___]]];
 check["only external mass rules at N="<>ToString[n],FreeQ[First[qfPair],_Multiparticle]];
 Do[
  qfCurrentChart=qfChart[n,qfSpins];
  qfValue=Together[prMapOnChart[qfAmplitude,qfCurrentChart]];
  check["known expression N="<>ToString[n]<>" spins="<>ToString[qfSpins],
   Together[qfValue-prMapOnChart[qfKnown[n],qfCurrentChart]]===0];
  check["electron antisymmetry N="<>ToString[n]<>" spins="<>ToString[qfSpins],
   Together[qfValue+prMapOnChart[qfAmplitude,qfSwapChart[qfCurrentChart,<|1->4,4->1|>]]]===0];
  check["positron antisymmetry N="<>ToString[n]<>" spins="<>ToString[qfSpins],
   Together[qfValue+prMapOnChart[qfAmplitude,qfSwapChart[qfCurrentChart,<|2->3,3->2|>]]]===0];
  check["off-shell current Ward N="<>ToString[n]<>" spins="<>ToString[qfSpins],
   AllTrue[qfWardValues[n,qfCurrentChart],#===0&]];
  If[n>=1,check["reference change N="<>ToString[n]<>" spins="<>ToString[qfSpins],
   Together[qfValue-prMapOnChart[qfReferenceAmplitude,qfCurrentChart]]===0]];
  If[n===2,check["photon Bose symmetry spins="<>ToString[qfSpins],
   Together[qfValue-prMapOnChart[qfAmplitude,qfSwapChart[qfCurrentChart,<|5->6,6->5|>]]]===0]],
 {qfSpins,Tuples[{1,2},4]}],
 {n,0,2}];
check["zero mass rejected",FailureQ[QEDFourFermionAllPlusFormula[1,0,1]]];
check["numerical zero mass rejected",FailureQ[QEDFourFermionAllPlusFormula[1,0.,1]]];
check["negative photon count rejected",FailureQ[QEDFourFermionAllPlusFormula[-1,1,1]]];
check["square reference rejected",FailureQ[QEDFourFermionAllPlusFormula[1,1,1,
 "AngleReference"->Spinor["Xi","Square",0]]]];
check["self-referencing positive photon rejected",FailureQ[QEDFourFermionAllPlusFormula[1,1,1,
 "AngleReference"->Spinor["Helicity","Angle",5]]]];
Print["CHECKS ",checks,"; FAILURES ",Length[failures]];
If[failures==={},Print["FOUR_FERMION_ALL_PLUS_SYMBOLIC_TESTS_PASS"],Print[failures]];
Quit[If[failures==={},0,1]];
