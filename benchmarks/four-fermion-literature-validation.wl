(* Independent published-amplitude controls for two massive fermion pairs.
   Source: Lazopoulos/Ochirov/Shi, arXiv:2111.06847v2 Eq. (3.8)
   and Appendix B four-quark seed; complete one-photon Abelianization
   from Johansson/Ochirov, arXiv:1507.00332v3 Eq. (3.14).
   No diagram generation, completion, internal on-shell identities, fitting
   to the candidate expression, or model amplitude is used as the oracle.

   LOS momenta are all outgoing. Evaluate its analytic continuation on the
   same conserving chart used for HEPCAT, with labels (1,2,3,4) -> (1,2,4,3).
   The LOS rest-frame square spinors (Appendix A) have the opposite sign
   to the HEPCAT massive chart (their Eq. (A.12)): square_LOS = -square_HEPCAT for a massive
   endpoint; massless square spinors agree. This is a convention conversion,
   not a physical helicity flip. Without it Eq. (3.8)'s mass term is wrong.
   Their vertex is g/sqrt(2): multiply the stripped result by (sqrt(2)e)^(N+2).
   The single global -i converts their A convention to our invariant amplitude.
   The same fixed conversion is used at every spin, mass, charge and chart.

   Full QED N=1 amplitude = A(1,5,2,3,4)+A(1,2,3,5,4), since c2=c3=1,
   c5=0. Cyclicity makes the first term A(3,4,1,5,2). Then subtract the
   identical-antifermion relabeling. Eq. (3.10) alone is NOT a complete
   Abelianization at N>=2; additional gluon placements are required.
   This file deliberately makes no N>=2 published-formula validation claim. *)
root=DirectoryName[DirectoryName[$InputFileName]];
Get[FileNameJoin[{root,"source","HEPCAT.wl"}]];
Get[FileNameJoin[{root,"tests","QED-four-fermion-all-plus-formula.wl"}]];
$HistoryLength=0;
ClearAll[flAngle,flSquare,flChain,flProp,flSeed,flPrimitivePart,flPrimitive,
 flPublished,flChart,flChainValue,flMap];
flConvertMassiveSquares=True;
flAngle[i_]:=Spinor[If[i<=4,"Spin","Helicity"],"Angle",i];
flSquare[i_]:=Spinor[If[i<=4,"Spin","Helicity"],"Square",i];
flChain[parts__]:=If[TrueQ[flConvertMassiveSquares],
 (-1)^Count[{First[{parts}],Last[{parts}]},Spinor["Spin","Square",_]],1]*SpinorChain[parts];
flProp[ids_List,mass_]:=PropDen[Mom[Multiparticle@@Sort[ids]],mass];
(* Direct transcription of the published four-quark seed. *)
flSeed[a_,b_,c_,d_]:=-I/flProp[{a,b},0]*(
 flChain[flAngle[a],flAngle[c]]flChain[flSquare[b],flSquare[d]]+
 flChain[flAngle[a],flAngle[d]]flChain[flSquare[b],flSquare[c]]+
 flChain[flSquare[a],flSquare[c]]flChain[flAngle[b],flAngle[d]]+
 flChain[flSquare[a],flSquare[d]]flChain[flAngle[b],flAngle[c]]);
(* Direct transcription of Eq. (3.8); P45 is explicitly p4+p5. *)
flPrimitivePart[a_,b_,c_,d_,g_,mass_]:=
 flChain[flAngle[a],flAngle[d]]flChain[flSquare[b],flSquare[c]]flChain[flSquare[g],Mom[c],Mom[d],flSquare[g]]-
 flChain[flAngle[a],flAngle[d]]flChain[flSquare[b],flSquare[g]]flChain[flSquare[c],flSquare[g]]flProp[{d,g},mass]+
 mass flChain[flAngle[a],flAngle[c]]flChain[flSquare[b],flSquare[g]]flChain[flAngle[d],Mom[c],flSquare[g]]+
 flChain[flAngle[a],flAngle[c]](flChain[flSquare[b],Mom[d],Mom[c],flSquare[g]]+
 flChain[flSquare[b],Mom[g],Mom[c],flSquare[g]])flChain[flSquare[d],flSquare[g]];
flPrimitive[a_,b_,c_,d_,g_,mass_]:=-I*(
 flPrimitivePart[a,b,c,d,g,mass]+flPrimitivePart[b,a,c,d,g,mass])/
 (flProp[{a,b},0]flProp[{c,g},mass]flProp[{d,g},mass]);
flPublished[0]:=-I*(Sqrt[2]flCharge)^2*(flSeed[1,2,4,3]-flSeed[1,3,4,2]);
flPublished[1]:=-I*(Sqrt[2]flCharge)^3*(
 flPrimitive[1,2,4,3,5,flMass]+flPrimitive[4,3,1,2,5,flMass]-
 flPrimitive[1,3,4,2,5,flMass]-flPrimitive[4,2,1,3,5,flMass]);
flSigma={IdentityMatrix[2],{{0,1},{1,0}},{{0,-I},{I,0}},{{1,0},{0,-1}}};
flJ={{0,-1},{1,0}};flL=-flJ;
flP[v_]:=Total[MapThread[Times,{v,flSigma}]];
flAdj[p_]:={{p[[2,2]],-p[[1,2]]},{-p[[2,1]],p[[1,1]]}};
flBaseMomenta[0]=<|1->{5/4,0,0,3/4},2->{5/4,0,0,-3/4},
 3->{-5/4,-3/5,0,-9/20},4->{-5/4,3/5,0,9/20}|>;
flBaseMomenta[1]=<|1->{5/4,0,0,3/4},2->{5/4,0,0,-3/4},
 3->{-9/8,-1/2,0,1/8},4->{-9/8,1/2,0,1/8},5->{-1/4,0,0,-1/4}|>;
flPhotonAngles[0]=<||>;flPhotonSquares[0]=<||>;
flPhotonAngles[1]=<|5->{1,0}|>;flPhotonSquares[1]=<|5->{-1/2,0}|>;
(* Three exact SL(2,C) charts, including complex entries and nonzero p_y. *)
flTransforms={IdentityMatrix[2],{{1,1/3},{2/5,17/15}},{{1,I/3},{2/5,1+2I/15}}};
flParameters={{1,1/3},{2/5,2/7},{7/3,-1/4}};
flChart[n_,spins_List,mass_,transform_]:=Module[{momenta,angles,squares},
 momenta=Map[Function[v,mass transform.flP[v].ConjugateTranspose[transform]],flBaseMomenta[n]];
 angles=Join[AssociationThread[Range[4],(Sqrt[mass]transform.UnitVector[2,#])&/@spins],
  Map[Sqrt[mass]transform.#&,flPhotonAngles[n]]];
 squares=Join[Association@Table[i->flL.flAdj[momenta[i]].angles[i]/mass,{i,4}],
  Map[Sqrt[mass]Conjugate[transform].#&,flPhotonSquares[n]]];
 <|"Momenta"->momenta,"Angle"->angles,"Square"->squares|>];
flMomentum[i_Integer,chart_]:=chart["Momenta"][i];
flMomentum[i_Multiparticle,chart_]:=Total[flMomentum[#,chart]&/@List@@i];
flChainValue[chain_SpinorChain,chart_]:=Module[
 {parts=List@@chain,first,last,kind,row,matrices},
 first=First[parts];last=Last[parts];kind=first[[2]];
 row=If[kind==="Angle",chart[kind][first[[3]]].flJ,chart[kind][first[[3]]]];
 matrices=MapIndexed[Function[{insertion,position},With[{p=flMomentum[insertion[[1]],chart]},
  If[OddQ[First[position]]===(kind==="Angle"),p,flAdj[p]]]],parts[[2;;-2]]];
 Fold[Dot,row,matrices].If[last[[2]]==="Square",flL.chart["Square"][last[[3]]],chart["Angle"][last[[3]]]]];
flMap[expr_,chart_,mass_,charge_]:=Module[{mapped},
 mapped=expr/.{flMass->mass,flCharge->charge};
 mapped=mapped/.chain_SpinorChain:>flChainValue[chain,chart];
 mapped/.{PropDen[Mom[index_],poleMass_]:>Det[flMomentum[index,chart]]-poleMass^2,
 MomProd[i_,j_]:>(Det[flMomentum[i,chart]+flMomentum[j,chart]]-
  Det[flMomentum[i,chart]]-Det[flMomentum[j,chart]])/2}];
checks=0;failures={};negativeControls={};
Do[
 candidate=Last[QEDFourFermionAllPlusFormula[n,flMass,flCharge]];
 published=flPublished[n];
 Do[
  mass=parameters[[1]];charge=parameters[[2]];
  chart=flChart[n,spins,mass,transform];
  checks++;
  If[!TrueQ[AllTrue[Range[4],Together[Det[chart["Momenta"][#]]-mass^2]===0&]&&
   AllTrue[Range[5,n+4],Together[Det[chart["Momenta"][#]]]===0&]&&
   Total[Values[chart["Momenta"]]]===ConstantArray[0,{2,2}]],AppendTo[failures,{"Kinematics",n,parameters,spins}]];
  difference=Together[flMap[candidate-published,chart,mass,charge]];
  checks++;
  If[difference=!=0,AppendTo[failures,{"Amplitude",n,parameters,spins,difference}]],
 {parameters,flParameters},{transform,flTransforms},{spins,Tuples[{1,2},4]}];
 Print["Published amplitude N=",n,": completed 144 exact coherent-spin comparisons."],{n,0,1}];
(* Expected failures demonstrate that the second radiating line and the
   massive-spinor convention conversion are both necessary, not cosmetic. *)
chart=flChart[1,{1,1,1,2},1,IdentityMatrix[2]];
full=flMap[flPublished[1],chart,1,1/3];
missingLine=-I*(Sqrt[2]flCharge)^3*(flPrimitive[1,2,4,3,5,flMass]-flPrimitive[1,3,4,2,5,flMass]);
AppendTo[negativeControls,"MissingRadiatingLineDetected"->TrueQ[Together[full-flMap[missingLine,chart,1,1/3]]=!=0]];
flConvertMassiveSquares=False;unconverted=flPublished[1];flConvertMassiveSquares=True;
AppendTo[negativeControls,"MassiveSquareConventionDetected"->TrueQ[Together[full-flMap[unconverted,chart,1,1/3]]=!=0]];
If[!AllTrue[Last/@negativeControls,TrueQ],AppendTo[failures,{"NegativeControls",negativeControls}]];
report=<|"Sources"->{"https://arxiv.org/abs/2111.06847","https://arxiv.org/abs/1507.00332"},
 "Scope"->"N=0 and N=1 complete QED amplitudes; no N>=2 claim",
 "ExactAmplitudeComparisons"->288,"ExactKinematicChecks"->288,
 "MassChargePairsExact"->Map[ToString[#,InputForm]&,flParameters,{2}],"SL2CCharts"->3,"CoherentSpinComponents"->16,
 "Failures"->failures,"NegativeControls"->Association[negativeControls],
 "InternalOnShellConditionsUsed"->False,"WolframVersion"->$Version|>;
Print["FOUR_FERMION_LITERATURE_",If[failures==={},"PASS","FAIL"],": ",checks," exact checks; ",Length[failures]," failures."];
flReportDirectory=FileNameJoin[{DirectoryName[$InputFileName],"four-fermion-literature"}];
If[!DirectoryQ[flReportDirectory],CreateDirectory[flReportDirectory]];
Export[FileNameJoin[{flReportDirectory,"validation-summary.json"}],report,"RawJSON"];
Quit[If[failures==={},0,1]];
