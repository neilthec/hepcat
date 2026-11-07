(* Exact fitting specialization preserves symbolic families and chart coordinates. *)
root=DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Get[FileNameJoin[{root,"source","HEPCAT.wl"}]];$HistoryLength=0;
checks=0;failures={};SetAttributes[check,HoldRest];check[n_,v_]:=(checks++;If[!TrueQ[v],AppendTo[failures,n];Print["FAIL ",n]]);
new=ConstructiveDiagrams`Private`completionHEAffineAtomLaurentRows;
chart=<|"PoleVariable"->t,"SpinVariables"->{z1,z2},"Masses"-><||>|>;vars={c1,c2,c3};g=ModelParameter["EE","specialization-a","Real"];h=ModelParameter["Me","specialization-a","Real"];
f=z1/t^2+z2/t;trial=(1+c1)(g+h)f+(2+c2)(2g-h)f+c3 g h f;saved=trial;rules={g->3/5,h->7/11};
b=new[trial,chart,rules,vars,0,10];
expected=Together[(1+c1)(3/5+7/11)+(2+c2)(6/5-7/11)+c3(21/55)];
check["specialized engine yields every independently known exact Laurent row",
 AssociationQ[b]&&b["Status"]==="COMPLETE"&&
 Sort[b["RowKeys"]]===Sort[{{-2,{1,0}},{-1,{0,1}}}]&&
 AllTrue[b["Rows"],Together[#-expected]===0&]];
check["fixed fitting specialization merges repeated scalar-prefactor atoms",b["AtomCount"]===2];
check["full symbolic input and correction coordinates are preserved",trial===saved&&b["OriginalCoordinates"]===vars&&b["Certificate"]["AllOriginalCoordinatesRetained"]&&b["Certificate"]["EverySpecializedAffinePartReconstructed"]];
check["certificate records assignment scope without generic symbolic equality",b["FittingParameterSpecialization"]["ScopedParameterRules"]===rules&&!b["FittingParameterSpecialization"]["AcrossParameterAssignmentIdentityClaimed"]&&b["Certificate"]["ReconstructionScope"]==="OriginalAffineFamilyAtRecordedFittingAssignment"];
check["affine coordinate rule collision safely declines",MatchQ[new[trial,chart,{c1->7},vars,0,5],Missing["TypedAtomParameterCoordinateCollision"]]];
check["affine coordinate on rule right-hand side safely declines",MatchQ[new[trial,chart,{g->c1},vars,0,5],Missing["TypedAtomParameterCoordinateCollision"]]];
check["energy coordinate rules safely decline",MatchQ[new[trial,chart,{t->7},vars,0,5],Missing["TypedAtomParameterCoordinateCollision"]]];
check["coherent spin coordinate rules safely decline",MatchQ[new[trial,chart,{z1->7},vars,0,5],Missing["TypedAtomParameterCoordinateCollision"]]];
check["integer label replacements are never accepted as scalar parameters",MatchQ[new[trial,chart,{2->7},vars,0,5],Missing["TypedAtomParameterCoordinateCollision"]]];
check["inexact fitting assignments are not silently exactified",MissingQ[new[trial,chart,{g->.3,h->.7},vars,0,5]]];
check["symbolic chained assignments retain the old fallback",MatchQ[new[trial,chart,{g->h,h->3},vars,0,5],Missing["NonNumericTypedAtomFittingAssignment"]]];
check["nonfinite fitting assignments are rejected",MissingQ[new[trial,chart,{g->Infinity,h->3},vars,0,5]]];
aSpin[i_]:=Spinor["Spin","Angle",i];chain=SpinorChain[aSpin[1],aSpin[2]];
typedChart=<|"PoleVariable"->t,"SpinVariables"->{z1,z2},"Masses"-><|1->2,2->7|>,"Angle"-><|1->{1,z1},2->{z2,1}|>,"Square"-><|1->{1,z1},2->{z2,1}|>,"Momenta"-><|1->{{2,0},{0,1}},2->{{1,1},{1,2}}|>|>;
mt=(1+c1)(Mass[2]+h)chain^2/t^2;mr=new[mt,typedChart,{h->3},{c1},0,10];ra=AssociationThread[mr["RowKeys"],mr["Rows"]];
check["typed external masses do not rewrite integer spinor labels",AssociationQ[mr]&&Expand[Lookup[ra,{{-2,{0,0}},{-2,{1,1}},{-2,{2,2}}}]-10(1+c1){1,-2,1}]==={0,0,0}&&mr["FittingParameterSpecialization"]["TypedExternalMassRules"]==={Mass[1]->2,Mass[2]->7}];
check["mass metadata depending on affine coordinates safely declines",MatchQ[new[mt,Join[typedChart,<|"Masses"-><|1->2,2->c1|>|>],{h->3},{c1},0,5],Missing["TypedAtomMassCoordinateCollision"]]];
foreign=ModelParameter["EE","specialization-b","Real"];
check["model-scoped parameter names never match a foreign model",!AssociationQ[new[(1+c1)foreign f,chart,{g->3},{c1},0,5]]];
short=new[(1+c1)f,chart,{}, {c1},0,4];long=new[(1+c1)f,chart,{}, {c1},0,500];
check["explicit shorter chart budgets are retained",short["ChartTimeConstraint"]===4&&short["ChartWorkloadEstimate"]["RequestedChartTimeConstraint"]===4];
check["workload estimate remains within a finite 240 second cap",long["ChartTimeConstraint"]===240&&long["ChartWorkloadBudget"]<=240&&long["ChartWorkloadBudget"]>=60&&long["ChartWorkloadEstimate"]["AtomCount"]===long["AtomCount"]];
allowed=new[(1+c1)g z1 t^2,chart,{g->3},{c1},2,5];
check["empty forbidden-row result still records exact specialization scope",AssociationQ[allowed]&&allowed["Rows"]==={}&&allowed["FittingParameterSpecialization"]["OriginalCoordinates"]==={c1}];
(* A collision is terminal before the whole-family mapping fallback. *)
collisionCallerResults=Internal`InheritedBlock[{
 ConstructiveDiagrams`Private`completionHEAffineAtomLaurentEligibleQ,
 ConstructiveDiagrams`Private`completionHEAffineAtomLaurentRows,
 ConstructiveDiagrams`Private`highEnergyConstraintChart,
 ConstructiveDiagrams`Private`residueEvaluateChart},
 Clear[ConstructiveDiagrams`Private`completionHEAffineAtomLaurentEligibleQ,
  ConstructiveDiagrams`Private`completionHEAffineAtomLaurentRows,
  ConstructiveDiagrams`Private`highEnergyConstraintChart,
  ConstructiveDiagrams`Private`residueEvaluateChart];
 ConstructiveDiagrams`Private`completionHEAffineAtomLaurentEligibleQ[___]:=True;
 ConstructiveDiagrams`Private`highEnergyConstraintChart[___]:=chart;
 ConstructiveDiagrams`Private`completionHEAffineAtomLaurentRows[___]:=collisionReason;
 ConstructiveDiagrams`Private`residueEvaluateChart[___]:=(collisionWholeMapCalls++;
  Throw[Failure["UnexpectedWholeFamilyMapping",<||>]]);
 collisionWholeMapCalls=0;
 Table[collisionReason=reason;
  ConstructiveDiagrams`Private`completionHEEnergyConstraints[
   1+c1/t,{c1},<|"ExternalLegs"->{1,2,3,4},"ExternalMassRules"->{}|>,
   1,31,{c1->7},0,None,False],
  {reason,{Missing["TypedAtomParameterCoordinateCollision"],
    Missing["TypedAtomMassCoordinateCollision"]}}]];
check["coordinate collisions cannot enter whole-family mapping fallback",
 collisionWholeMapCalls===0&&AllTrue[collisionCallerResults,
 AssociationQ[#]&&#["Status"]==="UNRESOLVED"&&#["Equations"]==={}&&
 !TrueQ[Lookup[#,"PointReady",False]]&&
 MatchQ[First[#["Diagnostics"]],Failure["FittingCoordinateCollision",_Association]]&&
 First[#["Diagnostics"]][[2]]["NoWholeFamilyMappingAttempted"]&]];

indexedUnknown=indexedCoefficient[1];indexedTrial=(1+indexedUnknown)f;indexedSaved=indexedTrial;
check["parameter rules cannot rewrite an indexed coordinate head",
 MatchQ[new[indexedTrial,chart,{indexedCoefficient->7},{indexedUnknown},0,5],
  Missing["TypedAtomParameterCoordinateCollision"]]&&indexedTrial===indexedSaved];
massIndexedUnknown=indexedCoefficient[Mass[1]];
check["typed mass substitution cannot rewrite an indexed coordinate argument",
 MatchQ[new[(1+massIndexedUnknown)f,Join[chart,<|"Masses"-><|1->3|>|>],{},
  {massIndexedUnknown},0,5],Missing["TypedAtomMassCoordinateCollision"]]];
indexedCaller=Internal`InheritedBlock[{
 ConstructiveDiagrams`Private`completionHEAffineAtomLaurentEligibleQ,
 ConstructiveDiagrams`Private`highEnergyConstraintChart,
 ConstructiveDiagrams`Private`residueEvaluateChart},
 Clear[ConstructiveDiagrams`Private`completionHEAffineAtomLaurentEligibleQ,
  ConstructiveDiagrams`Private`highEnergyConstraintChart,
  ConstructiveDiagrams`Private`residueEvaluateChart];
 ConstructiveDiagrams`Private`completionHEAffineAtomLaurentEligibleQ[___]:=True;
 ConstructiveDiagrams`Private`highEnergyConstraintChart[___]:=chart;
 ConstructiveDiagrams`Private`residueEvaluateChart[___]:=(indexedWholeMapCalls++;
  Throw[Failure["UnexpectedWholeFamilyMapping",<||>]]);
 indexedWholeMapCalls=0;
 ConstructiveDiagrams`Private`completionHEEnergyConstraints[indexedTrial,{indexedUnknown},
  <|"ExternalLegs"->{1,2,3,4},"ExternalMassRules"->{}|>,1,31,
  {indexedCoefficient->7},0,None,False]];
check["indexed-head collision is terminal and preserves its original coordinate",
 indexedWholeMapCalls===0&&AssociationQ[indexedCaller]&&indexedCaller["Status"]==="UNRESOLVED"&&
 First[indexedCaller["Diagnostics"]][[2]]["OriginalCorrectionCoordinates"]==={indexedUnknown}&&
 First[indexedCaller["Diagnostics"]][[2]]["Cause"]===Missing["TypedAtomParameterCoordinateCollision"]];

Print["CHECKS: ",checks,"; FAILURES: ",Length[failures]];If[failures=!={},Print[failures]];Print["HEPCAT_TEST_COMPLETED:high-energy-fitting-specialization-tests.wl"];Quit[If[failures==={},0,1]];
