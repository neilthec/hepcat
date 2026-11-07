root=DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Get[FileNameJoin[{root,"source","HEPCAT.wl"}]];$HistoryLength=0;
checks=0;failures={};SetAttributes[check,HoldRest];check[n_,v_]:=(checks++;If[!TrueQ[v],AppendTo[failures,n];Print["FAIL ",n]]);
vars={c1,c2,c3,c4};chart=<|"PoleVariable"->t,"SpinVariables"->{z1,z2},"Masses"-><||>|>;
trial=7z1/t^2+11z2/t+3+c1(z1/t^2+z2/t)+c2(2z1/t^2-z2/t)+c3(3z1/t^2+5z2/t)+c4;
old=ConstructiveDiagrams`Private`completionHEAffineAtomLaurentRows[trial,chart,{},vars,0,10];
new=ConstructiveDiagrams`Private`completionHEAffineChunkLaurentRows[trial,chart,{},vars,0,10,2];
rowAssoc[r_]:=AssociationThread[r["RowKeys"],r["Rows"]];keys=Union[old["RowKeys"],new["RowKeys"]];
check["all Laurent coefficients equal unsplit engine",AllTrue[keys,Together[Lookup[rowAssoc[old],Key[#],0]-Lookup[rowAssoc[new],Key[#],0]]===0&]];
check["complete exact reconstruction and every coordinate",new["Status"]==="COMPLETE"&&new["OriginalCoordinates"]===vars&&new["Certificate"]["WholeChunkReconstructionResidual"]===0&&new["Certificate"]["EveryCoordinateRetainedExactlyOnce"]&&
 (new["Certificate"]["FrozenFamily"]/.new["Certificate"]["OpaqueCoefficientRules"])===trial&&
 Expand[new["Certificate"]["FrozenFamily"]-Total[new["Certificate"]["FrozenChunks"]]]===0];
check["center is included exactly once",new["Certificate"]["CenterIncludedOnce"]&&Together[Lookup[rowAssoc[new],Key[{-2,{1,0}}]]-7-c1-2c2-3c3]===0];
check["no chunk row scaling before key merge",new["Certificate"]["NoNonzeroRowScaling"]];
collision=ConstructiveDiagrams`Private`completionHEAffineChunkLaurentRows[trial,chart,{c1->7},vars,0,10,2];
check["colliding coordinate diagnostic propagates without assigning a coordinate",MatchQ[collision,Missing["TypedAtomParameterCoordinateCollision"]]];
check["nonlinear families decline",MissingQ[ConstructiveDiagrams`Private`completionHEAffineChunkLaurentRows[trial+c1 c2,chart,{},vars,0,5,2]]];
check["unknown denominator declines",MissingQ[ConstructiveDiagrams`Private`completionHEAffineChunkLaurentRows[trial+1/c1,chart,{},vars,0,5,2]]];
check["zero budget declines without partial rows",MissingQ[ConstructiveDiagrams`Private`completionHEAffineChunkLaurentRows[trial,chart,{},vars,0,0,2]]];
g=ModelParameter["EE","chunk-zero-fixture","Real"];
zero=ConstructiveDiagrams`Private`completionHEAffineAtomLaurentRows[(g-3)(c1+c2)z1/t,chart,{g->3},{c1,c2},0,5];
check["literal-zero specialized block is complete with every coordinate",AssociationQ[zero]&&zero["Status"]==="COMPLETE"&&zero["Rows"]==={}&&zero["OriginalCoordinates"]==={c1,c2}&&zero["Certificate"]["SpecializedInputLiterallyZero"]];
withZero=ConstructiveDiagrams`Private`completionHEAffineChunkLaurentRows[(1+c1+c2)z1/t+(g-3)(c3+c4)z1/t,chart,{g->3},vars,0,10,2];
check["one zero block never discards the other block or its coordinates",AssociationQ[withZero]&&withZero["CompletedChunks"]===2&&withZero["OriginalCoordinates"]===vars&&withZero["Chunks"][[2]]["AtomCount"]===0&&withZero["Rows"]==={1+c1+c2}];
check["one shared deadline and complete block certificates are retained",new["Certificate"]["OneSharedDeadline"]&&new["Certificate"]["AllBlocksComplete"]&&new["CompletedChunks"]===new["ChunkCount"]&&new["ChartTimeConstraint"]===10];
clock=AbsoluteTime[];
limited=Internal`InheritedBlock[{ConstructiveDiagrams`Private`completionHEAffineAtomLaurentRows},
 Clear[ConstructiveDiagrams`Private`completionHEAffineAtomLaurentRows];
 ConstructiveDiagrams`Private`completionHEAffineAtomLaurentRows[___]:=(Pause[1];<||>);
 ConstructiveDiagrams`Private`completionHEAffineChunkLaurentRows[trial,chart,{},vars,0,1/20,2]];
check["hard shared timeout covers unfinished child work without partial rows",MissingQ[limited]&&AbsoluteTime[]-clock<1/2];
failedChunkResults=Internal`InheritedBlock[{
 ConstructiveDiagrams`Private`completionHEAffineAtomLaurentEligibleQ,
 ConstructiveDiagrams`Private`completionHEAffineAtomLaurentRows,
 ConstructiveDiagrams`Private`completionHEAffineChunkLaurentRows,
 ConstructiveDiagrams`Private`highEnergyConstraintChart,
 ConstructiveDiagrams`Private`residueEvaluateChart},
 Clear[ConstructiveDiagrams`Private`completionHEAffineAtomLaurentEligibleQ,
  ConstructiveDiagrams`Private`completionHEAffineAtomLaurentRows,
  ConstructiveDiagrams`Private`completionHEAffineChunkLaurentRows,
  ConstructiveDiagrams`Private`highEnergyConstraintChart,
  ConstructiveDiagrams`Private`residueEvaluateChart];
 ConstructiveDiagrams`Private`completionHEAffineAtomLaurentEligibleQ[___]:=True;
 ConstructiveDiagrams`Private`highEnergyConstraintChart[___]:=chart;
 ConstructiveDiagrams`Private`completionHEAffineAtomLaurentRows[___]:=Missing["TypedAtomAffineGuard"];
 ConstructiveDiagrams`Private`completionHEAffineChunkLaurentRows[___]:=chunkFailureCause;
 ConstructiveDiagrams`Private`residueEvaluateChart[___]:=(wholeFallbackCalls++;
  Throw[Failure["UnexpectedWholeFamilyMapping",<||>]]);
 wholeFallbackCalls=0;
 Table[chunkFailureCause=cause;
  ConstructiveDiagrams`Private`completionHEEnergyConstraints[trial,vars,
   <|"ExternalLegs"->{1,2,3,4},"ExternalMassRules"->{}|>,1,31,{},0,None,False],
  {cause,{Missing["AffineChunkMemoryGuard"],Missing["AffineChunkTimeConstraint"]}}]];
check["failed oversized chunk fit is terminal with cause and original coordinates",
 wholeFallbackCalls===0&&AllTrue[failedChunkResults,
 AssociationQ[#]&&#["Status"]==="UNRESOLVED"&&#["Equations"]==={}&&
 MatchQ[First[#["Diagnostics"]],Failure["IncompleteAffineBlockFitting",_Association]]&&
 First[#["Diagnostics"]][[2]]["OriginalCorrectionCoordinates"]===vars&&
 First[#["Diagnostics"]][[2]]["NoWholeFamilyMappingAttempted"]&&
 MemberQ[{Missing["AffineChunkMemoryGuard"],Missing["AffineChunkTimeConstraint"]},
  First[#["Diagnostics"]][[2]]["Cause"]]&]];

Print["CHECKS: ",checks,"; FAILURES: ",Length[failures]];If[failures=!={},Print[failures]];Print["HEPCAT_TEST_COMPLETED:high-energy-affine-chunk-tests.wl"];Quit[If[failures==={},0,1]];
