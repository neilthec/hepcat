(* Exact jets must retain delayed leading powers and every affine spin row. *)
root=DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];Get[FileNameJoin[{root,"source","HEPCAT.wl"}]];
checks=0;failures={};check[name_,test_]:=(checks++;If[!TrueQ[test],AppendTo[failures,name];Print["FAIL: ",name]]);
{jt,ju,jv,jw,jz,jx,jy}=Table[Unique["jetTest"],{7}];js={ju,jv,jw,jz};
jet[expr_,spins_:js,unknowns_:{jx,jy},threshold_:0]:=ConstructiveDiagrams`Private`completionHELaurentJetRows[expr,jt,spins,unknowns,threshold];
space[rows_,unknowns_]:=If[rows==={}, {},Module[{a=CoefficientArrays[rows,unknowns]},DeleteCases[RowReduce[Join[List/@Normal[First[a]],Normal[If[Length[a]===2,Last[a],ConstantArray[0,{Length[rows],Length[unknowns]}]]],2]],ConstantArray[0,Length[unknowns]+1]]]];
r=jet[((jx+jy)ju+(jx+2)jv)/jt^6];
check["coupled affine rows retain constant offset",AssociationQ[r]&&space[r["Rows"],{jx,jy}]===space[{jx+jy,jx+2},{jx,jy}]];
check["complete field and spin certificate",AssociationQ[r]&&r["FullCoefficientCoverage"]&&r["FullAffineReconstruction"]&&r["Certificate"]["AllSpinCoefficientsCovered"]&&r["Certificate"]["RemainderOrdersPreserved"]&&r["Certificate"]["ValuationLowerBoundsPreserved"]&&r["Certificate"]["NormalOnlyAfterAllCoverage"]];
check["first allowed power contributes no forbidden rows",jet[(jx ju+jy jv)jt^4,js,{jx,jy},4]["Rows"]==={}];
check["zero family includes its constant coefficient",jet[0]["ComponentCount"]===3&&jet[0]["Rows"]==={}];
check["indexed coefficient coordinates remain exact",space[jet[(jc[1]+jc[2]+1)ju/jt,js,{jc[1],jc[2]}]["Rows"],{jc[1],jc[2]}]===space[{jc[1]+jc[2]+1},{jc[1],jc[2]}]];
check["nonlinear unknown products rejected",FailureQ[jet[jx jy ju/jt]]];
check["unknown denominator rejected",FailureQ[Quiet[jet[ju/(jx jt)]]]];
check["spin dependent denominator rejected",FailureQ[jet[(jx+jy)/(ju+jt)]]];
check["nonpolynomial spin dependence rejected",FailureQ[jet[jx Sqrt[ju+jt]]]];
check["independent positive valuation remains above forbidden boundary",jet[(jx+1)(ju jt+jv jt/Sqrt[1+jt^4])^2]["Rows"]==={}];
delayed=jet[(jx+1)(ju+jt^10)(ju+jt^-12)];
check["positive power delayed into a negative coefficient survives",AssociationQ[delayed]&&MemberQ[delayed["RowKeys"],{-2,ConstantArray[0,4]}]&&space[delayed["Rows"],{jx,jy}]===space[{jx+1},{jx,jy}]];
delayedPower=jet[(jx+1)(ju+jt^10)^2(jv+jt^-24)];
check["positive integer powers preserve later negative-power demand",AssociationQ[delayedPower]&&MemberQ[delayedPower["RowKeys"],{-4,ConstantArray[0,4]}]];
check["arbitrarily tiny exact row retained",space[jet[(jx+10^-100)ju/jt]["Rows"],{jx,jy}]===space[{jx+10^-100},{jx,jy}]];
check["root power threshold permits only exact integer Laurent powers",FailureQ[jet[(jx+1)ju/Sqrt[jt]]]];
(* Random exact coefficient examples are compared with independent native
   coefficient extraction of the COMPLETE small expression. *)
BlockRandom[SeedRandom[812381];Do[
 a=RandomInteger[{-3,3},{4,3}];basis={1,ju,ju jv,jw^2 jz};
 expression=Total[(a.{1,jx,jy})basis](1+jt^3)/((1+2jt)jt^2 Sqrt[1+jt^4]);
 actual=jet[expression,js,{jx,jy},1];normal=Normal[Series[expression,{jt,0,1}]];
 expected=Flatten[Table[Last/@CoefficientRules[Coefficient[normal,jt,k],js],{k,-2,0}]];
 check["full native affine and spin oracle "<>ToString[i],AssociationQ[actual]&&space[actual["Rows"],{jx,jy}]===space[expected,{jx,jy}]],{i,8}]];
(* Incomplete O terms or assumed numeric zeros cannot silently become rows. *)
partial=Internal`InheritedBlock[{ConstructiveDiagrams`Private`highEnergyCertifiedScalarSeries},Clear[ConstructiveDiagrams`Private`highEnergyCertifiedScalarSeries];ConstructiveDiagrams`Private`highEnergyCertifiedScalarSeries[_,v_,q_]:=<|"Status"->"COMPLETE","Series"->SeriesData[v,0,{},-1,-1,1],"ZeroAssumptionCertificates"->{}|>;jet[(jx+1)ju/jt]];
check["incomplete Laurent coefficient coverage is rejected",FailureQ[partial]&&partial[[1]]==="JetIncompleteCoverage"];
failed=Internal`InheritedBlock[{ConstructiveDiagrams`Private`highEnergyCertifiedScalarSeries},Clear[ConstructiveDiagrams`Private`highEnergyCertifiedScalarSeries];ConstructiveDiagrams`Private`highEnergyCertifiedScalarSeries[___]:=<|"Status"->"UNRESOLVED","Reason"->"UnprovedSeriesZeroAssumption"|>;jet[(jx+1)ju/jt]];
check["uncertified atom series is terminal",FailureQ[failed]&&failed[[1]]==="JetUncertifiedAtomSeries"];
(* Both production entry points use the SAME certified coefficient field.
   Force admission on a small exact fixture to exercise these interfaces. *)
public=Internal`InheritedBlock[{ConstructiveDiagrams`Private`highEnergyLaurentJetEligibleQ},
 Clear[ConstructiveDiagrams`Private`highEnergyLaurentJetEligibleQ];
 ConstructiveDiagrams`Private`highEnergyLaurentJetEligibleQ[___]:=True;
 ConstructiveDiagrams`Private`highEnergyScalarCoefficientSeries[(1+ju+jv)^2/((1+jt)jt^2),jt,{ju,jv},0]];
check["fixed-candidate checker uses the shared exact jet engine",public["Method"]==="ExactLaurentJetsBeforeCoherentSpinConvolution"&&Expand[public["Series"]-Normal[Series[(1+ju+jv)^2/((1+jt)jt^2),{jt,0,0}]]]===0];
check["fixed checker keeps every coherent component including constant",public["Polynomial"]["FullCoefficientCoverage"]&&public["Polynomial"]["ReconstructionVerified"]&&public["Polynomial"]["ConstantCoefficientIncluded"]&&public["CompletedComponents"]===6&&public["NumericalSpinSamples"]===0];
entry=Internal`InheritedBlock[{ConstructiveDiagrams`Private`highEnergyLaurentJetEligibleQ},
 Clear[ConstructiveDiagrams`Private`highEnergyLaurentJetEligibleQ];
 ConstructiveDiagrams`Private`highEnergyLaurentJetEligibleQ[___]:=True;
 ConstructiveDiagrams`Private`completionHELaurentRows[((jx+jy)ju+(jx+2)jv)/jt,jt,js,{jx,jy},0]];
check["fitting entry point returns the identical coupled row space",space[entry,{jx,jy}]===space[{jx+jy,jx+2},{jx,jy}]];
terminal=Internal`InheritedBlock[{ConstructiveDiagrams`Private`highEnergyLaurentJetEligibleQ,ConstructiveDiagrams`Private`highEnergyLaurentJetExpansion,ConstructiveDiagrams`Private`highEnergyCertifiedScalarSeries},
 Clear[ConstructiveDiagrams`Private`highEnergyLaurentJetEligibleQ,ConstructiveDiagrams`Private`highEnergyLaurentJetExpansion,ConstructiveDiagrams`Private`highEnergyCertifiedScalarSeries];
 ConstructiveDiagrams`Private`highEnergyLaurentJetEligibleQ[___]:=True;
 ConstructiveDiagrams`Private`highEnergyLaurentJetExpansion[___]:=Failure["JetUncertifiedAtomSeries",<||>];
 jetFallbackCalls=0;ConstructiveDiagrams`Private`highEnergyCertifiedScalarSeries[___]:=(jetFallbackCalls++;<|"Status"->"COMPLETE","Series"->0,"ZeroAssumptionCertificates"->{}|>);
 {ConstructiveDiagrams`Private`highEnergyScalarCoefficientSeries[(ju+1)/jt,jt,{ju},0],ConstructiveDiagrams`Private`completionHELaurentRows[(jx+1)ju/jt,jt,js,{jx},0],jetFallbackCalls}];
check["unproved scalar assumptions never fall through to an accepting route",terminal[[1]]["Status"]==="UNRESOLVED"&&FailureQ[terminal[[2]]]&&terminal[[3]]===0];
(* Admission is based on actual tensor work, not process identity. *)
check["small QED-like tensors retain their established path",!ConstructiveDiagrams`Private`highEnergyLaurentJetEligibleQ[(1+ju)(1+jv)(1+jw)(1+jz),js]];
(* Use a large scalar leaf inventory without expanding a spin polynomial.
   Only the generic admission predicate is exercised on this inert field. *)
largeJetField=jetGateCoefficient@@Range[500000];
fermionSpins=Table[Unique["jetDegreeOneSpin"],{6}];
fermionTensor=largeJetField Times@@(1+fermionSpins);
check["six degree-one legs have64components but retain the established engine",
 ConstructiveDiagrams`Private`completionHECoherentComponentBound[fermionTensor,fermionSpins]===64 &&
 !ConstructiveDiagrams`Private`highEnergyLaurentJetEligibleQ[fermionTensor,fermionSpins]];
check["three degree-two legs qualify structurally for the new engine",
 ConstructiveDiagrams`Private`highEnergyLaurentJetEligibleQ[largeJetField(1+ju)^2(1+jv)^2(1+jw)^2,{ju,jv,jw}]];
check["mixed degree-one and degree-two legs qualify without species names",
 ConstructiveDiagrams`Private`highEnergyLaurentJetEligibleQ[largeJetField(1+ju)(1+jv)(1+jw)^2(1+jz)^2,js]];
check["small degree-two tensors still retain the established engine",
 !ConstructiveDiagrams`Private`highEnergyLaurentJetEligibleQ[(1+ju)^2(1+jv)^2(1+jw)^2,{ju,jv,jw}]];
Clear[largeJetField,fermionTensor];
jetBoundaryTensor=(1+ju)^2(1+jv)^2(1+jw)^2;
jetBoundaryFixture[count_Integer]:=(jetBoundaryField@@Range[count-LeafCount[jetBoundaryTensor]-1])jetBoundaryTensor;
belowBoundary=jetBoundaryFixture[99999];atBoundary=jetBoundaryFixture[100000];
check["threshold fixtures have exactly the intended mapped leaf inventories",
 LeafCount[belowBoundary]===99999&&LeafCount[atBoundary]===100000];
check["a higher-spin tensor below the measured work threshold retains the old route",
 !ConstructiveDiagrams`Private`highEnergyLaurentJetEligibleQ[belowBoundary,{ju,jv,jw}]];
check["a higher-spin tensor at the measured threshold admits certified jets",
 ConstructiveDiagrams`Private`highEnergyLaurentJetEligibleQ[atBoundary,{ju,jv,jw}]];
Clear[belowBoundary,atBoundary];
limited=ConstructiveDiagrams`Private`highEnergyLaurentJetCoefficients[Sum[jc[i]ju,{i,18}],jt,js,Table[jc[i],{i,18}],0,12,1];
check["bounded dense-affine admission returns explicit failure",FailureQ[limited]&&limited[[1]]==="JetAffineWorkLimit"];
Print["CHECKS: ",checks,"; FAILURES: ",Length[failures]];If[failures=!={},Print[failures]];If[failures=!={},Quit[1]];Print["HEPCAT_TEST_COMPLETED:high-energy-laurent-jet-tests.wl"];Quit[0];
