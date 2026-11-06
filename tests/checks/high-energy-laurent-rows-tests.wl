(* Laurent constraints retain the complete affine correction polynomial. *)
root=DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Global`$HEPCATpath=FileNameJoin[{root,"source"}];Get[FileNameJoin[{$HEPCATpath,"HEPCAT.wl"}]];
checks=0;failures={};check[test_,value_]:=(checks++;If[!TrueQ[value],AppendTo[failures,test];Print["FAIL: ",test]]);
{hlT,hlU,hlV,hlW,hlZ,hlX,hlY}=Table[Unique["laurentConstraint"],{7}];
hlSpins={hlU,hlV,hlW,hlZ};hlUnknowns={hlX,hlY};hlPad=Total[Table[hlT^k,{k,0,1800}]];
hlRows[expr_,unknowns_:hlUnknowns]:=ConstructiveDiagrams`Private`completionHELaurentRows[expr,hlT,hlSpins,unknowns,4];
(* Keep the older route's fault-injection checks independent of the new
   streaming route, which is covered by exact affine examples below. *)
hlLegacyRows[expr_,unknowns_:hlUnknowns]:=Internal`InheritedBlock[
 {ConstructiveDiagrams`Private`completionHEStreamingLaurentRows},
 Clear[ConstructiveDiagrams`Private`completionHEStreamingLaurentRows];
 ConstructiveDiagrams`Private`completionHEStreamingLaurentRows[___]:=
   Failure["UnsupportedStreamingPolynomial",<||>];hlRows[expr,unknowns]];
hlSeries[expr_,unknowns_:hlUnknowns]:=ConstructiveDiagrams`Private`completionHELaurentSeries[expr,hlT,hlSpins,unknowns,4];
hlRowSpace[rows_,variables_]:=If[rows==={}, {},Module[{arrays=CoefficientArrays[rows,variables]},DeleteCases[RowReduce[Join[List/@Normal[First[arrays]],Normal[Last[arrays]],2]],ConstantArray[0,Length[variables]+1]]]];
hlPlus=hlRows[(hlX+hlY)hlU hlPad/hlT^6];
check["a coupled sum remains one affine constraint",hlRowSpace[hlPlus,hlUnknowns]===hlRowSpace[{hlX+hlY},hlUnknowns]];
check["opposite coefficient choices still cancel across unknown monomials",AllTrue[hlPlus,TrueQ[(#/.{hlX->1,hlY->-1})===0]&]];
hlMinus=hlRows[(hlX-hlY)hlU hlPad/hlT^6];
check["a coupled difference retains the expected free direction",hlRowSpace[hlMinus,hlUnknowns]===hlRowSpace[{hlX-hlY},hlUnknowns]];
hlMixed=hlRows[(hlX hlU+hlY hlV)hlPad/hlT^6];
check["distinct spin monomials remain independent constraints",hlRowSpace[hlMixed,hlUnknowns]===hlRowSpace[{hlX,hlY},hlUnknowns]];
check["different tensor components cannot cancel by choosing opposite coefficients",AnyTrue[hlMixed,!TrueQ[(#/.{hlX->1,hlY->-1})===0]&]];
hlTiny=hlRows[(hlX+1/10^100)hlU hlPad/hlT^6,{hlX}];
check["an arbitrarily small exact forbidden term is retained",hlRowSpace[hlTiny,{hlX}]===hlRowSpace[{hlX+1/10^100},{hlX}]];
check["tiny constants are never rounded to zero",AllTrue[hlTiny,TrueQ[(#/.hlX->-1/10^100)===0]&]&&AnyTrue[hlTiny,!TrueQ[(#/.hlX->0)===0]&]];
check["the first allowed Laurent power is not an equation",hlRows[(hlX hlU hlT^4+hlY hlV hlT^5)hlPad]==={}];
hlHigher=hlRows[(hlX+hlY)hlU^3 hlV^2 hlPad/hlT^6];
check["higher coherent-spin powers keep their complete coefficient",hlRowSpace[hlHigher,hlUnknowns]===hlRowSpace[{hlX+hlY},hlUnknowns]];
check["identical opposite terms vanish before constraint formation",hlRows[(hlX hlU-hlX hlU)hlPad/hlT^6]==={}];
hlNonlinear=hlRows[hlX hlY hlU hlPad/hlT^6];
check["nonlinear correction products are explicitly unsupported",FailureQ[hlNonlinear]&&hlNonlinear[[1]]==="NonlinearCorrectionFamily"];
check["unknown-dependent denominators are unsupported",FailureQ[hlRows[hlU hlPad/(hlX hlT^6)]]];
check["fractional correction powers are unsupported",FailureQ[hlRows[Sqrt[hlX]hlU hlPad/hlT^6]]];
hlSparseReport=hlSeries[(hlX+hlY)hlU hlPad/hlT^6];
check["large coherent tensors select the certified scalar coefficient method",AssociationQ[hlSparseReport]&&hlSparseReport["Method"]==="ExactSparseCoherentAndCorrectionCoefficientsBeforeLaurent"];
check["the whole affine polynomial is reconstructed before row extraction",TrueQ[hlSparseReport["FullAffineReconstruction"]]&&TrueQ[hlSparseReport["CoefficientExpansion"]["Polynomial"]["FullCoefficientCoverage"]]&&TrueQ[hlSparseReport["CoefficientExpansion"]["Polynomial"]["ReconstructionVerified"]]];
check["the scalar route does not sample tensor or correction coefficients",hlSparseReport["CoefficientExpansion"]["NumericalSpinSamples"]===0];
check["small coherent expressions retain their original whole-series route",hlSeries[(hlX+hlY)hlU/hlT^6]["Method"]==="ExactWholeLaurentExpansion"];
check["a smaller spin sector retains the original route",ConstructiveDiagrams`Private`completionHELaurentSeries[(hlX+hlY)hlU hlPad/hlT^6,hlT,{hlU,hlV},hlUnknowns,4]["Method"]==="ExactWholeLaurentExpansion"];
check["a zero affine expression has no constraints",hlRows[0]==={}];
hlFallback=Internal`InheritedBlock[{ConstructiveDiagrams`Private`highEnergyScalarCoefficientSeries},Clear[ConstructiveDiagrams`Private`highEnergyScalarCoefficientSeries];ConstructiveDiagrams`Private`highEnergyScalarCoefficientSeries[___]:=<|"Status"->"UNRESOLVED","CompletedComponents"->1|>;hlSeries[(hlX+hlY)hlU hlPad/hlT^6]];
check["an incomplete sparse attempt falls back without accepting partial coefficients",hlFallback["Method"]==="ExactWholeLaurentExpansion"];
hlBadMetadata=Internal`InheritedBlock[{ConstructiveDiagrams`Private`highEnergyScalarCoefficientSeries},Clear[ConstructiveDiagrams`Private`highEnergyScalarCoefficientSeries];ConstructiveDiagrams`Private`highEnergyScalarCoefficientSeries[___]:=<|"Status"->"COMPLETE","Series"->0,"Polynomial"-><|"ComponentMonomials"->{{-1,0,0,0,0,0}}|>|>;hlLegacyRows[(hlX+hlY)hlU hlPad/hlT^6]];
check["malformed monomial metadata cannot certify zero rows",FailureQ[hlBadMetadata]&&hlBadMetadata[[1]]==="UnverifiedLaurentPolynomial"];
hlFalseCoverage=Internal`InheritedBlock[{ConstructiveDiagrams`Private`highEnergyScalarCoefficientSeries},Clear[ConstructiveDiagrams`Private`highEnergyScalarCoefficientSeries];ConstructiveDiagrams`Private`highEnergyScalarCoefficientSeries[___]:=<|"Status"->"COMPLETE","Series"->0,"CompletedComponents"->1,"ComponentCount"->1,"Polynomial"-><|"ComponentCount"->1,"ComponentMonomials"->{{0,0,0,0,0,0}},"FullCoefficientCoverage"->False,"ReconstructionVerified"->True,"ConstantCoefficientIncluded"->True|>|>;hlLegacyRows[(hlX+hlY)hlU hlPad/hlT^6]];
check["a COMPLETE label cannot replace full coefficient coverage",FailureQ[hlFalseCoverage]];
hlCountMismatch=Internal`InheritedBlock[{ConstructiveDiagrams`Private`highEnergyScalarCoefficientSeries},Clear[ConstructiveDiagrams`Private`highEnergyScalarCoefficientSeries];ConstructiveDiagrams`Private`highEnergyScalarCoefficientSeries[___]:=<|"Status"->"COMPLETE","Series"->0,"CompletedComponents"->0,"ComponentCount"->1,"Polynomial"-><|"ComponentCount"->1,"ComponentMonomials"->{{0,0,0,0,0,0}},"FullCoefficientCoverage"->True,"ReconstructionVerified"->True,"ConstantCoefficientIncluded"->True|>|>;hlLegacyRows[(hlX+hlY)hlU hlPad/hlT^6]];
check["unfinished scalar components cannot be accepted through metadata",FailureQ[hlCountMismatch]];
hlUnprovedWarning=Internal`InheritedBlock[{Series,ConstructiveDiagrams`Private`highEnergyScalarCoefficientSeries},Unprotect[Series];Clear[Series];Series[_,{_,0,4}]:=(Message[Series::ztest,{1/10^40}];0);Protect[Series];Off[Series::ztest];Clear[ConstructiveDiagrams`Private`highEnergyScalarCoefficientSeries];ConstructiveDiagrams`Private`highEnergyScalarCoefficientSeries[___]:=<|"Status"->"UNRESOLVED"|>;Quiet[hlLegacyRows[(hlX+hlY)hlU hlPad/hlT^6]]];
check["disabled warnings cannot let an unproved fallback assumption erase rows",FailureQ[hlUnprovedWarning]];
hlExactWarning=Internal`InheritedBlock[{Series},Unprotect[Series];Clear[Series];Series[_,{_,0,4}]:=(Message[Series::ztest1,0];0);Protect[Series];Off[Series::ztest1];Quiet[hlSeries[(hlX+hlY)hlU/hlT^6]]];
check["a fallback zero assumption needs its exact certificate even under Off",AssociationQ[hlExactWarning]&&hlExactWarning["ZeroAssumptionCertificates"]=!={}&&AllTrue[Flatten[Lookup[hlExactWarning["ZeroAssumptionCertificates"],"ExactValues"]],#===0&]];
hlOtherWarning=Internal`InheritedBlock[{Series},Unprotect[Series];Clear[Series];Series[_,{_,0,4}]:=(Message[Power::infy,0];0);Protect[Series];Quiet[hlRows[(hlX+hlY)hlU/hlT^6]]];
check["other fallback warnings retain ordinary Check rejection",FailureQ[hlOtherWarning]];
hlPartialSeries=Internal`InheritedBlock[{Series},Unprotect[Series];Clear[Series];Series[_,{variable_,0,4}]:=SeriesData[variable,0,{},3,3,1];Protect[Series];hlRows[(hlX+hlY)hlU/hlT^6]];
check["a fallback series must cover the complete requested order",FailureQ[hlPartialSeries]];
hlExpired=TimeConstrained[Internal`InheritedBlock[{ConstructiveDiagrams`Private`highEnergyScalarCoefficientSeries},Clear[ConstructiveDiagrams`Private`highEnergyScalarCoefficientSeries];ConstructiveDiagrams`Private`highEnergyScalarCoefficientSeries[___]:=(Pause[0.03];<|"Status"->"UNRESOLVED"|>);hlLegacyRows[(hlX+hlY)hlU hlPad/hlT^6]],0.001,hlDeadline];
check["the caller's shared deadline still aborts unfinished work",hlExpired===hlDeadline];
BlockRandom[SeedRandom[118271];Do[
 hlMatrix=RandomInteger[{-3,3},{4,3}];hlExpected=hlMatrix.{1,hlX,hlY};hlMonomials={hlU,hlV,hlU hlV,hlW hlZ};
 hlActual=hlRows[Total[hlExpected hlMonomials]hlPad/hlT^6];
 check["all affine rows agree with an independent coefficient matrix "<>ToString[index],hlRowSpace[hlActual,hlUnknowns]===hlRowSpace[hlExpected,hlUnknowns]],{index,3}]];
hlStream[expr_,spins_:hlSpins,unknowns_:hlUnknowns,threshold_:4]:=
 ConstructiveDiagrams`Private`completionHEStreamingLaurentRows[expr,hlT,spins,unknowns,threshold];
hlStreamCoupled=hlStream[((hlX+hlY)hlU+(hlX+2)hlV)/hlT^6];
check["streaming preserves coupled affine rows and the constant offset",
 AssociationQ[hlStreamCoupled] && hlRowSpace[hlStreamCoupled["Rows"],hlUnknowns]===
  hlRowSpace[{hlX+hlY,hlX+2},hlUnknowns]];
check["streaming certifies every affine and coherent coefficient",
 hlStreamCoupled["FullCoefficientCoverage"] && hlStreamCoupled["FullAffineReconstruction"] &&
  hlStreamCoupled["ComponentCount"]===hlStreamCoupled["CompletedComponents"] &&
  hlStreamCoupled["AffineDecompositionCertificate"]["ConstantCoefficientIncluded"] &&
  AllTrue[hlStreamCoupled["PolynomialCertificates"],TrueQ[#["ReconstructionVerified"]]&]];
check["streaming does not separate the two unknowns of one equation",
 AllTrue[hlStream[(hlX+hlY)hlU/hlT^6]["Rows"],TrueQ[(#/.{hlX->3,hlY->-3})===0]&]];
check["the streaming boundary retains the first allowed power",
 hlStream[(hlX hlU+hlY hlV)hlT^4]["Rows"]==={}];
hlIndexed=hlStream[(hlIndexedCoefficient[1]+hlIndexedCoefficient[2]+1)hlU/hlT^6,
 hlSpins,{hlIndexedCoefficient[1],hlIndexedCoefficient[2]}];
check["indexed affine coordinates remain independent and are preserved",
 AssociationQ[hlIndexed] && hlRowSpace[hlIndexed["Rows"],{hlIndexedCoefficient[1],hlIndexedCoefficient[2]}]===
  hlRowSpace[{hlIndexedCoefficient[1]+hlIndexedCoefficient[2]+1},
   {hlIndexedCoefficient[1],hlIndexedCoefficient[2]}]];
check["streaming refuses nonlinear correction products",
 FailureQ[hlStream[hlX hlY hlU/hlT^6]]];
check["streaming refuses unknowns in a denominator",
 FailureQ[Quiet[hlStream[hlU/(hlX hlT^6)]]]];
hlPartialAffine=Internal`InheritedBlock[
 {ConstructiveDiagrams`Private`amplitudeCoherentPolynomialRules},
 hlPolynomialCalls=0;
 Clear[ConstructiveDiagrams`Private`amplitudeCoherentPolynomialRules];
 ConstructiveDiagrams`Private`amplitudeCoherentPolynomialRules[___]:=(
  hlPolynomialCalls++;
  If[hlPolynomialCalls===1,<|"Status"->"COMPLETE","FullCoefficientCoverage"->True,
   "ReconstructionVerified"->True,"ConstantCoefficientIncluded"->True,
   "ComponentCount"->1,"ComponentMonomials"->{{0,0,0,0}},
   "CoefficientRules"->{{0,0,0,0}->0}|>,<|"Status"->"UNRESOLVED"|>]);
 hlStream[(hlX+hlY)hlU/hlT^6]];
check["an unfinished later affine component cannot certify an earlier zero prefix",
 MatchQ[hlPartialAffine,Failure["UnsupportedStreamingPolynomial",_Association]] && hlPolynomialCalls===2];
check["three vector spin variables have all twenty-seven component slots",
 ConstructiveDiagrams`Private`completionHECoherentComponentBound[
  (1+hlU)^2(1+hlV)^2(1+hlW)^2,{hlU,hlV,hlW}]===27];
check["three spin-half variables retain eight component slots",
 ConstructiveDiagrams`Private`completionHECoherentComponentBound[
  (1+hlU)(1+hlV)(1+hlW),{hlU,hlV,hlW}]===8];
hlStreamingWarning=Internal`InheritedBlock[{Series},Unprotect[Series];Clear[Series];
 Series[_,{_,0,4}]:=(Message[Series::ztest,{1/10^40}];0);Protect[Series];
 Off[Series::ztest];Quiet[hlStream[(hlX+hlY)hlU/hlT^6]]];
check["streaming cannot discard a nonzero coefficient under a quiet series warning",
 MatchQ[hlStreamingWarning,Failure["UncertifiedStreamingScalarSeries",_Association]]];
hlStreamExpired=TimeConstrained[Internal`InheritedBlock[
 {ConstructiveDiagrams`Private`highEnergyCertifiedScalarSeries},
 Clear[ConstructiveDiagrams`Private`highEnergyCertifiedScalarSeries];
 ConstructiveDiagrams`Private`highEnergyCertifiedScalarSeries[___]:=(Pause[0.05];<|"Status"->"UNRESOLVED"|>);
 hlStream[(hlX+hlY)hlU/hlT^6]],0.001,hlDeadline];
check["the caller deadline interrupts streaming without accepting partial rows",hlStreamExpired===hlDeadline];
check["these exact scalar-row tests need no worker pool",Kernels[]==={}];
Print[checks," high-energy Laurent-row checks; ",Length[failures]," failures."];
If[failures=!={},Print[failures];Quit[1]];Print["HEPCAT_TEST_COMPLETED:high-energy-laurent-rows-tests.wl"];Quit[0];
