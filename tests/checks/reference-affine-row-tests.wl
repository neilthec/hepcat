(* Large reference checks keep their original coupled affine coordinates. *)
root=DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Global`$HEPCATpath=FileNameJoin[{root,"source"}];
Get[FileNameJoin[{$HEPCATpath,"HEPCAT.wl"}]];
$HistoryLength=0;checks=0;failures={};
SetAttributes[check,HoldRest];
check[name_,value_]:=(checks++;If[!TrueQ[value],AppendTo[failures,name];Print["FAIL: ",name]]);
helper=ConstructiveDiagrams`Private`completionHEReferenceAffineSpinCoefficients;
rowFunction=ConstructiveDiagrams`Private`completionHEPolynomialRows;
get[a_,key_]:=If[KeyExistsQ[a,key],a[key],0];
affineRows[result_,variables_,references_,baseline_]:=DeleteCases[Flatten[Map[
 Function[component,With[{v=Last[component]},rowFunction[
 Prepend[variables,1].(Cancel[Together[#-(#/.baseline)]]&/@v),references]]],
 result["CoefficientVectors"]]],0];
legacyRows[expression_,spins_,references_,baseline_]:=DeleteCases[Flatten[
 (rowFunction[Last[#]-(Last[#]/.baseline),references]&)/@
 If[spins==={},{ {}->expression},CoefficientRules[expression,spins]]],0];
rowSpace[rows_,variables_]:=Module[{ca},If[rows==={},Return[{}]];
 ca=CoefficientArrays[rows,variables];DeleteCases[RowReduce[MapThread[Prepend,
 {Normal[ca[[2]]],Normal[ca[[1]]]}]],{0..}]];
vars={rfa,rfb,rfc};fixture=rfs^2(rfa rfx+rfb)/(rfx+1)+rfc rft;
parsed=helper[fixture,vars,{rfs,rft}];
check["affine coefficients and every coherent component are certified",
 parsed["Status"]==="COMPLETE"&&parsed["FullCoefficientCoverage"]&&
 parsed["ReconstructionVerified"]&&parsed["UnknownDegreeAtMostOne"]];
check["affine coordinates retain their original order and constant column",
 parsed["AffineVariables"]===vars&&parsed["AffinePartCount"]===4&&
 AllTrue[parsed["CoefficientVectors"],Length[Last[#]]===4&]];
check["formal replay uses ordinary algebra without internal shell assumptions",
 parsed["AffineReconstructionMethod"]==="ExactCoefficientArraysReplay"&&
 parsed["InternalOnShellConditions"]==={}];
newRows=affineRows[parsed,vars,{rfx},{rfx->2}];
oldRows=legacyRows[fixture,{rfs,rft},{rfx},{rfx->2}];
check["all old reference rows have the same exact coupled affine row space",
 rowSpace[newRows,vars]===rowSpace[oldRows,vars]];
check["coupled rational reference terms impose equality rather than independent zeros",
 newRows=!={}&&AllTrue[newRows,Cancel[Together[#/.rfa->rfb]]===0&]&&
 MatrixRank[Last[CoefficientArrays[newRows,vars]]]===1];
check["a genuinely reference-dependent coefficient is retained",
 AnyTrue[newRows,Cancel[Together[#/.{rfa->1,rfb->0}]]=!=0&]];
check["reference-independent affine coordinates remain unconstrained",
 FreeQ[newRows,rfc]];
inhomogeneous=helper[rfs(rfa rfx+1)/(rfx+1),{rfa},{rfs}];
inhomogeneousRows=affineRows[inhomogeneous,{rfa},{rfx},{rfx->2}];
check["the constant coefficient produces the required inhomogeneous constraint",
 inhomogeneousRows=!={}&&AllTrue[inhomogeneousRows,
 Cancel[Together[#/.rfa->1]]===0&]&&AnyTrue[inhomogeneousRows,!FreeQ[#,rfa]&]];
inverseFixture=rfs(rfa/rfx-rfb/rfx)+rfc rfs^2;
inverse=helper[inverseFixture,vars,{rfs}];
inverseRows=affineRows[inverse,vars,{rfx},{rfx->2}];
check["inverse reference poles retain exact cancellations among columns",
 rowSpace[inverseRows,vars]===rowSpace[legacyRows[inverseFixture,{rfs},{rfx},{rfx->2}],vars]&&
 AllTrue[inverseRows,Cancel[Together[#/.rfa->rfb]]===0&]];
multiFixture=(rfa rfx+rfb)/(rfx+1) rfs+rfc rfs^2 rft;
multi=helper[multiFixture,vars,{rfs,rft}];
check["mixed coherent monomials are present and reconstruct all constant and variable terms",
 MemberQ[First/@multi["CoefficientVectors"],{1,0}]&&
 MemberQ[First/@multi["CoefficientVectors"],{2,1}]&&
 MemberQ[First/@multi["CoefficientVectors"],{0,0}]];
check["independent symbolic scalar parameters are preserved",
 helper[rfScalar rfa rfs/(rfx+1),{rfa},{rfs}]["Status"]==="COMPLETE"];
check["nonlinear correction dependence is rejected",
 helper[rfa rfb rfs,{rfa,rfb},{rfs}]["Status"]==="UNRESOLVED"];
check["correction-dependent denominators are rejected",
 helper[rfs/(rfa+1),{rfa},{rfs}]["Status"]==="UNRESOLVED"];
check["spin-dependent denominators cannot get a polynomial certificate",
 helper[rfa/rfs,{rfa},{rfs}]["Status"]==="UNRESOLVED"];
check["duplicate correction coordinates are rejected",
 helper[rfa rfs,{rfa,rfa},{rfs}]["Status"]==="UNRESOLVED"];
check["overlapping coherent and correction coordinates are rejected",
 helper[rfa^2,{rfa},{rfa}]["Status"]==="UNRESOLVED"];
check["a bounded extraction failure is explicit and never certified",
 helper[rfa rfs,{rfa},{rfs},0]["Status"]==="UNRESOLVED"];
padding=rfPaddingHead@@Range[6000];paddedVars=Join[{rfa,rfb},Table[Unique["referenceUnused"],{14}]];
fakeEvaluate[expression_,chart_,rules_]:=With[
 {x=First[chart["ReferenceOverrides"]["Angle"][10]]},
 padding rfs+(rfa x+rfb)/(x+1)rfs^2];
fakeChart=<|"SpinVariables"->{rfs}|>;
optimized=Block[{ConstructiveDiagrams`Private`residueEvaluateChart=fakeEvaluate},
 ConstructiveDiagrams`Private`completionHEReferenceRows[0,fakeChart,{10},1,773,{},paddedVars]];
original=Block[{ConstructiveDiagrams`Private`residueEvaluateChart=fakeEvaluate},
 ConstructiveDiagrams`Private`completionHEReferenceRows[0,fakeChart,{10},1,773,{}]];
check["a large reference family uses the verified affine extraction",
 AssociationQ[optimized]&&optimized["CoefficientExtraction"]["Status"]==="COMPLETE"&&
 optimized["CoefficientExtraction"]["AffinePartCount"]===17];
check["original and optimized routes choose exactly the same reference baseline",
 optimized["BaselineAssignments"]===original["BaselineAssignments"]];
check["the complete original family retains its original regular baseline test",
 AssociationQ[optimized]&&optimized["Status"]==="CONSTRAINTS"&&
 optimized["Method"]==="ExactReferenceDifferenceBySpinComponent"];
check["end-to-end extraction keeps the same coupled row space",
 rowSpace[optimized["Equations"],paddedVars]===rowSpace[original["Equations"],paddedVars]];
check["all original unused affine coordinates remain available",
 FreeQ[optimized["Equations"],Alternatives@@Drop[paddedVars,2]]];
small=Block[{ConstructiveDiagrams`Private`residueEvaluateChart=fakeEvaluate},
 ConstructiveDiagrams`Private`completionHEReferenceRows[0,fakeChart,{10},1,773,{},Take[paddedVars,2]]];
check["small affine families retain the original extraction route",
 small["CoefficientExtraction"]["Method"]==="OriginalWholeSpinPolynomial"];
nonlinearEvaluate[expression_,chart_,rules_]:=With[
 {x=First[chart["ReferenceOverrides"]["Angle"][10]]},padding rfs+rfa rfb x rfs];
fallback=Block[{ConstructiveDiagrams`Private`residueEvaluateChart=nonlinearEvaluate},
 ConstructiveDiagrams`Private`completionHEReferenceRows[0,fakeChart,{10},1,773,{},paddedVars]];
check["unsupported affine extraction falls back to the original exact reference calculation",
 AssociationQ[fallback]&&fallback["CoefficientExtraction"]["Method"]==="OriginalWholeSpinPolynomial"&&
 fallback["Equations"]=!={}];
domainHelper=ConstructiveDiagrams`Private`completionHEReferenceArithmeticDomain;
baselineHelper=ConstructiveDiagrams`Private`completionHEReferenceRegularBaseline;
regularDomain=domainHelper[rfs(rfa rfx+rfb)/(rfx+1),{rfs,rfa,rfb,rfx}];
regular=baselineHelper[regularDomain,{rfx->2},{rfs->1}];
check["the full original arithmetic expression has a sufficient exact regularity certificate",
 regularDomain["Status"]==="CERTIFIED"&&regular["Status"]==="PROVED_REGULAR"&&
 regular["SameFullExpressionBaseline"]&&regular["InternalOnShellConditions"]==={}];
check["every syntactic inverse base is explicitly evaluated and proved nonzero",
 Length[regular["InverseBases"]]===1&&regular["ExactBaseValues"]==={3}&&
 regular["ExactNonzeroStates"]==={"Nonzero"}];
check["a syntactic pole at the baseline cannot pass the sufficient certificate",
 baselineHelper[regularDomain,{rfx->-1},{rfs->1}]["Status"]==="NOT_CERTIFIED"];
fractionalDomain=domainHelper[rfa(rfx+1)^(-1/2),{rfa,rfx}];
check["negative rational powers are included in the complete inverse inventory",
 fractionalDomain["InverseBases"]==={1+rfx}&&
 baselineHelper[fractionalDomain,{rfx->2},{}]["Status"]==="PROVED_REGULAR"&&
 baselineHelper[fractionalDomain,{rfx->-1},{}]["Status"]==="NOT_CERTIFIED"];
unknownBaseDomain=domainHelper[rfs/(1+rfa),{rfs,rfa}];
check["an unresolved correction-dependent inverse base retains the original fallback",
 baselineHelper[unknownBaseDomain,{}, {rfs->1}]["Status"]==="NOT_CERTIFIED"];
removableDomain=domainHelper[(rfx^2-1)/(rfx-1),{rfx}];
check["a removable syntactic pole is not silently admitted at its singular baseline",
 baselineHelper[removableDomain,{rfx->1},{}]["Status"]==="NOT_CERTIFIED"];
check["unsupported functions and non-arithmetic atoms retain the original full check",
 domainHelper[Log[rfx],{rfx}]["Status"]==="NOT_CERTIFIED"&&
 domainHelper["reference text",{}]["Status"]==="NOT_CERTIFIED"];
check["symbolic powers cannot be treated as finite arithmetic without a proof",
 domainHelper[rfx^rfExponent,{rfx,rfExponent}]["Status"]==="NOT_CERTIFIED"];
check["unlisted scalar symbols cannot enter an arithmetic certificate",
 domainHelper[rfScalar+rfx,{rfx}]["Status"]==="NOT_CERTIFIED"];
check["missing complete-domain proof metadata cannot certify a baseline",
 baselineHelper[<|"Status"->"CERTIFIED","InverseBases"->{}|>,{},{}]["Status"]==="NOT_CERTIFIED"];
check["nonfinite expressions and malformed allowed-symbol inventories are rejected",
 domainHelper[ComplexInfinity,{}]["Status"]==="NOT_CERTIFIED"&&
 domainHelper[rfx,{rfx,rfx}]["Status"]==="NOT_CERTIFIED"];
polynomialDomain=domainHelper[rfa rfx+rfx^2,{rfa,rfx}];
check["a finite arithmetic polynomial needs no inverse-base assumptions",
 baselineHelper[polynomialDomain,{rfx->0},{}]["Status"]==="PROVED_REGULAR"];
check["a complex exact nonzero inverse base is admissible without a numerical tolerance",
 baselineHelper[regularDomain,{rfx->I},{rfs->1}]["Status"]==="PROVED_REGULAR"];
check["uncertain approximate inverse-base values cannot get an exact nonzero certificate",
 baselineHelper[regularDomain,{rfx->0.25},{rfs->1}]["Status"]==="NOT_CERTIFIED"];
Clear[padding,paddedVars,fakeEvaluate,nonlinearEvaluate];
Print["CHECKS: ",checks,"; FAILURES: ",Length[failures]];
If[failures=!={},Print[failures];Quit[1]];
Print["HEPCAT_TEST_COMPLETED:reference-affine-row-tests.wl"];Quit[0];
