(* Sparse coherent coefficients must retain the complete exact polynomial. *)
root=DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Global`$HEPCATpath=FileNameJoin[{root,"source"}];Get[FileNameJoin[{$HEPCATpath,"HEPCAT.wl"}]];
$HistoryLength=0;checks=0;failures={};SetAttributes[check,HoldRest];
check[name_,value_]:=(checks++;If[!TrueQ[value],AppendTo[failures,name];Print["FAIL: ",name]]);
crExtract=ConstructiveDiagrams`Private`amplitudeCoherentPolynomialRules;
crValue[rules_,vars_]:=Total[(Last[#]Times@@MapThread[Power,{vars,First[#]}])&/@rules];
crCase[name_,expr_,vars_]:=Module[{report=crExtract[expr,vars,5],restored},
 check[name<>" completes",report["Status"]==="COMPLETE"];
 If[report["Status"]=!="COMPLETE",Return[]];
 restored=crValue[report["CoefficientRules"],vars];
 check[name<>" reconstructs exactly",TrueQ[Expand[Together[restored-expr]]===0]];
 check[name<>" retains constant coverage",TrueQ[report["ConstantCoefficientIncluded"]]&&MemberQ[First/@report["CoefficientRules"],ConstantArray[0,Length[vars]]]];
 check[name<>" has a verified local reconstruction",TrueQ[report["AbstractRestorationExact"]]&&TrueQ[report["FullCoefficientCoverage"]]&&TrueQ[report["ReconstructionVerified"]]&&report["ReconstructionProof"]["Method"]==="ExactRecursivePolynomialConvolution"];
 report];
crZero=crCase["zero polynomial",0,{crX,crY}];
crScalar=crCase["scalar coefficient",(crA+crB)/(crM^2+crQ),{}];
crMixed=crCase["mixed coherent terms",(crA+crB crX)(crC+crD crY)+crE crX crY,{crX,crY}];
crHigher=crCase["two higher-spin variables",(crA+crB crX)^2(crC+crD crY)^2+1/10^40,{crX,crY}];
check["quadratic coherent degrees remain complete",crHigher["DegreeUpperBounds"]==={2,2}&&crHigher["ComponentCount"]===9];
crTiny=First[Select[crHigher["CoefficientRules"],First[#]==={0,0}&]];
check["a tiny constant coefficient is preserved exactly",Last[crTiny]===crA^2 crC^2+1/10^40];
crRational=crCase["spin-independent rational field",((crA+crB crX)^2+crY)/(Sin[crQ]+crM^2),{crX,crY}];
crGeometry=Times@@Table[(crQ+i crM),{i,30}];crGeometry2=Times@@Table[(crR-i crM),{i,25}];
crFactored=crExtract[(crGeometry crX+crGeometry2 crY)^2,{crX,crY},5];
check["large kinematic coefficient products remain factored",crFactored["Status"]==="COMPLETE"&&
 Last[First[Select[crFactored["CoefficientRules"],First[#]==={2,0}&]]]===crGeometry^2&&
 TrueQ[crFactored["ReconstructionProof"]["ScalarCoefficientFieldExpanded"]===False]];
crPure=crExtract[crX^3,{crX},5];
check["a missing constant is explicitly retained as zero",MemberQ[crPure["CoefficientRules"],{0}->0]];
Do[crUnsupported=crExtract[expression,{crX,crY},5];
 check["unsupported spin dependence remains unresolved "<>ToString[expression,InputForm],
  crUnsupported["Status"]==="UNRESOLVED"&&crUnsupported["CoefficientRules"]==={}&&!TrueQ[crUnsupported["FullCoefficientCoverage"]]],
 {expression,{1/(1+crX),Sin[crX],Sqrt[crX],(1-crX^2)/(1-crX)}}];
check["duplicate coherent variables are rejected",crExtract[crX,{crX,crX},5]["Status"]==="UNRESOLVED"];
check["non-symbol variables are rejected",crExtract[crX,{crX+crY},5]["Status"]==="UNRESOLVED"];
crStopped=crExtract[(crA+crB crX)(crC+crD crY),{crX,crY},0];
check["an expired extraction never exposes partial rules",crStopped["Status"]==="UNRESOLVED"&&crStopped["CoefficientRules"]==={}&&!TrueQ[crStopped["ReconstructionVerified"]]];
crLimit=crExtract[(1+crX)^4096(1+crY)^4096,{crX,crY},2];
check["an oversized convolution preserves unsupported status",crLimit["Status"]==="UNRESOLVED"&&crLimit["CoefficientRules"]==={}&&!TrueQ[crLimit["FullCoefficientCoverage"]]];
SeedRandom[1729];
Do[crPolynomial=Total[Table[RandomInteger[{-3,3}](1+RandomInteger[{-2,2}]crX)^RandomInteger[{0,3}](1+RandomInteger[{-2,2}]crY)^RandomInteger[{0,3}],{8}]];
 crReport=crExtract[crPolynomial,{crX,crY},5];
 check["independent exact polynomial reconstruction "<>ToString[i],crReport["Status"]==="COMPLETE"&&Expand[crValue[crReport["CoefficientRules"],{crX,crY}]-crPolynomial]===0],{i,10}];
check["temporary scalar names never escape returned coefficients",FreeQ[{crMixed,crHigher,crRational,crFactored},symbol_Symbol/;StringStartsQ[SymbolName[Unevaluated[symbol]],"coherentScalar"]]];
crLargeScalar=crOpaqueGeometry[Sequence@@Range[50010]];
crZeroCoefficient=(crA^2-crB^2)/(crA-crB)-crA-crB;
crLargePolynomial=crLargeScalar crZeroCoefficient(1+crX+crX^2)(1+crY+crY^2);
crLargeProof=ConstructiveDiagrams`Private`completionHECoherentComponentCertificate[crLargePolynomial,{crX,crY},15];
check["large component proofs use complete exact sparse coefficients",crLargeProof["Status"]==="PROVED_ZERO"&&crLargeProof["CoefficientMethod"]==="ExactSparseCoherentCoefficients"&&crLargeProof["ComponentCount"]===9&&crLargeProof["ComponentsChecked"]===9];
check["large component proof metadata retain every constant and mixed power",crLargeProof["FullCoefficientCoverage"]===True&&crLargeProof["ConstantCoefficientIncluded"]===True&&crLargeProof["ReconstructionVerified"]===True&&Sort[crLargeProof["ComponentMonomials"]]===Sort[Tuples[{Range[0,2],Range[0,2]}]]];
crBadLarge=ConstructiveDiagrams`Private`completionHECoherentComponentCertificate[crLargePolynomial+crX crY/10^40,{crX,crY},15];
check["a tiny genuine mixed coefficient defeats a large zero certificate",crBadLarge["Status"]==="NOT_PROVED"&&crBadLarge["AllComponentsZero"]===False];
crBadConstant=ConstructiveDiagrams`Private`completionHECoherentComponentCertificate[crLargePolynomial+1/10^40,{crX,crY},15];
check["a tiny genuine constant defeats a large zero certificate",crBadConstant["Status"]==="NOT_PROVED"&&crBadConstant["AllComponentsZero"]===False];
crSparseStopped=ConstructiveDiagrams`Private`completionHECoherentComponentCertificate[crLargePolynomial,{crX,crY},1/1000];
check["expired large component proofs never certify zero",crSparseStopped["Status"]==="NOT_PROVED"];
Print[checks," coherent-polynomial extraction checks; ",Length[failures]," failures."];
If[failures=!={},Print[failures];Exit[1]];Print["HEPCAT_TEST_COMPLETED:coherent-polynomial-rules-tests.wl"];Exit[0];
