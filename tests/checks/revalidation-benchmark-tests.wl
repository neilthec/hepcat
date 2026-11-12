(* HEPCAT - High Energy Physics Constructive Amplitude Tool
   Copyright (C) 2026 Neil Christensen
   SPDX-License-Identifier: GPL-3.0-or-later *)

root=DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Get[FileNameJoin[{root,"source","HEPCAT.wl"}]];
Get[FileNameJoin[{root,"tests","SM-5-point","revalidate-benchmark.wl"}]];
checks=0;failures={};SetAttributes[check,HoldRest];
check[name_,value_]:=(checks++;If[!TrueQ[value],AppendTo[failures,name];Print["FAIL: ",name]]);
rbPrepare=SMFivePointRevalidation`Private`prepareIndependentBenchmark;
rbRaw={{{Mass[1]->rbMass,Mass[2]->0},LorentzScalarProduct[Mom[1],Mom[2]]},
 {{Mass[1]->rbMass,Mass[2]->0},2 LorentzScalarProduct[Mom[1],Mom[2]]}};
rbConverted=FeynmanToSpinorProducts[rbRaw,"ReduceSpinorProducts"->False];
rbPartial[input_]:=<|"Status"->"PARTIAL","InputType"->"FeynmanDiagrams",
 "OriginalAmplitudes"->input,"PreparedAmplitudes"->input,
 "Options"-><|"InternalOnShell"->False,"PolePreference"->"IndependentXi"|>,
 "ItemReports"->Map[<|"Status"->"PARTIAL","StopReason"->"TimeConstraint",
  "PreparedExpression"->Last[#],"InternalOnShellChannels"->{},
  "DenominatorsKeptOffShell"->True,"FeynmanConversionFailed"->False,
  "PolePreference"-><|"Status"->"NOT_REACHED"|>|>&,input],"Diagnostics"->{"Synthetic deadline fixture."}|>;
rbRun[mode_,seconds_:5,raw_:rbRaw]:=Internal`InheritedBlock[
 {PrepareAmplitudes,FeynmanToSpinorProducts,ConstructAmplitude},
 Unprotect[PrepareAmplitudes,FeynmanToSpinorProducts,ConstructAmplitude];
 rbPreparationCalls={};rbConversionCalls={};rbConstructionCalls=0;
 Clear[PrepareAmplitudes,FeynmanToSpinorProducts,ConstructAmplitude];
 ConstructAmplitude[___]:=(rbConstructionCalls++;Failure["ForbiddenConstruction",<||>]);
 PrepareAmplitudes[input_,model_,opts___Rule]:=Module[{report=rbPartial[input]},
  AppendTo[rbPreparationCalls,<|"Input"->input,"Options"->Association[{opts}]|>];
  Switch[mode,
   "PreparationTimeout",Pause[1];report,
   "Prepared"|"UsablePartial",Join[report,<|"Status"->If[mode==="Prepared","PREPARED","PARTIAL"],
     "PreparedAmplitudes"->rbConverted,"ItemReports"->MapThread[
      Join[#1,<|"PreparedExpression"->Last[#2],"PolePreference"-><|"Status"->"COMPLETE"|>|>]&,
      {report["ItemReports"],rbConverted}]|>],
   "Invalid",Join[report,<|"ItemReports"->(Join[#,<|"StopReason"->"InvalidReduction"|>]&/@report["ItemReports"])|>],
   _,report]];
 FeynmanToSpinorProducts[input_,opts___Rule]:=(
  AppendTo[rbConversionCalls,<|"Input"->input,"Options"->Association[{opts}]|>];
  Switch[mode,"Subset",Most[rbConverted],"Raw",rbRaw,"WrongMass",ReplacePart[rbConverted,{1,1,1}->(Mass[1]->0)],
   "ConversionFailure",Failure["Conversion",<||>],"ConversionTimeout",Pause[1];rbConverted,_,rbConverted]);
 rbPrepare[raw,rbModel,seconds]];
rbResult=rbRun["Complete"];rbSavedPrep=rbPreparationCalls;rbSavedConvert=rbConversionCalls;
check["a typed partial benchmark recovers complete conversion",rbResult["Status"]==="COMPLETE"&&rbResult["Recovery"]["Status"]==="COMPLETE"];
check["the exact original diagram list is used by both attempts",rbSavedPrep[[1,"Input"]]===rbRaw&&rbSavedConvert[[1,"Input"]]===rbRaw];
check["the worker policy requests details and off-shell independent references",
 rbSavedPrep[[1,"Options"]]===<|"PolePreference"->"IndependentXi","MaxSteps"->0,"InternalOnShell"->False,"ReturnDetails"->True|>];
check["the fallback preserves automatic per-leg references without numerator reduction",
 rbSavedConvert[[1,"Options","ReferenceSpinors"]]===Automatic&&rbSavedConvert[[1,"Options","ReduceSpinorProducts"]]===False];
check["both attempts share a finite allowance",0<rbSavedConvert[[1,"Options","TimeConstraint"]]<=5&&rbResult["TimeConstraint"]===5];
check["the original partial report remains inspectable",rbResult["PreparationDetails"]["Status"]==="PARTIAL"];
check["all original mass-rule pairs and diagram counts survive",rbResult["BenchmarkAmplitudes"]===rbConverted&&rbResult["OriginalAmplitudes"]===rbRaw&&rbResult["CompleteDiagramCount"]===2];
check["construction is never called by benchmark revalidation",rbConstructionCalls===0&&rbResult["BenchmarkOnly"]===True&&rbResult["InternalOnShell"]===False];
rbReady=rbRun["Prepared"];
check["successful preparation keeps its original path",rbReady["Status"]==="COMPLETE"&&rbConversionCalls==={}&&rbReady["Recovery"]["Attempted"]===False];
rbUsable=rbRun["UsablePartial"];
check["a usable partial checkpoint also keeps its original path",rbUsable["Status"]==="COMPLETE"&&rbConversionCalls==={}];
rbInvalid=rbRun["Invalid"];
check["an invalid reduction cannot trigger recovery",rbInvalid["Status"]==="FAIL"&&rbConversionCalls==={}];
rbSubset=rbRun["Subset"];
check["a proper subset is never a complete independent benchmark",rbSubset["Status"]==="FAIL"&&rbSubset["CompleteDiagramCount"]===0];
rbUnconverted=rbRun["Raw"];
check["raw tensor leftovers are rejected",rbUnconverted["Status"]==="FAIL"];
rbWrongMassResult=rbRun["WrongMass"];
check["changed mass rules are rejected",rbWrongMassResult["Status"]==="FAIL"];
rbFailed=rbRun["ConversionFailure"];
check["failed conversion retains its actual failure",rbFailed["Status"]==="FAIL"&&FailureQ[rbFailed["Recovery"]["ConversionResult"]]];
rbTooSlow=rbRun["PreparationTimeout",.03];
check["an exhausted preparation allowance starts no further conversion",rbTooSlow["Status"]==="FAIL"&&rbConversionCalls==={}];
rbConvertSlow=rbRun["ConversionTimeout",.03];
check["the complete conversion cannot outlive the shared allowance",rbConvertSlow["Status"]==="FAIL"&&FailureQ[rbConvertSlow["Recovery"]["ConversionResult"]]];
check["an infinite allowance is rejected",rbPrepare[rbRaw,rbModel,Infinity]["Status"]==="FAIL"];
check["a nonpositive allowance is rejected",rbPrepare[rbRaw,rbModel,0]["Status"]==="FAIL"];
check["an invalid raw list is rejected before either attempt",rbRun["Complete",5,{}]["Status"]==="FAIL"&&rbPreparationCalls==={}&&rbConversionCalls==={}];
rbInput=SMFivePointRevalidation`Private`independentBenchmarkInput;
rbAdmit=SMFivePointRevalidation`Private`nativeRevalidationRowsPassQ;
rbZero=Internal`InheritedBlock[{ConstructiveDiagrams`Private`constructExternalMasses},
 Clear[ConstructiveDiagrams`Private`constructExternalMasses];
 ConstructiveDiagrams`Private`constructExternalMasses[___]:={rbMass,rbMass,0,0,0};
 rbInput[{},{},{"f","F","v","v","v"},rbModel]];
check["two successful empty lists produce the exact external-mass zero pair",
 rbZero==={{Thread[(Mass/@Range[5])->{rbMass,rbMass,0,0,0}],0}}];
check["a failed graph list is never promoted to zero",FailureQ[rbInput[$Failed,{},{"x"},rbModel]]];
check["a failed amplitude list is never promoted to zero",FailureQ[rbInput[{},$Failed,{"x"},rbModel]]];
check["a nonempty graph with missing amplitudes is never promoted to zero",FailureQ[rbInput[{rbGraph},{},{"x"},rbModel]]];
check["a graph-empty nonzero amplitude is rejected",FailureQ[rbInput[{},rbRaw,{"x"},rbModel]]];
check["a complete nonempty graph map is retained literally",rbInput[{rbGraph1,rbGraph2},rbRaw,{"x","y"},rbModel]===rbRaw];
rbNoCutRows=(<|"Type"->#,"Status"->"PASS"|>&/@{"HighEnergy","Reference","Feynman"});
check["all applicable no-cut checks suffice without inventing physical rows",rbAdmit[rbNoCutRows,{}]];
check["a supplied physical channel still requires physical checks",!rbAdmit[rbNoCutRows,{rbChannel}]];
check["independent Feynman comparison remains mandatory",!rbAdmit[Most[rbNoCutRows],{}]];
check["a failed applicable row can never pass admission",!rbAdmit[Prepend[Rest[rbNoCutRows],<|"Type"->"HighEnergy","Status"->"FAIL"|>],{}]];
rbRealModel=ReadModel[FileNameJoin[{root,"models"}],3];
rbNullRules=Thread[(Mass/@Range[5])->ConstantArray[0,5]];
rbNullReport=CheckAmplitudes[{rbNullRules,0},{},rbRealModel,"Output"->"Details",
 "FeynmanAmplitudes"->{{rbNullRules,0}},"RandomSeed"->223711,"Samples"->1,
 "HighEnergySamples"->1,"ReferenceSamples"->1,"FeynmanSamples"->1,
 "Parallelization"->False,"TimeConstraint"->20];
rbNullRows=If[AssociationQ[rbNullReport]&&MatchQ[Lookup[rbNullReport,"Table",None],_Dataset],Normal[rbNullReport["Table"]],{}];
check["the real normal checker can validate a formal no-cut zero benchmark",
 rbAdmit[rbNullRows,{}]&&!MemberQ[Lookup[rbNullRows,"Type",{}],"Physical"]];
rbAllowance=SMFivePointRevalidation`Private`nativeRevalidationCheckAllowance;
check["an omitted native check clock retains Automatic",rbAllowance[$Failed]===Automatic&&rbAllowance[""]===Automatic];
check["the same-seed larger-clock replay accepts finite600",rbAllowance["600"]===600];
check["finite decimal allowances are supported",rbAllowance["12.5"]===12.5];
check["an explicit Automatic allowance remains Automatic",rbAllowance["Automatic"]===Automatic];
check["zero and negative allowances are rejected",FailureQ[rbAllowance["0"]]&&FailureQ[rbAllowance["-1"]]];
check["unbounded and nonfinite allowances are rejected",AllTrue[{"Infinity","inf","NaN","1e999"},FailureQ[rbAllowance[#]]&]];
check["environment text cannot introduce a Wolfram expression",FailureQ[rbAllowance["1;Quit[]"]]&&FailureQ[rbAllowance["2^100"]]];
If[checks=!=38,AppendTo[failures,"Not every benchmark-workflow assertion executed."]];
Print["REVALIDATION BENCHMARK CHECKS: ",checks,"; FAILURES: ",Length[failures]];
If[failures=!={},Print[failures],Print["HEPCAT_TEST_COMPLETED:revalidation-benchmark-tests.wl"]];
Quit[If[failures==={},0,1]];
