(* An unfinished benchmark simplification may retry only its complete off-shell conversion. *)

(*
HEPCAT - High Energy Physics Constructive Amplitude Tool
Copyright (C) 2026 Neil Christensen
This file is part of HEPCAT, distributed under the GNU General Public License,
version 3 or (at your option) any later version. See <https://www.gnu.org/licenses/>.
*)

root=DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Get[FileNameJoin[{root,"source","HEPCAT.wl"}]];
checks=0;failures={};SetAttributes[check,HoldRest];
check[name_,value_]:=(checks++;If[!TrueQ[value],AppendTo[failures,name];Print["FAIL: ",name]]);
fbRecover=ConstructiveDiagrams`Private`constructBenchmarkConversionRecoveryQ;
fbComplete=ConstructiveDiagrams`Private`constructBenchmarkConversionCompleteQ;
fbPairs={{{Mass[1]->fbMass,Mass[2]->0},LorentzScalarProduct[Mom[1],Mom[2]]},
 {{Mass[1]->fbMass,Mass[2]->0},2 LorentzScalarProduct[Mom[1],Mom[2]]}};
fbPartial[input_]:=<|"Status"->"PARTIAL","InputType"->"FeynmanDiagrams",
 "OriginalAmplitudes"->input,"PreparedAmplitudes"->input,
 "Options"-><|"InternalOnShell"->False,"PolePreference"->"AllPhysical"|>,
 "ItemReports"->Map[<|"Status"->"PARTIAL","StopReason"->"TimeConstraint",
  "PreparedExpression"->Last[#],"InternalOnShellChannels"->{},
  "DenominatorsKeptOffShell"->True,"FeynmanConversionFailed"->False,
  "PolePreference"-><|"Status"->"NOT_REACHED"|>|>&,input],
 "Diagnostics"->{"Synthetic per-item deadline: dispatch fixture only."}|>;
fbReport=fbPartial[fbPairs];
fbChange[change_]:=Join[fbReport,<|"ItemReports"->(Join[#,change]&/@fbReport["ItemReports"])|>];
check["an unfinished automatic off-shell benchmark admits complete conversion",fbRecover[fbReport,fbPairs,{},30]];
check["automatic controls can remain automatic",fbRecover[fbReport,fbPairs,<|"TimeConstraint"->Automatic|>,30]];
check["a successful prepared benchmark keeps its original path",!fbRecover[Join[fbReport,<|"Status"->"PREPARED"|>],fbPairs,{},30]];
fbUsable=Join[fbReport,<|"ItemReports"->(Join[#,<|"PolePreference"-><|"Status"->"COMPLETE"|>|>]&/@fbReport["ItemReports"])|>];
check["an already usable partial checkpoint keeps its original path",!fbRecover[fbUsable,fbPairs,{},30]];
check["a finite explicit preparation clock is literal",!fbRecover[fbReport,fbPairs,{"TimeConstraint"->7},30]];
check["an explicit infinite clock is also literal",!fbRecover[fbReport,fbPairs,{"TimeConstraint"->Infinity},30]];
check["an explicit reference choice is literal",!fbRecover[fbReport,fbPairs,{"PolePreference"->"Xi"},30]];
check["an explicit fermion-helicity choice is literal",!fbRecover[fbReport,fbPairs,{"FermionHelicities"->{1->1}},30]];
check["other explicit preparation controls are not silently bypassed",!fbRecover[fbReport,fbPairs,{"MaxSteps"->0},30]];
check["an exhausted parent clock cannot start another stage",!fbRecover[fbReport,fbPairs,{},0]];
check["an invalid remaining clock cannot start another stage",!fbRecover[fbReport,fbPairs,{},Indeterminate]];
check["invalid reductions do not authorize conversion recovery",!fbRecover[fbChange[<|"StopReason"->"InvalidReduction"|>],fbPairs,{},30]];
check["inconsistent numerator shells do not authorize recovery",!fbRecover[fbChange[<|"JointInvariantFailure"->Failure["bad",<||>]|>],fbPairs,{},30]];
check["a partial conversion may retry from the complete original tensor",fbRecover[fbChange[<|"StopReason"->"FeynmanConversionFailed","FeynmanConversionFailed"->True|>],fbPairs,{},30]];
check["missing diagram metadata cannot authorize recovery",!fbRecover[KeyDrop[fbReport,"ItemReports"],fbPairs,{},30]];
check["preparation must describe the same complete input list",!fbRecover[Join[fbReport,<|"OriginalAmplitudes"->Most[fbPairs]|>],fbPairs,{},30]];
check["internal-shell preparation cannot become a benchmark",!fbRecover[fbChange[<|"InternalOnShellChannels"->{{1,2}}|>],fbPairs,{},30]];
check["off-shell denominator evidence is required",!fbRecover[fbChange[<|"DenominatorsKeptOffShell"->False|>],fbPairs,{},30]];
fbConverted=FeynmanToSpinorProducts[fbPairs,"ReduceSpinorProducts"->False];
check["actual complete conversion preserves every ordered mass-rule pair",fbComplete[fbPairs,fbConverted]];
check["raw tensor leftovers cannot pass as converted",!fbComplete[fbPairs,fbPairs]];
check["a proper subset cannot pass conversion coverage",!fbComplete[fbPairs,Most[fbConverted]]];
check["an extra diagram cannot pass conversion coverage",!fbComplete[fbPairs,Append[fbConverted,First[fbConverted]]]];
check["a changed mass rule cannot pass conversion coverage",!fbComplete[fbPairs,ReplacePart[fbConverted,{1,1,1}->(Mass[1]->0)]]];
check["an abort is never a converted expression",!fbComplete[fbPairs,ReplacePart[fbConverted,{1,2}->$Aborted]]];
check["a converter failure is never a complete benchmark",!fbComplete[fbPairs,Failure["conversion",<||>]]];

fbModel=ReadModel[FileNameJoin[{root,"models"}],3];
fbFeynman=ReadModel[FileNameJoin[{root,"tests","reference-models","calchep"}],2];
fbProcess={"e","E","A.+","A.+"};
fbOptions=<|"Output"->"Details","FeynmanModel"->fbFeynman,
 "ParameterRules"->{"Me"->1,"EE"->1},"TimeConstraint"->120,
 "Samples"->1,"ValidationSamples"->1,"RandomSeed"->192871,
 "Parallelization"->False,"ProgressReporting"->False,"PrintReport"->False,
 "PreparationOptions"->{"MaxSteps"->0,"TimeConstraint"->20},
 "ChannelPreparationOptions"->{"MaxSteps"->0,"TimeConstraint"->20},
 "FinalPreparationOptions"->{"MaxSteps"->0,"TimeConstraint"->20},
 "FactorizationOptions"->{"TimeConstraint"->30,"ValidationParameterRules"->{{"Me"->1,"EE"->1}}},
 "HighEnergyOptions"->{"TimeConstraint"->30,"ValidationParameterRules"->{{"Me"->1,"EE"->1}}},
 "CheckOptions"->{"Samples"->1,"FeynmanSamples"->1,"TimeConstraint"->20}|>;
fbRun[mode_,extra_List:{}]:=Internal`InheritedBlock[{PrepareAmplitudes,FeynmanToSpinorProducts},
 fbConversionCalls={};fbRecording=True;
 Unprotect[PrepareAmplitudes,FeynmanToSpinorProducts];
 DownValues[PrepareAmplitudes]=Prepend[DownValues[PrepareAmplitudes],
  HoldPattern[PrepareAmplitudes[input_List,model_FeynmanModel,opts:OptionsPattern[]]]:>
   fbPartial[input]];
 DownValues[FeynmanToSpinorProducts]=Prepend[DownValues[FeynmanToSpinorProducts],
  HoldPattern[FeynmanToSpinorProducts[input_,opts:OptionsPattern[]]/;TrueQ[fbRecording]]:>
   Module[{converted,settings=Association[{opts}]},
    AppendTo[fbConversionCalls,<|"Input"->input,"Options"->settings|>];
    converted=Block[{fbRecording=False},FeynmanToSpinorProducts[input,opts]];
    If[mode==="Subset"&&ListQ[converted],Most[converted],converted]]];
 ConstructAmplitude[fbProcess,fbModel,fbModel,
  Sequence@@Normal[Join[fbOptions,Association[extra]]]]];
fbRecovered=fbRun["Complete"];fbRecoveredCalls=fbConversionCalls;
check["normal construction can recover the benchmark and pass every final test",
 AssociationQ[fbRecovered]&&fbRecovered["Status"]==="COMPLETE"&&
 fbRecovered["Checks"]["FeynmanCheck"]["Status"]==="PASS"&&
 AllTrue[fbRecovered["Checks"]["Checks"],#["Status"]==="PASS"&]];
check["the original partial preparation and distinct recovery remain inspectable",
 fbRecovered["Stages"]["Preparing Feynman amplitudes"]["Status"]==="PARTIAL"&&
 fbRecovered["Stages"]["Feynman benchmark recovery"]["Status"]==="COMPLETE"&&
 KeyExistsQ[fbRecovered["Stages"],"Recovering complete Feynman benchmark"]];
check["recovery converts all original diagrams once without changing their references",
 Length[fbRecoveredCalls]===1&&
 fbRecoveredCalls[[1,"Input"]]===fbRecovered["Stages"]["Building Feynman amplitudes"]&&
 fbRecoveredCalls[[1,"Options","ReferenceSpinors"]]===Automatic&&
 fbRecoveredCalls[[1,"Options","ReduceSpinorProducts"]]===False];
check["the recovery converter receives only the remaining overall allowance",
 0<fbRecoveredCalls[[1,"Options","TimeConstraint"]]<=fbOptions["TimeConstraint"]&&
 fbRecovered["Stages"]["Feynman benchmark recovery"]["InputDiagrams"]===2&&
 fbRecovered["Stages"]["Feynman benchmark recovery"]["ConvertedDiagrams"]===2&&
 fbRecovered["Stages"]["Feynman benchmark recovery"]["InternalOnShell"]===False];
fbPinned=fbRun["Complete",{"FeynmanPreparationOptions"->{"TimeConstraint"->3}}];
check["a pinned benchmark clock stops at its partial preparation instead of retrying",
 fbPinned["Status"]==="FAILED"&&fbConversionCalls==={}&&
 !KeyExistsQ[fbPinned["Stages"],"Feynman benchmark recovery"]];
fbSubset=fbRun["Subset"];
check["a finite incomplete conversion stops the constructor before independent validation",
 fbSubset["Status"]==="FAILED"&&Length[fbConversionCalls]===1&&
 fbSubset["Stages"]["Feynman benchmark recovery"]["Status"]==="ATTEMPTED"&&
 SelectFirst[fbSubset["StageTimings"],#["Stage"]==="Recovering complete Feynman benchmark"&]["Status"]==="FAILED"];
If[checks=!=31,AppendTo[failures,"Not every benchmark-recovery assertion executed."]];
Print["CONSTRUCT BENCHMARK RECOVERY CHECKS: ",checks,"; FAILURES: ",Length[failures]];
If[failures=!={},Print[failures],Print["HEPCAT_TEST_COMPLETED:construct-benchmark-recovery-tests.wl"]];
Quit[If[failures==={},0,1]];
