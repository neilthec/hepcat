(* Reuse exact retained affine coefficients without trusting inconsistent metadata. *)

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
Global`$HEPCATpath = FileNameJoin[{root, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
$HistoryLength = 0;
checks = 0; failures = {};
SetAttributes[check, HoldRest];
check[name_, value_] := (checks++; If[!TrueQ[value], AppendTo[failures, name]; Print["FAIL: ", name]]);
raModel = ReadModel[FileNameJoin[{root, "models"}], 4];
raMass = ModelParameter[raModel, "Me"];
raRules = Thread[Mass /@ Range[4] -> {raMass, raMass, 0, 0}];
raCoordinates = {Unique["retainedCoordinate"], Unique["retainedCoordinate"]};
raDirections = {(MomProd[1, 3] + MomProd[1, 4])^8,
  SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]/
    PropDen[Mom[Multiparticle[1, 3]], raMass]};
raFamily = 7 + raCoordinates.raDirections;
raCoefficientCalls = 0;
raGood = Block[{ConstructiveDiagrams`Private`$completionParallelSettings =
      <|"Parallelization" -> False, "MaxKernels" -> 1|>,
    CoefficientArrays = Function[{value, variables}, raCoefficientCalls++; $Failed]},
  ConstructiveDiagrams`Private`completionHEPruneIdentityFamily[
    raFamily, raCoordinates, raRules, raModel, 0, raDirections]];
check["matching literal affine metadata avoids expanding the full family with CoefficientArrays",
  raCoefficientCalls === 0];
check["an exhausted proof budget retains the exact supplied directions and center",
  raGood["FamilyAmplitude"] === raFamily && raGood["RemainingBasis"] === raDirections &&
    raGood["FreeParameters"] === raCoordinates && raGood["RemovedParameters"] === {}];
raCoefficientCalls = 0;
raRejectedMetadata = Block[{ConstructiveDiagrams`Private`$completionParallelSettings =
      <|"Parallelization" -> False, "MaxKernels" -> 1|>,
    CoefficientArrays = Function[{value, variables}, raCoefficientCalls++; $Failed]},
  ConstructiveDiagrams`Private`completionHEPruneIdentityFamily[
    raFamily, raCoordinates, raRules, raModel, 0, {1, 2}]];
check["incorrect retained coefficients fall back to ordinary coefficient inspection",
  raCoefficientCalls > 0 && raRejectedMetadata["FamilyAmplitude"] === raFamily &&
    raRejectedMetadata["FreeParameters"] === raCoordinates];
raWrong = Block[{ConstructiveDiagrams`Private`$completionParallelSettings =
      <|"Parallelization" -> False, "MaxKernels" -> 1|>},
  ConstructiveDiagrams`Private`completionHEPruneIdentityFamily[
    raFamily, raCoordinates, raRules, raModel, 0, {1, 2}]];
check["ordinary fallback extracts the actual coefficients rather than the incorrect metadata",
  raWrong["RemainingBasis"] === raDirections && raWrong["FamilyAmplitude"] === raFamily];
raShort = Block[{ConstructiveDiagrams`Private`$completionParallelSettings =
    <|"Parallelization" -> False, "MaxKernels" -> 1|>},
  ConstructiveDiagrams`Private`completionHEPruneIdentityFamily[
    raFamily, raCoordinates, raRules, raModel, 0, Take[raDirections, 1]]];
check["a truncated coefficient list cannot drop a family coordinate",
  raShort["FreeParameters"] === raCoordinates && raShort["RemainingBasis"] === raDirections];
raNonlinear = 7 + (Times @@ raCoordinates) raDirections[[1]];
raDependent = Block[{ConstructiveDiagrams`Private`$completionParallelSettings =
    <|"Parallelization" -> False, "MaxKernels" -> 1|>},
  ConstructiveDiagrams`Private`completionHEPruneIdentityFamily[
    raNonlinear, raCoordinates, raRules, raModel, 0,
    {raCoordinates[[2]] raDirections[[1]], 0}]];
check["coordinate-dependent metadata cannot make a nonlinear family appear affine",
  raDependent["AffineFamily"] === False && raDependent["FamilyAmplitude"] === raNonlinear &&
    raDependent["FreeParameters"] === raCoordinates && raDependent["RemovedParameters"] === {}];
raZeroCoordinate = Unique["retainedZero"];
raZero = Block[{ConstructiveDiagrams`Private`$completionParallelSettings =
    <|"Parallelization" -> False, "MaxKernels" -> 1|>},
  ConstructiveDiagrams`Private`completionHEPruneIdentityFamily[
    7, {raZeroCoordinate}, raRules, raModel, 1, {0}]];
check["an exactly zero retained direction is removed only after its zero certificate",
  raZero["RemainingBasis"] === {} && raZero["FreeParameters"] === {} &&
    raZero["RemovedParameters"] === {raZeroCoordinate} &&
    AllTrue[raZero["Certificates"], #["Status"] === "PROVED_ZERO" &]];
check["proof or metadata handling never modifies the original supplied coefficients",
  raDirections === {(MomProd[1, 3] + MomProd[1, 4])^8,
    SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]/
      PropDen[Mom[Multiparticle[1, 3]], raMass]}];

(* Validate solved-family cancellation without changing original cut equations.
   The scalar cubic model is generic: no QED formula or saved answer is used.
   Slow/failing grouped attempts must preserve time and the exact chart for the
   original-coordinate fallback. *)
Begin["HEPCATZeroComponentRegression`"];
SetAttributes[test, HoldRest];
test[name_, value_] := Global`check[name, value];
group[c_List,u_List,r_List,f_List]:=ConstructiveDiagrams`Private`completionSolvedComponentGroups[c,u,r,f,2];
r=group[{s,s,t},{u,v},{u->-1,v->2},{}];
test["identical input core cancels for unique solved point",r["Status"]==="AVAILABLE"&&Length[r["CanceledGroups"]]===1&&r["CanceledGroups"][[1]]["Indices"]==={1,2}];
test["uncanceled coefficient and body remain",r["ActiveGroups"][[1]]["Component"]===t&&r["ActiveGroups"][[1]]["FamilyCoefficient"]===2];
r=group[{2 s,s,t},{u,v},{u->-2,v->3},{}];test["exact numeric scaling is retained",Length[r["CanceledGroups"]]===1&&r["CanceledGroups"][[1]]["ExactScales"]==={2,1}];
r=group[{(1/3+I/2)s,(-2/3-I)s,t},{u,v},{u->1/2,v->1},{}];test["exact complex rational scaled components cancel",Length[r["CanceledGroups"]]===1];
r=group[{s,s,s,t},{u,v,w},{u->-1+f,v->-f,w->2},{f}];test["cancellation holds across entire free family",Length[r["CanceledGroups"]]===1&&r["FullFreeFamily"]==={f}];
test["full affine grouping reconstructs exactly",Expand[Prepend[{u,v,w}/.{u->-1+f,v->-f,w->2},1].{s,s,s,t}-Total[(#["FamilyCoefficient"] #["Component"])&/@r["Groups"]]]===0];
r=group[{s,s,t},{u,v},{u->-1+f,v->2},{f}];test["candidate-only zero does not cancel a free-family group",r["CanceledGroups"]==={}&&r["ActiveGroups"][[1]]["FamilyCoefficient"]===f];
r=group[{s,s},{u},{u->-1+1/10^30},{}];test["small exact nonzero coefficient is never chopped",r["CanceledGroups"]==={}];
r=group[{s,s},{u},{u->-1.},{}];test["inexact solved coefficients stay on original path",r["Status"]==="UNAVAILABLE"];
r=group[{s1,s2},{u},{u->-1},{}];test["different physical expressions are not inferred equal",Length[r["Groups"]]===2&&r["CanceledGroups"]==={}];
r=group[{s,s},{u},{},{ }];test["incomplete coefficient map rejected",r["Status"]==="UNAVAILABLE"];
r=group[{s,s,t},{u,u},{u->-1},{}];test["repeated unknowns rejected",r["Status"]==="UNAVAILABLE"];
r=group[{s,s},{u},{u->-1},{f[1]}];test["non-symbol free coordinate rejected",r["Status"]==="UNAVAILABLE"];
r=group[{s},{u},{u->-1},{}];test["mismatched component dimensions rejected",r["Status"]==="UNAVAILABLE"];
r=group[{s+u,s},{u},{u->-1},{}];test["coordinate hidden in a component rejected",r["Status"]==="UNAVAILABLE"];
r=group[{s,s},{u},{u->-1+Sin[f]},{f}];test["opaque free-coordinate dependence rejected",r["Status"]==="UNAVAILABLE"];
r=group[{Missing["source"],Missing["source"]},{u},{u->-1},{}];test["unresolved source components cannot be masked",r["Status"]==="UNAVAILABLE"];
r=group[{s,s},{u},{u->Indeterminate},{}];test["nonfinite solved coefficient cannot be masked",r["Status"]==="UNAVAILABLE"];
r=ConstructiveDiagrams`Private`completionSolvedComponentGroups[{s,s},{u},{u->-1},{},0];test["zero time budget preserves original route",r["Status"]==="UNAVAILABLE"&&r["OriginalComponents"]==={s,s}];
c={s,s,t};us={u,v};rules={u->-1,v->2};saved={c,us,rules};r=group[c,us,rules,{}];test["inputs and original feedback coordinates unchanged",{c,us,rules}===saved&&r["OriginalFeedbackCoordinatesRetained"]&&r["OriginalComponents"]===c&&r["OriginalUnknowns"]===us&&r["OriginalSolutionRules"]===rules];
slowCalls=0;legacyCalls=0;
map[value_]:=Which[value===s,slowCalls++;Missing["expensive canceled core"],value===t,7,True,value];
feedback[report_,target_] := Module[{value},value=Total[(#["FamilyCoefficient"] map[#["Component"]])&/@report["ActiveGroups"]];If[value===target,<|"Status"->"PASS","Equations"->{}|>,legacyCalls++;<|"Status"->"LEGACY","OriginalComponents"->report["OriginalComponents"],"OriginalUnknowns"->report["OriginalUnknowns"]|>]];
r=group[{s,s,t},{u,v},{u->-1,v->1},{}];out=feedback[r,7];test["passing fast image never evaluates canceled core",out["Status"]==="PASS"&&slowCalls===0&&legacyCalls===0];
out=feedback[r,8];test["failed fast image returns complete original feedback route",out["Status"]==="LEGACY"&&out["OriginalComponents"]==={s,s,t}&&out["OriginalUnknowns"]==={u,v}&&legacyCalls===1];
counter=0;r=group[{s,s},{u},{u:>(counter++;-1)},{}];test["delayed coefficient rules are rejected without executing them",r["Status"]==="UNAVAILABLE"&&counter===0];
r=group[{s,s},{u},{u->-1,externalParameter->0},{}];test["rules outside original fit coordinates rejected",r["Status"]==="UNAVAILABLE"];
r=group[{s,s},{u},{u->-1,u->0},{}];test["duplicate coefficient rules rejected",r["Status"]==="UNAVAILABLE"];

Do[r=group[{s,s,t},{u,v},{u->-1,v->coordinate},{}];test["kinematic or nonnumeric coefficient rejected: "<>ToString[coordinate,InputForm],r["Status"]==="UNAVAILABLE"],{coordinate,{PropDen[Multiparticle[1,2],m],MomProd[1,2],SpinorChain[Angle,1,2],Mass[1],symbolicCoupling}}];
SetAttributes[kinematicNumber,NumericFunction];
r=group[{s,s,t},{u,v},{u->-1,v->kinematicNumber[1]},{}];test["user-defined NumericFunction coefficient is not treated as a constant scalar",r["Status"]==="UNAVAILABLE"];
r=group[{s,s,t},{u,v},{u->-1,v->Sqrt[2]},{ }];test["exact System algebraic scalar coefficient remains supported",r["Status"]==="AVAILABLE"&&r["ActiveGroups"][[1]]["FamilyCoefficient"]===Sqrt[2]];

model={{{"Scalar","s","s",1,0,m,0,1,1,"",""}},{{"s","s","s","",g,1}},{},{}};
process=ConstantArray["s",4];diagrams=DiagramAmplitudes[CreateDiagrams[process,model]];channels=FactorizationAmplitudeChannels[process,model];tree=Total[Last/@diagrams];external=Thread[Mass/@Range[4]->ConstantArray[m,4]];
normalized=ConstructiveDiagrams`Private`completionNormalizeInput[{external,tree},channels,model];targets=ConstructiveDiagrams`Private`completionTargets[normalized["Targets"]];target=First[targets];rules={m->1,g->2};seed=16777259;job={1,1};pole=target["Propagator"];core=1/pole^2;
components={core,core,tree};unknowns={u,v};solution={u->-1,v->1};plan=ConstructiveDiagrams`Private`completionSolvedComponentPlan[components,unknowns,solution,{},Range[4],2];profiles=ConstructiveDiagrams`Private`completionComponentPoleProfiles[components,Range[4],2];
legacy=ConstructiveDiagrams`Private`completionSparseValidationSample[components,unknowns,targets,job,solution,{}, {},{},seed,rules,10,AbsoluteTime[]+20,profiles];
fast=ConstructiveDiagrams`Private`completionSparseValidationSample[components,unknowns,targets,job,solution,{}, {},{},seed,rules,10,AbsoluteTime[]+20,profiles,plan];
test["legacy double-pole candidate is regular after coefficient cancellation",legacy["Status"]==="COMPLETE"&&legacy["Result"]["Status"]==="PASS"&&legacy["FamilyPass"]];
test["legacy path actually retained Laurent principal parts",Lookup[legacy["Result"],"ConstraintMethod",None]==="ExactRationalLaurent"];
test["grouped solved image passes same exact physical cut",fast["Status"]==="COMPLETE"&&fast["Result"]["Status"]==="PASS"&&fast["FamilyPass"]];
test["the passing shortcut used an exact full-family cancellation certificate",AssociationQ[Lookup[fast["Result"],"SolvedComponentCancellation",None]]];
chartAlphaNormal[chart_Association]:=Module[{fresh,canonical},
  fresh=DeleteDuplicates[Join[{chart["PoleVariable"]}, Last /@ Values[chart["ExternalSpinVectors"]]]];
  canonical=Array[chartCoordinate,Length[fresh]];
  chart/.Thread[fresh->canonical]
];
test["fast and legacy use identical numeric charts modulo declared fresh spin/pole names",SameQ[chartAlphaNormal[fast["Sample"]["Chart"]],chartAlphaNormal[legacy["Sample"]["Chart"]]]];
test["calculated and expected residues are unchanged",fast["Result"]["CalculatedResidue"]===legacy["Result"]["CalculatedResidue"]&&fast["Result"]["ExpectedResidue"]===legacy["Result"]["ExpectedResidue"]];
test["passing family shortcut adds no group-coordinate feedback",fast["Equations"]==={}&&fast["Sample"]["Difference"]===0&&fast["Sample"]["ConstraintMethod"]==="ExactSolvedFamilyComponentCancellation"];
componentsF={core,core,core,tree};unknownsF={u,v,w};solutionF={u->-1+f,v->-f,w->1};planF=ConstructiveDiagrams`Private`completionSolvedComponentPlan[componentsF,unknownsF,solutionF,{f},Range[4],2];profilesF=ConstructiveDiagrams`Private`completionComponentPoleProfiles[componentsF,Range[4],2];
family=ConstructiveDiagrams`Private`completionSparseValidationSample[componentsF,unknownsF,targets,job,solutionF,{f->0},{f},{},seed,rules,10,AbsoluteTime[]+20,profilesF,planF];
test["entire nontrivial free family passes canceled higher-order cut",family["Status"]==="COMPLETE"&&family["FamilyPass"]&&family["Result"]["Status"]==="PASS"];
test["free coordinates retained in certificate",family["Result"]["SolvedComponentCancellation"]["FullFreeFamily"]==={f}];
wrongSolution={u->-1,v->2};wrongPlan=ConstructiveDiagrams`Private`completionSolvedComponentPlan[components,unknowns,wrongSolution,{},Range[4],2];
actualChartCalls={};
Internal`InheritedBlock[{ConstructiveDiagrams`Private`completionLinearResidue},
  Clear[savedLinearResidue];
  DownValues[savedLinearResidue]=DownValues[ConstructiveDiagrams`Private`completionLinearResidue]/.ConstructiveDiagrams`Private`completionLinearResidue->savedLinearResidue;
  Clear[ConstructiveDiagrams`Private`completionLinearResidue];
  ConstructiveDiagrams`Private`completionLinearResidue[a_,b_,c_,d_,actualChart_,rest___]:=(AppendTo[actualChartCalls,actualChart];savedLinearResidue[a,b,c,d,actualChart,rest]);
  wrong=ConstructiveDiagrams`Private`completionSparseValidationSample[components,unknowns,targets,job,wrongSolution,{}, {},{},seed,rules,10,AbsoluteTime[]+20,profiles,wrongPlan];
];
test["a wrong solved image is still rejected",wrong["Status"]==="COMPLETE"&&wrong["Result"]["Status"]==="FAIL"&&!wrong["FamilyPass"]];
test["wrong-image feedback is in original unknowns",!FreeQ[wrong["Equations"],u|v]&&FreeQ[wrong["Equations"],Alternatives@@wrongPlan["Unknowns"]]];
test["failed fast image calls grouped then original evaluator on literally same chart",Length[actualChartCalls]===2&&SameQ[actualChartCalls[[1]],actualChartCalls[[2]]]];
test["failed original chart matches deterministic legacy numeric coordinates modulo declared fresh names",SameQ[chartAlphaNormal[wrong["Sample"]["Chart"]],chartAlphaNormal[legacy["Sample"]["Chart"]]]];
test["failed image preserves Laurent principal-part equations",Lookup[wrong["Result"],"ConstraintMethod",None]==="ExactRationalLaurent"&&wrong["Equations"]=!={}];
notWholeSolution={u->-1+f,v->1};notWholePlan=ConstructiveDiagrams`Private`completionSolvedComponentPlan[components,unknowns,notWholeSolution,{f},Range[4],2];
test["candidate-only higher-pole cancellation does not trigger optimization",!TrueQ[notWholePlan["UseGroupedValidation"]]];

constraintDefinitionsBefore=DownValues[ConstructiveDiagrams`Private`completionResidueConstraints];
constraintBaseline=Block[{ConstructiveDiagrams`Private`$completionParallelSettings=<|"Parallelization"->False|>},ConstructiveDiagrams`Private`completionResidueConstraints[tree+u tree,{u},normalized,1,seed,rules,20,Function[Null],False]];

test["validation hook leaves exact constraint builder definitions unchanged",SameQ[constraintDefinitionsBefore,DownValues[ConstructiveDiagrams`Private`completionResidueConstraints]]];
constraintAfter=Block[{ConstructiveDiagrams`Private`$completionParallelSettings=<|"Parallelization"->False|>},ConstructiveDiagrams`Private`completionResidueConstraints[tree+u tree,{u},normalized,1,seed,rules,20,Function[Null],False]];
test["constraint construction still completes with identical equations",constraintBaseline["Status"]==="COMPLETE"&&constraintAfter["Status"]==="COMPLETE"&&SameQ[constraintBaseline["Equations"],constraintAfter["Equations"]]];
caller=Block[{ConstructiveDiagrams`Private`$completionParallelSettings=<|"Parallelization"->False|>},ConstructiveDiagrams`Private`completionSparseValidation[core+u core+v tree,{u,v},<|"SolutionRules"->solution,"FreeParameters"->{}|>,{},{},normalized,1,seed,rules,20,Function[Null],False]];
test["real validation caller dispatches full solved-family grouping",TrueQ[caller["FamilyPass"]]&&caller["Report"]["Summary"]===<|"PASS"->Length[targets]|>];
test["real caller supplies exact cancellation certificate to every cut",AllTrue[Flatten[Lookup[caller["Report"]["Channels"],"Samples",{}],1],AssociationQ[Lookup[#,"SolvedComponentCancellation",None]]&]];
wrongCaller=Block[{ConstructiveDiagrams`Private`$completionParallelSettings=<|"Parallelization"->False|>},ConstructiveDiagrams`Private`completionSparseValidation[core+u core+v tree,{u,v},<|"SolutionRules"->wrongSolution,"FreeParameters"->{}|>,{},{},normalized,1,seed,rules,20,Function[Null],False]];
test["real caller retains failed-original feedback coordinates",!TrueQ[wrongCaller["FamilyPass"]]&&!FreeQ[wrongCaller["Feedback"]["Equations"],u|v]&&FreeQ[wrongCaller["Feedback"]["Equations"],symbol_Symbol/;StringContainsQ[SymbolName[Unevaluated[symbol]],"solvedComponentGroup"]]];

(* A slow grouped image must leave budget for the unchanged original chart. *)
slowCharts={};
Internal`InheritedBlock[{ConstructiveDiagrams`Private`completionLinearResidue},
  Clear[savedSlowLinearResidue];
  DownValues[savedSlowLinearResidue]=DownValues[ConstructiveDiagrams`Private`completionLinearResidue]/.ConstructiveDiagrams`Private`completionLinearResidue->savedSlowLinearResidue;
  Clear[ConstructiveDiagrams`Private`completionLinearResidue];
  ConstructiveDiagrams`Private`completionLinearResidue[a_,b_,c_,d_,actualChart_,rest___]:=(AppendTo[slowCharts,actualChart];If[b===plan["Unknowns"],Pause[2]];savedSlowLinearResidue[a,b,c,d,actualChart,rest]);
  slow=ConstructiveDiagrams`Private`completionSparseValidationSample[components,unknowns,targets,job,solution,{}, {},{},seed,rules,10,AbsoluteTime[]+5,profiles,plan];
];
test["slow grouped attempt preserves time for complete original validation",slow["Status"]==="COMPLETE"&&slow["Result"]["Status"]==="PASS"&&TrueQ[slow["FamilyPass"]]&&!KeyExistsQ[slow["Result"],"SolvedComponentCancellation"]];
test["slow grouped attempt falls back on literally same chart",Length[slowCharts]===2&&SameQ[slowCharts[[1]],slowCharts[[2]]]];
(* The recursive fixed-chart call must preserve the outer retry metadata. *)
chartCalls=0;
Internal`InheritedBlock[{ConstructiveDiagrams`Private`completionCutChart},
  Clear[savedCutChart];DownValues[savedCutChart]=DownValues[ConstructiveDiagrams`Private`completionCutChart]/.ConstructiveDiagrams`Private`completionCutChart->savedCutChart;
  Clear[ConstructiveDiagrams`Private`completionCutChart];
  ConstructiveDiagrams`Private`completionCutChart[a_,b_,c_,d_]:=(chartCalls++;If[chartCalls===1,Failure["ForcedFirstChartFailure",<||>],savedCutChart[a,b,c,d]]);
  retried=ConstructiveDiagrams`Private`completionSparseValidationSample[components,unknowns,targets,job,solution,{}, {},{},seed,rules,10,AbsoluteTime[]+20,profiles,plan];
];
test["successful grouped result retains outer retry chart index and attempt",retried["Status"]==="COMPLETE"&&retried["Result"]["Status"]==="PASS"&&retried["Result"]["ChartSampleIndex"]===5&&retried["Result"]["Attempt"]===2&&chartCalls===2];
familyCaller=Block[{ConstructiveDiagrams`Private`$completionParallelSettings=<|"Parallelization"->False|>},ConstructiveDiagrams`Private`completionSparseValidation[core+u core+v core+w tree,{u,v,w},<|"SolutionRules"->solutionF,"FreeParameters"->{f}|>,{f},{},normalized,1,seed,rules,20,Function[Null],False]];
test["real validation caller preserves nontrivial full free family",TrueQ[familyCaller["FamilyPass"]]&&familyCaller["Report"]["Summary"]===<|"PASS"->Length[targets]|>];
test["real full-family caller retains free-coordinate certificate on every cut",AllTrue[Flatten[Lookup[familyCaller["Report"]["Channels"],"Samples",{}],1],Lookup[Lookup[#,"SolvedComponentCancellation",<||>],"FullFreeFamily",{}]==={f}&]];

(* Mixed pass/fail batch: only rows already satisfied by the ENTIRE old family
   may be omitted. A bad cut contributes original-coordinate refinement rows. *)
mixedVariables={u,v,w};mixedComponents={core,core,tree,1/pole};mixedSolution={u->-1,v->1,w->1+f};
mixed=Block[{ConstructiveDiagrams`Private`$completionParallelSettings=<|"Parallelization"->False|>},ConstructiveDiagrams`Private`completionSparseValidation[core+u core+v tree+w/pole,mixedVariables,<|"SolutionRules"->mixedSolution,"FreeParameters"->{f}|>,{f},{},normalized,1,seed,rules,20,Function[Null],False]];
mixedSamples=Flatten[Lookup[mixed["Report"]["Channels"],"Samples",{}],1];mixedPassed=Select[mixedSamples,AssociationQ[Lookup[#,"SolvedComponentCancellation",None]]&];mixedFailed=Select[mixedSamples,Lookup[#,"Status",None]==="FAIL"&];
test["mixed batch has exact certified full-family passes and an original failed cut",Length[mixedPassed]===Length[targets]-1&&Length[mixedFailed]===1&&!TrueQ[mixed["FamilyPass"]]&&AllTrue[mixedPassed,TrueQ[#["FamilyPass"]]&]];
mixedSaved=First[mixedFailed];mixedIndex=First[FirstPosition[Lookup[targets,"Cut"],mixedSaved["Cut"]]];mixedChart=ConstructiveDiagrams`Private`completionCoherentCutChart[mixedSaved["Kinematics"],rules];mixedProfiles=ConstructiveDiagrams`Private`completionComponentPoleProfiles[mixedComponents,Range[4],2];
mixedLegacy=ConstructiveDiagrams`Private`completionSparseValidationSample[mixedComponents,mixedVariables,targets,{mixedIndex,1},mixedSolution,{f->0},{f},{},seed,rules,10,AbsoluteTime[]+20,mixedProfiles,None,mixedChart];
test["mixed failed feedback exactly equals original-coordinate equations on the same saved chart",SameQ[mixed["Feedback"]["Equations"],mixedLegacy["Equations"]]&&!FreeQ[mixed["Feedback"]["Equations"],w]];
oldTraining={u+1,v-1};refined=ConstructiveDiagrams`Private`completionSolveLinear[Join[oldTraining,mixed["Feedback"]["Equations"]],mixedVariables,5];
test["mixed feedback refines while retaining every original training constraint",refined["Status"]==="COMPLETE"&&(mixedVariables/.refined["SolutionRules"])==={-1,1,0}&&AllTrue[oldTraining/.refined["SolutionRules"],#===0&]];
refinedCheck=Block[{ConstructiveDiagrams`Private`$completionParallelSettings=<|"Parallelization"->False|>},ConstructiveDiagrams`Private`completionSparseValidation[core+u core+v tree+w/pole,mixedVariables,refined,{},{},normalized,1,seed+13,rules,20,Function[Null],False]];
test["the original-coordinate refined solution passes fresh heldout cuts",TrueQ[refinedCheck["FamilyPass"]]&&refinedCheck["Report"]["Summary"]===<|"PASS"->Length[targets]|>];

End[];

Print["RETAINED AFFINE COEFFICIENT CHECKS: ", checks, "; FAILURES: ", Length[failures]];
If[failures === {}, Print["HEPCAT_TEST_COMPLETED:retained-affine-coefficients-tests.wl"], Print[failures]];
Quit[If[failures === {}, 0, 1]];
