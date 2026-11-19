(* Run with HEPCAT_BATCH_BINARY pointing to the native amplitude executable.
   All three comparisons start with identical prepared amplitudes and remainders. *)
$HistoryLength = 0;
nativeDirectory = DirectoryName[$InputFileName];
root = Replace[Environment["HEPCAT_ROOT"], Except[_String] :> DirectoryName[nativeDirectory]];
Get[FileNameJoin[{root,"source","HEPCAT.wl"}]];
Get[FileNameJoin[{nativeDirectory,"NativeAmplitude.wl"}]];
binary = Environment["HEPCAT_BATCH_BINARY"];
out = Replace[Environment["HEPCAT_FACTOR_RESULTS"], Except[_String] :>
  FileNameJoin[{root,"tests","results","native-factorization-benchmark"}]];
If[!DirectoryQ[out], CreateDirectory[out,CreateIntermediateDirectories->True]];
fail[msg_] := (Print[msg]; Exit[2]);
If[!StringQ[binary] || !FileExistsQ[binary], fail["Set HEPCAT_BATCH_BINARY to the executable."]];
sm = ReadModel[FileNameJoin[{root,"models"}],1];
params = {"EE"->2/5,"Mh"->6,"MW"->4,"MZ"->5,"SW"->3/5,"CW"->4/5};
fresh = {"EE"->3/7,"Mh"->11,"MW"->12,"MZ"->13,"SW"->5/13,"CW"->12/13};
details[rows_,rest_] := MapThread[
  <|"ShellRemainder"->#2,"PreparedExpression"->Last[#1],"FixedReferenceExpression"->Last[#1]+#2|>&,
  {rows,rest}];
(* Compare the vector spin polynomials off every cut. Model 2 is used only
   here, after completion. Its parameter scope can differ from model 1. *)
vectorComparison[expression_] := Module[
  {archive=ReadModel[FileNameJoin[{root,"models"}],2], entry, stored, parameters, mass, chart, coordinates,
   point, spin, values, coefficients, difference, keys, offCut, highEnergy, combined},
  entry=SelectFirst[archive[[2]],DeleteCases[Drop[#,-2],""]==={"W+","W+","W-","W-"}&];
  If[!ListQ[entry],fail["No archived four-W amplitude."]];
  stored=Times@@Take[entry,-2];
  offCut=Table[
    parameters={fresh,params}[[i]];mass="MW"/.parameters;
    chart=ConstructiveDiagrams`Private`completionHESymbolicExternalChart[Thread[(Mass/@Range[4])->ConstantArray[mass,4]]];
    If[!AssociationQ[chart] || !TrueQ[chart["ExternalMassShellsValidated"]] ||
       !TrueQ[chart["MomentumConservationValidated"]] || chart["InternalOnShellConditions"]=!={},
       fail["Invalid external chart."]];
    coordinates=chart["ExternalIdentityCoordinates"];
    point=Join[chart/.Thread[coordinates->Prime[Range[Length[coordinates]]+3 i]],<|"ReferenceSeed"->221+i|>];
    spin=point["SpinVariables"];
    values=MapThread[Expand[ConstructiveDiagrams`Private`spuriousEvaluateCurve[#1,point,ModelParameterRules[#2,parameters]]]&,
       {{expression,stored},{sm,archive}}];
    If[!AllTrue[values,PolynomialQ[#,spin]&],fail["Non-polynomial spin amplitude."]];
    coefficients=CoefficientRules[#,spin]&/@values;
    If[!AllTrue[Flatten[(Last/@#)&/@coefficients],NumericQ] ||
       !FreeQ[coefficients,_Real|_Missing|Indeterminate|_DirectedInfinity],fail["Invalid numerical coefficient."]];
    difference=CoefficientRules[Expand[values[[1]]-values[[2]]],spin];
    keys=Union@@((First/@#)&/@coefficients);
    <|"Point"->i,"CoefficientsCompared"->Length[keys],"UnequalCoefficients"->Length[difference]|>,
    {i,2}];
  combined=Join[ModelParameterRules[archive,fresh],ModelParameterRules[sm,fresh]];
  highEnergy[value_] := Module[{report=CheckAmplitudes[
    {Thread[(Mass/@Range[4])->Drop[First[channels],-3]],value},channels,sm,
    "Output"->"Details","RandomSeed"->381,"ParameterRules"->combined,
    "SpuriousPoles"->{},"ReferenceCheck"->False,"HighEnergyCheck"->True,
    "Parallelization"->False,"TimeConstraint"->35,"Samples"->3]},
    If[!AssociationQ[report],fail[report]];report["HighEnergySummary"]];
  <|"IndependentOffCutVsModel2"->offCut,"NativeHighEnergy"->highEnergy[expression],
    "ArchivedHighEnergy"->highEnergy[stored]|>
];
reports = <||>;
Do[
  proc = If[name==="WWWW",{"W+","W+","W-","W-"},{"h","h","h","h"}];
  diagrams = DiagramAmplitudes[CreateDiagrams[proc,sm]];
  channels = FactorizationAmplitudeChannels[proc,sm];
  batch = HEPCATNative`ReduceNativeAmplitude[diagrams,channels,binary];
  If[!AssociationQ[batch], fail[batch]];
  If[name==="hhhhMissingExchange",
    d = FirstPosition[batch["Diagrams"], p_ /; MatchQ[p, {_List,_}] &&
      !FreeQ[First[p],Mass[Multiparticle[1,2]]],Missing[],{1}];
    If[MissingQ[d], fail["No (1,2) Higgs exchange diagram."]];
    Do[AssociateTo[batch,k->Delete[batch[k],d]],{k,{"Diagrams","DiagramRemainders"}}]];
  ds = batch["Diagrams"]; cs = batch["FactorizationProducts"];
  dr = details[ds,batch["DiagramRemainders"]]; cr = details[cs,batch["FactorizationRemainders"]];
  wd = <|"Model"->sm,"PreparedAmplitudes"->ds,"RemainderData"->dr|>;
  wc = <|"Model"->sm,"PreparedAmplitudes"->cs,"RemainderData"->cr,
    "OriginalAmplitudes"->MapThread[ReplacePart[#1,-1->#2["FixedReferenceExpression"]] &,{cs,cr}]|>;
  wlSeconds = First[AbsoluteTiming[wl = TimeConstrained[
    CompleteFactorization[wd,wc,sm,"ExternalParticles"->proc,"ParameterRules"->params,
      "Parallelization"->False,"TimeConstraint"->60,"RandomSeed"->111,
      "ReturnDetails"->True,"ProgressReporting"->False], 65, $Aborted]]];
  nativeRuns = Table[HEPCATNative`CompleteNativeFactorization[batch,binary,"Workers"->w,"ReturnDetails"->True],
    {i,3},{w,{1,Automatic}}];
  If[!AllTrue[Flatten[nativeRuns],AssociationQ], fail[nativeRuns]];
  If[Length[DeleteDuplicates[Flatten[Map[#["Amplitude"][[2]]&,nativeRuns,{2}],1]]] =!= 1,
    fail["Amplitudes differed between worker counts or repetitions."]];
  amp = nativeRuns[[1,1]]["Amplitude"];
  refCheck[expr_] := Module[{report=CheckAmplitudes[expr,channels,sm,"Output"->"Details",
    "ParameterRules"->fresh,"Parallelization"->False,"RandomSeed"->381,
    "HighEnergyCheck"->False,"SpuriousPoles"->{},"ReferenceCheck"->False,"Samples"->4]},
    If[!AssociationQ[report],fail[report]];report["Summary"]];
  nativeCheck = refCheck[amp];
  If[nativeCheck =!= <|"PASS"->Length[channels]|>, fail[{name,nativeCheck}]];
  measures[runs_] := <|"Workers"->First[runs]["WorkersUsed"],
    "Route"->First[runs]["Route"],"Seconds"->Median[Lookup[runs,"CompletionSeconds"]],
    "RangeSeconds"->MinMax[Lookup[runs,"CompletionSeconds"]]|>;
  wlStatus = If[AssociationQ[wl],wl["Status"],"TIMEOUT"];
  result = <|"Process"->proc,"InputDiagrams"->Length[ds],"Channels"->Length[channels],
    "NativeSerial"->measures[nativeRuns[[All,1]]],"NativeParallel"->measures[nativeRuns[[All,2]]],
    "Wolfram"-><|"Status"->wlStatus,"Seconds"->wlSeconds|>,
    "IndependentPhysicalResidues"->nativeCheck|>;
  If[name=!="WWWW",
    check = TrueQ[Cancel[Last[amp]-Total[(Last[#]/.First[#])&/@diagrams]]===0];
    If[!check,fail["Four-Higgs symbolic amplitude differs from the complete diagram sum."]];
    AssociateTo[result,"ExactFullDiagramAgreement"->check]];
  If[name==="WWWW",result=Join[result,vectorComparison[Last[amp]]]];
  If[wlStatus==="COMPLETE", AssociateTo[result,"WolframPhysicalResidues"->refCheck[wl["Amplitude"]]]];
  AssociateTo[reports,name->result];Print[{name,result}];
  Export[FileNameJoin[{out,"factorization-sm-benchmark-results.json"}],<|
    "Platform"->$SystemID,"WolframVersion"->$Version,
    "Timing"->"In-memory completion only, using identical native-prepared input and retained remainders. Native reports the median of three runs per worker mode. Wolfram uses one serial call with a 60 s completion budget and a 65 s wall-time limit.",
    "Validation"->"Wolfram CheckAmplitudes using four fresh exact samples per raw channel at a second parameter point. Four-Higgs results are additionally compared symbolically to the full diagram sum including the quartic interaction. Four-W external spin polynomials are compared to SM model 2 at two off-cut points with consistent electroweak masses and angles. Physical pole tests alone cannot fix pole-free contributions.",
    "Cases"->reports|>,"RawJSON"],
  {name,{"hhhh","hhhhMissingExchange","WWWW"}}];
