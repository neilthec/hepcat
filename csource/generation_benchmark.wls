(* Run with a licensed kernel. The native stage timer excludes model I/O and bridge transfer. *)
$HistoryLength = 0;
root = Replace[Environment["HEPCAT_ROOT"], Except[_String] :> DirectoryName[DirectoryName[$InputFileName]]];
Get[FileNameJoin[{root,"source","HEPCAT.wl"}]];
Get[FileNameJoin[{DirectoryName[$InputFileName],"NativeAmplitude.wl"}]];
binary = Replace[Environment["HEPCAT_BATCH_BINARY"], Except[_String] :>
    FileNameJoin[{root,"tests","results","native-reduce-build","hepcat-reduce-amplitude"}]];
models = FileNameJoin[{root,"models"}];
out = Replace[Environment["HEPCAT_GENERATION_RESULTS"],Except[_String] :>
    FileNameJoin[{root,"tests","results","native-generation"}]];
If[!DirectoryQ[out], CreateDirectory[out,CreateIntermediateDirectories->True]];
check[test_,label_] := If[!TrueQ[test], Print["FAIL ",label]; Exit[1]];
processes = {
  {{"e","E","A.+","A.+"},3,4}, {{"e","E","A.+","A.-"},3,4},
  {{"e","E","A.+","A.+","A.-"},3,4}, {{"e","E","A.+","A.+","A.+","A.+"},3,4},
  {{"e","E","E","e"},3,4}, {{"h","h","h","h"},1,2},
  {{"W+","W+","W-","W-"},1,2}, {{"Z","Z","W+","W-"},1,2}};
(* Orient each diagram edge by its shorter side, then lexicographically on ties. *)
edges[rules_,n_] := Sort[DeleteDuplicates[Cases[rules,
  HoldPattern[Mass[m_Multiparticle] -> mass_] :>
    {First[SortBy[{List@@m,Complement[Range[n],List@@m]}, {Length,Identity}]], mass}]]];
fields = {"Diagrams","DiagramRemainders","FactorizationProducts","FactorizationRemainders"};
run[{process_,vertex_,factor_}] := Module[{dm,fm,n=Length[process],dg,ch,rawTime,results,auto,serial,
  ext,masses,params,fix,chart,sample,spin,eval,expected,actual,restore,cut,pole,ac,matches,
  values,seen,raw,point,passed,record},
  dm=ReadModel[models,vertex]; fm=ReadModel[models,factor];
  {rawTime,{dg,ch}} = AbsoluteTiming[{DiagramAmplitudes[CreateDiagrams[process,dm]],FactorizationAmplitudeChannels[process,fm]}];
  results=Table[GenerateNativeAmplitude[process,models,vertex,factor,binary,"Workers"->w],
    {trial,3},{w,If[OddQ[trial],{1,Automatic},{Automatic,1}]}];
  check[AllTrue[Flatten[results],AssociationQ],{process,"native generation",results}];
  results[[2]]=Reverse[results[[2]]]; serial=results[[All,1]]; auto=results[[All,2]];
  check[SameQ@@(KeyTake[#,fields]& /@ Flatten[results]),{process,"row equality across schedules"}];
  raw=First[auto]; ext=Cases[First[First[dg]],HoldPattern[Mass[_Integer] -> _]];
  masses=Last/@ext;
  params=Join[ModelParameterRules[dm,If[vertex==3,{"Me"->2,"EE"->1/3},
    {"EE"->1/3,"MW"->4,"MZ"->5,"CW"->4/5,"SW"->3/5,"Mh"->6}]],{ConstructiveDiagrams`z->3}];
  fix[row_] := If[Count[masses,0]>=2, HEPCATNative`Private`fixedExpression[row,masses,"Cyclic",{}],
    ReduceSpinContractions[Last[row]] /. First[row]];
  chart=ConstructiveDiagrams`Private`completionHESymbolicExternalChart[ext/.params];
  check[AssociationQ[chart],{process,"external chart"}]; spin=chart["SpinVariables"];
  restore=MapThread[Plus,{Last/@raw["Diagrams"],raw["DiagramRemainders"]}];
  Do[sample=Join[chart /. Thread[chart["ExternalIdentityCoordinates"]->
      Prime[Range[Length[chart["ExternalIdentityCoordinates"]]]+offset]],<|"ReferenceSeed"->23+offset|>];
    eval[x_] := Module[{v=Expand[ConstructiveDiagrams`Private`spuriousEvaluateCurve[x,sample,params]]},
      check[FreeQ[v,_Missing|_Real|_DirectedInfinity|Indeterminate] && PolynomialQ[v,spin] &&
        AllTrue[Last/@CoefficientRules[v,spin],NumericQ],{process,"exact spin polynomial"}];v];
    expected=Sort[{edges[First[#],n],eval[fix[#]]}& /@ dg];
    actual=Sort[MapThread[{edges[First[#1],n],eval[#2]}&,{raw["Diagrams"],restore}]];
    check[expected===actual,{process,"diagram and remainder at external point",offset}],{offset,2}];
  check[Sort[Most/@ch]===Sort[Most/@raw["FactorizationProducts"]],{process,"channel labels and masses"}];
  Do[cut=row[[-3]]; pole=PropDen[Mom[cut],row[[-2]]];
    ac=<|"ExternalLegs"->Range[n],"Cut"->List@@cut,"Complement"->Complement[Range[n],List@@cut],
      "Mass"->row[[-2]],"ExternalMassRules"->ext|>;
    matches=Select[raw["FactorizationProducts"],Most[#]===Most[row]&];
    check[Length[matches]==1,{process,cut,"unique channel"}];
    values=Function[x,With[{v=pole*x},If[FreeQ[v,pole],v,Cancel[v]]]] /@
      {fix[{ext,Last[row]}],Last[First[matches]]};
    seen=0;
    Do[point=ConstructiveDiagrams`Private`residueAutomaticChart[ac,frame,28183,params];
      If[!AssociationQ[point],Continue[]];
      sample=point["Chart"]/.point["Chart"]["PoleVariable"]->0;
      passed=Quiet[ConstructiveDiagrams`Private`spuriousEvaluateCurve[#,sample,params]&/@values];
      If[!AllTrue[passed,NumericQ[#]&&FreeQ[#,_Real|Indeterminate|_DirectedInfinity]&] || First[passed]===0,Continue[]];
      check[SameQ@@passed,{process,cut,"residue",frame}]; If[++seen==2,Break[]],{frame,5,35}];
    check[seen==2,{process,cut,"insufficient nonsingular residue samples"}],{row,ch}];
  record=<|"Process"->process,"Diagrams"->Length[dg],"Channels"->Length[ch],"Workers"->raw["WorkersUsed"],
    "SerialSeconds"->Median[Lookup[serial,"GenerationAndReductionSeconds"]],
    "ParallelSeconds"->Median[Lookup[auto,"GenerationAndReductionSeconds"]],
    "ModelReadSeconds"->Median[Lookup[auto,"ModelReadSeconds"]],
    "SerialPeakBytes"->Median[Lookup[serial,"ProcessPeakRSSBytes"]],
    "ParallelPeakBytes"->Median[Lookup[auto,"ProcessPeakRSSBytes"]],
    "WolframRawGenerationSeconds"->rawTime,"ExactSamplesPerDiagramAndChannel"->2|>;
  Print[record]; record
];
records = run /@ processes;
Export[FileNameJoin[{out,"generation-benchmark-results.json"}],
  <|"Platform" -> $SystemID, "LogicalProcessors" -> $ProcessorCount, "WolframVersion" -> $Version,
    "Timing" -> "Median of three interleaved fresh native processes for each worker mode. The stage includes topology, sewing and reduction. Model loading and bridge transfer are outside the stage timer. The Wolfram timing measures raw generation only, before reduction.",
    "Memory" -> "Median process resident high-water mark through generation, including model loading. Bytes.",
    "Cases" -> records|>, "RawJSON"];
