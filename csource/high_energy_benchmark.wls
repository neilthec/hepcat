(* Run in a licensed kernel after the C++ Release build. HEPCAT_BATCH_BINARY
   is the absolute path of hepcat-reduce-amplitude. HEPCAT_HE_CASES optionally
   limits the run to comma-separated names from the cases below. *)
$HistoryLength = 0;
nativeDirectory = DirectoryName[$InputFileName];
repo = Replace[Environment["HEPCAT_ROOT"], Except[_String] :> DirectoryName[nativeDirectory]];
Get[FileNameJoin[{repo,"source","HEPCAT.wl"}]];
Get[FileNameJoin[{nativeDirectory,"NativeAmplitude.wl"}]];
binary = Environment["HEPCAT_BATCH_BINARY"];
out = Replace[Environment["HEPCAT_HE_RESULTS"], Except[_String] :>
  FileNameJoin[{repo,"tests","results","native-high-energy-benchmark"}]];
fail[msg_] := (Print["ERROR ",msg]; Exit[2]);
If[!StringQ[binary] || !FileExistsQ[binary], fail["Set HEPCAT_BATCH_BINARY to the native executable."]];
If[!DirectoryQ[out], CreateDirectory[out,CreateIntermediateDirectories->True]];
sm = ReadModel[FileNameJoin[{repo,"models"}],1];
qed = ReadModel[FileNameJoin[{repo,"models"}],3];
qedChannels = ReadModel[FileNameJoin[{repo,"models"}],4];
ewPoint[e_,h_,w_,z_] := {"EE"->e,"Mh"->h,"MW"->w,"MZ"->z,"CW"->w/z,"SW"->Sqrt[1-(w/z)^2]};
ew = {ewPoint[2/5,6,4,5],ewPoint[3/7,11,12,13],ewPoint[4/9,8,15,17],ewPoint[2/7,9,7,25],
  ewPoint[4/11,12,21,29],ewPoint[6/13,13,35,37],ewPoint[3/8,14,40,41],ewPoint[3/10,15,45,53]};
em = {{"EE"->2/5,"Me"->3/2},{"EE"->3/7,"Me"->5/2},{"EE"->4/9,"Me"->7/2},{"EE"->2/7,"Me"->9/2}};
(* Independent model/kinematic comparisons use parameters that neither solver
   saw. Generic rational external points keep the entire spin polynomial. *)
heldOutEW = {ewPoint[5/11,10,24,25],ewPoint[7/13,9,8,17]};
heldOutEM = {{"EE"->5/11,"Me"->11/2},{"EE"->7/13,"Me"->13/2}};
cases = <|
  "hhhh" -> {"h","h","h","h"},
  "eePP" -> {"e","E","A.+","A.+"},
  "eePPM" -> {"e","E","A.+","A.+","A.-"},
  "eePPPP" -> {"e","E","A.+","A.+","A.+","A.+"},
  "WWWW" -> {"W+","W+","W-","W-"}|>;
selection = Replace[Environment["HEPCAT_HE_CASES"],
  {v_String :> StringSplit[v,","], _ :> Keys[cases]}];
If[!SubsetQ[Keys[cases],selection],fail["Unknown name in HEPCAT_HE_CASES."]];

factorInput[proc_,dmodel_,cmodel_,channels_,mode_] := Module[{diagrams,d,c,batch,fit},
  diagrams=DiagramAmplitudes[CreateDiagrams[proc,dmodel]];
  fit = If[mode==="WolframAllPhysical",
    d=PrepareAmplitudes[diagrams,dmodel,"PolePreference"->"AllPhysical","ReturnDetails"->True,
      "ProgressReporting"->False,"Parallelization"->False];
    c=PrepareAmplitudes[channels,cmodel,"PolePreference"->"AllPhysical","ReturnDetails"->True,
      "ProgressReporting"->False,"Parallelization"->False];
    HEPCATNative`CompleteNativeFactorization[d,c,binary],
    batch=HEPCATNative`ReduceNativeAmplitude[diagrams,channels,binary,
      "PolePreference"->If[mode==="Cyclic","Cyclic",None]];
    If[!AssociationQ[batch],fail[batch]];
    HEPCATNative`CompleteNativeFactorization[batch,binary]];
  If[!MatchQ[fit,{_List,_}],fail[fit]]; fit];

maxRSS[runs_, key_] := With[{values = Lookup[runs,key,Missing["Unavailable"]]},
  If[AllTrue[values,IntegerQ[#] && #>0 &],Max[values],Null]];

timeNative[seed_,model_,points_] := Module[{times=<|"Serial"->{},"Automatic"->{}|>,first=None,r,elapsed,key},
  Do[Do[
    {elapsed,r}=AbsoluteTiming[HEPCATNative`CompleteNativeHighEnergy[seed,model,binary,"Workers"->w,
      "ParameterPoints"->points,"ReturnDetails"->True]];
    If[!AssociationQ[r],fail[{"Native completion failed",r}]];
    If[first===None,first=r["Amplitude"],
      If[first=!=r["Amplitude"],fail["Different native answers across worker counts or runs."]]];
    key=If[w===1,"Serial","Automatic"];
    AssociateTo[times,key->Append[times[key],Join[KeyTake[r,{"CompletionSeconds","WorkersUsed",
      "Route","Rank","Candidates","TrainingSamples","ValidationSamples","ValidationSpinStates","ExhaustiveSpin","Bound","ProcessPeakRSSBytes","PreCompletionPeakRSSBytes"}],
      <|"BridgeSeconds"->elapsed|>]]],
    {w,If[OddQ[i],{1,Automatic},{Automatic,1}]}],{i,3}];
  {first,Function[runs,Join[KeyDrop[First[runs],{"CompletionSeconds","BridgeSeconds","ProcessPeakRSSBytes","PreCompletionPeakRSSBytes"}],
    <|"NativeSeconds"->Median[Lookup[runs,"CompletionSeconds"]],"BridgeSeconds"->Median[Lookup[runs,"BridgeSeconds"]],
      "NativeRangeSeconds"->MinMax[Lookup[runs,"CompletionSeconds"]],
      "ProcessPeakRSSBytes"->maxRSS[runs,"ProcessPeakRSSBytes"],
      "PreCompletionPeakRSSBytes"->maxRSS[runs,"PreCompletionPeakRSSBytes"]|>]]/@times}];

checkSummary[seed_,channels_,model_,parameters_] := Module[{report},
  report=CheckAmplitudes[seed,channels,model,"Output"->"Details","ParameterRules"->parameters,
    "Samples"->2,"HighEnergySamples"->2,"ReferenceSamples"->2,"TimeConstraint"->45,
    "RandomSeed"->1733,"Parallelization"->False,"HighEnergyCheck"->True,"ReferenceCheck"->True];
  If[!AssociationQ[report],fail[report]];
  KeyTake[report,{"Summary","PhysicalSummary","HighEnergySummary","SpuriousSummary","ReferenceSummary"}]];

offCut[seed_,candidate_,model_,proc_,fullModel_,points_] := Module[
  {entry,reference,chart,coords,point,spin,val,expected,values,coeff,diffs},
  entry=SelectFirst[fullModel[[2]],DeleteCases[Drop[#,-2],""]===proc&];
  If[!ListQ[entry],fail["Missing validation-only full model entry."]];
  reference=Times@@Take[entry,-2];
  Table[
    chart=ConstructiveDiagrams`Private`completionHESymbolicExternalChart[First[seed]/.ModelParameterRules[model,points[[j]]]];
    If[!AssociationQ[chart] || !TrueQ[chart["ExternalMassShellsValidated"]] ||
      !TrueQ[chart["MomentumConservationValidated"]] || chart["InternalOnShellConditions"]=!={},
      fail["Invalid external chart for independent full-expression comparison."]];
    coords=chart["ExternalIdentityCoordinates"];
    point=Join[chart/.Thread[coords->Prime[Range[Length[coords]]+11 j]],<|"ReferenceSeed"->1337+j|>];
    spin=point["SpinVariables"];
    val[expr_,m_]:=Expand[ConstructiveDiagrams`Private`spuriousEvaluateCurve[expr,point,ModelParameterRules[m,points[[j]]]]];
    expected=val[reference,fullModel];values={val[Last[seed],model],val[Last[candidate],model],expected};
    If[!AllTrue[values,PolynomialQ[#,spin]&] || !FreeQ[values,_Real|_Missing|Indeterminate|_DirectedInfinity],
      fail["Invalid spin polynomial at the independent model comparison."]];
    coeff=CoefficientRules[#,spin]&/@values;
    If[!AllTrue[Flatten[(Last/@#)&/@coeff],NumericQ],fail["Non-numerical spin coefficients."]];
    diffs=Length[CoefficientRules[Expand[#-expected],spin]]&/@Most[values];
    <|"Point"->j,"SpinCoefficientsCompared"->Length[Union@@((First/@#)&/@coeff)],
      "UnequalSeedCoefficients"->diffs[[1]],"UnequalNativeCoefficients"->diffs[[2]]|>,
    {j,Length[points]}]];

reports=<||>;
Do[
  process=cases[name]; electroweak=MemberQ[{"WWWW","hhhh"},name];
  diagramModel=If[electroweak,sm,qed];channelModel=If[electroweak,sm,qedChannels];
  params=If[electroweak,ew,em]; independentPoints=If[electroweak,heldOutEW,heldOutEM];
  mode=Which[electroweak,"Native",name==="eePP","WolframAllPhysical",True,"Cyclic"];
  rawChannels=FactorizationAmplitudeChannels[process,channelModel];
  seed=factorInput[process,diagramModel,channelModel,rawChannels,mode];
  Print[{"Benchmarking",name,"InputLeafCount",LeafCount[Last[seed]]}];
  wlPeakBefore=MaxMemoryUsed[];
  {wlTime,wl}=AbsoluteTiming[TimeConstrained[
    CompleteHighEnergy[seed,rawChannels,channelModel,"Output"->"Details","Samples"->3,
      "ValidationSamples"->3,"ParameterRules"->First[params],"ValidationParameterRules"->{params[[2]]},
      "RandomSeed"->123,"Parallelization"->False,"ProgressReporting"->False,"TimeConstraint"->60],75,$TimedOut]];
  wlPeakAfter=MaxMemoryUsed[];
  wr=If[AssociationQ[wl],<|"Status"->wl["Status"],"Seconds"->wlTime,
      "CorrectionTermCount"->Length[wl["CorrectionBasis"]],
      "Diagnostics"->Lookup[wl,"Diagnostics",{}]|>,<|"Status"->"TIMEOUT","Seconds"->wlTime|>];
  AssociateTo[wr,<|"KernelHighWaterBeforeBytes"->wlPeakBefore,"KernelHighWaterAfterBytes"->wlPeakAfter|>];
  Print[{"Wolfram",wr["Status"],wlTime}];
  {nativeAmp,nativeTiming}=timeNative[seed,channelModel,params];
  before=checkSummary[seed,rawChannels,channelModel,First[independentPoints]];
  after=If[nativeAmp===seed,before,checkSummary[nativeAmp,rawChannels,channelModel,First[independentPoints]]];
  If[after["HighEnergySummary"]=!=<|"PASS"->1|> ||
      after["PhysicalSummary"]=!=<|"PASS"->Length[rawChannels]|>,fail[{"Independent check failed",after}]];
  If[Keys[after["Summary"]]=!={"PASS"},fail[{"Additional independent amplitude checks did not pass",after}]];
  (* Full amplitudes are read only after completing and independently testing. *)
  full=If[electroweak,ReadModel[FileNameJoin[{repo,"models"}],2],qedChannels];
  off=offCut[seed,nativeAmp,channelModel,process,full,independentPoints];
  If[AnyTrue[off,#["UnequalNativeCoefficients"] =!= 0 &],fail[{"Independent spin polynomial mismatch",off}]];
  report=<|"Process"->process,"FactorizationInput"->mode,"ExpectedPower"->4-Length[process],
    "SeedLeafCount"->LeafCount[Last[seed]],"IdenticalSeedExpression"->SameQ[seed,nativeAmp],
    "Wolfram"->wr,"Native"->nativeTiming,"IndependentSeedChecks"->before,"IndependentNativeChecks"->after,
    "IndependentOffCut"->off|>;
  AssociateTo[reports,name->report];Print[{name,nativeTiming["Automatic"],after["Summary"],off}];
  Export[FileNameJoin[{out,name<>".json"}],report,"RawJSON"];
  Export[FileNameJoin[{out,"high-energy-benchmark-results.json"}],<|"Platform"->$SystemID,
    "LogicalProcessors"->$ProcessorCount,"WolframVersion"->$Version,
    "Timing"->"The same factorization result is constructed before each comparison. Native times are medians of three runs per worker mode, alternating order and excluding transfer. BridgeSeconds includes transfer and process startup. Wolfram uses one serial call, a 60 s option and a 75 s wall limit.",
    "Memory"->"Native RSS fields are the largest process resident high-water marks before completion and after completion across three fresh processes; pre-completion includes loading and parsing. Wolfram MaxMemoryUsed[] records cumulative kernel allocation high-water marks before and after the call in the same live kernel. These measurements describe different quantities.",
    "Scope"->"CompleteHighEnergy includes its integrated validation suite. The native stage runs its own fixed-angle checks and local-correction fit. Independent Wolfram CheckAmplitudes and comparisons against complete model expressions run outside both timers.",
    "Cases"->reports|>,"RawJSON"],
  {name,selection}];
