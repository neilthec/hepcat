(* Validate every diagram and factorization channel for three identical
   fermion pairs against a fresh symbolic Wolfram generation. *)
$HistoryLength=0;
base=DirectoryName[$InputFileName];
root=Replace[Environment["HEPCAT_ROOT"],Except[_String]:>DirectoryName[base]];
Get[FileNameJoin[{root,"source","HEPCAT.wl"}]];
Get[FileNameJoin[{base,"NativeAmplitude.wl"}]];
binary=Environment["HEPCAT_BATCH_BINARY"];
If[!StringQ[binary] || !FileExistsQ[binary],Print["Set HEPCAT_BATCH_BINARY to the native executable."];Exit[1]];
directory=FileNameJoin[{root,"models"}];
dm=ReadModel[directory,3]; fm=ReadModel[directory,4];
numeric=Join[ModelParameterRules[dm,{"Me"->2,"EE"->1/3}],{ConstructiveDiagrams`z->3}];
proc={"e","E","E","e","e","E"};ext=Range[Length[proc]];
fail[tag_,detail_]:=(Print["FAIL ",tag," ",Short[detail,2]];Exit[1]);
rkey[rules_]:=Sort[DeleteDuplicates[Cases[rules,
  HoldPattern[Mass[Multiparticle[s__Integer]]->m_]:>{First@SortBy[{{s},Complement[ext,{s}]},{Length,Identity}],m}]]];
exactQ[x_]:=NumericQ[x] && FreeQ[x,_Real|_Missing|Indeterminate|_DirectedInfinity];
native=GenerateNativeAmplitude[proc,directory,3,4,binary,"PolePreference"->None];
If[!AssociationQ[native],fail["native generation",native]];
diagrams=DiagramAmplitudes[CreateDiagrams[proc,dm]];
channels=FactorizationAmplitudeChannels[proc,fm];
counts=Length/@{diagrams,native["Diagrams"],channels,native["FactorizationProducts"]};
If[counts=!={36,36,18,18},fail["counts",counts]];
Print["Generated all 36 diagrams and 18 channels."];
raw={First[#],ReduceSpinContractions[Last[#]]/.First[#]}&/@diagrams;
restored=MapThread[{First[#1],Last[#1]+#2}&,{native["Diagrams"],native["DiagramRemainders"]}];
keys=rkey/@First/@raw;newKeys=rkey/@First/@restored;
If[Sort[keys]=!=Sort[newKeys],fail["diagram cuts",{keys,newKeys}]];
pm=Drop[First[channels],-3];masses=Thread[(Mass/@ext)->pm];
chart=ConstructiveDiagrams`Private`completionHESymbolicExternalChart[masses/.numeric];
If[!AssociationQ[chart],fail["external chart",chart]];
(* Substitute the spin variables as well as the momentum coordinates before
   expanding. The resulting exact numbers retain all relative fermion signs. *)
Do[point=Join[chart /. Thread[chart["ExternalIdentityCoordinates"]->Prime[Range[Length[chart["ExternalIdentityCoordinates"]]]+off]]
                   /. Thread[chart["SpinVariables"]->Prime[Range[Length[chart["SpinVariables"]]]+3 off]],<|"ReferenceSeed"->73+off|>];
  evaluate[x_]:=RootReduce[ConstructiveDiagrams`Private`spuriousEvaluateCurve[x,point,numeric]];
  oldV=evaluate/@Last/@raw;newV=evaluate/@Last/@restored;
  If[!AllTrue[Join[oldV,newV],exactQ] || MemberQ[oldV,0],fail["off-cut sample",{off,oldV,newV}]];
  If[Sort[Transpose[{keys,oldV}]]=!=Sort[Transpose[{newKeys,newV}]],fail["signed off-cut diagrams",off]],
  {off,{2,7}}];
Print["All 36 signed, restored diagrams matched at two exact off-cut points."];
original=Most/@channels;prepared=Most/@native["FactorizationProducts"];
If[Sort[original]=!=Sort[prepared],fail["channel metadata",proc]];
Do[positions=Flatten[Position[original,k,{1}]];nativePositions=Flatten[Position[prepared,k,{1}]];
  c=channels[[First[positions]]];cut=List@@c[[-3]];pole=PropDen[Mom[c[[-3]]],c[[-2]]];
  meta=<|"ExternalLegs"->ext,"Cut"->cut,"Complement"->Complement[ext,cut],"Mass"->c[[-2]],"ExternalMassRules"->masses|>;
  extract[x_]:=With[{y=x pole},If[FreeQ[y,pole],y,Cancel[y]]];
  numerators=extract/@Join[Last/@channels[[positions]],Last/@native["FactorizationProducts"][[nativePositions]]];
  size=Length[positions];sampled=0;
  Do[pt=ConstructiveDiagrams`Private`residueAutomaticChart[meta,seed,28183,numeric];If[!AssociationQ[pt],Continue[]];
    pt=pt["Chart"]/.pt["Chart"]["PoleVariable"]->0;
    vals=Quiet[RootReduce[ConstructiveDiagrams`Private`residueEvaluateChart[#,pt,numeric]]&/@numerators];
    If[!AllTrue[vals,exactQ] || MemberQ[Take[vals,size],0],Continue[]];
    If[Sort[Take[vals,size]]=!=Sort[Drop[vals,size]],fail["signed on-cut residues",{cut,seed,vals}]];
    sampled++;If[sampled===2,Break[]],{seed,4,33}];
  If[sampled=!=2,fail["insufficient regular points",cut]],{k,DeleteDuplicates[original]}];
Print["All 18 signed residues matched at two regular exact points per cut."];
