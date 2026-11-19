(* Compare native and Mathematica construction at amplitude level. Each test
   resolves the full SU(3) tensors, including summed internal colors, before
   comparing the complex spin coefficients with their original signs. *)
$HistoryLength = 0;
base = DirectoryName[$InputFileName];
root = Replace[Environment["HEPCAT_ROOT"],Except[_String]:>DirectoryName[base]];
Get[FileNameJoin[{root,"source","HEPCAT.wl"}]];
Get[FileNameJoin[{base,"NativeAmplitude.wl"}]];
binary = Environment["HEPCAT_BATCH_BINARY"];
directory = FileNameJoin[{root,"models"}];
If[!StringQ[binary] || !FileExistsQ[binary],Print["Set HEPCAT_BATCH_BINARY to hepcat-reduce-amplitude."];Exit[1]];
dm = ReadModel[directory,1]; fm = ReadModel[directory,2];
numeric = Join[ModelParameterRules[dm,{"Me"->2,"EE"->1/3,"GG"->2/5,"Mu"->2,"Mc"->3,
    "MW"->4,"MZ"->5,"SW"->3/5,"CW"->4/5,"Mh"->6}],{ConstructiveDiagrams`z->3}];
fail[label_,data_]:=(Print["FAIL ",label," ",Short[data,2]];Throw[False]);
normalize[e_]:= e /. HoldPattern[Spinor["Xi",kind_,index_,rest___]]:>Spinor["Xi",kind,1,rest];
(* For this process there is only one external massless reference. It may
   receive distinct local names in different diagrams. Use one shared exact
   spinor value on each chart in both evaluations. *)
run[proc_]:= Catch[Module[{n=Length[proc],diagrams,products,native,restored,project,rep,dc,cc,nd,nc,
  rkey,oldKeys,newKeys,oldChannelKeys,newChannelKeys,ext,pm,caseChart,q,vars,point,evaluate,values,good,
  raw,left,right,pole,cut,meta,candidates,regular,exactRationalQ,nativeIndex,refPositions,nativePositions},
  native=GenerateNativeAmplitude[proc,directory,1,2,binary,"PolePreference"->None];
  If[!AssociationQ[native],fail["native generation",native]];
  diagrams=DiagramAmplitudes[CreateDiagrams[proc,dm]]; products=FactorizationAmplitudeChannels[proc,fm];
  nd=Length[diagrams];nc=Length[products];
  Print[{proc,"diagrams and products",nd,nc,Length[native["Diagrams"]],Length[native["FactorizationProducts"]]}];
  If[{Length[native["Diagrams"]],Length[native["FactorizationProducts"]]}=!={nd,nc},fail["counts",proc]];
  ext=Range[n];pm=Drop[First[products],-3];
  diagrams={First[#],normalize[ReduceSpinContractions[Last[#]]/. First[#]]}& /@ diagrams;
  restored=MapThread[{First[#1],normalize[Last[#1]+#2]}&,{native["Diagrams"],native["DiagramRemainders"]}];
  rep=ConstructiveDiagrams`Private`colorParticleRepresentation[#,dm]& /@ proc;
  project=ConstructiveDiagrams`Private`colorProjectionData[Join[diagrams,restored],Join[products,native["FactorizationProducts"]],rep];
  If[!AssociationQ[project],fail["SU(3) projection",project]];
  dc=project["DiagramCoefficients"];cc=Map[normalize,project["ChannelCoefficients"],{2}];
  Print[{"SU(3) rank",Length[project["Basis"]]}];
  rkey[massRules_]:= Sort[DeleteDuplicates[Cases[massRules,
    HoldPattern[Mass[Multiparticle[s__Integer]]->m_]:>{First@SortBy[{{s},Complement[ext,{s}]},{Length,Identity}],m}]]];
  oldKeys=rkey/@First/@diagrams;newKeys=rkey/@First/@restored;
  oldChannelKeys=Most/@products;newChannelKeys=Most/@native["FactorizationProducts"];
  If[Sort[oldKeys]=!=Sort[newKeys] || Sort[oldChannelKeys]=!=Sort[newChannelKeys],fail["channels or masses differ",proc]];
  caseChart=ConstructiveDiagrams`Private`completionHESymbolicExternalChart[Thread[(Mass/@ext)->pm]/. numeric];
  If[!AssociationQ[caseChart],fail["chart",caseChart]];vars=caseChart["SpinVariables"];
  Do[point=Join[caseChart /. Thread[caseChart["ExternalIdentityCoordinates"]->Prime[Range[Length[caseChart["ExternalIdentityCoordinates"]]]+offset]],
       <|"ReferenceSeed"->41+offset|>];
    evaluate[e_]:=Module[{v=Quiet[ConstructiveDiagrams`Private`spuriousEvaluateCurve[e,point,numeric]]},
      v=Expand[v];If[FreeQ[v,_Missing|_Real|Indeterminate|_Failure|_DirectedInfinity] && PolynomialQ[v,vars] &&
        AllTrue[Last/@CoefficientRules[v,vars],NumericQ],v,fail["unevaluable off-cut spin polynomial",v]]];
    values=Map[evaluate,dc,{2}];
    If[Sort@MapThread[List,{oldKeys,Take[values,nd]}]=!=Sort@MapThread[List,{newKeys,Drop[values,nd]}],
      fail["off-cut diagram color coefficients",offset]],{offset,2}];
  Print[{"off-cut spin polynomial diagrams matched",nd,"points",2}];
  exactRationalQ[v_] := NumericQ[v] && FreeQ[v,_Real|_Missing|Indeterminate|_DirectedInfinity];
  Do[
    refPositions=Flatten[Position[oldChannelKeys,k,{1}]];
    nativePositions=Flatten[Position[newChannelKeys,k,{1}]];
    raw=products[[First[refPositions]]];cut=List@@raw[[-3]];pole=PropDen[Mom[raw[[-3]]],raw[[-2]]];
    meta=<|"ExternalLegs"->ext,"Cut"->cut,"Complement"->Complement[ext,cut],"Mass"->raw[[-2]],
      "ExternalMassRules"->Thread[(Mass/@ext)->pm]|>;
    left=cc[[refPositions]];right=cc[[nc+nativePositions]];
    candidates=Map[Function[x,With[{num=x*pole},If[FreeQ[num,pole],num,Cancel[num]]]],Join[left,right],{2}];
    regular=0;
    Do[point=ConstructiveDiagrams`Private`residueAutomaticChart[meta,s,28183,numeric];If[!AssociationQ[point],Continue[]];
      point=point["Chart"]/.point["Chart"]["PoleVariable"]->0;
      (* Keep the implicit x factor and its compensating cut-line spinors in
         the same expression while assigning the cut-photon spinor frame. *)
      values=Quiet@Map[RootReduce[ConstructiveDiagrams`Private`spuriousEvaluateCurve[
          ConstructiveDiagrams`Private`residueExpandCutXFactors[#,point],point,numeric]]&,candidates,{2}];
      If[AllTrue[Flatten[values],exactRationalQ] && AnyTrue[Flatten[Take[values,Length[left]]],#=!=0&],
        If[Sort[Take[values,Length[left]]]=!=Sort[Drop[values,Length[left]]],fail["on-cut color coefficients",{cut,raw[[-2]],values}]];
        regular++;If[regular===2,Break[]]],{s,4,33}];
    If[regular=!=2,fail["insufficient nonsingular cut points",{cut,raw[[-2]]}]],
    {k,DeleteDuplicates[oldChannelKeys]}];
  Print[{"on-cut color coefficient vectors matched",nc,"points per pole",2}];True]];
cases={{"u","U","C","c"},{"u","U","C","c","A.+"}};
passedCases=0;
Do[If[TrueQ[run[proc]],passedCases++,Exit[1]],{proc,cases}];
If[passedCases=!=Length[cases],Print["Incomplete colored-channel comparison."];Exit[1]];
Print["Colored amplitude generation checks passed."];
