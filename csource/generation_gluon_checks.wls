(* Run against the installed HEPCAT kernel and the built hepcat-reduce-amplitude executable.
   Signed four-gluon pole residues are compared in the independent SU(3) basis, obtained
   from all 8^4 external color assignments with the internal adjoint index contracted. *)
$HistoryLength=0;
base=DirectoryName[$InputFileName];
root=Replace[Environment["HEPCAT_ROOT"],Except[_String]:>DirectoryName[base]];
Get[FileNameJoin[{root,"source","HEPCAT.wl"}]];
Get[FileNameJoin[{base,"NativeAmplitude.wl"}]];
directory=FileNameJoin[{root,"models"}];
binary=Environment["HEPCAT_BATCH_BINARY"];
If[!StringQ[binary] || !FileExistsQ[binary],Print["Set HEPCAT_BATCH_BINARY to the native executable."];Exit[1]];
model=ReadModel[directory,2];
rules=Thread[(Mass/@Range[4])->0];
params=Append[ModelParameterRules[model,{"GG"->2/5}],ConstructiveDiagrams`z->3];
fail[reason_,details_]:=(Print["FAIL ",reason," ",details];Throw[False]);
checkCase[proc_]:=Catch[Module[{native,actual,ref,expectedZero,project,preps,cut,meta,
    pole,point,chart,t,vals,counts,nonzero,row,lhs,rhs,refByCut,used={}},
  native=GenerateNativeAmplitude[proc,directory,1,2,binary,"Generate"->"Channels","PolePreference"->None];
  If[!AssociationQ[native],fail["native generation",native]];
  ref=FactorizationAmplitudeChannels[proc,model];actual=native["FactorizationProducts"];
  expectedZero=Count[proc,"G.+"]=!=2;
  If[!ListQ[ref] || Sort[Most/@actual]=!=Sort[Most/@ref],fail["channel metadata",{proc,Most/@actual,Most/@ref}]];
  If[ref==={} || And@@(Last[#]===0&/@Join[ref,actual]),
    If[!expectedZero,fail["missing nonzero residues",proc]];
    Print[{proc,"exactly empty or zero channels",Length[actual]}];Return[True]];
  project=ConstructiveDiagrams`Private`colorProjectionData[{},Join[ref,actual],ConstantArray[8,4]];
  If[!AssociationQ[project] || Length[project["Basis"]]=!=2,fail["SU(3) rank",project]];
  refByCut=AssociationThread[Most/@ref,Take[project["ChannelCoefficients"],Length[ref]]];
  Do[
    row=actual[[i]];cut=List@@row[[-3]];pole=PropDen[Mom[row[[-3]]],0];
    lhs=Lookup[refByCut,Key[Most[row]]];
    rhs=project["ChannelCoefficients"][[Length[ref]+i]];
    If[!VectorQ[lhs],fail["missing matching cut",cut]];
    preps=Map[ConstructiveDiagrams`Private`factorizationExternalMasslessCancellation[Cancel[pole*#],rules]&, {lhs,rhs},{2}];
    meta=<|"ExternalLegs"->Range[4],"Cut"->cut,"Complement"->Complement[Range[4],cut],
      "Mass"->0,"ExternalMassRules"->rules|>;
    counts={0,0};nonzero={0,0};
    Do[point=ConstructiveDiagrams`Private`residueAutomaticChart[meta,s,28183,params];
      If[!AssociationQ[point],Continue[]];
      chart=point["Chart"];t=chart["PoleVariable"];point=chart/.t->0;
      vals=Quiet@Map[RootReduce[ConstructiveDiagrams`Private`spuriousEvaluateCurve[#,point,params]]&,preps,{2}];
      If[!AllTrue[Flatten[vals],NumericQ[#] && FreeQ[#,_Real|Indeterminate|_DirectedInfinity]&],
        (* Cancel a removable 0/0 along the transverse cut coordinate before taking the limit. *)
        vals=Quiet@Map[Function[term,With[{f=Cancel[Together[
             ConstructiveDiagrams`Private`spuriousEvaluateCurve[term,chart,params]]]},
          Quiet[RootReduce[Limit[f,t->0]]]]],preps,{2}]];
      If[!AllTrue[Flatten[vals],NumericQ[#] && FreeQ[#,_Real|Indeterminate|_DirectedInfinity]&],Continue[]];
      If[First[vals]=!=Last[vals],fail["signed color-vector discrepancy",{proc,cut,s,vals}]];
      If[expectedZero && !AllTrue[First[vals],#===0&],fail["vanishing helicity",{proc,cut,s,vals}]];
      counts[[1+Mod[s,2]]]++;
      If[AnyTrue[First[vals],#=!=0&],nonzero[[1+Mod[s,2]]]++];
      If[(Min[counts]>=2 && (expectedZero || Total[nonzero]>=3)),Break[]],{s,1,16}];
    If[(Min[counts]<2 || (!expectedZero && Total[nonzero]<3)),fail["insufficient regular samples",{proc,cut,counts,nonzero}]];
    AppendTo[used,{cut,counts,nonzero}],{i,Length[actual]}];
  Print[{proc,"SU(3) rank",Length[project["Basis"]],"cuts, regular and nonzero samples by branch",used}];True]];
cases=Tuples[{"G.+","G.-"},4];
passed=0;
Do[If[TrueQ[checkCase[proc]],passed++,Exit[1]],{proc,cases}];
If[passed=!=Length[cases],Print["Incomplete four-gluon color check."];Exit[1]];
Print["Four-gluon SU(3) residue checks passed."];
