(* Independent fermion-word grading and mixed-flavor sewing regressions. *)
fsRoot=DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Global`$HEPCATpath=FileNameJoin[{fsRoot,"source"}];
Get[FileNameJoin[{$HEPCATpath,"HEPCAT.wl"}]];
$HistoryLength=0;
fsChecks=0;fsFailures={};
SetAttributes[fsCheck,HoldRest];
fsCheck[name_,assertion_]:=Module[{value},fsChecks++;
 value=Quiet[Check[TimeConstrained[assertion,120,$TimedOut],$Failed]];
 If[!TrueQ[value],AppendTo[fsFailures,name];Print["FAIL: ",name]]];
fsGrade[a_Integer,b_Integer]:=ConstructiveDiagrams`Private`factorizationFermionReferenceSign[
 {Range[2a],Range[2b]}];
fsCheck["cubic fermion sewing preserves established lower-point phases",
 And@@Flatten[Table[fsGrade[1,r]===1&&fsGrade[r,1]===1,{r,1,5}]]];
fsCheck["two multiline fermion amplitudes include their spectator shuffle",
 fsGrade[2,2]===-1 && fsGrade[3,2]===1 && fsGrade[2,4]===-1];
fsCheck["spectator shuffles associate under successive fermion sewing",
 And@@Flatten[Table[fsGrade[a,b]fsGrade[a+b-1,c]===fsGrade[b,c]fsGrade[a,b+c-1],
 {a,1,4},{b,1,4},{c,1,4}]]];
fsCheck["malformed odd fermion words cannot yield an invented grading",
 ConstructiveDiagrams`Private`factorizationFermionReferenceSign[{{a,b,c},{d,e}}]===$Failed];
cm=ReadModel[FileNameJoin[{fsRoot,"models"}],2];
fm=ReadModel[FileNameJoin[{fsRoot,"model_feynman"}],2];
cMass=ModelParameter[cm,"Me"];cCharge=ModelParameter[cm,"EE"];
fMass=ModelParameter[fm,"Me"];fCharge=ModelParameter[fm,"EE"];
flavors={{"e","E",11},{"m","M",13},{"l","L",15}};
cParticles=Join[{cm[[1,1]]},Table[ReplacePart[cm[[1,2]],{1->("test flavor "<>ToString[i]),2->flavors[[i,1]],3->flavors[[i,2]],4->flavors[[i,3]],10->flavors[[i,1]],11->flavors[[i,2]]}],{i,3}]];
fParticles=Join[{fm[[1,1]]},Table[ReplacePart[fm[[1,2]],{1->("test flavor "<>ToString[i]),2->flavors[[i,1]],3->flavors[[i,2]],4->flavors[[i,3]],10->flavors[[i,1]],11->flavors[[i,2]]}],{i,3}]];
cRows=Flatten[Table[ReplacePart[#, {1->flavors[[i,1]],2->flavors[[i,2]]}]&/@cm[[2]],{i,3}],1];
fRows=Table[ReplacePart[fm[[2,1]],{1->flavors[[i,2]],2->flavors[[i,1]]}],{i,3}];
cm=ReplacePart[cm,{1->cParticles,2->cRows}];fm=ReplacePart[fm,{1->fParticles,2->fRows}];
rows4=Table[
 process=Join[flavors[[pair[[1]],{1,2}]],flavors[[pair[[2]],{2,1}]]];
 diagrams=DiagramAmplitudes[CreateDiagrams[process,cm]];
 amp=Total[(ReduceSpinContractions[Last[#]]/.First[#])&/@diagrams];
 fsCheck["mixed-flavor lower amplitude comes from one cubic photon diagram "<>ToString[process,InputForm],Length[diagrams]===1 && FreeQ[amp,_xFactor|_xTildeFactor]];
 Join[process,{1,amp}],{pair,Subsets[Range[3],{2}]}];
known=ReplacePart[cm,2->Join[cm[[2]],rows4]];
process6={"e","E","m","M","l","L"};
channels=FactorizationAmplitudeChannels[process6,known];
targets=ConstructiveDiagrams`Private`residueSimpleInputs[0,channels,known]["Targets"];
ferm=SelectFirst[targets,# ["Cut"]==={1,2,3}&];
fsCheck["six distinct fermions give six genuine fermion factorization channels",Length[targets]===6 && ferm["Cut"]==={1,2,3}];
raw=DiagramAmplitudes[CreateDiagrams[process6,fm]];
fsCheck["independent mixed-flavor Feynman construction contains six cubic diagrams",Length[raw]===6];
converted=FeynmanToSpinorProducts[raw,"TimeConstraint"->120];
If[FailureQ[converted],Print[converted];Exit[2]];
amp6=Total[(Last[#]/.First[#])&/@converted]/.{fMass->cMass,fCharge->cCharge};
fsCheck["Feynman conversion retains ordinary spinor products and propagators",FreeQ[amp6,_Failure|_DiracWord|_DiracGamma] && !FreeQ[amp6,_SpinorChain] && !FreeQ[amp6,_PropDen]];
rules={cMass->1,cCharge->1};
q={{1,0},{0,u}};p1={{1/2,2},{u/8-1/2,u/2}};p2=q-p1;
p3={{2,1},{2 v-6 u-1,v-3 u}};p4={{2,3},{4,13/2}};
SeedRandom[20261101];p56=ConstructiveDiagrams`Private`residueSplitTwoBody[-q-p3-p4,1,1,False];
mom=AssociationThread[Range[6],Join[{p1,p2,p3,p4},p56]];
j={{0,1},{-1,0}};avec=AssociationThread[Range[6],Table[{1,i+1},{i,6}]];
svec=AssociationThread[Range[6],Table[j.ConstructiveDiagrams`Private`residueAdjugate[mom[i]].avec[i],{i,6}]];
base=<|"Momenta"->mom,"Angle"->avec,"Square"->svec,"AngleBasis"->AssociationThread[Range[6],ConstantArray[IdentityMatrix[2],6]],"SquareBasis"->AssociationThread[Range[6],Table[j.ConstructiveDiagrams`Private`residueAdjugate[mom[i]],{i,6}]],"Masses"->AssociationThread[Range[6],ConstantArray[1,6]],"ReferenceSeed"->1729,"Cut"->{1,2},"Complement"->{3,4,5,6},"CutMass"->0,"AllowCutSpinors"->True,"PoleVariable"->u|>;
photProp=PropDen[Mom[Multiparticle[1,2]],0];fermProp=PropDen[Mom[Multiparticle[1,2,3]],cMass];
fullDouble=TimeConstrained[Cancel[Together[ConstructiveDiagrams`Private`residueEvaluateChart[(photProp fermProp amp6)/.rules,base,rules]]]/.{u->0,v->0},90,$TimedOut];
chartF=Join[base/.v->0,<|"PoleVariable"->u|>];
targetDouble=TimeConstrained[Cancel[Together[ConstructiveDiagrams`Private`residueEvaluateChart[(photProp ferm["StateSum"])/.rules,chartF,rules]]]/.u->0,90,$TimedOut];
result=<|"Process"->process6,"FeynmanDoubleResidue"->fullDouble,"FactorizationDoubleResidue"->targetDouble,"Equal"->TrueQ[Together[fullDouble-targetDouble]===0],"Opposite"->TrueQ[Together[fullDouble+targetDouble]===0],"Nonzero"->(!TrueQ[fullDouble===0]&&!TrueQ[targetDouble===0])|>;
fsCheck["mixed-flavor exact chart conserves momentum and keeps every external mass on shell",
 AllTrue[Values[mom],Together[Det[#]-1]===0&] &&
 AllTrue[Flatten[Total[Values[mom]]],Together[#]===0&] &&
 Together[Det[mom[1]+mom[2]]-u]===0 &&
 Together[Det[mom[1]+mom[2]+mom[3]]-1-v]===0];
fsCheck["the independent Feynman and factorization double residues are nonzero",
 result["Nonzero"] && FreeQ[{fullDouble,targetDouble},$Failed|$TimedOut|Indeterminate|DirectedInfinity]];
fsCheck["mixed-flavor fermion sewing agrees exactly with the off-shell Feynman double residue",
 result["Equal"]];

(* A common grouped-state reference is required for both kinds of sewing. *)
fsCheck["boson sewing includes the lower-to-full reference normalization",
 ConstructiveDiagrams`Private`factorizationFermionReferenceSign[{Range[2],Range[2]},False]===-1 &&
 ConstructiveDiagrams`Private`factorizationFermionReferenceSign[{Range[2],Range[4]},False]===1 &&
 ConstructiveDiagrams`Private`factorizationFermionReferenceSign[{{},Range[4]},False]===1];
fsReferenceGrade[a_,b_,sewnFermion_]:=ConstructiveDiagrams`Private`factorizationFermionReferenceSign[
 {Range[2a],Range[2b]},sewnFermion];
fsCheck["mixed boson and fermion reference transports compose consistently",
 And@@Flatten[Table[
 fsReferenceGrade[a,b,firstFermion] fsReferenceGrade[a+b-Boole[firstFermion],c,secondFermion]===
 fsReferenceGrade[b,c,secondFermion] fsReferenceGrade[a,b+c-Boole[secondFermion],firstFermion],
 {a,1,3},{b,1,3},{c,1,3},{firstFermion,{False,True}},{secondFermion,{False,True}}]]];

(* A paired Dirac reference must not invent a sign for fermion-number violation. *)
fsUnpairedModel=ReplacePart[cm,{
  1->{ReplacePart[cm[[1,1]],{1->"test scalar",2->"S",3->"S",4->999,5->0,6->0}],cm[[1,2]]},
  2->{{"e","e","S",1,SpinorChain[Spinor["Spin","Angle",1],Spinor["Spin","Angle",2]]},
      {"E","E","S",1,SpinorChain[Spinor["Spin","Square",1],Spinor["Spin","Square",2]]}}}];
fsUnpairedResult=FactorizationAmplitudeChannels[{"e","e","E","E"},fsUnpairedModel];
fsCheck["unpaired Dirac lower amplitudes return a public failure before multiplying an invalid sign",
 MatchQ[fsUnpairedResult,Failure["UnsupportedFermionSewing",_Association]] &&
 FreeQ[fsUnpairedResult,$Failed]];

(* Verify the lower data independently before using it in a larger cut. *)
Do[
 fsProcess=Drop[fsRow,-2];
 fsLowerFeynman=FeynmanToSpinorProducts[DiagramAmplitudes[CreateDiagrams[fsProcess,fm]],
   "TimeConstraint"->30]/.{fMass->cMass,fCharge->cCharge};
 fsLowerReport=CheckAmplitudes[
   {Thread[Mass/@Range[4]->ConstantArray[cMass,4]],fsRow[[-2]]fsRow[[-1]]},
   FactorizationAmplitudeChannels[fsProcess,known],known,
   "FeynmanAmplitudes"->fsLowerFeynman,"FeynmanSamples"->1,"Samples"->1,
   "RandomSeed"->20261101,"ParameterRules"->rules,
   "SpuriousPoles"->{},"HighEnergyCheck"->False,"ReferenceCheck"->False,
   "Parallelization"->False,"TimeConstraint"->15,"Output"->"Details"];
 fsCheck["mixed-flavor lower row independently agrees with every Feynman spin coefficient "<>
   ToString[fsProcess,InputForm],
   AssociationQ[fsLowerReport] && Lookup[fsLowerReport["FeynmanCheck"],"Status",None]==="PASS"],
 {fsRow,rows4}];

(* The three photon cuts require complete four-fermion-plus-photon lower data.
   Build these from ordinary off-shell Feynman diagrams, not from sewing. *)
fsRows5=Flatten[Table[Module[{p,raw5,prepared5,a},
 p=Join[flavors[[pair[[1]],{1,2}]],flavors[[pair[[2]],{2,1}]],{photon}];
 raw5=DiagramAmplitudes[CreateDiagrams[p,fm]];
 prepared5=PrepareAmplitudes[raw5,fm,"PolePreference"->"AllPhysical",
   "InternalOnShell"->False,"MaxSteps"->0,"TimeConstraint"->30,
   "Parallelization"->False,"ReturnDetails"->True,"ProgressReporting"->False];
 If[!AssociationQ[prepared5] || !ListQ[Lookup[prepared5,"PreparedAmplitudes",None]],Return[$Failed]];
 a=Total[Last/@prepared5["PreparedAmplitudes"]]/.{fMass->cMass,fCharge->cCharge};
 If[Length[raw5]=!=4 || !FreeQ[a,Spinor["Xi",___]],Return[$Failed]];
 Join[p,{1,a}]],{pair,Subsets[Range[3],{2}]},{photon,{"A.+","A.-"}}],1];
fsCheck["all six mixed four-fermion photon rows retain all four off-shell diagrams",
 Length[fsRows5]===6 && AllTrue[fsRows5,ListQ] && FreeQ[fsRows5,$Failed|_Failure]];
If[Length[fsRows5]===6 && AllTrue[fsRows5,ListQ],
 fsKnown5=ReplacePart[known,2->Join[known[[2]],fsRows5]];
 fsChannels9=FactorizationAmplitudeChannels[process6,fsKnown5];
 fsTargets9=ConstructiveDiagrams`Private`residueSimpleInputs[amp6,fsChannels9,fsKnown5]["Targets"];
 fsCoherentRows=TimeConstrained[Table[Module[{ch,chart,comparison},
   ch=ConstructiveDiagrams`Private`residueNormalizeChannel[target];
   chart=ConstructiveDiagrams`Private`completionCutChart[ch,1,20261101,rules];
   If[FailureQ[chart],Return[$Failed]];
   comparison=ConstructiveDiagrams`Private`residueCheckChannel[
     amp6,ch,{},fsTargets9,fsTargets9,chart["Map"],"Symbolic",18,10^-12,{}];
   {ch["Cut"],Length[chart["SpinVariables"]],Lookup[comparison,"Status",None],
     TrueQ[Lookup[comparison,"Difference",Missing[]]===0]}],{target,fsTargets9}],120,$TimedOut];
 fsCheck["all nine mixed photon and fermion cuts agree for every one of 64 spin components",
   Length[fsTargets9]===9 && MatchQ[fsCoherentRows,{{_List,6,"PASS",True}..}] && Length[fsCoherentRows]===9];
 fsAntiProcess=ReplacePart[process6,{2->process6[[4]],4->process6[[2]]}];
 fsAntiFeynman=FeynmanToSpinorProducts[DiagramAmplitudes[CreateDiagrams[fsAntiProcess,fm]],
   "TimeConstraint"->60]/.{fMass->cMass,fCharge->cCharge};
 fsAntiAmp=Total[Last/@fsAntiFeynman];
 fsAntiChannels=FactorizationAmplitudeChannels[fsAntiProcess,fsKnown5];
 fsAntiReport=CheckAmplitudes[fsAntiAmp,fsAntiChannels,fsKnown5,
   "ParameterRules"->rules,"Samples"->1,"RandomSeed"->20261101,
   "SpuriousPoles"->{},"HighEnergyCheck"->False,"ReferenceCheck"->False,
   "Parallelization"->False,"TimeConstraint"->15,"Output"->"Details"];
 fsCheck["all nine cuts keep one reference convention after exchanging two distinct antifermions",
   AssociationQ[fsAntiReport] && Length[fsAntiReport["Channels"]]===9 &&
   AllTrue[fsAntiReport["Channels"],Lookup[#,"Status",None]==="PASS"&]];
];

Print["FERMION_SEWING_GRADING_CHECKS=",fsChecks,"; FAILURES=",Length[fsFailures]];
If[fsFailures=!={},Print[fsFailures];Print["MIXED_RESULT ",InputForm[result]]];
Print["HEPCAT_TEST_COMPLETED:fermion-sewing-grading-tests.wl"];
Exit[If[fsFailures==={},0,1]];
