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
 diagrams=diagramAmplitudes[CreateDiagrams[process,cm]];
 amp=Total[(ReduceSpinContractions[Last[#]]/.First[#])&/@diagrams];
 fsCheck["mixed-flavor lower amplitude comes from one cubic photon diagram "<>ToString[process,InputForm],Length[diagrams]===1 && FreeQ[amp,_xFactor|_xTildeFactor]];
 Join[process,{1,amp}],{pair,Subsets[Range[3],{2}]}];
known=ReplacePart[cm,2->Join[cm[[2]],rows4]];
process6={"e","E","m","M","l","L"};
channels=FactorizationAmplitudeChannels[process6,known];
targets=ConstructiveDiagrams`Private`residueSimpleInputs[0,channels,known]["Targets"];
ferm=SelectFirst[targets,# ["Cut"]==={1,2,3}&];
fsCheck["six distinct fermions give six genuine fermion factorization channels",Length[targets]===6 && ferm["Cut"]==={1,2,3}];
raw=diagramAmplitudes[CreateDiagrams[process6,fm]];
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
Print["FERMION_SEWING_GRADING_CHECKS=",fsChecks,"; FAILURES=",Length[fsFailures]];
If[fsFailures=!={},Print[fsFailures];Print["MIXED_RESULT ",InputForm[result]]];
Print["HEPCAT_TEST_COMPLETED:fermion-sewing-grading-tests.wl"];
Exit[If[fsFailures==={},0,1]];
