(* HEPCAT - High Energy Physics Constructive Amplitude Tool
   Copyright (C) 2026 Neil Christensen
   SPDX-License-Identifier: GPL-3.0-or-later *)

Begin["AnalyticBosonClosureTests`Private`"];
acRoot=DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Get[FileNameJoin[{acRoot,"source","HEPCAT.wl"}]];
$HistoryLength=0;acChecks=0;acFailures={};
SetAttributes[acCheck,HoldRest];
acCheck[name_,value_]:=(acChecks++;If[!TrueQ[value],AppendTo[acFailures,name];Print["FAIL: ",name]]);
acModel=ReadModel[FileNameJoin[{acRoot,"models"}],4];
acSM=ReadModel[FileNameJoin[{acRoot,"models"}],1];
acModelBefore=acModel;acSMBefore=acSM;acGlobalBefore={OwnValues[Global`Me],OwnValues[Global`EE]};
acOptions=Join[KeyTake[Association[Options[WriteSPINASProcess]],{"IncomingLegs","ParameterRules","FixedHelicities"}],<|"ParameterRules"->{"Me"->1,"EE"->1/3}|>];
acProcesses=Join[{"e",#[[1]],"E"},{#[[2]]}]&/@Tuples[{"A.+","A.-"},2];
acEntries={#,LookupAmplitude[Thread[{#,Range[4]}],acModel,False,{acI,acJ,acK,acL}]}&/@acProcesses;
(* ++,+-,-- are supplied; the other ordering must come from Bose symmetry,
   not parity. Its independently stored model expression is the oracle. *)
acSupplied=acEntries[[{1,2,4}]];
acClosure=ConstructiveDiagrams`Private`spinasAnalyticBosonClosure[acSupplied,acModel,acOptions];
acCheck["one missing Compton role orbit adds one analytic expression",AssociationQ[acClosure]&&Length[acClosure["Entries"]]===4&&Length[acClosure["Added"]]===1];
If[!AssociationQ[acClosure],Print[acClosure];Quit[1]];
acAdded=Last[acClosure["Entries"]];acImage=First[acClosure["Added"]]["LegPermutation"];
acCheck["Bose transport moves only the two photons",acImage==={1,4,3,2}&&acAdded[[1]]===acProcesses[[3]]];
acCheck["the independent mixed-helicity model formula has a momentum insertion",!FreeQ[Last[acEntries[[3]]],_Mom]];
acGeneratedIR=ConstructiveDiagrams`Private`spinasProcessIR[First[acAdded],Last[acAdded],acModel,acOptions];
acExplicitIR=ConstructiveDiagrams`Private`spinasProcessIR[acEntries[[3,1]],acEntries[[3,2]],acModel,acOptions];
acCheck["generated and explicit target use the same masses and outgoing legs",AssociationQ[acGeneratedIR]&&AssociationQ[acExplicitIR]&&acGeneratedIR["Masses"]===acExplicitIR["Masses"]&&acGeneratedIR["IncomingLegs"]==={1,2}];
acFamilyIR=ConstructiveDiagrams`Private`spinasFamilyIR[acClosure["Entries"],acModel,acOptions];
acCheck["physical runtime permutations still preserve roles",AssociationQ[acFamilyIR]&&AllTrue[acFamilyIR["Branches"],Sort[#["LegPermutation"][[{1,2}]]]==={1,2}&]];
acAllOut=({{"e","E",#[[1,2]],#[[1,4]]},#[[2]]}&/@acSupplied);
(* Use independently read formulas for the all-outgoing layout. *)
acAllOut={#,LookupAmplitude[Thread[{#,Range[4]}],acModel,False,{acI,acJ,acK,acL}]}&/@({"e","E",#[[1]],#[[2]]}&/@{{"A.+","A.+"},{"A.+","A.-"},{"A.-","A.-"}});
acAllOutClosure=ConstructiveDiagrams`Private`spinasAnalyticBosonClosure[acAllOut,acModel,acOptions];
acAllOutIR=ConstructiveDiagrams`Private`spinasFamilyIR[acAllOutClosure["Entries"],acModel,acOptions];
acCheck["ordinary all-outgoing input and compiled branch counts stay unchanged",acAllOutClosure["Entries"]===acAllOut&&acAllOutClosure["Added"]==={}&&acAllOutIR["InputBranchCount"]===3&&acAllOutIR["GeneratedBranchCount"]===1];
acFixed=ConstructiveDiagrams`Private`spinasAnalyticBosonClosure[acSupplied,acModel,acOptions,{"e","A.-"}->{"e","A.+"}];
acFixedIR=ConstructiveDiagrams`Private`spinasFamilyIR[acFixed["Entries"],acModel,acOptions,{"e","A.-"}->{"e","A.+"}];
acCheck["fixed incoming and outgoing helicities filter target closure",Length[acFixed["Added"]]===1&&acFixedIR["HelicityConfigurations"]==={{0,-2,0,2}}&&acFixedIR["InitialSpinAverage"]===2&&acFixedIR["SymmetryFactor"]===1];
acAlreadyFixed=ConstructiveDiagrams`Private`spinasAnalyticBosonClosure[acSupplied,acModel,acOptions,{"e","A.+"}->{"e","A.-"}];
acCheck["an already supplied fixed sector adds nothing",acAlreadyFixed["Entries"]===acSupplied&&acAlreadyFixed["Added"]==={}];
acBadMass=ReplacePart[acSupplied,{1,2}->{{Mass[1]->2 ModelParameter[acModel,"Me"]},acSupplied[[1,2]]}];
acCheck["an invalid unselected original mass rule is not hidden",FailureQ[Quiet[ConstructiveDiagrams`Private`spinasAnalyticBosonClosure[acBadMass,acModel,acOptions,{"e","A.-"}->{"e","A.+"}]]]];
acCheck["duplicate original helicities are rejected before augmentation",FailureQ[ConstructiveDiagrams`Private`spinasAnalyticBosonClosure[Append[acSupplied,First[acSupplied]],acModel,acOptions]]];
acBadReport=ReplacePart[acSupplied,{1,2}-><|"Status"->"UNRESOLVED","Amplitude"->acSupplied[[1,2]]|>];
acCheck["an unsuccessful unselected original report is rejected",FailureQ[ConstructiveDiagrams`Private`spinasAnalyticBosonClosure[acBadReport,acModel,acOptions,{"e","A.-"}->{"e","A.+"}]]];
acBadParameter=ReplacePart[acSupplied,{1,2}->(ModelParameter[acSM,"EE"] acSupplied[[1,2]])];
acCheck["a foreign model parameter cannot enter a generated branch",FailureQ[ConstructiveDiagrams`Private`spinasAnalyticBosonClosure[acBadParameter,acModel,acOptions]]];
acFermionOnly={{{"ne","ne","Ne","Ne"},0}};
acCheck["no cross-role fermion or parity closure is inferred",ConstructiveDiagrams`Private`spinasAnalyticBosonClosure[acFermionOnly,acSM,acOptions]["Entries"]===acFermionOnly];
acDummy=ConstructiveDiagrams`Private`colorInternalIndex[7,3];
acTyped={{Mass[2]->0,Mass[Multiparticle[1,2]]->7},2 ColorGenerator[ColorIndex[2],acDummy,ColorIndex[4]] SpinorChain[Spinor["Spin","Angle",2,VectorSpinIndex[2,1]],Mom[Multiparticle[1,4]],Spinor["Helicity","Square",4]]+MomProd[2,Multiparticle[1,4]]};
acExpected={{Mass[4]->0,Mass[Multiparticle[1,4]]->7},2 ColorGenerator[ColorIndex[4],acDummy,ColorIndex[2]] SpinorChain[Spinor["Spin","Angle",4,VectorSpinIndex[4,1]],Mom[Multiparticle[1,2]],Spinor["Helicity","Square",2]]+MomProd[4,Multiparticle[1,2]]};
acCheck["typed transport changes masses composites spin slots and colors once",ConstructiveDiagrams`Private`spinasAnalyticRelabel[acTyped,{1,4,3,2}]===acExpected];
acCycle={2,3,1,4};
acCheck["a non-involutive typed permutation has an exact inverse",ConstructiveDiagrams`Private`spinasAnalyticRelabel[ConstructiveDiagrams`Private`spinasAnalyticRelabel[acTyped,acCycle],Ordering[acCycle]]===acTyped];
acTensorA=ColorGenerator[ColorIndex[3],acDummy,ColorIndex[2]] ColorGenerator[acDummy,ColorIndex[1],ColorIndex[4]];
acTensorB=ColorGenerator[ColorIndex[3],acDummy,ColorIndex[4]] ColorGenerator[acDummy,ColorIndex[1],ColorIndex[2]];
acTensor=acTensorA+2 acTensorB;
acColorEntries=Map[{StringReplace[#[[1]],{"e"->"u","E"->"U","A"->"G"}],acTensor (#[[2]]/.{ModelParameter[acModel,"Me"]->ModelParameter[acSM,"Mu"],ModelParameter[acModel,"EE"]->ModelParameter[acSM,"GG"]})}&,acSupplied];
acColorOptions=Join[acOptions,<|"ParameterRules"->{"Mu"->1,"GG"->1/3}|>];
acColorClosure=ConstructiveDiagrams`Private`spinasAnalyticBosonClosure[acColorEntries,acSM,acColorOptions];
acCheck["colored Compton closure transports a complete tensor branch",AssociationQ[acColorClosure]&&Length[acColorClosure["Added"]]===1&&Last[acColorClosure["Entries"]][[1]]==={"u","G.-","U","G.+"}];
acColorIR=ConstructiveDiagrams`Private`spinasColorFamilyIR[acColorClosure["Entries"],acSM,acColorOptions,Automatic];
acCheck["cross-role color closure retains coherent interference",AssociationQ[acColorIR]&&Length[acColorIR["HelicityConfigurations"]]===4&&acColorIR["ColorData"]["GramMatrix"]==={{16/3,-2/3},{-2/3,16/3}}];
acCheck["two local models retain their own parameters and do not change globals",ModelParameter[acModel,"EE"]=!=ModelParameter[acSM,"EE"]&&acModel===acModelBefore&&acSM===acSMBefore&&{OwnValues[Global`Me],OwnValues[Global`EE]}===acGlobalBefore];
acDirectory=CreateDirectory[];acSpinas=FileNameJoin[{ParentDirectory[acRoot],"spinas"}];
acColorExplicitEntries=Append[acColorEntries,{{"u","G.-","U","G.+"},(acTensorB+2 acTensorA)(acEntries[[3,2]]/.{ModelParameter[acModel,"Me"]->ModelParameter[acSM,"Mu"],ModelParameter[acModel,"EE"]->ModelParameter[acSM,"GG"]})}];
acColorGenerated=WriteSPINASProcess[FileNameJoin[{acDirectory,"colorGenerated"}],acColorEntries,acSM,"ProcessName"->"acColorGenerated","SPINASDirectory"->acSpinas,"ParameterRules"->{"Mu"->1,"GG"->1/3},"ReturnDetails"->True];
acColorExplicit=WriteSPINASProcess[FileNameJoin[{acDirectory,"colorExplicit"}],acColorExplicitEntries,acSM,"ProcessName"->"acColorExplicit","SPINASDirectory"->acSpinas,"ParameterRules"->{"Mu"->1,"GG"->1/3},"ReturnDetails"->True];
acCheck["public colored export retains a separate analytic lineage ledger",AssociationQ[acColorGenerated]&&AssociationQ[acColorExplicit]&&acColorGenerated["InputBranchCount"]===3&&acColorGenerated["GeneratedBranchCount"]===1&&acColorGenerated["AnalyticHelicityClosure"]["Branches"][[1]]["PreCrossingInputIndex"]===4];
acGenerated=WriteSPINASProcess[FileNameJoin[{acDirectory,"generated"}],acSupplied,acModel,"ProcessName"->"acGenerated","SPINASDirectory"->acSpinas,"ParameterRules"->{"Me"->1,"EE"->1/3},"ReturnDetails"->True];
acExplicit=WriteSPINASProcess[FileNameJoin[{acDirectory,"explicit"}],acEntries,acModel,"ProcessName"->"acExplicit","SPINASDirectory"->acSpinas,"ParameterRules"->{"Me"->1,"EE"->1/3},"ReturnDetails"->True];
acCheck["public export records supplied and analytic branches separately",AssociationQ[acGenerated]&&acGenerated["InputBranchCount"]===3&&acGenerated["GeneratedBranchCount"]===1&&acGenerated["AnalyticHelicityClosure"]["AddedBranchCount"]===1];
If[AssociationQ[acGenerated]&&AssociationQ[acExplicit]&&AssociationQ[acColorGenerated]&&AssociationQ[acColorExplicit],
 acMain=StringRiffle[{"#include \"generated/process.hpp\"","#include \"explicit/process.hpp\"","#include \"colorGenerated/process.hpp\"","#include \"colorExplicit/process.hpp\"","#include <cmath>","#include <iostream>",
 "int main(){ using G=hepcat_generated::acGenerated; using E=hepcat_generated::acExplicit; G g; E e; hepcat_generated::acColorGenerated cg; hepcat_generated::acColorExplicit ce; unsigned count=0,colorCount=0;",
 "for(unsigned point=1;point<=3;++point){auto p=hepcat_spinas::make_example_momenta(g.masses(),G::incoming_legs(),10,point);g.set_momenta(p);e.set_momenta(p);cg.set_momenta(p);ce.set_momenta(p);",
 "for(int s1=-1;s1<=1;s1+=2)for(int s3=-1;s3<=1;s3+=2)for(int h2=-2;h2<=2;h2+=4)for(int h4=-2;h4<=2;h4+=4){G::Spins s{{s1,h2,s3,h4}};auto a=g.amp(s),b=e.amp(s);if(std::abs(a-b)>1e-10L*std::max(ldouble(1),std::abs(b)))return 1;++count;for(int c1=1;c1<=3;++c1)for(int c2=1;c2<=8;++c2)for(int c3=1;c3<=3;++c3)for(int c4=1;c4<=8;++c4){hepcat_generated::acColorGenerated::Colors c{{c1,c2,c3,c4}};auto ca=cg.amp(s,c),cb=ce.amp(s,c);if(std::abs(ca-cb)>1e-10L*std::max(ldouble(1),std::abs(cb)))return 2;++colorCount;}}}",
 "std::cout<<\"COMPLEX_COMPTON_COMPARISONS=\"<<count<<'\\n'<<\"COMPLEX_COLOR_COMPARISONS=\"<<colorCount<<'\\n';return 0;}"},"\n"];
 Export[FileNameJoin[{acDirectory,"oracle.cpp"}],acMain,"Text"];
 acLibrary=SelectFirst[FileNameJoin[{acSpinas,#,"libspinas.a"}]&/@{"build","lib"},FileExistsQ];
 acManifest=Import[acGenerated["Files"]["Manifest"],"RawJSON"];
 acCompile=RunProcess[Join[{"c++","-std=c++11","-O1"},If[acManifest["Precision"]==="LongDouble",{"-DWITH_LONG_DOUBLE"},{}],{"-I"<>FileNameJoin[{acSpinas,"include"}],FileNameJoin[{acDirectory,"oracle.cpp"}],acGenerated["Files"]["Source"],acExplicit["Files"]["Source"],acColorGenerated["Files"]["Source"],acColorExplicit["Files"]["Source"],acLibrary,"-o",FileNameJoin[{acDirectory,"oracle"}]}]];
 acCheck["the generated and independently supplied Compton oracle compiles",acCompile["ExitCode"]===0];
 If[acCompile["ExitCode"]===0,acRun=RunProcess[{FileNameJoin[{acDirectory,"oracle"}]}];acCheck["48 complex amplitudes agree including outgoing angle crossing",acRun["ExitCode"]===0&&StringContainsQ[acRun["StandardOutput"],"COMPLEX_COMPTON_COMPARISONS=48"]];acCheck["27648 explicit-color complex amplitudes preserve transported interference",acRun["ExitCode"]===0&&StringContainsQ[acRun["StandardOutput"],"COMPLEX_COLOR_COMPARISONS=27648"]],Print[acCompile]]];
Print["ANALYTIC BOSON CLOSURE CHECKS: ",acChecks,"; FAILURES: ",Length[acFailures]];
If[acFailures==={},DeleteDirectory[acDirectory,DeleteContents->True];Print["HEPCAT_TEST_COMPLETED:spinas-analytic-closure-tests.wl"],Print[acFailures];Print["TEST FILES: ",acDirectory]];
End[];Quit[If[AnalyticBosonClosureTests`Private`acFailures==={},0,1]];
