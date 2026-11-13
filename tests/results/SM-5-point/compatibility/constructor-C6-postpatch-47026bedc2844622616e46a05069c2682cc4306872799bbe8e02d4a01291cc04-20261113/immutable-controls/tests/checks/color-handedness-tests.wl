(* Model-derived handedness, complete color unions and guarded fresh partners. *)
root=DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Get[FileNameJoin[{root,"source","HEPCAT.wl"}]];
$HistoryLength=0;checks=0;failures={};SetAttributes[check,HoldRest];
check[name_,value_]:=(checks++;If[!TrueQ[value],AppendTo[failures,name];Print["FAIL: ",name]]);
model=ReadModel[FileNameJoin[{root,"models"}],1];lower=ReadModel[FileNameJoin[{root,"models"}],2];before={model,lower};
target={"G.+","G.+","G.-","G.-","G.-"};
cert=ConstructiveDiagrams`Private`colorHandednessCertificate[target,model,lower];
check["used cubics and every complete lower-four-point transform are exactly certified",AssociationQ[cert]&&cert["Status"]==="MODEL_CERTIFIED"&&
 AllTrue[cert["UsedCubicCertificates"],#["ExactDifference"]===0&&#["Phase"]===1&]&&And@@Lookup[cert["CompleteLowerFourPointCertificates"],"Proved"]];
check["partner and full leg image come from declared external-state metadata",cert["Partner"]==={"G.+","G.+","G.+","G.-","G.-"}&&cert["PermutationImage"]==={3,4,5,1,2}];
check["the model phase preserves I and has no numerical phase fit",ConstructiveDiagrams`Private`colorHandednessFlip[I]===I&&cert["DerivedAmplitudePhase"]===1&&cert["NumericPhaseFit"]===False];
check["no model list or parameter default is mutated",{model,lower}===before];
check["an undeclared external state cannot obtain a canonical map",FailureQ[ConstructiveDiagrams`Private`colorHandednessCanonicalMap[ReplacePart[target,1->"missing"],model]]];
invalidMap=Internal`InheritedBlock[{ConstructiveDiagrams`Private`colorHandednessStateData},
 Clear[ConstructiveDiagrams`Private`colorHandednessStateData];
 ConstructiveDiagrams`Private`colorHandednessStateData[_]:=<|"FlipRules"->{"x"->"y","y"->"y"},"Order"-><|"x"->{1,0},"y"->{1,1}|>|>;
 ConstructiveDiagrams`Private`colorHandednessCanonicalMap[{"x","y"},model]];
check["an inconsistent opposite-state multiplicity cannot escape the permutation guard",FailureQ[invalidMap]];

renamedModel=model/.{"G.+"->"X.+","G.-"->"X.-"};renamedLower=lower/.{"G.+"->"X.+","G.-"->"X.-"};
renamed=ConstructiveDiagrams`Private`colorHandednessCertificate[target/.{"G.+"->"X.+","G.-"->"X.-"},renamedModel,renamedLower];
check["the same certificate works with renamed species",AssociationQ[renamed]&&renamed["Status"]==="MODEL_CERTIFIED"];
indices=Flatten[Position[model[[2]],row_List/;Length[row]===6&&DeleteCases[Drop[row,-2],""]==={"G.-","G.-","G.+"},{1}]];
badModel=ReplacePart[model,{2,First[indices],-2}->I model[[2,First[indices],-2]]];
check["a mismatched cubic phase is rejected rather than fitted",FailureQ[ConstructiveDiagrams`Private`colorHandednessCertificate[target,badModel,lower]]];
noLower=lower;noLower[[2]]=Select[lower[[2]],!AllTrue[DeleteCases[Drop[#,-2],""],MemberQ[{"G.+","G.-"},#]&]&];
check["missing complete lower amplitudes cannot certify handedness",FailureQ[ConstructiveDiagrams`Private`colorHandednessCertificate[target,model,noLower]]];
angle[i_,j_]:=SpinorChain[Spinor["Helicity","Angle",i],Spinor["Helicity","Angle",j]];
square[i_,j_]:=SpinorChain[Spinor["Helicity","Square",i],Spinor["Helicity","Square",j]];
external=Thread[(Mass/@Range[5])->ConstantArray[0,5]];
tensor=ColorDelta[ColorIndex[1],ColorIndex[2]]ColorStructure[ColorIndex[3],ColorIndex[4],ColorIndex[5]];
source=I tensor angle[1,2]/square[3,4]/PropDen[Mom[Multiparticle[1,3]],0];
proposal=ConstructiveDiagrams`Private`colorHandednessPropose[{external,source},cert["Partner"],cert];
expected=I ColorDelta[ColorIndex[3],ColorIndex[4]]ColorStructure[ColorIndex[5],ColorIndex[1],ColorIndex[2]]square[3,4]/angle[5,1]/PropDen[Mom[Multiparticle[3,5]],0];
check["spinors, momenta and external colors are all transported with the certified image",proposal["Expression"]===expected];
check["the source expression and imaginary unit are not conjugated",proposal["ConjugatedImaginaryUnit"]===False&&proposal["NoBenchmarkAmplitudeInput"]===True];
check["an unsupported momentum-insertion source returns no proposal",FailureQ[ConstructiveDiagrams`Private`colorHandednessPropose[
 {external,SpinorChain[Spinor["Helicity","Square",1],Mom[2],Spinor["Helicity","Angle",3]]},cert["Partner"],cert]]];
ci[i_]:=ColorIndex[i];internal[i_,d_]:=ConstructiveDiagrams`Private`colorInternalIndex[i,d];
q1=ColorGenerator[ci[2],internal[1,3],ci[3]]ColorGenerator[internal[1,3],ci[1],ci[4]];
q2=ColorGenerator[ci[2],internal[2,3],ci[4]]ColorGenerator[internal[2,3],ci[1],ci[3]];
q3=ColorDelta[ci[1],ci[2]]ColorDelta[ci[3],ci[4]];
synthetic=<|"Status"->"PROPOSED","PartnerReport"-><|"ColorBasis"->{q1,q2}|>,
 "Proposal"-><|"ExternalMassRules"->external,"Expression"->alpha q1+beta q2,"TypedCycles"->{}|>|>;
projected=ConstructiveDiagrams`Private`colorHandednessProjection[synthetic,{{external,q1}},{},{3,-3,8,8,1},{{external,q3}}];
check["a benchmark-only independent color direction remains in the common union",AssociationQ[projected]&&Length[projected["Amplitudes"]]===3&&Last[projected["Amplitudes"]]===0&&projected["Certificate"]["ExtraIndependentDirections"]===1];
check["all original diagram and full benchmark rows are retained",projected["Certificate"]["AllOriginalDiagramPairsIncluded"]===1&&
 projected["Certificate"]["AllIndependentFeynmanPairsIncluded"]===1&&AllTrue[projected["Data"]["ProjectedFeynmanAmplitudes"],Length[#]===1&]];
check["the complete union has an exact Gram reconstruction certificate",TrueQ[projected["Certificate"]["ExactCommonUnionGramReconstruction"]]];
extraReport=Block[{ConstructiveDiagrams`Private`$colorCheckingProjection=True},CheckAmplitudes[
 {external,Last[projected["Amplitudes"]]},{},model,"FeynmanAmplitudes"->Last[projected["Data"]["ProjectedFeynmanAmplitudes"]],
 "Samples"->1,"TimeConstraint"->10,"HighEnergyCheck"->True,"ReferenceCheck"->True,"Output"->"Details"]];
check["a nonzero reference in that extra direction fails its ordinary check",extraReport["FeynmanCheck"]["Status"]==="FAIL"&&
 !TrueQ[ConstructiveDiagrams`Private`constructAllChecksPassQ[extraReport,{external,0},{},model,Last[projected["Data"]["ProjectedFeynmanAmplitudes"]]]]];

settings=Join[Association[Options[ConstructAmplitude]],<|"FeynmanModel"->"synthetic-independent-model","RandomSeed"->1234,"ProgressReporting"->False|>];
nonzeroChannels={{0,0,0,0,0,Multiparticle[1,2],0,1}};
prepareFixture[certificateFails_:False,partnerFails_:False,active_:False]:=Internal`InheritedBlock[
 {ConstructiveDiagrams`Private`colorHandednessCertificate,ConstructiveDiagrams`ConstructAmplitude},
 Unprotect[ConstructiveDiagrams`ConstructAmplitude];Clear[ConstructiveDiagrams`Private`colorHandednessCertificate,ConstructiveDiagrams`ConstructAmplitude];calls={};
 ConstructiveDiagrams`Private`colorHandednessCertificate[___]:=If[certificateFails,Failure["SyntheticCertificate",<||>],cert];
 ConstructiveDiagrams`ConstructAmplitude[p_,d_,f_,opts___]:=(AppendTo[calls,<|"Process"->p,"Options"->Association[{opts}],
 "RecursionGuard"->TrueQ[ConstructiveDiagrams`Private`colorHandednessActive],"ProjectionGuard"->TrueQ[ConstructiveDiagrams`Private`$colorConstructingProjection]|>];
 If[partnerFails,Failure["SyntheticPartner",<||>],<|"Status"->"COMPLETE","ColorBasis"->{tensor,2tensor},"AmplitudePair"->{external,source}|>]);
 outcome=Block[{ConstructiveDiagrams`Private`colorHandednessActive=active},ConstructiveDiagrams`Private`colorHandednessPrepare[target,model,lower,settings,nonzeroChannels,AbsoluteTime[]+400]];
 <|"Outcome"->outcome,"Calls"->calls|>];
fresh=prepareFixture[];
check["a certified canonical partner is constructed fresh through the public API",fresh["Outcome"]["Status"]==="PROPOSED"&&Length[fresh["Calls"]]===1&&First[fresh["Calls"]]["Process"]===cert["Partner"]&&fresh["Outcome"]["SavedAmplitudeInput"]===False];
check["the partner shares the clock and cannot recurse",First[fresh["Calls"]]["RecursionGuard"]===True&&First[fresh["Calls"]]["ProjectionGuard"]===False&&
 0<First[fresh["Calls"]]["Options"]["TimeConstraint"]<=200];
badCert=prepareFixture[True];badPartner=prepareFixture[False,True];recursive=prepareFixture[False,False,True];
check["certificate failure preserves the ordinary route without a partner call",badCert["Calls"]==={}&&!TrueQ[badCert["Outcome"]["Selected"]]];
check["a failed source partner is retained and is never selected",Length[badPartner["Calls"]]===1&&FailureQ[badPartner["Outcome"]["PartnerReport"]]&&!TrueQ[badPartner["Outcome"]["Selected"]]];
check["the recursion guard prevents nested handedness attempts",recursive["Calls"]==={}&&recursive["Outcome"]["Attempted"]===False];
check["an already canonical orientation remains on its ordinary path",ConstructiveDiagrams`Private`colorHandednessPrepare[cert["Partner"],model,lower,settings,nonzeroChannels,AbsoluteTime[]+400]["Attempted"]===False];
check["a zero-channel sector retains its existing direct construction",ConstructiveDiagrams`Private`colorHandednessPrepare[target,model,lower,settings,{},AbsoluteTime[]+400]["Attempted"]===False];
check["an expired small explicit budget cannot start a new partner",ConstructiveDiagrams`Private`colorHandednessPrepare[target,model,lower,settings,nonzeroChannels,AbsoluteTime[]+1]["Attempted"]===False];
check["an explicit reference-spinor choice is preserved literally",ConstructiveDiagrams`Private`colorHandednessPrepare[target,model,lower,Join[settings,<|"PolePreference"->"Xi"|>],nonzeroChannels,AbsoluteTime[]+400]["Attempted"]===False];
check["all new helpers are private",Names["ConstructiveDiagrams`colorHandedness*"]==={}];
Print["Checks: ",checks,"; failures: ",Length[failures]];If[failures=!={},Print[failures]];
Quit[If[failures==={},0,1]];
