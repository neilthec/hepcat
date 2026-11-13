(* Portable source, projection and fallback controls. No saved amplitude is loaded. *)
ccTDefaultRoot=DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
ccTRoot=Replace[Environment["HEPCAT_TEST_REPOSITORY"],{value_String/;StringLength[value]>0:>value,_:>ccTDefaultRoot}];
ccTEntry=Replace[Environment["HEPCAT_TEST_PACKAGE_ENTRYPOINT"],{value_String/;StringLength[value]>0:>value,_:>FileNameJoin[{ccTRoot,"source","HEPCAT.wl"}]}];
Get[ccTEntry];
Begin["HEPCATColorCurrentPortableTests`Private`"];
$ContextPath=Prepend[DeleteCases[$ContextPath,"HEPCATColorCurrentPortableTests`Private`"],"HEPCATColorCurrentPortableTests`Private`"];
ccTReportPath=Environment["HEPCAT_TEST_REPORT"];ccTRepo=Replace[Environment["HEPCAT_TEST_REPOSITORY"],{value_String/;StringLength[value]>0:>value,_:>DirectoryName[DirectoryName[DirectoryName[$InputFileName]]]}];$HistoryLength=0;ccTRecords={};ccTPassed=0;ccTFailed=0;
ccTAssert[name_String,value_]:=Module[{ok=TrueQ[value]},AppendTo[ccTRecords,<|"Test"->name,"Pass"->ok|>];If[ok,ccTPassed++;Print["PASS ",name],ccTFailed++;Print["FAIL ",name]]];
ccTAssert["five private source heads loaded",AllTrue[{ConstructiveDiagrams`Private`colorCurrentSourceClass,ConstructiveDiagrams`Private`colorCurrentSource,
 ConstructiveDiagrams`Private`colorCurrentSourceCertificate,ConstructiveDiagrams`Private`colorCurrentPrepare,ConstructiveDiagrams`Private`colorCurrentProjection},DownValues[#]=!={}&]];
If[ccTFailed>0,Quit[2]];
ccTActual=ReadModel[FileNameJoin[{ccTRepo,"models"}],1];
ccTAssert["massive fourquark scalar class admitted",ConstructiveDiagrams`Private`colorCurrentSourceClass[{"u","U","c","C","h"},ccTActual]];
ccTAssert["massive mixed color scalar class admitted",ConstructiveDiagrams`Private`colorCurrentSourceClass[{"e","E","u","U","h"},ccTActual]];
ccTAssert["nonconjugate weak fourquark class admitted",ConstructiveDiagrams`Private`colorCurrentSourceClass[{"u","D","C","s","h"},ccTActual]];
ccTAssert["massless chiral pairs excluded",!ConstructiveDiagrams`Private`colorCurrentSourceClass[{"ne","Ne","u","U","h"},ccTActual]];
ccTAssert["one massless chiral endpoint excluded",!ConstructiveDiagrams`Private`colorCurrentSourceClass[{"e","Ne","u","D","h"},ccTActual]];
ccTAssert["old colorless massive scalar class excluded",!ConstructiveDiagrams`Private`colorCurrentSourceClass[{"e","E","m","M","h"},ccTActual]];
ccTAssert["unbalanced colored pair excluded",!ConstructiveDiagrams`Private`colorCurrentSourceClass[{"u","u","e","E","h"},ccTActual]];
ccTAssert["QED photon classes excluded",!ConstructiveDiagrams`Private`colorCurrentSourceClass[{"e","E","e","E","A.+"},ccTActual]&&!ConstructiveDiagrams`Private`colorCurrentSourceClass[{"e","E","A.+","A.+","A.-"},ccTActual]];
ccTAssert["fourpoint SM and fourfermionvector classes excluded",!ConstructiveDiagrams`Private`colorCurrentSourceClass[{"u","U","c","C"},ccTActual]&&!ConstructiveDiagrams`Private`colorCurrentSourceClass[{"e","E","m","M","Z"},ccTActual]];
ccTAssert["twofermion scalar class excluded",!ConstructiveDiagrams`Private`colorCurrentSourceClass[{"u","U","Z","h","h"},ccTActual]];
ccTInventory=Import[FileNameJoin[{ccTRepo,"tests","SM-5-point","inventory.json"}],"RawJSON"];ccTMetadata=ccTInventory["Particles"];
ccTColoredH=Select[ccTInventory["Families"],#["Sector"]==="FourFermionHiggs"&&AnyTrue[#["Particles"],ccTMetadata[#]["ColorDimension"]=!=1&]&];
ccTMassiveH=Select[ccTColoredH,AllTrue[Select[#["Particles"],OddQ[ccTMetadata[#]["Spin2"]]&],ccTMetadata[#]["Mass"]=!="0"&]&];
ccTChiralH=Complement[ccTColoredH,ccTMassiveH];
ccTAssert["all45new massive coloredHfamilies eligible",Length[ccTMassiveH]===45&&AllTrue[ccTMassiveH,ConstructiveDiagrams`Private`colorCurrentSourceClass[#["Particles"],ccTActual]&]];
ccTAssert["all36historical chiral coloredHfamilies excluded",Length[ccTChiralH]===36&&AllTrue[ccTChiralH,!ConstructiveDiagrams`Private`colorCurrentSourceClass[#["Particles"],ccTActual]&]];
Do[ccTPacket=ConstructiveDiagrams`Private`colorCurrentSource[ccTPilot,ccTActual];
 ccTCert=If[AssociationQ[ccTPacket],ConstructiveDiagrams`Private`colorCurrentSourceCertificate[ccTPacket,ccTActual],ccTPacket];
 ccTAssert["fresh exact color and graph replay "<>StringRiffle[ccTPilot,","],AssociationQ[ccTCert]&&ccTCert["Status"]==="CERTIFICATE_VERIFIED"&&ccTPacket["Status"]==="UNVALIDATED_CANDIDATE"&&ccTPacket["IndependentBenchmarkRead"]===False],
 {ccTPilot,{{"u","U","c","C","h"},{"e","E","u","U","h"},{"u","D","C","s","h"}}}];
ccTModel={{{"q","q","Q",1,1,1,0,3,"","q","Q"},{"r","r","R",2,1,2,0,3,"","r","R"},{"h","h","h",3,0,3,0,1,"","h","h"}}, {},{}, {}};
ccTProcess={"q","Q","r","R","h"};ccTExt=Thread[(Mass/@Range[5])->{1,1,2,2,3}];
ccTT1=ColorDelta[ColorIndex[1],ColorIndex[2]]ColorDelta[ColorIndex[3],ColorIndex[4]];
ccTT2=ColorDelta[ColorIndex[1],ColorIndex[4]]ColorDelta[ColorIndex[2],ColorIndex[3]];
ccTT3=ColorDelta[ColorIndex[1],ColorIndex[3]]ColorDelta[ColorIndex[2],ColorIndex[4]];
ccTSettings=Join[Association[Options[ConstructAmplitude]],<|"FeynmanModel"->"SyntheticIndependentModel","ProgressReporting"->False,"PrintReport"->False,"RandomSeed"->12345|>];
ccTPrepMock[bag_Association:<||>,sourceStatus_:"UNVALIDATED_CANDIDATE",certOK_:True,leftXi_:False,internalRule_:False,clock_:Automatic]:=Module[
 {seen=<||>,pair={ccTExt,ccTT1+2ccTT2},proposal,outcome,opts=Join[ccTSettings,bag],sourceBefore=DownValues[ConstructiveDiagrams`Private`colorCurrentSource],prepareBefore=DownValues[PrepareAmplitudes]},
 If[leftXi,pair[[2]]*=SpinorChain[Spinor["Xi","Angle",1],Spinor["Helicity","Angle",2]]];If[internalRule,pair[[1]]=Append[pair[[1]],Mass[Multiparticle[1,2]]->0]];
 proposal=<|"Status"->sourceStatus,"Amplitude"->pair,"ExternalMassRules"->First[pair],"EveryOriginalOrderedGraphAndFermionSignMatched"->True,
 "EveryOriginalDiagramCovered"->True,"EveryUsedOrderedVertexCertified"->True,"CompleteOffShellConversionCoverage"->True,"ModelParametersUnchanged"->True,"RequiresCheckAmplitudes"->True,
 "NumericalCoefficientFit"->False,"IndependentBenchmarkRead"->False,"NativeValidationPerformed"->False,"NewIndependentContacts"->False,"NumericalParameterAssignments"->{},"InternalOnShellConditions"->{},
 "SourceModelSHA256"->HEPCATCovariantCurrentSources`Private`objectSHA[ccTModel],"OriginalDiagramCount"->1|>;
 outcome=Internal`InheritedBlock[{ConstructiveDiagrams`Private`colorCurrentSource,ConstructiveDiagrams`Private`colorCurrentSourceCertificate,PrepareAmplitudes},
 Unprotect[PrepareAmplitudes];Clear[ConstructiveDiagrams`Private`colorCurrentSource,ConstructiveDiagrams`Private`colorCurrentSourceCertificate,PrepareAmplitudes];
 ConstructiveDiagrams`Private`colorCurrentSource[p_,m_]:=(seen["SourceArguments"]={p,m};proposal);
 ConstructiveDiagrams`Private`colorCurrentSourceCertificate[___]:=(seen["CertificateCalled"]=True;If[certOK,<|"Status"->"CERTIFICATE_VERIFIED"|>,Failure["SyntheticCertificateFailure",<||>]]);
 PrepareAmplitudes[input_,m_,options___]:=(seen["Input"]=input;seen["Options"]=Association[{options}];<|"Status"->"PREPARED","PreparedAmplitudes"->{input},"Options"->seen["Options"],"ItemReports"->{<|"Status"->"PREPARED","PreparedExpression"->Last[input],"DenominatorsKeptOffShell"->True,"PolePreference"-><|"Status"->"COMPLETE"|>|>}|>);
 ConstructiveDiagrams`Private`colorCurrentPrepare[ccTProcess,ccTModel,ccTModel,opts,Replace[clock,Automatic:>AbsoluteTime[]+120],{"SyntheticDiagram"}]];
 <|"Outcome"->outcome,"Seen"->seen,"DefinitionsRestored"->(sourceBefore===DownValues[ConstructiveDiagrams`Private`colorCurrentSource]&&prepareBefore===DownValues[PrepareAmplitudes])|>];
ccTMock=ccTPrepMock[];
ccTAssert["source only receives declared process and diagram model",ccTMock["Seen"]["SourceArguments"]==={ccTProcess,ccTModel}];
ccTAssert["completed pair prepared without diagram wrapper",MatchQ[ccTMock["Seen"]["Input"],{_List,_}]&&First[ccTMock["Seen"]["Input"]]===ccTExt];
ccTAssert["source preparation always uses external identities",ccTMock["Seen"]["Options"]["InternalOnShell"]===False];
ccTAssert["allphysical default forwarded",ccTMock["Seen"]["Options"]["PolePreference"]==="AllPhysical"];
ccTAssert["proposal remains unselected until independent validation",ccTMock["Outcome"]["Status"]==="PROPOSED"&&ccTMock["Outcome"]["Selected"]===False];
ccTAssert["preparation mocks restore exact definitions",ccTMock["DefinitionsRestored"]];
ccTMock=ccTPrepMock[<|"FinalPreparationOptions"->{"InternalOnShell"->True,"TimeConstraint"->7}|>];
ccTAssert["caller child clock preserved while internal shells remain disabled",ccTMock["Seen"]["Options"]["TimeConstraint"]<=7&&ccTMock["Seen"]["Options"]["InternalOnShell"]===False];
ccTMock=ccTPrepMock[<|"PolePreference"->"AllSpurious"|>];ccTAssert["allspurious preference forwarded literally",ccTMock["Seen"]["Options"]["PolePreference"]==="AllSpurious"];
ccTMock=ccTPrepMock[<|"PolePreference"->"Xi"|>,"UNVALIDATED_CANDIDATE",True,True];ccTAssert["explicit Xi choice retained",ccTMock["Outcome"]["Status"]==="PROPOSED"&&ccTMock["Seen"]["Options"]["PolePreference"]==="Xi"];
ccTMock=ccTPrepMock[<|"PolePreference"->None|>,"UNVALIDATED_CANDIDATE",True,True];ccTAssert["explicit None choice retained",ccTMock["Outcome"]["Status"]==="PROPOSED"&&ccTMock["Seen"]["Options"]["PolePreference"]===None];
ccTMock=ccTPrepMock[<||>,"UNVALIDATED_CANDIDATE",True,True];ccTAssert["default cannot retain unresolved Xi",ccTMock["Outcome"]["Status"]==="NOT_PROPOSED"];
ccTMock=ccTPrepMock[<||>,"COMPLETE"];ccTAssert["source cannot fabricate COMPLETE acceptance",ccTMock["Outcome"]["Status"]==="NOT_PROPOSED"&&!KeyExistsQ[ccTMock["Seen"],"Input"]];
ccTMock=ccTPrepMock[<||>,"UNVALIDATED_CANDIDATE",False];ccTAssert["certificate failure prevents preparation and retains diagnostics",ccTMock["Outcome"]["Status"]==="NOT_PROPOSED"&&FailureQ[ccTMock["Outcome"]["Certificate"]]&&!KeyExistsQ[ccTMock["Seen"],"Input"]];
ccTMock=ccTPrepMock[<||>,"UNVALIDATED_CANDIDATE",True,False,True];ccTAssert["internal shell mass rule cannot become completed source",ccTMock["Outcome"]["Status"]==="NOT_PROPOSED"];
ccTAssert["independent benchmark is mandatory",ConstructiveDiagrams`Private`colorCurrentPrepare[ccTProcess,ccTModel,ccTModel,Join[ccTSettings,<|"FeynmanModel"->None|>],AbsoluteTime[]+60,{}]["Attempted"]===False];
ccTAssert["expired source budget skips safely",ConstructiveDiagrams`Private`colorCurrentPrepare[ccTProcess,ccTModel,ccTModel,ccTSettings,AbsoluteTime[]-1,{}]["Attempted"]===False];
ccTMock=ccTPrepMock[<||>,"UNVALIDATED_CANDIDATE",True,False,False,Infinity];
ccTAssert["infinite overall clock admits a finite source preparation",ccTMock["Outcome"]["Status"]==="PROPOSED"&&NumericQ[ccTMock["Seen"]["Options"]["TimeConstraint"]]&&ccTMock["Seen"]["Options"]["TimeConstraint"]<=40];
ccTMock=ccTPrepMock[<|"FinalPreparationOptions"->{"TimeConstraint"->Infinity}|>,"UNVALIDATED_CANDIDATE",True,False,False,Infinity];
ccTAssert["source proposal step stays finite even with unlimited final preparation requested",ccTMock["Outcome"]["Status"]==="PROPOSED"&&ccTMock["Seen"]["Options"]["TimeConstraint"]<=40];
ccTPair={ccTExt,5ccTT1+7ccTT2};ccTPairs={{ccTExt,2ccTT1},{ccTExt,3ccTT2}};ccTChannels={{1,1,2,2,3,Multiparticle[1,2],0,ccTT1+ccTT2}};
ccTAttempt=<|"Status"->"PROPOSED","PreparedPair"->ccTPair|>;
ccTProjection=ConstructiveDiagrams`Private`colorCurrentProjection[ccTAttempt,ccTPairs,ccTChannels,{3,-3,3,-3,1},{{ccTExt,ccTT1-ccTT2}}];
ccTAssert["common projection keeps all diagrams channels candidate and full benchmark",AssociationQ[ccTProjection]&&ccTProjection["Certificate"]["AllOriginalDiagramPairsIncluded"]===2&&ccTProjection["Certificate"]["AllOriginalTargetRowsIncluded"]===1&&ccTProjection["Certificate"]["AllIndependentFeynmanPairsIncluded"]===1&&ccTProjection["Certificate"]["CandidatePairIncluded"]===True];
ccTAssert["all independent color coefficients retained",Length[ccTProjection["Amplitudes"]]===2&&ccTProjection["Certificate"]["FullUnionRank"]===2];
ccTExtra=ConstructiveDiagrams`Private`colorCurrentProjection[ccTAttempt,ccTPairs,ccTChannels,{3,-3,3,-3,1},{{ccTExt,ccTT1+ccTT3}}];
ccTAssert["benchmark-only extra color direction is never dropped",AssociationQ[ccTExtra]&&ccTExtra["Certificate"]["FullUnionRank"]===3&&Length[ccTExtra["Amplitudes"]]===3];
ccTAssert["projection cannot omit independent benchmark",MissingQ[ConstructiveDiagrams`Private`colorCurrentProjection[ccTAttempt,ccTPairs,ccTChannels,{3,-3,3,-3,1},{}]]];
ccTFakeCheck[]:=<|"Checks"->{<|"Type"->"Feynman","Pole"->"synthetic benchmark","Status"->"PASS"|>},"Summary"-><|"PASS"->1|>|>;
ccTWorkflow[select_:False,clock_:60]:=Module[{calls={},validationInput=None,original,newData,result},
 original=<|"Basis"->{ccTT1,ccTT2},"GramMatrix"->{{9,3},{3,9}},"ProjectedDiagrams"->{{{ccTExt,11}},{{ccTExt,22}}},"ProjectedChannels"->{{},{}},"ProjectedFeynmanAmplitudes"->{{{ccTExt,111}},{{ccTExt,222}}}|>;
 newData=Join[original,<|"Basis"->{ccTT2,ccTT1},"ProjectedDiagrams"->{{{ccTExt,99}},{{ccTExt,98}}},"ProjectedFeynmanAmplitudes"->{{{ccTExt,999}},{{ccTExt,998}}}|>];
 result=Internal`InheritedBlock[{CreateDiagrams,DiagramAmplitudes,FactorizationAmplitudeChannels,ConstructAmplitude,FeynmanToSpinorProducts,
 ConstructiveDiagrams`Private`colorProjectionData,ConstructiveDiagrams`Private`colorScalarCurrentProjectionCertificate,
 ConstructiveDiagrams`Private`colorCurrentPrepare,ConstructiveDiagrams`Private`colorCurrentProjection,ConstructiveDiagrams`Private`colorOrbitValidate,
 ConstructiveDiagrams`Private`colorSpectatorPrepare,ConstructiveDiagrams`Private`colorHandednessPrepare,ConstructiveDiagrams`Private`colorFeynmanSignAgreement},
 Unprotect[CreateDiagrams,DiagramAmplitudes,FactorizationAmplitudeChannels,ConstructAmplitude,FeynmanToSpinorProducts];
 Clear[CreateDiagrams,DiagramAmplitudes,FactorizationAmplitudeChannels,ConstructAmplitude,FeynmanToSpinorProducts,
 ConstructiveDiagrams`Private`colorProjectionData,ConstructiveDiagrams`Private`colorScalarCurrentProjectionCertificate,
 ConstructiveDiagrams`Private`colorCurrentPrepare,ConstructiveDiagrams`Private`colorCurrentProjection,ConstructiveDiagrams`Private`colorOrbitValidate,
 ConstructiveDiagrams`Private`colorSpectatorPrepare,ConstructiveDiagrams`Private`colorHandednessPrepare,ConstructiveDiagrams`Private`colorFeynmanSignAgreement];
 CreateDiagrams[___]:={"SyntheticDiagram"};DiagramAmplitudes[___]:={{ccTExt,ccTT1+2ccTT2}};FactorizationAmplitudeChannels[___]:={};FeynmanToSpinorProducts[input_,___]:=input;
 ConstructiveDiagrams`Private`colorProjectionData[___]:=original;ConstructiveDiagrams`Private`colorScalarCurrentProjectionCertificate[___]:=Missing["Synthetic"];
 ConstructiveDiagrams`Private`colorCurrentPrepare[___]:=<|"Status"->"PROPOSED","PreparedPair"->ccTPair,"Selected"->False|>;
 ConstructiveDiagrams`Private`colorCurrentProjection[___]:=<|"Status"->"PROJECTED","Data"->newData,"Amplitudes"->{99,98}|>;
 ConstructiveDiagrams`Private`colorSpectatorPrepare[___]:=<|"Status"->"NOT_APPLICABLE","Attempted"->False|>;
 ConstructiveDiagrams`Private`colorHandednessPrepare[___]:=<|"Status"->"NOT_APPLICABLE","Attempted"->False|>;
 ConstructiveDiagrams`Private`colorFeynmanSignAgreement[___]:=<|"Status"->"PASS"|>;
 ConstructiveDiagrams`Private`colorOrbitValidate[p_,a_,e_,data_,___]:=(validationInput=data;If[select,<|"Status"->"COMPLETE","CoefficientReports"->Table[<|"Status"->"COMPLETE","Amplitude"->a[[j]],"AmplitudePair"->{e,a[[j]]},"Checks"->ccTFakeCheck[]|>,{j,Length[a]}]|>,<|"Status"->"NOT_SELECTED","Reason"->"Synthetic held-out failure"|>]);
 ConstructAmplitude[___]:=(AppendTo[calls,ConstructiveDiagrams`Private`$colorConstructionInput];With[{amp=Last[First[Last[calls]["DiagramAmplitudes"]]]},<|"Status"->"COMPLETE","Amplitude"->amp,"AmplitudePair"->{ccTExt,amp},"Checks"->ccTFakeCheck[]|>]);
 ConstructiveDiagrams`Private`colorConstructAmplitude[ccTProcess,ccTModel,ccTModel,Join[ccTSettings,<|"Output"->"Details","TimeConstraint"->clock,"BoseSymmetryGroups"->{},"FermionSymmetryGroups"->{}|>]]];
 <|"Result"->result,"Calls"->calls,"ValidationInput"->validationInput,"Original"->original,"NewData"->newData|>];
ccTFlow=ccTWorkflow[False];
ccTAssert["failed source validation falls back to original coefficient inputs",Length[ccTFlow["Calls"]]===2&&Lookup[ccTFlow["Calls"],"DiagramAmplitudes"]===ccTFlow["Original"]["ProjectedDiagrams"]&&Lookup[ccTFlow["Calls"],"FeynmanAmplitudes"]===ccTFlow["Original"]["ProjectedFeynmanAmplitudes"]];
ccTAssert["failed source diagnostics retained without selection",ccTFlow["Result"]["Status"]==="COMPLETE"&&ccTFlow["Result"]["ColorCurrentAttempt"]["Selected"]===False&&ccTFlow["Result"]["ColorBasis"]===ccTFlow["Original"]["Basis"]];
ccTFlow=ccTWorkflow[True];
ccTAssert["validated source skips ordinary coefficient reconstruction",ccTFlow["Calls"]==={}&&ccTFlow["Result"]["Status"]==="COMPLETE"&&ccTFlow["Result"]["ColorCurrentAttempt"]["Selected"]===True];
ccTAssert["selection uses the fully validated common color basis",ccTFlow["Result"]["ColorBasis"]===ccTFlow["NewData"]["Basis"]&&ccTFlow["ValidationInput"]===ccTFlow["NewData"]];
ccTFlow=ccTWorkflow[True,Infinity];
ccTAssert["public colored workflow supports an explicitly infinite overall clock",ccTFlow["Calls"]==={}&&ccTFlow["Result"]["Status"]==="COMPLETE"&&ccTFlow["Result"]["ColorCurrentAttempt"]["Selected"]===True];
ccTAssert["all new helpers remain private",Names["ConstructiveDiagrams`colorCurrent*"]==={}];
ccTSummary=<|"Status"->If[ccTFailed===0,"PASS","FAIL"],"Passed"->ccTPassed,"Failed"->ccTFailed,"Records"->ccTRecords,"SavedAmplitudeRead"->False,"NativePhaseSpaceChecksPerformed"->False|>;
If[StringQ[ccTReportPath]&&StringLength[ccTReportPath]>0,Export[ccTReportPath,ccTSummary,"RawJSON"]];Print[ccTSummary];Print["HEPCAT_TEST_COMPLETED:color-current-source-tests.wl"];
End[];Quit[If[ccTSummary["Status"]==="PASS",0,1]];
