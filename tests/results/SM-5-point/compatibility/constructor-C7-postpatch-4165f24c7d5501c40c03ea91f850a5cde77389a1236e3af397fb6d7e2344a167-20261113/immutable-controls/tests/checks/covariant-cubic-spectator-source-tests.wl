Get[FileNameJoin[{DirectoryName[DirectoryName[DirectoryName[$InputFileName]]],"source","HEPCAT.wl"}]];
Begin["HEPCATCubicSpectatorTests`Private`"];
$ContextPath=Prepend[DeleteCases[$ContextPath,"HEPCATCubicSpectatorTests`Private`"],"HEPCATCubicSpectatorTests`Private`"];
c7Directory=DirectoryName[$InputFileName];c7Passed=0;c7Failed=0;c7Records={};
c7Assert[c7Name_String,condition_]:=Module[{ok=TrueQ[condition]},AppendTo[c7Records,<|"Test"->c7Name,"Pass"->ok|>];If[ok,c7Passed++;Print["PASS ",c7Name],c7Failed++;Print["FAIL ",c7Name]]];
c7UnchangedHeads={HEPCATCovariantCurrentSources`Private`sourceCatalog,HEPCATCovariantCurrentSources`Private`exactVertexMatch,
 HEPCATCovariantCurrentSources`Private`liftVertex,ReduceSpinorProducts,PrepareAmplitudes,CheckAmplitudes};
c7Before=DownValues/@c7UnchangedHeads;
c7Assert["new preparation and projection helpers loaded",DownValues[ConstructiveDiagrams`Private`colorSpectatorPrepare]=!={}&&DownValues[ConstructiveDiagrams`Private`colorSpectatorProjection]=!={}];
c7Assert["required existing algebra helpers are available",AllTrue[c7Before,#=!={}&]];
c7Model={{{"f","f","F",1,1,1,0,1,"","f","F"},{"g","g","G",2,1,2,0,1,"","g","G"},
 {"A","A.+","A.-",22,2,0,0,1,"","A","A"},{"h","h","h",25,0,1,0,1,"","h","h"},
 {"Z","Z","Z",23,2,2,0,1,"","Z","Z"},{"n","n","N",12,1,0,0,1,"L","n","N"}}, {},{}, {}};
c7ClassProbe[c7Process_List,c7M_:c7Model]:=Module[{c7Calls=0,c7Result},
 c7Result=Internal`InheritedBlock[{CreateDiagrams},Unprotect[CreateDiagrams];Clear[CreateDiagrams];
  CreateDiagrams[___]:=(c7Calls++;{});
  HEPCATCovariantCurrentSources`Private`covariantCurrentSource[c7Process,c7M]];
 <|"Calls"->c7Calls,"Failure"->If[FailureQ[c7Result],c7Result[[1]],None]|>];
c7Assert["positive-helicity cubic class reaches graph generation",c7ClassProbe[{"f","F","A.+","h","h"}]["Calls"]===1];
c7Assert["negative-helicity cubic class reaches graph generation",c7ClassProbe[{"f","F","A.-","h","h"}]["Calls"]===1];
c7Assert["old massive four-fermion source class preserved",c7ClassProbe[{"f","F","g","G","h"}]["Calls"]===1];
c7Assert["chiral four-fermion source class remains excluded",c7ClassProbe[{"f","F","n","N","h"}]["Calls"]===0];
c7Assert["two-vector calibrated class not added to cubic helper",c7ClassProbe[{"f","F","Z","Z","h"}]["Calls"]===0];
c7Assert["three-photon class not added",c7ClassProbe[{"f","F","A.+","A.+","A.-"}]["Calls"]===0];
c7Assert["massless undeclared Dirac endpoints remain excluded",c7ClassProbe[{"f","F","A.+","h","h"},ReplacePart[c7Model,{1,1,6}->0]]["Calls"]===0];
c7Assert["colored current cannot bypass spectator proof",c7ClassProbe[{"f","F","A.+","h","h"},ReplacePart[c7Model,{1,1,8}->3]]["Calls"]===0];
c7Assert["massive L/R inconsistency remains excluded",c7ClassProbe[{"f","F","A.+","h","h"},ReplacePart[c7Model,{1,1,9}->"L"]]["Calls"]===0];
c7Assert["explicit positive photon signed weight",HEPCATCovariantCurrentSources`Private`declaredExternalSpinWeights[{"f","F","A.+","h","h"},c7Model]==={1,1,-2,0,0}];
c7Assert["explicit negative photon signed weight",HEPCATCovariantCurrentSources`Private`declaredExternalSpinWeights[{"f","F","A.-","h","h"},c7Model]==={1,1,2,0,0}];
c7Assert["old chiral and massive-vector weights unchanged",HEPCATCovariantCurrentSources`Private`declaredExternalSpinWeights[{"n","N","Z","h","h"},c7Model]==={1,-1,2,0,0}];
c7Settings=Association[Options[ConstructAmplitude]];
c7Assert["spectator source requires independent benchmark",Lookup[ConstructiveDiagrams`Private`colorSpectatorPrepare[{"f","F","A.+","h","h"},c7Model,c7Model,c7Settings,AbsoluteTime[]+30,{}],"Attempted",True]===False];
c7Colored=ReplacePart[c7Model,{1,1,8}->3];c7Settings["FeynmanModel"]="PresenceOnlyUnusedFixture";
c7Assert["three-photon spectator shape does not attempt a source",Lookup[ConstructiveDiagrams`Private`colorSpectatorPrepare[{"f","F","A.+","A.+","A.-"},c7Colored,c7Colored,c7Settings,AbsoluteTime[]+30,{}],"Attempted",True]===False];
c7Assert["four-colored-fermion spectator shape excluded",Lookup[ConstructiveDiagrams`Private`colorSpectatorPrepare[{"f","F","f","F","h"},c7Colored,c7Colored,c7Settings,AbsoluteTime[]+30,{}],"Attempted",True]===False];
c7Assert["zero remaining clock skips spectator attempt",Lookup[ConstructiveDiagrams`Private`colorSpectatorPrepare[{"f","F","Z","h","h"},c7Colored,c7Colored,c7Settings,AbsoluteTime[]-1,{}],"Attempted",True]===False];
c7Ext=Thread[(Mass/@Range[5])->{1,1,0,1,1}];c7Delta=ColorDelta[ColorIndex[2],ColorIndex[1]];
c7Attempt=<|"Status"->"PROPOSED","PreparedPair"->{c7Ext,c7Delta}|>;c7Pairs={{c7Ext,2c7Delta}};
c7Channels={{1,1,0,1,1,Multiparticle[1,3],1,3c7Delta}};c7Benchmark={{c7Ext,c7Delta}};c7Reps={3,-3,1,1,1};
c7Projection=ConstructiveDiagrams`Private`colorSpectatorProjection[c7Attempt,c7Pairs,c7Channels,c7Reps,c7Benchmark];
c7Assert["projection retains all original diagrams targets and benchmark",AssociationQ[c7Projection]&&c7Projection["Certificate"]["AllOriginalDiagramPairsIncluded"]===1&&c7Projection["Certificate"]["AllOriginalTargetRowsIncluded"]===1&&c7Projection["Certificate"]["AllIndependentFeynmanPairsIncluded"]===1];
c7Assert["projection remains unvalidated candidate data",Lookup[c7Projection,"Status",None]==="PROJECTED"&&!KeyExistsQ[c7Projection,"CoefficientReports"]];
c7Assert["rank-one common Gram remains exact",c7Projection["Data"]["GramMatrix"]==={{3}}];
c7Assert["independent benchmark-only direction rejects",!AssociationQ[ConstructiveDiagrams`Private`colorSpectatorProjection[c7Attempt,c7Pairs,c7Channels,c7Reps,{{c7Ext,1+c7Delta}}]]];
c7Assert["missing independent benchmark rejects",!AssociationQ[ConstructiveDiagrams`Private`colorSpectatorProjection[c7Attempt,c7Pairs,c7Channels,c7Reps,{}]]];
c7Assert["source rejection cannot become projected amplitude",!AssociationQ[ConstructiveDiagrams`Private`colorSpectatorProjection[<|"Status"->"NOT_PROPOSED"|>,c7Pairs,c7Channels,c7Reps,c7Benchmark]]];
c7MockPrepare[c7Bag_Association,c7Reference_:False,c7SourceStatus_:"UNVALIDATED_CANDIDATE"]:=Module[
 {c7Options=c7Settings,c7Pair,c7Proposal,c7Seen=<||>,c7Outcome,c7PrepBefore=DownValues[PrepareAmplitudes],c7SourceBefore=DownValues[ConstructiveDiagrams`Private`colorSpectatorSource]},
 c7Options=Join[c7Settings,c7Bag];c7Pair={c7Ext,If[TrueQ[c7Reference],c7Delta SpinorChain[Spinor["Xi","Angle",3],Spinor["Helicity","Angle",3]],c7Delta]};
 c7Proposal=<|"Status"->c7SourceStatus,"Amplitude"->c7Pair,"Expression"->Last[c7Pair],"ExternalMassRules"->c7Ext,
  "EveryOriginalDiagramCovered"->True,"EveryUsedOrderedVertexCertified"->True,"CompleteOffShellConversionCoverage"->True,"RequiresCheckAmplitudes"->True,
  "NumericalCoefficientFit"->False,"IndependentBenchmarkRead"->False,"NativeValidationPerformed"->False,"NewIndependentContacts"->False,
  "InternalOnShellConditions"->{},"NumericalParameterAssignments"->{},"SourceModelSHA256"->HEPCATCovariantCurrentSources`Private`objectSHA[c7Colored],
  "LowerModelSHA256"->HEPCATCovariantCurrentSources`Private`objectSHA[c7Colored],"OriginalDiagramCount"->1|>;
 c7Outcome=Internal`InheritedBlock[{PrepareAmplitudes,ConstructiveDiagrams`Private`colorSpectatorSource},
  Unprotect[PrepareAmplitudes];Clear[PrepareAmplitudes];Clear[ConstructiveDiagrams`Private`colorSpectatorSource];
  ConstructiveDiagrams`Private`colorSpectatorSource[___]:=c7Proposal;
  PrepareAmplitudes[c7Value_,c7Supplied_,c7Rules___Rule]:=(c7Seen=<|"Input"->c7Value,"Model"->c7Supplied,"Options"->Association[{c7Rules}]|>;
    <|"Status"->"PREPARED","PreparedAmplitudes"->{c7Pair},"Options"->c7Seen["Options"],
      "ItemReports"->{<|"Status"->"PREPARED","PreparedExpression"->Last[c7Pair],"DenominatorsKeptOffShell"->True,
        "PolePreference"-><|"Status"->"COMPLETE"|>|>}|>);
  ConstructiveDiagrams`Private`colorSpectatorPrepare[{"f","F","Z","h","h"},c7Colored,c7Colored,c7Options,AbsoluteTime[]+80,{<||>}]];
 <|"Result"->c7Outcome,"Observed"->c7Seen,"Pair"->c7Pair,"DefinitionsRestored"->(c7PrepBefore===DownValues[PrepareAmplitudes]&&c7SourceBefore===DownValues[ConstructiveDiagrams`Private`colorSpectatorSource])|>
];
c7Observed=c7MockPrepare[<|"PolePreference"->"AllPhysical"|>];
c7Assert["completed pair is passed without a diagram-list wrapper",c7Observed["Observed"]["Input"]===c7Observed["Pair"]];
c7Assert["source preparation always disables internal shells",c7Observed["Observed"]["Options"]["InternalOnShell"]===False];
c7Assert["all-physical preference forwarded",c7Observed["Observed"]["Options"]["PolePreference"]==="AllPhysical"];
c7Assert["source preparation mock definitions restore",c7Observed["DefinitionsRestored"]];
c7Observed=c7MockPrepare[<|"PolePreference"->"Xi"|>,True];
c7Assert["explicit Xi preference forwarded and retained",c7Observed["Observed"]["Options"]["PolePreference"]==="Xi"&&c7Observed["Result"]["Status"]==="PROPOSED"];
c7Observed=c7MockPrepare[<|"PolePreference"->None|>,True];
c7Assert["explicit None preference retained",c7Observed["Result"]["Status"]==="PROPOSED"];
c7Observed=c7MockPrepare[<|"PolePreference"->"AllPhysical"|>,True];
c7Assert["unresolved references prevent default source selection",c7Observed["Result"]["Status"]==="NOT_PROPOSED"&&KeyExistsQ[c7Observed["Result"],"Proposal"]&&KeyExistsQ[c7Observed["Result"],"Preparation"]];
c7Observed=c7MockPrepare[<|"FinalPreparationOptions"->{"TimeConstraint"->1,"InternalOnShell"->True}|>];
c7Assert["explicit child clock retained but shells stay external-only",c7Observed["Observed"]["Options"]["TimeConstraint"]<=1&&c7Observed["Observed"]["Options"]["InternalOnShell"]===False];
c7Observed=c7MockPrepare[<||>,False,"COMPLETE"];
c7Assert["source may never bypass independent validation with a COMPLETE label",c7Observed["Result"]["Status"]==="NOT_PROPOSED"];
c7Assert["existing matchers reducers preparation and checkers restored",c7Before===(DownValues/@c7UnchangedHeads)];
c7Assert["all thirty-four preceding controls ran",Length[c7Records]===34];
c7Summary=<|"Status"->If[c7Failed===0,"PASS","FAIL"],"Passed"->c7Passed,"Failed"->c7Failed,"Records"->c7Records,
 "ActualFivePointConstructionPerformed"->False,
 "NativeKinematicsEvaluated"->False,"ExistingMatchersReducersAndCheckersUnchanged"->(c7Before===(DownValues/@c7UnchangedHeads))|>;
Print[c7Summary];
Print["CHECKS: ",c7Passed+c7Failed,"; FAILURES: ",c7Failed];
If[c7Failed===0,Print["HEPCAT_TEST_COMPLETED:covariant-cubic-spectator-source-tests.wl"]];
End[];Quit[If[c7Failed===0,0,1]];
