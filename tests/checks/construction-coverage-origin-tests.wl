(* Constructor admission coverage uses the ordinary checker's target origins.
   Synthetic negative reports test bookkeeping only; the scalar cancellation
   report is produced by the real four-sample CheckAmplitudes implementation. *)
Begin["HEPCATCoverageOriginTests`"];
$HistoryLength=0;
HEPCATCoverageOriginTests`root=DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
HEPCATCoverageOriginTests`source=Replace[Environment["HEPCAT_QCD_TEST_SOURCE"],{""->FileNameJoin[{HEPCATCoverageOriginTests`root,"source"}],Except[_String]->FileNameJoin[{HEPCATCoverageOriginTests`root,"source"}]}];
Get[FileNameJoin[{HEPCATCoverageOriginTests`source,"HEPCAT.wl"}]];
 HEPCATCoverageOriginTests`rows={};
 HEPCATCoverageOriginTests`assert[HEPCATCoverageOriginTests`name_String,HEPCATCoverageOriginTests`condition_]:=AppendTo[HEPCATCoverageOriginTests`rows,<|"Test"->HEPCATCoverageOriginTests`name,"PASS"->TrueQ[HEPCATCoverageOriginTests`condition]|>];
 HEPCATCoverageOriginTests`prop[HEPCATCoverageOriginTests`a_Integer,HEPCATCoverageOriginTests`b_Integer,HEPCATCoverageOriginTests`mass_:0]:=ConstructiveDiagrams`PropDen[ConstructiveDiagrams`Mom[ConstructiveDiagrams`Multiparticle[HEPCATCoverageOriginTests`a,HEPCATCoverageOriginTests`b]],HEPCATCoverageOriginTests`mass];
 HEPCATCoverageOriginTests`model={{{"Scalar","s","s",1,0,0,0,1,""}},{},{},{}};
 HEPCATCoverageOriginTests`ext=Thread[ConstructiveDiagrams`Mass/@Range[4]->ConstantArray[0,4]];
 HEPCATCoverageOriginTests`channel={0,0,0,0,ConstructiveDiagrams`Multiparticle[1,2],0,-1/HEPCATCoverageOriginTests`prop[1,2]};
 HEPCATCoverageOriginTests`pair={HEPCATCoverageOriginTests`ext,1/HEPCATCoverageOriginTests`prop[1,2]+(HEPCATCoverageOriginTests`prop[1,3]-2 ConstructiveDiagrams`MomProd[1,3])/HEPCATCoverageOriginTests`prop[1,3]};
 HEPCATCoverageOriginTests`real=ConstructiveDiagrams`CheckAmplitudes[HEPCATCoverageOriginTests`pair,{HEPCATCoverageOriginTests`channel},HEPCATCoverageOriginTests`model,"Samples"->4,"HighEnergySamples"->4,"ReferenceSamples"->4,"FeynmanSamples"->4,"RandomSeed"->17291,"Parallelization"->False,"TimeConstraint"->2,"FeynmanAmplitudes"->{HEPCATCoverageOriginTests`ext,1/HEPCATCoverageOriginTests`prop[1,2]},"Output"->"Details"];
 HEPCATCoverageOriginTests`gate[HEPCATCoverageOriginTests`report_,HEPCATCoverageOriginTests`amp_:Automatic,HEPCATCoverageOriginTests`channels_:Automatic]:=ConstructiveDiagrams`Private`constructAllChecksPassQ[HEPCATCoverageOriginTests`report,Replace[HEPCATCoverageOriginTests`amp,Automatic->HEPCATCoverageOriginTests`pair],Replace[HEPCATCoverageOriginTests`channels,Automatic->{HEPCATCoverageOriginTests`channel}],HEPCATCoverageOriginTests`model,1];
 HEPCATCoverageOriginTests`assert["Real automatic checker emits completed spurious PASS",Length[HEPCATCoverageOriginTests`real["SpuriousChecks"]]===1&&HEPCATCoverageOriginTests`real["SpuriousChecks"][[1]]["Status"]==="PASS"&&HEPCATCoverageOriginTests`real["SpuriousChecks"][[1]]["CompletedSamples"]===4];
 HEPCATCoverageOriginTests`assert["Real ordinary checks all PASS",AllTrue[HEPCATCoverageOriginTests`real["Checks"],Lookup[#,"Status",None]==="PASS"&]];
 HEPCATCoverageOriginTests`assert["Real inferred modeled zero is covered by spurious check",HEPCATCoverageOriginTests`gate[HEPCATCoverageOriginTests`real]];
 HEPCATCoverageOriginTests`assert["Legacy physical coverage acceptance preserved",HEPCATCoverageOriginTests`gate[HEPCATCoverageOriginTests`real,{HEPCATCoverageOriginTests`ext,1/HEPCATCoverageOriginTests`prop[1,2]}]];
 HEPCATCoverageOriginTests`assert["Missing supplied physical channel rejects",!HEPCATCoverageOriginTests`gate[Join[HEPCATCoverageOriginTests`real,<|"Channels"->{}|>]]];
 HEPCATCoverageOriginTests`assert["Missing inferred spurious row rejects",!HEPCATCoverageOriginTests`gate[Join[HEPCATCoverageOriginTests`real,<|"SpuriousChecks"->{}|>]]];
 HEPCATCoverageOriginTests`assert["Missing inferred discovery row rejects",!HEPCATCoverageOriginTests`gate[Join[HEPCATCoverageOriginTests`real,<|"PoleDiscovery"->{}|>]]];
 Do[HEPCATCoverageOriginTests`assert["Inferred spurious "<>HEPCATCoverageOriginTests`status<>" rejects",!HEPCATCoverageOriginTests`gate[Join[HEPCATCoverageOriginTests`real,<|"SpuriousChecks"->(Join[#,<|"Status"->HEPCATCoverageOriginTests`status|>]&/@HEPCATCoverageOriginTests`real["SpuriousChecks"])|>]]],{HEPCATCoverageOriginTests`status,{"FAIL","UNRESOLVED","Opp Sign"}}];
 HEPCATCoverageOriginTests`assert["Canonical complement coverage accepted",HEPCATCoverageOriginTests`gate[Join[HEPCATCoverageOriginTests`real,<|"SpuriousChecks"->(Join[#,<|"Pole"->HEPCATCoverageOriginTests`prop[2,4]|>]&/@HEPCATCoverageOriginTests`real["SpuriousChecks"])|>]]];
 HEPCATCoverageOriginTests`assert["Different cut coverage rejected",!HEPCATCoverageOriginTests`gate[Join[HEPCATCoverageOriginTests`real,<|"SpuriousChecks"->(Join[#,<|"Pole"->HEPCATCoverageOriginTests`prop[1,4]|>]&/@HEPCATCoverageOriginTests`real["SpuriousChecks"])|>]]];
 HEPCATCoverageOriginTests`assert["Supplied zero target still requires physical row",!HEPCATCoverageOriginTests`gate[HEPCATCoverageOriginTests`real,Automatic,{HEPCATCoverageOriginTests`channel,{0,0,0,0,ConstructiveDiagrams`Multiparticle[1,3],0,0}}]];
 HEPCATCoverageOriginTests`assert["Automatic discovery remains mandatory",!HEPCATCoverageOriginTests`gate[Join[HEPCATCoverageOriginTests`real,<|"AutomaticPoleDiscovery"->False|>]]];
 Do[HEPCATCoverageOriginTests`assert[HEPCATCoverageOriginTests`key<>" remains mandatory",!HEPCATCoverageOriginTests`gate[Join[HEPCATCoverageOriginTests`real,Association[HEPCATCoverageOriginTests`key-><|"Status"->"UNRESOLVED"|>]]]],{HEPCATCoverageOriginTests`key,{"HighEnergyCheck","ReferenceCheck","FeynmanCheck"}}];
 HEPCATCoverageOriginTests`assert["Any other true failure remains terminal",!HEPCATCoverageOriginTests`gate[Join[HEPCATCoverageOriginTests`real,<|"Checks"->Append[HEPCATCoverageOriginTests`real["Checks"],<|"Type"->"Physical","Status"->"FAIL"|>]|>]]];
 HEPCATCoverageOriginTests`inferred=Last[ConstructiveDiagrams`Private`residueSimpleInputs[HEPCATCoverageOriginTests`pair,{HEPCATCoverageOriginTests`channel},HEPCATCoverageOriginTests`model]["Targets"]];
 HEPCATCoverageOriginTests`assert["Unknown inferred exchange rejected",!ConstructiveDiagrams`Private`constructAutomaticZeroPoleCoverageQ[HEPCATCoverageOriginTests`real,{Join[HEPCATCoverageOriginTests`inferred,<|"Complete"->False,"StateSum"->Missing["UnknownExchangeMass"]|>]}]];
 HEPCATCoverageOriginTests`assert["Nonzero inferred expectation rejected",!ConstructiveDiagrams`Private`constructAutomaticZeroPoleCoverageQ[HEPCATCoverageOriginTests`real,{Join[HEPCATCoverageOriginTests`inferred,<|"StateSum"->1|>]}]];
 HEPCATCoverageOriginTests`assert["Supplied channel expression cannot use inferred-zero coverage",!ConstructiveDiagrams`Private`constructAutomaticZeroPoleCoverageQ[HEPCATCoverageOriginTests`real,{Join[HEPCATCoverageOriginTests`inferred,<|"ChannelExpression"->0|>]}]];
 HEPCATCoverageOriginTests`ownerA=ConstructiveDiagrams`ModelParameter["m","ownerA","Real"];HEPCATCoverageOriginTests`ownerB=ConstructiveDiagrams`ModelParameter["m","ownerB","Real"];
 HEPCATCoverageOriginTests`ownedTarget=Join[HEPCATCoverageOriginTests`inferred,<|"Mass"->HEPCATCoverageOriginTests`ownerA|>];
 HEPCATCoverageOriginTests`ownedReport=Join[HEPCATCoverageOriginTests`real,<|"SpuriousChecks"->{<|"Status"->"PASS","Pole"->HEPCATCoverageOriginTests`prop[1,3,HEPCATCoverageOriginTests`ownerA]|>},"PoleDiscovery"->{<|"Classification"->"Spurious","CanonicalFactor"->HEPCATCoverageOriginTests`prop[1,3,HEPCATCoverageOriginTests`ownerA]|>}|>];
 HEPCATCoverageOriginTests`assert["Same model mass owner retained",ConstructiveDiagrams`Private`constructAutomaticZeroPoleCoverageQ[HEPCATCoverageOriginTests`ownedReport,{HEPCATCoverageOriginTests`ownedTarget}]];
 HEPCATCoverageOriginTests`assert["Distinct model mass owners remain distinct",!ConstructiveDiagrams`Private`constructAutomaticZeroPoleCoverageQ[Join[HEPCATCoverageOriginTests`ownedReport,<|"SpuriousChecks"->{<|"Status"->"PASS","Pole"->HEPCATCoverageOriginTests`prop[1,3,HEPCATCoverageOriginTests`ownerB]|>}|>],{HEPCATCoverageOriginTests`ownedTarget}]];

HEPCATCoverageOriginTests`failed=Count[Lookup[HEPCATCoverageOriginTests`rows,"PASS"],False];
Scan[If[!TrueQ[#["PASS"]],Print["FAIL: ",#["Test"]]]&,HEPCATCoverageOriginTests`rows];
Print[Length[HEPCATCoverageOriginTests`rows]-HEPCATCoverageOriginTests`failed," passed, ",HEPCATCoverageOriginTests`failed," failed"];
Print["HEPCAT_TEST_COMPLETED:construction-coverage-origin-tests.wl"];
End[];Quit[If[HEPCATCoverageOriginTests`failed===0&&Length[HEPCATCoverageOriginTests`rows]===23,0,1]];
