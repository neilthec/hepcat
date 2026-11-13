Global`qcdTestRoot=Replace[Environment["HEPCAT_QCD_REPOSITORY_ROOT"],{""->DirectoryName[DirectoryName[DirectoryName[$InputFileName]]],Except[_String]->DirectoryName[DirectoryName[DirectoryName[$InputFileName]]]}];
Global`$HEPCATpath=Replace[Environment["HEPCAT_QCD_TEST_SOURCE"],{""->FileNameJoin[{Global`qcdTestRoot,"source"}],Except[_String]->FileNameJoin[{Global`qcdTestRoot,"source"}]}];
Get[FileNameJoin[{Global`$HEPCATpath,"HEPCAT.wl"}]];
Begin["HEPCATQCDDraftMetadataControls`Private`"];$ContextPath=Prepend[DeleteCases[$ContextPath,"HEPCATQCDDraftMetadataControls`Private`"],"HEPCATQCDDraftMetadataControls`Private`"];$HistoryLength=0;
root=Global`qcdTestRoot;out=CreateDirectory[FileNameJoin[{$TemporaryDirectory,CreateUUID["hepcat-qcd-unit-"]}]];passed=0;failed=0;records={};SetAttributes[test,HoldRest];
test[name_,value_]:=Module[{ok=TrueQ[value]},AppendTo[records,<|"Test"->name,"Pass"->ok|>];If[ok,passed++,failed++];Print[If[ok,"PASS ","FAIL "],name]];
Global`reject[args___]:=HoldComplete[args];Global`particleRow="global sentinel";Global`sourceClass[args___]:=False;Global`canonicalColour="global colour sentinel";
ambientSymbols={Global`reject,Global`particleRow,Global`sourceClass,Global`canonicalColour};SetAttributes[definitionSHA,HoldAll];definitionSHA[s_]:=Hash[{OwnValues[s],DownValues[s],UpValues[s],SubValues[s],NValues[s],DefaultValues[s],FormatValues[s],Options[s],Attributes[s],Messages[s]},"SHA256"];
ambientBefore={definitionSHA[Global`reject],definitionSHA[Global`particleRow],definitionSHA[Global`sourceClass],definitionSHA[Global`canonicalColour]};globalsBefore=Names["Global`*"];contextBefore={$Context,$ContextPath};
oldHeads={ConstructiveDiagrams`ConstructAmplitude,ConstructiveDiagrams`CheckAmplitudes,ConstructiveDiagrams`PrepareAmplitudes,ConstructiveDiagrams`CompleteFactorization,ConstructiveDiagrams`CompleteHighEnergy,ConstructiveDiagrams`FeynmanToSpinorProducts,ConstructiveDiagrams`Private`colorConstructAmplitude,ConstructiveDiagrams`Private`colorOrbitValidate,ConstructiveDiagrams`Private`colorCurrentPrepare,ConstructiveDiagrams`Private`colorCurrentProjection};oldBefore=definitionSHA/@oldHeads;
Get[FileNameJoin[{Global`$HEPCATpath,"internal","HEPCAT-QCDCurrents.wl"}]];
test["coldload preserves existing public and private math definitions",oldBefore===(definitionSHA/@oldHeads)];
test["coldload creates no Global symbols",Names["Global`*"]===globalsBefore];
test["coldload preserves ambient helper ownvalues and downvalues",ambientBefore==={definitionSHA[Global`reject],definitionSHA[Global`particleRow],definitionSHA[Global`sourceClass],definitionSHA[Global`canonicalColour]}];
test["coldload restores caller contexts",{$Context,$ContextPath}===contextBefore];
test["new implementation definitions are private",FreeQ[Names["HEPCATMinimalAdjointCubicSources`*"] ,s_String/;!StringStartsQ[s,"HEPCATMinimalAdjointCubicSources`Private`"]]&&FreeQ[Names["HEPCATMinimalQCDCurrentSources`*"],s_String/;!StringStartsQ[s,"HEPCATMinimalQCDCurrentSources`Private`"]]];
model=ReadModel[root<>"/models",1];lower=ReadModel[root<>"/models",2];qed=ReadModel[root<>"/models",3];inventory=Import[root<>"/tests/SM-5-point/inventory.json","RawJSON"];
(* Block all amplitude-producing APIs during the inventory audit. *)
blocked=0;SetAttributes[metadataOnly,HoldAll];metadataOnly[body_]:=Internal`InheritedBlock[{ConstructiveDiagrams`CreateDiagrams,ConstructiveDiagrams`DiagramAmplitudes,ConstructiveDiagrams`PrepareAmplitudes,ConstructiveDiagrams`CheckAmplitudes,ConstructiveDiagrams`FeynmanToSpinorProducts,ConstructiveDiagrams`LookupAmplitude,HEPCATCovariantCurrentSources`Private`sourceCatalog,HEPCATCovariantCurrentSources`Private`exactVertexMatch},
 Unprotect[ConstructiveDiagrams`CreateDiagrams,ConstructiveDiagrams`DiagramAmplitudes,ConstructiveDiagrams`PrepareAmplitudes,ConstructiveDiagrams`CheckAmplitudes,ConstructiveDiagrams`FeynmanToSpinorProducts,ConstructiveDiagrams`LookupAmplitude];
 Clear[ConstructiveDiagrams`CreateDiagrams,ConstructiveDiagrams`DiagramAmplitudes,ConstructiveDiagrams`PrepareAmplitudes,ConstructiveDiagrams`CheckAmplitudes,ConstructiveDiagrams`FeynmanToSpinorProducts,ConstructiveDiagrams`LookupAmplitude,HEPCATCovariantCurrentSources`Private`sourceCatalog,HEPCATCovariantCurrentSources`Private`exactVertexMatch];
 ConstructiveDiagrams`CreateDiagrams[___]:=(blocked++;Failure["BlockedPhysicsCall",<||>]);ConstructiveDiagrams`DiagramAmplitudes[___]:=(blocked++;Failure["BlockedPhysicsCall",<||>]);ConstructiveDiagrams`PrepareAmplitudes[___]:=(blocked++;Failure["BlockedPhysicsCall",<||>]);ConstructiveDiagrams`CheckAmplitudes[___]:=(blocked++;Failure["BlockedPhysicsCall",<||>]);ConstructiveDiagrams`FeynmanToSpinorProducts[___]:=(blocked++;Failure["BlockedPhysicsCall",<||>]);ConstructiveDiagrams`LookupAmplitude[___]:=(blocked++;Failure["BlockedPhysicsCall",<||>]);HEPCATCovariantCurrentSources`Private`sourceCatalog[___]:=(blocked++;Failure["BlockedPhysicsCall",<||>]);HEPCATCovariantCurrentSources`Private`exactVertexMatch[___]:=(blocked++;Failure["BlockedPhysicsCall",<||>]);body];
all=metadataOnly[Map[Function[c,<|"ID"->c["ID"],"FamilyID"->c["FamilyID"],"Particles"->c["Particles"],"Eligible"->TrueQ[ConstructiveDiagrams`Private`colorQCDCurrentSourceClass[c["Particles"],model]]|>],inventory["Cases"]]];
eligible=Select[all,#["Eligible"]&];
(* The regression cohort is selected by the preexisting source capability,
   never by the mutable campaign ledger. New QCD results may later be present. *)
qualified=Select[all,TrueQ[ConstructiveDiagrams`Private`colorCurrentSourceClass[#["Particles"],model]]&];
oldG=Select[all,Function[c,With[{r=HEPCATCovariantCurrentSources`Private`particleRow[#,model]&/@c["Particles"]},Count[r[[All,5]],1]===4&&Count[r[[All,8]],8]===1&&Count[Abs[r[[All,8]]],3]===2&&Count[r[[All,8]],1]===2&&Count[r[[All,6]],0]===3&&Count[r[[All,9]],"L"|"R"]===2]]];
test["narrow gate admits exactly54cases27families",Length[eligible]===54&&Length[DeleteDuplicates[Lookup[eligible,"FamilyID"]]]===27];
test["every preexisting fullcolour current source class remains outside the gate",qualified=!={}&&AllTrue[qualified,!#["Eligible"]&]];
test["every colourless external state remains excluded",AllTrue[Select[all,FreeQ[ConstructiveDiagrams`Private`colorParticleRepresentation[#,model]&/@#["Particles"],8]&],!#["Eligible"]&]];
test["earlier36Gcases are twofundamental twochiral singlet cases",Length[oldG]===36&&AllTrue[oldG,Function[c,With[{r=HEPCATCovariantCurrentSources`Private`particleRow[#,model]&/@c["Particles"]},Count[Abs[r[[All,8]]],3]===2&&Count[r[[All,8]],1]===2&&Count[r[[All,6]],0]===3&&Count[r[[All,9]],"L"|"R"]===2]]]];
test["all earlier36Gcases excluded",AllTrue[oldG,!#["Eligible"]&]];
sm4=Select[HEPCATCovariantCurrentSources`Private`rowParticles/@lower[[2]],Length[#]===4&];sm4catalog=Import[root<>"/tests/checks/fixtures/sm-factorization-catalog.json","RawJSON"];test["all savedSM4states excluded",Length[sm4]===Length[sm4catalog["Entries"]]&&Length[sm4]>0&&AllTrue[sm4,!ConstructiveDiagrams`Private`colorQCDCurrentSourceClass[#,lower]&]];
qrow=SelectFirst[qed[[1]],#[[5]]===1&];vrow=SelectFirst[qed[[1]],#[[5]]===2&];qbase=qrow[[{2,3}]];qhelicities=vrow[[{2,3}]];
qprocesses=Join[
 Flatten[Table[(Join[qbase,#]&/@Tuples[qhelicities,n]),{n,2,5}],1],
 (Join[qbase,qbase,#]&/@(List/@qhelicities)),
 (Join[qbase,qbase,#]&/@Tuples[qhelicities,2])];
test["QEDfourthroughsevenpointfixtures excluded",AllTrue[qprocesses,!ConstructiveDiagrams`Private`colorQCDCurrentSourceClass[#,qed]&]];
test["metadata audit never calls a physics builder or converter",blocked===0];
example=First[eligible]["Particles"];
noReference=metadataOnly[ConstructiveDiagrams`Private`colorQCDCurrentPrepare[example,model,lower,<|"FeynmanModel"->None|>,Infinity,{}]];test["no independent benchmark preserves old route without source work",noReference["Status"]==="NOT_APPLICABLE"&&!noReference["Attempted"]];
noClock=metadataOnly[ConstructiveDiagrams`Private`colorQCDCurrentPrepare[example,model,lower,<|"FeynmanModel"->"declared"|>,AbsoluteTime[]-1,{}]];test["expired overall budget performs no proposal work",noClock["Status"]==="NO_REMAINING_BUDGET"&&!noClock["Attempted"]];
test["unlimited overall clock has a supported typed entrypoint",ConstructiveDiagrams`Private`constructSecondsQ[Infinity]];
test["all metadata and early-exit guards make no physics call",blocked===0];
audit=<|"Status"->If[failed===0,"PASS","FAIL"],"ContextSHA256"->Null,"PriorCurrentClassCasesInspected"->Length[qualified],"NativeRecordsInspected"->0,"AllPriorCurrentSourceClassesExcluded"->AllTrue[qualified,!#["Eligible"]&],"EligibleCases"->eligible,"EligibleCaseCount"->Length[eligible],"EligibleFamilyCount"->Length[DeleteDuplicates[Lookup[eligible,"FamilyID"]]],"EarlierGCaseCount"->Length[oldG],"EarlierGMetadata"->oldG,"SM4RowsInspected"->Length[sm4],"PhysicsCalls"->blocked,"NoInstalledSourceEdits"->True|>;
Export[out<>"/eligibility-audit.json",audit,"RawJSON"];Export[out<>"/metadata-and-cold-controls.json",<|"Status"->If[failed===0,"ALL_PASS","FAIL"],"Passed"->passed,"Failed"->failed,"Records"->records|>,"RawJSON"];Print[passed," metadata/coldload controls; ",failed," failures."];Print["HEPCAT_TEST_COMPLETED:qcd-metadata-cold-tests.wl"];End[];Quit[If[failed===0,0,1]];
