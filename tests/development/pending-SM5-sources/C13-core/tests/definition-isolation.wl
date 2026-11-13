(* Metadata only: two package loads and one literal helper load. No amplitudes. *)
HEPCATC13Isolation`Private`base=Environment["HEPCAT_C13_BASE_SOURCE"];
HEPCATC13Isolation`Private`candidate=Environment["HEPCAT_C13_SOURCE"];
HEPCATC13Isolation`Private`reference=Environment["HEPCAT_C13_REFERENCE_EXTENSION"];
HEPCATC13Isolation`Private`output=Environment["HEPCAT_C13_TEST_OUTPUT"];
Global`$HEPCATpath=HEPCATC13Isolation`Private`base;Get[Global`$HEPCATpath<>"/HEPCAT.wl"];
Begin["HEPCATC13Isolation`Private`"];$ContextPath=Prepend[DeleteCases[$ContextPath,"HEPCATC13Isolation`Private`"],"HEPCATC13Isolation`Private`"];$HistoryLength=0;
passed=0;failed=0;records={};SetAttributes[test,HoldRest];test[name_,value_]:=Module[{ok=TrueQ[value]},AppendTo[records,<|"Name"->name,"Pass"->ok|>];If[ok,passed++,failed++;Print["FAIL ",name]]];
fields[name_String]:=ToExpression[name,InputForm,Function[s,<|"OwnValues"->OwnValues[s],"DownValues"->DownValues[s],"UpValues"->UpValues[s],"SubValues"->SubValues[s],"NValues"->NValues[s],"DefaultValues"->DefaultValues[s],"FormatValues"->FormatValues[s],"Options"->Options[s],"Attributes"->Attributes[s],"Messages"->Messages[s]|>,HoldAllComplete]];
oldNames={"HEPCATMinimalAdjointCubicSources`Private`zero","HEPCATMinimalAdjointCubicSources`Private`canonicalColour","HEPCATMinimalAdjointCubicSources`Private`canonicalMetric","HEPCATMinimalAdjointCubicSources`Private`operator","HEPCATMinimalAdjointCubicSources`Private`chart","HEPCATMinimalAdjointCubicSources`Private`match","HEPCATMinimalQCDCurrentSources`Private`sourceClass","HEPCATMinimalQCDCurrentSources`Private`canonicalColour","HEPCATMinimalQCDCurrentSources`Private`catalog","HEPCATMinimalQCDCurrentSources`Private`source","HEPCATMinimalQCDCurrentSources`Private`sourceCertificate","ConstructiveDiagrams`Private`colorQCDCurrentPrepare","ConstructiveDiagrams`Private`colorQCDCurrentProjection"};
wrappers={"ConstructiveDiagrams`Private`colorQCDCurrentSourceClass","ConstructiveDiagrams`Private`colorQCDCurrentSource","ConstructiveDiagrams`Private`colorQCDCurrentSourceCertificate"};newNames={"HEPCATMinimalQCDCurrentSources`Private`twoFermionClass","HEPCATMinimalQCDCurrentSources`Private`sourceTwoFermion","HEPCATMinimalQCDCurrentSources`Private`sourceTwoFermionCertificate"};
oldFields=AssociationThread[oldNames,fields/@oldNames];oldWrapper=AssociationThread[wrappers,fields/@wrappers];
Get[reference];referenceFields=AssociationThread[newNames,fields/@newNames];referenceWrapper=AssociationThread[wrappers,fields/@wrappers];
(* Reset only new helper definitions and restore the exact original wrapper tables
   before loading the unified tree. A stale conditional rule cannot satisfy it. *)
Scan[Function[name,ToExpression[name,InputForm,Function[s,Clear[s],HoldAllComplete]]],newNames];
Scan[Function[name,With[{rules=oldWrapper[name]["DownValues"]},ToExpression[name,InputForm,Function[s,DownValues[s]=rules,HoldAllComplete]]]],wrappers];
globalSentinelNames={"tfProcess","tfModel","tfRows","tfFermions","tfBosons","tfReps","tfAdjoints","tfProposal","qTag","qFail","qRows","qRaw","qConverted","sourceTwoFermion","sourceTwoFermionCertificate","twoFermionClass"};
Scan[Function[name,ToExpression["Global`"<>name,InputForm,Function[s,OwnValues[s]={HoldPattern[s]:>"C13 namespace sentinel"},HoldAllComplete]]],globalSentinelNames];sentinelBefore=AssociationThread[globalSentinelNames,fields["Global`"<>#]&/@globalSentinelNames];
Global`$HEPCATpath=candidate;Get[candidate<>"/HEPCAT.wl"];
$ContextPath=Prepend[DeleteCases[$ContextPath,"HEPCATC13Isolation`Private`"],"HEPCATC13Isolation`Private`"];
Scan[Function[name,test["old all10fields exact "<>name,fields[name]===oldFields[name]]],oldNames];
Scan[Function[name,test["new helper all10fields literal compiled/binder identity "<>name,fields[name]===referenceFields[name]]],newNames];
Scan[Function[name,With[{before=oldWrapper[name],after=fields[name]},test["old wrapper nine fields exact "<>name,KeyDrop[before,"DownValues"]===KeyDrop[after,"DownValues"]];test["old wrapper rules literal and exactly one typed arm "<>name,Length[after["DownValues"]]===Length[before["DownValues"]]+1&&AllTrue[before["DownValues"],Function[rule,AnyTrue[after["DownValues"],Function[other,SameQ[rule,other]]]]]];test["wrapper compiled identity against tested prototype "<>name,after===referenceWrapper[name]]]],wrappers];
test["ambient Global sentinels unchanged",sentinelBefore===AssociationThread[globalSentinelNames,fields["Global`"<>#]&/@globalSentinelNames]];
test["full loader uses unified private tree",FileNameJoin[{ConstructiveDiagrams`Private`$amplitudeParallelSourceDirectory,"HEPCAT.wl"}]===FileNameJoin[{candidate,"HEPCAT.wl"}]];
result=<|"Status"->If[failed===0,"PASS","FAIL"],"Passed"->passed,"Failed"->failed,"Records"->records,"Scope"->"MetadataOnlyDefinitionBinderAndNamespaceProof","NoPhysicsCallsRequested"->True,"OldFieldTables"->oldFields,"ReferenceNewFieldTables"->referenceFields,"ActualNewFieldTables"->AssociationThread[newNames,fields/@newNames]|>;
Export[output<>"/definition-isolation.wxf",result,"WXF"];Export[output<>"/definition-isolation.json",KeyDrop[result,{"OldFieldTables","ReferenceNewFieldTables","ActualNewFieldTables"}],"RawJSON"];Print[passed," passed, ",failed," failed"];End[];Quit[If[HEPCATC13Isolation`Private`failed===0,0,1]];
