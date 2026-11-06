(* A fully solved automatic-timeout point is validated in a separate stage.
   Its failed child status, exact fit assignments and independent seeds remain. *)
root=DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];Get[FileNameJoin[{root,"source","HEPCAT.wl"}]];
checks=0;failures={};SetAttributes[check,HoldRest];check[name_,value_]:=(checks++;If[!TrueQ[value],AppendTo[failures,name];Print["FAIL: ",name]]);
m=ReadModel[FileNameJoin[{root,"models"}],2];
{u,v}=Table[Unique["recoveryFree"],{2}];
assignments=Table[ConstructiveDiagrams`Private`residueParameterRules[m,{"Me"->i,"EE"->1}],{i,1,3}];
parameterReports=(<|"ParameterRules"->#,"Report"-><|"Status"->"COMPLETE"|>|>&/@assignments);
groups=AssociationThread[{"Physical","HighEnergy","Spurious","Reference"},ConstantArray[<|"Status"->"COMPLETE","Equations"->{},"ParameterAssignments"->parameterReports|>,4]];
certificates=(<|"Parameter"->#,"Status"->"PROVED_ZERO","Method"->"DenseSymbolicExternalChart","InternalOnShellConditions"->{}|>&/@{u,v});
h=<|"Stage"->"HighEnergy","Status"->"UNRESOLVED","Amplitude"->0,"FamilyAmplitude"->0,
 "ExternalMassRules"->{Mass[1]->0,Mass[2]->0,Mass[3]->0,Mass[4]->0},
 "FreeParameters"->{},"RemainingBasis"->{},"RandomSeed"->98731,"ExpectedPower"->0,
 "ConstraintData"->groups,"LinearSystem"-><|"Status"->"UNDERDETERMINED","FreeParameters"->{u,v}|>,
 "SolvedFamilyIdentityReduction"-><|"AffineFamily"->True,"FreeParameters"->{},"RemovedParameters"->{u,v},"Certificates"->certificates,
  "FamilyRepresentationCertificate"-><|"OnlyCertifiedZeroDirectionsRemoved"->True|>|>,
 "ResourcePolicy"-><|"Requested"-><|"TimeConstraint"->Automatic|>,"Resolved"-><|"TimeConstraint"->100,"ValidationSamples"->4|>|>,
 "Diagnostics"->{"High-energy completion exceeded TimeConstraint. The best retained candidate is provided for inspection, not claimed complete."}|>;
admit[report_]:=ConstructiveDiagrams`Private`constructRetainedHighEnergyCheckpoint[report];
rejected[report_,reason_]:=MatchQ[admit[report],Failure["RetainedHECheckpointNotAdmitted",a_Association]/;Lookup[a,"Reason",None]===reason];
a=admit[h];
check["fully classified automatic-timeout point is eligible",AssociationQ[a]&&a["Status"]==="ELIGIBLE"];
check["original assignments remain distinct instead of freezing parameter variations",a["ValidationParameterAssignments"]===assignments&&Length[a["ValidationParameterAssignments"]]===3];
check["independent seeds match the normal high-energy stage",a["ValidationSeeds"]==={203460,308189,412918}&&a["ValidationSamples"]===4];
check["admission does not change the unresolved child status",h["Status"]==="UNRESOLVED"&&a["OriginalHighEnergyStatus"]==="UNRESOLVED"&&a["Certificate"]["NoChildCompletionClaimed"]];
check["explicit high-energy limits never receive recovery",rejected[Join[h,<|"ResourcePolicy"-><|"Requested"-><|"TimeConstraint"->100|>|>|>],"ExplicitHighEnergyTimeConstraint"]];
check["an incomplete fit is not a validation-only checkpoint",rejected[Join[h,<|"ConstraintData"->Join[groups,<|"Reference"-><|"Status"->"UNRESOLVED"|>|>]|>],"MissingCompletedConstraintGroup"]];
check["a non-timeout unresolved result is not recovered",rejected[Join[h,<|"Diagnostics"->{"No supported correction space"}|>],"NotRetainedStageTimeout"]];
check["remaining freedom stays unresolved",rejected[Join[h,<|"FreeParameters"->{u}|>],"NotFullyClassifiedFinitePoint"]];
check["a stale nonempty remaining basis blocks admission",rejected[Join[h,<|"RemainingBasis"->{1}|>],"NotFullyClassifiedFinitePoint"]];
check["a different retained family blocks admission",rejected[Join[h,<|"FamilyAmplitude"->1|>],"NotFullyClassifiedFinitePoint"]];
check["an inconsistent exact solve cannot be recovered",rejected[Join[h,<|"LinearSystem"-><|"Status"->"INCONSISTENT","FreeParameters"->{}|>|>],"NoSolvedExactLinearSystem"]];
check["an unfinished identity proof cannot remove a coordinate",rejected[Join[h,<|"SolvedFamilyIdentityReduction"->Join[h["SolvedFamilyIdentityReduction"],<|"Certificates"->{First[certificates]}|>]|>],"UnclassifiedOrUnprovedRemainingCoordinate"]];
check["an internal-shell certificate is insufficient for the final family",rejected[Join[h,<|"SolvedFamilyIdentityReduction"->Join[h["SolvedFamilyIdentityReduction"],<|"Certificates"->(Join[#,<|"InternalOnShellConditions"->{MomProd[1,2]->0}|>]&/@certificates)|>]|>],"UnclassifiedOrUnprovedRemainingCoordinate"]];
check["known failed held-out checks are never retried as accepted candidates",rejected[Join[h,<|"AfterReport"-><|"Checks"->{<|"Status"->"FAIL"|>}|>|>],"KnownFailedIndependentValidation"]];
check["known opposite-sign held-out comparisons block recovery",rejected[Join[h,<|"ValidationReports"->{<|"Report"-><|"Checks"->{<|"Status"->"Opp Sign"|>}|>|>}|>],"KnownFailedIndependentValidation"]];
changedGroups=Join[groups,<|"Spurious"->Join[groups["Spurious"],<|"ParameterAssignments"->Take[parameterReports,2]|>]|>];
check["every fit group must retain the same complete assignments",rejected[Join[h,<|"ConstraintData"->changedGroups|>],"DifferentConstraintParameterAssignments"]];
passing=<|"Checks"->{<|"Type"->"HighEnergy","Status"->"PASS"|>,<|"Type"->"Reference","Status"->"PASS"|>},"Channels"->{},"SpuriousChecks"->{},"AutomaticPoleDiscovery"->True,"HighEnergyCheck"-><|"Status"->"PASS"|>,"ReferenceCheck"-><|"Status"->"PASS"|>|>;
runMock[failAt_:Infinity,seconds_:2]:=Internal`InheritedBlock[{ConstructiveDiagrams`Private`completionHECheck},
 Clear[ConstructiveDiagrams`Private`completionHECheck];validationCalls={};
 ConstructiveDiagrams`Private`completionHECheck[expression_,normalized_,channels_,model_,settings_,seed_]:=(AppendTo[validationCalls,<|"Expression"->expression,"Settings"->settings,"Seed"->seed|>];If[Length[validationCalls]===failAt,Join[passing,<|"Checks"->{<|"Status"->"FAIL"|>}|>],passing]);
 ConstructiveDiagrams`Private`constructValidateRetainedHighEnergy[h,{},m,seconds,False]];
all=runMock[];
check["every original assignment must validate before selection",all["Status"]==="COMPLETE"&&Length[all["ValidationReports"]]===3&&Lookup[all["ValidationReports"],"ParameterRules"]===assignments];
check["recorded validation seeds are passed to every exact checker",Lookup[validationCalls,"Seed"]===a["ValidationSeeds"]];
check["normal held-out sample counts and conditions are unchanged",AllTrue[validationCalls,#["Settings"]["ValidationSamples"]===4&&#["Settings"]["HighEnergyBound"]===0&]];
check["separate validation stage does not relabel the child",all["Stage"]==="Validating retained HE candidate"&&all["OriginalHighEnergyStatus"]==="UNRESOLVED"&&h["Status"]==="UNRESOLVED"];
secondFail=runMock[2];
check["one failed assignment prevents a complete result",secondFail["Status"]==="UNRESOLVED"&&Length[secondFail["ValidationReports"]]===2];
check["unvisited assignments are never claimed as completed",secondFail["RequestedParameterAssignments"]===3&&secondFail["CompletedParameterAssignments"]===2];
empty=runMock[Infinity,0];
check["an exhausted parent budget cannot select a candidate",empty["Status"]==="UNRESOLVED"&&empty["ValidationReports"]==={}];
check["recovery budget explicitly belongs to the parent",all["TimeBudgetScope"]==="RemainingOverallConstructionBudgetNotExpiredChildStage"];
unique=admit[Join[h,<|"LinearSystem"-><|"Status"->"COMPLETE","FreeParameters"->{}|>,"SolvedFamilyIdentityReduction"->Missing["NoPruneNeeded"]|>]];
check["a directly unique exact solution needs no artificial identity proof",AssociationQ[unique]&&unique["Status"]==="ELIGIBLE"];
Print["CHECKS: ",checks,"; FAILURES: ",Length[failures]];If[failures=!={},Print[failures];Quit[1]];
Print["HEPCAT_TEST_COMPLETED:retained-high-energy-validation-tests.wl"];Quit[0];
