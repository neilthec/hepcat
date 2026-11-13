(* Compact ordinary errors, exact detailed reports, no physics-result changes. *)
root=DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Get[FileNameJoin[{root,"source","HEPCAT.wl"}]];
checks=0;failures={};SetAttributes[check,HoldRest];
check[name_,x_]:=(checks++;If[!TrueQ[x],AppendTo[failures,name];Print["FAIL: ",name]]);
fHuge=Table[Range[300],{100}];
fCause=Failure["AmplitudeConstructionFailed",<|"MessageTemplate"->"The exact stage remained unresolved.",
 "Stage"->"Completing high-energy behavior","Status"->"FAILED","Diagnostics"->{"A held-out direction remained unresolved."}|>];
fReport=<|"Status"->"FAILED","Failure"->fCause,"Stages"-><|"Exact fit"->fHuge|>,"CandidateAmplitude"->Total[Range[300]]|>;
fOriginal=Failure["IncompleteColorAmplitude",<|"MessageTemplate"->"An independent color coefficient did not complete.",
 "ColorCoefficient"->2,"ColorCoefficients"->6,"Report"->fReport,"ColorCoefficientReports"->{fReport},
 "ColorOrbitAttempt"-><|"OriginalSeedReports"->fHuge|>|>];
fBefore=fOriginal;fSummary=ConstructiveDiagrams`Private`constructFailureSummary[fOriginal];
check["the original failure tag and actual failed status remain visible",FailureQ[fSummary]&&First[fSummary]===First[fOriginal]&&fSummary[[2,"Status"]]==="FAILED"];
check["the coefficient and actual failed stage survive without the fit payload",fSummary[[2,"ColorCoefficient"]]===2&&fSummary[[2,"ColorCoefficients"]]===6&&fSummary[[2,"Stage"]]==="Completing high-energy behavior"];
check["the concise diagnostic comes from the original failure",fSummary[[2,"Diagnostics"]]==={"A held-out direction remained unresolved."}];
check["ordinary formatting cannot embed the original rich reports",!KeyExistsQ[fSummary[[2]],"Report"]&&!KeyExistsQ[fSummary[[2]],"ColorCoefficientReports"]&&ByteCount[fSummary]<6000&&ByteCount[fOriginal]>100000];
check["the exact original object remains unchanged",SameQ[fOriginal,fBefore]];
check["the full-report request is explained in ordinary output",StringContainsQ[fSummary[[2,"Details"]],"Output"]&&StringContainsQ[fSummary[[2,"Details"]],"Details"]];
fLong=Failure["Test",<|"MessageTemplate"->"Timeout","Report"-><|"Failure"->Failure["stage",<|"Stage"->"checking", "Diagnostics"->ConstantArray[StringRepeat["x",2000],10]|>]|>|>];
fShort=ConstructiveDiagrams`Private`constructFailureSummary[fLong];
check["a repeated oversized diagnostic has a bounded honest preview",Length[fShort[[2,"Diagnostics"]]]===1&&StringLength[First[fShort[[2,"Diagnostics"]]]]===400];
fModel=ReadModel[FileNameJoin[{root,"models"}],1];fLower=ReadModel[FileNameJoin[{root,"models"}],2];
fOld=DownValues[ConstructiveDiagrams`Private`colorConstructAmplitude];
fCall[output_]:=Internal`InheritedBlock[{ConstructiveDiagrams`Private`colorConstructAmplitude},
 Clear[ConstructiveDiagrams`Private`colorConstructAmplitude];
 ConstructiveDiagrams`Private`colorConstructAmplitude[___]:=fOriginal;
 ConstructAmplitude[{"u","U","G.+","G.-","h"},fModel,fLower,"Output"->output,
 "ProgressReporting"->False,"PrintReport"->False,"RandomSeed"->33107]];
fDefault=fCall["Expression"];fPair=fCall["Amplitude"];fDetails=fCall["Details"];
check["the public expression output gives the small failure",SameQ[fDefault,fSummary]];
check["the public amplitude-pair output gives the same small failure",SameQ[fPair,fSummary]];
check["only explicit detailed output returns the exact full partial report",SameQ[fDetails,fOriginal]&&KeyExistsQ[fDetails[[2]],"ColorCoefficientReports"]];
check["controlled public-dispatch tests restore the actual color constructor",SameQ[DownValues[ConstructiveDiagrams`Private`colorConstructAmplitude],fOld]];
Print["CONSTRUCTION FAILURE DISPLAY CHECKS: ",checks,"; FAILURES: ",Length[failures]];
If[failures=!={},Print[failures]];
If[failures==={},Print["HEPCAT_TEST_COMPLETED:construction-failure-display-tests.wl"]];
Quit[If[failures==={},0,1]];
