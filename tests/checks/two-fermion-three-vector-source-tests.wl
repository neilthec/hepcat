(* HEPCAT - High Energy Physics Constructive Amplitude Tool
   Copyright (C) 2026 Neil Christensen
   SPDX-License-Identifier: GPL-3.0-or-later *)
Global`$twoFThreeVTestRepository=Replace[Environment["HEPCAT_TEST_REPOSITORY"],Except[_String]->DirectoryName[DirectoryName[DirectoryName[$InputFileName]]]];
Global`$twoFThreeVTestEntry=Replace[Environment["HEPCAT_TEST_PACKAGE_ENTRYPOINT"],Except[_String]->FileNameJoin[{Global`$twoFThreeVTestRepository,"source","HEPCAT.wl"}]];
Global`$HEPCATpath=DirectoryName[Global`$twoFThreeVTestEntry];Get[Global`$twoFThreeVTestEntry];
Begin["HEPCATTwoFermionThreeVectorStructural`Private`"];
tRoot=Global`$twoFThreeVTestRepository; tDir=DirectoryName[$InputFileName];tChecks={};
tCheck[name_,ok_]:=(AppendTo[tChecks,<|"Test"->name,"Passed"->TrueQ[ok]|>];Print[If[TrueQ[ok],"PASS ","FAIL "],name]);
tModel=ReadModel[FileNameJoin[{tRoot,"models"}],1];tBefore=DownValues[ConstructiveDiagrams`CreateDiagrams];
tHits[proc_]:=Module[{hits=0,result},result=Internal`InheritedBlock[{ConstructiveDiagrams`CreateDiagrams},
 Unprotect[ConstructiveDiagrams`CreateDiagrams];Clear[ConstructiveDiagrams`CreateDiagrams];
 ConstructiveDiagrams`CreateDiagrams[___]:=(hits++;{});
 HEPCATCovariantCurrentSources`Private`covariantCurrentSource[proc,tModel]];{hits,If[FailureQ[result],result[[1]],result]}];
tCheck["massive neutral triple vector admitted",First[tHits[{"e","E","Z","Z","Z"}]]===1];
tCheck["two massive one photon admitted",First[tHits[{"e","E","Z","Z","A.+"}]]===1];
tCheck["one massive two photons admitted",First[tHits[{"e","E","Z","A.+","A.-"}]]===1];
tCheck["declared chiral neutral fermions admitted",First[tHits[{"ne","Ne","Z","Z","Z"}]]===1];
tCheck["known all massless QED class excluded",tHits[{"e","E","A.+","A.-","A.-"}] === {0,"UnsupportedExternalCurrentClass"}];
tCheck["old four fermion scalar class retained",First[tHits[{"e","E","m","M","h"}]]===1];
tCheck["old photon two scalar class retained",First[tHits[{"e","E","A.+","h","h"}]]===1];
tCheck["old four fermion massive vector class retained",First[tHits[{"e","E","m","M","Z"}]]===1];
tCheck["colored external states remain excluded",tHits[{"u","U","Z","Z","Z"}] === {0,"UnsupportedExternalCurrentClass"}];
tCheck["unknown external state remains rejected",tHits[{"unknown","E","Z","Z","Z"}]==={0,"UnknownExternalParticle"}];
tUsed=HEPCATCovariantCurrentSources`Private`rowParticles;
tVVV=SelectFirst[tModel[[2]],Length[tUsed[#]]===3&&Sort[(HEPCATCovariantCurrentSources`Private`particleRow[#,tModel][[5]]&/@tUsed[#])]==={2,2,2}&];
tCheck["unsupported triple gauge local operator still rejected",MissingQ[HEPCATCovariantCurrentSources`Private`liftVertex[tVVV,tModel]]];
tQuartic=SelectFirst[tModel[[2]],Length[tUsed[#]]===4&];
tCheck["bare quartic still rejected",FailureQ[HEPCATCovariantCurrentSources`Private`sourceCatalog[tModel,{tQuartic}]]];
tCheck["scoped stub restored actual diagram generator",DownValues[ConstructiveDiagrams`CreateDiagrams]===tBefore];
tSpectatorHits[proc_]:=Module[{hits=0,result},result=Internal`InheritedBlock[{ConstructiveDiagrams`CreateDiagrams},
 Unprotect[ConstructiveDiagrams`CreateDiagrams];Clear[ConstructiveDiagrams`CreateDiagrams];ConstructiveDiagrams`CreateDiagrams[___]:=(hits++;{});
 ConstructiveDiagrams`Private`colorSpectatorSource[proc,tModel,tModel]];{hits,If[FailureQ[result],result[[1]],result]}];
tCheck["rank-one quark three massive vectors admitted",First[tSpectatorHits[{"u","U","Z","Z","Z"}]]===1];
tCheck["rank-one quark photon and two massive vectors admitted",First[tSpectatorHits[{"u","U","A.+","Z","Z"}]]===1];
tCheck["rank-one quark one massive vector boundary admitted",First[tSpectatorHits[{"u","U","A.+","A.-","Z"}]]===1];
tCheck["all-photon quark QED excluded",tSpectatorHits[{"u","U","A.+","A.-","A.-"}]==={0,"UnsupportedSpectatorExternalClass"}];
tCheck["massless chiral endpoints stay outside spectator branch",tSpectatorHits[{"ne","Ne","Z","Z","Z"}]==={0,"UnsupportedSpectatorFermionMetadata"}];
tCheck["colorless endpoints stay outside spectator branch",tSpectatorHits[{"e","E","Z","Z","Z"}]==={0,"NotOneFundamentalPairAndSinglets"}];
tCheck["old quark photon two scalar branch retained",First[tSpectatorHits[{"u","U","A.+","h","h"}]]===1];
tCheck["old quark massive vector two scalar branch retained",First[tSpectatorHits[{"u","U","Z","h","h"}]]===1];
tCheck["old quark two massive vectors scalar branch retained",First[tSpectatorHits[{"u","U","Z","Z","h"}]]===1];
tCheck["spectator scoped stub restored actual generator",DownValues[ConstructiveDiagrams`CreateDiagrams]===tBefore];
tPrepareBefore=DownValues[ConstructiveDiagrams`Private`colorSpectatorSource];
tPrepareHits[proc_]:=Module[{hits=0,result},result=Internal`InheritedBlock[{ConstructiveDiagrams`Private`colorSpectatorSource},
 Unprotect[ConstructiveDiagrams`Private`colorSpectatorSource];Clear[ConstructiveDiagrams`Private`colorSpectatorSource];
 ConstructiveDiagrams`Private`colorSpectatorSource[___]:=(hits++;Failure["StructuralStop",<||>]);
 ConstructiveDiagrams`Private`colorSpectatorPrepare[proc,tModel,tModel,<|"FeynmanModel"->tModel,"ProgressReporting"->False|>,AbsoluteTime[]+10,{}]];{hits,Lookup[result,"Attempted",None]}];
tCheck["public scheduler reaches three massive vector spectator source",tPrepareHits[{"u","U","Z","Z","Z"}]==={1,True}];
tCheck["public scheduler reaches mixed massive photon spectator source",tPrepareHits[{"u","U","A.+","Z","Z"}]==={1,True}];
tCheck["public scheduler reaches one massive vector boundary",tPrepareHits[{"u","U","A.+","A.-","Z"}]==={1,True}];
tCheck["public scheduler excludes all photon QED",tPrepareHits[{"u","U","A.+","A.-","A.-"}]==={0,False}];
tCheck["public scheduler excludes colourless endpoints",tPrepareHits[{"e","E","Z","Z","Z"}]==={0,False}];
tCheck["public scheduler excludes chiral endpoints",tPrepareHits[{"ne","Ne","Z","Z","Z"}]==={0,False}];
tCheck["public scheduler retains old photon two scalar source",tPrepareHits[{"u","U","A.+","h","h"}]==={1,True}];
tCheck["public scheduler retains old two massive vector scalar source",tPrepareHits[{"u","U","Z","Z","h"}]==={1,True}];
tCheck["public scheduler structural source stub restored",DownValues[ConstructiveDiagrams`Private`colorSpectatorSource]===tPrepareBefore];
tInventory=Import[tRoot<>"/tests/SM-5-point/inventory.json","RawJSON"];
tHeads={ConstructiveDiagrams`CreateDiagrams,HEPCATCovariantCurrentSources`Private`objectSHA,ConstructiveDiagrams`Private`colorSpectatorSource,ConstructiveDiagrams`Private`colorSpectatorPrepare};
tBefore=DownValues/@tHeads;tGraphTag=Unique["graphPreflight"];
tProbe[process_,scheduler_]:=Internal`InheritedBlock[{ConstructiveDiagrams`CreateDiagrams,HEPCATCovariantCurrentSources`Private`objectSHA},
 Unprotect[ConstructiveDiagrams`CreateDiagrams,HEPCATCovariantCurrentSources`Private`objectSHA];
 Clear[ConstructiveDiagrams`CreateDiagrams,HEPCATCovariantCurrentSources`Private`objectSHA];
 HEPCATCovariantCurrentSources`Private`objectSHA[___]:="STRUCTURAL_CONSTANT_ONLY";
 ConstructiveDiagrams`CreateDiagrams[___]:=Throw[True,tGraphTag];
 Catch[If[TrueQ[scheduler],
  ConstructiveDiagrams`Private`colorSpectatorPrepare[process,tModel,tModel,<|"FeynmanModel"->tModel,"ProgressReporting"->False|>,AbsoluteTime[]+10,{}],
  ConstructiveDiagrams`Private`colorSpectatorSource[process,tModel,tModel]];False,tGraphTag]];
tRows=TimeConstrained[MemoryConstrained[Table[<|"ID"->c["ID"],"Particles"->c["Particles"],"SourceReachesGraph"->TrueQ[tProbe[c["Particles"],False]],"PrepareDelegatesToSourceGraph"->TrueQ[tProbe[c["Particles"],True]]|>,{c,tInventory["Cases"]}],512*1024^2,$Failed],60,$Aborted];
tValid=ListQ[tRows]&&Length[tRows]===Length[tInventory["Cases"]];
tMismatch=If[tValid,Select[tRows,#["SourceReachesGraph"]=!=#["PrepareDelegatesToSourceGraph"]&],{}];
tReachChecks={
 <|"Test"->"every inventory case was actually probed","Passed"->tValid|>,
 <|"Test"->"actual Source and Prepare graph preflights agree for all inventory","Passed"->(tValid&&tMismatch==={})|>,
 <|"Test"->"actual new all massive boundary delegated","Passed"->TrueQ[tProbe[{"u","U","Z","Z","Z"},True]]|>,
 <|"Test"->"actual new one massive boundary delegated","Passed"->TrueQ[tProbe[{"u","U","Z","A.+","A.-"},True]]|>,
 <|"Test"->"actual old quark photon scalar branch delegated","Passed"->TrueQ[tProbe[{"u","U","A.+","h","h"},True]]|>,
 <|"Test"->"actual old quark massive scalar branch delegated","Passed"->TrueQ[tProbe[{"u","U","Z","h","h"},True]]|>,
 <|"Test"->"actual all photon QED branch excluded","Passed"->(!TrueQ[tProbe[{"u","U","A.+","A.-","A.-"},False]]&&!TrueQ[tProbe[{"u","U","A.+","A.-","A.-"},True]])|>,
 <|"Test"->"all actual and stubbed function definitions restored","Passed"->(DownValues/@tHeads===tBefore)|>};
tChecks=Join[tChecks,tReachChecks];Print["Actual inventory source/prepare preflight cases: ",If[tValid,Length[tRows],0],"; mismatches: ",Length[tMismatch]];
tRecord=<|"Status"->If[AllTrue[tChecks,TrueQ[#["Passed"]]&],"PASS","FAIL"],"Checks"->tChecks,"Passed"->Count[Lookup[tChecks,"Passed"],True],"Failed"->Count[Lookup[tChecks,"Passed"],False],"OnlyStructuralStubs"->True,"PhysicsJobsExecuted"->0,"ProductionSourceEdited"->False|>;

Print["Two-fermion three-vector source tests: ",tRecord["Passed"]," checks, ",tRecord["Failed"]," failures."];Print[If[tRecord["Status"]==="PASS","ALL TWO FERMION THREE VECTOR SOURCE TESTS PASSED","TWO FERMION THREE VECTOR SOURCE TESTS FAILED"]];If[tRecord["Status"]==="PASS",Print["HEPCAT_TEST_COMPLETED:two-fermion-three-vector-source-tests.wl"]];Global`$twoFThreeVTestExitCode=If[tRecord["Status"]==="PASS",0,1];End[];Quit[Global`$twoFThreeVTestExitCode];
