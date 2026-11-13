(* Portable source-only candidate builder. No saved amplitude or reference model is used. *)
root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Get[FileNameJoin[{root, "source", "HEPCAT.wl"}]];
(* Preexisting ambient definitions must survive loading the private prototype. *)
Global`reject["sentinel"] := "old reject";
Global`particleRow["sentinel"] := "old particle row";
Global`particleRow = "ambient own value";
Global`rowParticles = "ambient row value";
Global`objectSHA["sentinel"] := "old hash";
ccPublicBefore = {OwnValues[ConstructiveDiagrams`rowParticles], DownValues[ConstructiveDiagrams`rowParticles]};
ccAmbientBefore = {OwnValues[Global`particleRow], DownValues[Global`particleRow],
 OwnValues[Global`rowParticles], DownValues[Global`reject], DownValues[Global`objectSHA]};
Get[FileNameJoin[{root, "tests", "SM-5-point", "covariant-current-sources.wl"}]];
checks = 0; failures = {}; $HistoryLength = 0;
SetAttributes[check, HoldRest];
check[name_, condition_] := (checks++; If[!TrueQ[condition], AppendTo[failures, name]; Print["FAIL: ", name]]);
check["loading private implementation heads preserves preexisting global own and down values",
 ccAmbientBefore === {OwnValues[Global`particleRow], DownValues[Global`particleRow],
  OwnValues[Global`rowParticles], DownValues[Global`reject], DownValues[Global`objectSHA]} &&
 Global`reject["sentinel"] === "old reject" && Global`objectSHA["sentinel"] === "old hash"];
check["loading the standalone helper never replaces an existing package implementation",
 ccPublicBefore === {OwnValues[ConstructiveDiagrams`rowParticles], DownValues[ConstructiveDiagrams`rowParticles]}];
ccBuild = HEPCATCovariantCurrentSources`Private`covariantCurrentSource;
ccCatalog = HEPCATCovariantCurrentSources`Private`sourceCatalog;
ccSHA = HEPCATCovariantCurrentSources`Private`objectSHA;
ccParameter[name_, scope_] := ModelParameter[name, scope, "Real"];
ccToy[scope_] := Module[{mf, mk, ms, sf, pf, sk, pk, cubic, parameters, angle, square},
 parameters = ccParameter[#, scope] & /@ {"mf", "mk", "ms", "sf", "pf", "sk", "pk", "cubic"};
 {mf,mk,ms,sf,pf,sk,pk,cubic} = parameters;
 angle = SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]];
 square = SpinorChain[Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]];
 {{{"first", "f", "F", 1, 1, mf, 0, 1, "", "f", "F"},
   {"second", "k", "K", 2, 1, mk, 0, 1, "", "k", "K"},
   {"scalar", "s", "s", 3, 0, ms, 0, 1, "", "s", "s"}},
  {{"f", "F", "s", "", 1, sf (angle + square) + pf (square - angle)},
   {"k", "K", "s", "", 1, sk (angle + square) + pk (square - angle)},
   {"s", "s", "s", "", cubic, 1}}, Thread[parameters -> {1,2,3,1/3,1/5,2/7,3/11,1/13}] /. Rule -> List, {}}
];
ccModel = ccToy["portable-first"]; ccBefore = ccModel;
ccProcess = {"f", "F", "k", "K", "s"};
ccSource = MemoryConstrained[TimeConstrained[ccBuild[ccProcess, ccModel], 35, $TimedOut],2*2^30,$Aborted];
check["a minimal scalar and pseudoscalar toy yields only an unvalidated candidate",
 AssociationQ[ccSource] && ccSource["Status"] === "UNVALIDATED_CANDIDATE" && !ccSource["NativeValidationPerformed"] && ccSource["RequiresCheckAmplitudes"]];
check["every original graph ordered port pole and oriented fermion line is covered",
 AssociationQ[ccSource] && ccSource["EveryOriginalDiagramCovered"] && ccSource["EveryUsedOrderedVertexCertified"] &&
 Sort[ccSource["OrderedGraphCertificates"]] === Sort[ccSource["LiftedGraphCertificates"]] && ccSource["OriginalDiagramCount"] > 0];
check["scalar and pseudoscalar couplings are both retained symbolically",
 !FreeQ[ccSource["Expression"],ccParameter["sf","portable-first"]] && !FreeQ[ccSource["Expression"],ccParameter["pf","portable-first"]] &&
 !FreeQ[ccSource["Expression"],ccParameter["pk","portable-first"]]];
check["the output has only external mass replacements and no on-shell internal assumptions",
 First /@ ccSource["ExternalMassRules"] === Mass /@ Range[5] && ccSource["InternalOnShellConditions"] === {} &&
 ccSource["CompleteOffShellConversionCoverage"] && !FreeQ[ccSource["Expression"],_PropDen]];
check["no numeric fitting independent benchmark or contact was introduced",
 !ccSource["NumericalCoefficientFit"] && !ccSource["IndependentBenchmarkRead"] && !ccSource["NewIndependentContacts"] &&
 ccSource["NumericalParameterAssignments"] === {} && FreeQ[ccSource["Expression"],_Real|_DiracChain|_DiracWord]];
check["every local operator has an exact symbolic certificate",
 AllTrue[ccSource["VertexCertificates"], #["Status"] === "EXACT_SYMBOLIC_MATCH" && #["ExactDifference"] === 0 &]];
ccNames = {"f"->"rho", "F"->"Rho", "k"->"sigma", "K"->"Sigma", "s"->"phi"};
ccParameters = Cases[ccModel[[3]], {p_ModelParameter, _} :> p];
ccRenames = (# -> ModelParameter["renamed" <> ModelParameterName[#], "renamed-scope", "Real"] &) /@ ccParameters;
ccRenamedModel = ccModel /. Join[ccNames,ccRenames];
ccRenamed = ccBuild[ccProcess /. ccNames, ccRenamedModel];
check["renaming every particle and coupling preserves the exact candidate",
 AssociationQ[ccRenamed] && Expand[ccRenamed["Expression"] - (ccSource["Expression"] /. ccRenames)] === 0 &&
 ccRenamed["ExternalMassRules"] === (ccSource["ExternalMassRules"] /. ccRenames)];
ccOther = ccToy["portable-second"]; ccOtherSource = ccBuild[ccProcess,ccOther];
check["two models with identical printed parameter names remain independent",
 AssociationQ[ccOtherSource] && Complement[DeleteDuplicates[Cases[ccOtherSource["Expression"],_ModelParameter,Infinity]],First/@ccOther[[3]]] === {} &&
 Intersection[DeleteDuplicates[Cases[ccOtherSource["Expression"],_ModelParameter,Infinity]],First/@ccModel[[3]]] === {}];
ccForeign = ccModel; ccForeign[[2,1,-1]] *= ccParameter["sf","portable-second"];
check["a foreign parameter cannot enter a source-current certificate", FailureQ[ccBuild[ccProcess,ccForeign]]];
ccDerivative = ccModel; ccDerivative[[2,1,-1]] *= MomProd[1,2];
check["a higher-derivative used vertex is rejected rather than minimally continued", FailureQ[ccBuild[ccProcess,ccDerivative]]];
check["all momentum-like scalar prefactors are rejected by positive model-scalar ownership",
 AllTrue[{Mandelstahm[1,2],MomHat[1],MomQ[2],Mass[1],PropDen[Mom[Multiparticle[1,2]],1]},
  Function[prefactor,With[{bad = ReplacePart[ccModel,{2,1,-1}->prefactor ccModel[[2,1,-1]]]},
   FailureQ[ccBuild[ccProcess,bad]]]]]];
ccBare = ccModel; ccBare[[2,1,-1]] *= ccUndeclaredScalar;
check["an undeclared scalar symbol is not accepted as a model-owned coupling", FailureQ[ccBuild[ccProcess,ccBare]]];
check["the positive scalar grammar admits model functions exact complex phases and scalar arithmetic",
 HEPCATCovariantCurrentSources`Private`modelScalarQ[
  (1+I)Sqrt[2]Cos[ccParameter["sf","portable-first"]]/ccParameter["mf","portable-first"],ccModel]];
ccContact = ccModel; AppendTo[ccContact[[2]],{"f","F","s","s",1,ccModel[[2,1,-1]]}];
check["a used quartic contact is rejected before any candidate is returned", FailureQ[ccBuild[ccProcess,ccContact]]];
check["an unsupported five-port source row is rejected by the exact catalog",
 FailureQ[ccCatalog[ccModel,{{"f","F","k","K","s",1,1}}]]];
check["unsupported external arity and colored or chiral external classes are rejected",
 FailureQ[ccBuild[Drop[ccProcess,-1],ccModel]] &&
 FailureQ[ccBuild[ccProcess,ReplacePart[ccModel,{1,1,8}->3]]] &&
 FailureQ[ccBuild[ccProcess,ReplacePart[ccModel,{1,1,9}->"L"]]]];
ccBad = ccModel; AppendTo[ccBad[[1]],{"unused","q","Q",5,4,0,0,1,"","q","Q"}];
AppendTo[ccBad[[2]],{"q","Q","s","",1,MomProd[1,2]^3}];
ccUnused = ccBuild[ccProcess,ccBad];
check["unreachable unsupported species are not silently admitted or allowed to poison the used catalog",
 AssociationQ[ccUnused] && ccUnused["Expression"] === ccSource["Expression"] &&
 ccUnused["SourceUsedRows"] === ccSource["SourceUsedRows"]];
check["the helper leaves all source model values unchanged and records their exact object hash",
 ccModel === ccBefore && ccSource["SourceModelSHA256"] === ccSHA[ccModel]];
(* Real-model coverage is built fresh and never imports a saved formula or a reference model. *)
ccElementary = ReadModel[FileNameJoin[{root,"models"}],1];
ccDistinct = ccBuild[{"e","E","m","M","h"},ccElementary];
ccIdentical = ccBuild[{"e","E","e","E","h"},ccElementary];
check["fresh distinct and identical Dirac processes preserve every diagram and endpoint pairing",
 AssociationQ[ccDistinct] && AssociationQ[ccIdentical] &&
 ccDistinct["OriginalDiagramCount"] === 14 && ccIdentical["OriginalDiagramCount"] === 28 &&
 Sort[ccIdentical["OrderedGraphCertificates"]] === Sort[ccIdentical["LiftedGraphCertificates"]]];
check["the fresh supported model exercises all exact current classes including paired massless helicities",
 Sort[DeleteDuplicates[Lookup[ccDistinct["VertexCertificates"],"Class"]]] ===
 Sort[{"FermionFermionScalar","FermionFermionMassiveVector","FermionFermionMasslessVector","VectorVectorScalar","ScalarCubic"}]];
ccToyDiagrams = CreateDiagrams[ccProcess,ccModel];
ccToyCatalog = ccCatalog[ccModel,HEPCATCovariantCurrentSources`Private`usedRows[ccToyDiagrams]];
ccCorruptCatalog = ccToyCatalog; ccCorruptCertificates = ccToyCatalog["Certificates"];
ccCorruptCertificates[[1]] = Append[First[ccCorruptCertificates],"SourceRows"->{}];
ccCorruptCatalog["Certificates"] = ccCorruptCertificates;
check["ordered-port certification rejects a missing original vertex witness",
 AnyTrue[HEPCATCovariantCurrentSources`Private`diagramCertificate[#,ccCorruptCatalog,ccModel,False]& /@ ccToyDiagrams,FailureQ]];
check["source expressions remain entirely scoped to the supplied constructive model",
 Complement[DeleteDuplicates[Cases[ccDistinct["Expression"],_ModelParameter,Infinity]],
  First/@Join[ccElementary[[3]],ccElementary[[4]]]] === {}];
If[checks =!= 24, AppendTo[failures,"Not every covariant source assertion executed"]];
Print[ToString[checks-Length[failures]]<>"/"<>ToString[checks]<>" covariant-current-source checks passed"];
If[failures =!= {}, Print[failures]];
If[failures === {}, Print["HEPCAT_TEST_COMPLETED:covariant-current-source-tests.wl"]];
Quit[If[failures==={},0,1]];
