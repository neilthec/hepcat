(* Portable QED worked references. No fitted answers or temporary caches are read. *)
root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Global`$HEPCATpath = FileNameJoin[{root, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
Get[FileNameJoin[{root, "tests", "QED-5-point-residues.wl"}]];
$HistoryLength = 0; checks = 0; failures = {};
SetAttributes[check, HoldRest];
check[name_, value_] := (checks++; If[!TrueQ[value],
  AppendTo[failures, name]; Print["FAIL: ", name]]);
qwModel = ReadModel[FileNameJoin[{root, "models"}], 3];
qwModelHash = Hash[qwModel, "SHA256"];
qwMass = ModelParameter[qwModel, "Me"]; qwCharge = ModelParameter[qwModel, "EE"];
qwBindings = DeleteDuplicates[Cases[qwModel, _ModelParameter, Infinity]];
qwScopedBindings = (# -> ModelParameter[ModelParameterName[#], "QEDWorkedExampleTest", #[[3]]] &) /@ qwBindings;
qwScopedModel = qwModel /. qwScopedBindings;
qwScopedHash = Hash[qwScopedModel, "SHA256"];
qwHelpers = {QEDSixPointPPPMCompactExample, QEDSixPointPPMMPhysicalCurrentExample};
qwProcesses = {{"e", "E", "A.+", "A.+", "A.+", "A.-"},
  {"e", "E", "A.+", "A.+", "A.-", "A.-"}};
qwNames = {"three-positive", "balanced"};
qwExpectedRules = Thread[Mass /@ Range[6] -> {qwMass, qwMass, 0, 0, 0, 0}];
qwPhysicalCatalog = Sort[Join[
  PropDen[Mom[Multiparticle[1, #]], qwMass] & /@ Range[3, 6],
  PropDen[Mom[Multiparticle[2, #]], qwMass] & /@ Range[3, 6],
  PropDen[Mom[Multiparticle @@ Prepend[#, 1]], qwMass] & /@ Subsets[Range[3, 6], {2}]]];
check["the public checker retains four default samples", ("Samples" /. Options[CheckAmplitudes]) === 4];
qwReports = {};
Do[
  qwPair = qwHelpers[[i]][qwModel]; qwPairHash = Hash[qwPair, "SHA256"];
  check[qwNames[[i]] <> " materializes as an external-mass pair",
    MatchQ[qwPair, {_List, _}] && First[qwPair] === qwExpectedRules &&
      FreeQ[qwPair, _Failure | _Missing | Mass[_Multiparticle]]];
  check[qwNames[[i]] <> " uses exactly the supplied mass and coupling references",
    Sort[DeleteDuplicates[Cases[qwPair, _ModelParameter, Infinity]]] === Sort[{qwMass, qwCharge}]];
  check[qwNames[[i]] <> " creates no unresolved local or global symbols",
    Cases[Last[qwPair], symbol_Symbol /; Context[symbol] === "Global`", {0, Infinity}] === {}];
  qwScopedPair = qwHelpers[[i]][qwScopedModel];
  check[qwNames[[i]] <> " materialization follows a new model scope exactly",
    qwScopedPair === (qwPair /. qwScopedBindings)];
  check[qwNames[[i]] <> " ignores unrelated global mass and charge values",
    Block[{Global`Me = "unrelated mass", Global`EE = "unrelated coupling"},
      qwHelpers[[i]][qwModel]] === qwPair];
  check[qwNames[[i]] <> " rejects missing or unsupported models",
    FailureQ[qwHelpers[[i]][qwUnsupportedModel]] &&
      FailureQ[qwHelpers[[i]][{{}, {}, {}, {}}]]];
  qwPoles = Sort[DeleteDuplicates[Cases[Last[qwPair], _PropDen, Infinity]]];
  check[qwNames[[i]] <> " has exactly the fourteen physical denominator factors",
    qwPoles === qwPhysicalCatalog];
  check[qwNames[[i]] <> " contains no Xi, x factors or inverse spinor poles",
    FreeQ[Last[qwPair], Spinor["Xi", ___] | _xFactor | _xTildeFactor] &&
      AllTrue[Cases[Last[qwPair], Power[base_, exponent_Integer?Negative] :> base, Infinity],
        FreeQ[#, _SpinorChain | _SpinorTrace] &]];
  qwChannels = FactorizationAmplitudeChannels[qwProcesses[[i]], qwModel];
  qwReport = CheckAmplitudes[qwPair, qwChannels, qwModel,
    "Output" -> "Details", "Parallelization" -> False];
  AppendTo[qwReports, qwReport];
  check[qwNames[[i]] <> " passes every default amplitude check",
    AssociationQ[qwReport] && qwReport["Summary"] === <|"PASS" -> 16|> &&
      Length[qwReport["Checks"]] === 16 && AllTrue[qwReport["Checks"], #["Status"] === "PASS" &]];
  check[qwNames[[i]] <> " checks all fourteen channels and high-energy/reference behavior",
    Count[Lookup[qwReport["Checks"], "Type"], "Physical"] === 14 &&
      MemberQ[Lookup[qwReport["Checks"], "Type"], "HighEnergy"] &&
      MemberQ[Lookup[qwReport["Checks"], "Type"], "Reference"]];
  check[qwNames[[i]] <> " leaves the expression and model unchanged",
    Hash[qwPair, "SHA256"] === qwPairHash && Hash[qwModel, "SHA256"] === qwModelHash &&
      Hash[qwScopedModel, "SHA256"] === qwScopedHash],
  {i, Length[qwHelpers]}];
qwIdentityLists = {QEDSixPointPPPMIdentities[qwModel], QEDSixPointPPMMPhysicalCurrentIdentities[qwModel]};
Do[
  check[qwNames[[i]] <> " retains six inspectable inactive equalities",
    Length[qwIdentityLists[[i]]] === 6 &&
      AllTrue[qwIdentityLists[[i]], Head[#] === Inactive[Equal] && Length[#] === 2 &]];
  Do[qwIdentity = qwIdentityLists[[i, j]];
    qwProof = ConstructiveDiagrams`Private`completionHEZeroDirection[
      qwIdentity[[1]] - qwIdentity[[2]], qwExpectedRules, qwModel, 5];
    check[qwNames[[i]] <> " ordinary displayed identity " <> ToString[j],
      Lookup[qwProof, "Status", None] === "PROVED_ZERO"], {j, 6}],
  {i, 2}];
check["the physical reference keeps weighted complexity bounded",
  SpinorExpressionComplexity[Last[QEDSixPointPPMMPhysicalCurrentExample[qwModel]]] <= 3224];
check["the seven-group reference keeps weighted complexity bounded",
  SpinorExpressionComplexity[Last[QEDSixPointPPPMCompactExample[qwModel]]] <= 327];
If[checks =!= 39, AppendTo[failures, "Not every worked-example assertion executed."]];
Print["QED WORKED EXAMPLE CHECKS: ", checks, "; FAILURES: ", Length[failures]];
If[failures =!= {}, Print[failures]];
If[failures === {}, Print["HEPCAT_TEST_COMPLETED:qed-worked-example-tests.wl"]];
Quit[If[failures === {}, 0, 1]];
