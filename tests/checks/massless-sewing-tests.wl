(* Internal massless spinors are continued only after a neutral-weight proof. *)
root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Get[FileNameJoin[{root, "source", "HEPCAT.wl"}]];
$HistoryLength = 0; checks = 0; failures = {};
SetAttributes[check, HoldRest];
check[name_, assertion_] := (checks++; If[!TrueQ[assertion], AppendTo[failures, name]; Print["FAIL: ", name]]);
q = Multiparticle[7]; sp[kind_, leg_] := Spinor["Helicity", kind, leg];
jmat = {{0, -1}, {1, 0}}; lmat = -jmat;
adj[p_] := {{p[[2, 2]], -p[[1, 2]]}, {-p[[2, 1]], p[[1, 1]]}};
lambda = {qa, qb}; square = {qc, qd};
angles = <|1 -> {a1, a2}, 2 -> {b1, b2}|>;
squares = <|1 -> {c1, c2}, 2 -> {d1, d2}|>;
referenceAngle = {r1, r2}; referenceSquare = {s1, s2};
matrices = <|7 -> Outer[Times, lambda, square], 3 -> {{1+I, 2}, {3-I, 4+2 I}},
  5 -> {{2-I, -3+I}, {4, 1+I}}|>;
vector[s_Spinor] := Which[s[[1]] === "Xi", If[s[[2]] === "Angle", referenceAngle, referenceSquare],
  s[[3]] === q, If[s[[2]] === "Angle", lambda, square],
  s[[2]] === "Angle", angles[s[[3]]], True, squares[s[[3]]]];
matrix[Mom[mp_Multiparticle]] := Total[matrices /@ List @@ mp];
matrix[Mom[leg_Integer]] := matrices[leg];
evaluate[chain_SpinorChain] := Module[{items = List @@ chain, kind, row, p},
  kind = First[items][[2]];
  row = If[kind === "Angle", vector[First[items]].jmat, vector[First[items]]];
  Do[p = matrix[insertion]; row = row.If[kind === "Angle", p, adj[p]];
    kind = If[kind === "Angle", "Square", "Angle"], {insertion, items[[2 ;; -2]]}];
  row.If[Last[items][[2]] === "Square", lmat.vector[Last[items]], vector[Last[items]]]];
value[expression_] := Cancel[Together[expression /. chain_SpinorChain :> evaluate[chain]]];
continue[expression_] := ConstructiveDiagrams`Private`factorizationReferenceContinuation[expression, q, 991];
endpoint[kind_, side_, length_, leg_] := Module[{other, momenta = Take[{Mom[3], Mom[5]}, length]},
  other = If[EvenQ[length], kind, If[kind === "Angle", "Square", "Angle"]];
  Apply[SpinorChain, If[side === "Left", Join[{sp[kind, q]}, momenta, {sp[other, leg]}],
    Join[{sp[other, leg]}, momenta, {sp[kind, q]}]]]];
Do[expression = (endpoint["Angle", left, 0, 1] endpoint["Square", right, 0, 2])^power;
  check["symbolic null momentum: " <> ToString[{left,right,power}],
    value[expression - continue[expression]] === 0],
  {left, {"Left", "Right"}}, {right, {"Left", "Right"}}, {power, {-2, -1, 1, 2}}];
lambda = {2+I, 3-2 I}; square = {1-I, 4+I};
angles = <|1 -> {1+I, 2-I}, 2 -> {3, -1+2 I}|>;
squares = <|1 -> {2-I, 1+2 I}, 2 -> {-1+I, 3-I}|>;
referenceAngle = {3-I, 2+2 I}; referenceSquare = {2+I, -1+I};
matrices[7] = Outer[Times, lambda, square];
Do[expression = (endpoint["Angle", left, n, 1] endpoint["Square", right, m, 2])^power;
  check["arbitrary inserted matrices: " <> ToString[{left,right,n,m,power}],
    value[expression - continue[expression]] === 0],
  {left, {"Left", "Right"}}, {right, {"Left", "Right"}}, {n, 0, 2}, {m, 0, 2}, {power, {-2, -1, 1, 2}}];
ratio = SpinorChain[sp["Angle", 1], sp["Angle", q]]/SpinorChain[sp["Angle", 2], sp["Angle", q]];
check["same-chirality ratios with negative powers retain their neutral phase", value[ratio - continue[ratio]] === 0];
both = SpinorChain[sp["Angle", q], Mom[3], sp["Square", q]];
check["both internal endpoints use the correct orientation signs", value[both - continue[both]] === 0];
check["a non-neutral internal phase is rejected before choosing references",
  MatchQ[continue[endpoint["Angle", "Left", 0, 1]], Failure["MasslessSewingWeight", _Association]]];
propagator = PropDen[Mom[q], 0];
check["the actual propagator denominator is retained off shell", !FreeQ[continue[both/propagator], propagator]];
ang[i_, k_] := SpinorChain[sp["Angle", i], sp["Angle", k]];
sqr[i_, k_] := SpinorChain[sp["Square", i], sp["Square", k]];
singular = SpinorChain[sp["Square", 1], Mom[1]+Mom[3], sp["Angle", 4]]^3/
  (sqr[3,1] SpinorChain[sp["Angle", 2], Mom[1]+Mom[3], sp["Square", 3]]);
canceled = ConstructiveDiagrams`Private`factorizationExternalMasslessCancellation[singular,
  Thread[(Mass /@ Range[4]) -> ConstantArray[0,4]]];
check["external massless factorization cancels the zero-over-zero cut factors first",
  Cancel[Together[canceled - sqr[1,3] ang[3,4]^3/ang[1,2]]] === 0 && FreeQ[canceled, _Mom]];
(* A fit may cancel singular columns only in their complete affine sum. *)
laurentChannel = <|"StateSum" -> 3|>;
laurentChart = <|"Map" -> Function[{expression, channels, poles}, expression], "SpinVariables" -> {}|>;
laurentInitial = <|"PoleVariables" -> {testPole}, "Channels" -> {laurentChannel},
  "KinematicChecks" -> <|"Valid" -> True|>|>;
laurent[parts_, variables_, target_: laurentChannel] :=
  ConstructiveDiagrams`Private`completionLaurentResidueConstraints[parts, variables,
    target, laurentChart, laurentInitial, 10];
joint = laurent[{0, 1/testPole^2, -1/testPole^2 + 3/testPole}, {ca, cb}];
check["double-pole columns are constrained together rather than rejected or discarded",
  AssociationQ[joint] && joint["Status"] === "COMPLETE" &&
    Solve[Thread[joint["Equations"] == 0], {ca, cb}] === {{ca -> 1, cb -> 1}} &&
    joint["PoleStrippedDenominatorOrder"] === 1];
triple = laurent[{0, (1 + testPole)/testPole^3,
  -(1 + testPole)/testPole^3 + 3/testPole}, {ca, cb}];
check["the actual denominator valuation determines every required principal coefficient",
  AssociationQ[triple] && triple["PoleStrippedDenominatorOrder"] === 2 &&
    Solve[Thread[triple["Equations"] == 0], {ca, cb}] === {{ca -> 1, cb -> 1}}];
check["finite residues remain affine while all principal coefficients are retained",
  joint["FiniteResidue"] === 3 cb && triple["FiniteResidue"] === 3 cb];
check["an expected target with its own selected-cut divergence remains unresolved",
  MissingQ[laurent[{0, 1/testPole^2}, {ca}, <|"StateSum" -> 1/testPole|>]]];
check["the Laurent fallback cannot bypass invalid chart certification",
  MissingQ[ConstructiveDiagrams`Private`completionLaurentResidueConstraints[
    {0, 1/testPole^2}, {ca}, laurentChannel, laurentChart,
    Join[laurentInitial, <|"KinematicChecks" -> <|"Valid" -> False|>|>], 10]]];
nullChannel[cut_List] := <|"Cut" -> cut, "Complement" -> Complement[Range[4],cut],
  "ExternalLegs" -> Range[4], "ExternalMassRules" -> Thread[(Mass /@ Range[4]) -> ConstantArray[0,4]],
  "Mass" -> 0, "Propagator" -> PropDen[Mom[Multiparticle @@ cut],0]|>;
Do[chart = ConstructiveDiagrams`Private`completionCutChart[nullChannel[{1,2}], sample, 7129, {}];
  chartCheck = ConstructiveDiagrams`Private`residueKinematicChecks[chart["Map"],
    {nullChannel[{1,2}]}, {testPole}, nullChannel /@ {{1,3},{1,4}}];
  check["null four-point cut stays generic on both branches: sample " <> ToString[sample],
    TrueQ[chartCheck["Valid"]]], {sample, 4}];
validationChannel = Join[nullChannel[{1,2}],<|"Complete" -> True,"StateSum" -> 3|>];
validationPole = validationChannel["Propagator"];
validation = ConstructiveDiagrams`Private`completionSparseValidationSample[
  {0,1/validationPole^2,-1/validationPole^2+3/validationPole},{ca,cb},
  {validationChannel},{1,1},{ca->1,cb->1},{},{},{},8191,{},10,Infinity];
check["held-out validation checks the regular complete sum of singular correction columns",
  AssociationQ[validation] && validation["Status"] === "COMPLETE" &&
    validation["Result"]["Status"] === "PASS" && validation["Result"]["CalculatedResidue"] === 3 &&
    validation["Result"]["ConstraintMethod"] === "ExactRationalLaurent" &&
    TrueQ[validation["Result"]["PrincipalPartCancellationVerified"]] &&
    AllTrue[validation["Result"]["PrincipalPartCertificate"]["CandidatePrincipalEquations"],#===0&]];
invalidValidation = ConstructiveDiagrams`Private`completionSparseValidationSample[
  {0,1/validationPole^2,-1/validationPole^2+3/validationPole},{ca,cb},
  {validationChannel},{1,1},{ca->0,cb->1},{},{},{},8191,{},10,Infinity];
check["held-out validation rejects a surviving higher pole in the fitted complete sum",
  invalidValidation["Status"] === "UNRESOLVED"];
branchFirst = ang[3,4]^3 sqr[1,4]/(ang[1,2] ang[2,3]);
branchSecond = ang[1,4] sqr[1,2]^3/(sqr[2,3] sqr[3,4]);
check["common branch continuations have an exact symbolic ordinary-identity certificate",
  TrueQ[ConstructiveDiagrams`Private`factorizationFourNullIdentityQ[branchFirst-branchSecond,Range[4]]]];
check["the symbolic certificate rejects a nonzero change or inconsistent spin weights",
  !TrueQ[ConstructiveDiagrams`Private`factorizationFourNullIdentityQ[branchFirst-2 branchSecond,Range[4]]] &&
  !TrueQ[ConstructiveDiagrams`Private`factorizationFourNullIdentityQ[branchFirst+1,Range[4]]]];
branchSupport[kind_, pair_] := <|"Kind" -> kind,"Pair" -> pair|>;
disjointSupports = {{branchSupport["Angle",{1,4}],branchSupport["Square",{2,3}]},
  {branchSupport["Square",{1,4}],branchSupport["Angle",{2,3}]}};
branchRules = Thread[(Mass /@ Range[4]) -> ConstantArray[0,4]];
selectedBranches = ConstructiveDiagrams`Private`factorizationMasslessBranchContributions[
  {branchFirst,branchSecond},disjointSupports,branchRules];
check["disjoint exhaustive three-point supports select one certified common continuation",
  Count[selectedBranches,0] === 1 &&
    TrueQ[ConstructiveDiagrams`Private`factorizationFourNullIdentityQ[Total[selectedBranches]-branchFirst,Range[4]]]];
check["equal three-point branches have no generic single-cut support",
  ConstructiveDiagrams`Private`factorizationMasslessBranchContributions[
    {branchFirst,0},{{branchSupport["Angle",{1,4}],branchSupport["Angle",{2,3}]},{}},branchRules] === {0,0}];
check["absence of a branch certificate cannot remove or average a contribution",
  ConstructiveDiagrams`Private`factorizationMasslessBranchContributions[
    {branchFirst,branchSecond},{{},{}},branchRules] === {branchFirst,branchSecond}];
compoundFactor = PropDen[Mom[Multiparticle[1,4]],0] ang[2,3] +
  ang[1,2] ang[4,3] sqr[1,4];
factoredPole = ConstructiveDiagrams`Private`poleExternalMasslessFactor[compoundFactor,
  <|"ExternalLegs" -> Range[4],"ExternalMassRules" -> branchRules|>];
check["ordinary Schouten exposes physical brackets inside compound denominators",
  factoredPole === -ang[1,3] ang[2,4] sqr[1,4]];
poleRows = ConstructiveDiagrams`Private`poleDiscoverDenominators[1/compoundFactor,
  nullChannel /@ {{1,2},{1,3},{1,4}},{}];
check["exposed physical bracket factors are classified against the supplied channels",
  Length[poleRows] === 3 && AllTrue[poleRows, #["Classification"] === "Physical" &]];
sm = ReadModel[FileNameJoin[{root,"models"}],1];
check["reference-dependent vertices are not mislabeled as pure holomorphic support",
  MissingQ[ConstructiveDiagrams`Private`factorizationThreePointSupport[
    SpinorChain[Spinor["Xi","Square",991],sp["Square",1]],
    {{"G.+",1},{"G.+",2},{"G.-",Multiparticle[3,4]}},sm]]];
zeroProcess = {"G.+","G.+","G.+","G.-"};
zeroDiagrams = CreateDiagrams[zeroProcess,sm];
check["same-branch source vertices yield certified zero representatives without a helicity-name rule",
  zeroDiagrams =!= {} && AllTrue[Last /@ DiagramAmplitudes[zeroDiagrams], # === 0 &] &&
    FactorizationAmplitudeChannels[zeroProcess,sm] === {}];
zeroRecords = DiagramAmplitudeRecords[zeroDiagrams];
check["detailed diagram records retain the original sewn quotient and its scoped certificate",
  AnyTrue[zeroRecords, #["OriginalSewnExpression"] =!= 0 && #["Expression"] === 0 &&
    #["MasslessBranchCertificates"] =!= {} &&
    AllTrue[#["MasslessBranchCertificates"], #["OffCutIdentityClaimed"] === False &] &&
    MissingQ[#["DiscardedSupportTerms"][[1,"OrdinaryOffCutRemainder"]]] &]];
(* Massive-vector sewing can contract all explicit internal photon
   spinors, leaving a neutral x-factor product. The physical cut still
   supplies its unique null momentum, with an arbitrary frame that cancels. *)
vectorChannels=FactorizationAmplitudeChannels[{"W+","W+","W-","W-"},sm];
vectorNormalized=ConstructiveDiagrams`Private`completionNormalizeInput[
  0,vectorChannels,sm];
vectorTargets=ConstructiveDiagrams`Private`completionTargets[vectorNormalized["Targets"]];
vectorPhotonTargets=Select[vectorTargets,# ["Mass"]===0&];
vectorRules=ConstructiveDiagrams`Private`residueParameterRules[sm,Automatic];
vectorChart=ConstructiveDiagrams`Private`completionCutChart[First[vectorPhotonTargets],1,1729,vectorRules];
vectorX=xFactor[1,3] xTildeFactor[2,4];
vectorExpanded=ConstructiveDiagrams`Private`residueExpandCutXFactors[vectorX,vectorChart["Chart"]];
check["a neutral implicit cut-photon frame expands the remaining massive-vector x factors",
  FreeQ[vectorExpanded,_Missing|_xFactor|_xTildeFactor] &&
    ConstructiveDiagrams`Private`factorizationInternalSpinorWeight[vectorExpanded,Multiparticle[1,3]]===0];
check["an unbalanced implicit cut-photon frame remains unsupported",
  MatchQ[ConstructiveDiagrams`Private`residueExpandCutXFactors[xFactor[1,3],vectorChart["Chart"]],
    Missing["UnbalancedImplicitCutPhotonWeight"]]];
check["neutral inverse powers retain exact implicit little-group cancellation",
  FreeQ[ConstructiveDiagrams`Private`residueExpandCutXFactors[
    vectorX^-2+3 vectorX,vectorChart["Chart"]],_Missing|_xFactor|_xTildeFactor]];
check["synthetic spurious charts still cannot infer internal on-shell x factors",
  !FreeQ[ConstructiveDiagrams`Private`residueExpandCutXFactors[vectorX,
    Join[vectorChart["Chart"],<|"AllowCutSpinors"->False|>]],_Missing]];
Do[vectorPole=Unique["vectorCut"];
  vectorResidue=ConstructiveDiagrams`Private`residueOnChart[target["StateSum"],
    {target},{vectorPole},ConstructiveDiagrams`Private`completionCutChart[target,1,1729,vectorRules]["Map"],{}];
  check["the massive-vector photon target is regular after all explicit internal spinors contract " <>
      ToString[target["Cut"]],
    ConstructiveDiagrams`Private`residueFiniteQ[vectorResidue] &&
      !ConstructiveDiagrams`Private`residueUnmappedQ[vectorResidue]],{target,vectorPhotonTargets}];
Print["MASSLESS SEWING CHECKS: ", checks, "; FAILURES: ", Length[failures]];
If[failures === {}, Print["HEPCAT_TEST_COMPLETED:massless-sewing-tests.wl"], Print[failures]];
Quit[If[failures === {}, 0, 1]];
