(* Final amplitudes use ordinary identities and external masses only. *)
root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Global`$HEPCATpath = FileNameJoin[{root, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
$HistoryLength = 0;
checks = 0; failures = {};
SetAttributes[check, HoldRest];
check[name_, value_] := (checks++;
  If[!TrueQ[value], AppendTo[failures, name]; Print["FAIL: ", name]]);
fsRank = ConstructiveDiagrams`Private`preparationRank;
fsCompact = ConstructiveDiagrams`Private`preparationCompactExternalSums;
fsSparse = ConstructiveDiagrams`Private`preparationSparsePoleContent;
fsFactor = ConstructiveDiagrams`Private`rspLocalFactorContent;
fsZero[value_] := Together[value] === 0;
fsBracket[type_, i_, j_] := SpinorChain[
  Spinor["Helicity", type, i], Spinor["Helicity", type, j]];
fsPhysicalPoles[expr_] := Sort[DeleteDuplicates[Cases[expr, _PropDen, Infinity]]];
fsNoTokens[expr_] := FreeQ[expr, symbol_Symbol /;
  StringStartsQ[SymbolName[symbol], {"sparsePhysicalPole", "sparseContentAtom",
    "compactSpinorInverse", "spinorFactorInverse"}]];

(* A representation guard must never suppress a required endpoint equation. *)
Do[
  fsOtherType = If[fsType === "Angle", "Square", "Angle"];
  fsLeft = SpinorChain[Spinor["Spin", fsType, 1], Mom[1],
    Spinor["Spin", fsOtherType, 4]];
  fsLeftExpected = Mass[1] SpinorChain[Spinor["Spin", fsOtherType, 1],
    Spinor["Spin", fsOtherType, 4]];
  fsRight = SpinorChain[Spinor["Spin", fsOtherType, 1], Mom[4],
    Spinor["Spin", fsType, 4]];
  check[{"left external Dirac equation remains mandatory", fsType},
    ReduceSpinorProducts[fsLeft] === fsLeftExpected];
  check[{"right external Dirac equation retains its sign", fsType},
    ReduceSpinorProducts[fsRight] === -Mass[4] SpinorChain[
      Spinor["Spin", fsOtherType, 1], Spinor["Spin", fsOtherType, 4]]];
  check[{"massless endpoint annihilation remains mandatory", fsType},
    ReduceSpinorProducts[SpinorChain[Spinor["Helicity", fsType, 1], Mom[1],
      Spinor["Spin", fsOtherType, 4]]] === 0],
  {fsType, {"Angle", "Square"}}];
fsIndexed = SpinorChain[Spinor["Spin", "Angle", 1], Mom[4],
  Spinor["Spin", "Upper", "Square", 4, fsProjection]];
check["an indexed endpoint equation preserves the explicit projection",
  ReduceSpinorProducts[fsIndexed] === -Mass[4] SpinorChain[
    Spinor["Spin", "Angle", 1], Spinor["Spin", "Upper", "Angle", 4, fsProjection]]];

(* Independent two-component determinants, without momentum conservation. *)
fsEpsilon = {{0, 1}, {-1, 0}};
fsColumn[leg_, seed_] := {leg + seed, 2 leg^2 - seed + 1};
fsBracketValue[expr_, seed_] := expr /. {
  SpinorChain[Spinor["Helicity", type_, i_Integer],
      Spinor["Helicity", type_, j_Integer]] :>
    If[type === "Angle", 1, -1] fsColumn[i, seed].fsEpsilon.fsColumn[j, seed]};
Do[
  fsSchouten = 3 fsBracket[fsType, 1, 4] fsBracket[fsType, 2, 3] -
    3 fsBracket[fsType, 1, 3] fsBracket[fsType, 2, 4];
  fsSchoutenExpected = -3 fsBracket[fsType, 1, 2] fsBracket[fsType, 3, 4];
  fsSchoutenReduced = ReduceSpinorProducts[fsSchouten];
  check[{"ordinary Schouten still reduces a shared-coefficient sum", fsType},
    fsSchoutenReduced === fsSchoutenExpected];
  check[{"Schouten reduction agrees with independent exact determinants", fsType},
    And @@ Table[fsBracketValue[fsSchoutenReduced - fsSchouten, seed] === 0, {seed, 2, 4}]];
  check[{"ordinary sum reductions remain idempotent", fsType},
    ReduceSpinorProducts[fsSchoutenReduced] === fsSchoutenReduced],
  {fsType, {"Angle", "Square"}}];

fsA = fsBracket["Angle", 3, 4]; fsB = fsBracket["Angle", 3, 5];
fsC = fsBracket["Square", 4, 6];
fsPole1 = PropDen[Mom[Multiparticle[1, 3]], fsMass];
fsPole2 = PropDen[Mom[Multiparticle[1, 4]], fsMass];
fsInverse = 1/(fsA + fsC);
fsRational = (2 fsA fsC + 2 fsB fsC) fsInverse/fsPole1;
fsFactored = fsFactor[fsRational];
check["local factoring is an exact ordinary rational identity", fsZero[fsFactored - fsRational]];
check["local factoring retains the complete auxiliary inverse base", !FreeQ[fsFactored, fsInverse]];
check["local factoring leaves the original physical propagator intact",
  fsPhysicalPoles[fsFactored] === {fsPole1}];
check["local factoring introduces no internal shell substitutions",
  FreeQ[fsFactored, Mass[_Multiparticle]]];
fsDistinct = fsA/fsPole1 + fsB/fsPole2;
check["different physical poles remain separate during local factoring",
  fsPhysicalPoles[fsFactor[fsDistinct]] === Sort[{fsPole1, fsPole2}] &&
    fsZero[fsFactor[fsDistinct] - fsDistinct]];
fsScope1 = ModelParameter["m", "final-simplification-one", "Real"];
fsScope2 = ModelParameter["m", "final-simplification-two", "Real"];
fsScoped = fsScope1 fsA/fsPole1 + fsScope2 fsA/fsPole2;
fsScopedReduced = fsCompact[fsScoped];
check["equally printed parameters retain their distinct model scopes",
  Sort[DeleteDuplicates[Cases[fsScopedReduced, _ModelParameter, Infinity]]] === Sort[{fsScope1, fsScope2}] &&
    fsZero[fsScopedReduced - fsScoped]];
check["factoring and compaction create no parameter assignments",
  OwnValues[fsMass] === {} && OwnValues[fsProjection] === {}];
check["temporary factoring tokens never escape a result",
  fsNoTokens[{fsFactored, fsScopedReduced}]];
fsNoncommutative = (fsA + fsB) Dot[fsMatrix1, fsMatrix2];
check["the rational compactor declines a noncommutative expression",
  fsCompact[fsNoncommutative] === fsNoncommutative];

(* A self-contained version of the verified twelve-term all-positive six-point
   expression. This is a test fixture, never a process-specific production rule. *)
fsModel = ReadModel[FileNameJoin[{root, "models"}], 3];
fsMass = ModelParameter[fsModel, "Me"]; fsCharge = ModelParameter[fsModel, "EE"];
fsPhotons = Range[3, 6]; fsPairs = Subsets[fsPhotons, {2}];
fsAngle12 = SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]];
fsSquare[i_, j_] := fsBracket["Square", i, j];
fsK[i_] := SpinorChain[Spinor["Helicity", "Square", i], Mom[1], Mom[2],
  Spinor["Helicity", "Square", i]];
fsD[parts_List] := PropDen[Mom[Multiparticle @@ Sort[parts]], fsMass];
fsSixTerms = Flatten[Table[With[{other = Complement[fsPhotons, pair]}, {
  -4 fsCharge^4 fsMass^3 fsAngle12 (fsSquare @@ pair)^2 (fsSquare @@ other)^2/
    ((Times @@ (fsD[{1, #}] & /@ other)) (Times @@ (fsD[{2, #}] & /@ pair)) fsD[Prepend[other, 1]]),
  4 fsCharge^4 fsMass fsAngle12 (fsSquare @@ other)^2 (Times @@ (fsK /@ pair))/
    ((Times @@ (fsD[{1, #}] & /@ fsPhotons)) (Times @@ (fsD[{2, #}] & /@ pair)))}],
  {pair, fsPairs}], 1];
fsSixOriginal = FactorTerms[Total[fsSixTerms]];
fsSixSaved = fsSixOriginal;
fsSixCompact = fsSparse[fsSixOriginal, 2];
check["the six-point fixture has its documented complexity and original size",
  fsRank[fsSixOriginal] === {108, 1, 1347} &&
    ConstructiveDiagrams`Private`expressionTermCounts[fsSixOriginal]["Terms"] === 12];
check["sparse pole-content extraction finds the compact six-block representation",
  fsRank[fsSixCompact][[1]] === 108 && LeafCount[fsSixCompact] <= 732 &&
    LeafCount[fsSixCompact] < LeafCount[fsSixOriginal]];
check["six-point sparse compaction is exactly equal away from all poles",
  fsZero[fsSixCompact - fsSixOriginal]];
check["sparse compaction preserves the physical pole inventory",
  fsPhysicalPoles[fsSixCompact] === fsPhysicalPoles[fsSixOriginal]];
check["sparse compaction retains model-local mass and charge parameters",
  Sort[DeleteDuplicates[Cases[fsSixCompact, _ModelParameter, Infinity]]] === Sort[{fsMass, fsCharge}]];
check["the complete rational amplitude receives no shell assumptions or auxiliary symbols",
  FreeQ[fsSixCompact, _Mass | Spinor["Xi", ___]] && fsNoTokens[fsSixCompact]];
check["sparse compaction is idempotent on the verified six-point representative",
  fsSparse[fsSixCompact, 2] === fsSixCompact];
check["the input expression is preserved for the caller", fsSixOriginal === fsSixSaved];
Do[
  fsPermuted = PermuteParticles[fsSixOriginal, fsCycle];
  fsPermutedCompact = fsSparse[fsPermuted, 2];
  check[{"the sparse rule works after a typed particle permutation", fsCycle},
    LeafCount[fsPermutedCompact] < LeafCount[fsPermuted] &&
      SpinorExpressionComplexity[fsPermutedCompact] === SpinorExpressionComplexity[fsPermuted] &&
      fsZero[fsPermutedCompact - fsPermuted]],
  {fsCycle, {{3, 6, 4}, {3, 5}}}];
fsFixedSix = fsSixOriginal /. {
  Spinor["Spin", "Angle", 1] -> Spinor["Spin", "Upper", "Angle", 1, 1/2],
  Spinor["Spin", "Angle", 2] -> Spinor["Spin", "Lower", "Angle", 2, -1/2]};
fsFixedCompact = fsSparse[fsFixedSix, 2];
check["explicit external spin indices survive sparse rational grouping",
  !FreeQ[fsFixedCompact, Spinor["Spin", "Upper", "Angle", 1, 1/2]] &&
    !FreeQ[fsFixedCompact, Spinor["Spin", "Lower", "Angle", 2, -1/2]] &&
    fsZero[fsFixedCompact - fsFixedSix]];
fsSixRules = Join[{Mass[1] -> fsMass, Mass[2] -> fsMass}, Table[Mass[i] -> 0, {i, 3, 6}]];
fsPrepared = PrepareAmplitudes[{fsSixRules, fsSixOriginal}, fsModel,
  "PolePreference" -> None, "MomentumConservation" -> False,
  "MaxSteps" -> 0, "TimeConstraint" -> 30, "ProgressReporting" -> False,
  "ReturnDetails" -> True];
check["public preparation compacts a completed amplitude without internal on-shell rules",
  fsPrepared["Status"] === "PREPARED" && fsPrepared["Options"]["InternalOnShell"] === False &&
    First[fsPrepared["ItemReports"]]["InternalOnShellChannels"] === {}];
check["public preparation retains the compact best whole expression",
  LeafCount[fsPrepared["Amplitude"]] <= 732 && fsZero[fsPrepared["Amplitude"] - fsSixOriginal]];
check["completed compaction is explicitly recorded as an ordinary identity",
  AnyTrue[First[fsPrepared["ItemReports"]]["Reductions"],
    #["Operation"] === "CompactExternalAmplitude" &&
      #["InternalOnShellIdentity"] === False && #["OnShellChannels"] === {} &]];
check["all original external masses remain available after preparation",
  fsPrepared["ExternalMassRules"] === fsSixRules &&
    First[First[fsPrepared["PreparedAmplitudes"]]] === fsSixRules];
fsPreparedAgain = PrepareAmplitudes[First[fsPrepared["PreparedAmplitudes"]], fsModel,
  "PolePreference" -> None, "MomentumConservation" -> False,
  "MaxSteps" -> 0, "TimeConstraint" -> 30, "ProgressReporting" -> False];
check["the public completed-amplitude route is idempotent",
  fsPreparedAgain === First[fsPrepared["PreparedAmplitudes"]]];
fsProbe = PrepareAmplitudes[{fsSixRules, MomProd[1, 3]/fsD[{1, 3}]}, fsModel,
  "PolePreference" -> None, "MomentumConservation" -> False,
  "MaxSteps" -> 0, "ProgressReporting" -> False];
check["a propagator in a final amplitude never licenses its numerator shell condition",
  Last[fsProbe] =!= 0 && fsZero[(Last[fsProbe] - 1/2) /.
    fsD[{1, 3}] -> 2 MomProd[1, 3]]];
fsVirtuality = ConstructiveDiagrams`Private`preparationExternalVirtualities[
  MomProd[1, 3]/fsD[{1, 3}], fsSixRules];
check["the exact external pair virtuality cancels a removable physical factor",
  fsVirtuality === 1/2];
fsVirtualityInverse = ConstructiveDiagrams`Private`preparationExternalVirtualities[
  MomProd[1, 3]/(fsD[{1, 3}] (5 + MomProd[1, 3])), fsSixRules];
check["ordinary virtuality reduction leaves inverse scalar products off shell",
  fsZero[fsVirtualityInverse - 1/(2 (5 + MomProd[1, 3]))]];

(* Known five-point model rows provide independent, process-diverse rational
   inputs. Compaction here changes algebraic grouping, never the spinor atoms. *)
Do[
  fsKnown = LookupAmplitude[MapIndexed[{#1, First[#2]} &, fsParts], fsModel, False,
    {fsSpin1, fsSpin2, fsSpin3, fsSpin4}];
  fsKnownCompact = fsCompact[fsKnown];
  check[{"known five-point rows retain a nonworsening representation", fsParts},
    OrderedQ[{fsRank[fsKnownCompact], fsRank[fsKnown]}]];
  check[{"known five-point compaction is an exact rational regrouping", fsParts},
    Collect[fsKnownCompact - fsKnown,
      DeleteDuplicates[Cases[{fsKnownCompact, fsKnown}, _SpinorChain, Infinity]], Together] === 0],
  {fsParts, {{"e", "E", "A.+", "A.+", "A.+"},
    {"e", "E", "A.+", "A.+", "A.-"}, {"e", "E", "E", "e", "A.+"}}}];
check["zero amplitudes remain zero in every reduction route",
  ReduceSpinorProducts[0] === 0 && fsCompact[0] === 0 && fsSparse[0] === 0];

(* Momentum conservation must treat a bare momentum square as a Lorentz
   scalar while retaining the original inverse factors and physical pole. *)
fsConservationRules = Table[Mass[i] -> fsExternalMass[i], {i, 4}];
fsCompositeMomentum = Mom[Multiparticle[1, 2]];
fsCompositeChain = SpinorChain[Spinor["Xi", "Angle", 70], fsCompositeMomentum,
  Spinor["Xi", "Square", 71]];
fsSquareInverse = 1/(9 + fsCompositeMomentum^2);
fsConservationInput = fsCompositeMomentum^2 fsCompositeChain fsSquareInverse/fsPole1;
Do[
  fsConserved = ConstructiveDiagrams`Private`preparationConservationTrial[
    fsConservationInput, fsEdit, fsConservationRules];
  fsConservedNumerator = Cancel[fsConserved fsPole1/fsSquareInverse] /.
    MomProd[i_Integer, i_Integer] :> Mass[i]^2;
  fsExpectedSquare = If[fsEdit["Identity"] === "CompositeComplement",
    Mass[1]^2 + MomProd[1, 2] + MomProd[2, 1] + Mass[2]^2,
    Mass[1]^2 - Mass[2]^2 - 2 MomProd[2, 3] - 2 MomProd[2, 4]];
  fsExpectedChain = SpinorChain[Spinor["Xi", "Angle", 70], -Mom[3] - Mom[4],
    Spinor["Xi", "Square", 71]];
  check[{"a conserved momentum square retains its Lorentz cross products", fsEdit["Identity"]},
    fsZero[fsConservedNumerator - fsExpectedSquare fsExpectedChain]];
  check[{"conservation preserves original physical and auxiliary inverse factors", fsEdit["Identity"]},
    fsPhysicalPoles[fsConserved] === {fsPole1} && !FreeQ[fsConserved, fsSquareInverse]];
  check[{"conservation introduces neither commuting momentum squares nor internal masses", fsEdit["Identity"]},
    FreeQ[fsConservedNumerator, Power[_Mom, 2] | Mass[_Multiparticle]]];
  fsMatrices = <|2 -> {{2, 1}, {3, 4}}, 3 -> {{5, 2}, {1, 3}}, 4 -> {{1, 4}, {2, 7}}|>;
  fsMatrices[1] = -Total[Values[fsMatrices]];
  fsAdj[m_] := {{m[[2, 2]], -m[[1, 2]]}, {-m[[2, 1]], m[[1, 1]]}};
  fsScalarCoefficient = Cancel[fsConservedNumerator/fsExpectedChain] /. {
    HoldPattern[Mass[i_Integer]^2] :> Det[fsMatrices[i]],
    MomProd[i_Integer, j_Integer] :> Tr[fsMatrices[i].fsAdj[fsMatrices[j]]]/2};
  check[{"the conserved scalar square agrees with an independent off-cut determinant", fsEdit["Identity"]},
    fsScalarCoefficient === 42 && Det[fsMatrices[1] + fsMatrices[2]] === 42],
  {fsEdit, {<|"Identity" -> "ExternalElimination", "Legs" -> {1}|>,
    <|"Identity" -> "CompositeComplement", "Legs" -> {1, 2}|>}}];

Do[
  fsUnknownTrial = ConstructiveDiagrams`Private`preparationConservationTrial[
    (Mom[fsUnknown]^2 + fsCompositeChain)/fsPole1,
    <|"Identity" -> "ExternalElimination", "Legs" -> {1}|>, fsConservationRules];
  fsUnknownNumerator = Cancel[fsUnknownTrial fsPole1];
  check[{"undeclared momentum squares are never assigned a mass shell", fsUnknown},
    !FreeQ[fsUnknownNumerator, Mom[fsUnknown]^2] &&
      FreeQ[fsUnknownNumerator, Mass[fsUnknown]]],
  {fsUnknown, {99, fsUnknownMomentum}}];

(* A one-insertion conservation trial must not rewrite a longer word or any
   protected inverse. The four short terms sum to total external momentum. *)
fsSingleRules = Table[Mass[i] -> fsExternalMass[i], {i, 4}];
fsLongWord = SpinorChain[Spinor["Xi", "Angle", 70], Mom[900], Mom[901],
  Spinor["Xi", "Angle", 71]];
fsSingleInverse = 1/(9 + SpinorChain[Spinor["Xi", "Angle", 80], Mom[1],
  Spinor["Xi", "Square", 81]]);
fsShortTotal = Total[Table[SpinorChain[Spinor["Xi", "Angle", 70], Mom[i],
  Spinor["Xi", "Square", 71]], {i, 4}]];
fsSingleInput = (fsShortTotal + fsLongWord) fsSingleInverse/fsPole1;
fsSingleTrial = ConstructiveDiagrams`Private`preparationSingleMomentumConservation[
  fsSingleInput, fsSingleRules, 2];
fsSingleCanonicalInverse = 1/(9 + SpinorChain[Spinor["Xi", "Square", 81], Mom[1],
  Spinor["Xi", "Angle", 80]]);
check["single-insertion conservation removes only the declared total external momentum",
  fsZero[fsSingleTrial - fsLongWord fsSingleCanonicalInverse/fsPole1]];
check["single-insertion conservation preserves longer words and inverse factors",
  !FreeQ[fsSingleTrial, fsLongWord] && !FreeQ[fsSingleTrial, fsSingleCanonicalInverse] &&
    fsPhysicalPoles[fsSingleTrial] === {fsPole1}];
fsSingleOff = PrepareAmplitudes[{fsSingleRules, fsSingleInput}, fsModel,
  "PolePreference" -> None, "MomentumConservation" -> False,
  "MaxSteps" -> 0, "TimeConstraint" -> 15, "ProgressReporting" -> False, "ReturnDetails" -> True];
check["MomentumConservation False disables the additional completed-amplitude route",
  fsSingleOff["Options"]["MomentumConservation"] === False &&
    !AnyTrue[First[fsSingleOff["ItemReports"]]["Reductions"],
      #["Operation"] === "SingleMomentumConservation" &]];
fsSingleOn = PrepareAmplitudes[{fsSingleRules, fsSingleInput}, fsModel,
  "PolePreference" -> None, "MomentumConservation" -> True,
  "MaxSteps" -> 0, "TimeConstraint" -> 15, "ProgressReporting" -> False, "ReturnDetails" -> True];
check["the enabled conservation route remains an external ordinary-identity stage",
  AnyTrue[First[fsSingleOn["ItemReports"]]["Reductions"],
    #["Operation"] === "SingleMomentumConservation" &&
      #["InternalOnShellIdentity"] === False && #["OnShellChannels"] === {} &]];

(* The actual saved mixed-helicity completion contains an off-shell composite
   Lorentz square. This typed fixture is independent of the new reducers. *)
(* Saved five-point ++- completion representative, retained as typed data.
   All momenta are ingoing. No internal on-shell identity is permitted.
   Its scalar Mom[Multiparticle[1,2]]^2 is an off-shell Lorentz square. *)
fsSavedMixedFixture[fixtureMass_, fixtureCharge_] := Module[{spinor, chain, prop},
  spinor[leg_, kind_] := Spinor[If[MemberQ[{1,2}, leg], "Spin", "Helicity"], kind, leg];
  chain[left_, i_, momenta_List, right_, j_] := SpinorChain @@ Join[
    {spinor[i,left]}, Mom /@ momenta, {spinor[j,right]}];
  prop[legs_List, mass_] := PropDen[Mom[Multiparticle @@ legs], mass];
  (2*Sqrt[2]*fixtureCharge^3*fixtureMass*chain["Angle", 5, {}, "Angle", 1]*
   chain["Angle", 5, {}, "Angle", 2]*chain["Square", 3, {}, "Square", 4]^2)/
  (prop[{1, 5}, fixtureMass]*prop[{2, 3}, fixtureMass]*prop[{2, 4}, fixtureMass]) -
 (2*Sqrt[2]*fixtureCharge^3*chain["Angle", 5, {}, "Angle", 2]*chain["Angle", 5, {1}, "Square", 3]*
   chain["Square", 3, {}, "Square", 4]*chain["Square", 4, {}, "Square", 1])/
  (prop[{1, 4}, fixtureMass]*prop[{2, 3}, fixtureMass]*prop[{2, 5}, fixtureMass]) -
 (2*Sqrt[2]*fixtureCharge^3*chain["Angle", 5, {}, "Angle", 2]*chain["Square", 3, {}, "Square", 4]*
   chain["Square", 3, {4}, "Angle", 5]*chain["Square", 4, {}, "Square", 1])/
  (prop[{1, 4}, fixtureMass]*prop[{2, 3}, fixtureMass]*prop[{2, 5}, fixtureMass]) +
 (2*Sqrt[2]*fixtureCharge^3*chain["Angle", 5, {}, "Angle", 2]*chain["Angle", 5, {1}, "Square", 4]*
   chain["Square", 3, {}, "Square", 1]*chain["Square", 3, {}, "Square", 4])/
  (prop[{1, 3}, fixtureMass]*prop[{2, 4}, fixtureMass]*prop[{2, 5}, fixtureMass]) +
 (2*Sqrt[2]*fixtureCharge^3*chain["Angle", 5, {}, "Angle", 2]*chain["Angle", 5, {3}, "Square", 1]*
   chain["Square", 3, {}, "Square", 4]^2)/(prop[{1, 3}, fixtureMass]*prop[{2, 4}, fixtureMass]*
   prop[{2, 5}, fixtureMass]) - (2*Sqrt[2]*fixtureCharge^3*chain["Angle", 5, {}, "Angle", 2]*
   chain["Angle", 5, {1}, "Square", 4]*chain["Square", 3, {1, 2}, "Square", 3]*
   chain["Square", 4, {}, "Square", 1])/(prop[{1, 3}, fixtureMass]*prop[{2, 3}, fixtureMass]*
   prop[{2, 4}, fixtureMass]*prop[{2, 5}, fixtureMass]) +
 (2*Sqrt[2]*fixtureCharge^3*chain["Angle", 5, {}, "Angle", 2]*chain["Square", 3, {}, "Square", 4]*
   chain["Square", 3, {1, 2, 3}, "Angle", 5]*chain["Square", 4, {}, "Square", 1])/
  (prop[{1, 3}, fixtureMass]*prop[{2, 3}, fixtureMass]*prop[{2, 4}, fixtureMass]*prop[{2, 5}, fixtureMass]) -
 (2*Sqrt[2]*fixtureCharge^3*chain["Angle", 5, {}, "Angle", 1]*chain["Angle", 5, {1}, "Square", 4]*
   chain["Square", 3, {1, 2}, "Square", 3]*chain["Square", 4, {}, "Square", 2])/
  (prop[{1, 3}, fixtureMass]*prop[{2, 3}, fixtureMass]*prop[{2, 4}, fixtureMass]*prop[{2, 5}, fixtureMass]) +
 (2*Sqrt[2]*fixtureCharge^3*chain["Angle", 5, {}, "Angle", 1]*chain["Square", 3, {}, "Square", 4]*
   chain["Square", 3, {1, 2, 3}, "Angle", 5]*chain["Square", 4, {}, "Square", 2])/
  (prop[{1, 3}, fixtureMass]*prop[{2, 3}, fixtureMass]*prop[{2, 4}, fixtureMass]*prop[{2, 5}, fixtureMass]) -
 (2*Sqrt[2]*fixtureCharge^3*chain["Angle", 5, {}, "Angle", 2]*chain["Angle", 5, {1}, "Square", 3]*
   chain["Square", 3, {}, "Square", 1]*chain["Square", 4, {1, 2}, "Square", 4])/
  (prop[{1, 4}, fixtureMass]*prop[{2, 3}, fixtureMass]*prop[{2, 4}, fixtureMass]*prop[{2, 5}, fixtureMass]) -
 (2*Sqrt[2]*fixtureCharge^3*chain["Angle", 5, {}, "Angle", 1]*chain["Angle", 5, {1}, "Square", 3]*
   chain["Square", 3, {}, "Square", 2]*chain["Square", 4, {1, 2}, "Square", 4])/
  (prop[{1, 4}, fixtureMass]*prop[{2, 3}, fixtureMass]*prop[{2, 4}, fixtureMass]*prop[{2, 5}, fixtureMass]) -
 (2*Sqrt[2]*fixtureCharge^3*chain["Angle", 5, {}, "Angle", 2]*chain["Square", 3, {}, "Square", 1]*
   chain["Square", 3, {4}, "Angle", 5]*chain["Square", 4, {1, 2}, "Square", 4])/
  (prop[{1, 4}, fixtureMass]*prop[{2, 3}, fixtureMass]*prop[{2, 4}, fixtureMass]*prop[{2, 5}, fixtureMass]) -
 (2*Sqrt[2]*fixtureCharge^3*chain["Angle", 5, {}, "Angle", 1]*chain["Square", 3, {}, "Square", 2]*
   chain["Square", 3, {4}, "Angle", 5]*chain["Square", 4, {1, 2}, "Square", 4])/
  (prop[{1, 4}, fixtureMass]*prop[{2, 3}, fixtureMass]*prop[{2, 4}, fixtureMass]*prop[{2, 5}, fixtureMass]) -
 (2*Sqrt[2]*fixtureCharge^3*fixtureMass^2*chain["Angle", 5, {}, "Angle", 2]*
   chain["Angle", 5, {3}, "Square", 1]*chain["Square", 3, {}, "Square", 4]^2)/
  (prop[{1, 5}, fixtureMass]*prop[{2, 3}, fixtureMass]*prop[{2, 4}, fixtureMass]*prop[{2, 5}, fixtureMass]) -
 (2*Sqrt[2]*fixtureCharge^3*fixtureMass^2*chain["Angle", 5, {}, "Angle", 1]*
   chain["Angle", 5, {3}, "Square", 2]*chain["Square", 3, {}, "Square", 4]^2)/
  (prop[{1, 5}, fixtureMass]*prop[{2, 3}, fixtureMass]*prop[{2, 4}, fixtureMass]*prop[{2, 5}, fixtureMass]) +
 (2*Sqrt[2]*fixtureCharge^3*fixtureMass^2*chain["Angle", 5, {}, "Angle", 2]*
   chain["Square", 3, {}, "Square", 4]*chain["Square", 3, {4}, "Angle", 5]*
   chain["Square", 4, {}, "Square", 1])/(prop[{1, 5}, fixtureMass]*prop[{2, 3}, fixtureMass]*
   prop[{2, 4}, fixtureMass]*prop[{2, 5}, fixtureMass]) +
 (2*Sqrt[2]*fixtureCharge^3*fixtureMass^2*chain["Angle", 5, {}, "Angle", 1]*
   chain["Square", 3, {}, "Square", 4]*chain["Square", 3, {4}, "Angle", 5]*
   chain["Square", 4, {}, "Square", 2])/(prop[{1, 5}, fixtureMass]*prop[{2, 3}, fixtureMass]*
   prop[{2, 4}, fixtureMass]*prop[{2, 5}, fixtureMass]) +
 (2*Sqrt[2]*fixtureCharge^3*fixtureMass*chain["Angle", 5, {}, "Angle", 1]*
   chain["Angle", 5, {}, "Angle", 2]*chain["Square", 3, {}, "Square", 4]^2*
   Mom[Multiparticle[1, 2]]^2)/(prop[{1, 5}, fixtureMass]*prop[{2, 3}, fixtureMass]*
   prop[{2, 4}, fixtureMass]*prop[{2, 5}, fixtureMass])
];

Get[FileNameJoin[{root, "tests", "QED-5-point-residues.wl"}]];
fsMixedOriginal = fsSavedMixedFixture[prElectronMass, prElectricCharge];
fsMixedRules = {Mass[1] -> prElectronMass, Mass[2] -> prElectronMass,
  Mass[3] -> 0, Mass[4] -> 0, Mass[5] -> 0};
fsMixedPrepared = PrepareAmplitudes[{fsMixedRules, fsMixedOriginal}, prModel,
  "PolePreference" -> None, "MaxSteps" -> 0,
  "TimeConstraint" -> 30, "ProgressReporting" -> False, "ReturnDetails" -> True];
check["the saved mixed-helicity fixture retains the off-shell square that exposed the bug",
  SpinorExpressionComplexity[fsMixedOriginal] === 148 &&
    !FreeQ[fsMixedOriginal, Mom[Multiparticle[1, 2]]^2]];
check["ordinary completed preparation reaches the improved mixed-helicity form",
  fsMixedPrepared["Status"] === "PREPARED" &&
    SpinorExpressionComplexity[fsMixedPrepared["Amplitude"]] <= 70 &&
    LeafCount[fsMixedPrepared["Amplitude"]] <= 591];
check["the mixed-helicity simplification uses no internal shell condition",
  fsMixedPrepared["Options"]["InternalOnShell"] === False &&
    First[fsMixedPrepared["ItemReports"]]["InternalOnShellChannels"] === {} &&
    FreeQ[fsMixedPrepared["Amplitude"], Mass[_Multiparticle]]];
fsLorentzChartValue[expression_, chart_] := prMapOnChart[expression /.
  HoldPattern[Power[Mom[index_], 2]] :> Det[prMomentum[index, chart]], chart];
fsMixedPhysicalPoles = fsPhysicalPoles[{fsMixedOriginal, fsMixedPrepared["Amplitude"]}];
Do[
  fsMixedCharts = Flatten[Table[prDoubleChart[point[[1]], point[[2]], s1, s2],
    {s1, 2}, {s2, 2}], 1];
  fsMixedPairs = {fsLorentzChartValue[fsMixedOriginal, #],
      fsLorentzChartValue[fsMixedPrepared["Amplitude"], #]} & /@ fsMixedCharts;
  check[{"mixed-helicity final forms agree on every coherent spin component off cut", point},
    AnyTrue[fsMixedPairs[[All, 1]], # =!= 0 &] &&
      FreeQ[fsMixedPairs, Indeterminate | ComplexInfinity | _DirectedInfinity] &&
      AllTrue[fsMixedCharts, Function[chart, AllTrue[fsMixedPhysicalPoles,
        fsLorentzChartValue[#, chart] =!= 0 &]]] &&
      AllTrue[fsMixedPairs, prZero[First[#] - Last[#]] &]],
  {point, {{11, 17}, {13, 19}}}];
check["completed mixed-helicity reduction contains only Lorentz scalar squares",
  FreeQ[fsMixedPrepared["Amplitude"],
    product_Times /; Count[List @@ product, _Mom] >= 2] &&
  FreeQ[fsMixedPrepared["Amplitude"], Power[base_Plus, 2] /;
    ConstructiveDiagrams`Private`rspLinearMomentumTerms[base] =!= $Failed]];

(* A correct final form may retain canceling spurious bracket poles. Its
   inverse chains do not make it an uncontracted or reference-dependent input. *)
Do[
  fsS2Input = If[fsParity,
    prS2 /. Spinor[type_, kind : ("Angle" | "Square"), rest___] :>
      Spinor[type, If[kind === "Angle", "Square", "Angle"], rest], prS2];
  fsS2Prepared = PrepareAmplitudes[{fsMixedRules, fsS2Input}, prModel,
    "PolePreference" -> None, "MaxSteps" -> 0, "TimeConstraint" -> 10,
    "ProgressReporting" -> False, "ReturnDetails" -> True];
  check[{"spurious-pole final forms retain their simpler original candidate", fsParity},
    fsS2Prepared["Status"] === "PREPARED" &&
      fsS2Prepared["Options"]["InternalOnShell"] === False &&
      First[fsS2Prepared["ItemReports"]]["OriginalCandidateEligible"] === True &&
      First[fsS2Prepared["ItemReports"]]["OriginalCandidateAdmitted"] === True &&
      OrderedQ[{fsRank[fsS2Prepared["Amplitude"]], fsRank[fsS2Input]}]];
  fsS2Charts = Flatten[Table[prDoubleChart[point[[1]], point[[2]], s1, s2],
    {point, {{11, 17}, {13, 19}}}, {s1, 2}, {s2, 2}], 2];
  fsS2Pairs = {fsLorentzChartValue[fsS2Input, #],
      fsLorentzChartValue[fsS2Prepared["Amplitude"], #]} & /@ fsS2Charts;
  fsS2Poles = fsPhysicalPoles[{fsS2Input, fsS2Prepared["Amplitude"]}];
  check[{"spurious-pole final forms remain equal off every physical pole", fsParity},
    AnyTrue[fsS2Pairs[[All, 1]], # =!= 0 &] &&
      FreeQ[fsS2Pairs, Indeterminate | ComplexInfinity | _DirectedInfinity] &&
      AllTrue[fsS2Charts, Function[chart, AllTrue[fsS2Poles,
        fsLorentzChartValue[#, chart] =!= 0 &]]] &&
      AllTrue[fsS2Pairs, prZero[First[#] - Last[#]] &]],
  {fsParity, {False, True}}];
fsIndexedInverse = SpinorChain[Spinor["Spin", "Upper", "Angle", 1, 1/2],
    Spinor["Helicity", "Angle", 4]]^2 *
  SpinorChain[Spinor["Spin", "Lower", "Square", 2, -1/2],
    Spinor["Helicity", "Square", 3]] /
  (SpinorChain[Spinor["Spin", "Upper", "Angle", 1, 1/2],
      Spinor["Helicity", "Angle", 3]] fsPole1);
check["completed inverse brackets accept explicit physical spin indices while rejecting Xi",
  ConstructiveDiagrams`Private`preparationCompleteExternalInputQ[fsIndexedInverse, fsMixedRules] &&
    !ConstructiveDiagrams`Private`preparationCompleteExternalInputQ[
      fsIndexedInverse /. Spinor["Spin", "Upper", "Angle", 1, 1/2] ->
        Spinor["Xi", "Angle", 1], fsMixedRules]];
check["inverse chains containing direct endpoint or repeated-momentum contractions are not retained",
  AllTrue[{
      SpinorChain[Spinor["Spin", "Angle", 1], Mom[1],
        Spinor["Spin", "Square", 2]]^-1,
      SpinorChain[Spinor["Spin", "Square", 1], Mom[2],
        Spinor["Spin", "Angle", 2]]^-1,
      SpinorChain[Spinor["Spin", "Angle", 1], Mom[3], Mom[3],
        Spinor["Spin", "Angle", 2]]^-1},
    !ConstructiveDiagrams`Private`preparationCompleteExternalInputQ[#, fsMixedRules] &]];
check["inverse trace retention still requires elementary trace contractions",
  AllTrue[{SpinorTrace[Mom[1], Mom[1]]^-1,
      SpinorTrace[Mom[1], Mom[2], Mom[3]]^-1,
      SpinorTrace[Mom[1], Mom[2], Mom[2], Mom[3]]^-1},
    !ConstructiveDiagrams`Private`preparationCompleteExternalInputQ[#, fsMixedRules] &] &&
    ConstructiveDiagrams`Private`preparationCompleteExternalInputQ[
      SpinorTrace[Mom[1], Mom[2], Mom[3], Mom[4]]^-1, fsMixedRules]];

(* A tiny factored input can represent exponentially many expanded terms. *)
fsLargeBase = fsA + fsB + fsC;
fsLarge = fsLargeBase^2000/fsD[{1, 3}];
fsProfile = ConstructiveDiagrams`Private`rspExpansionProfile[fsLarge];
check["the expansion estimator saturates without constructing huge products",
  fsProfile[[1]] > 4096 && fsProfile[[2]] > 24000];
fsLargeReduced = TimeConstrained[Block[{$RecursionLimit = 256}, ReduceSpinorProducts[fsLarge]], 10, $TimedOut];
check["a large factored product returns intact below a low recursion bound",
  fsLargeReduced === fsLarge && FreeQ[fsLargeReduced, _TerminatedEvaluation | $Aborted | $Failed]];
fsLargeDirac = fsLargeBase^2000 SpinorChain[Spinor["Spin", "Angle", 1], Mom[1],
  Spinor["Spin", "Square", 2]]/fsD[{1, 3}];
fsLargeDiracExpected = fsLargeBase^2000 Mass[1] SpinorChain[
  Spinor["Spin", "Square", 1], Spinor["Spin", "Square", 2]]/fsD[{1, 3}];
check["the expansion guard still performs a direct external Dirac contraction",
  TimeConstrained[ReduceSpinorProducts[fsLargeDirac], 10, $TimedOut] === fsLargeDiracExpected];
check["the sum compactor declines an oversized prospective distribution",
  fsCompact[fsLarge] === fsLarge];

If[checks =!= 79, AppendTo[failures, "The suite did not execute all 79 checks."]];
Print[checks, " final-amplitude simplification checks; ", Length[failures], " failures."];
If[failures =!= {}, Print[failures]; Quit[1]];
Print["HEPCAT_TEST_COMPLETED:final-amplitude-simplification-tests.wl"];
Quit[0];
