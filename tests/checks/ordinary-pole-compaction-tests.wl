(* Ordinary off-shell virtuality identities retain every quotient/remainder. *)
root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Global`$HEPCATpath = FileNameJoin[{root, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
$HistoryLength = 0; checks = 0; failures = {};
SetAttributes[check, HoldRest];
check[name_, expression_] := (checks++;
  If[!TrueQ[expression], AppendTo[failures, name]; Print["FAIL: ", name]]);
opDefinitions = ConstructiveDiagrams`Private`preparationPropagatorDefinitions;
opTrials = ConstructiveDiagrams`Private`preparationVirtualityTrials;
opBlock = ConstructiveDiagrams`Private`preparationCrossDenominatorBlock;
opCompact = ConstructiveDiagrams`Private`preparationOrdinaryPoleCompaction;
check["equal weighted complexity prefers fewer expression leaves over one dense fraction",
  ConstructiveDiagrams`Private`preparationBetterQ[
    {71, 7, 318, "SpinorExpressionComplexity"},
    {71, 1, 647, "SpinorExpressionComplexity"}] &&
  !ConstructiveDiagrams`Private`preparationBetterQ[
    {71, 1, 647, "SpinorExpressionComplexity"},
    {71, 7, 318, "SpinorExpressionComplexity"}]];
check["weighted complexity remains the primary simplification objective",
  ConstructiveDiagrams`Private`preparationBetterQ[
    {70, 7, 900, "SpinorExpressionComplexity"},
    {71, 1, 318, "SpinorExpressionComplexity"}]];
check["additive groups break an otherwise exact score and size tie",
  ConstructiveDiagrams`Private`preparationBetterQ[
    {71, 3, 318, "SpinorExpressionComplexity"},
    {71, 7, 318, "SpinorExpressionComplexity"}] &&
  !ConstructiveDiagrams`Private`preparationBetterQ[
    {71, 3, 318, "SpinorExpressionComplexity"},
    {71, 3, 318, "SpinorExpressionComplexity"}]];
check["different measured and fallback metrics remain incomparable",
  !ConstructiveDiagrams`Private`preparationBetterQ[
    {71, 1, 100, "LeafCountFallback"},
    {71, 7, 318, "SpinorExpressionComplexity"}]];
check["legacy three-field ranks use the same size-first tie policy",
  ConstructiveDiagrams`Private`preparationBetterQ[{71, 7, 318}, {71, 1, 647}]];
opRules = Table[Mass[i] -> Symbol["opMass" <> ToString[i]], {i, 6}];
opDot[i_, j_] := MomProd @@ Sort[{i, j}];
opProp[legs_List, mass_] := PropDen[Mom[Multiparticle @@ legs], mass];
opT = opProp[{1, 2, 3}, opTripleMass];
opA = opProp[{1, 2}, opPairMass12]; opB = opProp[{1, 3}, opPairMass13];
opC = opProp[{1, 4}, opPairMass14]; opInventory = {opT, opA, opB, opC};
opRight = opA + opB + 2 opDot[2, 3] - opMass1^2 +
  opPairMass12^2 + opPairMass13^2 - opTripleMass^2;
opExpression = opCoefficient (opRight + opOffset)/(opT opC);
opResult = opBlock[opExpression, opT, opRules, opInventory, False, 3];
check["the triple virtuality definition is exact for unequal masses",
  Expand[opDefinitions[opRight - opT, opRules]] === 0];
check["a regular quotient and a nonzero pole remainder are both retained",
  Together[opResult["Amplitude"] - opCoefficient/opC - opCoefficient opOffset/(opT opC)] === 0];
check["virtuality division applies no internal mass shell",
  opResult["InternalOnShellConditions"] === {} &&
    FreeQ[opResult["Amplitude"], Mass[_Multiparticle]]];
opSingleton = opCompact[opExpression, opRules, False, 3];
check["one fraction receives the same complete pole-block reduction",
  opSingleton["Status"] === "IMPROVED" &&
    Together[opDefinitions[opSingleton["Amplitude"] - opExpression, opRules]] === 0 &&
    ConstructiveDiagrams`Private`preparationBetterQ[
      opSingleton["Rank"], ConstructiveDiagrams`Private`preparationRank[opExpression]]];
check["single-fraction compaction retains its regular quotient and pole remainder",
  Together[opSingleton["Amplitude"] - opCoefficient/opC -
    opCoefficient opOffset/(opT opC)] === 0 &&
    opSingleton["InternalOnShellConditions"] === {}];
opGenerated = opTrials[opExpression, opRules, opInventory, False];
check["every direct and centered substitution has an exact polynomial certificate",
  opGenerated =!= {} && AllTrue[opGenerated,
    Expand[opDefinitions[#["Identity"], opRules]] === 0 &]];
opUnknownRules = DeleteCases[opRules, HoldPattern[Mass[1] -> _]];
opUnknownRight = opRight + opMass1^2 - opDot[1, 1];
check["an undeclared center virtuality is kept as a scalar product",
  Expand[opDefinitions[opUnknownRight - opT, opUnknownRules]] === 0];
opT4 = opProp[{1, 2, 3, 4}, opQuadrupleMass];
opRight4 = opA + opB + opC + 2 (opDot[2, 3] + opDot[2, 4] + opDot[3, 4]) -
  2 opMass1^2 + opPairMass12^2 + opPairMass13^2 + opPairMass14^2 - opQuadrupleMass^2;
check["the centered identity extends beyond three legs with arbitrary masses",
  Expand[opDefinitions[opRight4 - opT4, opRules]] === 0];
opComplement = opProp[{4, 5, 6}, opTripleMass];
opCompOff = opTrials[opRight/(opComplement opC), opRules,
  {opComplement, opA, opB, opC}, False];
opCompOn = opTrials[opRight/(opComplement opC), opRules,
  {opComplement, opA, opB, opC}, True];
check["complementary momentum sets are never inferred with conservation disabled",
  NoneTrue[opCompOff, TrueQ[#["MomentumConservationUsed"]] &]];
opComplementTrials = Select[opCompOn,
  #["Kind"] === "CenteredPairDefinition" && #["Legs"] === {1, 2, 3} &];
check["conservation explicitly enables the complementary representation",
  opComplementTrials =!= {} && AllTrue[opComplementTrials,
    TrueQ[#["MomentumConservationUsed"]] &]];
opMatrices = AssociationThread[Range[5],
  Table[Array[Function[{i, j}, Unique["opComponent"]], {2, 2}], {5}]];
opMatrices[6] = -Total[Values[opMatrices]];
opAdj[value_] := {{value[[2, 2]], -value[[1, 2]]}, {-value[[2, 1]], value[[1, 1]]}};
opMassSubstitutions = Table[(Mass[i] /. opRules)^2 -> Det[opMatrices[i]], {i, 6}];
check["complement identities hold for five arbitrary independent momentum matrices",
  AllTrue[opComplementTrials, Expand[opDefinitions[#["Identity"], opRules] /.
    opMassSubstitutions /. MomProd[i_Integer, j_Integer] :>
      Tr[opMatrices[i].opAdj[opMatrices[j]]]/2] === 0 &]];
check["no propagator outside the input inventory is introduced",
  ContainsAll[opInventory, DeleteDuplicates[Cases[opResult["Amplitude"], _PropDen, Infinity]]]];
opHuge = (opA + opB)^100000;
check["large factored powers are rejected before expansion",
  opCompact[opHuge, opRules, True, 1]["Amplitude"] === opHuge];
check["a zero budget returns the original expression",
  opCompact[opExpression, opRules, True, 0]["Amplitude"] === opExpression];
check["noncommutative coefficients stay untouched",
  opCompact[opExpression Dot[opMatrixA, opMatrixB], opRules, True, 1]["Amplitude"] ===
    opExpression Dot[opMatrixA, opMatrixB]];
check["conflicting mass declarations cannot license a reduction",
  opCompact[opExpression, Append[opRules, Mass[1] -> opOtherMass], True, 1]["Amplitude"] === opExpression];
opNonscalarRules = Append[Rest[opRules], Mass[1] -> {opMass1, opOtherMass}];
check["nonscalar external masses leave direct propagator definitions untouched",
 opDefinitions[opExpression, opNonscalarRules] === opExpression];
check["nonscalar external masses generate no virtuality substitutions",
 opTrials[opExpression, opNonscalarRules, opInventory, True] === {}];
check["nonscalar external masses cannot license cross-denominator division",
 opBlock[opExpression, opT, opNonscalarRules, opInventory, True, 1]["Amplitude"] === opExpression];
check["nonscalar external masses cannot license whole-amplitude compaction",
 opCompact[opExpression, opNonscalarRules, True, 1]["Amplitude"] === opExpression];
opConflictingRules = Append[opRules, Mass[1] -> opOtherMass];
check["conflicting external masses also leave direct definitions and trials untouched",
 opDefinitions[opExpression, opConflictingRules] === opExpression &&
 opTrials[opExpression, opConflictingRules, opInventory, False] === {}];
opRepeatedRules = Append[opRules, First[opRules]];
check["identical repeated external mass entries do not change an ordinary definition",
 Expand[opDefinitions[opRight - opT, opRepeatedRules]] === 0];
opInternalMetadata = Join[opRules, {Mass[Multiparticle[1, 3]] -> {opMass1, opOtherMass},
 Mass[Multiparticle[1, 3]] -> opAnotherInternalMass}];
check["internal mass metadata cannot enter these external-only identities",
 Expand[opDefinitions[opRight - opT, opInternalMetadata]] === 0 &&
 opTrials[opExpression, opInternalMetadata, opInventory, False] === opGenerated &&
 Together[opBlock[opExpression, opT, opInternalMetadata, opInventory, False, 3]["Amplitude"] -
   opResult["Amplitude"]] === 0];

(* Algebraic collection preserves rational coefficients and existing inverses. *)
opCollect = ConstructiveDiagrams`Private`preparationCollectRegularRemainders;
opInverseBase = SpinorChain[Spinor["Helicity", "Angle", 3], Mom[1],
    Spinor["Helicity", "Square", 4]] +
  SpinorChain[Spinor["Helicity", "Angle", 3], Mom[2], Spinor["Helicity", "Square", 4]];
opCarrier = SpinorChain[Spinor["Helicity", "Angle", 5], Spinor["Helicity", "Angle", 6]];
opRationalInput = opNumeratorA opCarrier/(opA opInverseBase) +
  opNumeratorB opCarrier/(opB opInverseBase);
opRationalReport = opCollect[opRationalInput, 3];
check["regular collection keeps exact rational equality",
  Together[opRationalReport["Amplitude"] - opRationalInput] === 0];
check["regular collection preserves a preexisting inverse spinor factor",
  !FreeQ[opRationalReport["Amplitude"], Power[opInverseBase, -1]] &&
  ContainsAll[{opA, opB}, DeleteDuplicates[Cases[opRationalReport["Amplitude"], _PropDen, Infinity]]]];
opUnfinishedInput = opRationalInput + $TimedOut;
opUnfinished = opCollect[opUnfinishedInput, 1];
check["an unfinished rational calculation cannot become an accepted improvement",
  opUnfinished["Status"] =!= "IMPROVED" && opUnfinished["Amplitude"] === opUnfinishedInput];

(* Presentation compaction is exact rational algebra on independent spinor and
   propagator atoms. This fixture is the seven-term mixed five-point result. *)
opDisplay = ConstructiveDiagrams`Private`preparationPoleDisplayGrouping;
opDisplayMass = ModelParameter["displayMass", "ordinary-pole-tests", "Real"];
opDisplayCharge = ModelParameter["displayCharge", "ordinary-pole-tests", "Real"];
opDisplaySpinor[i_, chirality_] := Spinor[If[i <= 2, "Spin", "Helicity"], chirality, i];
opDisplayAngle[i_, j_] := SpinorChain[opDisplaySpinor[i, "Angle"], opDisplaySpinor[j, "Angle"]];
opDisplaySquare[i_, j_] := SpinorChain[opDisplaySpinor[i, "Square"], opDisplaySpinor[j, "Square"]];
opDisplayChain[i_] := SpinorChain[opDisplaySpinor[i, "Square"], Mom[1], opDisplaySpinor[5, "Angle"]];
opDisplayDouble[i_] := SpinorChain[opDisplaySpinor[i, If[i === 5, "Angle", "Square"]],
  Mom[1], Mom[2], opDisplaySpinor[i, If[i === 5, "Angle", "Square"]]];
opDisplayDen[i_, j_] := PropDen[Mom[Multiparticle @@ Sort[{i, j}]], opDisplayMass];
opDisplayRules = {Mass[1] -> opDisplayMass, Mass[2] -> opDisplayMass,
  Mass[3] -> 0, Mass[4] -> 0, Mass[5] -> 0};
opDisplayInput = 2 Sqrt[2] opDisplayCharge^3/(opDisplayDen[1, 3] opDisplayDen[1, 4]) * (
  opDisplayMass opDisplayAngle[1, 5] opDisplayAngle[2, 5] opDisplaySquare[3, 4]^2/opDisplayDen[2, 5] +
  (opDisplayMass opDisplayAngle[1, 5] opDisplayAngle[2, 5] opDisplaySquare[3, 4]^2 -
   opDisplayAngle[1, 5] opDisplayAngle[2, 5] opDisplaySquare[4, 5] opDisplayChain[4] opDisplayDouble[3]/
      (opDisplayMass opDisplayDen[2, 3]) +
   opDisplayAngle[1, 2] opDisplayChain[4]^2 opDisplayDouble[3]/(opDisplayMass opDisplayDen[2, 3]) -
   opDisplayAngle[1, 5] opDisplayAngle[2, 5] opDisplaySquare[3, 5] opDisplayChain[3] opDisplayDouble[4]/
      (opDisplayMass opDisplayDen[2, 4]) +
   opDisplayAngle[1, 2] opDisplayChain[3]^2 opDisplayDouble[4]/(opDisplayMass opDisplayDen[2, 4]) +
   opDisplayMass opDisplayAngle[1, 2] opDisplaySquare[3, 4]^2 opDisplayDouble[5]/opDisplayDen[2, 5])/
    opDisplayDen[1, 5]);
opDisplayReport = opDisplay[opDisplayInput, opDisplayRules, 1];
opDisplayResult = opDisplayReport["Amplitude"];
check["pole-star grouping preserves weighted complexity while shortening the actual mixed result",
  SpinorExpressionComplexity[opDisplayInput] === 71 && LeafCount[opDisplayInput] === 423 &&
  opDisplayReport["Status"] === "IMPROVED" && SpinorExpressionComplexity[opDisplayResult] === 71 &&
  LeafCount[opDisplayResult] <= 320];
check["display grouping is an exact identity with no kinematic assumptions",
  Together[opDisplayResult - opDisplayInput] === 0 && opDisplayReport["InternalOnShellConditions"] === {}];
check["the accepted display route retains its independent-atom equality certificate",
  Length[opDisplayReport["Route"]] === 1 &&
  First[opDisplayReport["Route"]]["After"] === opDisplayResult &&
  First[opDisplayReport["Route"]]["Before"] === opDisplayInput &&
  First[opDisplayReport["Route"]]["EquivalenceProof"]["Method"] ===
    "ExactIndependentAtomRationalIdentity" &&
  First[opDisplayReport["Route"]]["EquivalenceProof"]["NormalForm"] === 0];
opDisplayProps = DeleteDuplicates[Cases[opDisplayInput, _PropDen, Infinity]];
check["presentation does not manufacture a new physical propagator",
  ContainsAll[opDisplayProps, DeleteDuplicates[Cases[opDisplayResult, _PropDen, Infinity]]]];
opDisplayLabels = AssociationThread[Range[5], {8, 3, 11, 7, 2}];
opRelabel = ConstructiveDiagrams`Private`relabelLegs;
opRelabeledInput = opRelabel[opDisplayInput, opDisplayLabels];
opRelabeledRules = opRelabel[opDisplayRules, opDisplayLabels];
opRelabeledReport = opDisplay[opRelabeledInput, opRelabeledRules, 1];
check["pole stars do not depend on a fixed leg number or ordering",
  opRelabeledReport["Status"] === "IMPROVED" &&
  Together[opRelabeledReport["Amplitude"] - opRelabeledInput] === 0 &&
  LeafCount[opRelabeledReport["Amplitude"]] < LeafCount[opRelabeledInput]];
opDisplayInverse = opDisplayAngle[3, 4] + opDisplayAngle[3, 5];
opDisplayInverseInput = opDisplayInput/(opInverseCoefficient^2 opDisplayInverse^2);
opDisplayInverseReport = opDisplay[opDisplayInverseInput, opDisplayRules, 1];
check["inverse spinor sums and scalar coefficient powers are retained exactly",
  Together[opDisplayInverseReport["Amplitude"] - opDisplayInverseInput] === 0 &&
  !FreeQ[opDisplayInverseReport["Amplitude"], Power[opDisplayInverse, -2]] &&
  !FreeQ[opDisplayInverseReport["Amplitude"], Power[opInverseCoefficient, -2]] &&
  Together[opInverseCoefficient D[opDisplayInverseReport["Amplitude"], opInverseCoefficient] +
    2 opDisplayInverseReport["Amplitude"]] === 0];
check["no new singular spinor factor is introduced by rational presentation",
  AllTrue[Cases[opDisplayInverseReport["Amplitude"],
    Power[base_, exponent_] /; !TrueQ[exponent >= 0] && !FreeQ[base, _SpinorChain] :> base,
    Infinity], # === opDisplayInverse &]];
opOtherParameter = ModelParameter["displayMass", "another-model", "Real"];
opParameterInput = opDisplayInput /. opDisplayMass -> opOtherParameter;
opParameterReport = opDisplay[opParameterInput, opDisplayRules, 1];
check["equally named parameters from different models remain distinct scalar atoms",
  Together[opParameterReport["Amplitude"] - opParameterInput] === 0 &&
    FreeQ[opParameterReport["Amplitude"], opDisplayMass] &&
    !FreeQ[opParameterReport["Amplitude"], opOtherParameter]];
opCompoundInput = opDisplayInput /. opDisplayDen[1, 3] ->
  PropDen[Mom[Multiparticle[1, 3, 4]], opAnotherInternalMass];
opCompoundReport = opDisplay[opCompoundInput, opDisplayRules, 1];
check["the same algebra accepts compound physical cuts and unrelated internal masses",
  Together[opCompoundReport["Amplitude"] - opCompoundInput] === 0 &&
  ContainsAll[DeleteDuplicates[Cases[opCompoundInput, _PropDen, Infinity]],
    DeleteDuplicates[Cases[opCompoundReport["Amplitude"], _PropDen, Infinity]]]];
check["zero amplitudes and zero coefficients remain zero",
  opDisplay[0 opDisplayInput, opDisplayRules, 1]["Amplitude"] === 0];
check["a disabled presentation allowance returns the exact input",
  opDisplay[opDisplayInput, opDisplayRules, 0]["Amplitude"] === opDisplayInput];
opProfileGuardTouchedRank = False;
opDisplayHuge = (opDisplayAngle[1, 3] + opDisplayAngle[2, 4])^100000/opDisplayDen[1, 3];
opDisplayHugeReport = Block[{ConstructiveDiagrams`Private`preparationRank},
  ConstructiveDiagrams`Private`preparationRank[___] := (opProfileGuardTouchedRank = True; {0, 0, 0, "fake"});
  opDisplay[opDisplayHuge, opDisplayRules, 1]];
check["huge compact positive powers are rejected before scoring or expansion",
  opDisplayHugeReport["Amplitude"] === opDisplayHuge && !opProfileGuardTouchedRank &&
  opDisplayHugeReport["Expansions"] === 0 && opDisplayHugeReport["StopReason"] === "ExpansionLimit"];
opDisplayMonomial = opDisplayChain[3]^100000/opDisplayDen[1, 3];
opMonomialReport = opDisplay[opDisplayMonomial, opDisplayRules, 0.05];
check["a huge monomial degree retains the input within the bounded fallback",
  opMonomialReport["Amplitude"] === opDisplayMonomial && opMonomialReport["Status"] === "UNCHANGED"];
opDisplayTimeout = opDisplay[opDisplayInput, opDisplayRules, 0.000001];
check["an interrupted display search keeps an expression with a complete proof checkpoint",
  Together[opDisplayTimeout["Amplitude"] - opDisplayInput] === 0 &&
  (opDisplayTimeout["Status"] === "UNCHANGED" ||
    (Length[opDisplayTimeout["Route"]] === 1 &&
      First[opDisplayTimeout["Route"]]["After"] === opDisplayTimeout["Amplitude"] &&
      First[opDisplayTimeout["Route"]]["EquivalenceProof"]["NormalForm"] === 0))];
check["nonfinite inputs cannot become a display improvement",
  With[{report = opDisplay[opDisplayInput + $TimedOut, opDisplayRules, 1]},
    report["Status"] === "UNCHANGED" && report["Amplitude"] === opDisplayInput + $TimedOut]];

(* A synthetic four-block tensor, with unrelated exchange and radiation
   masses, exercises denominator partitioning without any model lookup or
   process-specific identity. Its dense fraction must return to readable blocks. *)
opPairedRules = Join[Table[Mass[i] -> opPairedMass, {i, 4}], {Mass[5] -> 0}];
opPairedSpin[i_, kind_] := Spinor[If[i === 5, "Helicity", "Spin"], kind, i];
opPairedAngle[i_, j_] := SpinorChain[opPairedSpin[i, "Angle"], opPairedSpin[j, "Angle"]];
opPairedSquare[i_, j_] := SpinorChain[opPairedSpin[i, "Square"], opPairedSpin[j, "Square"]];
opPairedExchange[i_, j_] := PropDen[Mom[Multiparticle @@ Sort[{i, j}]], opExchangeMass];
opPairedRadiation[i_] := PropDen[Mom[Multiparticle[i, 5]], opRadiationMass];
opPairedBlock[{a_, b_, c_, d_}] := (
  opPairedSquare[a, 5] (opPairedAngle[b, c] opPairedSquare[d, 5] +
    opPairedAngle[b, d] opPairedSquare[c, 5])/opPairedRadiation[a] -
  opPairedSquare[b, 5] (opPairedAngle[a, c] opPairedSquare[d, 5] +
    opPairedAngle[a, d] opPairedSquare[c, 5])/opPairedRadiation[b] +
  SpinorChain[opPairedSpin[5, "Square"], Mom[b], Mom[a], opPairedSpin[5, "Square"]] *
    (opPairedAngle[b, c] opPairedSquare[a, d] + opPairedAngle[b, d] opPairedSquare[a, c] +
      opPairedAngle[a, d] opPairedSquare[b, c] + opPairedAngle[a, c] opPairedSquare[b, d])/
    (opPairedRadiation[a] opPairedRadiation[b]))/opPairedExchange[c, d];
opPairedForm = opPairedCharge^3 Total[opPairedBlock /@
  {{1, 2, 3, 4}, {2, 4, 1, 3}, {1, 3, 2, 4}, {3, 4, 1, 2}}];
opPairedDense = Together[opPairedForm];
opPairedReport = opDisplay[opPairedDense, opPairedRules, 1];
check["hierarchical grouping restores compact exchange blocks from a dense fraction",
  opPairedReport["Status"] === "IMPROVED" &&
    LeafCount[opPairedReport["Amplitude"]] < 1000 &&
    LeafCount[opPairedDense] > 2000 &&
    MemberQ[Lookup[opPairedReport["Route"], "GroupingMode"], "HierarchicalPolePartition"]];
check["denominator partitioning changes neither the full tensor nor weighted complexity",
  Together[opPairedReport["Amplitude"] - opPairedDense] === 0 &&
    SpinorExpressionComplexity[opPairedReport["Amplitude"]] === 160 &&
    opPairedReport["InternalOnShellConditions"] === {}];
check["the grouping checkpoint contains a complete independent-atom proof",
  Length[opPairedReport["Route"]] === 1 &&
    First[opPairedReport["Route"]]["Before"] === opPairedDense &&
    First[opPairedReport["Route"]]["After"] === opPairedReport["Amplitude"] &&
    First[opPairedReport["Route"]]["EquivalenceProof"]["Method"] ===
      "ExactIndependentAtomRationalIdentity" &&
    First[opPairedReport["Route"]]["EquivalenceProof"]["NormalForm"] === 0 &&
    opPairedReport["Candidates"] <= 12];
opPairedPoles = DeleteDuplicates[Cases[opPairedDense, _PropDen, Infinity]];
check["both unrelated propagator mass classes and the original pole inventory survive",
  Sort[DeleteDuplicates[Cases[opPairedReport["Amplitude"], _PropDen, Infinity]]] ===
      Sort[opPairedPoles] &&
    !FreeQ[opPairedReport["Amplitude"], opExchangeMass] &&
    !FreeQ[opPairedReport["Amplitude"], opRadiationMass]];
opPairedRelabeled = opRelabel[opPairedDense, opDisplayLabels];
opPairedRelabeledRules = opRelabel[opPairedRules, opDisplayLabels];
opPairedRelabeledReport = opDisplay[opPairedRelabeled, opPairedRelabeledRules, 1];
check["hierarchical pole partitions work after nonconsecutive particle relabeling",
  opPairedRelabeledReport["Status"] === "IMPROVED" &&
    Together[opPairedRelabeledReport["Amplitude"] - opPairedRelabeled] === 0 &&
    LeafCount[opPairedRelabeledReport["Amplitude"]] < 1000];
opPairedIndexed = opPairedDense /. Spinor["Spin", kind_, leg_Integer] :>
  Spinor["Spin", If[OddQ[leg], "Upper", "Lower"], kind, leg, opPairedIndex[leg]];
opPairedIndexedReport = opDisplay[opPairedIndexed, opPairedRules, 1];
check["factorization of pole blocks retains every explicit spin slot and its variance",
  Together[opPairedIndexedReport["Amplitude"] - opPairedIndexed] === 0 &&
    Sort[DeleteDuplicates[Cases[opPairedIndexedReport["Amplitude"],
      Spinor["Spin", _, _, _, _], Infinity]]] ===
    Sort[DeleteDuplicates[Cases[opPairedIndexed, Spinor["Spin", _, _, _, _], Infinity]]]];
opPairedRegular = opPairedDense + opPairedContact;
opPairedRegularReport = opDisplay[opPairedRegular, opPairedRules, 1];
check["a pole-free spectator is retained while exchange blocks are regrouped",
  Together[opPairedRegularReport["Amplitude"] - opPairedRegular] === 0 &&
    Together[D[opPairedRegularReport["Amplitude"], opPairedContact]] === 1];
check["sign placement removes redundant wrappers while retaining the paired-current tree",
  LeafCount[opPairedReport["Amplitude"]] <= LeafCount[opPairedForm] &&
    AnyTrue[opPairedReport["Route"], TrueQ[Lookup[#, "SignPlacementChanged", False]] &]];
opPairedAgain = opDisplay[opPairedReport["Amplitude"], opPairedRules, 1];
check["a second bounded grouping pass retains the same sign-normalized expression",
  opPairedAgain["Amplitude"] === opPairedReport["Amplitude"]];
check["nonconsecutive labels reach the same sign-normalized presentation size",
  LeafCount[opPairedRelabeledReport["Amplitude"]] <= LeafCount[opPairedForm]];
opPairedInverse = opPairedAngle[1, 5] + opPairedAngle[2, 5];
opPairedPowerInput = -opPairedDense opPairedAngle[1, 5]^2/
  (opPairedInverse^3 opPairedInverseParameter^2);
opPairedPowerReport = opDisplay[opPairedPowerInput, opPairedRules, 1];
check["sign placement preserves a positive chain power and complete inverse powers",
  opPairedPowerReport["Status"] === "IMPROVED" &&
    Together[opPairedPowerReport["Amplitude"] - opPairedPowerInput] === 0 &&
    !FreeQ[opPairedPowerReport["Amplitude"], Power[opPairedAngle[1, 5], 2]] &&
    !FreeQ[opPairedPowerReport["Amplitude"], Power[opPairedInverse, -3]] &&
    !FreeQ[opPairedPowerReport["Amplitude"], Power[opPairedInverseParameter, -2]]];
check["the sign optimizer does not inspect or split a protected inverse spinor sum",
  AllTrue[Cases[opPairedPowerReport["Amplitude"],
    Power[base_, exponent_] /; !TrueQ[exponent >= 0] && !FreeQ[base, _SpinorChain] :> base,
    Infinity], # === opPairedInverse &] &&
    First[opPairedPowerReport["Route"]]["EquivalenceProof"]["NormalForm"] === 0];

(* Start with the earlier validated expression, not a fitted answer or /tmp cache.
   The model-specific functions below are example fixtures only. The function
   under test remains the process-independent ordinary compactor. *)
Get[FileNameJoin[{root, "tests", "QED-5-point-residues.wl"}]];
opQEDModel = ReadModel[FileNameJoin[{root, "models"}], 3];
opModelHash = Hash[opQEDModel, "SHA256"];
opPrevious = QEDSixPointPPPMPreviousExample[opQEDModel];
opCompactTarget = QEDSixPointPPPMCompactExample[opQEDModel];
opPreviousHash = Hash[opPrevious, "SHA256"];
opEarlyPreparation = PrepareAmplitudes[opPrevious, opQEDModel,
  "PolePreference" -> None, "InternalOnShell" -> False, "MaxSteps" -> 0,
  "TimeConstraint" -> 6, "Parallelization" -> False,
  "ReturnDetails" -> True, "ProgressReporting" -> False];
opEarlyReductions = First[opEarlyPreparation["ItemReports"]]["Reductions"];
check["completed external inputs retain an early original grouped-pole competitor",
  AnyTrue[opEarlyReductions,
    Lookup[#, "Operation", None] === "OriginalGroupedOrdinaryPoleCompaction" &&
      TrueQ[Lookup[#, "AlternativeRoute", False]] &&
      Lookup[#, "OnShellChannels", None] === {} &&
      !TrueQ[Lookup[#, "InternalOnShellIdentity", True]] &]];
check["the bounded early portfolio never replaces its input by a worse complete checkpoint",
  With[{inputRank = ConstructiveDiagrams`Private`preparationRank[Last[opPrevious]],
      outputRank = ConstructiveDiagrams`Private`preparationRank[opEarlyPreparation["Amplitude"]]},
    outputRank === inputRank ||
      ConstructiveDiagrams`Private`preparationBetterQ[outputRank, inputRank]] &&
    First[opEarlyPreparation["ItemReports"]]["InternalOnShellConditions"] === {}];
check["sorting expression candidates agrees with the preparation rank comparison",
  ConstructiveDiagrams`Private`preparationSortKey[Last[opPrevious]] ===
    ConstructiveDiagrams`Private`preparationRank[Last[opPrevious]][[{1, 3, 2, 4}]]];
check["the worked regression starts with the earlier eighteen-term numerator form",
  Head[Last[opPrevious]] === Plus && Length[Last[opPrevious]] === 18 &&
    SpinorExpressionComplexity[Last[opPrevious]] === 1128];
opEndToEnd = opCompact[Last[opPrevious], First[opPrevious], True, 30];
opEndAmplitude = opEndToEnd["Amplitude"];
opEndRoute = Lookup[opEndToEnd, "Route", {}];
check["general compaction improves the complete six-point expression",
  opEndToEnd["Status"] === "IMPROVED" &&
    SpinorExpressionComplexity[opEndAmplitude] <= 327];
check["general compaction exposes seven compact groups",
  Head[opEndAmplitude] === Plus && Length[opEndAmplitude] === 7 &&
    LeafCount[opEndAmplitude] < 2100];
opEndDifference = ConstructiveDiagrams`Private`withProtectedPropagators[
  ConstructiveDiagrams`Private`canonicalChains[opEndAmplitude - Last[opCompactTarget]], Together];
check["the generic result equals the independent compact fixture by ordinary formal algebra",
  opEndDifference === 0];
check["six-point compaction applies no internal-shell condition",
  opEndToEnd["InternalOnShellConditions"] === {} &&
    AllTrue[Cases[{opEndRoute}, record_Association :>
      Lookup[record, "InternalOnShellConditions", {}], Infinity], # === {} &] &&
    FreeQ[opEndAmplitude, Mass[_Multiparticle]]];
opOriginalProps = DeleteDuplicates[Cases[Last[opPrevious], _PropDen, Infinity]];
check["six-point compaction introduces no new physical denominator or reference spinor",
  ContainsAll[opOriginalProps, DeleteDuplicates[Cases[opEndAmplitude, _PropDen, Infinity]]] &&
    FreeQ[opEndAmplitude, Spinor["Xi", ___] | SpinorHat["Xi", ___] | _xFactor | _xTildeFactor]];
opEndOperations = Cases[{opEndRoute}, record_Association :> Lookup[record, "Operation", Nothing], Infinity];
check["the actual route includes reverse normalization",
  MemberQ[opEndOperations, "ReverseNormalizationFactors"]];
check["the actual route contains nonempty determinant-pair edits",
  Cases[opEndRoute, record_Association /;
    Lookup[record, "Operation", None] === "DeterminantPairs" &&
      Length[Lookup[record, "Edits", {}]] > 0, Infinity] =!= {}];
check["the actual route uses scalar Schouten relations",
  MemberQ[opEndOperations, "GeneralizedScalarSchouten"] &&
    Cases[{opEndRoute}, record_Association /; KeyExistsQ[record, "Pivot"] &&
      KeyExistsQ[record, "Cofactor"], Infinity] =!= {}];
check["the actual route records both quotient and remainder of exact virtuality division",
  Cases[opEndRoute, record_Association /;
    Lookup[record, "Operation", None] === "ExactVirtualityDivision" &&
      KeyExistsQ[record, "Quotient"] && KeyExistsQ[record, "Remainder"], Infinity] =!= {}];
check["the model and earlier amplitude remain unchanged",
  Hash[opQEDModel, "SHA256"] === opModelHash && Hash[opPrevious, "SHA256"] === opPreviousHash];
opRepeatedCompaction = opCompact[opEndAmplitude, First[opPrevious], True, 10];
check["the completed compact expression is idempotent under another bounded pass",
  opRepeatedCompaction["Amplitude"] === opEndAmplitude];

Print["Ordinary pole compaction: ", checks, " checks; ", Length[failures], " failures."];
If[failures === {}, Print["HEPCAT_TEST_COMPLETED:ordinary-pole-compaction-tests.wl"]; Quit[0], Quit[1]];
