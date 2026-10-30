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

(* Start with the earlier validated expression, not a fitted answer or /tmp cache.
   The model-specific functions below are example fixtures only. The function
   under test remains the process-independent ordinary compactor. *)
Get[FileNameJoin[{root, "tests", "QED-5-point-residues.wl"}]];
opQEDModel = ReadModel[FileNameJoin[{root, "models"}], 3];
opModelHash = Hash[opQEDModel, "SHA256"];
opPrevious = QEDSixPointPPPMPreviousExample[opQEDModel];
opCompactTarget = QEDSixPointPPPMCompactExample[opQEDModel];
opPreviousHash = Hash[opPrevious, "SHA256"];
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
