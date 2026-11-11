(* Portable exact checks for the two dense null-axis chirality choices. *)

(*
HEPCAT - High Energy Physics Constructive Amplitude Tool

Copyright (C) 2026 Neil Christensen

This file is part of HEPCAT.

HEPCAT is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

HEPCAT is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with HEPCAT.  If not, see <https://www.gnu.org/licenses/>.
*)


root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Global`$HEPCATpath = FileNameJoin[{root, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
$HistoryLength = 0; checks = 0; failures = {};
SetAttributes[check, HoldRest];
check[name_, value_] := (checks++; If[!TrueQ[value], AppendTo[failures, name]; Print["FAIL: ", name]]);
ndChart = ConstructiveDiagrams`Private`completionHERestNullSymbolicChart;
ndChoice = ConstructiveDiagrams`Private`completionHENativeNullAxisChoice;
ndPrepare = ConstructiveDiagrams`Private`completionHEPrepareNativeIdentity;
ndAdj = ConstructiveDiagrams`Private`residueAdjugate;
ndZero[value_] := TrueQ[Cancel[Together[value]] === 0];
ndRules = {Mass[1] -> ndMass, Mass[2] -> ndMass, Mass[3] -> ndMass,
  Mass[4] -> ndMass, Mass[5] -> 0, Mass[6] -> 0};
ndUnitRules = ndRules /. ndMass -> 1;
ndJ = {{0, 1}, {-1, 0}};
ndCharts = Association@Table[kind -> ndChart[ndRules, Automatic, kind], {kind, {"Angle", "Square"}}];
Do[
  ndCurrent = ndCharts[kind];
  check[kind <> " chart is constructed", AssociationQ[ndCurrent]];
  check[kind <> " shells are exact", AllTrue[Range[6], ndZero[
    Det[ndCurrent["Momenta"][#]] - (Mass[#] /. ndRules)^2] &]];
  check[kind <> " conservation is exact", AllTrue[Flatten[Total[Values[ndCurrent["Momenta"]]]], ndZero]];
  check[kind <> " null outer products are exact", AllTrue[{5, 6}, Function[leg,
    AllTrue[Flatten[Outer[Times, ndCurrent["Angle"][leg], ndCurrent["Square"][leg]] - ndCurrent["Momenta"][leg]], ndZero]]]];
  check[kind <> " massive bases and both Dirac orientations are exact", AllTrue[Range[4], Function[leg,
    With[{p = ndCurrent["Momenta"][leg], a = ndCurrent["AngleBasis"][leg],
      s = ndCurrent["SquareBasis"][leg], m = Mass[leg] /. ndRules},
      ndZero[Det[a] - m] && ndZero[Det[s] - m] &&
      AllTrue[Flatten[p.ndJ.s + m a], ndZero] &&
      AllTrue[Flatten[ndAdj[p].a + m ndJ.s], ndZero]]]]];
  check[kind <> " fixed null chirality has coordinate axes", ndCurrent[kind][5] === {1, 0} && ndCurrent[kind][6] === {0, 1}];
  check[kind <> " chart retains coherent coverage and assumptions", Length[ndCurrent["SpinVariables"]] === 4 &&
    Length[ndCurrent["ExternalIdentityCoordinates"]] === 9 &&
    DuplicateFreeQ[Join[ndCurrent["SpinVariables"], ndCurrent["ExternalIdentityCoordinates"]]] &&
    ndCurrent["NullAxisChirality"] === kind && ndCurrent["InternalOnShellConditions"] === {} &&
    ndCurrent["OriginalNullSpinorBracketRequiredNonzero"] === <|"Chirality" -> kind, "Legs" -> {5, 6}|>],
  {kind, {"Angle", "Square"}}];
ndCanonical[chart_] := KeyTake[chart, {"Momenta", "Angle", "Square", "AngleBasis", "SquareBasis"}] /.
  Thread[Join[chart["ExternalIdentityCoordinates"], chart["SpinVariables"]] ->
    Table[ndCoordinate[i], {i, Length[Join[chart["ExternalIdentityCoordinates"], chart["SpinVariables"]]]}]];
check["legacy default remains the Angle chart up to fresh symbols",
  ndCanonical[ndChart[ndRules]] === ndCanonical[ndCharts["Angle"]]];
check["invalid axis chirality is declined", ndChart[ndRules, Automatic, "Invalid"] === $Failed];
ndReversed = ndChart[ndUnitRules, {6, 5}, "Square"];
check["Square axes preserve requested null ordering", ndReversed["NullAxisLegs"] === {6, 5} &&
  ndReversed["Square"][6] === {1, 0} && ndReversed["Square"][5] === {0, 1}];
ndAngle = SpinorChain[Spinor["Helicity", "Angle", 5], Spinor["Helicity", "Angle", 6]];
ndSquare = SpinorChain[Spinor["Helicity", "Square", 5], Spinor["Helicity", "Square", 6]];
check["Square majority selects Square", ndChoice[ndSquare^2 ndAngle, ndRules]["Chirality"] === "Square"];
check["Angle majority selects Angle", ndChoice[ndAngle^2 ndSquare, ndRules]["Chirality"] === "Angle"];
check["ties retain Angle", ndChoice[ndAngle ndSquare, ndRules]["Chirality"] === "Angle"];
check["integer inverse powers count their full absolute multiplicity",
  ndChoice[ndAngle^2/ndSquare^3, ndRules]["Counts"] === <|"Angle" -> 4, "Square" -> 6|>];
check["Xi Zeta massive and undeclared legs do not influence the chooser",
  ndChoice[ndAngle Spinor["Xi", "Square", 5]^20 Spinor["Zeta", "Square", 6]^20
    Spinor["Spin", "Square", 1]^20 Spinor["Helicity", "Square", 99]^20, ndRules]["Counts"] ===
      <|"Angle" -> 2, "Square" -> 0|>];
check["declared null Spin and Helicity endpoints both count",
  ndChoice[Spinor["Spin", "Square", 5] Spinor["Helicity", "Square", 6], ndRules]["Counts"] ===
    <|"Angle" -> 0, "Square" -> 2|>];
ndCore = SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]
  SpinorChain[Spinor["Spin", "Angle", 3], Spinor["Spin", "Angle", 4]];
ndPole = PropDen[Mom[Multiparticle[1, 5]], ndMass];
ndPrepared = ndPrepare[ndSquare^2 ndCore/ndPole, ndRules, 5];
check["native preparation records and uses the typed Square choice", ndPrepared["Status"] === "PREPARED" &&
  ndPrepared["NullAxisChiralitySelection"]["Chirality"] === "Square" &&
  ndPrepared["Chart"]["NullAxisChirality"] === "Square"];
check["massive Helicity tags remain unsupported", ndPrepare[
  SpinorChain[Spinor["Helicity", "Angle", 1], Spinor["Spin", "Angle", 2]], ndRules, 2]["Status"] === "NOT_PROVED"];
check["fixed spin projections remain unsupported", ndPrepare[
  SpinorChain[Spinor["Spin", "Upper", "Angle", 1, 1/2], Spinor["Spin", "Angle", 2]], ndRules, 2]["Status"] === "NOT_PROVED"];
check["neither fixed null bracket can erase inhomogeneous little-group weights",
  AllTrue[{ndSquare/ndMass - 1, ndAngle/ndMass + 1},
    ndPrepare[#, ndRules, 2]["Status"] === "NOT_PROVED" &]];

(* The mixed chart fixes an angle line and a square line independently.
   Its exact tests use unequal symbolic masses and every massive-spin tensor
   coefficient, rather than one helicity projection or numerical momenta. *)
ndMixedChart = ConstructiveDiagrams`Private`completionHERestMixedNullSymbolicChart;
ndMixedCases = <|
  "five-point unequal-mass" -> <|"Rules" -> {Mass[1] -> ndMassA,
    Mass[2] -> ndMassB, Mass[3] -> ndMassC, Mass[4] -> 0, Mass[5] -> 0},
    "Pivots" -> {4, 5}|>,
  "six-point relabeled" -> <|"Rules" -> {Mass[8] -> ndMassA,
    Mass[23] -> ndMassB, Mass[3] -> 0, Mass[11] -> 0, Mass[17] -> 0,
    Mass[31] -> 0}, "Pivots" -> {31, 3}|>|>;
Do[
  ndMixedRules = ndMixedCases[case]["Rules"];
  ndMixedPivots = ndMixedCases[case]["Pivots"];
  ndMixed = ndMixedChart[ndMixedRules, ndMixedPivots, kind];
  ndLabel = case <> " " <> kind;
  check[ndLabel <> " mixed chart is constructed", AssociationQ[ndMixed]];
  If[!AssociationQ[ndMixed], Continue[]];
  ndExternal = Sort[First[#][[1]] & /@ ndMixedRules];
  ndNull = Select[ndExternal, TrueQ[(Mass[#] /. ndMixedRules) === 0] &];
  ndMassive = Complement[ndExternal, ndNull];
  ndAnchor = ndMixed["RestFrameLeg"];
  {ndAngleAxis, ndSquareAxis} = ndMixedPivots;
  check[ndLabel <> " preserves every declared unequal external shell",
    AllTrue[ndExternal, ndZero[
      Det[ndMixed["Momenta"][#]] - (Mass[#] /. ndMixedRules)^2] &] &&
    Lookup[ndMixed["Masses"], ndMassive] === ((Mass /@ ndMassive) /. ndMixedRules)];
  check[ndLabel <> " preserves exact conservation and each null outer product",
    AllTrue[Flatten[Total[Values[ndMixed["Momenta"]]]], ndZero] &&
    AllTrue[ndNull, Function[leg, AllTrue[Flatten[
      Outer[Times, ndMixed["Angle"][leg], ndMixed["Square"][leg]] -
        ndMixed["Momenta"][leg]], ndZero]]]];
  check[ndLabel <> " fixes the requested opposite chiral axes without reordering labels",
    ndMixed["NullAxisLegs"] === ndMixedPivots &&
    ndMixed["Angle"][ndAngleAxis] === {1, 0} &&
    ndMixed["Square"][ndSquareAxis] === {1, 0} &&
    ndMixed["NullAxisChirality"] === "Mixed" &&
    ndMixed["MassiveSpinorBasis"] === kind];
  check[ndLabel <> " retains both independent massive-spin columns and Dirac orientations",
    AllTrue[ndMassive, Function[leg,
      With[{p = ndMixed["Momenta"][leg], a = ndMixed["AngleBasis"][leg],
        s = ndMixed["SquareBasis"][leg], m = Mass[leg] /. ndMixedRules},
        ndZero[Det[a] - m] && ndZero[Det[s] - m] &&
        AllTrue[Flatten[p.ndJ.s + m a], ndZero] &&
        AllTrue[Flatten[ndAdj[p].a + m ndJ.s], ndZero] &&
        AllTrue[Flatten[a.ndJ.Transpose[s] - p], ndZero]]]]];
  ndMixedNullInvariant = Cancel[Together[Det[
    ndMixed["Momenta"][ndAngleAxis] + ndMixed["Momenta"][ndSquareAxis]]]];
  ndMixedRestVirtuality = Cancel[Together[Det[
    ndMixed["Momenta"][ndAnchor] + ndMixed["Momenta"][ndAngleAxis]] -
      (Mass[ndAnchor] /. ndMixedRules)^2]];
  check[ndLabel <> " does not impose collinearity or an internal rest-null shell",
    !ndZero[ndMixedNullInvariant] && !ndZero[ndMixedRestVirtuality] &&
    !FreeQ[ndMixedNullInvariant, Alternatives @@ ndMixed["ExternalIdentityCoordinates"]] &&
    !FreeQ[ndMixedRestVirtuality, Alternatives @@ ndMixed["ExternalIdentityCoordinates"]]];
  ndMixedSandwich = ConstructiveDiagrams`Private`spuriousEvaluateCurve[
    SpinorChain[Spinor["Helicity", "Angle", ndAngleAxis], Mom[ndAnchor],
      Spinor["Helicity", "Square", ndSquareAxis]], ndMixed, {}];
  check[ndLabel <> " records a nonempty dense mixed-sandwich patch",
    ndMixed["OriginalMixedNullSandwichRequiredNonzero"] ===
      {ndAngleAxis, ndAnchor, ndSquareAxis} &&
    !ndZero[ndMixedSandwich] &&
    AllTrue[ndMixed["DenseDomainNonzeroExpressions"], !ndZero[#] &] &&
    ndMixed["ResidualGaugeDimension"] === 1];
  ndMixedSpinLists = Table[Intersection[ndMixed["SpinVariables"],
    Cases[Join[ndMixed["Angle"][leg], ndMixed["Square"][leg]], _Symbol, Infinity]],
    {leg, ndMassive}];
  check[ndLabel <> " gives each massive leg its own independent coherent-spin variable",
    AllTrue[ndMixedSpinLists, Length[#] === 1 &] &&
    Length[ndMixed["SpinVariables"]] === Length[ndMassive] &&
    DuplicateFreeQ[Join[ndMixed["SpinVariables"], ndMixed["ExternalIdentityCoordinates"]]]];
  If[AllTrue[ndMixedSpinLists, Length[#] === 1 &],
    ndMixedSpins = First /@ ndMixedSpinLists;
    ndMixedTensor = Times @@ Table[
      ndMixed["Angle"][leg][[1]] + ndAngleWeight[leg] ndMixed["Angle"][leg][[2]] +
      ndSquareWeight[leg] ndMixed["Square"][leg][[1]] +
      ndCrossWeight[leg] ndMixed["Square"][leg][[2]], {leg, ndMassive}];
    check[ndLabel <> " reconstructs every coherent massive-spin tensor coefficient",
      PolynomialQ[ndMixedTensor, ndMixedSpins] &&
      AllTrue[Tuples[{0, 1}, Length[ndMassive]], Function[bits,
        ndZero[Fold[Function[{value, variablePower},
          Coefficient[value, First[variablePower], Last[variablePower]]],
          ndMixedTensor, Transpose[{ndMixedSpins, bits}]] -
          Times @@ MapThread[Function[{leg, bit},
            ndMixed["AngleBasis"][leg][[1, bit + 1]] +
            ndAngleWeight[leg] ndMixed["AngleBasis"][leg][[2, bit + 1]] +
            ndSquareWeight[leg] ndMixed["SquareBasis"][leg][[1, bit + 1]] +
            ndCrossWeight[leg] ndMixed["SquareBasis"][leg][[2, bit + 1]]],
            {ndMassive, bits}]]]]]];
  check[ndLabel <> " uses only exact external kinematics",
    ndMixed["NumericalKinematicSamples"] === 0 && ndMixed["InternalOnShellConditions"] === {} &&
    ndMixed["ExternalMassShellsValidated"] === True &&
    ndMixed["MomentumConservationValidated"] === True &&
    FreeQ[ndMixed["Momenta"], _Real]],
  {case, Keys[ndMixedCases]}, {kind, {"Angle", "Square"}}];
ndMixedGuardRules = ndMixedCases["five-point unequal-mass"]["Rules"];
check["mixed chart declines malformed or repeated null pivots before assigning axes",
  AllTrue[{{}, {4}, {4, 5, 6}, {4, 4}, {1, 5}, {4, 99}, 4},
    ndMixedChart[ndMixedGuardRules, #] === $Failed &]];
check["mixed chart declines duplicate mass metadata",
  ndMixedChart[Append[ndMixedGuardRules, Mass[1] -> ndOtherMass]] === $Failed];
check["mixed chart declines approximate or unresolved external masses",
  ndMixedChart[ndMixedGuardRules /. ndMassA -> 1.0] === $Failed &&
  ndMixedChart[ndMixedGuardRules /. ndMassA -> Mass[99]] === $Failed];
check["mixed chart requires two massive and two null external momenta",
  ndMixedChart[ndMixedGuardRules /. {ndMassA -> 0, ndMassB -> 0}] === $Failed &&
  ndMixedChart[ReplacePart[ndMixedGuardRules, 4 -> (Mass[4] -> ndFourthMass)]] === $Failed];
check["mixed chart declines an unsupported massive-spin basis",
  ndMixedChart[ndMixedGuardRules, Automatic, "Invalid"] === $Failed];
ndMixedAutomatic = ndMixedChart[ndMixedGuardRules];
check["mixed chart automatic pivots use the declared null legs deterministically",
  ndMixedAutomatic["NullAxisLegs"] === {4, 5} &&
    ndMixedAutomatic["OriginalMixedNullSandwichRequiredNonzero"] === {4, 1, 5}];
Print["Native rest/null dual checks: ", checks, "; failures: ", Length[failures]];
If[failures === {}, Print["HEPCAT_TEST_COMPLETED:native-rest-null-dual-tests.wl"]; Quit[0], Print[failures]; Quit[1]];
