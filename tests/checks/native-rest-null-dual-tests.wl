(* Portable exact checks for the two dense null-axis chirality choices. *)
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
Print["Native rest/null dual checks: ", checks, "; failures: ", Length[failures]];
If[failures === {}, Print["HEPCAT_TEST_COMPLETED:native-rest-null-dual-tests.wl"]; Quit[0], Print[failures]; Quit[1]];
