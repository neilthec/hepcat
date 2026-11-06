(* Exact rational-chart regression for the research formula. Run directly with
   WolframKernel -script benchmarks/one-minus-symbolic-tests.wl.
   The chart coordinate stays symbolic; no internal pole is set to zero.
   These one-parameter charts supplement the general derivation and independent
   MadGraph tests; they are not a proof over generic external kinematics. *)
root = DirectoryName[DirectoryName[$InputFileName]];
Get[FileNameJoin[{root, "source", "HEPCAT.wl"}]];
Get[FileNameJoin[{root, "tests", "QED-5-point-residues.wl"}]];
Get[FileNameJoin[{root, "tests", "QED-one-minus-formula.wl"}]];
$HistoryLength = 0;
checks = 0; failures = {};
SetAttributes[check, HoldRest];
check[name_, expression_] := (checks++;
  If[!TrueQ[expression], AppendTo[failures, name]; Print["FAIL: ", name]]);
mass = prElectronMass; charge = prElectricCharge;
four = QEDOneMinusFormula[2, mass, charge, "ReturnCoefficients" -> True];
five = QEDOneMinusFormula[3, mass, charge, "ReturnCoefficients" -> True];
fourAlternative = QEDOneMinusFormula[2, mass, charge,
  "SquareReference" -> Spinor["Xi", "Square", 0]];
fiveAlternative = QEDOneMinusFormula[3, mass, charge, "SquareReference" -> 4];
fourRow = SelectFirst[prFixtureModel[[2]],
  Drop[#, -2] === {"e", "E", "A.+", "A.-"} &];
knownFour = Times @@ Take[fourRow, -2];
knownFive = qedFivePointMixedRoute[DiagramAmplitudes[CreateDiagrams[
  {"e", "E", "A.+", "A.+", "A.-"}, prModel]]]["Final"];
Do[
  fourChart = prFourPointChart[{1, 3}, coordinate, first, second];
  fiveChart = prChart[{1, 3}, coordinate, first, second];
  check["four-point model agreement " <> ToString[{first, second}],
    Together[prMapOnChart[four["Amplitude"][[2]] - knownFour, fourChart]] === 0];
  check["five-point compact fixture agreement " <> ToString[{first, second}],
    Together[prMapOnChart[five["Amplitude"][[2]] - knownFive, fiveChart]] === 0];
  check["four-point square reference " <> ToString[{first, second}],
    Together[prMapOnChart[four["Amplitude"][[2]] - fourAlternative[[2]], fourChart]] === 0];
  check["five-point square reference " <> ToString[{first, second}],
    Together[prMapOnChart[five["Amplitude"][[2]] - fiveAlternative[[2]], fiveChart]] === 0];
  check["four-point Ward identity " <> ToString[{first, second}],
    Together[prMapOnChart[four["WardIdentity"], fourChart]] === 0];
  check["five-point Ward identity " <> ToString[{first, second}],
    Together[prMapOnChart[five["WardIdentity"], fiveChart]] === 0],
  {first, 2}, {second, 2}];
check["two scalar currents and the correct number of partitions",
  four["PartitionCount"] === 2 && five["PartitionCount"] === 4];
check["default references leave no Xi spinors",
  FreeQ[{four["Amplitude"], five["Amplitude"]}, Spinor["Xi", ___]]];
check["result retains only external mass rules",
  FreeQ[First[five["Amplitude"]], _Multiparticle]];
check["invalid negative-photon label is rejected",
  FailureQ[QEDOneMinusFormula[3, mass, charge, "NegativePhoton" -> 1]]];
check["singular self reference is rejected",
  FailureQ[QEDOneMinusFormula[3, mass, charge, "SquareReference" -> 5]]];
check["termwise massless-fermion representation is rejected",
  FailureQ[QEDOneMinusFormula[3, 0, charge]]];
check["numerical zero mass is also rejected",
  FailureQ[QEDOneMinusFormula[3, 0., charge]]];
check["an angle spinor is not a square reference",
  FailureQ[QEDOneMinusFormula[3, mass, charge,
    "SquareReference" -> Spinor["Xi", "Angle", 0]]]];
Print["CHECKS ", checks, "; FAILURES ", Length[failures]];
If[failures === {}, Print["ONE_MINUS_SYMBOLIC_TESTS_PASS"], Print[failures]];
Quit[If[failures === {}, 0, 1]];
