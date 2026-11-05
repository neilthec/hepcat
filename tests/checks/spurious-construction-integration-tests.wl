(* Replay the notebook with its actual models and no supplied answer.
   Check any separately worked formulas only after the automatic call. *)
root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Global`$HEPCATpath = FileNameJoin[{root, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
$HistoryLength = 0;
diagrams = ReadModel[FileNameJoin[{root, "models"}], 2];
modelDirectory = FileNameJoin[{root, "models"}];
benchmark = ReadModel[FileNameJoin[{root, "model_feynman"}], 2];
cases = <|
  "PPP" -> {{"e", "E", "A.+", "A.+", "A.+"}, 8085212822663136897, 6, 3, 30},
  "MMM" -> {{"e", "E", "A.-", "A.-", "A.-"}, 1866157270020603678, 6, 3, 30},
  "PPM" -> {{"e", "E", "A.+", "A.+", "A.-"}, 9188430219292690389, 6, 8, 133},
  "PMM" -> {{"e", "E", "A.+", "A.-", "A.-"}, 3418550451434630771, 6, 8, 133},
  "P" -> {{"e", "E", "E", "e", "A.+"}, 8126805240028689964, 8, 0, 168},
  "M" -> {{"e", "E", "E", "e", "A.-"}, 5310282517098944578, 8, 0, 168},
  "PPPP6" -> {{"e", "E", "A.+", "A.+", "A.+", "A.+"}, 1658277745900164952, 14, 6, 120},
  "MMMM6" -> {{"e", "E", "A.-", "A.-", "A.-", "A.-"}, 4452583888334340141, 14, 6, 120},
  "PPPM6" -> {{"e", "E", "A.+", "A.+", "A.+", "A.-"}, 4493323141101633411, 14, 4, 528},
  "PMMM6" -> {{"e", "E", "A.+", "A.-", "A.-", "A.-"}, 3640391260799291343, 14, 4, 528},
  "PPMM6" -> {{"e", "E", "A.+", "A.+", "A.-", "A.-"}, 202611040608, 14, 4, Infinity},
  "FourFermionsPP6" -> {{"e", "E", "E", "e", "A.+", "A.+"}, 9008490858615875420, 20, 1, 1216},
  "FourFermionsPM6" -> {{"e", "E", "E", "e", "A.+", "A.-"}, 8315759881186794091, 20, 2, 1632},
  "FourFermionsMM6" -> {{"e", "E", "E", "e", "A.-", "A.-"}, 7049700354755523172, 20, 1, 1216},
  "SixF6" -> {{"e", "E", "e", "E", "e", "E"}, 579024387046450637, 18, 0, 3168}|>;
sixPointMethods = <|"PPPP6" -> "SameChiralPairSplit",
  "MMMM6" -> "SameChiralPairSplit",
  "PPPM6" -> "CommonChiralVectorLine", "PMMM6" -> "CommonChiralVectorLine",
  "PPMM6" -> "OffShellVectorLine", "SixF6" -> "OffShellFermionExchange"|>;
fourFermionForms = <|"FourFermionsPP6" -> {"PP", 1, 1360},
  "FourFermionsPM6" -> {"PM", 2, 1664}, "FourFermionsMM6" -> {"MM", 1, 1360}|>;
sixPointCases = Join[Keys[sixPointMethods], Keys[fourFermionForms]];
requested = Environment["HEPCAT_SPURIOUS_INTEGRATION_CASES"];
selected = If[MemberQ[{$Failed, ""}, requested],
  Complement[Keys[cases], sixPointCases], StringSplit[requested, ","]];
If[selected === {} || !ContainsAll[Keys[cases], selected], Quit[2]];
failures = {};
Do[
  {process, seed, physical, spurious, ceiling} = cases[name];
  Print["BEGIN ", name];
  factorization = ReadModel[modelDirectory, If[MemberQ[sixPointCases, name], 4, 3]];
  report = ConstructAmplitude[process, diagrams, factorization,
    "PolePreference" -> "AllSpurious", "FeynmanModel" -> benchmark,
    "RandomSeed" -> seed, "Output" -> "Details", "ProgressReporting" -> False, "PrintReport" -> False];
  checks = report["Checks"];
  actualSpurious = If[AssociationQ[checks], Length[checks["SpuriousChecks"]], -1];
  If[report["Status"] =!= "COMPLETE" || actualSpurious < 0 ||
      (spurious =!= Automatic && (actualSpurious =!= spurious ||
        report["AchievedPhysicalDenominators"] =!= (spurious === 0))) ||
      !FreeQ[report["Amplitude"], (Spinor | SpinorHat)["Xi", ___]] ||
      ceiling =!= Infinity && (!IntegerQ[report["Complexity"]] || report["Complexity"] > ceiling) ||
      Length[checks["Channels"]] =!= physical ||
      checks["Summary"] =!= <|"PASS" -> physical + actualSpurious + 3|> ||
      KeyExistsQ[sixPointMethods, name] && Lookup[report["Stages"][
        Switch[name, "PPMM6", "Off-shell vector form", "SixF6", "Off-shell fermion exchange",
          _, "Compact spurious form"]], "Method", None] =!= sixPointMethods[name] ||
      KeyExistsQ[fourFermionForms, name] &&
        Lookup[report["Stages"]["Grouped vector currents"], "Method", None] =!= "GroupedVectorCurrents" ||
      name === "SixF6" && (LeafCount[report["Amplitude"]] > 20500 ||
        Lookup[report["RepresentativeSelection"], "BenchmarkUsedForConstruction", None] =!= False),
    AppendTo[failures, name]];
  Print[name, " ", report["Status"], " complexity ", report["Complexity"],
    " ", checks["Summary"], " seconds ", report["TotalTime"]];
  If[name === "SixF6" && report["Status"] === "COMPLETE",
    sixMasses = First[report["AmplitudePair"]];
    sixOrder = {2, 3, 4, 1, 6, 5};
    sixProcess = process[[sixOrder]];
    sixGraphs = CreateDiagrams[sixProcess, diagrams];
    sixCandidate = ConstructiveDiagrams`Private`constructOffShellFermionExchangeForm[
      sixGraphs, sixProcess, diagrams, sixMasses];
    sixRelabeled = ConstructiveDiagrams`Private`relabelLegs[report["Amplitude"],
      AssociationThread[sixOrder, Range[6]]];
    sixCanon[expr_] := ConstructiveDiagrams`Private`canonicalChains[
      ConstructiveDiagrams`Private`residueCanonicalPropagators[Expand[expr], Range[6]]];
    (* The odd-leg ordering undergoes one transposition, the even legs none. *)
    If[!AssociationQ[sixCandidate] ||
        Expand[sixCanon[sixCandidate["Expression"]] + sixCanon[sixRelabeled]] =!= 0,
      AppendTo[failures, name <> " signed relabeling"]];
    If[!MissingQ[ConstructiveDiagrams`Private`constructOffShellFermionExchangeForm[
        Append[Most[sixGraphs], First[sixGraphs]], sixProcess, diagrams, sixMasses]],
      AppendTo[failures, name <> " incomplete attachment inventory"]]];
  If[name === "PPMM6" || KeyExistsQ[fourFermionForms, name],
    Get[FileNameJoin[{root, "tests", "QED-5-point-residues.wl"}]];
    If[name === "PPMM6",
      compact = QEDSixPointPPMMSpuriousExample[factorization];
      {compactSpurious, compactCeiling} = {spurious, 1044},
      {helicities, compactSpurious, compactCeiling} = fourFermionForms[name];
      compact = QEDSixPointFourFermionSpuriousExample[factorization, helicities]];
    compactComplexity = SpinorExpressionComplexity[Last[compact]];
    compactChecks = CheckAmplitudes[compact, FactorizationAmplitudeChannels[process, factorization],
      factorization, "FeynmanAmplitudes" -> DiagramAmplitudes[CreateDiagrams[process, benchmark]],
      "FeynmanModel" -> benchmark, "RandomSeed" -> seed, "Output" -> "Details",
      "TimeConstraint" -> If[name === "PPMM6", 30, 180]];
    If[!IntegerQ[compactComplexity] || compactComplexity > compactCeiling ||
        !FreeQ[Last[compact], (Spinor | SpinorHat)["Xi", ___]] ||
        Length[compactChecks["Channels"]] =!= physical ||
        Length[compactChecks["SpuriousChecks"]] =!= compactSpurious ||
        compactChecks["Summary"] =!= <|"PASS" -> physical + compactSpurious + 3|>,
      AppendTo[failures, name <> " compact example"]];
    Print[name, " compact example complexity ", compactComplexity, " ", compactChecks["Summary"]]];
  ,{name, selected}];
If[failures =!= {}, Print["FAIL ", failures]; Quit[1]];
Print["HEPCAT_TEST_COMPLETED:spurious-construction-integration-tests.wl"];
Quit[0];
