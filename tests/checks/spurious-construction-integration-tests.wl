(* Replay the notebook with its actual models and no supplied answer. *)
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
  "PPPM6" -> {{"e", "E", "A.+", "A.+", "A.+", "A.-"}, 4493323141101633411, 14, 4, 528},
  "PMMM6" -> {{"e", "E", "A.+", "A.-", "A.-", "A.-"}, 3640391260799291343, 14, 4, 528},
  "PPMM6" -> {{"e", "E", "A.+", "A.+", "A.-", "A.-"}, 202611040608, 14, 4, Infinity}|>;
sixPointMethods = <|"PPPP6" -> "ReducedCompoundPoleOrbit",
  "PPPM6" -> "CommonChiralVectorLine", "PMMM6" -> "CommonChiralVectorLine",
  "PPMM6" -> "OffShellVectorLine"|>;
requested = Environment["HEPCAT_SPURIOUS_INTEGRATION_CASES"];
selected = If[MemberQ[{$Failed, ""}, requested],
  Select[Keys[cases], !KeyExistsQ[sixPointMethods, #] &], StringSplit[requested, ","]];
If[selected === {} || !ContainsAll[Keys[cases], selected], Quit[2]];
failures = {};
Do[
  {process, seed, physical, spurious, ceiling} = cases[name];
  Print["BEGIN ", name];
  factorization = ReadModel[modelDirectory, If[KeyExistsQ[sixPointMethods, name], 4, 3]];
  report = ConstructAmplitude[process, diagrams, factorization,
    "PolePreference" -> "AllSpurious", "FeynmanModel" -> benchmark,
    "RandomSeed" -> seed, "Output" -> "Details", "ProgressReporting" -> False, "PrintReport" -> False];
  checks = report["Checks"];
  If[report["Status"] =!= "COMPLETE" || report["AchievedPhysicalDenominators"] =!= (spurious === 0) ||
      !FreeQ[report["Amplitude"], (Spinor | SpinorHat)["Xi", ___]] ||
      ceiling =!= Infinity && (!IntegerQ[report["Complexity"]] || report["Complexity"] > ceiling) ||
      Length[checks["Channels"]] =!= physical ||
      Length[checks["SpuriousChecks"]] =!= spurious ||
      checks["Summary"] =!= <|"PASS" -> physical + spurious + 3|> ||
      KeyExistsQ[sixPointMethods, name] && Lookup[report["Stages"][
        If[name === "PPMM6", "Off-shell vector form", "Compact spurious form"]],
        "Method", None] =!= sixPointMethods[name],
    AppendTo[failures, name]];
  Print[name, " ", report["Status"], " complexity ", report["Complexity"],
    " ", checks["Summary"], " seconds ", report["TotalTime"]];
  If[name === "PPMM6",
    Get[FileNameJoin[{root, "tests", "QED-5-point-residues.wl"}]];
    compact = QEDSixPointPPMMSpuriousExample[factorization];
    compactComplexity = SpinorExpressionComplexity[Last[compact]];
    compactChecks = CheckAmplitudes[compact, FactorizationAmplitudeChannels[process, factorization],
      factorization, "FeynmanAmplitudes" -> DiagramAmplitudes[CreateDiagrams[process, benchmark]],
      "FeynmanModel" -> benchmark, "RandomSeed" -> seed, "Output" -> "Details"];
    If[!IntegerQ[compactComplexity] || compactComplexity > 1044 ||
        Length[compactChecks["Channels"]] =!= physical || Length[compactChecks["SpuriousChecks"]] =!= spurious ||
        compactChecks["Summary"] =!= <|"PASS" -> physical + spurious + 3|>,
      AppendTo[failures, name <> " compact example"]];
    Print[name, " compact example complexity ", compactComplexity, " ", compactChecks["Summary"]]];
  ,{name, selected}];
If[failures =!= {}, Print["FAIL ", failures]; Quit[1]];
Print["HEPCAT_TEST_COMPLETED:spurious-construction-integration-tests.wl"];
Quit[0];
