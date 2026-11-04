(* Replay the five-point notebook with its actual models and no supplied answer. *)
root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Global`$HEPCATpath = FileNameJoin[{root, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
$HistoryLength = 0;
diagrams = ReadModel[FileNameJoin[{root, "models"}], 2];
factorization = ReadModel[FileNameJoin[{root, "models"}], 3];
benchmark = ReadModel[FileNameJoin[{root, "model_feynman"}], 2];
cases = <|
  "PPP" -> {{"e", "E", "A.+", "A.+", "A.+"}, 8085212822663136897, 6, 3, 30},
  "MMM" -> {{"e", "E", "A.-", "A.-", "A.-"}, 1866157270020603678, 6, 3, 30},
  "PPM" -> {{"e", "E", "A.+", "A.+", "A.-"}, 9188430219292690389, 6, 8, 133},
  "PMM" -> {{"e", "E", "A.+", "A.-", "A.-"}, 3418550451434630771, 6, 8, 133},
  "P" -> {{"e", "E", "E", "e", "A.+"}, 8126805240028689964, 8, 0, 168},
  "M" -> {{"e", "E", "E", "e", "A.-"}, 5310282517098944578, 8, 0, 168}|>;
requested = Environment["HEPCAT_SPURIOUS_INTEGRATION_CASES"];
selected = If[MemberQ[{$Failed, ""}, requested], Keys[cases], StringSplit[requested, ","]];
If[selected === {} || !ContainsAll[Keys[cases], selected], Quit[2]];
failures = {};
Do[
  {process, seed, physical, spurious, ceiling} = cases[name];
  Print["BEGIN ", name];
  report = ConstructAmplitude[process, diagrams, factorization,
    "PolePreference" -> "AllSpurious", "FeynmanModel" -> benchmark,
    "RandomSeed" -> seed, "Output" -> "Details", "ProgressReporting" -> False, "PrintReport" -> False];
  checks = report["Checks"];
  If[report["Status"] =!= "COMPLETE" || report["AchievedPhysicalDenominators"] =!= (spurious === 0) ||
      !FreeQ[report["Amplitude"], (Spinor | SpinorHat)["Xi", ___]] ||
      !IntegerQ[report["Complexity"]] || report["Complexity"] > ceiling ||
      Length[checks["Channels"]] =!= physical ||
      Length[checks["SpuriousChecks"]] =!= spurious ||
      checks["Summary"] =!= <|"PASS" -> physical + spurious + 3|>,
    AppendTo[failures, name]];
  Print[name, " ", report["Status"], " complexity ", report["Complexity"],
    " ", checks["Summary"], " seconds ", report["TotalTime"]];
  ,{name, selected}];
If[failures =!= {}, Print["FAIL ", failures]; Quit[1]];
Print["HEPCAT_TEST_COMPLETED:spurious-construction-integration-tests.wl"];
Quit[0];
