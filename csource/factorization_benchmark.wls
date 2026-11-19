(* Use HEPCAT_BATCH_BINARY for the compiled hepcat-reduce-amplitude executable.
   HEPCAT_FACTOR_RESULTS selects the output directory. Running without a case
   filter includes the cyclic five-point stress cases. *)
$HistoryLength = 0;
nativeDirectory = DirectoryName[$InputFileName];
repo = Replace[Environment["HEPCAT_ROOT"], Except[_String] :> DirectoryName[nativeDirectory]];
Get[FileNameJoin[{repo, "source", "HEPCAT.wl"}]];
Get[FileNameJoin[{nativeDirectory, "NativeAmplitude.wl"}]];
binary = Environment["HEPCAT_BATCH_BINARY"];
output = Replace[Environment["HEPCAT_FACTOR_RESULTS"], Except[_String] :>
  FileNameJoin[{repo, "tests", "results", "native-factorization-benchmark"}]];
If[!DirectoryQ[output], CreateDirectory[output, CreateIntermediateDirectories -> True]];
fail[msg_] := (Print["ERROR ", msg]; Exit[2]);
If[!StringQ[binary] || !FileExistsQ[binary], fail["Set HEPCAT_BATCH_BINARY to the native executable."]];
diagramsModel = ReadModel[FileNameJoin[{repo, "models"}], 3];
channelsModel = ReadModel[FileNameJoin[{repo, "models"}], 4];

(* Both solvers see the same seeds, original fixed-reference continuations
   and exact remainders. Independent checks below also use the unprepared,
   original model channels, including their own reference choices. *)
retained[rows_, remainders_] := MapThread[
  <|"ShellRemainder" -> #2, "PreparedExpression" -> Last[#1],
    "FixedReferenceExpression" -> Last[#1] + #2|> &, {rows, remainders}];
wlInputs[batch_] := Module[{ds = batch["Diagrams"], cs = batch["FactorizationProducts"], rd, rc},
  rd = retained[ds, batch["DiagramRemainders"]];
  rc = retained[cs, batch["FactorizationRemainders"]];
  {<|"Model" -> diagramsModel, "PreparedAmplitudes" -> ds, "RemainderData" -> rd|>,
   <|"Model" -> channelsModel, "PreparedAmplitudes" -> cs,
     "OriginalAmplitudes" -> MapThread[ReplacePart[#1, -1 -> #2["FixedReferenceExpression"]] &, {cs, rc}],
     "RemainderData" -> rc|>}
];
prepare[raw_, cs_, mode_] := Module[{d, c},
  If[mode === "Cyclic", Return[
    HEPCATNative`ReduceNativeAmplitude[raw, cs, binary, "PolePreference" -> "Cyclic"]]];
  d = PrepareAmplitudes[raw, diagramsModel, "PolePreference" -> mode, "Parallelization" -> False,
    "ReturnDetails" -> True, "ProgressReporting" -> False];
  c = PrepareAmplitudes[cs, channelsModel, "PolePreference" -> mode, "Parallelization" -> False,
    "ReturnDetails" -> True, "ProgressReporting" -> False];
  If[!AssociationQ[d] || !AssociationQ[c], fail["Wolfram preparation failed."]];
  <|"Diagrams" -> d["PreparedAmplitudes"], "FactorizationProducts" -> c["PreparedAmplitudes"],
    "DiagramRemainders" -> Lookup[d["RemainderData"], "ShellRemainder"],
    "FactorizationRemainders" -> Lookup[c["RemainderData"], "ShellRemainder"]|>
];
(* Compare all spin coefficients on conserved exact external mass shells for
   two separate parameter and momentum choices. Nothing is put on an internal
   shell here. A nonzero numerical difference is retained in the JSON as a
   failed check even when all physical pole residues pass. *)
offCut[expressions_Association, masses_List] := Module[
  {results = {}, params, chart, coords, pt, spin, poly, values, rules, reference},
  Do[
    params = ModelParameterRules[channelsModel, {"Me" -> sample/2, "EE" -> 2/(sample + 2)}];
    rules = Thread[(Mass /@ Range[Length[masses]]) -> masses] /. params;
    chart = ConstructiveDiagrams`Private`completionHESymbolicExternalChart[rules];
    If[!AssociationQ[chart] || !TrueQ[chart["ExternalMassShellsValidated"]] ||
        !TrueQ[chart["MomentumConservationValidated"]] || chart["InternalOnShellConditions"] =!= {},
      fail["Invalid exact external chart."]];
    coords = chart["ExternalIdentityCoordinates"];
    pt = Join[chart /. Thread[coords -> Prime[Range[Length[coords]] + sample]], <|"ReferenceSeed" -> 919 + sample|>];
    If[Total[Values[pt["Momenta"]]] =!= ConstantArray[0, {2,2}] ||
        !AllTrue[Keys[pt["Momenta"]], Det[pt["Momenta"][#]] === pt["Masses"][#]^2 &],
      fail["The test point violates external kinematics."]];
    spin = pt["SpinVariables"];
    poly[expr_] := Expand[ConstructiveDiagrams`Private`spuriousEvaluateCurve[expr, pt, params]];
    values = poly /@ expressions;
    If[!AllTrue[Values[values], FreeQ[#, _Missing | _Failure | Indeterminate | _DirectedInfinity | _Real] &&
        PolynomialQ[#, spin] && AllTrue[Last /@ CoefficientRules[#, spin], NumericQ] &],
      fail["Non-numerical or singular external validation point."]];
    If[values["Stored"] === 0, fail["Degenerate stored amplitude."]];
    reference = values["Stored"];
    AppendTo[results, <|"ElectronMass" -> ToString[sample/2, InputForm],
      "MatchesStored" -> (# === reference & /@ KeyDrop[values, {"Stored"}]),
      "NativeMatchesWolfram" -> If[And @@ (KeyExistsQ[values, #] & /@ {"Native", "Wolfram"}),
        values["Native"] === values["Wolfram"], Null]|>],
    {sample, {3, 5}}];
  results
];
rawResidues[expr_, cs_] := Module[{r},
  r = CheckAmplitudes[expr, cs, channelsModel, "Output" -> "Details", "RandomSeed" -> 7271,
    "SpuriousPoles" -> {}, "ReferenceCheck" -> False, "HighEnergyCheck" -> False,
    "ParameterRules" -> {"Me" -> 3/2, "EE" -> 2/5}, "Parallelization" -> False,
    "Samples" -> 2, "TimeConstraint" -> 60];
  If[!AssociationQ[r] || !AssociationQ[r["Summary"]] || Total[Values[r["Summary"]]] =!= Length[cs],
    fail["The independent checker did not evaluate every physical channel."]];
  r["Summary"]
];
nativeRun[b_, workers_] := Module[{res, wall},
  wall = First[AbsoluteTiming[res = HEPCATNative`CompleteNativeFactorization[b, binary,
    "Workers" -> workers, "ReturnDetails" -> True]]];
  If[!AssociationQ[res] || !KeyExistsQ[res,"Amplitude"],
    Return[<|"Status" -> "UNRESOLVED", "BridgeSeconds" -> wall,
      "Reason" -> ToString[res, InputForm]|>]];
  Join[res, <|"Status" -> "COMPLETE", "BridgeSeconds" -> wall|>]
];
timeNative[b_, repetitions_] := Module[{runs = <|"Serial" -> {}, "Automatic" -> {}|>, native, expr = None, times, key},
  Do[
    Do[key = If[n === 1, "Serial", "Automatic"]; native = nativeRun[b, n];
      If[native["Status"] === "COMPLETE",
        If[expr === None, expr = Last[native["Amplitude"]], If[Last[native["Amplitude"]] =!= expr,
          fail["The completed native expression depends on the worker count or repetition."]]]];
      AssociateTo[runs, key -> Append[runs[key], KeyDrop[native, {"Amplitude"}]]],
      {n, If[OddQ[i], {1, Automatic}, {Automatic, 1}]}],
    {i, 1, repetitions}];
  times = Function[rs, Module[{successful = Select[rs, #["Status"] === "COMPLETE" &]},
    If[successful === {}, KeyTake[First[rs], {"Status", "Reason"}],
      If[Length[successful] =!= Length[rs], fail["Inconsistent native completion status."]];
      Join[KeyTake[First[rs], {"Status", "Route", "WorkersUsed", "Channels", "Candidates", "Rank",
           "TrainingSamples", "ValidationSamples"}],
        <|"NativeSeconds" -> Median[Lookup[rs,"CompletionSeconds"]], "BridgeSeconds" -> Median[Lookup[rs,"BridgeSeconds"]],
          "NativeRangeSeconds" -> MinMax[Lookup[rs,"CompletionSeconds"]]|>]]]] /@ runs;
  If[times["Serial"]["Status"] =!= times["Automatic"]["Status"], fail["Native worker modes disagree."]];
  {times,expr}
];
cases = <|
  "eePP" -> {{"e","E","A.+","A.+"}, "AllPhysical"},
  "eePPMissingDiagram" -> {{"e","E","A.+","A.+"}, "Cyclic"},
  "eePPP" -> {{"e","E","A.+","A.+","A.+"}, "AllPhysical"},
  "eePPPCyclic" -> {{"e","E","A.+","A.+","A.+"}, "Cyclic"},
  "eePPM" -> {{"e","E","A.+","A.+","A.-"}, "AllPhysical"},
  "eePPMCyclic" -> {{"e","E","A.+","A.+","A.-"}, "Cyclic"},
  "eePPPP" -> {{"e","E","A.+","A.+","A.+","A.+"}, "AllPhysical"},
  "eePPPPCyclic" -> {{"e","E","A.+","A.+","A.+","A.+"}, "Cyclic"}|>;
selection = Replace[Environment["HEPCAT_FACTOR_CASES"],
  {v_String :> StringSplit[v, ","], _ :> Keys[cases]}];
If[!SubsetQ[Keys[cases], selection], fail["Unknown name in HEPCAT_FACTOR_CASES."]];
reports = <||>;
Do[
  {proc, mode} = cases[name];
  Print["Benchmarking ",name];
  raw = DiagramAmplitudes[CreateDiagrams[proc, diagramsModel]];
  cs = FactorizationAmplitudeChannels[proc, channelsModel];
  batch = prepare[raw, cs, mode]; If[!AssociationQ[batch], fail[batch]];
  If[name === "eePPMissingDiagram", batch = Join[batch, <|
    "Diagrams" -> Rest[batch["Diagrams"]], "DiagramRemainders" -> Rest[batch["DiagramRemainders"]]|>]];
  seedAmp = Total[Last /@ batch["Diagrams"]];
  {wd,wc} = wlInputs[batch];
  wlTime = First[AbsoluteTiming[wlResult = CompleteFactorization[wd, wc, channelsModel,
    "ExternalParticles" -> proc, "ParameterRules" -> {"Me" -> 2, "EE" -> 1/3}, "RandomSeed" -> 191,
    "TimeConstraint" -> 90, "Parallelization" -> False, "ProgressReporting" -> False, "ReturnDetails" -> True]]];
  If[!AssociationQ[wlResult], fail[{"Invalid Wolfram result",wlResult}]];
  {timings, nativeAmp} = timeNative[batch, If[Length[proc] > 5, 1, 3]];
  If[nativeAmp === None, fail[{name, "C++ completion failed.", timings}]];
  stored = SelectFirst[channelsModel[[2]], DeleteCases[Drop[#,-2], ""] === proc &];
  If[!ListQ[stored], fail["The validation-only model entry was not found."]];
  expressions = <|"Stored" -> Times @@ Take[stored,-2], "Seed" -> seedAmp|>;
  If[wlResult["Status"] === "COMPLETE", AssociateTo[expressions, "Wolfram" -> wlResult["Amplitude"]]];
  If[nativeAmp =!= None, AssociateTo[expressions, "Native" -> nativeAmp]];
  checks = rawResidues[#, cs] & /@ expressions;
  If[checks["Stored"] =!= <|"PASS" -> Length[cs]|>, fail["Independent check failed on the stored model entry."]];
  report = <|"Process" -> proc, "Preparation" -> mode, "SeedDiagrams" -> Length[batch["Diagrams"]],
    "ChannelRows" -> Length[cs], "Wolfram" -> <|"Status" -> wlResult["Status"], "Route" -> wlResult["CorrectionSpace"],
    "Seconds" -> wlTime|>, "Native" -> timings, "IndependentPhysicalResidues" -> checks,
    "IndependentOffCut" -> offCut[expressions, Drop[First[cs],-3]]|>;
  If[KeyExistsQ[checks,"Native"] && checks["Native"] =!= <|"PASS" -> Length[cs]|>,
    fail[{name,"C++ claimed completion but failed independent physical checks.",checks}]];
  If[name === "eePPMissingDiagram",
    If[!AllTrue[report["IndependentOffCut"], TrueQ[#["NativeMatchesWolfram"]] &],
      fail[{name, "The corrected cyclic control differs from the Wolfram completion off cut."}]],
    If[!AllTrue[report["IndependentOffCut"], TrueQ[#["MatchesStored"]["Native"]] &],
      fail[{name, "The C++ candidate disagrees with the stored amplitude at generic momenta."}]]];
  AssociateTo[reports, name -> report];
  Export[FileNameJoin[{output,name<>".json"}],report,"RawJSON"];
  Print[{name,report["Wolfram"],timings["Automatic"],checks}],
  {name,selection}];
processor = If[$OperatingSystem === "MacOSX",
  StringTrim[RunProcess[{"sysctl", "-n", "machdep.cpu.brand_string"}, "StandardOutput"]], $ProcessorType];
Export[FileNameJoin[{output,"factorization-benchmark-results.json"}], <|
  "Platform" -> $SystemID, "Processor" -> processor, "LogicalProcessors" -> $ProcessorCount, "WolframVersion" -> $Version,
  "Preparation" -> "AllPhysical uses Mathematica preparation. Cyclic uses native batch preparation. Both completion functions then consume the same prepared inputs and remainders.",
  "WolframTiming" -> "One serial in-memory CompleteFactorization call per case. Includes its internal fit and validation.",
  "NativeTiming" -> "Median of three completion calls per worker mode at four and five points, alternating which mode runs first. Six points uses one call per worker mode. Preparation and expression transfer excluded. BridgeSeconds includes both transfers and process startup.",
  "IndependentChecks" -> "Two exact samples per raw physical channel, followed by two exact conserved off-cut points comparing every external spin coefficient with the separately stored model amplitude. The stored result is never supplied to completion.",
  "Cases" -> reports|>,"RawJSON"];
