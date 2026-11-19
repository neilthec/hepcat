(* Run with HEPCAT_BATCH_BINARY set to the compiled hepcat-reduce-amplitude. *)
$HistoryLength = 0;
nativeDirectory = DirectoryName[$InputFileName];
repo = Replace[Environment["HEPCAT_ROOT"], Except[_String] :> DirectoryName[nativeDirectory]];
Get[FileNameJoin[{repo, "source", "HEPCAT.wl"}]];
Get[FileNameJoin[{nativeDirectory, "NativeAmplitude.wl"}]];
binary = Environment["HEPCAT_BATCH_BINARY"];
output = Replace[Environment["HEPCAT_BATCH_RESULTS"], Except[_String] :>
  FileNameJoin[{repo, "tests", "results", "native-batch-benchmark"}]];
If[!DirectoryQ[output], CreateDirectory[output, CreateIntermediateDirectories -> True]];
fail[message_] := (Print["ERROR ", message]; Exit[2]);
If[!StringQ[binary] || !FileExistsQ[binary], fail["Set HEPCAT_BATCH_BINARY to the native executable"]];
diagramModel = ReadModel[FileNameJoin[{repo, "models"}], 3];
channelModel = ReadModel[FileNameJoin[{repo, "models"}], 4];
resultKeys = {"Diagrams", "FactorizationProducts", "DiagramRemainders", "FactorizationRemainders"};
bind[diagrams_, channels_, workers_] := Module[{answer},
  answer = HEPCATNative`ReduceNativeAmplitude[diagrams, channels, binary, "Workers" -> workers];
  If[!AssociationQ[answer] || !AllTrue[Join[resultKeys, {"ReductionSeconds", "WorkersUsed"}],
      KeyExistsQ[answer, #] &], fail[{"Invalid native result", Short[answer, 2]}]];
  answer
];
timed[mode_, diagrams_, channels_] := Module[{diag, chan, out, wall, workers, seconds},
  wall = First[AbsoluteTiming[
    If[mode === "Separate",
      diag = bind[diagrams, {}, Automatic]; chan = bind[{}, channels, Automatic];
      out = AssociationMap[Join[diag[#], chan[#]] &, resultKeys];
      workers = {diag["WorkersUsed"], chan["WorkersUsed"]};
      seconds = diag["ReductionSeconds"] + chan["ReductionSeconds"],
      out = bind[diagrams, channels, If[mode === "Serial", 1, Automatic]];
      workers = out["WorkersUsed"]; seconds = out["ReductionSeconds"]]]];
  <|"Output" -> KeyTake[out, resultKeys], "Run" -> <|"Mode" -> mode,
    "NativeSeconds" -> seconds, "BridgeSeconds" -> wall, "Workers" -> workers|>|>
];
(* Check exact spin polynomials off all internal poles. Xi spinors are mapped by
   HEPCAT's independent typed evaluator. The factorization bookkeeping variable
   is assigned a nonzero rational value only after the expression is transferred. *)
offCutChecks[orig_List, reduced_List, remainder_List, masses_List] := Module[
  {checks = {}, params, chart, coordinates, sampled, mom, spins, toPoly, all, values, triplets},
  all = Join[orig, reduced, remainder];
  Do[
    params = Join[ModelParameterRules[diagramModel, {"Me" -> mass, "EE" -> 1/(mass + 1)}],
      {ConstructiveDiagrams`z -> 2 + 1/mass}];
    chart = ConstructiveDiagrams`Private`completionHESymbolicExternalChart[Thread[(Mass /@ Range[Length[masses]]) -> masses] /. params];
    If[!AssociationQ[chart] || !TrueQ[chart["ExternalMassShellsValidated"]] ||
      !TrueQ[chart["MomentumConservationValidated"]] || chart["InternalOnShellConditions"] =!= {}, fail["Invalid external chart"]];
    coordinates = chart["ExternalIdentityCoordinates"]; spins = chart["SpinVariables"];
    Do[
      sampled = Join[chart /. Thread[coordinates -> Prime[Range[Length[coordinates]] + offset]], <|"ReferenceSeed" -> 101 + offset|>];
      mom = sampled["Momenta"];
      If[Total[Values[mom]] =!= ConstantArray[0, {2,2}] ||
        !AllTrue[Keys[mom], Det[mom[#]] === sampled["Masses"][#]^2 &], fail["Invalid test momenta"]];
      toPoly[expr_] := Module[{v = ConstructiveDiagrams`Private`spuriousEvaluateCurve[expr, sampled, params]},
        If[!FreeQ[v, _Real | _Missing | _Failure | Indeterminate | _DirectedInfinity] ||
            !PolynomialQ[v, spins] || !AllTrue[Last /@ CoefficientRules[Expand[v], spins], NumericQ], fail[{"Unresolved chart", Short[v, 2]}]];
        Expand[v]];
      values = AssociationMap[toPoly, DeleteDuplicates[all]];
      triplets = Transpose[{(values[#] &) /@ orig, (values[#] &) /@ reduced, (values[#] &) /@ remainder}];
      If[!AllTrue[triplets, TrueQ[Expand[#[[1]] - #[[2]] - #[[3]]] === 0] &],
        fail[{"Off-shell reconstruction", mass, offset, Flatten[Position[(Expand[#[[1]]-#[[2]]-#[[3]]] === 0 &) /@ triplets, False]]}]];
      If[!AllTrue[First /@ triplets, # =!= 0 &], fail["Degenerate input sample"]];
      AppendTo[checks, <|"ElectronMass" -> mass, "Coordinates" -> Prime[Range[Length[coordinates]] + offset],
        "ReferenceSeed" -> sampled["ReferenceSeed"], "Items" -> Length[triplets]|>],
      {offset, 1, 2}], {mass, {2, 3}}];
  checks
];
(* Cancel the explicit channel propagator and evaluate its numerator on an
   independently generated, exact, single-channel mass shell. *)
onCutCheck[original_, reduced_, sample_] := Module[{n, cut, mass, masses, chan, params, pole, evaluate, kin, chart, invariant, numerators, vals},
  n = Length[original] - 3; masses = Take[original, n];
  cut = List @@ original[[-3]]; mass = original[[-2]];
  chan = <|"ExternalLegs" -> Range[n], "Cut" -> cut, "Complement" -> Complement[Range[n], cut], "Mass" -> mass,
    "ExternalMassRules" -> Thread[(Mass /@ Range[n]) -> masses]|>;
  params = Join[ModelParameterRules[diagramModel, {"Me" -> 2, "EE" -> 1/3}], {ConstructiveDiagrams`z -> 5/2}];
  pole = PropDen[Mom[original[[-3]]], mass];
  (* Times performs the common case of a syntactically factored pole exactly.
     Cancel handles sums whose terms each carry that explicit pole. *)
  numerators = (Function[expr, With[{p = pole expr}, If[FreeQ[p, pole], p, Cancel[p]]]] /@ {Last[original], Last[reduced]});
  Do[
    kin = ConstructiveDiagrams`Private`residueAutomaticChart[chan, seed, 28183, params];
    If[!AssociationQ[kin], Continue[]];
    chart = kin["Chart"] /. kin["Chart"]["PoleVariable"] -> 0;
    invariant = Det[Total[Lookup[chart["Momenta"], cut]]] - (mass /. params)^2;
    If[invariant =!= 0, fail["Invalid channel chart"]];
    vals = Quiet[ConstructiveDiagrams`Private`spuriousEvaluateCurve[#, chart, params] & /@ numerators];
    If[AllTrue[vals, NumericQ[#] && FreeQ[#, _Real | Indeterminate | _DirectedInfinity] &] && First[vals] =!= 0,
      If[!TrueQ[Expand[First[vals] - Last[vals]] === 0], fail[{"Factorization residue mismatch", cut}]];
      Return[True, Module]], {seed, sample, sample+5}];
  fail[{"No regular nonzero sample on channel", cut}]
];
(* Two different actual four-point cuts make the dropped continuations visible.
   The clean diagram and clean channel have zero remainder for these inputs. *)
remainderControl[diagrams_, channels_, masses_] := Module[
  {diagram = First[diagrams], dcut, channel, ccut, dterm, cterm, control, clean, rem, input, red},
  dcut = FirstCase[First[diagram], HoldPattern[Mass[Multiparticle[i_, j_]] -> _] :> {i,j}];
  channel = SelectFirst[channels, With[{cut = List @@ #[[-3]]},
      Sort[cut] =!= Sort[dcut] && Sort[cut] =!= Complement[Range[Length[masses]], dcut]] &];
  If[!ListQ[dcut] || !ListQ[channel], fail["Missing distinct benchmark cuts"]];
  ccut = List @@ channel[[-3]];
  dterm = 7 (MomProd @@ dcut); cterm = 11 (MomProd @@ ccut);
  clean = bind[{diagram}, {channel}, 1];
  If[Lookup[clean, {"DiagramRemainders","FactorizationRemainders"}] =!= {{0},{0}}, fail["Remainder control baseline changed"]];
  control = bind[{MapAt[(# + dterm) &, diagram, -1]}, {MapAt[(# + cterm) &, channel, -1]}, 2];
  rem = Join[control["DiagramRemainders"], control["FactorizationRemainders"]];
  If[Length[rem] =!= 2 || MemberQ[rem, 0], fail["Missing discarded remainder"]];
  input = {dterm, cterm, ReduceSpinContractions[Last[diagram]] /. First[diagram], Last[channel]};
  red = Join[rem, Last /@ Join[control["Diagrams"], control["FactorizationProducts"]]];
  offCutChecks[input, red, {0,0,0,0}, masses];
  <|"DiagramShell" -> dcut, "ChannelShell" -> ccut, "CheckedNonzeroRemainders" -> 2|>
];
processes = <|"qed2" -> {"e", "E", "A.+", "A.+"}, "qed3" -> {"e", "E", "A.+", "A.+", "A.-"},
  "qed4" -> {"e", "E", "A.+", "A.+", "A.+", "A.+"}|>;
reports = <||>;
repetitions = 7;
Do[
  proc = processes[name]; n = Length[proc];
  diagrams = DiagramAmplitudes[CreateDiagrams[proc, diagramModel]];
  channels = FactorizationAmplitudeChannels[proc, channelModel];
  If[Length[diagrams] =!= Factorial[n-2] || Length[channels] =!= 2^(n-2)-2, fail["Unexpected job count"]];
  Print[{name, "Native timing", Length[diagrams], Length[channels]}];
  (* Load the bridge and the native executable before recording wall times. Each
     timed call starts a fresh C++ process with the same immutable input rows. *)
  baseline = KeyTake[bind[diagrams, channels, 1], resultKeys];
  records = {};
  Do[
    Do[measurement = timed[mode, diagrams, channels]; out = measurement["Output"];
      If[out =!= baseline, fail[{name, mode, "Output differs from serial"}]];
      AppendTo[records, measurement["Run"]], {mode, RotateLeft[{"Serial", "Combined", "Separate"}, rep-1]}],
    {rep, 1, repetitions}];
  If[(First /@ out["Diagrams"]) =!= (First /@ diagrams) || (Most /@ out["FactorizationProducts"]) =!= (Most /@ channels),
    fail["Input metadata changed"]];
  If[Length /@ Lookup[out, resultKeys] =!= Length /@ {diagrams, channels, diagrams, channels}, fail["Unexpected output lengths"]];
  originals = Join[(ReduceSpinContractions[Last[#]] /. First[#] &) /@ diagrams, Last /@ channels];
  prepared = Last /@ Join[out["Diagrams"], out["FactorizationProducts"]];
  remainders = Join[out["DiagramRemainders"], out["FactorizationRemainders"]];
  checks = offCutChecks[originals, prepared, remainders, Take[First[channels], n]];
  residues = MapThread[onCutCheck[#1, #2, 1] &, {channels, out["FactorizationProducts"]}];
  statistics = AssociationMap[Function[m, With[{runs = Select[records, #["Mode"] === m &]},
    With[{native = Lookup[runs, "NativeSeconds"], bridge = Lookup[runs, "BridgeSeconds"]},
      <|"Workers" -> DeleteDuplicates[Lookup[runs,"Workers"]], "MedianNativeSeconds" -> Median[native],
        "NativeRangeSeconds" -> MinMax[native], "NativeSamplesSeconds" -> native,
        "MedianBridgeSeconds" -> Median[bridge], "BridgeRangeSeconds" -> MinMax[bridge],
        "BridgeSamplesSeconds" -> bridge|>]]], {"Serial","Combined","Separate"}];
  report = <|"Process" -> proc, "Diagrams" -> Length[diagrams], "FactorizationProducts" -> Length[channels],
    "Timings" -> statistics, "AllOutputsIdentical" -> True, "OffCutSamples" -> checks,
    "NonzeroChannelResiduesCompared" -> Count[residues, True], "ReturnedRemainders" -> Count[remainders, Except[0]]|>;
  If[name === "qed2", AssociateTo[report, "RemainderControl" -> remainderControl[diagrams, channels, Take[First[channels], n]]]];
  AssociateTo[reports, name -> report];
  Export[FileNameJoin[{output,name<>".json"}], Join[report, <|"Runs" -> records|>], "RawJSON"];
  Print[{name, (KeyTake[#, {"MedianNativeSeconds", "MedianBridgeSeconds"}] & /@ statistics), "Exact checks passed"}], {name, Keys[processes]}];
cpu = If[$OperatingSystem === "MacOSX", StringTrim[RunProcess[{"sysctl","-n","machdep.cpu.brand_string"}, "StandardOutput"]], $ProcessorType];
Export[FileNameJoin[{output, "batch-benchmark-results.json"}], <|
  "Platform" -> $SystemID, "Processor" -> cpu, "LogicalProcessors" -> $ProcessorCount, "WolframVersion" -> $Version,
  "Repetitions" -> repetitions, "TimingOrder" -> "Serial, Combined, Separate rotated by one place for each repetition, after a serial warmup.",
  "NativeSeconds" -> "Native reduction only. Separate sums two calls, diagrams followed by factorization products.",
  "BridgeSeconds" -> "Wall time for complete Wolfram bridge calls on pre-generated rows, including encoding, process startup, decoding and returning expressions.",
  "Validation" -> "Every spin-contracted input equals its reduced expression plus its remainder in every spin coefficient at four regular exact points. Each product also retains its nonzero residue on an independent exact sample of its own channel. Raw Xi labels retained.",
  "Cases" -> reports|>, "RawJSON"];
