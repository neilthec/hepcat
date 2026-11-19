(* Optional interface to the native amplitude reducer. Load HEPCAT once. *)
If[!MemberQ[$Packages, "ConstructiveDiagrams`"],
  Get[FileNameJoin[{DirectoryName[DirectoryName[$InputFileName]], "source", "HEPCAT.wl"}]]];
BeginPackage["HEPCATNative`", {"ConstructiveDiagrams`"}];

GenerateNativeAmplitude::usage =
  "GenerateNativeAmplitude[particles, directory, vertexModelNumber, factorizationModelNumber, executable] reads both constructive models in C++, generates the diagram amplitudes and lower-point products, and reduces each with its own numerator shells on the worker that sewed it. The result can be passed to CompleteNativeFactorization. Separate {directory, number} pairs can also be supplied. With one model number the function generates diagrams only, including raw tensors when reading a Feynman model. Workers selects the worker count. Generate selects Both, Diagrams or Channels.";
Options[GenerateNativeAmplitude] = {"Workers" -> Automatic, "PolePreference" -> Automatic, "Generate" -> "Both"};

ReduceNativeAmplitude::usage =
  "ReduceNativeAmplitude[process, diagramModel, factorizationModel, executable] generates all diagrams and factorization products, then reduces both together in C++. ReduceNativeAmplitude[diagramPairs, channelRows, executable] accepts existing HEPCAT rows. For example, with loaded models qed and qedFactorization, run ReduceNativeAmplitude[{\"e\", \"E\", \"A.+\", \"A.+\"}, qed, qedFactorization, binary]. The result contains Diagrams, FactorizationProducts, their aligned DiagramRemainders and FactorizationRemainders, WorkersUsed and ReductionSeconds. The remainders restore each spin-contracted input away from its numerator cuts in the chosen reference. Workers -> Automatic uses the available native workers; 1 requests serial reduction. PolePreference -> None keeps supplied references. Cyclic uses a common massless reference cycle. ExternalMasses can provide the ordered masses for empty collections.";
Options[ReduceNativeAmplitude] = {"Workers" -> Automatic, "PolePreference" -> None, "ExternalMasses" -> Automatic};

CompleteNativeFactorization::usage =
  "CompleteNativeFactorization[batch, executable] compares prepared diagrams with the sewn physical channels and fits missing pole terms in C++. Supply the batch returned by ReduceNativeAmplitude to reuse its retained remainders. It also accepts [diagramPairs, channelRows, executable] or [process, diagramModel, factorizationModel, executable] for raw input. The result is {externalMassRules, expression}. ReturnDetails -> True includes the native time and validation sample counts. Two detailed PrepareAmplitudes reports can also be passed together as [diagramReport, channelReport, executable]. Raw input uses Automatic reference assignment unless overridden. CorrectionBasis can supply additional full-amplitude directions.";
Options[CompleteNativeFactorization] = Join[
  DeleteCases[Options[ReduceNativeAmplitude], ("PolePreference" -> _)], {"PolePreference" -> Automatic},
  {"ReturnDetails" -> False, "CorrectionBasis" -> Automatic}];

CompleteNativeHighEnergy::usage =
  "CompleteNativeHighEnergy[amp, model, executable] checks the fixed-angle high-energy bound and fits a local correction if needed. amp can be the external-mass pair or details returned by CompleteNativeFactorization. CompleteNativeHighEnergy[amp, batch, model, executable] also supplies retained reduction data. Passing the prepared batch alone runs both native stages in one process. Use [amp, originalChannelRows, model, executable] to also require the full Wolfram CheckAmplitudes report before returning. ReturnDetails -> True reports the fit and native time. HighEnergyBound defaults to 4 minus the number of external legs. ParameterPoints accepts lists of exact model overrides for fitting and validation.";
Options[CompleteNativeHighEnergy] = {"Workers" -> Automatic, "CorrectionBasis" -> Automatic,
  "ReturnDetails" -> False, "HighEnergyBound" -> Automatic, "ParameterPoints" -> Automatic};

Begin["`Private`"];

$batchFailure = Unique["batchFailure"];
fail[code_, message_, details_: <||>] :=
  Throw[Failure[code, Join[<|"MessageTemplate" -> message|>, details]], $batchFailure];

(* Only the heads with native semantics have short names on the wire. A user
   symbol named Mom or an unexported color index retains its full identity. *)
$coreSymbols = {Plus, Times, Power, List, Spinor, SpinorChain, SpinorTrace, Mom, MomProd,
  Mass, Multiparticle, Mandelstahm, PropDen, ModelParameter};
wireName[s_Symbol] := If[MemberQ[$coreSymbols, s], SymbolName[s], Context[s] <> SymbolName[s]];
wire[s_Symbol] := wireName[s];
wire[i_Integer] := ToString[i, InputForm];
wire[r_Rational] := "Rational[" <> wire[Numerator[r]] <> "," <> wire[Denominator[r]] <> "]";
wire[c_Complex] := "System`Complex[" <> wire[Re[c]] <> "," <> wire[Im[c]] <> "]";
wire[s_String] := StringReplace[ToString[s, InputForm, PageWidth -> Infinity], {"\n" -> "\\n", "\r" -> "\\r"}];
wire[x_] := If[AtomQ[x] || Head[Head[x]] =!= Symbol,
  fail["NativeExpression", "The native reducer expects an exact expression with symbolic heads.", <|"Expression" -> x|>],
  wireName[Head[x]] <> "[" <> StringRiffle[wire /@ (List @@ x), ","] <> "]"];

(* The reply encodes the same dialect. Source symbols outside the core have
   their own explicit contexts and cannot accidentally bind to notebook names. *)
decode[s_String] := Block[{$ContextPath = {"ConstructiveDiagrams`", "System`"}, $Context = "HEPCATNative`Private`"},
  With[{expr = Quiet[Check[ToExpression[s, InputForm], $Failed]]},
    If[expr === $Failed, fail["NativeProtocol", "The executable returned an invalid expression."], expr]]];

externalMasses[rules_List] := Module[{external},
  external = SortBy[DeleteDuplicates[Cases[rules, r : HoldPattern[Mass[_Integer] -> _] :> r]], First[First[#]] &];
  If[external === {} || First /@ external =!= Mass /@ Range[Length[external]],
    fail["NativeExternalMasses", "Each diagram must declare one mass for every consecutive external leg."]];
  Last /@ external
];

shell[legs_List, mass_] := "Shell[" <> wire[legs] <> "," <> wire[mass] <> "]";
diagramShells[rules_List, n_Integer] := Module[{shells, cuts},
  shells = Cases[rules, HoldPattern[Mass[Multiparticle[legs__Integer]] -> mass_] :> {{legs}, mass}];
  If[!AllTrue[shells, Function[p, 2 <= Length[First[p]] < n &&
      DuplicateFreeQ[First[p]] && AllTrue[First[p], 1 <= # <= n &]]],
    fail["NativeShell", "An internal shell contains an invalid external leg subset."]];
  (* The diagram model can list both orientations of one internal line. *)
  cuts = GatherBy[shells, ConstructiveDiagrams`Private`canonicalPhysicalCut[First[#], Range[n]] &];
  If[AnyTrue[cuts, Length[DeleteDuplicates[Last /@ #]] > 1 &],
    fail["NativeShell", "Complementary internal shells have conflicting masses."]];
  "List[" <> StringRiffle[(shell @@ First[#] &) /@ cuts, ","] <> "]"
];

fixedExpression[pair_, masses_, preference_, inventory_] := Module[{value, result, massless, targets},
  If[TrueQ[ConstructiveDiagrams`Private`feynmanRawExpressionQ[Last[pair]]],
    fail["NativeFeynmanTensors", "Convert Feynman tensors to spinor products before native reduction and supply only external mass rules."]];
  value = ReduceSpinContractions[Last[pair]] /. First[pair];
  If[!FreeQ[value, _Mass], fail["NativeMass", "An expression contains a mass without a supplied replacement."]];
  If[preference === None || FreeQ[value, Spinor["Xi", ___] | SpinorHat["Xi", ___]], Return[value]];
  massless = Flatten[Position[masses, 0, {1}]];
  If[preference =!= "Cyclic",
    result = ConstructiveDiagrams`Private`preparationReferences[value, First[pair], {}, inventory, preference];
    If[!AssociationQ[result] || result["Status"] =!= "COMPLETE" ||
        !FreeQ[result["Expression"], Spinor["Xi", ___] | SpinorHat["Xi", ___]],
      fail["NativeReference", "Could not replace the external references using the supplied physical channels.",
        <|"ReferenceReport" -> result|>]];
    Return[result["Expression"]]];
  If[Length[massless] < 2, fail["NativeReference", "A shared massless reference cycle requires two massless external legs."]];
  targets = AssociationThread[massless, RotateLeft[massless]];
  (* With two or more massless legs, the shared automatic route uses a strict
     massless cycle independent of physical-pole inventories. *)
  result = ConstructiveDiagrams`Private`preparationReferences[value, First[pair], {}, {}, Automatic, {}, True];
  If[!AssociationQ[result] || result["Status"] =!= "COMPLETE" ||
      !FreeQ[result["Expression"], Spinor["Xi", ___] | SpinorHat["Xi", ___]] ||
      !AllTrue[result["Assignments"], Function[r, KeyExistsQ[targets, r["Leg"]] && targets[r["Leg"]] === r["Target"]]],
    fail["NativeReference", "The supplied reference spinors could not be assigned consistently across the batch.",
      <|"ReferenceReport" -> result|>]];
  result["Expression"]
];

runBatch[diagrams_, channels_, executable_, workers_, preference_, specifiedMasses_, completion_: None] := Module[
  {pairs, masses, candidates, n, diagramLines, channelLines, fields, output, temp,
   count, command, run, response, body, values, remainders, nd, threads, seconds, header,
   prepared, expression, diagramRest, channelRest, basis, result, energy, points, bound, seed},
  If[!MemberQ[{None, Automatic, "AllPhysical", "AllSpurious", "Mixed", "Cyclic"}, preference],
    fail["NativeReference", "Use Automatic, AllPhysical, AllSpurious, Mixed, Cyclic, or None for the reference choice."]];
  count = Replace[workers, Automatic -> 0];
  If[!IntegerQ[count] || !(workers === Automatic || count > 0),
    fail["NativeWorkers", "Workers must be Automatic or a positive integer."]];
  If[!FileExistsQ[executable] || DirectoryQ[executable],
    fail["NativeExecutable", "The native amplitude executable could not be found.", <|"Executable" -> executable|>]];
  If[!AllTrue[diagrams, ConstructiveDiagrams`Private`preparationPairQ] ||
      !AllTrue[channels, ConstructiveDiagrams`Private`preparationChannelQ],
    fail["NativeRows", "Expected diagram mass-rule pairs and complete HEPCAT factorization rows."]];
  pairs = Join[diagrams, ConstructiveDiagrams`Private`preparationInputPair[#, "FactorizationChannel"] & /@ channels];
  candidates = Join[If[specifiedMasses === Automatic, {}, {specifiedMasses}],
    externalMasses /@ (First /@ diagrams), Drop[#, -3] & /@ channels];
  If[candidates === {} || !ListQ[First[candidates]] || First[candidates] === {} ||
      !SameQ @@ candidates || !FreeQ[candidates, _Missing | _Mass | _Real | _Failure | Indeterminate | _DirectedInfinity],
    fail["NativeExternalMasses", "All items must have the same ordered exact external masses. For an empty batch supply ExternalMasses."]];
  masses = First[candidates]; n = Length[masses]; nd = Length[diagrams];
  energy = AssociationQ[completion] && Lookup[completion, "Stage", None] === "HighEnergy";
  prepared = AssociationQ[completion] && TrueQ[completion["Prepared"]];
  expression[p_] := If[prepared, Last[p], fixedExpression[p, masses, preference, pairs]];
  diagramLines = Function[p, {diagramShells[First[p], n], wire[expression[p]]}] /@ diagrams;
  channelLines = MapThread[Function[{row, p},
    {shell[List @@ row[[-3]], row[[-2]]], wire[expression[p]]}], {channels, Drop[pairs, nd]}];
  If[prepared,
    diagramRest = completion["DiagramRemainders"]; channelRest = completion["FactorizationRemainders"];
    If[!ListQ[diagramRest] || !ListQ[channelRest] || Length[diagramRest] =!= nd || Length[channelRest] =!= Length[channels],
      fail["NativeRows", "Supply one discarded remainder per prepared diagram and per factorization product."]];
    diagramLines = MapThread[Append[#1, wire[#2]] &, {diagramLines, diagramRest}];
    channelLines = MapThread[Append[#1, wire[#2]] &, {channelLines, channelRest}]];
  header = Which[energy, "HEPCAT_HIGH_ENERGY_1", prepared, "HEPCAT_FACTOR_PREPARED_1", AssociationQ[completion], "HEPCAT_FACTOR_1", True, "HEPCAT_BATCH_1"];
  fields = Join[{header, wire[masses], ToString[nd]}, Flatten[diagramLines],
    {ToString[Length[channels]]}, Flatten[channelLines]];
  If[AssociationQ[completion],
    basis = Replace[completion["Basis"], Automatic -> {}];
    If[!ListQ[basis], fail["NativeBasis", "CorrectionBasis must be Automatic or a list of amplitude expressions."]];
    fields = Join[fields, {ToString[Length[basis]]}, wire /@ basis]];

  If[energy,
    seed = completion["Seed"]; bound = completion["Bound"];
    If[bound =!= Automatic && !IntegerQ[bound], fail["NativeBound", "HighEnergyBound must be Automatic or an integer."]];
    points = highEnergyPoints[completion["Model"], completion["Points"],
      DeleteDuplicates[Cases[{masses,seed,basis,pairs,diagramRest,channelRest}, _ModelParameter, Infinity]]];
    fields = Join[fields, {If[seed === Automatic, "", wire[seed]], ToString[bound], ToString[Length[points]]},
      Flatten[Function[point, Join[{ToString[Length[point]]}, ({wire[First[#]],wire[Last[#]]} &) /@ point]] /@ points]]];

  temp = CreateDirectory[];
  If[!StringQ[temp], fail["NativeTemporaryDirectory", "Could not create the native reduction's temporary directory."]];
  Internal`WithLocalSettings[Null,
    output = FileNameJoin[{temp, "result.txt"}];
    command = {ExpandFileName[executable], FileNameJoin[{temp, "input.txt"}], output, ToString[count]};
    If[Export[command[[2]], StringRiffle[fields, "\n"] <> "\n", "Text", CharacterEncoding -> "UTF-8"] === $Failed,
      fail["NativeTransfer", "Could not write the batch to the native reducer."]];
    run = RunProcess[command];
    If[!AssociationQ[run] || run["ExitCode"] =!= 0 || !FileExistsQ[output],
      fail[Which[energy, "IncompleteHighEnergy", completion === None, "NativeReduction", True, "IncompleteFactorization"],
        Which[energy, "Native high-energy completion failed.", completion === None, "The native batch reduction failed.",
          True, "Native factorization did not produce a validated amplitude."],
        If[AssociationQ[run], KeyTake[run, {"ExitCode","StandardError"}], <||>]]];
    response = Import[output, "Lines", CharacterEncoding -> "UTF-8"];
    If[!ListQ[response] || Length[response] < 3 || !StringMatchQ[response[[2]], DigitCharacter ..] ||
        !StringMatchQ[response[[3]], RegularExpression["[0-9]+(?:\\.[0-9]+)?(?:[eE][+-]?[0-9]+)?"]],
      fail["NativeProtocol", "Invalid header or timing in the native response."]];
    threads = FromDigits[response[[2]]];
    seconds = ToExpression[StringReplace[response[[3]], {"e"|"E" -> "*^"}], InputForm];
    If[AssociationQ[completion],
      If[Length[response] =!= If[energy,14,10] || First[response] =!= If[energy, "HEPCAT_HIGH_ENERGY_RESULT_1", "HEPCAT_FACTOR_RESULT_1"] ||
          !StringMatchQ[response[[4]], If[energy, RegularExpression["-?[0-9]+"], DigitCharacter ..]] ||
          !AllTrue[response[[5 ;; 8]], StringMatchQ[#, DigitCharacter ..] &],
        fail["NativeProtocol", "Invalid completion result or sample counts."]];
      result = <|"Amplitude" -> {Thread[(Mass /@ Range[n]) -> masses], decode[Last[response]]},
        "CompletionSeconds" -> seconds, "WorkersUsed" -> threads, "Route" -> response[[9]],
        If[energy, "Bound", "Channels"] -> ToExpression[response[[4]],InputForm], "Candidates" -> FromDigits[response[[5]]],
        "Rank" -> FromDigits[response[[6]]], "TrainingSamples" -> FromDigits[response[[7]]],
        "ValidationSamples" -> FromDigits[response[[8]]]|>;
      If[energy,
        If[!StringMatchQ[response[[10]], DigitCharacter ..] || !MemberQ[{"0","1"},response[[11]]] ||
            !AllTrue[response[[12 ;; 13]], StringMatchQ[#, "-1" | (DigitCharacter ..)] &],
          fail["NativeProtocol", "Invalid spin coverage or memory measurements in the native response."]];
        AssociateTo[result, <|"ParameterPoints" -> points,
          "FreeCoefficients" -> If[result["Route"] === "Unchanged", Missing["NotFitted"], result["Candidates"] - result["Rank"]],
          "ValidationSpinStates" -> FromDigits[response[[10]]], "ExhaustiveSpin" -> (response[[11]] === "1"),
          "PreCompletionPeakRSSBytes" -> Replace[response[[12]], {"-1" -> Missing["Unavailable"], x_ :> FromDigits[x]}],
          "ProcessPeakRSSBytes" -> Replace[response[[13]], {"-1" -> Missing["Unavailable"], x_ :> FromDigits[x]}]|>]];
      If[TrueQ[completion["Details"]], result, result["Amplitude"]],
    If[Length[response] =!= 3 + 2 Length[pairs] || First[response] =!= "HEPCAT_BATCH_RESULT_1",
      fail["NativeProtocol", "Invalid item count in the native batch response."]];
    body = Partition[decode /@ Drop[response, 3], 2];
    values = First /@ body; remainders = Last /@ body;
    <|"Diagrams" -> MapThread[{First[#1], #2} &, {diagrams, Take[values, nd]}],
      "FactorizationProducts" -> MapThread[ReplacePart[#1, -1 -> #2] &, {channels, Drop[values, nd]}],
      "DiagramRemainders" -> Take[remainders, nd], "FactorizationRemainders" -> Drop[remainders, nd],
      "WorkersUsed" -> threads, "ReductionSeconds" -> seconds|>],
    DeleteDirectory[temp, DeleteContents -> True]]
];

ReduceNativeAmplitude[diagrams_List, channels_List, executable_String, OptionsPattern[]] := Catch[
  runBatch[diagrams, channels, executable, OptionValue["Workers"], OptionValue["PolePreference"], OptionValue["ExternalMasses"]],
  $batchFailure];

CompleteNativeFactorization[batch_Association, executable_String, OptionsPattern[]] := Catch[
  If[!AllTrue[{"Diagrams", "FactorizationProducts", "DiagramRemainders", "FactorizationRemainders"}, KeyExistsQ[batch, #] &],
    fail["NativeRows", "A prepared batch must contain the diagram and channel rows and their retained remainders."]];
  runBatch[batch["Diagrams"], batch["FactorizationProducts"], executable, OptionValue["Workers"], None,
    OptionValue["ExternalMasses"], Join[KeyTake[batch, {"DiagramRemainders", "FactorizationRemainders"}],
      <|"Prepared" -> True, "Details" -> OptionValue["ReturnDetails"], "Basis" -> OptionValue["CorrectionBasis"]|>]],
  $batchFailure];

CompleteNativeFactorization[diagramReport_Association, channelReport_Association, executable_String, opts : OptionsPattern[]] :=
  Catch[Module[{rows, rest},
    rows = Lookup[#, "PreparedAmplitudes", Missing[]] & /@ {diagramReport,channelReport};
    rest = Lookup[#, "RemainderData", Missing[]] & /@ {diagramReport,channelReport};
    If[!AllTrue[Join[rows,rest], ListQ] || !AllTrue[Flatten[rest,1],
        AssociationQ[#] && KeyExistsQ[#, "ShellRemainder"] &],
      fail["NativeRows", "Supply the detailed diagram and channel reports from PrepareAmplitudes."]];
    CompleteNativeFactorization[<|"Diagrams" -> First[rows], "FactorizationProducts" -> Last[rows],
      "DiagramRemainders" -> (Lookup[#, "ShellRemainder"] & /@ First[rest]),
      "FactorizationRemainders" -> (Lookup[#, "ShellRemainder"] & /@ Last[rest])|>, executable, opts]], $batchFailure];

CompleteNativeFactorization[diagrams_List, channels_List, executable_String, OptionsPattern[]] := Catch[
  runBatch[diagrams, channels, executable, OptionValue["Workers"], OptionValue["PolePreference"], OptionValue["ExternalMasses"],
    <|"Prepared" -> False, "Details" -> OptionValue["ReturnDetails"], "Basis" -> OptionValue["CorrectionBasis"]|>], $batchFailure];

generatedBatch[process_, diagramModel_, factorizationModel_, executable_, workers_, preference_, givenMasses_, completion_] :=
  Catch[Module[{diagrams, pairs, channels, masses = givenMasses},
    diagrams = CreateDiagrams[process, diagramModel];
    If[!ListQ[diagrams], fail["NativeDiagramGeneration", "HEPCAT diagram generation failed.", <|"Result" -> diagrams|>]];
    pairs = DiagramAmplitudes[diagrams];
    channels = FactorizationAmplitudeChannels[process, factorizationModel];
    If[!ListQ[pairs] || !ListQ[channels], fail["NativeGeneration", "HEPCAT did not generate both collections.",
        <|"DiagramFailure" -> If[ListQ[pairs], None, pairs], "ChannelFailure" -> If[ListQ[channels], None, channels]|>]];
    If[masses === Automatic, masses = ConstructiveDiagrams`Private`constructExternalMasses[process, diagramModel]];
    runBatch[pairs, channels, executable, workers, preference, masses, completion]], $batchFailure];

ReduceNativeAmplitude[process : {__String}, diagramModel_List, factorizationModel_List, executable_String, OptionsPattern[]] :=
  generatedBatch[process, diagramModel, factorizationModel, executable, OptionValue["Workers"], OptionValue["PolePreference"],
    OptionValue["ExternalMasses"], None];

CompleteNativeFactorization[process : {__String}, diagramModel_List, factorizationModel_List, executable_String, OptionsPattern[]] :=
  generatedBatch[process, diagramModel, factorizationModel, executable, OptionValue["Workers"], OptionValue["PolePreference"],
    OptionValue["ExternalMasses"], <|"Prepared" -> False, "Details" -> OptionValue["ReturnDetails"],
      "Basis" -> OptionValue["CorrectionBasis"]|>];

(* Use exact generic parameter values for identities in model variables. Small
   rational points on x^2+y^2=1 keep common square-root definitions rational.
   The definitions themselves are always evaluated by the model reader. *)
highEnergyPoints[{}, Automatic, {}] := {};
highEnergyPoints[model_List, requested_, active_List] := Module[
  {points, base, independent, declared, assignments, rules, variant, pick, complete},
  If[model =!= {} && (Length[model] =!= 4 || !AllTrue[model, ListQ]),
    fail["NativeModel", "Supply a model returned by ReadModel."]];
  If[requested === Automatic,
    If[active === {}, Return[{}]];
    If[model === {}, fail["NativeParameters", "Supply the model or explicit ParameterPoints for model-scoped parameters."]];
    base = ModelParameterRules[model];
    independent = First /@ model[[3]];
    pick[value_, i_, k_] := Module[{u = (1 + Mod[i + 2 k, 7])/(9 + Mod[3 i + k, 11]), m},
      If[TrueQ[value == 0], Return[0]];
      If[!TrueQ[Element[value,Reals]], Return[value]];
      m = Abs[value];
      If[m < 1, Sign[value] 2 u/(1 + u^2),
        Sign[value] (1 + Mod[i + k, 5]) 2^Floor[Log[2, m]]/(1 + Mod[i + 2 k, 3])]];
    points = Table[MapIndexed[Function[{parameter,index}, With[{value = parameter /. base},
      If[NumberQ[value] && FreeQ[value, _Real], parameter -> pick[value, First[index], variant], Nothing]]],
      independent], {variant, 3}],
    points = requested];
  If[!ListQ[points] || points === {} || !AllTrue[points, MatchQ[#, {___Rule}] &],
    fail["NativeParameters", "ParameterPoints must be a nonempty list of exact parameter-rule lists."]];
  declared = If[model === {}, {}, First /@ Join[model[[3]],model[[4]]]];
  complete[point_List] := Module[{normalized, extras, names},
    names = First /@ point;
    If[!DuplicateFreeQ[names] || !AllTrue[names,
        MatchQ[#, _Symbol | _ModelParameter] || (StringQ[#] && MemberQ[ModelParameterName /@ declared, #]) &],
      fail["NativeParameters", "Each point needs distinct parameters from the model or explicit symbolic parameters."]];
    extras = Select[point, Head[First[#]] === Symbol &];
    normalized = Select[point, Head[First[#]] =!= Symbol &];
    rules = If[model === {}, normalized, ModelParameterRules[model, normalized]];
    If[!ListQ[rules] || !ContainsAll[First /@ rules, active],
      fail["NativeParameters", "One or more model parameters could not be resolved from the supplied model and overrides."]];
    assignments = Join[Select[rules, MemberQ[active,First[#]] &],extras];
    If[!AllTrue[Last /@ assignments, MatchQ[#, _Integer | _Rational] &],
      fail["NativeParameters", "Native parameter samples require exact rational values. Supply ParameterPoints if the automatic model assignments leave a radical."]];
    assignments];
  DeleteDuplicates[complete /@ points]
];

emptyBatch = <|"Diagrams" -> {}, "FactorizationProducts" -> {}, "DiagramRemainders" -> {},
  "FactorizationRemainders" -> {}|>;
energyStage[input_, batch_, model_, executable_, workers_, basis_, details_, bound_, points_] := Catch[
  Module[{seed, masses = Automatic, opts, raw, inherited, directions},
    If[!AllTrue[Keys[emptyBatch], KeyExistsQ[batch,#] &],
      fail["NativeRows", "Supply a prepared native batch with its diagram and channel remainders."]];
    If[AssociationQ[input] && KeyExistsQ[input, "Status"] && input["Status"] =!= "COMPLETE",
      fail["NativeFactorization", "The supplied report does not contain a completed factorization.",
        <|"FactorizationStatus" -> input["Status"]|>]];
    raw = If[AssociationQ[input], Lookup[input, "Amplitude", Missing[]], input];
    If[AssociationQ[input] && !ListQ[raw] && KeyExistsQ[input,"ExternalMassRules"],
      raw = {input["ExternalMassRules"],raw}];
    If[raw === Automatic, seed = Automatic,
      If[!ConstructiveDiagrams`Private`preparationPairQ[raw] ||
          !AllTrue[First[raw], MatchQ[#, HoldPattern[Mass[_Integer] -> _]] &],
        fail["NativeSeed", "Supply a completed amplitude with only external mass rules, or a native factorization report."]];
      masses = externalMasses[First[raw]]; seed = Last[raw]];
    inherited = If[AssociationQ[input], Lookup[input, "RemainingBasis", {}], {}];
    If[!ListQ[inherited] || (basis =!= Automatic && !ListQ[basis]),
      fail["NativeBasis", "Supply an affine factorization report and a list of local correction directions."]];
    directions = DeleteDuplicates[Join[inherited, Replace[basis, Automatic -> {}]]];
    opts = Join[KeyTake[batch, {"DiagramRemainders", "FactorizationRemainders"}],
      <|"Stage" -> "HighEnergy", "Prepared" -> True, "Seed" -> seed, "Bound" -> bound,
        "Model" -> model, "Points" -> points, "Basis" -> directions, "Details" -> details|>];
    runBatch[batch["Diagrams"],batch["FactorizationProducts"],executable,workers,None,masses,opts]], $batchFailure];

CompleteNativeHighEnergy[input_, model_List, executable_String, opts : OptionsPattern[]] :=
  If[AssociationQ[input] && KeyExistsQ[input,"Diagrams"],
    CompleteNativeHighEnergy[Automatic,input,model,executable,opts],
    CompleteNativeHighEnergy[input,emptyBatch,model,executable,opts]];
CompleteNativeHighEnergy[input_, batch_Association, model_List, executable_String, OptionsPattern[]] :=
  energyStage[input,batch,model,executable,OptionValue["Workers"],OptionValue["CorrectionBasis"],
    OptionValue["ReturnDetails"],OptionValue["HighEnergyBound"],OptionValue["ParameterPoints"]];

(* With original channel rows present, run the independent package checker
   explicitly. Its timing and report stay separate from the native stage. *)
CompleteNativeHighEnergy[input_, channels_List, model_List, executable_String, opts : OptionsPattern[]] := Catch[
  Module[{result, point, report, elapsed, summary, settings},
    If[!AllTrue[channels, ConstructiveDiagrams`Private`preparationChannelQ],
      fail["NativeRows", "Supply the original channel rows returned by FactorizationAmplitudeChannels."]];
    settings = DeleteCases[{opts}, ("ReturnDetails" -> _)];
    result = CompleteNativeHighEnergy[input,model,executable,"ReturnDetails" -> True,Sequence @@ settings];
    If[FailureQ[result], Return[result]];
    point = If[result["ParameterPoints"] === {}, Automatic, Last[result["ParameterPoints"]]];
    {elapsed,report} = AbsoluteTiming[CheckAmplitudes[result["Amplitude"],channels,model,
      "Output" -> "Details", "ParameterRules" -> point, "HighEnergyCheck" -> True,
      "HighEnergyBound" -> result["Bound"]]];
    summary = If[AssociationQ[report], Lookup[report,"Summary",None], None];
    If[!AssociationQ[summary] || Keys[summary] =!= {"PASS"} || !TrueQ[summary["PASS"] > 0],
      fail["IncompleteAmplitude", "The native growth fit completed, but the independent amplitude checks did not all pass.",
        <|"ValidationReport" -> report, "CompletionSeconds" -> result["CompletionSeconds"]|>]];
    If[TrueQ[OptionValue["ReturnDetails"]],
      Join[result,<|"ValidationReport" -> report, "ValidationSeconds" -> elapsed|>],result["Amplitude"]]], $batchFailure];

(* The two model scope strings let native rows be combined directly with rows
   from ReadModel. Only file locations and particle names cross the input bridge. *)
GenerateNativeAmplitude[particles : {__String}, directory_String, vertex_Integer,
    executable_String, opts : OptionsPattern[]] :=
  If[MemberQ[{"Both", "Diagrams"}, OptionValue["Generate"]],
    GenerateNativeAmplitude[particles, directory, vertex, vertex, executable, "Generate" -> "Diagrams",
      Sequence @@ DeleteCases[{opts}, ("Generate" -> _)]],
    Failure["NativeModel", <|"MessageTemplate" -> "Supply a factorization model to generate channels."|>]];

GenerateNativeAmplitude[particles : {__String}, directory_String, vertex_Integer, factor_Integer,
    executable_String, opts : OptionsPattern[]] :=
  GenerateNativeAmplitude[particles,{directory,vertex},{directory,factor},executable,opts];

GenerateNativeAmplitude[particles : {__String}, {vd_String, vn_Integer}, {fd_String, fn_Integer},
    executable_String, OptionsPattern[]] := Catch[Module[
  {work = OptionValue["Workers"], refs = OptionValue["PolePreference"], mode = OptionValue["Generate"],
   input, temp, res, lines, counters, numbers, masses, next = 7, rows, diagrams, products, rules, make, root},
  If[work =!= Automatic && !(IntegerQ[work] && work >= 1), fail["NativeWorkers","Workers must be Automatic or a positive integer."]];
  If[!MemberQ[{None, Automatic, "Cyclic", "AllSpurious", "AllPhysical"},refs] || !MemberQ[{"Both","Diagrams","Channels"},mode],
    fail["NativeGeneration","Choose Automatic, Cyclic, AllSpurious, AllPhysical or None for PolePreference and Both, Diagrams or Channels for Generate."]];
  If[!FileExistsQ[executable] || DirectoryQ[executable],fail["NativeExecutable","Could not find the native generation executable."]];
  If[vn<0 || fn<0,fail["NativeModel","Model numbers must be nonnegative."]];
  input=Join[{"HEPCAT_GENERATE_1"},Flatten[Function[spec,{ExpandFileName[First[spec]],ToString[Last[spec]],
    ConstructiveDiagrams`Private`modelParameterScope[ExpandFileName[First[spec]],Last[spec]]}]/@{{vd,vn},{fd,fn}}],
    {mode,Switch[refs,None,"Keep","AllPhysical","Physical",_,"Automatic"],ToString[Length[particles]]},particles];
  If[!AllTrue[input,StringQ[#] && StringFreeQ[#, "\n"|"\r"]&],fail["NativeModel","Invalid path, model scope or particle label."]];
  temp=CreateDirectory[];
  Internal`WithLocalSettings[Null,
    root[name_]:=FileNameJoin[{temp,name}];
    Export[root["input"],StringRiffle[input,"\n"]<>"\n","Text",CharacterEncoding->"UTF-8"];
    res=RunProcess[{ExpandFileName[executable],root["input"],root["output"],ToString[Replace[work,Automatic->0]]}];
    If[!AssociationQ[res] || res["ExitCode"]=!=0,fail["NativeGeneration","Native model generation failed.",
      If[AssociationQ[res],KeyTake[res,{"ExitCode","StandardError"}],<||>]]];
    lines=Import[root["output"],"Lines",CharacterEncoding->"UTF-8"];
    If[!ListQ[lines] || Length[lines]<8 || First[lines]=!="HEPCAT_GENERATED_1",fail["NativeProtocol","Incomplete generation output."]];
    counters[x_] := If[StringMatchQ[x,DigitCharacter..],FromDigits[x],fail["NativeProtocol","Expected a row or worker count."]];
    numbers[x_] := If[StringMatchQ[x,RegularExpression["[0-9]+(?:\\.[0-9]+)?(?:[Ee][+-]?[0-9]+)?"]],
      ToExpression[StringReplace[x,{"e"|"E"->"*^"}]],fail["NativeProtocol","Invalid duration."]];
    masses=decode[lines[[6]]];
    rules=Thread[(Mass/@Range[Length[masses]])->masses];
    rows[] := Module[{n=counters[lines[[next++]]],data},
      If[next+3 n-1>Length[lines],fail["NativeProtocol","Missing generated rows."]];
      data=Table[Table[decode[lines[[next++]]],3],n];data];
    diagrams=rows[];products=rows[];
    If[next=!=Length[lines]+1,fail["NativeProtocol","Unexpected trailing generation output."]];
    make[entry_]:={Join[rules,(Mass[Multiparticle@@First[#]]->Last[#]& /@ First[entry])],entry[[2]]};
    <|"Diagrams"->(make /@ diagrams),"DiagramRemainders"->(Last/@diagrams),
      "FactorizationProducts"->(Function[entry,With[{shell=First[First[entry]]},
        Join[masses,{Multiparticle@@First[shell],Last[shell],entry[[2]]}]]]/@products),
      "FactorizationRemainders"->(Last/@products),"WorkersUsed"->counters[lines[[2]]],
      "ModelReadSeconds"->numbers[lines[[3]]],"GenerationAndReductionSeconds"->numbers[lines[[4]]],
      "ProcessPeakRSSBytes"->If[lines[[5]]==="-1",Missing["Unavailable"],counters[lines[[5]]]]|>,
    DeleteDirectory[temp,DeleteContents->True]]], $batchFailure];

End[];
EndPackage[];
