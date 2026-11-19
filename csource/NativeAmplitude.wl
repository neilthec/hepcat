(* Optional interface to the native amplitude reducer. Load HEPCAT once. *)
If[!MemberQ[$Packages, "ConstructiveDiagrams`"],
  Get[FileNameJoin[{DirectoryName[DirectoryName[$InputFileName]], "source", "HEPCAT.wl"}]]];
BeginPackage["HEPCATNative`", {"ConstructiveDiagrams`"}];

ReduceNativeAmplitude::usage =
  "ReduceNativeAmplitude[process, diagramModel, factorizationModel, executable] generates all diagrams and factorization products, then reduces both together in C++. ReduceNativeAmplitude[diagramPairs, channelRows, executable] accepts existing HEPCAT rows. For example, with loaded models qed and qedFactorization, run ReduceNativeAmplitude[{\"e\", \"E\", \"A.+\", \"A.+\"}, qed, qedFactorization, binary]. The result contains Diagrams, FactorizationProducts, their aligned DiagramRemainders and FactorizationRemainders, WorkersUsed and ReductionSeconds. The remainders restore each spin-contracted input away from its numerator cuts in the chosen reference. Workers -> Automatic uses the available native workers; 1 requests serial reduction. PolePreference -> None keeps supplied references. Cyclic uses a common massless reference cycle. ExternalMasses can provide the ordered masses for empty collections.";
Options[ReduceNativeAmplitude] = {"Workers" -> Automatic, "PolePreference" -> None, "ExternalMasses" -> Automatic};

CompleteNativeFactorization::usage =
  "CompleteNativeFactorization[batch, executable] compares prepared diagrams with the sewn physical channels and fits missing pole terms in C++. Supply the batch returned by ReduceNativeAmplitude to reuse its retained remainders. It also accepts [diagramPairs, channelRows, executable] or [process, diagramModel, factorizationModel, executable] for raw input. The result is {externalMassRules, expression}. ReturnDetails -> True includes the native time and validation sample counts. Two detailed PrepareAmplitudes reports can also be passed together as [diagramReport, channelReport, executable]. Raw input uses Automatic reference assignment unless overridden. CorrectionBasis can supply additional full-amplitude directions.";
Options[CompleteNativeFactorization] = Join[
  DeleteCases[Options[ReduceNativeAmplitude], ("PolePreference" -> _)], {"PolePreference" -> Automatic},
  {"ReturnDetails" -> False, "CorrectionBasis" -> Automatic}];

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
   prepared, expression, diagramRest, channelRest, basis, result},
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
  header = Which[prepared, "HEPCAT_FACTOR_PREPARED_1", AssociationQ[completion], "HEPCAT_FACTOR_1", True, "HEPCAT_BATCH_1"];
  fields = Join[{header, wire[masses], ToString[nd]}, Flatten[diagramLines],
    {ToString[Length[channels]]}, Flatten[channelLines]];
  If[AssociationQ[completion],
    basis = Replace[completion["Basis"], Automatic -> {}];
    If[!ListQ[basis], fail["NativeBasis", "CorrectionBasis must be Automatic or a list of amplitude expressions."]];
    fields = Join[fields, {ToString[Length[basis]]}, wire /@ basis]];

  temp = CreateDirectory[];
  If[!StringQ[temp], fail["NativeTemporaryDirectory", "Could not create the native reduction's temporary directory."]];
  Internal`WithLocalSettings[Null,
    output = FileNameJoin[{temp, "result.txt"}];
    command = {ExpandFileName[executable], FileNameJoin[{temp, "input.txt"}], output, ToString[count]};
    If[Export[command[[2]], StringRiffle[fields, "\n"] <> "\n", "Text", CharacterEncoding -> "UTF-8"] === $Failed,
      fail["NativeTransfer", "Could not write the batch to the native reducer."]];
    run = RunProcess[command];
    If[!AssociationQ[run] || run["ExitCode"] =!= 0 || !FileExistsQ[output],
      fail[If[completion === None, "NativeReduction", "IncompleteFactorization"],
        If[completion === None, "The native batch reduction failed.", "Native factorization did not produce a validated amplitude."],
        If[AssociationQ[run], KeyTake[run, {"ExitCode","StandardError"}], <||>]]];
    response = Import[output, "Lines", CharacterEncoding -> "UTF-8"];
    If[!ListQ[response] || Length[response] < 3 || !StringMatchQ[response[[2]], DigitCharacter ..] ||
        !StringMatchQ[response[[3]], RegularExpression["[0-9]+(?:\\.[0-9]+)?(?:[eE][+-]?[0-9]+)?"]],
      fail["NativeProtocol", "Invalid header or timing in the native response."]];
    threads = FromDigits[response[[2]]];
    seconds = ToExpression[StringReplace[response[[3]], {"e"|"E" -> "*^"}], InputForm];
    If[AssociationQ[completion],
      If[Length[response] =!= 10 || First[response] =!= "HEPCAT_FACTOR_RESULT_1" ||
          !AllTrue[response[[4 ;; 8]], StringMatchQ[#, DigitCharacter ..] &],
        fail["NativeProtocol", "Invalid completion result or sample counts."]];
      result = <|"Amplitude" -> {Thread[(Mass /@ Range[n]) -> masses], decode[Last[response]]},
        "CompletionSeconds" -> seconds, "WorkersUsed" -> threads, "Route" -> response[[9]],
        "Channels" -> FromDigits[response[[4]]], "Candidates" -> FromDigits[response[[5]]],
        "Rank" -> FromDigits[response[[6]]], "TrainingSamples" -> FromDigits[response[[7]]],
        "ValidationSamples" -> FromDigits[response[[8]]]|>;
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

End[];
EndPackage[];
