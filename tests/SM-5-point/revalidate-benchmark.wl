(* HEPCAT - High Energy Physics Constructive Amplitude Tool
   Copyright (C) 2026 Neil Christensen
   SPDX-License-Identifier: GPL-3.0-or-later *)

(* Internal to saved-expression revalidation. This prepares only an independent
   benchmark; it never calls construction or supplies a correction direction. *)
Begin["SMFivePointRevalidation`Private`"];
prepareIndependentBenchmark[raw_, model_, seconds_] := Module[
  {started = AbsoluteTime[], remaining, prepared, converted, amplitudes,
   recovery = <|"Attempted" -> False|>, record, ready = False},
  If[!NumberQ[seconds] || !TrueQ[0 < seconds < Infinity],
    Return[<|"Status" -> "FAIL", "Diagnostics" -> {"The benchmark allowance must be finite and positive."}|>]];
  remaining[] := Max[0, seconds - (AbsoluteTime[] - started)];
  record[status_, value_, diagnostics_] := <|"Status" -> status,
    "BenchmarkAmplitudes" -> value, "OriginalAmplitudes" -> raw,
    "PreparationDetails" -> prepared, "Recovery" -> recovery,
    "TimeConstraint" -> seconds, "ElapsedSeconds" -> N[AbsoluteTime[] - started],
    "CompleteDiagramCount" -> If[status === "COMPLETE", Length[value], 0],
    "InternalOnShell" -> False, "BenchmarkOnly" -> True, "Diagnostics" -> diagnostics|>;
  If[!ListQ[raw] || raw === {} ||
      !AllTrue[raw, ConstructiveDiagrams`Private`preparationPairQ],
    prepared = Missing["InvalidRawBenchmark"];
    Return[record["FAIL", Missing["UnusableBenchmark"], {"The complete original Feynman pair list is required."}]]];
  Print["Preparing the complete independent Feynman benchmark."];
  prepared = TimeConstrained[PrepareAmplitudes[raw, model,
    "PolePreference" -> "IndependentXi", "MaxSteps" -> 0,
    "InternalOnShell" -> False, "ReturnDetails" -> True],
    Max[0.000001, remaining[]], Failure["RevalidationBenchmarkTimeConstraint", <|
      "MessageTemplate" -> "The shared independent benchmark allowance expired during preparation."|>]];
  If[TrueQ[ConstructiveDiagrams`Private`constructPreparationUsableQ[prepared]],
    amplitudes = prepared["PreparedAmplitudes"];
    If[TrueQ[ConstructiveDiagrams`Private`constructBenchmarkConversionCompleteQ[raw, amplitudes]],
      Return[record["COMPLETE", amplitudes, {}]],
      Return[record["FAIL", Missing["IncompletePreparedBenchmark"],
        {"The prepared benchmark did not preserve every complete original pair."}]]]];
  (* IndependentXi and MaxSteps are this worker's policy, not caller pins.
     The guard still requires the exact complete off-shell source and a typed
     PARTIAL deadline/conversion checkpoint before admitting the retry. *)
  ready = TrueQ[ConstructiveDiagrams`Private`constructBenchmarkConversionRecoveryQ[
    prepared, raw, {}, remaining[]]];
  If[!ready, Return[record["FAIL", Missing["UnusablePreparedBenchmark"],
    {"The independent benchmark preparation was incomplete and no bounded conversion recovery was eligible."}]]];
  recovery = <|"Attempted" -> True, "Status" -> "ATTEMPTED",
    "Reason" -> "DefaultPreparationIncomplete", "InputDiagrams" -> Length[raw],
    "ReferenceSpinors" -> Automatic, "ReduceSpinorProducts" -> False,
    "TimeBudgetScope" -> "RemainingSharedBenchmarkAllowance"|>;
  Print["Retrying all ", Length[raw], " Feynman diagrams by complete off-shell conversion; ",
    N[remaining[]], " s remain in the benchmark allowance."];
  converted = TimeConstrained[FeynmanToSpinorProducts[raw,
    "ReferenceSpinors" -> Automatic, "ReduceSpinorProducts" -> False,
    "TimeConstraint" -> Max[0.000001, remaining[]]], Max[0.000001, remaining[]],
    Failure["RevalidationBenchmarkTimeConstraint", <|
      "MessageTemplate" -> "The shared independent benchmark allowance expired during complete conversion."|>]];
  If[!TrueQ[ConstructiveDiagrams`Private`constructBenchmarkConversionCompleteQ[raw, converted]],
    recovery = Join[recovery, <|"Status" -> "FAIL", "ConversionResult" -> converted|>];
    Return[record["FAIL", Missing["IncompleteConvertedBenchmark"],
      {"Complete original diagram coverage and mass rules were not established by the bounded conversion."}]]];
  recovery = Join[recovery, <|"Status" -> "COMPLETE",
    "ConvertedDiagrams" -> Length[converted], "CompleteOrderedCoverage" -> True|>];
  record["COMPLETE", converted, {}]
];

(* A zero pair is justified only by two successful empty lists, never by a
   failure, a dropped amplitude or an incomplete graph-to-expression map. *)
independentBenchmarkInput[diagrams_, amplitudes_, process_List, model_] := Module[{masses},
  If[!ListQ[diagrams] || !ListQ[amplitudes] || Length[diagrams] =!= Length[amplitudes],
    Return[Failure["RevalidationFeynmanCoverage", <|"MessageTemplate" ->
      "The successful complete graph and amplitude lists must have matching coverage."|>]]];
  If[diagrams === {} && amplitudes === {},
    masses = ConstructiveDiagrams`Private`constructExternalMasses[process, model];
    If[!ListQ[masses] || Length[masses] =!= Length[process] ||
        !ConstructiveDiagrams`Private`residueFiniteQ[masses],
      Return[Failure["RevalidationExternalMasses", <|"MessageTemplate" ->
        "External mass metadata is required for a graph-empty zero benchmark."|>]]];
    Return[{{Thread[(Mass /@ Range[Length[process]]) -> masses], 0}}]];
  amplitudes
];

(* Physical rows are required when channels exist. With no physical cuts,
   retain every applicable native check without fabricating a cut row. *)
nativeRevalidationRowsPassQ[rows_, channels_] := ListQ[rows] && rows =!= {} &&
  ListQ[channels] && AllTrue[rows, AssociationQ[#] && Lookup[#, "Status", None] === "PASS" &] &&
  ContainsAll[Lookup[rows, "Type", {}], {"HighEnergy", "Reference", "Feynman"}] &&
  (channels === {} || MemberQ[Lookup[rows, "Type", {}], "Physical"]);

(* Parse only decimal numbers. Environment text is never executable code. *)
nativeRevalidationCheckAllowance[text_] := Module[{value},
  If[MemberQ[{$Failed, "", "Automatic"}, text], Return[Automatic]];
  If[!StringQ[text] || !StringMatchQ[text,
      RegularExpression["(?:[0-9]+(?:\\.[0-9]*)?|\\.[0-9]+)"]],
    Return[Failure["RevalidationCheckTimeConstraint", <|"MessageTemplate" ->
      "HEPCAT_SM5_REVALIDATION_CHECK_SECONDS must be Automatic or a finite positive decimal number."|>]]];
  value = ToExpression[text, InputForm];
  If[NumberQ[value] && TrueQ[0 < value < Infinity], value,
    Failure["RevalidationCheckTimeConstraint", <|"MessageTemplate" ->
      "HEPCAT_SM5_REVALIDATION_CHECK_SECONDS must be finite and positive."|>]]
];
End[];
