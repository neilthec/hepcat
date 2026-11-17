(* HEPCAT - High Energy Physics Constructive Amplitude Tool
   Copyright (C) 2026 Neil Christensen
   SPDX-License-Identifier: GPL-3.0-or-later *)

(* Independently verify a candidate obtained with the exact generation map.
   No saved source check or benchmark is reused as a target check. *)
Begin["SMFivePointGenerationTransport`Private`"];

validateTargetCandidate[candidate_Association, elementary_List, lower_List,
    feynman_, seed_Integer, seconds_?NumberQ] := Module[
  {process, masses, external, expression, diagrams, raw, benchmark, targets,
   representations, projection, coefficients, settings, checked, rows, start = AbsoluteTime[],
   deadline},
  deadline = start + seconds;
  If[candidate["Status"] =!= "CANDIDATE" || seconds <= 0,
    Return[Failure["MissingGenerationCandidate", <||>]]];
  process = candidate["Particles"];
  expression = candidate["Amplitude"];
  masses = ConstructiveDiagrams`Private`constructExternalMasses[process, elementary];
  If[!ListQ[masses] || Length[masses] =!= Length[process] || !FreeQ[masses, _Missing | _Failure],
    Return[Failure["InvalidTransportMasses", <||>]]];
  external = Thread[(Mass /@ Range[Length[process]]) -> masses];
  If[ConstructiveDiagrams`Private`residueAmplitudePairQ[expression],
    If[Sort[First[expression]] =!= Sort[external],
      Return[Failure["TransportMassMismatch", <||>]]];
    expression = Last[expression]];

  diagrams = CreateDiagrams[process, feynman];
  If[!ListQ[diagrams] || diagrams === {},
    Return[Failure["TransportBenchmarkHasNoDiagrams", <||>]]];
  raw = DiagramAmplitudes[diagrams];
  If[!ListQ[raw] || Length[raw] =!= Length[diagrams],
    Return[Failure["IncompleteTransportBenchmarkDiagrams", <||>]]];
  benchmark = FeynmanToSpinorProducts[raw, "ReferenceSpinors" -> Automatic,
    "ReduceSpinorProducts" -> False, "TimeConstraint" -> Max[0.001, deadline - AbsoluteTime[]]];
  If[!ConstructiveDiagrams`Private`constructBenchmarkConversionCompleteQ[raw, benchmark],
    Return[Failure["IncompleteTransportBenchmarkConversion", <||>]]];

  targets = FactorizationAmplitudeChannels[process, lower];
  If[!ListQ[targets], Return[Failure["IncompleteTransportTargets", <||>]]];
  representations = ConstructiveDiagrams`Private`colorParticleRepresentation[#, elementary]& /@ process;
  projection = ConstructiveDiagrams`Private`colorProjectionData[{{external, expression}},
    targets, representations, benchmark];
  If[!AssociationQ[projection] || projection["Basis"] === {},
    Return[Failure["IncompleteTransportColorProjection", <|"Projection" -> projection|>]]];
  coefficients = Last /@ (First /@ projection["ProjectedDiagrams"]);
  settings = Join[Association[Options[ConstructAmplitude]], <|"FeynmanModel" -> feynman,
    "Samples" -> 4, "RandomSeed" -> seed, "Parallelization" -> False|>];
  checked = ConstructiveDiagrams`Private`colorOrbitValidate[process, coefficients, external,
    projection, lower, settings, deadline];
  If[!AssociationQ[checked] || checked["Status"] =!= "COMPLETE",
    Return[Failure["TransportTargetChecksIncomplete", <|"Validation" -> checked|>]]];
  rows = Map[Function[group,
    KeyTake[#, {"Type", "Pole", "Status"}] & /@ group["Report"]["Checks"]],
    checked["ValidationReports"]];
  rows = Flatten[MapThread[Function[{group, groupRows},
    (Join[<|"ParameterSet" -> group["ParameterSetIndex"],
        "Color" -> group["ColorCoefficient"]|>, #] & /@ groupRows)],
    {checked["ValidationReports"], rows}], 1];
  <|"Status" -> "COMPLETE", "Amplitude" -> expression, "AmplitudePair" -> {external, expression},
    "Checks" -> Dataset[rows, MaxItems -> {All, All}], "Validation" -> checked,
    "Complexity" -> SpinorExpressionComplexity[expression], "Candidate" -> candidate,
    "IndependentBenchmark" -> benchmark, "TargetChannels" -> targets,
    "FeynmanBenchmarkDiagramCount" -> Length[raw],
    "FeynmanBenchmarkDiagramsSHA256" -> IntegerString[Hash[raw, "SHA256"], 16, 64],
    "Method" -> "ExactModelGenerationTransport", "ElapsedSeconds" -> N[AbsoluteTime[] - start]|>
];
End[];
