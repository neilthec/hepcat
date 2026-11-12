(* HEPCAT - High Energy Physics Constructive Amplitude Tool
   Copyright (C) 2026 Neil Christensen
   SPDX-License-Identifier: GPL-3.0-or-later *)

(* Campaign-only candidate transport. This does not construct or validate an amplitude.
   Load HEPCAT first. Bijective generation maps and external leg maps are explicit. *)
Begin["SMFivePointGenerationTransport`Private`"];
generationMapCertificate[model_List, mapping_Association, modelSource_: Automatic] := Module[
  {names, masses, parameters, rows, targets, vertexOK, particleOK, sourceFiles, sourceHashes = <||>},
  If[modelSource =!= Automatic,
    If[!MatchQ[modelSource, {_String, _Integer}] ||
        ReadModel[First[modelSource], Last[modelSource]] =!= model,
      Return[Failure["GenerationCertificateSourceMismatch", <||>]]];
    sourceFiles = FileNameJoin[{First[modelSource], # <> ToString[Last[modelSource]] <> ".mdl"}] & /@
      {"amps", "prtcls", "vars", "func", "extlib"};
    If[!AllTrue[sourceFiles, FileExistsQ], Return[Failure["GenerationCertificateSourceMissing", <||>]]];
    sourceHashes = Association[(FileNameTake[#] -> IntegerString[FileHash[#, "SHA256"], 16, 64]) & /@ sourceFiles]];
  names = Lookup[mapping, "ParticleMap", <||>]; masses = Lookup[mapping, "MassMap", <||>];
  If[!AssociationQ[names] || !AssociationQ[masses] ||
      !DuplicateFreeQ[Values[names]] || !DuplicateFreeQ[Values[masses]],
    Return[Failure["InvalidGenerationMap", <||>]]];
  parameters = KeyValueMap[ModelParameter[model, #1] -> ModelParameter[model, #2] &, masses];
  If[!FreeQ[parameters, _Failure], Return[Failure["UnknownGenerationMass", <||>]]];
  rows = model[[2]];
  vertexOK = AllTrue[rows, Function[row,
    targets = Select[rows, Drop[#, -2] === (Drop[row, -2] /. Normal[names]) &];
    Length[targets] === 1 && SameQ[(row[[-2]] row[[-1]]) /. parameters,
      First[targets][[-2]] First[targets][[-1]]]]];
  particleOK = AllTrue[model[[1]], Function[row,
    targets = Select[model[[1]], #[[2]] === (row[[2]] /. Normal[names]) &];
    Length[targets] === 1 && SameQ[row[[{2, 3}]] /. Normal[names], First[targets][[{2, 3}]]] &&
      SameQ[row[[{5, 6, 8, 9}]] /. parameters, First[targets][[{5, 6, 8, 9}]]]]];
  <|"Status" -> If[vertexOK && particleOK, "EXACT_ROW_MATCH", "FAIL"],
    "ElementaryRows" -> Length[rows], "ParticleRows" -> Length[model[[1]]],
    "Scope" -> "Bare tree amplitudes with widths disabled",
    "VertexExpressionsSameQ" -> vertexOK, "ParticleRolesSameQ" -> particleOK,
    "ParameterRules" -> parameters, "NumericalPhaseFitted" -> False,
    "FullGenerationMapSHA256" -> IntegerString[Hash[mapping, "SHA256"], 16, 64],
    "ElementaryModelExpressionSHA256" -> IntegerString[Hash[model, "SHA256"], 16, 64],
    "ElementarySourceFilesSHA256" -> sourceHashes,
    "ElementarySourceSHA256" -> If[sourceHashes === <||>, Missing["SourceNotSpecified"],
      IntegerString[Hash[sourceHashes, "SHA256"], 16, 64]]|>
];
generationTransportCandidate[value_, source_List, target_List, model_List,
    mapping_Association, image_List, elementary_List, modelSource : {_String, _Integer}] := Module[
  {n = Length[source], names, mapped, parameters, declared, roles = <||>, fermions, certificate,
   particleLegs, antiLegs, phase, cycles, transported, pairQ},
  certificate = generationMapCertificate[elementary, mapping, modelSource];
  If[!AssociationQ[certificate] || Lookup[certificate, "Status", None] =!= "EXACT_ROW_MATCH" ||
      model[[{1, 3, 4}]] =!= elementary[[{1, 3, 4}]],
    Return[Failure["UncertifiedGenerationTransport", <|"Certificate" -> certificate|>]]];
  If[Length[target] =!= n || Sort[image] =!= Range[n],
    Return[Failure["InvalidExternalLegBijection", <||>]]];
  names = Lookup[mapping, "ParticleMap", <||>];
  mapped = source /. Normal[names];
  If[target[[image]] =!= mapped, Return[Failure["GenerationLegMapMismatch", <||>]]];
  parameters = KeyValueMap[ModelParameter[model, #1] -> ModelParameter[model, #2] &,
    Lookup[mapping, "MassMap", <||>]];
  declared = First /@ Join[model[[3]], model[[4]]];
  If[!FreeQ[parameters, _Failure] ||
      Complement[DeleteDuplicates[Cases[value, _ModelParameter, Infinity]], declared] =!= {},
    Return[Failure["ForeignOrUnknownModelParameters", <||>]]];
  fermions = Select[model[[1]], IntegerQ[#[[5]]] && OddQ[#[[5]]] &];
  If[Intersection[fermions[[All, 2]], fermions[[All, 3]]] =!= {},
    Return[Failure["AmbiguousSelfConjugateFermionReference", <||>]]];
  Scan[(AssociateTo[roles, #[[2]] -> 0]; AssociateTo[roles, #[[3]] -> 1]) &, fermions];
  If[!AllTrue[Range[n], Lookup[roles, source[[#]], None] === Lookup[roles, mapped[[#]], None] &],
    Return[Failure["GenerationMapChangesFermionRole", <||>]]];
  particleLegs = Select[Range[n], Lookup[roles, source[[#]], None] === 0 &];
  antiLegs = Select[Range[n], Lookup[roles, source[[#]], None] === 1 &];
  (* The package's external state reference orders particles and antiparticles
     separately. A cross-role shuffle alone adds no reference phase. *)
  phase = Signature[Ordering[image[[particleLegs]]]] Signature[Ordering[image[[antiLegs]]]];
  cycles = ConstructiveDiagrams`Private`completionBoseCycles[Range[n], image];
  transported = Fold[ConstructiveDiagrams`Private`completionBoseCycle, value /. parameters, cycles];
  pairQ = MatchQ[transported, {_List, _}] && AllTrue[First[transported], MatchQ[#, _Rule] &];
  transported = If[pairQ, {First[transported], phase Last[transported]}, phase transported];
  <|"Status" -> "CANDIDATE", "Particles" -> target, "Amplitude" -> transported,
    "SourceParticles" -> source, "MappedSourceParticles" -> mapped,
    "GenerationCertificate" -> certificate,
    "TargetModelExpressionSHA256" -> IntegerString[Hash[model, "SHA256"], 16, 64],
    "LegPermutation" -> image, "PermutationCycles" -> cycles,
    "FermionReferenceSign" -> phase, "MassParameterRules" -> parameters,
    "NativeValidationPerformed" -> False, "ExternalValidationPerformed" -> False,
    "NumericalPhaseFitted" -> False, "InternalOnShellAssumptionsAdded" -> False|>
];
generationTransportCandidate[___] := Failure["GenerationCertificateRequired", <|
  "MessageTemplate" -> "Supply the exact elementary model and its {directory, modelNumber} source to certify the map before transporting a candidate."|>];
End[];
