(* Export saved QED formulas for timing, without changing their representation.
   Run with WolframKernel -script tests/benchmarks/export-physical.wl.
   Optional arguments: --output DIR, --source DIR, --spinas DIR,
     --calchep DIR, --madgraph DIR, --configuration MANIFEST,
     --code-style Optimized|Readable, --representation AllPhysical|AllSpurious,
     --helicity-symmetry Conjugation|Automatic|None.
   The configuration manifest is an existing WriteSPINASProcess export. Its
   explicitly selected reference installations/models are retained by default.
   No amplitude is constructed, simplified, fitted or changed by this script. *)

(*
HEPCAT - High Energy Physics Constructive Amplitude Tool

Copyright (C) 2026 Neil Christensen

This file is part of HEPCAT.

HEPCAT is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

HEPCAT is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with HEPCAT.  If not, see <https://www.gnu.org/licenses/>.
*)


$CharacterEncoding = "UTF-8";
benchmarkRepository = DirectoryName[DirectoryName[DirectoryName[ExpandFileName[$InputFileName]]]];
benchmarkArguments = If[$ScriptCommandLine =!= {}, Rest[$ScriptCommandLine],
  With[{benchmarkScriptPosition = FirstPosition[$CommandLine, "-script", Missing["NotScript"]]},
    If[ListQ[benchmarkScriptPosition], Drop[$CommandLine, First[benchmarkScriptPosition] + 1], {}]]];
benchmarkOptions = <||>;
If[OddQ[Length[benchmarkArguments]], Print["Expected option/value argument pairs."]; Quit[1]];
Do[
  If[!MemberQ[{"--output", "--source", "--spinas", "--calchep", "--madgraph", "--configuration", "--code-style", "--representation", "--helicity-symmetry"}, benchmarkArguments[[benchmarkArgumentIndex]]],
    Print["Unknown option: ", benchmarkArguments[[benchmarkArgumentIndex]]]; Quit[1]];
  AssociateTo[benchmarkOptions, benchmarkArguments[[benchmarkArgumentIndex]] -> benchmarkArguments[[benchmarkArgumentIndex + 1]]],
  {benchmarkArgumentIndex, 1, Length[benchmarkArguments], 2}];
benchmarkRepresentation = Lookup[benchmarkOptions, "--representation", "AllPhysical"];
If[!MemberQ[{"AllPhysical", "AllSpurious"}, benchmarkRepresentation],
  Print["--representation must be AllPhysical or AllSpurious."]; Quit[1]];
benchmarkModelNumber = If[benchmarkRepresentation === "AllPhysical", 3, 4];
benchmarkOutput = ExpandFileName[Lookup[benchmarkOptions, "--output",
  FileNameJoin[{benchmarkRepository, "tests", "benchmarks",
    If[benchmarkRepresentation === "AllPhysical", "build-physical-double", "build-spurious-double"], "exports"}]]];
benchmarkSource = ExpandFileName[Lookup[benchmarkOptions, "--source", FileNameJoin[{benchmarkRepository, "source"}]]];
(* These saved models are tree-level vectorlike QED. This is an explicit
   theory declaration, not a symmetry fitted from the timing samples. *)
benchmarkHelicitySymmetryName = Lookup[benchmarkOptions, "--helicity-symmetry", "Conjugation"];
If[!MemberQ[{"Conjugation", "Automatic", "None"}, benchmarkHelicitySymmetryName],
  Print["--helicity-symmetry must be Conjugation, Automatic or None."]; Quit[1]];
benchmarkHelicitySymmetry = Switch[benchmarkHelicitySymmetryName,
  "Automatic", Automatic, "None", None, _, "Conjugation"];
benchmarkCodeStyle = Lookup[benchmarkOptions, "--code-style", "Optimized"];
If[!MemberQ[{"Optimized", "Readable"}, benchmarkCodeStyle],
  Print["--code-style must be Optimized or Readable."]; Quit[1]];
benchmarkSPINAS = ExpandFileName[Lookup[benchmarkOptions, "--spinas", FileNameJoin[{DirectoryName[benchmarkRepository], "spinas"}]]];
benchmarkConfigurationFile = ExpandFileName[Lookup[benchmarkOptions, "--configuration",
  FileNameJoin[{benchmarkRepository, "tests", "results", "spinas-output", "eeAA", "manifest.json"}]]];
If[!FileExistsQ[benchmarkConfigurationFile],
  Print["Supply --configuration with an existing complete QED export manifest containing both reference model selections."]; Quit[1]];
benchmarkConfiguration = Import[benchmarkConfigurationFile, "RawJSON"];
If[!AssociationQ[benchmarkConfiguration], Print["Cannot read the reference configuration manifest."]; Quit[1]];
benchmarkCH = Lookup[benchmarkConfiguration, "CalcHEPValidation", <||>];
benchmarkMG = Lookup[benchmarkConfiguration, "MadGraphValidation", <||>];
If[Lookup[benchmarkCH, "Status", ""] =!= "AVAILABLE" || Lookup[benchmarkMG, "Status", ""] =!= "AVAILABLE" ||
    Lookup[benchmarkCH, "AllowedParticlePDGs", {}] =!= {11,22} ||
    Lookup[benchmarkMG, "AllowedParticlePDGs", {}] =!= {11,22},
  Print["The manifest must explicitly configure both independent references and the electron/photon restriction {11,22}."]; Quit[1]];
Get[FileNameJoin[{benchmarkSource, "HEPCAT.wl"}]];
benchmarkModelDirectory = FileNameJoin[{benchmarkRepository, "models"}];
benchmarkModel = ReadModel[benchmarkModelDirectory, benchmarkModelNumber];
If[!ListQ[benchmarkModel], Print["Could not load QED factorization model ", benchmarkModelNumber, "."]; Quit[1]];
benchmarkMass = ModelParameter[benchmarkModel, "Me"];
benchmarkLookup[particles_] := LookupAmplitude[
  MapIndexed[{#1, First[#2]} &, particles], benchmarkModel, False,
  {benchmarkSpin1, benchmarkSpin2, benchmarkSpin3, benchmarkSpin4}];
benchmarkPhysicalPoleQ[PropDen[Mom[Multiparticle[labels__Integer]], mass_], legs_Integer] := Module[{cut = {labels}},
  DuplicateFreeQ[cut] && SubsetQ[Range[legs], cut] && 2 <= Length[cut] <= legs - 2 &&
    Count[cut, 1 | 2] === 1 && mass === benchmarkMass];
benchmarkPhysicalPoleQ[_, _] := False;
benchmarkInverseCategory[base_, legs_Integer] := Which[
  benchmarkPhysicalPoleQ[base, legs], "PhysicalElectronPropagator",
  MatchQ[base, _ModelParameter], "ModelParameter",
  NumberQ[base], "NumericalConstant",
  MatchQ[base, _SpinorChain], "SpinorProduct",
  !FreeQ[base, _SpinorChain], "CompositeSpinorExpression",
  !FreeQ[base, _PropDen | _MomProd], "ScalarInvariantExpression",
  True, "OtherExpression"];
benchmarkFileHashes = Association@Table[
  filename -> FileHash[FileNameJoin[{benchmarkModelDirectory, filename}], "SHA256", "HexString"],
  {filename, (# <> ToString[benchmarkModelNumber] <> ".mdl" & /@ {"amps", "prtcls", "vars", "func"})}];
benchmarkRecords = {};
Do[
  benchmarkEntries = Table[
    benchmarkParticles = Join[{"e", "E"}, ConstantArray["A.+", photons - negative], ConstantArray["A.-", negative]];
    benchmarkExpression = benchmarkLookup[benchmarkParticles];
    If[benchmarkExpression === 0 || !FreeQ[benchmarkExpression, _Failure | $Failed | Spinor["Xi", ___] | _xFactor | _xTildeFactor],
      Print["Missing or unsupported saved amplitude: ", benchmarkParticles]; Quit[1]];
    benchmarkInversePowers = DeleteDuplicates[Cases[benchmarkExpression, inverse : Power[_, _?Negative] :> inverse, Infinity]];
    benchmarkInverseBases = DeleteDuplicates[First /@ benchmarkInversePowers];
    benchmarkBadBases = Select[benchmarkInverseBases,
      !MatchQ[#, _ModelParameter] && !benchmarkPhysicalPoleQ[#, photons + 2] &];
    If[benchmarkRepresentation === "AllPhysical" && benchmarkBadBases =!= {},
      Print["Nonphysical denominator in ", benchmarkParticles, ": ", InputForm[benchmarkBadBases]]; Quit[1]];
    AppendTo[benchmarkRecords, <|
      "Particles" -> benchmarkParticles,
      "Complexity" -> SpinorExpressionComplexity[benchmarkExpression],
      "ExpressionLeaves" -> LeafCount[benchmarkExpression],
      "ExpressionSHA256" -> Hash[benchmarkExpression, "SHA256", "HexString"],
      "PhysicalPoleCuts" -> (Cases[#, Multiparticle[labels__] :> {labels}, Infinity][[1]] & /@
        Select[benchmarkInverseBases, benchmarkPhysicalPoleQ[#, photons + 2] &]),
      "NonkinematicInverseFactors" -> (ModelParameterName /@ Select[benchmarkInverseBases, Head[#] === ModelParameter &]),
      "InverseFactors" -> (<|"Base" -> ToString[#[[1]], InputForm],
        "Exponent" -> ToString[#[[2]], InputForm],
        "Category" -> benchmarkInverseCategory[#[[1]], photons + 2]|> & /@ benchmarkInversePowers)|>];
    {benchmarkParticles, benchmarkExpression},
    {negative, 0, photons}];
  benchmarkName = "ee" <> StringRepeat["A", photons];
  benchmarkDirectory = FileNameJoin[{benchmarkOutput, benchmarkName}];
  benchmarkExport = WriteSPINASProcess[benchmarkDirectory,
    {"e", "E"} -> ConstantArray["A", photons], benchmarkEntries, benchmarkModel,
    "SPINASDirectory" -> benchmarkSPINAS, "Precision" -> "Double", "ProcessName" -> benchmarkName,
    "CodeStyle" -> benchmarkCodeStyle, "HelicitySymmetry" -> benchmarkHelicitySymmetry,
    "CalcHEPDirectory" -> ExpandFileName[Lookup[benchmarkOptions, "--calchep", benchmarkCH["CalcHEPDirectory"]]],
    "CalcHEPModel" -> {benchmarkCH["ModelDirectory"], benchmarkCH["ModelNumber"]},
    "CalcHEPAllowedParticles" -> {11,22},
    "MadGraphDirectory" -> ExpandFileName[Lookup[benchmarkOptions, "--madgraph", benchmarkMG["MadGraphDirectory"]]],
    "MadGraphModel" -> benchmarkMG["MadGraphModel"],
    "MadGraphAllowedParticles" -> {11,22},
    "MadGraphParameterRules" -> Normal[Lookup[benchmarkMG, "ParameterRules", <||>]],
    "MadGraphParameterMap" -> Normal[Lookup[benchmarkMG, "ParameterMap", <||>]], "Overwrite" -> True];
  If[FailureQ[benchmarkExport] || benchmarkExport === $Failed, Print[InputForm[benchmarkExport]]; Quit[1]];
  benchmarkProvenance = <|
    "Representation" -> benchmarkRepresentation, "Precision" -> "Double", "Photons" -> photons,
    "CodeStyle" -> benchmarkCodeStyle, "HelicitySymmetry" -> benchmarkHelicitySymmetryName,
    "HelicitySymmetryScope" -> "Declared tree-level vectorlike QED with real parameters; complete massive-spin sums",
    "AmplitudeSource" -> "ReadModel[models," <> ToString[benchmarkModelNumber] <>
      "] and LookupAmplitude; unchanged saved complete amplitudes",
    "ModelNumber" -> benchmarkModelNumber,
    "ModelFilesSHA256" -> benchmarkFileHashes,
    "InputHelicitySectors" -> Select[benchmarkRecords, Length[#["Particles"]] === photons + 2 &],
    "CompletePhotonHelicityCount" -> 2^photons,
    "ShellScope" -> "External masses only; no internal on-shell substitutions during export",
    "SourceDirectory" -> benchmarkSource,
    "ExporterSHA256" -> FileHash[FileNameJoin[{benchmarkSource, "internal", "HEPCAT-SPINAS.wl"}], "SHA256", "HexString"],
    "OptimizationBackendSHA256" -> FileHash[FileNameJoin[{benchmarkSource, "HEPCAT-SPINAS-Optimize.wl"}], "SHA256", "HexString"],
    "ReferenceConfigurationSHA256" -> FileHash[benchmarkConfigurationFile, "SHA256", "HexString"]|>;
  Export[FileNameJoin[{benchmarkDirectory, "benchmark-amplitudes.json"}], benchmarkProvenance, "RawJSON"];
  Print["Exported N=", photons, " complete ", benchmarkRepresentation, " family: ", benchmarkDirectory],
  {photons, 2, 4}];
Print[benchmarkRepresentation, " double-precision exports are ready. Build the numerical benchmark separately."];
Quit[0];
