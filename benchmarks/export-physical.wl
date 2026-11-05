(* Export the saved, verified all-physical QED formulas for timing.
   Run with WolframKernel -script benchmarks/export-physical.wl.
   Optional arguments: --output DIR, --source DIR, --spinas DIR,
     --calchep DIR, --madgraph DIR, --configuration MANIFEST,
     --code-style Optimized|Readable.
   The configuration manifest is an existing WriteSPINASProcess export. Its
   explicitly selected reference installations/models are retained by default.
   No amplitude is constructed, simplified, fitted or changed by this script. *)
$CharacterEncoding = "UTF-8";
benchmarkRepository = DirectoryName[DirectoryName[ExpandFileName[$InputFileName]]];
benchmarkArguments = If[$ScriptCommandLine =!= {}, Rest[$ScriptCommandLine],
  With[{benchmarkScriptPosition = FirstPosition[$CommandLine, "-script", Missing["NotScript"]]},
    If[ListQ[benchmarkScriptPosition], Drop[$CommandLine, First[benchmarkScriptPosition] + 1], {}]]];
benchmarkOptions = <||>;
If[OddQ[Length[benchmarkArguments]], Print["Expected option/value argument pairs."]; Quit[1]];
Do[
  If[!MemberQ[{"--output", "--source", "--spinas", "--calchep", "--madgraph", "--configuration", "--code-style"}, benchmarkArguments[[benchmarkArgumentIndex]]],
    Print["Unknown option: ", benchmarkArguments[[benchmarkArgumentIndex]]]; Quit[1]];
  AssociateTo[benchmarkOptions, benchmarkArguments[[benchmarkArgumentIndex]] -> benchmarkArguments[[benchmarkArgumentIndex + 1]]],
  {benchmarkArgumentIndex, 1, Length[benchmarkArguments], 2}];
benchmarkOutput = ExpandFileName[Lookup[benchmarkOptions, "--output",
  FileNameJoin[{benchmarkRepository, "benchmarks", "build-physical-double", "exports"}]]];
benchmarkSource = ExpandFileName[Lookup[benchmarkOptions, "--source", FileNameJoin[{benchmarkRepository, "source"}]]];
benchmarkCodeStyle = Lookup[benchmarkOptions, "--code-style", "Optimized"];
If[!MemberQ[{"Optimized", "Readable"}, benchmarkCodeStyle],
  Print["--code-style must be Optimized or Readable."]; Quit[1]];
benchmarkSPINAS = ExpandFileName[Lookup[benchmarkOptions, "--spinas", FileNameJoin[{DirectoryName[benchmarkRepository], "spinas"}]]];
benchmarkConfigurationFile = ExpandFileName[Lookup[benchmarkOptions, "--configuration",
  FileNameJoin[{benchmarkRepository, "tests", "spinas-output", "eeAA", "manifest.json"}]]];
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
benchmarkModel = ReadModel[benchmarkModelDirectory, 3];
If[!ListQ[benchmarkModel], Print["Could not load QED factorization model 3."]; Quit[1]];
benchmarkMass = ModelParameter[benchmarkModel, "Me"];
benchmarkLookup[particles_] := LookupAmplitude[
  MapIndexed[{#1, First[#2]} &, particles], benchmarkModel, False,
  {benchmarkSpin1, benchmarkSpin2, benchmarkSpin3, benchmarkSpin4}];
benchmarkPhysicalPoleQ[PropDen[Mom[Multiparticle[labels__Integer]], mass_], legs_Integer] := Module[{cut = {labels}},
  DuplicateFreeQ[cut] && SubsetQ[Range[legs], cut] && 2 <= Length[cut] <= legs - 2 &&
    Count[cut, 1 | 2] === 1 && mass === benchmarkMass];
benchmarkPhysicalPoleQ[_, _] := False;
benchmarkFileHashes = Association@Table[
  filename -> FileHash[FileNameJoin[{benchmarkModelDirectory, filename}], "SHA256", "HexString"],
  {filename, {"amps3.mdl", "prtcls3.mdl", "vars3.mdl", "func3.mdl"}}];
benchmarkRecords = {};
Do[
  benchmarkEntries = Table[
    benchmarkParticles = Join[{"e", "E"}, ConstantArray["A.+", photons - negative], ConstantArray["A.-", negative]];
    benchmarkExpression = benchmarkLookup[benchmarkParticles];
    If[benchmarkExpression === 0 || !FreeQ[benchmarkExpression, _Failure | $Failed | Spinor["Xi", ___] | _xFactor | _xTildeFactor],
      Print["Missing or unsupported saved amplitude: ", benchmarkParticles]; Quit[1]];
    benchmarkInverseBases = DeleteDuplicates[Cases[benchmarkExpression, Power[base_, exponent_?Negative] :> base, Infinity]];
    benchmarkBadBases = Select[benchmarkInverseBases,
      !MatchQ[#, _ModelParameter] && !benchmarkPhysicalPoleQ[#, photons + 2] &];
    If[benchmarkBadBases =!= {},
      Print["Nonphysical denominator in ", benchmarkParticles, ": ", InputForm[benchmarkBadBases]]; Quit[1]];
    AppendTo[benchmarkRecords, <|
      "Particles" -> benchmarkParticles,
      "Complexity" -> SpinorExpressionComplexity[benchmarkExpression],
      "ExpressionLeaves" -> LeafCount[benchmarkExpression],
      "ExpressionSHA256" -> Hash[benchmarkExpression, "SHA256", "HexString"],
      "PhysicalPoleCuts" -> (Cases[#, Multiparticle[labels__] :> {labels}, Infinity][[1]] & /@
        Select[benchmarkInverseBases, Head[#] === PropDen &]),
      "NonkinematicInverseFactors" -> (ModelParameterName /@ Select[benchmarkInverseBases, Head[#] === ModelParameter &])|>];
    {benchmarkParticles, benchmarkExpression},
    {negative, 0, photons}];
  benchmarkName = "ee" <> StringRepeat["A", photons];
  benchmarkDirectory = FileNameJoin[{benchmarkOutput, benchmarkName}];
  benchmarkExport = WriteSPINASProcess[benchmarkDirectory,
    {"e", "E"} -> ConstantArray["A", photons], benchmarkEntries, benchmarkModel,
    "SPINASDirectory" -> benchmarkSPINAS, "Precision" -> "Double", "ProcessName" -> benchmarkName,
    "CodeStyle" -> benchmarkCodeStyle,
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
    "Representation" -> "AllPhysical", "Precision" -> "Double", "Photons" -> photons,
    "CodeStyle" -> benchmarkCodeStyle,
    "AmplitudeSource" -> "ReadModel[models,3] and LookupAmplitude; unchanged saved complete amplitudes",
    "ModelFilesSHA256" -> benchmarkFileHashes,
    "InputHelicitySectors" -> Select[benchmarkRecords, Length[#["Particles"]] === photons + 2 &],
    "CompletePhotonHelicityCount" -> 2^photons,
    "ShellScope" -> "External masses only; no internal on-shell substitutions during export",
    "SourceDirectory" -> benchmarkSource,
    "ExporterSHA256" -> FileHash[FileNameJoin[{benchmarkSource, "HEPCAT-SPINAS.wl"}], "SHA256", "HexString"],
    "OptimizationBackendSHA256" -> FileHash[FileNameJoin[{benchmarkSource, "HEPCAT-SPINAS-Optimize.wl"}], "SHA256", "HexString"],
    "ReferenceConfigurationSHA256" -> FileHash[benchmarkConfigurationFile, "SHA256", "HexString"]|>;
  Export[FileNameJoin[{benchmarkDirectory, "benchmark-amplitudes.json"}], benchmarkProvenance, "RawJSON"];
  Print["Exported N=", photons, " complete all-physical family: ", benchmarkDirectory],
  {photons, 2, 4}];
Print["All physical double-precision exports are ready. Build the numerical benchmark separately."];
Quit[0];
