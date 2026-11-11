(* Opt-in public six-point parity regressions. Construction receives fresh
   models, never a prior amplitude, supplied correction basis or saved report.
   Replay controls change only the seed and diagnostic output, not resources. *)

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


root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Global`$HEPCATpath = FileNameJoin[{root, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
$HistoryLength = 0;
piChecks = 0; piFailures = {}; piQualityFailures = {}; piResults = <||>;
SetAttributes[piCheck, HoldRest];
piCheck[name_, assertion_] := (piChecks++;
  If[!TrueQ[assertion], AppendTo[piFailures, name]; Print["FAIL: ", name]]);
SetAttributes[piQualityCheck, HoldRest];
piQualityCheck[name_, assertion_] := If[!TrueQ[assertion],
  AppendTo[piQualityFailures, name]; Print["QUALITY REGRESSION: ", name]];
piCases = <|
  "PPPM" -> {"e", "E", "A.+", "A.+", "A.+", "A.-"},
  "PMMM" -> {"e", "E", "A.+", "A.-", "A.-", "A.-"}|>;
(* These reproduce the user's saved successful and failed notebook runs. *)
piSeeds = <|"PPPM" -> 7000974597326515410, "PMMM" -> 6600655340731863618|>;
piSelection = Environment["HEPCAT_PHYSICAL_INTEGRATION_CASES"];
piSelection = If[MemberQ[{$Failed, ""}, piSelection], Keys[piCases],
  StringTrim /@ StringSplit[piSelection, ","]];
If[piSelection === {} || !AllTrue[piSelection, KeyExistsQ[piCases, #] &],
  Print["Unknown physical integration case: ", piSelection]; Exit[2]];
piSeedSelection = Environment["HEPCAT_PHYSICAL_INTEGRATION_SEEDS"];
piSeedWords = If[MemberQ[{$Failed, ""}, piSeedSelection], {},
  StringTrim /@ StringSplit[piSeedSelection, ","]];
If[Length[piSeedWords] > 4 || !AllTrue[piSeedWords,
    # === "Automatic" || StringMatchQ[#, DigitCharacter ..] &],
  Print["Provide at most four integer seeds, or Automatic."]; Exit[2]];
piSeedSweep = DeleteDuplicates[If[# === "Automatic", Automatic, FromDigits[#]] & /@ piSeedWords];
piRuns = Flatten[Table[<|"Case" -> name, "Seed" -> seed,
    "ResultKey" -> If[piSeedSweep === {}, name, name <> "@" <> ToString[seed]]|>,
  {name, piSelection}, {seed, If[piSeedSweep === {}, {piSeeds[name]}, piSeedSweep]}], 1];
piStrictQuality = Environment["HEPCAT_PHYSICAL_INTEGRATION_STRICT_QUALITY"] === "1";
piDiagnosticDirectory = CreateDirectory[];
piDiagramModel = ReadModel[FileNameJoin[{root, "models"}], 2];
piFactorizationModel = ReadModel[FileNameJoin[{root, "models"}], 3];
piFeynmanModel = ReadModel[FileNameJoin[{root, "tests", "reference-models", "calchep"}], 2];
piModelsBefore = {piDiagramModel, piFactorizationModel, piFeynmanModel};
piSourceFiles = Sort[Join[FileNames["*.wl", FileNameJoin[{root, "source"}], Infinity],
  FileNames["laurent-proof-*", FileNameJoin[{root, "source", "internal", "proofs"}]]]];
piSourceHashes = Association[(# -> FileHash[#, "SHA256", "HexString"] &) /@ piSourceFiles];
piModelFiles = Sort[Join[FileNames["*.mdl", FileNameJoin[{root, "models"}]],
  FileNames["*.mdl", FileNameJoin[{root, "tests", "reference-models", "calchep"}]]]];
piModelHashes = Association[(# -> FileHash[#, "SHA256", "HexString"] &) /@ piModelFiles];
piStageFixture[stage_] := If[AssociationQ[stage], KeyTake[stage,
  {"Status", "Amplitude", "OriginalExpression", "ExternalMassRules", "FamilyAmplitude",
   "FreeParameters", "RemainingBasis", "CorrectionBasis", "CorrectionSpace", "RandomSeed",
   "ParameterRules", "PolePreference", "Diagnostics", "ElapsedSeconds"}], stage];
Do[
  piCase = piRun["Case"]; piSeed = piRun["Seed"]; piKey = piRun["ResultKey"];
  Print["BEGIN PHYSICAL CONSTRUCTION: ", piKey, "; seed: ", piSeed];
  piStarted = AbsoluteTime[];
  piReport = ConstructAmplitude[piCases[piCase], piDiagramModel, piFactorizationModel,
    "PolePreference" -> "AllPhysical", "FeynmanModel" -> piFeynmanModel,
    "RandomSeed" -> piSeed, "Output" -> "Details"];
  piWall = AbsoluteTime[] - piStarted;
  piComplete = AssociationQ[piReport] && Lookup[piReport, "Status", None] === "COMPLETE";
  piCheck[piKey <> ": the public construction completes with ordinary Automatic resources", piComplete];
  piSummary = <|"Case" -> piCase, "RequestedSeed" -> piSeed,
    "SourceHashes" -> piSourceHashes, "ModelHashes" -> piModelHashes,
    "RandomSeed" -> If[AssociationQ[piReport], Lookup[piReport, "RandomSeed", Missing["Unavailable"]],
      Missing["Unavailable"]], "WallSeconds" -> piWall, "PhysicsEstablished" -> False, "QualityStatus" -> "NOT RUN"|>;
  If[piComplete,
    piPair = piReport["AmplitudePair"];
    piChannels = FactorizationAmplitudeChannels[piCases[piCase], piFactorizationModel];
    piNormalized = ConstructiveDiagrams`Private`completionNormalizeInput[
      piPair, piChannels, piFactorizationModel];
    piCheck[piKey <> ": only external mass declarations accompany the final expression",
      MatchQ[piPair, {_List, _}] && FreeQ[First[piPair], _Multiparticle]];
    piCheckReport = Lookup[piReport, "Checks", <||>];
    piRows = Lookup[piCheckReport, "Checks", {}];
    piPhysicalRows = Lookup[piCheckReport, "Channels", {}];
    piChannelsPass = Length[piChannels] === 14 && Length[piPhysicalRows] === 14 &&
      AllTrue[piPhysicalRows, Lookup[#, "Status", None] === "PASS" &];
    piCheck[piKey <> ": all fourteen original physical channels are checked and pass", piChannelsPass];
    piAllPass = ListQ[piRows] && piRows =!= {} &&
      AllTrue[piRows, Lookup[#, "Status", None] === "PASS" &];
    piMandatoryPresent = AllTrue[{"HighEnergyCheck", "ReferenceCheck"},
      Lookup[Lookup[piCheckReport, #, <||>], "Status", None] === "PASS" &] &&
      ListQ[Lookup[piCheckReport, "SpuriousChecks", None]];
    piCheck[piKey <> ": every mandatory final condition is present and passes", piAllPass && piMandatoryPresent];
    piFeynmanPass = Lookup[Lookup[piCheckReport, "FeynmanCheck", <||>], "Status", None] === "PASS";
    piCheck[piKey <> ": the independently generated Feynman comparison is present and passes", piFeynmanPass];
    piCheck[piKey <> ": no unresolved reference, shifted or on-cut-only spinor survives",
      FreeQ[Last[piPair], Spinor["Xi" | "Zeta", ___] | _SpinorHat | _MomHat |
        _xFactor | _xTildeFactor | Spinor[_, "Angle" | "Square", _Multiparticle, ___]]];
    piPhysical = TrueQ[ConstructiveDiagrams`Private`completionPhysicalDenominatorOnlyQ[
      Last[piPair], piNormalized]];
    piComplexity = SpinorExpressionComplexity[Last[piPair]];
    piQualityCheck[piKey <> ": AllPhysical contains only declared physical inverse factors", piPhysical];
    piQualityCheck[piKey <> ": complexity reaches the verified parity benchmark of 327", piComplexity <= 327];
    piSummary = Join[piSummary, <|"PhysicsEstablished" -> (piAllPass && piMandatoryPresent && piFeynmanPass && piChannelsPass),
      "PhysicalOnly" -> piPhysical, "CompactnessSatisfied" -> (piComplexity <= 327),
      "QualityStatus" -> If[piPhysical && piComplexity <= 327, "PASS", "FAIL"],
      "Complexity" -> piComplexity, "Leaves" -> LeafCount[Last[piPair]],
      "StageTimings" -> Lookup[piReport, "StageTimings", {}],
      "Checks" -> Lookup[piCheckReport, "Summary", <||>]|>],
    (* Keep an unresolved affine family reusable for focused proof tests, without
       serializing unrelated sample histories or ever feeding it to this run. *)
    piStages = If[AssociationQ[piReport], Lookup[piReport, "Stages", <||>], <||>];
    piFailureFixture = <|"Process" -> piCases[piCase], "RequestedSeed" -> piSeed,
      "SourceHashes" -> piSourceHashes, "ModelHashes" -> piModelHashes,
      "FactorizationModel" -> piFactorizationModel,
      "Failure" -> If[AssociationQ[piReport], Lookup[piReport, "Failure", None], piReport],
      "FactorizationFamily" -> piStageFixture[Lookup[piStages, "Completing factorization", Missing["NotRun"]]],
      "HighEnergyFamily" -> piStageFixture[Lookup[piStages, "Completing high-energy behavior", Missing["NotRun"]]],
      "Summary" -> piSummary|>;
    Put[piFailureFixture, FileNameJoin[{piDiagnosticDirectory, piKey <> "-failure-family.wl"}]];
    Print["Failure family: ", FileNameJoin[{piDiagnosticDirectory, piKey <> "-failure-family.wl"}]]
  ];
  AssociateTo[piResults, piKey -> piSummary];
  Put[piSummary, FileNameJoin[{piDiagnosticDirectory, piKey <> "-summary.wl"}]];
  Print["PHYSICAL CONSTRUCTION RESULT: ", ToString[piSummary, System`InputForm]],
  {piRun, piRuns}];
piCheck["construction and checks leave every supplied model unchanged",
  piModelsBefore === {piDiagramModel, piFactorizationModel, piFeynmanModel}];
piCheck["source files stay unchanged throughout the sequential comparison",
  piSourceHashes === Association[(# -> FileHash[#, "SHA256", "HexString"] &) /@ piSourceFiles]];
If[piStrictQuality, Do[piCheck["strict representation quality: " <> name, False], {name, piQualityFailures}]];
Print["PHYSICAL CONSTRUCTION RESULTS: ", ToString[piResults, System`InputForm]];
Print["PHYSICS CHECKS: ", piChecks, "; FAILURES: ", Length[piFailures],
  "; REPRESENTATION QUALITY FAILURES: ", Length[piQualityFailures]];
Print["Reports retained in: ", piDiagnosticDirectory];
If[piFailures === {}, Print["HEPCAT_TEST_COMPLETED:physical-construction-integration-tests.wl"], Print[piFailures]];
Quit[If[piFailures === {}, 0, 1]];
