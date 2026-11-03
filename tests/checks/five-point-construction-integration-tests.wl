(* Opt-in end-to-end regressions for the six five-point QED notebook calls.
   Every amplitude is constructed from the models, without a reference answer.
   HEPCAT_FIVE_POINT_INTEGRATION_CASES may select comma-separated case names.
   Use the external runner's timeout for the complete selected batch. *)
fpciRoot = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Get[FileNameJoin[{fpciRoot, "source", "HEPCAT.wl"}]];
$HistoryLength = 0;
fpciChecks = 0; fpciFailures = {}; fpciResults = <||>;
SetAttributes[fpciCheck, HoldRest];
fpciCheck[name_, assertion_] := (fpciChecks++;
  If[!TrueQ[assertion], AppendTo[fpciFailures, name]; Print["FAIL: ", name]]);
fpciCases = <|
  "PPP" -> {"e", "E", "A.+", "A.+", "A.+"},
  "PPM" -> {"e", "E", "A.+", "A.+", "A.-"},
  "PMM" -> {"e", "E", "A.+", "A.-", "A.-"},
  "MMM" -> {"e", "E", "A.-", "A.-", "A.-"},
  "FourFermionP" -> {"e", "E", "E", "e", "A.+"},
  "FourFermionM" -> {"e", "E", "E", "e", "A.-"}|>;
(* The same seed within each parity pair makes repeated comparisons useful.
   Timing and display controls do not alter the notebook's default algorithm. *)
fpciSeeds = <|"PPP" -> 410071, "MMM" -> 410071,
  "PPM" -> 410073, "PMM" -> 410073,
  "FourFermionP" -> 410077, "FourFermionM" -> 410077|>;
(* These are upper bounds achieved by independently checked constructions,
   not formulas supplied to the constructor. They detect a common regression
   even when both members of a helicity pair become equally complicated. *)
fpciComplexityBounds = <|"PPP" -> 27, "MMM" -> 27, "PPM" -> 71, "PMM" -> 71,
  "FourFermionP" -> 160, "FourFermionM" -> 160|>;
(* Keep the independently verified pole-block grouping visible in the public
   result, rather than testing only its private helper. This bound permits
   minor scalar presentation differences while rejecting the older nesting. *)
fpciLeafBounds = <|"PPM" -> 330, "PMM" -> 330|>;
fpciSelection = Environment["HEPCAT_FIVE_POINT_INTEGRATION_CASES"];
fpciSelection = If[MemberQ[{$Failed, ""}, fpciSelection], Keys[fpciCases],
  StringTrim /@ StringSplit[fpciSelection, ","]];
If[!AllTrue[fpciSelection, KeyExistsQ[fpciCases, #] &],
  Print["Unknown five-point integration case: ", fpciSelection]; Exit[2]];
fpciDiagnosticDirectory = CreateDirectory[];
fpciDiagramModel = ReadModel[FileNameJoin[{fpciRoot, "models"}], 2];
fpciFactorizationModel = ReadModel[FileNameJoin[{fpciRoot, "models"}], 3];
fpciFeynmanModel = ReadModel[FileNameJoin[{fpciRoot, "model_feynman"}], 2];
fpciModelsBefore = {fpciDiagramModel, fpciFactorizationModel, fpciFeynmanModel};
Do[
  fpciStarted = AbsoluteTime[];
  fpciReport = ConstructAmplitude[fpciCases[fpciCase], fpciDiagramModel,
    fpciFactorizationModel, "FeynmanModel" -> fpciFeynmanModel,
    "RandomSeed" -> fpciSeeds[fpciCase], "Output" -> "Details"];
  fpciElapsed = AbsoluteTime[] - fpciStarted;
  Put[fpciReport, FileNameJoin[{fpciDiagnosticDirectory, fpciCase <> ".wl"}]];
  fpciValid = AssociationQ[fpciReport] && Lookup[fpciReport, "Status", None] === "COMPLETE";
  fpciCheck[fpciCase <> ": the unmodified default construction completes", fpciValid];
  If[fpciValid,
    fpciPair = fpciReport["AmplitudePair"];
    fpciChannels = FactorizationAmplitudeChannels[fpciCases[fpciCase], fpciFactorizationModel];
    fpciNormalized = ConstructiveDiagrams`Private`completionNormalizeInput[
      fpciPair, fpciChannels, fpciFactorizationModel];
    fpciCheck[fpciCase <> ": only external masses accompany the final amplitude",
      MatchQ[fpciPair, {_List, _}] && FreeQ[First[fpciPair], _Multiparticle]];
    fpciCheck[fpciCase <> ": the final representation contains only physical poles",
      TrueQ[ConstructiveDiagrams`Private`completionPhysicalDenominatorOnlyQ[
        Last[fpciPair], fpciNormalized]]];
    fpciCheck[fpciCase <> ": every mandatory final check passes",
      ListQ[fpciReport["Checks"]["Checks"]] && fpciReport["Checks"]["Checks"] =!= {} &&
        AllTrue[fpciReport["Checks"]["Checks"], Lookup[#, "Status", None] === "PASS" &]];
    fpciCheck[fpciCase <> ": the independent Feynman comparison passes",
      Lookup[Lookup[fpciReport["Checks"], "FeynmanCheck", <||>], "Status", None] === "PASS"];
    fpciCheck[fpciCase <> ": no reference or internal on-shell spinor survives",
      FreeQ[Last[fpciPair], Spinor["Xi" | "Zeta", ___] | _SpinorHat | _MomHat |
        _xFactor | _xTildeFactor | Spinor[_, "Angle" | "Square", _Multiparticle, ___]]];
    fpciComplexity = SpinorExpressionComplexity[Last[fpciPair]];
    fpciCheck[fpciCase <> ": the result is no more complicated than the known achievable form",
      fpciComplexity <= fpciComplexityBounds[fpciCase]];
    If[KeyExistsQ[fpciLeafBounds, fpciCase],
      fpciCheck[fpciCase <> ": the public result retains compact pole-block grouping",
        LeafCount[Last[fpciPair]] <= fpciLeafBounds[fpciCase]]];
    AssociateTo[fpciResults, fpciCase -> <|"Complexity" -> fpciComplexity,
      "Leaves" -> LeafCount[Last[fpciPair]], "Seconds" -> fpciElapsed,
      "StageTimings" -> fpciReport["StageTimings"], "Summary" -> fpciReport["Checks"]["Summary"]|>],
    Print["Failed construction diagnostics: ", FileNameJoin[{fpciDiagnosticDirectory, fpciCase <> ".wl"}]]],
  {fpciCase, fpciSelection}];
Do[If[AllTrue[fpciPairNames, KeyExistsQ[fpciResults, #] &],
  fpciCheck[StringRiffle[fpciPairNames, "/"] <> ": opposite helicities have equally simple results",
    SameQ @@ (fpciResults[#]["Complexity"] & /@ fpciPairNames)]],
  {fpciPairNames, {{"PPP", "MMM"}, {"PPM", "PMM"}, {"FourFermionP", "FourFermionM"}}}];
fpciCheck["all supplied models remain unchanged",
  fpciModelsBefore === {fpciDiagramModel, fpciFactorizationModel, fpciFeynmanModel}];
Print["FIVE-POINT CONSTRUCTION RESULTS: ", ToString[fpciResults, System`InputForm]];
Print["Five-point construction integration checks: ", fpciChecks, "; failures: ", Length[fpciFailures]];
If[fpciFailures === {}, Print["HEPCAT_TEST_COMPLETED:five-point-construction-integration-tests.wl"],
  Print[fpciFailures]; Print["Reports retained in: ", fpciDiagnosticDirectory]];
Exit[If[fpciFailures === {}, 0, 1]];
