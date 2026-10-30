(* Opt-in expensive regression: the exact notebook call must discover its result.
   No example formula, prior amplitude, supplied correction basis or saved
   preparation report is loaded into this construction. *)
root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Global`$HEPCATpath = FileNameJoin[{root, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
$HistoryLength = 0;
checks = 0; failures = {};
SetAttributes[check, HoldRest];
check[name_, value_] := (checks++; If[!TrueQ[value], AppendTo[failures, name]; Print["FAIL: ", name]]);
diagramModel = ReadModel[FileNameJoin[{root, "models"}], 2];
factorizationModel = ReadModel[FileNameJoin[{root, "models"}], 3];
qedFeynmanModel = ReadModel[FileNameJoin[{root, "model_feynman"}], 2];
piModelsBefore = {diagramModel, factorizationModel, qedFeynmanModel};
piProcess = {"e", "E", "A.+", "A.+", "A.+", "A.-"};
piStarted = AbsoluteTime[];
qed6PPPM = ConstructAmplitude[
  {"e", "E", "A.+", "A.+", "A.+", "A.-"},
  diagramModel, factorizationModel,
  "PolePreference" -> "AllPhysical",
  "FeynmanModel" -> qedFeynmanModel];
piWall = AbsoluteTime[] - piStarted;
piValid = !FailureQ[qed6PPPM] && qed6PPPM =!= $Failed &&
  !AssociationQ[qed6PPPM] && !ListQ[qed6PPPM] && FreeQ[qed6PPPM, _ConstructAmplitude];
check["the exact notebook call returns a validated expression with ordinary Automatic resources", piValid];
If[piValid,
  piChannels = FactorizationAmplitudeChannels[piProcess, factorizationModel];
  piExternal = Thread[(Mass /@ Range[6]) -> {
    ModelParameter[factorizationModel, "Me"], ModelParameter[factorizationModel, "Me"], 0, 0, 0, 0}];
  piNormalized = ConstructiveDiagrams`Private`completionNormalizeInput[
    {piExternal, qed6PPPM}, piChannels, factorizationModel];
  piPhysical = AssociationQ[piNormalized] &&
    ConstructiveDiagrams`Private`completionPhysicalDenominatorOnlyQ[qed6PPPM, piNormalized];
  piComplexity = SpinorExpressionComplexity[qed6PPPM];
  check["the freshly discovered result contains only declared physical denominator factors", TrueQ[piPhysical]];
  check["the freshly discovered result reaches the verified compact complexity benchmark", piComplexity <= 327];
  check["no unresolved reference, shifted or on-cut-only spinors survive",
    FreeQ[qed6PPPM, Spinor["Xi" | "Zeta", ___] | _SpinorHat | _MomHat | _xFactor | _xTildeFactor |
      Spinor[_, "Angle" | "Square", _Multiparticle, ___]]];
  (* Rebuild the independent Feynman benchmark after construction. This is only
     validation, never a reconstruction input or replacement answer. *)
  piFeynman = PrepareAmplitudes[
    diagramAmplitudes[CreateDiagrams[piProcess, qedFeynmanModel]], qedFeynmanModel];
  piCheck = CheckAmplitudes[{piExternal, qed6PPPM}, piChannels, factorizationModel,
    "FeynmanAmplitudes" -> piFeynman, "FeynmanModel" -> qedFeynmanModel,
    "Output" -> "Details"];
  check["independently regenerated physical, spurious, high-energy, reference and Feynman checks pass",
    AssociationQ[piCheck] && AllTrue[piCheck["Checks"], #["Status"] === "PASS" &]];
  check["the independent Feynman amplitude comparison is present and passes",
    AssociationQ[piCheck] && Lookup[Lookup[piCheck, "FeynmanCheck", <||>], "Status", None] === "PASS"];
  piSummary = <|"WallSeconds" -> piWall, "PhysicalOnly" -> piPhysical,
    "Complexity" -> piComplexity, "Leaves" -> LeafCount[qed6PPPM],
    "Groups" -> If[Head[qed6PPPM] === Plus, Length[qed6PPPM], 1],
    "IndependentChecks" -> Lookup[piCheck, "Summary", Missing["Unavailable"]]|>;
  Print["EXACT PHYSICAL CONSTRUCTION RESULT: ", InputForm[piSummary]]];
check["fresh construction and checks leave all supplied models unchanged",
  {diagramModel, factorizationModel, qedFeynmanModel} === piModelsBefore];
Print["PHYSICAL CONSTRUCTION INTEGRATION CHECKS: ", checks, "; FAILURES: ", Length[failures]];
If[failures === {}, Print["HEPCAT_TEST_COMPLETED:physical-construction-integration-tests.wl"],
  piFailureDirectory = CreateDirectory[];
  Put[<|"Amplitude" -> qed6PPPM, "Failures" -> failures,
    "WallSeconds" -> piWall|>, FileNameJoin[{piFailureDirectory, "construction-failure.m"}]];
  Print[failures]; Print["CONSTRUCTION DIAGNOSTICS: ", piFailureDirectory]];
Quit[If[failures === {}, 0, 1]];
