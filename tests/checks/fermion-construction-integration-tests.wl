(* Expensive opt-in regressions. Each result is discovered by the public
   constructor from fresh models, without a saved amplitude or correction basis.
   HEPCAT_FERMION_INTEGRATION_CASES may select comma-separated PP, PM, MM, SixF. *)

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
Global`ficChecks = 0; Global`ficFailures = {};
SetAttributes[ficCheck, HoldRest];
ficCheck[name_, value_] := (Global`ficChecks++;
  If[!TrueQ[value], AppendTo[Global`ficFailures, name]; Print["FAIL: ", name]]);
Global`ficCases = <|
  "PP" -> {"e", "E", "E", "e", "A.+", "A.+"},
  "PM" -> {"e", "E", "E", "e", "A.+", "A.-"},
  "MM" -> {"e", "E", "E", "e", "A.-", "A.-"},
  "SixF" -> {"e", "E", "e", "E", "e", "E"}|>;
Global`ficSeeds = <|"PP" -> 8311276771406094639, "PM" -> 5752922635413568164,
  "MM" -> 103825747827186080, "SixF" -> 3174522571357952910|>;
Global`ficSelection = Environment["HEPCAT_FERMION_INTEGRATION_CASES"];
Global`ficSelection = If[MemberQ[{$Failed, ""}, Global`ficSelection], Keys[Global`ficCases],
  StringTrim /@ StringSplit[Global`ficSelection, ","]];
If[!AllTrue[Global`ficSelection, KeyExistsQ[Global`ficCases, #] &],
  Print["Unknown fermion integration case: ", Global`ficSelection]; Quit[2]];
Global`ficDiagramModel = ReadModel[FileNameJoin[{root, "models"}], 3];
Global`ficFactorizationModel = ReadModel[FileNameJoin[{root, "models"}], 4];
Global`ficFeynmanModel = ReadModel[FileNameJoin[{root, "tests", "reference-models", "calchep"}], 2];
Global`ficModelsBefore = {Global`ficDiagramModel, Global`ficFactorizationModel, Global`ficFeynmanModel};
Do[
  Global`ficStarted = AbsoluteTime[];
  Global`ficReport = ConstructAmplitude[Global`ficCases[Global`ficCase],
    Global`ficDiagramModel, Global`ficFactorizationModel,
    "PolePreference" -> "AllPhysical", "FeynmanModel" -> Global`ficFeynmanModel,
    "RandomSeed" -> Global`ficSeeds[Global`ficCase], "Output" -> "Details"];
  Global`ficValid = AssociationQ[Global`ficReport] &&
    Lookup[Global`ficReport, "Status", None] === "COMPLETE";
  ficCheck[Global`ficCase <> ": public construction finishes with Automatic resources", Global`ficValid];
  If[Global`ficValid,
    Global`ficAmplitude = Global`ficReport["AmplitudePair"];
    Global`ficChannels = FactorizationAmplitudeChannels[Global`ficCases[Global`ficCase], Global`ficFactorizationModel];
    Global`ficNormalized = ConstructiveDiagrams`Private`completionNormalizeInput[
      Global`ficAmplitude, Global`ficChannels, Global`ficFactorizationModel];
    ficCheck[Global`ficCase <> ": the result contains only external mass declarations",
      MatchQ[Global`ficAmplitude, {_List, _}] && FreeQ[First[Global`ficAmplitude], _Multiparticle]];
    ficCheck[Global`ficCase <> ": the result has only declared physical inverse factors",
      ConstructiveDiagrams`Private`completionPhysicalDenominatorOnlyQ[Last[Global`ficAmplitude], Global`ficNormalized]];
    ficCheck[Global`ficCase <> ": every mandatory final check passes",
      AllTrue[Global`ficReport["Checks"]["Checks"], Lookup[#, "Status", None] === "PASS" &]];
    ficCheck[Global`ficCase <> ": the independent Feynman comparison is present and passes",
      Lookup[Lookup[Global`ficReport["Checks"], "FeynmanCheck", <||>], "Status", None] === "PASS"];
    ficCheck[Global`ficCase <> ": no reference or on-cut spinors survive",
      FreeQ[Last[Global`ficAmplitude], Spinor["Xi" | "Zeta", ___] | _SpinorHat | _MomHat |
        _xFactor | _xTildeFactor | Spinor[_, "Angle" | "Square", _Multiparticle, ___]]];
    Print["FERMION CONSTRUCTION RESULT: ", System`InputForm[<|
      "Case" -> Global`ficCase, "WallSeconds" -> AbsoluteTime[] - Global`ficStarted,
      "Complexity" -> SpinorExpressionComplexity[Last[Global`ficAmplitude]],
      "Leaves" -> LeafCount[Last[Global`ficAmplitude]],
      "Checks" -> Global`ficReport["Checks"]["Summary"]|>]]],
  {Global`ficCase, Global`ficSelection}];
ficCheck["all supplied models remain unchanged",
  Global`ficModelsBefore === {Global`ficDiagramModel, Global`ficFactorizationModel, Global`ficFeynmanModel}];
Print["FERMION CONSTRUCTION INTEGRATION CHECKS: ", Global`ficChecks,
  "; FAILURES: ", Length[Global`ficFailures]];
If[Global`ficFailures === {}, Print["HEPCAT_TEST_COMPLETED:fermion-construction-integration-tests.wl"],
  Print[Global`ficFailures]];
Quit[If[Global`ficFailures === {}, 0, 1]];
