(* HEPCAT - High Energy Physics Constructive Amplitude Tool
   Copyright (C) 2026 Neil Christensen
   SPDX-License-Identifier: GPL-3.0-or-later

   Whole-Series assumptions must be certified even when message display is
   suppressed. These small synthetic controls use no model or saved report. *)

root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
repositoryOverride = Environment["HEPCAT_TEST_REPOSITORY"];
If[StringQ[repositoryOverride] && repositoryOverride =!= "", root = repositoryOverride];
entry = Environment["HEPCAT_TEST_PACKAGE_ENTRYPOINT"];
If[!StringQ[entry] || entry === "", entry = FileNameJoin[{root, "source", "HEPCAT.wl"}]];
Global`$HEPCATpath = DirectoryName[entry];
Get[entry];
Begin["HEPCATWholeSeriesWarningTests`Private`"];
$ContextPath = Prepend[DeleteCases[$ContextPath, "HEPCATWholeSeriesWarningTests`Private`"],
  "HEPCATWholeSeriesWarningTests`Private`"];
$HistoryLength = 0;
checks = 0; failures = {};
check[name_, result_] := (checks++;
  If[!TrueQ[result], AppendTo[failures, name]; Print["FAIL: ", name]]);

messageFields[] := {DownValues[Message], UpValues[Message], SubValues[Message],
  NValues[Message], OwnValues[Message], DefaultValues[Message], FormatValues[Message],
  Options[Message], Attributes[Message], Messages[Message]};
messageBefore = messageFields[];
seriesBefore = Messages[Series];
seriesAttributesBefore = Attributes[Series];
scalarBefore = DownValues[ConstructiveDiagrams`Private`highEnergyCertifiedScalarSeries];
entryBefore = DownValues[ConstructiveDiagrams`Private`highEnergyLaurentJetCoefficients];
nativeBefore = DownValues[ConstructiveDiagrams`Private`highEnergyLaurentJetCoefficientsNative];
DownValues[nativeScalarSeries] = scalarBefore /.
  HoldPattern[ConstructiveDiagrams`Private`highEnergyCertifiedScalarSeries] :> nativeScalarSeries;
{variable, spin} = Table[Unique["wholeWarningTest"], {2}];
fixture = (1 + spin)^2 (Sqrt[2 + variable^2] - Sqrt[2])/variable^2;
actualJet[] := ConstructiveDiagrams`Private`highEnergyLaurentJetCoefficients[
  fixture, variable, {spin}, {}, 0, 12, 4];
baseline = actualJet[];
check["actual native baseline is complete",
  AssociationQ[baseline] && baseline["Status"] === "COMPLETE"];

(* Inject into an actual native scalar expansion, rather than replacing the
   tested wrapper. Zero and nonzero assumptions are checked under every display
   state. Scope the Series message state as well as the replaced scalar helper. *)
warningRun[mode_String, message_String, argument_Integer] := Module[
  {hits = 0, result, certificates, complete},
  result = Internal`InheritedBlock[
    {Series, ConstructiveDiagrams`Private`highEnergyCertifiedScalarSeries},
    On[MessageName[Series, message]];
    Clear[ConstructiveDiagrams`Private`highEnergyCertifiedScalarSeries];
    ConstructiveDiagrams`Private`highEnergyCertifiedScalarSeries[e_, v_, n_] :=
      Module[{value = nativeScalarSeries[e, v, n]},
        hits++;
        If[mode === "InnerQuiet", Quiet[Message[MessageName[Series, message], argument]],
          Message[MessageName[Series, message], argument]];
        value];
    Which[mode === "OuterQuiet", Quiet[actualJet[]],
      mode === "GlobalOff", Off[MessageName[Series, message]]; actualJet[],
      True, actualJet[]]];
  certificates = If[AssociationQ[result],
    Lookup[result, "WholeJetZeroAssumptionCertificates", {}], {}];
  complete = AssociationQ[result] && Lookup[result, "Status", None] === "COMPLETE";
  hits > 0 && If[argument === 0,
    complete && Length[certificates] === hits &&
      AllTrue[certificates, Lookup[#, "ExactValues", None] === {0} &&
        Lookup[#, "NumericalTolerance", None] === None &],
    !complete && MatchQ[result, Failure["JetUncertifiedWholeArithmeticAssumption", _Association]]]
];
Do[
  check["actual native " <> message <> " " <> mode <> " argument " <> ToString[argument],
    warningRun[mode, message, argument]],
  {message, {"ztest", "ztest1"}}, {argument, {0, 1}},
  {mode, {"Normal", "OuterQuiet", "InnerQuiet", "GlobalOff"}}];
(* On/Off changes a kernel message registry that InheritedBlock does not
   restore for previously unmaterialized built-in messages. Restore the exact
   Series state deliberately changed by this harness, including absent tags. *)
Unprotect[Series];
Messages[Series] = seriesBefore;
Attributes[Series] = seriesAttributesBefore;
check["scalar expansion definition restored after warning matrix",
  DownValues[ConstructiveDiagrams`Private`highEnergyCertifiedScalarSeries] === scalarBefore];
check["entry and native core restored after warning matrix",
  DownValues[ConstructiveDiagrams`Private`highEnergyLaurentJetCoefficients] === entryBefore &&
  DownValues[ConstructiveDiagrams`Private`highEnergyLaurentJetCoefficientsNative] === nativeBefore];
check["message definitions and Series state restored after warning matrix",
  messageFields[] === messageBefore && Messages[Series] === seriesBefore];

certify[arguments_] := Block[{
    ConstructiveDiagrams`Private`$highEnergyJetZeroAssumptions = {
      <|"Message" -> HoldComplete[Series::ztest1], "Arguments" -> arguments|>},
    ConstructiveDiagrams`Private`$highEnergyJetZeroCertificates = {},
    ConstructiveDiagrams`Private`$highEnergyJetZeroProofCache = <||>,
    ConstructiveDiagrams`Private`$highEnergyJetProvedEvents = 0},
  ConstructiveDiagrams`Private`highEnergyJetCertifyZeroAssumptions[]];
check["exact integer zero certifies", certify[{0}] === True];
check["exact algebraic zero certifies", certify[{Sqrt[2] - Sqrt[8]/2}] === True];
Do[check["reject unsupported or nonzero assumption " <> ToString[value, InputForm],
    FailureQ[certify[value]]],
  {value, {{1}, {Sqrt[2]}, {0.}, {unresolvedSymbol}, {}, {Infinity},
    {Missing["Unavailable"]}, ConstantArray[0, 65]}}];
other = ConstructiveDiagrams`Private`highEnergyJetCaptureZeroAssumptions[
  Quiet[Check[1/0, $Failed]]];
check["unrelated messages are not converted to certified Series assumptions", other === $Failed];

(* Stubs represent native return categories, not amplitude answers. They expose
   the literal effort arguments, retry count and shared parent deadline. *)
runFake[callback_] := Internal`InheritedBlock[
  {ConstructiveDiagrams`Private`highEnergyLaurentJetCoefficientsNative},
  Clear[ConstructiveDiagrams`Private`highEnergyLaurentJetCoefficientsNative];
  ConstructiveDiagrams`Private`highEnergyLaurentJetCoefficientsNative[args___] := callback[args];
  ConstructiveDiagrams`Private`highEnergyLaurentJetExpansion[1, variable, {}, {}, 2]];
calls = {};
fast[args___] := (AppendTo[calls, {args}]; <|"Status" -> "COMPLETE", "Marker" -> "exact fast result"|>);
fastResult = runFake[fast];
check["completed native pass is returned literally without retry",
  Length[calls] === 1 && fastResult === <|"Status" -> "COMPLETE", "Marker" -> "exact fast result"|>];
check["first pass retains literal precision12 and30second allowance",
  calls[[1, -2]] === 12 && calls[[1, -1]] === 30];
Do[
  calls = {};
  resource[args___] := (AppendTo[calls, {args}];
    If[Length[calls] === 1, firstFailure, <|"Status" -> "COMPLETE"|>]);
  retried = runFake[resource];
  check["one bounded retry for " <> ToString[firstFailure, InputForm],
    Length[calls] === 2 && calls[[2, -2]] === 24 && 0 < calls[[2, -1]] <= 90],
  {firstFailure, {Failure["JetIncompleteCoverage", <||>],
    Failure["JetTimeConstraint", <|"Phase" -> "JetSpinRing"|>],
    Failure["JetTimeConstraint", <|"Phase" -> "Coverage"|>]}}];
Do[
  calls = {};
  rejected[args___] := (AppendTo[calls, {args}]; terminalFailure);
  runFake[rejected];
  check["no retry for " <> ToString[terminalFailure, InputForm], Length[calls] === 1],
  {terminalFailure, {Failure["JetMemoryLimit", <||>], Failure["JetProductLimit", <||>],
    Failure["JetUncertifiedArithmeticMessage", <||>],
    Failure["JetUncertifiedWholeArithmeticAssumption", <||>],
    Failure["JetTimeConstraint", <||>], Failure["JetTimeConstraint", <|"Phase" -> "Input"|>]}}];
calls = {};
repeated[args___] := (AppendTo[calls, {args}];
  Failure["JetTimeConstraint", <|"Phase" -> "JetSpinRing"|>]);
runFake[repeated];
check["resource retry never exceeds two total passes", Length[calls] === 2];
calls = {};
badWarning[args___] := (AppendTo[calls, {args}];
  Message[Series::ztest1, System`HoldCompleteForm[1]];
  Failure["JetIncompleteCoverage", <||>]);
rejectedWarning = runFake[badWarning];
check["nonzero whole warning cannot receive a favorable retry",
  Length[calls] === 1 && MatchQ[rejectedWarning,
    Failure["JetUncertifiedWholeArithmeticAssumption", _Association]]];
calls = {};
timed[args___] := (AppendTo[calls, {args}]; Pause[.01];
  If[Length[calls] === 1, Failure["JetIncompleteCoverage", <||>], <|"Status" -> "COMPLETE"|>]);
Block[{ConstructiveDiagrams`Private`$highEnergyJetParentDeadline = AbsoluteTime[] + 1},
  runFake[timed]];
check["retry receives only the remaining parent time",
  Length[calls] === 2 && 0 < calls[[2, -1]] < calls[[1, -1]] < 1];
slow[args___] := (Pause[.2]; <|"Status" -> "COMPLETE"|>);
expired = Block[{ConstructiveDiagrams`Private`$highEnergyJetParentDeadline = AbsoluteTime[] + .04},
  runFake[slow]];
check["expired parent cannot return a partial pass", FailureQ[expired]];
directExpired = Internal`InheritedBlock[
  {ConstructiveDiagrams`Private`highEnergyLaurentJetCoefficientsNative},
  Clear[ConstructiveDiagrams`Private`highEnergyLaurentJetCoefficientsNative];
  ConstructiveDiagrams`Private`highEnergyLaurentJetCoefficientsNative[args___] := slow[args];
  ConstructiveDiagrams`Private`highEnergyLaurentJetCoefficients[1, variable, {}, {}, 2, 12, .04]];
check["direct typed-atom call retains its explicit small budget", FailureQ[directExpired]];
terminal = Internal`InheritedBlock[
  {ConstructiveDiagrams`Private`highEnergyLaurentJetEligibleQ,
   ConstructiveDiagrams`Private`highEnergyLaurentJetExpansion,
   ConstructiveDiagrams`Private`amplitudeCoherentPolynomialRules},
  Clear[ConstructiveDiagrams`Private`highEnergyLaurentJetEligibleQ,
    ConstructiveDiagrams`Private`highEnergyLaurentJetExpansion,
    ConstructiveDiagrams`Private`amplitudeCoherentPolynomialRules];
  ConstructiveDiagrams`Private`highEnergyLaurentJetEligibleQ[___] := True;
  ConstructiveDiagrams`Private`highEnergyLaurentJetExpansion[___] :=
    Failure["JetUncertifiedWholeArithmeticAssumption", <||>];
  ConstructiveDiagrams`Private`amplitudeCoherentPolynomialRules[___] := Throw["ForbiddenFallback"];
  Catch[ConstructiveDiagrams`Private`highEnergyScalarCoefficientSeries[1, variable, {spin}, 2]]];
check["uncertified whole assumption cannot fall through to scalar acceptance",
  AssociationQ[terminal] && Lookup[terminal, "Reason", None] === "UncertifiedScalarJetSeries"];
check["all modified system and native definitions restored",
  messageFields[] === messageBefore && Messages[Series] === seriesBefore &&
  DownValues[ConstructiveDiagrams`Private`highEnergyCertifiedScalarSeries] === scalarBefore &&
  DownValues[ConstructiveDiagrams`Private`highEnergyLaurentJetCoefficients] === entryBefore &&
  DownValues[ConstructiveDiagrams`Private`highEnergyLaurentJetCoefficientsNative] === nativeBefore];
Print["CHECKS: ", checks, "; FAILURES: ", Length[failures]];
If[failures =!= {}, Print[failures]; Quit[1]];
Print["HEPCAT_TEST_COMPLETED:high-energy-whole-warning-tests.wl"];
End[];
Quit[0];
