(* Scalar coefficient expansion must preserve the complete coherent-spin
   polynomial and the exact Laurent series, including small bad-growth terms. *)
root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Global`$HEPCATpath = FileNameJoin[{root, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
checks = 0; failures = {}; SetAttributes[check, HoldRest];
check[name_, value_] := (checks++; If[!TrueQ[value], AppendTo[failures, name]; Print["FAIL: ", name]]);
hesSeries = ConstructiveDiagrams`Private`highEnergyScalarCoefficientSeries;
hesExpressions = {
  (1 + hesZ1) (1 - hesZ2)/(hesT^2 (1 + hesT)) + hesZ2 hesT^3,
  (hesZ1^2 + hesZ2^4 + hesZ1 hesZ2)/(1 - hesT)^2,
  hesT^4 (hesZ1 - hesZ2)^2 + hesT^5 (1 + hesZ1)^3,
  hesT^4 hesZ1^2 + hesZ2/(10^80 hesT^7),
  ((hesT + 1) hesZ1 + (hesT^2 + 1) hesZ2)^4/(1 + hesT + hesT^2),
  0, 7 + hesT^3, hesZ1 hesZ2/hesT
};
Do[
  hesReport = hesSeries[hesExpression, hesT, {hesZ1, hesZ2}, 4];
  hesOriginal = Normal[Series[hesExpression, {hesT, 0, 4}]];
  check["every coefficient expansion completes " <> ToString[hesIndex], hesReport["Status"] === "COMPLETE"];
  check["scalar and whole polynomial Laurent expansions agree exactly " <> ToString[hesIndex],
    hesReport["Status"] === "COMPLETE" && Cancel[Together[hesReport["Series"] - hesOriginal]] === 0];
  check["complete coherent coverage is recorded " <> ToString[hesIndex],
    TrueQ[hesReport["Polynomial"]["FullCoefficientCoverage"]] &&
    TrueQ[hesReport["Polynomial"]["ReconstructionVerified"]] &&
    TrueQ[hesReport["Polynomial"]["ConstantCoefficientIncluded"]] &&
    hesReport["CompletedComponents"] === hesReport["ComponentCount"] &&
    hesReport["NumericalSpinSamples"] === 0],
  {hesIndex, Length[hesExpressions]}, {hesExpression, {hesExpressions[[hesIndex]]}}];
hesTiny = hesSeries[hesExpressions[[4]], hesT, {hesZ1, hesZ2}, 4];
check["a tiny but genuine growing spin component is preserved exactly",
  Coefficient[Coefficient[Expand[hesTiny["Series"] hesT^7], hesT, 0], hesZ2] === 1/10^80];
check["a spin-dependent inverse cannot claim complete coefficients",
  hesSeries[1/(1 + hesZ1 hesT), hesT, {hesZ1}, 4]["Status"] === "UNRESOLVED"];
check["a spin-dependent function cannot claim complete coefficients",
  hesSeries[Sin[hesZ1], hesT, {hesZ1}, 4]["Status"] === "UNRESOLVED"];
check["the helper does not assign values to the user's spin variables", OwnValues[hesZ1] === {} && OwnValues[hesZ2] === {}];
hesMessageDefinitions = DownValues[Message]; hesMessageAttributes = Attributes[Message];
hesZeroWarning /: Series[hesZeroWarning[value_], {variable_, 0, order_Integer}] :=
  (Message[Series::ztest1, System`HoldCompleteForm[
    Sqrt[2] + Sqrt[3] - Sqrt[5 + 2 Sqrt[6]]]];
   SeriesData[variable, 0, {}, order + 1, order + 1, 1]);
hesBadWarning /: Series[hesBadWarning[value_], {variable_, 0, order_Integer}] :=
  (Message[Series::ztest1, System`HoldCompleteForm[1/10^80]];
   SeriesData[variable, 0, {}, order + 1, order + 1, 1]);
hesOtherWarning /: Series[hesOtherWarning[value_], {variable_, 0, order_Integer}] :=
  (Message[Power::infy, 1/0]; SeriesData[variable, 0, {}, order + 1, order + 1, 1]);
hesFiveWarnings /: Series[hesFiveWarnings[value_], {variable_, 0, order_Integer}] :=
  (Do[Message[Series::ztest1, System`HoldCompleteForm[0]], {5}];
   SeriesData[variable, 0, {}, order + 1, order + 1, 1]);
hesPartialSeries /: Series[hesPartialSeries[value_], {variable_, 0, order_Integer}] :=
  SeriesData[variable, 0, {}, 2, 2, 1];
hesSlowSeries /: Series[hesSlowSeries[value_], {variable_, 0, order_Integer}] :=
  (Pause[0.2]; SeriesData[variable, 0, {}, order + 1, order + 1, 1]);
hesCertified = ConstructiveDiagrams`Private`highEnergyCertifiedScalarSeries[hesZeroWarning[hesT], hesT, 4];
check["an assumed algebraic zero needs an exact certificate", hesCertified["Status"] === "COMPLETE" &&
  Length[hesCertified["ZeroAssumptionCertificates"]] === 1 &&
  hesCertified["ZeroAssumptionCertificates"][[1]]["ExactValues"] === {0}];
check["a genuine tiny nonzero assumption is rejected",
  ConstructiveDiagrams`Private`highEnergyCertifiedScalarSeries[hesBadWarning[hesT], hesT, 4]["Status"] === "UNRESOLVED"];
check["other Series messages still reject the scalar expansion",
  ConstructiveDiagrams`Private`highEnergyCertifiedScalarSeries[hesOtherWarning[hesT], hesT, 4]["Status"] === "UNRESOLVED"];
hesQuietReport = Internal`InheritedBlock[{Series}, Off[Series::ztest, Series::ztest1]; Quiet[
  ConstructiveDiagrams`Private`highEnergyCertifiedScalarSeries[hesFiveWarnings[hesT], hesT, 4]]];
check["caller Quiet and Off cannot hide repeated zero assumptions", hesQuietReport["Status"] === "COMPLETE" &&
  Length[hesQuietReport["ZeroAssumptionCertificates"]] === 5];
check["a finite but insufficient series cannot claim complete coverage",
  ConstructiveDiagrams`Private`highEnergyCertifiedScalarSeries[hesPartialSeries[hesT], hesT, 4]["Status"] === "UNRESOLVED"];
hesInterrupted = TimeConstrained[
  ConstructiveDiagrams`Private`highEnergyCertifiedScalarSeries[hesSlowSeries[hesT], hesT, 4], 0.02, "INTERRUPTED"];
check["an interrupted scalar attempt remains interrupted", hesInterrupted === "INTERRUPTED"];
check["system Message definitions and attributes survive success and timeout",
  DownValues[Message] === hesMessageDefinitions && Attributes[Message] === hesMessageAttributes];
hesWholeSample[wrapped_] := Internal`InheritedBlock[{ConstructiveDiagrams`Private`residueEvaluateChart},
  Clear[ConstructiveDiagrams`Private`residueEvaluateChart];
  ConstructiveDiagrams`Private`residueEvaluateChart[expression_, chart_Association, rules_List] := wrapped;
  Internal`InheritedBlock[{Series}, Off[Series::ztest, Series::ztest1];
    ConstructiveDiagrams`Private`highEnergyOneSample[7,
      <|"PoleVariable" -> hesT, "SpinVariables" -> {}|>, {}, -2]]];
hesWholeGood = hesWholeSample[hesZeroWarning[hesT]];
check["whole-tensor fallback retains an exact zero-assumption certificate",
  hesWholeGood["Status"] === "PASS" &&
  Length[hesWholeGood["WholeExpansion"]["ZeroAssumptionCertificates"]] === 1];
check["disabled warnings cannot turn a genuine nonzero into a passing whole-tensor sample",
  hesWholeSample[hesBadWarning[hesT]]["Status"] === "UNRESOLVED"];
Print[checks, " high-energy coefficient-series checks; ", Length[failures], " failures."];
If[failures =!= {}, Print[failures]; Quit[1]];
Print["HEPCAT_TEST_COMPLETED:high-energy-coefficient-series-tests.wl"]; Quit[0];
