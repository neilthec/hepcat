(* Run in a fresh licensed kernel with HEPCAT_BATCH_BINARY set to the release CLI.
   HEPCAT_ROOT can point to the package when this bridge is in a staging directory. *)
$HistoryLength = 0;
nativeDirectory = DirectoryName[$InputFileName];
root = Replace[Environment["HEPCAT_ROOT"], Except[_String] :> DirectoryName[nativeDirectory]];
Get[FileNameJoin[{root, "source", "HEPCAT.wl"}]];
Get[FileNameJoin[{nativeDirectory, "NativeAmplitude.wl"}]];
binary = Environment["HEPCAT_BATCH_BINARY"];
checks = 0;
check[name_, assertion_] := If[TrueQ[assertion], checks++, Print["FAIL ", name]; Exit[1]];
check["batch executable", StringQ[binary] && FileExistsQ[binary]];

rules = Thread[(Mass /@ Range[4]) -> {1, 1, 0, 0}];
growth = Mandelstahm[1, 3];
seed = {rules, 7 + growth};
scalar = CompleteNativeHighEnergy[seed, {}, binary, "ReturnDetails" -> True, "Workers" -> 1];
check["scalar correction preserves the bounded constant", AssociationQ[scalar] &&
  scalar["Amplitude"] === {rules, 7} && scalar["Bound"] === 0 && scalar["Rank"] === 1 &&
  scalar["ValidationSamples"] > 0 && TrueQ[scalar["ExhaustiveSpin"]]];
inherited = <|"Stage" -> "Factorization", "Status" -> "COMPLETE", "Amplitude" -> Last[seed],
  "ExternalMassRules" -> rules, "RemainingBasis" -> {growth, 1}|>;
familyFit = CompleteNativeHighEnergy[inherited, {}, binary, "ReturnDetails" -> True];
check["local directions from a detailed Wolfram fit", AssociationQ[familyFit] &&
  familyFit["Route"] === "ContactBasis" && familyFit["Amplitude"] === {rules, 7} &&
  familyFit["FreeCoefficients"] === 1];
check["unfinished factorization report rejected", MatchQ[CompleteNativeHighEnergy[
  Join[inherited, <|"Status" -> "UNRESOLVED"|>], {}, binary], Failure["NativeFactorization", _]]];
check["override the growth bound", CompleteNativeHighEnergy[seed, {}, binary, "HighEnergyBound" -> 2] === seed];

symbolicSeed = {rules, g (7 + growth)};
points = {{g -> 2/3}, {g -> 5/7}};
symbolic = CompleteNativeHighEnergy[symbolicSeed, {}, binary,
  "ParameterPoints" -> points, "ReturnDetails" -> True];
check["exact explicit parameter points preserve the symbolic answer", AssociationQ[symbolic] &&
  symbolic["ParameterPoints"] === points && symbolic["Amplitude"] === {rules, 7 g}];

(* A synthetic scalar exchange checks both seed overloads with the ordinary
   HEPCAT sewing sign. The full native pipeline must retain its physical pole. *)
cut = Multiparticle[1, 3];
pole = PropDen[Mom[cut], 1];
channel = Join[Last /@ rules, {cut, 1, -1/(ConstructiveDiagrams`z pole)}];
batch = ReduceNativeAmplitude[{{rules, 7}}, {channel}, binary];
check["scalar reduction", AssociationQ[batch]];
factor = CompleteNativeFactorization[batch, binary, "ReturnDetails" -> True];
check["physical exchange actually reconstructed", AssociationQ[factor] &&
  factor["Amplitude"] === {rules, 7 + 1/pole}];
check["prepared batch runs both native stages", CompleteNativeHighEnergy[batch, {}, binary] === factor["Amplitude"]];
check["factorization report and retained batch", CompleteNativeHighEnergy[factor, batch, {}, binary] === factor["Amplitude"]];
withGrowth = {rules, Last[factor["Amplitude"]] + growth};
check["supplied seed used with retained data", CompleteNativeHighEnergy[withGrowth, batch, {}, binary] === factor["Amplitude"]];

check["reject internal masses on final seed", MatchQ[CompleteNativeHighEnergy[
  {Append[rules, Mass[cut] -> 1], 4}, {}, binary], Failure["NativeSeed", _]]];
check["reject a missing aligned remainder", MatchQ[CompleteNativeHighEnergy[
  seed, Join[batch, <|"FactorizationRemainders" -> {}|>], {}, binary], Failure["NativeRows", _]]];
check["reject a noninteger energy bound", MatchQ[CompleteNativeHighEnergy[
  seed, {}, binary, "HighEnergyBound" -> 1/2], Failure["NativeBound", _]]];
check["reject inexact chart parameters", MatchQ[CompleteNativeHighEnergy[
  symbolicSeed, {}, binary, "ParameterPoints" -> {{g -> 0.5}}], Failure["NativeParameters", _]]];
check["reject pole-bearing correction", FailureQ[CompleteNativeHighEnergy[
  seed, {}, binary, "CorrectionBasis" -> {1/pole}]]];
check["reject missing external masses", FailureQ[CompleteNativeHighEnergy[
  {Delete[rules, 2], growth}, {}, binary]]];

qed = ReadModel[FileNameJoin[{root, "models"}], 4];
process = {"e", "E", "A.+", "A.+"};
channels = FactorizationAmplitudeChannels[process, qed];
check["two physical QED channels", ListQ[channels] && Length[channels] === 2];
entry = SelectFirst[qed[[2]], DeleteCases[Drop[#, -2], ""] === process &];
check["archived four-point process", ListQ[entry]];
qedSeed = {Thread[(Mass /@ Range[4]) -> Drop[First[channels], -3]], Times @@ Take[entry, -2]};
qedNative = CompleteNativeHighEnergy[qedSeed, qed, binary, "ReturnDetails" -> True];
check["QED seed-only native route with automatic model parameters", AssociationQ[qedNative] &&
  qedNative["Route"] === "Unchanged" && qedNative["Rank"] === 0 && qedNative["ValidationSamples"] > 0 &&
  Length[qedNative["ParameterPoints"]] >= 2 && TrueQ[qedNative["ExhaustiveSpin"]]];
points = {{"Me" -> 3/2, "EE" -> 2/5}, {"Me" -> 5/2, "EE" -> 3/7}};
checked = CompleteNativeHighEnergy[qedSeed, channels, qed, binary, "ParameterPoints" -> points,
  "ReturnDetails" -> True];
check["independent checks run for an unchanged seed without a supplied basis",
  AssociationQ[checked] && checked["Amplitude"] === qedNative["Amplitude"] &&
    checked["ValidationReport"]["PhysicalSummary"] === <|"PASS" -> 2|> &&
    checked["ValidationReport"]["HighEnergySummary"] === <|"PASS" -> 1|> &&
    checked["ValidationReport"]["ReferenceSummary"] === <|"PASS" -> 1|> &&
    Keys[checked["ValidationReport"]["Summary"]] === {"PASS"}];
wrong = {First[qedSeed], 2 Last[qedSeed]};
wrongNative = CompleteNativeHighEnergy[wrong, qed, binary, "ParameterPoints" -> points, "ReturnDetails" -> True];
check["growth alone accepts the wrong normalization", AssociationQ[wrongNative] && wrongNative["Route"] === "Unchanged"];
rejected = CompleteNativeHighEnergy[wrong, channels, qed, binary, "ParameterPoints" -> points];
check["the channel-checked overload rejects both wrong residues with no correction directions",
  MatchQ[rejected, Failure["IncompleteAmplitude", _]] &&
    rejected[[2, "ValidationReport", "PhysicalSummary"]] === <|"FAIL" -> 2|> &&
    rejected[[2, "ValidationReport", "HighEnergySummary"]] === <|"PASS" -> 1|>];
check["reject unknown model parameter names", MatchQ[CompleteNativeHighEnergy[
  qedSeed, qed, binary, "ParameterPoints" -> {{"NeverDeclared" -> 2}}], Failure["NativeParameters", _]]];
Print[checks, " native high-energy bridge checks passed."];
