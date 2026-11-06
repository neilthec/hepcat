(* Odd derivative vertices must interfere correctly with fermion emission.
   Gauge invariance supplies a reference-free independent sign check. *)
root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Global`$HEPCATpath = FileNameJoin[{root, "source"}];
Get[FileNameJoin[{$HEPCATpath, "HEPCAT.wl"}]];
$HistoryLength = 0; checks = 0; failures = {};
SetAttributes[check, HoldRest];
check[name_, value_] := (checks++;
  If[!TrueQ[value], AppendTo[failures, name]; Print["FAIL: ", name]]);
vcRelabel = ConstructiveDiagrams`Private`feynmanRelabelVertex;
vcColor = ConstructiveDiagrams`Private`feynmanVertexColor;
vcOrdered = Table[<|"Momentum" -> Mom[index + 10], "LorentzIndices" -> {vcMu[index]}|>, {index, 3}];
vcOdd = LorentzMomentum[Mom[1], LorentzIndex[2]] - LorentzMomentum[Mom[3], LorentzIndex[2]];
check["odd source momentum slots keep the attached field momentum signs",
  Expand[vcRelabel[vcOdd, vcOrdered] -
    (LorentzMomentum[Mom[11], vcMu[2]] - LorentzMomentum[Mom[13], vcMu[2]])] === 0];
vcInternal = ReplacePart[vcOrdered, {1, "Momentum"} -> -Mom[Multiparticle[2, 5]]];
check["an inward internal field momentum retains its explicitly assigned sign",
  vcRelabel[LorentzMomentum[Mom[1], LorentzIndex[2]], vcInternal] ===
    -LorentzMomentum[Mom[Multiparticle[2, 5]], vcMu[2]]];
check["scalar momentum products preserve their ordinary bilinear signs",
  Expand[vcRelabel[MomProd[1, 3], vcInternal] + MomProd[2, 13] + MomProd[5, 13]] === 0];
(* The amplitude model includes both photon helicities for chart metadata. *)
vcSM = ReadModel[FileNameJoin[{root, "models"}], 1];
vcFW = ReadModel[FileNameJoin[{root, "model_feynman"}], 1];
vcTripleRow = SelectFirst[vcFW[[2]], Take[#, 4] === {"W+", "W-", "Z", ""} &];
vcTripleExpected = Last[vcTripleRow] /. {
  Mom[index_Integer] :> Mom[index + 10], LorentzIndex[index_Integer] :> vcMu[index]};
check["the complete imported three-vector tensor follows its source field ordering",
  Expand[vcRelabel[Last[vcTripleRow], vcOrdered] - vcTripleExpected] === 0];
vcColored = Table[<|"Particle" -> "g", "ColorIndex" -> ColorIndex[index]|>, {index, 3}];
vcColorData = <|"Color" -> <|"g" -> 8|>|>;
vcNewColor = vcColor[vcColored, vcColorData];
check["the imported adjoint tensor has CalcHEP's documented minus-i phase",
  vcNewColor === -I ColorStructure[ColorIndex[1], ColorIndex[2], ColorIndex[3]]];
check["the paired convention correction leaves every linear three-gluon vertex unchanged",
  Expand[vcNewColor vcRelabel[Last[vcTripleRow], vcOrdered] -
    (I ColorStructure[ColorIndex[1], ColorIndex[2], ColorIndex[3]])
      (-vcTripleExpected)] === 0];
vcOverrides = {"Mu" -> 4, "Md" -> 7, "Me" -> 4, "MW" -> 20, "SW" -> 3/5, "EE" -> 1/3};
vcRules = ConstructiveDiagrams`Private`residueParameterRules[vcSM, vcOverrides];
vcFeynmanRules = ConstructiveDiagrams`Private`residueParameterRules[List @@ vcFW, vcOverrides];
vcOriginalModel = vcFW;
Do[
  vcProcess = case;
  vcChannels = FactorizationAmplitudeChannels[vcProcess, vcSM];
  vcExternal = Thread[Mass /@ Range[4] ->
    (ConstructiveDiagrams`Private`physicalParticleMass[#, vcSM[[1]]] & /@ vcProcess)];
  vcTargets = ConstructiveDiagrams`Private`completionTargets[
    ConstructiveDiagrams`Private`residueSimpleInputs[{vcExternal, 0}, vcChannels, vcSM]["Targets"]];
  vcDiagrams = CreateDiagrams[vcProcess, vcFW];
  vcRaw = Total[Last /@ DiagramAmplitudes[vcDiagrams]];
  vcPhotonWard = vcRaw /. PolarizationVector[4, index_, 0, _] :> LorentzMomentum[Mom[4], index];
  vcConverted = FeynmanToSpinorProducts[vcPhotonWard,
    "ReduceSpinorProducts" -> False, "TimeConstraint" -> 30];
  check[{"real charged process has every charged-line photon attachment", vcProcess},
    Length[vcDiagrams] === If[MemberQ[vcProcess, "ne" | "Ne"], 2, 3] && !FailureQ[vcConverted]];
  Do[
    vcChart = ConstructiveDiagrams`Private`amplitudeRationalComparisonChart[
      vcTargets, sample, 202611061001, vcRules];
    vcValue = ConstructiveDiagrams`Private`residueEvaluateChart[
      vcConverted /. _ColorDelta -> 1, vcChart, vcFeynmanRules];
    vcCoefficients = CoefficientRules[Expand[vcValue], vcChart["SpinVariables"]];
    check[{"photon Ward identity vanishes exactly for every massive spin", vcProcess, sample},
      AssociationQ[vcChart] && And @@ (Together[Last[#]] === 0 & /@ vcCoefficients)],
    {sample, 2}],
  {case, {{"U", "d", "W+", "A.+"}, {"D", "u", "W-", "A.-"},
    {"E", "ne", "W-", "A.+"}}}];
check["the independent Feynman model remains unchanged",
  vcFW === vcOriginalModel];
check["no numerical mass or coupling is assigned to a global parameter",
  OwnValues[vcMu] === {} && Names["Global`MW"] === {} && Names["Global`EE"] === {}];
Print[checks, " Feynman vertex-convention checks; ", Length[failures], " failures."];
If[failures =!= {}, Print[failures]; Quit[1]];
Print["HEPCAT_TEST_COMPLETED:feynman-vertex-convention-tests.wl"]; Quit[0];
