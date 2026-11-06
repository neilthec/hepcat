(* Complete external currents must simplify without assuming a pole on shell. *)
root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Get[FileNameJoin[{root, "source", "HEPCAT.wl"}]];
$HistoryLength = 0; checks = 0; failures = {};
SetAttributes[check, HoldRest];
check[name_, assertion_] := (checks++; If[!TrueQ[assertion],
  AppendTo[failures, name]; Print["FAIL: ", name]]);
ccRules = {Mass[1] -> ccM1, Mass[2] -> ccM2, Mass[3] -> ccM3, Mass[4] -> ccM4};
ccA[i_] := Spinor["Spin", "Angle", i];
ccS[i_] := Spinor["Spin", "Square", i];
ccBracketA[i_, j_] := SpinorChain[ccA[i], ccA[j]];
ccBracketS[i_, j_] := SpinorChain[ccS[i], ccS[j]];
ccX[i_, k_, j_] := SpinorChain[ccS[i], Mom[k], ccA[j]];
(* The two descriptions of each current use P=p1+p2=-(p3+p4).
   No identity sets P^2 to the propagator mass. All four masses are distinct. *)
ccU = ccX[1, 2, 4] + ccM1 ccBracketA[1, 4];
ccOtherU = -ccX[1, 3, 4] + ccM4 ccBracketS[1, 4];
ccV = ccX[3, 1, 2] + ccM2 ccBracketS[2, 3];
ccOtherV = -ccX[3, 4, 2] + ccM3 ccBracketA[2, 3];
ccPacket = Expand[ccU ccV - ccOtherU ccOtherV];
ccNumerator = ccM1 ccM2 ccBracketA[1, 2] ccBracketS[3, 4];
ccDenominator = PropDen[Mom[Multiparticle[1, 2]], ccMediatorMass, ccWidth];
ccInput = (ccNumerator + ccPacket)/ccDenominator;
ccResult = ConstructiveDiagrams`Private`rspChannelCurrentNormalization[ccInput, ccRules, True, 3];
check["a complete current packet cancels for arbitrary four external masses",
  ccResult["Status"] === "IMPROVED" &&
    Together[ccResult["Amplitude"] - ccNumerator/ccDenominator] === 0];
check["no momentum insertions or temporary vector escape a cancelled packet",
  FreeQ[ccResult["Amplitude"], _SpinorChain?(Length[#] > 2 &)] &&
    FreeQ[ccResult["Amplitude"], Mom[_Symbol]]];
check["the original massive width-bearing propagator is unchanged",
  DeleteDuplicates[Cases[ccResult["Amplitude"], _PropDen, Infinity]] === {ccDenominator}];
check["all accepted chains retain an exact vector residual and empty internal shells",
  ccResult["Route"] =!= {} && AllTrue[ccResult["Route"],
    TrueQ[#["ExactFormalReplay"]] && TrueQ[#["DenominatorPreserved"]] &&
      #["InternalOnShellConditions"] === {} &&
      #["ExactCoefficientCollectionResidual"] === 0 &&
      AllTrue[#["ChainRewrites"], #["ExactMomentumResidual"] === 0 &&
        #["InternalOnShellConditions"] === {} && TrueQ[#["ExternalDiracNormalization"]] &] &]];
ccDual = ccInput /. Spinor["Spin", type_, leg_] :>
  Spinor["Spin", If[type === "Angle", "Square", "Angle"], leg];
ccDualTarget = (ccNumerator/ccDenominator) /. Spinor["Spin", type_, leg_] :>
  Spinor["Spin", If[type === "Angle", "Square", "Angle"], leg];
ccDualResult = ConstructiveDiagrams`Private`rspChannelCurrentNormalization[ccDual, ccRules, True, 3];
check["the angle-square dual has the same ordinary reduction",
  Together[ccDualResult["Amplitude"] - ccDualTarget] === 0];
ccIndexed = ccInput /. Spinor["Spin", type_, leg_] :>
  Spinor["Spin", "Upper", type, leg, ccSpin[leg]];
ccIndexedTarget = (ccNumerator/ccDenominator) /. Spinor["Spin", type_, leg_] :>
  Spinor["Spin", "Upper", type, leg, ccSpin[leg]];
ccIndexedResult = ConstructiveDiagrams`Private`rspChannelCurrentNormalization[ccIndexed, ccRules, True, 3];
check["explicit fixed spin indices survive every endpoint equation",
  Together[ccIndexedResult["Amplitude"] - ccIndexedTarget] === 0];
ccCutRules = Append[ccRules, Mass[Multiparticle[1, 2]] -> ccForbiddenInternalShell];
ccCutResult = ConstructiveDiagrams`Private`rspChannelCurrentNormalization[ccInput, ccCutRules, True, 3];
check["an internal mass rule never licenses an internal shell",
  ccCutResult["Amplitude"] === ccResult["Amplitude"] &&
    FreeQ[ccCutResult, ccForbiddenInternalShell]];
ccDisabled = ConstructiveDiagrams`Private`rspChannelCurrentNormalization[ccInput, ccRules, False, 3];
check["without explicit total momentum conservation the original is retained",
  ccDisabled["Status"] === "UNCHANGED" && ccDisabled["Amplitude"] === ccInput];
ccNoTime = ConstructiveDiagrams`Private`rspChannelCurrentNormalization[ccInput, ccRules, True, 0];
check["a zero budget retains its original complete checkpoint",
  ccNoTime["Amplitude"] === ccInput && ccNoTime["Route"] === {}];
ccBadRules = Append[ccRules, Mass[2] -> ccDifferentMass];
check["conflicting external mass declarations decline the optional trial",
  ConstructiveDiagrams`Private`rspChannelCurrentNormalization[ccInput, ccBadRules, True, 3]["Amplitude"] === ccInput];
ccInverse = SpinorChain[ccS[1], Mom[2], ccA[4]]^-1;
ccInverseResult = ConstructiveDiagrams`Private`rspChannelCurrentNormalization[
  ccInverse ccInput, ccRules, True, 3];
check["inverse spinor factors are retained as unchanged denominators",
  Together[ccInverseResult["Amplitude"] - ccInverse ccNumerator/ccDenominator] === 0];
(* A separate exact dense external chart checks the nontrivial packet,
   independently of its common-vector normalization transcript. *)
ccIndependentProof = ConstructiveDiagrams`Private`completionHEZeroDirection[ccPacket, ccRules, {}, 10];
check["an independent generic external chart proves the original packet exactly",
  ccIndependentProof["Status"] === "PROVED_ZERO" &&
    ccIndependentProof["NumericalKinematicSamples"] === 0 &&
    ccIndependentProof["InternalOnShellConditions"] === {}];
Print["Checks: ", checks, "; failures: ", Length[failures]];
If[failures =!= {}, Print[failures]];
If[failures === {}, Print["HEPCAT_TEST_COMPLETED:channel-current-normalization-tests.wl"]];
Quit[If[failures === {}, 0, 1]];
