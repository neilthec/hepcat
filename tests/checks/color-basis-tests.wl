(* Exact color checks preserve coherent interference, independent of spinors. *)
root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Get[FileNameJoin[{root, "source", "HEPCAT.wl"}]];
Get[FileNameJoin[{root, "source", "HEPCAT-Color.wl"}]];
$HistoryLength = 0;
checks = 0; failures = {};
SetAttributes[check, HoldRest];
check[name_, assertion_] := (checks++; If[!TrueQ[assertion],
  AppendTo[failures, name]; Print["FAIL: ", name]]);
cb[tensors_, representations_, rest___] := ConstructiveDiagrams`Private`colorTensorBasis[tensors, representations, rest];
ci[n_] := ColorIndex[n];
internal[n_, dimension_] := ConstructiveDiagrams`Private`colorInternalIndex[n, dimension];
model = ReadModel[FileNameJoin[{root, "models"}], 1];
before = model;
t = ConstructiveDiagrams`Private`colorSU3Generators[];
f = ConstructiveDiagrams`Private`colorSU3StructureConstants[];
check["fundamental generators have the declared trace normalization",
  Table[Tr[t[[a]].t[[b]]], {a, 8}, {b, 8}] === IdentityMatrix[8]/2];
check["the adjoint phase is fixed by the fundamental commutator",
  And @@ Flatten[Table[TrueQ[Simplify[t[[a]].t[[b]] - t[[b]].t[[a]] -
      I Sum[f[[a, b, c]] t[[c]], {c, 8}]] === ConstantArray[0, {3, 3}]], {a, 8}, {b, 8}]]];
check["a fundamental singlet tensor sums to three colors",
  cb[{ColorDelta[ci[1], ci[2]]}, {3, -3}]["GramMatrix"] === {{3}}];
d1 = ColorDelta[ci[1], ci[2]] ColorDelta[ci[3], ci[4]];
d2 = ColorDelta[ci[1], ci[4]] ColorDelta[ci[3], ci[2]];
check["four-quark singlet pairings retain their nonzero interference",
  cb[{d1, d2}, {3, -3, 3, -3}]["GramMatrix"] === {{9, 3}, {3, 9}}];
g1 = ColorGenerator[ci[2], ci[1], internal[1, 8]] ColorGenerator[ci[4], ci[3], internal[1, 8]];
g2 = ColorGenerator[ci[4], ci[1], internal[2, 8]] ColorGenerator[ci[2], ci[3], internal[2, 8]];
fourQuarks = cb[{d1, d2, g1, g2}, {3, -3, 3, -3}];
check["the fundamental completeness identity is reproduced exactly",
  fourQuarks["DiagramCoordinates"] === {{1, 0}, {0, 1}, {-1/6, 1/2}, {1/2, -1/6}}];
check["crossed gluon exchanges have the negative interference required by SU(3)",
  fourQuarks["DiagramGramMatrix"][[{3, 4}, {3, 4}]] === {{2, -2/3}, {-2/3, 2}}];
q34 = ColorGenerator[ci[2], internal[1, 3], ci[3]] ColorGenerator[internal[1, 3], ci[1], ci[4]];
q43 = ColorGenerator[ci[2], internal[1, 3], ci[4]] ColorGenerator[internal[1, 3], ci[1], ci[3]];
qf = I ColorStructure[ci[3], ci[4], internal[2, 8]] ColorGenerator[ci[2], ci[1], internal[2, 8]];
quarkGluons = cb[{q34, q43, qf}, {3, -3, 8, 8}];
check["quark-gluon ordered products span the commutator channel",
  quarkGluons["DiagramCoordinates"] === {{1, 0}, {0, 1}, {1, -1}}];
check["quark-gluon squared color factors and their interference match the paper",
  quarkGluons["DiagramGramMatrix"] === {{16/3, -2/3, 6}, {-2/3, 16/3, -6}, {6, -6, 12}}];
check["bare Feynman color dummies inherit their unique generator representation",
  cb[{q34 /. internal[1, 3] -> cbDummy}, {3, -3, 8, 8}]["GramMatrix"] === {{16/3}}];
check["an unpaired or dimensionally inconsistent color dummy is rejected",
  FailureQ[cb[{ColorGenerator[ci[2], ci[1], cbDummy]}, {3, -3}]] &&
    FailureQ[cb[{ColorGenerator[ci[2], ci[1], internal[1, 3]]}, {3, -3}]]];
s = -ColorStructure[ci[1], ci[2], internal[1, 8]] ColorStructure[internal[1, 8], ci[3], ci[4]];
u = -ColorStructure[ci[2], ci[3], internal[1, 8]] ColorStructure[internal[1, 8], ci[1], ci[4]];
v = -ColorStructure[ci[3], ci[1], internal[1, 8]] ColorStructure[internal[1, 8], ci[2], ci[4]];
gluons = cb[{s, u, v}, {8, 8, 8, 8}];
check["four-adjoint contractions obey Jacobi and have two independent tensors",
  Length[gluons["Basis"]] === 2 && Total[gluons["DiagramCoordinates"]] === {0, 0} &&
    Diagonal[gluons["DiagramGramMatrix"]] === {72, 72, 72}];
complex = cb[{d1, I d1}, {3, -3, 3, -3}];
check["complex relative color phases are preserved rather than squared separately",
  complex["DiagramCoordinates"] === {{1}, {I}} &&
    complex["DiagramGramMatrix"] === {{9, 9 I}, {-9 I, 9}}];
check["inexact coefficients cannot claim an exact color certificate",
  FailureQ[cb[{0.5 d1}, {3, -3, 3, -3}]]];
check["unsupported representations and excessive color spaces fail explicitly",
  FailureQ[cb[{1}, {6, -6}]] && FailureQ[cb[{s}, {8, 8, 8, 8}, 100]]];
Do[
  diagrams = CreateDiagrams[process, model];
  basis = ConstructiveDiagrams`Private`colorDiagramBasis[diagrams, model];
  check["generated color basis is complete: " <> ToString[process, InputForm],
    AssociationQ[basis] && Length[basis["Basis"]] === 2 &&
      Length[basis["DiagramCoordinates"]] === Length[basis["Diagrams"]] &&
      Length[basis["DiagramOrigins"]] === Length[basis["Diagrams"]]],
  {process, {{"u", "U", "c", "C"}, {"u", "U", "G.+", "G.-"}, {"G.+", "G.+", "G.-", "G.-"}}}];
ambiguous = model;
ambiguous[[1]] = Append[ambiguous[[1]], {"adjoint scalar", "X", "X", 999, 0, 1, 0, 8, "", "X", "X"}];
ports = Table[<|"Particle" -> "X", "ColorIndex" -> ci[i]|>, {i, 3}];
check["three adjoints without an antisymmetric kinematic certificate remain ambiguous",
  MatchQ[ConstructiveDiagrams`Private`colorVertexTensor[ports, ambiguous, {"X", "X", "X", "", 1, 1}],
    Failure["AmbiguousColorVertex", _Association]]];
check["an unspecified four-adjoint interaction is never invented",
  FailureQ[ConstructiveDiagrams`Private`colorVertexTensor[Append[ports,
    <|"Particle" -> "X", "ColorIndex" -> ci[4]|>], ambiguous, {"X", "X", "X", "X", 1, 1}]]];
check["color analysis never modifies the caller's model", model === before];
colorParticles = {"u", "U", "A.+", "A.+"};
colorDiagrams = CreateDiagrams[colorParticles, model];
colorPairs = DiagramAmplitudes[colorDiagrams];
colorChannels = FactorizationAmplitudeChannels[colorParticles, model];
check["public diagram amplitudes retain explicit coherent color tensors",
  ListQ[colorPairs] && Length[colorPairs] === 2 &&
    !FreeQ[colorPairs, _ColorDelta] && FreeQ[colorPairs, _Failure]];
check["public factorization sews the same color indices before spinor reduction",
  ListQ[colorChannels] && Length[colorChannels] === 2 &&
    !FreeQ[colorChannels, _ColorDelta] && FreeQ[colorChannels, _Failure]];
colorProjection = ConstructiveDiagrams`Private`colorProjectionData[colorPairs,
  colorChannels, {3,-3,1,1}];
check["the public quark-photon sources share one exact color direction",
  AssociationQ[colorProjection] && colorProjection["GramMatrix"] === {{3}} &&
    FreeQ[{colorProjection["ProjectedDiagrams"], colorProjection["ProjectedChannels"]},
      _ColorDelta | _ColorGenerator | _ColorStructure]];
Do[
  currentProcess=currentSpec[[1]];
  currentDiagrams=CreateDiagrams[currentProcess,model];
  currentPairs=DiagramAmplitudes[currentDiagrams];
  currentChannels=FactorizationAmplitudeChannels[currentProcess,model];
  currentReps=ConstructiveDiagrams`Private`colorParticleRepresentation[#,model]& /@ currentProcess;
  currentProjection=ConstructiveDiagrams`Private`colorProjectionData[
    currentPairs,currentChannels,currentReps];
  currentCertificate=ConstructiveDiagrams`Private`colorScalarCurrentProjectionCertificate[
    currentDiagrams,model,currentProjection,currentReps];
  check["scalar/vector current color flow is proved exactly: "<>ToString[currentProcess,InputForm],
    AssociationQ[currentCertificate] && TrueQ[currentCertificate["Certified"]] &&
      Head[currentCertificate["CanonicalTensor"]]===currentSpec[[2]] &&
      currentCertificate["CommonDiagramCoordinate"]===1 &&
      currentCertificate["DiagramCoordinates"]===ConstantArray[1,Length[currentDiagrams]]],
  {currentSpec,{{{"u","U","A.+","h"},ColorDelta},
    {{"h","G.+","U","u"},ColorGenerator}}}];
check["a current color certificate cannot strip a nonunique tensor space",
  MissingQ[ConstructiveDiagrams`Private`colorScalarCurrentProjectionCertificate[
    currentDiagrams,model,Join[currentProjection,<|"Basis"->{1,2}|>],currentReps]]];
currentRephased=ConstructiveDiagrams`Private`colorScalarCurrentProjectionCertificate[
  currentDiagrams,model,Join[currentProjection,<|"Basis"->I currentProjection["Basis"]|>],currentReps];
check["current color projection keeps a nontrivial exact basis phase",
  AssociationQ[currentRephased] && currentRephased["CommonDiagramCoordinate"]===-I &&
    currentRephased["CanonicalCoordinate"]===-I];
check["singular color amplitude retains every internally summed contribution",
  And @@ MapThread[TrueQ[Expand[#1 - Last[#2]] === 0] &,
    {diagramAmplitude /@ colorDiagrams, colorPairs}]];
zeroTree = ConstructAmplitude[ConstantArray["G.+",4],model,model,
  "Output"->"Details","PrintReport"->False,"ProgressReporting"->False];
check["a source-forbidden colored tree is checked as zero without an invented basis",
  AssociationQ[zeroTree] && zeroTree["Status"]==="COMPLETE" &&
    zeroTree["Amplitude"]===0 && FreeQ[zeroTree["Checks"]["Summary"],"FAIL"|"UNRESOLVED"]];
limitedColor = ConstructAmplitude[colorParticles,model,model,
  "TimeConstraint"->10^-6,"PrintReport"->False,"ProgressReporting"->False];
check["an unfinished color coefficient never becomes an assembled successful amplitude",
  FailureQ[limitedColor] && FreeQ[limitedColor,_Dot|Missing["NotValidated"]]];
(* Probe only the shared-clock bridge. The deliberate child failure prevents
   this test from manufacturing a physical amplitude or repeating an SM fit. *)
cbClockModel=ReadModel[FileNameJoin[{root,"models"}],1];
cbClockProcess={"u","U","W-","W+"};
cbClockSettings=Join[Association[Options[ConstructAmplitude]],
 <|"Output"->"Details","PrintReport"->False,"ProgressReporting"->False|>];
cbGenericClock=ConstructiveDiagrams`Private`amplitudeAutomaticSettings["Construction",cbClockSettings,
 ConstructiveDiagrams`Private`amplitudeResourceMetrics[{}, {},cbClockProcess]]["Settings"]["TimeConstraint"];
cbModelClock=ConstructiveDiagrams`Private`amplitudeAutomaticSettings["Construction",cbClockSettings,
 ConstructiveDiagrams`Private`amplitudeConstructionResourceMetrics[cbClockProcess,cbClockModel]]["Settings"]["TimeConstraint"];
cbProbeClock[settings_]:=Internal`InheritedBlock[{ConstructiveDiagrams`ConstructAmplitude},
 cbReceivedClock=Missing["NoChild"];
 Unprotect[ConstructiveDiagrams`ConstructAmplitude];
 Clear[ConstructiveDiagrams`ConstructAmplitude];
 ConstructiveDiagrams`ConstructAmplitude[_,_,_,probeOptions___Rule]:=(
  cbReceivedClock=Lookup[Association[{probeOptions}],"TimeConstraint"];
  Failure["DeliberatelyStoppedClockProbe",<||>]);
 Protect[ConstructiveDiagrams`ConstructAmplitude];
 ConstructiveDiagrams`Private`colorConstructAmplitude[cbClockProcess,cbClockModel,cbClockModel,settings];
 cbReceivedClock];
cbAutomaticChildClock=cbProbeClock[cbClockSettings];
check["colored construction forwards its model-aware massive-spin allowance",
 NumericQ[cbAutomaticChildClock] && cbModelClock>cbGenericClock &&
 cbAutomaticChildClock>cbGenericClock && cbAutomaticChildClock<=cbModelClock];
cbExplicitChildClock=cbProbeClock[Join[cbClockSettings,<|"TimeConstraint"->10|>]];
check["colored construction keeps an explicit shared clock authoritative",
 NumericQ[cbExplicitChildClock] && 0<cbExplicitChildClock<=10];
Print["COLOR BASIS CHECKS: ", checks, "; FAILURES: ", Length[failures]];
If[failures === {}, Print["HEPCAT_TEST_COMPLETED:color-basis-tests.wl"], Print[failures]];
Quit[If[failures === {}, 0, 1]];
