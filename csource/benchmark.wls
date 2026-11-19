(* HEPCAT C++ reducer benchmark. Each timed Wolfram evaluation uses its own kernel.
   Set HEPCAT_ROOT, HEPCAT_BENCH_DIR, HEPCAT_BENCH_CASE and HEPCAT_BENCH_MODE. *)
$HistoryLength = 0;
sourceRoot = Replace[Environment["HEPCAT_ROOT"], Except[_String] :> DirectoryName[DirectoryName[$InputFileName]]];
Get[FileNameJoin[{sourceRoot, "source", "HEPCAT.wl"}]];
workDir = Environment["HEPCAT_BENCH_DIR"];
case = Environment["HEPCAT_BENCH_CASE"];
mode = Environment["HEPCAT_BENCH_MODE"];
fail[msg_] := (Print["ERROR: ", msg]; Exit[2]);
If[!DirectoryQ[workDir] || !MemberQ[{"qed", "wwww"}, case], fail["Invalid benchmark directory or case"]];
path[s_] := FileNameJoin[{workDir, case <> "-" <> s}];
process = If[case === "qed", {"e", "E", "A.+", "A.+"}, {"W+", "W+", "W-", "W-"}];
model = ReadModel[FileNameJoin[{sourceRoot, "models"}], If[case === "qed", 3, 1]];

(* Encode the supported expression tree, preserving model-owned parameters verbatim. *)
wire[z_Integer] := ToString[z, InputForm];
wire[z_String] := ToString[z, InputForm, PageWidth -> Infinity];
wire[z_Symbol] := SymbolName[z];
wire[z_Rational] := "Rational[" <> wire[Numerator[z]] <> "," <> wire[Denominator[z]] <> "]";
wire[z_] := SymbolName[Head[z]] <> "[" <> StringRiffle[wire /@ (List @@ z), ","] <> "]";

(* Fix the two QED reference spinors before either reducer sees the expression.
   Each Xi in a polarization denominator is paired with its photon. Use the other
   photon as reference. Both graphs then use the same reference for each photon. *)
referenceRules[expr_] := Cases[expr,
  Power[SpinorChain[x : Spinor["Xi", "Angle", _], Spinor["Helicity", "Angle", i_Integer]], -1] :>
    (x -> Spinor["Helicity", "Angle", First[Complement[{3, 4}, {i}]]]), Infinity];
makeFixture[] := Module[{diagrams, pairs, terms, expr, masses, external, data},
  diagrams = CreateDiagrams[process, model];
  pairs = DiagramAmplitudes[diagrams];
  If[Length[pairs] =!= If[case === "qed", 2, 6], fail["Unexpected diagram count"]];
  (* Resolve explicit scalar mass tags in each contracted diagram using its own
     mass list. The QED electron exchanges contain Mass[Multiparticle[1,3]]
     or Mass[Multiparticle[1,4]] = Me. The two WWWW Z exchanges carry MZ.
     No internal condition is used on momentum products or pole denominators. *)
  terms = Function[pair, Module[{value = ReduceSpinContractions[Last[pair]] /. First[pair]},
    If[case === "qed", value /. referenceRules[value], value]]] /@ pairs;
  expr = Total[terms];
  If[!FreeQ[expr, Spinor["Xi", __] | _Mass], fail["Unresolved input mass or reference"]];
  masses = ConstructiveDiagrams`Private`constructExternalMasses[process, model];
  external = Thread[(Mass /@ Range[Length[masses]]) -> masses];
  data = <|"Process" -> process, "DiagramCount" -> Length[pairs], "Masses" -> masses,
    "MassRules" -> external, "Expression" -> expr|>;
  Export[path["input.wxf"], data, "WXF"];
  Export[path["input.txt"], StringRiffle[wire /@ {Length[masses], masses, {}, expr}, "\n"] <> "\n", "Text"];
  If[ToExpression[wire[expr], InputForm] =!= expr, fail["Wire encoding changed input"]];
  Print[{case, Length[pairs], LeafCount[expr], SpinorExpressionComplexity[expr]}]
];
If[mode === "generate", makeFixture[]; Exit[0]];
data = Import[path["input.wxf"], "WXF"];
input = data["Expression"]; ext = data["MassRules"];
phase = Environment["HEPCAT_BENCH_PHASE"];
barrier[signal_, reply_] := If[StringQ[phase] && phase =!= "",
  CreateFile[phase <> "." <> signal];
  If[TimeConstrained[While[!FileExistsQ[phase <> "." <> reply], Pause[0.001]]; True, 60, False] =!= True,
    fail["Benchmark controller acknowledgement timed out"]]];
If[MemberQ[{"reduce", "prepare"}, mode],
  barrier["ready", "go"];
  seconds = First[AbsoluteTiming[
    peak = MaxMemoryUsed[answer = If[mode === "reduce",
      ReduceSpinorProducts[input],
      PrepareAmplitudes[{ext, input}, model, "ExternalParticles" -> process, "InternalOnShell" -> False,
        "Parallelization" -> False, "ProgressReporting" -> False, "Output" -> "Expression"]]];]];
  barrier["done", "ack"];
  If[!FreeQ[answer, $Failed | _Failure | _PrepareAmplitudes | _ReduceSpinorProducts], fail["Wolfram reduction failed"]];
  Export[path[mode <> ".wxf"], answer, "WXF"];
  Export[path[mode <> ".json"], <|"Case" -> case, "Method" -> mode, "Seconds" -> seconds,
    "WolframEvaluationPeakAdditionalBytes" -> peak, "InputComplexity" -> SpinorExpressionComplexity[input],
    "OutputComplexity" -> SpinorExpressionComplexity[answer], "InputLeaves" -> LeafCount[input],
    "OutputLeaves" -> LeafCount[answer], "Version" -> $Version|>, "RawJSON"];
  Exit[0]
];
If[mode =!= "validate", fail["Unknown benchmark mode"]];
(* Evaluate independent HEPCAT spin frames. Each spin remains a variable, so a
   single comparison checks all coherent spin components at that momentum point. *)
native = ToExpression[Import[path["native.txt"], "Text"], InputForm];
representatives = <|"Input" -> input, "C++" -> native,
  "ReduceSpinorProducts" -> Import[path["reduce.wxf"], "WXF"],
  "PrepareAmplitudes" -> Import[path["prepare.wxf"], "WXF"]|>;
repeatFiles = Join[
  FileNames[RegularExpression[case <> "-reduce-[0-9]+\\.wxf"], workDir],
  FileNames[RegularExpression[case <> "-prepare-[0-9]+\\.wxf"], workDir],
  FileNames[RegularExpression[case <> "-native-[0-9]+\\.txt"], workDir]];
Do[AssociateTo[representatives, FileBaseName[file] ->
   If[FileExtension[file] === "wxf", Import[file, "WXF"],
      ToExpression[Import[file, "Text"], InputForm]]], {file, repeatFiles}];
If[!FreeQ[Values[representatives], $Failed | _Failure | _Missing | Indeterminate], fail["Missing reducer output"]];
assignments = If[case === "qed", {{"Me" -> 2, "EE" -> 1/3}, {"Me" -> 3, "EE" -> 2/7}},
  {{"MW" -> 20, "SW" -> 3/5, "EE" -> 1/3, "Mh" -> 30},
   {"MW" -> 21, "SW" -> 5/13, "EE" -> 2/7, "Mh" -> 32}}];
points = {{3, 5, 7}, {5, 7, 11}, {7, 11, 13}};
checkAt[parameters_, point_] := Module[{chart, coords, spins, sampled, poles, den, polys, coeff, degrees, clean},
  chart = ConstructiveDiagrams`Private`completionHESymbolicExternalChart[ext /. parameters, Automatic, Automatic];
  If[!AssociationQ[chart] || !TrueQ[chart["ExternalMassShellsValidated"]] ||
    !TrueQ[chart["MomentumConservationValidated"]] || chart["InternalOnShellConditions"] =!= {}, fail["Invalid check chart"]];
  coords = chart["ExternalIdentityCoordinates"]; spins = chart["SpinVariables"];
  If[Length[point] =!= Length[coords] || Length[spins] =!= If[case === "qed", 2, 4], fail["Invalid spin or coordinate count"]];
  sampled = chart /. Thread[coords -> point];
  poles = DeleteDuplicates[Cases[Values[representatives], Power[d_, k_Integer?Negative] :> d, Infinity]];
  den = ConstructiveDiagrams`Private`spuriousEvaluateCurve[#, sampled, parameters] & /@ poles;
  If[!AllTrue[den, NumericQ[#] && !TrueQ[# == 0] &], fail["Test point on a pole or unresolved denominator"]];
  polys = Map[ConstructiveDiagrams`Private`spuriousEvaluateCurve[#, sampled, parameters] &, representatives];
  clean = FreeQ[Values[polys], _Real | Indeterminate | _DirectedInfinity | _SpinorChain | _PropDen | _MomProd | _ModelParameter];
  If[!clean || !AllTrue[Values[polys], PolynomialQ[#, spins] &], fail["Unresolved numerical or spin expression"]];
  coeff = Map[Sort[CoefficientRules[Expand[#], spins]] &, polys];
  If[coeff["Input"] === {} || !AllTrue[Last /@ coeff["Input"], NumericQ], fail["Zero or unmapped oracle"]];
  <|"Coordinates" -> point, "NonzeroSpinCoefficients" -> Length[coeff["Input"]],
    "EqualToInput" -> Map[SameQ[#, coeff["Input"]] &, coeff]|>];
(* For QED all external coordinates, both spin variables, the charge and the
   electron mass can be left symbolic. Equality on this rational chart is an
   identity for generic kinematics in its domain. *)
qedIdentity[] := Module[{m = Unique["electronMass"], e = Unique["charge"], rules, chart, variables, values},
  rules = {ModelParameter[model, "Me"] -> m, ModelParameter[model, "EE"] -> e};
  chart = ConstructiveDiagrams`Private`completionHESymbolicExternalChart[ext /. rules, Automatic, Automatic];
  If[!AssociationQ[chart] || !TrueQ[chart["ExternalMassShellsValidated"]] ||
    !TrueQ[chart["MomentumConservationValidated"]] || chart["InternalOnShellConditions"] =!= {}, Return[False]];
  variables = Join[{m, e}, chart["ExternalIdentityCoordinates"], chart["SpinVariables"]];
  values = Map[Together[ConstructiveDiagrams`Private`spuriousEvaluateCurve[#, chart, rules]] &, representatives];
  With[{inputValue = values["Input"]},
    inputValue =!= 0 && AllTrue[Values[values], Function[value,
      FreeQ[value, _Real | Indeterminate | _DirectedInfinity | _SpinorChain | _PropDen | _MomProd | _ModelParameter] &&
      AllTrue[Join[CoefficientRules[Numerator[value], variables], CoefficientRules[Denominator[value], variables]],
        NumericQ[Last[#]] &] && Together[value - inputValue] === 0]]]];
qedProof = If[case === "qed", qedIdentity[], Null];
checks = Table[checkAt[ModelParameterRules[model, assignment], point], {assignment, assignments}, {point, points}];
wireMatches = ToExpression[Last[StringSplit[Import[path["input.txt"], "Text"], "\n"]], InputForm] === input;
passed = wireMatches && (case =!= "qed" || TrueQ[qedProof]) && AllTrue[Flatten[Values[#["EqualToInput"]] & /@ Flatten[checks]], TrueQ];
Export[path["agreement.json"], <|"Case" -> case, "Process" -> process, "DiagramCount" -> data["DiagramCount"],
  "Complexities" -> (SpinorExpressionComplexity /@ representatives), "Results" -> checks,
  "ExactInputWireRoundTrip" -> wireMatches, "QEDSymbolicChartIdentity" -> qedProof,
  "ParameterAssignments" -> Map[Association[First[#] -> ToString[Last[#],InputForm] & /@ #] &, assignments],
  "Passed" -> passed|>, "RawJSON"];
If[!passed, fail["Spin-resolved agreement failed"]];
