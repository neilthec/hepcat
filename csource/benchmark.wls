(* HEPCAT C++ reducer benchmark. Each timed Wolfram evaluation uses its own kernel.
   Set HEPCAT_ROOT, HEPCAT_BENCH_DIR, HEPCAT_BENCH_CASE and HEPCAT_BENCH_MODE. *)
$HistoryLength = 0;
sourceRoot = Replace[Environment["HEPCAT_ROOT"], Except[_String] :> DirectoryName[DirectoryName[$InputFileName]]];
Get[FileNameJoin[{sourceRoot, "source", "HEPCAT.wl"}]];
workDir = Environment["HEPCAT_BENCH_DIR"];
case = Environment["HEPCAT_BENCH_CASE"];
mode = Environment["HEPCAT_BENCH_MODE"];
fail[msg_] := (Print["ERROR: ", msg]; Exit[2]);
processes = <|"qed" -> {"e", "E", "A.+", "A.+"}, "wwww" -> {"W+", "W+", "W-", "W-"},
  "qed3" -> {"e", "E", "A.+", "A.+", "A.-"}, "qed4" -> {"e", "E", "A.+", "A.+", "A.+", "A.+"}|>;
If[!DirectoryQ[workDir] || !KeyExistsQ[processes, case], fail["Invalid benchmark directory or case"]];
path[s_] := FileNameJoin[{workDir, case <> "-" <> s}];
process = processes[case];
photonLegs = Flatten[Position[process, "A.+" | "A.-", {1}]];
model = ReadModel[FileNameJoin[{sourceRoot, "models"}], If[photonLegs === {}, 1, 3]];

(* Encode the supported expression tree, preserving model-owned parameters verbatim. *)
wire[z_Integer] := ToString[z, InputForm];
wire[z_String] := ToString[z, InputForm, PageWidth -> Infinity];
wire[z_Symbol] := SymbolName[z];
wire[z_Rational] := "Rational[" <> wire[Numerator[z]] <> "," <> wire[Denominator[z]] <> "]";
wire[z_] := SymbolName[Head[z]] <> "[" <> StringRiffle[wire /@ (List @@ z), ","] <> "]";

(* Fix a shared cyclic photon reference for each external photon before the
   diagram sum. The HEPCAT model reader owns the masses and coupling symbols.
   Explicit mass tags from a contracted diagram use its own model replacements.
   No momentum product is put on an internal shell. *)
makeFixture[] := Module[{diagrams, pairs, terms, expr, masses, external, data, references, expected},
  diagrams = CreateDiagrams[process, model];
  pairs = DiagramAmplitudes[diagrams];
  If[Length[pairs] =!= If[photonLegs === {}, 6, Factorial[Length[photonLegs]]], fail["Unexpected diagram count"]];
  expected = AssociationThread[photonLegs, RotateLeft[photonLegs]];
  terms = Function[pair, Module[{value = ReduceSpinContractions[Last[pair]] /. First[pair]},
    If[photonLegs === {}, value,
      references = ConstructiveDiagrams`Private`preparationReferences[value, First[pair], model, pairs, "Cyclic", {}, True];
      If[!AssociationQ[references] || references["Status"] =!= "COMPLETE" ||
          Sort[Lookup[references["Assignments"], "Leg"]] =!= photonLegs ||
          !AllTrue[references["Assignments"], #["Target"] === expected[#["Leg"]] &],
        fail["Cyclic reference assignment failed"]];
      references["Expression"]]]] /@ pairs;
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
(* The detailed report contains potentially large algebraic certificates. Summarize
   only accepted changes and the witnesses attached to their nonzero identity rows. *)
traceStep[step_Association] := Module[{operations = {}, methods = {}, witnesses = 0, visit},
  visit[records_List] := Scan[visit, records];
  visit[record_Association] := Module[{rows = Lookup[record, "IdentityWitnesses", {}]},
    If[StringQ[record["Operation"]], AppendTo[operations, record["Operation"]]];
    If[StringQ[record["Method"]], AppendTo[methods, record["Method"]]];
    If[ListQ[rows], witnesses += Length[rows]; Scan[visit, rows]];
    Scan[visit, Values[KeyTake[record, {"IdentityRoute", "CoreRoute", "GroupCertificates",
      "SectorCertificates", "CoefficientRoute", "Route"}]]]];
  visit[_] := Null;
  visit[Lookup[step, "IdentityRoute", {}]];
  <|"Operation" -> step["Operation"], "BeforeComplexity" -> SpinorExpressionComplexity[step["Before"]],
    "AfterComplexity" -> SpinorExpressionComplexity[step["After"]], "RouteOperations" -> Counts[operations],
    "IdentityMethods" -> Counts[methods], "TensorIdentityWitnesses" -> witnesses|>];
If[mode === "trace",
  trace = PrepareAmplitudes[{ext, input}, model, "ExternalParticles" -> process, "InternalOnShell" -> False,
    "Parallelization" -> False, "ProgressReporting" -> False, "ReturnDetails" -> True];
  If[!AssociationQ[trace] || !MatchQ[trace["ItemReports"], {_Association}], fail["Missing preparation trace"]];
  item = First[trace["ItemReports"]];
  If[!ListQ[item["Reductions"]] || !KeyExistsQ[item, "PreparedExpression"], fail["Incomplete preparation trace"]];
  result = <|"Case" -> case, "Status" -> item["Status"], "InputComplexity" -> SpinorExpressionComplexity[input],
    "OutputComplexity" -> SpinorExpressionComplexity[item["PreparedExpression"]],
    "Steps" -> (traceStep /@ Select[item["Reductions"], #["Before"] =!= #["After"] &])|>;
  Export[path["trace.json"], result, "RawJSON"]; Print[result]; Exit[0]];
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
repeatCount = Environment["HEPCAT_BENCH_REPEATS"];
repeatFiles = If[StringQ[repeatCount] && StringMatchQ[repeatCount, DigitCharacter ..],
  Flatten[Table[path[method <> "-" <> ToString[i] <> If[method === "native", ".txt", ".wxf"]],
    {method, {"reduce", "prepare", "native"}}, {i, FromDigits[repeatCount]}]],
  Join[FileNames[RegularExpression[case <> "-reduce-[0-9]+\\.wxf"], workDir],
    FileNames[RegularExpression[case <> "-prepare-[0-9]+\\.wxf"], workDir],
    FileNames[RegularExpression[case <> "-native-[0-9]+\\.txt"], workDir]]];
If[!AllTrue[repeatFiles, FileExistsQ], fail["Missing a timed repetition output"]];
Do[AssociateTo[representatives, FileBaseName[file] ->
   If[FileExtension[file] === "wxf", Import[file, "WXF"],
      ToExpression[Import[file, "Text"], InputForm]]], {file, repeatFiles}];
If[!FreeQ[Values[representatives], $Failed | _Failure | _Missing | Indeterminate], fail["Missing reducer output"]];
(* Different repetitions may choose the same tree. Evaluate each distinct expression
   once per chart while reporting every timed repetition individually. *)
groups = GatherBy[Keys[representatives], representatives];
mapRepresentatives[f_] := Association[Flatten[Function[names,
  With[{value = f[representatives[First[names]]]}, (# -> value) & /@ names]] /@ groups, 1]];
assignments = If[photonLegs =!= {}, {{"Me" -> 2, "EE" -> 1/3}, {"Me" -> 3, "EE" -> 2/7}},
  {{"MW" -> 20, "SW" -> 3/5, "EE" -> 1/3, "Mh" -> 30},
   {"MW" -> 21, "SW" -> 5/13, "EE" -> 2/7, "Mh" -> 32}}];
points = If[Length[process] === 4, {{3, 5, 7}, {5, 7, 11}, {7, 11, 13}},
  (* The package chart fixes Lorentz redundancy and solves two dependent shells.
     Query its dimension since the number of independent coordinates grows with multiplicity. *)
  With[{chart = ConstructiveDiagrams`Private`completionHESymbolicExternalChart[
      ext /. ModelParameterRules[model, First[assignments]], Automatic, Automatic]},
    If[!AssociationQ[chart], fail["Cannot construct conserved test chart"]];
    Table[Prime[Range[Length[chart["ExternalIdentityCoordinates"]]] + offset], {offset, 1, 3}]]];
checkAt[parameters_, point_] := Module[{chart, coords, spins, sampled, poles, den, polys, coeff, degrees, clean},
  chart = ConstructiveDiagrams`Private`completionHESymbolicExternalChart[ext /. parameters, Automatic, Automatic];
  If[!AssociationQ[chart] || !TrueQ[chart["ExternalMassShellsValidated"]] ||
    !TrueQ[chart["MomentumConservationValidated"]] || chart["InternalOnShellConditions"] =!= {}, fail["Invalid check chart"]];
  coords = chart["ExternalIdentityCoordinates"]; spins = chart["SpinVariables"];
  If[Length[point] =!= Length[coords] || Length[spins] =!= Length[process] - Length[photonLegs], fail["Invalid spin or coordinate count"]];
  sampled = chart /. Thread[coords -> point];
  If[!AllTrue[Flatten[Values[sampled["Momenta"]]], IntegerQ[#] || Head[#] === Rational &] ||
      Total[Values[sampled["Momenta"]]] =!= ConstantArray[0, {2, 2}] ||
      !AllTrue[Keys[sampled["Momenta"]], Det[sampled["Momenta"][#]] === sampled["Masses"][#]^2 &],
    fail["Test point violates an external mass shell or momentum conservation"]];
  If[!AllTrue[Subsets[photonLegs, {2}], Function[legs,
      AllTrue[{"Angle", "Square"}, Det[Lookup[sampled[#], legs]] =!= 0 &]]],
    fail["Test point contains collinear photon spinors"]];
  poles = DeleteDuplicates[Cases[Values[representatives], Power[d_, k_Integer?Negative] :> d, Infinity]];
  den = ConstructiveDiagrams`Private`spuriousEvaluateCurve[#, sampled, parameters] & /@ poles;
  If[!AllTrue[den, NumericQ[#] && !TrueQ[# == 0] &], fail["Test point on a pole or unresolved denominator"]];
  polys = mapRepresentatives[ConstructiveDiagrams`Private`spuriousEvaluateCurve[#, sampled, parameters] &];
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
  values = mapRepresentatives[Together[ConstructiveDiagrams`Private`spuriousEvaluateCurve[#, chart, rules]] &];
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
