(* Exact representation, graded transport and constructor fallback controls. *)
root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Get[FileNameJoin[{root, "source", "HEPCAT.wl"}]];
$HistoryLength = 0;
checks = 0; failures = {};
SetAttributes[check, HoldRest];
check[name_, assertion_] := (checks++; If[!TrueQ[assertion], AppendTo[failures, name]; Print["FAIL: ", name]]);
model = ReadModel[FileNameJoin[{root, "models"}], 1];
ci[n_] := ColorIndex[n];
internal[n_, dimension_] := ConstructiveDiagrams`Private`colorInternalIndex[n, dimension];
plan[process_, data_, reps_] := ConstructiveDiagrams`Private`colorOrbitPlan[process, model, data, reps, 20];
cb[tensors_, reps_] := ConstructiveDiagrams`Private`colorTensorBasis[tensors, reps, 65536, False];
process = {"u", "U", "G.+", "G.+", "h"}; reps = {3, -3, 8, 8, 1};
t1 = ColorGenerator[ci[2], internal[1, 3], ci[3]] ColorGenerator[internal[1, 3], ci[1], ci[4]];
t2 = ColorGenerator[ci[2], internal[2, 3], ci[4]] ColorGenerator[internal[2, 3], ci[1], ci[3]];
data = cb[{t1, t2}, reps]; orbit = plan[process, data, reps];
check["same-state gluons give one exact seed for two color coefficients",
  AssociationQ[orbit] && orbit["Status"] === "AVAILABLE" && orbit["SeedIndices"] === {1}];
check["the external-state group excludes particles of other species",
  orbit["IdenticalStateGroups"] === {{3, 4}}];
check["all generator sparse components and Hermitian Gram matrices are certified",
  And @@ Lookup[orbit["Generators"], "ExactSparseComponentClosure"] &&
  And @@ Lookup[orbit["Generators"], "GramInvariant"]];
check["row closure is full rank and bounded by rank times generators",
  MatrixRank[orbit["Rows"]] === 2 && Length[orbit["Attempts"]] <= 2 Length[orbit["Generators"]]];
check["both exact inverse identities are retained",
  orbit["Rows"].orbit["InverseRows"] === IdentityMatrix[2] &&
  orbit["InverseRows"].orbit["Rows"] === IdentityMatrix[2]];
check["the color plan contains no amplitude or benchmark coefficients",
  TrueQ[orbit["NoAmplitudeOrBenchmarkCoefficientInput"]]];
check["opposite gluon helicities sharing a model row are not grouped",
  MissingQ[plan[{"u", "U", "G.+", "G.-", "h"}, data, reps]]];
check["four-point calls bypass the new orbit method",
  MissingQ[plan[Most[process], data, Most[reps]]]];
check["rank-one calls bypass the new orbit method",
  MissingQ[plan[process, cb[{t1}, reps], reps]]];
check["bounded rank admission rejects an oversized formal basis before tensor work",
  MissingQ[plan[process, <|"Basis" -> Range[17], "GramMatrix" -> IdentityMatrix[17]|>, reps]]];
check["unknown external model states remain unsupported",
  MissingQ[plan[ReplacePart[process, 1 -> "unknown"], data, reps]]];
check["a color subspace not closed under exchange is rejected",
  MissingQ[plan[process, cb[{t1, ColorDelta[ci[1], ci[2]] ColorDelta[ci[3], ci[4]]}, reps], reps]]];
permutation = {1, 2, 4, 3, 5};
check["typed relabeling changes external color labels only",
  ConstructiveDiagrams`Private`colorOrbitRelabel[ColorGenerator[ci[1], ci[2], internal[9, 8]], permutation] ===
    ColorGenerator[ci[1], ci[2], internal[9, 8]] &&
  ConstructiveDiagrams`Private`colorOrbitRelabel[ci[3] ci[4] ci[99], permutation] === ci[3] ci[4] ci[99]];
seed = SpinorChain[Spinor["Helicity", "Square", 3], Mom[1], Spinor["Helicity", "Angle", 4]];
seedReport = <|1 -> <|"Status" -> "COMPLETE", "Amplitude" -> seed|>|>;
transport = ConstructiveDiagrams`Private`colorOrbitTransport[orbit, seedReport];
exchangedSeed = PermuteParticles[seed, {3, 4}];
check["boson transport uses the literal typed momentum/spinor permutation",
  transport["Amplitudes"] === {seed, exchangedSeed}];
check["incomplete or failed seed reports cannot produce a transported point",
  MissingQ[ConstructiveDiagrams`Private`colorOrbitTransport[orbit, <||>]] &&
  MissingQ[ConstructiveDiagrams`Private`colorOrbitTransport[orbit, <|1 -> Failure["test", <||>]|>]]];
complexData = cb[{t1 + I t2, t2}, reps]; complexOrbit = plan[process, complexData, reps];
check["complex color bases preserve conjugate Gram invariance",
  AssociationQ[complexOrbit] && And @@ Lookup[complexOrbit["Generators"], "GramInvariant"]];
fProcess = {"u", "u", "U", "U", "h"}; fReps = {3, 3, -3, -3, 1};
f1 = ColorDelta[ci[1], ci[3]] ColorDelta[ci[2], ci[4]];
f2 = ColorDelta[ci[1], ci[4]] ColorDelta[ci[2], ci[3]];
fOrbit = plan[fProcess, cb[{f1, f2}, fReps], fReps];
check["identical fermions and antifermions both participate",
  fOrbit["IdenticalStateGroups"] === {{1, 2}, {3, 4}}];
check["odd twice-spin supplies the fermion grade to each exchange",
  Lookup[fOrbit["Generators"], "FermionGrade"] === {-1, -1}];
fSeed = SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 3]];
fTransport = ConstructiveDiagrams`Private`colorOrbitTransport[fOrbit,
  <|1 -> <|"Status" -> "COMPLETE", "Amplitude" -> fSeed|>|>];
check["fermion transport applies the statistics sign exactly once",
  fTransport["Amplitudes"] === {fSeed, -PermuteParticles[fSeed, {1, 2}]}];
angle[i_, j_] := SpinorChain[Spinor["Helicity", "Angle", i], Spinor["Helicity", "Angle", j]];
external = Thread[(Mass /@ Range[5]) -> ConstantArray[0, 5]];
apparent = 1/(angle[1, 3] angle[2, 4] - angle[1, 2] angle[3, 4]);
polished = ConstructiveDiagrams`Private`colorOrbitPolish[apparent, external, 5];
check["an inverse Schouten base becomes a product of physical brackets",
  polished["Amplitude"] === 1/(angle[1, 4] angle[2, 3]) && TrueQ[polished["ExactIdentityVerified"]]];
check["the official Schouten edit and external-only proof are retained",
  Length[polished["OfficialSchoutenRoutes"]] > 0 && polished["InternalOnShellConditions"] === {}];
unknownMass = ConstructiveDiagrams`ModelParameter["testMass", "testScope", "Real"];
opaque = 1/PropDen[Mom[Multiparticle[1, 2]], unknownMass];
check["massive propagators are not replaced by massless bracket products",
  ConstructiveDiagrams`Private`colorOrbitPolish[opaque, external, 2]["Amplitude"] === opaque];
check["internal-shell metadata is not accepted by the final-expression polish",
  ConstructiveDiagrams`Private`colorOrbitPolish[apparent,
    Append[external, Mass[Multiparticle[1, 2]] -> 0], 2]["Amplitude"] === apparent];

(* Synthetic workflow records test dispatch and failure preservation, not physics. *)
workflow[validationFailure_:False, childFailure_:False, explicitGroups_:Automatic, clock_:60] :=
 Internal`InheritedBlock[{ConstructiveDiagrams`CreateDiagrams, ConstructiveDiagrams`DiagramAmplitudes,
   ConstructiveDiagrams`FactorizationAmplitudeChannels, ConstructiveDiagrams`ConstructAmplitude,
   ConstructiveDiagrams`CheckAmplitudes, ConstructiveDiagrams`Private`colorProjectionData,
   ConstructiveDiagrams`Private`colorScalarCurrentProjectionCertificate,
   ConstructiveDiagrams`Private`completionValidationRuleSets,
   ConstructiveDiagrams`Private`constructValidationOverrides},
  Unprotect[ConstructiveDiagrams`CreateDiagrams, ConstructiveDiagrams`DiagramAmplitudes,
    ConstructiveDiagrams`FactorizationAmplitudeChannels, ConstructiveDiagrams`ConstructAmplitude,
    ConstructiveDiagrams`CheckAmplitudes];
  Clear[ConstructiveDiagrams`CreateDiagrams, ConstructiveDiagrams`DiagramAmplitudes,
    ConstructiveDiagrams`FactorizationAmplitudeChannels, ConstructiveDiagrams`ConstructAmplitude,
    ConstructiveDiagrams`CheckAmplitudes, ConstructiveDiagrams`Private`colorProjectionData,
    ConstructiveDiagrams`Private`colorScalarCurrentProjectionCertificate,
    ConstructiveDiagrams`Private`completionValidationRuleSets,
    ConstructiveDiagrams`Private`constructValidationOverrides];
  constructionCalls = {}; validationCalls = {};
  fakeCheck[status_:"PASS"] := <|"Checks" -> {
      <|"Type" -> "HighEnergy", "Pole" -> "energy", "Status" -> status|>,
      <|"Type" -> "Reference", "Pole" -> "reference", "Status" -> "PASS"|>},
    "Summary" -> Counts[{status, "PASS"}], "Channels" -> {},
    "AutomaticPoleDiscovery" -> True, "HighEnergyCheck" -> <|"Status" -> status|>,
    "ReferenceCheck" -> <|"Status" -> "PASS"|>|>;
  ConstructiveDiagrams`CreateDiagrams[___] := {"syntheticDiagram"};
  ConstructiveDiagrams`DiagramAmplitudes[___] := {{external, 1}};
  ConstructiveDiagrams`FactorizationAmplitudeChannels[___] := {};
  ConstructiveDiagrams`Private`colorProjectionData[___] := Join[data,
    <|"ProjectedDiagrams" -> {{{external, 1}}, {{external, 2}}},
      "ProjectedChannels" -> {{}, {}}, "ProjectedFeynmanAmplitudes" -> {{}, {}}|>];
  ConstructiveDiagrams`Private`colorScalarCurrentProjectionCertificate[___] := Missing["Synthetic"];
  ConstructiveDiagrams`Private`completionValidationRuleSets[___] := {{}, {}, {}};
  ConstructiveDiagrams`Private`constructValidationOverrides[___] := Automatic;
  ConstructiveDiagrams`ConstructAmplitude[___] := Module[{index = Last[First[
      ConstructiveDiagrams`Private`$colorConstructionInput["DiagramAmplitudes"]]]},
    AppendTo[constructionCalls, index];
    If[TrueQ[childFailure] && index === 2, Return[Failure["SyntheticChildFailure", <||>]]];
    <|"Status" -> "COMPLETE", "Amplitude" -> index,
      "AmplitudePair" -> {external, index}, "Checks" -> fakeCheck[]|>];
  ConstructiveDiagrams`CheckAmplitudes[pair_, channels_, suppliedModel_, opts___] := (
    AppendTo[validationCalls, Association[{opts}]];
    fakeCheck[If[TrueQ[validationFailure] && Length[validationCalls] === 2, "FAIL", "PASS"]]);
  result = ConstructiveDiagrams`Private`colorConstructAmplitude[process, model, model,
    Join[Association[Options[ConstructAmplitude]], <|"Output" -> "Details", "FeynmanModel" -> None,
      "ProgressReporting" -> False, "PrintReport" -> False, "TimeConstraint" -> clock,
      "RandomSeed" -> 12345, "BoseSymmetryGroups" -> explicitGroups|>]];
  <|"Result" -> result, "Constructed" -> constructionCalls, "Validated" -> validationCalls|>
 ];
selected = workflow[];
check["a complete orbit constructs only its required coefficient seed",
  selected["Constructed"] === {1} && selected["Result"]["ColorOrbitAttempt"]["Selected"] === True];
check["every coefficient is normally checked at all validation assignments",
  Length[selected["Validated"]] === 6 &&
    AllTrue[selected["Validated"], #["SpuriousPoles"] === Automatic &&
      #["HighEnergyCheck"] === True && #["ReferenceCheck"] === True &]];
check["orbit details preserve the original seed report separately from transported reports",
  KeyExistsQ[selected["Result"]["ColorOrbitAttempt"]["OriginalSeedReports"], 1] &&
    AllTrue[selected["Result"]["ColorCoefficientReports"],
      Length[#["ColorOrbitValidationReports"]] === 3 &]];
fallback = workflow[True];
check["a failed derived check falls back and reuses its completed seed",
  fallback["Constructed"] === {1, 2} && fallback["Result"]["Status"] === "COMPLETE" &&
    fallback["Result"]["ColorOrbitAttempt"]["Selected"] === False];
retained = workflow[True, True];
check["a later independent child failure retains the earlier complete report exactly once",
  FailureQ[retained["Result"]] && retained["Constructed"] === {1, 2} &&
    Length[retained["Result"][[2]]["ColorCoefficientReports"]] === 2 &&
    retained["Result"][[2]]["ColorCoefficientReports"][[1, "Status"]] === "COMPLETE"];
literal = workflow[False, False, {}];
check["an explicit symmetry override preserves ordinary independent construction",
  literal["Constructed"] === {1, 2} && literal["Validated"] === {} &&
    literal["Result"]["ColorOrbitAttempt"]["Attempted"] === False];
expired = workflow[False, False, {}, 10^-9];
check["a deadline before any child preserves an honest empty indexed report cache",
  FailureQ[expired["Result"]] && expired["Constructed"] === {} &&
    expired["Result"][[2]]["CompletedColorCoefficients"] === 0 &&
    Length[expired["Result"][[2]]["ColorCoefficientReports"]] === 2];
check["the new helper names remain private",
  Names["ConstructiveDiagrams`colorOrbit*"] === {}];
Print["Checks: ", checks, "; failures: ", Length[failures]];
If[failures =!= {}, Print[failures]];
Quit[If[failures === {}, 0, 1]];
