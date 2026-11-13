(* Source-only class/encoding controls. Set HEPCAT_4FV_CANDIDATE_SOURCE to test a shadow. *)
root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Get[FileNameJoin[{root, "source", "HEPCAT.wl"}]];
Begin["HEPCATFourFermionVectorSourceTests`Private`"];
$ContextPath = Prepend[DeleteCases[$ContextPath, "HEPCATFourFermionVectorSourceTests`Private`"], "HEPCATFourFermionVectorSourceTests`Private`"];
$HistoryLength = 0; checks = 0; failures = {};
SetAttributes[check, HoldRest];
check[name_, condition_] := (checks++; If[!TrueQ[condition], AppendTo[failures, name]; Print["FAIL: ", name]]);
model = ReadModel[FileNameJoin[{root, "models"}], 1]; before = model;
build = HEPCATCovariantCurrentSources`Private`covariantCurrentSource;
legacyScalar = build[{"e", "E", "m", "M", "h"}, model];
legacyPhoton = build[{"e", "E", "A.+", "h", "h"}, model];
originalOtherDefinitions = AssociationMap[With[{symbol = ToExpression[#]}, DownValues[symbol]] &,
 DeleteCases[Names["HEPCATCovariantCurrentSources`Private`*"], "HEPCATCovariantCurrentSources`Private`covariantCurrentSource"]];
candidate = Environment["HEPCAT_4FV_CANDIDATE_SOURCE"];
If[!StringQ[candidate] || candidate === "", candidate = FileNameJoin[{root, "source", "internal", "HEPCAT-CovariantSources.wl"}]];
Get[candidate];
check["all preexisting current matchers and graph helpers are unchanged",
 originalOtherDefinitions === AssociationMap[With[{symbol = ToExpression[#]}, DownValues[symbol]] &, Keys[originalOtherDefinitions]]];
newScalar = build[{"e", "E", "m", "M", "h"}, model];
newPhoton = build[{"e", "E", "A.+", "h", "h"}, model];
check["the existing four-fermion scalar amplitude is literally unchanged",
 AssociationQ[legacyScalar] && AssociationQ[newScalar] && legacyScalar["Amplitude"] === newScalar["Amplitude"]];
check["the existing two-fermion photon two-scalar amplitude is literally unchanged",
 AssociationQ[legacyPhoton] && AssociationQ[newPhoton] && legacyPhoton["Amplitude"] === newPhoton["Amplitude"]];
distinct = build[{"e", "E", "m", "M", "Z"}, model];
identical = build[{"e", "E", "e", "E", "Z"}, model];
check["a massive-vector process is only an unvalidated source proposal",
 AssociationQ[distinct] && distinct["Status"] === "UNVALIDATED_CANDIDATE" &&
 !distinct["NativeValidationPerformed"] && distinct["RequiresCheckAmplitudes"]];
check["the raw symmetric tensor is retained beside its implicit spin polynomial",
 AssociationQ[distinct] && !FreeQ[distinct["RawTensorExpression"], _VectorSpinIndex] &&
 FreeQ[distinct["Expression"], _VectorSpinIndex] && TrueQ[distinct["CoherentExternalVectorEncoding"]["RawTensorRetained"]]];
check["coherent encoding is exactly the existing symmetric-vector representation",
 AssociationQ[distinct] && distinct["Expression"] === (distinct["RawTensorExpression"] /.
 Spinor["Spin", "Upper", kind : ("Angle" | "Square"), leg_Integer,
 VectorSpinIndex[leg_Integer, 1 | 2]] :> Spinor["Spin", kind, leg])];
check["all four Dirac degrees and both vector slots are retained",
 AssociationQ[distinct] && distinct["CompleteSpinWeights"] === {1, 1, 1, 1, 2} &&
 distinct["CoherentExternalVectorEncoding"]["CompleteSpinDegrees"] === {1, 1, 1, 1, 2}];
check["the distinct process preserves every ordered graph and physical pole",
 AssociationQ[distinct] && distinct["OriginalDiagramCount"] === 14 && distinct["LiftedDiagramCount"] === 14 &&
 Sort[distinct["OrderedGraphCertificates"]] === Sort[distinct["LiftedGraphCertificates"]]];
check["the identical process preserves every ordered fermion endpoint pairing",
 AssociationQ[identical] && identical["OriginalDiagramCount"] === 28 && identical["LiftedDiagramCount"] === 28 &&
 Sort[identical["OrderedGraphCertificates"]] === Sort[identical["LiftedGraphCertificates"]]];
check["four-fermion photon processes remain outside the new source class",
 FailureQ[build[{"e", "E", "m", "M", "A.+"}, model]] &&
 FailureQ[build[{"e", "E", "m", "M", "A.-"}, model]]];
electronRow = First[FirstPosition[model[[1]], r_List /; MemberQ[r[[{2, 3}]], "e"], Missing["UnknownElectron"]]];
check["colored or chiral external metadata are still rejected",
 FailureQ[build[{"e", "E", "m", "M", "Z"}, ReplacePart[model, {1, electronRow, 8} -> 3]]] &&
 FailureQ[build[{"e", "E", "m", "M", "Z"}, ReplacePart[model, {1, electronRow, 9} -> "L"]]]];
check["unvalidated proposals never use benchmark coefficients or numerical fits",
 AssociationQ[distinct] && !distinct["IndependentBenchmarkRead"] && !distinct["NumericalCoefficientFit"] &&
 distinct["NumericalParameterAssignments"] === {} && distinct["InternalOnShellConditions"] === {} &&
 !distinct["NewIndependentContacts"]];
check["all retained coefficients belong to the supplied model",
 AssociationQ[distinct] && Complement[DeleteDuplicates[Cases[distinct["Expression"], _ModelParameter, Infinity]],
 First /@ Join[model[[3]], model[[4]]]] === {}];
check["input model data are untouched", model === before];
check["massive-vector conversion still certifies every used local vertex",
 AssociationQ[distinct] && distinct["EveryUsedOrderedVertexCertified"] &&
 AllTrue[distinct["VertexCertificates"], MemberQ[{"EXACT_SYMBOLIC_MATCH", "EXACT_STANDARD_X_XT_STRUCTURE_MATCH"}, #["Status"]] &]];
(* Corrupt only a mocked final converter payload. These are guard tests, not physics validation. *)
catalog = HEPCATCovariantCurrentSources`Private`sourceCatalog[model, distinct["SourceUsedRows"]];
definitionsBefore = {DownValues[FeynmanToSpinorProducts], DownValues[HEPCATCovariantCurrentSources`Private`sourceCatalog]};
rejectEncoding[payload_] := Internal`InheritedBlock[{FeynmanToSpinorProducts, HEPCATCovariantCurrentSources`Private`sourceCatalog},
 Unprotect[FeynmanToSpinorProducts]; Clear[FeynmanToSpinorProducts, HEPCATCovariantCurrentSources`Private`sourceCatalog];
 HEPCATCovariantCurrentSources`Private`sourceCatalog[___] := catalog;
 FeynmanToSpinorProducts[___] := Table[{distinct["ExternalMassRules"], If[j === 1, payload, 0]}, {j, distinct["OriginalDiagramCount"]}];
 build[{"e", "E", "m", "M", "Z"}, model]];
check["an unknown vector slot cannot leave the coherent encoding guard",
 With[{result = rejectEncoding[distinct["RawTensorExpression"] /. VectorSpinIndex[5, 1] -> VectorSpinIndex[5, 3]]},
 FailureQ[result] && First[result] === "UnsupportedCoherentExternalTensorEncoding"]];
check["a mismatched vector leg cannot leave the coherent encoding guard",
 With[{result = rejectEncoding[distinct["RawTensorExpression"] /. VectorSpinIndex[5, 1] -> VectorSpinIndex[4, 1]]},
 FailureQ[result] && First[result] === "UnsupportedCoherentExternalTensorEncoding"]];
check["an undeclared vector slot cannot leave the coherent encoding guard",
 With[{result = rejectEncoding[distinct["RawTensorExpression"] /. VectorSpinIndex[5, 1] -> VectorSpinIndex[5, unknownSlot]]},
 FailureQ[result] && First[result] === "UnsupportedCoherentExternalTensorEncoding"]];
check["an implicit polynomial with the wrong external spin degree is rejected",
 With[{result = rejectEncoding[distinct["RawTensorExpression"] SpinorChain[Spinor["Spin", "Angle", 1], Spinor["Spin", "Angle", 2]]]},
 FailureQ[result] && First[result] === "UnsupportedCoherentExternalTensorEncoding"]];
check["all guard-test instrumentation restores exact original converter and catalog definitions",
 definitionsBefore === {DownValues[FeynmanToSpinorProducts], DownValues[HEPCATCovariantCurrentSources`Private`sourceCatalog]}];
If[checks =!= 20, AppendTo[failures, "Not every source control executed"]];
Print[ToString[checks - Length[failures]] <> "/" <> ToString[checks] <> " four-fermion-vector source checks passed"];
If[failures =!= {}, Print[failures]];
If[failures === {}, Print["HEPCAT_TEST_COMPLETED:covariant-four-fermion-vector-source-tests.wl"]];
Quit[If[failures === {}, 0, 1]];
