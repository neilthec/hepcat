(* Large retained sources keep their original coupled coefficient coordinates. *)
root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Get[FileNameJoin[{root, "source", "HEPCAT.wl"}]];
$HistoryLength = 0; checks = 0; failures = {};
SetAttributes[check, HoldRest];
check[name_, value_] := (checks++; If[!TrueQ[value], AppendTo[failures, name]; Print["FAIL: ", name]]);
cnMass = Unique["externalMass"]; cnInternal = Unique["internalMass"];
cnCoupling = Unique["coupling"]; cnOtherCoupling = Unique["otherCoupling"];
cnRules = {Mass[1] -> cnMass, Mass[2] -> cnMass, Mass[3] -> 0, Mass[4] -> 0, Mass[5] -> 0};
cnPole = PropDen[Mom[Multiparticle[1, 3]], cnInternal];
cnA = Spinor["Spin", "Angle", 1]; cnB = Spinor["Xi", "Angle", 90];
cnR = Spinor["Xi", "Angle", 91]; cnS = Spinor["Helicity", "Square", 5];
cnLeading = SpinorChain[cnA, cnB] SpinorChain[cnR, Mom[1], cnS];
cnPartner = cnMass SpinorChain[cnR, cnB] SpinorChain[Spinor["Spin", "Square", 1], cnS];
cnExpected = cnCoupling SpinorChain[cnA, cnR] SpinorChain[cnB, Mom[1], cnS]/cnPole;
cnInput = cnCoupling (cnLeading - cnPartner)/cnPole;
cnSaved = cnInput;
cnReduced = ConstructiveDiagrams`Private`completionHEOrdinarySourceColumn[cnInput, cnRules, 5];
check["ordinary grouping exposes the external Dirac-Schouten mass partner",
  cnReduced["Status"] === "IMPROVED" && cnReduced["Expression"] === ConstructiveDiagrams`Private`canonicalChains[cnExpected]];
check["source normalization preserves its literal physical denominator",
  Cases[cnReduced["Expression"], _PropDen, Infinity] === {cnPole}];
check["the reducer records exact regrouping and external-only mass assumptions",
  cnReduced["ExactFormalRegrouping"] && cnReduced["InternalOnShellConditions"] === {} &&
    cnReduced["ExternalMassRules"] === cnRules && !cnReduced["CoefficientCoordinatesSplit"]];
check["source arguments remain unchanged", cnInput === cnSaved];
cnKey = ConstructiveDiagrams`Private`completionHEOrdinarySourceGroupKey;
check["external mass powers are ignored only in the grouping key",
  cnKey[cnCoupling cnMass^2 cnLeading/cnPole, cnRules] ===
    cnKey[cnCoupling cnMass cnLeading/cnPole, cnRules]];
check["different model couplings cannot be stripped from a grouping key",
  cnKey[cnCoupling cnLeading/cnPole, cnRules] =!=
    cnKey[cnOtherCoupling cnLeading/cnPole, cnRules]];
check["different physical propagators remain separate groups",
  cnKey[cnCoupling cnLeading/cnPole, cnRules] =!=
    cnKey[cnCoupling cnLeading/(cnPole /. cnInternal -> cnMass), cnRules]];
check["an internal mass is not treated as an external mass partner",
  cnKey[cnCoupling cnInternal cnLeading/cnPole, cnRules] =!=
    cnKey[cnCoupling cnLeading/cnPole, cnRules]];
cnDots = MomProd[1, 3]/cnPole;
cnDotReduced = ConstructiveDiagrams`Private`completionHEOrdinarySourceColumn[cnDots,
  Append[cnRules, Mass[Multiparticle[1, 3]] -> cnMass], 3];
check["composite mass metadata cannot put an assembled source numerator on an internal shell",
  cnDotReduced["Expression"] === cnDots &&
    FreeQ[cnDotReduced["ExternalMassRules"], Mass[_Multiparticle]]];
check["zero budget retains the original column",
  ConstructiveDiagrams`Private`completionHEOrdinarySourceColumn[cnInput, cnRules, 0]["Expression"] === cnInput];
check["conflicting external masses retain the original column",
  ConstructiveDiagrams`Private`completionHEOrdinarySourceColumn[cnInput,
    Append[cnRules, Mass[1] -> cnInternal], 5]["Expression"] === cnInput];
cnLarge = Total[Array[cnOpaque, 9000]];
check["ordinary source admission requires a large coherent tensor and actual large sources",
  ConstructiveDiagrams`Private`completionHEOrdinarySourceEligibleQ[
    ConstantArray[cnLarge, 6], <|"CoherentComponents" -> 27, "CoherentSpinDegrees" -> <|1 -> 2, 2 -> 2, 3 -> 2|>|>]];
check["small coherent-spin families keep the established path",
  !ConstructiveDiagrams`Private`completionHEOrdinarySourceEligibleQ[
    ConstantArray[cnLarge, 6], <|"CoherentComponents" -> 16|>]];
check["small source catalogs have no preprocessing overhead",
  !ConstructiveDiagrams`Private`completionHEOrdinarySourceEligibleQ[
    {cnInput}, <|"CoherentComponents" -> 81|>]];
cnColumns = {cnLarge, 2 cnLarge, 3}; cnOriginalColumns = cnColumns; cnEvents = {};
Internal`InheritedBlock[{ConstructiveDiagrams`Private`completionHEOrdinarySourceColumn},
  Clear[ConstructiveDiagrams`Private`completionHEOrdinarySourceColumn];
  ConstructiveDiagrams`Private`completionHEOrdinarySourceColumn[expr_, rules_, time_] :=
    <|"Expression" -> 0, "Status" -> "IMPROVED", "InternalOnShellConditions" -> {}|>;
  cnCatalog = ConstructiveDiagrams`Private`completionHEOrdinarySourceColumns[
    cnColumns, cnRules, 2, (AppendTo[cnEvents, #] &)]];
check["a source becoming ordinary zero retains its original coefficient position",
  cnCatalog["Basis"] === {0, 0, 3} && cnCatalog["RetainedColumns"] === 3 &&
    cnCatalog["ColumnCoordinatesPreserved"] && !cnCatalog["ExpandedTermsAssignedIndependentCoefficients"]];
check["the original complete source catalog remains available",
  cnCatalog["OriginalBasis"] === cnOriginalColumns && cnColumns === cnOriginalColumns];
check["source progress identifies each processed original column",
  Length[cnEvents] === 2 && Sort[Lookup[cnEvents, "Column"]] === {1, 2}];
cnNoBudget = ConstructiveDiagrams`Private`completionHEOrdinarySourceColumns[cnColumns, cnRules, 0];
check["a preprocessing timeout cannot truncate or replace unvisited columns",
  cnNoBudget["Basis"] === cnColumns && cnNoBudget["RetainedColumns"] === 3 &&
    cnNoBudget["ColumnRecords"] === {}];
Print["CHECKS: ", checks]; Print["FAILURES: ", Length[failures]];
If[failures =!= {}, Print["FAILED_CHECKS: ", failures]]; Print["HEPCAT_TEST_COMPLETED:high-energy-source-normalization-tests.wl"]; Quit[If[failures === {}, 0, 1]];
