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
check["literal exact zeros declare their unrestricted algebraic scope",
  ConstructiveDiagrams`Private`completionHEZeroDirection[0,cnRules,{{},{},{},{}},1]["InternalOnShellConditions"] === {}];
cnVectorFixture = Get[FileNameJoin[{root,"tests","checks","fixtures","ordinary-vector-source-zero.wl"}]];
cnVectorProof = ConstructiveDiagrams`Private`completionHEZeroDirection[
  cnVectorFixture["Expression"],cnVectorFixture["ExternalMassRules"],{{},{},{},{}},8];
check["ordinary preconditioning certifies a symbolic vector identity without model fitting",
  cnVectorProof["Status"] === "PROVED_ZERO" && KeyExistsQ[cnVectorProof,"OrdinarySourcePreconditioning"]];
check["the proof retains the original direction and complete external-only reduction replay",
  cnVectorProof["OrdinarySourcePreconditioning"]["OriginalExpression"] === cnVectorFixture["Expression"] &&
    cnVectorProof["InternalOnShellConditions"] === {} &&
    cnVectorProof["OrdinarySourcePreconditioning"]["Reduction"]["GroupReductions"] =!= {} &&
    AllTrue[cnVectorProof["OrdinarySourcePreconditioning"]["Reduction"]["GroupReductions"],
      #["InternalOnShellConditions"] === {} &]];
cnDegreeGuard = ConstructiveDiagrams`Private`completionHEOrdinaryProofEligibleQ;
check["a degree-two massive tensor activates the proof preconditioner",
  cnDegreeGuard[cnLarge,Thread[(Mass /@ Range[4])->1],{2,2,2,0},27]];
check["massless photon degree cannot activate a six-fermion tensor",
  !cnDegreeGuard[cnLarge,Join[Thread[(Mass /@ Range[6])->1],{Mass[7]->0}],{1,1,1,1,1,1,2},64]];
check["a small ordinary direction does not acquire preprocessing overhead",
  !cnDegreeGuard[cnInput,cnRules,{2,2,2,0,0},27]];
cnPerturbation = SpinorChain[Spinor["Spin","Angle",1],Spinor["Spin","Angle",2]] *
  SpinorChain[Spinor["Spin","Angle",2],Spinor["Spin","Angle",3]] *
  SpinorChain[Spinor["Spin","Angle",3],Spinor["Spin","Angle",1]];
cnNonzeroProof = ConstructiveDiagrams`Private`completionHEZeroDirection[
  cnVectorFixture["Expression"]+cnPerturbation,cnVectorFixture["ExternalMassRules"],{{},{},{},{}},5];
check["an improved ordinary representative still requires its own exact zero proof",
  cnNonzeroProof["Status"] === "NOT_PROVED"];
cnRawParameter = ModelParameter["rawCoupling", "ordinary-source-proof-test", "Real"];
cnRawIndependent = ModelParameter["independentCoupling", "ordinary-source-proof-test", "Real"];
cnRawFixture = cnVectorFixture["Expression"] /. vectorIdentityParameter1 -> cnRawParameter;
cnRawModel = {{}, {}, {{cnRawIndependent, 1/3}}, {{cnRawParameter, Sqrt[1-cnRawIndependent^2]}}};
cnRawProof = ConstructiveDiagrams`Private`completionHEZeroDirection[
  cnRawFixture, cnVectorFixture["ExternalMassRules"], cnRawModel, 20];
check["an independent-parameter identity is tried before derived model expansion",
  cnRawProof["Status"] === "PROVED_ZERO" && KeyExistsQ[cnRawProof,"RawModelParameterProof"] &&
    !KeyExistsQ[cnRawProof,"ModelDefinitionExpansion"]];
check["the raw proof preserves its original expression and stronger assumption scope",
  cnRawProof["RawModelParameterProof"]["OriginalExpression"] === cnRawFixture &&
    cnRawProof["RawModelParameterProof"]["ModelDefinitionsUsed"] === {} &&
    cnRawProof["RawModelParameterProof"]["NumericalParameterAssignments"] === {} &&
    cnRawProof["InternalOnShellConditions"] === {}];
cnModelOnlyTerm = First[List @@ Expand[cnRawFixture]];
cnModelOnlyInput = cnRawFixture + (cnRawParameter-Sqrt[1-cnRawIndependent^2]) cnModelOnlyTerm;
cnModelOnlyProof = ConstructiveDiagrams`Private`completionHEZeroDirection[
  cnModelOnlyInput, cnVectorFixture["ExternalMassRules"], cnRawModel, 20];
check["failure of the stronger raw identity retains model-definition fallback",
  cnModelOnlyProof["Status"] === "PROVED_ZERO" && KeyExistsQ[cnModelOnlyProof,"ModelDefinitionExpansion"] &&
    cnModelOnlyProof["RawParameterProofAttempt"]["Status"] === "NOT_PROVED"];
check["raw-parameter recursion cannot change later proof calls",
  !TrueQ[ConstructiveDiagrams`Private`$completionHEKeepRawModelParameters]];
check["raw numerator clearing retains a separate exact nonzero-denominator certificate",
  cnRawProof["RawModelParameterProof"]["ExactRationalRegrouping"] &&
    cnRawProof["RawModelParameterProof"]["DenominatorNonzeroCertificate"]["Status"] === "PROVED_NONZERO" &&
    cnRawProof["RawModelParameterProof"]["DenominatorNonzeroCertificate"]["AmplitudeZeroSamples"] === 0];
cnDenominatorChart = ConstructiveDiagrams`Private`completionHESymbolicExternalChart[cnVectorFixture["ExternalMassRules"]];
cnInvalidDenominator = ConstructiveDiagrams`Private`completionHERawDenominatorWitness[
  PropDen[Mom[1], Mass[1] /. cnVectorFixture["ExternalMassRules"]], cnDenominatorChart, 2];
check["an external on-shell zero cannot certify a nonempty cleared-denominator domain",
  MissingQ[cnInvalidDenominator]];
Internal`InheritedBlock[{ConstructiveDiagrams`Private`completionHERawDenominatorWitness},
  Clear[ConstructiveDiagrams`Private`completionHERawDenominatorWitness];
  ConstructiveDiagrams`Private`completionHERawDenominatorWitness[___] := Missing["UnprovedDenominator"];
  cnUncertifiedRaw = ConstructiveDiagrams`Private`completionHERawParameterIdentityProof[
    cnRawFixture,cnVectorFixture["ExternalMassRules"],cnRawModel,10]];
check["a zero numerator alone cannot admit an uncertified denominator",
  cnUncertifiedRaw["Status"] === "NOT_PROVED" &&
    cnUncertifiedRaw["Method"] === "UncertifiedClearedDenominator"];
Internal`InheritedBlock[{ConstructiveDiagrams`Private`completionHEOrdinarySourceColumn},
  Clear[ConstructiveDiagrams`Private`completionHEOrdinarySourceColumn];
  ConstructiveDiagrams`Private`completionHEOrdinarySourceColumn[___] := (Pause[2];<||>);
  {cnRawDeadlineSeconds,cnRawDeadline} = AbsoluteTiming[
    ConstructiveDiagrams`Private`completionHERawParameterIdentityProof[
      cnRawFixture,cnVectorFixture["ExternalMassRules"],cnRawModel,1/10]]];
check["raw numerator preparation shares its finite original proof deadline",
  cnRawDeadline["Status"] === "NOT_PROVED" && cnRawDeadlineSeconds < 1];
Internal`InheritedBlock[{ConstructiveDiagrams`Private`completionHEOrdinarySourceColumn},
  Clear[ConstructiveDiagrams`Private`completionHEOrdinarySourceColumn];
  ConstructiveDiagrams`Private`completionHEOrdinarySourceColumn[___] := Missing["NotPrepared"];
  cnRawWithoutReduction = ConstructiveDiagrams`Private`completionHERawParameterIdentityProof[
    0,cnRules,cnRawModel,3]];
check["a successful original numerator keeps an unavailable reduction record honestly",
  cnRawWithoutReduction["Status"] === "PROVED_ZERO" &&
    MissingQ[cnRawWithoutReduction["RawModelParameterProof"]["OrdinaryReduction"]]];
cnChartAllowance = ConstructiveDiagrams`Private`completionHEPreferredChartAllowance;
check["the bounded raw prefix gives its preferred chart the complete existing allowance",
  Block[{ConstructiveDiagrams`Private`$completionHEKeepRawModelParameters = True},
    cnChartAllowance[12,True,2] === 12]];
check["ordinary portfolios retain their established alternative-chart reserve",
  Block[{ConstructiveDiagrams`Private`$completionHEKeepRawModelParameters = False},
    cnChartAllowance[12,True,2] === 8]];
check["a single chart and later alternatives retain only their actual remaining allowance",
  Block[{ConstructiveDiagrams`Private`$completionHEKeepRawModelParameters = True},
    cnChartAllowance[5,False,2] === 5 && cnChartAllowance[7,True,1] === 7] &&
  Block[{ConstructiveDiagrams`Private`$completionHEKeepRawModelParameters = False},
    cnChartAllowance[5,False,2] === 5 && cnChartAllowance[7,True,1] === 7]];
check["preferred-chart allocation never extends its supplied portfolio clock",
  And @@ Flatten[Table[Block[{ConstructiveDiagrams`Private`$completionHEKeepRawModelParameters = raw},
    With[{allocation = cnChartAllowance[budget,first,count]}, 0 <= allocation <= budget]],
    {raw,{False,True}},{budget,{1/10,3,30}},{first,{False,True}},{count,{1,2}}]]];
Print["CHECKS: ", checks]; Print["FAILURES: ", Length[failures]];
If[failures =!= {}, Print["FAILED_CHECKS: ", failures]]; Print["HEPCAT_TEST_COMPLETED:high-energy-source-normalization-tests.wl"]; Quit[If[failures === {}, 0, 1]];
