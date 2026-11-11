(* Scalar jets may be shared only inside one exact energy-chart evaluation.
   These fixtures exercise cache boundaries, rather than timing the backend. *)

(*
HEPCAT - High Energy Physics Constructive Amplitude Tool

Copyright (C) 2026 Neil Christensen

This file is part of HEPCAT.

HEPCAT is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

HEPCAT is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with HEPCAT.  If not, see <https://www.gnu.org/licenses/>.
*)


root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Get[FileNameJoin[{root, "source", "HEPCAT.wl"}]];
$HistoryLength = 0; checks = 0; failures = {};
SetAttributes[check, HoldRest];
check[name_, assertion_] := (checks++;
  If[!TrueQ[assertion], AppendTo[failures, name]; Print["FAIL: ", name]]);
hscSeries = ConstructiveDiagrams`Private`highEnergyCertifiedScalarSeries;
hscCore = ConstructiveDiagrams`Private`highEnergyCertifiedScalarSeriesUncached;
hscRows = ConstructiveDiagrams`Private`completionHEAffineAtomLaurentRows;
hscStats = <|"Hits" -> 0, "Misses" -> 0, "Stores" -> 0|>;
SetAttributes[hscLocal, HoldAll];
hscLocal[body_] := Block[
  {ConstructiveDiagrams`Private`$highEnergyScalarSeriesCache = <||>,
   ConstructiveDiagrams`Private`$highEnergyScalarSeriesCacheStatistics = hscStats}, body];
hscOutsideState[] := {
  OwnValues[ConstructiveDiagrams`Private`$highEnergyScalarSeriesCache],
  OwnValues[ConstructiveDiagrams`Private`$highEnergyScalarSeriesCacheStatistics]};
hscInitialState = hscOutsideState[];

hscKeys = hscLocal[Module[{first, second, variable, order, expression},
  first = hscSeries[1/(1 - hscT), hscT, 3];
  second = hscSeries[1/(1 - hscT), hscT, 3];
  variable = hscSeries[1/(1 - hscT), hscU, 3];
  order = hscSeries[1/(1 - hscT), hscT, 4];
  expression = hscSeries[1/(1 + hscT), hscT, 3];
  <|"Reports" -> {first, second, variable, order, expression},
    "Stats" -> ConstructiveDiagrams`Private`$highEnergyScalarSeriesCacheStatistics,
    "Entries" -> Length[ConstructiveDiagrams`Private`$highEnergyScalarSeriesCache],
    "Keys" -> Keys[ConstructiveDiagrams`Private`$highEnergyScalarSeriesCache]|>]];
check["a repeated exact request returns the identical complete certificate",
  hscKeys["Reports"][[1]] === hscKeys["Reports"][[2]] &&
    hscKeys["Reports"][[1]]["Status"] === "COMPLETE"];
check["the repeated exact request is a cache hit", hscKeys["Stats"]["Hits"] === 1];
check["different expression variable and order create separate entries",
  hscKeys["Stats"]["Misses"] === 4 && hscKeys["Stats"]["Stores"] === 4 &&
    hscKeys["Entries"] === 4];
check["the full exact expression variable order tuple is the key",
  MemberQ[hscKeys["Keys"], {1/(1-hscT), hscT, 3}] &&
    MemberQ[hscKeys["Keys"], {1/(1-hscT), hscU, 3}] &&
    MemberQ[hscKeys["Keys"], {1/(1-hscT), hscT, 4}]];
check["a scalar call outside the chart has the uncached result",
  hscSeries[1/(1-hscT), hscT, 3] === hscCore[1/(1-hscT), hscT, 3]];
check["ordinary return restores prior cache state", hscOutsideState[] === hscInitialState];

(* Mock only the inherited certified core. The production facade must check
   all report fields before storing anything in its call-local association. *)
hscMockReport[report_] := Internal`InheritedBlock[
  {ConstructiveDiagrams`Private`highEnergyCertifiedScalarSeriesUncached},
  Clear[ConstructiveDiagrams`Private`highEnergyCertifiedScalarSeriesUncached];
  ConstructiveDiagrams`Private`highEnergyCertifiedScalarSeriesUncached[___] := report;
  hscLocal[Module[{first, second},
    first = hscSeries[hscOpaque, hscT, 3]; second = hscSeries[hscOpaque, hscT, 3];
    <|"Reports" -> {first, second},
      "Stats" -> ConstructiveDiagrams`Private`$highEnergyScalarSeriesCacheStatistics,
      "Entries" -> Length[ConstructiveDiagrams`Private`$highEnergyScalarSeriesCache]|>]]];
hscBadReports = {
  <|"Status" -> "UNRESOLVED", "Reason" -> "Unsupported"|>,
  <|"Status" -> "COMPLETE", "RequestedOrderCovered" -> False, "Series" -> 0|>,
  <|"Status" -> "COMPLETE", "Series" -> 0|>,
  <|"Status" -> "COMPLETE", "RequestedOrderCovered" -> True|>,
  <|"Status" -> "COMPLETE", "RequestedOrderCovered" -> True,
    "Series" -> SeriesData[hscT, 0, {}, 2, 2, 1]|>,
  <|"Status" -> "COMPLETE", "RequestedOrderCovered" -> True, "Series" -> Indeterminate|>
};
Do[hscBad = hscMockReport[hscBadReports[[i]]];
  check["incomplete or failed report is not stored " <> ToString[i],
    hscBad["Entries"] === 0 && hscBad["Stats"]["Stores"] === 0 &&
      hscBad["Stats"]["Misses"] === 2 && hscBad["Stats"]["Hits"] === 0],
  {i, Length[hscBadReports]}];
hscGood = hscMockReport[<|"Status" -> "COMPLETE", "RequestedOrderCovered" -> True,
  "Series" -> SeriesData[hscT, 0, {1, 1, 1, 1}, 0, 4, 1],
  "ZeroAssumptionCertificates" -> {<|"Method" -> "ExactFixtureWitness"|>}|>];
check["only a fully covered complete report is stored",
  hscGood["Entries"] === 1 && hscGood["Stats"]["Stores"] === 1 && hscGood["Stats"]["Hits"] === 1];
check["a cache hit retains the full zero-assumption certificate",
  hscGood["Reports"][[1]] === hscGood["Reports"][[2]] &&
    hscGood["Reports"][[2]]["ZeroAssumptionCertificates"] ===
      {<|"Method" -> "ExactFixtureWitness"|>}];

hscInexact = hscLocal[(hscSeries[1.0/(1-hscT),hscT,3];
  hscSeries[1.0/(1-hscT),hscT,3];
  {Length[ConstructiveDiagrams`Private`$highEnergyScalarSeriesCache],
   ConstructiveDiagrams`Private`$highEnergyScalarSeriesCacheStatistics})];
check["inexact input bypasses storage and hit counters", hscInexact === {0,hscStats}];
hscNonfinite = hscLocal[(Quiet[hscSeries[Indeterminate,hscT,3]];
  Length[ConstructiveDiagrams`Private`$highEnergyScalarSeriesCache])];
check["nonfinite input is not stored", hscNonfinite === 0];

hscCap = hscLocal[Module[{first, before, second, hit},
  ConstructiveDiagrams`Private`$highEnergyScalarSeriesCache = Association[
    Table[{hscPrefilled[i],hscT,3} -> <|"Status"->"COMPLETE","Series"->0,
      "RequestedOrderCovered"->True|>,{i,4095}]];
  first=hscSeries[1/(1-hscT),hscT,3];
  before=Length[ConstructiveDiagrams`Private`$highEnergyScalarSeriesCache];
  second=hscSeries[1/(1+hscT),hscT,3];
  hit=hscSeries[1/(1-hscT),hscT,3];
  <|"Before"->before,"After"->Length[ConstructiveDiagrams`Private`$highEnergyScalarSeriesCache],
    "Stats"->ConstructiveDiagrams`Private`$highEnergyScalarSeriesCacheStatistics,
    "Reports"->{first,second,hit}|>]];
check["entry storage stops at the finite 4096-entry cap",
  hscCap["Before"]===4096 && hscCap["After"]===4096 && hscCap["Stats"]["Stores"]===1];
check["a full cache still computes new requests correctly",
  hscCap["Reports"][[2]]===hscCore[1/(1+hscT),hscT,3] && hscCap["Stats"]["Misses"]===2];
check["existing exact entries remain reusable at capacity",
  hscCap["Reports"][[1]]===hscCap["Reports"][[3]] && hscCap["Stats"]["Hits"]===1];

hscChart=<|"PoleVariable"->hscT,"SpinVariables"->{hscZ1,hscZ2},"Masses"-><||>|>;
hscUnknowns={hscC1,hscC2,hscC3};
hscFixture[parameter_] := (2+hscC1+3hscC2) hscZ1/((1+parameter hscT)hscT^2)+
  (1-hscC1+hscC3) hscZ2/((1+parameter hscT)hscT)+
  (7+hscC2+hscC3) hscZ1 hscZ2/((1+parameter hscT)hscT^2);
Do[
  hscTrial=hscFixture[hscParameter];
  hscCached=hscRows[hscTrial,hscChart,{},hscUnknowns,0,10];
  hscUncached=Internal`InheritedBlock[
    {ConstructiveDiagrams`Private`highEnergyCertifiedScalarSeries},
    Clear[ConstructiveDiagrams`Private`highEnergyCertifiedScalarSeries];
    ConstructiveDiagrams`Private`highEnergyCertifiedScalarSeries[e_,v_Symbol,q_Integer] :=
      ConstructiveDiagrams`Private`highEnergyCertifiedScalarSeriesUncached[e,v,q];
    hscRows[hscTrial,hscChart,{},hscUnknowns,0,10]];
  check["small typed affine chart completes "<>ToString[hscParameter],
    AssociationQ[hscCached] && hscCached["Status"]==="COMPLETE" &&
      hscCached["FullCoefficientCoverage"] && hscCached["ExactAffineReconstruction"]];
  check["cached and uncached exact coefficient rows agree "<>ToString[hscParameter],
    hscCached["Rows"]===hscUncached["Rows"] && hscCached["RowKeys"]===hscUncached["RowKeys"]];
  check["a chart records local exact reuse "<>ToString[hscParameter],
    hscCached["ScalarSeriesCache"]["Hits"]>0 &&
      hscCached["ScalarSeriesCache"]["Stores"]===hscCached["ScalarSeriesCache"]["Entries"] &&
      hscCached["ScalarSeriesCache"]["Scope"]==="SingleEnergyChart"];
  check["all original correction coordinates are retained "<>ToString[hscParameter],
    hscCached["OriginalCoordinates"]===hscUnknowns &&
      hscCached["Certificate"]["AllOriginalCoordinatesRetained"]],
  {hscParameter,{2,3}}];
check["two chart calls leave no persistent model or parameter cache",
  hscOutsideState[]===hscInitialState];

(* The chart's own Block, rather than a test-only wrapper, must restore any
   pre-existing caller state under ordinary return, timeout and abort. *)
hscRestoration=Block[
 {ConstructiveDiagrams`Private`$highEnergyScalarSeriesCache=<|hscCallerKey->hscCallerValue|>,
  ConstructiveDiagrams`Private`$highEnergyScalarSeriesCacheStatistics=<|"Caller"->17|>},
 Module[{before=hscOutsideState[],returned,timed,aborted,afterReturn,afterTimeout,afterAbort},
  returned=hscRows[hscFixture[2],hscChart,{},hscUnknowns,0,5]; afterReturn=hscOutsideState[];
  timed=Internal`InheritedBlock[{ConstructiveDiagrams`Private`highEnergyCertifiedScalarSeriesUncached},
    Clear[ConstructiveDiagrams`Private`highEnergyCertifiedScalarSeriesUncached];
    ConstructiveDiagrams`Private`highEnergyCertifiedScalarSeriesUncached[___] := (Pause[1];$Failed);
    hscRows[hscFixture[2],hscChart,{},hscUnknowns,0,0.02]]; afterTimeout=hscOutsideState[];
  aborted=Internal`InheritedBlock[{ConstructiveDiagrams`Private`highEnergyCertifiedScalarSeriesUncached},
    Clear[ConstructiveDiagrams`Private`highEnergyCertifiedScalarSeriesUncached];
    ConstructiveDiagrams`Private`highEnergyCertifiedScalarSeriesUncached[___] := Abort[];
    CheckAbort[hscRows[hscFixture[2],hscChart,{},hscUnknowns,0,5],"ABORTED"]]; afterAbort=hscOutsideState[];
  <|"Returned"->AssociationQ[returned],"Timed"->timed,"Aborted"->aborted,
    "ReturnRestored"->(before===afterReturn),"TimeoutRestored"->(before===afterTimeout),
    "AbortRestored"->(before===afterAbort)|>]];
check["chart return restores an enclosing caller cache",
  hscRestoration["Returned"] && hscRestoration["ReturnRestored"]];
check["chart timeout remains unresolved and restores an enclosing cache",
  hscRestoration["Timed"]===Missing["TypedAtomTimeConstraint"] &&
    hscRestoration["TimeoutRestored"]];
check["chart abort propagates and restores an enclosing cache",
  hscRestoration["Aborted"]==="ABORTED" && hscRestoration["AbortRestored"]];
check["outer cache symbols have their original definitions after every exit",
  hscOutsideState[]===hscInitialState];

Print["CHECKS: ",checks,"; FAILURES: ",Length[failures]];
If[failures=!={},Print[failures]];
Print["HEPCAT_TEST_COMPLETED:high-energy-scalar-cache-tests.wl"];
Quit[If[failures==={},0,1]];
