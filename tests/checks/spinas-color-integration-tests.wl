(* Opt-in independent comparisons of complete color sums and their interference. *)

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
$HistoryLength = 0;
checks = 0; failures = {};
SetAttributes[check, HoldRest];
check[name_, assertion_] := (checks++; If[!TrueQ[assertion], AppendTo[failures, name]; Print["FAIL: ", name]]);
directory = CreateDirectory[];
sm = ReadModel[FileNameJoin[{root, "models"}], 1];
qed = ReadModel[FileNameJoin[{root, "models"}], 4];
spinas = Replace[Environment["HEPCAT_SPINAS_DIRECTORY"], $Failed | "" -> FileNameJoin[{ParentDirectory[root], "spinas"}]];
madgraph = FileNameJoin[{root, "tests", "reference-models", "madgraph", "sm"}];
mgRoot = Replace[Environment["HEPCAT_MADGRAPH_DIRECTORY"], $Failed | "" -> FileNameJoin[{ParentDirectory[root], "MadGraph"}]];
calchep = Replace[Environment["HEPCAT_CALCHEP_DIRECTORY"], $Failed | "" -> FileNameJoin[{ParentDirectory[root], "CalcHEP", "calchep_3.9.2"}]];
compare[name_, entries_, allowed_, helicityCount_] := Module[{report, ch, mg},
  report = WriteSPINASProcess[FileNameJoin[{directory, name}], entries, sm,
    "ProcessName" -> name, "SPINASDirectory" -> spinas,
    "CalcHEPDirectory" -> calchep, "CalcHEPModel" -> {FileNameJoin[{root, "tests", "reference-models", "calchep"}], 1},
    "CalcHEPAllowedParticles" -> allowed, "CalcHEPPoints" -> 20, "CalcHEPSeed" -> 12789, "RunCalcHEPValidation" -> True,
    "MadGraphDirectory" -> mgRoot, "MadGraphModel" -> madgraph, "MadGraphAllowedParticles" -> allowed,
    "MadGraphPoints" -> 20, "MadGraphSeed" -> 12789, "RunMadGraphValidation" -> True, "ReturnDetails" -> True];
  check[name <> ": compiled export runs both independent references", AssociationQ[report]];
  If[!AssociationQ[report], Print["EXPORT FAILURE: ", InputForm[report]]; Return[]];
  ch = report["CalcHEPValidation"]; mg = report["MadGraphValidation"];
  check[name <> ": CalcHEP color sum passes all twenty points", Lookup[ch, "Status", None] === "PASS" &&
    StringContainsQ[Lookup[ch, "StandardOutput", ""], "Mismatches/errors: 0/0"]];
  check[name <> ": MadGraph color sum passes every spin/helicity sector", Lookup[mg, "Status", None] === "PASS" &&
    Length[Lookup[mg, "Rows", {}]] === helicityCount &&
    AllTrue[Lookup[mg, "Rows", {}], Lookup[#, "Points", 0] === 20 && Lookup[#, "Mismatches", -1] === 0 &]]];
Do[
  {name, process} = entry;
  diagrams = Select[CreateDiagrams[process, sm], MemberQ[Lookup[Values[#["Propagators"]], "Particle"], "G"] &];
  pairs = DiagramAmplitudes[diagrams];
  amplitude = Total[Table[Total[Table[term[[1]] Last[First[PrepareAmplitudes[{{pair[[1]], term[[2]]}}, sm]]],
    {term, ConstructiveDiagrams`Private`colorExpressionTerms[pair[[2]]]}]], {pair, pairs}]];
  compare[name, {{process, amplitude}}, {1, 2, 21}, 1],
  {entry, {{"color_quark_distinct", {"u", "U", "d", "D"}}, {"color_quark_identical", {"u", "u", "U", "U"}}}}];
mu = ModelParameter[sm, "Mu"]; gg = ModelParameter[sm, "GG"];
qm = ModelParameter[qed, "Me"]; qe = ModelParameter[qed, "EE"];
dummy = ConstructiveDiagrams`Private`colorInternalIndex[1, 3];
c34 = ColorGenerator[ColorIndex[2], dummy, ColorIndex[3]] ColorGenerator[dummy, ColorIndex[1], ColorIndex[4]];
c43 = c34 /. {ColorIndex[3] -> ColorIndex[4], ColorIndex[4] -> ColorIndex[3]};
t = PropDen[Mom[Multiparticle[1, 3]], mu]; u = PropDen[Mom[Multiparticle[1, 4]], mu]; s = PropDen[Mom[Multiparticle[1, 2]], 0];
processes = Join[{"e", "E"}, #] & /@ {{"A.+", "A.+"}, {"A.+", "A.-"}, {"A.-", "A.-"}};
entries = Table[
  numerator = (LookupAmplitude[Thread[{p, Range[4]}], qed, False, {i, j, k, l}] /. {qm -> mu, qe -> gg}) t u;
  {{"u", "U", StringReplace[p[[3]], "A" -> "G"], StringReplace[p[[4]], "A" -> "G"]},
    c34 numerator/s/t + c43 numerator/s/u}, {p, processes}];
compare["color_quark_gluons", entries, {2, 21}, 4];
Print["SPINAS COLOR INTEGRATION CHECKS: ", checks, "; FAILURES: ", Length[failures]];
If[failures === {}, DeleteDirectory[directory, DeleteContents -> True]; Print["HEPCAT_TEST_COMPLETED:spinas-color-integration-tests.wl"],
  Print[failures]; Print["COLOR INTEGRATION FILES: ", directory]];
Quit[If[failures === {}, 0, 1]];
