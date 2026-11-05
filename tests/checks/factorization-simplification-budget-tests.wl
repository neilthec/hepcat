(* Large sewn products remain complete when optional ordinary Simplify stalls. *)
fsRoot = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Get[FileNameJoin[{fsRoot, "source", "HEPCAT.wl"}]];
$HistoryLength = 0;
fsChecks = 0; fsFailures = {};
SetAttributes[fsCheck, HoldRest];
fsCheck[name_, test_] := (fsChecks++;
  If[!TrueQ[test], AppendTo[fsFailures, name]; Print["FAIL: ", name]]);
fsSimplify[expression_] := ConstructiveDiagrams`Private`factorizationChannelSimplify[expression];

fsSmall = (fsX^2 - 1)/(fsX - 1);
fsCheck["small ordinary algebra keeps the established Simplify result",
  fsSimplify[fsSmall] === Simplify[fsSmall] && fsSimplify[fsSmall] === 1 + fsX];
fsPole = PropDen[Mom[Multiparticle[1, 3]], fsMass];
fsShellSensitive = (MomProd[1, 3] + fsPole)/fsPole;
fsCheck["no internal on-shell relation is inferred from a channel propagator",
  fsSimplify[fsShellSensitive] === Simplify[fsShellSensitive] &&
    !FreeQ[fsSimplify[fsShellSensitive], MomProd[1, 3]]];

fsLarge = Total[Table[fsTerm[i]/(fsX + i), {i, 2500}]];
fsCheck["the generic large fixture exceeds the bounded heuristic admission",
  LeafCount[fsLarge] > 12000];
(* Deliberately slow the heuristic, not the algebra. Block restores the
   builtin immediately after this deterministic timeout probe. *)
fsElapsed = AbsoluteTiming[
  fsFallback = Block[{Simplify = Function[body, Pause[1]; body]}, fsSimplify[fsLarge]];
][[1]];
fsCheck["a stalled large-expression heuristic returns the exact complete input",
  fsFallback === fsLarge];
fsCheck["the optional large-expression heuristic has a finite short clock",
  fsElapsed < 0.9];
fsCheck["the builtin simplifier is restored after the timeout probe",
  Simplify[(fsX^2 - 1)/(fsX - 1)] === 1 + fsX];

fsModel = ReadModel[FileNameJoin[{fsRoot, "models"}], 2];
fsBefore = fsModel;
fsParticles = {"e", "E", "A.+", "A.+"};
fsChannels = FactorizationAmplitudeChannels[fsParticles, fsModel];
fsCheck["the established four-point channel inventory and public format remain unchanged",
  Length[fsChannels] === 2 && AllTrue[fsChannels,
    ListQ[#] && Length[#] === 7 && MatchQ[#[[-3]], _Multiparticle] &&
      !FreeQ[Last[#], _PropDen] &]];
fsCheck["sewing keeps the caller's model intact", fsBefore === fsModel];
fsCheck["factorization adds no new public tuning options",
  Options[FactorizationAmplitudeChannels] === {}];

Print["FACTORIZATION SIMPLIFICATION BUDGET CHECKS: ", fsChecks,
  "; FAILURES: ", Length[fsFailures]];
If[fsFailures === {}, Print["HEPCAT_TEST_COMPLETED:factorization-simplification-budget-tests.wl"],
  Print[fsFailures]];
Quit[If[fsFailures === {}, 0, 1]];
