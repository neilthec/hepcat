(* Re-evaluate the native expressions and their independent notebook references in Wolfram. *)
require[condition_, message_] := If[!TrueQ[condition], Print[message]; Quit[1]];
smCrossScoreArguments = If[Length[$ScriptCommandLine] > 1, Rest[$ScriptCommandLine],
  Drop[$CommandLine, First[FirstPosition[$CommandLine, "-script"]] + 1]];
require[Length[smCrossScoreArguments] == 2, "Use WolframKernel -script cross-score.wls NATIVE_RUN OUTPUT.tsv"];
{run, output} = ExpandFileName /@ smCrossScoreArguments;
base = DirectoryName[$InputFileName];
Get[FileNameJoin[{Nest[DirectoryName, base, 3], "source", "HEPCAT.wl"}]];
$HistoryLength = 0;
readTable[path_] := StringSplit[#, "\t"] & /@ Rest[StringSplit[Import[path, "Text"], "\n"]];
byCase = Association[(First[#] -> #) & /@ Join @@ (readTable[FileNameJoin[{base, #}]] & /@
  {"catalog.tsv", "flavor-closure.tsv"})];
rows = Select[readTable[FileNameJoin[{run, "scores.tsv"}]], First[#] =!= "external" &];
require[Length[byCase] == 392 && rows =!= {} && DuplicateFreeQ[First /@ rows], "Incomplete or duplicate inventory"];
(* Unqualified physics heads belong to HEPCAT. Arithmetic heads and I have normal System` semantics.
   Reject unfamiliar symbols before releasing the parsed expression. *)
allowedSymbols = Join[
  ("System`" <> # &) /@ {"HoldComplete", "List", "Times", "Plus", "Power", "Rational", "Complex", "I"},
  ("ConstructiveDiagrams`" <> # &) /@ {"ModelParameter", "PropDen", "Mom", "MomProd", "Multiparticle", "SpinorChain", "Spinor",
    "ColorGenerator", "ColorIndex", "ColorDelta", "ColorStructure", "Private`colorInternalIndex"}];
SetAttributes[qualifiedName, HoldAllComplete];
qualifiedName[s_Symbol] := Context[Unevaluated[s]] <> SymbolName[Unevaluated[s]];
parse[text_] := Module[{held, unexpected},
  held = Block[{$Context = "SM4CrossScore`", $ContextPath = {"System`", "ConstructiveDiagrams`"}},
    ToExpression[text, InputForm, HoldComplete]];
  unexpected = Cases[held, s_Symbol /; !MemberQ[allowedSymbols, qualifiedName[s]] :> qualifiedName[s],
    {0, Infinity}, Heads -> True];
  require[MatchQ[held, _HoldComplete] && unexpected === {}, {"Unrecognized symbols", unexpected}];
  ReleaseHold[held]];
score[expr_] := {ConstructiveDiagrams`SpinorExpressionComplexity[expr], LeafCount[expr]};
results = Table[Module[{id = row[[1]], family = row[[2]], ref, path, benchmark, measured},
  require[KeyExistsQ[byCase, id] && row[[3]] === "PASS", "Unverified native input " <> id];
  ref = byCase[id];
  benchmark = score[parse[Last[ref]]];
  require[ToString /@ benchmark === ref[[6 ;; 7]] && family === ref[[2]], "Could not reproduce reference score " <> id];
  path = FileNameJoin[{run, family, id <> ".expr"}];
  measured = score[parse[Import[path, "Text"]]];
  require[VectorQ[measured, NonNegative[#] && IntegerQ[#] &], "Invalid native cross-score " <> id];
  Join[{id, family, FileHash[path, "SHA256", "HexString"], row[[4]], row[[6]]}, measured, benchmark, {$Version}]],
  {row, rows}];
columns = {"case", "family", "expression_sha256", "native_weight", "native_leaves", "wolfram_weight", "wolfram_leaves",
  "notebook_weight", "notebook_leaves", "wolfram_version"};
Export[output, StringJoin[(StringRiffle[ToString /@ #, "\t"] <> "\n") & /@ Prepend[results, columns]], "Text"];
Print["Cross-scored ", Length[results], " native expressions and their notebook references. ", output];
Quit[0];
