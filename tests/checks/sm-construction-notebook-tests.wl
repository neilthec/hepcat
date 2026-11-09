(* Read the user's worked notebook as data, never by evaluating its inputs. *)
root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Get[FileNameJoin[{root,"source","HEPCAT.wl"}]];
checks = 0; failures = {};
check[name_, value_] := (checks++; If[!TrueQ[value], AppendTo[failures,name]]);
path = FileNameJoin[{root,"tests","SM-4-point-construction.nb"}];
held = Quiet[Check[ToExpression[Import[path,"Text"],InputForm,HoldComplete],$Failed]];
(* Front-end notebook comments can parse as top-level Null entries. Remove
   only those comments, keeping any unexpected executable expression visible. *)
held = DeleteCases[held,Null,{1}];
check["the SM construction notebook parses as one held notebook",
  MatchQ[held,HoldComplete[_Notebook]]];
active = Cases[held, Cell[text_String,"Input",opts___] /;
  !MemberQ[{opts},Evaluatable->False] :> text, Infinity];
parsed = ToExpression[#,InputForm,HoldComplete] & /@ active;
check["all modern calculation inputs parse without execution", FreeQ[parsed,$Failed|_Syntax]];
constructions = Flatten[Cases[#,HoldPattern[ConstructAmplitude[p_,d_,f_,opts___]] :>
  HoldComplete[p,d,f,{opts}],Infinity] & /@ parsed];
check["the manual process inventory has at least 132 explicit helicity calls",
  Length[constructions] >= 132];
check["the SM calls use the same declared cubic model for four-point factorization",
  AllTrue[constructions, MatchQ[#,HoldComplete[_,sm,sm,_List]] &]];
check["the separate massive SM Feynman model is loaded in Setup",
  !FreeQ[parsed,HoldPattern[Set[smFeynman,
    ReadModel[FileNameJoin[{hepcatDirectory,"model_feynman"}],1]]]]];
check["every SM construction explicitly requests its independent Feynman model",
  AllTrue[constructions,
    Count[#,HoldPattern["FeynmanModel"->smFeynman],Infinity]===1 &]];
check["the notebook uses the direct construction workflow without optional stage cells",
  FreeQ[held, Cell["Inspect the stages (optional)", "Subsection", ___]] &&
    Count[parsed, HoldPattern[CreateDiagrams[__] | CheckAmplitudes[__]], Infinity] === 0];
check["the notebook has no archived result cells or validation tables",
  FreeQ[held, HoldPattern[CellTags -> tag_String] /;
    StringStartsQ[tag, "hepcat-cache-" | "hepcat-precision-qualified-numerical-table-" |
      "hepcat-hhhh-symbolic-table" | "hepcat-gluon-symbolic-table" | "hepcat-gluon-numerical-table"]]];
exports = Flatten[Cases[#,HoldPattern[WriteSPINASProcess[args___]] :>
  HoldComplete[args],Infinity] & /@ parsed];
check["each original process and the additional charge conjugate have a numerical export", Length[exports] >= 74];
check["all exports state their incoming and outgoing particles",
  AllTrue[exports, MatchQ[#,HoldComplete[_,_Rule,_List,sm,___]] &]];
check["reference tools retain the full selected SM rather than a QED restriction",
  !AnyTrue[active,StringContainsQ[#,"AllowedParticles\" -> {11,22}"|"AllowedParticles\" -> {11, 22}"] &]];
check["both independent numerical comparisons are requested in the shared setup",
  AnyTrue[parsed,!FreeQ[#,HoldPattern["RunCalcHEPValidation"->True]] &&
    !FreeQ[#,HoldPattern["RunMadGraphValidation"->True]] &]];
check["the local massive-quark UFO and diagonal flavor mixing are explicit",
  AnyTrue[active,StringContainsQ[#,"model_madgraph"] &] &&
    AnyTrue[parsed,!FreeQ[#,HoldPattern["lamWS"->0]] &]];
check["only setup and live construction or export inputs remain",
  Length[active] === 209 && Length[constructions] === 132 && Length[exports] === 74];
sections = Cases[held,Cell[_,"Section",opts___] /;
  AnyTrue[{opts},MatchQ[#,CellTags->_String] &&
    StringStartsQ[Last[#],"construction-sm"] &],Infinity];
check["every original process remains separately visible",Length[sections]>=73];
check["the extra original charge-conjugate process is constructed independently",
  AnyTrue[active,StringContainsQ[#,"sm61CParticles = {\"D\", \"u\", \"Z\", \"W-\"}"] &&
    StringContainsQ[#,"ConstructAmplitude[sm61CParticles, sm, sm,"] &]];
check["the extra charged process has its own SPINAS class and export",
  AnyTrue[active,StringContainsQ[#,"sm61CExport = WriteSPINASProcess["] &&
    StringContainsQ[#,"\"ProcessName\" -> \"sm61C\""] &]];
cellTags = Flatten[Cases[held,HoldPattern[Rule[CellTags,value_]] :> value,Infinity]];
constructionTags = Select[cellTags, StringQ[#] && StringStartsQ[#, "construction-sm"] &];
check["all process families retain their navigation tags",
  Length[DeleteDuplicates[constructionTags]] === 74];
check["the removed manual appendix has no dangling links",
  FreeQ[held, HoldPattern[CellTags -> tag_String] /; StringStartsQ[tag, "manual-link-"]] &&
    FreeQ[held, TemplateBox[{_, {"SM-4-point-construction.nb", target_String}, ___},
      "HyperlinkTemplate", ___] /; StringStartsQ[target, "manual-sm"]]];
check["portable precision notes remain for the two near-zero comparisons",
  ContainsAll[cellTags, {"hepcat-hhhh-extended-precision-replay", "hepcat-gluon-exact-zero-replay"}]];
Print["SM NOTEBOOK CHECKS: ",checks,"; FAILURES: ",Length[failures]];
If[failures==={},Print["HEPCAT_TEST_COMPLETED:sm-construction-notebook-tests.wl"],Print[failures]];
Quit[If[failures==={},0,1]];
