(* Read the user's worked notebook as data, never by evaluating its inputs. *)
root = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]];
Get[FileNameJoin[{root,"source","HEPCAT.wl"}]];
checks = 0; failures = {};
check[name_, value_] := (checks++; If[!TrueQ[value], AppendTo[failures,name]]);
path = FileNameJoin[{root,"tests","SM-4-point-construction.nb"}];
held = Quiet[Check[ToExpression[Import[path,"Text"],InputForm,HoldComplete],$Failed]];
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
check["no optional Feynman converter is silently made mandatory",
  FreeQ[constructions,HoldPattern["FeynmanModel"->_]]];
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
inactive = Cases[held,Cell[_,"Input",opts___] /;
  MemberQ[{opts},Evaluatable->False],Infinity];
check["the historical derivation inputs are retained but cannot evaluate",
  Length[inactive] >= 857];
sections = Cases[held,Cell[_,"Section",opts___] /;
  AnyTrue[{opts},MatchQ[#,CellTags->_String] &&
    StringStartsQ[Last[#],"construction-sm"] &],Infinity];
check["every original process remains separately visible",Length[sections]>=73];
check["the extra original charge-conjugate process is constructed independently",
  AnyTrue[active,StringContainsQ[#,"sm61CParticles = {\"D\", \"u\", \"Z\", \"W-\"}"] &&
    StringContainsQ[#,"ConstructAmplitude[sm61CParticles, sm, sm]"] &]];
check["the extra charged process has its own SPINAS class and export",
  AnyTrue[active,StringContainsQ[#,"sm61CExport = WriteSPINASProcess["] &&
    StringContainsQ[#,"\"ProcessName\" -> \"sm61C\""] &]];
cellTags = Flatten[Cases[held,HoldPattern[Rule[CellTags,value_]] :> value,Infinity]];
referenceTargets = Cases[held,
  TemplateBox[{_,{_String,target_String},___},"HyperlinkTemplate",___] :> target,Infinity];
manualTargets = Select[referenceTargets,StringStartsQ[#,"manual-sm"] &];
check["every current family links to an existing original reference cell",
  Length[manualTargets] >= 74 && AllTrue[manualTargets,MemberQ[cellTags,#] &]];
check["the original references link back to every current calculation",
  Length[DeleteDuplicates[Select[referenceTargets,StringStartsQ[#,"construction-sm"] &]]] >= 74];
Print["SM NOTEBOOK CHECKS: ",checks,"; FAILURES: ",Length[failures]];
If[failures==={},Print["HEPCAT_TEST_COMPLETED:sm-construction-notebook-tests.wl"],Print[failures]];
Quit[If[failures==={},0,1]];
