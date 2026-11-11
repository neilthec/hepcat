(* Audit saved four-point results without evaluating notebook input cells.
   Run with WolframKernel -script tests/sm-four-point-simplification-audit.wl.
   HEPCAT_SM_AUDIT_OUTPUT optionally selects the report directory. Models and
   the worked notebook are read only. This is an opt-in audit, not a unit test. *)
auditRoot = ParentDirectory[DirectoryName[$InputFileName]];
Get[FileNameJoin[{auditRoot, "source", "HEPCAT.wl"}]];
$HistoryLength = 0;
auditModel = ReadModel[FileNameJoin[{auditRoot, "models"}], 1];
auditNotebook = FileNameJoin[{auditRoot, "tests", "SM-4-point-construction.nb"}];
auditHeld = DeleteCases[ToExpression[Import[auditNotebook, "Text"], InputForm, HoldComplete], Null, {1}];
If[!MatchQ[auditHeld, HoldComplete[_Notebook]], Print["Invalid notebook."]; Quit[1]];
auditCells = Cases[auditHeld,
  Cell[content : (_String | _BoxData), style : ("Input" | "Output"), opts___] :>
    {content, style, {opts}}, Infinity];
auditRecords = {}; auditPending = None;
Do[
  If[cell[[2]] === "Input",
    If[MemberQ[cell[[3]], Evaluatable -> False], Continue[]];
    auditPending = None;
    If[StringQ[cell[[1]]] && StringContainsQ[cell[[1]], " = ConstructAmplitude["],
      heldInput = ToExpression[cell[[1]], InputForm, HoldComplete];
      names = StringCases[cell[[1]], RegularExpression[
        "(sm[0-9]+[A-Z]*)Amplitude\\s*=\\s*ConstructAmplitude\\["] -> "$1"];
      processes = Cases[heldInput, HoldPattern[Set[_Symbol, p : {__String}]] :> p, Infinity];
      If[Length[names] =!= 1 || Length[processes] =!= 1,
        Print["Ambiguous construction input."]; Quit[1]];
      auditPending = <|"Name" -> First[names], "Process" -> First[processes]|>],
    If[AssociationQ[auditPending],
      heldValue = Quiet[Check[ToExpression[First[cell[[1]]], StandardForm, HoldComplete], $Failed]];
      If[!MatchQ[heldValue, HoldComplete[_]] || !FreeQ[heldValue, _Failure | $Failed | $Aborted],
        Print["No usable saved amplitude for ", auditPending["Name"]]; Quit[1]];
      (* Older saved boxes omitted InterpretationBox for the specific display
         p_i.p_j. Restore only that known display in this imported audit data;
         this is deliberately not a symbolic rule in the package. *)
      restored = Count[heldValue, HoldPattern[Dot[
        Subscript[ConstructiveDiagrams`p, _Integer],
        Subscript[ConstructiveDiagrams`p, _Integer]]], Infinity];
      value = ReleaseHold[heldValue] /. HoldPattern[Dot[
        Subscript[ConstructiveDiagrams`p, i_Integer],
        Subscript[ConstructiveDiagrams`p, j_Integer]]] :> MomProd[i, j];
      If[!FreeQ[value, _Dot | _Subscript | _Row],
        Print["Unrecognized display-only data for ", auditPending["Name"]]; Quit[1]];
      masses = ConstructiveDiagrams`Private`constructExternalMasses[auditPending["Process"], auditModel];
      AppendTo[auditRecords, Join[auditPending, <|"Amplitude" -> value,
        "ExternalMassRules" -> Thread[Mass /@ Range[4] -> masses],
        "RestoredMomentumProductDisplays" -> restored|>]];
      auditPending = None]],
  {cell, auditCells}];
If[Length[auditRecords] =!= 132 || !DuplicateFreeQ[Lookup[auditRecords, "Name"]],
  Print["Expected 132 distinct saved four-point amplitudes; found ", Length[auditRecords]]; Quit[1]];
auditDirectory = Environment["HEPCAT_SM_AUDIT_OUTPUT"];
If[!StringQ[auditDirectory] || auditDirectory === "", auditDirectory = CreateDirectory[]];
If[!DirectoryQ[auditDirectory], CreateDirectory[auditDirectory, CreateIntermediateDirectories -> True]];
auditRank = ConstructiveDiagrams`Private`preparationRank;
auditBetter = ConstructiveDiagrams`Private`preparationBetterQ;
auditScore[expression_] := TimeConstrained[SpinorExpressionComplexity[expression], 3, "Unmeasured"];
auditRows = {}; auditResults = {}; auditStarted = AbsoluteTime[];
Do[
  before = record["Amplitude"]; started = AbsoluteTime[];
  Print["Preparing ", record["Name"]];
  prepared = TimeConstrained[PrepareAmplitudes[{record["ExternalMassRules"], before}, auditModel,
    "InternalOnShell" -> False, "PolePreference" -> None, "MaxSteps" -> 0,
    "TimeConstraint" -> 15, "Parallelization" -> False, "ProgressReporting" -> False,
    "ReturnDetails" -> True], 25, $TimedOut];
  If[!AssociationQ[prepared] || !KeyExistsQ[prepared, "PreparedAmplitudes"],
    Print["Preparation failed for ", record["Name"]]; Quit[1]];
  after = Last[First[prepared["PreparedAmplitudes"]]];
  beforeRank = auditRank[before]; afterRank = auditRank[after];
  If[auditBetter[beforeRank, afterRank], Print["Rank regression: ", record["Name"]]; Quit[1]];
  report = First[prepared["ItemReports"]];
  If[report["InternalOnShellChannels"] =!= {} || report["InternalOnShellConditions"] =!= {},
    Print["Unexpected internal-shell assumption: ", record["Name"]]; Quit[1]];
  changed = after =!= before;
  AppendTo[auditRows, {record["Name"], StringRiffle[record["Process"], ","],
    auditScore[before], auditScore[after], LeafCount[before], LeafCount[after],
    N[AbsoluteTime[] - started], If[changed, "Ordinary identities only", "Unchanged"],
    record["RestoredMomentumProductDisplays"], Last[beforeRank], Last[afterRank]}];
  AppendTo[auditResults, Join[record, <|"PreparedAmplitude" -> after,
    "BeforeRank" -> beforeRank, "AfterRank" -> afterRank,
    "PreparationDetails" -> prepared,
    "ValidationScope" -> "Algebraic simplification of saved result; independent physical checks are not rerun"|>]];
  If[Mod[Length[auditRows], 10] === 0, Print["Audited ", Length[auditRows], "/132"]],
  {record, auditRecords}];
auditHeaders = {"Name", "Process", "BeforeComplexity", "AfterComplexity", "BeforeLeaves", "AfterLeaves",
  "Seconds", "IdentityScope", "RestoredMomentumProductDisplays", "BeforeRankMetric", "AfterRankMetric"};
Print["Writing audit reports."];
auditCSVField[field_] := "\"" <> StringReplace[If[StringQ[field], field, ToString[field, InputForm]], "\"" -> "\"\""] <> "\"";
auditCSVText = StringRiffle[(StringRiffle[auditCSVField /@ #, ","] &) /@ Prepend[auditRows, auditHeaders], "\n"] <> "\n";
Export[FileNameJoin[{auditDirectory, "SM-four-point-simplification.csv"}], auditCSVText, "Text"];
Export[FileNameJoin[{auditDirectory, "SM-four-point-simplification.wxf"}],
  KeyDrop[#, {"PreparationDetails"}] & /@ auditResults, "WXF"];
If[Environment["HEPCAT_SM_AUDIT_DETAILS"] === "1",
  Export[FileNameJoin[{auditDirectory, "SM-four-point-reduction-details.wxf"}], auditResults, "WXF"]];
Print["Audited ", Length[auditRows], " amplitudes in ", N[AbsoluteTime[] - auditStarted], " seconds."];
Print["Strict improvements: ", Count[auditResults,
  r_Association /; auditBetter[r["AfterRank"], r["BeforeRank"]], {1}]];
Print["Reports: ", auditDirectory];
Quit[0];
