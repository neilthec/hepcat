(* HEPCAT, Copyright (C) 2026 Neil Christensen. GPL-3.0-or-later. *)
(* Ordinary ConstructAmplitude compared to the publication's worked scores.
   Set HEPCAT_PUBLICATION_CASES to a comma-separated list of catalog keys,
   for example physical/eeAAA/++-,spurious/eeAAAA/++--, or to all.
   HEPCAT_ROOT can point to the checkout when using an external test driver. *)

pubRoot = Environment["HEPCAT_ROOT"];
If[!StringQ[pubRoot] || pubRoot === "", pubRoot = DirectoryName[DirectoryName[DirectoryName[$InputFileName]]]];
Get[FileNameJoin[{pubRoot, "source", "HEPCAT.wl"}]];
$HistoryLength = 0;
pubDir = FileNameJoin[{pubRoot, "models"}];
pubDiagrams = ReadModel[pubDir, 3];
pubPhysical = ReadModel[pubDir, 4];
pubSpurious = ReadModel[pubDir, 5];
pubFeynman = ReadModel[FileNameJoin[{pubRoot,"tests","reference-models","calchep"}], 2];

(* Use the strongest measured catalog, native or earlier worked score. This
   frozen table contains no expression. Only the particle list and models go
   into ConstructAmplitude. *)
pubFile = FileNameJoin[{pubRoot,"tests","benchmarks","publication-qed","complexities.tsv"}];
pubRecords = StringSplit[#, "\t"]& /@ ReadList[pubFile, Record];
pubRows = Rest[pubRecords];
pubIntegerQ[word_] := StringMatchQ[word, DigitCharacter..];
If[Take[First[pubRecords], 6] =!= {"form", "family", "helicity", "catalog_weight", "native_weight", "earlier_weight"} ||
    Length[pubRows] =!= 42 || !AllTrue[pubRows, Length[#] === 9 &&
      AllTrue[#[[{4,5}]], pubIntegerQ] && (#[[6]] === "NA" || pubIntegerQ[#[[6]]])&], Exit[2]];
pubWeights = Association[(StringRiffle[Take[#,3],"/"] -> Min[FromDigits /@ DeleteCases[#[[4;;6]], "NA"]])& /@ pubRows];
If[Length[pubWeights] =!= Length[pubRows], Exit[2]];
pubRequest=Environment["HEPCAT_PUBLICATION_CASES"];
pubSelection=Which[!StringQ[pubRequest] || pubRequest==="",
  {"physical/eeAAA/++-","spurious/eeAAA/++-","physical/eeAAAA/+++-",
   "spurious/eeAAAA/+++-","physical/eeAAAA/++--","spurious/eeAAAA/++--"},
  pubRequest==="all",Keys[pubWeights],True,StringTrim /@ StringSplit[pubRequest,","]];
If[pubSelection==={} || !AllTrue[pubSelection,KeyExistsQ[pubWeights,#]&],Print["Unknown case in ",pubSelection];Exit[2]];
pubFailures={};
Do[
  {pubMode,pubFamily,pubHelicity}=StringSplit[pubKey,"/"];
  pubFermions=Switch[StringCount[pubFamily,"e"],2,{"e","E"},4,{"e","E","E","e"},6,{"e","E","e","E","e","E"}];
  pubProcess=Join[pubFermions,If[pubHelicity==="massive",{},Characters[pubHelicity]/.{"+"->"A.+","-"->"A.-"}]];
  pubFactors=Which[Length[pubProcess]===4,pubDiagrams, Length[pubProcess]===5 || pubMode==="physical",pubPhysical,True,pubSpurious];
  pubBound=pubWeights[pubKey];
  Print["Constructing ",pubKey," with score bound ",pubBound];
  {pubSeconds,pubResult}=AbsoluteTiming[ConstructAmplitude[
    pubProcess,pubDiagrams,pubFactors,"FeynmanModel"->pubFeynman,
    "PolePreference"->If[pubMode==="physical","AllPhysical","AllSpurious"],
    "RandomSeed"->410073,"Output"->"Details","ProgressReporting"->False,"PrintReport"->False]];
  pubPassed=False;
  If[AssociationQ[pubResult] && pubResult["Status"]==="COMPLETE",
    pubFinalChecks=pubResult["Checks"];pubCheckRows=pubFinalChecks["Checks"];
    pubFD=pubFinalChecks["FeynmanCheck"];pubMeasured=SpinorExpressionComplexity[pubResult["Amplitude"]];
    pubPassed=TrueQ[pubMeasured<=pubBound && ListQ[pubCheckRows] && Length[pubCheckRows]>0 &&
      AllTrue[pubCheckRows,#["Status"]==="PASS"&] && AssociationQ[pubFD] && pubFD["Status"]==="PASS" &&
      MemberQ[pubFD["MatchingSigns"],1] && (pubMode=!="physical" || Length[pubFinalChecks["SpuriousChecks"]]===0)];
    Print[pubKey,"  score ",pubMeasured," / ",pubBound,"  leaves ",LeafCount[pubResult["Amplitude"]],
      "  signed Feynman ",pubFD["MatchingSigns"],"  checks ",pubFinalChecks["Summary"],"  seconds ",pubSeconds]];
  If[!pubPassed,AppendTo[pubFailures,pubKey]];
  Clear[pubResult,pubFinalChecks,pubCheckRows,pubFD],{pubKey,pubSelection}];
If[pubFailures==={},Print["HEPCAT_TEST_COMPLETED:publication-qed-construction-tests.wl"];Exit[0],
 Print["Failed publication comparisons ",pubFailures];Exit[1]];
