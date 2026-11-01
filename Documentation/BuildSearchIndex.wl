(* ::Package:: *)

(* Rebuild Mathematica's native Help search index after editing the reference
   notebooks, or when using a Wolfram version with a different index format.
   This is a documentation-maintenance task, never a package-loading step. *)
Module[{documentationDirectory, pages, indexDirectory, version, index, result, nativeTextFiles},
  documentationDirectory = FileNameJoin[{DirectoryName[$InputFileName], "English"}];
  pages = Sort[FileNames["*.nb", documentationDirectory, Infinity]];
  If[pages === {}, Return[Failure["NoDocumentationPages",
    <|"MessageTemplate" -> "No HEPCAT reference notebooks were found."|>]]];
  Needs["DocumentationSearch`"];
  version = StringTrim[ToString[TextSearch`PackageScope`$CurrentVersion], "."];
  indexDirectory = FileNameJoin[{documentationDirectory, "SearchIndex"}];
  If[!DirectoryQ[indexDirectory], CreateDirectory[indexDirectory]];
  (* Wolfram's own documentation builder suppresses this optional-field warning. *)
  index = Quiet[DocumentationSearch`CreateDocumentationIndex[
    pages, indexDirectory, version, UseWolframLanguageData -> False,
    OverwriteTarget -> True], {CreateSearchIndex::uf}];
  If[MatchQ[index, _SearchIndexObject],
    nativeTextFiles = FileNames["*.wl", FileNameJoin[{indexDirectory, version}]];
    Scan[Function[file, Export[file,
      StringReplace[Import[file, "Text"], RegularExpression["(?m)[ \t]+$"] -> ""], "Text"]], nativeTextFiles]
  ];
  result = If[MatchQ[index, _SearchIndexObject],
    <|"Pages" -> Length[pages], "SearchIndex" -> FileNameJoin[{indexDirectory, version}]|>,
    Failure["DocumentationIndexBuildFailed",
      <|"MessageTemplate" -> "Mathematica could not build the HEPCAT Help search index."|>]];
  If[AssociationQ[result], Print["Indexed ", Length[pages], " HEPCAT Help pages."]];
  result
]
