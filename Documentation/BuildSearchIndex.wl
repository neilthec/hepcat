(* ::Package:: *)

(* Rebuild Mathematica's native Help search index after editing the reference
   notebooks, or when using a Wolfram version with a different index format.
   This is a documentation-maintenance task, never a package-loading step. *)

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
