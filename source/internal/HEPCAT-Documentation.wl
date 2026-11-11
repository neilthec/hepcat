(* ::Package:: *)

(* Connect this checkout's documentation to Mathematica's Help system.
   The module registers the local paclet and fills missing usage descriptions
   from the documentation's generated symbol registry.
   Loading documentation also supports a checkout containing source files alone. *)

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



(* ::Section::Closed:: *)
(*Built-In Help Registration*)

(* Register the local documentation with Mathematica's paclet manager. This
   enables Documentation Center links and symbol help without copying files to
   the Mathematica installation, opening a window, or changing the kernel API.
   A source-only checkout without documentation remains usable. *)
Module[{packageDirectory, documentationDirectory, pacletInformation,
    usageFile, usageRegistry, existingNames, documentationRoot,
    documentationBundle, packedDocumentationRoot, managedCacheRoot, obsoleteRoots},
  packageDirectory = ParentDirectory[ParentDirectory[ExpandFileName[DirectoryName[$InputFileName]]]];
  documentationRoot = packageDirectory;
  documentationDirectory = FileNameJoin[{documentationRoot, "Documentation", "English"}];
  documentationBundle = FileNameJoin[{packageDirectory, "Documentation", "HEPCAT-Documentation.zip"}];

  (* The publication archive keeps the full native Help and its editable source
     together. Expand it outside the checkout, so read-only installations work.
     A content-addressed cache is accepted only after every manifest entry has
     its declared size and SHA-256. Loading never evaluates example notebooks. *)
  packedDocumentationRoot[archive_String] := Module[
    {digest, cacheParent, cacheDirectory, markerName, safePathQ,
      manifestQ, completeQ, archiveNames, stage = None, lockDirectory,
      lockOwned = False, previous = None, marker, result, catchTag,
      manifest, createResult, unpackResult},
    digest = Quiet[Check[FileHash[archive, "SHA256", "HexString"], $Failed]];
    If[!StringQ[digest], Return[$Failed]];
    cacheParent = FileNameJoin[{$UserBaseDirectory, "ApplicationData", "HEPCAT",
      "Documentation", digest}];
    cacheDirectory = FileNameJoin[{cacheParent, "Contents"}];
    lockDirectory = FileNameJoin[{cacheParent, ".publish-lock"}];
    markerName = ".hepcat-documentation-complete";
    safePathQ[path_String] := StringLength[path] > 0 &&
      !StringStartsQ[path, "/"] && !StringContainsQ[path, {"\\", ":"}] &&
      !StringEndsQ[path, "/"] &&
      FreeQ[StringSplit[path, "/"], "" | "." | ".."] &&
      (path === "PacletInfo.wl" || StringStartsQ[path, "Documentation/"]);
    safePathQ[_] := False;
    manifestQ[value_] := AssociationQ[value] &&
      Lookup[value, "format", None] === "HEPCATDocumentationBundle-1" &&
      With[{items = Lookup[value, "files", None]},
        ListQ[items] && items =!= {} && AllTrue[items,
          AssociationQ[#] && Sort[Keys[#]] === {"bytes", "path", "sha256"} &&
            safePathQ[#["path"]] && IntegerQ[#["bytes"]] && #["bytes"] >= 0 &&
            StringQ[#["sha256"]] &&
            StringMatchQ[#["sha256"], RegularExpression["[0-9a-f]{64}"]] &] &&
          DuplicateFreeQ[Lookup[items, "path"]]];
    completeQ[directory_String] := TrueQ[Quiet[Check[Module[{data, entries, markerPath},
      If[!DirectoryQ[directory], Return[False]];
      data = Import[FileNameJoin[{directory, "BUNDLE-MANIFEST.json"}], "RawJSON"];
      If[!manifestQ[data], Return[False]];
      markerPath = FileNameJoin[{directory, markerName}];
      If[!FileExistsQ[markerPath] ||
          StringTrim[Import[markerPath, "Text"]] =!= digest <> " " <>
            FileHash[FileNameJoin[{directory, "BUNDLE-MANIFEST.json"}], "SHA256", "HexString"],
        Return[False]];
      entries = data["files"];
      AllTrue[entries, With[{path = FileNameJoin[Prepend[StringSplit[#["path"], "/"], directory]]},
        FileExistsQ[path] && !DirectoryQ[path] && FileByteCount[path] === #["bytes"] &&
          FileHash[path, "SHA256", "HexString"] === #["sha256"]] &]
    ], False]]];
    If[completeQ[cacheDirectory], Return[cacheDirectory]];
    If[!DirectoryQ[cacheParent],
      Quiet[Check[CreateDirectory[cacheParent, CreateIntermediateDirectories -> True], Null]]];
    If[!DirectoryQ[cacheParent], Return[$Failed]];
    archiveNames = TimeConstrained[Quiet[Check[Import[archive, "FileNames"], $Failed]], 60, $Failed];
    If[!ListQ[archiveNames] || !DuplicateFreeQ[archiveNames] ||
        !MemberQ[archiveNames, "BUNDLE-MANIFEST.json"] ||
        !AllTrue[DeleteCases[archiveNames, "BUNDLE-MANIFEST.json"], safePathQ], Return[$Failed]];
    catchTag = Unique["documentationBundle"];
    result = CheckAbort[Catch[
      stage = Quiet[Check[CreateDirectory[FileNameJoin[{cacheParent,
        ".prepare-" <> CreateUUID[]}]], $Failed]];
      If[!StringQ[stage], Throw[$Failed, catchTag]];
      unpackResult = Quiet[Check[ExtractArchive[archive, stage], $Failed]];
      If[unpackResult === $Failed, Throw[$Failed, catchTag]];
      manifest = Quiet[Check[Import[FileNameJoin[{stage, "BUNDLE-MANIFEST.json"}], "RawJSON"], $Failed]];
      If[!manifestQ[manifest] ||
          Sort[Lookup[manifest["files"], "path"]] =!= Sort[DeleteCases[archiveNames, "BUNDLE-MANIFEST.json"]],
        Throw[$Failed, catchTag]];
      marker = digest <> " " <>
        FileHash[FileNameJoin[{stage, "BUNDLE-MANIFEST.json"}], "SHA256", "HexString"];
      Export[FileNameJoin[{stage, markerName}], marker, "Text"];
      If[!completeQ[stage], Throw[$Failed, catchTag]];

      (* Extraction is private. Only the short final publish takes a lock, so
         concurrent kernels either publish one complete tree or reuse it. *)
      Do[
        If[completeQ[cacheDirectory], Throw[cacheDirectory, catchTag]];
        createResult = Quiet[Check[CreateDirectory[lockDirectory], $Failed]];
        If[StringQ[createResult], lockOwned = True; Break[]];
        If[DirectoryQ[lockDirectory] && TrueQ[Quiet[Check[
            AbsoluteTime[] - AbsoluteTime[FileDate[lockDirectory]] > 60, False]]],
          Quiet[Check[DeleteDirectory[lockDirectory], Null]]];
        Pause[0.05], {40}];
      If[!lockOwned, Throw[$Failed, catchTag]];
      If[completeQ[cacheDirectory], Throw[cacheDirectory, catchTag]];
      If[DirectoryQ[cacheDirectory],
        previous = FileNameJoin[{cacheParent, ".incomplete-" <> CreateUUID[]}];
        If[Quiet[Check[RenameDirectory[cacheDirectory, previous], $Failed]] === $Failed,
          Throw[$Failed, catchTag]]];
      If[Quiet[Check[RenameDirectory[stage, cacheDirectory], $Failed]] === $Failed,
        Throw[$Failed, catchTag]];
      stage = None;
      If[completeQ[cacheDirectory], cacheDirectory, $Failed], catchTag],
      If[StringQ[stage] && DirectoryQ[stage], Quiet[DeleteDirectory[stage, DeleteContents -> True]]];
      If[lockOwned && DirectoryQ[lockDirectory], Quiet[DeleteDirectory[lockDirectory]]];
      Abort[]];
    If[StringQ[stage] && DirectoryQ[stage], Quiet[Check[DeleteDirectory[stage, DeleteContents -> True], Null]]];
    If[StringQ[previous] && DirectoryQ[previous], Quiet[Check[DeleteDirectory[previous, DeleteContents -> True], Null]]];
    If[lockOwned && DirectoryQ[lockDirectory], Quiet[Check[DeleteDirectory[lockDirectory], Null]]];
    result
  ];

  If[!DirectoryQ[documentationDirectory] && FileExistsQ[documentationBundle],
    With[{packedRoot = packedDocumentationRoot[documentationBundle]},
      If[StringQ[packedRoot], documentationRoot = packedRoot;
        documentationDirectory = FileNameJoin[{documentationRoot, "Documentation", "English"}]]]
  ];
  pacletInformation = FileNameJoin[{documentationRoot, "PacletInfo.wl"}];
  If[FileExistsQ[pacletInformation] && DirectoryQ[documentationDirectory],
    (* PacletDirectoryLoad keeps earlier registrations with the same version.
       Retire this loader's older cache roots before selecting the new bundle;
       installed third-party paclets and unrelated checkouts are left alone. *)
    managedCacheRoot = ExpandFileName[FileNameJoin[{$UserBaseDirectory,
      "ApplicationData", "HEPCAT", "Documentation"}]];
    obsoleteRoots = Select[Quiet[Check[(#["Location"] &) /@ PacletFind["HEPCAT"], {}]],
      StringQ[#] && ExpandFileName[#] =!= ExpandFileName[documentationRoot] &&
        (StringStartsQ[ExpandFileName[#], managedCacheRoot <> $PathnameSeparator] ||
          ExpandFileName[#] === ExpandFileName[packageDirectory]) &];
    Scan[(Quiet[Check[PacletDirectoryUnload[#], Null]] &), DeleteDuplicates[obsoleteRoots]];
    Quiet[Check[PacletDirectoryLoad[documentationRoot], $Failed]]
  ];

  (* Legacy symbols often have no ?symbol description. The registry contains
     strings only. Keep richer descriptions already defined by the package and
     do not create symbols or evaluate any documentation example. *)
  usageFile = FileNameJoin[{documentationDirectory, "SymbolUsage.wl"}];
  If[FileExistsQ[usageFile],
    usageRegistry = Quiet[Check[Get[usageFile], <||>]];
    existingNames = (If[StringStartsQ[#, "ConstructiveDiagrams`"], #,
      "ConstructiveDiagrams`" <> #] &) /@ Names["ConstructiveDiagrams`*"];
    If[AssociationQ[usageRegistry],
      KeyValueMap[
        Function[{name, text},
          If[MemberQ[existingNames, name] && StringQ[text] && StringLength[text] > 0,
            With[{newUsage = text},
              ToExpression[name, InputForm,
                Function[existingSymbol,
                  Module[{oldUsage, wasProtected},
                    oldUsage = MessageName[existingSymbol, "usage"];
                    If[!StringQ[oldUsage] ||
                        MemberQ[{"", "Check this out!"}, StringTrim[oldUsage]],
                      wasProtected = MemberQ[Attributes[existingSymbol], Protected];
                      If[wasProtected, Unprotect[existingSymbol]];
                      Quiet[Check[MessageName[existingSymbol, "usage"] = newUsage, Null]];
                      If[wasProtected, Protect[existingSymbol]]
                    ]
                  ], HoldAllComplete]
              ]
            ]
          ]
        ], usageRegistry]
    ]
  ];
  Null
]
