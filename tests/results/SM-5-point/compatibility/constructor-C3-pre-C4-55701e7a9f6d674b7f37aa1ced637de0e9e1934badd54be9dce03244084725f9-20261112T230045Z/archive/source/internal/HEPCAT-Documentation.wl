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
    usageFile, usageRegistry, existingNames},
  packageDirectory = ParentDirectory[ParentDirectory[ExpandFileName[DirectoryName[$InputFileName]]]];
  documentationDirectory = FileNameJoin[{packageDirectory, "Documentation", "English"}];
  pacletInformation = FileNameJoin[{packageDirectory, "PacletInfo.wl"}];
  If[FileExistsQ[pacletInformation] && DirectoryQ[documentationDirectory],
    Quiet[Check[PacletDirectoryLoad[packageDirectory], $Failed]]
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
