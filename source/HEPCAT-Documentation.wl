(* ::Package:: *)

(* ::Section::Closed:: *)
(*Built-In Help Registration*)

(* Register the local documentation with Mathematica's paclet manager. This
   enables Documentation Center links and symbol help without copying files to
   the Mathematica installation, opening a window, or changing the kernel API.
   A source-only checkout without documentation remains usable. *)
Module[{packageDirectory, documentationDirectory, pacletInformation,
    usageFile, usageRegistry, existingNames},
  packageDirectory = ParentDirectory[ExpandFileName[DirectoryName[$InputFileName]]];
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
