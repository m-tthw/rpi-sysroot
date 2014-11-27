(* All loading of the paclet's Wolfram Language code should go through this file. *)

(* This If test, and the entire 'else' branch, are temporary until the build machines that make the .mx files have a very
   recent PacletManager. By the time we are building true release candidates for M10, this all should be replaced by a
   single call to PacletManager`Package`loadWolframLanguageCode.
*)
If[Length[DownValues[PacletManager`Package`loadWolframLanguageCode]] > 0,

    PacletManager`Package`loadWolframLanguageCode["Macros", "Macros`", DirectoryName[$InputFileName], "Macros.m",
                   "AutoUpdate" -> True,
                   "AutoloadSymbols" -> {}],

(* else *)

    Macros`Private`symsToProtect =
        Hold[
            Select[Names["Macros`*"] ~Join~ Names["Macros`PackageScope`*"],
                ToExpression[#, InputForm, Function[{sym}, Length[DownValues[sym]] > 0 || Length[SubValues[sym]] > 0, HoldFirst]] &
            ]
        ];
        
    (* If .m files are present, always load them. Otherwise, fall back to .mx file. *)
    If[FileExistsQ[FileNameJoin[{DirectoryName[$InputFileName], "Macros.m"}]],
        (Unprotect[#]; ClearAll[#])& /@ ReleaseHold[Macros`Private`symsToProtect];
        Get[FileNameJoin[{DirectoryName[$InputFileName], "Macros.m"}]];
        (* Protect the appropriate exported symbols. *)
        Macros`Private`attrs = If[TrueQ[System`Private`$buildingMX], {Protected, ReadProtected, Locked}, {Protected, ReadProtected}];
        ToExpression[#, InputForm, Function[sym, SetAttributes[sym, Macros`Private`attrs], HoldFirst]]& /@ ReleaseHold[Macros`Private`symsToProtect],
    (* else *)
        (* Load via .mx file. We need special treatment for dependent packages, to ensure they get loaded in the current session. *)

        (* Needs statements for non-hidden imports of the package go here. You probably shouldn't have any. *)
        (* Needs["NonHiddenPackage`"] *)

        Block[{$ContextPath = {"System`"}},
            (* Clear the autoload defs from the autoload symbols. *)
            (Unprotect[#]; ClearAll[#])& /@ Macros`Private`autoloadSymbols;

            (* Load the .mx file. *)
            Get[FileNameJoin[{DirectoryName[$InputFileName], "Kernel", ToString[$SystemWordLength]<>"Bit", "Macros.mx"}]]
        ]
    ]
];
