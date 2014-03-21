(* :Title: MakeMaster *)


(* :Context: ProgrammingInMathematica`MakeMaster` *)

(* :Author: Roman E. Maeder *)

(* :Summary:
   generate init.m autoload packages
 *)

(* :Copyright: © 1996 by Roman E. Maeder *)

(* :Package Version: 2.0 *)

(* :Mathematica Version: 3.0 *)

(* :History:
   2.0 for Programming in Mathematica, 3rd ed.
*)

(* :Keywords: autoloading, init.m, master package *)

(* :Sources:
   Roman E. Maeder. Programming in Mathematica, 3rd ed. Addison-Wesley, 1996.
*)

(* :Discussion:
   See Section 2.5 of "Programming in Mathematica"
*)

BeginPackage["ProgrammingInMathematica`MakeMaster`"]

MakeMaster::usage = "MakeMaster[file, {contexts..}] writes DeclarePackage
	commands for all given contexts to file. MakeMaster[file, directory]
	writes a master file for all packages in directory. Directory is
	interpreted relative to $Path. The current directory can be given as \".\"."
PackageContext::usage = "PackageContext -> context is an option of
	MakeMaster that gives the context to use for the master package."

Begin["`Private`"]

(* possibly OS-dependent constants *)
$packageExtension = ".m"
$contextMark = StringTake[$Context, -1]
$master = "init"
$masterName = $master <> $packageExtension
$masterFormat = InputForm    (* format for writing master file *)
$oldMasterName = "Master" <> $packageExtension	(* V2.2 files *)

Options[MakeMaster] = {
	PackageContext :> $master <> $contextMark
}

MakeMaster[ filename_String:$masterName, contexts_List, opts___?OptionQ ] :=
    Module[{loaded, file},
        Needs /@ contexts; (* load them *)
        loaded = Union @@
            Function[cont, Select[$ContextPath, StringMatchQ[#, cont]&]] /@
            contexts;
        file = OpenWrite[filename, FormatType -> $masterFormat];
        If[ file === $Failed, Return[file] ];
        (* preamble *)
        With[{cont = PackageContext /. {opts} /. Options[MakeMaster]},
            Write[file, OutputForm["(* Created by MakeMaster *)"]];
            Write[file, Unevaluated[BeginPackage[cont]]];
            Write[file, Unevaluated[EndPackage[]]];
            Write[file, OutputForm[""]];
        ];
        makeMaster[ file, #]& /@ loaded;
        Write[file, Null];
        Close[file]
    ]

MakeMaster[ filename_String:$masterName, directory_String ] :=
    Module[{contexts, cont},
        contexts = contextsForDirectory[directory];
        If[contexts === $Failed, Return[contexts] ];
        cont = If[ directory == "" || directory == ".",
                   $masterName,
                   directory <> $PathnameSeparator <> $masterName ];
        MakeMaster[ filename, contexts,
            PackageContext -> filenameToContext[cont] ]
    ]

filenameToContext[filename_String] :=
    Module[{cont},
    cont = StringReplace[filename, {$PathnameSeparator -> $contextMark,
                                    $packageExtension -> $contextMark}];

    If[ StringTake[cont, -1] != $contextMark, cont = cont <> $contextMark ];
    cont
    ]

makeMaster[ file_, context_String ] :=
    With[{names = Names[context <> "**"]},
        If[ names =!= {},
            Write[file, Unevaluated[DeclarePackage[context, names]]] ]
    ]

(* current directory is special *)
contextsForDirectory["" | "."] :=
    Module[{files},
        files = Complement[FileNames["*" <> $packageExtension], {$masterName}];
        filenameToContext /@ files
    ]

contextsForDirectory[directory_String] :=
    Module[{absdir, files},
        absdir = FileNames[directory, $Path];
        If[ Length[absdir] == 0, Message[MakeMaster::nodir, directory];
                                 Return[$Failed] ];
        If[ Length[absdir] > 1,  Message[MakeMaster::sdir, directory] ];
        absdir = First[absdir];
        SetDirectory[ ParentDirectory[absdir] ];
        files = Complement[FileNames["*" <> $packageExtension, directory],
                           {directory <> $PathnameSeparator <> $masterName,
                            directory <> $PathnameSeparator <> $oldMasterName}];
        ResetDirectory[];
        filenameToContext /@ files
    ]

MakeMaster::nodir = "No directory matching `` found on $Path."
MakeMaster::sdir = "Warning: more than one directory matching `` found on $Path."

End[]

Protect[MakeMaster]

EndPackage[]
