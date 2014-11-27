(* All loading of the paclet's Wolfram Language code should go through this file. *)

(* Developer maintains this list of symbols.
   autoloadSymbols must agree with the symbols listed in the Kernel extension in the PacletInfo.m file.
*)
Dataset`Private`autoloadSymbols = {
	"System`Dataset", 
	"System`Query", 
	"System`DataForm", 
	"System`FailureAction",
	"System`MissingBehavior",
	"System`PartBehavior",
	"System`ValidateQuery"
}

PacletManager`Package`loadWolframLanguageCode["Dataset", "Dataset`", DirectoryName[$InputFileName], "Assignment.m",
           "AutoUpdate" -> True, "Lock" -> False,
           "AutoloadSymbols" -> Dataset`Private`autoloadSymbols,
           "HiddenImports" -> {"Macros`", "GeneralUtilities`", "TypeSystem`"}
]