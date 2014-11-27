(* All loading of the paclet's Wolfram Language code should go through this file. *)

(* Developer maintains this list of symbols.
   autoloadSymbols must agree with the symbols listed in the Kernel extension in the PacletInfo.m file.
*)

PacletManager`Package`loadWolframLanguageCode[
	"Forms", 
	"Forms`", 
	DirectoryName[$InputFileName], 
	"Primitives.m",
	"AutoUpdate" -> True,
    "ForceMX" -> False, 
    "Lock" -> False,
	"AutoloadSymbols" -> {
		"System`FormObject", 
		"System`FormTheme", 
		"System`FormFunction",
		"System`FormLayoutFunction", 
		"System`AppearanceRules", 
		"System`APIFunction"
	},
	"HiddenImports" -> {"GeneralUtilities`"}
]