(* All loading of the paclet's Wolfram Language code should go through this file. *)

(* Developer maintains this list of symbols.
   autoloadSymbols must agree with the symbols listed in the Kernel extension in the PacletInfo.m file.
*)
GeneralUtilities`Private`autoloadSymbols = {
            "System`BooleanQ",
            "System`DeleteMissing",
            "System`DisjointQ", 
            "System`IntersectingQ", 
            "System`SubsetQ",
            "System`Failure",
            "System`PowerRange",
            "System`CountDistinct",
            "System`CountDistinctBy",
            "System`DeleteDuplicatesBy",
            "System`Pivot",
            "System`TextString",
            "System`IndexBy",
            "System`AssociationMap",
            
			"System`AssociationFormat","System`ListFormat",
			"System`BooleanStrings", "System`MissingString",
			"System`TimeFormat", "System`ElidedForms" 

        }
        
        

PacletManager`Package`loadWolframLanguageCode["GeneralUtilities", "GeneralUtilities`", DirectoryName[$InputFileName], "Failure.m",
           "AutoUpdate" -> False, "ForceMX" -> True, "Lock" -> False, 
           "AutoloadSymbols" -> GeneralUtilities`Private`autoloadSymbols,
           "HiddenImports" -> {"Macros`"}
]