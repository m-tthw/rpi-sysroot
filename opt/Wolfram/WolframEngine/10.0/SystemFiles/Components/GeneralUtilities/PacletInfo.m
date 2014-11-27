Paclet[
	Name -> "GeneralUtilities",
	Version -> "0.9.2",
	MathematicaVersion -> "9+",
	Description -> "General utilities",
	Loading -> Automatic,
	Extensions -> {
		{"Kernel", Context -> {"GeneralUtilitiesLoader`", "GeneralUtilities`"}, Symbols -> {
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
			
			(* Options for TextString -- I wish these didn't have autoload stubs, but there is no
			other way of getting PacletManager to make these system symbols *)
			"System`AssociationFormat","System`ListFormat",
			"System`BooleanStrings", "System`MissingString",
			"System`TimeFormat", "System`ElidedForms" 
		}}
	}
]
