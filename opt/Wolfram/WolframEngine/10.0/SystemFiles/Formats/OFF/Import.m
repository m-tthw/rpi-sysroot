(* ::Package:: *)

ImportExport`RegisterImport[
  "OFF",
  System`Convert`OFFDump`ImportOFF,
  {
		Automatic			:> System`Convert`OFFDump`CreateGraphics,
		"Graphics"			:> System`Convert`OFFDump`CreateGraphics2D,
		"Graphics3D"		:> System`Convert`OFFDump`CreateGraphics3D,
		"GraphicsComplex"	:> System`Convert`OFFDump`CreateGC,
		"PolygonObjects" 	:> System`Convert`OFFDump`CreatePolygonObjects
  },
  "Sources" -> ImportExport`DefaultSources[{"Common3D", "OFF"}],
  "FunctionChannels" -> {"Streams"},
 "AvailableElements" -> {"BinaryFormat", "Graphics", "Graphics3D", "GraphicsComplex",
			"InvertNormals", "PolygonColors", "PolygonData", "PolygonObjects",
			"VertexColors", "VertexData", "VertexNormals", "VerticalAxis"},
  "DefaultElement" 	 -> Automatic,
  "Options" 		 -> {"BinaryFormat", "InvertNormals", "VerticalAxis"},
  "BinaryFormat"     -> True
]

