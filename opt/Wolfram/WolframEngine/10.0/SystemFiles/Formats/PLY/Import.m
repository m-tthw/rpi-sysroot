(* ::Package:: *)

Begin["System`Convert`PLYDump`"]


ImportExport`RegisterImport[
  "PLY",
  ImportPLY,
  {
	"Graphics3D" 		:> CreateGraphics,
	"GraphicsComplex"    :> CreateGC,
	"PolygonObjects" 	:> CreatePolygonObjects,
	"LineObjects" 	   :> CreateLineObjects
  },
  "Sources" 		  -> {"Convert`Common3D`", "Convert`PLY`"},
  "FunctionChannels"  -> {"Streams"},
  "AvailableElements" -> {"BinaryFormat", "Comments", "DataFormat", "Graphics3D",
			"GraphicsComplex", "InvertNormals", "LineData", "LineObjects", "PolygonData",
			"PolygonObjects", "UserExtensions", "VertexColors",	"VertexData", "VertexNormals", "VerticalAxis"},
  "DefaultElement"    -> "Graphics3D",
  "Options" 		  -> {"BinaryFormat", "DataFormat", "Comments", "VerticalAxis", "InvertNormals"},
  "BinaryFormat" 	  -> True
]


End[]
