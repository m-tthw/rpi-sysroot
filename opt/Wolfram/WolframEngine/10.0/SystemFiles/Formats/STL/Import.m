(* ::Package:: *)

Begin["System`Convert`STLDump`"]


ImportExport`RegisterImport[
  "STL",
  TestSTL,
  {
 	"Graphics3D" 		:> CreateGraphics,
	"GraphicsComplex" 	:> CreateGraphicsComplex,
	"VertexData" 		:> CreateVertexData,
	"PolygonData" 		:> CreatePolygonData,
	"PolygonObjects"	:> CreatePolygonObjects
  },
  "Sources" -> {"Convert`Common3D`", "Convert`STL`"},
  "FunctionChannels" -> {"Streams"},
  "AvailableElements" -> {"BinaryFormat", "Graphics3D", "GraphicsComplex", "PolygonData", "PolygonObjects", "VertexData", "VerticalAxis"},
  "DefaultElement" -> "Graphics3D",
  "Options" -> {"BinaryFormat", "VerticalAxis"},
  "BinaryFormat" -> True
]


End[]
