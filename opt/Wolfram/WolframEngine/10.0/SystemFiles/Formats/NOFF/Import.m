(* ::Package:: *)

Begin["System`Convert`OFFDump`"]


ImportExport`RegisterImport[
 "NOFF",
 ImportOFF,
 {
	Automatic 		:> CreateGraphics,
	"Graphics" 		:> CreateGraphics2D,
	"Graphics3D" 	  :> CreateGraphics3D,
	"GraphicsComplex"  :> CreateGC,
	"PolygonObjects"   :> CreatePolygonObjects
 },
 "Sources" -> ImportExport`DefaultSources[{"Common3D", "OFF"}],
 "FunctionChannels"	-> {"Streams"},
 "AvailableElements" -> {"BinaryFormat", "Graphics", "Graphics3D", "GraphicsComplex",
			"InvertNormals", "PolygonColors", "PolygonData", "PolygonObjects",
			"VertexColors", "VertexData", "VertexNormals", "VerticalAxis"},
 "DefaultElement" 	-> Automatic,
 "Options" 		-> {"BinaryFormat", "InvertNormals", "VerticalAxis"},
 "BinaryFormat" -> True
]


End[]
