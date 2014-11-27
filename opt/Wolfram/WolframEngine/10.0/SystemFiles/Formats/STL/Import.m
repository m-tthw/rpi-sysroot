(* ::Package:: *)

Begin["System`Convert`STLDump`"]


ImportExport`RegisterImport[
	"STL",
	{
		"Elements"			:> GetElements,
		"MeshRegion"		:> CreateMeshRegion,
		"ElementMesh"		:> CreateElementMesh,
		"Graphics3D"		:> CreateGraphics,
		"GraphicsComplex" 	:> CreateGraphicsComplex,
		"VertexData"		:> CreateVertexData,
		"PolygonData"		:> CreatePolygonData,
		"PolygonObjects"	:> CreatePolygonObjects,
		CreateGraphics
	}
	,"AvailableElements" -> {"Elements", "BinaryFormat", 
		"MeshRegion", "ElementMesh", "Graphics3D", "GraphicsComplex", 
		"PolygonData", "PolygonObjects", "VertexData", "VerticalAxis"}
	,"BinaryFormat" -> True
	,"DefaultElement" -> "Graphics3D"
	,"FunctionChannels" -> {"Streams"}
	,"Options" -> {"BinaryFormat", "VerticalAxis"}
	,"Sources" -> {"Convert`Common3D`", "Convert`STL`"}
]



End[]
