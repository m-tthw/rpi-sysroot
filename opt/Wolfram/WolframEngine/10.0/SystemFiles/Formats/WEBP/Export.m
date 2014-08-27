(* ::Package:: *)

Begin["System`ConvertersDump`"]


ImportExport`RegisterExport[
    "WEBP",
    System`Convert`CommonGraphicsDump`ExportElementsToRasterFormat["WEBP", ##]&,
    "Sources" -> {"Convert`CommonGraphics`"},
	"DefaultElement" -> Automatic,
	"Options" -> {"BitDepth", "ColorSpace"},
	"BinaryFormat" -> True
]


End[]
