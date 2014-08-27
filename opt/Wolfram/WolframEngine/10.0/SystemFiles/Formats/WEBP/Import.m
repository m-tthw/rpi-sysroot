(* ::Package:: *)

Begin["System`Convert`CommonGraphicsDump`"]


ImportExport`RegisterImport["WEBP",
	{
		"Graphics" 						:> GetGraphicsElement["WEBP"],
		"Image" 						:> GetImageElement["WEBP"],
		"Data" 							:> GetDataElement["WEBP", All],
		"BitDepth" 						:> GetImageMetaData["WEBP", "BitDepth", All],
		"ColorProfileData" 				:> GetImageMetaData["WEBP", "ColorProfileData", All],
		"ColorSpace" 					:> GetImageMetaData["WEBP", "ColorSpace", All],
		"ImageSize" 					:> GetImageMetaData["WEBP", "ImageSize", All],
		"RGBColorArray" 				:> GetRGBColorArrayElement["WEBP", All],
		"GrayLevels" 					:> GetGrayLevelsElement["WEBP", All],
		"Elements"						:> GetListOfElements["WEBP"],
		GetListOfElements["WEBP"]
	},
	"Sources" -> {"JLink`", "Convert`Exif`", "Convert`CommonGraphics`"},
	"AvailableElements" ->
		{
			"BitDepth", "ColorProfileData", "ColorSpace", "Data",
			"Graphics", "GrayLevels", "Image", "ImageSize", "RGBColorArray"
		},
	"DefaultElement" -> "Image",
	"Options" -> {"BitDepth", "ColorSpace", "ImageSize"},
	"BinaryFormat" -> True
]

End[]
