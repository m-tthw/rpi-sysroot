(* Mathematica Package *)

(* Created by the Wolfram Workbench Oct 19, 2013 *)

BeginPackage["RaspiCam`"]
(* Exported symbols added here with SymbolName::usage *) 

Begin["`Private`"]
(* Implementation of the package *)

If[$SystemID === "Linux-ARM",
	DeviceAPI`DeviceClassRegister["RaspiCam", Null,
		"FindFunction" -> DeviceOpen,
	 	"ReadFunction" -> RaspiCurrentImage
	 ];
	
	RaspiCurrentImage[{iHandle_, dHandle_}, {width_Integer,height_Integer},options_String]:= Import[ "!raspistill -n -w "<> ToString[ width] <> " -h " <> ToString[ height] <> " -t 0 -o -" <> " " <> ToString[options], "JPG"];
	 
	RaspiCurrentImage[{iHandle_, dHandle_}, {width_Integer,height_Integer}]:=RaspiCurrentImage[ {iHandle, dHandle}, {width,height},""];
	 
	RaspiCurrentImage[{iHandle_, dHandle_}, width_Integer,height_Integer]:=RaspiCurrentImage[ {iHandle, dHandle}, {width,height},""];
	
	RaspiCurrentImage[{iHandle_, dHandle_}]:= RaspiCurrentImage[ {iHandle, dHandle}, {320,240}];
]




End[]

EndPackage[]
