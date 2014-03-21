(* $Id: DefaultShellCommandDemo.m,v 1.1.2.1 2013/11/09 01:22:28 bakshee Exp $ *)

(* error handling; default arguments *)

BeginPackage["DeviceAPI`Drivers`Demos`DefaultShellCommandDemo`Dump`"];

Begin["`Private`"];

properties[_][_] = {};

open[_,str:(_String):"date"] := Module[
{
	h = RandomInteger[10^10]
},
	properties[h]["cmnd"] = str;
	h
]

open[_,e_] := (
	Message[DeviceOpen::blnulst,e,2];
	$Failed
)

open[_,args__] := $Failed


read[{_,h_}] := read[Null, properties[h]["cmnd"] ]
read[_,c_String] := ReadList["!"<>c, Record]
read[__] := $Failed


DeviceAPI`DeviceClassRegister["DefaultShellCommandDemo",
	"ReadFunction" -> read,
	"FindFunction" -> DeviceOpen,
	"OpenFunction" -> open,
	"DriverVersion" -> 0.001
];

End[];

EndPackage[];
