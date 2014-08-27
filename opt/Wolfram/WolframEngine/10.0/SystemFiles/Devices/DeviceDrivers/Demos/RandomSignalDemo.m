(* $Id: RandomSignalDemo.m,v 1.1.2.6 2014/01/03 00:01:56 bakshee Exp $ *)

BeginPackage["DeviceAPI`Drivers`Demos`RandomSignalDemo`Dump`"];
Begin["`Private`"];

read[_,"Integer"] := RandomInteger[]
read[_,"Complex"] := RandomComplex[]
read[_,type_:"Real"] := RandomReal[]
read[_,types__] := read[Null,#]&/@{types}

DeviceAPI`DeviceClassRegister["RandomSignalDemo",
	"ReadFunction" -> read,
	"ReadBufferFunction" -> (RandomReal[{-1,1},#2]&),
	"FindFunction" -> ({{}}&),
	"Singleton" -> True,
	"DeviceIconFunction" -> (ListLinePlot[RandomReal[{0, 1}, 40], 
		Frame -> True, FrameTicks -> None, Axes -> False, 
		AspectRatio -> 1
	]&),
	"DriverVersion" -> 0.001
];

End[];
EndPackage[];
