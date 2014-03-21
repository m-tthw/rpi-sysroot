(* $Id: RandomSignalDemo.m,v 1.1.2.4 2013/11/13 20:24:00 bakshee Exp $ *)

BeginPackage["DeviceAPI`Drivers`Demos`RandomSignalDemo`Dump`"];

DeviceAPI`DeviceClassRegister["RandomSignalDemo",
	"ReadFunction" -> (RandomReal[]&),
	"ReadBufferFunction" -> (RandomReal[{-1,1},#2]&),
	"FindFunction" -> DeviceAPI`DevicePrepare,
	"Singleton" -> True,
	"DeviceIconFunction" -> (ListLinePlot[RandomReal[{0, 1}, 40], 
		Frame -> True, FrameTicks -> None, Axes -> False, 
		AspectRatio -> 1
	]&),
	"DriverVersion" -> 0.001
];

EndPackage[];
