(* $Id: ShellCommandDemo.m,v 1.1.2.1 2013/11/02 03:19:53 bakshee Exp $ *)

(* A discoverable singleton class that executes a shell command. *)

BeginPackage["DeviceAPI`Drivers`Demos`ShellCommandDemo`Dump`"];

DeviceAPI`DeviceClassRegister["ShellCommandDemo",
	"ReadFunction" -> (ReadList["!"<>#2, Record]&),
	"FindFunction" -> DeviceOpen,
	"Singleton" -> True,
	"DriverVersion" -> 0.001
];

EndPackage[];
