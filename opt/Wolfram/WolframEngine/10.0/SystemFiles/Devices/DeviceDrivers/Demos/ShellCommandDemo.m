(* $Id: ShellCommandDemo.m,v 1.1.2.3 2013/12/12 00:07:42 bakshee Exp $ *)

(* A discoverable singleton class that executes a shell command. *)

BeginPackage["DeviceAPI`Drivers`Demos`ShellCommandDemo`Dump`"];

DeviceAPI`DeviceClassRegister["ShellCommandDemo",
	"ReadFunction" -> (ReadList["!"<>#2, Record]&),
	"FindFunction" -> ({{True,{}}}&),
	"ExecuteAsynchronousFunction" -> (Missing["NotAvailable"]&),
	"Singleton" -> True,
	"DriverVersion" -> 0.001
];

EndPackage[];
