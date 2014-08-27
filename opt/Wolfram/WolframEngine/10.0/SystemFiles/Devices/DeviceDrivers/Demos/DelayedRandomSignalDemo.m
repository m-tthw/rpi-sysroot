(* $Id: DelayedRandomSignalDemo.m,v 1.1.2.3 2013/11/23 01:01:55 bakshee Exp $ *)

(* Simulates long measurements. Uses inheritance. *)

BeginPackage["DeviceAPI`Drivers`Demos`DelayedRandomSignalDemo`Dump`"];
Begin["`Private`"];

$delay = .1;

open[_] := open[Null,0]
open[_,delay_] := ($delay = delay; Null)

DeviceAPI`DeviceClassRegister["DelayedRandomSignalDemo", "RandomSignalDemo",
	"ReadFunction" :> (
		(
			Pause[$delay];
			(* call the parent's function *)
			DeviceAPI`DeviceDriverFunction["RandomSignalDemo", "ReadFunction"][]
		)&
	),
	"OpenFunction" -> open,
	"Singleton" -> False,
	"DriverVersion" -> 0.001
];

End[];
EndPackage[];
