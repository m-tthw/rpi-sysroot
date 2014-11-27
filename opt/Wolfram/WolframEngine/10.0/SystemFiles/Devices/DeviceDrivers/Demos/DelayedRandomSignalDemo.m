(* $Id: DelayedRandomSignalDemo.m,v 1.8 2014/05/08 22:52:40 bakshee Exp $ *)

(* Simulates long measurements. Uses inheritance. *)

BeginPackage["DeviceAPI`Drivers`Demos`DelayedRandomSignalDemo`Dump`"];
Begin["`Private`"];

$delay = .1;

open[_] := open[Null,0]
open[_,delay_] := ($delay = delay; CreateUUID[])

DeviceFramework`DeviceClassRegister["DelayedRandomSignalDemo", "RandomSignalDemo",
	"ReadFunction" :> (
		(
			Pause[$delay];
			(* the parent's function *)
			DeviceFramework`DeviceDriverFunction["RandomSignalDemo", "ReadFunction"][##]
		)&
	),
	"OpenFunction" -> open,
	"Singleton" -> False,
	"FindFunction" -> ({}&),
	"DriverVersion" -> 0.001
];

End[];
EndPackage[];
