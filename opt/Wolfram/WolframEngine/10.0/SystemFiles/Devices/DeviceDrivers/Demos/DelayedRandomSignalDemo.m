(* $Id: DelayedRandomSignalDemo.m,v 1.1.2.1 2013/11/15 01:58:08 bakshee Exp $ *)

(* Simulates long measurements. Uses inheritance. *)

BeginPackage["DeviceAPI`Drivers`Demos`DelayedRandomSignalDemo`Dump`"];
Begin["`Private`"];

$delay = .1;

open[_] := open[Null,0]
open[_,delay_] := ($delay = delay; Null)

DeviceAPI`DeviceClassRegister["DelayedRandomSignalDemo",
	"Extends" -> "RandomSignalDemo",
	"ReadFunction" :> ((Pause[$delay];RandomReal[])&),
	"OpenFunction" -> open,
	"Singleton" -> False,
	"DriverVersion" -> 0.001
];

End[];
EndPackage[];
