(* $Id: AsynchronousDemo.m,v 1.1.2.1 2013/10/10 19:08:48 bakshee Exp $ *)

(* "DriverVersion" -> 0.001 *)


BeginPackage["DeviceAPI`Drivers`Demos`AsynchronousDemo`"];

Begin["`Private`"];

(*----------- modified example from ref/URLFetchAsynchronous ----------*)

properties[_][_] = {};

eventFunction[devHandle_][_, "cookies", data_] := (
	properties[devHandle]["LastAccess"] = Date[];
	properties[devHandle]["Cookies"] = data
)

getProp[devHandle_,prop_] := properties[devHandle][prop]

fetch[handles:{_,devHandle_},url_,opts___?OptionQ] := 
	fetch[handles, url, eventFunction[devHandle], opts]

fetch[_,args__] := URLFetchAsynchronous[args]

		   
(*-----------------------------------------------------------------*)  

DeviceAPI`DeviceClassRegister["AsynchronousDemo", Null ,
	"ReadAsynchronousFunction" -> fetch,
	"NativeProperties" -> {"Cookies", "LastAccess"},
	"GetPropertyFunction" -> getProp,
	"CloseFunction" -> (Quiet[
		properties[ #[[2]] ]["LastAccess"] =.;
		properties[ #[[2]] ]["Cookies"] =.;
	]&)
];

End[];

EndPackage[];
