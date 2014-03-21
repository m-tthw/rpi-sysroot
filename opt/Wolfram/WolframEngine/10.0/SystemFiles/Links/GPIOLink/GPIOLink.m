(* Mathematica Package *)

(* Created by the Wolfram Workbench Sep 18, 2013 *)
(* $Id: GPIOLink.m,v 1.8 2013/10/24 23:18:57 lambertc Exp $ *)

BeginPackage["GPIOLink`"]
(* Exported symbols added here with SymbolName::usage *) 

(* Polling *)
GPIOPinConnect::usage = "GPIOPinConnect[pin, direction] exports the GPIO pin and sets its direction as \"Input\" or \"Output\".";
GPIOPinDisconnect::usage = "GPIOPinDisconnect[pinObj] disconnects and unexports the pin corresponding to pinObj.";
GPIORead::usage = "GPIORead[pinObj] reads from the pin corresponding to pinObj.";
GPIOWrite::usage = "GPIOWrite[pinObj, value] writes the value (0 or 1) to the pin corresponding to pinObj.";
GPIOPinObject::usage = "GPIOPinObject represents a connected pin.";
GPIOPins::usage = "GPIOPins[] lists the GPIOPinObjects for connected pins.";


(* Asynchronous *)
CreateGPIOReadAsynchronousTask::usage = "CreateGPIOReadAsynchronousTask[ pinObj, options] creates an AsynchronousTaskObject for reading from the GPIO pin via asynchronous callback.";
CreateGPIOWriteAsynchronousTask::usage = "CreateGPIOWriteAsynchronousTask[ pinObj, options]  creates an AsynchronousTaskObject for writing asynchronously to the GPIO pin."
CurrentGPIOData::usage = "CurrentGPIOData[asyncObj] returns the latest captured GPIOData from an asynchronous read task.";
GPIOWriteAsynchronous::usage = "GPIOWriteAsynchronous[asyncObj, data] writes data of 0's and 1's to the asynchronous write task.";

(* Messages *)
GPIOPinConnect::inuse = "The GPIO pin `1` has already been connected as a `2` pin."
GPIORead::mode = GPIOWrite::mode = "The GPIO pin `1` was set for `2` mode.  `3` was expected.";
GPIOPinDisconnect::gpiopinobj = "GPIO pin object `1` is invalid and is not connected."

Begin["`Private`"]
(* Implementation of the package *)

$packageFile = $InputFileName;
$libName = Switch[ $SystemID,
	"Linux-ARM",
		"GPIOLink.so"
]

$adapterLib = FileNameJoin[{FileNameTake[$packageFile, {1,-2}], "LibraryResources", $SystemID, $libName}];
$adapterInitialized;

$pinObjects = {};

loadAdapter[]:= 
(
	If[!ValueQ@$adapterInitialized,
		lfGPIOExportPin = LibraryFunctionLoad[ $adapterLib, "GPIOExportPin", { Integer}, Integer];
		lfGPIOUnexportPin = LibraryFunctionLoad[ $adapterLib, "GPIOUnexportPin", { Integer}, Integer];
		lfGPIOSetDirection = LibraryFunctionLoad[ $adapterLib, "GPIOSetDirection", {Integer, Integer}, Integer];
		lfGPIOGetValue = LibraryFunctionLoad[ $adapterLib, "GPIOGetValue", { Integer}, Integer];
		lfGPIOSetValue = LibraryFunctionLoad[ $adapterLib, "GPIOSetValue", { Integer, Integer}, Integer];
		lfGPIOAsyncReadCreate = LibraryFunctionLoad[ $adapterLib, "GPIOAsyncReadCreate", { Integer, Integer, Integer}, Integer];
		lfGPIOAsyncWriteCreate = LibraryFunctionLoad[ $adapterLib, "GPIOAsyncWriteCreate", { Integer, Integer, Integer}, Integer];
		lfGPIOAsyncWrite = LibraryFunctionLoad[ $adapterLib, "GPIOAsyncWrite", { Integer, { Integer, 1, "Manual"}}, Integer];
		
		$adapterInitialized = True;
	]
)

$$enums = {
	"Input" -> 0,
	"Output" -> 1
}

GPIOPinConnect[ pin_Integer, mode:( "Input" | "Output")]:=
Module[{ obj, existingObj},
	loadAdapter[];
	lfGPIOExportPin[ pin];
	lfGPIOSetDirection[ pin, mode /. $$enums];
	obj = GPIOPinObject[ pin, mode];
	If[ pinAlreadyConnected[ pin],
		existingObj = getPinObject[ pin];
		Message[GPIOPinConnect::inuse, pin, getPinObjectData[ existingObj, "Direction"]];
		Return@$Failed;,
		(* else *)
		AppendTo[ $pinObjects, obj];
		Return[ obj];
	];
]

GPIOPins[]:= Return@$pinObjects

pinAlreadyConnected[ pin_Integer]:= MemberQ[ $pinObjects[[ ;;,1]], pin]

getPinObject[ pin_Integer]:= 
If[ pinAlreadyConnected[ pin],
	Return[ $pinObjects[[ First@Flatten@Position[ $pinObjects[[;;, 1]], pin]]]];,
	Return@$Failed;
]

GPIOPinDisconnect[ obj_GPIOPinObject]:=
Module[{ pin, result},
	loadAdapter[];
	pin = getPinObjectData[ obj, "PinNumber"];
	If[ MemberQ[ $pinObjects, obj],
		$pinObjects = DeleteCases[ $pinObjects, obj];
		result = lfGPIOUnexportPin[ pin];,
		(* else error *)
		Message[ GPIOPinDisconnect::gpiopinobj, obj];
		Return@$Failed;
	]
]

GPIORead[ obj_GPIOPinObject]:= 
Module[{ pin, mode},
	loadAdapter[];
	pin = getPinObjectData[ obj, "PinNumber"];
	mode = getPinObjectData[ obj, "Direction"];
	If[ mode =!= "Input", 
		Message[ GPIORead::mode, pin, mode, "Input"];
		Return@$Failed;
	];
	Return[ lfGPIOGetValue[ pin]];
]

GPIOWrite[ obj_GPIOPinObject, value:(0 | 1)]:=
Module[{ pin, mode},
	loadAdapter[];
	pin = getPinObjectData[ obj, "PinNumber"];
	mode = getPinObjectData[ obj, "Direction"];
	If[ mode =!= "Output", 
		Message[ GPIOWrite::mode, pin, mode, "Output"];
		Return@$Failed;
	];
	Return[ lfGPIOSetValue[ pin, value]];
]

Options[ CreateGPIOReadAsynchronousTask] = 
{
	"DataHandler" -> None,
	"SampleRate" -> 1000,
	"SampleCount" -> 50
}

CreateGPIOReadAsynchronousTask[ obj_GPIOPinObject, opts:OptionsPattern[]]:=
Module[{ pin, mode, asyncObj, callback, sampleRate},
	loadAdapter[];
	pin = getPinObjectData[ obj, "PinNumber"];
	mode = getPinObjectData[ obj, "Direction"];
	callback = OptionValue@"DataHandler";
	sampleRate = OptionValue["SampleRate"] /. {Infinity -> -1};
	If[ mode =!= "Input", 
		Message[ CreateGPIOReadAsynchronousTask::mode, pin, mode, "Input"];
		Return@$Failed;
	];
	asyncObj = Internal`CreateAsynchronousTask[ 
		lfGPIOAsyncReadCreate, 
		{ pin, sampleRate, OptionValue@"SampleCount"}, 
		( gpioReadAsyncCallback[#1, #2, #3]; callback[#1, #2, #3])&,
		"TaskDetail" -> { obj}];
	Return@asyncObj;
]

gpioReadAsyncCallback[ asyncObj_AsynchronousTaskObject, eventType_, eventData_]:= 
Module[{ pin, pinObj},
	pinObj = getAsynchronousTaskObjectData[ asyncObj, "PinObject"];
	pin = getPinObjectData[ pinObj, "PinNumber"];
	Devices`Developer`SetStringVariable["GPIOLink`Private`$currentGPIOData", ToString@pin, eventData[[1]]];
]

CurrentGPIOData[ asyncObj_AsynchronousTaskObject]:= 
Module[{ pin, pinObj},
	pinObj = getAsynchronousTaskObjectData[ asyncObj, "PinObject"];
	pin = getPinObjectData[ pinObj, "PinNumber"];
	Return[ Symbol[ "GPIOLink`Private`$currentGPIOData" <> ToString@pin]];
]

Options[ CreateGPIOWriteAsynchronousTask] = 
{
	"DataHandler" -> None,
	"SampleRate" -> 1000,
	"SampleCount" -> 50
}

CreateGPIOWriteAsynchronousTask[ obj_GPIOPinObject, opts:OptionsPattern[]]:=
Module[{ pin, mode, asyncObj, writeDoneCallback, sampleRate},
	loadAdapter[];
	pin = getPinObjectData[ obj, "PinNumber"];
	mode = getPinObjectData[ obj, "Direction"];
	writeDoneCallback = OptionValue@"DataHandler";
	If[ mode =!= "Output", 
		Message[ CreateGPIOWriteAsynchronousTask::mode, pin, mode, "Output"];
		Return@$Failed;
	];
	sampleRate = OptionValue["SampleRate"] /. {Infinity -> -1};
	asyncObj = Internal`CreateAsynchronousTask[ 
		lfGPIOAsyncWriteCreate, 
		{ pin, sampleRate, OptionValue@"SampleCount"}, 
		writeDoneCallback[#1, #2, #3[[1]]]&,
		"TaskDetail" -> { obj}];
	Return@asyncObj;
]

(*gpioWriteAsyncCallback[ asyncObj_AsynchronousTaskObject, eventType_, eventData_]:= Null*)

GPIOWriteAsynchronous[ asyncObj_AsynchronousTaskObject, data_]:=
Module[{ pinObj, pin},
	loadAdapter[];
	pinObj = getAsynchronousTaskObjectData[ asyncObj, "PinObject"];
	pin = getPinObjectData[ pinObj, "PinNumber"];
	Return[lfGPIOAsyncWrite[ pin, data]];
]


(*getGPIOPinObjectData[ obj_GPIOPinObject, "Pin"]:= obj[[1]]
getGPIOPinObjectData[ obj_GPIOPinObject, "Mode"]:= obj[[2]]*)

getPinObjectData[ pinObj_GPIOPinObject, "PinNumber"]:= pinObj[[1]]
getPinObjectData[ pinObj_GPIOPinObject, "Direction"]:= pinObj[[2]]
getAsynchronousTaskObjectData[ asyncObj_AsynchronousTaskObject, "PinObject"]:= asyncObj[[1, 1]] 

End[]

EndPackage[]

