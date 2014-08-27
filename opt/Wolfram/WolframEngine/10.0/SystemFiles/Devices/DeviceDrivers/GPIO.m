(* Mathematica package *)

(* $Id: GPIO.m,v 1.1.2.10 2013/12/12 00:11:32 bakshee Exp $ *)

BeginPackage["GPIO`", {"GPIOLink`"}]

DeviceRead::gpioconfig = "GPIO pins `1` are configured for output and could not be read from."
DeviceWrite::gpioconfig = "GPIO pins `1` are configured for input and could not be written to."
DeviceConfigure::gpioargs = "A rule or list of rules specifying pin direction p->dir is expected."
DeviceRead::gpioargs = "A pin number of list of pin numbers is expected."
DeviceWrite::gpioargs = "A rule or list of rules specifying output values p->v is expected."
General::gpiopins = "The pins `1` are not configurable for GPIO."

Begin["`Private`"] (* Begin Private Context *) 

$bcmPins = {4, 7, 8, 9, 10, 11, 14, 15, 17, 18, 22, 23, 24, 25, 27, 28, 29, 30, 31};

$wiringPiToBCMMapping = {"WiringPi2" -> 27, "WiringPi7" -> 4, "WiringPi0" -> 17, "WiringPi3" -> 22, 
	"WiringPi12" -> 10, "WiringPi13" -> 9, "WiringPi14" -> 11, 
	"WiringPi15" -> 14, "WiringPi16" -> 15, "WiringPi1" -> 18, 
	"WiringPi4" -> 23, "WiringPi5" -> 24, "WiringPi6" -> 25, 
	"WiringPi10" -> 8, "WiringPi11" -> 7, "WiringPi18" -> 29, 
	"WiringPi20" -> 31, "WiringPi19" -> 30, "WiringPi17" -> 28}
	
$BCMToWiringPiMapping = {4 -> "WiringPi7", 17 -> "WiringPi0", 22 -> "WiringPi3", 
 10 -> "WiringPi12", 9 -> "WiringPi13", 11 -> "WiringPi14", 
 14 -> "WiringPi15", 15 -> "WiringPi16", 18 -> "WiringPi1", 
 23 -> "WiringPi4", 24 -> "WiringPi5", 25 -> "WiringPi6", 
 8 -> "WiringPi10", 7 -> "WiringPi11", 29 -> "WiringPi18", 
 31 -> "WiringPi20", 30 -> "WiringPi19", 27 -> "WiringPi2", 28 -> "WiringPi17"}

(* ::Section:: *) (* API Registration Function *)
If[ $SystemID === "Linux-ARM",
	DeviceAPI`DeviceClassRegister[ "GPIO",
		"FindFunction" -> ({{}}&),
		"CloseFunction" -> gpioClose,
		"ConfigureFunction" -> gpioConfigure,
		"ReadFunction" -> gpioRead,
		"WriteFunction" -> gpioWrite,
		"ExecuteMethodFunction" -> iGPIOMethods,
		"ReadAsynchronousFunction" -> iRunGPIOReadAsynchronousTask,
		"WriteAsynchronousFunction" -> iRunGPIOWriteAsynchronousTask
	]
]

(* ::Section:: *) (* Device API Adapter Functions *)


(* ::SubSection:: *) (* Port Management *)

$$configuredInputPins = {};
$$configuredOutputPins = {};

(* List of pins that have currently persisting direction from a previously run, non-configured Read/Write *)
$$tempInputPins = {};
$$tempOutputPins = {};

convertPinRulesToPinNums[configRules_, dir_] := Cases[configRules, r : (p_ -> dir) :> p]

$$configuredInputPins = {};
$$configuredOutputPins = {};
pinConfiguredQ[pin_Integer, dir_]:=
	Switch[ dir,
		"Input", MemberQ[ $$configuredInputPins, pin],
		"Output", MemberQ[ $$configuredOutputPins, pin]
	]

tempPinQ[ pin_Integer, dir_]:=
	Switch[ dir,
		"Input", MemberQ[ $$tempInputPins, pin],
		"Output", MemberQ[ $$tempOutputPins, pin]
	]


addConfiguredPin[ pin_Integer, dir_]:=
	Switch[ dir,
		"Input",
			If[ !pinConfiguredQ[ pin, "Input"], AppendTo[ $$configuredInputPins, pin]];,
		"Output",
			If[ !pinConfiguredQ[ pin, "Output"], AppendTo[ $$configuredOutputPins, pin]];
	]

removeConfiguredPin[ pin_Integer, dir_]:= 
	Switch[ dir,
		"Input",
			$$configuredInputPins = DeleteCases[ $$configuredInputPins, pin];,
		"Output",
			$$configuredOutputPins = DeleteCases[ $$configuredOutputPins, pin];
	]
	
addTempPin[ pin_Integer, dir_]:=
	Switch[ dir,
		"Input",
			If[ !pinConfiguredQ[ pin, "Input"], AppendTo[ $$tempInputPins, pin]];,
		"Output",
			If[ !pinConfiguredQ[ pin, "Output"], AppendTo[ $$tempOutputPins, pin]];
	]

removeTempPin[ pin_Integer, dir_]:= 
	Switch[ dir,
		"Input",
			$$tempInputPins = DeleteCases[ $$tempInputPins, pin];,
		"Output",
			$$tempOutputPins = DeleteCases[ $$tempOutputPins, pin];
	]

gpioConfigure[{ iHandle_, dHandle_}, config__Rule]:=
Module[{ remappedConfig, badPins},
	remappedConfig = { config} /. $wiringPiToBCMMapping;
	badPins = Select[{ config}, (!MemberQ[ $bcmPins, (#/.$wiringPiToBCMMapping)[[1]]])&][[;;,1]];
	If[ Length@badPins > 0, Message[ DeviceConfigure::gpiopins, badPins]; Return@$Failed];
	iGPIOConfigure[{ iHandle, dHandle}, Sequence@@remappedConfig]
];
	

iGPIOConfigure[{ iHandle_, dHandle_}, config:Sequence[Rule__]]:= 
Module[{ 
	connectedPins (* configured and temporary pins that are currently connected *),
	disconnectPins (* pins the user requests to be disconnected *),  
	configPins (* list of config pin numbers provided by user *), 
	tempInputPinsForConfig, (* temp input pins to be tracked as configured *)
	tempOutputPinsForConfig, (* temp output pins to be tracked as configured *)
	reconfigPins (* list of pin numbers that need to be reconfigured, both configured and temporary *), 
	asyncGPIOPins (* list of all pin numbers running async tasks *), 
	reconfigAsyncPins (* list of pin numbers to be reconfigured that are already running async tasks, *), 
	gpioAsyncTasks (* list of GPIO async tasks *), 
	reconfigRules,
	pinObjsToBeDisconnected
	},
	
	(* TODO : Check args *)
	(* Check pins not already configured. *)
	
	disconnectPins = ( Select[{config}, (#[[2]]===None)&])[[;;, 1]];
	(*Print[{ "disconnectPins", disconnectPins}];*)
	configPins = { config}[[ ;;, 1]];
	(* Upgrade temp pins to config'd *)
	tempInputPinsForConfig = Select[{ config}, (#[[2]] === "Input" && MemberQ[$$tempInputPins, #[[1]]])&][[ ;;,1]];
	tempOutputPinsForConfig = Select[{ config}, (#[[2]] === "Output" && MemberQ[$$tempOutputPins, #[[1]]])&][[ ;;, 1]];
	removeTempPin[ #, "Input"]& /@ tempInputPinsForConfig;
	removeTempPin[ #, "Output"]& /@ tempOutputPinsForConfig;
	addConfiguredPin[ #, "Input"]& /@ tempInputPinsForConfig;
	addConfiguredPin[ #, "Output"]& /@ tempOutputPinsForConfig;
	(*Print[ {"configPins: ", configPins}];*)
	connectedPins = GPIOLink`Private`getPinObjectData[#, "PinNumber"]& /@ GPIOPins[];
	(*Print[ {"Connected Pin Number", connectedPins}];*)
	reconfigPins = Select[ configPins, ( MemberQ[ connectedPins, #] && GPIOLink`Private`getPinObjectData[ First@Cases[ GPIOPins[], GPIOPinObject[#, _]], "Direction"] =!= (# /. {config}))&];
	(*Print[ {"reconfigPins", reconfigPins}];*)
	gpioAsyncTasks = Cases[Flatten[ AsynchronousTasks[][[ ;;, 1]]], _GPIOPinObject];
	asyncGPIOPins = GPIOLink`Private`getPinObjectData[ #, "PinNumber"]& /@ gpioAsyncTasks;
	(*Print[{"asyncGPIOPins", asyncGPIOPins}];*)
	reconfigAsyncPins = Select[ reconfigPins, 
		( MemberQ[ asyncGPIOPins, #] && GPIOLink`Private`getPinObjectData[ First@Cases[ GPIOPins[], GPIOPinObject[#, _]], "Direction"])&
		];
	(*Print[{"reconfigAsyncPins", reconfigAsyncPins}];*)
	
	(* Disconnect specified pins *)
	pinObjsToBeDisconnected = Select[GPIOPins[], (MemberQ[ disconnectPins, GPIOLink`Private`getPinObjectData[#, "PinNumber"]])&];
		(* This automatically culls unconnected pins from the disconnect list *)
	(*Print[{ "pinObjsToBeDisconnected", pinObjsToBeDisconnected}];*)
	If[ Length@pinObjsToBeDisconnected > 0,
		GPIOPinDisconnect /@ pinObjsToBeDisconnected;
		(* the remove functions are safe against removing pins that aren't connected *)
		removeConfiguredPin[ #, "Input"]& /@ disconnectPins;
		removeConfiguredPin[ #, "Output"]& /@ disconnectPins;
		removeTempPin[ #, "Input"]& /@ disconnectPins;
		removeTempPin[ #, "Output"]& /@ disconnectPins;
	];
	
	If[ Length@reconfigAsyncPins > 0,
		Message[ DeviceConfigure::configfail];
		Return@$Failed;,
		(* else *)
		reconfigRules = FilterRules[{ config}, reconfigPins];
		(*Print[ {"reconfigRules", reconfigRules}];*)
		(*Print[ {"objsToDisconnect",  Select[ GPIOPins[], ( MemberQ[ reconfigPins, GPIOLink`Private`getPinObjectData[#, "PinNumber"]])&]}];*)
		GPIOPinDisconnect/@( Select[ GPIOPins[], ( MemberQ[ reconfigPins, GPIOLink`Private`getPinObjectData[#, "PinNumber"]])&]);
		removeConfiguredPin[ #, "Input"]& /@ reconfigPins;
		removeConfiguredPin[ #, "Output"]& /@ reconfigPins;
		removeTempPin[ #, "Input"]& /@ reconfigPins;
		removeTempPin[ #, "Output"]& /@ reconfigPins;
		GPIOPinConnect@@@Join[ reconfigRules, FilterRules[{ config}, Complement[configPins, connectedPins]]];
		addConfiguredPin@@@Join[ reconfigRules, FilterRules[{ config}, Complement[configPins, connectedPins]]];
		addConfiguredPin@@@FilterRules[{ config}, Complement[configPins, connectedPins]];
	];
]

gpioConfigure[{ iHandle_, dHandle_}, args___]:= 
(
	Message[ DeviceConfigure::gpioargs];
	Return@$Failed;	
)	
	
(*iGPIOPinOpen[ iHandle_, args___]:= Check[ GPIOPinConnect[ args], $Failed]*)

(*iGPIOPinClose[ iHandle_, args___]:= Check[ GPIOPinDisconnect[ iHandle], $Failed]*)

(* Failsafe disconnect of all pins. *)
gpioClose[ args___]:= iGPIOClose[ args];

iGPIOClose[ iHandle_, args___]:= GPIOPinDisconnect /@ GPIOPins[]
(* TODO: add cleanup for async tasks *)

(* ::SubSection:: *) (* Synchronous I/O *)

(*iGPIORead[{ iHandle_, pin_GPIOPinObject}]:= GPIORead[ pin]*)

gpioRead[{ iHandle_, dHandle_}, pins__]:= 
Module[{ remappedPins, badPins, argReverseMapping, res},
	If[ Length[ Select[{pins}, (!(StringQ[#] || IntegerQ[#]))&]] > 0, Message[ DeviceRead::gpioargs]; Return@$Failed];
	remappedPins = {pins}/.$wiringPiToBCMMapping;
	badPins = Select[{ pins}, (!MemberQ[ $bcmPins, (#/.$wiringPiToBCMMapping)])&];
	If[ Length@badPins > 0, Message[ DeviceRead::gpiopins, badPins]; Return@$Failed];
	res = iGPIORead[{ iHandle, dHandle}, Sequence@@remappedPins];
	argReverseMapping = Select[ $BCMToWiringPiMapping, MemberQ[ {pins}, #[[2]]]& ];
	res /. argReverseMapping
];	

gpioRead[{ iHandle_, dHandle_}, args___]:=
(
	Message[ DeviceRead::gpioargs];
	Return[$Failed];
)
	

iGPIORead[{ iHandle_, dHandle_}, pins__Integer]:=
Module[{ misconfiguredPins, newTempInputPins, pinsToSwitch}, 
	(* TODO: validate arguments, check that pins are configured *)
	misconfiguredPins = Select[{ pins}, pinConfiguredQ[#, "Output"]&];
	If[ Length@misconfiguredPins > 0, 
		Message[ DeviceRead::gpioconfig, misconfiguredPins]; 
		Return@$Failed;
	];
	pinsToSwitch = Select[ {pins}, (MemberQ[ Join[$$tempOutputPins, $$configuredOutputPins], #])&];
	newTempInputPins = Select[ {pins}, (!MemberQ[ Join[$$configuredInputPins, $$configuredOutputPins, $$tempInputPins, $$tempOutputPins], #])&];
	( GPIOPinDisconnect[ GPIOPinObject[ #, "Output"]])& /@ pinsToSwitch;
	( GPIOPinConnect[ #, "Input"])& /@ pinsToSwitch;
	( removeTempPin[ #, "Output"])& /@ pinsToSwitch;
	( addTempPin[ #, "Input"])& /@ pinsToSwitch;
	( GPIOPinConnect[ #, "Input"])& /@ newTempInputPins;
	( addTempPin[ #, "Input"])& /@ newTempInputPins;
	
	( Rule[#, GPIORead[ GPIOPinObject[ #, "Input"]]])& /@ { pins}
	(*TODO: add guards for asynchronous *)
]

(*iGPIOWrite[{ iHandle_, pin_GPIOPinObject}, args___]:= GPIOWrite[ pin, args]*)

gpioWrite[{ iHandle_, dHandle_}, pinRules__Rule]:=
Module[{ remappedPinRules, badPins},
	If[ Length[ Select[{pinRules}[[;;,1]], (!(StringQ[#] || IntegerQ[#]))&]] > 0, Message[ DeviceRead::gpioargs]; Return@$Failed];
	remappedPinRules = {pinRules}/.(Rule[p_,v_]:> Rule[p/.$wiringPiToBCMMapping, v]);
	badPins = Select[{ pinRules}, (!MemberQ[ $bcmPins, (#[[1]]/.$wiringPiToBCMMapping)])&][[;;,1]];
	If[ Length@badPins > 0, Message[ DeviceWrite::gpiopins, badPins]; Return@$Failed];
	iGPIOWrite[{ iHandle, dHandle}, Sequence@@remappedPinRules]
];	

gpioWrite[{ iHandle_, dHandle_}, args___]:=
(
	Message[ DeviceWrite::gpioargs];
	Return[$Failed];
)


iGPIOWrite[{ iHandle_, dHandle_}, pinRules__Rule]:=
Module[{ pins, misconfiguredPins, newTempOutputPins, pinsToSwitch}, 
	(* TODO: validate arguments, check that pins are configured *)
	
	pins = {pinRules}[[ ;;, 1]];
	misconfiguredPins = Select[ pins, pinConfiguredQ[#, "Input"]&];
	If[ Length@misconfiguredPins > 0, 
		Message[ DeviceWrite::gpioconfig, misconfiguredPins]; 
		Return@$Failed;
	];
	pinsToSwitch = Select[ pins, (MemberQ[ Join[$$configuredInputPins, $$tempInputPins], #])&];
	newTempOutputPins = Select[ pins, (!MemberQ[ Join[$$configuredOutputPins, $$tempOutputPins, $$configuredInputPins, $$tempInputPins], #])&];
	( GPIOPinDisconnect[ GPIOPinObject[ #, "Input"]])& /@ pinsToSwitch;
	( GPIOPinConnect[ #, "Output"])& /@ pinsToSwitch;
	( removeTempPin[ #, "Input"])& /@ pinsToSwitch;
	( addTempPin[ #, "Output"])& /@ pinsToSwitch;
	( GPIOPinConnect[ #, "Output"])& /@ newTempOutputPins;
	( addTempPin[ #, "Output"])& /@ newTempOutputPins;
	iPinRules = {pinRules} /. {Rule[ pin_Integer, value_Integer] :> Rule[ getPinObject@pin, value]};
	GPIOWrite@@@iPinRules;
	
]

(*iGPIOWrite[{ iHandle_, dHandle_}, pinRules__Rule]:=
Module[{ iPinRules},
	iPinRules = {pinRules} /. {Rule[ pin_Integer, value_Integer] :> Rule[ getPinObject@pin, value]};
	GPIOWrite@@@iPinRules;
	(* TODO: safeguards *)
]
*)
(* ::SubSection:: *) (* Asynchronous Read *)

Options[ iRunGPIOReadAsynchronousTask] = Options[ CreateGPIOReadAsynchronousTask]

iRunGPIOReadAsynchronousTask[{ iHandle_, pin_GPIOPinObject}, opts:OptionsPattern[]]:= CreateGPIOReadAsynchronousTask[ pin, opts]

iGPIOMethods[ pin_GPIOPinObject, "CurrentGPIOData", args_List]:= iCurrentGPIOData[ pin, Sequence@@args]

iCurrentGPIOData[ pin_GPIOPinObject, asyncObj_AsynchronousTaskObject]:= CurrentGPIOData[ asyncObj]

(* ::SubSection:: *) (* Asynchronous Write *)

Options[ iRunGPIOWriteAsynchronousTask] = Options[ RunGPIOWriteAsynchronousTask]

iRunGPIOWriteAsynchronousTask[{ iHandle_, pin_GPIOPinObject}, opts:OptionsPattern[]]:= CreateGPIOWriteAsynchronousTask[ pin, opts]

iGPIOMethods[ pin_GPIOPinObject, "GPIOWriteAsynchronous", args_List]:= iGPIOWriteAsynchronous[ pin, Sequence@@args]

iGPIOWriteAsynchronous[ pin_GPIOPinObject, asyncObj_AsynchronousTaskObject, data_]:= GPIOWriteAsynchronous[asyncObj, data]

(* ::SubSection:: *) (* Utilties *)
 
getPinObject[ pin_Integer]:= First@Cases[ GPIOPins[], GPIOPinObject[ pin, _]]
getPinObjects[{ pin__Integer}]:= Select[ GPIOPins[], MemberQ[ {pin}, #[[1]]]&]
getPinNumber[ pinObj_GPIOPinObject]:= GPIOLink`Private`getPinObjectData[ pinObj, "PinNumber"]

End[]

EndPackage[]
