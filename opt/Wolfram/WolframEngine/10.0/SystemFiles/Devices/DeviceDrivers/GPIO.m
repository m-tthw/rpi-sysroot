(* Mathematica package *)

(* $Id: GPIO.m,v 1.1.2.14.2.3 2014/08/07 20:36:59 lambertc Exp $ *)

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
(*If[ $SystemID === "Linux-ARM",*)
	DeviceFramework`DeviceClassRegister[ "GPIO",
		"FindFunction" -> ({{}}&),
		"CloseFunction" -> gpioClose,
		"ConfigureFunction" -> gpioConfigure,
		"ReadFunction" -> gpioRead,
		"WriteFunction" -> gpioWrite,
		(*
		"ExecuteMethodFunction" -> iGPIOMethods,
		"ReadAsynchronousFunction" -> iRunGPIOReadAsynchronousTask,
		"WriteAsynchronousFunction" -> iRunGPIOWriteAsynchronousTask, 
		*)
		"DeviceIconFunction" -> GPIOIconFunction
	]
(*]*)

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

GPIOIconFunction[{ iHandle_, dHandle_}, ___]:= Graphics[{Thickness[0.038461538461538464`], 
  Style[{FilledCurve[{{{1, 4, 3}, {0, 1, 0}, {1, 3, 3}, {0, 1, 0}, {1,
         3, 3}, {0, 1, 0}, {1, 3, 3}, {0, 1, 0}}}, {{{25.5, 
        2.5}, {25.5, 1.395}, {24.605, 0.5}, {23.5, 0.5}, {2.5, 
        0.5}, {1.395, 0.5}, {0.5, 1.395}, {0.5, 2.5}, {0.5, 
        23.5}, {0.5, 24.605}, {1.395, 25.5}, {2.5, 25.5}, {23.5, 
        25.5}, {24.605, 25.5}, {25.5, 24.605}, {25.5, 23.5}, {25.5, 
        2.5}}}]}, FaceForm[RGBColor[0.941, 0.961, 0.957, 1.]]], 
  Style[{JoinedCurve[{{{1, 4, 3}, {0, 1, 0}, {1, 3, 3}, {0, 1, 0}, {1,
         3, 3}, {0, 1, 0}, {1, 3, 3}, {0, 1, 0}}}, {{{25.5, 
        2.5}, {25.5, 1.395}, {24.605, 0.5}, {23.5, 0.5}, {2.5, 
        0.5}, {1.395, 0.5}, {0.5, 1.395}, {0.5, 2.5}, {0.5, 
        23.5}, {0.5, 24.605}, {1.395, 25.5}, {2.5, 25.5}, {23.5, 
        25.5}, {24.605, 25.5}, {25.5, 24.605}, {25.5, 23.5}, {25.5, 
        2.5}}}, CurveClosed -> {1}]}, JoinForm[{"Miter", 10.}], 
   RGBColor[0.7, 0.7, 0.7, 1.]], 
  Style[{FilledCurve[{{{1, 4, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 
        3}}}, {{{7.717, 13.}, {7.717, 12.26}, {7.116999999999999, 
        11.66}, {6.377, 11.66}, {5.637, 11.66}, {5.0360000000000005`, 
        12.26}, {5.0360000000000005`, 13.}, {5.0360000000000005`, 
        13.739999999999998`}, {5.637, 14.34}, {6.377, 
        14.34}, {7.116999999999999, 14.34}, {7.717, 
        13.739999999999998`}, {7.717, 13.}}}], 
    FilledCurve[{{{1, 4, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 
        3}}}, {{{20.853, 13.}, {20.853, 12.26}, {20.252, 
        11.66}, {19.512, 11.66}, {18.772, 11.66}, {18.172, 
        12.26}, {18.172, 13.}, {18.172, 13.739999999999998`}, {18.772,
         14.34}, {19.512, 14.34}, {20.252, 14.34}, {20.853, 
        13.739999999999998`}, {20.853, 13.}}}]}, 
   FaceForm[RGBColor[0.941, 0.961, 0.957, 1.]]], 
  Style[{FilledCurve[{{{1, 4, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 
        3}}}, {{{17.150000000000002`, 12.803}, {17.150000000000002`, 
        10.559000000000001`}, {15.331, 8.740999999999998}, {13.088, 
        8.740999999999998}, {10.844, 8.740999999999998}, {9.025, 
        10.559000000000001`}, {9.025, 12.803}, {9.025, 
        15.046999999999999`}, {10.844, 16.866}, {13.088, 
        16.866}, {15.331, 16.866}, {17.150000000000002`, 
        15.046999999999999`}, {17.150000000000002`, 12.803}}}], 
    FilledCurve[{{{0, 2, 0}, {0, 1, 0}, {0, 1, 
        0}}}, {{{18.483999999999998`, 17.316000000000003`}, {21.165, 
        17.316000000000003`}, {21.165, 15.337}, {18.483999999999998`, 
        15.337}}}]}, FaceForm[RGBColor[0.938, 0.961, 0.952, 1.]]], 
  Style[{FilledCurve[{{{0, 2, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 
        0}}, {{0, 2, 0}, {0, 1, 0}, {0, 1, 0}, {0, 1, 
        0}}}, {{{6.299284375, 17.66166875}, {4.31883125, 
        17.66166875}, {4.31883125, 8.08566875}, {6.299284375, 
        8.08566875}, {6.299284375, 
        17.66166875}}, {{7.5078387499999995`, 8.08566875}, {9.02487, 
        8.08566875}, {12.73640125, 17.954684375}, {11.21937, 
        17.954684375}, {7.5078387499999995`, 8.08566875}}}], 
    FilledCurve[{{{1, 4, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 
        3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}}, {{1, 4, 3}, {1, 3, 
        3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1, 3, 3}, {1,
         3, 3}}}, {{{17.708440000000003`, 9.496715625}, {16.91044, 
        9.496715625}, {16.272455625, 9.789731249999999}, {15.79240875,
         10.373684375}, {15.326908750000001`, 
        10.959715625000001`}, {15.087924375, 11.7972}, {15.087924375, 
        12.87366875}, {15.087924375, 
        13.952215625000001`}, {15.326908750000001`, 
        14.7897}, {15.79240875, 15.375731250000001`}, {16.272455625, 
        15.959684375000002`}, {16.91044, 
        16.2527}, {17.708440000000003`, 16.2527}, {18.50644, 
        16.2527}, {19.131955625000003`, 
        15.959684375000002`}, {19.597455625000002`, 
        15.375731250000001`}, {20.062955625, 
        14.77723125}, {20.301940000000002`, 
        13.952215625000001`}, {20.301940000000002`, 
        12.87366875}, {20.301940000000002`, 11.7972}, {20.062955625, 
        10.959715625000001`}, {19.597455625000002`, 
        10.373684375}, {19.131955625000003`, 
        9.789731249999999}, {18.50644, 
        9.496715625}, {17.708440000000003`, 
        9.496715625}}, {{22.284471250000003`, 
        12.87366875}, {22.284471250000003`, 
        14.629684375}, {21.804424375000004`, 
        15.947215625}, {20.833940000000002`, 
        16.824184375}, {20.11490875, 
        17.57023125}, {19.065455625000002`, 
        17.942215625000003`}, {17.69597125, 
        17.942215625000003`}, {16.32440875, 
        17.942215625000003`}, {15.287424375, 
        17.57023125}, {14.555924375, 16.824184375}, {13.58544, 
        15.947215625}, {13.10747125, 14.629684375}, {13.10747125, 
        12.87366875}, {13.10747125, 11.159215625}, {13.58544, 
        9.841684375}, {14.555924375, 8.92523125}, {15.287424375, 
        8.179184375}, {16.32440875, 7.8072}, {17.69597125, 
        7.8072}, {19.065455625000002`, 7.8072}, {20.11490875, 
        8.179184375}, {20.833940000000002`, 
        8.92523125}, {21.804424375000004`, 
        9.841684375}, {22.284471250000003`, 
        11.159215625}, {22.284471250000003`, 12.87366875}}}]}, 
   FaceForm[RGBColor[0.7, 0.7, 0.7, 1.]]]}, 
 ImageSize -> {26., 26.}, PlotRange -> {{0., 26.}, {0., 26.}}, 
 AspectRatio -> Automatic]

End[]

EndPackage[]
