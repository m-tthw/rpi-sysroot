(* Mathematica package *)

(* $Id: EdisonArduino.m,v 1.1.2.1 2014/08/20 20:51:07 lambertc Exp $ *)

BeginPackage["EdisonArduino`"]

Begin["`Private`"] (* Begin Private Context *) 

(* TODO: Figure out the commented out mappings *)
$EdisonArduinoPinMappings = Association[ 0 -> 130, 1-> 131, 2-> 128,(* 3-> ?,*) 4-> 129, (* 5 -> ?, 6->?, *) 7 -> 48, 8 -> 49, (* 9 -> ?, 10 -> ?, 11 -> ?, *) 12 -> 42, 13 -> 40];

$$configuredPins = Association[];

(* ::Section:: *)
(* Digital IO *)

configureDigitalPin[ digitalPin_Integer, dir_String]:=
Module[{ currState},
	currState = $$configuredPins[ digitalPin];
	Print[10];
	If[ Head@currState === Missing && !FileExistsQ[ "/sys/class/gpio/gpio" <> ToString@digitalPin],
		Print[20,  "!echo " <> ToString@digitalPin <> " > " <>  "/sys/class/gpio/export"];
		Import[ "!echo " <> ToString@digitalPin <> " > " <>  "/sys/class/gpio/export", "Text"];
		Print[30];
	];
	Print[40];
	If[ currState =!= dir,(* not configured *)
		Print["/sys/class/gpio/gpio" <> ToString@digitalPin <> "/direction"];
		Export["/sys/class/gpio/gpio" <> ToString@digitalPin <> "/direction", dir, "Text"];
		Print[50];
		$$configuredPins[ digitalPin] = dir;
	]
]

readDigitalPin[ digitalPin_Integer]:= Import[ "/sys/class/gpio/gpio"<>ToString@digitalPin<>"/value"]

edisonArduinoRead[ { iHandle_, dHandle_}, digitalPin_Integer]:= 
(
	configureDigitalPin[ $EdisonArduinoPinMappings@digitalPin, "in"];
	Return[{ $EdisonArduinoPinMappings@digitalPin -> readDigitalPin[ $EdisonArduinoPinMappings@digitalPin]}];
)

edisonArduinoRead[ { iHandle_, dHandle_}, digitalPins__Integer]:= 
(
	(configureDigitalPin[$EdisonArduinoPinMappings[#]]&) /@ { digitalPins};
	Return[( # -> readDigitalPin[$EdisonArduinoPinMappings[#]])& /@ { digitalPins}];
)

edisonArduinoWrite[{ iHandle_, dHandle_}, pinRule_Rule]:=
Module[{ pin, value},
	pin = pinRule[[1]];
	value = pinRule[[2]];
	configureDigitalPin[ $EdisonArduinoPinMappings@pin, "out"];
	writeDigitalPin[ $EdisonArduinoPinMappings@pin, value];
	Return[ pin -> value];
]

writeDigitalPin[ pin_Integer, value_Integer]:= (*Export[ "/sys/class/gpio/gpio" <> ToString@pin <> "/value", value, "Text"]*)
Module[{ valuePath},
	valuePath = "/sys/class/gpio/gpio" <> ToString@pin <> "/value";
	Print["!echo -n " <> ToString@value <> " > " <> valuePath];
	Import["!echo -n " <> ToString@value <> " > " <> valuePath, "Text"];
]

(* ::Section:: *)
(* Analog Input *)

edisonArduinoRead[{ iHandle_, dHandle_}, analogPin_String]:= 
Module[{ pinNum, value},
	pinNum = StringTake[ analogPin, 2];
	(* TODO: Set GPIO pin for analog input mux*)
	Print[ "TODO: Set GPIO pin for analog input mux.  Needs info from Intel."];
	value = ToExpression@Import[ "/sys/bus/iio/devices/iio\:device" <> pinNum <> "/in_voltage" <> pinNum <> "_raw", "Text"];
	Return[ analogPin -> value];
]

edisonArduinoRead[{ iHandle_, dHandle_}, analogPins__String]:= 
(
	edisonArduinoRead[{ iHandle, dHandle}, #]& /@ analogPins;
)

(* ::Section:: *)
(* PWM *)

edisonArduinoWrite[{ iHandle_, dHandle_}, "PWM", pwmPin_Integer, dutyCycle_Integer, period_Integer]:=
(
	configurePWMPin[ pwmPin];
	(* NOTE:  we may need to set duty cycle and period before enabling *)
	(* set period *)
	Import[ "!echo -n " <> ToString@period <> " > /sys/class/pwm/pwmchip0/pwm" <> ToString@pwmPin <> "/period"];
	(* set duty cycle" *)
	Import[ "!echo -n " <> ToString[ IntegerPart[ N[ period * dutyCycle/100]]] <>  " > /sys/class/pwm/pwmchip0/pwm" <> ToString@pwmPin <> "/duty_cycle"];
)

configurePWMPin[ pwmPin_Integer]:= 
Module[{ currState},
	currState = $$configuredPins[ pwmPin];
	If[ Head@currState === Missing && !FileExistsQ[ "/sys/class/pwm/pwmchip0/pwm" <> ToString@pwmPin <> "/enable"],
		(* Export PWM pin *)
		Import[ "!echo -n" <> ToString@pwmPin <> " > " <>  "/sys/class/pwm/pwmchip0/export", "Text"];
		(* Enable PWM pin *)
		Import[ "!echo -n "1" > /sys/class/pwm/pwchip0/pwm" <> ToString@pwmPin <> "/enable"];
		$$configuredPins[ pwmPin] = "PWM";
	];
]

(* ::Section:: *)
(* Close *)

edisonArduinoClose[ iHandle_, args___]:= 
Module[
	{ openPins = Normal[ $$configuredPins]},
	
	iClose@@@openPins;
]

iClose[ pin_Integer, "PWM"]:= 
(
	(* disable pin *)
	Import[ "!echo -n "0" > /sys/class/pwm/pwchip0/pwm" <> ToString@pin <> "/enable"];
	(* unexport pin *)
	Import[ "!echo -n" <> ToString@pin <> " > " <>  "/sys/class/pwm/pwmchip0/unexport", "Text"];
	KeyDropFrom[ $$configuredPins, pin];
)

iClose[ pin_Integer, "Input"]:= iClose[ pin, "Digital"]

iClose[ pin_Integer, "Output"] := iClose[ pin, "Digital"]

iClose[ pin_Integer, "Digital"]:= 
(
	Import[ "!echo " <> ToString@pin <> " > " <>  "/sys/class/gpio/unexport", "Text"];
	KeyDropFrom[ $$configuredPins, pin];
)

iClose[ pin_String, "Analog"]:= 
(
	(* TODO *)
	Print[ "TODO"];
)

(* ::Section:: *)
(* Registration *)

DeviceFramework`DeviceClassRegister[ "EdisonArduino",
		"FindFunction" -> ({{}}&),
		"CloseFunction" -> edisonArduinoClose,
		"ConfigureFunction" -> edisonArduinoConfigure,
		"ReadFunction" -> edisonArduinoRead,
		"WriteFunction" -> edisonArduinoWrite
	]

End[]

EndPackage[]