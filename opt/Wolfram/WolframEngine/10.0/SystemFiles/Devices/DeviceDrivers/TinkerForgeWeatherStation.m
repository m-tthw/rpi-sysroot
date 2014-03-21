(* ::Package:: *)

(* $Id: TinkerForgeWeatherStation.m,v 1.1.2.4 2013/11/19 16:29:06 lambertc Exp $ *)

BeginPackage["TinkerForgeWeatherStation`", {"TinkerForgeWeatherStationTools`"}]

Begin["`Private`"];

$scaleFactor1 = 0.1;
$scaleFactor2 = 0.001;
$scaleFactor3 = 0.01;
$hostName = "localhost";
$portNumber = 4223;
$lightSensorID = "gwF";
$humSensorID = "fRV";
$pressTempSensorID = "fUD";
$lcdID = "gDh";
$strList = {"Illuminance", "Humidity", "Pressure", "Temperature"};

(*TinkerForge::ndev = "The Head of argument `1` must be DeviceObject."
TinkerForge::badarg = "Argument `1` is not one of "<>ToString[$strList]<>" or any combination thereof."*)

(*prepFunction[]:= (PrependTo[$Path, NotebookDirectory[]];
If[$OperatingSystem === "Windows",
Install[ "tinkerforge-mathlink-proxy.exe" ],
Install["tinkerforge-mathlink-proxy"]
]
)*)
prepFunction[]:= TinkerForgeWeatherStationTools`InstallMathLinkEXE[];

makeHandleFunction[]:= Tinkerforge`IPConnectionCreate[]

openFunction[ipconn_]:= (
     Tinkerforge`IPConnectionConnect[ipconn, $hostName, $portNumber];
	 {
	 Tinkerforge`BrickletAmbientLightCreate[$lightSensorID, ipconn],
	 Tinkerforge`BrickletHumidityCreate[$humSensorID, ipconn],
	 Tinkerforge`BrickletBarometerCreate[$pressTempSensorID, ipconn]
	 }
	 )

readFunction[{_,arg_},str_]:= With[{ill = Tinkerforge`BrickletAmbientLightGetIlluminance[arg[[1]]]*$scaleFactor1,
	                     hum = Tinkerforge`BrickletHumidityGetHumidity[arg[[2]]]*$scaleFactor1,
	                     press =Tinkerforge`BrickletBarometerGetAirPressure[arg[[3]]]*$scaleFactor2,
	                     temp = Tinkerforge`BrickletBarometerGetChipTemperature[arg[[3]]]*$scaleFactor3
	                      },
	                  
	                  If[ str === "fromRF",   
	                  
						  Return[ {
						"Temperature"-> Quantity[temp,"Celsius"],
						"Humidity"-> Quantity[hum,"Percent"],
						"Pressure"-> Quantity[press,"Millibar"],
	                    "Illuminance"-> Quantity[ill,"Lux"]
						  }],
						  
						  Return[ {
	                    ill,
	                    hum,
	                    press,
	                    temp
						  }]
						  
	                  ]
]

closeFunction[{ipconn_,{ltHandle_,humHandle_,baroHandle_}}]:= (Tinkerforge`IPConnectionDisconnect[ipconn];
                                                               Tinkerforge`BrickletAmbientLightDestroy[ltHandle];
                                                               Tinkerforge`BrickletHumidityDestroy[humHandle];
                                                               Tinkerforge`BrickletBarometerDestroy[baroHandle];
                 												)

releaseFunction[link___]:=((*RemoveScheduledTask[ScheduledTasks[]];*)LinkClose[link])

writeFunction[{ipconn_,handles_},None]:= With[{lcd =  Tinkerforge`BrickletLCD20x4Create[$lcdID, ipconn]},
Tinkerforge`BrickletLCD20x4ClearDisplay[lcd]
]

writeFunction[{ipconn_,handles_},str_String]:= Module[{lcd},
lcd =  Tinkerforge`BrickletLCD20x4Create[$lcdID, ipconn];
Tinkerforge`BrickletLCD20x4BacklightOn[lcd];
Tinkerforge`BrickletLCD20x4WriteLine[lcd, 0, 0, str];
]


writeFunction[{ipconn_,handles_},arg__]:= Module[{lcd,n,p,str},
{n, p,str}={arg};
lcd =  Tinkerforge`BrickletLCD20x4Create[$lcdID, ipconn];
Tinkerforge`BrickletLCD20x4BacklightOn[lcd];
Tinkerforge`BrickletLCD20x4WriteLine[lcd, n, p, str];
]

writeFunction[{ipconn_,handles_}]:= Module[{lcd,ill, hum, press, temp},
lcd =  Tinkerforge`BrickletLCD20x4Create[$lcdID, ipconn];
{ill,hum, press, temp} = readFunction[{None,handles},"fromWF"];
Tinkerforge`BrickletLCD20x4BacklightOn[lcd];
Tinkerforge`BrickletLCD20x4WriteLine[lcd, 3, 0, Tinkerforge`StringToKS0066U["Temperature" <> ToString[PaddedForm[N[temp], {4, 2}]] <> 
   " \[Degree]C"]];
Tinkerforge`BrickletLCD20x4WriteLine[lcd, 1, 0, "Humidity   " <> ToString[PaddedForm[N[hum], {4, 2}]] <> " % "];
Tinkerforge`BrickletLCD20x4WriteLine[lcd, 2, 0, "Air Press" <> ToString[PaddedForm[N[press], {6, 2}]] <> " mb"];
Tinkerforge`BrickletLCD20x4WriteLine[lcd, 0, 0, "Illuminanc" <> ToString[PaddedForm[N[ill], {5, 2}]] <> " lx"];
]

(*executeFunction[args___]:= With[{lcd =  Tinkerforge`BrickletLCD20x4Create[$lcdID, ipconn]},
Tinkerforge`BrickletLCD20x4ClearDisplay[lcd]
]*)

(*executeFunction[{ipconn_,handles_},___]:= $Failed*)

DeviceAPI`DeviceClassRegister["TinkerForgeWeatherStation", Null, 
	"OpenManagerFunction" :> prepFunction,
	"MakeManagerHandleFunction" -> (makeHandleFunction[]&),
	"CloseFunction" :> closeFunction,
	"ReleaseFunction" :> releaseFunction(*(RemoveScheduledTask[ScheduledTasks[]];LinkClose)*),
	"OpenFunction" :> (openFunction[#]&), 
	"ReadFunction" -> (readFunction[#,"fromRF"]&),
	"WriteFunction" -> (writeFunction),
	"FindFunction" -> DeviceOpen
]

End[];

EndPackage[]
