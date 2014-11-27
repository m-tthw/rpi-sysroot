(* ::Package:: *)

(* $Id: TinkerForgeWeatherStation.m,v 1.1.2.11.2.1 2014/07/17 22:16:57 lambertc Exp $ *)

BeginPackage["TinkerForgeWeatherStation`", {"TinkerForgeWeatherStationTools`"}]

Begin["`Private`"];

$scaleFactor1 = 0.1;
$scaleFactor2 = 0.001;
$scaleFactor3 = 0.01;
$hostName = "localhost";
$portNumber = 4223;
$lcdID = (*"gDh"*)"";
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

openFunction[ipconn_,{lightSensorID_,humSensorID_,pressTempSensorID_,lcdID_}]:= ( 
     $lcdID = lcdID;
     Tinkerforge`IPConnectionConnect[ipconn, $hostName, $portNumber];
	 {
	 Tinkerforge`BrickletAmbientLightCreate[lightSensorID, ipconn],
	 Tinkerforge`BrickletHumidityCreate[humSensorID, ipconn],
	 Tinkerforge`BrickletBarometerCreate[pressTempSensorID, ipconn]
	 }
	 )

readFunction[{_,arg_},str_] :=
    With[ {ill = Tinkerforge`BrickletAmbientLightGetIlluminance[arg[[1]]]*$scaleFactor1,
    hum = Tinkerforge`BrickletHumidityGetHumidity[arg[[2]]]*$scaleFactor1,
    press = Tinkerforge`BrickletBarometerGetAirPressure[arg[[3]]]*$scaleFactor2,
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

closeFunction[{ipconn_,{ltHandle_,humHandle_,baroHandle_}}] :=
    (Tinkerforge`IPConnectionDisconnect[ipconn];
     Tinkerforge`BrickletAmbientLightDestroy[ltHandle];
     Tinkerforge`BrickletHumidityDestroy[humHandle];
     Tinkerforge`BrickletBarometerDestroy[baroHandle];
       )

releaseFunction[link___]:=((*RemoveScheduledTask[ScheduledTasks[]];*)LinkClose[link])

writeFunction[{ipconn_,handles_}] :=
    Module[ {lcd,ill, hum, press, temp},
        lcd =  Tinkerforge`BrickletLCD20x4Create[$lcdID, ipconn];
        {ill,hum, press, temp} = readFunction[{None,handles},"fromWF"];
        Tinkerforge`BrickletLCD20x4BacklightOn[lcd];
        Tinkerforge`BrickletLCD20x4WriteLine[lcd, 3, 0, Tinkerforge`StringToKS0066U["Temperature" <> ToString[PaddedForm[N[temp], {4, 2}]] <> 
           " \[Degree]C"]];
        Tinkerforge`BrickletLCD20x4WriteLine[lcd, 1, 0, "Humidity   " <> ToString[PaddedForm[N[hum], {4, 2}]] <> " % "];
        Tinkerforge`BrickletLCD20x4WriteLine[lcd, 2, 0, "Air Press" <> ToString[PaddedForm[N[press], {6, 2}]] <> " mb"];
        Tinkerforge`BrickletLCD20x4WriteLine[lcd, 0, 0, "Illuminanc" <> ToString[PaddedForm[N[ill], {5, 2}]] <> " lx"];
    ]

writeFunction[{ipconn_,handles_},None] :=
    With[ {lcd =  Tinkerforge`BrickletLCD20x4Create[$lcdID, ipconn]},
        Tinkerforge`BrickletLCD20x4ClearDisplay[lcd]
    ]

writeFunction[{ipconn_,handles_},str_String]:= writeFunction[{ipconn,handles},0,0,str]

writeFunction[{ipconn_,handles_},arg__] :=
    Module[ {n,p,str},
        {n, p,str} = {arg};
        writeFunctionFormatted[{ipconn,handles},{n,p,str}]
    ]


writeFunctionFormatted[{ipconn_,handles_},{nn_Integer,pp_Integer,string_String}] :=
    Module[ {lcd,n,p,
    str, rem,quot,lin = 20,str1,str2,retStr,writeStr1,writeStr2,writeStr3,writeStr4},
        n = nn+ 1;
        p = pp(*+ 1*);
        lcd =  Tinkerforge`BrickletLCD20x4Create[$lcdID, ipconn];
        Tinkerforge`BrickletLCD20x4BacklightOn[lcd];
        str = getWriteString[string,n,p]; (*this is the formatted string*)
        {str1,str2} = {StringTake[str,lin-p+1],StringDrop[str,lin-p+1]};
        {quot,rem} = QuotientRemainder[StringLength[str2],20];
        Which[
        quot===3,
        (retStr = StringTake[str2,{20,{21,40},{41,60}}];
         {writeStr1,writeStr2,writeStr3,writeStr4} = Insert[retStr[[1;;3]],str1,1];
         Tinkerforge`BrickletLCD20x4WriteLine[lcd,0,p,writeStr1];
         Tinkerforge`BrickletLCD20x4WriteLine[lcd,1,0,writeStr2];
         Tinkerforge`BrickletLCD20x4WriteLine[lcd,2,0,writeStr3];
         Tinkerforge`BrickletLCD20x4WriteLine[lcd,3,0,writeStr4];
        ),
        quot===2,
        (retStr = StringTake[str2,{20,{21,40}}];
         {writeStr1,writeStr2,writeStr3} = Insert[retStr[[1;;2]],str1,1];
         Tinkerforge`BrickletLCD20x4WriteLine[lcd,1,p,writeStr1];
         Tinkerforge`BrickletLCD20x4WriteLine[lcd,2,0,writeStr2];
         Tinkerforge`BrickletLCD20x4WriteLine[lcd,3,0,writeStr3];
        ),
        quot==1,
        (retStr = StringTake[str2,20];
         {writeStr1,writeStr2} = {str1,retStr};(*Insert[retStr[[1]],str1,1];*)
         Tinkerforge`BrickletLCD20x4WriteLine[lcd,2,p,writeStr1];
         Tinkerforge`BrickletLCD20x4WriteLine[lcd,3,0,writeStr2];
        ),
        True,
        (*retStr=str2*)
        (writeStr1 = str1;
         Tinkerforge`BrickletLCD20x4WriteLine[lcd,3,p,writeStr1];
        )
        ];
    ]
    
(*utility function: to be moved to TinkerForgeWeatherStationTools`*)
format[n_,p_] :=
    Module[ {lin = 4,P = 20,elems},
        elems = (P-p+1)(lin-n+1)+(lin-n)(p-1);
        elems
    ]

getWriteString[str_,n_,p_] :=
    Module[ {charLength = StringLength[str],formLength = format[n,p]},
        If[ charLength===formLength,
            str,
            If[ charLength>formLength,
                StringTake[str,formLength],
                StringJoin@PadRight[Characters@str,formLength," "]
            ]
        ]
    ]

executeFunction[{ipconn_,handles_}, "ClearLCDDisplay" ,args___] :=
    With[ {lcd =  Tinkerforge`BrickletLCD20x4Create[$lcdID, ipconn]},
        Tinkerforge`BrickletLCD20x4ClearDisplay[lcd]
    ]

DeviceFramework`DeviceClassRegister["TinkerForgeWeatherStation",
    "OpenManagerFunction" :> prepFunction,
    "MakeManagerHandleFunction" -> (makeHandleFunction[]&),
    "CloseFunction" :> closeFunction,
    "ReleaseFunction" :> releaseFunction(*(RemoveScheduledTask[ScheduledTasks[]];LinkClose)*),
    "OpenFunction" :> (openFunction[#1,{#2,#3,#4,#5}]&),
    "ReadFunction" -> (readFunction[#,"fromRF"]&),
    "WriteFunction" -> (writeFunction),
    "ExecuteFunction" -> (executeFunction),
    "FindFunction" -> ({{}}&)
]

End[];

EndPackage[]
