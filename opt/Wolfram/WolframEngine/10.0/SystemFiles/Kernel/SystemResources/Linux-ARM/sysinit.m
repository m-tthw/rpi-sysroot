(* ::Package:: *)

 
Begin[ "System`"]		(* Everything in System context *)
   
General::writewarn = "Defining rule for `1`."

(*
General::spell1 = 
	"Possible spelling error: new symbol name \"`1`\" is similar to existing symbol \"`2`\"."

General::spell = 
	"Possible spelling error: new symbol name \"`1`\" is similar to existing symbols `2`."
*)

Off[General::newsym]

Off[DumpGet::valwarn]
Off[Compile::noinfo]
Off[General::sandbox]

General::sysmain = 
	"Error loading the main binary file `1`.
	 Get[\"sysmake.m\"] must be run before continuing."

Begin[ "System`Private`"]

If[  Hold[ $InstallationDirectory] === Hold @@ { $InstallationDirectory},
	$InstallationDirectory = DirectoryName[ $Input, 5]]


If[ Hold[ $SystemFileDir] === Hold @@ { $SystemFileDir},
	$SystemFileDir = 
   	 	ToFileName[ {$InstallationDirectory, "SystemFiles", "Kernel", 
				"SystemResources", $SystemID}]]

$MessagesDir = 
    ToFileName[ {$InstallationDirectory, "SystemFiles", "Kernel", "TextResources"}]

(*
When the mainload.mx system can save a symbol with
a value these should be moved into mainload.mx.
*)

System`NotebookInformation = Developer`NotebookInformation

If[ $OperatingSystem === "MacOS" , SetDirectory[ $InstallationDirectory]]

If [ $OperatingSystem === "MacOSX" && Environment["LANG"] === "ja_JP",
	$SystemCharacterEncoding = "ShiftJIS" ]


(*
 Utility function for building MX files for an Application
*)

System`Private`BuildApplicationMXFunction[ {appName_, context_, outputMX_, path_}] :=
	Module[{outFile, app},
		Print[ "FF " , path, " ", context];
		app = appName <> "`";
		Get[app];
		CreateDirectory[ ToFileName[{path, appName, "Kernel","SystemResources"}, $SystemID],
				CreateIntermediateDirectories -> True];
		outFile = ToFileName[{path, appName, "Kernel","SystemResources", $SystemID}, outputMX];
		Print[ "Out file is ", outFile];
		DumpSave[outFile, context, HoldAllComplete];
	]


Which[ 
	Names[ "System`$SystemInstall"] =!= {}
	,
		System`Private`$SysmakeError = 0;
		AppendTo[ $Path, 
			ToFileName[ 
				{$InstallationDirectory, "AddOns", "StandardPackages"},"StartUp"]
		];
		SetOptions[$Output, PageWidth->Infinity];
		Get[ "sysmake.m"];
		Exit[System`Private`$SysmakeError]
	,
	ListQ[ System`Private`BuildApplicationMX]
	,
		System`Private`BuildApplicationMXFunction[System`Private`BuildApplicationMX];
		Exit[]
	,
	True (* Normal start *)
	,
		If[
			DumpGet[StringJoin[ $SystemFileDir, ContextToFileName[ "mainload`"], "x"] ] =!= Null,
			Message[ General::sysmain, "mainload`"]
		];
	]

Off[ Get::noopen]
Off[ General::initg]
Off[ General::initc]
Off[ General::spell]
Off[ General::spell1]
Off[ General::obspkgfn]


$CharacterEncoding = $SystemCharacterEncoding;



SetDirectory[ ToFileName[{$InstallationDirectory, "SystemFiles", "CharacterEncodings"}]];

System`$CharacterEncodings = DeleteCases[ StringTake[#, {1, -3}] & /@ FileNames[ "*.m"], "UTF-8" ];

Protect[ System`$CharacterEncodings];

ResetDirectory[];


Internal`AddHandler["MessageTextFilter", Internal`MessageButtonHandler]

(*
 Set $Path
*)

$Path =
	Which[
 		$LicenseType === "Player",
 		{
		ToFileName[ {$InstallationDirectory, "SystemFiles"}, "Links"],
        ToFileName[ {$InstallationDirectory, "AddOns"}, "Packages"],
        ToFileName[ {$InstallationDirectory, "AddOns"}, "LegacyPackages"],
        ToFileName[ {$InstallationDirectory, "SystemFiles"}, "Autoload"],
        ToFileName[ {$InstallationDirectory, "AddOns"}, "Applications"],
        ToFileName[ {$InstallationDirectory, "AddOns"}, "ExtraPackages"],
        ToFileName[ {$InstallationDirectory, "SystemFiles","Kernel"}, "Packages"],
        ToFileName[ {$InstallationDirectory, "Documentation",$Language}, "System"]
 		},
 		True,
		{
      	ToFileName[ {$InstallationDirectory, "SystemFiles"}, "Links"],
        ToFileName[ $UserBaseDirectory, "Kernel"],
    	ToFileName[ $UserBaseDirectory, "Autoload"],
    	ToFileName[ $UserBaseDirectory, "Applications"],
    	ToFileName[ $BaseDirectory, "Kernel"],
    	ToFileName[ $BaseDirectory, "Autoload"],
    	ToFileName[ $BaseDirectory, "Applications"],
    	".", 
    	HomeDirectory[],
		ToFileName[ {$InstallationDirectory, "AddOns"}, "Packages"],
    	ToFileName[ {$InstallationDirectory, "AddOns"}, "LegacyPackages"],
    	ToFileName[ {$InstallationDirectory, "SystemFiles"}, "Autoload"],
    	ToFileName[ {$InstallationDirectory, "AddOns"}, "Autoload"],
    	ToFileName[ {$InstallationDirectory, "AddOns"}, "Applications"],
    	ToFileName[ {$InstallationDirectory, "AddOns"}, "ExtraPackages"],
    	ToFileName[ {$InstallationDirectory, "SystemFiles","Kernel"}, "Packages"],
    	ToFileName[ {$InstallationDirectory, "Documentation",$Language}, "System"],
    	ToFileName[ {$InstallationDirectory, "SystemFiles","Data"}, "ICC"]
    	}];

$Epilog := Get[ "end`"] ;
 

Begin[ "Global`"];


If[MathLink`NotebookFrontEndLinkQ[$ParentLink],
    System`Private`origFrontEnd = MathLink`SetFrontEnd[$ParentLink],
(* else *)
    System`Private`origFrontEnd = Null
]


(*
	Set up parallel computation features.
*)

If[ FindFile["Parallel`Kernel`sysload`"] =!= $Failed && 
		System`Parallel`$NoParallelLoad =!= True,
	Get["Parallel`Kernel`sysload`"]]


(*
 Set up autoloading, using the new Package`DeclareLoad functionality.
 Don't do this in a player variety.
*)

If[ $LicenseType =!= "Player Pro" && $LicenseType =!= "Player",
	Package`DeclareLoad[
		{System`InstallService, WebServices`$InstalledServices,
	 	WebServices`$PrintServiceRequest, WebServices`$PrintServiceResponse,
	 	WebServices`$PrintShortErrorMessages, WebServices`$PrintWSDLDebug,
	 	WebServices`FromServiceResponse, WebServices`InstallServiceOperation,
	 	WebServices`InvokeServiceOperation, WebServices`ToServiceRequest
		}, 
		"WebServices`", Package`ExportedContexts -> {"WebServices`"}]]

Package`DeclareLoad[
		{JSONTools`ToJSON,JSONTools`FromJSON}, 
		"JSONTools`", Package`HiddenImport -> True]

Package`DeclareLoad[
		{System`URLFetch, System`URLSave,
	 	 System`URLFetchAsynchronous, System`URLSaveAsynchronous,
	 	 System`$HTTPCookies
		}, 
		"HTTPClient`", Package`HiddenImport -> True]

Developer`RegisterInputStream["HTTP", 
	StringMatchQ[#, RegularExpression["https?://.*"]] &, 
	Get["HTTPClient`"]; HTTPClient`Private`Initialize[]];

CloudObject`Private`$CloudObjectAutoloads = Hold[
	System`APIFunction, System`DefaultReturnType, 
	System`DefaultParameterType, System`CloudConnect, 
	System`CloudPut, System`CloudSave, System`CloudGet, 
	System`CloudFunction, System`CloudEvaluate, System`CloudDeploy, 
	System`ExportForm, System`LocalizeDefinitions, System`DefaultView, 
	System`CloudObject, System`$CloudBase, System`$CloudRootDirectory, 
	System`$CloudDirectory, System`FormFunction, System`CreateUUID,
	System`CloudDisconnect, System`$CloudConnected
]

Internal`DisableCloudObjectAutoloader[] := Block[{$Path},
	Quiet[CloudObject[]]
]

Package`DeclareLoad[
	List @@ CloudObject`Private`$CloudObjectAutoloads, 
	"CloudObjectLoader`",
	Package`ExportedContexts -> {"CloudObject`"}
]


QuantityUnits`Private`$QuantityUnitsAutoloads = Hold[
	System`QuantityQ,System`MixedRadix,
	System`MixedRadixQuantity,System`QuantityUnit,
	System`QuantityMagnitude,System`IndependentUnit,
	System`UnitConvert,System`CompatibleUnitQ,
	System`CommonUnits,System`UnitDimensions,
	System`UnitSimplify, System`Quantity,
	System`KnownUnitQ,Internal`DimensionToBaseUnit,
	QuantityUnits`Private`ToQuantityBox,QuantityUnits`Private`ToQuantityString,
	QuantityUnits`Private`quantifyPacletRequest, System`CurrencyConvert,
	System`QuantityVariable, System`QuantityVariableIdentifier,
	System`QuantityVariablePhysicalQuantity, System`QuantityVariableDimensions, 
	System`QuantityVariableCanonicalUnit
]

Begin["QuantityUnits`Private`"];
Internal`DisableQuantityUnits[]:=CompoundExpression[
	Set[Internal`$DisableQuantityUnits,True],
	Set[$AlphaBlockFlag,True],
	(* Make the autoload OwnValues of Quantity and friends vanish completely; *)
	(* Now that the format values come from elsewhere, need to nuke those as well *)
	ReleaseHold @ Map[
		Function[
			s, 
			CompoundExpression[
				Unprotect[s],
				ClearAttributes[s, {ReadProtected}], 
				OwnValues[s]={},
				FormatValues[s] = Select[FormatValues[s], FreeQ[#, is_Symbol /; Context[is]==="QuantityUnits`"]&]
			],
			HoldFirst
		],
		QuantityUnits`Private`$QuantityUnitsAutoloads
	],
	System`QuantityMagnitude=Identity, 
	True
];
Internal`DisablePredictiveAlphaUtilities[]:=CompoundExpression[
	Set[Internal`$DisablePredictiveAlphaUtilities,True],
	True
];
End[];

Package`DeclareLoad[
	List @@ QuantityUnits`Private`$QuantityUnitsAutoloads, 
	"QuantityUnits`",
	Package`ExportedContexts -> {"QuantityUnits`"}
]


EntityFramework`Private`$EntityFrameworkAutoloads = Hold[
       System`Entity, System`EntityValue, System`EntityProperty, System`EntityProperties,
       System`CommonName, System`CanonicalName, System`TypeName,
       Experimental`FindEntities, Internal`AddToEntityNameCache
]

(* The following are provisional option names used by EntityFramework,
   they don't need to autoload anything, but need to be declared somewhere *)
{System`FormatName, System`Qualifiers, System`SourceEntityType};

(* Prevent loading of EntityFramework package *)
Begin["EntityFramework`Private`"];
Internal`DisableEntityFramework[]:=CompoundExpression[
	Internal`$DisableEntityFramework = True;
	ReleaseHold @ Map[
		Function[
			s, 
			CompoundExpression[
				Unprotect[s],
				ClearAttributes[s, {ReadProtected}], 
				OwnValues[s]={},
				FormatValues[s] = Select[FormatValues[s], FreeQ[#, is_Symbol /; Context[is]==="EntityFramework`Private`"]&]
			],
			HoldFirst
		],
		EntityFramework`Private`$EntityFrameworkAutoloads
	],
	True
];
End[];

Package`DeclareLoad[
       List @@ EntityFramework`Private`$EntityFrameworkAutoloads,
       "EntityFrameworkLoader`"
]


FormulaData`Private`$FormulaDataAutoloads = Hold[
	System`FormulaData, System`FormulaLookup, System`RequiredPhysicalQuantities, System`ExcludedPhysicalQuantities
]


Package`DeclareLoad[
		List @@ FormulaData`Private`$FormulaDataAutoloads,
		"FormulaDataLoader`",
		Package`HiddenImport->True
	]  

Begin["FormulaData`Private`"];
Internal`DisableFormulaData[]:=CompoundExpression[
	Internal`$DisableFormulaData = True;
	ReleaseHold @ Map[
		Function[
			s, 
			CompoundExpression[
				Unprotect[s],
				ClearAttributes[s, {ReadProtected}], 
				OwnValues[s]={},
				FormatValues[s] = {}
			],
			HoldFirst
		],
		FormulaData`Private`$FormulaDataAutoloads
	],
	True
];
End[];

Package`DeclareLoad[
		{System`InflationAdjust, System`InflationMethod}, 
		"InflationAdjustLoader`",
		Package`HiddenImport -> True
	]

Package`DeclareLoad[
		{System`URLEncode, System`URLDecode, System`URLBuild, System`URLParse, System`URLShorten, System`URLExpand, 
			System`URLQueryEncode, System`URLQueryDecode, System`URLExistsQ,
			System`ServiceConnect, System`ServiceDisconnect,System`ServiceObject, System`ServiceExecute,System`SendMessage},
		"OAuth`",
		Package`HiddenImport -> True
	]
 
Package`DeclareLoad[
		{System`Classify, System`Predict, System`ClassifierFunction, System`PredictorFunction},
       "MachineLearning`"
	]

Package`DeclareLoad[{
		System`StartProcess, System`RunProcess, System`KillProcess,
		System`ProcessConnection, System`ProcessInformation, System`ProcessStatus,
		System`ProcessObject},
		"ProcessLink`"
]

Package`DeclareLoad[
		{System`StringTemplate, System`FileTemplate, System`XMLTemplate, System`NotebookTemplate,
			System`TemplateApply, System`FileTemplateApply, System`TemplateObject,
			System`TemplateIf, System`TemplateSequence, System`TemplateSlot, System`TemplateExpression, System`TemplateWith, System`TemplateBlock
		},
       "Templating`"
	]

Package`DeclareLoad[
		{
			System`DataAssembly, System`PartSpecification, 
			System`InferType, System`InduceType, System`NormalizeType, System`TypeChecksQ,
			System`ConformsQ, System`TypeQ, System`ValidTypeQ,
			System`ListType, System`TupleType, System`StructType, System`IndexType, System`ScalarType, System`Enum, System`ForeignKey,
			System`GroupedBy, System`Selected, System`CountedBy, System`Flattened, System`Computed, System`IndexedBy
		},
       "DataModel`"
	]


(*
  Start the PacletManager. 
*)

Quiet[
    Needs["PacletManager`"];
    PacletManager`Package`preparePacletManager[]
]

(*
  All contexts so far are to regarded as "system" contexts and should be excluded from parallel distribution.
  Contexts loaded from init.m files below should be *included*.
*)

SetOptions[Language`ExtendedFullDefinition, 
 "ExcludedContexts" -> Complement[
   Union[StringReplace[#, "`" ~~ ___ -> ""] & /@ 
       Contexts[]], {"Global"}]]

(*
 Utility function for loading init.m files.
*)

System`Private`loadInitFile[ System`Private`dir_] :=
	Module[ {System`Private`tmp},
		If[ (System`Private`tmp = FileNames[ "init.m", System`Private`dir]) =!= {},
			System`Private`tmp = First[ System`Private`tmp];
			Get[ System`Private`tmp];
			True,
			False]
	]

(*
 Load init.m from $InstallationDirectory/Configuration/Kernel.
*)

System`Private`loadInitFile[ ToFileName[{$InstallationDirectory, "Configuration"}, "Kernel"]];
	
(*
 If -noinit is not set then load
 
  Load init.m from $BaseDirectory/Kernel.
  Load init.m from $UserBaseDirectory/Kernel.
  Load any -initfile files
*)

If[ !MemberQ[ $CommandLine, "-noinit"],

	If[ !($LicenseType === "Player Pro" || $LicenseType === "Player"),
		System`Private`loadInitFile[ ToFileName[{$BaseDirectory}, "Kernel"]];
		System`Private`loadInitFile[ ToFileName[{$UserBaseDirectory}, "Kernel"]]];
		
		If[ MemberQ[ $CommandLine, "-initfile"],
			Do[ If[Part[$CommandLine,System`Private`tmp] === "-initfile" && 
				StringQ[ Part[ $CommandLine,System`Private`tmp+1]] , 
				Get[ Part[ $CommandLine, System`Private`tmp+1]]],
				{System`Private`tmp, Length[ $CommandLine]-1}]];
		If[ StringQ[ System`Private`tmp], Get[ System`Private`tmp]];
		]

(*
 Load init.m files from Autoload directories.
*)

Map[ If[ (System`Private`tmp = FileNames[ "init.m", ToFileName[ {#}, "Kernel"]]) =!= {}, 
	Get[ First[ System`Private`tmp]],
	If[ (System`Private`tmp = FileNames[ "init.m", #]) =!= {}, 
				Get[ First[ System`Private`tmp]]]]&,
		Select[
			Which[
				$LicenseType === "Player Pro" || $LicenseType === "Player",
				FileNames[ "*", 
					ToFileName[ {$InstallationDirectory, "AddOns", "Autoload"}]] 
				,
				True,
				Join[
 	 		    	FileNames[ "*", 
						ToFileName[ {$InstallationDirectory, "AddOns", "Autoload"}]], 
 	 		    	FileNames[ "*", 
				 		ToFileName[ {$BaseDirectory, "Autoload"}]],
 	 		    	FileNames[ "*", 
						ToFileName[ {$UserBaseDirectory,"Autoload"}]]
						]],
			(FileType[#] === Directory)&]]
			
System`Private`$InitsLoaded = True;

If[System`Private`origFrontEnd =!= Null,
    MathLink`RestoreFrontEnd[System`Private`origFrontEnd]
]


End[]

On[ Get::noopen]
On[ General::initg]
On[ General::initc]
Off[ Series::esss]
Off[NIntegrate::levswitchosc]
Off[Integrate::gener]

End[]

End[]

Null


