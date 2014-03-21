BeginPackage["JSONTools`"]

ToJSON::usage = "ToJSON[expression]	Encodes a given expression into it's corresponding JSON representation";
FromJSON::usage = "FromJSON[JSON] Decodes a given JSON string into a expression \n
FromJSON[file] Decodes a given JSON file's contents into a expression";

Begin["`Private`"]

(* ::Section:: *)
(* Internal Variables *)
(******************************************************************************)
(* ::Subsection:: *)

(* The name of the dynamic library filename *)
$DynamicLibrary = "JSONLink";

$MessageHead = General;

(* Evaluate, in order to detirmine the path of the dynamic library  *)
$LibraryDirectory = FileNameJoin[{DirectoryName[$InputFileName], "LibraryResources", $SystemID}];

(*****************************************************************************************)

(* ::Section:: *)
(* Options/Attributes *)
(******************************************************************************)
cToJSON = Null;
cFromJSON = Null;
(* ::Subsection:: *)
Options[FromJSON]={ 
	(*
		By default, the decoder expects an array or object as the input. 
		With this flag enabled, the decoder accepts any valid JSON value or type.
	*)
	"StrictDecoding"-> True,
	(*
		 By default, the decoder expects that its whole input constitutes
		 a valid JSON text, and issues an error if there is a extra data after
		 the otherwise valid JSON input. With this flag enabled, the decoder 
		 stops after decoding a valid JSON array or object, and thus allows
		 extra data after the JSON text.
	*)
	"DisableEOF" -> False
};

Options[ToJSON]={ 
	(* 
		An expertimetail feature that handles more symbols 
		than Null, True, False 
	*)
	"AllowAllSymbols"->False,
	(*
		If True compact the JSON in a nice a minimal fashion, 
		or returns nicely formatted JSON with spacing and
		proper indention if False
	*)
	"Compact" -> False, 
	(*
		Ensures that only ASCII characters are brought back.
	*)
	"ASCIIOnly" -> False, 
	(*
		Sort the the keys in alpha numeric order 
	*)
	"SortKeys" -> False , 
	(*
		Preserves the order in which the input is given.
		ie Lists and Rule Lists
	*)
	"PreserveOrder" -> True,
	(*
		Without this option set, only objects and arrays can be passed as the root input
	    to the encoding functions
	*) 
	"StrictEncoding" -> True
};
(******************************************************************************)
(* End of Attributes/Options *)


(* ::Section:: *)
(* Internal Functions *)
(******************************************************************************)
(* ::Subsection:: *)

(* 
	loadLibrary:
	Loads the dynamic library into Mathematica at runtime.
*)
loadLibrary[name_String] :=
	Check[
		Module[{lib = FindLibrary[name]},
			If[StringQ[lib] && FileExistsQ[lib],
				LibraryLoad[name],
				Throw[message["nolib"]; $Failed]
			]
		],
		Throw[message["libload"]; $Failed]
	]
(*
	loadFunction:
	A wrapper function, that cleanly loads a dynamic library function
	into a global mathematica function. 
*)	
loadFunction[name_String, inputArgs_, outputArg_] :=
	Check[
		LibraryFunctionLoad[$DynamicLibrary, name, inputArgs, outputArg]
		,
		Throw[message["lfload", $DynamicLibrary, name]; $Failed]
	];
(*
	setMessageHead:
	Sets the $MessageHead variable.
*)
setMessageHead[head_] := ($MessageHead = head);
(*
	message:
	Cleanly call imessage, using only the tag and args.
*)
message[tag_, args___] := imessage[$MessageHead, tag, args];

(*
	imessage:
	Print out a Message onto Mathematica using it's given parameters.
*)
imessage[head_, tag_, args___] := Message[MessageName[head, tag], args];

(* 
	successQ:
	Checks all of the dynamic library function calls 
	have failed.
*)
successQ[_LibraryFunctionError | _LibraryFunction | $Failed] := False
successQ[___] := True

(* 
	failQ:
	Checks all of the dynamic library function calls 
	to detirmine if they were successfully called.
*)
failQ[_LibraryFunctionError | _LibraryFunction | $Failed] := True
failQ[___] := False

(*
	initialize:
	Is responsible for loading the exported functions from the dynamic
	library. This function only needs to become executed, at least once.s
*)
initialize[] := initialize[] = Module[{},
		(* Check if the dynamic library path is a member of the Global $LibraryPath *)
		If[!MemberQ[$LibraryPath, $LibraryDirectory],
			(* Prepend the $LibraryDirectory to the $LibrayPath, considering that it isn't apart. *)
			PrependTo[$LibraryPath, $LibraryDirectory];
		];
		loadLibrary[$DynamicLibrary];
		cToJSON = loadFunction["ToJSON", LinkObject, LinkObject];
		cFromJSON = loadFunction["FromJSON", LinkObject, LinkObject];
		True
	];
	
(*
	initializedQ:
	A wrapper around the initialize function,
	returning True or False, considering
	if the dynamic library has been correctly loaded.
*)
initializedQ[] :=
	Module[{res},
		Check[
			res = Catch[initialize[]];
			If[res === $Failed,
				message["init"];
				False,
				True
			],
			False
		]
	]


(* 
	InitOptions:
	Allows the C side of the code the chance to set the options.
	So that once the corresponding library call is made, it has the correct
	options in memory.
*)
InitOptions[options_List]:= Prepend[
			(*
				
				Prepend the PrintPrecision value for correctly formatted reals, 
				while deleting duplicates. 
			*)
			DeleteDuplicates[ Flatten@Join[options,Options[$MessageHead]],( #1[[1]] === #2[[1]] )&] ,
			"PrintPrecision"-> "MachineRealPrintPrecision" /. SystemOptions["MachineRealPrintPrecision"]
]
(******************************************************************************)
(* End of the Internal Functions *)


(* ::Section:: *)
(* Exported Functions *)
(******************************************************************************)
(* ::Subsection:: *)
(*
	ToJSON:
	Encodes a given Mathematica expression into a corresponding JSON 
	representation

*)
ToJSON[expr_, opts:OptionsPattern[]] :=
	Module[{res},
		
		setMessageHead[ToJSON];
		(
			res = cToJSON[{InitOptions[List[opts]], expr}];
			If[successQ[res], 
				res,
				$Failed
			]

		) /; initializedQ[]
	]
	
(*
	FromJSON:
	Decodes a given JSON string or file into the appropriate Mathematica expression
*)
FromJSON[""] = $Failed;
FromJSON[jsonstring_String, opts:OptionsPattern[]] := Module[{res},
		setMessageHead[FromJSON];
		(
				res = cFromJSON[{InitOptions[List[opts]] , jsonstring }];
				If[successQ[res], 
					res,
					$Failed
				]
			
		) /; initializedQ[]
	]

(******************************************************************************)
(* End of the Exported Functions *)

End[]

EndPackage[]