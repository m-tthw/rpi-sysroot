(* Mathematica package *)
BeginPackage["CloudObject`"]

System`$Permissions;
System`ClearProperties;
System`CloudDeploy;
System`CloudEvaluate;
System`CloudExport;
System`CloudFunction;
System`CloudGet;
System`CloudImport;
System`CloudPut;
System`CloudSave;
System`CloudSymbol;
System`ExportForm;
System`Permissions;
System`Properties;
System`SetProperties;
System`CloudObjectInformation;
System`CloudObjectInformationData;
System`CloudObjects

ExportForm::usage = "ExportForm[expr, form] specifies that expr should be exported in the specified format in functions like CloudDeploy, and in external results from APIFunction and FormFunction.";

Permissions::usage = "Permissions is an option for CloudObject and related constructs that specifies permissions for classes of users to access or perform operations."

$Permissions::usage = "$Permissions is the default setting used for the Permissions option when cloud objects are created.";

CloudSymbol::usage = "CloudSymbol[uri] represents a symbol whose value is stored in the cloud.";

$UseRemoteServer::usage = "$UseRemoteServer controls whether to use the remote cloud or the local server prototype.";
CloudObject::notauth = "Unable to authenticate with Wolfram Cloud server. Please try authenticating again.";
CloudObject::notperm = "Unable to perform the requested operation. Permission denied.";
CloudObject::unavailable = "The cloud service is not available. Please try again shortly.";
CloudObject::notparam = "Invalid parameters were specified.";
CloudObject::notmethod = "The specified method is not allowed.";
CloudObject::rejreq = "The specified request was rejected by the server.";(*rate limit exceeded, etc*)
CloudObject::srverr = "An unknown server error occurred.";
CloudObject::invperm = "Invalid permissions specification `1`.";
CloudObject::notfound = "No CloudObject found at the given address.";

Begin["`Private`"]

Unprotect[CloudPut, CloudSave, CloudGet, CloudSymbol, CloudDeploy, ExportForm, CloudFunction, CloudEvaluate]

$Permissions = "Private";

Unprotect[Put, Save, Export, Get, Import];
  
(*Put*)

cloudSymbolContext = "CloudSymbols`";

definitionsToString[defs_] := StringJoin @ Riffle[Flatten[List @@ Replace[Unevaluated @ defs,
	(HoldForm[symbol_] -> def_) :> (Replace[Unevaluated@def, {
        (Attributes -> attributes_) :> 
            If[Length[attributes] > 0, 
                ToString[Unevaluated[Attributes[symbol] = attributes], InputForm],
                {}
            ],
        (DefaultValues -> options_) :>
            ReplaceAll[options,
        	    (Verbatim[HoldPattern][lhs_] -> rhs_) :> 
                    ToString[Unevaluated[lhs = rhs], InputForm]
            ],
        (Messages -> messages_) :>
            ReplaceAll[messages,
            	(Verbatim[HoldPattern][messagename_] -> message_) :> 
                    ToString[Unevaluated[messagename = message], InputForm]
            ],
        (name_ -> values_) :>
            ReplaceAll[Unevaluated@values, {
                (lhs_ -> rhs_) :> ToString[Unevaluated[lhs = rhs], InputForm],
                (lhs_ :> rhs_) :> ToString[Unevaluated[lhs := rhs], InputForm]
            }]
    }, {1}]), {1}
]], "\n\n"]

(*Prefix all symbols in a definition list (both on the LHS and RHS of rules) with a context:*)
contextifyDefinitions[defs_] := Module[{symbols, newDefs = defs},
    symbols = Cases[defs, (Verbatim[HoldForm][symbol_] -> _) :> Hold[symbol]];
    Quiet[Remove[#] &[cloudSymbolContext <> "*"], {Remove::rmnsm}];
    log["Contextifying symbols `1`", symbols];
    Apply[Function[{symbol}, newDefs = newDefs /. 
        HoldPattern[symbol] :> Evaluate[Symbol[cloudSymbolContext <> ToString[Unevaluated[symbol]]]];, {HoldAll}
    ], symbols, {1}];
    newDefs
]

cleanup[tempfilename_, expr_: Null] := (DeleteFile[tempfilename]; expr)

getRawContents[obj_CloudObject] :=
    responseToFile @ execute[obj]
    
getAPIResult[obj_CloudObject, arguments_] :=
    ToExpression[responseToString @ execute[obj, "GET", "objects", Parameters -> Join[{"view" -> "API", "resultformat" -> "WL"}, arguments]]] 

normalizePermissionsSpec["Read"] = "r";
normalizePermissionsSpec["Write"] = "w";
normalizePermissionsSpec["Execute"] = "x";
normalizePermissionsSpec[list_List] := StringJoin[normalizePermissionsSpec /@ list];
normalizePermissionsSpec[spec_String] :=
    Replace[Characters[spec], {
	    (a : ("r" | "w" | "x")) :> a,
	    invalid_ :> (Message[CloudObject::invperm, invalid]; "")
    }, {1}];
normalizePermissionsSpec[spec_] := (Message[CloudObject::invperm, spec]; {})

normalizePermissions["Public"] := {"All" -> "r", "Owner" -> "rwx"}
normalizePermissions["Private"] := {"Owner" -> "rwx"}
normalizePermissions[list_List] := list /. (user_ -> spec_) :> (user -> normalizePermissionsSpec[spec])
normalizePermissions[spec_String] := normalizePermissions[{"All" -> spec}]
normalizePermissions[Automatic] := normalizePermissions[$Permissions]
normalizePermissions[spec_] := (Message[CloudObject::invperm, spec]; {})

escapeAndNormalizePermissions = Composition[EscapeURL, toJSON, normalizePermissions]

putRawContents[obj_CloudObject, content_, mimetype_, permissions_ : Automatic] := Module[{result},
    result = responseToString @ execute[obj, Automatic, UseUUID -> False, Body -> content, Type -> mimetype, Parameters -> {"permissions" -> escapeAndNormalizePermissions[permissions]}];
    If[result === $Failed, $Failed, obj]
]

expressionMimeType["Expression"] = "application/vnd.wolfram.expression";
expressionMimeType["API"] = "application/vnd.wolfram.expression.api";
expressionMimeType["FCI"] = "application/vnd.wolfram.expression.fci";
expressionMimeType["Form"] = "application/vnd.wolfram.expression.form";
expressionMimeType["NB"] = "application/vnd.wolfram.expression.notebook";
expressionMimeType[_] = "application/vnd.wolfram.expression";
expressionMimeType[] = "application/vnd.wolfram.expression";

Options[CloudPut] = {SaveDefinitions -> False, "LocalizeDefinitions" -> False, Permissions -> Automatic, "SetIcon" -> False};

CloudPut[expr_, obj : CloudObject[uri_], type_String : "Expression", OptionsPattern[]] := 
    Module[{content, tempfilename, result},
        If[OptionValue[SaveDefinitions],
        (* save definitions *)
        	content = exprToStringBytesWithSaveDefinitions[expr, OptionValue["LocalizeDefinitions"]],
        (* do not save definitions *)
            tempfilename = CreateTemporary[];
            Put[Unevaluated[expr], tempfilename];
            content = BinaryReadList[tempfilename];
        ];
        result = putRawContents[obj, content, expressionMimeType[type], OptionValue[Permissions]];
        If[TrueQ[OptionValue["SetIcon"]],
	        SetCloudIcon[expr, obj]
        ];
        result
    ]

SetAttributes[SetCloudIcon, HoldAllComplete];
$CloudIconFormat = "image/jpg";
$CloudIconSize = Medium;
SetCloudIcon[expr_, obj_CloudObject] := 
	Module[{iconImage, iconTempFile, iconFileContent},
		iconImage = CloudObject`Iconize`IconizeThumbnails`Iconize[expr, Size -> $CloudIconSize];
		iconTempFile = CreateTemporary[];
		Export[iconTempFile, iconImage, $CloudIconFormat];
		iconFileContent = BinaryReadList[iconTempFile];
		DeleteFile[iconTempFile];
		(* TODO Ro's suggestion is to put the icon asynchronously *)
		$lastSetCloudIconResult = execute[obj, "PUT", "files", {"icon"}, Body -> iconFileContent,
			Type -> $CloudIconFormat]
	]

exprToStringBytesWithSaveDefinitions[expr_, localizeDefinitions_] := 
	Module[{defs, content, exprLine},
        defs = Language`ExtendedFullDefinition[expr];
        If[TrueQ[localizeDefinitions],
        (* localize definitions *)
            AppendTo[defs, 
                HoldForm[$CloudSymbol] -> {OwnValues -> {HoldPattern[$CloudSymbol] :> 
                    Unevaluated[expr]}}
            ];
            content = definitionsToString[contextifyDefinitions[defs]];
            exprLine = cloudSymbolContext <> "$CloudSymbol";,
        (* do not localize definitions *)
            content = definitionsToString[defs];
            exprLine = ToString[Unevaluated[expr], InputForm];
        ];
        content = content <> "\n\n" <> exprLine <> "\n";
        ToCharacterCode[content, "UTF-8"]
	]

CloudPut[expr_, options : OptionsPattern[]] := 
    CloudPut[Unevaluated[expr], CloudObject[], options]

CloudPut[expr_, uri_String, type_String : "Expression", options : OptionsPattern[]] := 
    CloudPut[Unevaluated[expr], CloudObject[uri], type, options]

CloudPut[args___] := (ArgumentCountQ[CloudPut,Length[DeleteCases[{args},_Rule,Infinity]],1,3];Null/;False)

Put[expr_, obj_CloudObject] := CloudPut[Unevaluated[expr], obj]

SetAttributes[CloudPut,{ReadProtected}];
Protect[CloudPut];

(*Save*)

Save[obj : CloudObject[uri_], expr_] := Module[{content, tempfilename},
    tempfilename = CreateTemporary[];
    Save[tempfilename, Unevaluated[expr]];
    content = BinaryReadList[tempfilename];
    putRawContents[obj, content, expressionMimeType[]]
]

CloudSave[uri_, expr_] := Save[CloudObject[uri], expr]

CloudSave[expr_] := Save[CloudObject[], expr]

CloudSave[args___] := (ArgumentCountQ[CloudSave,Length[DeleteCases[{args},_Rule,Infinity]],1,2];Null/;False)
(*TODO: deal with HoldAll in CloudSave; prevent evaluation leaks...*)
Attributes[CloudSave] = {HoldAll};

SetAttributes[CloudSave,{ReadProtected}];
Protect[CloudSave];

(*Get*)

Get[co_CloudObject] :=
	Module[{tempfilename, mimetype}, 
		{tempfilename, mimetype} = getRawContents[co];
		Which[
			tempfilename === $Failed, $Failed,
    
			mimetype === "inode/directory", Message[Get::noopen, co]; $Failed,
    
			bundleMimeTypeQ[mimetype], CloudGet[FileNameJoin[{co, ".bundle"}]],
    
			True, cleanup[tempfilename, Get[tempfilename]]
		]
   ];

bundleMimeTypeQ[mimetype_] := 
	StringQ[mimetype] && 
	StringMatchQ[mimetype, "application/vnd.wolfram.bundle" ~~ ___]

CloudGet[uri:(_String|_CloudObject)] := Get[CloudObject[uri]]

CloudGet[args___] := (ArgumentCountQ[CloudSave,Length[DeleteCases[{args},_Rule,Infinity]],1,1];Null/;False)

SetAttributes[CloudGet,{ReadProtected}];
Protect[CloudGet];

(*Import*)

mimeToFormat =
	Quiet[DeleteCases[Flatten @ Map[
	    Function[{format}, Function[{mime}, mime -> format] /@ ImportExport`GetMIMEType[format]], $ExportFormats],
	$Failed], FileFormat::fmterr];

(* Give non-"application/..." types precedence (e.g. image/png should be used instead of application/png). *)
uniqueType[types_List] := First @ SortBy[ToLowerCase /@ types, If[StringMatchQ[#, "application" ~~ __], 2, 1] &]
uniqueType[type_] := uniqueType[{type}]

formatToMime =
	Quiet[Map[# -> (If[Length[#] > 0, uniqueType @ #, "application/octet-stream"] &) @
	    ImportExport`GetMIMEType[#] &, $ExportFormats
	], FileFormat::fmterr];

mimetypeToFormat[type_, filename_: Null] := type /. Join[mimeToFormat, {
	_ -> If[filename =!= Null, FileFormat[filename], "Text"]
}]

formatToMimetype[format_] := format /. Join[formatToMime, {_ -> "application/octet-stream"}]

CloudObject /: Import[co_CloudObject, format_ : Automatic] := 
    Module[{tempfilename, mimetype},
        {tempfilename, mimetype} = getRawContents[co];
        If[tempfilename === $Failed, Return[$Failed]];
        cleanup[tempfilename, 
            Import[tempfilename, 
                If[format === Automatic, mimetypeToFormat[mimetype, tempfilename], format]
            ]
        ]
    ]
    
CloudImport[uri_, format_ : Automatic] := Import[CloudObject[uri], format]

(*Export*)

Options[CloudExport] = {Permissions -> Automatic};

CloudExport[obj : CloudObject[uri_], expr_, format_, rest___Rule] := Module[{tempfilename, content, mimetype},
    tempfilename = CreateTemporary[];
    Export[tempfilename, Unevaluated[expr], format, rest];
    content = BinaryReadList[tempfilename];
    cleanup[tempfilename];
    mimetype = formatToMimetype[format];
    putRawContents[obj, content, mimetype, Quiet[OptionValue[CloudExport, {rest}, Permissions], OptionValue::nodef]]
]

CloudExport[uri_String, expr_, format_, rest___Rule] := 
    CloudExport[CloudObject[uri], Unevaluated[expr], format, rest]
    
CloudExport[expr_, format_, rest___Rule] := 
    CloudExport[CloudObject[], Unevaluated[expr], format, rest]

CloudExport[args___] := (ArgumentCountQ[CloudExport, Length[DeleteCases[{args},_Rule,Infinity]],2,3];Null/;False)

CloudObject /: Export[obj_CloudObject, rest___] := CloudExport[obj, rest]

Protect[Put, Save, Export, Get, Import];

(* CopyFile *)

CloudObject /: CopyFile[src_, obj_CloudObject] := Module[{format, mimetype, content},
    format = FileFormat[src];
    mimetype = formatToMimetype[format];
    content = BinaryReadList[src];
    putRawContents[obj, content, mimetype]
]

CloudObject /: CopyFile[obj_CloudObject, target_] := Module[{tempfilename, mimetype},
    {tempfilename, mimetype} = getRawContents[obj];
    If[tempfilename === $Failed, Return[$Failed]];
    cleanup[tempfilename, CopyFile[tempfilename, target]]
]

CloudObject /: CopyFile[src_CloudObject, target_CloudObject] :=
    Module[{tempfilename, mimetype, content},
        {tempfilename, mimetype} = getRawContents[src];
        If[tempfilename === $Failed, Return[$Failed]];
        content = BinaryReadList[tempfilename];
	    cleanup[tempfilename, putRawContents[target, content, mimetype, "Private"]]
    ]
    
(* ReadList *)

CloudObject /: ReadList[co_CloudObject, rest___] :=
    Module[{tempfilename, mimetype},
        {tempfilename, mimetype} = getRawContents[co];
        If[tempfilename === $Failed, Return[$Failed]];
        cleanup[tempfilename, 
            ReadList[tempfilename, rest]
        ]
    ]

(* DeleteFile *)

CloudObject /: DeleteFile[co_CloudObject] :=
    responseCheck @ execute[co, "DELETE"]
    
(* DeleteDirectory *)
Unprotect[DeleteDirectory];
CloudObject /: DeleteDirectory[dir : CloudObject[uri_], OptionsPattern[]] := 
	Module[{recursive = TrueQ[OptionValue[DeleteContents]], 
		cloud, uuid, params},
		{cloud, uuid} = Quiet[getCloudAndUUID[dir]];
		If[!UUIDQ[uuid], (* named directory not found *)
			Message[DeleteDirectory::nodir, dir];
			Return[$Failed];
		];
		   
		params = {"recursive" -> ToLowerCase@ToString[recursive]};
		execute[cloud, "DELETE", {"files", uuid}, Parameters -> params] /. {
			{_String, _List} :> Return[Null] (* success *),
			HTTPError[400] :>
				If[recursive, Message[CloudObject::srverr]; $Failed, Null],
			HTTPError[404] :> (Message[DeleteDirectory::nodir, dir];
				Return[$Failed]),
			other_ :> (Message[CloudObject::srverr]; Return[$Failed])
		};
		(* assert: delete failed with status 400 and recursive was not true ->  
			directory was not empty. *)
		(* if directory is a bundle type, does it have only the bundle file? *)
		(* TODO handle bundle type *)
		Message[DeleteDirectory::dirne, dir];
		$Failed
	];
Protect[DeleteDirectory];

(* CreateDirectory *)

CloudObject /: CreateDirectory[co_CloudObject] :=
    responseCheck[
    	execute[co, Automatic, UseUUID -> False, Type -> "inode/directory"] /. {
    		HTTPError[400] :> (Message[CreateDirectory::filex, co];
    			Return[co])
    	},
    	co]

(*CloudDeploy*)

deployType[_APIFunction] = "API";
deployType[_Delayed] = "API";
deployType[_FormFunction] = "Form";
deployType[_ExternalFunction] = "FCI";
deployType[_Notebook] = "NB";
deployType[_] := "Expression";

ExportForm[expr_NotebookObject] := ExportForm[expr, "NB"]
ExportForm[expr_Notebook] := ExportForm[expr, "NB"]
ExportForm[expr_Manipulate|expr_Graphics3D] := ExportForm[Notebook[{Cell[BoxData[ToBoxes[expr]], "Output"]}], "NB"]

Options[CloudDeploy] = {Permissions -> Automatic};

CloudDeploy[uri_CloudObject, apigroup_APIFunctionGroup, opts:OptionsPattern[]] := 
	apiCloudDeploy[uri, apigroup, opts]

CloudDeploy[uri_CloudObject, expr_, OptionsPattern[]] := 
    CloudPut[Unevaluated[expr], CloudObject[uri], deployType[Unevaluated[expr]], SaveDefinitions -> True, Permissions -> OptionValue[Permissions]]

CloudDeploy[uri_CloudObject, ExportForm[expr_, format_, rest___], OptionsPattern[]] :=
    CloudExport[CloudObject[uri], Unevaluated[expr], format, rest, Permissions -> OptionValue[Permissions]]

CloudDeploy[uri_CloudObject, expr_NotebookObject|expr_Notebook|expr_Manipulate|expr_Graphics3D, options : OptionsPattern[]] :=
    CloudDeploy[uri, ExportForm[Unevaluated[expr]], options]

CloudDeploy[uri_String, expr_, options : OptionsPattern[]] := CloudDeploy[CloudObject[uri], Unevaluated[expr], options]

CloudDeploy[expr_, options : OptionsPattern[]] := CloudDeploy[CloudObject[], Unevaluated[expr], options]

CloudDeploy[args___] := (ArgumentCountQ[CloudDeploy,Length[DeleteCases[{args},_Rule,Infinity]],1,2];Null/;False)

SetAttributes[CloudDeploy,{ReadProtected}];
Protect[CloudDeploy];

Options[apiCloudDeploy] = Options[CloudDeploy];
apiCloudDeploy[dest:CloudObject[uri_], 
	APIFunctionGroup[apifunctions : {Rule[_String, _APIFunction] ...}, groupOptions___?OptionsQ], 
	OptionsPattern[]] := 
	Module[{uriparts = parseURI[uri], useUUID, cloud, diruuid, 
		apifunctionObjects, apigroup, 
		permissionsOptionValue = OptionValue[Permissions], 
		bundleFilename = ".bundle"},
		(* TODO validate the API names *)
   
		(* Step 1 of 3. Ensure the APIFunctionGroup bundle directory exists *)
		(* TODO check whether something exists at the destination *)
		If[!FileExistsQ[dest],
			createBundle[dest, ".api"]
		];

		{cloud, diruuid} = Take[uriparts, 2];
		useUUID = Quiet[UUIDQ[diruuid]];(* is this APIFunctionGroup anonymous? *)

		(* Step 2 of 3. deploy the individual functions *)
		apifunctionObjects = $lastAPIResult = Map[
			deployAPIFunctionInAPIFunctionGroup[cloud, diruuid, #]&,
			apifunctions
		];
		(* TODO check if any apifunctionObjects failed *)

		(* Step 3 of 3. deploy the APIFunctionGroup content *)
		apigroup = APIFunctionGroup[
			Map[Apply[Rule, #]&, 
				Transpose[{Map[First, apifunctions], apifunctionObjects}]
			], 
			groupOptions];
		If[useUUID,
			putNamedExpressionIntoUnnamedDirectory[uriparts[[1]], diruuid, 
				bundleFilename, apigroup, expressionMimeType["Expression"], 
				permissionsOptionValue],
		     (* else *)
			CloudPut[apigroup, CloudObject[uri <> "/" <> bundleFilename], 
				Permissions -> permissionsOptionValue]
		];

		dest
   ];

createBundle[dest_CloudObject, mimeTypeExtension_String:""] := 
	responseCheck[execute[dest, Automatic, UseUUID -> False, 
		Type -> "application/vnd.wolfram.bundle"<>mimeTypeExtension], dest];

Options[deployAPIFunctionInAPIFunctionGroup] = {Permissions -> Automatic};
deployAPIFunctionInAPIFunctionGroup[apiFunctionGroup_CloudObject, 
	name_String -> apiFn_APIFunction, options:OptionsPattern[]] := 
	Module[{cloud, diruuid},
		{cloud, diruuid} = getCloudAndUUID[apiFunctionGroup];
		deployAPIFunctionInAPIFunctionGroup[cloud, diruuid, name -> apiFn,
			options]
	]

deployAPIFunctionInAPIFunctionGroup[cloud_String, diruuid_String?UUIDQ, 
	name_String -> apiFn_APIFunction, OptionsPattern[]] := 
	putNamedContentsIntoUnnamedDirectory[cloud, diruuid, name, 
		exprToStringBytesWithSaveDefinitions[apiFn, False], 
		expressionMimeType["API"], OptionValue[Permissions]]

putNamedExpressionIntoUnnamedDirectory[obj_CloudObject, filename_String,
	contents_, mimeType_String:expressionMimeType["Expression"], 
	permissions_:Automatic] := 
	Module[{cloud, diruuid},
		{cloud, diruuid} = getCloudAndUUID[obj];
		putNamedExpressionIntoUnnamedDirectory[cloud, diruuid, filename, 
			contents, mimeType, permissions]
	]

putNamedExpressionIntoUnnamedDirectory[cloud_, diruuid_, filename_String, contents_, mimeType_String, permissions_] := 
	putNamedContentsIntoUnnamedDirectory[cloud, diruuid, filename,
		ToCharacterCode[ToString[contents, InputForm]], mimeType, permissions];

putNamedContentsIntoUnnamedDirectory[cloud_, diruuid_, filename_String, contents_List, mimeType_String, permissions_] :=
	Module[{accessJSON = toJSON[normalizePermissions[permissions]]},
		responseToString @ 
		execute[cloud, "POST", {"files"}, Type -> mimeType, 
			Parameters -> {"path" -> diruuid <> "/" <> filename, 
				"permissions" -> EscapeURL[accessJSON]}, Body -> contents
		] /. {
			uuid_String?UUIDQ :> (CloudObject[cloud <> "/files/" <> uuid]),
			_ :> $Failed
		}
	]

putNamedObjectIntoNamedDirectory[cloud_, path_String, contents_List, mimeType_String, permissions_] :=
	Module[{accessJSON = toJSON[normalizePermissions[permissions]]},
		responseToString @ 
		execute[cloud, "POST", {"files"}, Type -> mimeType, 
			Parameters -> {"path" -> "user-" <> $WolframUUID <> "/" <> path, 
				"permissions" -> EscapeURL[accessJSON]}, Body -> contents
		] /. {
			uuid_String?UUIDQ :> CloudObject[cloud <> "/files/" <> StringTrim[path, "/"]],
			_ :> $Failed
		}
	]

(* CloudSymbol *)

DownValues[CloudSymbol] = {
    HoldPattern[CloudSymbol[uri_]] :> CloudGet[uri]
};

UpValues[CloudSymbol] = {
    HoldPattern[Set[CloudSymbol[uri_], expr_]] :> (CloudPut[expr, uri]; expr),
    HoldPattern[SetDelayed[CloudSymbol[uri_], expr_]] :> (CloudPut[Unevaluated[expr], uri];)
};

SetAttributes[CloudSymbol,{ReadProtected}];
Protect[CloudSymbol];

(* TODO: override more Set-related operations *)

(*CloudFunction*)

(*CloudFunction stores an expression as an APIFunction in the cloud and executes it (in the cloud).*)
CloudFunction[expr_][args___] := 
    Module[{co},
        Block[{formalargs},
	        co = CloudPut[APIFunction[{{"args", "UnsafeExpression"} -> formalargs}, expr @@ formalargs], SaveDefinitions -> True];
	        If[co === $Failed, Return[$Failed]];
	        getAPIResult[co, {"args" -> EscapeURL[ToString[{args}, InputForm]]}]
        ]
    ]
    
CloudFunction[obj_CloudObject][args___] := CloudFunction[Get[obj]][args]

CloudFunction[args___] := (ArgumentCountQ[CloudFunction,Length[DeleteCases[{args},_Rule,Infinity]],1,1];Null/;False)
(*CloudEvaluate*)

SetAttributes[CloudFunction,{ReadProtected}];
Protect[CloudFunction];

CloudEvaluate[expr_] := CloudFunction[expr &][]

CloudEvaluate[args___] := (ArgumentCountQ[CloudEvaluate,Length[DeleteCases[{args},_Rule,Infinity]],1,1];Null/;False)
(*TODO: deal with HoldAll; prevent evaluation leaks*)
Attributes[CloudEvaluate] = {HoldAll};

SetAttributes[CloudEvaluate,{ReadProtected}];
Protect[CloudEvaluate];

(* Properties *)

Unprotect[SetProperties];

SetProperties[obj_CloudObject, key_String -> value_String] :=
    responseCheck @ execute[obj, "PUT", "files", {"properties", key}, Body -> ToCharacterCode[value, "UTF-8"]]

Protect[SetProperties];

Unprotect[Properties];

Properties[obj_CloudObject] := Module[{content},
    content = responseToString @ execute[obj, "GET", "files", {"properties"}];
    If[content === $Failed, Return[$Failed]];
    If[content === "", Return[{}]];
    ImportString[content, "JSON"]
]

Properties[obj_CloudObject, key_String] := 
	execute[obj, "GET", "files", {"properties", key}] /. {
		HTTPError[404] :> 
	    	If[FileExistsQ[obj], 
	    		Missing["Undefined"], 
	    		Message[CloudObject::notfound, obj];
	    		$Failed
	    	],
	    {type_String, contentBytes:{_Integer ...}} :> 
	    	(* server is returning a JSON object for some reason *)
	    	ImportString[FromCharacterCode[contentBytes], "JSON"][[1,2]],
	    _ :> (Message[CloudObject::srverr]; $Failed)
	}

(*
Properties[obj_CloudObject, key_String] :=
	Properties[obj] /. {
		properties_List :> Replace[key, Join[properties, {_ :> Missing["Undefined"]}]],
		_ :> (* CloudObject::notfound should already be issued *) $Failed
	}
*)
Protect[Properties];

SetAttributes[CloudObject,{ReadProtected}];
Protect[CloudObject]

(* FileExistsQ *)
Unprotect[FileExistsQ];
Unprotect[CloudObject];

CloudObject /: FileExistsQ[CloudObject[uri_String]] := 
	Module[{uriparts = parseURI[uri], cloud},
		cloud = First[uriparts];
		If[MatchQ[uriparts, {_String, _String, ___}],
			(* anonymous cloud object *)
			MatchQ[execute[cloud, "GET", {"files", uriparts[[2]], {"path"}}],
				{_String, _List}],
			(* named cloud object *)
			MatchQ[execute[cloud, "GET", {"files"}, 
				Parameters -> {"path" -> uriparts[[3]] <> "/" <> 
					FileNameJoin[uriparts[[4]], OperatingSystem -> "Unix"]}],
				{_String, _List}]
		]
	];

Protect[FileExistsQ];

(* List objects *)
CloudObjectsByType[contentType_String] := 
	Module[{response, uuids},
		response = responseToString @ execute[$CloudBase, "GET", {"files"}, 
			Parameters->{"type" -> contentType}];
		If[!StringQ[response], Return[$Failed]];
		uuids = Map[FileNameTake[#, -1]&, StringSplit[response]];
		Map[cloudObjectFromUUID, uuids]
	]

CloudObjects[] := unnamedCloudObjects[]

CloudObjects[path_String] := CloudObjects[CloudObject[path]]

unnamedCloudObjects[] := 
	execute[$CloudBase, "GET", {"files"}, Parameters -> {"path" -> ""}] /. {
		err_HTTPError :> (Message[CloudObject::notfound]; $Failed),
		{_, bytes_List} :>
			uuidListingToCloudObjects[bytes]
	}

uuidListingToCloudObjects[bytes_List] := 
	uuidListingToCloudObjects[FromCharacterCode[bytes]]
	
uuidListingToCloudObjects[listing_String] := 
	Cases[Map[uuidListEntryToCloudObject, StringSplit[listing]], _CloudObject]

uuidListEntryToCloudObject[text_String] := 
	StringDrop[text, 7] /. {
		uuid_?UUIDQ :> cloudObjectFromUUID[uuid],
		_ :> $Failed
	}

CloudObjects[dir_CloudObject] := 
	Module[{cloud, uuid},
		{cloud, uuid} = getCloudAndUUID[dir];
		If[!UUIDQ[uuid], Message[CloudObject::notfound]; $Failed];
		execute[$CloudBase, "GET", {"files", uuid}] /. {
			err_HTTPError :> (Message[CloudObject::notfound]; $Failed),
			{_, bytes_List} :>
				uuidListingToCloudObjects[bytes]
		}
	]

(* Cloud file name manipulation *)

Unprotect[FileNameJoin]

FileNameJoin[{CloudObject[uri_, rest___], path___}] := CloudObject[JoinURL[uri, path], rest]

Protect[FileNameJoin]

(* CloudObjectInformation *)
CloudObjectInformation[obj_CloudObject] := 
	Module[{cloud, uuid, result, allinfo, files, mimetype},
		{cloud, uuid} = getCloudAndUUID[obj];
		If[cloud === $Failed, Return[$Failed]];
		result = execute[cloud, "GET", {"files", uuid, "info"}];
		If[result === HTTPError[404],
			Message[CloudObject::notfound, obj];
			Return[$Failed]
		];
		If[!MatchQ[result, {_String, {_Integer ...}}],
			Message[CloudObject::srverr];
			Return[$Failed]
		];
		allinfo = Check[ImportString[FromCharacterCode[Last[result]], "JSON"],
			Message[CloudObject::srverr];
			Return[$Failed]];
		files = "files" /. allinfo;
		If[Length[files] =!= 1,
			Message[CloudObjectInformation::dirbug]; (* internal error -- info about directories is broken right now *)
			Return[$Failed]
		];
		info = files[[1]];
		mimetype = "type" /. info;
		System`CloudObjectInformationData[<|
			"UUID" -> ("uuid" /. info),
			"Name" -> ("name" /. info),
			"OwnerWolframUUID" -> ("owner" /. info),
			"MimeType" -> mimetype,
			"FileType" -> 
				If[mimetype === "inode/directory" || bundleMimeTypeQ[mimetype],
					Directory,
					File
				],
			"FileByteCount" -> ("fileSize" /. info),
			"Created" -> DateString[DateList[("created" /. info)]],
			"LastAccessed" -> DateString[DateList[("lastAccessed" /. info)]]
		|>]
	]

(* FileType *)

Unprotect[FileType, CloudObject];

CloudObject /: FileType[co_CloudObject] := 
	With[{info = Quiet[CloudObjectInformation[co]]},
		If[Head[info] === System`CloudObjectInformationData,
			First[info]["FileType"],
			None
		]
	]

Protect[FileType, CloudObject];

End[]

EndPackage[]
