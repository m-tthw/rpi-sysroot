(* Mathematica package *)
BeginPackage["CloudObject`"]

General::notauth = "Unable to authenticate with Wolfram Cloud server. Please try authenticating again.";
General::notperm = "Unable to perform the requested operation. Permission denied.";
General::unavailable = "The cloud service is not available. Please try again shortly.";
General::notparam = "Invalid parameters were specified.";
General::notmethod = "The specified method is not allowed.";
General::rejreq = "The specified request was rejected by the server.";(*rate limit exceeded, etc*)
General::srverr = "An unknown server error occurred.";
General::cloudnf = "No CloudObject found at the given address"; (* we would like an argument: "No CloudObject at `1`."; but it requires some refactoring *)
General::cloudunknown = "An unknown error occurred.";

Begin["`Private`"]

(*TODO: handle various error codes*)
checkError[response_, msghd_Symbol:CloudObject] :=
    With[{res = MatchQ[response, HTTPError[_Integer]]},
        If[res,
            Switch[response,
                HTTPError[400], Message[msghd::notparam],
                HTTPError[401], Message[msghd::notauth],(*might need a different message here*)
                HTTPError[403], Message[msghd::notperm],
                HTTPError[405], Message[msghd::notmethod],
                HTTPError[429], Message[msghd::rejreq],
                HTTPError[404], Message[msghd::cloudnf], (* TODO need a CloudObject to pass here *)
                HTTPError[500], Message[msghd::srverr],
                HTTPError[503], Message[msghd::unavailable],
                _, Message[msghd::cloudunknown]
            ]
        ];
        res
    ]

fetchURL[url_, elements_, options___] := If[TrueQ[$CloudConnected], authenticatedURLFetch, URLFetch][url, elements, options]

(* The asynchronous version simply ignores the requested elements. It does not return anything, it just sets off the request. *)
fetchURLAsync[url_, elements_, options___] := (
    If[TrueQ[$CloudConnected], authenticatedURLFetchAsynchronous, URLFetchAsynchronous][url, Null, options];
    {200, {}, {}}
)

contentDisplay[list:{_Integer...}] := FromCharacterCode[list]
contentDisplay[value_String] := value
contentDisplay[expr_] := ToString[expr, InputForm]

callServer[url_, mimetype_: "text/plain", httpVerb_: "GET", body_: {}, async_ : False] := Module[{response,status, headers, content, callFunction, finalURL},
   log["Calling remote server `1` with MIME type `2`", url, mimetype];
   log["Decoded URL: `1`", URLDecode[url], DebugLevel->2];
   log["Request content: `1`", contentDisplay[body], DebugLevel->2];
   (* Otherwise, check for actual authentication. *)
   If[Not[TrueQ[authenticatedQ[]]],
       With[{res=CloudConnect[]}, (*TODO: what to do for stand-alone kernel? *)
           If[UnsameQ[res, $WolframID], Message[CloudObject::notauth]; Return[HTTPError[401]]]
       ]
   ];
   finalURL = url;
   callFunction = If[async, fetchURLAsync, fetchURL];
   response = callFunction[finalURL, {"StatusCode", "Headers", "ContentData"},
       "Method"->httpVerb, "Headers"->{"Content-Type"->mimetype}, "BodyData"->body,
       "VerifyPeer"->False
   ];
   If[MatchQ[response,{_,_,_}],{status, headers, content} = response,Return[HTTPError[404]]];
   log["Response status: `1`", status];
   If[headers =!= {},
       log["Response headers: `1`", headers, DebugLevel->2];
   ];
   log["Response content: `1`", contentDisplay[content], DebugLevel->2];
   If[status =!= 200, Return[HTTPError[status]]];
   {"Content-Type" /. (Rule @@@ headers), content}
]

getUUID[cloud_, path_] := Module[{pathString, uuid},
    pathString = JoinURL[path];
    uuid = responseToString @ execute[cloud, "GET", {"files"}, Parameters -> {"path" -> pathString}];
    log["UUID for path `1`: `2`", pathString, uuid];
    If[uuid === "", None, uuid]
]

getCloudAndUUID[obj : CloudObject[uri_, ___]] :=
    Module[{cloud, uuid, user, path, ext, extraPath, search},
        {cloud, uuid, user, path, ext, extraPath, search} = parseURI[uri];
        If[uuid === None,
            uuid = getUUID[cloud, {user, path}],
        (* uuid set, check for path inside it (file inside an unnamed directory) *)
            If[extraPath =!= {},
                uuid = getUUID[cloud, {uuid, extraPath}]
            ]
        ];
        {cloud, uuid}
    ]

getCloudAndUUIDOrPath[CloudObject[uri_, ___]] :=
    Module[{cloud, uuid, user, path, ext, extraPath, search},
        {cloud, uuid, user, path, ext, extraPath, search} = parseURI[uri];
        If[extraPath === {},
            {cloud, uuid, If[path === None, None, Join[{user}, path]]},
        (* else: *)
            If[uuid === None,
            (* this will not actually happen, because extraPath is only set when uuid is set *)
                {cloud, None, Join[{user}, path, extraPath]},
            (* else *)
                {cloud, None, Join[{uuid}, extraPath]}
            ]
        ]
    ]

getCloudAndPathList[obj_CloudObject] :=
    Module[{cloud, uuid, path},
        {cloud, uuid, path} = getCloudAndUUIDOrPath[obj];
        {cloud, If[path === None, {uuid}, path]}
    ]

Options[execute] = {Parameters -> {}, Body -> {}, Type -> "text/plain", UseUUID -> True, Asynchronous -> False};

(* perform the execute locally, we are already in the cloud *)
Options[executeInCloud] = Options[execute];
executeInCloud[cloud_String, method_String, path_List : {}, OptionsPattern[]] :=
	Module[{parameters, mimetype = OptionValue[Type], body = OptionValue[Body]},

		parameters = OptionValue[Parameters];

		log["Calling server `1` `2` with MIME type `3`, parameters `4`", method,
			JoinURL[path], mimetype, ToString[parameters, InputForm],
			DebugLevel -> 2];
		If[body =!= {},
			log["Content: `1`", body, DebugLevel->2];
		];

	    ($lastExecuteResult = CloudSystem`Private`writeCallPacketService[
			CloudSystem`CloudObject`DoCloudOperation[method, path, parameters,
				mimetype, body
			]
		]) /. {
			{type_String, resultFile_String} :>
				{type, BinaryReadList[resultFile]},
			finalResult : {_String, _List} :> finalResult,
			err:HTTPError[_Integer?Positive] :> err,
			_ :> HTTPError[500]
		}
	]

(* make an HTTP request to perform the execute *)
Options[executeRemotely] = Options[execute];
executeRemotely[cloud_String, method_String, path_List : {}, OptionsPattern[]] := Module[{url},
    url = JoinURL[{cloud, path}] <> JoinURLSearch[OptionValue[Parameters]];
    callServer[url, OptionValue[Type], method, OptionValue[Body], OptionValue[Asynchronous]]
]

execute[cloud_String, method_String, path_List : {}, opts:OptionsPattern[]] :=
	If[TrueQ[System`$CloudEvaluation],
		executeInCloud[cloud, method, path, opts]
		,
		executeRemotely[cloud, method, path, opts]
	]

execute[obj_CloudObject, method : _String | Automatic : "GET", api_String : "files", subpath_List : {}, options : OptionsPattern[]] :=
    Module[{cloud, uuid, path, methodToUse},
        If[OptionValue[UseUUID] === True,
	        {cloud, uuid} = Quiet[getCloudAndUUID[obj]];
	        If[!StringQ[uuid], Message[CloudObject::cloudnf, obj]; Return[{$Failed, $Failed}]];
	        log["Executing on UUID `1`", uuid];
	        execute[cloud, method, {api, uuid, subpath}, options],
	    (* else *)
            {cloud, uuid, path} = getCloudAndUUIDOrPath[obj];
            If[method === Automatic,
                If[uuid === None,
                    methodToUse = "POST",
                    methodToUse = "PUT"
                ]
            ];
            If[uuid === None,
	            execute[cloud, methodToUse, {api, subpath}, Parameters -> Join[OptionValue[Parameters], {"path" -> JoinURL[path]}], options],
	            execute[cloud, methodToUse, {api, uuid, subpath}, options]
            ]
        ]
    ]

responseToString[{type_, content_List}, head_] := FromCharacterCode[content, "UTF-8"]
responseToString[{type_, content_String}, head_] := content
responseToString[{$Failed, $Failed}, head_] := $Failed
responseToString[response_, head_] := $Failed /; checkError[response, head]
responseToString[response_] := responseToString[response, CloudObject]

responseToExpr[response_] := responseToString[response] /. r_String :> ToExpression[r]

dumpBinary[filename_, contents_] := Module[{file},
    file = OpenWrite[filename, BinaryFormat -> True];
    BinaryWrite[file, contents];
    Close[file];
]

responseToFile[{type_, content_List}, head_:CloudObject] := Module[{tempfilename},
	tempfilename = CreateTemporary[];
	dumpBinary[tempfilename, content];
	{tempfilename, type}
]
responseToFile[{$Failed, $Failed}, head_:CloudObject] := {$Failed, $Failed}
responseToFile[response_, head_:CloudObject] := {$Failed, $Failed} /; checkError[response, head]

responseCheck[{$Failed, $Failed}, head_, result_] := $Failed
responseCheck[response_, head_, result_] :=
    If[checkError[response, head],
        $Failed,
        result
    ]
responseCheck[response_, head_ : CloudObject] := responseCheck[response, head, Null]

End[]

EndPackage[]
