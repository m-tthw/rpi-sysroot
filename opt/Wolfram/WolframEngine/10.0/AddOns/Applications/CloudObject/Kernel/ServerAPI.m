(* Mathematica package *)
BeginPackage["CloudObject`"]

Begin["`Private`"]

$UseRemoteServer = True;

HTTPError::usage = "HTTPError[code] represents an error while communicating with the server.";
(*TODO: handle various error codes*)
checkError[response_] := With[{res=MatchQ[response, HTTPError[_Integer]]},
    If[res, 
        Switch[response,
            HTTPError[400], Message[CloudObject::notparam],
            HTTPError[401], Message[CloudObject::notauth],(*might need a different message here*)
            HTTPError[403], Message[CloudObject::notperm],
            HTTPError[405], Message[CloudObject::notmethod],
            HTTPError[429], Message[CloudObject::rejreq],
            HTTPError[404], Message[CloudObject::notfound],
            HTTPError[500], Message[CloudObject::srverr],
            HTTPError[503],Message[CloudObject::unavailable],
            _,Message[CloudObject::notfound]
        ]
    ];
    res
]

callServer[url_, mimetype_: "text/plain", httpVerb_: "GET", body_: {}] := Module[{response,status, headers, content, callFunction, userid, finalURL},
   If[$UseRemoteServer === True,
       log["Calling remote server `1` with MIME type `2`", url, mimetype, DebugLevel -> 2];
       If[body =!= {},
           log["Content: `1`", body, DebugLevel -> 3];
       ];
       If[$CloudEvaluation === True,
       (* If we're in the cloud, skip authentication and pass the user ID as a URL parameter. *)
           If[StringMatchQ[url, __ ~~ "?" ~~ __], userid = "&userId=" <> $WolframUUID, userid = "?userId=" <> $WolframUUID];
           finalURL = url <> userid,
       (* Otherwise, check for actual authentication. *)
           If[Not[TrueQ[authenticatedQ[]]],
               With[{res=CloudConnect[]}, (*TODO: what to do for stand-alone kernel? *)
                   If[UnsameQ[res, $WolframID], Message[CloudObject::notauth]; Return[HTTPError[401]]]
               ]
           ];
           finalURL = url;
       ];
       callFunction = If[TrueQ[$CloudConnected], authenticatedURLFetch, URLFetch];(*TODO: this is where CallPacket would go to call controler*)
       response = callFunction[finalURL, {"StatusCode", "Headers", "ContentData"}, 
           "Method"->httpVerb, "Headers"->{"Content-Type"->mimetype}, "BodyData"->body,
           "VerifyPeer"->False
       ];
       If[MatchQ[response,{_,_,_}],{status, headers, content} = response,Return[HTTPError[404]]];
       log["Response status: `1`; headers: `2`", status, headers, DebugLevel -> 2];
       If[content =!= {},
           log["Response content: `1`", content, DebugLevel -> 3];
       ];
       If[status =!= 200, Return[HTTPError[status]]];
       {"Content-Type" /. (Rule @@@ headers), content},
   (* else *)
       call[url, mimetype, httpVerb, body]
   ]
]

getUUID[cloud_, path_] := Module[{pathString, uuid},
    pathString = JoinURL[path];
    uuid = responseToString @ execute[cloud, "GET", {"files"}, Parameters -> {"path" -> pathString}];
    log["UUID for path `1`: `2`", pathString, uuid];
    If[uuid === "", False, uuid]
]

getCloudAndUUID[obj : CloudObject[uri_]] :=
    Module[{cloud, uuid, user, path, ext, extraPath, search},
        {cloud, uuid, user, path, ext, extraPath, search} = parseURI[uri];
        If[!uuid,
            uuid = getUUID[cloud, {user, path}]
        ];
        {cloud, uuid}
    ]

getCloudAndUUIDOrPath[obj : CloudObject[uri_]] :=
    Module[{cloud, uuid, user, path, ext, extraPath, search},
        {cloud, uuid, user, path, ext, extraPath, search} = parseURI[uri];
        If[extraPath === {},
            {cloud, uuid, Join[{user}, path]},
        (* else: *)
            If[uuid === False,
            (* this will not actually happen, because extraPath is only set when uuid is set *)
                {cloud, False, Join[{user}, path, extraPath]},
            (* else *)
                {cloud, False, Join[{uuid}, extraPath]}
            ]
        ]
    ]

Options[execute] = {Parameters -> {}, Body -> {}, Type -> "text/plain", UseUUID -> True};

execute[cloud_String, method_String, path_List : {}, OptionsPattern[]] := Module[{url},
    (* TODO: Here is the place to switch to a Java CallPacket mechanism in the cloud. *)
    url = JoinURL[{cloud, path}] <> JoinURLSearch[OptionValue[Parameters]];
    callServer[url, OptionValue[Type], method, OptionValue[Body]]
]

execute[obj_CloudObject, method : _String | Automatic : "GET", api_String : "files", subpath_List : {}, options : OptionsPattern[]] :=
    Module[{cloud, uuid, path, methodToUse},
        If[OptionValue[UseUUID] === True,
	        {cloud, uuid} = getCloudAndUUID[obj];
	        If[uuid === False, Message[CloudObject::notfound]; Return[{$Failed, $Failed}]];
	        execute[cloud, method, {api, uuid, subpath}, options],
	    (* else *)
            {cloud, uuid, path} = getCloudAndUUIDOrPath[obj];
            If[method === Automatic,
                If[uuid === False,
                    methodToUse = "POST",
                    methodToUse = "PUT"
                ]
            ];
            If[uuid === False,
	            execute[cloud, methodToUse, {api, subpath}, Parameters -> Join[OptionValue[Parameters], {"path" -> JoinURL[path]}], options],
	            execute[cloud, methodToUse, {api, uuid, subpath}, options]
            ]
        ]
    ]

responseToString[{type_, content_List}] := FromCharacterCode[content, "UTF-8"]
responseToString[{type_, content_String}] := content
responseToString[{$Failed, $Failed}] := $Failed
responseToString[response_?checkError] := $Failed

responseToFile[{type_, content_List}] := Module[{tempfilename},
	tempfilename = CreateTemporary[];
	dumpBinary[tempfilename, content];
	{tempfilename, type}
]
responseToFile[{$Failed, $Failed}] := {$Failed, $Failed}
responseToFile[response_?checkError] := {$Failed, $Failed}

responseCheck[_] := Null
responseCheck[{$Failed, $Failed}] := $Failed
responseCheck[response_?checkError] := $Failed

End[]

EndPackage[]