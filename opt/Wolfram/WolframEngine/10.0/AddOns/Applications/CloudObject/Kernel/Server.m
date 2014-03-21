(* Mathematica package *)
BeginPackage["CloudObject`"]
(* Exported symbols added here with SymbolName::usage *)

Begin["`Private`"]

CloudServer::uuidnotfound = "Object with UUID `1` does not exist.";
CloudServer::invalidurl = "URL `1` is invalid.";

cloudDomain = "http://wolframcloud.com/";

storage = FileNameJoin[{Directory[], "CloudObjectStorage"}];

(* Do not create the storage directory by default. This is only the server prototype code and should not run in production. *)
createStorageDirectory[] :=
    Quiet[CreateDirectory[storage, CreateIntermediateDirectories -> True], {CreateDirectory::filex}];
    
(* TODO: If this is ever used again, use Association instead of Collection. *)

$CollectionMissing = $Failed;

(*These Collections will eventually be database tables:*)
objects = Collection[{}]; (* UUID -> object *)
userNamedObjects = Collection[{}]; (* {path___} -> object UUID *)

joinPath[names___] := StringJoin[Riffle[Flatten[{names}], "/"]]

(*Dummy authentication*)
currentUser := $WolframUUID

(*Creating & finding objects*)

(*The attributes of a cloud object:*)
{coUUID, coName, coParent, coOwner, coType, coDefaultView, coEnabledViews, coAccess, coTags} = Range[9];

createObject[name_String: "", parent_: Null, uuidGiven_: Null] := 
    Module[{uuid},
        uuid = If[uuidGiven === Null, CreateUUID[], uuidGiven];
        objects[uuid] = {uuid, name, parent, currentUser,
        	"" (* type *),
        	"" (* default view *),
            {} (* enabled views *),
            {} (* access specifications *),
            {} (* tags *)
        };
        If[name =!= "", userNamedObjects[getPath[uuid]] = uuid];
        uuid
    ]

(*Find a cloud object UUID given its path of the form {username, dir1, ..., dirN, file}:*)
findObject[{first_, rest___}, create_: False, parent_: {}, parentUUID_: Null] := 
    Module[{nextPath = Join[parent, {first}], nextUUID, child},
        nextUUID = userNamedObjects[nextPath];
        If[create && nextUUID === $CollectionMissing,
            nextUUID = createObject[first, parentUUID];
        ];
        If[nextUUID =!= $CollectionMissing,
            child = findObject[{rest}, create, nextPath, nextUUID];
            If[child === $Failed,
            	{nextUUID, {rest}},
            	child
            ],
        (* else: nextPath not existing yet *)
            $Failed
        ]
    ]

findObject[{}, ___] := $Failed

(*Physical files*)
getPath[uuid_] := Module[{object = objects[uuid], name, parent, owner},
    If[object === $Failed, Message[CloudServer::uuidnotfound, uuid]; Return[{}]];
    name = object[[coName]];
    parent = object[[coParent]];
    owner = object[[coOwner]];
    If[parent === Null,
    	If[name === "", {uuid}, {name}],
    (* else *)
        Join[getPath[parent], {name}]
    ]
]

(*Get the filename (and create the corresponding file or directory) given a path:*)
getFilename[path_, isDir_: False] := Module[{iteration = 1, result}, 
    result = Fold[Module[{name = #2, filename, dirname, file},
        filename = FileNameJoin[{#1, name}];
        dirname = FileNameJoin[{#1, name <> "."}];
        If[iteration < Length[path] || isDir,
        (* expect a directory *)
            If[FileExistsQ[dirname],
            (* dirname exists *)
                log["Directory `1` already exists", dirname]; file = dirname,
            (* else: dirname does not exist *)
                If[FileExistsQ[filename],
                    log["File `1` already exists", filename];
                    If[DirectoryQ[filename],
                    (* filename is a directory *)
                        file = filename,
                    (* else: dirname does not exist, filename exists but is not a directory *)
                        log["Creating new directory `1`", dirname];
                        file = dirname; 
                        CreateDirectory[dirname]
                    ],
                (* else: neither filename nor dirname exist yet *)
                    log["Creating new file directory `1`", filename];
                    file = filename; CreateDirectory[filename]
                ]
            ],
        (* else: expect a file *)
            If[FileExistsQ[filename],
                If[DirectoryQ[filename],
                    log["Renaming directory `1` to `2`", filename, dirname];
                    RenameFile[filename, dirname]
                ],
            (* else *)
                log["Creating new file `1`", filename];
                Put[filename]
            ];
            file = filename
        ];
        ++iteration;
        file
    ] &, storage, path];
    log["File name for path `1`: `2`", path, result];
    result
]

readContents[uuid_] := Module[{filename = getFilename[getPath[uuid]]},
    BinaryReadList[filename]
]

dumpBinary[filename_, contents_] := Module[{file},
    file = OpenWrite[filename, BinaryFormat -> True];
    BinaryWrite[file, contents];
    Close[file];
]

writeContents[uuid_, contents_] := Module[{filename = getFilename[getPath[uuid]]},
    log["Writing to file `1`", filename];
    dumpBinary[filename, contents];
]

(*JSON*)

toJSON[s_String] :=
    "\"" <> StringJoin[
	    If[32 <= # < 128 && ! MemberQ[ToCharacterCode["\"\\"], #], 
	        FromCharacterCode[#], "\\u" <> IntegerString[#, 16, 4]
	    ] & /@ ToCharacterCode[s]
	] <> "\""

toJSON[s_Symbol] := toJSON[SymbolName[s]]

toJSON[n_?NumberQ] := ToString[n]

isObject[list_] := 
    Length[list] > 0 && (And @@ (MatchQ[#, _ -> _] & /@ DeleteCases[list, Null]))

toJSON[list_List?isObject] := 
    "{" <> StringJoin @@ Riffle[DeleteCases[list, Null] /.
        	(name_ -> value_) :> toJSON[name] <> ":" <> toJSON[value], ","
    ] <> "}"

toJSON[list_List] := 
    "[" <> StringJoin @@ Riffle[toJSON /@ DeleteCases[list, Null], ","] <> "]"

(*Server-side API handling*)

handleAPI[object_, extraPath_, search_] := 
    Module[{filename = getFilename[getPath[object[[1]]]], func, body, params, result, type, response},
        func = Get[filename];
        If[Head[func] =!= APIFunction, func = APIFunction[body[##]] /. body -> func];
        log["Calling API function `1`", func];
        params = extraPath;
        search /. {
        	("args[]" -> value_) :> AppendTo[params, value],
        	(name_ -> value_) :> AppendTo[params, Collection[{name -> value}]]
        };
        result = func @@ params;
        type = "type" /. search;
        If[type === "type" && Head[func] === APIFunction, 
            type = DefaultReturnType /. Options[func]
        ];
        response = Switch[type,
            "wl",
            	{expressionMIMEType, ToCharacterCode[ToString[result, InputForm], "UTF-8"]},
            _,
            	{"text/json", ToCharacterCode[toJSON[{"result" -> ToString[result, InputForm]}], "UTF-8"]}
        ];
        log["API response: `1`", response];
        response
    ]

(*HTTP interface*)
(*This is the centerpiece connecting client- and server-side code. It mimics issueing an HTTP request on the client side, given URL, MIME-type, HTTP verb, and a raw request body (containing byte data).*)
(*It does whatever a Java controller might do (parsing the URL and processing the request), and returns something corresponding to an HTTP response, containing the MIME-type and raw byte data of the raw response body.*)
(*On the client side, this will eventually be replaced by URLFetch or something similar.*)

$Error404 = "HTTP 404";

getSearchParam[search_, name_, default_: ""] := name /. Join[search, {_ -> default}]

call[url_, mimetype_: "text/plain", httpVerb_: "GET", body_: {(* list of bytes in binary data *)}] := 
    Module[{verb, view, cloud, uuid, user, path, ext, extraPath, search, foundObject, object, defaultView},
        log["Calling `1` (`2`, `3`)", url, httpVerb, mimetype];
        {cloud, uuid, user, path, ext, extraPath, search} = 
            parseURI[url, cloudDomain, currentUser, {"", "objects", currentUser},
            	{"", "objects", currentUser}, "objects", "files"
            ];
        log["Parsed URI: `1`", {cloud, uuid, user, path, ext, extraPath, search}];
        verb = getSearchParam[search, "method", httpVerb];
        view = getSearchParam[search, "view", ext];
        If[uuid === False,
            If[path === False,
                Message[CloudServer::invalidurl, url]; Return[Null],
            (* else: path specified, but no UUID *)
                foundObject = findObject[Join[{user}, path], verb === "PUT"];
                    (* if PUT request, create the file *)
                If[foundObject === $Failed, uuid = False, {uuid, extraPath} = foundObject];
            ]
        ];
        object = objects[uuid];
        log["View: `1`, object: `2`", view, object];
        If[view === False && object =!= $CollectionMissing, 
            view = object[[coDefaultView]];
            log["Using default view `1`", view]
        ];
        If[view === False || view === "",
        	view = "file"
        ];
        If[!MatchQ[view, "api" | "form"] && Length[extraPath] > 0,
            (* only allow positional arguments separated by / in certain views *)
            log["Unexpected extra arguments `1` in view `2`", extraPath, view];
            Return[$Error404];
        ];
        If[object === $CollectionMissing && view =!= "file", Return[$Error404]];
        Switch[view,
            "api",
                handleAPI[object, extraPath, search],
            "file",
                If[verb =!= "GET" && object === $CollectionMissing,
                    If[uuid === False,
                        {uuid, extraPath} = findObject[Join[{user}, path], True],
                    (* else *)
                        createObject["", Null, uuid];
                    ];
                    object = objects[uuid];
                ];
                Switch[verb,
                    "PUT",
                        defaultView = getSearchParam[search, "defaultview", ""];
                        log[mimetype];
                        writeContents[uuid, body]; 
                        objects[uuid] = ReplacePart[object, {coType -> mimetype, coDefaultView -> defaultView}];
                        {Null, Null},
                    "GET",
                        If[object === $CollectionMissing, Return[$Error404]]; 
                        {object[[coType]], readContents[uuid]}
                ]
        ]
    ]
  
End[];

EndPackage[];
