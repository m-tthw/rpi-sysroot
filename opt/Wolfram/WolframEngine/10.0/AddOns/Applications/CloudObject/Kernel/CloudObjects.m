(* Mathematica Package *)

BeginPackage["CloudObject`"]
System`CloudObject;
System`$CloudBase;

System`CloudDirectory;
System`SetCloudDirectory;

System`CloudObject::usage = 
  "CloudObject[uri] is a reference to an object in the cloud.";

CloudBase::usage = 
  "CloudBase is an option to CloudObject setting the base cloud.";

$CloudBase::usage = "$CloudBase is the current cloud where new cloud objects are created in.";

CloudDirectory::usage = "CloudDirectory[] gives the current directory in the cloud, within the user's root directory.";
SetCloudDirectory::usage = "SetCloudDirectory[dir] sets the current directory in the cloud, within the user's root directory.";

$CloudDebug::usage = "$CloudDebug controls whether debug information should be printed while accessing cloud functionality.";
$CloudDebugLevel::usage = "$CloudDebugLevel controls the level of debug information.";

Begin["`Private`"]

expressionMIMEType = "application/vnd.wolfram.expression";

userUUIDPrefix = "user-";

$CloudRootDirectory::usage = "$CloudRootDirectory is the root directory in the cloud, which absolute URIs are based on.";
$CloudDirectory::usage = "$CloudDirectory is the current directory inside $CloudRootDirectory, which relative URIs are based on.";

$CloudDebug = False;
$CloudDebugLevel = 3;

If[!StringQ[$UrlScheme],
	$UrlScheme = "https";
];
$CloudBase = "https://www.devel.wolframcloud.com/";
If[ValueQ[CloudSystem`$ApplicationDomain],
	If[CloudSystem`$ApplicationDomain === "localhost",
		$UrlScheme = "http";
	];
    $CloudBase = $UrlScheme<>"://" <> CloudSystem`$ApplicationDomain <> "/";
];
$CloudRootDirectory = "/objects/~/";
$CloudDirectory = "";

CloudDirectory[] := $CloudDirectory;
SetCloudDirectory[dir_] := ($CloudDirectory = dir);

$CloudObjectsRoot = "/objects";
$CloudFilesRoot = "/files";

Options[log] = {DebugLevel -> 1};

log[msg_String, Shortest[args___], OptionsPattern[]] := 
    If[$CloudDebug && $CloudDebugLevel >= OptionValue[DebugLevel], 
        Print[ToString @ StringForm[msg, args]]
    ]

DebugLevel::usage = "DebugLevel is an option to log.";

parseURI[uri_, currentCloud_, currentUser_, rootPath_, currentPath_, objectsRoot_, filesRoot_] := 
    Module[{protocol, host, pathname, search, cloud = currentCloud, user = currentUser, cloudprefix, request,
    	    uuid = False, path = False, ext = False, extraPath = {}},
        {protocol, host, pathname, search} = ParseURL[uri];
        log["Parsed URL: `1`", {protocol, host, pathname, search}, DebugLevel -> 2];
        If[!protocol,
            cloud = currentCloud;
            If[Length[pathname] >= 1 && First[pathname] === "",
                pathname = Join[rootPath, Rest[pathname]],
                pathname = Join[currentPath, pathname]
            ]
        ];
        log["Parsing URI `1`", {protocol, host, pathname, search}, DebugLevel -> 2];
        Switch[protocol,
            "wolfram:",
                If[! host && Length[pathname] >= 1,
                	{uuid, ext} = ParseUUID[First[pathname]]; extraPath = Rest[pathname]
                ],
            "user:",
                If[! host && Length[pathname] >= 1,
                	user = First[pathname];  path = pathname
                ],
            "http:" | "https:" | False,
                If[protocol =!= False && host =!= False, 
                    cloud = protocol <> "//" <> host; PrependTo[pathname, ""]
                ];
                If[protocol === False && Length[pathname] >= 1,
                    If[pathname[[1]] =!= "",
                        (* relative (non-absolute) path *)
                        pathname = Join[rootPath, pathname]
                    ]
                ];
                log["Path `1` (object root: `2`, files root: `3`)", pathname, objectsRoot, filesRoot, DebugLevel -> 2];
                If[MatchQ[pathname, {___, objectsRoot | filesRoot, ___}],
                    {cloudprefix, request, pathname} = Replace[pathname,
                    	{Shortest[prefix___], type : objectsRoot | filesRoot, rest___} :> {{prefix}, type, {rest}}
                    ];
                    cloud = cloud <> StringJoin[Riffle[cloudprefix, "/"]];
	                If[request === filesRoot,
				        request = objectsRoot;
				        AppendTo[search, "view" -> "file"];
				    ];
                    If[UUIDQ[First@pathname],
                    (* URI is of the form .../objects/<uuid>..." *)
                        log["UUID-based URI: `1`", pathname, DebugLevel -> 2];
                        {uuid, ext} = ParseUUID[First@pathname];
                        extraPath = Rest[pathname],
                    (* URI is of the form .../objects/<username>..." *)
                        log["Username-based URI `1`", pathname, DebugLevel -> 2];
                        user = First[pathname];
                        If[user === "~", user = If[currentUser === None, "", userUUIDPrefix <> currentUser]];
                        log["User: `1`", user, DebugLevel -> 2];
                        path = Rest[pathname];
                    ]
                ]
        ];
        log["Parsed URI: `1`", {cloud, uuid, user, path, ext, extraPath, search}, DebugLevel -> 2];
        {cloud, uuid, user, path, ext, extraPath, search}
    ]
    
parseURI[uri_, base_] := Module[{currentPath},
    currentPath = "/" <> JoinURL[$CloudRootDirectory, $CloudDirectory];
    log["Current path: `1` (`2`, `3`)", currentPath, $CloudRootDirectory, $CloudDirectory, DebugLevel -> 2];
	parseURI[uri, base, $WolframUUID,
	            StringSplit[$CloudRootDirectory, "/", All], 
	            StringSplit[currentPath, "/", All], 
	            StringSplit[$CloudObjectsRoot, "/", All][[2]], 
	            StringSplit[$CloudFilesRoot, "/", All][[2]]
	        ]
]

parseURI[uri_] := parseURI[uri, $CloudBase]
    
Unprotect[CloudObject]

Options[CloudObject] = {CloudBase -> Automatic};

CloudBase::invalid = "Invalid CloudBase `1`; a fully qualified domain expected.";

getCloudBase[base_] := Module[{protocol, host, pathname, search},
    {protocol, host, pathname, search} = ParseURL[base];
    If[protocol === False || Length[pathname] > 0 || Length[search] > 0,
        Message[CloudBase::invalid, base]; Automatic,
        If[StringLength[base] >= 1 && StringTake[base, -1] =!= "/", base <> "/", base]
    ]
]

getCloudBase[Automatic] = Automatic;

CloudObject::uristring = "URI `1` expected to be a string.";

CloudObject::invaliduri = "The URI `1` is not valid.";

CloudObject::unauth = "URI `1` only valid when authenticated.";

CloudObject[OptionsPattern[]] := Module[{base = getCloudBase[OptionValue[CloudBase]]},
    If[base === Automatic, base = $CloudBase];
    CloudObject[JoinURL[base, $CloudObjectsRoot, CreateUUID[]]]
]

CloudObject[uri_String, OptionsPattern[]] := 
    Module[{base = getCloudBase[OptionValue[CloudBase]], cloud, uuid, user, path, ext, extraPath, search, newURI},
        CloudObject[newURI]
    /; (
        If[base === Automatic, base = $CloudBase];
        {cloud, uuid, user, path, ext, extraPath, search} = parseURI[uri, base];
        If[uuid === False,
            If[path === False,
                Message[CloudObject::invaliduri, uri]; False,
            (* else *)
                If[user === False || user === "",
                    Message[CloudObject::unauth, uri]; False,
                (* else *)
                    newURI = StringJoin[JoinURL[cloud, $CloudObjectsRoot, user, path], 
                        If[ext === False, "", "." <> ext]
                    ];
                    log["New named URI: `1`", newURI, DebugLevel -> 2];
                    newURI =!= uri
                ]
            ],
        (* else: explicit UUID set *)
            newURI = StringJoin[JoinURL[cloud, $CloudObjectsRoot, uuid, extraPath], 
                If[ext === False, "", "." <> ext]
            ];
            log["New UUID-based URI: `1`", newURI, DebugLevel -> 2];
            newURI =!= uri
        ]
    )]

CloudObject[co : CloudObject[uri_], OptionsPattern[]] := 
    Module[{base = OptionValue[CloudBase], protocol, host, pathname, search},
        If[base === Automatic,
            co,
        (* else *)
            {protocol, host, pathname, search} = ParseURL[uri];
            CloudObject[StringJoin[JoinURL[base, $CloudObjectsRoot, pathname], 
                JoinURLSearch[search]
            ]]
        ]
    ]
    
(* Only use hyperlinks inside CloudObject in desktop Mathematica. Otherwise, a "This feature is not supported" dialog is shown. *)
If[$CloudEvaluation =!= True,
    Format[CloudObject[uri_String], StandardForm] := Interpretation[CloudObject[Hyperlink[uri]], CloudObject[uri]]
]

End[]

EndPackage[]
