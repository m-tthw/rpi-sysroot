(* ::Package:: *)

(* $Id: URL.m,v 1.4 2014/01/28 20:04:19 bobs Exp $ *)

(* :Summary:
    URLEncode and Decode
*)

(* :Mathematica Version: Mathematica 10.0 *)

(* :Keywords: 
OAuth
*)

(* :Examples:
*)
(* Documentation http://tools.ietf.org/html/rfc3986 *)

Package["OAuth`URL`"]

System`URLBuild;
System`URLParse;
System`URLEncode;
System`URLDecode;
System`URLQueryDecode;
System`URLQueryEncode;
System`URLShorten;
System`URLExpand;
System`URLExistsQ;

(Unprotect[#]; Clear[#];) & /@ {
    URLBuild, 
    URLParse, 
    URLEncode, 
    URLDecode, 
    URLQueryDecode, 
    URLQueryEncode, 
    URLShorten, 
    URLExpand,
	URLExistsQ
};

(* debug and utils *)

ClearAll[associationJoin, Destroy]

(* Internal utilities *)
associationJoin[ass___Association] := Association[Flatten[Normal /@ {ass}]];

SetAttributes[Destroy, SequenceHold]
Destroy := Sequence[]

(* Globals *)

(*
$URIGenDelims  = Alternatives[":", "/", "?", "#", "[", "]", "@"];
$URISubDelims  = Alternatives["!", "$", "&", "'", "(", ")", "*", "+", ",", ";", "="];
$URIReserved   = $URIGenDelims ~ Join ~ $URISubDelims;
$URIUnreserved = Alternatives[WordCharacter, "-", ".", "_", "~"];
*)
$URLDefaultEncoding = "UTF-8";

(* Usage *)
URLEncode::usage        = "Use this to escape a parameter to be used inside a querystring";
URLDecode::usage        = "Use this to unescape a querystring paramenter";
URLBuild::usage         = "Create a URL string from its component parts";
URLParse::usage         = "Parse and split an URL inside it's components: {scheme, domain, port, path, querystring, fragment}";
URLQueryEncode::usage   = "Encode a query string from an Association or a List or Rule";
URLQueryDecode::usage   = "Decode a query string to an Association";
URLExistsQ::usage       = "Check if an URL is reachable from your machine.";

(* Messages *)
URLEncode::nvldarg      = "The argument `` should be a string or a number.";
URLEncode::argr         = "\[NoBreak]``\[NoBreak] called with `` argument; \[NoBreak]``\[NoBreak] argument(s) expected.";
URLEncode::encoding     = "Cannot encode the string with current encoding";

URLDecode::nvldarg      = "The argument `` should be a string.";
URLDecode::argr         = URLEncode::argr;

URLExistsQ::nvldarg     = URLDecode::nvldarg;
URLExistsQ::argr        = URLDecode::argr;

URLParse::nvldlookup    = "Invalid value `` given, it should be a one of the following values: ``.";
URLParse::nvldarg       = URLDecode::nvldarg;
URLParse::argr          = URLEncode::argr;
URLParse::nolookup      = "The second argument `` should be a string or a list of strings.";

URLBuild::nvldarg       = "The argument `` should be a list of rules, an association or a string.";
URLBuild::argr          = URLEncode::argr;

URLQueryEncode::nvldarg = "The argument `` should be a list of rules or an association.";
URLQueryEncode::nvldrule = "Rule key `` is not a string.";
URLQueryEncode::argr    = URLEncode::argr;

URLQueryDecode::nvldarg = URLDecode::nvldarg;
URLQueryDecode::argr    = URLEncode::argr;

(* Options declaration *)
Options[URLEncode]      = {CharacterEncoding -> "UTF-8"};
Options[URLDecode]      = Options[URLEncode];
Options[URLQueryEncode] = Options[URLEncode] ~ Join ~ {"SortQueries" -> False, "RemoveEmptyQueries" -> False};
Options[URLQueryDecode] = Options[URLQueryEncode];
Options[URLBuild]       = Options[URLQueryEncode];
Options[URLParse]       = Options[URLQueryEncode];
(* URLEXistsQ *)
URLExistsQ[url_String] := If[URLFetch[url, "Method" -> "HEAD"] === $Failed, False, True];
URLExistsQ[_String, args___] := (Message[URLExistsQ::argr, URLExistsQ, Length[{args}] + 1, 1];$Failed)
URLExistsQ[x_:"", ___] := (Message[URLExistsQ::nvldarg, x];$Failed)

toIntegerString[None] := (Message[URLEncode::encoding];Throw @ $Failed);
toIntegerString[val_] :=  ToUpperCase @ IntegerString[val, 16];

(* URLEncode *)
URLEncode[s_String, OptionsPattern[]] := Catch[
	StringReplace[s, {
	    " " -> "+", 
	    RegularExpression["[^a-zA-Z0-9\\ _ \\.\\-]"] :> 
	        StringJoin @ (("%" <> toIntegerString[#] &) /@ 
	            ToCharacterCode["$0", OptionValue[CharacterEncoding]])
	}]
]

URLEncode[s_List, options___Rule] := URLEncode[#, options] & /@ s;
URLEncode[True, options___Rule] := "true"; 
URLEncode[False, options___Rule] := "false"; 
URLEncode[None|Null|Missing|_Missing, options___Rule] := ""; 
URLEncode[n_Integer, options___Rule] := ToString[n]; 
URLEncode[n_, options___Rule] /; NumberQ[n] := URLEncode[ToString[N[n]], options];
URLEncode[_String, args___] := (Message[URLEncode::argr, URLEncode, Length[{args}] + 1, 1];$Failed)
URLEncode[x_:"", ___] := (Message[URLEncode::nvldarg, x];$Failed)

(* URLDecode *)

URLDecode["", options___Rule] := "";
URLDecode[s_String, OptionsPattern[]] := FromCharacterCode[List @@ StringReplace[s, {
        RegularExpression["%([0-9a-fA-F]{2})"] :> FromDigits["$1", 16], 
        "+" :> First@ToCharacterCode[" "], 
        char_ :> First@ToCharacterCode[char]
    }], OptionValue[CharacterEncoding]]

URLDecode[s_List, options___Rule] := URLDecode[#, options] & /@ s;
URLDecode[_String, args___] := (Message[URLDecode::argr, URLDecode, Length[{args}] + 1, 1];$Failed)
URLDecode[x_:"", ___] := (Message[URLDecode::nvldarg, x];$Failed)
(* URLQueryDecode *)
URLQueryDecode[s_List, options___Rule] := URLQueryDecode[#, options] & /@ s;
URLQueryDecode[query_String, OptionsPattern[]] := Module[
	{parsed = Rule @@ PadRight[Map[Function[s, URLDecode[s, CharacterEncoding -> OptionValue[CharacterEncoding]]], StringSplit[#, "=", 2]], 2, ""] & /@ StringSplit[query, "&"]},
	If[OptionValue["SortQueries"], parsed = SortBy[parsed, First]];
	If[OptionValue["RemoveEmptyQueries"], parsed = DeleteCases[parsed, Verbatim[Rule][_, ""]]];
	parsed
];

URLQueryDecode[_String, args___] := (Message[URLQueryDecode::argr, URLQueryDecode, Length[{args}] + 1, 1];$Failed)
URLQueryDecode[x_:"", ___] := (Message[URLQueryDecode::nvldarg, x];$Failed)

(* URLQueryEncode *)
URLQueryEncode[args_Rule, options___Rule] := URLQueryEncode[List[args], options];
URLQueryEncode[args_Association, options___Rule] := URLQueryEncode[Normal[args], options];
URLQueryEncode[args_List, OptionsPattern[]] := Module[
	{parsed = Replace[#, {
		Verbatim[Rule][a_String, b_List] :> (Message[URLEncode::nvldarg, b];$Failed),
		Verbatim[Rule][a_String, b_] :> With[
			{s = URLEncode[b, CharacterEncoding -> OptionValue[CharacterEncoding]]}, 
			If[
				StringQ[s], 
				If[
					StringLength[s] > 0, 
					StringJoin[{URLEncode[a, CharacterEncoding -> OptionValue[CharacterEncoding]], "=", s}], 
					If[
						OptionValue["RemoveEmptyQueries"],
						Destroy,
						URLEncode[a, CharacterEncoding -> OptionValue[CharacterEncoding]]
					]
				], 
				$Failed
			] 
		],
		Verbatim[Rule][a_, _] :> (Message[URLQueryEncode::nvldrule, a]; $Failed), 
		s_ :> If[
			OptionValue["RemoveEmptyQueries"], 
			Destroy, 
			URLEncode[s, CharacterEncoding -> OptionValue[CharacterEncoding]]
		]
	}] & /@ Flatten[args]},
	If[OptionValue["SortQueries"], parsed = Sort[parsed]];
	If[FreeQ[parsed, $Failed], StringJoin[Riffle[parsed, "&"]], $Failed]
]
URLQueryEncode[_Association|_List, args___] := (Message[URLQueryEncode::argr, URLQueryEncode, Length[{args}] + 1, 1];$Failed)
URLQueryEncode[x_:"", ___] := (Message[URLQueryEncode::nvldarg, x];$Failed)

(* URLParse and URLBuild utils *)

ClearAll[noChars, oneOrNull, authorityParse, validateLookup, parsedURI, isParamNull, toURLString, buildURLParts, joinQueries, pathSplit]

noChars[chars_] := Except[Characters[chars]];
oneOrNull[pattern_] := Repeated[pattern, {0, 1}];
authorityParse[autority_String] := First[StringCases[
    autority, 
    StartOfString ~~ 
    oneOrNull[userinfo: noChars["@"] ... ~~ "@"] ~~ 
    domain: Alternatives[noChars[":[]"]..., "[" ~~ ___ ~~ "]"]~~ 
    oneOrNull[":" ~~ port: ___] ~~
    EndOfString :> {userinfo, ToLowerCase[domain], port}
]]

$URLKeysDefault = {"Scheme", "User", "Domain", "Port", "Path", "Query", "Fragment"};
$URLKeysValid = $URLKeysDefault ~ Join ~ {"PathString", "QueryString", "Username", "Password"};
stringOrDefault[key_String, default_:None] := If[StringLength[key] > 0, key, default];

pathSplit[path_] := Module[
	{splitted = StringSplit[path,"/"]},
	If[
		StringMatchQ[path, StartOfString~~"/" ~~ ___],
		splitted = {""} ~ Join ~ splitted
	];
	If[
		StringMatchQ[path, ___ ~~"/" ~~ EndOfString],
		splitted = splitted ~ Join ~ {""} 
	];
	splitted
];

 
parsedURI[{scheme_, ___}, ___]["Scheme"] := stringOrDefault[ToLowerCase[scheme]];
parsedURI[{_, user_, ___}, ___]["User"] := stringOrDefault[user];
parsedURI[{_, user_, ___}, ___]["Username"] := stringOrDefault[First @ StringSplit[user, ":", 2]];
parsedURI[{_, user_, ___}, ___]["Password"] := stringOrDefault[Last @ PadRight[StringSplit[user, ":", 2], 2, ""]];
parsedURI[{_, _, domain_, ___}, ___]["Domain"] := stringOrDefault[ToLowerCase[domain]];
parsedURI[{_, _, _, port_, ___}, ___]["Port"] := If[
	StringMatchQ[port, DigitCharacter..],
	FromDigits[port],
	stringOrDefault[port]
];
parsedURI[{_, _, _, _, path_, ___}, ___]["Path"] := pathSplit[path];
parsedURI[{_, _, _, _, path_, ___}, ___]["PathString"] := stringOrDefault[path];
parsedURI[{_, _, _, _, _, query_, ___}, options___Rule]["Query"] := URLQueryDecode[query, options];
parsedURI[{_, _, _, _, _, query_, ___}, ___]["QueryString"] := stringOrDefault[query];
parsedURI[{_, _, _, _, _, _, fragment_, ___}, ___]["Fragment"] := stringOrDefault[fragment];
parsedURI[uri_String, options___Rule] := Module[{parsed = StringCases[uri, {   

    (* simple case, just scheme and path *)
    StartOfString ~~ 
    scheme:noChars[":/"].. ~~ ":" ~~ 
    path:Alternatives[
        "/" ~~ Except["/"] ~~ ___, 
        Repeated[Except["/"], {2}] ~~ ___, 
        RepeatedNull[Except["/"]]
        ] ~~
    EndOfString :> {scheme, "", "", "", path, "", ""},

    (* hierarchy case, scheme, path query and fragment *)
    StartOfString ~~ 
    oneOrNull[scheme: noChars[":/?#"].. ~~ ":"] ~~
    oneOrNull["//" ~~ autority: noChars["/?#"]...] ~~
    oneOrNull[path: noChars["?#"]...]   ~~
    oneOrNull["?"  ~~ query: noChars["#"]...]   ~~
    oneOrNull["#"  ~~ fragment: ___] ~~
    EndOfString :> {scheme, Sequence @@ authorityParse[autority], path, query, fragment}

    }, 1]},
    If[
        MatchQ[parsed, {}],
        $Failed,
        parsedURI[First[parsed], options]
    ]
]
validateLookup[lookup_, params_:$URLKeysValid] := If[
	MemberQ[params, lookup],
	True,
	Message[URLParse::nvldlookup, lookup, StringJoin[Riffle[Sort[params], ", "]]];
	False
];

(* URLParse *)

URLParse[uri_List, args___] := URLParse[#, args] & /@ uri;
URLParse[uri_String, {lookup__}, options___Rule] := If[
	And @@ Map[validateLookup, {lookup}],
	With[
		{parsed = parsedURI[uri, options]},
		If[parsed === $Failed, $Failed, Map[parsed, {lookup}]]
	],
	$Failed
];
URLParse[uri_String, lookup_String, options___Rule]  := If[
	validateLookup[lookup],
	With[
		{parsed = parsedURI[uri, options]},
		If[parsed === $Failed, $Failed, parsed[lookup]]
	],
	$Failed
];
URLParse[uri_String, options___Rule] := With[
	{parsed = URLParse[uri, $URLKeysDefault, options]},
	If[
		parsed === $Failed, 
		$Failed, 
		Association[Rule @@@ Transpose[{$URLKeysDefault, parsed}]]
	]	
]
URLParse[_String, lookup_] := (Message[URLParse::nolookup, lookup];$Failed)
URLParse[_String, _, args___] := (Message[URLParse::argr, URLParse, Length[{args}] + 2, 2];$Failed)
URLParse[x_:"", ___] := (Message[URLParse::nvldarg, x];$Failed)

(* URLBuild *)

isParamNull[key_String, params_Association] := params[key] === "";
buildURLParts[input_Association, options___] := Association[# -> toURLString[#, Lookup[input, #, None], options] & /@ $URLKeysDefault];

toURLString["Query",  param_Association, options___]  := URLQueryEncode[param, options];
toURLString["Query",  param_List, options___]         := URLQueryEncode[param, options];
toURLString["Domain", param_String, options___]       := ToLowerCase[param];
toURLString["Scheme", Automatic, options___]          := "";
toURLString["Scheme", param_String, options___]       := ToLowerCase[param];
toURLString["Path",   param_List, options___]         := StringJoin @ Riffle[ToString /@ param, "/"];
toURLString[_,        None|Null|Missing, options___]  := "";
toURLString[_,        param_, options___]             := ToString[param];

joinQueries[queries_, options___] := Flatten[Map[Replace[#, {
	None|Null|Missing|_Missing -> {},
	s_String :> URLQueryDecode[s, options],
	a_Association :> Normal[a]
}] &, queries]];
URLBuild[info_Association, options___Rule] := Module[
	{url, parsed, parts, hasAuthority}, 

	url    = "";
	parts  = buildURLParts[info, options];
	hasAuthority = Or @@ Map[! isParamNull[#, parts] &, {"Port", "Domain", "User"}];

	If[! FreeQ[List @@ parts, $Failed], Return[$Failed]];

	(* If scheme is None, Automatic or "" just don't add it *)

	If[
		! isParamNull["Scheme", parts],
		url = url <> parts["Scheme"] <> ":"
	];

	(* If there is at least one hirarc component must add // *)
	If[
		hasAuthority,
		url = url <> "//";
		If[
			! isParamNull["User", parts], 
			url = url <> parts["User"] <> "@";
		];
		url = url <> parts["Domain"];
		If[
			! isParamNull["Port", parts], 
			url = url <> ":" <> parts["Port"]
		];
		url = url <> "/";
	];

	(* add a path *)
	If[
		! isParamNull["Path", parts], 		
		If[
			isParamNull["Domain", parts],
			url = url <> parts["Path"],
			If[
				StringTake[parts["Path"], 1] === "/", 
				url = url <> StringTake[parts["Path"], {2, -1}],
				url = url <> parts["Path"]
			]
		]
	];

	If[
		! isParamNull["Query", parts],
		url = url <> "?" <> parts["Query"]
	];

	If[
		! isParamNull["Fragment", parts],
		url = url <> "#" <> parts["Fragment"]
	];

	url
];

URLBuild[info_Association, query:(_String|_List|_Association), options___Rule] := URLBuild[
	associationJoin[
		info, 
		Association[{"Query" -> joinQueries[{Lookup[info, "Query", ""], query}, options]}]
	],
	options
]
URLBuild[{args___Rule}, options___]   := URLBuild[Association[{args}], options];
URLBuild[{paths__String}, options___] := URLBuild[toURLString["Path", {paths}], options];

URLBuild[url_String, options___Rule]  := URLBuild[URLParse[url, options], options]
URLBuild[url_String, query:(_String|_List|_Association), options___Rule] := URLBuild[URLParse[url, options], query, options]

URLBuild[_, query_] := (Message[URLBuild::nvldarg, query];$Failed)
URLBuild[_, _, args___] := (Message[URLBuild::argr, URLBuild, Length[{args}] + 2, 2];$Failed)
URLBuild[x_:"", ___] := (Message[URLBuild::nvldarg, x];$Failed)

(* starting URLShorten and URLExpand *)

ClearAll[
 	jsonSelect,
 	bitlyShorten,

 	$BitlyApiUrl,
 	$BitlyAppToken,
 	$GoogleApiUrl,
 	$WolframUrlShorten,
 	$BitlyUrlShorten,
 	$GoogleUrlShorten
];

jsonSelect[expr_][lookup_String] := With[{results = Cases[
	expr,  
	Verbatim[Rule][lookup, a_] :> a,
	All
]}, If[Length[results] > 0, First @ results, $Missing]];

jsonSelect[string_String] := jsonSelect[ImportString[string, "JSON"]];


$BitlyAppToken = "29cac16e5c5562368d135a325ac592a99c4f1253";
$BitlyApiUrl = "https://api-ssl.bitly.com/v3";
$GoogleApiUrl = "https://www.googleapis.com/urlshortener/v1/url";
$DefaultUrlShorten = "wolfr.am";
$BitlyServices = {
   {"wolfr.am", "Wolfram"},
   {"bit.ly", "Bitly"},
   {"j.mp", "Jmp"},
   {"bitly.com"}	
};
$GoogleService = {"goo.gl", "Googl"};
$ShortenServices = $BitlyServices ~ Join ~ {$GoogleService};

bitlyShorten[url_, domain_: "bit.ly"] := With[{
    data = jsonSelect[
		URLFetch[URLBuild[{$BitlyApiUrl, "shorten"}, {"access_token" -> $BitlyAppToken, "longUrl" -> url}]]
    ]},
	If[
    	data["status_code"] === 200,
    	"http://" <> With[{lookup = Cases[$BitlyServices,{real__, domain}:>real]}, If[Length[lookup]>0, First[lookup], domain]] <> "/" <> data["hash"],
    	Message[URLShorten::err, data["status_txt"]];
    	$Failed
    ]
];
(* message and usage *)

serviceString := StringJoin[Riffle[Sort[Flatten[$ShortenServices]], ", "]];

URLShorten::usage = StringJoin[
   "Use URLShorten to shorten a long url. Usage: URLShorten[url, service:\" wolfr.am\"]. Avilable services are: ",
   serviceString, 
   "."
];
URLExpand::usage = StringJoin[
	"Use URLExpand to shorten a long url. Usage: URLShorten[url]. Avilable services are: ",
	serviceString, 
	"."
];

URLShorten::err = "Server returned: `1` ";
URLShorten::nvldarg = URLDecode::nvldarg;
URLShorten::nvldservice = "`` it's not a valid service. Services are: ``";
URLShorten::argr = URLEncode::argr;

URLExpand::nvldarg = URLDecode::nvldarg;
URLExpand::err = URLShorten::err;
URLExpand::nvldurl = "`1` is not a valid shortened url ";
URLExpand::nvldservice = URLShorten::nvldservice;
URLExpand::argr = URLEncode::argr;




URLShorten[urllist_List] := Map[URLShorten, urllist];
URLShorten[urllist_List, service_String] := Map[URLShorten[#, service] &, urllist];

URLShorten[url_String] := URLShorten[url, $DefaultUrlShorten]
URLShorten[url_String, service_String] /; MemberQ[Flatten[$BitlyServices], service] := bitlyShorten[url, service]
URLShorten[url_String, Alternatives @@ $GoogleService] := With[{
    data = jsonSelect[
        URLFetch[
       		$GoogleApiUrl,
       		"Method" -> "POST",
       		"BodyData" -> ExportString[{"longUrl" -> url}, "JSON"],
       		"Headers" -> {"Content-Type" -> "application/json"}
		]
	]},
	If[
		StringQ[data["id"]],
		data["id"],
		Message[URLShorten::err, data["message"]];
		$Failed
	]
];
URLShorten[_String, service_] := (Message[URLShorten::nvldservice, service, serviceString]; $Failed)
URLShorten[_String, _, args___] := (Message[URLShorten::argr, URLShorten, Length[{args}] + 2, 2];$Failed)
URLShorten[x_:"", ___] := (Message[URLShorten::nvldarg, x];$Failed)

URLExpand[urllist_List] := Map[URLExpand, urllist];
URLExpand[urllist_List, service_String] := Map[URLExpand[#, service] &, urllist];
URLExpand[url_String] := With[
	{cases = StringCases[url, "http://" ~~ Shortest[a__] ~~ "/" :> a, 1]},
  	If[
		And[Length[cases] === 1, MemberQ[First /@ $ShortenServices, First[cases]]],
   		URLExpand[url, First[cases]],
   		Message[URLExpand::nvldurl, url];
   		$Failed
	]
]
URLExpand[url_String, service_] /; MemberQ[Flatten[$BitlyServices], service] := With[{
    data = jsonSelect[
		  URLFetch[URLBuild[{$BitlyApiUrl, "expand"}, {"access_token" -> $BitlyAppToken, "shortUrl" -> url}]]
     ]},
    If[
    	data["status_code"] === 200,
    	data["long_url"],
    	Message[URLExpand::err, data["status_txt"]];
    	$Failed
    ]
];
URLExpand[url_String, Alternatives @@ $GoogleService] := With[{
    data = jsonSelect[
		URLFetch[URLBuild[$GoogleApiUrl, {"shortUrl" -> url}]]
    ]},
    If[
    	StringQ[data["longUrl"]],
    	data["longUrl"],
    	Message[URLExpand::err, data["message"]];
    	$Failed
    ]
];
URLExpand[_String, service_] := (Message[URLExpand::nvldservice, service, serviceString]; $Failed)
URLExpand[_String, _, args___] := (Message[URLExpand::argr, URLExpand, Length[{args}] + 2, 2];$Failed)
URLExpand[x_:"", ___] := (Message[URLExpand::nvldarg, x];$Failed)

SetAttributes[
	{URLBuild, URLParse, URLEncode, URLDecode, URLQueryDecode, URLQueryEncode, URLShorten, URLExpand, URLExistsQ},
    {ReadProtected, Protected}
];

PackageExport[URLBuild]
PackageExport[URLParse]
PackageExport[URLEncode]
PackageExport[URLDecode]
PackageExport[URLQueryDecode]
PackageExport[URLQueryEncode]
PackageExport[URLShorten]
PackageExport[URLExpand]
PackageExport[URLExistsQ]





