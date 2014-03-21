(* ::Package:: *)

<< "URL.m"

BeginPackage["CloudObject`"]

PackageExport[URLShorten];
PackageExport[URLExpand];

ClearAll[
 	JSONSelect,
 	BitlyShorten,
 	URLShorten,
 	URLExpand,
 	$BitlyApiUrl,
 	$BitlyAppToken,
 	$GoogleApiUrl,
 	$WolframUrlShorten,
 	$BitlyUrlShorten,
 	$GoogleUrlShorten
];

JSONSelect[expr_][lookup_String] := With[{results = Cases[
	expr,  
	Verbatim[Rule][lookup, a_] :> a,
	All
]}, If[Length[results] > 0, First @ results, $Missing]];

JSONSelect[string_String] := JSONSelect[ImportString[string, "JSON"]];

$BitlyAppToken = "29cac16e5c5562368d135a325ac592a99c4f1253";
$BitlyApiUrl = "https://api-ssl.bitly.com/v3/";
$GoogleApiUrl = "https://www.googleapis.com/urlshortener/v1/url";
$DefaultUrlShorten = "wolfr.am";
$BitlyServices = {
   {"wolfr.am", "Wolfram"},
   {"bit.ly", "Bitly"},
   {"j.mp"},
   {"bitly.com"}	
};

$GoogleService = {"goo.gl", "Googl"};

$ShortenServices = $BitlyServices ~ Join ~ {$GoogleService};

BitlyShorten[url_, domain_: "bit.ly"] := With[{
    data = JSONSelect[
		URLFetch[
       		$BitlyApiUrl <> "shorten" <> JoinURLSearch[{
          		"access_token" -> $BitlyAppToken, 
          		"longUrl" -> EscapeURL[url]
          	}]
       	]]
    },
	If[
    	data["status_code"] === 200,
    	"http://" <> With[{lookup = Cases[$BitlyServices,{real__, domain}:>real]}, If[Length[lookup]>0, First[lookup], domain]] <> "/" <> data["hash"],
    	Message[URLShorten::err, data["status_txt"]];
    	$Failed
    ]
];

URLShorten::usage = StringJoin[
   "Use URLShorten to shorten a long url. Usage: URLShorten[url, service:\"wolfr.am\"]. Avilable services are: ",
   Riffle[$ShortenServices, ", "], 
   "."
];
URLShorten::err = "Server returned: `1`";
URLShorten[url_] := URLShorten[url, $DefaultUrlShorten]
URLShorten[url_, service_String] /; MemberQ[Flatten[$BitlyServices], service] := BitlyShorten[url, service]
URLShorten[url_, Alternatives @@ $GoogleService] := With[{
    data = JSONSelect[
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
URLExpand::usage = StringJoin[
	"Use URLExpand to shorten a long url. Usage: URLShorten[url]. Avilable services are: ",
	Riffle[$ShortenServices, ", "], 
	"."
];
URLExpand::err = "Server returned: `1`";
URLExpand::invalidurl = "`1` is not a valid shortened url";
URLExpand[url_] := With[
	{cases = StringCases[url, "http://" ~~ Shortest[a__] ~~ "/" :> a, 1]},
  	If[
		And[Length[cases] === 1, MemberQ[First /@ $ShortenServices, First[cases]]],
   		URLExpand[url, First[cases]],
   		Message[URLExpand::invalidurl, url];
   		$Failed
	]
]
URLExpand[url_, service_] /; MemberQ[Flatten[$BitlyServices], service] := With[{
    data = JSONSelect[
      	URLFetch[
       		$BitlyApiUrl <> "expand" <> JoinURLSearch[{
          		"access_token" -> $BitlyAppToken, 
          		"shortUrl" -> EscapeURL[url]
          	}]] 
      	]
    },
    If[
    	data["status_code"] === 200,
    	data["long_url"],
    	Message[URLExpand::err, data["status_txt"]];
    	$Failed
    ]
];

URLExpand[url_, Alternatives @@ $GoogleService] := With[{
    data = JSONSelect[URLFetch[$GoogleApiUrl <> JoinURLSearch[{"shortUrl" -> EscapeURL[url]}]]]},
    If[
    	StringQ[data["longUrl"]],
    	data["longUrl"],
    	Message[URLExpand::err, data["message"]];
    	$Failed
    ]
];

EndPackage[];

URLExpand[URLShorten["http://miao.com", "goo.gl"]]



