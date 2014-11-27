System`Private`NewContextPath[{"OAuthClient`","System`"}];
Begin["InternetArchive`"] (* Begin Private Context *) 
Begin["`Private`"](* Begin Private Context *) 
(******************************* InternetArchive *************************************)
internetarchivedata[]={
		"Gets"				-> {"Available","CDX","Import"},
		"RawGets"			-> {"RawAvailable", "RawCDX","RawImport"},
		"Posts"				-> {},
		"RawPosts"			-> {},
		"ServiceName"		-> "InternetArchive",
 		"Information"		-> "Wolfram Language connection to the internet archive machine API"
}
(****** Auxiliary Functions ******)
internetarchivegettimestamp[$Failed]:=Throw[$Failed]
internetarchivegettimestamp[date_String]:=Block[{$DateStringFormat = {"Year", "Month", "Day"}}, 
			DateString@DateList@Interpreter["Date"][date]
];
(****** Raw Properties ******)
internetarchiverawdata["RawAvailable",args_]:=Block[{url,timestamp, res},
	url=ToString[Lookup[args,"url",Lookup[args,"URL",0]]];
	timestamp=internetarchivegettimestamp[ToString[Lookup[args,"timestamp",Lookup[args,"TIMESTAMP",0]]]];
	res=URLFetch["http://archive.org/wayback/available","Parameters"->{"url"->url,"timestamp"->timestamp},"Method" -> "GET"];
	ImportString[res,"JSON"]
]

internetarchiverawdata["RawImport",args_]:=Block[{call,url, elements},
	call=internetarchiverawdata["RawAvailable", args];
	elements=ToString[Lookup[args,"elements",Lookup[args,"Elements",0]]]/.{"All"->"Elements"};
	If[KeyExistsQ[call,"archived_snapshots"],
		(
			url="url"/.("closest"/.Lookup[call,"archived_snapshots"]);
			Import[url,elements]	
		),
		$Failed
	]
]

internetarchiverawdata["RawCDX",args_]:=Block[{url, res},
	url=ToString[Lookup[args,"url",Lookup[args,"URL",0]]];
	res=URLFetch["http://web.archive.org/cdx/search/cdx","Parameters"->{"url"->url},"Method" -> "GET"];
	res
]

internetarchiverawdata[___]:=$Failed

(****** Cooked Properties ******)
internetarchivecookeddata["Available",args_]:=With[{res=internetarchiverawdata["RawAvailable", args]},
	If[KeyExistsQ[res,"archived_snapshots"],
		"url"/.("closest"/.Lookup[res,"archived_snapshots"]),
		$Failed
	]
]
internetarchivecookeddata["Import",args_]:=With[{res=internetarchiverawdata["RawImport", args]},
	res	
]
internetarchivecookeddata["CDX",args_]:=With[{res=internetarchiverawdata["RawCDX",args]},
	res
]
internetarchivecookeddata[___]:=$Failed

(****** Send Message ******)
internetarchivesendmessage[args_]:=internetarchivecookeddata["PostData",args]
End[] (* End Private Context *)       		
End[]
System`Private`RestoreContextPath[];
{InternetArchive`Private`internetarchivedata,InternetArchive`Private`internetarchivecookeddata,InternetArchive`Private`internetarchivesendmessage,InternetArchive`Private`internetarchiverawdata}
