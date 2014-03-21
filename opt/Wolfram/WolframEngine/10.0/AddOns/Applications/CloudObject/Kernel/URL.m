(* Mathematica package *)
BeginPackage["CloudObject`"]

EscapeURL;
UnescapeURL;
ParseURL;
JoinURL;
JoinURLSearch;

Begin["`Private`"]

(*URL escaping*)

EscapeURL[s_String] :=
    StringReplace[s, {
	    " " -> "+", 
	    RegularExpression["[^a-zA-Z0-9\\_\\.\\-]"] :> 
	        StringJoin @ ("%" <> ToUpperCase @ IntegerString[#, 16] & /@ 
	            ToCharacterCode["$0", "UTF-8"])
	}]

UnescapeURL[s_String] := 
    FromCharacterCode[List @@ StringReplace[s, {
        RegularExpression["%([0-9a-fA-F]{2})"] :> FromDigits["$1", 16], 
        "+" :> First@ToCharacterCode[" "], 
        char_ :> First@ToCharacterCode[char]
    }], "UTF-8"]

(*URL parsing*)

ParseURL[url_] := Module[{protocol, host, pathname, search, parts},
    search = StringSplit[url, "?", 2];
    parts = First[search];
    search = Rest[search];
    search = If[Length[search] === 0, "", First[search]];
    parts = StringSplit[parts, "/", All];
    protocol = StringSplit[First[parts], ":", 2];
    If[Length[protocol] === 2,
        parts[[1]] = Last[protocol]; protocol = First[protocol] <> ":",
    (* else *)
        protocol = False
    ];
    If[Length[parts] > 2 && parts[[1]] === parts[[2]] === "",
        host = parts[[3]]; parts = Drop[parts, 3],
    (* else *)
        host = False
    ];
    pathname = parts;
    search = (If[Length[#] === 2,
        UnescapeURL /@ Rule @@ #, 
        UnescapeURL @ First[#]
    ] & @ StringSplit[#, "=", 2] &) /@ StringSplit[search, "&"];
    {protocol, host, pathname, search}
]

JoinURL[items__] := 
    StringJoin[Riffle[StringTrim[#, "/"] & /@ DeleteCases[Flatten[{items}], ""], "/"]]

JoinURLSearch[values_] :=
    If[Length[values] > 0,
        "?" <> Riffle[values /. (key_ -> value_) :> StringJoin[key, "=", value], "&"],
    (* else *)
        ""
    ]

End[]

EndPackage[]
