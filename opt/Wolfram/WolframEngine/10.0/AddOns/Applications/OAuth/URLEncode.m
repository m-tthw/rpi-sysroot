(* ::Package:: *)

(* $Id: URLEncode.m,v 1.16 2013/11/14 20:29:51 riccardod Exp $ *)

(* :Summary:
    URLEncode and Decode
*)

(* :Mathematica Version: Mathematica 10.0 *)

(* :Keywords: 
OAuth
*)

(* :Examples:
*)

System`Private`NewContextPath[{"System`","OAuthClientDump`"}];

Needs["HTTPClient`"];

System`URLEncode;/Users/riccardo/WolframWorkspaces/Base/OAuth/URLEncode.m
System`URLDecode;
System`URLParse;
System`URLBuild;

Unprotect /@ {URLBuild, URLParse,URLEncode, URLDecode};
ClearAll[URLBuild, URLParse,URLEncode, URLDecode];

Begin["URLEncodeDump`"];

(***************** Usage ***********************)
URLEncode::usage = "Use this to escape a parameter to be used inside a querystring";
URLDecode::usage = "Use this to unescape a querystring paramenter";
URLBuild::usage  = "Create a URL string from its component parts";
URLParse::usage  = "Parse and split an URL inside it's components: {scheme, domain, port, path, querystring, fragment}";
(***************** Messages ***********************)
URLEncode::nostr = "The value `1` given to URLEncode should be a string.";
URLParse::nostr  = "The value `1` given to URLParse should be a string.";
URLDecode::nostr = "The value `1` given to URLDecode should be a string.";
(***************** URLEncode **********************)

Options[URLBuild]=Options[urlbuild]={"Port"->None,"Fragment"->None,"User"->None,"Password"->None, "Scheme"->None,
    "EncodedComponents"->"ParameterValues","SortQuery"->False}

URLBuild[args___]:=With[{res=Catch[urlbuild[args]]},
    res/;res=!=$Failed]

urlbuild[base_,param_Rule, rest___]:=If[MemberQ[First/@Options[URLBuild],First[param]],
    urlbuild[base,{},param, rest],
    urlbuild[base,{param}, rest]
]

(*
$SortURLParameters=True;
$EncodedURLComponents=All;
*)

urlassoc=If[$VersionNumber>=10,
    Association[Replace[#,{Rule[_,Null|{}|""]:>Sequence@@{}},{1}]]&,
    Identity]

clearmissing={$Missing->{},Missing[__]->{},None->{},Null->{}};

urlbuild[as_,rest___]:=With[{
    path=Flatten[{as["Domain"],as["Path"]}/.clearmissing],
    params=as["Query"]/.clearmissing,
    opts=DeleteCases[Normal[as],Rule["Domain"|"Path"|"Query",_]]
    },
    debugPrint["from assoc."->Hold[urlbuild][path, params, Sequence@@opts, rest]];
    urlbuild[If[StringQ[path],pathSplit[path],path], If[StringQ[params],fromurlparameters[params],params], Sequence@@opts, rest]
]/;MatchQ[as,_Association]

(*
urlbuild[base_String]:=urlpercentencode[base]
*)

urlbuild[base_]:=urlbuild[base,{}]

urlbuild[base_,rest___]:=urlbuild[{base},rest]/;!ListQ[base]
        
urlbuild[base_List,params_List, opts:OptionsPattern[]]:=Module[
    {sparams, url, port, fragment, username, password, scheme, sort=Sequence@@{}, path,
    urlpercentencode1,encodedcomponents=Flatten[{OptionValue["EncodedComponents"]}]},
    debugPrint["urlbuild 0"];
    
    If[(!TrueQ[OptionValue["SortQuery"]])&&($VersionNumber>=10),sort=Identity];
    (*
    If[!MatchQ[base,{_String...}],Message[URLBuild::invb,base];Throw[$Failed]];
    If[!MatchQ[params,{_Rule...}],Message[URLBuild::invp,params];Throw[$Failed]];
    *)
    
    urlpercentencode1[_][x_,___]:=ToString[x];
    debugPrint[encodedcomponents->encodedcomponents];
    (urlpercentencode1[#]=(urlencode[##]&))&/@(encodedcomponents/.{All}->{
        "Scheme","User","Password","Domain","Path","Port","Parameters","ParameterValues","Fragment"});
    
    debugPrint[Definition[urlpercentencode1]];
    Switch[Length[base],
    	0,
        url="";
        path={},
        1,
        url=ToString[First[base]];
        path={},
        _,
        url=pathstripEnd[ToString[First[base]]];
        path=Rest[base];
        path=Join[pathstrip/@Most[path],{pathstripStart[Last[path]]}]
    ];
    
    scheme=OptionValue["Scheme"];
    
    (* Encode the base *)
    Switch[scheme,
        (*Automatic,
        If[StringFreeQ[url,"//"],
        url=urlpercentencode1["Host"]@url;scheme="http",
        url=splicein[url,{"//",""},"",
            {StringJoin[urlpercentencode1["Scheme"][First[StringSplit[#,":"]]],":"]&,urlpercentencode1["Host"]}];scheme=None]
        ,*)
        None,
        url=urlpercentencode1["Domain"]@url;
        ,
        _,
        If[StringFreeQ[url,"//"],
        url=urlpercentencode1["Domain"]@url,
        url=urlpercentencode1["Domain"]@Last[StringSplit[url,"//"]]]        
    ];
    
    If[!StringQ[url],Throw[$Failed]];
    username=OptionValue["User"];
    password=OptionValue["Password"];
    fragment=OptionValue["Fragment"];
    port=OptionValue["Port"];
    
    sparams=If[MatchQ[#,_Rule],#,#->""]&/@params;
    sparams=Map[ToString, sparams, {2}];
    sparams[[All,1]]=urlpercentencode1["Parameters"]/@sparams[[All,1]];
    sparams[[All,2]]=urlpercentencode1["ParameterValues"]/@sparams[[All,2]];
    sparams=urlParameterString[sparams, sort];
    If[username=!=None,
        If[password=!=None,
            username=StringJoin[urlpercentencode1["Username"]@username,":",
                urlpercentencode1["Password"]@password],
            username=urlpercentencode1["User"][username]
        ];
        If[scheme===None&&!StringFreeQ[url,"//"],
            url=splicein[url,{"//","@"},username,Identity]
            (* url=StringJoin[{#[[1]],"://",username,"@",#[[2]]}&@StringSplit[url,"//:"]] *)
            ,
            url=StringJoin[username, "@",url]
        ]
    ];
    If[!StringQ[url],Throw[$Failed]];
    If[scheme=!=None,
            url=StringJoin[
                If[scheme===Automatic,"",
                    {urlpercentencode1["Scheme"][scheme],":"}
                ],"//",url]];
                
    If[port=!=None,
            url=StringJoin[url,":",urlpercentencode1["Port"][port]]];
    url=StringJoin[Riffle[Join[{url},urlpercentencode1["Path"]/@path],"/"]];    
    (* Create the url string *)
    If[params=!={},
        url=StringJoin[url,"?",sparams];
    ];
    
    If[fragment=!=None,
            url=StringJoin[url,"#",urlpercentencode1["Fragment"]@fragment]];
            
    url
    
]

urlbuild[___]:=$Failed

urlParameterString[r:{_Rule...}, sortfun_:Sort] :=
    StringJoin[
        Riffle[StringJoin[#1, "=", #2]& @@@ sortfun[r],
            "&"
        ]
    ]
    
splicein[str_,{divider1_,divider2_},add_,{fun1_,fun2_}]:=
    With[{split=StringSplit[str,divider1]},
    debugPrint["split"->split];
        Switch[Length[split],
            2,
            StringJoin[{fun1[split[[1]]],divider1,add,divider2,fun2[split[[2]]]}&@split],
            (* check with the divider is before or after *)
            1,
            If[StringMatchQ[str,split[[1]]<>"*"],
                fun1[split[[1]]]<>divider1,
                divider1<>fun1[split[[1]]]
            ],
            _,Throw[$Failed]
        ]
    ]
splicein[str_,{divider1_,divider2_},add_,fun_]:=splicein[str,{divider1,divider2},add,{fun,fun}]

pathstrip[""]=""
pathstrip[str_]:=pathstripEnd[pathstripStart[str]]
pathstripStart[str_String]:=StringDrop[str,1]/;StringTake[str,1]==="/"
pathstripEnd[str_String]:=StringDrop[str,-1]/;StringTake[str,-1]==="/"
pathstripStart[str_]:=str
pathstripEnd[str_]:=str

(*********************************** URLEncode ************************************)
(* 2.1.  Percent-Encoding

   A percent-encoding mechanism is used to represent a data octet in a
   component when that octet's corresponding character is outside the
   allowed set or is being used as a delimiter of, or within, the
   component.  A percent-encoded octet is encoded as a character
   triplet, consisting of the percent character "%" followed by the two
   hexadecimal digits representing that octet's numeric value.  For
   example, "%20" is the percent-encoding for the binary octet
   "00100000" (ABNF: %x20), which in US-ASCII corresponds to the space
   character (SP).  Section 2.4 describes when percent-encoding and
   decoding is applied.

      pct-encoded = "%" HEXDIG HEXDIG

   The uppercase hexadecimal digits 'A' through 'F' are equivalent to
   the lowercase digits 'a' through 'f', respectively.  If two URIs
   differ only in the case of hexadecimal digits used in percent-encoded
   octets, they are equivalent.  For consistency, URI producers and
   normalizers should use uppercase hexadecimal digits for all percent-
   encodings. *)

URLEncode[args___]:=With[{res=Catch[urlencode[args]]},
    res/;res=!=$Failed
]

SetAttributes[URLEncode, Listable]

urlencode[s_String]:=urlpercentencode[s]

urlencode[x_]:=With[{s=ToString[x]},
    urlpercentencode[s]/;StringQ[s]
]

urlencode[___]:=$Failed

urlpercentencode[s_String] := StringReplace[s, {
    " "->"+",
    RegularExpression["[^\\w\\_\\.-]"] :>
    "%" <> ToUpperCase[IntegerString[ToCharacterCode["$0"][[1]],16]]
}]

(*********************************** URLDecode ************************************)
URLDecode[args___]:=With[{res=Catch[urldecode[args]]},
    res/;res=!=$Failed
]

SetAttributes[URLDecode,Listable]

urldecode[s_String]:=urlpercentdecode[s]

urldecode[x_]:=With[{s=ToString[x]},
    urlpercentdecode[s]/;StringQ[s]
]

urldecode[___]:=$Failed

urlpercentdecode[str_String]:=StringReplace[str, {
    "+"->" ",
    "%" ~~ x : (HexadecimalCharacter ~~ HexadecimalCharacter) :> FromCharacterCode[FromDigits[x, 16]]
}]

$URIGenDelims  = Alternatives[":", "/", "?", "#", "[", "]", "@"];
$URISubDelims  = Alternatives["!", "$", "&", "'", "(", ")", "*", "+", ",", ";", "="];
$URIReserved   = $URIGenDelims ~ Join ~ $URISubDelims;
$URIUnreserved = Alternatives[WordCharacter, "-", ".", "_", "~"];

noChars[chars_] := Except[Characters[chars]];
oneOrNull[pattern_] := Repeated[pattern, {0, 1}]
authorityParse[autority_String] := First[StringCases[
    autority, 
    StartOfString ~~ 
    oneOrNull[userinfo: noChars["@"] ... ~~ "@"] ~~ 
    domain: Alternatives[noChars[":[]"]..., "[" ~~ ___ ~~ "]"]~~ 
    oneOrNull[":" ~~ port: ___] ~~
    EndOfString :> {userinfo, ToLowerCase[domain], port}
]]

$SplitURLPath=False;
$SplitURLQuery=False;

URLParse[args___]:=With[{res=Catch[urlparse[args]]},
    res/;res=!=$Failed
]

urlparse[uri_String] := Module[{parsed = StringCases[uri, {   

    (* simple case, just scheme and path *)
    StartOfString ~~ 
    scheme:noChars[":/"].. ~~ ":" ~~ 
    path:Alternatives[
        "/" ~~ Except["/"] ~~ ___, 
        Repeated[Except["/"], {2}] ~~ ___, 
        RepeatedNull[Except["/"]]
        ] ~~
    EndOfString :> {ToLowerCase[scheme], "", "", "", path, "", ""},

    (* hierarchy case, scheme, path query and fragment *)
    StartOfString ~~ 
    oneOrNull[scheme: noChars[":/?#"].. ~~ ":"] ~~
    oneOrNull["//" ~~ autority: noChars["/?#"]...] ~~
    oneOrNull[path: noChars["?#"]...]   ~~
    oneOrNull["?"  ~~ query: noChars["#"]...]   ~~
    oneOrNull["#"  ~~ fragment: ___] ~~
    EndOfString :> {ToLowerCase[scheme], Sequence @@ authorityParse[autority], path, query, fragment}

    }, 1]},

    If[
        MatchQ[parsed, {}],
        $Failed,
        debugPrint["parsed"->parsed];
        parsed=First[parsed];
        debugPrint["parsed"->parsed];
        (* Split URL parameter string into a list of rules *)
        If[$SplitURLQuery,parsed[[6]]=fromurlparameters[parsed[[6]]]];
        (* Optionally split the Path into a list *)
        If[$SplitURLPath,parsed[[5]]=pathSplit[parsed[[5]]]];
        urlassoc[Rule @@@ Transpose[{{"Scheme", "User", "Domain", "Port", "Path", "Query", "Fragment"}, parsed} /. {Verbatim[Rule]["Scheme", ""] -> Automatic,  "" -> None}]]
    ]
]
urlparse[x_]:=(Message[URLParse::nostr,x];Throw[$Failed])/;!StringQ[x]
urlparse[___]:=$Failed

fromurlparameters[params_]:=fromurlparameters[params, {Identity,urlpercentdecode}]
fromurlparameters[params_, {decfun1_,decfun2_}]:=Module[{splitparams},
    splitparams=StringSplit[params,"&"];
    splitparams=StringSplit[#,"="]&/@splitparams;
    splitparams=PadRight[#,2,""]&/@splitparams;
    splitparams[[All,1]]=decfun1/@splitparams[[All,1]];
    splitparams[[All,2]]=decfun2/@splitparams[[All,2]];
    Rule@@@splitparams
]

pathSplit[path_]:=Module[{split=StringSplit[path,"/"]},
    If[StringMatchQ[split,"/*"],split=Join[{""},split]];
    If[StringMatchQ[split,"*/"],split=Join[split,{""}]];
    split
]

End[];

SetAttributes[
	{URLBuild, URLParse, URLEncode, URLDecode},
    {ReadProtected, Protected}
];

System`Private`RestoreContextPath[];
ClearAll[isParamNull, toURLString, URLBuildAlt];

isParamNull["Path", param_List] := MemberQ[{{}, {""}}, param];
isParamNull["Fragment", param_] := MemberQ[{None, "", "#"}, param];
isParamNull["Query", param_] := MemberQ[{None, "", "?"}, param];
isParamNull["Port", param_] := MemberQ[{None, "", 80, "80"}, param];
isParamNull[_, param_] := MemberQ[{None, ""}, param];

toURLString["Domain", param_] := ToLowerCase[ToString[param]];
toURLString["Scheme", param_] := ToLowerCase[ToString[param]];
toURLString["Fragment", param_] /; StringTake[param, 1] === "#" := StringTake[param, {2, -1}];
toURLString["Query", param_] /; StringTake[param, 1] === "?" := StringTake[param, {2, -1}];
toURLString["Path", param_List] := StringJoin @ Riffle[ToString /@ param, "/"];
toURLString[_, param_] := ToString[param];

Options[URLBuildAlt] := {
	"Scheme" -> None, 
	"User" -> None, 
	"Domain" -> None, 
	"Port" -> None, 
	"Path" -> None, 
	"Query" -> None, 
	"Fragment" -> None
};
URLBuildAlt::nodomain = "You must specify a domain in an URL.";
URLBuildAlt[OptionsPattern[]] := Module[
	{url = ""}, 
	(* If scheme is None, Automatic or "" just don't add it *)

	If[
		And[! isParamNull["Scheme", OptionValue["Scheme"]], ! OptionValue["Scheme"] === Automatic], 
		url = url <> toURLString["Scheme", OptionValue["Scheme"]] <> ":",
		If[And[isParamNull["Scheme", OptionValue["Scheme"]], ! isParamNull["Domain", OptionValue["Domain"]]], url = "http:"]
	];
	
	(* If there is at least one hirarc component must add // *)
	If[
		Or @@ Map[! isParamNull[#, OptionValue[#]] &, {"Port", "Domain", "User", "Query", "Fragment"}],
		If[isParamNull["Domain", OptionValue["Domain"]], Message[URLBuildAlt::nodomain]; Return[$Failed]];
		url = url <> "//";
	
		If[! isParamNull["User", OptionValue["User"]], url = url <> toURLString["User", OptionValue["User"]] <> "@" ];
		
		url = url <> toURLString["Domain", OptionValue["Domain"]] <> "/";
	];

	(* add a path *)
	If[
		! isParamNull["Path", OptionValue["Path"]], 
		If[
			And[! isParamNull["Domain", OptionValue["Domain"]], StringTake[toURLString["Path", OptionValue["Path"]], 1] === "/"],
			url = url <> StringTake[toURLString["Path", OptionValue["Path"]], {2, -1}],
			url = url <> toURLString["Path", OptionValue["Path"]]
		]
	];
	
	(* TODO: Accept lists of parameters {"p1"->"v1",...} *)
	If[
		! isParamNull["Query", OptionValue["Query"]],
		url = url <> "?" <> toURLString["Query", OptionValue["Query"]]
	];

	If[
		! isParamNull["Fragment", OptionValue["Fragment"]],
		url = url <> "#" <> toURLString["Fragment", OptionValue["Fragment"]]
	];

	url
	
];

toURLString["Scheme", "HHH"]

URLBuildAlt["Domain" -> "Google.com", "Scheme" -> "SSH", "User" -> "riccardo"]







