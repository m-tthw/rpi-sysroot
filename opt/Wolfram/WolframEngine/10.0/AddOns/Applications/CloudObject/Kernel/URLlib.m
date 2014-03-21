(* ::Package:: *)

(* Mathematica package *)
BeginPackage["CloudObject`"];

URLQ::usage = "Test if a string it's a valid URL.";
URLParse::usage = "Parse and split an URL inside it's components: {scheme, domain, port, path, querystring, fragment}";
URLEncode::usage = "Use this to escape a parameter to be used inside a querystring";
URLDecode::usage = "Use this to unescape a querystring paramenter";
QueryString::usage = "Rapresents the querystring data"; 

Begin["`Private`"];

ClearAll[
	URLParse,
	URLEncode,
	URLDecode,
	QueryString,
	$IPV6Pattern,
	$IPV4Pattern,
	$DNSPattern
]

URLEncode[s_String] :=
    StringReplace[s, {
	    " " -> "+", 
	    RegularExpression["[^a-zA-Z0-9\\_\\.\\-]"] :> 
	        StringJoin @ ("%" <> ToUpperCase @ IntegerString[#, 16] & /@ 
	            ToCharacterCode["$0", "UTF-8"])
	}]

URLDecode[s_String] := 
    FromCharacterCode[List @@ StringReplace[s, {
        RegularExpression["%([0-9a-fA-F]{2})"] :> FromDigits["$1", 16], 
        "+" :> First@ToCharacterCode[" "], 
        char_ :> First@ToCharacterCode[char]
    }], "UTF-8"]
$IPV6Pattern = StringExpression @@ Riffle[ConstantArray[Repeated[HexadecimalCharacter, {0, 8}], 8], ":"];
$IPV4Pattern = (ip1:DigitCharacter.. /; FromDigits[ip1] <= 255) ~~ 
	    "." ~~ (ip2:DigitCharacter.. /; FromDigits[ip2] <= 255) ~~ 
		"." ~~ (ip3:DigitCharacter.. /; FromDigits[ip3] <= 255) ~~ 
		"." ~~ (ip4:DigitCharacter.. /; FromDigits[ip4] <= 255);

$DNSPattern = Repeated[WordCharacter .. ~~ "."] ~~ LetterCharacter ..;
$NetworkPattern = "localhost";

Options[URLParse] := {
	"SchemaPattern" -> Alternatives[
		"http", 
		"https", 
		"ftp", 
		"ftps"
	],
	"DomainPattern" -> Alternatives[
		$DNSPattern,
		$NetworkPattern,
		$IPV6Pattern,
		$IPV4Pattern
	]
}

URLParse[url_, OptionsPattern[]] := With[{parsed = StringCases[url, 
	StartOfString
	~~ RepeatedNull[
		RepeatedNull[scheme:OptionValue["SchemaPattern"] ~~ ":"]
		~~ Repeated["//" ~~ domain:OptionValue["DomainPattern"] ~~ RepeatedNull[":" ~~ port:DigitCharacter..]]
	]
	~~ RepeatedNull["/" ~~ path:RepeatedNull[Except["?"|"#"|"/"].. ~~ Except["?"|"#"]...]]
	~~ RepeatedNull["?" ~~ querystring:Except["#"]...]
	~~ RepeatedNull["#" ~~ fragment:__]
	~~ EndOfString
 :> {scheme, domain, port, path, querystring, fragment}, 1]},
	If[
		MatchQ[parsed, {}],
		$Failed,
		First[parsed]
	]
]

Options[URLQ] := Options[URLParse] ~ Join ~ {"VerifyExist" -> False}

URLQ[url_, OptionsPattern[]] := With[{
	parsed = URLParse[
		url, 
		"SchemaPattern" -> OptionValue["SchemaPattern"],
		"DomainPattern" -> OptionValue["DomainPattern"]
    ]}, If[
		parsed === $Failed,
		False,
		If[
			OptionValue["VerifyExists"],
			URLFetch[url, "Method"->"HEAD"] =!= $Failed,
			True
		]
	]
]

QueryString[querystring_String] := QueryString @@ (Rule @@ PadRight[Map[URLDecode, StringSplit[#, "=", 2]], 2, ""] & /@ StringSplit[querystring, "&"]);
QueryString /: ToString[QueryString[args___]] := StringJoin[
	Riffle[
		If[
			StringLength[#[[2]]] > 0, 
			StringJoin[{URLEncode[#[[1]]], "=", URLEncode[#[[2]]]}], 
			URLEncode[First[#]]
		] & /@ {args}, 
		"&"
	]
];
QueryString[rules___Rule][lookup_String, All] := Cases[
	{rules},  
	Verbatim[Rule][lookup, a_] :> a
]
QueryString[rules___Rule][lookup_String] := With[
	{results = QueryString[rules][lookup, All]}, 
	If[Length[results] === 0, $Missing, Last[results]]
]

End[];

EndPackage[];
