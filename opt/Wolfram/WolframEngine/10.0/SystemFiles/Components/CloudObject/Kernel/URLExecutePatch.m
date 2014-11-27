BeginPackage["CloudObject`"]

System`URLExecute;

Begin["`Private`"]

Unprotect[System`URLExecute]
ClearAll[System`URLExecute]

URLExecute::clouderror = "Unable to execute `1`. The object does not exist or is private.";

$NullValues = HoldPattern[None|Null|_Missing|Missing|""];

$URLheadPattern = HoldPattern @ Alternatives[
	_CloudObject,
	Hyperlink[__],
	Entity["Internet", __]
];


getFromURLhead[HoldPattern[Entity["Internet", uri_, ___]]] := uri;
getFromURLhead[HoldPattern[CloudObject[uri_, ___]]] := uri;
getFromURLhead[Hyperlink[uri_]] := uri;
getFromURLhead[Hyperlink[_, uri_, ___]] := uri;


(* Normal URLExecute Behaviour *)

URLExecute[url:_String|_List|_Association|_Rule|_RuleDelayed|$URLheadPattern, params:$NullValues|_String, args___] :=
	System`URLExecute[url, URLQueryDecode[params], args];

URLExecute[url:_String|_List|_Association|_Rule|_RuleDelayed, params:_List|_Association:{}, format_:Automatic] :=
	Replace[
		URLUtilities`URLCorrect @ URLBuild[url, params], {
			$Failed -> $Failed,
			value_ :> Import[
				value,
				Replace[
					format, {
						Automatic -> Sequence[],
						$NullValues -> "String"
					}
				]
			]
		}
	]


(* On the cloud behaviour *)

URLExecute[url_CloudObject, params:_List|_Association:{}, format_:Automatic] :=
	Replace[
		execute[
			url,
			"GET",
			"objects",
			"Parameters" -> params
		], {
			$Failed -> $Failed,
			_HTTPError :> (Message[URLExecute::clouderror, url]; $Failed),
			body_ :> ImportString[
				FromCharacterCode[body // Last],
				Replace[
					format, {
						Automatic -> Sequence[],
						$NullValues -> "String"
					}
				]
			]
		}
	]


URLExecute[url:$URLheadPattern, args___] := URLExecute[getFromURLhead[url], args];


End[]

EndPackage[]
