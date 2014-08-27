(* Mathematica package *)
BeginPackage["CloudObject`"]

System`ExternalFunction::usage = "ExternalFunction represents an external function to be called from another language.";

Begin["`Private`"]

Unprotect[ExternalFunction]

convertAPIParameter[value_String, "JSONExpr"] := Module[{data, type},
    data = ImportString[value, "JSON"];
    type = Null;
    Cases[data, ("type" -> v_) :> (type = v)];
    If[type === "datetime",
        Association[data]["value"],
        data
    ]
]

ExternalFunction[lang_, params_, body_][args_] := Module[{parameters},
    parameters = parseAPIParameters[params];
    parameters = parameters /. (name_ -> {symbol_, type_, rest___}) :> (name -> {symbol, "JSONExpr", rest});
    applyAPIFunction[parameters, body, {args}, ExternalFunction[lang, params, body]]
]

SetAttributes[ExternalFunction, {HoldRest, ReadProtected}];

Protect[ExternalFunction];

End[]

EndPackage[]