(* Mathematica package *)
BeginPackage["CloudObject`"]

System`APIFunction::usage = "APIFunction is a function that takes named parameters.";

System`APIFunctionGroup::usage = "APIFunctionGroup is a group of APIFunctions.";

System`DefaultResultFormat::usage = "DefaultResultFormat is an option to APIFunction and similar functions that specifies the default return format of an API call.";

System`DefaultParameterType::usage = "DefaultParameterType is an option to APIFunction and similar functions that specifies what parameter type to use when none is explicitly given for a parameter.";

Begin["`Private`"]

Unprotect[APIFunction]

extraAllowedSymbols = HoldComplete[Integrate, Sort];
allowedContexts = {"Global`"};
allowedSymbols = Join[Security`Private`defaultAllowedSymbols, extraAllowedSymbols];

toExpressionSecure[expr_String] := Security`ToExpressionSecure[expr, {allowedContexts, allowedSymbols, {}, {}}, InputForm, Null]

APIFunction::invalidparams = "Invalid parameters specification `1`.";

APIFunction::invalidparam = "Invalid parameter specification `1` in `2`.";

APIFunction::paramsn = "Not enough parameters given in `1` for parameter list `2`.";

APIFunction::missingparam = "Required parameter `1` missing in arguments `2`.";

APIFunction::paramconv =  "Can not convert argument `1` to parameter type `2`.";

integerPattern = DigitCharacter ..;

realPattern = (DigitCharacter ..) | (DigitCharacter ... ~~ "." ~~ DigitCharacter ..) |
        (DigitCharacter .. ~~ "." ~~ DigitCharacter ...);

convertAPIParameter[value_String, type_] := (log["Converting API value `1` to type `2`", InputForm @ value, type];
    Switch[type,
        Automatic,
            If[StringMatchQ[value, realPattern],
                ToExpression[value],
                value
            ],
        Integer, 
            If[StringMatchQ[value, integerPattern],
                FromDigits[value], 
                Message[APIFunction::paramconv, value, type]
            ],
        Real | Number, 
            If[StringMatchQ[value, realPattern],
                ToExpression[value], 
                Message[APIFunction::paramconv, value, type]
            ],
        Expression,
            toExpressionSecure[value],
        "UnsafeExpression",
            ToExpression[value],
        _,
            value
   ])

convertAPIParameter[CloudObject`Private`APIUploadedFileContent[value_String], "FileName"] := 
	Module[{filepath = CreateTemporary[]},
		Export[value, filepath, "String"];
		filepath
	]

convertAPIParameter[CloudObject`Private`APIUploadedFileContent[value_String], type_] := 
	convertAPIParameter[value, type]

convertAPIParameter[CloudObject`Private`APIUploadedFileName[filepath_String], "FileName"] := 
	filepath

convertAPIParameter[CloudObject`Private`APIUploadedFileName[filepath_String], Integer | Real | Number] := 
	Import[filepath, "Text"]

convertAPIParameter[CloudObject`Private`APIUploadedFileName[filepath_String], Automatic] := 
	Import[filepath]

convertAPIParameter[value_, Automatic] := (log["Automatic API value `1`", value]; value)

(*When an arbitrary Mathematica expression is given, convert it to a String first to mimic actual API behavior. *)
convertAPIParameter[value_, type_] := convertAPIParameter[ToString[value, InputForm], type]

convertAPIParameter[value_] := convertAPIParameter[value, Automatic]

Options[APIFunction] = {DefaultResultFormat -> "PNG", DefaultParameterType -> Automatic};

scopedNames[expr_] := Alternatives[]
scopedNames[Verbatim[Function][body_]] := _Slot | _SlotSequence
scopedNames[Verbatim[Function][{vars___}, body_]] := HoldPattern[Alternatives[vars]]
Attributes[scopedNames] = {HoldFirst};
(* TODO: add rules for APIFunction *)

(* Sow the parts that will be replaced *)
scopedParts[atom_, rules_, patterns_, pos_] :=
    If[MatchQ[Unevaluated[atom], patterns],
        Sow[pos :> atom /. rules]
    ] /; AtomQ[Unevaluated[atom]] || MatchQ[Unevaluated[atom], _Slot|_SlotSequence]
scopedParts[expr_, rules_, pattern_, pos_] :=
    Module[{scoped, newRules, newPattern},
        scoped = scopedNames[expr];
        newRules = DeleteCases[rules, (Rule | RuleDelayed)[scoped, _]];
        newPattern = DeleteCases[pattern, scoped];
        MapIndexed[Function[{part, index},
            scopedParts[part, newRules, newPattern, Join[pos, index]], {HoldFirst}
        ], Unevaluated[expr], Heads->True]
    ]
Attributes[scopedParts] = {HoldFirst};

applyScoped[expr_, rules_] := Module[{patterns, replacements},
    patterns = Alternatives @@ rules[[ ;; , 1]];
    replacements = Last @ Reap[scopedParts[expr, rules, patterns, {}]];
    If[replacements === {},
        expr, 
    (* else: non-empty replacements *)
        If[MatchQ[First @ replacements, {{} :> _}],
            Last @ First @ First @ replacements,
        (* else *)
            ReplacePart[Unevaluated[expr], First @ replacements]
        ]
    ]
]
Attributes[applyScoped] = {HoldFirst};

isSymbol[expr_] := AtomQ[Unevaluated[expr]] && MatchQ[Unevaluated[expr], _Symbol]
Attributes[isSymbol] = {HoldFirst}

isMissing[expr_] := MatchQ[expr, _Missing]

parseAPIParameters[params_, defaultType_ : Automatic, holdDefault_:False] := Module[{paramsList},
    If[Head[Unevaluated[params]] === List,
        paramsList = Hold[params],
        paramsList = Hold[{params}]
    ];
    ReleaseHold @ Replace[paramsList, {
        (spec_ -> symbol_?isSymbol) :> (Replace[Unevaluated[spec], {
            {name_String, type_, default_} :> (name -> {HoldPattern[symbol], type, If[TrueQ[holdDefault], Hold[default], default]}), (* TODO consider making holdDefault=True the behavior, but need to change all callers *)
            {name_String, type_} :> (name -> {HoldPattern[symbol], type}),
            {name_String} :> (name -> {HoldPattern[symbol], defaultType}),
            name_String :> (name -> {HoldPattern[symbol], defaultType}),
            _ :> (Message[APIFunction::invalidparam, spec, params]; Return[$Failed])
        }, {0}]),
        symbol_ :> (SymbolName[symbol] -> {HoldPattern[symbol], Automatic}), 
        _ :> (Message[APIFunction::invalidparams, params]; Return[$Failed])
    }, {2}]
]
    
Attributes[parseAPIParameters] = {HoldAll}

applyAPIFunction[parameters_, body_, {args___}, fullExpr_] :=
    Module[{position, arguments, givenParameters, notGivenParameters, slots, positionalSlots, argvalue},
        position = 1;
        arguments = {};
        givenParameters = {};
        positionalSlots = {};
        Check[
            Map[Function[arg,
                If[Head[arg] === Association,
                (* Association -> named arguments *)
                    Cases[parameters, (name_ -> {symbol_, type_, ___}) :> 
                        If[!isMissing[arg[name]],
                            (* Collection contains the formal parameter *)
                            AppendTo[arguments, symbol -> convertAPIParameter[arg[name], type]];
                            AppendTo[givenParameters, name]
                        ]
                    ],
                (* no Association *)
                    If[Head[arg] === List && And @@ (MatchQ[#, _String -> _]& /@ arg),
                    (* List of Rules -> named arguments *)
                        Cases[parameters, (name_ -> {symbol_, type_, ___}) :> Module[{missing, value},
                            missing = False;
                            value = Replace[name, Join[arg, {_ :> (missing = True)}]];
                            If[!missing,
                                AppendTo[arguments, symbol -> convertAPIParameter[value, type]];
                                AppendTo[givenParameters, name]
                            ]
                        ]],
                    (* positional arguments *)
                        If[position <= Length[parameters],
                        (* fill formal parameter from this position *)
                            argvalue = convertAPIParameter[arg, parameters[[position, 2, 2]]];
                            AppendTo[arguments, parameters[[position, 2, 1]] -> argvalue]; 
                            AppendTo[givenParameters, parameters[[position, 1]]];
                            ++position;,
                        (* no formal parameter at this position any more *)
                            argvalue = convertAPIParameter[arg, defaultType];
                            AppendTo[positionalSlots, argValue];
                        ]
                    ]
                ]
            ], {args}];
            slots = {};
            (* Intentionally, do not include optional parameters in the slots. *)
            Replace[parameters, (name_ -> {symbol_, type_}) :> If[MemberQ[givenParameters, name],
                AppendTo[slots, First @ Cases[arguments, (Verbatim[symbol] -> value_) -> value, {1}, 1]]
            ], {1}];
            slots = Join[slots, positionalSlots];
            notGivenParameters = DeleteCases[parameters, Alternatives @@ (# -> _ & /@ givenParameters)];
            Replace[notGivenParameters, {
                (name_ -> {_, _}) :> (Message[APIFunction::missingparam, name, {args}]; Return[$Failed]),
                (name_ -> {symbol_, type_, default_}) :> AppendTo[arguments, symbol -> default]}, {1}
            ];,
        (* if any parameter conversion fails *)
            Return[$Failed]
        ];
        log["Arguments: `1`", arguments, DebugLevel -> 2];
        
        (* Apply arguments, but take into account other scoping constructs (specifically Function). *)
        applyScoped[body, Join[arguments, {SlotSequence[n_] :> Sequence @@ slots[[n ;; ]]},
            MapIndexed[Function[{value, index}, Slot[First @ index] :> value], slots],
            {Slot[0] :> fullExpr}
        ]]
    ]
    
Attributes[applyAPIFunction] = {HoldAll}

APIFunction[params_, body_, options : OptionsPattern[]][args___] := 
    Module[{defaultType, parameters},
        defaultType = OptionValue[DefaultParameterType];
        parameters = parseAPIParameters[params, defaultType];
        If[parameters === $Failed, Return[$Failed]];
        applyAPIFunction[parameters, body, {args}, APIFunction[Unevaluated[params], Unevaluated[body], options]]
	]

(*TODO: Care about Function scoping.*)

(*APIFunction[body_][args___]:=Module[{params},APIFunction[params,body]/.\
params\[Rule]Table[Slot[index],{index,Length[{args}]}]][args]*)

(*APIFunction[body_][args___]:=Module[{arguments},
arguments=MapIndexed[Function[{arg,index},Slot[index]\[Rule]arg],{arguments}];
]*)

APIFunction[body_, OptionsPattern[]][args___] := Module[{defaultType},
    defaultType = OptionValue[DefaultParameterType]; 
    Check[Function[body] @@ (convertAPIParameter[#, defaultType] & /@ 
        DeleteCases[{args}, _Collection]),
    (* if the Function can not be applied *)
        $Failed,
        Function::slotn
    ]
]


(*APIFunction[args___] := (ArgumentCountQ[APIFunction,Length[DeleteCases[{args},_Rule,Infinity]],1,2];Null/;False)
*)

SetAttributes[APIFunction, {HoldAll,ReadProtected}]

Protect[APIFunction]

cloudAPIFunctionHyperlink[CloudObject[uri_], params_List : {}] :=
    Hyperlink[uri <> JoinURLSearch[Replace[params, (Rule|RuleDelayed)[key_, value_] :> (key -> ToString[value]), {1}]]]

cloudAPIFunctionHyperlink[CloudObject[uri_], param_ : {}] := CloudAPIFunctionHyperlink[CloudObject[uri], {param}]

cloudAPIFunctionHyperlink[uri_, params_] := CloudAPIFunctionHyperlink[CloudObject[uri], params] 

End[]

EndPackage[]