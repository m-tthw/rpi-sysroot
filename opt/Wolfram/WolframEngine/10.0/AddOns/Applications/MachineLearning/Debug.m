BeginPackage["MachineLearning`Debug`"]

PrintFull;
Echo;
EchoHeld;
EchoSteps;
EchoStep;

Begin["`Private`"];


PrintFull[x___] := Apply[Print, FullForm /@ {x}];

Echo[x_] := With[{r = x}, With[{s = makeShallow[3][r]}, EchoStep[s, r]]; r]; 


SetAttributes[EchoHeld, HoldAll];

EchoHeld[x_] := With[{res = x}, WithColor[EchoStep[HoldForm[x], res], LightBlue]; makeShallow[3][res]]; 


SetAttributes[{EchoSteps, HeldSequence}, HoldAll];

HeldSequence[func_] := Sequence[];

$EchoIndent = 0;
SetAttributes[WithIndent, HoldFirst];
WithIndent[body_] := Block[{$EchoIndent = $EchoIndent + 1}, body];

$EchoColor = GrayLevel[0.95];
SetAttributes[WithColor, HoldRest];
WithColor[color_, body_] := Block[{$EchoColor = color}, body];

makePaster[value_][expr_] := EventHandler[
	MouseAppearance[expr, "LinkHand"], {
	"MouseClicked" :> CellPrint[ExpressionCell[value, "Output"]]}
];

makePaster[$none][x_] := x;

SetAttributes[EchoStep, HoldFirst];
EchoStep[step_, other_:$none] := 
	CellPrint[
		ExpressionCell[makePaster[other] @ 	
			Framed[HoldForm[step] /. 
				s_Symbol /; !MemberQ[$ContextPath, Context[Unevaluated[s]]] :> 
					Block[{}, Symbol["Global`" <> SymbolName[Unevaluated[s]]] /; True], 
				BaseStyle -> {
					LineBreakWithin -> False,
					ShowStringCharacters -> True
				},
				FrameStyle -> None,
				Background -> $EchoColor
			], "Print",
			CellMargins -> {{20+$EchoIndent*25,0},{0,0}}
		]
	];

HeldSequence[func_, first_, rest___] := Sequence[func[first], HeldSequence[func, rest]];

Clear[makeShallow, shortenList];
	
makeShallow[0][head_Symbol[body___]] :=
	If[head === List, 
		Skeleton[Length[{body}]],
		HoldForm[head][Skeleton[Length[{body}]]]
	];

makeShallow[n_][a_ ? AtomQ] := a;

shortenList[list_List, n_] := If[Length[list] <= 10, makeShallow[n-1] /@ list, 
	Insert[makeShallow[n-1] /@ Drop[list, {4,-4}], Skeleton[Length[list]-6], 4]];

makeShallow[n_][list_List] := shortenList[list, n];

makeShallow[n_?Positive][head_[body___]] :=
	If[head === List, 
		shortenList[{body}, n], 
		HoldForm[head] @@ makeShallow[n][{body}]
	];
	
makeShallow[_][x_] := x;
	
EchoSteps[CompoundExpression[args___]] := Last[{HeldSequence[EchoSteps,args]}];

EchoSteps[Null] := Sequence[];

EchoSteps[Verbatim[Set][sym_Symbol, value_]] := With[{v = value},
	WithColor[LightOrange, EchoStep[Set[sym, value], makeShallow[3][v]]];
	Set[sym, v]
];

EchoSteps[Verbatim[If][test_, a_, b_:Null, c_:Null]] := With[{res = test},
	WithColor[If[res, LightGreen, LightRed, Orange], EchoStep[If[test]]]; 
	WithIndent[If[res, EchoSteps[a], EchoSteps[b], EchoSteps[c]]]
];

EchoSteps[Return[x_]] := 
	Return[WithColor[LightPurple, EchoStep[Return[x]]]; x, Module];

EchoSteps[Verbatim[Module][vars_, body_]] := (
	WithColor[GrayLevel[0.85], EchoStep[Module[vars]]]; 
	WithIndent[Module[vars, EchoSteps[body]]]
);

EchoSteps[Verbatim[With][vars_, body_]] := (
	WithColor[GrayLevel[0.85], EchoStep[With[vars]]]; 
	WithIndent[With[vars, EchoSteps[body]]]
);

EchoSteps[Verbatim[Switch][expr_, rest___]] := 
	With[{value = expr},
		WithColor[RGBColor[0.725, 0.753, 0.314], EchoStep[Switch[expr], value]]; 
		findMatchingClause[expr, rest]
	];

SetAttributes[findMatchingClause, HoldAll];

findMatchingClause[expr_, pattern_, action_, rest___] :=
	If[MatchQ[expr, pattern], 
		WithIndent[
			WithColor[Lighter[Red,0.4], EchoStep[pattern]];
			WithIndent[EchoSteps[action]]]
	,
		findMatchingClause[expr, rest]
	];
	
EchoSteps[expr_] := (EchoStep[expr]; expr);

End[];
EndPackage[];