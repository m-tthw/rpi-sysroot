Package["Macros`"]


PackageExport["SetUsage"]

SetAttributes[SetUsage, HoldFirst];

SetUsage[msg_Symbol, string_String] := 
	Set[MessageName[msg, "usage"], tolinearsyntax[string]];
	
SetUsage[msg_Symbol, strings__String] :=
	Set[MessageName[msg, "usage"], StringJoin @ Riffle[tolinearsyntax /@ {strings}, "\n"]];

tolinearsyntax[string_] := StringReplace[string, {
		"->" -> "\[Rule]",
		"'" -> "\"",
		w:LetterCharacter.. ~~ "$" ~~ i:DigitCharacter|LetterCharacter :> ToString[Style[Subscript[w, Style[i, "TR"]],"TI"], StandardForm] <> "\[VeryThinSpace]",
		w:LetterCharacter.. ~~ "$(" ~~ Shortest[i___] ~~ ")" :> ToString[Style[Subscript[w, Style[i, "TR"]],"TI"], StandardForm] <> "\[VeryThinSpace]",
		w:LetterCharacter.. ~~ "$" :> ToString[Style[w,"TI"], StandardForm] <> "\[VeryThinSpace]",
		"$$" -> ToString[Style["\[Ellipsis]", "TR"]]
	}]


PackageExport["InactivateFull"]

SetAttributes[InactivateFull,HoldFirst];
InactivateFull[x_] := 
	Inactivate @@ ReplaceAll[Hold[x], 
		s_Symbol ? ValueQ :> Inactive[s]
	];


PackageExport["InactiveSymbol"]

InactiveSymbol[str_String] :=
	ToExpression[str, InputForm, Inactive];


PackageExport["DeclareMacro"]
PackageExport["$MacroHead"]

DeclareMacro[sym_Symbol, func_] := 
	TagSetDelayed[sym,
		Verbatim[SetDelayed][lhs_, Verbatim[sym][args___]],
		Activate[
			Inactive[SetDelayed][
				Inactive[lhs], 
				ParseInactives[
					Block[{$MacroHead = gethead[lhs]},
						func @@ InactivateFull[{args}]]
				]
			]
		]
	];


PackageExport["ParseInactives"]

ParseInactives[expr_] :=
	ReplaceAll[
		expr,
		s_Symbol /; StringMatchQ[SymbolName[Unevaluated[s]], "$" ~~ ___ ~~ "$"] :> 
			ToExpression[StringTake[SymbolName[s], {2, -2}], InputForm, Inactive]
	];
	
	
SetAttributes[gethead, HoldAll];
gethead[head_[___]] := gethead[head];
gethead[head_Symbol] := head;
gethead[_] := None;
