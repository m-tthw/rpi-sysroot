(* Mathematica package *)
BeginPackage["CloudObject`"]

System`Delayed;
System`UpdateInterval;

Begin["`Private`"]

Unprotect[Delayed]

Options[Delayed] = {UpdateInterval -> Infinity};

Delayed[expr_, OptionsPattern[]][___] := Module[{updateInterval},
    updateInterval = OptionValue[UpdateInterval];
    If[TrueQ[updateInterval < Infinity],
        ExportForm[expr, "CloudExpression", UpdateInterval->updateInterval],
    (* else *)
        expr
    ]
]

SetAttributes[Delayed, {HoldFirst}]

Protect[Delayed]

End[]

EndPackage[]