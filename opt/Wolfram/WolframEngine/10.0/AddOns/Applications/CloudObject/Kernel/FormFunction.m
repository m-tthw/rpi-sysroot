(* Mathematica package *)
BeginPackage["CloudObject`"]

System`FormFunction::usage = "FormFunction represents a web form.";

Begin["`Private`"]

Unprotect[FormFunction]

Options[FormFunction] = {DefaultResultFormat -> "PNG", DefaultParameterType -> Automatic};

FormFunction[params_, body_, options : OptionsPattern[]][args___] := 
    APIFunction[params, body, options][args]

FormFunction[body_, options : OptionsPattern[]][args___] :=
    APIFunction[body, options][args]

SetAttributes[FormFunction, {HoldAll, ReadProtected}];

Protect[FormFunction];

End[]

EndPackage[]