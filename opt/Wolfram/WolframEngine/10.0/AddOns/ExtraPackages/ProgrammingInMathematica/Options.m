(* :Title: Options *)


(* :Context: ProgrammingInMathematica`Options` *)

(* :Author: Roman E. Maeder *)

(* :Summary:
   manipulating symbols with options
 *)

(* :Copyright: © 1996 by Roman E. Maeder *)

(* :Package Version: 2.0 *)

(* :Mathematica Version: 3.0 *)

(* :History:
   2.0 for Programming in Mathematica, 3rd ed.
*)

(* :Keywords: options, symbols *)

(* :Sources:
   Roman E. Maeder. Programming in Mathematica, 3rd ed. Addison-Wesley, 1996.
*)

(* :Discussion:
   See Section 3.3 of "Programming in Mathematica"
*)

BeginPackage["ProgrammingInMathematica`Options`"]

SymbolsWithOptions::usage = "SymbolsWithOptions[opt1, opt2, ...] gives a list of
	all symbols that know about the options named opt1, opt2, ..."
SetAllOptions::usage = "SetAllOptions[opt1 -> val1, opt2 -> val2, ...] sets the
	given options for all commands that know about all of these options."

Begin["`Private`"]

SymbolsInContext[context_String] :=
        Hold @@ (ToExpression[#, InputForm, Hold]&) /@ Names[context <> "*"] /.
        Hold[sym_] :> sym

allSymbols := Join @@ SymbolsInContext /@ $ContextPath

allSymbolsWithOptions :=
    List @@ Select[ allSymbols,
                    Function[sym, Length[Options[Unevaluated[sym]]] > 0,
                                  {HoldFirst}] ]

symbolsWithOptions[ symbols_List, opts_List ] :=
        Fold[ symbolsWithOptions, symbols, opts ]

symbolsWithOptions[ symbols_List, opt_Symbol ] :=
    Select[ symbols, Function[sym, MemberQ[First /@ Options[sym], opt]] ]

SymbolsWithOptions[ opts_List ] :=
    symbolsWithOptions[ allSymbolsWithOptions, opts ]

SymbolsWithOptions[ opts__ ] := SymbolsWithOptions[ {opts} ]

SetAllOptions[ args___?OptionQ ] :=
    With[{syms = SymbolsWithOptions[First /@ Flatten[{args}]]},
         Scan[ Function[sym, SetOptions[sym, args]], syms];
         syms
    ]

End[]

Protect[ SymbolsWithOptions, SetAllOptions ]

EndPackage[]
