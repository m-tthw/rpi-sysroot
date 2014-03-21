(* :Title: Formats for trigonometric functions *)


(* :Context: ProgrammingInMathematica`TrigFormats` *)

(* :Author: Roman E. Maeder *)

(* :Summary:
   non-US traditional forms for inverse trigonometric and hyperbolic functions,
   that is, arsinh instead of sinh^{-1}
 *)

(* :Copyright: © 1996 by Roman E. Maeder *)

(* :Package Version: 2.0 *)

(* :Mathematica Version: 3.0 *)

(* :History:
   2.0 for Programming in Mathematica, 3rd ed.
*)

(* :Keywords: arc, inverse trigonometric functions, hyperbolic functions *)

(* :Sources:
   Roman E. Maeder. Programming in Mathematica, 3rd ed. Addison-Wesley, 1996.
*)

(* :Discussion:
   See Section A.2 of "Programming in Mathematica"
*)

BeginPackage["ProgrammingInMathematica`TrigFormats`"]

Begin["`Private`"]

arctrig = {ArcCos, ArcCot, ArcCsc, ArcSec, ArcSin, ArcTan};
artrigh = {ArcCosh, ArcCoth, ArcCsch, ArcSech, ArcSinh, ArcTanh};

defTraditional[arctrig_Symbol, name_String] :=
    With[{string = ToString[arctrig], form=TraditionalForm},
        MakeBoxes[arctrig[x_], form] :=
            RowBox[{name, "(", MakeBoxes[x, form], ")"}];
        MakeExpression[RowBox[{name, "(", x_, ")"}], form] :=
            MakeExpression[RowBox[{string, "(", x, ")"}], form]
    ]

defTraditional[#, ToLowerCase[ToString[#]]]& /@ arctrig
defTraditional[#, ToLowerCase[StringReplace[ToString[#], "Arc"->"Ar"]]]& /@
    artrigh

End[]

EndPackage[]
