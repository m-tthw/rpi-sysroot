(* :Title: Continued Fractions *)


(* :Context: ProgrammingInMathematica`ContinuedFraction` *)

(* :Author: Roman E. Maeder *)

(* :Summary:
   conversion from and to continued fractions
 *)

(* :Copyright: © 1996 by Roman E. Maeder *)

(* :Package Version: 2.0 *)

(* :Mathematica Version: 3.0 *)

(* :History:
   2.0 for Programming in Mathematica, 3rd ed.
*)

(* :Keywords: continued fraction, approximation *)

(* :Sources:
   Roman E. Maeder. Programming in Mathematica, 3rd ed. Addison-Wesley, 1996.
*)

(* :Discussion:
   See Section 7.3 of "Programming in Mathematica"
*)

BeginPackage["ProgrammingInMathematica`ContinuedFraction`"]

CF::usage = "CF[r, n] computes up to n terms of the continued fraction
	expansion of r."
CFValue::usage = "CFValue[list] gives the rational value of a continued fraction."

Begin["`Private`"]

CF[r0_?NumericQ, n_Integer?NonNegative] :=
    Module[{l = {}, r = r0, a},
        Do[ a = Floor[r];          (* integer part *)
            AppendTo[l, a];
            r = r - a;             (* fractional part; 0 <= r < 1 *)
            If[ r == 0, Break[] ];
            r = 1/r;               (* r > 1 *)
          , {n}];
        l
    ]

CFValue[l_List] := Fold[ 1/#1 + #2&, Infinity, Reverse[l] ]

End[]

Protect[ CF, CFValue ]

EndPackage[]
