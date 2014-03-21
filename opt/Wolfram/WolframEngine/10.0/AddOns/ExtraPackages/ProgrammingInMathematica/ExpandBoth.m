(* :Title: ExpandBoth *)


(* :Context: ProgrammingInMathematica`ExpandBoth` *)

(* :Author: Roman E. Maeder *)

(* :Summary:
   expanding numerators and denominators separately
 *)

(* :Copyright: © 1989-1996 by Roman E. Maeder *)

(* :Package Version: 2.0 *)

(* :Mathematica Version: 3.0 *)

(* :History:
   2.0 for Programming in Mathematica, 3rd ed.
   1.1 for Programming in Mathematica, 2nd ed.
   1.0 for Programming in Mathematica, 1st ed.
*)

(* :Keywords: expand, numerator, denominator *)

(* :Sources:
   Roman E. Maeder. Programming in Mathematica, 3rd ed. Addison-Wesley, 1996.
*)

(* :Discussion:
   See Section 2.1 of "Programming in Mathematica"
*)

ExpandBoth::usage = "ExpandBoth[e] expands all numerators and denominators in e."

Begin["`Private`"]

ExpandBoth[x_Plus] := ExpandBoth /@ x
ExpandBoth[x_] := Expand[ Numerator[x] ] / Expand[ Denominator[x] ]

End[]
Null
