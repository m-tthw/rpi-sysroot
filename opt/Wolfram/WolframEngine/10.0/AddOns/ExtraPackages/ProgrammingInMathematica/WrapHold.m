(* :Title: WrapHold *)


(* :Author: Roman E. Maeder *)

(* :Summary:
   keep expression unevaluated
 *)

(* :Copyright: © 1989-1996 by Roman E. Maeder *)

(* :Package Version: 2.0 *)

(* :Mathematica Version: 3.0 *)

(* :History:
   2.0 for Programming in Mathematica, 3rd ed.
   1.1 for Programming in Mathematica, 2nd ed.
   1.0 for Programming in Mathematica, 1st ed.
*)

(* :Sources:
   Roman E. Maeder. Programming in Mathematica, 3rd ed. Addison-Wesley, 1996.
*)

(* :Discussion:
   See Section 5.3 of "Programming in Mathematica"
*)

WrapHold::usage = "WrapHold[expr] wraps Hold[] around the head
	and the elements of expr without evaluating them."

Begin["`Private`"]

SetAttributes[WrapHold, HoldAll]

WrapHold[expr_] :=
	Map[ Hold, MapAt[Hold, Hold[expr], {1, 0}], {2}] [[1]]

End[];
