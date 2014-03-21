(* :Title: PrintTime *)


(* :Author: Roman E. Maeder *)

(* :Summary:
   printing evaluation timings
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

PrintTime::usage = "PrintTime[expr] prints the time it takes
	to evaluate expr and returns the result of the evaluation."

Begin["`Private`"]

SetAttributes[PrintTime, HoldAll]

PrintTime[expr_] :=
	With[{timing = Timing[expr]},
		Print[ timing[[1]] ];
		timing[[2]]
	]

End[];
