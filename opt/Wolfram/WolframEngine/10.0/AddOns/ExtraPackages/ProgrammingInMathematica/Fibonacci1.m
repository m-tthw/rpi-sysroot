(* :Title: Fibonacci1 *)


(* :Context: ProgrammingInMathematica`Fibonacci1` *)

(* :Author: Roman E. Maeder *)

(* :Summary:
   iterative computation of Fibonacci numbers
 *)

(* :Copyright: © 1989-1996 by Roman E. Maeder *)

(* :Package Version: 2.0 *)

(* :Mathematica Version: 3.0 *)

(* :History:
   2.0 for Programming in Mathematica, 3rd ed.
   1.1 for Programming in Mathematica, 2nd ed.
   1.0 for Programming in Mathematica, 1st ed.
*)

(* :Keywords: Fibonacci *)

(* :Sources:
   Roman E. Maeder. Programming in Mathematica, 3rd ed. Addison-Wesley, 1996.
*)

(* :Limitations:
   iterative computation is slow
*)

(* :Discussion:
   See Section 4.2 of "Programming in Mathematica"
*)

fibonacci[n_Integer?Positive] :=
	Module[{fn1=1, fn2=0},
		Do[ {fn1, fn2} = {fn1 + fn2, fn1}, {n-1} ];
		fn1
	]
