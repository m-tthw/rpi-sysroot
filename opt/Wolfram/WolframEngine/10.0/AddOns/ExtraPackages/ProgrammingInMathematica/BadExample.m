(* :Title: BadExample *)


(* :Context: ProgrammingInMathematica`BadExample` *)

(* :Author: Roman E. Maeder *)

(* :Summary:
   a function with a local variable that has global scope
 *)

(* :Copyright: © 1989-1996 by Roman E. Maeder *)

(* :Package Version: 2.0 *)

(* :Mathematica Version: 3.0 *)

(* :History:
   2.0 for Programming in Mathematica, 3rd ed.
   1.1 for Programming in Mathematica, 2nd ed.
   1.0 for Programming in Mathematica, 1st ed.
*)

(* :Keywords: scope, nesting, bad programming style *)

(* :Sources:
   Roman E. Maeder. Programming in Mathematica, 3rd ed. Addison-Wesley, 1996.
*)

(* :Warnings:
   as the name implies, this is an example of bad programming
*)

(* :Discussion:
   See Section 1.2 of "Programming in Mathematica"
*)

(* :Requirements:
   ProgrammingInMathematica/Package1.m
*)

(* this function returns the sum of the first n powers of x *)

PowerSum[x_, n_] := Sum[ x^i, {i, 1, n} ]
