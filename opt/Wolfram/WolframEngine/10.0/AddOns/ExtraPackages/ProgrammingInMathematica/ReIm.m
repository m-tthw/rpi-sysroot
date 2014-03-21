(* :Title: ReIm *)


(* :Context: ProgrammingInMathematica`ReIm` *)

(* :Author: Roman E. Maeder *)

(* :Summary:
   simple definitions for Re and Im to handle real variables
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

(* :Warnings:
   redefine Re and Im. cannot be used together with other packages that
   do symbolic complex arithmetic, such as ComplexExpand
   or Algebra`ReIm`
*)

(* :Discussion:
   See Section 2.3 of "Programming in Mathematica"
*)

BeginPackage["ProgrammingInMathematica`ReIm`"]

Begin["`Private`"]

protected = Unprotect[Re, Im]

Re[x_] := x  /; Im[x] == 0

Re[x_+y_] := Re[x] + Re[y]
Im[x_+y_] := Im[x] + Im[y]

Re[x_ y_] := Re[x] Re[y] - Im[x] Im[y]
Im[x_ y_] := Re[x] Im[y] + Im[x] Re[y]

Protect[ Evaluate[protected] ]

End[ ]

EndPackage[ ]
