(* :Title: Until *)


(* :Context: ProgrammingInMathematica`Until` *)

(* :Author: Roman E. Maeder *)

(* :Summary:
   a loop that tests its condition at the end of each iteration
 *)

(* :Copyright: © 1989-1996 by Roman E. Maeder *)

(* :Package Version: 2.0 *)

(* :Mathematica Version: 3.0 *)

(* :History:
   2.0 for Programming in Mathematica, 3rd ed.
   1.1 for Programming in Mathematica, 2nd ed.
   1.0 for Programming in Mathematica, 1st ed.
*)

(* :Keywords: loops, do until *)

(* :Sources:
   Roman E. Maeder. Programming in Mathematica, 3rd ed. Addison-Wesley, 1996.
*)

(* :Discussion:
   See Section 4.2 of "Programming in Mathematica"
*)

BeginPackage["ProgrammingInMathematica`Until`"]

Until::usage = "Until[body, test] evaluates body until test becomes true."

Begin["`Private`"]

Attributes[Until] = {HoldAll}

Until[body_, test_] := Module[ {t}, For[ t=False, !t, t=test, body ] ]

End[]

EndPackage[]
