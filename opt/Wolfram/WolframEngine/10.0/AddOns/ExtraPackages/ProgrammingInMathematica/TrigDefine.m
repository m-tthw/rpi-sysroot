(* :Title: TrigDefine *)


(* :Context: ProgrammingInMathematica`TrigDefine` *)

(* :Author: Roman E. Maeder *)

(* :Summary:
   global rules for for putting products of trigonometric functions into
   normal form
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
   See Section 6.3 of "Programming in Mathematica"
*)

BeginPackage["ProgrammingInMathematica`TrigDefine`"]

TrigDefine::usage = "TrigDefine.m defines global rules for putting
	products of trigonometric functions into normal form."

Begin["`Private`"]    (* set the private context *)

(* unprotect any system functions for which rules will be defined *)

protected = Unprotect[ Sin, Cos ]

(* linearization *)

Sin/: Sin[x_] Cos[y_] := Sin[x+y]/2 + Sin[x-y]/2
Sin/: Sin[x_] Sin[y_] := Cos[x-y]/2 - Cos[x+y]/2
Cos/: Cos[x_] Cos[y_] := Cos[x+y]/2 + Cos[x-y]/2

Sin/: Sin[x_]^n_Integer?Positive := Expand[(1/2 - Cos[2x]/2) Sin[x]^(n-2)]
Cos/: Cos[x_]^n_Integer?Positive := Expand[(1/2 + Cos[2x]/2) Cos[x]^(n-2)]

Protect[ Evaluate[protected] ]      (* restore protection of system symbols *)

End[]         (* end the private context *)

EndPackage[]  (* end the package context *)
