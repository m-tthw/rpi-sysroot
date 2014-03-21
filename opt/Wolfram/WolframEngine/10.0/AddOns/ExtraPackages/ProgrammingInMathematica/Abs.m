(* :Title: Abs *)


(* :Context: ProgrammingInMathematica`Abs` *)

(* :Author: Roman E. Maeder *)

(* :Summary:
   rules for Abs[] and Sign[], assuming real arguments
 *)

(* :Copyright: © 1989-1996 by Roman E. Maeder *)

(* :Package Version: 2.0 *)

(* :Mathematica Version: 3.0 *)

(* :History:
   2.0 for Programming in Mathematica, 3rd ed.
   1.1 for Programming in Mathematica, 2nd ed.
   1.0 for Programming in Mathematica, 1st ed.
*)

(* :Keywords: Abs, Sign, real *)

(* :Sources:
   Roman E. Maeder. Programming in Mathematica, 3rd ed. Addison-Wesley, 1996.
*)

(* :Discussion:
   See Section 8.3 of "Programming in Mathematica"
*)

BeginPackage["ProgrammingInMathematica`Abs`"]

Begin["`Private`"]    (* set the private context *)

protected = Unprotect[Abs, Sign]

Abs[x_?Positive] :=  x
Abs[x_?Negative] := -x

Sign/: x_ Sign[x_] := Abs[x]

Abs/: Derivative[n_Integer?Positive][Abs] := Derivative[n-1][Sign]

Derivative[n_Integer?Positive][Sign][x_]/; x != 0 := 0
Derivative[n_Integer?Positive][Sign][0] := Indeterminate

Sign[0] = Indeterminate (* consistency *)

Abs /: Integrate[Abs[x_], x_] := x Abs[x]/2
Sign/: Integrate[Sign[x_], x_] := Abs[x]

Protect[ Evaluate[protected] ]      (* restore protection of system symbols *)

End[]         (* end the private context *)

EndPackage[]  (* end the package context *)
