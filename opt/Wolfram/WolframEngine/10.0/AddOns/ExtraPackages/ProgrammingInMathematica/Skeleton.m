
(* :Title: Skeleton.m -- a package template *)

(* :Context: ProgrammingInMathematica`Skeleton` *)

(* :Author: Roman E. Maeder *)

(* :Summary:
   The skeleton package is a syntactically correct framework for package
   development.
 *)

(* :Copyright: © <year> by <name or institution> *)

(* :Package Version: 2.0 *)

(* :Mathematica Version: 3.0 *)

(* :History:
   2.0 for Programming in Mathematica, 3rd ed.
   1.1 for Programming in Mathematica, 2nd ed.
   1.0 for Programming in Mathematica, 1st ed.
*)

(* :Keywords: template, skeleton, package *)

(* :Sources:
   Roman E. Maeder. Programming in Mathematica, 3rd ed. Addison-Wesley, 1996.
*)

(* :Warnings:
   <description of global effects, incompatibilities>
*)

(* :Limitations:
   <special cases not handled, known problems>
*)

(* :Discussion:
   <description of algorithm, information for experts>
*)

(* :Requirements:
   ProgrammingInMathematica/Package1.m
   ProgrammingInMathematica/Package2.m
   ProgrammingInMathematica/Package3.m
*)

(* :Examples:
   <sample input that demonstrates the features of this package>
*)


(* set up the package context, including public imports *)

BeginPackage["ProgrammingInMathematica`Skeleton`", 
	{"ProgrammingInMathematica`Package1`", "ProgrammingInMathematica`Package2`"}]

(* usage messages for the exported functions and the context itself *)

Skeleton::usage = "Skeleton.m is a package that does nothing."

Function1::usage = "Function1[n] does nothing."
Function2::usage = "Function2[n, (m:17)] does even more nothing."

(* error messages for the exported objects *)

Skeleton::badarg = "You twit, you called `1` with argument `2`!"

Begin["`Private`"]    (* begin the private context (implementation part) *)

Needs["ProgrammingInMathematica`Package3`"]    (* read in any hidden imports *)

(* unprotect any system functions for which definitions will be made *)

protected = Unprotect[ Sin, Cos ]

(* definition of auxiliary functions and local (static) variables *)

Aux[f_] := Do[something, iterator]

staticvar = 0

(* definition of the exported functions *)

Function1[n_] := n

Function2[n_, m_:17] := n m /; n < 5 || Message[Skeleton::badarg, Function2, n]

(* definitions for system functions *)

Sin/: Sin[x_]^2 := 1 - Cos[x]^2

Protect[ Evaluate[protected] ]     (* restore protection of system symbols *)

End[ ]         (* end the private context *)

Protect[ Function1, Function2 ]    (* protect exported symbols *)

EndPackage[ ]  (* end the package context *)
