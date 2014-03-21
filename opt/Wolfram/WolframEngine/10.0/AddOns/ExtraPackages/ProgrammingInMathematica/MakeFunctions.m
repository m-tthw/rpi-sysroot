(* :Title: MakeFunctions *)


(* :Context: ProgrammingInMathematica`MakeFunctions` *)

(* :Author: Roman E. Maeder *)

(* :Summary:
   functions that define functions
 *)

(* :Copyright: © 1989-1996 by Roman E. Maeder *)

(* :Package Version: 2.0 *)

(* :Mathematica Version: 3.0 *)

(* :History:
   2.0 for Programming in Mathematica, 3rd ed.
*)

(* :Sources:
   Roman E. Maeder. Programming in Mathematica, 3rd ed. Addison-Wesley, 1996.
*)

(* :Discussion:
   See Section 5.1 of "Programming in Mathematica"
*)

BeginPackage["ProgrammingInMathematica`MakeFunctions`"]

StepFunction::usage = "StepFunction[f, a, x0, b] defines rules for f
	such that f[x] = a for x <= x0, f[x] = b for x > x0."

LinearFunction::usage = "LinearFunction[f, a, x0, x1, b] defines rules for f
	such that f[x] = a for x <= x0, f[x] = b for x >= x1 and
	f increases linearly from a to b between x0 and x1."

MakeRule::usage = "MakeRule[f, x, rhs] globally defines the rule f[x_] := rhs."

MakeRuleConditional::usage = "MakeRuleConditional[f, x, rhs, cond]
	globally defines the rule f[x_] /; cond := rhs."


Begin["`Private`"]

SetAttributes[MakeRule, HoldAll]

MakeRule[f_Symbol, var_Symbol, rhs_] := f[var_] := rhs

SetAttributes[MakeRuleConditional, HoldAll]

MakeRuleConditional[f_Symbol, var_Symbol, rhs_, condition_] :=
	f[var_] /; condition := rhs


StepFunction[f_Symbol, a_, x0_, b_] := (
	MakeRuleConditional[f, x, a, x <= x0];
	MakeRuleConditional[f, x, b, x >  x0];
	)

LinearFunction[f_Symbol, a_, x0_, x1_, b_] :=
	With[{slope = (b-a)/(x1-x0)},
		MakeRuleConditional[f, x, a, x <= x0];
		MakeRuleConditional[f, x, a + (x-x0) slope, x0 < x < x1];
		MakeRuleConditional[f, x, b, x >= x1];
	]

End[]

Protect[StepFunction, LinearFunction, MakeRule, MakeRuleConditional]

EndPackage[]
