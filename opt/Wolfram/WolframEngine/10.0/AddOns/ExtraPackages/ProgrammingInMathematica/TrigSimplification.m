(* :Title: TrigSimplification *)


(* :Context: ProgrammingInMathematica`TrigSimplification` *)

(* :Author: Roman E. Maeder *)

(* :Summary:
   functions for expanding and contracting products and powers of
   trigonometric functions. For educational purposes only.
 *)

(* :Copyright: © 1989-1996 by Roman E. Maeder *)

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

(* :Discussion:
   See Section 6.2 of "Programming in Mathematica"
*)

BeginPackage["ProgrammingInMathematica`TrigSimplification`"]

TrigLinear::usage = "TrigLinear[e] expands products and powers of
	trigonometric functions."

TrigArgument::usage = "TrigArgument[e] writes trigonometric functions
	of sums and products as products of simple trigonometric functions."

Begin["`Private`"]

trigLinearRules = {
	Sin[x_] Cos[y_] :> Sin[x+y]/2 + Sin[x-y]/2,
	Sin[x_] Sin[y_] :> Cos[x-y]/2 - Cos[x+y]/2,
	Cos[x_] Cos[y_] :> Cos[x+y]/2 + Cos[x-y]/2,
	Sin[x_]^(m1_Integer?EvenQ) :>
	 With[{m=Abs[m1]},
	  (2^(-m+1) (Sum[(-1)^(m/2-k) Binomial[m,k] Cos[(m-2k)x], {k, 0, m/2-1}]+
	             Binomial[m,m/2]/2))^Sign[m1] ],
	Cos[x_]^(m1_Integer?EvenQ) :>
	 With[{m=Abs[m1]},
	  (2^(-m+1) (Sum[Binomial[m,k] Cos[(m-2k)x], {k, 0, m/2-1}] +
	             Binomial[m,m/2]/2))^Sign[m1] ],
	Sin[x_]^(m1_Integer?OddQ) :>
	 With[{m=Abs[m1]},
	  (2^(-m+1) Sum[(-1)^((m-1)/2-k)*
	     Binomial[m,k] Sin[(m-2k)x], {k, 0, (m-1)/2}])^Sign[m1] ],
	Cos[x_]^(m1_Integer?OddQ) :>
	 With[{m=Abs[m1]},
	  (2^(-m+1) Sum[Binomial[m,k] Cos[(m-2k)x], {k, 0, (m-1)/2}])^Sign[m1] ]
}

trigArgumentRules = {
	Sin[x_ + y_] :> Sin[x] Cos[y] + Sin[y] Cos[x],
	Cos[x_ + y_] :> Cos[x] Cos[y] - Sin[x] Sin[y],
	Sin[n_Integer?Positive x_.] :>
	  Sum[ (-1)^((i-1)/2) Binomial[n, i] Cos[x]^(n-i) Sin[x]^i, {i, 1, n, 2} ],
	Cos[n_Integer?Positive x_.] :>
	  Sum[ (-1)^(i/2) Binomial[n, i] Cos[x]^(n-i) Sin[x]^i, {i, 0, n, 2} ]
}

SetAttributes[{TrigLinear, TrigArgument}, Listable]

TrigLinear[expr_] :=
	FixedPoint[ Function[e, Expand[e //. trigLinearRules]], expr ]

TrigArgument[expr_] :=
	Together[ FixedPoint[ Function[e, e //. trigArgumentRules], expr ] ]

End[]

Protect[ TrigLinear, TrigArgument ]

EndPackage[]
