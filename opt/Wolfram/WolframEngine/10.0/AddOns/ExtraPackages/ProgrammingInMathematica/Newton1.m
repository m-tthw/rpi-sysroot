(* :Title: Newton1 *)


(* :Context: ProgrammingInMathematica`Newton1` *)

(* :Author: Roman E. Maeder *)

(* :Summary:
   a preliminary version of Newton.m
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
   See Section 4.4 of "Programming in Mathematica"
*)

BeginPackage["ProgrammingInMathematica`Newton`"]

NewtonZero::usage = "NewtonZero[f, x0] finds a zero of the function f using
	the initial guess x0 to start the iteration. NewtonZero[expr, x, x0]
	finds a zero of expr as a function of x. The recursion limit
	determines the maximum number of iteration steps that are performed."

NewtonFixedPoint::usage = "NewtonFixedPoint[f, x0] finds a fixed point of the
	function f using the initial guess x0 to start the iteration.
	NewtonFixedPoint[expr, x, x0] finds a fixed point of expr as a function of x."

Options[NewtonZero] = Options[NewtonFixedPoint] = {
	MaxIterations :> $RecursionLimit
}

Newton::noconv = "Iteration did not converge in `1` steps."

Begin["`Private`"]

extraPrecision = 10 (* the extra working precision *)

NewtonZero[ f_, x0_, opts___?OptionQ ] :=
    Module[{res, maxiter},
      maxiter = MaxIterations /. {opts} /. Options[NewtonZero];
      With[{prec = Precision[x0], fp = f'},
        Block[{$MaxPrecision = prec + extraPrecision},
            res = FixedPoint[(# - f[#]/fp[#])&, x0, maxiter] ];
        If [ !TrueQ[Abs[f[res]] <= 10^-prec],
             Message[Newton::noconv, maxiter] ];
        res
    ] ]

NewtonZero[ expr_, x_, x0_, opts___?OptionQ ] :=
	NewtonZero[ Function[x, expr], x0, opts ]

NewtonFixedPoint[ f_, x0_, opts___?OptionQ ] :=
    Module[{maxiter},
        maxiter = MaxIterations /. {opts} /. Options[NewtonFixedPoint];
        NewtonZero[(f[#] - #)&, x0, MaxIterations -> maxiter]
    ]

NewtonFixedPoint[ expr_, x_, x0_, opts___?OptionQ ] :=
	NewtonFixedPoint[ Function[x, expr], x0, opts ]

End[ ]

Protect[ NewtonZero, NewtonFixedPoint ]

EndPackage[ ]
