(* :Title: Newton *)


(* :Context: ProgrammingInMathematica`Newton` *)

(* :Author: Roman E. Maeder *)

(* :Summary:
   Newton iteration for zeroes and fixed points of functions
 *)

(* :Copyright: © 1989-1996 by Roman E. Maeder *)

(* :Package Version: 2.0 *)

(* :Mathematica Version: 3.0 *)

(* :History:
   2.0 for Programming in Mathematica, 3rd ed.
   1.1 for Programming in Mathematica, 2nd ed.
   1.0 for Programming in Mathematica, 1st ed.
*)

(* :Keywords: Newton iteration, root of function, fixed point *)

(* :Sources:
   Roman E. Maeder. Programming in Mathematica, 3rd ed. Addison-Wesley, 1996.
*)

(* :Discussion:
   See Section 7.2 of "Programming in Mathematica"
*)

BeginPackage["ProgrammingInMathematica`Newton`"]

NewtonZero::usage = "NewtonZero[f, x0] finds a zero of the function f using
	the initial guess x0 to start the iteration. NewtonZero[expr, x, x0]
	finds a zero of expr as a function of x. The recursion limit
	determines the maximum number of iteration steps that are performed."

NewtonFixedPoint::usage = "NewtonFixedPoint[f, x0] finds a fixed point
	of the function f using the initial guess x0 to start the iteration.
	NewtonFixedPoint[expr, x, x0] finds a fixed point of expr as a
	function of x."

Options[NewtonZero] = Options[NewtonFixedPoint] = {
	MaxIterations :> $RecursionLimit,
	AccuracyGoal -> Automatic,
	WorkingPrecision -> Automatic
}

Newton::noconv = "Iteration did not converge in `1` steps."

Begin["`Private`"]

extraPrecision = 10 (* the extra working precision *)

NewtonZero[ f_, x0_, opts___?OptionQ ] :=
    Module[{res, maxiter, accugoal, workprec, x = x0},
      {maxiter, accugoal, workprec} =
          {MaxIterations, AccuracyGoal, WorkingPrecision} /.
              Flatten[{opts}] /. Options[NewtonZero];
      With[{fp = f'},
        If[ accugoal === Automatic,
            accugoal = Max[Precision[x0], $MachinePrecision] ];
        If[ accugoal === Infinity, (* exact *)
            res = FixedPoint[(# - f[#]/fp[#])&, x, maxiter];
            If [ !TrueQ[f[res] === 0], Message[Newton::noconv, maxiter] ];
          , (* else approximate *)
            If[ workprec === Automatic, workprec = accugoal;
              If[ accugoal > $MachinePrecision, workprec += extraPrecision ];
            ];
            x = SetPrecision[x, workprec];
            If[x === 0, x = SetAccuracy[x, workprec]];
            Block[{$MaxPrecision = workprec},
              res = FixedPoint[(# - f[#]/fp[#])&, x, maxiter] ];
            If [ !TrueQ[Abs[f[res]] <= 10.0^-accugoal],
                 Message[Newton::noconv, maxiter] ];
        ];
        res
    ] ]

NewtonZero[ expr_, x_, x0_, opts___?OptionQ ] :=
	NewtonZero[ Function[x, expr], x0, opts ]

optnames = First /@ Options[ NewtonFixedPoint ]

NewtonFixedPoint[ f_, x0_, opts___?OptionQ ] :=
    Module[{optvals},
        optvals = optnames /. Flatten[{opts}] /. Options[NewtonFixedPoint];
        NewtonZero[ (f[#] - #)&, x0, Thread[optnames -> optvals]]
    ]

NewtonFixedPoint[ expr_, x_, x0_, opts___?OptionQ ] :=
	NewtonFixedPoint[ Function[x, expr], x0, opts ]

End[ ]

Protect[ NewtonZero, NewtonFixedPoint ]

EndPackage[ ]
