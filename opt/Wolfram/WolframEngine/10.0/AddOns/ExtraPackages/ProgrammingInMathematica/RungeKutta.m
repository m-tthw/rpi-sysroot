(* :Title: RungeKutta *)


(* :Context: ProgrammingInMathematica`RungeKutta` *)

(* :Author: Roman E. Maeder *)

(* :Summary:
   a simple numerical integrator
 *)

(* :Copyright: © 1989-1996 by Roman E. Maeder *)

(* :Package Version: 2.0 *)

(* :Mathematica Version: 3.0 *)

(* :History:
   2.0 for Programming in Mathematica, 3rd ed.
   1.1 for Programming in Mathematica, 2nd ed.
   1.0 for Programming in Mathematica, 1st ed.
*)

(* :Keywords: Runge Kutta, numerical integration *)

(* :Sources:
   Roman E. Maeder. Programming in Mathematica, 3rd ed. Addison-Wesley, 1996.
*)

(* :Discussion:
   See Section 7.4 of "Programming in Mathematica"
*)

BeginPackage["ProgrammingInMathematica`RungeKutta`"]

RKSolve::usage = 
	"RKSolve[{e1,e2,..}, {y1,y2,..}, {a1,a2,..}, {t1, dt}]
	numerically integrates the ei as functions of the yi with inital values ai.
	The integration proceeds in steps of dt from 0 to t1.
	RKSolve[{e1,e2,..}, {y1,y2,..}, {a1,a2,..}, {t, t0, t1, dt}] integrates
	a time-dependent system from t0 to t1."

Begin["`Private`"]

RKStep[f_, y_, y0_, dt_] :=
	Module[{ k1, k2, k3, k4 },
		k1 = dt N[ f /. Thread[y -> y0] ];
		k2 = dt N[ f /. Thread[y -> y0 + k1/2] ];
		k3 = dt N[ f /. Thread[y -> y0 + k2/2] ];
		k4 = dt N[ f /. Thread[y -> y0 + k3] ];
		y0 + (k1 + 2 k2 + 2 k3 + k4)/6
	]

RKSolve[f_List, y_List, y0_List, {t1_, dt_}] :=
	NestList[ RKStep[f, y, #, N[dt]]&, N[y0], Round[N[t1/dt]] ] /;
	Length[f] == Length[y] == Length[y0]

RKSolve[f_List, y_List, y0_List, {t_, t0_, t1_, dt_}] :=
	Module[{res},
		res = RKSolve[ Append[f, 1], Append[y, t], Append[y0, t0], {t1 - t0, dt} ];
		Drop[#, -1]& /@ res
	] /; Length[f] == Length[y] == Length[y0]

End[]

Protect[ RKSolve ]

EndPackage[]
