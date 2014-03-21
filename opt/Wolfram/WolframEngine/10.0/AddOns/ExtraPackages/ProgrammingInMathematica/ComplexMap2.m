(* :Title: ComplexMap2 *)


(* :Context: ProgrammingInMathematica`ComplexMap2` *)

(* :Author: Roman E. Maeder *)

(* :Summary:
   a prelimiary version of the ComplexMap package
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

(* :Limitations:
   this is a preliminary version, for educational purposes only.
   The final code is in ProgrammingInMathematica/ComplexMap.m
*)

(* :Discussion:
   See Section 1.4 of "Programming in Mathematica"
*)

BeginPackage["ProgrammingInMathematica`ComplexMap`"]

CartesianMap::usage = "CartesianMap[f, {x0, x1, (dx)}, {y0, y1, (dy)}, opts..]
	plots the image of the cartesian coordinate lines under the function f.
	The default values of dx and dy are chosen so that the number of lines
	is equal to the value of the option Lines."

PolarMap::usage = "PolarMap[f, {r0:0, r1, (dr)}, {phi0, phi1, (dphi)}, opts..]
	plots the image of the polar coordinate lines under the function f.
	The default values of dr and dphi are chosen so that the number of lines
	is equal to the value of the option Lines."

Lines::usage = "Lines->{lx, ly} is an option of CartesianMap and PolarMap
	that gives the number of lines to draw."

Begin["`Private`"]

Options[CartesianMap] = {Lines->15}
Options[PolarMap] = {Lines->15}

(* explicit increments *)

CartesianMap[ func_, {x0_, x1_, dx_}, {y0_, y1_, dy_}, opts___ ] :=
    Module[ {x, y},
        Picture[ func[x + I y], {x, x0, x1, dx}, {y, y0, y1, dy} ]
    ]

PolarMap[ func_, {r0_, r1_, dr_}, {p0_, p1_, dp_}, opts___ ] :=
    Module[ {r, p},
        Picture[ func[r Exp[I p]], {r, r0, r1, dr}, {p, p0, p1, dp} ]
    ]

(* default increments *)

CartesianMap[ func_, {x0_, x1_}, {y0_, y1_}, opts___ ] :=
    Module[ {lines, dx, dy},
        lines = Lines /. {opts} /. Options[CartesianMap];
        If[ Head[lines] =!= List, lines = {lines, lines} ];
        dx = N[(x1 - x0)/(lines[[1]]-1)];
        dy = N[(y1 - y0)/(lines[[2]]-1)];
        CartesianMap[ func, {x0, x1, dx}, {y0, y1, dy}, opts ]
    ]

PolarMap[ func_, {r0_, r1_}, {p0_, p1_}, opts___ ] :=
    Module[ {lines, dr, dp},
        lines = Lines /. {opts} /. Options[PolarMap];
        If[ Head[lines] =!= List, lines = {lines, lines} ];
        dr = N[(r1 - r0)/(lines[[1]]-1)];
        dp = N[(p1 - p0)/(lines[[2]]-1)];
        PolarMap[ func, {r0, r1, dr}, {p0, p1, dp}, opts ]
    ]

Picture[ e_, {s_, s0_, s1_, ds_}, {t_, t0_, t1_, dt_} ] :=
    Module[ {hg, vg},
        hg = Curves[ e, {s, s0, s1, ds}, {t, t0, t1} ];
        vg = Curves[ e, {t, t0, t1, dt}, {s, s0, s1} ];
        Show[ Graphics[ Join[hg, vg] ],
              AspectRatio->Automatic, Axes->True ]
    ]

Curves[ xy_, spread_, bounds_ ] :=
    With[{curves = Table[{Re[xy], Im[xy]}, spread]},
        ParametricPlot[curves, bounds, DisplayFunction->Identity][[1]]
    ]

End[ ]

EndPackage[ ]
