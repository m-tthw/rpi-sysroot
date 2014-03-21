(* :Title: ComplexMap5 *)


(* :Context: ProgrammingInMathematica`ComplexMap5` *)

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
   See Section 1.5 of "Programming in Mathematica"
*)

(* :Requirements:
   Utilities/FilterOptions.m
*)

BeginPackage["ProgrammingInMathematica`ComplexMap`"]

CartesianMap::usage = "CartesianMap[f, {x0, x1, (dx)}, {y0, y1, (dy)}] plots
	the image of the cartesian coordinate lines under the function f.
	The default values of dx and dy are chosen so that the number of lines
	is equal to the value of the option Lines."

PolarMap::usage = "PolarMap[f, {r0:0, r1, (dr)}, {phi0, phi1, (dphi)}] plots
	the image of the polar coordinate lines under the function f.
	The default values of dr and dphi are chosen so that the number of lines
	is equal to the value of the option Lines."

Lines::usage = "Lines->{lx, ly} is an option of CartesianMap and PolarMap
	that gives the number of lines to draw."

Begin["`Private`"]

Needs["Utilities`FilterOptions`"]

Options[CartesianMap] = Options[PolarMap] = {Lines->15}

CartesianMap[ func_, {x0_, x1_, dx_:Automatic}, {y0_, y1_, dy_:Automatic},
              opts___?OptionQ ] :=
    Module[ {x, y}, Picture[ CartesianMap, func[x + I y],
                             {x, x0, x1, dx}, {y, y0, y1, dy}, opts ]
    ] /;
    NumericQ[x0] && NumericQ[x1] && NumericQ[y0] && NumericQ[y1] && 
    (NumericQ[dx] || dx === Automatic) && (NumericQ[dy] || dy === Automatic)

PolarMap[ func_, {r0_:0, r1_, dr_:Automatic}, {p0_, p1_, dp_:Automatic},
          opts___?OptionQ ] :=
    Module[ {r, p}, Picture[ PolarMap, func[r Exp[I p]],
                             {r, r0, r1, dr}, {p, p0, p1, dp}, opts ]
    ] /;
    NumericQ[r0] && NumericQ[r1] && NumericQ[p0] && NumericQ[p1] && 
    (NumericQ[dr] || dr === Automatic) && (NumericQ[dp] || dp === Automatic)

Picture[ cmd_, e_, {s_, s0_, s1_, ds_}, {t_, t0_, t1_, dt_}, opts___ ] :=
    Module[ {hg, vg, lines, nds = ds, ndt = dt},
        lines = Lines /. {opts} /. Options[cmd];
        If[ Head[lines] =!= List, lines = {lines, lines} ];
        If[ ds === Automatic, nds = N[(s1-s0)/(lines[[1]]-1)] ];
        If[ dt === Automatic, ndt = N[(t1-t0)/(lines[[2]]-1)] ];
        hg = Curves[ e, {s, s0, s1, nds}, {t, t0, t1}, opts ];
        vg = Curves[ e, {t, t0, t1, ndt}, {s, s0, s1}, opts ];
        Show[ Graphics[ Join[hg, vg] ],
              FilterOptions[Graphics, opts],
              AspectRatio -> Automatic, Axes -> True ]
    ]

Curves[ xy_, spread_, bounds_, opts___ ] :=
    With[{curves = Table[{Re[xy], Im[xy]}, spread]},
        ParametricPlot[ curves, bounds, DisplayFunction -> Identity,
                        Evaluate[FilterOptions[ParametricPlot, opts]]
        ][[1]]
    ]

End[ ]

EndPackage[ ]
