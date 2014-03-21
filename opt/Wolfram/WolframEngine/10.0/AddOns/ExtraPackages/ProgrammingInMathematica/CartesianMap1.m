(* :Title: CartesianMap1 *)


(* :Context: ProgrammingInMathematica`CartesianMap1` *)

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
   See Section 1.1 of "Programming in Mathematica"
*)

CartesianMap[ func_, {x0_, x1_, dx_}, {y0_, y1_, dy_} ] :=
    Module[ {xy, x, y, hg, vg},
        xy = func[x + I y];
        hg = Curves[ xy, {x, x0, x1, dx}, {y, y0, y1} ];
        vg = Curves[ xy, {y, y0, y1, dy}, {x, x0, x1} ];
        Show[ Graphics[ Join[hg, vg] ],
              AspectRatio->Automatic, Axes->True ]
    ]

Curves[ xy_, spread_, bounds_ ] :=
    With[{curves = Table[{Re[xy], Im[xy]}, spread]},
        ParametricPlot[curves, bounds, DisplayFunction->Identity][[1]]
    ]
