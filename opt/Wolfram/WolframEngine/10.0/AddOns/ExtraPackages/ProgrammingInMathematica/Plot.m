(* :Title: Plot *)


(* :Context: ProgrammingInMathematica`Plot` *)

(* :Author: Roman E. Maeder *)

(* :Summary:
   an extension of the built-in Plot functions to use different plot styles
   for plotting several functions
 *)

(* :Copyright: © 1996 by Roman E. Maeder *)

(* :Package Version: 2.0 *)

(* :Mathematica Version: 3.0 *)

(* :History:
   2.0 for Programming in Mathematica, 3rd ed.
*)

(* :Keywords: Plot, styles, automatic *)

(* :Sources:
   Roman E. Maeder. Programming in Mathematica, 3rd ed. Addison-Wesley, 1996.
*)

(* :Discussion:
   See Section 8.3 of "Programming in Mathematica"
*)

BeginPackage["ProgrammingInMathematica`Plot`"]

Plot::usage = Plot::usage <> " If several functions are plotted, different
	plot styles are chosen automatically."

Begin["`Private`"]

protected = Unprotect[Plot]

$PlotActive = True

Plot[f_List, args___]/; $PlotActive :=
    Block[{$PlotActive = False},
      With[{styles = NestList[nextStyle, firstStyle, Length[Unevaluated[f]]-1]},
        Plot[f, args, PlotStyle -> styles]
      ]
    ]

(* style definitions *)

unit = 1/100
max = 5

firstStyle = Dashing[{}]

nextStyle[Dashing[{alpha___, x_, y_, omega___}]] /; x > y + unit :=
	Dashing[{alpha, x, y + unit, omega}]
nextStyle[Dashing[l_List]] :=
	Dashing[Prepend[Table[unit, {Length[l] + 1}], max unit]]

Protect[ Evaluate[protected] ]

End[]

EndPackage[]
