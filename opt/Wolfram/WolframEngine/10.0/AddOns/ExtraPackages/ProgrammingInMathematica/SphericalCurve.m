(* :Title: SphericalCurve *)


(* :Context: ProgrammingInMathematica`SphericalCurve` *)

(* :Author: Roman E. Maeder *)

(* :Summary:
   parametric curves in space in spherical coordinates
 *)

(* :Copyright: © 1989-1996 by Roman E. Maeder *)

(* :Package Version: 2.0 *)

(* :Mathematica Version: 3.0 *)

(* :History:
   2.0 for Programming in Mathematica, 3rd ed.
   1.1 for Programming in Mathematica, 2nd ed.
   1.0 for Programming in Mathematica, 1st ed.
*)

(* :Keywords: spherical curves, parametric curves, sperical coordinates *)

(* :Sources:
   Roman E. Maeder. Programming in Mathematica, 3rd ed. Addison-Wesley, 1996.
*)

(* :Discussion:
   See Section 2.2 of "Programming in Mathematica"
*)

(* :Requirements:
   Graphics`ParametricPlot3D`
*)

BeginPackage["ProgrammingInMathematica`SphericalCurve`", "Graphics`ParametricPlot3D`"]

SphericalCurve::usage =
	"SphericalCurve[{r, theta, phi}, {u,u0,u1,(du)}, (options...)]
	plots a 3D parametric curve given in spherical coordinates."

Begin["`Private`"]

SphericalCurve[{r_, theta_, phi_}, ur_List, opts___] :=
	ParametricPlot3D[r {Sin[theta] Cos[phi], Sin[theta] Sin[phi], Cos[theta]}, ur, opts]

End[ ]

EndPackage[ ]
