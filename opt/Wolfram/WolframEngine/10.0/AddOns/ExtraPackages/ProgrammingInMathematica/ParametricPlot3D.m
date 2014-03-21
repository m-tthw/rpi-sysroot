(* :Title: ParametricPlot3D *)


(* :Context: Graphics`ParametricPlot3D` *)

(* :Author: Roman E. Maeder *)

(* :Summary:
   a replacement for the standard package ParametricPlot3D
 *)

(* :Copyright: © 1989-1996 by Roman E. Maeder *)

(* :Package Version: 2.0 *)

(* :Mathematica Version: 3.0 *)

(* :History:
   2.0 for Programming in Mathematica, 3rd ed.
   1.1 for Programming in Mathematica, 2nd ed.
   1.0 for Programming in Mathematica, 1st ed.
*)

(* :Keywords: parametric plotting, functions, curves, surfaces *)

(* :Sources:
   Roman E. Maeder. Programming in Mathematica, 3rd ed. Addison-Wesley, 1996.
*)

(* :Discussion:
   See Section 10.1 of "Programming in Mathematica"
*)

BeginPackage["Graphics`ParametricPlot3D`"]

ParametricPlot3D::usage = ParametricPlot3D::usage <> 
	" ParametricPlot3D[{x,y,z,(style)}, {u,u0,u1,du}, ({v,v0,v1,dv})]
	uses increments du and dv instead of the PlotPoints option."

PointParametricPlot3D::usage =
	"PointParametricPlot3D[{x,y,z}, {u,u0,u1,(du)}, (options)]
	plots a one-parameter set of points in space.
	PointParametricPlot3D[{x,y,z}, {u,u0,u1,(du)}, {v,v0,v1,(dv)}, (options)]
	plots a two-parameter set of points in space. Options are passed to Show[]."

SphericalPlot3D::usage = "SphericalPlot3D[r, {theta, thetamin, thetamax}, {phi, phimin, phimax}, (options)]
	plots r as a function of the angles theta and phi.
	SphericalPlot3D[{r, style}, ...] uses style to render each surface patch."

CylindricalPlot3D::usage = "CylindricalPlot3D[z, {r, rmin, rmax}, {phi, phimin, phimax}, (options)]
	plots z as a function of r and phi.
	CylindricalPlot3D[{z, style},  ...] uses style to render each surface patch."

Begin["`Private`"]

protected = Unprotect[ParametricPlot3D]

Needs["Utilities`FilterOptions`"]

(* overload ParametricPlot3D to allow increments in iterators *)

ParametricPlot3D[ fun_, {u_, u0_, u1_, du_:Automatic}, {v_, v0_, v1_, dv_:Automatic}, opts___?OptionQ ] :=
	Module[{plotpoints},
		plotpoints = PlotPoints /. {opts} /. Options[ParametricPlot3D];
		If[ Head[plotpoints] =!= List, plotpoints = {plotpoints, plotpoints} ];
		If[ du =!= Automatic, plotpoints[[1]] = Round[(u1-u0)/du] + 1 ];
		If[ dv =!= Automatic, plotpoints[[2]] = Round[(v1-v0)/dv] + 1 ];
		ParametricPlot3D[ fun, {u, u0, u1}, {v, v0, v1}, PlotPoints -> plotpoints, opts]
	]  /; du =!= Automatic || dv =!= Automatic

ParametricPlot3D[ fun_, {u_, u0_, u1_, du_}, opts___?OptionQ ] :=
	ParametricPlot3D[ fun, {u, u0, u1}, PlotPoints -> Round[(u1-u0)/du] + 1, opts]


Attributes[PointParametricPlot3D] = {HoldFirst}

PointParametricPlot3D[ fun_, {u_, u0_?NumericQ, u1_?NumericQ, du_:Automatic},
		{v_, v0_?NumericQ, v1_?NumericQ, dv_:Automatic}, opts___?OptionQ ] :=
	Module[{plotpoints, ndu = N[du], ndv = N[dv]},
		plotpoints = PlotPoints /. {opts} /. Options[ParametricPlot3D];
		If[ plotpoints === Automatic, plotpoints = 15];
		If[ Head[plotpoints] =!= List, plotpoints = {plotpoints, plotpoints} ];
		If[ du === Automatic, ndu = N[(u1-u0)/(plotpoints[[1]]-1)] ];
		If[ dv === Automatic, ndv = N[(v1-v0)/(plotpoints[[2]]-1)] ];
		Show[ Graphics3D[Table[ Point[N[fun]], {u, u0, u1, ndu}, {v, v0, v1, ndv} ]],
		      FilterOptions[Graphics3D, opts] ]
	]

(* point space curve *)

PointParametricPlot3D[ fun_, ul:{_, u0_?NumericQ, u1_?NumericQ, du_?NumericQ}, opts___?OptionQ ] :=
	Show[ Graphics3D[Table[ Point[N[fun]], ul ]], FilterOptions[Graphics3D, opts] ]

PointParametricPlot3D[ fun_, {u_, u0_, u1_}, opts___?OptionQ ] :=
    Module[{plotpoints},
    	plotpoints = PlotPoints /. {opts} /. Options[ParametricPlot3D];
    	If[ Head[plotpoints] == List, plotpoints = plotpoints[[1]] ];
	PointParametricPlot3D[ fun, {u, u0, u1, (u1-u0)/(plotpoints-1)}, opts ]
    ]


Attributes[SphericalPlot3D] = {HoldFirst}

SphericalPlot3D[ {r_, style_}, tlist:{theta_, __}, plist:{phi_, __}, opts___?OptionQ ] :=
	Module[{rs},
		ParametricPlot3D[ {(rs = r) Sin[theta] Cos[phi],
		                   rs Sin[theta] Sin[phi],
		                   rs Cos[theta],
		                   style},
		                  tlist, plist, opts ]
	]

SphericalPlot3D[ r_, tlist:{theta_, __}, plist:{phi_, __}, opts___?OptionQ ] :=
      ParametricPlot3D[ r{Sin[theta] Cos[phi],
                          Sin[theta] Sin[phi],
                          Cos[theta]},
                        tlist, plist, opts ]


Attributes[CylindricalPlot3D] = {HoldFirst}

CylindricalPlot3D[ {z_, style_}, rlist:{r_, __}, plist:{phi_, __}, opts___?OptionQ ] :=
	ParametricPlot3D[{r Cos[phi], r Sin[phi], z, style}, rlist, plist, opts]

CylindricalPlot3D[ z_, rlist:{r_, __}, plist:{phi_, __}, opts___?OptionQ ] :=
	ParametricPlot3D[{r Cos[phi], r Sin[phi], z}, rlist, plist, opts]

Protect[ Evaluate[protected] ]

End[]

Protect[PointParametricPlot3D, SphericalPlot3D, CylindricalPlot3D]

EndPackage[]
