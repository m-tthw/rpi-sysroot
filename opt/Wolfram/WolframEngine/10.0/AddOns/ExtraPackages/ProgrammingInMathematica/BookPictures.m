(* :Title: BookPictures *)


(* :Context: ProgrammingInMathematica`BookPictures` *)

(* :Author: Roman E. Maeder *)

(* :Summary:
   code for the chapter-opener pictures in "Programming in Mathematica"
 *)

(* :Copyright: © 1989-1996 by Roman E. Maeder *)

(* :Package Version: 2.0 *)

(* :Mathematica Version: 3.0 *)

(* :History:
   2.0 for Programming in Mathematica, 3rd ed.
   1.1 for Programming in Mathematica, 2nd ed.
   1.0 for Programming in Mathematica, 1st ed.
*)

(* :Keywords: Möbius, minimal surface, parametric surface, sphere, Van der Pol,
    fractal, Fourier, spiral, Barnsley, fern, random walk, Sierpinski *)

(* :Sources:
   Roman E. Maeder. Programming in Mathematica, 3rd ed. Addison-Wesley, 1996.
*)

(* :Discussion:
   See Section 10.3 of "Programming in Mathematica"
*)

(* :Requirements:
   ProgrammingInMathematica/*.m
*)

Needs["Graphics`ParametricPlot3D`"]
Needs["Graphics`Shapes`"]
Needs["Graphics`ArgColors`"]
Needs["Graphics`Colors`"]

Needs["ProgrammingInMathematica`ComplexMap`"]
Needs["ProgrammingInMathematica`RungeKutta`"]
Needs["ProgrammingInMathematica`RandomWalk`"]

Needs["ProgrammingInMathematica`ChaosGame`"]

SetOptions[Graphics3D, PlotRange -> All]
SetOptions[ParametricPlot3D, PlotRange -> All]

(* Moebius transform *)

chapter1 := chapter1 =
PolarMap[ (2# - I)/(# - 1 + 0.1I)&, {0.001, 5}, {0, 2Pi},
          Frame -> True, Lines -> {20, 36}, PlotPoints -> 40 ]

(* Minimal surface *)

chapter2 := chapter2 =
ParametricPlot3D[{r*Cos[phi] - (r^2*Cos[2*phi])/2,
                  -(r*Sin[phi]) - (r^2*Sin[2*phi])/2,
                  (4*r^(3/2)*Cos[(3*phi)/2])/3},
                 {r, 0.0001, 1}, {phi, 0, 4Pi}, PlotPoints -> {8, 60}]

(* rotationally symmetric parametric surface *)

chapter3 := chapter3 =
ParametricPlot3D[
    {r Cos[Cos[r]] Cos[psi], r Cos[Cos[r]] Sin[psi], r Sin[Cos[r]]},
    {r, 0.001, 9Pi/2}, {psi, 0, 3Pi/2}, PlotPoints -> {72, 30}]

(* J. Waldvogel's Christmas picture *)

chapter4 := chapter4 =
  With[{c = Pi(1 + Sqrt[5])/2.0, x = Range[50000]},
      ListPlot[{Re[#], Im[#]}& /@ FoldList[Plus, 0, Exp[I c x^2]],
               PlotJoined -> True, AspectRatio -> Automatic, Axes -> None]
  ]

(* Sphere with random holes *)

chapter5 := chapter5 =
Show[ Graphics3D[Select[Sphere[1, 72, 54], Random[]>0.5&]] ]

(* Saddle surface *)

chapter6 := chapter6 =
CylindricalPlot3D[r^2 Cos[2 phi], {r, 0, 1/2, 1/20}, {phi, 0, 2Pi, 2Pi/36}]

(* Van-der-Pol equation *)

chapter7 := chapter7 =
Module[{vdp, eps = 1.5, x, xdot},
  vdp = RKSolve[{xdot, eps(1 - x^2)xdot - x}, {x, xdot}, #, {5Pi, 0.05}]&
        /@ {{0.1,0}, {-0.1,0}, {2,-2}, {-2,2}};
  vdp = ListPlot[#, PlotJoined -> True, DisplayFunction -> Identity]& /@ vdp;
  Show[ vdp, AspectRatio -> Automatic, DisplayFunction -> $DisplayFunction ]
]

(* Fractal tile *)

hexarotation = ArcSin[Sqrt[3/7]/2];
mids = Solve[x(x^6-1) == 0];
r7 = translation[{Re[#], Im[#]}]& /@ (x /. mids);
sr = Composition[ scale[1/Sqrt[7]], rotation[hexarotation] ];
rmaps = Composition[#, sr]& /@ r7;
hexatile = IFS[ N[rmaps] ]

chapter8 := chapter8 =
    Module[{pts, gr},
        pts = Flatten[ Nest[hexatile, Point[{0,0}], 6] ];
        gr = Graphics[{PointSize[0.0015], pts}];
        Show[gr, AspectRatio -> Automatic, PlotRange -> All]
    ]

(* Fourier approximations of saw-tooth *)

l5 = Table[ Sum[Sin[i x]/i, {i, n}], {n, 10} ];

chapter9 := chapter9 =
Plot[ Evaluate[l5], {x, -0.3, 2Pi+0.3} ]

(* spiral with varying radius *)

chapter10 := chapter10 =
ParametricPlot3D[{r (1 + phi/2) Cos[phi], r (1 + phi/2) Sin[phi], -phi/2},
	 	 {r, 0.1, 1.1, 0.125}, {phi, 0, 11Pi/2, Pi/16}]

(* diagonally shaded surface *)

chapter11 := chapter11 =
SphericalPlot3D[{Sin[theta],
                 FaceForm[GrayLevel[0.05 + 0.9 Sin[2theta + phi]^2],
                          GrayLevel[0.05 + 0.9 Sin[2theta - phi]^2]]},
                {theta, 0, Pi, Pi/48}, {phi, 0, 3Pi/2, Pi/24}, Lighting->False ]

(* Barnsley's Fern *)

bf1 = AffineMap[ -2.5 Degree, -2.5 Degree, 0.85, 0.85, 0, 1.6];
bf2 = AffineMap[ 49. Degree, 49. Degree, 0.3, 0.34, 0, 1.6];
bf3 = AffineMap[ 120. Degree, -50. Degree, 0.3, 0.37, 0, 0.44];
bf4 = AffineMap[ 0. Degree, 0. Degree, 0, 0.16, 0, 0];

fern = IFS[ {bf1, bf2, bf3, bf4}, Probabilities -> {0.73, 0.13, 0.11, 0.03} ]

chapter12 := chapter12 =
ChaosGame[fern, 50000, PlotStyle -> PointSize[0.0015]]

(* Random walk *)

appendixA := RandomWalk[5000]

(* Minimal Surface II *)

appendixB := appendixB =
ParametricPlot3D[
    {(r^2*Cos[2*phi])/2 - Log[r], -phi - (r^2*Sin[2*phi])/2, 2*r*Cos[phi]},
    {r, 0.0004, 2}, {phi, -2Pi, 3Pi}, PlotPoints -> {12, 100},
    ViewPoint->{-2.1, -1.1, 1.2} ]

(* Sierpinski sponge *)

(* Order: left, right, front, back, bottom, top *)

cheese[0, False, False, False, __ ] := {} (* small optimization *)

cheese[0, right_, front_, top_, x0_, y0_, z0_] :=
  With[{xs = x0+1, ys = y0+1, zs = z0+1},
    { If[right, Polygon[{{xs,y0,z0}, {xs,ys,z0}, {xs,ys,zs}, {xs,y0,zs}}], {}],
      If[front, Polygon[{{x0,y0,z0}, {xs,y0,z0}, {xs,y0,zs}, {x0,y0,zs}}], {}],
      If[top,   Polygon[{{x0,y0,zs}, {xs,y0,zs}, {xs,ys,zs}, {x0,ys,zs}}], {}]
    }]

cheese[n_, right_, front_, top_, x0_, y0_, z0_] :=
  With[{s = 3^(n-1), t = 2 3^(n-1), n1 = n-1},
   With[{xs = x0 + s, xt = x0 + t,
         ys = y0 + s, yt = y0 + t,
         zs = z0 + s, zt = z0 + t},
   { (* bottom layer *)
    cheese[n1, False, front, False, x0  , y0  , z0],
    cheese[n1, False, front, True , xs  , y0  , z0],
    cheese[n1, right, front, False, xt  , y0  , z0],
    cheese[n1, True , False, True , x0  , ys  , z0],
    cheese[n1, right, False, True , xt  , ys  , z0],
 (* cheese[n1, False, False, False, x0  , yt  , z0], invisible *)
    cheese[n1, False, True , True , xs  , yt  , z0],
    cheese[n1, right, False, False, xt  , yt  , z0],
     (* middle layer *)
    cheese[n1, True , front, False, x0  , y0  , zs],
    cheese[n1, right, front, False, xt  , y0  , zs],
    cheese[n1, True , True , False, x0  , yt  , zs],
    cheese[n1, right, True , False, xt  , yt  , zs],
     (* top layer *)
    cheese[n1, False, front, top  , x0  , y0  , zt],
    cheese[n1, False, front, top  , xs  , y0  , zt],
    cheese[n1, right, front, top  , xt  , y0  , zt],
    cheese[n1, True , False, top  , x0  , ys  , zt],
    cheese[n1, right, False, top  , xt  , ys  , zt],
    cheese[n1, False, False, top  , x0  , yt  , zt],
    cheese[n1, False, True , top  , xs  , yt  , zt],
    cheese[n1, right, False, top  , xt  , yt  , zt]
  }
 ]]

Sierpinski[n_Integer] :=
    Block[{polylist},
        polylist = {EdgeForm[],
        cheese[n, True, True, True, 0, 0, 0]};
        polylist = Flatten[ polylist ];
        Graphics3D[polylist, Boxed->False]
    ]   /; n >= 0

bookoptions :=
    Sequence[ViewPoint->{0.95, -3.1, 0.8},
        LightSources -> {{{1., 0., 5.}, RGBColor[1, 0, 0]}, 
                         {{1., 1., 1.}, RGBColor[0, 1, 0]}, 
                         {{0., 1., 0.4}, RGBColor[0, 0, 1]}, 
                         {{0., -1., 0.4}, RGBColor[0, 0, 1]}},
        Boxed->False, Background -> GrayLevel[0] ]

index := index =
Show[ Sierpinski[3], bookoptions ]

(* Cover *)

dia = 0.08;
exp = 0.4;
{u1, u2} = {0.001, 1};
{phi1, phi2} = 2 Pi{0, 2.43};
phi3 = phi2 + 2Pi/3;

res = 20;
thick = 0.0015

rs[u_, phi_] := dia u Exp[phi/(2Pi)]
hs[u_, phi_] := u (phi+1)^-exp

param[u_, phi_] :=
    { rs[u, phi] Cos[phi], rs[u, phi] Sin[phi], hs[u, phi] }

cover3 := cover3 =
    Module[{surf, lines, line, liner, phis, us, u, phi, max, maz, pr},
      surf = ParametricPlot3D[Evaluate[param[u, phi]],
               {u, u1, u2, 2(u2-u1)/res}, {phi, phi1, phi2, Pi/res},
               DisplayFunction -> Identity ];
      us = Range[-u2, -u1, 2(u2-u1)/res];
      lines = ParametricPlot3D[Evaluate[Function[u, param[u, phi]] /@ us],
               {phi, phi1, phi2, Pi/res},
               DisplayFunction -> Identity ];
      line = ParametricPlot3D[Evaluate[param[1.001u2, phi]],
               {phi, phi1, phi3, Pi/res},
               DisplayFunction -> Identity ];
      phis = Range[phi2, phi3, Pi/res];
      liner = ParametricPlot3D[Evaluate[Function[phi, param[u, phi]] /@ phis],
               {u, u1, 1.001u2, 2(1.001u2-u1)/res},
               DisplayFunction -> Identity ];
      max = rs[u2, phi3]; maz = hs[u2, phi1];
      pr = {-#,#}& /@ {max, max, maz};
      Show[ Graphics3D[{EdgeForm[], Thickness[thick],
                        surf[[1]], line[[1]], lines[[1]], liner[[1]]}],
            PlotRange -> pr, ViewPoint -> {1.7,-2,1.2},
            Boxed -> False ]
    ]

cover := cover = cover3 (* alias *)

(* cover of 2nd edition: exponentially shrinking torus *)

torus[ R_, r_, psi_, phi_, h_ ] :=
	{ (R + r Cos[psi]) Cos[phi],
	  (R + r Cos[psi]) Sin[phi],
	  r Sin[psi],
	  FaceForm[ ColorCircle[h], ColorCircle[h, 0.6] ]
	}

segment[ {phi0_, phi1_, dphi_}, {psi0_, psi1_, dpsi_} ] :=
	ParametricPlot3D[
		Evaluate[torus[1.2, Exp[-phi/(3Pi)], psi + phi/8, phi, psi-3Pi/4]],
		{phi, phi0, phi1, dphi}, {psi, psi0, psi1, dpsi},
		DisplayFunction -> Identity ]

cover2 := cover2 =
 Module[{glist, dphi = 2Pi/36, dpsi = 2Pi/32},
    glist = {segment[{-Pi/4,    0, dphi}, {Pi/2, 2Pi, dpsi}],
             segment[{  0,  3Pi/2, dphi}, {0, 2Pi, dpsi}],
             segment[{3Pi/2,  2Pi, dphi}, {1Pi/4, 7Pi/4, dpsi}],
             segment[{2Pi,  7Pi/2, dphi}, {0, 2Pi, 2dpsi}],
             segment[{7Pi/2,  4Pi, dphi}, {0, 6Pi/4, 2dpsi}],
             segment[{4Pi, 11Pi/2, dphi}, {0, 2Pi, 4dpsi}],
             segment[{11Pi/2, 6Pi, dphi}, {-Pi/4, 5Pi/4, 4dpsi}],
             segment[{6Pi, 17Pi/2, dphi/2}, {0, 2Pi, 4dpsi}]};
    Show[ glist, Boxed -> False, Lighting -> False, PlotRange -> All,
    	  DisplayFunction -> $DisplayFunction ] ]

(* cover of first edition: Maeder's shell *)

t0 = 0.001; t1 = N[ Pi - t0 ]
dt = (t1 - t0)/36; dp = N[ Pi/20 ]

part[t0_, phi0_, phi1_] := Block[{theta, phi},
	SphericalPlot3D[{Sin[theta] (2+Cos[phi/2]),
	                 FaceForm[ColorCircle[phi/2, 1], ColorCircle[phi/2, 0.7]]},
	                {theta, t0, t1, dt}, {phi, phi0, phi1, dp},
	                DisplayFunction -> Identity] ]

cover1 := cover1 =
Module[{glist},
    glist = {part[t0, 0, 3Pi/2],     part[Pi/2, 3Pi/2, 4Pi/2],
             part[t0, 4Pi/2, 7Pi/2], part[Pi/2, 7Pi/2, 8Pi/2]};
   Show[ glist, Boxed -> False, Lighting -> False,
         DisplayFunction -> $DisplayFunction ] ]
