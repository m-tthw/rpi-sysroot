(* :Title: AffineMaps *)


(* :Context: ProgrammingInMathematica`AffineMaps` *)

(* :Author: Roman E. Maeder *)

(* :Summary:
   Transformation of graphic objects with planar affine maps
 *)

(* :Copyright: © 1996 by Roman E. Maeder *)

(* :Package Version: 2.0 *)

(* :Mathematica Version: 3.0 *)

(* :History:
   2.0 for Programming in Mathematica, 3rd ed.
*)

(* :Keywords: affine, map, chaos, transformations *)

(* :Sources:
   Roman E. Maeder. Programming in Mathematica, 3rd ed. Addison-Wesley, 1996.
*)

(* :Discussion:
   See Section 12.1 of "Programming in Mathematica"
*)

BeginPackage["ProgrammingInMathematica`AffineMaps`"]

AffineMap::usage = "AffineMap[\[Phi], \[Psi], r, s, e, f] generates an affine map with
	rotation angles \[Phi], \[Psi], scale factors r, s, and translation
	components e, f. AffineMap[{x, y}, {fxy, gxy}] generates an affine map
	with the two components given as expressions in x and y.
	AffineMap[matrix] uses the 2x3 matrix for the affine map."

map::usage = "-map- represents an affine map."

rotation::usage = "rotation[\[Alpha]] generates a rotation by \[Alpha]."
scale::usage = "scale[s, t] generates a scaling map with factors s and t.
	scale[r] scales both coordinates by r."
translation::usage = "translation[{x, y}] generates a translation by
	the vector {x, y}."

AverageContraction::usage = "AverageContraction[map] gives the average
	area contraction factor (the determinant) of an affine map."

$CirclePoints::usage = "$CirclePoints is the number of vertices of the
	polygon approximating the affine image of a circle."

$CirclePoints = 24

Begin["`Private`"]

(* affine map datatype *)

Format[m_map] := "-map-"

(* Terminology of Peitgen/Jürgens/Saupe *)

AffineMap[phi_, psi_, r_, s_, e_, f_] :=
	map[{{r Cos[phi], -s Sin[psi], e},
	     {r Sin[phi],  s Cos[psi], f}}]

(* as expressions. Does not test for affinity *)

AffineMap[params:{_Symbol, _Symbol}, expr:{_, _}] := map[ Function[params, expr] ]

(* matrix directly *)

AffineMap[ mat_?MatrixQ ] /; Dimensions[mat] == {2, 3} := map[ mat ]

(* apply to points *)

map[mat_?MatrixQ][{x_, y_}] := mat . {x, y, 1}
map[f_Function][{x_, y_}] := f[x, y]

(* simplify composition *)

map/: Composition[map[mat1_?MatrixQ], map[mat2_?MatrixQ]] :=
	map[ mat1 . Append[mat2, {0,0,1}] ]
map/: Composition[map[f_Function], map[g_Function]] :=
    Module[{x, y}, AffineMap[{x, y}, f @@ g[x, y]] ]

(* properties *)

AverageContraction[map[mat_?MatrixQ]] := Abs[Det[ Drop[#, -1]& /@ mat ]]
AverageContraction[map[f_Function]] :=
    Module[{x, y}, Abs[Det[ Outer[D, f[x, y], {x, y}] ]] ]

(* Graphic objects *)

(m_map)[Point[xy_]] := Point[m[xy]]

(m_map)[Line[points_]] := Line[m /@ points]

(m_map)[Polygon[points_]] := Polygon[m /@ points]

(* rectangles: convert to polygon *)

(m_map)[Rectangle[{xmin_, ymin_}, {xmax_, ymax_}]] :=
    m[Polygon[{{xmin, ymin}, {xmax, ymin}, {xmax, ymax}, {xmin, ymax}}]]

(* Circles/Ellipses: convert to lines/polygons *)

(m_map)[Circle[xy_, {rx_, ry_}]] :=
  With[{dp = N[2Pi/$CirclePoints]},
    m[ Line[ Table[xy + {rx Cos[phi], ry Sin[phi]},
                {phi, 0, 2Pi, dp}] ]
  ] ]

(m_map)[ Circle[xy_, r_] ] := m[ Circle[xy, {r, r}] ]

(m_map)[Disk[xy_, {rx_, ry_}]] :=
  With[{dp = N[2Pi/$CirclePoints]},
    m[ Polygon[ Table[xy + {rx Cos[phi], ry Sin[phi]},
                {phi, 0, 2Pi-dp, dp}] ]
  ] ]

(m_map)[ Disk[xy_, r_] ] := m[ Disk[xy, {r, r}] ]

(m_map)[ (Circle|Disk)[xy_, r_, args__] ] := Sequence[] (* not implemented *)

(* text: transform location *)

(m_map)[Text[text_, pos:{_, _}, args___]] := Text[text, m[pos], args]

(* not implemented: circular arcs, Raster, RasterArray,
   scaled coordinates, scaling of text *)

(* directives *)

(m_map)[(h:PointSize|AbsolutePointSize|Thickness|AbsoluteThickness)[r_]] :=
	h[r Sqrt[AverageContraction[m]]]

(* Graphics *)

(m_map)[Graphics[objs_List, opts___]] :=
	Graphics[Function[g, m[g], Listable] /@ objs, opts]

(* catchall *)

(m_map)[unknown_] := unknown

(* generators *)

rotation[alpha_] := AffineMap[alpha, alpha, 1, 1, 0, 0]

scale[s_, t_] := AffineMap[0, 0, s, t, 0, 0]
scale[r_] := scale[r, r]

translation[{x_, y_}] := AffineMap[0, 0, 1, 1, x, y]

End[ ]

Protect[ AffineMap, rotation, scale, translation, AverageContraction ]

EndPackage[ ]
