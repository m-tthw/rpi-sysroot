(* :Title: FoldRight *)


(* :Context: ProgrammingInMathematica`FoldRight` *)

(* :Author: Roman E. Maeder *)

(* :Summary:
   folding a binary operation to the right
 *)

(* :Copyright: © 1996 by Roman E. Maeder *)

(* :Package Version: 2.0 *)

(* :Mathematica Version: 3.0 *)

(* :History:
   2.0 for Programming in Mathematica, 3rd ed.
*)

(* :Sources:
   Roman E. Maeder. Programming in Mathematica, 3rd ed. Addison-Wesley, 1996.
*)

(* :Discussion:
   See Section 4.4 of "Programming in Mathematica"
*)

BeginPackage["ProgrammingInMathematica`FoldRight`"]

FoldRight::usage = "FoldRight[g, x, {y1, y2, ..., yn}] gives
	g[y1, g[y2, ..., g[yn, x]...]]."
FoldRightList::usage = "FoldRightList[g, x, {y1, y2, ..., yn}] gives
	{x, g[yn, x], ..., g[y2, ..., g[yn, x]...], g[y1, g[y2, ..., g[y1, x]...]]}."

FoldLeft = Fold          (* additional names for consistency *)
FoldLeftList = FoldList

Begin["`Private`"]

FoldRight[ g_, x0_, y_List ] :=
    Module[{x = x0},
        Do[ x = g[y[[i]], x], {i, Length[y], 1, -1} ];
        x
    ]

FoldRightList[ g_, x0_, y_List ] :=
    Module[{x = x0},
        Prepend[ Table[ x = g[y[[i]], x], {i, Length[y], 1, -1} ], x0 ]
    ]

End[ ]

Protect[ FoldRight, FoldRightList ]

EndPackage[ ]
