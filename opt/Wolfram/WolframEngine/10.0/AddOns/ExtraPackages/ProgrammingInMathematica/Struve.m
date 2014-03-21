(* :Title: Struve *)


(* :Context: ProgrammingInMathematica`Struve` *)

(* :Author: Roman E. Maeder *)

(* :Summary:
   Definitions for the Struve functions
 *)

(* :Copyright: © 1989-1996 by Roman E. Maeder *)

(* :Package Version: 2.0 *)

(* :Mathematica Version: 3.0 *)

(* :History:
   2.0 for Programming in Mathematica, 3rd ed.
   1.1 for Programming in Mathematica, 2nd ed.
   1.0 for Programming in Mathematica, 1st ed.
*)

(* :Keywords: Struve *)

(* :Sources:
   Roman E. Maeder. Programming in Mathematica, 3rd ed. Addison-Wesley, 1996.
*)

(* :Discussion:
   See Section 8.4 of "Programming in Mathematica"
*)

BeginPackage["ProgrammingInMathematica`Struve`"]

StruveH::usage = "StruveH[nu, z] gives the Struve function."

Begin["`Private`"]

SetAttributes[ StruveH, {NumericFunction, Listable} ]

(* special values *)

StruveH[r_Rational?Positive, z_] /; Denominator[r] == 2 :=
	BesselY[r, z] +
	Sum[Gamma[m + 1/2] (z/2)^(-2m + r - 1)/Gamma[r + 1/2 - m], {m, 0, r-1/2}]/Pi

StruveH[r_Rational?Negative, z_] /; Denominator[r] == 2 :=
	(-1)^(-r-1/2) BesselJ[-r, z]

(* Series expansion *)

StruveH/: Series[StruveH[nu_?NumberQ, z_], {z_, 0, ord_Integer}] :=
	(z/2)^(nu + 1) Sum[ (-1)^m (z/2)^(2m)/Gamma[m + 3/2]/Gamma[m + nu + 3/2],
	                   {m, 0, (ord-nu-1)/2} ] + O[z]^(ord+1)

(* numerical evaluation *)

StruveH[_, 0] := 0

StruveH[nu_?NumericQ, z_?NumericQ] /; Precision[{nu, z}] < Infinity :=
	Module[{s = 0, so = -1, z2 = -(z/2)^2, k1 = 3/2, k2 = nu + 3/2, g1, g2, zf},
		zf = (z/2)^(nu+1); g1 = Gamma[k1]; g2 = Gamma[k2];
		While[so != s,
			so = s; s += zf/g1/g2;
			g1 *= k1; g2 *= k2; zf *= z2; k1++; k2++
		]; s
	]

(* derivatives *)

StruveH/: Derivative[0, n_Integer?Positive][StruveH] :=
  Function[{nu, z},
    D[ (StruveH[nu-1, z] - StruveH[nu+1, z] + (z/2)^nu/Sqrt[Pi]/Gamma[nu + 3/2])/2,
       {z, n-1} ]
  ]

(* interpretation and formatting for traditional form *)

StruveH/:
MakeBoxes[StruveH[nu_, z_], form:TraditionalForm] :=
    RowBox[{SubscriptBox["H", MakeBoxes[nu, form]], "(", MakeBoxes[z, form], ")"}]

MakeExpression[ RowBox[{SubscriptBox["H", nu_], "(", z_, ")"}],
                form:TraditionalForm ] :=
  MakeExpression[ RowBox[{"StruveH", "[", RowBox[{nu, ",", z}], "]"}], form ]

End[]

Protect[StruveH]

EndPackage[]
