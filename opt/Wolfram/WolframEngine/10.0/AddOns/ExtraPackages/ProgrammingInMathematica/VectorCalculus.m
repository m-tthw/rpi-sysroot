(* :Title: VectorCalculus *)


(* :Context: ProgrammingInMathematica`VectorCalculus` *)

(* :Author: Roman E. Maeder *)

(* :Summary:
   simple vector calculus: divergence, gradient, Jacobian, Laplacian
   in Cartesian coordinates
 *)

(* :Copyright: © 1989-1996 by Roman E. Maeder *)

(* :Package Version: 2.0 *)

(* :Mathematica Version: 3.0 *)

(* :History:
   2.0 for Programming in Mathematica, 3rd ed.
   1.1 for Programming in Mathematica, 2nd ed.
   1.0 for Programming in Mathematica, 1st ed.
*)

(* :Keywords: Jacobi, Laplace, Descartes *)

(* :Sources:
   Roman E. Maeder. Programming in Mathematica, 3rd ed. Addison-Wesley, 1996.
*)

(* :Discussion:
   See Section 4.7 of "Programming in Mathematica"
*)

BeginPackage["ProgrammingInMathematica`VectorCalculus`"]

Div::usage = "Div[v, varlist] computes the divergence of
	the vectorfield v w.r.t. the given variables in Cartesian coordinates."

Grad::usage = "Grad[s, varlist] computes the gradient of s
	w.r.t. the given variables in Cartesian coordinates."

Laplacian::usage = "Laplacian[s, varlist] computes the Laplacian of
	the scalar field s w.r.t. the given variables in Cartesian coordinates."

JacobianMatrix::usage = "JacobianMatrix[flist, varlist] computes the Jacobian of
	the functions flist w.r.t. the given variables."

Begin["`Private`"]

Div[v_List, var_List] := Inner[ D, v, var, Plus ]

Grad[s_, var_List] := D[s, #]& /@ var

Laplacian[s_, var_List] := Div[ Grad[s, var], var ]

JacobianMatrix[f_List, var_List] := Outer[ D, f, var ]

End[ ]

Protect[ Div, Laplacian, Grad, JacobianMatrix ]

EndPackage[ ]
