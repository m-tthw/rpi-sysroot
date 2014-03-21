(* :Title: Atoms *)


(* :Context: ProgrammingInMathematica`Atoms` *)

(* :Author: Roman E. Maeder *)

(* :Summary:
   conversion between symbols and expression in a Lisp-like fashion
 *)

(* :Copyright: © 1989-1996 by Roman E. Maeder *)

(* :Package Version: 2.0 *)

(* :Mathematica Version: 3.0 *)

(* :History:
   2.0 for Programming in Mathematica, 3rd ed.
   1.1 for Programming in Mathematica, 2nd ed.
   1.0 for Programming in Mathematica, 1st ed.
*)

(* :Keywords: Lisp, atom, intern, explode *)

(* :Sources:
   Roman E. Maeder. Programming in Mathematica, 3rd ed. Addison-Wesley, 1996.
*)

(* :Discussion:
   See Section 6.5 of "Programming in Mathematica"
*)

BeginPackage["ProgrammingInMathematica`Atoms`"]

Explode::usage = "Explode[expr] turns an expression into
	a list of characters that make up its name."

Intern::usage = "Intern[charlist] turns a list of characters into an expression."

Begin["`Private`"]

Explode[ atom_ ] := Characters[ ToString[InputForm[atom]] ]

Intern[l:{_String..}] := ToExpression[ StringJoin @@ l ]

Protect[Explode, Intern]

End[]

EndPackage[]
