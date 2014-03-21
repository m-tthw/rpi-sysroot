(* :Title: AlgExp *)


(* :Context: ProgrammingInMathematica`AlgExp` *)

(* :Author: Roman E. Maeder *)

(* :Summary:
   An example for a user-defined language of "algebraic expressions"
 *)

(* :Copyright: © 1989-1996 by Roman E. Maeder *)

(* :Package Version: 2.0 *)

(* :Mathematica Version: 3.0 *)

(* :History:
   2.0 for Programming in Mathematica, 3rd ed.
   1.1 for Programming in Mathematica, 2nd ed.
   1.0 for Programming in Mathematica, 1st ed.
*)

(* :Keywords: expressions, language, parser, recognizer *)

(* :Sources:
   Roman E. Maeder. Programming in Mathematica, 3rd ed. Addison-Wesley, 1996.
*)

(* :Discussion:
   See Section 6.5 of "Programming in Mathematica"
*)

BeginPackage["ProgrammingInMathematica`AlgExp`"]

AlgExpQ::usage = "AlgExpQ[expr] returns true if expr is an algebraic expression."

Begin["`Private`"]

SetAttributes[AlgExpQ, Listable]

AlgExpQ[ _Integer ]  = True
AlgExpQ[ _Rational ] = True
AlgExpQ[ c_Complex ] := AlgExpQ[Re[c]] && AlgExpQ[Im[c]]
AlgExpQ[ _Symbol ]   = True

AlgExpQ[ a_ + b_ ] := AlgExpQ[a] && AlgExpQ[b]
AlgExpQ[ a_ * b_ ] := AlgExpQ[a] && AlgExpQ[b]
AlgExpQ[ a_ ^ b_Integer ]  := AlgExpQ[a]
AlgExpQ[ a_ ^ b_Rational ] := AlgExpQ[a]

AlgExpQ[_] = False

End[]

EndPackage[]
