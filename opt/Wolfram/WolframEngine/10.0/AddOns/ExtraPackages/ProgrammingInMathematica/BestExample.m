(* :Title: BestExample *)


(* :Context: ProgrammingInMathematica`BestExample` *)

(* :Author: Roman E. Maeder *)

(* :Summary:
   an example of good programming style
 *)

(* :Copyright: © 1989-1996 by Roman E. Maeder *)

(* :Package Version: 2.0 *)

(* :Mathematica Version: 3.0 *)

(* :History:
   2.0 for Programming in Mathematica, 3rd ed.
   1.1 for Programming in Mathematica, 2nd ed.
   1.0 for Programming in Mathematica, 1st ed.
*)

(* :Keywords: programming style, local variables *)

(* :Sources:
   Roman E. Maeder. Programming in Mathematica, 3rd ed. Addison-Wesley, 1996.
*)

(* :Discussion:
   See Section 1.2 of "Programming in Mathematica"
*)

PowerSum::usage = "PowerSum[x, n] returns the sum of the first n powers of x."

Begin["Private`"]

PowerSum[x_, n_] :=
    Module[{i},
    	Sum[ x^i, {i, 1, n} ]
    ]

End[]
