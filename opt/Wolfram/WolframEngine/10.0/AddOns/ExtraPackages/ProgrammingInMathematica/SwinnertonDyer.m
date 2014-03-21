(* :Title: The Swinnerton-Dyer polynomials *)


(* :Context: ProgrammingInMathematica`SwinnertonDyer` *)

(* :Author: Roman E. Maeder *)

(* :Copyright: © 1989-1996 by Roman E. Maeder *)

(* :Package Version: 2.0 *)

(* :Mathematica Version: 3.0 *)

(* :History:
   2.0 for Programming in Mathematica, 3rd ed.
   1.1 for Programming in Mathematica, 2nd ed.
   1.0 for Programming in Mathematica, 1st ed.
*)

(* :Keywords: Swinnerton-Dyer, polynomial factorization *)

(* :Sources:
   Roman E. Maeder. Programming in Mathematica, 3rd ed. Addison-Wesley, 1996.
*)

(* :Discussion:
   See Sections 4.7 and 5.5 of "Programming in Mathematica"
*)

BeginPackage["ProgrammingInMathematica`SwinnertonDyer`"]

SwinnertonDyerP::usage = "SwinnertonDyerP[n, var] gives the minimal polynomial
	of the sum of the square roots of the first n primes."

Begin["`Private`"]

SwinnertonDyerP[ 0, x_ ] := x

SwinnertonDyerP[ n_Integer?Positive, x_ ] :=
    Module[{sd, srp = Sqrt[Prime[n]]},
    	sd[y_] = SwinnertonDyerP[n-1, y];
    	Expand[ sd[x + srp] sd[x - srp] ]
    ]

End[ ]

EndPackage[]
