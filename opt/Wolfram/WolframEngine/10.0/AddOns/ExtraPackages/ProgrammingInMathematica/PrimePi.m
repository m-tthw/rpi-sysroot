(* :Title: PrimePi *)


(* :Author: Roman E. Maeder *)

(* :Summary:
   a simple computation of PrimePi
 *)

(* :Copyright: © 1989-1996 by Roman E. Maeder *)

(* :Package Version: 2.0 *)

(* :Mathematica Version: 3.0 *)

(* :History:
   2.0 for Programming in Mathematica, 3rd ed.
   1.1 for Programming in Mathematica, 2nd ed.
   1.0 for Programming in Mathematica, 1st ed.
*)

(* :Keywords: PrimePi *)

(* :Sources:
   Roman E. Maeder. Programming in Mathematica, 3rd ed. Addison-Wesley, 1996.
*)

(* :Discussion:
   See Section 4.2 of "Programming in Mathematica"
*)

Attributes[primePi] = {Listable}

primePi[x_/; x < 2] := 0

primePi[x_/; x >= 2] :=
	Module[{li = LogIntegral[x], n0, n1, m},
		n0 = Floor[li - LogIntegral[Sqrt[x]]];
		n1 = Ceiling[li];
		While[ n1 - n0 > 1,
			m = Floor[(n0 + n1)/2];              (* midpoint *)
			If[ Prime[m] <= x, n0 = m, n1 = m ]
		];
		n0
	]
