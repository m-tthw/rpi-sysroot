(* :Title: OddEvenRules *)


(* :Author: Roman E. Maeder *)

(* :Package Version: 2.0 *)

(* :Mathematica Version: 3.0 *)

(* :History:
   2.0 for Programming in Mathematica, 3rd ed.
   1.1 for Programming in Mathematica, 2nd ed.
   1.0 for Programming in Mathematica, 1st ed.
*)

(* :Sources:
   Roman E. Maeder. Programming in Mathematica, 3rd ed. Addison-Wesley, 1996.
*)

(* :Discussion:
   See Section 6.1 of "Programming in Mathematica"
*)

OddEvenRules = {
	(f_Symbol?OddQ)[n_?Negative x_.] :> -f[-n x],
	(f_Symbol?OddQ)[n_?Negative x_. + y__] /; OrderedQ[{n x, y}] :>
	    -f[-n x - Plus[y]],
	(f_Symbol?EvenQ)[n_?Negative x_.] :> f[-n x],
	(f_Symbol?EvenQ)[n_?Negative x_. + y__] /; OrderedQ[{n x, y}] :>
	    f[-n x - Plus[y]]
};
