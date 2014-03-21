(* :Title: RandomWalk *)


(* :Context: ProgrammingInMathematica`RandomWalk` *)

(* :Author: Roman E. Maeder *)

(* :Summary:
   a simple random walk
 *)

(* :Copyright: © 1989-1996 by Roman E. Maeder *)

(* :Package Version: 2.0 *)

(* :Mathematica Version: 3.0 *)

(* :History:
   2.0 for Programming in Mathematica, 3rd ed.
   1.1 for Programming in Mathematica, 2nd ed.
   1.0 for Programming in Mathematica, 1st ed.
*)

(* :Keywords: random walk *)

(* :Sources:
   Roman E. Maeder. Programming in Mathematica, 3rd ed. Addison-Wesley, 1996.
*)

(* :Discussion:
   See Section 4.4 of "Programming in Mathematica"
*)

BeginPackage["ProgrammingInMathematica`RandomWalk`"]

RandomWalk::usage = "RandomWalk[n, opts] plots a random walk of length n.
	Options of Graphics are passed to Show."

Begin["`Private`"]

range = N[{0, 2Pi}]

randomDelta := With[{dir = Random[Real, range]}, {Cos[dir], Sin[dir]}]

RandomWalk[n_Integer, opts___?OptionQ] :=
	Module[{points},
		points = NestList[ # + randomDelta&, {0, 0}, n ];
		Show[ Graphics[{Point[{0, 0}], Line[points]}], opts,
		      Frame -> True, FrameTicks -> None, AspectRatio -> 1 ]
	]

End[]

Protect[ RandomWalk ]

EndPackage[]
