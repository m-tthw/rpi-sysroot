(* :Title: Collatz *)


(* :Context: ProgrammingInMathematica`Collatz` *)

(* :Author: Roman E. Maeder *)

(* :Summary:
   The Collatz game, also known as 3n+1 game
 *)

(* :Copyright: © 1989-1996 by Roman E. Maeder *)

(* :Package Version: 2.0 *)

(* :Mathematica Version: 3.0 *)

(* :History:
   2.0 for Programming in Mathematica, 3rd ed.
   1.1 for Programming in Mathematica, 2nd ed.
   1.0 for Programming in Mathematica, 1st ed.
*)

(* :Keywords: Collatz, sequence, termination, computability *)

(* :Sources:
   Roman E. Maeder. Programming in Mathematica, 3rd ed. Addison-Wesley, 1996.
*)

(* :Warnings:
   it is not known whether the function terminates for every input
*)

(* :Discussion:
   See Section 9.3 of "Programming in Mathematica"
*)

BeginPackage["ProgrammingInMathematica`Collatz`"]

Collatz::usage = "Collatz[n] gives a list of the iterates in the 3n+1 problem,
	starting from the positive integer n, until reaching 1 for the first
	time. The conjecture is that this sequence always terminates."

StoppingTime::usage = "StoppingTime[n] finds the total stopping time of the
	integer n. This is the length of the Collatz sequence before hitting 1."

FindMaxima::usage = "FindMaxima[from] reports successive maxima of the
	total stopping time starting the search with the integer from."

Begin["`Private`"]

c[ k_?EvenQ ] := k/2
c[ k_ ] := (3k + 1)

Collatz[ k_Integer?Positive ] := Flatten[ appendCollatz[{}, k] ]

appendCollatz[sofar_, 1] := {sofar, 1}
appendCollatz[sofar_, k_Integer] := appendCollatz[{sofar, k}, c[k]]

StoppingTime[ k_Integer?Positive ] :=
	Module[{i=1, m=k}, While[m != 1, m = c[m]; i++]; i ]

FindMaxima[ low_ ] :=
	Module[{m=0, k=0, i=low, si},
		While[ True,
			si = StoppingTime[i];
			If[si > m, {m, k} = {si, i}; Print["StoppingTime[", k, "] = ", m] ];
			i++;
			If[ Mod[i, 100] == 0, Print["i = ", i] ] ]
	]

End[]

Protect[ Collatz, StoppingTime, FindMaxima ]

EndPackage[]
