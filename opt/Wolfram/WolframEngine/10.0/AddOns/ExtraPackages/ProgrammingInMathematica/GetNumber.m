(* :Title: GetNumber *)


(* :Context: ProgrammingInMathematica`GetNumber` *)

(* :Author: Roman E. Maeder *)

(* :Summary:
   a simple input loop
 *)

(* :Copyright: © 1989-1996 by Roman E. Maeder *)

(* :Package Version: 2.0 *)

(* :Mathematica Version: 3.0 *)

(* :History:
   2.0 for Programming in Mathematica, 3rd ed.
   1.1 for Programming in Mathematica, 2nd ed.
   1.0 for Programming in Mathematica, 1st ed.
*)

(* :Keywords: template, skeleton, package *)

(* :Sources:
   Roman E. Maeder. Programming in Mathematica, 3rd ed. Addison-Wesley, 1996.
*)

(* :Discussion:
   See Section 5.4 of "Programming in Mathematica"
*)

GetNumber[prompt_String, predicate_:(True&)] :=
	Module[{answer},
		While[ True,
			answer = Input[prompt];
			If[ NumberQ[answer] && predicate[answer], Break[] ];  (* good   *)
			If[ answer === EndOfFile, Break[] ];                  (* escape *)
			Print["Please enter a number that satisfies ", predicate]
		];
		answer
	]
