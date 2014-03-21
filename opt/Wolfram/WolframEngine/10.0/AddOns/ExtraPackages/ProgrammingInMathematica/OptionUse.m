(* :Title: OptionUse *)


(* :Context: ProgrammingInMathematica`OptionUse` *)

(* :Author: Roman E. Maeder *)

(* :Summary:
   a template for setting up functions with options
 *)

(* :Copyright: © 1989-1996 by Roman E. Maeder *)

(* :Package Version: 2.0 *)

(* :Mathematica Version: 3.0 *)

(* :History:
   2.0 for Programming in Mathematica, 3rd ed.
   1.1 for Programming in Mathematica, 2nd ed.
   1.0 for Programming in Mathematica, 1st ed.
*)

(* :Keywords: template, skeleton, package, options *)

(* :Sources:
   Roman E. Maeder. Programming in Mathematica, 3rd ed. Addison-Wesley, 1996.
*)

(* :Discussion:
   See Section3.2 of "Programming in Mathematica"
*)

BeginPackage["ProgrammingInMathematica`OptionUse`"]

g::usage = "g[n, options...] serves as an example for using options."

Opt1::usage = "Opt1 is an option of g[]."
Opt2::usage = "Opt2 is another option of g[]."

Options[g] = {Opt1 -> val1, Opt2 -> val2}

Begin["`Private`"]

g[ n_, opts___?OptionQ ] :=
	Module[ {opt1, opt2},
		opt1 = Opt1 /. {opts} /. Options[g];
		opt2 = Opt2 /. {opts} /. Options[g];
		{n, opt1, opt2}
	]

End[]

EndPackage[]
