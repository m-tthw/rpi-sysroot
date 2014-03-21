(* :Title: Iterated Function Systems *)


(* :Context: ProgrammingInMathematica`IFS` *)

(* :Author: Roman E. Maeder *)

(* :Summary:
   data types for iterated function systems, composed of affine maps
 *)

(* :Copyright: © 1996 by Roman E. Maeder *)

(* :Package Version: 2.0 *)

(* :Mathematica Version: 3.0 *)

(* :History:
   2.0 for Programming in Mathematica, 3rd ed.
*)

(* :Keywords: template, skeleton, package *)

(* :Sources:
   Roman E. Maeder. Programming in Mathematica, 3rd ed. Addison-Wesley, 1996.
*)

(* :Discussion:
   See Section 12.2 of "Programming in Mathematica"
*)

(* :Requirements:
   ProgrammingInMathematica/AffineMaps.m
*)

BeginPackage["ProgrammingInMathematica`IFS`", "ProgrammingInMathematica`AffineMaps`"]

IFS::usage = "IFS[{maps..}, {options..}] generates an iterated
	function system (IFS)."

ifs::usage = "-ifs- represents an iterated function system (IFS)."

Probabilities::usage = "Probabilities -> {pr..} is an option of IFS
	that gives the probabilities of the maps for the chaos game."

Options[ IFS ] = {
	Probabilities -> Automatic
};

Begin["`Private`"]

Format[ _ifs ] := "-ifs-"

(* Freeze missing options *)

optnames = First /@ Options[IFS]

IFS[ ms:{_map...}, opts___?OptionQ ] :=
    Module[{optvals},
        optvals = optnames /. Flatten[{opts}] /. Options[IFS];
        ifs[ ms, Thread[optnames -> optvals] ]
    ]

(* apply *)

ifs[ms_List, _][gr:Graphics[_, opts___]] :=
	Graphics[ First /@ Through[ms[gr]], opts ]
(i_ifs)[objs_List] := i /@ objs
ifs[ms_List, _][obj_] := Through[ ms[obj] ]

End[ ]

Protect[ IFS, ifs, Probabilities ]

EndPackage[ ]
