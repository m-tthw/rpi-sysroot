(* :Title: NotebookStuff *)


(* :Context: ProgrammingInMathematica`NotebookStuff` *)

(* :Author: Roman E. Maeder *)

(* :Summary:
   sample functions for manipulating notebooks with Mathematica
 *)

(* :Copyright: © 1996 by Roman E. Maeder *)

(* :Package Version: 2.0 *)

(* :Mathematica Version: 3.0 *)

(* :History:
   2.0 for Programming in Mathematica, 3rd ed.
*)

(* :Keywords: notebooks, programming, hyperlinks *)

(* :Sources:
   Roman E. Maeder. Programming in Mathematica, 3rd ed. Addison-Wesley, 1996.
*)

(* :Discussion:
   See Chapter 11 of "Programming in Mathematica"
*)

BeginPackage["ProgrammingInMathematica`NotebookStuff`"]

makeHyperlink::usage = "makeHyperlink[filename] gives a button box
	that acts as a hyperlink to filename."

Options[makeHyperlink] = {
	Directory -> ".",
	Tag -> None,
	ButtonStyle -> "Hyperlink"
}

Begin["`Private`"]

Needs["Utilities`FilterOptions`"]

protected = Unprotect[Notebook, Cell]


(* formats for notebooks and cells *)

skel = "\[SkeletonIndicator]"

Format[Notebook[cells_List, ___]] := SequenceForm[skel, "Notebook", skel]

Format[Cell[c_, style_, ___]] := SequenceForm[skel, style, skel]

Format[Cell[CellGroupData[c_List, Closed]]] := SequenceForm[skel, "closed group", skel]


(* hyperlink buttons *)

buttonOpts = {ButtonStyle, ButtonData, ButtonNote, ButtonFunction, ButtonEvaluator}
q = "\""

makeHyperlink[name_String, opts___?OptionQ] :=
    With[{ dir = Directory /. {opts} /. Options[makeHyperlink],
           tag = Tag /. {opts} /. Options[makeHyperlink],
           label = StyleBox[q <> name <> q, ShowStringCharacters->False]
         },
      Module[{path},
        path = If[dir == "" || dir == ".", "", dir <> $PathnameSeparator] <> name;
        ButtonBox[ label, ButtonData->{path, tag},
            FilterOptions[buttonOpts, opts, Options[makeHyperlink]],
            ButtonNote->name ]
    ] ]


Protect[ Evaluate[protected] ]

End[]

EndPackage[]
