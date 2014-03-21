(* :Title: AutoAnimation *)


(* :Context: ProgrammingInMathematica`AutoAnimation` *)

(* :Author: Roman E. Maeder *)

(* :Summary:
   an animation setting that causes new animations to be rendered automatically
   by the frontend
 *)

(* :Copyright: © 1996 by Roman E. Maeder *)

(* :Package Version: 2.0 *)

(* :Mathematica Version: 3.0 *)

(* :History:
   2.0 for Programming in Mathematica, 3rd ed.
*)

(* :Keywords: animation, frontend, programming *)

(* :Sources:
   Roman E. Maeder. Programming in Mathematica, 3rd ed. Addison-Wesley, 1996.
*)

(* :Discussion:
   See Section 11.3 of "Programming in Mathematica"
*)

(* :Requirements:
   Graphics/Animation.m
*)

System`$RasterFunction = $DisplayFunction

System`$AnimationTime = 60

System`$AnimationFunction =
    With[{nb = EvaluationNotebook[]},
        SelectionMove[nb, All, GeneratedCell];
        SelectionMove[nb, All, CellGroup];
        FrontEndExecute[FrontEndToken["OpenCloseGroup"]];
        Options[nb]; (* waste some time for display update *)
        FrontEndExecute[FrontEnd`SelectionAnimate[nb, $AnimationTime]];
        SelectionMove[nb, After, CellGroup];
        #
    ]&

Null
