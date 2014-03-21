(* :Title: Tensors *)


(* :Context: ProgrammingInMathematica`Tensors` *)

(* :Author: Roman E. Maeder *)

(* :Summary:
   formatting of tensors
 *)

(* :Copyright: © 1989-1996 by Roman E. Maeder *)

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
   See Section 9.1 of "Programming in Mathematica"
*)

BeginPackage["ProgrammingInMathematica`Tensors`"]

ui::usage = "ui[index] denotes an upper index in a tensor."
li::usage = "li[index] denotes a lower index in a tensor."
Tensor::usage = "Tensor[h][indices] denotes a tensor h with index list indices."

Begin["`Private`"]

Format[ Tensor[t_][ind___] ] :=
	Module[{indices},
		indices = {ind} /. {ui->Superscript, li->Subscript};
		SequenceForm[t, Sequence @@ indices]
	] 

Format[ Tensor[t_][ind___], TeXForm ] :=
	Module[{indices},
		indices = {ind} /. {ui->Superscript, li->Subscript};
		indices = Transpose[{Table["{}", {Length[indices]}], indices}];
		SequenceForm[t, Sequence @@ Flatten[indices, 1]]
	] 

End[]

Protect[ Tensor, ui, li ]

EndPackage[]
