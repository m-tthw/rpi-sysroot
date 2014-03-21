(* :Title: ReadList *)


(* :Author: Roman E. Maeder *)

(* :Package Version: 2.0 *)

(* :Mathematica Version: 3.0 *)

(* :History:
   2.0 for Programming in Mathematica, 3rd ed.
*)

(* :Sources:
   Roman E. Maeder. Programming in Mathematica, 3rd ed. Addison-Wesley, 1996.
*)

(* :Discussion:
   See Section 9.2 of "Programming in Mathematica"
*)

MyReadList[fileName_String, thing_:Expression] :=
	Module[{file, expr, list = {}},
		file = OpenRead[fileName];
		If[ file === $Failed, Return[file] ];
		While[ True,
			expr = Read[file, thing];
			If[ expr === EndOfFile, Break[] ];
			AppendTo[list, expr]
		];
		Close[file];
		list
	]
