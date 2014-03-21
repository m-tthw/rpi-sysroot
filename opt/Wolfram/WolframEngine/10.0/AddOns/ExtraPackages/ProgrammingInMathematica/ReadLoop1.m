(* :Title: ReadLoop1 *)


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

ReadLoop[fileName_String] :=
	Module[{file, expr},
		file = OpenRead[fileName];
		If[ file === $Failed, Return[file] ];
		While[ True,
			expr = Read[file];
			If[ expr === EndOfFile, Break[] ];
			Print["expr is ", expr]
		];
		Close[file]
	]
