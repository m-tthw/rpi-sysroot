(* :Title: SessionLog *)


(* :Context: ProgrammingInMathematica`SessionLog` *)

(* :Author: Roman E. Maeder *)

(* :Summary:
   simple session logging
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
   See Section 9.4 of "Programming in Mathematica"
*)

BeginPackage["ProgrammingInMathematica`SessionLog`"]

OpenLog::usage = "OpenLog[filename, opts..] starts logging all input and output
	to filename."
CloseLog::usage = "CloseLog[] closes the logfile opened by OpenLog[]."

Begin["`Private`"]

logfile=""

OpenLog[ filename_String, opts___?OptionQ ] := (
	logfile = OpenWrite[filename, opts];
	If[ logfile === $Failed, Return[logfile] ];
	AppendTo[$Echo, logfile];
	AppendTo[$Output, logfile];
	logfile
	)

CloseLog[ ] := (
	$Echo = Complement[$Echo, {logfile}];
	$Output = Complement[$Output, {logfile}];
	Close[logfile]
	)

End[ ]

Protect[ OpenLog, CloseLog ]

EndPackage[ ]
