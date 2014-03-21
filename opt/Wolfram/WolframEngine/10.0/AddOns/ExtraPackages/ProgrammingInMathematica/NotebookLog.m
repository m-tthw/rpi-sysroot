(* :Title: NotebookLog *)


(* :Context: ProgrammingInMathematica`NotebookLog` *)

(* :Author: Roman E. Maeder *)

(* :Summary:
   generate a notebook from a transcript of a kernel session
 *)

(* :Copyright: © 1996 by Roman E. Maeder *)

(* :Package Version: 2.0 *)

(* :Mathematica Version: 3.0 *)

(* :History:
   2.0 for Programming in Mathematica, 3rd ed.
*)

(* :Keywords: notebook, log, session *)

(* :Sources:
   Roman E. Maeder. Programming in Mathematica, 3rd ed. Addison-Wesley, 1996.
*)

(* :Warnings:
   <description of global effects, incompatibilities>
*)

(* :Limitations:
   uses $Post
*)

(* :Discussion:
   See Section 9.4 of "Programming in Mathematica"
*)

BeginPackage["ProgrammingInMathematica`NotebookLog`"]

NotebookLog::usage = "NotebookLog[\"file.nb\"] starts a transcript in notebook
	format. NotebookLog[] closes the log file."

Begin["`Private`"]

NotebookLog::notso = "Log file is not open."
NotebookLog::already = "Log file is already open."
NotebookLog::post = "Note: old value of $Post overwritten."

fileopts = {FormatType -> InputForm, PageHeight -> Infinity, PageWidth -> 78}

prolog = OutputForm["Notebook[{"]
first = Cell["Session Transcript", "Title", TextAlignment -> Center]
last = Cell["end of transcript", "SmallText"]
epilog = OutputForm["}]"]
comma = OutputForm[","]

`transcript
`open = False

NotebookLog[filename_String]/; !open := (
	transcript = OpenWrite[filename, fileopts];
	If[ transcript === $Failed, Return[transcript] ];
	If[ ValueQ[$Post], Message[NotebookLog::post] ];
	$Post := ($Post = makeTranscript; Protect[$Post]; Identity);
	closeOnExit; open = True;
	Write[transcript, prolog];
	Write[transcript, first, comma]; )

NotebookLog[filename_String]/; Message[NotebookLog::already] := Null

e:NotebookLog[]/; open := (
	Unprotect[$Post]; Unset[$Post];
	Write[transcript, last];
	Write[transcript, epilog];
	restoreExit; open = False;
	Close[transcript] )

e:NotebookLog[]/; Message[NotebookLog::notso] := Null

inCell[boxes_]  := Cell[BoxData[boxes], "Input"]
outCell[boxes_] := Cell[BoxData[boxes], "Output"]

makeTranscript =
    Function[output,
        Module[{in, out},
          in = Replace[DownValues[In][[-1]], (_ :> r_) :> ToBoxes[Unevaluated[r]]];
          Write[transcript, inCell[in], comma];
          If[ output =!= Null,
              out = ToBoxes[output];
              Write[transcript, outCell[out], comma] ]
        ];
        output ]

closeOnExit :=
    If[ ValueQ[$Epilog],
        OwnValues[$Epilog][[-1]] /.
          (l_ :> CompoundExpression[r__]|r_) :>
            ($Epilog := (r; NotebookLog[])),
        (* else *) $Epilog := NotebookLog[] ]

restoreExit := OwnValues[$Epilog][[-1]] /.
          {(l_ :> (a___; NotebookLog[]; z___)) :> ($Epilog := (a; z)),
           (l_ :> NotebookLog[]) :> Unset[$Epilog]}

End[]

Protect[ NotebookLog ]

EndPackage[]
