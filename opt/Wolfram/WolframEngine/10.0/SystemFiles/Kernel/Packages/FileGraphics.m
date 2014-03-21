(* Put PostScript output in the temporary file pstmp *)

Begin["System`"]

$Display := $FileDisplay
$FileDisplay = OpenWrite["pstmp"]

$DisplayFunction = Display[$Display, #, "MPS"]&

(* Print and flush the temporary file *)

PrintPlot := ( Run["psfix pstmp | lpr"];
	       Close[$FileDisplay]; $FileDisplay = OpenWrite["pstmp"]; )

End[]

If[ !($BatchOutput || $Linked || $ParentLink =!= Null),
	Print[" -- File graphics initialized -- "] ]
