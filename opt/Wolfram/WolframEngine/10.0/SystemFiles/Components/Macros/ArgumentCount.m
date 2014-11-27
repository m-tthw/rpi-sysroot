Package["Macros`"]


PackageExport["SetArgumentCount"]

SetArgumentCount[func_, min_Integer] := 
	SetArgumentCount[func, {min, min}];

SetArgumentCount[func_Symbol, {min_Integer, max_}] := (
	TagSet[func, ArgumentCount[func], {min, max}];
	func[args___] /; ArgumentCountInvalidQ[func, Length[HoldComplete[args]]] := $Failed /; False
);

ArgumentCountInvalidQ[func_, len_] := 
	!ArgumentCountQ[func, len, First @ ArgumentCount[func], Last @ ArgumentCount[func]];


