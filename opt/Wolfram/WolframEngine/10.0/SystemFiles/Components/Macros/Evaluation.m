Package["Macros`"]


PackageExport["HoldSequence"]

SetAttributes[HoldSequence, SequenceHold];


PackageExport["ReleaseHoldSequence"]

ReleaseHoldSequence[HoldSequence[x_]] := x;
ReleaseHoldSequence[x_] := x;

PackageExport["$FailRHS"]
PackageExport["ConditionalRHS"]
PackageExport["UnevaluatedLHS"]

ConditionalRHS /: SetDelayed[head_Symbol[args___], ConditionalRHS[checks___, body_]] := 
	With[{rhs = Join @@ Append[Hold[True,HoldSequence[body]]] @ Replace[
		Partition[Hold[checks], 2],
		Hold[test_, {msg_String,msgargs___}] :> 
			Hold[!Quiet[TrueQ[test]], Message[MessageName[head, msg], msgargs]; $FailRHS]
		,
		{1}
	] //. UnevaluatedLHS -> Self},
	applyConditionalRHS[head, Self:head[args], rhs]
];

SetAttributes[applyConditionalRHS, HoldAll];
applyConditionalRHS[head_, lhs_, Hold[rhs___]] := Module[
	{dvs = DownValues[head], dvs2},
	dvs2 = Map[If[shouldReplaceQ[HoldPattern[lhs], First[#]], Unevaluated[Sequence[]], #]&, dvs];
	If[dvs2 =!= dvs, DownValues[head] = dvs2];
	SetDelayed[lhs, 
		Module[{r}, 
			r = Which[rhs]; 
			ReleaseHoldSequence[r] /; !MatchQ[r, $FailRHS | HoldSequence[$FailRHS]]
		]
	];
];

shouldReplaceQ[new_, existing_] := 
	MatchQ[Internal`ComparePatterns[existing, new], "Equivalent" | "Identical"];
