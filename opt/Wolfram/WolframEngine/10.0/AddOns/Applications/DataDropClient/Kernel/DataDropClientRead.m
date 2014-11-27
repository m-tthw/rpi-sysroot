(* Mathematica Package *)


BeginPackage["DataDropClient`"]
(* Exported symbols added here with SymbolName::usage *)  

$readapi="Read";
$cacheDatabinResults=False;
Begin["`Private`"] (* Begin Private Context *) 

datadropRecent[args__]:=Block[{$readapi="Recent"},
	datadropRead[args]
]

datadropLatestDate[id_, arg___]:=With[{res=datadropLatest[id, arg]},
	Replace[Quiet[Lookup[res,"Timestamp",Missing[]]],Except[_DateObject]->Missing[],{0}]
]

datadropLatest[id_, arg___]:=First[Switch[{arg},
	{},datadropRecent[id,Association[{"Count"->1}]],
	{_Association},datadropRecent[id,Join[arg,Association[{"Count"->1}]]],
	{_List},datadropRecent[id,Association[{"Parameters"->arg,"Count"->1}]],
	{_},datadropRecent[id,Association[{"Parameters"->{arg},"Count"->1}]]
]]


readarguments[f1_, f2_]:=(
f1[id_, as_Association]:=f2[id,as];
f1[id_, n_Integer]:=f2[id,Association[{"Count"->n}]];
f1[id_, l_List]:=f2[id,Association[{"Parameters"->l}]];
f1[id_, key_]:=f2[id,Association[{"Parameters"->{key}}]];
)

readarguments[datadropRead,datadropread];
readarguments[datadropEventSeries,datadropeventSeries];

datadropRead[___]:=$Failed

datadropread[id_, as_]:=With[{res=datadropRecent[id,as],samplesize=5},
	If[Length[res]>=samplesize,
		Message[Databin::readcon]
	];
	res
]/;!$CloudConnected&&$readapi=!="Recent"

datadropread[id_,as_]:=With[{res=datadropread0[id,addreadauth[id, as]]},
	If[$cacheDatabinResults,datadropread0[id,as]=res];
	storetoken[as, id, "Read",res];
	res
]

datadropread0[id_,as_]:=Block[{res=apifun[$readapi,Join[Association[{"Bin"->id}],as]]},
	If[Quiet[KeyExistsQ[res,"Drops"]],res=Lookup[res,"Drops"]];
	If[MatchQ[res,{_Association..}],
		(* store latest *)
		storelatest[id, res];
		(* update recent *)
		(* addtosample[id, res]; *)
		importdrop/@res
		,
		(* Error Handling *)
		If[Quiet[KeyExistsQ[res,"Message"]],
			Message[Databin::apierr,Lookup[res,"Message","Data Drop could not perform the request."]];
			$Failed
		]
	]
]

datadropeventSeries[id_,as_Association]:=Module[{drops=datadropRead[id, Join[Association[{"Count"->All}],as]], data, keys},
	If[MatchQ[drops,{_Association..}],
		data=Lookup[drops,"Data"];
		data=Cases[Replace[data,  x : Except[_Association] :>  Association[{None -> x}], {1}], _Association, {1}];
		keys=Union[Flatten[getKeys/@data]];
		data=Transpose[Lookup[data,keys]];
		Association[MapThread[#1->EventSeries[DeleteCases[Transpose[{Lookup[drops,"Timestamp"],#2}],{_,_Missing}]]&,{keys,data}]]
		,
		Missing[]
	]
]

(* utilities *)
addreadauth[id_,as_]:=Block[{token},
		token=readauth[id];
		If[token===None||KeyExistsQ[as,"Authorization"],
			as,
			Join[Association[{"Authorization"->token}],as]
		]
]

End[] (* End Private Context *)

EndPackage[]
