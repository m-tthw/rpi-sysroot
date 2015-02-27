(* Mathematica Package *)


BeginPackage["DataDropClient`"]
(* Exported symbols added here with SymbolName::usage *)  

System`DatabinAdd

$placeholder="DataDropPlaceholderKey";

Begin["`Private`"] (* Begin Private Context *) 

System`DatabinAdd[args___]:=Catch[databinAdd[args]]

databinAdd[bin_Databin,rest___]:=datadropexecute[bin,"Add",rest]
databinAdd[str_String,rest___]:=Block[{bin},
	Check[bin=Databin[str],
		(Message[DatabinAdd::nobin,first];Return[$Failed])
	];
	datadropexecute[bin,"Add",rest]
]

databinAdd[first_,___]:=(Message[DatabinAdd::nobin,first];$Failed)
databinAdd[___]:=$Failed

datadropAdd[id_, as_Association]:=datadropadd[id,as]
datadropAdd[id_, value_]:=datadropadd[id,Association[{$placeholder->value}]]
(* Might want to make an adding function
datadropAdd[id_]:=datadropadd[id,Association[]]
*)

datadropAdd[___]:=$Failed


datadropadd[id_,as_]:=With[{res=datadropadd0[id,addwriteauth[id, as]]},
	storetoken[as, id, "Add",res];
	res
]

(* Add *)
datadropadd0[id_,as_]:=Block[{res=apifun["Add",Join[Association[{"Bin"->id}],as]], info, old},
	If[Quiet[KeyExistsQ[res,"Data"]],
		info=Lookup[res,"Information",datadropclientcache[{"DatabinStats", id}]];
		res=removeplaceholders[res];
		If[KeyExistsQ[info,"Size"],
			info=Normal@MapAt[Quantity[#/1000,"Kilobytes"]&,If[ListQ[info],Association,Identity]@info,"Size"]
		];
		(* update stats *)
		old=datadropclientcache[{"DatabinStats", id}]/.$Failed->{};
		info=Merge[{old,info},Last];
		datadropclientcache[{"DatabinStats", id}]=info;
		(* update latest *)
		storelatest[id, {KeyTake[res,{"Data","Timestamp"}]}];
		(* update recent *)
		(* addtosample[id, {KeyTake[res,{"Data","Timestamp"}]}]; *)
		Databin[id]
		,
		(* error handling *)
		If[Quiet[KeyExistsQ[res,"Message"]],
			Message[Databin::apierr,Lookup[res,"Message",""]];
			$Failed
		]
	]
]


(* utilities *)
addwriteauth[id_,as_]:=Block[{token},
		token=writeauth[id];
		If[token===None||KeyExistsQ[as,"Authorization"],
			as,
			Join[Association[{"Authorization"->token}],as]
		]
]

removeplaceholders[data_]:=MapAt[If[Quiet[Keys[#]==={$placeholder}],#[$placeholder],#]&,data,"Data"]

End[] (* End Private Context *)

EndPackage[]
