(* Mathematica Package *)

(Unprotect[#]; Clear[#])& /@ {System`CreateDatabin}

BeginPackage["DataDropClient`"]
(* Exported symbols added here with SymbolName::usage *)  
System`CreateDatabin
DataDropClient`CreateClass

Begin["`Private`"] (* Begin Private Context *) 

(********************** Create Databins ************************)
System`CreateDatabin[args___]:=Catch[createDatabin[args]]

createDatabin[]:=createDatabin[Association[{}]]

createDatabin[as_Association]:=If[KeyExistsQ[as,"Administrator"]||!$CloudConnected,
	createdatabin[as],
	createdatabin[Join[Association[{"Administrator"->$WolframID}],as]]
]

createDatabin[opts___?OptionQ]:=createDatabin[Association[{opts}]]

createdatabin[___]:=(Message[CreateDatabin::cloudc];$Failed)/;!$CloudConnected
createdatabin[as_Association]:=Block[{id,name,shortid, 
	res=apifun["Create",KeyMap[# /. Permissions -> "Permissions" &, as]], 
	tokens},
	If[KeyExistsQ[res,"UUID"],
		shortid="ShortID"/.res;
		id="UUID"/.res;
		If[shortid==="ShortID",shortid=id];
		name=Lookup[res,"Name",Lookup[as,"Name",None]];
		tokens="Tokens"/.res;
		If[tokens=!="Tokens",
			readauth[id]=First@Replace[Lookup[tokens,"ReadTokens",{None}], {Except[_List] :> {None},{}->{None}}, {0}];
			writeauth[id]=First@Replace[Lookup[tokens,"WriteTokens",{None}], {Except[_List] :> {None},{}->{None}}, {0}];
		];
		creationDate[id]=Quiet[Replace[
			timezoneconvert[Lookup[res,"CreationDate",DateObject[]]], Except[_DateObject] :> Missing[], {0}]
			];
		
		binroles[id,"Creator"]=Lookup[res,"Creator",None];
		binroles[id,"Owner"]=Lookup[res,"Owner",None];
		datadropclientcache[{"DatabinLatest", id}]=datadropclientcache[{"DatabinLatest", shortid}]={};
		(* datadropclientcache[{"DatabinRecentEventSeries", id}]=datadropclientcache[{"DatabinRecentEventSeries", shortid}]=Missing[]; *)
		
		$loadeddatabins=Join[$loadeddatabins,{id,shortid}];
		
		If[StringQ[id]&&id=!="ShortID",
			makedatabin[shortid, id, name],
			error["create"]
		],
		error["create"]
		
	]
]

createdatabin[class_String,rest___]:=createdatabin["Class"->class,rest]
createdatabin[class_,email_String,rest___]:=createdatabin[class,"Administrator"->email]
createdatabin[r:(_Rule...)]:=createdatabin[Association[{r}]]
createdatabin[{r:(_Rule...)}]:=createdatabin[Association[{r}]]

createDatabin[___]:=$Failed
createdatabin[___]:=$Failed


(********************** Create Classes ************************)
DataDropClient`CreateClass[args___]:=createClass[args]

createClass[as_Association]:=createclass[as]

createClass[opts___?OptionQ]:=createClass[Association[opts]]

createClass[___]:=$Failed

createclass[as_]:=Block[{name, res},
	res=apifun["CreateClass",as];
	If[Quiet[KeyExistsQ[res,"ClassName"]],
		Lookup[res,"ClassName",$Failed],
		$Failed
	]
	
]/;KeyExistsQ[as,"ClassName"]


End[] (* End Private Context *)

EndPackage[]

SetAttributes[{CreateDatabin},
   {ReadProtected, Protected}
];


{System`CreateDatabin}