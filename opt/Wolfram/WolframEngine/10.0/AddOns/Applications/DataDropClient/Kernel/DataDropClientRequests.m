(* Mathematica Package *)


BeginPackage["DataDropClient`"]
(* Exported symbols added here with SymbolName::usage *)  


Begin["`Private`"] (* Begin Private Context *) 

$authenticatedrequests={"Data","WebReport","Report", "FullRecords","WebForm"};
$nonauthenticatedrequests={"Add","Latest","LatestDate","Recent","ShortID","UUID","Name","Creator","CreationDate","Information","Keys","URL",(* "ShortURL", *)
	"Permissions", "ExpirationDate", "LatestTimestamp","Entries","Size","Administrators","Owner","Class","Interpretation"};
$undocumentedrequests={"Administrator"}

(*** Get api locations and create cloud objects *)
$DataDropClientRequests:=If[$CloudConnected,
	Union[$authenticatedrequests,$nonauthenticatedrequests],
	Sort[$nonauthenticatedrequests]
]


datadropExecute[args___]:=Catch[datadropexecute[args]]


datadropexecute[databin_Databin,request_String,rest___]:=
	datadropexecute[getBinID[databin],request,rest]

datadropexecute[id_,request_,args_]:=datadropexecute[id,request,Association[args]]/;MatchQ[args,{_Rule...}]
datadropexecute[id_,request_,args___]:=datadropexecute[id,request,Association[{args}]]/;MatchQ[{args},{_Rule...}]

datadropexecute[id_,"Add",rest___]:=datadropAdd[id,rest];
datadropexecute[id_,"FullRecords",rest___]:=datadropRead[id,rest];
datadropexecute[id_,"Latest",rest___]:=datadropLatest[id,rest];
datadropexecute[id_,"LatestDate",rest___]:=datadropLatestDate[id,rest];
datadropexecute[id_,"Recent",rest___]:=datadropRecent[id,rest];
datadropexecute[id_,"Data",rest___]:=datadropEventSeries[id,rest];
datadropexecute[id_,"Embed",rest___]:=datadropEmbed[id, rest]


datadropexecute[id_,prop:("Keys"|"Interpretation"),___]:=With[{res=apifun["ExpectedParameters",Join[Association[{"Bin"->id}],Association[]]]},
	If[ListQ[res],
		If[prop==="Keys",
			First/@res,
			res
		],
		$Failed		
	]
]


datadropexecute[id_,"Information",as_Association]:=Join[getBinSettings[id,as],datadropexecute[id,"BinStats",as]];

datadropexecute[id_,request_,as_Association]:=datadropexecute1[id,request,as]

datadropexecute[___]:=$Failed


datadropexecute1[id_,"WebForm",as_]:=With[{res=datadropexecute0[id, "WebForm",as]},
	If[KeyExistsQ[res,"URL"],
		CloudObject["URL"/.res],
		$Failed
	]
]



(* {"Latest","Information"} *)
(* {"Data","WebReport"} *)
(* Local requests *)
datadropexecute1[id_,"UUID",_]:=(id)
datadropexecute1[id_,"ShortID",_]:=getShortBinID[id]
datadropexecute1[id_,"Name",_]:=getBinName[id]
datadropexecute1[id_,"CreationDate",_]:=getCreationDate[id]
datadropexecute1[id_,"Requests",_]:=$DataDropClientRequests
datadropexecute1[id_,"Creator",_]:=getCreator[id]
datadropexecute1[id_,"Owner",_]:=getCreator[id,"Owner"]
(* datadropexecute1[id_,"ShortURL",_]:=Hyperlink[getBinURL[id]] *)
datadropexecute1[id_,"URL",_]:=Hyperlink[getBinURL[id,"Long"]]

datadropexecute1[id_,"Entries",_]:=With[{res=Lookup[datadropclientcache[{"DatabinStats", id}],"Entries",None]},
	If[res===None,
		datadropinforequest[id,"Entries"],
		res
	]
]

datadropexecute1[id_,"Administrator",rest___]:=datadropexecute1[id,"Administrators",rest]
datadropexecute1[id_,"Permissions",rest___]:=With[{info=datadropexecute[id,"Information",Association[{}]]},
	KeyTake[info,{"ReadAuthorization","WriteAuthorization"}]
]

(* Information *)
inforequests={"ReadAuthorization","WriteAuthorization", "ExpirationDate", "LatestTimestamp","Class","Administrators","Size"}
datadropexecute1[id_,prop:(Alternatives@@inforequests),_]:=datadropinforequest[id, prop]

datadropinforequest[id_,prop_]:=With[{info=datadropexecute[id,"Information",Association[{}]]},
	Lookup[info,prop,default[prop]]
]
default["Class"]=default["ExpirationDate"]=None
default[_]:=Missing[]

datadropexecute1[id_,req_,as_]:=datadropexecuteToken[id, req,addreadauth[id, as]]/;MemberQ[{"Dashboard"},req]
datadropexecute1[id_,req_,as_]:=datadropexecute0[id, req,as]

datadropexecute1[___]:=$Failed

datadropexecuteToken[id_,req_,as_]:=With[{res=datadropexecute0[id, req, as]},
	storetoken[as, id, req,res];
	res
]

(* Requests that require special handling *)
datadropexecute0[id_,"BinStats",as_]:=getBinStats0[id]
datadropexecute0[id_,"WebReport",as_]:=datadropclientdashboard[id, as]
datadropexecute0[id_,"Report",as_]:=datadropclientlocaldashboard[id, as]
(* No special handling needed, since only the BinID is required *)
datadropexecute0[id_,req_,as_]:=apifun[req,Join[Association[{"Bin"->id}],as]]/;MemberQ[$DataDropClientRequests,req]

datadropexecute0[___]:=$Failed

storetoken[as_, id_, req_,res_]:=Switch[req,
	"Add",
	writeauth[id]=as["Authorization"],
	"Read"|"Recent"|"WebReport"|"Data"|"FullRecords",
	readauth[id]=as["Authorization"],
	_,Null
]/;KeyExistsQ[as,"Authorization"]&&validresultQ[res]

storetoken[___]:=Null

validresultQ[res_]:=FreeQ[res, "error" | "Error" | $Failed]


(* Dashboard *)
datadropclientdashboard[id_, as_]:=If[$CloudConnected,
	With[{res=apifun["Dashboard",Join[Association[{"Bin"->id}],as]]},
		If[KeyExistsQ[res,"URL"],
			Hyperlink[Lookup[res,"URL"]],
			$Failed
		]
	],
	Message[Databin::dashcon];$Failed
]

datadropclientlocaldashboard[id_, as_]:=If[$CloudConnected,
	With[{res=apifun["Dashboard",Join[Association[{"Bin"->id,"Deployed"->False}],as]]},
		If[Head[res]===Notebook,
			NotebookPut[res],
			$Failed
		]
	],
	Message[Databin::dashcon];$Failed
]

End[] (* End Private Context *)

EndPackage[]
