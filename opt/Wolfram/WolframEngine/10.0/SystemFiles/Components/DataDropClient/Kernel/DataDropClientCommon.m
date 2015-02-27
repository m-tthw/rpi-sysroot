(* Mathematica Package *)

BeginPackage["DataDropClient`"]
(* Exported symbols added here with SymbolName::usage *)  
System`Databin
System`Databins

Begin["`Private`"] (* Begin Private Context *) 
$loadeddatabins={};
$BinLoadLimit=20;

shorturlbase="http://wolfr.am/"; 
urlbase="http://datadrop.wolfram.com/";
binurlbase="http://datadrop.wolframcloud.com/databin/";
gatewayapi="https://datadrop.wolframcloud.com/api/v1.0/Gateway";

(*** API Function ***)

apifun[name_,as_]:=
With[{
	raw=URLFetch[gatewayapi,{"StatusCode","Content"},
		"Parameters"->Join[{"API"->name,"SourceType"->"\"Connected Wolfram Language\"","InputStrings"->"True"},
			preparedata[Normal[as]]],
			"Headers" -> {"Authorization" -> CloudObject`Private`makeOAuthHeader[gatewayapi, "GET"]}, 
			"VerifyPeer" -> False,"CredentialsProvider" -> None]},
	checkAvailable[Quiet[ToExpression[raw]],name]
]/;$CloudConnected

apifun[name_,as_]:=
With[{
	raw=URLFetch[gatewayapi,{"StatusCode","Content"},
		"Parameters"->Join[{"API"->name,"SourceType"->"\"Unconnected Wolfram Language\"","InputStrings"->"True"},
			preparedata[Normal[as]]],"VerifyPeer" -> False,"CredentialsProvider" -> None]},
	checkAvailable[Quiet[ToExpression[raw]],name]
]

checkAvailable[{501,res_},_]:=(Message[Databin::notav];Throw[$Failed])
checkAvailable[{504,res_},"Read"|"Dashboard"]:=(Message[Databin::timeout1];Throw[$Failed])
checkAvailable[{504,res_},_]:=(Message[Databin::timeout2];Throw[$Failed])
checkAvailable[{_,res_},_]:=res

(**** Utilities ****)
databinID[databin_]:=First[databin]/;Length[databin]>0
databinID[databin_]:=$Failed

getBinID[id_String]:=With[{cached=datadropclientcache[{"DatabinIDs", id}]},
	If[cached=!=$Failed,
		Lookup[cached,"UUID",$Failed],
		getBinID0[id,"UUID"]
	]
]

getBinID[databin_]:=getBinID[databinID[databin]]

getShortBinID[id_String]:=With[{cached=datadropclientcache[{"DatabinIDs", id}]},
	If[cached=!=$Failed,
		Lookup[cached,"ShortID",None],
		getBinID0[id,"ShortID"]
	]
]

getShortBinID[databin_]:=getShortBinID[databinID[databin]]

(* Bin Name can change, we should have an update mechanism *)
getBinName[id_String]:=With[{cached=datadropclientcache[{"DatabinIDs", id}]},
	If[cached=!=$Failed,
		Lookup[cached,"Name",Missing[]],
		getBinID0[id,"Name"]
	]
]

getBinName[databin_]:=getBinName[databinID[databin]]

getBinURL[id_String, rest___]:=With[{cached=datadropclientcache[{"DatabinIDs", id}]},
	If[cached=!=$Failed&&{rest}=!={"Long"},
		Lookup[cached,"ShortURL",Missing[]]
		,
		buildbinURL[id,rest]
	]
]

buildbinURL[id_String]:=With[{shortid=getShortBinID[id]},
	If[StringQ[shortid],
		shorturlbase<>shortid
		,
		binurlbase<>id
	]
]

buildbinURL[id_String,"Long"]:=With[{uuid=getBinID[id]},
	binurlbase<>uuid
]

getBinSettings[id_]:=getBinSettings[id,Association[{}]]
getBinSettings[id_,as_]:=Block[{res=apifun["BinInfo",Join[as,Association[{"Bin"->id}]]]},
	If[Quiet[KeyExistsQ[res,"CreationDate"]],
		res=MapAt[timezoneconvert,res,"CreationDate"]
	];
	If[Quiet[KeyExistsQ[res,"ExpirationDate"]],
		res=MapAt[timezoneconvert,res,"ExpirationDate"]
	];
	res
	
]

getBinFormatting[id_]:=getBinFormatting[id]=With[{settings=getBinSettings[id]},
	If[Quiet[KeyExistsQ[settings,"DataFormatting"]],
		settings["DataFormatting"]
		,
		"EventSeries"
	]
]

(* Token management *)
readauth[_]:=None
writeauth[_]:=None

loaddatabin[id_]:=getBinID[id]/;MemberQ[$loadeddatabins,id]

(* get all the needed values for to represent an existing data bin, store them and return the uuid *)
loaddatabin[id_]:=Block[{res=apifun["LoadBin",Association[{"Bin"->id}]],uuid},
	storebin[id,res]
]

storebin[as_Association]:=storebin[Quiet[Lookup[Lookup[as,"BinIDs",{}],"UUID",$Failed]],as]
storebin[$Failed,___]:=$Failed
storebin[id_,res_Association]:=Module[{uuid,shortid},
	If[Quiet[KeyExistsQ[res,"BinIDs"]],
		{uuid,shortid}=storeBinID0[id,{"UUID","ShortID"},res["BinIDs"]];
		storeBinStats0[uuid,res["BinStats"]];
		storeRecent[uuid,importdrops@res["Recent"]];
		
		$loadeddatabins=Join[$loadeddatabins,{uuid,shortid}];
		uuid
		,
		(* Error handling *)
		If[Quiet[KeyExistsQ[res,"Message"]],
			Message[Databin::apierr,Lookup[res,"Message",""]];
			$Failed
		]
		
	]
]
storebin[$Failed,___]:=$Failed

loaddatabins[ids_List]:=Block[{ids1,res},
	ids1=Complement[ids,$loadeddatabins];
	res=apifun["LoadBin",Association[{"Bins"->ids1}]];
	storebin/@res	
]/;Length[ids]<=$BinLoadLimit

loaddatabins[ids_List]:=Join@@(loaddatabins/@Partition[ids,$BinLoadLimit,$BinLoadLimit, 1, {}])

getBinID0[id_, key_]:=Module[{res=apifun["BinIDs",Association[{"Bin"->id}]]},
	storeBinID0[id,key, res]
]

storeBinID0[id_,key_, res_]:=If[MatchQ[res,_Association|_List],
		If[KeyExistsQ[res,"UUID"],
			datadropclientcache[{"DatabinIDs", id}]=datadropclientcache[{"DatabinIDs", res["UUID"]}]=res/.$Failed->None;
			If[key===All,
				res,
				Lookup[res,key,Missing[]]
			]
			,
			error["nobin",id]
		],
		error["nobin",id]
	]
	
creationDate[_]:=Missing[]
expirationDate[_]:=None

getCreationDate[id_]:=With[{cached=creationDate[id]},
	If[Head[cached]===DateObject,
		cached,
		getCreationDate0[id]
	]
]

getExpirationDate[id_]:=With[{cached=expirationDate[id]},
	If[Head[cached]===DateObject,
		cached,
		getExpirationDate0[id]
	]
]

getRecent[id_]:=With[{res=getRecent0[id]},
	storeRecent[id,res]
]
storeRecent[id_,res_]:=If[ListQ[res],
		If[res==={},
			datadropclientcache[{"DatabinLatest", id}]:={};
			(* datadropclientcache[{"DatabinRecentEventSeries", id}]=Missing[]; *)
			,
			datadropclientcache[{"DatabinLatest", id}]:=Quiet[MapAt[timezoneconvert,Last[res],{"Timestamp"}]];
			(* datadropclientcache[{"DatabinRecentEventSeries", id}]=
				valueseries[res] *)
		],
	
		datadropclientcache[{"DatabinLatest", id}]:={};
		(* datadropclientcache[{"DatabinRecentEventSeries", id}]=Missing[]; *)
	]

getCreationDate0[id_]:=Block[{new,cached=datadropclientcache[{"DatabinStats", id}]},
	If[Quiet[KeyExistsQ[cached,"CreationDate"]],
		Lookup[cached,"CreationDate",None],
		new=getBinStats0[id];
		If[Quiet[KeyExistsQ[new,"CreationDate"]],
			timezoneconvert[Lookup[new,"CreationDate",None]],
			None
		]
	]
]

getExpirationDate0[id_]:=Block[{new,cached=datadropclientcache[{"DatabinStats", id}]},
	If[Quiet[KeyExistsQ[cached,"ExpirationDate"]],
		Lookup[cached,"ExpirationDate",None],
		None
	]
]

getCreator[id_,role_:"Creator"]:=With[{cached=binroles[id,role]},
	If[Head[cached]===String,
		cached,
		getCreator0[id,role]
	]
]


getCreator0[id_, role_]:=Block[{new,cached=datadropclientcache[{"DatabinStats", id}]},
	If[Quiet[KeyExistsQ[cached,role]],
		Lookup[cached,role,None],
		new=getBinStats0[id];
		If[Quiet[KeyExistsQ[new,role]],
			Lookup[new,role,None],
			None
		]
	]
]

getBinStats0[id_]:=With[{res=apifun["BinStats",Association[{"Bin"->id}]]},
	storeBinStats0[id,res]
]
storeBinStats0[id_,res0_]:=Module[{shortid=getShortBinID[id],uuid=getBinID[id], temp, res=res0},
	If[MatchQ[res,_Association|_List],
		temp=Quiet[MapAt[timezoneconvert,res,{{"LatestTimestamp"},{"CreationDate"}}]];
		If[KeyeExistsQ[temp,"ExpirationDate"],temp=Quiet[MapAt[timezoneconvert,temp,"ExpirationDate"]]];
		temp=Quiet[MapAt[Quantity[#/1000,"Kilobytes"]&,temp,"Size"]];
		If[Head[temp]=!=MapAt,res=temp];
		datadropclientcache[{"DatabinStats", shortid}]=datadropclientcache[{"DatabinStats", uuid}]=res;
		res
		,
		error["nobin",id]
	]
]

getRecent0[id_]:=With[{res=apifun["Recent",Association[{"Bin"->id}]]},
	If[ListQ[res],
		importdrops@res,
		error["nobin",id]
	]
	
]

(*
addtosample[id_,drops_]:=Block[{old, new},
	old=datadropclientcache[{"DatabinRecentEventSeries", id}];
	Check[
		datadropclientcache[{"DatabinRecentEventSeries", id}]=If[MatchQ[Head[old],TemporalData],
			TimeSeriesInsert[old,
				valueseries[drops]
			]
			,
			valueseries[drops]
		],
		Null
	]
]
*)
storelatest[_, {}]:=Null
storelatest[id_, res_]:=Block[{new, pos,times},
	times=Quiet[AbsoluteTime/@Lookup[res,"Timestamp",0]];
	times=Replace[times, Except[_?NumberQ] :> 0, {1}];
	pos=First[Ordering[times,-1]];
	If[times[[pos]]>AbsoluteTime[Lookup[datadropclientcache[{"DatabinLatest", id}],"Timestamp",DateObject[0]]],
		datadropclientcache[{"DatabinLatest", id}]=
			Quiet[MapAt[timezoneconvert,res[[pos]],{"Timestamp"}]];
	]
]

valueseries[drops_List]:=(EventSeries[
			Transpose[{Lookup[drops,"Timestamp"],values[Lookup[drops,"Data"]]}]
			])
valueseries[drop_]:=valueseries[{drop}]

values[as:(_Association|{_Association...})]:=Replace[Values[as],{}->None,{0,1}] (* EventSeries can not take empty data set *)
values[expr_]:=expr

(************************* User bins ************************)
System`Databins[args___]:=Catch[databins[args]]

databins[]:=(Message[Databins::cloudc];$Failed)/;!$CloudConnected
databins[___]:=(Message[Databins::cloudc];$Failed)/;!$CloudConnected

databins[]:=getUserBins[]

databins[___]:=$Failed

getUserBins[args___]:=$Failed/;!$CloudConnectedQ
getUserBins[]:=getuserbins[$WolframID]
getUserBins[as:(_List|_Association)]:=getuserbins[Lookup[as,"WolframID",$WolframID]]
getUserBins[str_String]:=getuserbins[str]

getuserbins[user_]:=Block[{res=apifun["UserData",Association[{"WolframID"->user}]],as, bins,ids},
	If[KeyExistsQ[res,"Bins"],
		as=res["Bins"];
		ids=DeleteDuplicates[Flatten@Values[as]];
		bins=Join[loaddatabins[ids],$loadeddatabins];
		Map[memberDatabin[bins,#]&,ids,{1}]
		,
		$Failed
	]
]

getUserBins[___]:=$Failed

memberDatabin[list_,str_String]:=Databin[str]/;MemberQ[list,str]
memberDatabin[___]:=Sequence[]
(************************* Utilities ************************)
preparedata[data_]:=data/.HoldPattern[Rule[a_,b_]]:>Rule[a,ToString[b,InputForm]]

(******************)
(* Bulk TimeZoneConvert post 10.0.2 *)

(*
getDates[data_] := Join[data[[All, Key["Timestamp"]]],
  data[[All, Key["SourceInformation"], Key["TimeRecorded"]]],
  data[[All, Key["SourceInformation"], Key["TimeGiven"]]]
  ]

setDates[data_, newdates_] := Block[{n = Length[data], newdata = data},
  newdata[[All, Key["Timestamp"]]] = newdates[[;; n]];
  newdata[[All, Key["SourceInformation"], Key["TimeRecorded"]]] = newdates[[n + 1 ;; 2 n]];
  newdata[[All, Key["SourceInformation"], Key["TimeGiven"]]] = newdates[[2 n + 1 ;;]];
  newdata
  ]
  
convertTimeZones[data_] := Block[{dates},
  dates = getDates[data];
  dates = TimeZoneConvert[dates, $TimeZone];
  setDates[data, dates]
  ]
  
importdrops[drops_]:=convertTimeZones[drops]

 *)
(* Before 10.0.2 *)
importdrops[drops_]:=importdrop/@drops

importdrop[drop_]:=With[{res=Quiet[
	MapAt[timezoneconvert,drop,{{"Timestamp"},{"SourceInformation","TimeRecorded"},{"SourceInformation","TimeGiven"}}]]},
	If[Head[res]===MapAt,drop,res]
]
timezoneconvert[x:(_Missing|None),___]:=x
timezoneconvert[x__]:=DateObject[DateAndTime`DateObjectToDateList[x], TimeZone -> $TimeZone]
(******************)

makedatabin[shortid_,id_,name_, url_]:=(
	datadropclientcache[{"DatabinIDs", id}]=datadropclientcache[{"DatabinIDs", shortid}]=Association[{"UUID"->id, "ShortID"->shortid,"Name"->name,"ShortURL"->url}];
	datadropclientcache[{"DatabinStats", id}]=datadropclientcache[{"DatabinStats", shortid}]=Association[{
		"Entries"->0, "Size"->Quantity[0,"Kilobytes"],"LatestTimestamp"->None,"CreationDate"->timezoneconvert[DateObject[]],"Creator"->$WolframID,"Owner"->$WolframID}];
	System`Databin[shortid])

datadropclientcache[{"DatabinStats", _}]:={}
datadropclientcache[{"DatabinLatest", _}]:={}
datadropclientcache[___]:=$Failed


getKeys[as:(_List|_Association)]:=Keys[as]
getKeys[___]:={}

DataDropClient`DatabinQ[args___]:=Catch[databinQ[args]]

databinQ[db_Databin]:=True
databinQ[link_Hyperlink]:=databinq[First[link]]
databinQ[str_String]:=databinq[str]
databinQ[___]:=False

datadropbinform="*://datadrop.wolfram.com/dd"~~((WordCharacter | "-")..)~~("/"...);

databinq[str_]:=TrueQ[If[StringFreeQ[str,"://"],
	KeyExistsQ[apifun["BinIDs",Association["Bin"->str]],"UUID"],
	If[StringFreeQ[str,"wolfr.am"],
		StringMatchQ[str,datadropbinform],
		StringMatchQ[URLExpand[str],datadropbinform]
	]
]]

(* Messages *)
Databin::nobin="The specified databin `1` could not be found."
DatabinAdd::nobin="A Databin is expected instead of `1`."
Databin::readcon="Connect to the Wolfram cloud using CloudConnect to read the full databin; returning only a recent sample."
Databin::dashcon="Connect to the Wolfram cloud using CloudConnect to get the databin report."
Databin::nocr="A Wolfram Data Drop databin could not be created, please try again later."
Databin::optas="The options should be given as a list of rules or an Association."
Databin::apierr="`1`"
Databins::cloudc="Connect to the Wolfram cloud using CloudConnect to see your bin ids."
CreateDatabin::cloudc="Connect to the Wolfram cloud using CloudConnect to create a databin"
Databin::notav="Wolfram Data Drop is not yet available; check back soon"
Databin::timeout1="The request timed-out. Try to split up the request using the Count and Offset parameters."
Databin::timeout2="The request timed-out."
Databin::seriesn="A series can not be created for the databin with less than two data entries."

(* Results *)
error["nobin",id_]:=(Message[Databin::nobin,id];$Failed)
error["create"]:=(Message[Databin::nocr];$Failed)

End[] (* End Private Context *)

EndPackage[]