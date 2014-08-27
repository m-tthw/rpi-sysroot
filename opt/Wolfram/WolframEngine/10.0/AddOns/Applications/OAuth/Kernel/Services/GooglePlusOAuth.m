
System`Private`NewContextPath[{"OAuthClient`","System`"}];

(* declare functions that will be used by the OAuth paclet *)
OAuthClient`googleplusdata;
OAuthClient`googlepluscookeddata;
OAuthClient`googleplussendmessage;


Begin["GooglePlusOAuthDump`"] (* Begin Private Context *) 

(******************************* GooglePlus *************************************)

(* Authentication information *)

googleplusdata[]={
		"OAuthVersion"		->"2.0",
		"ServiceName"       -> "Google+",
	    "AuthorizeEndpoint" -> "https://accounts.google.com/o/oauth2/auth",
	    "AccessEndpoint"    -> "https://accounts.google.com/o/oauth2/token",
	    "RedirectURI"       -> "urn:ietf:wg:oauth:2.0:oob",
	    "VerifierLabel"      -> "code",
	 	"ClientInfo"		-> {40, 42, 33, 49, 78, 33, 42, 41, 53, 52, 42, 13, 10, 102, 118, 107, 
			194, 165, 62, 45, 35, 88, 43, 121, 60, 101, 83, 86, 36, 36, 41, 62, 
			114, 42, 116, 88, 37, 104, 78, 106, 76, 68, 122, 59, 72, 38, 95, 194, 
			172, 47, 125, 96, 89, 57, 50, 47, 63, 194, 163, 35, 82, 194, 166, 68, 
			90, 86, 32, 47, 120, 112, 79, 194, 165, 114, 34, 66, 65, 90, 49, 55, 
			44, 113, 194, 162, 194, 175, 63, 48, 116, 121, 13, 10, 87, 43, 194, 
			165, 118, 75, 98, 86, 92, 13, 10},
	 	"AuthenticationDialog" -> "TokenDialog",
	 	"Gets"				-> {"UserData","ActivityList","UserSearch","ActivitySearch","ActivityData",
	 		"ActivityPlusOners","ActivityResharers","UserPosts","UserPostsTimeline","UserPostsEventSeries","CircledUsers"},
	 	"Posts"				-> {},
	 	"RawGets"			-> {"RawUserData","RawPeopleSearch","RawPeopleByActivity","RawPeopleByCollection",
	 			"RawActivity","RawActivitySearch","RawComment","RawActivityComments","RawUserMoments","RawUserActivities"},
	 	"RawPosts"			-> {"RawInsertMoment"},
	 	"Scope"				-> {"https%3A%2F%2Fwww.googleapis.com%2Fauth%2Fplus.login&request_visible_actions=https%3A%2F%2Fschemas.google.com%2FAddActivity+https%3A%2F%2Fschemas.google.com%2FReviewActivity+http%3A%2F%2Fschemas.google.com%2FCheckInActivity"},
 		"Information"		-> "A service for receiving data from a Google+ account"
}

(* a function for importing the raw data - usually json or xml - from the service *)
googleplusimport[$Failed]:=Throw[$Failed]
googleplusimport[json_String]:=With[{res=ImportString[json,"JSON"]},
	If[FreeQ[res,_["error",_]],
		assoc@res,
		Message[ServiceExecute::apierr,"message"/.("error"/.res)];
		Throw[$Failed]
	]
]


googleplusimport[raw_]:=raw



(****** Raw Properties ******)
(* information:
 Each entry includes the api endpoint, the HTTP method ("GET" or "POST") as well as the different types of parameters that are used
 "Parameters" - standard URL parameters that appear in the query string as ?param1=val1&param2=val2...
 "PathParameters" - parameters that are included in the url path scheme://domain/path1/`1`/`2`...  make the URL (ToString[StringForm[...,#1,#2]]&) 
 "BodyData" - parameters in HTTP Post requests that are includes as body data
 "MultipartData" - parameters in HTTP Post requests that are includes as multip part data, 
 		usually large files or images are multipart data, each parameter should be given as {"parametername","datatype"} 
 "RequiredParameters" - all above parameters are assumed to be optional, list required parameters a second time here
 "Headers" - additional headers to be included in the HTTP request
 "RequiredPermissions" - If we support incrementally adding permission for a service, list the required permissions for the request here*)
 
(*** Raw ***) 

(** People **)
googleplusdata["RawUserData"] = {
        "URL"				-> (ToString@StringForm["https://www.googleapis.com/plus/v1/people/`1`",formatuserid[##]]&),
        "PathParameters"	-> {"userID"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> googleplusimport
    }

googleplusdata["RawPeopleSearch"] = {
        "URL"				-> "https://www.googleapis.com/plus/v1/people",
        "Parameters"		-> {"query","language","maxResults","pageToken"},
        "RequiredParameters"-> {"query"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> googleplusimport
    }

googleplusdata["RawPeopleByActivity"] = {
        "URL"				-> (ToString@StringForm["https://www.googleapis.com/plus/v1/activities/`1`/people/`2`",ToString[#1],tostring[##2,"plusoners"]]&),
        "PathParameters"	-> {"activityID","collection"},
        "Parameters"		-> {"maxResults","pageToken"},
        "RequiredParameters"-> {"activityID"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> googleplusimport
    }

googleplusdata["RawPeopleByCollection"] = {
        "URL"				-> (ToString@StringForm["https://www.googleapis.com/plus/v1/people/`1`/people/`2`",formatpath[{formatuserid,tostring[##,"visible"]&},##]]&),
        "PathParameters"	-> {"userID","collection"},
        "Parameters"		-> {"maxResults","orderBy","pageToken"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> googleplusimport
    }
    
(** Activities **)
googleplusdata["RawActivity"] = {
        "URL"				-> (ToString@StringForm["https://www.googleapis.com/plus/v1/activities/`1`",ToString[#]]&),
        "PathParameters"	-> {"activityID"},
        "RequiredParameters"-> {"activityID"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> googleplusimport
    }

googleplusdata["RawActivitySearch"] = {
        "URL"				-> "https://www.googleapis.com/plus/v1/activities",
        "Parameters"		-> {"query","language","maxResults","orderBy","pageToken"},
        "RequiredParameters"-> {"query"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> googleplusimport
    }

googleplusdata["RawUserActivities"] = {
        "URL"				-> (ToString@StringForm["https://www.googleapis.com/plus/v1/people/`1`/activities/`2`",formatpath[{formatuserid,tostring[##,"public"]&},##]]&),
        "PathParameters"	-> {"userID","collection"},
        "Parameters"		-> {"maxResults","pageToken"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> googleplusimport
    }    
    
(** Comments **)
googleplusdata["RawComment"] = {
        "URL"				-> (ToString@StringForm["https://www.googleapis.com/plus/v1/comments/`1`",ToString[#]]&),
        "PathParameters"	-> {"commentID"},
        "RequiredParameters"-> {"commentID"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> googleplusimport
    }
    
googleplusdata["RawActivityComments"] = {
        "URL"				-> (ToString@StringForm["https://www.googleapis.com/plus/v1/activities/`1`/comments",ToString[#]]&),
        "PathParameters"	-> {"activityID"},
        "RequiredParameters"-> {"activityID"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> googleplusimport
    }
(** Moments **)
googleplusdata["RawUserMoments"] = {
        "URL"				-> (ToString@StringForm["https://www.googleapis.com/plus/v1/people/`1`/moments/`2`",formatpath[{formatuserid,tostring[##,"vault"]&},##]]&),
        "PathParameters"	-> {"userID","collection"},
        "Parameters"		-> {"maxResults","targetUrl","pageToken","type"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> googleplusimport
    }
    
googleplusdata["RawInsertMoment"] = {
        "URL"				-> (ToString@StringForm["https://www.googleapis.com/plus/v1/people/`1`/moments/`2`",formatpath[{formatuserid,tostring[##,"vault"]&},##]]&),
        "PathParameters"	-> {"userID","collection"},
        "Parameters"		-> {"debug"},
        "BodyData"			-> {"ParameterlessBodyData"},
        "RequiredParameters"-> {"ParameterlessBodyData"},
        "HTTPSMethod"		-> "POST",
      	"Headers" 			-> {"Content-Type" -> "application/json"},
        "ResultsFunction"	-> googleplusimport
    }
    
googleplusdata[___]:=$Failed
(****** Cooked Properties ******)
  
(* cooked data queries 
	Cooked queries call at least one of the raw queried underneath but add pre and post processing to make the function easier to use and the result clearer.
*)  

googlepluscookeddata[prop_,id_,rule_Rule, rest___]:=googlepluscookeddata[prop,id,{rule}, rest]
googlepluscookeddata[prop_,id_]:=googlepluscookeddata[prop,id,{}]
  

(* Cooked *)
googlepluscookeddata["UserData",id_,args_]:=Module[{params,rawdata, data},
	params=filterparameters[args,getallparameters["RawUserData"]];
	debugPrint["params"->params];
	rawdata=OAuthClient`rawoauthdata[id,"RawUserData",params];
	data=googleplusimport[rawdata];
	assoc[Replace[Normal[data],(Rule[a_,b_]):>(Rule[camelcase[a],b]),Infinity]]
]

$googleplusactivitysearchlimit=20;
$googleplususersearchlimit=50;

googlepluscookeddata["UserSearch",id_,args_]:=Module[{params,rawdata, data,limit},
	params=filterparameters[args,getallparameters["RawPeopleSearch"]]/.fixlimit;
	If[FreeQ[params,"query"],Message[ServiceExecute::nparam,"query"];Throw[$Failed]];
	limit=("maxResults"/.params)/."maxResults"->$googleplususersearchlimit;
	If[ToExpression[limit]>$googleplususersearchlimit,
		debugPrint["paging..."];
		rawdata=pagethrough[OAuthClient`rawoauthdata,{id,"RawPeopleSearch",
			Join[FilterRules[params,Except["maxResults"]],{"maxResults"->ToString[$googleplususersearchlimit]}]},
			{ToExpression[limit],$googleplususersearchlimit}];
		debugPrint["paged rawdata"->rawdata];
		data=Join@@(Lookup[googleplusimport[#],"items",{}]&/@rawdata);
		,
		debugPrint["no paging..."];
		rawdata=OAuthClient`rawoauthdata[id,"RawPeopleSearch",params];
		data=googleplusimport[rawdata];
		data=Lookup[data,"items",{}];
	];
	parseuser/@data
]

googlepluscookeddata["ActivitySearch",id_,args_]:=Module[{params,rawdata, data,limit},
	params=filterparameters[args,getallparameters["RawActivitySearch"]]/.fixlimit;
	If[FreeQ[params,"query"],Message[ServiceExecute::nparam,"query"];Throw[$Failed]];
	limit=("maxResults"/.params)/."maxResults"->$googleplusactivitysearchlimit;
		debugPrint["limit"->limit->(ToExpression[limit]>$googleplusactivitysearchlimit)];
	If[ToExpression[limit]>$googleplusactivitysearchlimit,
		debugPrint["paging..."];
		rawdata=pagethrough[OAuthClient`rawoauthdata,{id,"RawActivitySearch",
			Join[FilterRules[params,Except["maxResults"]],{"maxResults"->ToString[$googleplusactivitysearchlimit]}]},
			{ToExpression[limit],$googleplususersearchlimit}];
		data=Join@@(Lookup[googleplusimport[#],"items",{}]&/@rawdata);
		,
		rawdata=OAuthClient`rawoauthdata[id,"RawActivitySearch",params];
		data=googleplusimport[rawdata];
		data=Lookup[data,"items",{}];
	];
	parseactivity/@data
]

(*
googlepluscookeddata["ActivitySearch",id_,args_]:=Module[{params,rawdata, data},
	params=filterparameters[args,getallparameters["RawActivitySearch"]]/.fixlimit;
	If[FreeQ[params,"query"],Message[ServiceExecute::nparam,"query"];Throw[$Failed]];
	debugPrint["params"->params];
	rawdata=OAuthClient`rawoauthdata[id,"RawActivitySearch",params];
	data=googleplusimport[rawdata];
	data=Lookup[data,"items",{}];
	parseactivity/@data
]
*)

googlepluscookeddata["ActivityData",id_,args_]:=Module[{params,rawdata, data},
	params=filterparameters[args,getallparameters["RawActivity"]];
	If[FreeQ[params,"activityID"],Message[ServiceExecute::nparam,"ActivityID"];Throw[$Failed]];
	debugPrint["params"->params];
	rawdata=OAuthClient`rawoauthdata[id,"RawActivity",params];
	data=googleplusimport[rawdata];
	parseactivity[Normal[data]]
]

googlepluscookeddata["UserPosts",id_,args_]:=Module[{types,pos=All,rawdata, data},
	rawdata=googlepluscookeddata["ActivityList",id,args];
	(* types="ObjectType"/.(Lookup[#,"Object",{}]&/@rawdata);
	pos=Flatten[Position[types,"note"]]; *)
	data=Lookup[#,"Object",{}]&/@rawdata[[pos]];
	fromHTML[("Content"/.data)/."Content":>Sequence@@{}]
]

googlepluscookeddata["UserPostsEventSeries",id_,args_]:=Module[{rawdata, data, times},
	rawdata=googlepluscookeddata["ActivityList",id,args];
	times=Lookup[#,"Published",{}]&/@rawdata;
	data=Lookup[#,"Object",{}]&/@rawdata;
	data=fromHTML[("Content"/.data)/."Content":>""];
	debugPrint["times"->times];
	debugPrint["data"->data];
	EventSeries[MapThread[{#1,#2}&,{times,data}]]
]

googlepluscookeddata["UserPostsTimeline",id_,args_]:=Module[{rawdata, data, times},
	rawdata=googlepluscookeddata["ActivityList",id,args];
	times=Lookup[#,"Published",{}]&/@rawdata;
	data=Lookup[#,"Object",{}]&/@rawdata;
	data=fromHTML[("Content"/.data)/."Content":>""];
	debugPrint["times"->times];
	debugPrint["data"->data];
	DateListPlot[MapThread[Tooltip[{#,1},#2]&,{times,data}],Filling->Axis,FrameTicks -> {None, {Automatic, Automatic}}]
]

$googleplusactivitylistlimit=100;

googlepluscookeddata["ActivityList",id_,args_]:=Module[{limit,params,rawdata, data},
	debugPrint["args"->args];
	params=filterparameters[args,getallparameters["RawUserActivities"]]/.fixlimit;
	debugPrint["params"->params];
	limit=("maxResults"/.params)/."maxResults"->$googleplusactivitylistlimit;
		debugPrint["limit"->limit->(ToExpression[limit]>$googleplusactivitylistlimit)];
	If[ToExpression[limit]>$googleplusactivitysearchlimit,
		debugPrint["paging..."];
		rawdata=pagethrough[OAuthClient`rawoauthdata,{id,"RawUserActivities",
			Join[FilterRules[params,Except["maxResults"]],{"maxResults"->ToString[$googleplusactivitylistlimit]}]},
			{ToExpression[limit],$googleplusactivitylistlimit}];
		data=Join@@(Lookup[googleplusimport[#],"items",{}]&/@rawdata);
		,
		rawdata=OAuthClient`rawoauthdata[id,"RawUserActivities",params];
		data=googleplusimport[rawdata];
		data=Lookup[data,"items",{}];
	];	
	parseactivity/@data
]

googlepluscookeddata[prop:("ActivityPlusOners"|"ActivityResharers"),id_,args_]:=Module[{limit,params,rawdata, data, col},
	params=filterparameters[args,getallparameters["RawPeopleByActivity"]]/.fixlimit;
	debugPrint["params"->params];
	col=If[prop==="ActivityPlusOners","plusoners","resharers"];
	limit=("maxResults"/.params)/."maxResults"->$googleplusactivitylistlimit;
		debugPrint["limit"->limit->(ToExpression[limit]>$googleplusactivitylistlimit)];
	If[ToExpression[limit]>$googleplusactivitysearchlimit,
		debugPrint["paging..."];
		rawdata=pagethrough[OAuthClient`rawoauthdata,{id,"RawPeopleByActivity",
			Join[{"collection"->col},FilterRules[params,Except["maxResults"]],{"maxResults"->ToString[$googleplusactivitylistlimit]}]},
			{ToExpression[limit],$googleplusactivitylistlimit}];
		data=Join@@(Lookup[googleplusimport[#],"items",{}]&/@rawdata);
		,
		rawdata=OAuthClient`rawoauthdata[id,"RawPeopleByActivity",Join[{"collection"->col},params]];
		data=googleplusimport[rawdata];
		data=Lookup[data,"items",{}];
	];	
	parseuser/@data
]

$defaultmaxuser=100;

googlepluscookeddata["CircledUsers",id_,args_]:=Module[{params,rawdata, data,limit},
	params=filterparameters[args,getallparameters["RawPeopleByCollection"]]/.fixlimit;
	debugPrint["params"->params];
	limit=("maxResults"/.params)/."maxResults"->$defaultmaxuser;
	If[ToExpression[limit]>$defaultmaxuser,
		rawdata=pagethrough[OAuthClient`rawoauthdata,{id,"RawPeopleByCollection",
			Join[FilterRules[params,Except["maxResults"]],{"maxResults"->ToString[$defaultmaxuser]}]},
			{limit,$defaultmaxuser}];
		debugPrint["paged rawdata"->rawdata];
		data=Join@@(Lookup[googleplusimport[#],"items",{}]&/@rawdata);
		,
		rawdata=OAuthClient`rawoauthdata[id,"RawPeopleByCollection",params];
		data=googleplusimport[rawdata];
		data=Lookup[data,"items",{}];
	];
	parseuser/@data
]


pagethrough[fun_, args_, {n_, n0_}]:=Block[{i,token, res},
	debugPrint["pagethrough 1"];
	res={fun@@args};
	debugPrint["pagethrough 2"];
	token=getpagetoken[res[[1]]];
	debugPrint["pagethrough token"->token];
	If[token=!=$Failed,
		Do[
			res=Join[res,{fun@@Join[args,{"pageToken"->token}]}];
			token=getpagetoken[Last[res]];
			If[token===$Failed,Return[res]],
			{i,Ceiling[n/n0]-1}
		];
		,Return[res]
	];
	res/;res=!=$Failed
]

getpagetoken[json_]:=With[
	{tokens=StringCases[json, "\"nextPageToken\": \"" ~~ (t : Shortest[__]) ~~ "\"" :> t]},
	debugPrint["getpagetoken"->tokens];
	If[Length[tokens]===1,First[tokens],$Failed]
]
(* Send Message *)

googleplussendmessage[___]:=$Failed

(*** Utilities ***)

fixlimit=HoldPattern[Rule["maxResults",l_]]:>Rule["maxResults",ToString[l]];

parseactivity[act_]:=assoc@Replace[FilterRules[Replace[act
		,{(Rule[a_,b_]):>(Rule[camelcase[a],b])},Infinity],{"Actor","URL","Updated","Object","Published","ID"}]/.{
			formatvalue["Updated"->(readDate[#]&)],formatvalue["Published"->(readDate[#]&)]},"ID"->"ActivityID",{2}]
			
parseuser[user_]:=assoc@Replace[FilterRules[Replace[user,
	{(Rule[a_,b_]):>(Rule[camelcase[a],b])},Infinity],{"DisplayName","ID"}]/.{formatvalue["Published"->(readDate[#]&)]},"ID"->"UserID",{2}]
			
formatuserid[]:="me"
formatuserid[Automatic]:="me"
formatuserid[id_]:=ToString[id]

tostring[str_String,_]:=str
tostring[default_]:=default
tostring[Automatic,default_]:=default
tostring[str_,_]:=ToString[str]

readDate[date_,form_:DateObject]:=form[DateList[date]]

getallparameters[str_]:=DeleteCases[Flatten[{"Parameters","PathParameters","BodyData","MultipartData"}/.googleplusdata[str]],
	("Parameters"|"PathParameters"|"BodyData"|"MultipartData")]

(*
fromHTML[str_]:=StringReplace[str, ("&#" ~~ (char : (WordCharacter ..) /; 
      StringLength[char] < 5) ~~ ";") :> (FromCharacterCode@
    FromDigits[char])]
    *)

fromHTML[str_String]:=ImportString[str,"HTML"]   
fromHTML[l:{___String}]:=ImportString[#,"HTML"]  &/@l   
    
	
End[] (* End Private Context *)

SetAttributes[{},{ReadProtected, Protected}];

System`Private`RestoreContextPath[];

(* Return three functions to define oauthservicedata, oauthcookeddata, oauthsendmessage  *)
{OAuthClient`googleplusdata,OAuthClient`googlepluscookeddata,OAuthClient`googleplussendmessage}
