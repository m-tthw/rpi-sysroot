
System`Private`NewContextPath[{"OAuthClient`","System`"}];

(* declare functions that will be used by the OAuth paclet *)
OAuthClient`linkedindata;
OAuthClient`linkedincookeddata;
OAuthClient`linkedinsendmessage;


Begin["LinkedInOAuthDump`"] (* Begin Private Context *) 

(******************************* LinkedIn *************************************)

(* Authentication information *)

linkedindata[]={
		"OAuthVersion"		->"2.0",
		"ServiceName" 		-> "LinkedIn", 
	 	"AuthorizeEndpoint" -> "https://www.linkedin.com/uas/oauth2/authorization", 
     	"AccessEndpoint"    -> "https://www.linkedin.com/uas/oauth2/accessToken",
     	"RedirectURI"       -> "https://user.wolfram.com/oauth/facebook_catch.php",
     	"VerifierLabel"     -> "code",
	 	"ClientInfo"		-> {40, 42, 33, 49, 78, 33, 42, 41, 53, 52, 42, 13, 10, 102, 118, 194, 
			163, 194, 173, 39, 45, 43, 41, 120, 91, 67, 100, 36, 114, 70, 194, 
			166, 103, 116, 85, 39, 92, 126, 123, 37, 74, 69, 60, 97, 52, 39, 118, 
			93, 58, 85, 54, 38, 37, 98, 39, 62, 112, 194, 167, 42, 124, 76, 81, 
			13, 10},
	 	"AuthenticationDialog" -> "TokenDialog",
	 	"Gets"				-> {"UserData","Connections","ConnectionIDs","EgoNetwork","GroupNames"},
	 	"Posts"				-> {"Share"},
	 	"RawGets"			-> {"RawUserData","RawPeopleSearch","RawUserGroups","RawUserGroup","RawGroups","RawGroupPosts","RawSuggestedGroups","RawConnections"},
	 	"RawPosts"			-> {"RawJoinGroup","RawShare"},
	 	"RequestFormat"		-> (Block[{params=Cases[{##},("Parameters"->x_):>x,Infinity], 
	 		url=DeleteCases[{##},"Parameters"->_,Infinity],
	 		method=Cases[{##},("Method"->x_):>x,Infinity]},
	 		debugPrint["method"->method];
	 		If[method==={"GET"},
	 			URLFetch@@({Sequence@@url, "Parameters"->Flatten[{params,"format"->"json"}]}/."access_token"->"oauth2_access_token"),
	 			url[[1]]=StringReplace[url[[1]],"access_token"->"oauth2_access_token"];
	 			URLFetch@@{Sequence@@url, "Parameters"->Flatten[{params}]}
	 		]
	 	]&),
	 	"Scope"				-> {"r_basicprofile r_fullprofile r_network rw_nus rw_groups r_contactinfo"},
 		"Information"		-> "A service for receiving data from a LinkedIn account"
}

(* a function for importing the raw data - usually json or xml - from the service *)
linkedinimport[$Failed]:=Throw[$Failed]
linkedinimport[json_String]:=With[{res=ImportString[json,"JSON"]},
	If[FreeQ[res,_["errorCode",_]],
		assoc@res,
		Message[ServiceExecute::apierr,"message"/.res];
		Throw[$Failed]
	]
]

linkedinimportxml[$Failed]:=Throw[$Failed]
linkedinimportxml[json_String]:=With[{res=ImportString[json,"XML"]},
	If[FreeQ[res,_["errorCode",_]],
		res,
		Message[ServiceExecute::apierr,Cases[res,XMLElement["errorCode",__],Infinity]];
		Throw[$Failed]
	]
]

linkedinimport[raw_]:=raw
linkedinimportxml[raw_]:=raw



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
linkedindata["RawUserData"] = {
        "URL"				-> (ToString@StringForm["https://api.linkedin.com/v1/people/`1`:`2`",formatpath[{formatuserid,formatuserfields},##]]&),
        "PathParameters"		-> {"UserID","Fields"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> linkedinimport
    }

linkedindata["RawPeopleSearch"] = {
        "URL"				->  (ToString@StringForm["https://api.linkedin.com/v1/people-search:`1`",formatusersearchfields[##]]&),
        "Parameters" 		-> {"keywords", "first-name", "last-name", "school-name", "company-name","current-company","title","current-title","postal-code","distance","count","sort"},
        "RequiredParameters" 		-> {"keywords"|"first-name"|"last-name"|"school-name"|"company-name"|"current-company"|"title"|"current-title"|"postal-code"|"distance"},
        "PathParameters"		-> {"Fields"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> linkedinimport
    }
    
linkedindata["RawConnections"] = {
        "URL"				-> (ToString@StringForm["https://api.linkedin.com/v1/people/`1`/connections:`2`",formatpath[{formatuserid,formatuserfields},##]]&),
        "PathParameters"	-> {"UserID","Fields"},
        "Parameters" 		-> {"start", "count", "modified", "modified-since"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> linkedinimport
    }
 
linkedindata["RawUserGroups"] = {
        "URL"				-> (ToString@StringForm["https://api.linkedin.com/v1/people/`1`/group-memberships:`2`",formatpath[{formatuserid,formatgroupmfields},##]]&),
        "PathParameters"	-> {"UserID","Fields"},
        "Parameters" 		-> {"start", "count","membership-state"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> linkedinimport
    }
 
linkedindata["RawUserGroup"] = {
        "URL"				-> (ToString@StringForm["https://api.linkedin.com/v1/people/`1`/group-memberships/`2`:`3`",
        	formatpath[{formatuserid,formatuserfields,formatgroupmfields},##]]&),
        "Parameters" 		-> {"start", "count","group-id","person-id"},
        "PathParameters"	-> {"UserID","GroupID","Fields"},
        "RequiredParameters" -> {"GroupID"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> linkedinimport
    }
 
(** Groups **)   
 linkedindata["RawGroups"] = {
        "URL"				-> (ToString@StringForm["https://api.linkedin.com/v1/groups/`1`:`2`",formatpath[{formatgroupid,formatgroupfields},##]]&),
        "PathParameters"	-> {"GroupID","Fields"},
        "RequiredParameters" -> {"GroupID"},
        "Parameters" 		-> {"start", "count"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> linkedinimport
    }

linkedindata["RawGroupPosts"] = {
        "URL"				-> (ToString@StringForm["https://api.linkedin.com/v1/groups/`1`/posts:`2`",formatpath[{formatgroupid,formatgrouppostfields},##]]&),
        "PathParameters"	-> {"GroupID","Fields"},
        "RequiredParameters" -> {"GroupID"},
        "Parameters" 		-> {"start", "count"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> linkedinimport
    }

linkedindata["RawJoinGroup"] = {
        "URL"				-> (ToString@StringForm["https://api.linkedin.com/v1/people/`1`/group-memberships/`2`",formatpath[{formatuserid,formatgroupmfields},##]]&),
        "PathParameters"	-> {"UserID","GroupID"},
        "RequiredParameters" -> {"GroupID"},
        "HTTPSMethod"		-> "POST",
        "ResultsFunction"	-> linkedinimport
    }

linkedindata["RawSuggestedGroups"] = {
        "URL"				-> (ToString@StringForm["https://api.linkedin.com/v1/people/`1`/suggestions/groups:`2`",formatpath[{formatuserid,formatgroupfields},##]]&),
        "PathParameters"	-> {"UserID","Fields"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> linkedinimport
    }

linkedindata["RawShare"] = {
        "URL"				-> "https://api.linkedin.com/v1/people/~/shares",
        "BodyData"			-> {"ParameterlessBodyData"},
        "HTTPSMethod"		-> "POST",
      	"Headers" 			-> {"Content-Type" -> "application/xml"},
        "ResultsFunction"	-> linkedinimportxml
    }     
    
linkedindata[___]:=$Failed
(****** Cooked Properties ******)
  
(* cooked data queries 
	Cooked queries call at least one of the raw queried underneath but add pre and post processing to make the function easier to use and the result clearer.
*)  

linkedincookeddata[prop_,id_,rule_Rule, rest___]:=linkedincookeddata[prop,id,{rule}, rest]
linkedincookeddata[prop_,id_]:=linkedincookeddata[prop,id,{}]
  
(* Cooked *)
linkedincookeddata["Share",id_,args_]:=Module[{params,rawdata, data},
	debugPrint["linkedincookeddata Share args"->args];
	params=toxmlcomment[args];
	debugPrint["linkedincookeddata Share params"->params];
	rawdata=OAuthClient`rawoauthdata[id,"RawShare",params];
	data=linkedinimportxml[rawdata];
	data=Cases[data,XMLElement["update-key", __]|XMLElement["update-url", {}, {_}],Infinity];
	If[data==={},
		$Failed,
		data/.XMLElement[a_,_,b_]:>Rule[a,b]
	]
]

userdatafields={"id", "formatted-name", "headline", "industry", "distance", 
"current-status", "current-share", "num-connections","picture-url", "public-profile-url"};
	
linkedincookeddata["UserData",id_,args_]:=Module[{params,rawdata, data},
	debugPrint["linkedincookeddata Share args"->args];
	params=filterparameters[args,getallparameters["RawUserData"]];
	params=If[FreeQ[params,"UserID"->_],
		Flatten[{params,"UserID"->"~","Fields"->userdatafields}],
		Flatten[{params,"Fields"->userdatafields}]
		];
	debugPrint["linkedincookeddata Share params"->params];
	rawdata=OAuthClient`rawoauthdata[id,"RawUserData",params];
	data=Normal@linkedinimport[rawdata];
	Association[Thread[(camelcase[userdatafields,{"_","-"}])->((first[getcamelcased[data,#]]&/@userdatafields))]]
]
	
linkedincookeddata[prop:("Connections"|"ConnectionIDs"),id_,args_]:=Module[{params,rawdata, data, field},
	field=If[prop==="Connections","formatted-name","id"];
	params=filterparameters[args,getallparameters["RawConnections"]];
	params=If[FreeQ[params,"UserID"->_],
		Flatten[{params,"UserID"->"~","Fields"->{field}}],
		Flatten[{params,"Fields"->{field}}]
		];
	debugPrint["linkedincookeddata Share params"->params];
	rawdata=OAuthClient`rawoauthdata[id,"RawConnections",params];
	data=linkedinimport[rawdata];
	Flatten[getcamelcased[Lookup[data,"values",{field->{}}],field,Infinity]]
]
	
linkedincookeddata["EgoNetwork",id_,args_]:=Module[{names,ids,root, rawdata,edges,vertices,labels,res,pos},
	rawdata=OAuthClient`rawoauthdata[id,"RawConnections",Join[args,{"Fields"->{"id","formatted-name"}}]];
	rawdata=linkedinimport[rawdata];
	rawdata=Lookup[rawdata,"values",{"formatted-name"->{},"id"->{}}];
	debugPrint["length rawdata"->(Length/@{rawdata})];
	names=first[getcamelcased[#,"formatted-name"]]&/@rawdata;
	ids=first[getcamelcased[#,"id"]]&/@rawdata;
	debugPrint["lengths"->(Length/@{ids, names})];
	rawdata=linkedincookeddata["UserData",id];
	root=Lookup[rawdata,#,""]&/@{"FormattedName","ID"};
	pos = Union[Flatten@Join[Position[ids, "private"], Position[names, "private"]]];
	ids = ReplacePart[ids, Thread[pos -> Sequence[]]];
	names = ReplacePart[names, Thread[pos -> Sequence[]]];
	edges = DirectedEdge[root[[2]], #] & /@ ids;
	ids = Join[{root[[2]]}, ids];
	names = Join[{root[[1]]}, names];
	debugPrint["lengths"->(Length/@{ids, names})];
    {vertices,labels} =
        Transpose[MapThread[{Property[#1, "name" -> #2] , (#1 -> Placed[#2, Tooltip])} &, {ids, names}]];
    res = Graph[vertices, edges, VertexLabels -> labels, GraphLayout -> "StarEmbedding"];
    res /; GraphQ[res]
]


linkedincookeddata["GroupNames",id_,args_]:=Module[
	{rawdata,groups},
	rawdata=OAuthClient`rawoauthdata[id,"RawUserGroups",Flatten[{args,"Fields"->{"group:(id,name)"}}]];
	rawdata=linkedinimport[rawdata];
	groups="group"/.rawdata["values"];
	groups=DeleteCases[groups,"group",Infinity];
	If[Flatten[groups]==={},Return[{}]];
	groups /; (groups =!= $Failed)
]

linkedincookeddata[___]:=$Failed 
(* Send Message *)

linkedinsendmessage[id_,message_String]:=linkedincookeddata["Share",id,"Message"->message]

linkedinsendmessage[___]:=$Failed

(**** Available fields ****)
(*** Profiles ***)
profilebasicfields={"id","first-name","last-name","maiden_name","formatted-name","phoentic-first-name","phoentic-last-name",
	"formatted-phonetic-name","headline","location:{name}","location:(country:(code))","industry","distance","relation-to-viewer:(distance)",
	"current-share","num-connections","num-connections-capped","summary","specialties","positions","picture-url",
	"site-standard-profile-request","api-standard-profile-request:(url)","api-standard-profile-request:(headers)","public-profile-url"};
profileemailfields={"email-address"};
profilefullfields={"last-modified-timestamp","proposal-comments","associations","interests","publications","patents","languages","skills","certifications",
	"educations","courses","volunteer","three-current-positions","three-past-positions","num-recommenders","recommendations-received","mfeed-rss-url",
	"following","job-bookmarks","suggestions","date-of-birth","member-url-resources","member-url-resources:(url)","member-url-resources:(name)","related-profile-views","honors-awards"};
profilecontactinfofields={"phone-numbers","bound-account-types","im-accounts","main-address","twitter-accounts","primary-twitter-account"};
profileconnectionfields={"connections"};
profilegroupmembershipfields={"group-memberships"};
profilepositionfields={"id","title","summary","start-date","end-date","is-current","company"};
profilecompanyfields={"id","name","type","size","industry","ticker"};
profilepublicationfields={"id","title","publisher:(name)","authors:(id)","authors:(name)","authors:(person)","date","url","summary"};
profilepatentfields={"id","title","summary","number","status:(id)","status:(name)","office:(name)","inventors:(id)","inventors:(name)","inventors:(person)","date","url"};
profilelanguagefields={"id","language:(name)","proficiency:(level)","proficiency:(name)"};
profileskillsfields={"id","skill:(name)"};
profilecertificationfields={"id","name","authority:(name)","number","start-date","end-date"};
profileeducationfields={"id","school-name","field-of-study","start-date","end-date","degree","activities","notes"};
profilecoursesfields={"id","name","number"};
profilevolunteerfields={"id","role","organization:(name)","cause:(name)"};
profilerecommendationfields={"id","recommendation-type","recommendation-text","recommender"};

(*** Groups ***)
groupoutputfields={"id","name","short-description","description","relation-to-viewer:(membership-state)",
	"relation-to-viewer:(available-actions)","posts","counts-by-category","is-open-to-non-members","category",
	"website-url","site-group-url","locale","location:(country)","location:(postal-code)","allow-member-invites"
	,"small-logo-url","large-logo-url","num-members"};

grouppostoutputfields={"id","type","category","creator","title",
	"summary","creation-timestamp","relation-to-viewer:(is-following)","relation-to-viewer:(is-liked)"
	,"relation-to-viewer:(available-actions)","likes","comments","attachment","site-group-post-url"};

groupcommentoutputfields={"id","text","creator","creation-timestamp","relation-to-viewer:(available-actions)"};

groupmembershipoutputfields={"person","group:(id)","group:(name)","membership-state","show-group-logo-in-profile",
	"allow-messages-from-members","email-digest-frequency","email-announcements-from-managers","email-for-every-new-post","posts"};
	
(*** Service specific utilites ****)
(* "Sat, 21 Aug 2010 22:31:20 +0000" *)
readDate[date_, form_: DateString] := 
 form@DateList[{StringSplit[date, {" +"," -"}][[1]], 
 	{"DayName", "Day", "MonthNameShort","Year", "Hour", ":", "Minute", ":",
      "Second"}}]

formatfield[str_String]:=str
formatfield[l_List]:=StringJoin["(",Riffle[l,","],")"]

formatuserid[Automatic|"Me"|"me"]:="~";
formatuserid[id_String]:="id="<>id
formatuserid[]:="~"

formatuserfields[]:="(id,formatted-name,headline,industry,distance,current-status,current-share,num-connections,summary,positions,picture-url,public-profile-url)"
formatuserfields[args___]:=formatfield[args]

formatusersearchfields[]:="(people:(id,first-name,last-name,formatted-name),num-results)"
formatusersearchfields[args___]:=formatfield[args]

formatgroupid[id_String]:=id
formatgroupid[int_Integer]:=ToString[int]
formatgroupid[___]:=(Message[ServiceExecute::nparam,"id"];Throw[$Failed])

formatgroupfields[]:="(id,name,short-description,posts,category,website-url,site-group-url,num-members)"
formatgroupfields[args___]:=formatfield[args]

formatgroupmfields[]:="(person,group:(id,name),membership-state)"
formatgroupmfields[args___]:=formatfield[args]

formatgrouppostfields[]:="(id,type,category,creator,title,summary,creation-timestamp,likes,comments,site-group-post-url)"
formatgrouppostfields[args___]:=formatfield[args]

getallparameters[str_]:=DeleteCases[Flatten[{"Parameters","PathParameters","BodyData","MultipartData"}/.linkedindata[str]],
	("Parameters"|"PathParameters"|"BodyData"|"MultipartData")]

toxmlcomment[args_]:=Block[{message,visibility, opts=Cases[args,_?OptionQ,Infinity]},
	message="Message"/.opts;
	visibility="Visibility"/.opts/."Visibility"->"anyone";
	Flatten@{"ParameterlessBodyData"->toxml["share"->{"comment"->message,"visibility"->{"code"->visibility}}],FilterRules[args,Except["Message"|"Visibility"]]}
]

toxml[rules_] := 
 "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" <> 
  ExportString[
   rules //. Rule[a_, b_] :> XMLElement[a, {}, Flatten[{b}]], "XML"]
  
getcamelcased[data_,key_, rest___]:=With[{cc=camelcase[key,{"_","-"}]},
	Cases[data,((key|cc|((ToLowerCase[StringTake[#,1]]<>StringDrop[#,1])&@cc))->x_):>x,rest]
]

first[{}]={};
first[l_]:=First[l]

End[] (* End Private Context *)

SetAttributes[{},{ReadProtected, Protected}];

System`Private`RestoreContextPath[];

(* Return three functions to define oauthservicedata, oauthcookeddata, oauthsendmessage  *)
{OAuthClient`linkedindata,OAuthClient`linkedincookeddata,OAuthClient`linkedinsendmessage}
