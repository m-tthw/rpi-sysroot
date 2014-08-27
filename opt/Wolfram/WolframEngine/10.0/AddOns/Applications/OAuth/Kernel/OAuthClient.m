
(* ::Package:: *)

(* $Id: OAuthClient.m,v 1.19 2014/01/28 21:51:20 bobs Exp $ *)

(* :Summary:
	A framework for authenticating and exchanging data with OAuth services
*)

(* :Mathematica Version: Mathematica 10.0 *)

(* :Keywords: 
OAuth
*)

(* :Examples:
*)

System`Private`NewContextPath[{"OAuthClient`","System`","HTTPClient`"}];

System`ServiceConnect;
System`ServiceExecute;
System`ServiceDisconnect;
System`SendMessage;
System`ServiceObject;

OAuthClient`ServiceInformation;
OAuthClient`$RegisteredServices;
OAuthClient`saveServiceConnection;
OAuthClient`loadServiceConnection;
OAuthClient`checkpermissions;
OAuthClient`addpermissions;
OAuthClient`rawoauthdata;
OAuthClient`$CacheResults;
OAuthClient`$SaveConnection;
OAuthClient`$SaveConnectionDefault;
OAuthClient`debugPrint;
(*
OAuthClient`definequeries;
*)
(Unprotect[#]; Clear[#])& /@ {
  ServiceConnect,ServiceDisconnect,ServiceExecute,SendMessage,ServiceObject,ServiceInformation,OAuthClient`rawoauthdata,
  OAuthClient`saveServiceConnection,OAuthClient`loadServiceConnection,OAuthClient`checkpermissions,OAuthClient`addpermissions,
  OAuthClient`debugPrint
}

<<HTTPClient`;

Begin["OAuthClientDump`"];

Begin["`Private`"];

$debug=False
debugPrint[args__]:=Print[args/.{("ConsumerSecret"->_):>("ConsumerSecret"->"xxxx")}]/;$debug

oauthservicesdata=OAuthClient`OAuthServicesData;

(* Use the cloud stored client credentials,
	For OAuth 1 this signs urls in the wolfram cloud,
	For OAuth 2 this gets the authorization url and access tokens in the wolfram cloud *)
$OAuthCloudCredentialsQ=False;

$oauthservices:=OAuthClient`$predefinedservicelist;
$authenticatedservices={};

(* Because the many of the services update thier data frequently (i.e. Twitter) caching is false by default.
	In some places where calls are often repeated, this is set to true *)
OAuthClient`$CacheResults=False;

(* Store access tokens locally on in the cloud. Automatic uses the cloud only if the user is already connected *)
OAuthClient`$ServiceStorageLocation=Automatic;

(* default save condition *)
OAuthClient`$SaveConnectionDefault=False;
OAuthClient`$SaveConnection=False;

$RegisteredServices:=$authenticatedservices

(* Messages *)
ServiceConnect::done="The service `1` has already been authenticated";
ServiceConnect::oauthver="The OAuth version should be 1.0 or 2.0";
ServiceConnect::url="The given url `1` is not a valid string";
ServiceConnect::skey="The key or secret `1` must be a valid string";
ServiceConnect::dialog="The value `1` is not valid for AuthenticationDialog";
ServiceConnect::reqform="The value `1` is not valid for RequestFormat";
ServiceConnect::token="Could not obtain a request token for the OAuth service `1`";
ServiceExecute::nolink="The service `1` is not authenticated. Try using ServiceConnect."
ServiceConnect::multst="There are multiple connections stored in the `1` directory. Please specify an id."
ServiceConnect::nost="A specified connection could not be found. Try to create a new connection."
ServiceConnect::nostc="The given connection id is not stored in your cloud."
ServiceConnect::nsave="The connection could not be saved."
(*
defineQueries::nolink="The service `1` is not authenticated. Try using ServiceConnect."
*)
ServiceExecute::nargs="The number of arguments given does not match the number of slots in the url template."
ServiceExecute::nparam="The parameter `1` is required"
ServiceConnect::unkn="The service `1` is unknown, try providing authentication options."
ServiceConnect::multser="One service was chosen from multiple `1` services."
ServiceObject::noget="The parameter `1` is not available for the service `2`."
ServiceExecute::apierr="The service returned the following error message: `1`."
ServiceExecute::ndata="The returned data is missing the `1` value."
ServiceInformation::nolink="The service `1` is not authenticated. Try using ServiceConnect."
ServiceConnect::nameid="The given connection id `1` is corresponds to a different service than the specified service, `2`."

(************************************** ServiceConnect **********************************)
ServiceConnect[args___]:=With[{res=Catch[authenticate[args]]},
	res/;res=!=$Failed
]

authenticate["Services"]:=Sort[$oauthservices]

authenticate[service_,rest___]:=oauthauthenticate[service,rest]/;MemberQ[$oauthservices,service]

authenticate[service_, rest__]:=oauthauthenticate[service, rest]/;!FreeQ[{rest},"OAuthVersion"]

authenticate[service_, ___]:=(Message[ServiceConnect::unkn, service];$Failed)

oauthauthenticate[name_,rest___]:=With[{service=oauthauthenticate1[name,rest],
	(* OAuthClient`$SaveConnection is set by the dialog window during authentication *)
	save="SaveServiceConnection"/.Flatten[Cases[{rest},_?OptionQ,Infinity]]/."SaveServiceConnection"->OAuthClient`$SaveConnection},
	debugPrint["save"->save];
	Switch[save,
		"SaveServiceConnection"|False|None,Null, (* default *)
		_,saveServiceConnection[service, save]
	];
	OAuthClient`$SaveConnection=OAuthClient`$SaveConnectionDefault;
	service
]

oauthauthenticate1[name_,"New",rest___]:=(Internal`DeleteCache[{"OAuthTokens", name}];
	debugPrint["oauthauthenticate1 New 1"];
	newoauthauthenticate[name, rest])

oauthauthenticate1[name_,connection_String,rest___]:=If[MemberQ[$authenticatedservices,connection],
	If[serviceName[connection]===name,
		getServiceObject[connection],
		Message[ServiceConnect::nameid,connection,name];$Failed
	],
	cloudoauthauthenticate[name,connection, rest]
]

oauthauthenticate1[name_,rest___]:=With[{authorized=checkservicelist[$authenticatedservices, name, ServiceConnect]},
	If[authorized===$Failed,
		cloudoauthauthenticate[name, rest],
		authorized
	]
]

cloudoauthauthenticate[name_,___?OptionQ]:=cloudoauthauthenticate[name,Automatic]

cloudoauthauthenticate[name_,id_,rest___]:=With[{savedconns=loadServiceConnection[name,id]},
	debugPrint["trying to load a connection"];
	debugPrint["savedconns"->savedconns];
	If[savedconns==="Multiple",Throw[$Failed]];
	Switch[savedconns,
		{_,(_HTTPClient`OAuth`Private`Token20|_HTTPClient`OAuth`Private`Token10)},
			Block[{HTTPClient`OAuth`Private`OAuthFlow, makeuuid},
				makeuuid[]=savedconns[[1]];
	debugPrint["rest"->{rest}];
				HTTPClient`OAuth`Private`OAuthFlow[___]=savedconns[[2]];
				newoauthauthenticate[name, rest]
			],
		{_,_ServiceObject},savedconns[[2]],
		$Failed,If[id===Automatic,newoauthauthenticate[name, rest],$Failed],
		_,$Failed
	]
]
	
newoauthauthenticate[name_,___]:=Module[{service, rawgets=oauthservicesdata[name, "RawGets"], 
	gets=oauthservicesdata[name, "Gets"],rawposts=oauthservicesdata[name, "RawPosts"], posts=oauthservicesdata[name, "Posts"], id, info},
	debugPrint["newoauthauthenticate 1"];
	info=getclientinfo[name];
	info=If[!MatchQ[info,{_String,_String}],
		Throw[$Failed],
		{"ConsumerKey"->info[[1]],"ConsumerSecret"->info[[2]]}
	];
	debugPrint["newoauthauthenticate 3"];
	service=newunknownoauthauthenticate[name,Join[oauthservicesdata[name, "Authentication"],info]];
	debugPrint["newoauthauthenticate 5"];
	id=getServiceID[service];
	(* predefinedqueries[service,#]&/@rawgets;
	   predefinedqueries[service,#]&/@rawposts;
	   *)
	   
	serviceRawRequests[id]=Sort[Flatten[{serviceRawRequests[id],rawgets}]];
	serviceRawPosts[id]=Sort[Flatten[{serviceRawPosts[id],rawposts}]];
	
	serviceRequests[id]=Sort[Flatten[{serviceRequests[id],gets}]];
	servicePosts[id]=Sort[Flatten[{servicePosts[id],posts}]];
	
	logout[id]=oauthservicesdata[name,"LogoutURL"];
	service	
]/;MemberQ[OAuthClient`$predefinedservicelist,name]

newoauthauthenticate[name_,rest___]:=newunknownoauthauthenticate[name, rest]

otheroauthoptions={"AccessVerb", "CodeExtractor",
"RequestTokenExtractor", "RequestVerb", "ScopeDomain",
"ScopeParameter", "SignatureMethod", "URLSignService","VerifierLabel","ResponseType"};

extraoauth2opts={"CodeExtractor", "AccessEndpoint", "AccessVerb", "ScopeDomain"}
extraoauth1opts={"RequestVerb", "CodeExtractor", "AccessEndpoint", "AccessVerb", 
"URLSignService", "SignatureMethod","AccessTokenExtractor", "ScopeParameter"}

Options[newunknownoauthauthenticate]={
	"ServiceName"       -> Null,
    "OAuthVersion"		-> "1.0a",
    "RequestEndpoint"   -> "",
    "AccessEndpoint"    -> Null,
    "AuthorizeEndpoint" -> Null,
    "ConsumerKey"		-> Null,
   	"ConsumerSecret"	-> Null,
   	"RedirectURI"		->	"oob",
   	"AdditionalOAuthParameter"	-> None,
   	"Scope"				-> None,
    "AuthenticationDialog" 	-> "TokenDialog",
    "RequestFormat"		-> "URL",
    "LogoutURL"			-> Null,
    "Information"		-> "",
    "AccessTokenExtractor" -> None,
    "tokenread"			-> Identity
}
defaultOAuthOptions={
	"ServiceName"       -> Null,
    "OAuthVersion"		-> "1.0a",
    "RequestEndpoint"   -> "",
    "AccessEndpoint"    -> Null,
    "AuthorizeEndpoint" -> Null,
    "ConsumerKey"		-> Null,
   	"ConsumerSecret"	-> Null,
   	"RedirectURI"		->	"oob",
   	"AdditionalOAuthParameter"	-> None,
   	"Scope"				-> None,
    "AuthenticationDialog" 	-> "TokenDialog",
    "RequestFormat"		-> "URL",
    "LogoutURL"			-> Null,
    "Information"		-> "",
    "AccessTokenExtractor" -> None,
    "tokenread"			-> Identity
};

newunknownoauthauthenticate[name_,opts___]:=Module[{version,authurl,requrl,accessurl,key,
	secret,redirect,dialogfun, token,urlfetchfun, service, id,dialog,requestformat,info,logouturl,
	extra,scope,atokenext,extraopts,params},
	
	params={
		"OAuthVersion","AuthorizeEndpoint","RequestEndpoint","AccessEndpoint","ConsumerKey","ConsumerSecret",
		"RedirectURI","AuthenticationDialog","RequestFormat","Information",
		"LogoutURL","AdditionalOAuthParameter","Scope","AccessTokenExtractor"};
	
	{version,authurl, requrl, accessurl,key, secret,redirect, dialog, 
		requestformat, info,logouturl, extra,scope, atokenext}=params/.Flatten[{opts}]/.defaultOAuthOptions;
	
	extraopts=FilterRules[Flatten[{opts}],Except[params]];
	
	
	If[!MatchQ[version,"1.0"|"1.0a"|"2.0"|"1"|"2"|1|2|1.|2.],
		Message[ServiceConnect::oauthver,version];Throw[$Failed]];
	If[!StringQ[#],Message[ServiceConnect::url,#];Throw[$Failed]]&/@{authurl, requrl, accessurl};
	If[!StringQ[#],Message[ServiceConnect::skey,#];Throw[$Failed]]&/@{key, secret};
	If[!StringQ[redirect],Message[ServiceConnect::url,redirect];Throw[$Failed]];
	dialogfun=Switch[dialog,
		"TokenDialog",
		OAuthClient`tokenOAuthDialog[#, name]&,
		"TokenlessDialog",
		OAuthClient`notokenOAuthDialog[#, name]&,
		Except[_String],dialog,
		_,
		Message[ServiceConnect::dialog,dialogfun];Throw[$Failed]
	];
	
	debugPrint["newunknownoauthauthenticate 3"];
	token=newAuthenticate[name,version,authurl, requrl, accessurl,key,secret,
		redirect,{extra,scope},dialogfun,atokenext,extraopts];
		
	urlfetchfun=Switch[requestformat,
		"URL",
		URLFetch,
		"Headers"|{"Headers",_},
		(With[{newurl=fromURLtoAuthorizationHeaders[{##}, version,requestformat]},
			URLFetch@@newurl
		]&),
		_Function,
		requestformat,
		_,
		Message[ServiceConnect::reqform,requestformat];Throw[$Failed]
	];
	
	debugPrint["urlfetchfun"->urlfetchfun];
	debugPrint["newunknownoauthauthenticate 9"];
	service=createOAuthObject[name,token];
	id=getServiceID[service];
	debugPrint["newunknownoauthauthenticate 11"];
	urlfetchFun[id]=urlfetchfun;
	debugPrint["newunknownoauthauthenticate 12"];
	tokenread[id]=Identity;
	debugPrint["newunknownoauthauthenticate 13"];
	serviceInfo[id]=info;
	debugPrint["newunknownoauthauthenticate 14"];
	logout[id]=logouturl;
	debugPrint["newunknownoauthauthenticate 15"];
	
	service
]
	
	
newAuthenticate[name_,version_,authurl_, requrl_, accessurl_,key_,secret_,redirect_,
	{additionalparam_,scope_},dialogfun_, accesstokenext_,extraopts_]:=Module[
	{token, parameters,oauthflowdef,resetflowdef=False},
	debugPrint["newAuthenticate 1"];
	debugPrint["newAuthenticate extraopts"->extraopts];
	parameters=
		If[MatchQ[version,"1"|"1.0"|"1.0a"|1|1.],
			Join[
			{
		        "ServiceName"       -> name,
		        "OAuthVersion"		-> "1.0a",
		        "RequestEndpoint"   -> requrl,
		        "AccessEndpoint"    -> accessurl,
		        "AuthorizeEndpoint" -> authurl,
		        "ConsumerKey"		-> key,
		       	"ConsumerSecret"	->	secret,
		        "AuthenticationDialog" -> dialogfun,
		        If[accesstokenext=!=None,
		        	"AccessTokenExtractor"->accesstokenext,Sequence@@{}]
		    },
		    	FilterRules[extraopts,extraoauth1opts]
			],
			Join[
		    {
			    "ServiceName"       -> name,
		        "OAuthVersion"		-> "2.0",
			    "AuthorizeEndpoint" -> authurl,
			    "AccessEndpoint"    -> accessurl,
			    "RedirectURI"       -> redirect,
			    "ConsumerKey"       -> key,
			    "ConsumerSecret"	 -> secret,
			  	"AuthenticationDialog" -> dialogfun,
		      	If[accesstokenext=!=None,
		        	"AccessTokenExtractor"->accesstokenext,Sequence@@{}]
			},
		    	FilterRules[extraopts,extraoauth2opts]
			]
		];
    
	debugPrint["newAuthenticate 4"];
    Switch[{additionalparam,scope},
    	{_Rule,None|{}},
    	(* Add additional parameter to request and access token calls *)
    	If[version==="2.0",Throw[$Failed]];
    	resetflowdef=True;
    	oauthflowdef=DownValues[HTTPClient`OAuth`Private`OAuthFlow];
    	parameters=Join[parameters,{"ScopeParameter"	-> 	additionalparam[[1]]}];
		DownValues[HTTPClient`OAuth`Private`OAuthFlow] = 
  			Join[{HoldPattern[HTTPClient`OAuth`Private`OAuthFlow][auth_] :> HTTPClient`OAuth`Private`OAuthFlow[auth, {additionalparam[[2]]}]}, 
  				oauthflowdef],
  		{None,{__}},
	  		If[version=!="2.0",Throw[$Failed]];
	    	resetflowdef=True;
	    	oauthflowdef=DownValues[HTTPClient`OAuth`Private`OAuthFlow];
			DownValues[HTTPClient`OAuth`Private`OAuthFlow] = 
	  			Join[{HoldPattern[HTTPClient`OAuth`Private`OAuthFlow][auth_] :> HTTPClient`OAuth`Private`OAuthFlow[auth, scope]}, 
	  				oauthflowdef],
  		{None,None|{}},Null,
  		_,Message[ServiceConnect::addparam,addparam];Throw[$Failed]
    ];
    
	token=tokenread[name]@getauthtoken[parameters];
	If[resetflowdef,
		DownValues[HTTPClient`OAuth`Private`OAuthFlow]=oauthflowdef
	];
    If[Head[token] =!= HTTPClient`OAuthToken,  Message[ServiceConnect::token, name];Throw[$Failed]];
    token
]

authenticationfunction[]:=If[$OAuthCloudCredentialsQ,
	oauthCloudCredentials,
	HTTPClient`OAuthAuthentication
]

getauthtoken[parameters_] :=
     Block[{name="ServiceName"/.parameters, token},
    	
         token = Internal`CheckCache[{"OAuthTokens", name}];
      (*    If[token === $Failed,
         	token=checkCloudOAuth[name]; *)
         	If[Head[token] =!= HTTPClient`OAuthToken,
         		debugPrint["parameters"->parameters];
	         	token = authenticationfunction[][parameters];
	            If[token === $Canceled, Return[$Canceled]];
	            If[Head[token] =!= HTTPClient`OAuthToken, Return[$Failed]];
	            Internal`SetCache[{"OAuthTokens", name}, token](*;
	         	 Do automatically or require "save"? saveCloudOAuth[name, token]
         	] *)
         ];
		token
     ]
     
(************************************** ServiceDisconnect *****************************)
ServiceDisconnect[args___]:=With[{res=Catch[servicedisconnect[args]]},
	res/;res=!=$Failed]
	
servicedisconnect[service_ServiceObject]:=Module[
	{name=getServiceName[service], id=getServiceID[service], link},
	Internal`DeleteCache[{"OAuthTokens", name}];
	
	DownValues[rawoauthdata]=DeleteCases[DownValues[rawoauthdata], _?(!FreeQ[#, id] &)];
	DownValues[parsedata]=DeleteCases[DownValues[parsedata], _?(!FreeQ[#, id] &)];
	
	(*
	(* Delete stored file ? *)
	deleteCloudFile[name];
	*)
	
	serviceName[id]=None;
	serviceRawRequests[id]={};
	serviceRequests[id]={};
	serviceRawPosts[id]={};
	servicePosts[id]={};
	serviceAuthentication[id]={};
	urlfetchFun[id]=URLFetch;
	serviceInfo[id]="";
	
	link=hyperlink[logout[id]];
	logout[id]=Null;
	
	$authenticatedservices=DeleteCases[$authenticatedservices,id];
	link
	
]/;authenticatedServiceQ[service]


servicedisconnect[___]:=$Failed
	
	
	
(************************************** ServiceObject **********************************)
(* objects can be used as pure functions on certain parameter values *)
(service_ServiceObject)[args___]:=With[{res=Catch[serviceobjectdata[service, args]]},
	res/;res=!=$Failed
]

serviceobjectdata[service_, param_, rest___]:=Module[{},
	If[MemberQ[serviceobjectRequests[service],param],
		externalservice[service, param, rest]
		,
		Message[ServiceObject::noget,param, getServiceName[service]];Throw[$Failed]
	]
]


serviceobjectdata[___]:=$Failed


(************************************** ExternalService **********************************)
ServiceExecute[args___]:=With[{res=Catch[externalservice[args]]},
	res/;res=!=$Failed
]

externalservice[service_ServiceObject,rest___]:=(
	oauthdata[service, rest])/;authenticatedServiceQ[service]

externalservice[name_String,rest___]:=With[{service=checkservicelist[$authenticatedservices, name, ServiceExecute]},
	If[service===$Failed,
		If[MemberQ[$oauthservices,name],
			oauthdata[authenticate[name], rest],
			Throw[$Failed]
		]
		,
		externalservice[service,rest]
	]
	
]

externalservice[___]:=$Failed

(* Special Requests *)
$specialRequests={"Authentication", "ID", "Information", "Name", "Requests","RawRequests"};

oauthdata[service_ServiceObject,"Name",___]:=getServiceName[service]
oauthdata[service_ServiceObject,"Requests",___]:=With[{id=getServiceID[service]},
	Sort[DeleteCases[Join[serviceRequests[id],servicePosts[id],	$specialRequests],"Requests"]]]
oauthdata[service_ServiceObject,"RawRequests",___]:=With[{id=getServiceID[service]},
	Sort[Join[serviceRawRequests[id],serviceRawPosts[id]]]]
oauthdata[service_ServiceObject,"ID",___]:=getServiceID[service]
oauthdata[service_ServiceObject,"Information",___]:=serviceinfo[service]
oauthdata[service_ServiceObject,"Authentication",___]:=With[{auth=serviceAuthentication[getServiceID[service]]},
	parseToken[auth,getServiceName[service]]
]

parseToken[token_,name_]:=parseToken0[
	Cases[token,(p_HTTPClient`OAuth`Private`OAuth10Parameters|p_HTTPClient`OAuth`Private`OAuth20Parameters):>p,Infinity],
	name]

parseToken0[{params_HTTPClient`OAuth`Private`OAuth10Parameters},name_]:=
	{"OAuthVersion"		->	"1.0",
	"RequestEndpoint"	->	params[[9]],
	"AuthorizeEndpoint"	->	params[[11]],
	"AccessEndpoint"	->	params[[13]],
	Sequence@@If[MemberQ[OAuthClient`$predefinedservicelist,name],
		{},
		{"ConsumerKey"->params[[6]],"ConsumerSecret"->params[[7]]}
	]
	}
	
parseToken0[{params_HTTPClient`OAuth`Private`OAuth20Parameters},name_]:=
	{"OAuthVersion"		->	"2.0",
	"AuthorizeEndpoint"	->	params[[9]],
	"AccessEndpoint"	->	params[[11]],
	Sequence@@If[MemberQ[OAuthClient`$predefinedservicelist,name],
		{},
		{"ConsumerKey"->params[[7]],"ConsumerSecret"->params[[8]]}
	]
	}
	
parseToken[___]:=Throw[$Failed]

oauthdata[service_ServiceObject,property_,rest___]:=Module[{raw, id=getServiceID[service]},
	If[!authenticatedServiceQ[service],
			Message[ServiceExecute::nolink, service];Throw[$Failed]];
	id=getServiceID[service];
	
	If[MemberQ[Join[serviceRequests[id],servicePosts[id]], property],
		OAuthClient`oauthcookeddata[getServiceName[service], property, id,rest]
		,
		raw=rawoauthdata[id,property,rest];
		parsedata[id,property]@raw
	]
]

oauthdata[args___]:=$Failed

rawoauthdata[id_,parameter_,rest_]:=rawoauthdata[id,parameter,{rest}]/;!ListQ[rest]

rawoauthdata[id_,url0_String]:=Module[{url, res},
	debugPrint["direct URL oauthdata "];
		url=getsignedurl[url0,serviceAuthentication[id]];
		If[url === $Failed, Throw[$Failed]];
		If[url === $Canceled, Return[$Canceled]];
        (
     		res = urlfetchFun[id]@@url;
     		res /; (res =!= $Failed)
        ) /; (url =!= $Failed)
	]/;!MemberQ[availablequeries[id],url0]


rawoauthdata[id_,property_,rest___]:=Module[
		{url0,method,pathparams,params,bodyparams,mpdata,headers,reqparams,
			url, res, auth, tmp, pvpairs=Flatten[{rest}], params1, bodyparams1,mpdata1,headers1
			,reqperms, missingperms, oauth1Q},	
		If[OAuthClient`$CacheResults,
			res = Internal`CheckCache[{"OAuth", {id, property, rest}}];
			If[res =!= $Failed, Return[res]];
		];
		{url0,method,pathparams,params,bodyparams,mpdata,headers,reqparams, reqperms}=getQueryData[id, property];	
		(* Check the required permissions *)
		If[reqperms=!={},
			missingperms=If[grantedpermissions[id]===All,{},
				With[{updated=updatepermissions[id]},
					Cases[reqperms,_?(!MemberQ[updated,#]&),1]
				]
			];
			(* Try to add any missing permissions *)
			If[missingperms=!={},
				If[requestpermissions[id,missingperms]===$Failed,
					Throw[$Failed]]
			];
		];		
		
		(* check for required parameters *)
		If[!MemberQ[First/@pvpairs,#],
			Message[ServiceExecute::nparam,#];Throw[$Failed]
		]&/@reqparams;
		
		
		(* Path Parameters use a StringForm Function *)
		url=If[Head[url0]===Function,
			insertpathparameters[url0,pathparams,pvpairs],
			url0
		];
			
		params1=Cases[params,_?(!FreeQ[pvpairs,#]&)];	
		params1=Thread[params1->(params1/.pvpairs)];	
		
		bodyparams1=Cases[bodyparams,_?(!FreeQ[pvpairs,#]&)];
		bodyparams1=Thread[bodyparams1->(bodyparams1/.pvpairs)];
			
		mpdata1=Cases[mpdata,_?(!FreeQ[pvpairs,First[#]]&)];
		mpdata1=formatMultipartData[mpdata1,pvpairs];
		
		auth=serviceAuthentication[id];
		oauth1Q=FreeQ[auth,HTTPClient`OAuth`Private`Token20|HTTPClient`OAuth`Private`OAuth20Parameters];
		(* Set POST or GET *)
		If[oauth1Q,
			url=getsignedurl[url,auth,"Parameters"->Join[params1, bodyparams1], "Method"->method],
			url=getsignedurl[url,auth,"Parameters"->params1,"BodyData"->bodyparams1, "Method"->method]
		];
	
		If[!MatchQ[url,_String|{_String,___}],Throw[$Failed]];
		If[method==="POST",
			If[oauth1Q,
				tmp=cutoutparameters1[url[[1]], bodyparams];
				url[[1]]=tmp[[1]];
				url=Join[url,{"BodyData"->tmp[[2]], "MultipartData"->mpdata1}],
				
				tmp=cutoutparameters2[Rest[url], bodyparams];
				tmp=tmp/.HoldPattern[Rule["BodyData",bd:(_Rule|{_Rule...})]]:>Rule["BodyData",URLQueryEncode[bd]];
				url=If[mpdata1==={},
					Join[{url[[1]]},tmp],
					Join[{url[[1]]},tmp,{"MultipartData"->mpdata1}]
				];					
				(* workaround for parameter issue *)
				url[[1]]=URLBuild[url[[1]],"Parameters"/.Rest[url]];		
	debugPrint["rawoauthdata url 1.6"->url];	
				url=DeleteCases[url,Rule["Parameters",_]];				
			]
		];
		
		If[headers=!={},
			(* Headers should have default values, check for given values *)
			headers1=If[FreeQ[pvpairs,First[#]],#,First[#]->(First[#]/.pvpairs)]&/@headers;
			url=Join[url,{"Headers"->headers1}]
		];
		
	debugPrint["rawoauthdata url"->url];		
	debugPrint["rawoauthdata urlfetchFun[id]"->urlfetchFun[id]];	
		If[url === $Canceled, Return[$Canceled]];
        (
     		res=urlfetchFun[id]@@url;
	debugPrint["rawoauthdata res"->res];	
     		(If[OAuthClient`$CacheResults,Internal`SetCache[{"OAuth", {id, property, rest}}, res]]; res) /; (res =!= $Failed)
        ) /; (url =!= $Failed)
	]/;property=!="Authentication"&&MemberQ[Join[serviceRawRequests[id],serviceRawPosts[id]], property]


rawoauthdata[___]:=Throw[$Failed]

parsedata[id_,property_]:=(("ResultsFunction"/.oauthservicesdata[serviceName[id],property])/."ResultsFunction"->Identity
	)/;MemberQ[Join[serviceRawRequests[id],serviceRawPosts[id]], property]
	
parsedata[__]:=Identity


insertpathparameters[url0_,pathparams_,pvpairs_]:=Module[{given, pparams1},
		given=Cases[pathparams,_?(!FreeQ[pvpairs,#]&)];
		
	debugPrint["insertpathparameters given"->given];
		pparams1=If[Length[given]<Length[pathparams],
			(Replace[pathparams,Except[Alternatives@@given]->Automatic,{1}]/.{before___,Automatic..}:>{before})/.pvpairs
			,
			pathparams/.pvpairs
		];
	debugPrint["insertpathparameters pparams1"->pparams1];
	debugPrint["insertpathparameters url0@@(pparams1)"->(url0@@(pparams1))];
		
		Check[url0@@(pparams1),Message[ExternalService::nargs];Throw[$Failed]]
]

(******************** ServiceInformation **********)
ServiceInformation[args___]:=With[{res=Catch[serviceinfo[args]]},
	res/;res=!=$Failed
]

serviceinfo[service_ServiceObject,rest___]:=(serviceInfo[getServiceID[service]])/;authenticatedServiceQ[service]

serviceinfo[service_,___]:=(Message[ServiceInformation::nolink,service];$Failed)

serviceInfo[id_]:=""/;!authenticatedServiceQ[id]

(****************** SendMessage *******************)

SendMessage[args___]:=With[{res=Catch[sendmessage[args]]},
	res/;res=!=$Failed]
	
sendmessage[name_String,rest___]:=With[{service=checkservicelist[$authenticatedservices, name, ServiceExecute]},
	If[service===$Failed,
		If[MemberQ[$oauthservices,name],
			sendmessage[authenticate[name], rest],
			Message[SendMessage::nolink,name];Throw[$Failed]
		]
		,
		sendmessage[service,rest]
	]
]

sendmessage[service_ServiceObject,rest__]:=With[{},
	OAuthClient`oauthsendmessage[getServiceName[service],getServiceID[service],rest]
]/;authenticatedServiceQ[service]

sendmessage[___]:=$Failed

(**************** Manage Permissions *************)
grantedpermissions[id_]:={}/;!authenticatedServiceQ[id]

grantedpermissions[id_]:=(grantedpermissions[id]=OAuthClient`checkpermissions[serviceName[id],id])

updatepermissions[id_]:=(grantedpermissions[id]=OAuthClient`checkpermissions[serviceName[id],id])/;authenticatedServiceQ[id]

requestpermissions[id_,p_]:=(OAuthClient`addpermissions[serviceName[id],id,p])/;authenticatedServiceQ[id]

updatepermissions[___]:=$Failed
requestpermissions[___]:=$Failed

(****************** Utilities *********************)

appendservicelist[service_]:=($oauthservices=Union[Flatten[{$oauthservices,getServiceName[service]}]])
appendauthservicelist[service_]:=($authenticatedservices=Union[Flatten[{$authenticatedservices,service}]])

makeuuid[]:=StringJoin["connection-",IntegerString[RandomInteger[{0, 16^32}], 16, 32]]

createOAuthObject[name_, token_, id0_:Automatic]:=Module[{link, id},
	id=If[id0===Automatic,makeuuid[], id0];
	link=ServiceObject[name, "ID"->id];
	appendservicelist[link];
	appendauthservicelist[id];
	
	serviceName[id]=name;
	serviceRawRequests[id]={};
	serviceRequests[id]={};
	serviceRawPosts[id]={};
	servicePosts[id]={};
	serviceAuthentication[id]=token;
	urlfetchFun[id]=URLFetch;
	tokenread[id]=Identity;
	
	link
]

getQueryData[id_,property_]:=With[{data=oauthservicesdata[serviceName[id],property]},
	(* URL, method, pathparams,params, bodyparams, mpdata, headers, optionalparams *)
	{("URL"/.data)/."URL"->"",
	("HTTPSMethod"/.data)/."HTTPSMethod"->"GET",
	listwrap@(("PathParameters"/.data)/."PathParameters"->{}),
	listwrap@(("Parameters"/.data)/."Parameters"->{}),
	listwrap@(("BodyData"/.data)/."BodyData"->{}),
	listwrap@(("MultipartData"/.data)/."MultipartData"->{}),
	listwrap@(("Headers"/.data)/."Headers"->{}),
	listwrap@(("RequiredParameters"/.data)/."RequiredParameters"->{}),
	listwrap@(("RequiredPermissions"/.data)/."RequiredPermissions"->{})}
]

setQueryData[id_,Rule[prop_,data_]]:=getQueryData[id,prop]=data

servicedata[id_]:=assoc[{
	"ServiceName"->serviceName[id],
	"ID"->id,
	"RawRequests"->serviceRawRequests[id],
	"Requests"->serviceRequests[id],
	"RawPostRequests"->serviceRawPosts[id],
	"PostRequests"->servicePosts[id],
	"Authentication"->serviceAuthentication[id],
	"Information"->serviceInfo[id]
}]

formatMultipartData[mpdata_,pvpairs_]:={#[[1]],#[[2]],#[[1]]/.pvpairs}&/@mpdata

availablequeries[id_]:=Join[serviceRequests[id],serviceRawRequests[id],servicePosts[id],serviceRawPosts[id],
	$specialRequests]/;authenticatedServiceQ[id]
availablequeries[_]:={}

getServiceObject[id_]:=ServiceObject[serviceName[id],"ID"->id]

getServiceName[ServiceObject[args___]]:=First[{args}]
getServiceID[service_ServiceObject]:="ID"/.Cases[service,_Rule,{1}]

serviceName[id_]:=None/;!authenticatedServiceQ[id]
serviceRawRequests[id_]:={}/;!authenticatedServiceQ[id]
serviceRequests[id_]:={}/;!authenticatedServiceQ[id]
serviceRawPosts[id_]:={}/;!authenticatedServiceQ[id]
servicePosts[id_]:={}/;!authenticatedServiceQ[id]
serviceAuthentication[id_]:={}/;!authenticatedServiceQ[id]

tokenread[id_]:=Identity/;!authenticatedServiceQ[id]

urlfetchFun[id_]:=URLFetch/;!authenticatedServiceQ[id]
logout[id_]:=Null/;!authenticatedServiceQ[id]

hyperlink[str_String]:=Hyperlink[str];
hyperlink[___]:=Null

serviceobjectRequests[service_]:=With[{id=getServiceID[service]},
	Join[serviceRawRequests[id],serviceRequests[id],$specialRequests]
]

authenticatedServiceQ[service_ServiceObject]:=authenticatedServiceQ[getServiceID[service]]
authenticatedServiceQ[id_]:=MemberQ[$authenticatedservices,id]

checkservicelist[list_, name_String, fn_]:=With[{matches=Select[list,serviceName[#]===name&]},
	Switch[Length[matches],
		1,getServiceObject[First[matches]],
		0,$Failed,
		_,Message[fn::multser,name];(*getServiceObject[First[matches]]*) Throw[$Failed]
	]
]

fromURLtoAuthorizationHeaders[url_,"2.0"|"2", header0_]:=Module[{params, token,headers, 
	rest=Rest[url],header=If[ListQ[header0],header0[[2]],"Oauth"]},
	params="Parameters"/.rest/."Parameters"->{};
  	headers=("Headers"/.rest)/."Headers"->{};
	token="access_token"/.params;
	params=FilterRules[params,Except["access_token"]];
	{First[url],"Headers"->Join[headers,{"Authorization"->(header<>" "<>token)}],"Parameters"->params,Sequence@@FilterRules[rest,Except["Parameters"|"Headers"]]}
]

			
fromURLtoAuthorizationHeaders[url_,__] := Module[{base, auth, headers},
  	{base, auth} = StringSplit[First[url], "?"];
  	headers=("Headers"/.Rest[url])/."Headers"->{};
	{base,"Headers"->Join[headers,{
		"Authorization"->"Oauth " <> StringReplace[auth, {"=" -> "=\"", "&" -> "\","}] <> "\""}],Rest[url]}
  ]
  
  

cutoutparameters1[str_, {}]:={str,""}
cutoutparameters1[str_, params0_] := Module[
	{tmp, url, body, url0,params},
	params=Join[params0,URLEncode/@params0];
  	tmp = StringSplit[str, {"?", "&"}];
  	tmp =  GatherBy[tmp, (StringFreeQ[#, StringJoin[#, "="] & /@ params] &)];
   	{url0,body}=If[Length[tmp]===1,
   		{First[tmp],{}},
   		tmp
   	];
  	url = First[url0];
  	If[Length[url0] > 1,
   		url = url <> "?" <> url0[[2]];
   		If[Length[url0] > 2,
    		url = StringJoin[url,"&", ##] & @@ Riffle[Drop[url0, 2], "&"]
    	]
   	];
   	StringReplace[body=StringJoin[Riffle[body, "&"]],"ParameterlessBodyData*="->""];
  	{url, body}
  	]  
  	
cutoutparameters2[opts_, {}]:=opts
cutoutparameters2[opts_, params0_] := Module[
	{body,params, urlparams,body0},
	params=Join[params0,URLEncode/@params0];
	body0="BodyData"/.opts;
	urlparams="Parameters"/.opts;
	body=DeleteCases[urlparams,_?(FreeQ[#,Alternatives@@params] &)];
	body=Join[body0,body]/.HoldPattern[Rule["ParameterlessBodyData",x_]]:>x;(*
	body=URLEncode[body/.({"ParameterlessBodyData"->x_}:>x)];*)
	urlparams=DeleteCases[urlparams, _?(!FreeQ[#,Alternatives@@params] &)];
  	Join[{"Parameters"->urlparams, "BodyData"->body},DeleteCases[opts,Rule["Parameters"|"BodyData",_]]]
  	]  
  	
listwrap[l_List]:=l
listwrap[x_]:={x}

nothing := Sequence @@ {}

serviceStatus[service_]:="Authenticated"/;authenticatedServiceQ[service]
serviceStatus[_]:="Unknown"

(****************************** Token Storage *************************************)
saveServiceConnection[service_ServiceObject, location_]:=Module[{rescloud, reslocal},
	debugPrint["trying to save a connection"];
	rescloud=If[MatchQ[location,All|"Cloud"]||(MatchQ[location,Automatic|True]&&$CloudConnected),
		saveServiceConnectionCloud[service],
		$Failed
	];
	If[MatchQ[location,All|"Local"]||(MatchQ[location,Automatic|True]&&rescloud===$Failed),
		reslocal=saveServiceConnectionLocal[service]
	];
	If[rescloud===reslocal===$Failed, Message[ServiceConnect::nsave];$Failed]
]

$StoreFullOAuthConnections=False;
createConnectionData[service_]:=Module[{id=getServiceID[service]},
	Join[Normal[servicedata[id]],
			{"urlfetch"->urlfetchFun[id],"logout"->logout[id],
			"RawPropertyData"->((#->getQueryData[id,#])&/@Join[serviceRawRequests[id],serviceRawPosts[id]])
			}]	
]

createConnectionTokenData[service_]:=Module[{id=getServiceID[service]},
	Last[serviceAuthentication[id]]
]

saveServiceConnectionLocal[service_ServiceObject]:=Module[{id=getServiceID[service], name=getServiceName[service], 
		dir, file, temp=FileNameJoin[{$TemporaryDirectory,"m"<>ToString[RandomInteger[1000]]<>".txt"}], data},
	debugPrint["trying to save a connection locally"];
	debugPrint["saving service "<>ToString[id]<>" locally"];
	dir=FileNameJoin[{$UserBaseDirectory,"Connections","Services",name}];
	file=FileNameJoin[{dir,id<>".txt"}];
	If[!DirectoryQ[dir],
		CreateDirectory[dir]
	];	
	If[FileExistQ[file],DeleteFile[file]];
	data=If[$StoreFullOAuthConnections,createConnectionData,createConnectionTokenData][service];
	Put[data,temp];
	Encode[temp,file];
	DeleteFile[temp];	
] 

saveServiceConnectionCloud[service_]:=Block[
	{id=getServiceID[service], name=getServiceName[service], co, file, temp1, temp2, data,
	OAuthUtilitiesDump`Private`deobflag=True, res,current},
	debugPrint["saving service "<>ToString[id]<>" on the cloud"];
	If[!$CloudConnected,
		CloudConnect[]
	];
	If[!$CloudConnected,Return[$Failed]];
	co=CloudObject["connections/services/"<>name];
	debugPrint["co"->co];
	current=Quiet[Import[co,"JSON"]];
	debugPrint["current"->current];
	If[current===$Failed,
		current={},
		current=DeleteCases[current,Rule[id,_]]
	];
	data=If[$StoreFullOAuthConnections,createConnectionData,createConnectionTokenData][service];
	data=OAuthClient`ob[data];
	res=Export[co,Append[current,id->data],"JSON"];	
	res/;res=!=$Failed
]

saveServiceConnectionCloud[___]:=$Failed

loadServiceConnection[name_,id_:Automatic]:=Module[{res},
	debugPrint["loadServiceConnection 0"];
	Switch[OAuthClient`$ServiceStorageLocation,
		Automatic|All (* default *),
		If[$CloudConnected,
			res=loadServiceConnectionCloud[name, id];
			If[res===$Failed,loadServiceConnectionLocal[name, id],res]
			,
			res=loadServiceConnectionLocal[name, id];
			If[res===$Failed&&(id=!=Automatic||OAuthClient`$ServiceStorageLocation===All),
				loadServiceConnectionCloud[name, id],res]
		],
		"Cloud",loadServiceConnectionCloud[name, id],
		"Local",loadServiceConnectionLocal[name, id]
	]
]

loadServiceConnectionLocal[name_, fileid_]:=Module[{tmp, dir,file, files},
	debugPrint["looking for file "<>ToString[fileid]<>" locally"];
	dir=FileNameJoin[{$UserBaseDirectory,"Connections","Services",name}];
	If[DirectoryQ[dir],
		If[MatchQ[fileid,Automatic|"Connections"],
			files=FileNames["connection-*.txt", dir];
	debugPrint["found files  "<>ToString[files]<>" locally"];
			If[fileid==="Connections",Throw[FileBaseName/@files]];
			Switch[Length[files],
				1,file=First[files],
				0,Return[$Failed],
				_,Message[ServiceConnect::multst,dir];Return["Multiple"](* TODO: How to choose which connection *)
			],
			file=FileNameJoin[{dir,fileid<>".txt"}];
			If[!FileExistsQ[file],
				Message[ServiceConnect::nost,dir];Return[$Failed]
			]
		];
		debugPrint["file"->file];
		tmp=Get[file];
		{FileBaseName@file,parseStoredConnection[tmp]}
		,
		$Failed		
	]
]

loadServiceConnectionCloud[name_, fileid_]:=Block[
	{co, file, data,
	OAuthUtilitiesDump`Private`deobflag=True, res,stored,fileid1},
	debugPrint["looking for file "<>ToString[fileid]<>" on the cloud"];
	If[!$CloudConnected,
		CloudConnect[]
	];
	If[!$CloudConnected,Return[$Failed]];
	co=CloudObject["connections/services/"<>name];
	stored=Quiet[Import[co,"JSON"]];
	debugPrint["stored1"->stored];
	If[stored===$Failed,Return[$Failed]];
	data=If[MatchQ[fileid,Automatic|"Connections"],
		If[fileid==="Connections",
			Throw[If[Length[stored]>0,
				First/@stored,{}]]];
		Switch[Length[stored],
				1,List@@stored[[1]],
				0,Return[$Failed],
				_,Message[ServiceConnect::multst,dir];Return["Multiple"](* TODO: How to choose which connection *)
			],
		stored=Cases[stored,HoldPattern[fileid->_],1];
		If[stored==={},Message[ServiceConnect::nost];Return[$Failed]];
		If[MatchQ[stored,{_Rule}],List@@stored[[1]]]
	];
	If[!ListQ[data],Return[$Failed],{fileid1, data}=data];
	data=OAuthClient`deob[data];
	res={fileid1, data};
	res/;res=!=$Failed
]

loadServiceConnectionCloud[___]:=$Failed

parseStoredConnection[tmp:{_Rule..}]:=Module[{service,servicename,id},
		servicename="ServiceName"/.tmp;
		id="ID"/.tmp;
		service=createOAuthObject[servicename, "Authentication"/.tmp, id];
		serviceRawRequests[id]="RawRequests"/.tmp;
		serviceRequests[id]="Requests"/.tmp;
		serviceRawPosts[id]="RawPostRequests"/.tmp;
		servicePosts[id]="PostRequests"/.tmp;
		urlfetchFun[id]="urlfetch"/.tmp;
		tokenread[id]="tokenread"/.tmp;
		serviceInfo[id]:="Information"/.tmp;
		logout[id]="logout"/.tmp;
		
		setQueryData[id,#]&/@("RawPropertyData"/.tmp);
		
		service
]
parseStoredConnection[tmp:(_HTTPClient`OAuth`Private`Token20|_HTTPClient`OAuth`Private`Token10)]:=tmp

deleteLocalFile[name_]:=If[FileExistsQ[name<>"OAuth.txt"],
		DeleteFile[name<>"OAuth.txt"]
		,$Failed]
(********************************** ServiceObject Typesetting ******************************)

(*** probably need another option? or just check *Values?  *)
simplePropQ[x_] := NumberQ[x] || StringQ[x] || MemberQ[{True,False,Automatic},x]

paddedRow[e_,margins_:{{0,0},{0,5}},opts___?OptionQ] := Row[e,opts,ImageMargins->margins]

greenLight[] := Graphics[
	{{LightGreen, Disk[{0, 0}]}, {RGBColor[0, .85, 0], Circle[{0, 0}]}}
	, 
	PlotRange->{-2.2, 1.1},
	ImageSize -> 8
]


ServiceObject/:
MakeBoxes[service_ServiceObject, form:StandardForm|TraditionalForm] := 
With[{name=getServiceName[service], id=getServiceID[service]},
	If[$VersionNumber>=10,
		BoxForm`ArrangeSummaryBox[
			(* Head *)ServiceObject, 
			(* Interpretation *)service, 
			(* Icon *)OAuthClient`defaultServiceConnectIcon, 
			(* Column or Grid *){name, (* id, *) Dynamic[If[TrueQ[authenticatedServiceQ[id]],"Connected","Not Connected"]]}, 
			(* Plus Box Column or Grid *){"Requests"}, 
			form]
			,
		InterpretationBox[#,service]&@ToBoxes[Framed@Row[{"ServiceObject       ",Column[{name, id, "Authenticated"->authenticatedServiceQ[id]}]}]]
	]
]



(*********************** Cloud Stored Client credentials *************************)
oauthCloudCredentials[parameters_]:=Block[{name=("ServiceName"/.parameters)},
	If[MemberQ[OAuthClient`$predefinedservicelist,name],
		If[MatchQ[("OAuthVersion"/.parameters),"1.0"|"1.0a"|"1"|1],
			oauth10CloudCredentials[parameters],
			oauth20CloudCredentials[parameters]
		],
	HTTPClient`OAuthAuthentication[parameters]
	]
]

(* OAuth 2.0 *)
cloudAuthorizationBaseURL="http://www.devel.wolframcloud.com/api/bobs/oauth20authurlapi/AssemAuthURLapi";
cloudAccessBaseURL="http://www.devel.wolframcloud.com/api/bobs/oauth20authurlapi/accessToken20api";

cloudassembleauthurl[rules_,scope_,state_]:=Block[{url, json},
	url=URLBuild[cloudAuthorizationBaseURL,Join[rules,{"scope"->scope,"state"->state}]];
	json=URLFetch[url];
	url="result"/.ImportString[json,"JSON"];
	If[(!StringQ[url])||MatchQ[url,"result"|"$Failed"],Throw[$Failed]];
	url
]
cloudassembleauthurl[___]:=$Failed

cloudaccesstoken[rules_,verifier_,state_]:=Block[{url, json,accesstoken},
	url=URLBuild[cloudAccessBaseURL,Join[rules,{"verifier"->verifier,"state"->state}]];
	json=URLFetch[url];
	accesstoken="result"/.ImportString[json,"JSON"];
	(accesstoken=ImportString[accesstoken,"JSON"];
	HTTPClient`OAuth`Private`Token20@@accesstoken)/;StringQ[accesstoken]&&accesstoken=!="result"
]

cloudaccesstoken[args___]:=(debugPrint["cloudaccesstoken failed to match"->{args}];$Failed)
	
preparescope[{}|None]:="None"
preparescope[str_String]:=str
preparescope[{str_String}]:=str
preparescope[l:{_String..}]:=StringJoin[Riffle[l,"+"]]

authToAuthRules20[auth_]:={
	"ServiceName"->auth[[1]],
	"AuthorizeEndpoint"->auth[[9]],
	"AccessEndpoint"->auth[[11]],
	"RedirectURI"->auth[[13]]
	}

authToAccRules20[auth_]:={
	"ServiceName"->auth[[1]],
	"AuthorizeEndpoint"->auth[[9]],
	"AccessEndpoint"->auth[[11]],
	"AccessVerb"->auth[[12]],
	"AccessTokenExtractor"->auth[[14]],
	"RedirectURI"->auth[[13]],
	"VerifyPeer"->ToString[auth[[3]]]
	}
	
oauth20CloudCredentials[parameters_]:=Block[
	{HTTPClient`OAuth`Private`assembleAuthorizationURL20, HTTPClient`OAuth`Private`getAccessToken20},
	HTTPClient`OAuth`Private`assembleAuthorizationURL20[
		auth_HTTPClient`OAuth`Private`OAuth20Parameters, token_, scope_, state_]:=(
		debugPrint["oauth20CloudCredentials scope"->scope];cloudassembleauthurl[
		authToAuthRules20[auth],preparescope[scope],state]);
	debugPrint["oauth20CloudCredentials 1"];
	HTTPClient`OAuth`Private`getAccessToken20[auth_, token_, verifier_, state_]:=(
		debugPrint[Hold[cloudaccesstoken][authToAccRules20[auth],verifier,state]];
		cloudaccesstoken[authToAccRules20[auth],verifier,state]);
	HTTPClient`OAuthAuthentication[parameters]	
]

(* OAuth 1.0 *)
cloudSignBaseURL="http://www.devel.wolframcloud.com/api/bobs/oauth20authurlapi/cloudSignURL"

cloudsignurl[name_,unsignedURL_, signatureMethod_, accessVerb_, consumerKey_, consumerSecret_, keyStr_, secretStr_]:=Block[{url, json},
	url=URLBuild[cloudSignBaseURL,{
		"name"->name,"unsignedURL"->unsignedURL,"signatureMethod"->signatureMethod,"accessVerb"->accessVerb,
		"consumerKey"->consumerKey,"consumerSecret"->consumerSecret,"keyStr"->keyStr,"secretStr"->secretStr}];
	json=URLFetch[url];
		debugPrint["cloudsignurl signed json"->json];
	url="result"/.ImportString[json,"JSON"];
	If[(!StringQ[url])||MatchQ[url,"result"|"$Failed"],Throw[$Failed]];
		debugPrint["cloudsignurl signed url"->url];
	url=ImportString[url,"JSON"];
	If[(!StringQ[url])||MatchQ[url,"$Failed"],Throw[$Failed]];
	url
]

oauth10CloudCredentials[parameters_]:=Block[
	{HTTPClient`OAuth`Private`HMACSha1SignatureService, name="ServiceName"/.parameters},
	HTTPClient`OAuth`Private`HMACSha1SignatureService[
		unsignedURL_, signatureMethod_, accessVerb_, consumerKey_, consumerSecret_, keyStr_, secretStr_]:=cloudsignurl[
		name,unsignedURL,signatureMethod,accessVerb,consumerKey, consumerSecret, keyStr, secretStr];
	HTTPClient`OAuthAuthentication[parameters]	
]

getsignedurl[url_,auth_, opts___]:=Block[
	{HTTPClient`OAuth`Private`HMACSha1SignatureService, name=auth[[1,1]], version},
	HTTPClient`OAuth`Private`HMACSha1SignatureService[
		unsignedURL_, signatureMethod_, accessVerb_, consumerKey_, consumerSecret_, keyStr_, secretStr_]:=cloudsignurl[
		name, unsignedURL,signatureMethod,accessVerb,consumerKey, consumerSecret, keyStr, secretStr];
	HTTPClient`OAuthSignURL[url, "OAuthAuthentication" -> auth, opts]
]/;$OAuthCloudCredentialsQ&&!FreeQ[auth,_HTTPClient`OAuth`Private`OAuth10Parameters]

getsignedurl[url_,auth_, opts___]:=HTTPClient`OAuthSignURL[url, "OAuthAuthentication" -> auth, opts]

End[];
End[];

SetAttributes[{
  ServiceConnect,ServiceDisconnect,ServiceExecute,SendMessage,ServiceObject,ServiceInformation,
  saveServiceConnection,loadServiceConnection (*,definequeries *)
},
   {ReadProtected, Protected}
];

System`Private`RestoreContextPath[];

{System`ServiceConnect,ServiceDisconnect,ServiceInformation,
System`ServiceExecute,System`SendMessage,
System`ServiceObject (*,
OAuthClient`saveServiceConnection,OAuthClient`loadServiceConnection,OAuthClient`definequeries *)}
