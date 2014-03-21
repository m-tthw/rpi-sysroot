
(* ::Package:: *)

(* $Id: OAuthClient.m,v 1.27 2013/11/18 19:45:11 bobs Exp $ *)

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
System`ServiceInformation;

OAuthClient`$RegisteredServices;
OAuthClient`saveServiceConnection;
OAuthClient`loadServiceConnection;
OAuthClient`checkpermissions;
OAuthClient`addpermissions;
(*
OAuthClient`definequeries;
*)
(Unprotect[#]; Clear[#])& /@ {
  ServiceConnect,ServiceDisconnect,ServiceExecute,SendMessage,ServiceObject,ServiceInformation,
  OAuthClient`saveServiceConnection,OAuthClient`loadServiceConnection,OAuthClient`checkpermissions,OAuthClient`addpermissions
}

<<HTTPClient`;

Begin["OAuthClientDump`"];

$debug=False
debugPrint[args__]:=Print[args/.{("ConsumerSecret"->_):>("ConsumerSecret"->"xxxx")}]/;$debug

oauthservicesdata=OAuthClient`OAuthServicesData;

$oauthservices=OAuthClient`predefinedservicelist;
$authenticatedservices={};

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
ServiceConnect::nost="The given connection id is not stored in the `1` directory. Try to create a new connection."
(*
defineQueries::nolink="The service `1` is not authenticated. Try using ServiceConnect."
*)
ServiceExecute::nargs="The number of arguments given does not match the number of slots in the url template."
ServiceExecute::naparam="The parameter `1` is required"
ServiceConnect::unkn="The service `1` is unknown, try providing authentication options."
ServiceConnect::multser="One service was chosen from multiple `1` services."
ServiceObject::noget="The parameter `1` is not available for the service `2`."
ServiceExecute::apierr="The service returned the following error message: `1`."
ServiceInformation::nolink="The service `1` is not authenticated. Try using ServiceConnect."

(************************************** ServiceConnect **********************************)
ServiceConnect[args___]:=With[{res=Catch[authenticate[args]]},
	res/;res=!=$Failed
]

authenticate["Services"]:=Sort[$oauthservices]

authenticate[service_,rest___]:=oauthauthenticate[service,rest]/;MemberQ[$oauthservices,service]

authenticate[service_, rest__]:=oauthauthenticate[service, rest]/;!FreeQ[{rest},"OAuthVersion"]

authenticate[service_, ___]:=(Message[ServiceConnect::unkn, service];$Failed)

oauthauthenticate[name_,rest___]:=With[{service=oauthauthenticate1[name,rest]},
	Switch["SaveServiceConnection"/.Flatten[Cases[{rest},_?OptionQ,Infinity]],
		True,
		saveServiceConnection[service],
		"Cloud",
		saveServiceConnectionCloud[service],
		"Local",
		saveServiceConnectionLocal[service],
		_,
		Null
	];
	service
]

oauthauthenticate1[name_,"New",rest___]:=(Internal`DeleteCache[{"OAuthTokens", name}];
	newoauthauthenticate[name, rest])

oauthauthenticate1[name_,rest___]:=With[{authorized=checkservicelist[$authenticatedservices, name, ServiceConnect]},
	If[authorized===$Failed,
		cloudoauthauthenticate[name, rest],
		authorized
	]
]

cloudoauthauthenticate[name_]:=cloudoauthauthenticate[name,Automatic]

cloudoauthauthenticate[name_,id_,rest___]:=With[{cloudstored=loadServiceConnection[name,id]},
	Switch[cloudstored,
		(_HTTPClient`OAuth`Private`Token20|_HTTPClient`OAuth`Private`Token10),
		Block[{HTTPClient`OAuth`Private`OAuthFlow},
			HTTPClient`OAuth`Private`OAuthFlow[___]=cloudstored;
			newoauthauthenticate[name, rest]
		],
		_ServiceObject,cloudstored,
		$Failed,newoauthauthenticate[name, rest],
		_,Throw[$Failed]
	]
]
	
newoauthauthenticate[name_,___]:=Module[{service, rawgets=oauthservicesdata[name, "RawGets"], 
	gets=oauthservicesdata[name, "Gets"],rawposts=oauthservicesdata[name, "RawPosts"], posts=oauthservicesdata[name, "Posts"], id, info},
	debugPrint["newoauthauthenticate 0"];
	(* Quiet[SocialMediaData[]];
	*)
	info=getclientinfo[name];
	debugPrint["info"->info];
	info=If[!MatchQ[info,{_String,_String}],
		Throw[$Failed],
		{"ConsumerKey"->info[[1]],"ConsumerSecret"->info[[2]]}
	];
	debugPrint["info"->info];
	service=newunknownoauthauthenticate[name,Join[oauthservicesdata[name, "Authentication"],info]];
	id=getServiceID[service];
	(* predefinedqueries[service,#]&/@rawgets;
	   predefinedqueries[service,#]&/@rawposts;
	   *)
	   
	serviceRawProperties[id]=Sort[Flatten[{serviceRawProperties[id],rawgets}]];
	serviceRawPosts[id]=Sort[Flatten[{serviceRawPosts[id],rawposts}]];
	
	serviceProperties[id]=Sort[Flatten[{serviceProperties[id],gets}]];
	servicePosts[id]=Sort[Flatten[{servicePosts[id],posts}]];
	
	logout[id]=oauthservicesdata[name,"LogoutURL"];
	service	
]/;MemberQ[OAuthClient`predefinedservicelist,name]

newoauthauthenticate[name_,rest___]:=newunknownoauthauthenticate[name, rest]

Options[newunknownoauthauthenticate]={
	"ServiceName"       -> Null,
    "OAuthVersion"		-> "1.0a",
    "RequestEndpoint"   -> "",
    "AccessEndpoint"    -> Null,
    "AuthorizeEndpoint" -> Null,
    "ConsumerKey"		-> Null,
   	"ConsumerSecret"	-> Null,
   	"RedirectURI"		->	"oob",
   	"VerifierLabel"		-> "verifier",
   	"AdditionalOAuthParameter"	-> None,
   	"Scope"				-> None,
    "AuthenticationDialog" 	-> "TokenDialog",
    "RequestFormat"		-> "URL",
    "ResponseType"		-> "code",
    "LogoutURL"			-> Null,
    "Information"		-> "",
    "AccessTokenExtractor" -> None,
    "tokenread"			-> Identity
}
	
newunknownoauthauthenticate[name_,opts:OptionsPattern[]]:=Module[{version,authurl,requrl,accessurl,key,
	secret,redirect,dialogfun, token,urlfetchfun, service, id},
	version=OptionValue["OAuthVersion"];
	If[!MatchQ[version,"1.0"|"1.0a"|"2.0"|"1"|"2"|1|2|1.|2.],
		Message[ServiceConnect::oauthver,version];Throw[$Failed]];
	{authurl, requrl, accessurl}={OptionValue["AuthorizeEndpoint"],OptionValue["RequestEndpoint"],OptionValue["AccessEndpoint"]};
	If[!StringQ[#],Message[ServiceConnect::url,#];Throw[$Failed]]&/@{authurl, requrl, accessurl};
	{key, secret}={OptionValue["ConsumerKey"],OptionValue["ConsumerSecret"]};
	If[!StringQ[#],Message[ServiceConnect::skey,#];Throw[$Failed]]&/@{key, secret};
	redirect=OptionValue["RedirectURI"];
	If[!StringQ[redirect],
		Message[ServiceConnect::url,redirect];Throw[$Failed]];
	dialogfun=Switch[OptionValue["AuthenticationDialog"],
		"TokenDialog",
		tokenOAuthDialog[#, name]&,
		"TokenlessDialog",
		notokenOAuthDialog[#, name]&,
		_,
		Message[ServiceConnect::dialog,dialogfun];Throw[$Failed]
	];
	
	debugPrint["Pre Auth"];
	token=newAuthenticate[name,version,authurl, requrl, accessurl,key,secret,
		redirect,OptionValue["VerifierLabel"],{OptionValue["AdditionalOAuthParameter"],OptionValue["Scope"]},
		dialogfun,OptionValue["AccessTokenExtractor"],OptionValue["ResponseType"]];
		
	debugPrint["token"->token];
	urlfetchfun=Switch[OptionValue["RequestFormat"],
		"URL",
		URLFetch,
		"Headers",
		(With[{spliturl=fromURLtoAuthorizationHeaders[#1], headers=("Headers"/.{##2})/."Headers"->{}},
			URLFetch[spliturl[[1]], "Headers"->Join[headers,{"Authorization"->spliturl[[2]]}],##2]
		]&),
		_Function,
		OptionValue["RequestFormat"],
		_,
		Message[ServiceConnect::reqform,OptionValue["RequestFormat"]];Throw[$Failed]
	];
	
	
	service=createOAuthObject[name,token];
	id=getServiceID[service];
	urlfetchFun[id]=urlfetchfun;
	tokenread[id]=Identity;
	serviceInfo[id]=OptionValue["Information"];
	logout[id]=OptionValue["LogoutURL"];
	
	service
]
	
	
newAuthenticate[name_,version_,authurl_, requrl_, accessurl_,key_,secret_,redirect_,vlabel_,
	{additionalparam_,scope_},dialogfun_, accesstokenext_,responsetype_]:=Module[
	{token, parameters,oauthflowdef,resetflowdef=False},
	parameters=
		If[MatchQ[version,"1"|"1.0"|"1.0a"|1|1.],
			{
		        "ServiceName"       -> name,
		        "OAuthVersion"		-> "1.0a",
		        "RequestEndpoint"   -> requrl,
		        "AccessEndpoint"    -> accessurl,
		        "AuthorizeEndpoint" -> authurl,
		        "VerifierLabel"		-> vlabel,
		        "ConsumerKey"		-> key,
		       	"ConsumerSecret"	->	secret,
		        "AuthenticationDialog" -> dialogfun,
		        If[accesstokenext=!=None,
		        	"AccessTokenExtractor"->accesstokenext,Sequence@@{}]
		    },
		    {
			    "ServiceName"       -> name,
		        "OAuthVersion"		-> "2.0",
			    "AuthorizeEndpoint" -> authurl,
			    "AccessEndpoint"    -> accessurl,
			    "RedirectURI"       -> redirect,
			    "VerifierLabel"     -> vlabel,
			    "ConsumerKey"       -> key,
			    "ConsumerSecret"	 -> secret,
			  	"AuthenticationDialog" -> dialogfun,
		      	If[accesstokenext=!=None,
		        	"AccessTokenExtractor"->accesstokenext,Sequence@@{}]
			}
		];
    
	debugPrint["newAuthenticate parameters"->parameters];
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
    
	debugPrint["newAuthenticate parameters"->parameters];
	token=tokenread[name]@getauthtoken[parameters];
	If[resetflowdef,
		DownValues[HTTPClient`OAuth`Private`OAuthFlow]=oauthflowdef
	];
    If[Head[token] =!= HTTPClient`OAuthToken,  Message[ServiceConnect::token, name];Throw[$Failed]];
    token
]


getauthtoken[parameters_] :=
     Block[{name="ServiceName"/.parameters, token},
    	
         token = Internal`CheckCache[{"OAuthTokens", name}];
      (*    If[token === $Failed,
         	token=checkCloudOAuth[name]; *)
         	If[Head[token] =!= HTTPClient`OAuthToken,
         		debugPrint[Hold[HTTPClient`OAuthAuthentication][parameters]];
	         	token = HTTPClient`OAuthAuthentication[parameters];
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
	serviceRawProperties[id]={};
	serviceProperties[id]={};
	serviceRawPosts[id]={};
	servicePosts[id]={};
	serviceAuthentication[id]={};
	urlfetchFun[id]=URLFetch;
	serviceInfo[id]="";
	
	link=hyperlink[logout[id]];
	debugPrint["logout link"->link];
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
	If[MemberQ[serviceobjectProperties[service],param],
		debugPrint[Hold[externalservice][service, param, rest]];
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
	debugPrint[Hold[oauthdata][service, rest]];oauthdata[service, rest])/;authenticatedServiceQ[service]

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

(* Special Properties *)
$specialproperties={"Authentication", "ID", "Information", "Name", "Properties"};

oauthdata[service_ServiceObject,"Name",___]:=getServiceName[service]
oauthdata[service_ServiceObject,"Properties",___]:=With[{id=getServiceID[service]},
	Sort[DeleteCases[availablequeries[id],"Properties"]]]
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
	Sequence@@If[MemberQ[OAuthClient`predefinedservicelist,name],
		{},
		{"ConsumerKey"->params[[6]],"ConsumerSecret"->params[[7]]}
	]
	}
	
parseToken0[{params_HTTPClient`OAuth`Private`OAuth20Parameters},name_]:=
	{"OAuthVersion"		->	"2.0",
	"AuthorizeEndpoint"	->	params[[9]],
	"AccessEndpoint"	->	params[[11]],
	Sequence@@If[MemberQ[OAuthClient`predefinedservicelist,name],
		{},
		{"ConsumerKey"->params[[7]],"ConsumerSecret"->params[[8]]}
	]
	}
	
parseToken[___]:=Throw[$Failed]

oauthdata[service_ServiceObject,property_,rest___]:=Module[{raw, id=getServiceID[service]},
	debugPrint["************** oauthdata *******************"];
	If[!authenticatedServiceQ[service],
			Message[ServiceExecute::nolink, service];Throw[$Failed]];
	id=getServiceID[service];
	
	If[MemberQ[Join[serviceProperties[id],servicePosts[id]], property],
		OAuthClient`oauthcookeddata[getServiceName[service], property, id,rest]
		,
		
		debugPrint["raw property"->property];
		raw=rawoauthdata[id,property,rest];
		debugPrint["raw"->raw];
		parsedata[id,property]@raw
	]
]

oauthdata[args___]:=(debugPrint["failed args"->{args}];$Failed)

rawoauthdata[id_,parameter_,rest_]:=rawoauthdata[id,parameter,{rest}]/;!ListQ[rest]

rawoauthdata[id_,url0_String]:=Module[{url, res},
		url=getsignedurl[url0,serviceAuthentication[id]];
		If[url === $Failed, Throw[$Failed]];
		If[url === $Canceled, Return[$Canceled]];
        (
     		res = urlfetchFun[id]@@url;
     		res /; (res =!= $Failed)
        ) /; (url =!= $Failed)
	]/;!MemberQ[availablequeries[id],url0]


rawoauthdata[id_,property_,rest___]:=Module[
		{url0,method,pathparams,params,bodyparams,mpdata,headers,optparams,
			url, res, auth, tmp, pvpairs=Flatten[{rest}], params1, bodyparams1,mpdata1,pathparams1,headers1,reqperms, missingperms},		
		{url0,method,pathparams,params,bodyparams,mpdata,headers,optparams, reqperms}=getQueryData[id, property];
		(* Check the required permissions *)
	debugPrint["reqperms"->reqperms];
		If[reqperms=!={},
	debugPrint["aaa"];
			missingperms=If[grantedpermissions[id]===All,{},
				Cases[reqperms,_?(!MemberQ[updatepermissions[id],#]&),1]
			];
	debugPrint["missingperms"->missingperms];
			(* Try to add any missing permissions *)
			If[missingperms=!={},
				If[requestpermissions[id,missingperms]===$Failed,
					Throw[$Failed]]
			];
		];		
		(* Path Parameters use a StringForm Function *)
		debugPrint["rawoauthdata property"->property];	
		If[!MemberQ[First/@pvpairs,#],
			Message[ServiceExecute::nparam,#];Throw[$Failed]
		]&/@DeleteCases[Join[pathparams,params,bodyparams,First/@mpdata],Alternatives@@optparams];
		pathparams1=Cases[pathparams,_?(!FreeQ[pvpairs,#]&)];
		debugPrint["rawoauthdata pathparams1"->pathparams1];	
		url=If[Head[url0]===Function,
			Check[url0@@(pathparams1/.pvpairs),Message[ExternalService::nargs];Throw[$Failed]],
			url0
		];
		
		debugPrint["rawoauthdata url"->url];	
		params1=Cases[params,_?(!FreeQ[pvpairs,#]&)];
		params1=Thread[params1->(params1/.pvpairs)];
		
		bodyparams1=Cases[bodyparams,_?(!FreeQ[pvpairs,#]&)];
		bodyparams1=Thread[bodyparams1->(bodyparams1/.pvpairs)];
			
		mpdata1=Cases[mpdata,_?(!FreeQ[pvpairs,First[#]]&)];
		mpdata1=formatMultipartData[mpdata1,pvpairs];
		
		auth=serviceAuthentication[id];
		(* Set POST or GET *)
		(* If[method==="POST",If[$VersionNumber>=10,
			auth[[1,1,9]]=method,
			auth[[1,10]]=method]; *)
		debugPrint[Hold[getsignedurl][url,auth,"Parameters"->Join[params1, bodyparams1], "Method"->method]];
		url=getsignedurl[url,auth,"Parameters"->Join[params1, bodyparams1], "Method"->method];	
		debugPrint["url"->url];
		If[!MatchQ[url,_String|{_String,___}],Throw[$Failed]];
		debugPrint["signed url"->url];	
		debugPrint["bodyparams"->bodyparams];
		If[method==="POST",
			tmp=cutoutparameters[url[[1]], bodyparams];
			url[[1]]=tmp[[1]];
			url=Join[url,{"BodyData"->tmp[[2]], "MultipartData"->mpdata1}]
		];
		
		If[headers=!={},
			(* Headers should have default values, check for given values *)
			headers1=If[FreeQ[pvpairs,First[#]],#,First[#]->(First[#]/.pvpairs)]&/@headers;
			url=Join[url,{"Headers"->headers1}]
		];
		
		debugPrint["signed split url"->url];
		If[url === $Canceled, Return[$Canceled]];
        (
     		debugPrint["url"->url];
     		res=urlfetchFun[id]@@url;
     		debugPrint["res"->res];
     		res /; (res =!= $Failed)
        ) /; (url =!= $Failed)
	]/;property=!="Authentication"&&MemberQ[Join[serviceRawProperties[id],serviceRawPosts[id]], property]


rawoauthdata[___]:=Throw[$Failed]

parsedata[id_,property_]:=(("ResultsFunction"/.oauthservicesdata[serviceName[id],property])/."ResultsFunction"->Identity
	)/;MemberQ[Join[serviceRawProperties[id],serviceRawPosts[id]], property]
	
parsedata[__]:=Identity

(******************** ServiceInformation **********)
ServiceInformation[args___]:=With[{res=Catch[serviceinfo[args]]},
	res/;res=!=$Failed
]

serviceinfo[service_ServiceObject,rest___]:=(serviceInfo[getServiceID[service]])/;authenticatedServiceQ[service]

serviceinfo[service_,___]:=(Message[ServiceInformation::nolink,service];$Failed)

serviceInfo[id_]:=""/;!authenticatedServiceQ[id]

(****************** SendMessage *******************)

SendMessage[args___]:=With[{res=Catch[sendmessage[args]]},
	debugPrint[Hold[sendmessage][args]];
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

requestpermissions[id_,p_]:=(debugPrint["trying to add permissions"->p];OAuthClient`addpermissions[serviceName[id],id,p])/;authenticatedServiceQ[id]

updatepermissions[___]:=$Failed
requestpermissions[___]:=$Failed

(****************** Utilities *********************)
getsignedurl[url_,auth_, opts___]:=HTTPClient`OAuthSignURL[url, "OAuthAuthentication" -> auth, opts]

assoc=If[$VersionNumber>=10,Association,Identity];

appendservicelist[service_]:=($oauthservices=Union[Flatten[{$oauthservices,service}]])
appendauthservicelist[service_]:=($authenticatedservices=Union[Flatten[{$authenticatedservices,service}]])

makeuuid[]:=StringJoin["connection-",IntegerString[RandomInteger[{0, 16^32}], 16, 32]]

createOAuthObject[name_, token_, id0_:Automatic]:=Module[{link, id},
	id=If[id0===Automatic,makeuuid[], id0];
	link=ServiceObject[name, "ID"->id];
	appendservicelist[link];
	appendauthservicelist[id];
	
	serviceName[id]=name;
	serviceRawProperties[id]={};
	serviceProperties[id]={};
	serviceRawPosts[id]={};
	servicePosts[id]={};
	serviceAuthentication[id]=token;
	urlfetchFun[id]=URLFetch;
	tokenread[id]=Identity;
	
	link
]

getQueryData[id_,property_]:=With[{data=oauthservicesdata[serviceName[id],property]},
	(* URL, method, pathparams,params, bodyparams, mpdata, headers, optionalparams *)
	{("URL"/.data)/."Parameters"->{},
	("HTTPSMethod"/.data)/."HTTPSMethod"->"GET",
	("PathParameters"/.data)/."PathParameters"->{},
	("Parameters"/.data)/."Parameters"->{},
	("BodyData"/.data)/."BodyData"->{},
	("MultipartData"/.data)/."MultipartData"->{},
	("Headers"/.data)/."Headers"->{},
	("OptionalParameters"/.data)/."OptionalParameters"->{},
	("RequiredPermissions"/.data)/."RequiredPermissions"->{}}
]

setQueryData[id_,Rule[prop_,data_]]:=getQueryData[id,prop]=data

servicedata[id_]:=assoc[{
	"ServiceName"->serviceName[id],
	"ID"->id,
	"RawProperties"->serviceRawProperties[id],
	"Properties"->serviceProperties[id],
	"RawPostProperties"->serviceRawPosts[id],
	"PostProperties"->servicePosts[id],
	"Authentication"->serviceAuthentication[id],
	"Information"->serviceInfo[id]
}]

formatMultipartData[mpdata_,pvpairs_]:={#[[1]],#[[2]],#[[1]]/.pvpairs}&/@mpdata

availablequeries[id_]:=Join[serviceProperties[id],serviceRawProperties[id],servicePosts[id],serviceRawPosts[id],
	$specialproperties]/;authenticatedServiceQ[id]
availablequeries[_]:={}

getServiceObject[id_]:=ServiceObject[serviceName[id],"ID"->id]

getServiceName[ServiceObject[args___]]:=First[{args}]
getServiceID[service_ServiceObject]:="ID"/.Cases[service,_Rule,{1}]

serviceName[id_]:=None/;!authenticatedServiceQ[id]
serviceRawProperties[id_]:={}/;!authenticatedServiceQ[id]
serviceProperties[id_]:={}/;!authenticatedServiceQ[id]
serviceRawPosts[id_]:={}/;!authenticatedServiceQ[id]
servicePosts[id_]:={}/;!authenticatedServiceQ[id]
serviceAuthentication[id_]:={}/;!authenticatedServiceQ[id]

tokenread[id_]:=Identity/;!authenticatedServiceQ[id]

urlfetchFun[id_]:=URLFetch/;!authenticatedServiceQ[id]
logout[id_]:=Null/;!authenticatedServiceQ[id]

hyperlink[str_String]:=Hyperlink[str];
hyperlink[___]:=Null

serviceobjectProperties[service_]:=With[{id=getServiceID[service]},
	Join[serviceRawProperties[id],serviceProperties[id],$specialproperties]
]

authenticatedServiceQ[service_ServiceObject]:=authenticatedServiceQ[getServiceID[service]]
authenticatedServiceQ[id_]:=MemberQ[$authenticatedservices,id]

checkservicelist[list_, name_String, fn_]:=With[{matches=Select[list,serviceName[#]===name&]},
	debugPrint["matches"];
	Switch[Length[matches],
		1,getServiceObject[First[matches]],
		0,$Failed,
		_,Message[fn::multser,name];(*getServiceObject[First[matches]]*) Throw[$Failed]
	]
]

fromURLtoAuthorizationHeaders[url_] := Module[{base, auth},
  {base, auth} = StringSplit[url, "?"];
  {base, "Oauth " <> 
    StringReplace[auth, {"=" -> "=\"", "&" -> "\","}] <> "\""}
  ]

getclientinfo[servicename_]:=Block[{OAuthUtilitiesDump`Private`deobflag = True, info},
	info=OAuthClient`OAuthServicesData[servicename,"ClientInfo"];
	debugPrint["info"->info];
	If[!MatchQ[info,{_?IntegerQ..}],Throw[$Failed]];
	OAuthClient`deob[info]
]

(* TODO: UPDATE for raw/cooked 
refreshqueries[service_]:=Module[
	{id=getServiceID[service],name=getServiceName[service],rod, pd, data},
	data=oauthservicesdata[name];
	rod=DownValues[rawoauthdata];
	pd=DownValues[parsedata];
	DownValues[rawoauthdata]=Delete[rod,Position[rod[[All, 1, 1, 1]], id]];
	DownValues[parsedata]=Delete[pd,Position[pd[[All, 1, 1, 1]], id]];
	predefinedqueries[service,#]&/@oauthservicesdata[name, "DefinedGets"];
	predefinedqueries[service,#]&/@oauthservicesdata[name, "DefinedPosts"];
	ServiceObject[name]
]/;MemberQ[OAuthClient`predefinedservicelist,getServiceName[service]]&&MemberQ[$authenticatedservices,getServiceName[service]]
*)
cutoutparameters[str_, {}]:={str,""}
cutoutparameters[str_, params0_] := Module[
  {tmp, url, body, url0,params},
  params=Join[params0,URLEncode/@params0];
  tmp = StringSplit[str, {"?", "&"}];
  {url0, body} = 
   GatherBy[tmp, (StringFreeQ[#, StringJoin[#, "="] & /@ params] &)];
  url = First[url0];
  If[Length[url0] > 1,
   url = url <> "?" <> url0[[2]];
   If[Length[url0] > 2,
    url = StringJoin[url,"&", ##] & @@ Riffle[Drop[url0, 2], "&"]
    ]
   ];
  {url, StringJoin[Riffle[body, "&"]]}
  ]  

listwrap[l_List]:=l
listwrap[x_]:={x}

nothing := Sequence @@ {}

serviceStatus[service_]:="Authenticated"/;authenticatedServiceQ[service]
serviceStatus[_]:="Unknown"

ServiceObject[name_]["Properties"]:=availablequeries[name]/;MemberQ[$authenticatedservices,name]
  
(****************************** Token Storage *************************************)
saveServiceConnection[service_ServiceObject]:=With[{cloudQ=False},
	If[cloudQ,saveServiceConnectionCloud[service]];
	saveServiceConnectionLocal[service]
];

$StoreFullOAuthConnections=False;
createConnectionData[service_]:=Module[{id=getServiceID[service]},
	Join[Normal[servicedata[id]],
			{"urlfetch"->urlfetchFun[id],"logout"->logout[id],
			"RawPropertyData"->((#->getQueryData[id,#])&/@Join[serviceRawProperties[id],serviceRawPosts[id]])
			}]	
]

createConnectionTokenData[service_]:=Module[{id=getServiceID[service]},
	Last[serviceAuthentication[id]]
]

saveServiceConnectionLocal[service_ServiceObject]:=Module[{id=getServiceID[service], name=getServiceName[service], 
		dir, file, temp=FileNameJoin[{$TemporaryDirectory,"m"<>ToString[RandomInteger[1000]]<>".txt"}], data},
	dir=FileNameJoin[{$UserBaseDirectory,"Connections","Services",name}];
	file=FileNameJoin[{dir,id<>".txt"}];
	debugPrint["Saving to file"->(name<>"OAuth.txt")];
	If[!DirectoryQ[dir],
		CreateDirectory[dir]
	];	
	If[FileExistQ[file],DeleteFile[file]];
	data=If[$StoreFullOAuthConnections,createConnectionData,createConnectionTokenData][service];
	Put[data,temp];
	Encode[temp,file];
	DeleteFile[temp];	
] 

saveServiceConnectionCloud[service_]:={}(* TODO: Store the token in the Wolfram Cloud *)


loadServiceConnection[name_,id_:Automatic]:=With[{cloudQ=False},
	If[cloudQ,loadServiceConnectionCloud[name, id],
	loadServiceConnectionLocal[name, id]]
](* TODO: How to choose which connection *)

loadServiceConnectionLocal[name_, fileid_]:=Module[{tmp, dir,file, files},
	dir=FileNameJoin[{$UserBaseDirectory,"Connections","Services",name}];
	If[DirectoryQ[dir],
		If[fileid===Automatic,
			files=FileNames["connection-*.txt", dir];
			Switch[Length[files],
				1,file=First[files],
				0,Return[$Failed],
				_,Message[ServiceConnect::multst,dir];Throw[$Failed]
			],
			file=FileNameJoin[{dir,fileid<>".txt"}];
			If[!FileExistsQ[file],
				Message[ServiceConnect::nost,dir];Return[$Failed]
			]
		];
		tmp=Get[file];
		parseStoredConnection[tmp]
		,
		$Failed		
	]
]

loadServiceConnectionCloud[name_, id_]:={}(* TODO: Check the Wolfram Cloud for a token *)

parseStoredConnection[tmp:{_Rule..}]:=Module[{service,servicename,id},
		servicename="ServiceName"/.tmp;
		id="ID"/.tmp;
		service=createOAuthObject[servicename, "Authentication"/.tmp, id];
		serviceRawProperties[id]="RawProperties"/.tmp;
		serviceProperties[id]="Properties"/.tmp;
		serviceRawPosts[id]="RawPostProperties"/.tmp;
		servicePosts[id]="PostProperties"/.tmp;
		urlfetchFun[id]="urlfetch"/.tmp;
		tokenread[id]="tokenread"/.tmp;
		serviceInfo[id]:="Information"/.tmp;
		logout[id]="logout"/.tmp;
		
		setQueryData[id,#]&/@("RawPropertyData"/.tmp);
		
		debugPrint["service"->service];
		service
]
parseStoredConnection[tmp:(_HTTPClient`OAuth`Private`Token20|_HTTPClient`OAuth`Private`Token10)]:=tmp

deleteLocalFile[name_]:=If[FileExistsQ[name<>"OAuth.txt"],
		debugPrint["deleting file"->(name<>"OAuth.txt")];
		DeleteFile[name<>"OAuth.txt"]
		,$Failed]

(*********************** Authentication Dialogs ***********************************)
tokenOAuthDialog[url_, text_] :=
    Block[{nb, value = Null, done = False, key = ""},
        nb = CreateDocument[
            DynamicModule[{
                     button = If[text==="Permissions",permissionButton[url],authenticationButton[url, text]],
                     smessage = Style["(you may be asked to authorize the app)", 10, FontFamily -> "Arial"],
                     pmessage = Style[Column[{"Copy your access key &", "paste it into the field below"}, Center], 13, FontFamily -> "Arial"],
                     hmessage = "Paste your access key here"},
                Column[{
                    Panel[Row[{Spacer[22], Style["<WC ICON>",Red], Spacer[10],
                              Style["WolframConnector", FontFamily -> "Arial", FontColor -> GrayLevel[0.30], FontSize -> 17]}],
                               ImageSize -> {500, Scaled[1]}, ImageMargins -> {{0, 0}, {0, 0}}, Alignment -> Left, Appearance -> topgradient, FrameMargins->{{0, 0}, {0,0}}]
                   ,
                       Column [{
                         Row[{Spacer[22], Column[{button, smessage}, Center], RawBoxes[arrowImage], Spacer[10], Column[{RawBoxes[copypasteImage], pmessage}, Center], Spacer[22]}]
                         ,
                        Row[{Spacer[22], InputField[Dynamic[key, (key = StringReplace[#, RegularExpression["(?ms) "] :> ""]) &],
                                 String, Enabled -> True, ContinuousAction -> True,
                                 FieldHint -> hmessage, ImageSize -> {445, 53}, ImageMargins -> {{0, 0}, {15, 0}}], Spacer[22]}]}
                      , Left, 1.5
                    ]
                  ,
                     Column[{Row[{Button[Style["Cancel", FontFamily -> "Arial", FontColor -> Black, FontSize -> 12], (value = $Canceled; done = True),
                                  Appearance -> whitebutton, ImageSize -> {90, Automatic}], Spacer[10],
                                    Button[Style["Done", FontFamily -> "Arial", FontColor -> White, FontSize -> 12], (value = key; done = True),
                                  Appearance -> redbutton, ImageSize -> {90, Automatic}], Spacer[22]}],
                             Spacer[1]}]
                  }
                  ,
                      Alignment -> {Right, Center}, Background -> {None, None, GrayLevel[.92]}, Dividers -> {None, {None, None, GrayLevel[.8], None}},
                      Spacings -> 2
                ]
            ],
            Modal -> True,
            Background -> White,
            ShowCellBracket->False,
            StyleDefinitions -> "Dialog.nb",
            CellMargins-> {{0, 0}, {0, 0}},
            CellFrameMargins->0,
            CellFrameLabelMargins -> 0,
            CellLabelMargins -> 0,
            WindowElements -> {},
            WindowFrameElements -> {"CloseBox"},
            WindowFrame -> "ModalDialog",
            System`NotebookEventActions -> {
                "ReturnKeyDown" :> (value = key; done = True),
                {"MenuCommand", "HandleShiftReturn"} :> (value = key; done = True),
                "EscapeKeyDown" :> (value = $Canceled; done = True),
                "WindowClose" :>   (value = $Canceled; done = True)},
            WindowSize -> {500, 330},
            ShowStringCharacters -> False,
            Evaluator -> CurrentValue["RunningEvaluator"]
        ];

        WaitUntil[done];

        FrontEndExecute[FrontEnd`NotebookClose[nb, Interactive -> True, "ClosingEvent" -> Null]];

        value
    ]


tokenOAuthDialog[___] := $Failed

notokenOAuthDialog[url_, text_] :=
    Block[{nb, authorizedQ, done = False},
        nb = CreateDocument[
            DynamicModule[{
                     button = If[text==="Permissions",permissionButton[url],authenticationButton[url, text]],
                     smessage = Style["(you may be asked to authorize the app)", 10, FontFamily -> "Arial"],
                     dmessage = "Authorized"},
                Column[{
                    Panel[Row[{Spacer[10], Style["<WC ICON>",Red], Spacer[10],
                              Style["WolframConnector", FontFamily -> "Arial", FontColor -> GrayLevel[0.30], FontSize -> 17]}],
                               ImageSize -> {400, 40}, ImageMargins -> {{0, 0}, {0, 0}}, Alignment -> Left, Appearance -> topgradient, FrameMargins->{{0, 0}, {0,0}}]
                   ,
                       Row[{Spacer[22], Column[{button, smessage}, Center], Spacer[22]}] 
                  ,
                     Column[{Row[{Spacer[22],Button[Style["Cancel", FontFamily -> "Arial", FontColor -> Black, FontSize -> 12], (authorizedQ = $Canceled; done = True),
                                  Appearance -> whitebutton, ImageSize -> {90, Automatic}], Spacer[5],
                                    Button[Style[dmessage, FontFamily -> "Arial", FontColor -> White, FontSize -> 12], (authorizedQ = "True"; done = True),
                                  Appearance -> redbutton, ImageSize -> {90, Automatic}], Spacer[22]}],Spacer[10]}]
                  }
                  ,
                      Alignment -> {Center, Center},Background -> {None, None, GrayLevel[.92]}, Dividers -> {None, {None, None, GrayLevel[.8], None}},
                      Spacings -> 2
                ]
            ],
            Modal -> True,
            Background -> White,
            ShowCellBracket->False,
            StyleDefinitions -> "Dialog.nb",
            CellMargins-> {{0, 0}, {0, 0}},
            CellFrameMargins->0,
            CellFrameLabelMargins -> 0,
            CellLabelMargins -> 0,
            WindowElements -> {},
            WindowFrameElements -> {"CloseBox"},
            WindowFrame -> "ModalDialog",
            System`NotebookEventActions -> {
                "ReturnKeyDown" :> (authorizedQ = "True"; done = True),
                {"MenuCommand", "HandleShiftReturn"} :> (authorizedQ = "True"; done = True),
                "EscapeKeyDown" :> (authorizedQ = $Canceled; done = True),
                "WindowClose" :>   (authorizedQ = $Canceled; done = True)},
            WindowSize -> {400, 175},
            ShowStringCharacters -> False,
            Evaluator -> CurrentValue["RunningEvaluator"]
        ];

        WaitUntil[done];
		
        FrontEndExecute[FrontEnd`NotebookClose[nb, Interactive -> True, "ClosingEvent" -> Null]];
		debugPrint["authorizedQ"->authorizedQ];
        authorizedQ
    ]

notokenOAuthDialog[___] := $Failed

(* :authenticationButton: *)
         
authenticationButton[url_, text_] /; ($OperatingSystem === "Unix") :=
    Panel[Hyperlink[Row[{Style["Sign in to ", FontFamily -> "Arial",
        FontColor -> White, FontSize -> 12], Style[text, FontFamily -> "Arial",
        FontColor -> White, FontSize -> 12, FontWeight->Bold], Spacer[9], RawBoxes[linkIcon]}], url],
         ImageSize -> {200, 32}, Appearance -> redbutton,
         Alignment -> Center]

authenticationButton[url_, text_] :=
    Panel[Hyperlink[Row[{Style["Sign in to ", FontFamily -> "Arial",
        FontColor -> White, FontSize -> 13], Style[text, FontFamily -> "Arial",
        FontColor -> White, FontSize -> 13, FontWeight->Bold], Spacer[9], RawBoxes[linkIcon]}], url],
         ImageSize -> {200, 32}, Appearance -> redbutton,
         Alignment -> Center]

permissionButton[url_] /; ($OperatingSystem === "Unix") :=
    Panel[Hyperlink[Row[{Style["Add Permissions", FontFamily -> "Arial",
        FontColor -> White, FontSize -> 12], Spacer[9], RawBoxes[linkIcon]}], url],
         ImageSize -> {200, 32}, Appearance -> redbutton,
         Alignment -> Center]
         
permissionButton[url_] :=
    Panel[Hyperlink[Row[{Style["Add Permissions", FontFamily -> "Arial",
        FontColor -> White, FontSize -> 13], Spacer[9], RawBoxes[linkIcon]}], url],
         ImageSize -> {200, 32}, Appearance -> redbutton,
         Alignment -> Center]

(* login images *)
linkIcon = GraphicsBox[TagBox[RasterBox[CompressedData["
1:eJxTTMoPSmNiYGAo5gASQYnljkVFiZXBLECOU2VJahIzkMEPxOJADBL8////
v//kAZA+BgKYlmbDACF16HpoaTZMTwYQm1PR7CtAvBDKXgvE34G4FIgFqGA2
Ml6AZMZhIHYhwWw+IG4B4m4c+Pp/VADyw3QgtiDCbOn/pAOQ+dFEmM0ExGJA
LI6EOYGYB8regGTGTyBeDsQqQMxKhNnYsDMQG0HZa6D6LwNxFJq6f2SYjewe
UNjOAGIpLOq+kGk2LO/I4FH3GYj/ArEICRjZbHz453/yASGz/0LVfSEB/yPG
3QDxIJM7
    "], {{0, 15}, {23, 0}}, {0, 255},
    ColorFunction->RGBColor],
   BoxForm`ImageTag[
   "Byte", ColorSpace -> "RGB", Interleaving -> True, Magnification -> 1],
   Selectable->False],
  BaseStyle->"ImageGraphics",
  ImageSize->Magnification[1],
  ImageSizeRaw->{23, 15},
  PlotRange->{{0, 23}, {0, 15}}]
  
redbutton = Image[RawArray["Byte", {{{255, 255, 255, 0}, {255, 255, 255, 0}, {255,
  255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255,
  0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {0, 0, 0, 255}, {0, 0, 0,
  255}, {0, 0, 0, 255}, {0, 0, 0, 255}, {0, 0, 0, 255}, {255, 255, 255,
  0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255,
  255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255,
  0}}, {{255, 255, 255, 0}, {237, 237, 238, 0}, {238, 185, 176, 0},
  {238, 130, 111, 78}, {239, 107, 83, 135}, {240, 108, 83, 153}, {239,
  107, 83, 153}, {239, 107, 83, 153}, {239, 107, 83, 153}, {239, 108,
  83, 153}, {239, 107, 83, 153}, {239, 107, 83, 153}, {239, 107, 83,
  153}, {239, 107, 83, 153}, {239, 107, 83, 153}, {239, 107, 83, 153},
  {239, 107, 83, 150}, {238, 117, 94, 99}, {237, 158, 145, 12}, {237,
  212, 209, 0}, {255, 255, 255, 0}}, {{255, 255, 255, 0}, {238, 165,
  153, 0}, {235, 77, 56, 165}, {228, 21, 0, 255}, {225, 1, 0, 255},
  {225, 1, 0, 255}, {225, 1, 0, 255}, {224, 0, 0, 255}, {225, 1, 0,
  255}, {225, 1, 0, 255}, {225, 1, 0, 255}, {225, 1, 0, 255}, {225, 1,
  0, 255}, {225, 1, 0, 255}, {225, 1, 0, 255}, {224, 0, 0, 255}, {225,
  1, 0, 255}, {226, 10, 0, 255}, {231, 44, 24, 222}, {238, 103, 80, 21},
  {255, 255, 255, 0}}, {{255, 255, 255, 0}, {238, 92, 70, 72}, {224, 5,
  0, 255}, {223, 1, 0, 255}, {223, 1, 0, 255}, {223, 1, 0, 255}, {222,
  0, 0, 255}, {223, 1, 0, 255}, {223, 1, 0, 255}, {222, 0, 0, 255},
  {223, 1, 0, 255}, {223, 1, 0, 255}, {223, 1, 0, 255}, {223, 1, 0,
  255}, {223, 1, 0, 255}, {222, 0, 0, 255}, {223, 1, 0, 255}, {223, 1,
  0, 255}, {223, 1, 0, 255}, {231, 49, 31, 144}, {255, 255, 255, 0}},
  {{255, 255, 255, 0}, {231, 54, 36, 117}, {222, 1, 0, 255}, {222, 1, 0,
  255}, {222, 1, 0, 255}, {222, 1, 0, 255}, {222, 1, 0, 255}, {222, 1,
  0, 255}, {222, 1, 0, 255}, {222, 1, 0, 255}, {222, 1, 0, 255}, {222,
  1, 0, 255}, {222, 1, 0, 255}, {222, 1, 0, 255}, {222, 1, 0, 255},
  {222, 1, 0, 255}, {222, 1, 0, 255}, {221, 0, 0, 255}, {222, 1, 0,
  255}, {223, 7, 0, 192}, {255, 255, 255, 0}}, {{0, 0, 0, 255}, {228,
  44, 25, 120}, {220, 1, 0, 255}, {220, 1, 0, 255}, {220, 1, 0, 255},
  {219, 0, 0, 255}, {220, 1, 0, 255}, {220, 1, 0, 255}, {219, 1, 0,
  255}, {219, 1, 0, 255}, {219, 0, 0, 255}, {218, 0, 0, 255}, {218, 0,
  0, 255}, {218, 0, 0, 255}, {218, 0, 0, 255}, {220, 1, 0, 255}, {220,
  1, 0, 255}, {220, 1, 0, 255}, {220, 1, 0, 255}, {219, 0, 0, 195}, {0,
  0, 0, 255}}, {{0, 0, 0, 255}, {226, 45, 25, 120}, {213, 1, 0, 255},
  {214, 2, 0, 255}, {214, 2, 0, 255}, {214, 2, 0, 255}, {213, 2, 0,
  255}, {213, 2, 0, 255}, {213, 2, 0, 255}, {213, 2, 0, 255}, {213, 2,
  0, 255}, {213, 1, 0, 255}, {214, 2, 0, 255}, {214, 2, 0, 255}, {214,
  2, 0, 255}, {213, 2, 0, 255}, {213, 2, 0, 255}, {213, 2, 0, 255},
  {213, 2, 0, 255}, {218, 2, 0, 195}, {0, 0, 0, 255}}, {{0, 0, 0, 255},
  {225, 45, 25, 120}, {209, 2, 0, 255}, {209, 2, 0, 255}, {209, 2, 0,
  255}, {209, 2, 0, 255}, {209, 2, 0, 255}, {209, 2, 0, 255}, {209, 2,
  0, 255}, {209, 2, 0, 255}, {209, 2, 0, 255}, {209, 2, 0, 255}, {209,
  2, 0, 255}, {209, 2, 0, 255}, {209, 2, 0, 255}, {209, 2, 0, 255},
  {209, 2, 0, 255}, {209, 2, 0, 255}, {209, 2, 0, 255}, {217, 2, 0,
  195}, {0, 0, 0, 255}}, {{0, 0, 0, 255}, {226, 46, 28, 120}, {210, 3,
  0, 255}, {209, 3, 0, 255}, {209, 3, 0, 255}, {209, 3, 0, 255}, {210,
  3, 0, 255}, {210, 3, 0, 255}, {210, 3, 0, 255}, {210, 3, 0, 255},
  {210, 3, 0, 255}, {210, 3, 0, 255}, {209, 3, 0, 255}, {209, 3, 0,
  255}, {209, 3, 0, 255}, {210, 3, 0, 255}, {210, 3, 0, 255}, {210, 3,
  0, 255}, {210, 3, 0, 255}, {215, 3, 0, 195}, {0, 0, 0, 255}}, {{0, 0,
  0, 255}, {226, 46, 28, 120}, {210, 3, 0, 255}, {209, 3, 0, 255}, {209,
  3, 0, 255}, {209, 3, 0, 255}, {210, 3, 0, 255}, {210, 3, 0, 255},
  {210, 3, 0, 255}, {210, 3, 0, 255}, {210, 3, 0, 255}, {210, 3, 0,
  255}, {209, 3, 0, 255}, {209, 3, 0, 255}, {209, 3, 0, 255}, {210, 3,
  0, 255}, {210, 3, 0, 255}, {210, 3, 0, 255}, {210, 3, 0, 255}, {215,
  3, 0, 195}, {0, 0, 0, 255}}, {{0, 0, 0, 255}, {241, 61, 46, 120},
  {241, 30, 13, 255}, {241, 29, 12, 255}, {241, 29, 12, 255}, {241, 29,
  12, 255}, {241, 29, 12, 255}, {241, 29, 12, 255}, {241, 29, 12, 255},
  {241, 29, 12, 255}, {241, 29, 12, 255}, {241, 30, 13, 255}, {241, 29,
  12, 255}, {241, 29, 12, 255}, {241, 29, 12, 255}, {241, 29, 12, 255},
  {241, 29, 12, 255}, {241, 29, 12, 255}, {241, 29, 12, 255}, {241, 30,
  13, 195}, {0, 0, 0, 255}}, {{0, 0, 0, 255}, {241, 65, 49, 120}, {241,
  35, 21, 255}, {241, 35, 21, 255}, {241, 35, 21, 255}, {241, 35, 21,
  255}, {241, 35, 21, 255}, {241, 35, 21, 255}, {241, 35, 21, 255},
  {241, 35, 21, 255}, {241, 35, 21, 255}, {241, 35, 21, 255}, {241, 35,
  21, 255}, {241, 35, 21, 255}, {241, 35, 21, 255}, {241, 35, 21, 255},
  {241, 35, 21, 255}, {241, 35, 21, 255}, {241, 35, 21, 255}, {242, 35,
  23, 195}, {0, 0, 0, 255}}, {{0, 0, 0, 255}, {241, 69, 55, 120}, {236,
  40, 31, 255}, {236, 41, 31, 255}, {236, 41, 31, 255}, {236, 41, 31,
  255}, {236, 40, 30, 255}, {236, 40, 30, 255}, {236, 40, 30, 255},
  {236, 40, 30, 255}, {236, 40, 30, 255}, {236, 40, 31, 255}, {236, 41,
  31, 255}, {236, 41, 31, 255}, {236, 41, 31, 255}, {236, 40, 30, 255},
  {236, 40, 30, 255}, {236, 40, 30, 255}, {236, 40, 30, 255}, {243, 43,
  34, 195}, {0, 0, 0, 255}}, {{0, 0, 0, 255}, {242, 74, 61, 120}, {235,
  47, 38, 255}, {235, 46, 38, 255}, {235, 46, 38, 255}, {235, 46, 38,
  255}, {235, 46, 38, 255}, {235, 46, 38, 255}, {235, 46, 38, 255},
  {235, 46, 38, 255}, {235, 46, 38, 255}, {235, 47, 38, 255}, {235, 46,
  38, 255}, {235, 46, 38, 255}, {235, 46, 38, 255}, {235, 46, 38, 255},
  {235, 46, 38, 255}, {235, 46, 38, 255}, {235, 46, 38, 255}, {244, 51,
  43, 195}, {0, 0, 0, 255}}, {{0, 0, 0, 255}, {243, 79, 66, 120}, {240,
  57, 51, 255}, {239, 57, 50, 255}, {239, 57, 50, 255}, {239, 57, 50,
  255}, {240, 57, 51, 255}, {240, 57, 51, 255}, {240, 57, 51, 255},
  {240, 57, 51, 255}, {240, 57, 51, 255}, {240, 57, 51, 255}, {239, 57,
  50, 255}, {239, 57, 50, 255}, {239, 57, 50, 255}, {240, 57, 51, 255},
  {240, 57, 51, 255}, {240, 57, 51, 255}, {240, 57, 51, 255}, {244, 60,
  53, 195}, {0, 0, 0, 255}}, {{0, 0, 0, 255}, {244, 85, 73, 120}, {245,
  70, 64, 255}, {244, 70, 64, 255}, {244, 70, 64, 255}, {244, 70, 64,
  255}, {245, 70, 64, 255}, {245, 70, 64, 255}, {245, 70, 64, 255},
  {245, 70, 64, 255}, {245, 70, 64, 255}, {245, 70, 64, 255}, {244, 70,
  64, 255}, {244, 70, 64, 255}, {244, 70, 64, 255}, {245, 70, 64, 255},
  {245, 70, 64, 255}, {245, 70, 64, 255}, {245, 70, 64, 255}, {246, 70,
  65, 195}, {0, 0, 0, 255}}, {{0, 0, 0, 255}, {243, 92, 79, 120}, {247,
  81, 77, 255}, {247, 81, 77, 255}, {247, 81, 77, 255}, {247, 81, 77,
  255}, {247, 81, 77, 255}, {247, 81, 77, 255}, {247, 81, 77, 255},
  {247, 81, 77, 255}, {247, 81, 77, 255}, {247, 81, 77, 255}, {247, 81,
  77, 255}, {247, 81, 77, 255}, {247, 81, 77, 255}, {247, 81, 77, 255},
  {247, 81, 77, 255}, {247, 81, 77, 255}, {247, 81, 77, 255}, {247, 81,
  77, 195}, {0, 0, 0, 255}}, {{0, 0, 0, 0}, {243, 99, 86, 114}, {247,
  92, 88, 255}, {248, 92, 88, 255}, {248, 93, 89, 255}, {247, 92, 88,
  255}, {248, 92, 89, 255}, {248, 93, 89, 255}, {248, 92, 89, 255},
  {248, 92, 88, 255}, {248, 92, 89, 255}, {248, 93, 89, 255}, {248, 93,
  89, 255}, {248, 93, 89, 255}, {248, 93, 89, 255}, {248, 92, 89, 255},
  {248, 93, 89, 255}, {247, 92, 88, 255}, {247, 92, 88, 255}, {248, 93,
  89, 186}, {255, 255, 255, 0}}, {{0, 0, 0, 0}, {242, 106, 88, 45},
  {248, 103, 100, 252}, {248, 103, 100, 255}, {249, 104, 101, 255},
  {249, 103, 100, 255}, {249, 103, 100, 255}, {248, 103, 100, 255},
  {248, 103, 100, 255}, {249, 103, 100, 255}, {249, 104, 101, 255},
  {249, 103, 100, 255}, {249, 103, 100, 255}, {249, 103, 100, 255},
  {249, 103, 100, 255}, {249, 104, 101, 255}, {249, 103, 100, 255},
  {249, 103, 100, 255}, {249, 103, 100, 255}, {246, 104, 96, 117}, {255,
  255, 255, 0}}, {{255, 255, 255, 0}, {237, 158, 145, 0}, {245, 112,
  101, 108}, {249, 115, 113, 246}, {250, 115, 113, 255}, {250, 115, 113,
  255}, {250, 116, 113, 255}, {250, 115, 113, 255}, {250, 115, 113,
  255}, {249, 115, 112, 255}, {249, 115, 112, 255}, {250, 115, 113,
  255}, {250, 115, 113, 255}, {250, 115, 113, 255}, {250, 115, 113,
  255}, {249, 115, 112, 255}, {250, 115, 113, 255}, {249, 115, 112,
  255}, {248, 114, 108, 174}, {242, 108, 89, 6}, {255, 255, 255, 0}},
  {{255, 255, 255, 0}, {237, 212, 208, 0}, {237, 158, 145, 0}, {242,
  111, 91, 18}, {243, 115, 100, 51}, {243, 115, 100, 51}, {244, 116,
  100, 51}, {243, 115, 100, 51}, {243, 115, 100, 51}, {243, 115, 100,
  51}, {244, 115, 100, 51}, {243, 115, 100, 51}, {243, 115, 100, 51},
  {243, 115, 100, 51}, {243, 115, 100, 51}, {243, 115, 100, 51}, {243,
  115, 99, 51}, {242, 113, 95, 33}, {240, 108, 85, 0}, {238, 177, 166,
  0}, {255, 255, 255, 0}}, {{255, 255, 255, 0}, {255, 255, 255, 0},
  {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255,
  255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {0, 0, 0, 255}, {0,
  0, 0, 255}, {0, 0, 0, 255}, {0, 0, 0, 255}, {0, 0, 0, 255}, {255, 255,
  255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0},
  {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255, 255, 0}, {255, 255,
  255, 0}}}], "Byte", ColorSpace -> "RGB", Interleaving -> True]

whitebutton = Image[RawArray["Byte", {{{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0,
  0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 255},
  {0, 0, 0, 255}, {0, 0, 0, 255}, {0, 0, 0, 255}, {0, 0, 0, 255}, {0, 0, 0, 0},
  {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0,
  0, 0}, {0, 0, 0, 0}}, {{0, 0, 0, 0}, {0, 0, 0, 0}, {106, 106, 106, 12}, {108,
  108, 108, 99}, {107, 107, 107, 150}, {107, 107, 107, 153}, {107, 107, 107,
  153}, {107, 107, 107, 153}, {107, 107, 107, 153}, {107, 107, 107, 153}, {107,
  107, 107, 153}, {107, 107, 107, 153}, {107, 107, 107, 153}, {107, 107, 107,
  153}, {107, 107, 107, 153}, {107, 107, 107, 153}, {107, 107, 107, 150}, {108,
  108, 108, 99}, {106, 106, 106, 12}, {0, 0, 0, 0}, {0, 0, 0, 0}}, {{0, 0, 0,
  0}, {109, 109, 109, 21}, {107, 107, 107, 222}, {164, 164, 164, 255}, {194,
  194, 194, 255}, {196, 196, 196, 255}, {196, 196, 196, 255}, {196, 196, 196,
  255}, {196, 196, 196, 255}, {196, 196, 196, 255}, {196, 196, 196, 255}, {196,
  196, 196, 255}, {196, 196, 196, 255}, {196, 196, 196, 255}, {196, 196, 196,
  255}, {196, 196, 196, 255}, {194, 194, 194, 255}, {164, 164, 164, 255}, {107,
  107, 107, 222}, {109, 109, 109, 21}, {0, 0, 0, 0}}, {{0, 0, 0, 0}, {106, 106,
  106, 144}, {180, 180, 180, 255}, {255, 255, 255, 255}, {255, 255, 255, 255},
  {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255,
  255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255},
  {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255,
  255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {180, 180, 180, 255},
  {106, 106, 106, 144}, {0, 0, 0, 0}}, {{0, 0, 0, 0}, {108, 108, 108, 192},
  {218, 218, 218, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255,
  255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255},
  {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255,
  255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255},
  {255, 255, 255, 255}, {255, 255, 255, 255}, {218, 218, 218, 255}, {108, 108,
  108, 192}, {0, 0, 0, 0}}, {{0, 0, 0, 255}, {107, 107, 107, 195}, {220, 220,
  220, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255},
  {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255,
  255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255},
  {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255,
  255, 255}, {255, 255, 255, 255}, {220, 220, 220, 255}, {107, 107, 107, 195},
  {0, 0, 0, 255}}, {{0, 0, 0, 255}, {107, 107, 107, 195}, {220, 220, 220, 255},
  {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255,
  255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255},
  {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255,
  255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255},
  {255, 255, 255, 255}, {220, 220, 220, 255}, {107, 107, 107, 195}, {0, 0, 0,
  255}}, {{0, 0, 0, 255}, {107, 107, 107, 195}, {220, 220, 220, 255}, {255,
  255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255,
  255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255,
  255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255,
  255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255,
  255, 255, 255}, {220, 220, 220, 255}, {107, 107, 107, 195}, {0, 0, 0, 255}},
  {{0, 0, 0, 255}, {107, 107, 107, 195}, {220, 220, 220, 255}, {255, 255, 255,
  255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255,
  255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255,
  255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255,
  255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255,
  255}, {220, 220, 220, 255}, {107, 107, 107, 195}, {0, 0, 0, 255}}, {{0, 0, 0,
  255}, {107, 107, 107, 195}, {220, 220, 220, 255}, {255, 255, 255, 255}, {255,
  255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255,
  255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255,
  255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255,
  255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {220,
  220, 220, 255}, {107, 107, 107, 195}, {0, 0, 0, 255}}, {{0, 0, 0, 255}, {107,
  107, 107, 195}, {220, 220, 220, 255}, {255, 255, 255, 255}, {255, 255, 255,
  255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255,
  255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255,
  255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255,
  255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {220, 220, 220,
  255}, {107, 107, 107, 195}, {0, 0, 0, 255}}, {{0, 0, 0, 255}, {107, 107, 107,
  195}, {220, 220, 220, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255,
  255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255,
  255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255,
  255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255,
  255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {220, 220, 220, 255}, {107,
  107, 107, 195}, {0, 0, 0, 255}}, {{0, 0, 0, 255}, {107, 107, 107, 195}, {220,
  220, 220, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255,
  255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255,
  255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255,
  255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255,
  255, 255, 255}, {255, 255, 255, 255}, {220, 220, 220, 255}, {107, 107, 107,
  195}, {0, 0, 0, 255}}, {{0, 0, 0, 255}, {107, 107, 107, 195}, {220, 220, 220,
  255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255,
  255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255,
  255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255,
  255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255,
  255}, {255, 255, 255, 255}, {220, 220, 220, 255}, {107, 107, 107, 195}, {0,
  0, 0, 255}}, {{0, 0, 0, 255}, {107, 107, 107, 195}, {220, 220, 220, 255},
  {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255,
  255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255},
  {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255,
  255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255},
  {255, 255, 255, 255}, {220, 220, 220, 255}, {107, 107, 107, 195}, {0, 0, 0,
  255}}, {{0, 0, 0, 255}, {107, 107, 107, 195}, {220, 220, 220, 255}, {255,
  255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255,
  255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255,
  255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255,
  255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255,
  255, 255, 255}, {220, 220, 220, 255}, {107, 107, 107, 195}, {0, 0, 0, 255}},
  {{0, 0, 0, 255}, {107, 107, 107, 195}, {220, 220, 220, 255}, {255, 255, 255,
  255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255,
  255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255,
  255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255,
  255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255,
  255}, {220, 220, 220, 255}, {107, 107, 107, 195}, {0, 0, 0, 255}}, {{0, 0, 0,
  0}, {107, 107, 107, 186}, {215, 215, 215, 255}, {255, 255, 255, 255}, {255,
  255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255,
  255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255,
  255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255,
  255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {215,
  215, 215, 255}, {107, 107, 107, 186}, {0, 0, 0, 0}}, {{0, 0, 0, 0}, {107,
  107, 107, 117}, {171, 171, 171, 255}, {255, 255, 255, 255}, {255, 255, 255,
  255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255,
  255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255,
  255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {255,
  255, 255, 255}, {255, 255, 255, 255}, {255, 255, 255, 255}, {171, 171, 171,
  255}, {107, 107, 107, 117}, {0, 0, 0, 0}}, {{0, 0, 0, 0}, {128, 128, 128, 6},
  {107, 107, 107, 174}, {126, 126, 126, 255}, {137, 137, 137, 255}, {137, 137,
  137, 255}, {137, 137, 137, 255}, {137, 137, 137, 255}, {137, 137, 137, 255},
  {137, 137, 137, 255}, {137, 137, 137, 255}, {137, 137, 137, 255}, {137, 137,
  137, 255}, {137, 137, 137, 255}, {137, 137, 137, 255}, {137, 137, 137, 255},
  {137, 137, 137, 255}, {126, 126, 126, 255}, {107, 107, 107, 174}, {128, 128,
  128, 6}, {0, 0, 0, 0}}, {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {108, 108,
  108, 33}, {105, 105, 105, 51}, {105, 105, 105, 51}, {105, 105, 105, 51},
  {105, 105, 105, 51}, {105, 105, 105, 51}, {105, 105, 105, 51}, {105, 105,
  105, 51}, {105, 105, 105, 51}, {105, 105, 105, 51}, {105, 105, 105, 51},
  {105, 105, 105, 51}, {105, 105, 105, 51}, {105, 105, 105, 51}, {108, 108,
  108, 33}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}, {{0, 0, 0, 0}, {0, 0, 0,
  0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0,
  0, 0, 0}, {0, 0, 0, 255}, {0, 0, 0, 255}, {0, 0, 0, 255}, {0, 0, 0, 255}, {0,
  0, 0, 255}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0,
  0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}}], "Byte", ColorSpace -> "RGB",
 Interleaving -> True]


arrowImage = GraphicsBox[
  TagBox[RasterBox[CompressedData["
1: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
Yp6oGBcUBs7BPkuczA7/B4V/HCI=
    "], {{0, 98}, {66, 0}}, {0, 255},
    ColorFunction->RGBColor],
    BoxForm`ImageTag[
    "Byte", ColorSpace -> "RGB", Interleaving -> True, Magnification -> 1],
    Selectable->False],
    BaseStyle->"ImageGraphics",
    ImageSize->Magnification[1],
    ImageSizeRaw->{66, 98},
    PlotRange->{{0, 66}, {0, 98}}]


copypasteImage = GraphicsBox[TagBox[RasterBox[CompressedData["
1:eJzVmPkvZlcYx6XtD/03mv4L0kRI1NIgllhDJMQStaeMdcaWoGIJgmDGkjEd
+zqLqWUwY2y1UzuJLbEPpTW2wdtP74kbNZMXxYx+EzfPe8655/meZz3XN04/
mf/4hYqKit/XPMwdg7Tu3HG8a/EVP7Tv+rs4fYnwLX/f8fePfHx8rFAoNjY2
urq6Ojo6frsdaG9vn56eVpzg7du3xcXFDx8+/OV24JGE3NzcoaEhDHh0dFRf
X5+Xl1dbW/vixYtfL4mamprLvnIuBI3y8vL8/Hyc+/79++rq6pKSEnQ9f/68
+tYAMthtaWnp8PAQGYaC/M3h4tzE+mfPnuHxlZUVGFZLNoThjdnjHDx9+hQ+
CE+ePJH9iHCGIV4WJ73Uea8XwkoiDmFIviwvLwuGpaWlVVVVcH78+DHef/SZ
gGqRy0VFRadtCGeypqCgYHJycmdn508Jf31yCL3v3r1bW1vDXJDEXNiQXK6r
q8vMzKRIUhtFAf/sWFhYoB5iNJlheno6PYUpTHos4dOzEnqpz8irq6vZ2dmF
hYWCIRUbhvQaxS2woSAAsaysLNmGMMzIyOjs7GRKHOE2MDxjw/8FQ+HlW4KP
MiRTyHQsCdXu7u7frwauJb29vezWdRmwvq2tjV5MwfkoQ65hLGBwbm6uubn5
9evXb06hpaWlWcKbf0MekQVWNjU1jY6OUm9RR2IuLi7KAkBACyWFQQQxyE/W
DwwMjI+Pw+RDhtiNcWHJkZERDQ0NdXV1LS0tHR0dTU1NWQAIP0hA+F6CtrY2
4wgs09XV1dfXV1VVpQsQ1X9IEAV5fX0dYWtrizsVPzc3N/nJc0MCs0yNjY3R
OJCVM+Qg9vb2SUlJdnZ2hoaGbm5uLi4u6A0JCQkNDTUyMtLT04OegYFBYGBg
dHS0qampn59fYmKijY0NU6xUU1Ojk6I3OTmZNWxL6YiIiKBZNDQ0+Pv75+Tk
7O7u0uBQTSO7FEPONTU1df/+fTZhmZWVFZcKNg8KCkpNTX3w4AEknZyceMIc
GnTPyMhIKiqzCQkJkGSliYkJV9C+vr7w8HBuLFB69eoVuwUEBAwODnJ2aKOd
n5x6f38fSpC8IEPMjgtSUlJwHNqNjY3j4+Pv3buH3rCwML4X4uLi2P/ly5e+
vr50T0aCg4MtLS2xISTd3d0rKysR8DJhxs6cjvXEGEo9PDxwOq8goJ1NYmJi
8Jpw9AUZsoygxSYcENcQXaiIioqytbXliZuwGBbGud7e3jT3tLQ0uEGeWdZj
Rp64r6KigozjpLGxscKbKOIs6OUOgxmZZYTjX5YhXiYOiSu87OXlRRa4uroS
h1iJ8+IUnMsUOzs4OPwswdHRkVPAGcvAgdNhNNr93t4em+Nl/EIR42MNVnDg
dQKAOJyZmenv78eq6xJYdi7Dnp6eg4MD4tBIgpmZmbW1tbm5OYKFhQUZYSaB
9MH7jMsjpidAZpxSwKUOVhQWMhqN8EHYkYAAeXHXYlxc9ngyNSlBCUPq2MTE
BBXpdBWVIQ92S5DlMwLHpCTCDZvj4tnZ2e3tbbbFPhMShAAToUuwQsYyFGRG
PsoQAuQUyyiGPGeVYkbCh4OyjEZsQnw6OzsTn3Cen58fkzB+AlkeOwFFHu04
/czti4CRbw4yjq8G/Ei0EKteEki61tbWq/RlYlswlG+wV2eIR2Do6elJMSTr
MSb1h4arkG5QSl5kgShN8v2wtraWOkwAXOv1RMHOMKQ8+vj4kOkUcD6O8PVF
3mUZ+Q5DKqr4nwOFl+LMnYScIgw2rwbReclT6g/cGhsbKeB4B6syruRFVJNT
VGNKOt2KsokNMWxHRweEaQSMZF0MuEDJLB9BtGMin4MTPNxb+GAvKysjzflk
O3dzXoceIYHZsRgmpSJRZCBJ5ceS+RcArUTJLBGOIpKUcIIbhXp4eJjWQ3Em
K9GifGfxzQ49DCj7HRfwc0nC8nUAT+FltkXAhsjkMvko3wmVgzW8Irgd3+TH
3enNxaUOf/3nHa4donpcfZ+/AUM7uQE=
    "], {{0, 35}, {54, 0}}, {0, 255},
    ColorFunction->RGBColor],
   BoxForm`ImageTag[
   "Byte", ColorSpace -> "RGB", Interleaving -> True, Magnification -> 1],
   Selectable->False],
  BaseStyle->"ImageGraphics",
  ImageSize->Magnification[1],
  ImageSizeRaw->{54, 35},
  PlotRange->{{0, 54}, {0, 35}}]


topgradient = Image[RawArray["Byte", {{{255, 255, 255}, {0, 0, 0}, {0, 0, 0}, {0, 0,
  0}, {0, 0, 0}, {0, 0, 0}, {0, 0, 0}, {0, 0, 0}, {0, 0, 0}, {0, 0, 0},
  {0, 0, 0}, {0, 0, 0}, {0, 0, 0}, {0, 0, 0}, {0, 0, 0}, {0, 0, 0}, {0,
  0, 0}, {0, 0, 0}, {0, 0, 0}, {0, 0, 0}, {0, 0, 0}, {0, 0, 0}, {0, 0,
  0}, {0, 0, 0}, {0, 0, 0}, {0, 0, 0}, {0, 0, 0}, {0, 0, 0}, {0, 0, 0},
  {0, 0, 0}, {0, 0, 0}, {0, 0, 0}, {0, 0, 0}, {0, 0, 0}, {0, 0, 0}, {0,
  0, 0}, {0, 0, 0}, {0, 0, 0}, {0, 0, 0}, {0, 0, 0}, {0, 0, 0}, {0, 0,
  0}, {0, 0, 0}, {0, 0, 0}, {0, 0, 0}, {0, 0, 0}, {0, 0, 0}, {0, 0, 0},
  {0, 0, 0}, {0, 0, 0}, {0, 0, 0}, {0, 0, 0}, {0, 0, 0}, {0, 0, 0}, {0,
  0, 0}, {0, 0, 0}, {0, 0, 0}, {0, 0, 0}, {255, 255, 255}}, {{255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}}, {{255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}}, {{255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}}, {{255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}}, {{255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}}, {{255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}}, {{255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}}, {{255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}}, {{255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}}, {{255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}}, {{0, 0, 0}, {255, 255, 255}, {255, 255, 255},
  {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255},
  {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255},
  {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255},
  {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255},
  {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255},
  {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255},
  {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255},
  {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255},
  {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255},
  {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255},
  {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255},
  {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255},
  {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255},
  {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {0, 0, 0}}, {{0, 0,
  0}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {0, 0, 0}}, {{0, 0, 0}, {255, 255, 255}, {255,
  255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255,
  255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255,
  255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255,
  255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255,
  255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255,
  255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255,
  255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255,
  255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255,
  255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255,
  255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255,
  255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255,
  255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255,
  255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255,
  255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {0, 0,
  0}}, {{0, 0, 0}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255},
  {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255},
  {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255},
  {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255},
  {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255},
  {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255},
  {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255},
  {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255},
  {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255},
  {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255},
  {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255},
  {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255},
  {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255},
  {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255},
  {255, 255, 255}, {255, 255, 255}, {0, 0, 0}}, {{0, 0, 0}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {255, 255, 255}, {255, 255, 255}, {255, 255, 255}, {255, 255,
  255}, {0, 0, 0}}, {{0, 0, 0}, {254, 254, 254}, {254, 254, 254}, {254,
  254, 254}, {254, 254, 254}, {254, 254, 254}, {254, 254, 254}, {254,
  254, 254}, {254, 254, 254}, {254, 254, 254}, {254, 254, 254}, {254,
  254, 254}, {254, 254, 254}, {254, 254, 254}, {254, 254, 254}, {254,
  254, 254}, {254, 254, 254}, {254, 254, 254}, {254, 254, 254}, {254,
  254, 254}, {254, 254, 254}, {254, 254, 254}, {254, 254, 254}, {254,
  254, 254}, {254, 254, 254}, {254, 254, 254}, {254, 254, 254}, {254,
  254, 254}, {254, 254, 254}, {254, 254, 254}, {254, 254, 254}, {254,
  254, 254}, {254, 254, 254}, {254, 254, 254}, {254, 254, 254}, {254,
  254, 254}, {254, 254, 254}, {254, 254, 254}, {254, 254, 254}, {254,
  254, 254}, {254, 254, 254}, {254, 254, 254}, {254, 254, 254}, {254,
  254, 254}, {254, 254, 254}, {254, 254, 254}, {254, 254, 254}, {254,
  254, 254}, {254, 254, 254}, {254, 254, 254}, {254, 254, 254}, {254,
  254, 254}, {254, 254, 254}, {254, 254, 254}, {254, 254, 254}, {254,
  254, 254}, {254, 254, 254}, {254, 254, 254}, {0, 0, 0}}, {{0, 0, 0},
  {254, 254, 254}, {254, 254, 254}, {254, 254, 254}, {254, 254, 254},
  {254, 254, 254}, {254, 254, 254}, {254, 254, 254}, {254, 254, 254},
  {254, 254, 254}, {254, 254, 254}, {254, 254, 254}, {254, 254, 254},
  {254, 254, 254}, {254, 254, 254}, {254, 254, 254}, {254, 254, 254},
  {254, 254, 254}, {254, 254, 254}, {254, 254, 254}, {254, 254, 254},
  {254, 254, 254}, {254, 254, 254}, {254, 254, 254}, {254, 254, 254},
  {254, 254, 254}, {254, 254, 254}, {254, 254, 254}, {254, 254, 254},
  {254, 254, 254}, {254, 254, 254}, {254, 254, 254}, {254, 254, 254},
  {254, 254, 254}, {254, 254, 254}, {254, 254, 254}, {254, 254, 254},
  {254, 254, 254}, {254, 254, 254}, {254, 254, 254}, {254, 254, 254},
  {254, 254, 254}, {254, 254, 254}, {254, 254, 254}, {254, 254, 254},
  {254, 254, 254}, {254, 254, 254}, {254, 254, 254}, {254, 254, 254},
  {254, 254, 254}, {254, 254, 254}, {254, 254, 254}, {254, 254, 254},
  {254, 254, 254}, {254, 254, 254}, {254, 254, 254}, {254, 254, 254},
  {254, 254, 254}, {0, 0, 0}}, {{0, 0, 0}, {253, 253, 253}, {253, 253,
  253}, {253, 253, 253}, {253, 253, 253}, {253, 253, 253}, {253, 253,
  253}, {253, 253, 253}, {253, 253, 253}, {253, 253, 253}, {253, 253,
  253}, {253, 253, 253}, {253, 253, 253}, {253, 253, 253}, {253, 253,
  253}, {253, 253, 253}, {253, 253, 253}, {253, 253, 253}, {253, 253,
  253}, {253, 253, 253}, {253, 253, 253}, {253, 253, 253}, {253, 253,
  253}, {253, 253, 253}, {253, 253, 253}, {253, 253, 253}, {253, 253,
  253}, {253, 253, 253}, {253, 253, 253}, {253, 253, 253}, {253, 253,
  253}, {253, 253, 253}, {253, 253, 253}, {253, 253, 253}, {253, 253,
  253}, {253, 253, 253}, {253, 253, 253}, {253, 253, 253}, {253, 253,
  253}, {253, 253, 253}, {253, 253, 253}, {253, 253, 253}, {253, 253,
  253}, {253, 253, 253}, {253, 253, 253}, {253, 253, 253}, {253, 253,
  253}, {253, 253, 253}, {253, 253, 253}, {253, 253, 253}, {253, 253,
  253}, {253, 253, 253}, {253, 253, 253}, {253, 253, 253}, {253, 253,
  253}, {253, 253, 253}, {253, 253, 253}, {253, 253, 253}, {0, 0, 0}},
  {{0, 0, 0}, {253, 253, 253}, {253, 253, 253}, {253, 253, 253}, {253,
  253, 253}, {253, 253, 253}, {253, 253, 253}, {253, 253, 253}, {253,
  253, 253}, {253, 253, 253}, {253, 253, 253}, {253, 253, 253}, {253,
  253, 253}, {253, 253, 253}, {253, 253, 253}, {253, 253, 253}, {253,
  253, 253}, {253, 253, 253}, {253, 253, 253}, {253, 253, 253}, {253,
  253, 253}, {253, 253, 253}, {253, 253, 253}, {253, 253, 253}, {253,
  253, 253}, {253, 253, 253}, {253, 253, 253}, {253, 253, 253}, {253,
  253, 253}, {253, 253, 253}, {253, 253, 253}, {253, 253, 253}, {253,
  253, 253}, {253, 253, 253}, {253, 253, 253}, {253, 253, 253}, {253,
  253, 253}, {253, 253, 253}, {253, 253, 253}, {253, 253, 253}, {253,
  253, 253}, {253, 253, 253}, {253, 253, 253}, {253, 253, 253}, {253,
  253, 253}, {253, 253, 253}, {253, 253, 253}, {253, 253, 253}, {253,
  253, 253}, {253, 253, 253}, {253, 253, 253}, {253, 253, 253}, {253,
  253, 253}, {253, 253, 253}, {253, 253, 253}, {253, 253, 253}, {253,
  253, 253}, {253, 253, 253}, {0, 0, 0}}, {{0, 0, 0}, {252, 252, 252},
  {252, 252, 252}, {252, 252, 252}, {252, 252, 252}, {252, 252, 252},
  {252, 252, 252}, {252, 252, 252}, {252, 252, 252}, {252, 252, 252},
  {252, 252, 252}, {252, 252, 252}, {252, 252, 252}, {252, 252, 252},
  {252, 252, 252}, {252, 252, 252}, {252, 252, 252}, {252, 252, 252},
  {252, 252, 252}, {252, 252, 252}, {252, 252, 252}, {252, 252, 252},
  {252, 252, 252}, {252, 252, 252}, {252, 252, 252}, {252, 252, 252},
  {252, 252, 252}, {252, 252, 252}, {252, 252, 252}, {252, 252, 252},
  {252, 252, 252}, {252, 252, 252}, {252, 252, 252}, {252, 252, 252},
  {252, 252, 252}, {252, 252, 252}, {252, 252, 252}, {252, 252, 252},
  {252, 252, 252}, {252, 252, 252}, {252, 252, 252}, {252, 252, 252},
  {252, 252, 252}, {252, 252, 252}, {252, 252, 252}, {252, 252, 252},
  {252, 252, 252}, {252, 252, 252}, {252, 252, 252}, {252, 252, 252},
  {252, 252, 252}, {252, 252, 252}, {252, 252, 252}, {252, 252, 252},
  {252, 252, 252}, {252, 252, 252}, {252, 252, 252}, {252, 252, 252},
  {0, 0, 0}}, {{0, 0, 0}, {252, 252, 252}, {252, 252, 252}, {252, 252,
  252}, {252, 252, 252}, {252, 252, 252}, {252, 252, 252}, {252, 252,
  252}, {252, 252, 252}, {252, 252, 252}, {252, 252, 252}, {252, 252,
  252}, {252, 252, 252}, {252, 252, 252}, {252, 252, 252}, {252, 252,
  252}, {252, 252, 252}, {252, 252, 252}, {252, 252, 252}, {252, 252,
  252}, {252, 252, 252}, {252, 252, 252}, {252, 252, 252}, {252, 252,
  252}, {252, 252, 252}, {252, 252, 252}, {252, 252, 252}, {252, 252,
  252}, {252, 252, 252}, {252, 252, 252}, {252, 252, 252}, {252, 252,
  252}, {252, 252, 252}, {252, 252, 252}, {252, 252, 252}, {252, 252,
  252}, {252, 252, 252}, {252, 252, 252}, {252, 252, 252}, {252, 252,
  252}, {252, 252, 252}, {252, 252, 252}, {252, 252, 252}, {252, 252,
  252}, {252, 252, 252}, {252, 252, 252}, {252, 252, 252}, {252, 252,
  252}, {252, 252, 252}, {252, 252, 252}, {252, 252, 252}, {252, 252,
  252}, {252, 252, 252}, {252, 252, 252}, {252, 252, 252}, {252, 252,
  252}, {252, 252, 252}, {252, 252, 252}, {0, 0, 0}}, {{0, 0, 0}, {251,
  251, 251}, {251, 251, 251}, {251, 251, 251}, {251, 251, 251}, {251,
  251, 251}, {251, 251, 251}, {251, 251, 251}, {251, 251, 251}, {251,
  251, 251}, {251, 251, 251}, {251, 251, 251}, {251, 251, 251}, {251,
  251, 251}, {251, 251, 251}, {251, 251, 251}, {251, 251, 251}, {251,
  251, 251}, {251, 251, 251}, {251, 251, 251}, {251, 251, 251}, {251,
  251, 251}, {251, 251, 251}, {251, 251, 251}, {251, 251, 251}, {251,
  251, 251}, {251, 251, 251}, {251, 251, 251}, {251, 251, 251}, {251,
  251, 251}, {251, 251, 251}, {251, 251, 251}, {251, 251, 251}, {251,
  251, 251}, {251, 251, 251}, {251, 251, 251}, {251, 251, 251}, {251,
  251, 251}, {251, 251, 251}, {251, 251, 251}, {251, 251, 251}, {251,
  251, 251}, {251, 251, 251}, {251, 251, 251}, {251, 251, 251}, {251,
  251, 251}, {251, 251, 251}, {251, 251, 251}, {251, 251, 251}, {251,
  251, 251}, {251, 251, 251}, {251, 251, 251}, {251, 251, 251}, {251,
  251, 251}, {251, 251, 251}, {251, 251, 251}, {251, 251, 251}, {251,
  251, 251}, {0, 0, 0}}, {{0, 0, 0}, {251, 251, 251}, {251, 251, 251},
  {251, 251, 251}, {251, 251, 251}, {251, 251, 251}, {251, 251, 251},
  {251, 251, 251}, {251, 251, 251}, {251, 251, 251}, {251, 251, 251},
  {251, 251, 251}, {251, 251, 251}, {251, 251, 251}, {251, 251, 251},
  {251, 251, 251}, {251, 251, 251}, {251, 251, 251}, {251, 251, 251},
  {251, 251, 251}, {251, 251, 251}, {251, 251, 251}, {251, 251, 251},
  {251, 251, 251}, {251, 251, 251}, {251, 251, 251}, {251, 251, 251},
  {251, 251, 251}, {251, 251, 251}, {251, 251, 251}, {251, 251, 251},
  {251, 251, 251}, {251, 251, 251}, {251, 251, 251}, {251, 251, 251},
  {251, 251, 251}, {251, 251, 251}, {251, 251, 251}, {251, 251, 251},
  {251, 251, 251}, {251, 251, 251}, {251, 251, 251}, {251, 251, 251},
  {251, 251, 251}, {251, 251, 251}, {251, 251, 251}, {251, 251, 251},
  {251, 251, 251}, {251, 251, 251}, {251, 251, 251}, {251, 251, 251},
  {251, 251, 251}, {251, 251, 251}, {251, 251, 251}, {251, 251, 251},
  {251, 251, 251}, {251, 251, 251}, {251, 251, 251}, {0, 0, 0}}, {{0, 0,
  0}, {250, 250, 250}, {250, 250, 250}, {250, 250, 250}, {250, 250,
  250}, {250, 250, 250}, {250, 250, 250}, {250, 250, 250}, {250, 250,
  250}, {250, 250, 250}, {250, 250, 250}, {250, 250, 250}, {250, 250,
  250}, {250, 250, 250}, {250, 250, 250}, {250, 250, 250}, {250, 250,
  250}, {250, 250, 250}, {250, 250, 250}, {250, 250, 250}, {250, 250,
  250}, {250, 250, 250}, {250, 250, 250}, {250, 250, 250}, {250, 250,
  250}, {250, 250, 250}, {250, 250, 250}, {250, 250, 250}, {250, 250,
  250}, {250, 250, 250}, {250, 250, 250}, {250, 250, 250}, {250, 250,
  250}, {250, 250, 250}, {250, 250, 250}, {250, 250, 250}, {250, 250,
  250}, {250, 250, 250}, {250, 250, 250}, {250, 250, 250}, {250, 250,
  250}, {250, 250, 250}, {250, 250, 250}, {250, 250, 250}, {250, 250,
  250}, {250, 250, 250}, {250, 250, 250}, {250, 250, 250}, {250, 250,
  250}, {250, 250, 250}, {250, 250, 250}, {250, 250, 250}, {250, 250,
  250}, {250, 250, 250}, {250, 250, 250}, {250, 250, 250}, {250, 250,
  250}, {250, 250, 250}, {0, 0, 0}}, {{0, 0, 0}, {249, 249, 249}, {249,
  249, 249}, {249, 249, 249}, {249, 249, 249}, {249, 249, 249}, {249,
  249, 249}, {249, 249, 249}, {249, 249, 249}, {249, 249, 249}, {249,
  249, 249}, {249, 249, 249}, {249, 249, 249}, {249, 249, 249}, {249,
  249, 249}, {249, 249, 249}, {249, 249, 249}, {249, 249, 249}, {249,
  249, 249}, {249, 249, 249}, {249, 249, 249}, {249, 249, 249}, {249,
  249, 249}, {249, 249, 249}, {249, 249, 249}, {249, 249, 249}, {249,
  249, 249}, {249, 249, 249}, {249, 249, 249}, {249, 249, 249}, {249,
  249, 249}, {249, 249, 249}, {249, 249, 249}, {249, 249, 249}, {249,
  249, 249}, {249, 249, 249}, {249, 249, 249}, {249, 249, 249}, {249,
  249, 249}, {249, 249, 249}, {249, 249, 249}, {249, 249, 249}, {249,
  249, 249}, {249, 249, 249}, {249, 249, 249}, {249, 249, 249}, {249,
  249, 249}, {249, 249, 249}, {249, 249, 249}, {249, 249, 249}, {249,
  249, 249}, {249, 249, 249}, {249, 249, 249}, {249, 249, 249}, {249,
  249, 249}, {249, 249, 249}, {249, 249, 249}, {249, 249, 249}, {0, 0,
  0}}, {{0, 0, 0}, {249, 249, 249}, {249, 249, 249}, {249, 249, 249},
  {249, 249, 249}, {249, 249, 249}, {249, 249, 249}, {249, 249, 249},
  {249, 249, 249}, {249, 249, 249}, {249, 249, 249}, {249, 249, 249},
  {249, 249, 249}, {249, 249, 249}, {249, 249, 249}, {249, 249, 249},
  {249, 249, 249}, {249, 249, 249}, {249, 249, 249}, {249, 249, 249},
  {249, 249, 249}, {249, 249, 249}, {249, 249, 249}, {249, 249, 249},
  {249, 249, 249}, {249, 249, 249}, {249, 249, 249}, {249, 249, 249},
  {249, 249, 249}, {249, 249, 249}, {249, 249, 249}, {249, 249, 249},
  {249, 249, 249}, {249, 249, 249}, {249, 249, 249}, {249, 249, 249},
  {249, 249, 249}, {249, 249, 249}, {249, 249, 249}, {249, 249, 249},
  {249, 249, 249}, {249, 249, 249}, {249, 249, 249}, {249, 249, 249},
  {249, 249, 249}, {249, 249, 249}, {249, 249, 249}, {249, 249, 249},
  {249, 249, 249}, {249, 249, 249}, {249, 249, 249}, {249, 249, 249},
  {249, 249, 249}, {249, 249, 249}, {249, 249, 249}, {249, 249, 249},
  {249, 249, 249}, {249, 249, 249}, {0, 0, 0}}, {{0, 0, 0}, {248, 248,
  248}, {248, 248, 248}, {248, 248, 248}, {248, 248, 248}, {248, 248,
  248}, {248, 248, 248}, {248, 248, 248}, {248, 248, 248}, {248, 248,
  248}, {248, 248, 248}, {248, 248, 248}, {248, 248, 248}, {248, 248,
  248}, {248, 248, 248}, {248, 248, 248}, {248, 248, 248}, {248, 248,
  248}, {248, 248, 248}, {248, 248, 248}, {248, 248, 248}, {248, 248,
  248}, {248, 248, 248}, {248, 248, 248}, {248, 248, 248}, {248, 248,
  248}, {248, 248, 248}, {248, 248, 248}, {248, 248, 248}, {248, 248,
  248}, {248, 248, 248}, {248, 248, 248}, {248, 248, 248}, {248, 248,
  248}, {248, 248, 248}, {248, 248, 248}, {248, 248, 248}, {248, 248,
  248}, {248, 248, 248}, {248, 248, 248}, {248, 248, 248}, {248, 248,
  248}, {248, 248, 248}, {248, 248, 248}, {248, 248, 248}, {248, 248,
  248}, {248, 248, 248}, {248, 248, 248}, {248, 248, 248}, {248, 248,
  248}, {248, 248, 248}, {248, 248, 248}, {248, 248, 248}, {248, 248,
  248}, {248, 248, 248}, {248, 248, 248}, {248, 248, 248}, {248, 248,
  248}, {0, 0, 0}}, {{0, 0, 0}, {247, 247, 247}, {247, 247, 247}, {247,
  247, 247}, {247, 247, 247}, {247, 247, 247}, {247, 247, 247}, {247,
  247, 247}, {247, 247, 247}, {247, 247, 247}, {247, 247, 247}, {247,
  247, 247}, {247, 247, 247}, {247, 247, 247}, {247, 247, 247}, {247,
  247, 247}, {247, 247, 247}, {247, 247, 247}, {247, 247, 247}, {247,
  247, 247}, {247, 247, 247}, {247, 247, 247}, {247, 247, 247}, {247,
  247, 247}, {247, 247, 247}, {247, 247, 247}, {247, 247, 247}, {247,
  247, 247}, {247, 247, 247}, {247, 247, 247}, {247, 247, 247}, {247,
  247, 247}, {247, 247, 247}, {247, 247, 247}, {247, 247, 247}, {247,
  247, 247}, {247, 247, 247}, {247, 247, 247}, {247, 247, 247}, {247,
  247, 247}, {247, 247, 247}, {247, 247, 247}, {247, 247, 247}, {247,
  247, 247}, {247, 247, 247}, {247, 247, 247}, {247, 247, 247}, {247,
  247, 247}, {247, 247, 247}, {247, 247, 247}, {247, 247, 247}, {247,
  247, 247}, {247, 247, 247}, {247, 247, 247}, {247, 247, 247}, {247,
  247, 247}, {247, 247, 247}, {247, 247, 247}, {0, 0, 0}}, {{0, 0, 0},
  {246, 246, 246}, {246, 246, 246}, {246, 246, 246}, {246, 246, 246},
  {246, 246, 246}, {246, 246, 246}, {246, 246, 246}, {246, 246, 246},
  {246, 246, 246}, {246, 246, 246}, {246, 246, 246}, {246, 246, 246},
  {246, 246, 246}, {246, 246, 246}, {246, 246, 246}, {246, 246, 246},
  {246, 246, 246}, {246, 246, 246}, {246, 246, 246}, {246, 246, 246},
  {246, 246, 246}, {246, 246, 246}, {246, 246, 246}, {246, 246, 246},
  {246, 246, 246}, {246, 246, 246}, {246, 246, 246}, {246, 246, 246},
  {246, 246, 246}, {246, 246, 246}, {246, 246, 246}, {246, 246, 246},
  {246, 246, 246}, {246, 246, 246}, {246, 246, 246}, {246, 246, 246},
  {246, 246, 246}, {246, 246, 246}, {246, 246, 246}, {246, 246, 246},
  {246, 246, 246}, {246, 246, 246}, {246, 246, 246}, {246, 246, 246},
  {246, 246, 246}, {246, 246, 246}, {246, 246, 246}, {246, 246, 246},
  {246, 246, 246}, {246, 246, 246}, {246, 246, 246}, {246, 246, 246},
  {246, 246, 246}, {246, 246, 246}, {246, 246, 246}, {246, 246, 246},
  {246, 246, 246}, {0, 0, 0}}, {{0, 0, 0}, {246, 246, 246}, {246, 246,
  246}, {246, 246, 246}, {246, 246, 246}, {246, 246, 246}, {246, 246,
  246}, {246, 246, 246}, {246, 246, 246}, {246, 246, 246}, {246, 246,
  246}, {246, 246, 246}, {246, 246, 246}, {246, 246, 246}, {246, 246,
  246}, {246, 246, 246}, {246, 246, 246}, {246, 246, 246}, {246, 246,
  246}, {246, 246, 246}, {246, 246, 246}, {246, 246, 246}, {246, 246,
  246}, {246, 246, 246}, {246, 246, 246}, {246, 246, 246}, {246, 246,
  246}, {246, 246, 246}, {246, 246, 246}, {246, 246, 246}, {246, 246,
  246}, {246, 246, 246}, {246, 246, 246}, {246, 246, 246}, {246, 246,
  246}, {246, 246, 246}, {246, 246, 246}, {246, 246, 246}, {246, 246,
  246}, {246, 246, 246}, {246, 246, 246}, {246, 246, 246}, {246, 246,
  246}, {246, 246, 246}, {246, 246, 246}, {246, 246, 246}, {246, 246,
  246}, {246, 246, 246}, {246, 246, 246}, {246, 246, 246}, {246, 246,
  246}, {246, 246, 246}, {246, 246, 246}, {246, 246, 246}, {246, 246,
  246}, {246, 246, 246}, {246, 246, 246}, {246, 246, 246}, {0, 0, 0}},
  {{0, 0, 0}, {245, 245, 245}, {245, 245, 245}, {245, 245, 245}, {245,
  245, 245}, {245, 245, 245}, {245, 245, 245}, {245, 245, 245}, {245,
  245, 245}, {245, 245, 245}, {245, 245, 245}, {245, 245, 245}, {245,
  245, 245}, {245, 245, 245}, {245, 245, 245}, {245, 245, 245}, {245,
  245, 245}, {245, 245, 245}, {245, 245, 245}, {245, 245, 245}, {245,
  245, 245}, {245, 245, 245}, {245, 245, 245}, {245, 245, 245}, {245,
  245, 245}, {245, 245, 245}, {245, 245, 245}, {245, 245, 245}, {245,
  245, 245}, {245, 245, 245}, {245, 245, 245}, {245, 245, 245}, {245,
  245, 245}, {245, 245, 245}, {245, 245, 245}, {245, 245, 245}, {245,
  245, 245}, {245, 245, 245}, {245, 245, 245}, {245, 245, 245}, {245,
  245, 245}, {245, 245, 245}, {245, 245, 245}, {245, 245, 245}, {245,
  245, 245}, {245, 245, 245}, {245, 245, 245}, {245, 245, 245}, {245,
  245, 245}, {245, 245, 245}, {245, 245, 245}, {245, 245, 245}, {245,
  245, 245}, {245, 245, 245}, {245, 245, 245}, {245, 245, 245}, {245,
  245, 245}, {245, 245, 245}, {0, 0, 0}}, {{0, 0, 0}, {244, 244, 244},
  {244, 244, 244}, {244, 244, 244}, {244, 244, 244}, {244, 244, 244},
  {244, 244, 244}, {244, 244, 244}, {244, 244, 244}, {244, 244, 244},
  {244, 244, 244}, {244, 244, 244}, {244, 244, 244}, {244, 244, 244},
  {244, 244, 244}, {244, 244, 244}, {244, 244, 244}, {244, 244, 244},
  {244, 244, 244}, {244, 244, 244}, {244, 244, 244}, {244, 244, 244},
  {244, 244, 244}, {244, 244, 244}, {244, 244, 244}, {244, 244, 244},
  {244, 244, 244}, {244, 244, 244}, {244, 244, 244}, {244, 244, 244},
  {244, 244, 244}, {244, 244, 244}, {244, 244, 244}, {244, 244, 244},
  {244, 244, 244}, {244, 244, 244}, {244, 244, 244}, {244, 244, 244},
  {244, 244, 244}, {244, 244, 244}, {244, 244, 244}, {244, 244, 244},
  {244, 244, 244}, {244, 244, 244}, {244, 244, 244}, {244, 244, 244},
  {244, 244, 244}, {244, 244, 244}, {244, 244, 244}, {244, 244, 244},
  {244, 244, 244}, {244, 244, 244}, {244, 244, 244}, {244, 244, 244},
  {244, 244, 244}, {244, 244, 244}, {244, 244, 244}, {244, 244, 244},
  {0, 0, 0}}, {{0, 0, 0}, {243, 243, 243}, {243, 243, 243}, {243, 243,
  243}, {243, 243, 243}, {243, 243, 243}, {243, 243, 243}, {243, 243,
  243}, {243, 243, 243}, {243, 243, 243}, {243, 243, 243}, {243, 243,
  243}, {243, 243, 243}, {243, 243, 243}, {243, 243, 243}, {243, 243,
  243}, {243, 243, 243}, {243, 243, 243}, {243, 243, 243}, {243, 243,
  243}, {243, 243, 243}, {243, 243, 243}, {243, 243, 243}, {243, 243,
  243}, {243, 243, 243}, {243, 243, 243}, {243, 243, 243}, {243, 243,
  243}, {243, 243, 243}, {243, 243, 243}, {243, 243, 243}, {243, 243,
  243}, {243, 243, 243}, {243, 243, 243}, {243, 243, 243}, {243, 243,
  243}, {243, 243, 243}, {243, 243, 243}, {243, 243, 243}, {243, 243,
  243}, {243, 243, 243}, {243, 243, 243}, {243, 243, 243}, {243, 243,
  243}, {243, 243, 243}, {243, 243, 243}, {243, 243, 243}, {243, 243,
  243}, {243, 243, 243}, {243, 243, 243}, {243, 243, 243}, {243, 243,
  243}, {243, 243, 243}, {243, 243, 243}, {243, 243, 243}, {243, 243,
  243}, {243, 243, 243}, {243, 243, 243}, {0, 0, 0}}, {{0, 0, 0}, {243,
  243, 243}, {243, 243, 243}, {243, 243, 243}, {243, 243, 243}, {243,
  243, 243}, {243, 243, 243}, {243, 243, 243}, {243, 243, 243}, {243,
  243, 243}, {243, 243, 243}, {243, 243, 243}, {243, 243, 243}, {243,
  243, 243}, {243, 243, 243}, {243, 243, 243}, {243, 243, 243}, {243,
  243, 243}, {243, 243, 243}, {243, 243, 243}, {243, 243, 243}, {243,
  243, 243}, {243, 243, 243}, {243, 243, 243}, {243, 243, 243}, {243,
  243, 243}, {243, 243, 243}, {243, 243, 243}, {243, 243, 243}, {243,
  243, 243}, {243, 243, 243}, {243, 243, 243}, {243, 243, 243}, {243,
  243, 243}, {243, 243, 243}, {243, 243, 243}, {243, 243, 243}, {243,
  243, 243}, {243, 243, 243}, {243, 243, 243}, {243, 243, 243}, {243,
  243, 243}, {243, 243, 243}, {243, 243, 243}, {243, 243, 243}, {243,
  243, 243}, {243, 243, 243}, {243, 243, 243}, {243, 243, 243}, {243,
  243, 243}, {243, 243, 243}, {243, 243, 243}, {243, 243, 243}, {243,
  243, 243}, {243, 243, 243}, {243, 243, 243}, {243, 243, 243}, {243,
  243, 243}, {0, 0, 0}}, {{0, 0, 0}, {242, 242, 242}, {242, 242, 242},
  {242, 242, 242}, {242, 242, 242}, {242, 242, 242}, {242, 242, 242},
  {242, 242, 242}, {242, 242, 242}, {242, 242, 242}, {242, 242, 242},
  {242, 242, 242}, {242, 242, 242}, {242, 242, 242}, {242, 242, 242},
  {242, 242, 242}, {242, 242, 242}, {242, 242, 242}, {242, 242, 242},
  {242, 242, 242}, {242, 242, 242}, {242, 242, 242}, {242, 242, 242},
  {242, 242, 242}, {242, 242, 242}, {242, 242, 242}, {242, 242, 242},
  {242, 242, 242}, {242, 242, 242}, {242, 242, 242}, {242, 242, 242},
  {242, 242, 242}, {242, 242, 242}, {242, 242, 242}, {242, 242, 242},
  {242, 242, 242}, {242, 242, 242}, {242, 242, 242}, {242, 242, 242},
  {242, 242, 242}, {242, 242, 242}, {242, 242, 242}, {242, 242, 242},
  {242, 242, 242}, {242, 242, 242}, {242, 242, 242}, {242, 242, 242},
  {242, 242, 242}, {242, 242, 242}, {242, 242, 242}, {242, 242, 242},
  {242, 242, 242}, {242, 242, 242}, {242, 242, 242}, {242, 242, 242},
  {242, 242, 242}, {242, 242, 242}, {242, 242, 242}, {0, 0, 0}}, {{0, 0,
  0}, {241, 241, 241}, {241, 241, 241}, {241, 241, 241}, {241, 241,
  241}, {241, 241, 241}, {241, 241, 241}, {241, 241, 241}, {241, 241,
  241}, {241, 241, 241}, {241, 241, 241}, {241, 241, 241}, {241, 241,
  241}, {241, 241, 241}, {241, 241, 241}, {241, 241, 241}, {241, 241,
  241}, {241, 241, 241}, {241, 241, 241}, {241, 241, 241}, {241, 241,
  241}, {241, 241, 241}, {241, 241, 241}, {241, 241, 241}, {241, 241,
  241}, {241, 241, 241}, {241, 241, 241}, {241, 241, 241}, {241, 241,
  241}, {241, 241, 241}, {241, 241, 241}, {241, 241, 241}, {241, 241,
  241}, {241, 241, 241}, {241, 241, 241}, {241, 241, 241}, {241, 241,
  241}, {241, 241, 241}, {241, 241, 241}, {241, 241, 241}, {241, 241,
  241}, {241, 241, 241}, {241, 241, 241}, {241, 241, 241}, {241, 241,
  241}, {241, 241, 241}, {241, 241, 241}, {241, 241, 241}, {241, 241,
  241}, {241, 241, 241}, {241, 241, 241}, {241, 241, 241}, {241, 241,
  241}, {241, 241, 241}, {241, 241, 241}, {241, 241, 241}, {241, 241,
  241}, {241, 241, 241}, {0, 0, 0}}, {{0, 0, 0}, {240, 240, 240}, {240,
  240, 240}, {240, 240, 240}, {240, 240, 240}, {240, 240, 240}, {240,
  240, 240}, {240, 240, 240}, {240, 240, 240}, {240, 240, 240}, {240,
  240, 240}, {240, 240, 240}, {240, 240, 240}, {240, 240, 240}, {240,
  240, 240}, {240, 240, 240}, {240, 240, 240}, {240, 240, 240}, {240,
  240, 240}, {240, 240, 240}, {240, 240, 240}, {240, 240, 240}, {240,
  240, 240}, {240, 240, 240}, {240, 240, 240}, {240, 240, 240}, {240,
  240, 240}, {240, 240, 240}, {240, 240, 240}, {240, 240, 240}, {240,
  240, 240}, {240, 240, 240}, {240, 240, 240}, {240, 240, 240}, {240,
  240, 240}, {240, 240, 240}, {240, 240, 240}, {240, 240, 240}, {240,
  240, 240}, {240, 240, 240}, {240, 240, 240}, {240, 240, 240}, {240,
  240, 240}, {240, 240, 240}, {240, 240, 240}, {240, 240, 240}, {240,
  240, 240}, {240, 240, 240}, {240, 240, 240}, {240, 240, 240}, {240,
  240, 240}, {240, 240, 240}, {240, 240, 240}, {240, 240, 240}, {240,
  240, 240}, {240, 240, 240}, {240, 240, 240}, {240, 240, 240}, {0, 0,
  0}}, {{0, 0, 0}, {239, 239, 239}, {239, 239, 239}, {239, 239, 239},
  {239, 239, 239}, {239, 239, 239}, {239, 239, 239}, {239, 239, 239},
  {239, 239, 239}, {239, 239, 239}, {239, 239, 239}, {239, 239, 239},
  {239, 239, 239}, {239, 239, 239}, {239, 239, 239}, {239, 239, 239},
  {239, 239, 239}, {239, 239, 239}, {239, 239, 239}, {239, 239, 239},
  {239, 239, 239}, {239, 239, 239}, {239, 239, 239}, {239, 239, 239},
  {239, 239, 239}, {239, 239, 239}, {239, 239, 239}, {239, 239, 239},
  {239, 239, 239}, {239, 239, 239}, {239, 239, 239}, {239, 239, 239},
  {239, 239, 239}, {239, 239, 239}, {239, 239, 239}, {239, 239, 239},
  {239, 239, 239}, {239, 239, 239}, {239, 239, 239}, {239, 239, 239},
  {239, 239, 239}, {239, 239, 239}, {239, 239, 239}, {239, 239, 239},
  {239, 239, 239}, {239, 239, 239}, {239, 239, 239}, {239, 239, 239},
  {239, 239, 239}, {239, 239, 239}, {239, 239, 239}, {239, 239, 239},
  {239, 239, 239}, {239, 239, 239}, {239, 239, 239}, {239, 239, 239},
  {239, 239, 239}, {239, 239, 239}, {0, 0, 0}}, {{0, 0, 0}, {239, 239,
  239}, {239, 239, 239}, {239, 239, 239}, {239, 239, 239}, {239, 239,
  239}, {239, 239, 239}, {239, 239, 239}, {239, 239, 239}, {239, 239,
  239}, {239, 239, 239}, {239, 239, 239}, {239, 239, 239}, {239, 239,
  239}, {239, 239, 239}, {239, 239, 239}, {239, 239, 239}, {239, 239,
  239}, {239, 239, 239}, {239, 239, 239}, {239, 239, 239}, {239, 239,
  239}, {239, 239, 239}, {239, 239, 239}, {239, 239, 239}, {239, 239,
  239}, {239, 239, 239}, {239, 239, 239}, {239, 239, 239}, {239, 239,
  239}, {239, 239, 239}, {239, 239, 239}, {239, 239, 239}, {239, 239,
  239}, {239, 239, 239}, {239, 239, 239}, {239, 239, 239}, {239, 239,
  239}, {239, 239, 239}, {239, 239, 239}, {239, 239, 239}, {239, 239,
  239}, {239, 239, 239}, {239, 239, 239}, {239, 239, 239}, {239, 239,
  239}, {239, 239, 239}, {239, 239, 239}, {239, 239, 239}, {239, 239,
  239}, {239, 239, 239}, {239, 239, 239}, {239, 239, 239}, {239, 239,
  239}, {239, 239, 239}, {239, 239, 239}, {239, 239, 239}, {239, 239,
  239}, {0, 0, 0}}, {{0, 0, 0}, {238, 238, 238}, {238, 238, 238}, {238,
  238, 238}, {238, 238, 238}, {238, 238, 238}, {238, 238, 238}, {238,
  238, 238}, {238, 238, 238}, {238, 238, 238}, {238, 238, 238}, {238,
  238, 238}, {238, 238, 238}, {238, 238, 238}, {238, 238, 238}, {238,
  238, 238}, {238, 238, 238}, {238, 238, 238}, {238, 238, 238}, {238,
  238, 238}, {238, 238, 238}, {238, 238, 238}, {238, 238, 238}, {238,
  238, 238}, {238, 238, 238}, {238, 238, 238}, {238, 238, 238}, {238,
  238, 238}, {238, 238, 238}, {238, 238, 238}, {238, 238, 238}, {238,
  238, 238}, {238, 238, 238}, {238, 238, 238}, {238, 238, 238}, {238,
  238, 238}, {238, 238, 238}, {238, 238, 238}, {238, 238, 238}, {238,
  238, 238}, {238, 238, 238}, {238, 238, 238}, {238, 238, 238}, {238,
  238, 238}, {238, 238, 238}, {238, 238, 238}, {238, 238, 238}, {238,
  238, 238}, {238, 238, 238}, {238, 238, 238}, {238, 238, 238}, {238,
  238, 238}, {238, 238, 238}, {238, 238, 238}, {238, 238, 238}, {238,
  238, 238}, {238, 238, 238}, {238, 238, 238}, {0, 0, 0}}, {{0, 0, 0},
  {237, 237, 237}, {237, 237, 237}, {237, 237, 237}, {237, 237, 237},
  {237, 237, 237}, {237, 237, 237}, {237, 237, 237}, {237, 237, 237},
  {237, 237, 237}, {237, 237, 237}, {237, 237, 237}, {237, 237, 237},
  {237, 237, 237}, {237, 237, 237}, {237, 237, 237}, {237, 237, 237},
  {237, 237, 237}, {237, 237, 237}, {237, 237, 237}, {237, 237, 237},
  {237, 237, 237}, {237, 237, 237}, {237, 237, 237}, {237, 237, 237},
  {237, 237, 237}, {237, 237, 237}, {237, 237, 237}, {237, 237, 237},
  {237, 237, 237}, {237, 237, 237}, {237, 237, 237}, {237, 237, 237},
  {237, 237, 237}, {237, 237, 237}, {237, 237, 237}, {237, 237, 237},
  {237, 237, 237}, {237, 237, 237}, {237, 237, 237}, {237, 237, 237},
  {237, 237, 237}, {237, 237, 237}, {237, 237, 237}, {237, 237, 237},
  {237, 237, 237}, {237, 237, 237}, {237, 237, 237}, {237, 237, 237},
  {237, 237, 237}, {237, 237, 237}, {237, 237, 237}, {237, 237, 237},
  {237, 237, 237}, {237, 237, 237}, {237, 237, 237}, {237, 237, 237},
  {237, 237, 237}, {0, 0, 0}}, {{0, 0, 0}, {236, 236, 236}, {236, 236,
  236}, {236, 236, 236}, {236, 236, 236}, {236, 236, 236}, {236, 236,
  236}, {236, 236, 236}, {236, 236, 236}, {236, 236, 236}, {236, 236,
  236}, {236, 236, 236}, {236, 236, 236}, {236, 236, 236}, {236, 236,
  236}, {236, 236, 236}, {236, 236, 236}, {236, 236, 236}, {236, 236,
  236}, {236, 236, 236}, {236, 236, 236}, {236, 236, 236}, {236, 236,
  236}, {236, 236, 236}, {236, 236, 236}, {236, 236, 236}, {236, 236,
  236}, {236, 236, 236}, {236, 236, 236}, {236, 236, 236}, {236, 236,
  236}, {236, 236, 236}, {236, 236, 236}, {236, 236, 236}, {236, 236,
  236}, {236, 236, 236}, {236, 236, 236}, {236, 236, 236}, {236, 236,
  236}, {236, 236, 236}, {236, 236, 236}, {236, 236, 236}, {236, 236,
  236}, {236, 236, 236}, {236, 236, 236}, {236, 236, 236}, {236, 236,
  236}, {236, 236, 236}, {236, 236, 236}, {236, 236, 236}, {236, 236,
  236}, {236, 236, 236}, {236, 236, 236}, {236, 236, 236}, {236, 236,
  236}, {236, 236, 236}, {236, 236, 236}, {236, 236, 236}, {0, 0, 0}},
  {{0, 0, 0}, {236, 236, 236}, {236, 236, 236}, {236, 236, 236}, {236,
  236, 236}, {236, 236, 236}, {236, 236, 236}, {236, 236, 236}, {236,
  236, 236}, {236, 236, 236}, {236, 236, 236}, {236, 236, 236}, {236,
  236, 236}, {236, 236, 236}, {236, 236, 236}, {236, 236, 236}, {236,
  236, 236}, {236, 236, 236}, {236, 236, 236}, {236, 236, 236}, {236,
  236, 236}, {236, 236, 236}, {236, 236, 236}, {236, 236, 236}, {236,
  236, 236}, {236, 236, 236}, {236, 236, 236}, {236, 236, 236}, {236,
  236, 236}, {236, 236, 236}, {236, 236, 236}, {236, 236, 236}, {236,
  236, 236}, {236, 236, 236}, {236, 236, 236}, {236, 236, 236}, {236,
  236, 236}, {236, 236, 236}, {236, 236, 236}, {236, 236, 236}, {236,
  236, 236}, {236, 236, 236}, {236, 236, 236}, {236, 236, 236}, {236,
  236, 236}, {236, 236, 236}, {236, 236, 236}, {236, 236, 236}, {236,
  236, 236}, {236, 236, 236}, {236, 236, 236}, {236, 236, 236}, {236,
  236, 236}, {236, 236, 236}, {236, 236, 236}, {236, 236, 236}, {236,
  236, 236}, {236, 236, 236}, {0, 0, 0}}, {{0, 0, 0}, {235, 235, 235},
  {235, 235, 235}, {235, 235, 235}, {235, 235, 235}, {235, 235, 235},
  {235, 235, 235}, {235, 235, 235}, {235, 235, 235}, {235, 235, 235},
  {235, 235, 235}, {235, 235, 235}, {235, 235, 235}, {235, 235, 235},
  {235, 235, 235}, {235, 235, 235}, {235, 235, 235}, {235, 235, 235},
  {235, 235, 235}, {235, 235, 235}, {235, 235, 235}, {235, 235, 235},
  {235, 235, 235}, {235, 235, 235}, {235, 235, 235}, {235, 235, 235},
  {235, 235, 235}, {235, 235, 235}, {235, 235, 235}, {235, 235, 235},
  {235, 235, 235}, {235, 235, 235}, {235, 235, 235}, {235, 235, 235},
  {235, 235, 235}, {235, 235, 235}, {235, 235, 235}, {235, 235, 235},
  {235, 235, 235}, {235, 235, 235}, {235, 235, 235}, {235, 235, 235},
  {235, 235, 235}, {235, 235, 235}, {235, 235, 235}, {235, 235, 235},
  {235, 235, 235}, {235, 235, 235}, {235, 235, 235}, {235, 235, 235},
  {235, 235, 235}, {235, 235, 235}, {235, 235, 235}, {235, 235, 235},
  {235, 235, 235}, {235, 235, 235}, {235, 235, 235}, {235, 235, 235},
  {0, 0, 0}}, {{0, 0, 0}, {234, 234, 234}, {234, 234, 234}, {234, 234,
  234}, {234, 234, 234}, {234, 234, 234}, {234, 234, 234}, {234, 234,
  234}, {234, 234, 234}, {234, 234, 234}, {234, 234, 234}, {234, 234,
  234}, {234, 234, 234}, {234, 234, 234}, {234, 234, 234}, {234, 234,
  234}, {234, 234, 234}, {234, 234, 234}, {234, 234, 234}, {234, 234,
  234}, {234, 234, 234}, {234, 234, 234}, {234, 234, 234}, {234, 234,
  234}, {234, 234, 234}, {234, 234, 234}, {234, 234, 234}, {234, 234,
  234}, {234, 234, 234}, {234, 234, 234}, {234, 234, 234}, {234, 234,
  234}, {234, 234, 234}, {234, 234, 234}, {234, 234, 234}, {234, 234,
  234}, {234, 234, 234}, {234, 234, 234}, {234, 234, 234}, {234, 234,
  234}, {234, 234, 234}, {234, 234, 234}, {234, 234, 234}, {234, 234,
  234}, {234, 234, 234}, {234, 234, 234}, {234, 234, 234}, {234, 234,
  234}, {234, 234, 234}, {234, 234, 234}, {234, 234, 234}, {234, 234,
  234}, {234, 234, 234}, {234, 234, 234}, {234, 234, 234}, {234, 234,
  234}, {234, 234, 234}, {234, 234, 234}, {0, 0, 0}}, {{0, 0, 0}, {233,
  233, 233}, {233, 233, 233}, {233, 233, 233}, {233, 233, 233}, {233,
  233, 233}, {233, 233, 233}, {233, 233, 233}, {233, 233, 233}, {233,
  233, 233}, {233, 233, 233}, {233, 233, 233}, {233, 233, 233}, {233,
  233, 233}, {233, 233, 233}, {233, 233, 233}, {233, 233, 233}, {233,
  233, 233}, {233, 233, 233}, {233, 233, 233}, {233, 233, 233}, {233,
  233, 233}, {233, 233, 233}, {233, 233, 233}, {233, 233, 233}, {233,
  233, 233}, {233, 233, 233}, {233, 233, 233}, {233, 233, 233}, {233,
  233, 233}, {233, 233, 233}, {233, 233, 233}, {233, 233, 233}, {233,
  233, 233}, {233, 233, 233}, {233, 233, 233}, {233, 233, 233}, {233,
  233, 233}, {233, 233, 233}, {233, 233, 233}, {233, 233, 233}, {233,
  233, 233}, {233, 233, 233}, {233, 233, 233}, {233, 233, 233}, {233,
  233, 233}, {233, 233, 233}, {233, 233, 233}, {233, 233, 233}, {233,
  233, 233}, {233, 233, 233}, {233, 233, 233}, {233, 233, 233}, {233,
  233, 233}, {233, 233, 233}, {233, 233, 233}, {233, 233, 233}, {233,
  233, 233}, {0, 0, 0}}, {{0, 0, 0}, {233, 233, 233}, {233, 233, 233},
  {233, 233, 233}, {233, 233, 233}, {233, 233, 233}, {233, 233, 233},
  {233, 233, 233}, {233, 233, 233}, {233, 233, 233}, {233, 233, 233},
  {233, 233, 233}, {233, 233, 233}, {233, 233, 233}, {233, 233, 233},
  {233, 233, 233}, {233, 233, 233}, {233, 233, 233}, {233, 233, 233},
  {233, 233, 233}, {233, 233, 233}, {233, 233, 233}, {233, 233, 233},
  {233, 233, 233}, {233, 233, 233}, {233, 233, 233}, {233, 233, 233},
  {233, 233, 233}, {233, 233, 233}, {233, 233, 233}, {233, 233, 233},
  {233, 233, 233}, {233, 233, 233}, {233, 233, 233}, {233, 233, 233},
  {233, 233, 233}, {233, 233, 233}, {233, 233, 233}, {233, 233, 233},
  {233, 233, 233}, {233, 233, 233}, {233, 233, 233}, {233, 233, 233},
  {233, 233, 233}, {233, 233, 233}, {233, 233, 233}, {233, 233, 233},
  {233, 233, 233}, {233, 233, 233}, {233, 233, 233}, {233, 233, 233},
  {233, 233, 233}, {233, 233, 233}, {233, 233, 233}, {233, 233, 233},
  {233, 233, 233}, {233, 233, 233}, {233, 233, 233}, {0, 0, 0}}, {{0, 0,
  0}, {232, 232, 232}, {232, 232, 232}, {232, 232, 232}, {232, 232,
  232}, {232, 232, 232}, {232, 232, 232}, {232, 232, 232}, {232, 232,
  232}, {232, 232, 232}, {232, 232, 232}, {232, 232, 232}, {232, 232,
  232}, {232, 232, 232}, {232, 232, 232}, {232, 232, 232}, {232, 232,
  232}, {232, 232, 232}, {232, 232, 232}, {232, 232, 232}, {232, 232,
  232}, {232, 232, 232}, {232, 232, 232}, {232, 232, 232}, {232, 232,
  232}, {232, 232, 232}, {232, 232, 232}, {232, 232, 232}, {232, 232,
  232}, {232, 232, 232}, {232, 232, 232}, {232, 232, 232}, {232, 232,
  232}, {232, 232, 232}, {232, 232, 232}, {232, 232, 232}, {232, 232,
  232}, {232, 232, 232}, {232, 232, 232}, {232, 232, 232}, {232, 232,
  232}, {232, 232, 232}, {232, 232, 232}, {232, 232, 232}, {232, 232,
  232}, {232, 232, 232}, {232, 232, 232}, {232, 232, 232}, {232, 232,
  232}, {232, 232, 232}, {232, 232, 232}, {232, 232, 232}, {232, 232,
  232}, {232, 232, 232}, {232, 232, 232}, {232, 232, 232}, {232, 232,
  232}, {232, 232, 232}, {0, 0, 0}}, {{0, 0, 0}, {231, 231, 231}, {231,
  231, 231}, {231, 231, 231}, {231, 231, 231}, {231, 231, 231}, {231,
  231, 231}, {231, 231, 231}, {231, 231, 231}, {231, 231, 231}, {231,
  231, 231}, {231, 231, 231}, {231, 231, 231}, {231, 231, 231}, {231,
  231, 231}, {231, 231, 231}, {231, 231, 231}, {231, 231, 231}, {231,
  231, 231}, {231, 231, 231}, {231, 231, 231}, {231, 231, 231}, {231,
  231, 231}, {231, 231, 231}, {231, 231, 231}, {231, 231, 231}, {231,
  231, 231}, {231, 231, 231}, {231, 231, 231}, {231, 231, 231}, {231,
  231, 231}, {231, 231, 231}, {231, 231, 231}, {231, 231, 231}, {231,
  231, 231}, {231, 231, 231}, {231, 231, 231}, {231, 231, 231}, {231,
  231, 231}, {231, 231, 231}, {231, 231, 231}, {231, 231, 231}, {231,
  231, 231}, {231, 231, 231}, {231, 231, 231}, {231, 231, 231}, {231,
  231, 231}, {231, 231, 231}, {231, 231, 231}, {231, 231, 231}, {231,
  231, 231}, {231, 231, 231}, {231, 231, 231}, {231, 231, 231}, {231,
  231, 231}, {231, 231, 231}, {231, 231, 231}, {231, 231, 231}, {0, 0,
  0}}, {{0, 0, 0}, {231, 231, 231}, {231, 231, 231}, {231, 231, 231},
  {231, 231, 231}, {231, 231, 231}, {231, 231, 231}, {231, 231, 231},
  {231, 231, 231}, {231, 231, 231}, {231, 231, 231}, {231, 231, 231},
  {231, 231, 231}, {231, 231, 231}, {231, 231, 231}, {231, 231, 231},
  {231, 231, 231}, {231, 231, 231}, {231, 231, 231}, {231, 231, 231},
  {231, 231, 231}, {231, 231, 231}, {231, 231, 231}, {231, 231, 231},
  {231, 231, 231}, {231, 231, 231}, {231, 231, 231}, {231, 231, 231},
  {231, 231, 231}, {231, 231, 231}, {231, 231, 231}, {231, 231, 231},
  {231, 231, 231}, {231, 231, 231}, {231, 231, 231}, {231, 231, 231},
  {231, 231, 231}, {231, 231, 231}, {231, 231, 231}, {231, 231, 231},
  {231, 231, 231}, {231, 231, 231}, {231, 231, 231}, {231, 231, 231},
  {231, 231, 231}, {231, 231, 231}, {231, 231, 231}, {231, 231, 231},
  {231, 231, 231}, {231, 231, 231}, {231, 231, 231}, {231, 231, 231},
  {231, 231, 231}, {231, 231, 231}, {231, 231, 231}, {231, 231, 231},
  {231, 231, 231}, {231, 231, 231}, {0, 0, 0}}, {{0, 0, 0}, {230, 230,
  230}, {230, 230, 230}, {230, 230, 230}, {230, 230, 230}, {230, 230,
  230}, {230, 230, 230}, {230, 230, 230}, {230, 230, 230}, {230, 230,
  230}, {230, 230, 230}, {230, 230, 230}, {230, 230, 230}, {230, 230,
  230}, {230, 230, 230}, {230, 230, 230}, {230, 230, 230}, {230, 230,
  230}, {230, 230, 230}, {230, 230, 230}, {230, 230, 230}, {230, 230,
  230}, {230, 230, 230}, {230, 230, 230}, {230, 230, 230}, {230, 230,
  230}, {230, 230, 230}, {230, 230, 230}, {230, 230, 230}, {230, 230,
  230}, {230, 230, 230}, {230, 230, 230}, {230, 230, 230}, {230, 230,
  230}, {230, 230, 230}, {230, 230, 230}, {230, 230, 230}, {230, 230,
  230}, {230, 230, 230}, {230, 230, 230}, {230, 230, 230}, {230, 230,
  230}, {230, 230, 230}, {230, 230, 230}, {230, 230, 230}, {230, 230,
  230}, {230, 230, 230}, {230, 230, 230}, {230, 230, 230}, {230, 230,
  230}, {230, 230, 230}, {230, 230, 230}, {230, 230, 230}, {230, 230,
  230}, {230, 230, 230}, {230, 230, 230}, {230, 230, 230}, {230, 230,
  230}, {0, 0, 0}}, {{0, 0, 0}, {230, 230, 230}, {230, 230, 230}, {230,
  230, 230}, {230, 230, 230}, {230, 230, 230}, {230, 230, 230}, {230,
  230, 230}, {230, 230, 230}, {230, 230, 230}, {230, 230, 230}, {230,
  230, 230}, {230, 230, 230}, {230, 230, 230}, {230, 230, 230}, {230,
  230, 230}, {230, 230, 230}, {230, 230, 230}, {230, 230, 230}, {230,
  230, 230}, {230, 230, 230}, {230, 230, 230}, {230, 230, 230}, {230,
  230, 230}, {230, 230, 230}, {230, 230, 230}, {230, 230, 230}, {230,
  230, 230}, {230, 230, 230}, {230, 230, 230}, {230, 230, 230}, {230,
  230, 230}, {230, 230, 230}, {230, 230, 230}, {230, 230, 230}, {230,
  230, 230}, {230, 230, 230}, {230, 230, 230}, {230, 230, 230}, {230,
  230, 230}, {230, 230, 230}, {230, 230, 230}, {230, 230, 230}, {230,
  230, 230}, {230, 230, 230}, {230, 230, 230}, {230, 230, 230}, {230,
  230, 230}, {230, 230, 230}, {230, 230, 230}, {230, 230, 230}, {230,
  230, 230}, {230, 230, 230}, {230, 230, 230}, {230, 230, 230}, {230,
  230, 230}, {230, 230, 230}, {230, 230, 230}, {0, 0, 0}}, {{255, 255,
  255}, {229, 229, 229}, {229, 229, 229}, {229, 229, 229}, {229, 229,
  229}, {229, 229, 229}, {229, 229, 229}, {229, 229, 229}, {229, 229,
  229}, {229, 229, 229}, {229, 229, 229}, {229, 229, 229}, {229, 229,
  229}, {229, 229, 229}, {229, 229, 229}, {229, 229, 229}, {229, 229,
  229}, {229, 229, 229}, {229, 229, 229}, {229, 229, 229}, {229, 229,
  229}, {229, 229, 229}, {229, 229, 229}, {229, 229, 229}, {229, 229,
  229}, {229, 229, 229}, {229, 229, 229}, {229, 229, 229}, {229, 229,
  229}, {229, 229, 229}, {229, 229, 229}, {229, 229, 229}, {229, 229,
  229}, {229, 229, 229}, {229, 229, 229}, {229, 229, 229}, {229, 229,
  229}, {229, 229, 229}, {229, 229, 229}, {229, 229, 229}, {229, 229,
  229}, {229, 229, 229}, {229, 229, 229}, {229, 229, 229}, {229, 229,
  229}, {229, 229, 229}, {229, 229, 229}, {229, 229, 229}, {229, 229,
  229}, {229, 229, 229}, {229, 229, 229}, {229, 229, 229}, {229, 229,
  229}, {229, 229, 229}, {229, 229, 229}, {229, 229, 229}, {229, 229,
  229}, {229, 229, 229}, {255, 255, 255}}, {{255, 255, 255}, {229, 229,
  229}, {229, 229, 229}, {229, 229, 229}, {229, 229, 229}, {229, 229,
  229}, {229, 229, 229}, {229, 229, 229}, {229, 229, 229}, {229, 229,
  229}, {229, 229, 229}, {229, 229, 229}, {229, 229, 229}, {229, 229,
  229}, {229, 229, 229}, {229, 229, 229}, {229, 229, 229}, {229, 229,
  229}, {229, 229, 229}, {229, 229, 229}, {229, 229, 229}, {229, 229,
  229}, {229, 229, 229}, {229, 229, 229}, {229, 229, 229}, {229, 229,
  229}, {229, 229, 229}, {229, 229, 229}, {229, 229, 229}, {229, 229,
  229}, {229, 229, 229}, {229, 229, 229}, {229, 229, 229}, {229, 229,
  229}, {229, 229, 229}, {229, 229, 229}, {229, 229, 229}, {229, 229,
  229}, {229, 229, 229}, {229, 229, 229}, {229, 229, 229}, {229, 229,
  229}, {229, 229, 229}, {229, 229, 229}, {229, 229, 229}, {229, 229,
  229}, {229, 229, 229}, {229, 229, 229}, {229, 229, 229}, {229, 229,
  229}, {229, 229, 229}, {229, 229, 229}, {229, 229, 229}, {229, 229,
  229}, {229, 229, 229}, {229, 229, 229}, {229, 229, 229}, {229, 229,
  229}, {255, 255, 255}}, {{255, 255, 255}, {228, 228, 228}, {228, 228,
  228}, {228, 228, 228}, {228, 228, 228}, {228, 228, 228}, {228, 228,
  228}, {228, 228, 228}, {228, 228, 228}, {228, 228, 228}, {228, 228,
  228}, {228, 228, 228}, {228, 228, 228}, {228, 228, 228}, {228, 228,
  228}, {228, 228, 228}, {228, 228, 228}, {228, 228, 228}, {228, 228,
  228}, {228, 228, 228}, {228, 228, 228}, {228, 228, 228}, {228, 228,
  228}, {228, 228, 228}, {228, 228, 228}, {228, 228, 228}, {228, 228,
  228}, {228, 228, 228}, {228, 228, 228}, {228, 228, 228}, {228, 228,
  228}, {228, 228, 228}, {228, 228, 228}, {228, 228, 228}, {228, 228,
  228}, {228, 228, 228}, {228, 228, 228}, {228, 228, 228}, {228, 228,
  228}, {228, 228, 228}, {228, 228, 228}, {228, 228, 228}, {228, 228,
  228}, {228, 228, 228}, {228, 228, 228}, {228, 228, 228}, {228, 228,
  228}, {228, 228, 228}, {228, 228, 228}, {228, 228, 228}, {228, 228,
  228}, {228, 228, 228}, {228, 228, 228}, {228, 228, 228}, {228, 228,
  228}, {228, 228, 228}, {228, 228, 228}, {228, 228, 228}, {255, 255,
  255}}, {{255, 255, 255}, {228, 228, 228}, {228, 228, 228}, {228, 228,
  228}, {228, 228, 228}, {228, 228, 228}, {228, 228, 228}, {228, 228,
  228}, {228, 228, 228}, {228, 228, 228}, {228, 228, 228}, {228, 228,
  228}, {228, 228, 228}, {228, 228, 228}, {228, 228, 228}, {228, 228,
  228}, {228, 228, 228}, {228, 228, 228}, {228, 228, 228}, {228, 228,
  228}, {228, 228, 228}, {228, 228, 228}, {228, 228, 228}, {228, 228,
  228}, {228, 228, 228}, {228, 228, 228}, {228, 228, 228}, {228, 228,
  228}, {228, 228, 228}, {228, 228, 228}, {228, 228, 228}, {228, 228,
  228}, {228, 228, 228}, {228, 228, 228}, {228, 228, 228}, {228, 228,
  228}, {228, 228, 228}, {228, 228, 228}, {228, 228, 228}, {228, 228,
  228}, {228, 228, 228}, {228, 228, 228}, {228, 228, 228}, {228, 228,
  228}, {228, 228, 228}, {228, 228, 228}, {228, 228, 228}, {228, 228,
  228}, {228, 228, 228}, {228, 228, 228}, {228, 228, 228}, {228, 228,
  228}, {228, 228, 228}, {228, 228, 228}, {228, 228, 228}, {228, 228,
  228}, {228, 228, 228}, {228, 228, 228}, {255, 255, 255}}, {{255, 255,
  255}, {195, 200, 200}, {195, 200, 200}, {195, 200, 200}, {195, 200,
  200}, {195, 200, 200}, {195, 200, 200}, {195, 200, 200}, {195, 200,
  200}, {195, 200, 200}, {195, 200, 200}, {195, 200, 200}, {195, 200,
  200}, {195, 200, 200}, {195, 200, 200}, {195, 200, 200}, {195, 200,
  200}, {195, 200, 200}, {195, 200, 200}, {195, 200, 200}, {195, 200,
  200}, {195, 200, 200}, {195, 200, 200}, {195, 200, 200}, {195, 200,
  200}, {195, 200, 200}, {195, 200, 200}, {195, 200, 200}, {195, 200,
  200}, {195, 200, 200}, {195, 200, 200}, {195, 200, 200}, {195, 200,
  200}, {195, 200, 200}, {195, 200, 200}, {195, 200, 200}, {195, 200,
  200}, {195, 200, 200}, {195, 200, 200}, {195, 200, 200}, {195, 200,
  200}, {195, 200, 200}, {195, 200, 200}, {195, 200, 200}, {195, 200,
  200}, {195, 200, 200}, {195, 200, 200}, {195, 200, 200}, {195, 200,
  200}, {195, 200, 200}, {195, 200, 200}, {195, 200, 200}, {195, 200,
  200}, {195, 200, 200}, {195, 200, 200}, {195, 200, 200}, {195, 200,
  200}, {195, 200, 200}, {255, 255, 255}}, {{255, 255, 255}, {0, 0, 0},
  {0, 0, 0}, {0, 0, 0}, {0, 0, 0}, {0, 0, 0}, {0, 0, 0}, {0, 0, 0}, {0,
  0, 0}, {0, 0, 0}, {0, 0, 0}, {0, 0, 0}, {0, 0, 0}, {0, 0, 0}, {0, 0,
  0}, {0, 0, 0}, {0, 0, 0}, {0, 0, 0}, {0, 0, 0}, {0, 0, 0}, {0, 0, 0},
  {0, 0, 0}, {0, 0, 0}, {0, 0, 0}, {0, 0, 0}, {0, 0, 0}, {0, 0, 0}, {0,
  0, 0}, {0, 0, 0}, {0, 0, 0}, {0, 0, 0}, {0, 0, 0}, {0, 0, 0}, {0, 0,
  0}, {0, 0, 0}, {0, 0, 0}, {0, 0, 0}, {0, 0, 0}, {0, 0, 0}, {0, 0, 0},
  {0, 0, 0}, {0, 0, 0}, {0, 0, 0}, {0, 0, 0}, {0, 0, 0}, {0, 0, 0}, {0,
  0, 0}, {0, 0, 0}, {0, 0, 0}, {0, 0, 0}, {0, 0, 0}, {0, 0, 0}, {0, 0,
  0}, {0, 0, 0}, {0, 0, 0}, {0, 0, 0}, {0, 0, 0}, {0, 0, 0}, {255, 255,
  255}}}], "Byte", ColorSpace -> "RGB", Interleaving -> True]
  
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


icon=Graphics[{EdgeForm[{Thickness[.025], Black}], FaceForm[White], 
  Rectangle[{0, 0}, {1, .75}, RoundingRadius -> .2], Red, 
  Arrowheads[.4], Thickness[.1], Opacity[.5], 
  Arrow[{{0, .25}, {1, .25}}], Blue, Arrow[{{1, .5}, {0, .5}}]},ImageSize -> {Automatic, Dynamic[3.5*CurrentValue["FontCapHeight"]]}];

ServiceObject/:
MakeBoxes[service_ServiceObject, form:StandardForm|TraditionalForm] := 
With[{name=getServiceName[service], id=getServiceID[service]},
	If[$VersionNumber>=10,
		BoxForm`ArrangeSummaryBox[
			(* Head *)ServiceObject, 
			(* Interpretation *)service, 
			(* Icon *)icon, 
			(* Column or Grid *){name, (* id, *) Dynamic[If[TrueQ[authenticatedServiceQ[id]],"Connected","Not Connected"]]}, 
			(* Plus Box Column or Grid *){"Properties"}, 
			form]
			,
		InterpretationBox[#,service]&@ToBoxes[Framed@Row[{"ServiceObject       ",Column[{name, id, "Authenticated"->authenticatedServiceQ[id]}]}]]
	]
]


$shortTtypesetting = False;

(*
DeviceObject/:
in:(Message|Information)[__, dev_DeviceObject, ___] := Block[{$shortTtypesetting = True},
	in
]/;!$shortTtypesetting
*)


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
