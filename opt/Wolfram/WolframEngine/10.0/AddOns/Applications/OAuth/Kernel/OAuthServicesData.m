(* 
This package contains the information about individual OAuth services for which Wolfram provides support 
The values here use Wolfram's "app" to provide a link between Mathematica and the users account on a service.
*)

System`Private`NewContextPath[{"OAuthClient`","System`"}];
(*
BeginPackage["OAuthServicesData`"]
*)
(* Exported symbols added here with SymbolName::usage *)  
OAuthClient`$predefinedservicelist;
OAuthClient`OAuthServicesData;
OAuthClient`oauthcookeddata;
OAuthClient`oauthsendmessage;
OAuthClient`addservice;

Begin["OAuthServicesDataDump`"] (* Begin Private Context *) 

(Unprotect[#]; Clear[#])& /@ {$predefinedservicelist,OAuthServicesData,oauthcookeddata,oauthsendmessage,addservice}

defaultOAuthParams={
					(* defaults *)
					"ServiceName"       -> Null,
				    "OAuthVersion"		-> "1.0a",
				    "RequestEndpoint"   -> "",
				    "AccessEndpoint"    -> Null,
				    "AuthorizeEndpoint" -> Null,
				   	"RedirectURI"		->	"oob",
				   	"VerifierLabel"		-> "verifier",
				   	"AdditionalOAuthParameter"	-> None,
				   	"Scope"				-> None,
				    "AuthenticationDialog" 	-> "TokenDialog",
				    "RequestFormat"		-> "URL",
				    "ResponseType"		-> "code",
				    "AccessTokenExtractor"	-> None,
				    "Information"		-> ""
				    };

defaultOAuthLabels=First/@defaultOAuthParams;		    
(*************************** OAuthServices *************************************)
(* A simple function for retrieving data from below *)
$predefinedservicelist={}

OAuthServicesData[args___]:=With[{res=oauthservices[args]},
	res/;res=!=$Failed&&Head[res]=!=oauthservicedata]

oauthservices[name_,prop___]:=Module[{data=oauthservicedata[name],availableproperties},
	availableproperties=First/@data;
	Switch[{prop},
		{},	data,
		{"Requests"},availableproperties,
		{"Authentication"},
			Join[Thread[defaultOAuthLabels->(defaultOAuthLabels/.Join[data,defaultOAuthParams])],
				FilterRules[data,Except[Join[defaultOAuthLabels,{"Gets","Posts","RawGets","RawPosts","ClientInfo"}]]]]
		,
		{Alternatives@@availableproperties},
		prop/.data,
		_,
		debugPrint[Hold[oauthservicedata][name,prop]];
		oauthservicedata[name,prop]		
	]
]

oauthservices[___]:=$Failed
OAuthServicesData[___]:=$Failed
(*************************** Data for Services *********************************)
(* Storage of information about our apps and the services they work with *)

$packagedirectory=FileNameJoin[{DirectoryName[System`Private`$InputFileName],"Services"}];

addservice[name_, dir_:$packagedirectory]:=Module[{funs},
	Unprotect[$predefinedservicelist,oauthservicedata,oauthcookeddata,oauthsendmessage];
	$predefinedservicelist=Union[AppendTo[$predefinedservicelist,name]];
	funs=Get[FileNameJoin[{dir, name<>"OAuth.m"}]];
	debugPrint["funs"->funs];
	oauthservicedata[name,args___]:=funs[[1]][args];
	oauthcookeddata[name,args___]:=funs[[2]][args];
	oauthsendmessage[name,args___]:=funs[[3]][args];
	If[Length[funs]>4,
		OAuthClient`checkpermissions[name,args___]:=funs[[4]][args];
		OAuthClient`addpermissions[name,args___]:=funs[[5]][args];
	];
	Protect[$predefinedservicelist,oauthservicedata,oauthcookeddata,oauthsendmessage];
]

addservice["Facebook"]
addservice["Instagram"]
addservice["Twitter"]
addservice["LinkedIn"]
addservice["Dropbox"]
addservice["GooglePlus"]
addservice["Fitbit"]
addservice["Runkeeper"]
(* will be phased out by Jawbone
addservice["BodyMedia"]
*)


Unprotect[oauthcookeddata,oauthsendmessage,oauthservicedata];

oauthservicedata[___]:=$Failed

(**** error handling ***)
oauthcookeddata[args___]:=Throw[$Failed]
oauthsendmessage[___]:=Throw[$Failed]
OAuthClient`checkpermissions[___]:=All
OAuthClient`addpermissions[___]:=Throw[$Failed]

parameterspresentQ[args_, params_]:=And@@(!FreeQ[args,(Rule|RuleDelayed)[#,_]]&/@params)

End[] (* End Private Context *)

SetAttributes[{$predefinedservicelist,OAuthServicesData,oauthcookeddata,oauthsendmessage,addservice},{ReadProtected, Protected}];

System`Private`RestoreContextPath[];

{}
(*
{OAuthClient`OAuthServicesData,OAuthClient`oauthcookeddata,OAuthClient`oauthsendmessage}
*)