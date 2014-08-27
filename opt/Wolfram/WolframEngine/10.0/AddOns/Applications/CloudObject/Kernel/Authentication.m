(* Mathematica package *)
BeginPackage["CloudObject`"]

System`CloudConnect;
System`CloudDisconnect;
System`$CloudConnected;
System`$WolframID;
System`$WolframUUID;
System`$RegisteredUserName;
$UseRemoteServer;
$CloudDebug;

Begin["`Private`"]

Unprotect[CloudConnect];
Unprotect[CloudDisconnect];
Unprotect[$CloudConnected];

CloudConnect::notauth = "Unable to authenticate with Wolfram Cloud server. Please try authenticating again."
CloudConnect::creds = "Incorrect username or password.";

$AuthenticationMethod = "XAuth"(*"Legacy"*);
$tag = "CloudObjectCatchThrowTag";
If[Not[ValueQ[$CloudConnected]],Set[$CloudConnected,TrueQ[$CloudEvaluation]]];

Options[CloudConnect] = {"RememberMe"->Automatic};

CloudConnect[args___,OptionsPattern[]]:=With[{res=Catch[
	With[{ov=Check[OptionValue["RememberMe"],Throw[$Failed,$tag],{OptionValue::nodef,OptionValue::optnf}]},
	If[
		Not[MatchQ[ov,Automatic|True|False]],
		Message[CloudConnect::opttf,"RememberMe",ov];Throw[$Failed,$tag]
	]];
	Block[{$CacheLogin = TrueQ[OptionValue["RememberMe"]/.Automatic->True]},
	iCloudConnect[args]],
	$tag]},
	res/;MatchQ[res,$WolframID|$Failed|$Canceled]]/;Not[TrueQ[$CloudEvaluation]]

iCloudConnect[] := With[{r=If[TrueQ[$CacheLogin],fetchCredentials[],$Failed]},
	If[FreeQ[r,$Failed],$WolframID,iCloudConnect[""]]]
iCloudConnect[username_String] := If[$UseRemoteServer === True, 
	With[
		{r=If[TrueQ[$CacheLogin],fetchCredentials[],$Failed]},
		If[
			FreeQ[r,$Failed],
			If[SameQ[username,$WolframID],
				$WolframID,
				CloudDisconnect[];authenticate[username]
			],
			authenticate[username]
		]
	]
]
iCloudConnect[username_String,password_String] := If[$UseRemoteServer === True, authenticate[username,password]]
iCloudConnect[args__] := (ArgumentCountQ[CloudConnect,Length[DeleteCases[{args},_Rule,Infinity]],0,2];Null/;False)

CloudDisconnect[] := (setCloudConnected[False];
setWolframID[None];
setWolframUUID[None];
setRegisteredUserName[""];
setAccessData["",""];
Quiet[DeleteFile[FileNameJoin[{$credsDir,$credsFile}]]];
)/;Not[TrueQ[$CloudEvaluation]]
CloudDisconnect[args__] := (ArgumentCountQ[CloudDisconnect,Length[{args}],0,0];Null/;False)

fetchCredentials[] := Catch[
	If[Not[FreeQ[getCredentials[],$Failed]],Throw[$Failed,$tag]];
	If[Not[StringQ[$CloudBase]],Throw[$Failed,$tag]];
	With[{r=authenticatedURLFetch[
		If[SameQ[StringTake[$CloudBase,-1],"/"],
			StringJoin[$CloudBase,"files/auth"],
			StringJoin[$CloudBase,"/files/auth"]
		],"StatusCode"]},(*ping server to verify credentials are still valid*)
		If[
			UnsameQ[r,200],(*TODO: needs more error handling*)
			Message[CloudConnect::notauth];CloudDisconnect[];Throw[$Failed,$tag],
			$WolframID
		]
	]
	,$tag]

authenticate[username_String]:=With[{creds=loginDialog[username]},
    (*placeholder while we wait on real authentication*)
If[creds===$Canceled || Not[MatchQ[creds,{_String,_String}]],
    $Canceled,
    If[TrueQ[And[authenticateWithServer[creds,$AuthenticationMethod],authenticatedQ[]]],
    	If[TrueQ[$CacheLogin],storeCredentials[]];
    $WolframID,
    Message[CloudConnect::notauth];$Failed]]
]

authenticate[username_String,password_String]:=With[{creds={username,password}},
    (*placeholder while we wait on real authentication*)
If[TrueQ[And[authenticateWithServer[creds,$AuthenticationMethod],authenticatedQ[]]],
		If[TrueQ[$CacheLogin],storeCredentials[]];
    $WolframID,
    Message[CloudConnect::notauth];$Failed]
]

authenticateWithServer[{username_String, password_String},"Legacy"] := Catch[
 Module[{headers}, 
  With[{authpage = "Location" /. (Rule @@@Check[URLFetch["https://online.wolfram.com/mathematica/j_spring_oauth_security_check", {"Headers"}],Throw[$Failed,$tag]])},
     If[TrueQ[$CloudDebug], Print["auth location: " <> authpage]];
     headers = Check[Rule@@@URLFetch[authpage, {"Headers"}, 
        "Method" -> "POST","Parameters" -> {"username" -> username, "password" -> password},"StoreCookies" -> True],Throw[$Failed,$tag]];
     If[TrueQ[$CloudDebug], Print[Row[{"headers are: ", headers}]]];
     ("Location"/.headers) =!= "Location"]], (*redirected indicates sucessful authentication*)
     $tag]
     
$randomCharacters = Join[
	CharacterRange["0", "9"], 
	CharacterRange["A", "Z"], 
	CharacterRange["a", "z"]
];


getServerFromCloudBase[base_String] := With[
	{server = StringCases[base, $UrlScheme<>"://www." ~~ ser___ ~~ "wolframcloud.com" ~~ ("/" | "") :> ser]}, 
  If[UnsameQ[server, {}], First[server], Throw[$Failed,$tag]]]
getServerFromCloudBase[___] := Throw[$Failed,$tag]

If[Not[ValueQ[$LocalhostAuthURL]], Set[$LocalhostAuthURL,"https://user.devel.wolfram.com/oauth/access_token"]];

getDomain[value_] := 
    StringReplace[value, ("http://" | "https://" | "") ~~ Shortest[domain_] ~~ 
        RegularExpression["(:[0-9]+)?(/.*)?"] :> domain]

makeAuthURL["localhost"] := $LocalhostAuthURL
makeAuthURL["www.devel.wolframcloud.com"] := "https://user.devel.wolfram.com/oauth/access_token"
makeAuthURL["www.test.wolframcloud.com"] := "https://user.test.wolfram.com/oauth/access_token"
makeAuthURL["www.wolframcloud.com"] := "https://user.wolfram.com/oauth/access_token"
makeAuthURL[url_]/;Not[TrueQ[$getDomainFlag]] := Block[{$getDomainFlag=True},makeAuthURL[getDomain[url]]]
makeAuthURL[___] := Throw[$Failed,$tag]

makeSignatureURL["localhost"] := "http://localhost"
makeSignatureURL["www.devel.wolframcloud.com"] := "http://devel.wolframcloud.com"
makeSignatureURL["www.test.wolframcloud.com"] := "http://test.wolframcloud.com"
makeSignatureURL["www.wolframcloud.com"] := "http://wolframcloud.com"
makeSignatureURL[url_]/;Not[TrueQ[$getDomainFlag]] := Block[{$getDomainFlag=True},makeSignatureURL[getDomain[url]]]
makeSignatureURL[___] := Throw[$Failed,$tag]

$authUrl := makeAuthURL[System`$CloudBase](*"https://user.test.wolfram.com/oauth/access_token"*)
$signatureURL := makeSignatureURL[System`$CloudBase](*"http://test.wolframcloud.com"*)
$oauthVersion = "1.0";
$unixtimebase = AbsoluteTime[{1970, 1, 1, 0, 0, 0}];
unixtime[] :=  Round[AbsoluteTime[TimeZone -> 0] - $unixtimebase];
nonce[] := StringJoin[RandomChoice[$randomCharacters, 20]]
$sigMethod = "HMAC-SHA1";

If[Not[ValueQ[$RegisteredUserName]],setRegisteredUserName[""]];

(*initial authentication*)
makeSubString[{username_String,password_String},{non_,time_}] := ExternalService`EncodeString[
	StringJoin["oauth_consumer_key=",getConsumerKey[],"&oauth_nonce=",non,"&oauth_signature_method=",$sigMethod,
		"&oauth_timestamp=",ToString[time],"&oauth_version=",$oauthVersion,
		"&x_auth_mode=client_auth&x_auth_password=",ExternalService`EncodeString[password],
		"&x_auth_username=",ExternalService`EncodeString[username]]]   
(*subsequent requests*)
makeSubString[{non_,time_}] := ExternalService`EncodeString[
	StringJoin["oauth_consumer_key=",getConsumerKey[],"&oauth_nonce=",non,"&oauth_signature_method=",$sigMethod,
		"&oauth_timestamp=",ToString[time],"&oauth_token=",getAccessToken[],
		"&oauth_version=",$oauthVersion
	]]

(*initial authentication*)
makeSignatureBase[{username_String,password_String},{non_,time_}] := StringJoin[
	"POST&",ExternalService`EncodeString[$authUrl], "&",makeSubString[{username,password},{non,time}]]
(*subsequent requests*)
makeSignatureBase[{non_,time_},url_String,method_String] := StringJoin[
	method,"&",ExternalService`EncodeString[$signatureURL], "&",makeSubString[{non,time}]]

(*initial authentication*)   
makeSignature[{username_String,password_String},{non_,time_}] := CloudHMAC[makeSignatureBase[{username,password},{non,time}],"Consumer"]
(*subsequent requests*)
makeSignature[{non_,time_},url_String,method_String] := CloudHMAC[makeSignatureBase[{non,time},url,method],"Access"]

(*initial authentication*)
makeOAuthHeader[{username_String,password_String}]:=With[{non=nonce[],time=unixtime[]},
StringJoin["OAuth realm=\"",$authUrl, "\", oauth_consumer_key=\"",getConsumerKey[],
	 "\", oauth_nonce=\"",non, "\", oauth_timestamp=\"",ToString[time],
	 "\", oauth_signature_method=\"",$sigMethod, "\", oauth_version=\"",$oauthVersion,
	 "\", oauth_signature=\"",makeSignature[{username,password},{non,time}],"\""]
]
(*subsequent requests*)
makeOAuthHeader[url_String,method_String]:=With[{non=nonce[],time=unixtime[]},
StringJoin["OAuth realm=\"",$signatureURL, "\", oauth_consumer_key=\"",getConsumerKey[],
	"\", oauth_token=\"",getAccessToken[],	
	 "\", oauth_nonce=\"",non, "\", oauth_timestamp=\"",ToString[time],
	 "\", oauth_signature_method=\"",$sigMethod, "\", oauth_version=\"",$oauthVersion,
	 "\", oauth_signature=\"",ExternalService`EncodeString[makeSignature[{non,time},url,method]],"\""]
]

setWolframID[id:(_String|None)]:=(Unprotect[$WolframID];Set[$WolframID,id];Protect[$WolframID])
setWolframUUID[uuid:(_String|None)]:=(Unprotect[$WolframUUID];Set[$WolframUUID,uuid];Protect[$WolframUUID])
setCloudConnected[value:True|False] := (Unprotect[$CloudConnected];Set[$CloudConnected,value];Protect[$CloudConnected];value)
setRegisteredUserName[name:(_String)]:=(Unprotect[$RegisteredUserName];Set[$RegisteredUserName,name];Protect[$RegisteredUserName])

authenticateWithServer[{username_String, password_String},other_] := Catch[
 Module[{status, contentData, content}, 
  {status, contentData}=Check[URLFetch[$authUrl, {"StatusCode", "ContentData"}, "Method" -> "POST", 
 "Headers" -> {"Authorization" -> makeOAuthHeader[{username,password}]}, 
 "Parameters" -> {"x_auth_mode" -> "client_auth", 
   "x_auth_password" -> password, "x_auth_username" -> username}, 
 "VerifyPeer" -> False(*TODO: remove for production*),"DisplayProxyDialog" -> False],Throw[$Failed,$tag]];
 content = FromCharacterCode[contentData];
 Switch[status,
 	200,
 	content=ImportString[content, "JSON"];
 	If[Not[MatchQ[content,{_Rule..}]],Return[$Failed]];
 	If[TrueQ[$CloudDebug],Print["response contents: ",content]];
 	setAuthentication[username, "uuid" /. content,
 		StringJoin[{"firstname"," ","lastname"}/. content], 
 		"oauth_token_secret" /. content, 
 		"oauth_token" /. content],
 		
 	401,
 	Message[CloudConnect::creds];
 	If[TrueQ[$CloudDebug],Print["authentication failed: ",{status,content}]];
 	$Failed,
 	
 	_,(*all other*)
 	If[TrueQ[$CloudDebug],Print["authentication failed: ",{status,content}]];$Failed
 	]],
     $tag]

authenticatedURLFetch[url_String,elements_:"Content",opts___?OptionQ] := Catch[
	If[Not[TrueQ[$CloudConnected]],Throw[$Failed["MustAuthenticate"],$tag]];
	With[{method=("Method"/.{opts})/."Method"->"GET",
		headers=("Headers"/.{opts})/."Headers"->{},
		options=Sequence@@DeleteCases[{opts},HoldPattern[Rule["Method"|"Headers",_]]]},
	With[{auth=makeOAuthHeader[url,method]},
		If[TrueQ[$CloudDebug],Print["headers: ",Join[headers, {"Authorization" -> auth}]]];
		If[TrueQ[$CloudDebug],Print[method," ",url]];
		If[TrueQ[$CloudDebug],Print["Options: ",ToString[{options},InputForm]]];
	Check[
		URLFetch[url, elements,"Method"->method,
			"Headers" ->Join[headers, {"Authorization" -> auth}],
			options],
		Throw[$Failed,$tag]]]],
 $tag]

authenticatedQ[]:=Catch[If[$AuthenticationMethod=!="Legacy",
	TrueQ[$CloudConnected](*non-legacy authentication*),
    With[{headers = Check[Rule@@@URLFetch["https://online.wolfram.com/mathematica/uuid.jsp", {"Headers"}],Throw[False,$tag]]},
        If[TrueQ[$CloudDebug], Print[Row[{"headers are: ", headers}]]];
        If[
        ("Location"/.headers) === "Location",(*no redirect indicates sucessful authentication*)
        Set[authenticatedQ[],True];True,(*TODO: this assumption may not be safe, but is ok for testing*)
        False]]],
  $tag]
  
setAuthentication[username_String, uuid_String, userdisplayname_String, accesssecret_String, accesstoken_String] := If[TrueQ[$UseLibraryStorage],
	setWolframID[username]; setWolframUUID[uuid];
	setRegisteredUserName[userdisplayname];setAccessData[accesstoken,accesssecret];
	setCloudConnected[True]]
setAuthentication[___] := $Failed

getAuthentication[] := If[ TrueQ[$CloudConnected],
	{$WolframID,$WolframUUID,$RegisteredUserName,getAccessSecret[],getAccessSecret[]},
	$Failed
]
getAuthentication[___] := $Failed

loadLibCloudObj[] :=
    Block[{path},

        path = FindLibrary["WolframAuthenticate"];
        If[path === $Failed,
        	path = FileNameJoin[{DirectoryName[DirectoryName[$InputFileName]], 
        		"LibraryResources", $SystemID, "WolframAuthenticate"}]
        ];
        AuthenticateLibraryFile = path;
        (
        	getConsumerKey = LibraryFunctionLoad[path, "get_consumer_key", {}, "UTF8String"];
            setAccessData = LibraryFunctionLoad[path, "set_access_data", {"UTF8String", "UTF8String"}, Integer];
            getAccessToken = LibraryFunctionLoad[path, "get_access_key", {}, "UTF8String"];
            getAccessSecret = LibraryFunctionLoad[path, "get_access_secret", {}, "UTF8String"];(*TODO: remove this?*)
            CloudHMAC = LibraryFunctionLoad[path,"cloud_object_oauth_hmac",{"UTF8String","UTF8String"},"UTF8String"];
			True/;SameQ[LibraryFunction,Sequence@@(Head/@{getConsumerKey,setAccessData,getAccessToken,getAccessSecret,CloudHMAC})]
        ) /; (path =!= $Failed)
    ]

loadLibCloudObj[___] := $Failed

$UseLibraryStorage = If[$CloudEvaluation === True,False,UnsameQ[loadLibCloudObj[], $Failed]]


$credsDir = FileNameJoin[{$UserBaseDirectory,"ApplicationData","CloudObject","Authentication"}];  
$credsFile ="config.pfx";(*TODO:make this an actual pfx file*)
$storageKey:=Internal`HouseKeep[$authUrl, {
	"machine_ID" -> $MachineID, 
	"version" -> $Version, 
	"system_ID" -> $SystemID,
	"user_name" -> $UserName
}]

encrypt[args___]:=With[{ef=If[
	MemberQ[Names["*`RijndaelDecryption"], "NumberTheory`AESDump`RijndaelDecryption"],
	Symbol["NumberTheory`AESDump`rijndaelEncryption"],
	SocialMediaData[];(*load AES.m from social media data*)Symbol["DataPaclets`SocialMediaDataDump`rijndaelEncryption"]]},
	ef[args]
]

decrypt[args___]:= With[{df=If[
	MemberQ[Names["*`RijndaelDecryption"], "NumberTheory`AESDump`RijndaelDecryption"],
	Symbol["NumberTheory`AESDump`RijndaelDecryption"],
	SocialMediaData[];(*load AES.m from social media data*)Symbol["DataPaclets`SocialMediaDataDump`RijndaelDecryption"]]},
	Block[{DataPaclets`SocialMediaDataDump`Private`flagQ = True, NumberTheory`AESDump`Private`flagQ = True},
	df[args]
	]
]

storeCredentials[]:=storeCredentials[$credsDir,$credsFile,$storageKey]
storeCredentials[directory_String,filename_String,key_String]/;authenticatedQ[]:= Catch[
Block[{$authenticationCredentials=encrypt[
		StringJoin["token=",getAccessToken[],"secret=",getAccessSecret[],"username=",$RegisteredUserName,"uuid=",$WolframUUID,"wolframid=",$WolframID],
		key,"CiphertextFormat" -> "ByteList"]},
With[{CreateDirectorymessages:={CreateDirectory::filex,CreateDirectory::privv},
	Savemessages:={Save::wtype,Save::argm,Save::argmu,General::stream,Save::sym,General::privv,General::noopen},
	file=FileNameJoin[{directory,filename}]},
	If[Not[DirectoryQ[directory]],(*make directory if it doesn't exist*)
		Quiet[Check[CreateDirectory[directory],Throw[$Failed["NoMakeDir"],$tag],CreateDirectorymessages],CreateDirectorymessages]];
	Quiet[Check[DumpSave[file,$authenticationCredentials],Throw[$Failed["NoDump"],$tag],Savemessages],Savemessages];
	True
]],$tag]
storeCredentials[___]:=$Failed["NotAuthenticated"]

getCredentials[]:=getCredentials[$credsDir,$credsFile,$storageKey]
getCredentials[directory_String,filename_String,key_String]:=Catch[
	If[TrueQ[$CloudDebug],Identity,Quiet][
	Block[{$authenticationCredentials},With[
	{Getmessages:={Get::enkey,Get::notencode,General::privv,General::noopen},file=FileNameJoin[{directory,filename}]},
	If[Not[DirectoryQ[directory]],Throw[$Failed["NotDir"],$tag]];
	Quiet[Check[Get[file],Throw[$Failed["NoGet"],$tag],Getmessages],Getmessages];
	If[Not[MatchQ[$authenticationCredentials,{_Integer..}]],Throw[$Failed["Bytes"],$tag]];
	$authenticationCredentials=decrypt[$authenticationCredentials, key];
	If[Not[StringQ[$authenticationCredentials]],$authenticationCredentials=ExportString[$authenticationCredentials,"Byte"]];
	$authenticationCredentials=StringSplit[$authenticationCredentials, {"token=", "secret=","username=","uuid=","wolframid="}];
	If[MatchQ[$authenticationCredentials,{_String,_String,_String,_String,_String}],
		setAuthentication[Sequence@@Reverse[$authenticationCredentials]],
		Throw[$Failed["NotStringPair"],$tag]
	]
]]],$tag]
getCredentials[__]:=$Failed["BadParameters"]

SetAttributes[CloudConnect,ReadProtected];
Protect[CloudConnect];
SetAttributes[CloudDisconnect,ReadProtected];
Protect[CloudDisconnect];
Protect[$CloudConnected];

End[]

EndPackage[]
