(* Mathematica package *)
BeginPackage["CloudObject`"]

System`CloudConnect;
System`CloudDisconnect;
System`$CloudConnected;
System`$WolframID;
System`$WolframUUID;
$UseRemoteServer;
$CloudDebug;

Begin["`Private`"]

Unprotect[CloudConnect];
Unprotect[CloudDisconnect];
Unprotect[$CloudConnected];

CloudConnect::notauth = "Unable to authenticate with Wolfram Cloud server. Please try authenticating again."

$AuthenticationMethod = "XAuth"(*"Legacy"*);
$tag = "CloudObjectCatchThrowTag";
If[Not[ValueQ[$CloudConnected]],Set[$CloudConnected,False]];

Options[CloudConnect] = {"RememberMe"->Automatic};

CloudConnect[args___,OptionsPattern[]]:=With[{res=Catch[
	With[{ov=Check[OptionValue["RememberMe"],Throw[$Failed,$tag],{OptionValue::nodef,OptionValue::optnf}]},
	If[
		Not[MatchQ[ov,Automatic|True|False]],
		Message[CloudConnect::opttf,"RememberMe",ov];Throw[$Failed,$tag]
	]];
	Block[{$CacheLogin = TrueQ[OptionValue["RememberMe"]]},
	iCloudConnect[args]],
	$tag]},
	res/;MatchQ[res,$WolframID|$Failed]]/;Not[TrueQ[$CloudEvaluation]]

iCloudConnect[] := With[{r=If[TrueQ[$CacheLogin],getCredentials["login"],$Failed]},If[FreeQ[r,$Failed],iCloudConnect[Sequence@@r],iCloudConnect[""]]]
iCloudConnect[username_String] := (If[$UseRemoteServer === True, authenticate[username]])
iCloudConnect[username_String,password_String] := (If[$UseRemoteServer === True, authenticate[username,password]])
iCloudConnect[args__] := (ArgumentCountQ[CloudConnect,Length[DeleteCases[{args},_Rule,Infinity]],1,2];Null/;False)

CloudDisconnect[] := (setCloudConnected[False];
setWolframID[None];
setWolframUUID[None];
Set[$DisplayUserName,""];
setAccessData["",""];
)/;Not[TrueQ[$CloudEvaluation]]
CloudDisconnect[args__] := (ArgumentCountQ[CloudDisconnect,Length[{args}],0,0];Null/;False)

authenticate[username_String]:=With[{creds=loginDialog[username]},
    (*placeholder while we wait on real authentication*)
If[creds===$Canceled || Not[MatchQ[creds,{_String,_String}]],
    $Canceled,
    If[TrueQ[And[authenticateWithServer[creds,$AuthenticationMethod],authenticatedQ[]]],
    	If[TrueQ[$CacheLogin],storeCredentials[creds]];
    $WolframID,
    Message[CloudConnect::notauth];$Failed]]
]

authenticate[username_String,password_String]:=With[{creds={username,password}},
    (*placeholder while we wait on real authentication*)
If[TrueQ[And[authenticateWithServer[creds,$AuthenticationMethod],authenticatedQ[]]],
		If[TrueQ[$CacheLogin],storeCredentials[creds]];
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

makeAuthURL["localhost"] := $LocalhostAuthURL
makeAuthURL["localhost/"] := makeAuthURL["localhost"]
makeAuthURL["http://localhost"] := makeAuthURL["localhost"]
makeAuthURL["https://localhost"] := makeAuthURL["localhost"]
makeAuthURL["devel"] := "https://user.devel.wolfram.com/oauth/access_token"
makeAuthURL["http://www.devel.wolframcloud.com/"] := makeAuthURL["devel"]
makeAuthURL["https://www.devel.wolframcloud.com/"] := makeAuthURL["devel"]
makeAuthURL["http://www.devel.wolframcloud.com"] := makeAuthURL["devel"]
makeAuthURL["https://www.devel.wolframcloud.com"] := makeAuthURL["devel"]
makeAuthURL["test"] := "https://user.test.wolfram.com/oauth/access_token"
makeAuthURL["http://www.test.wolframcloud.com/"] := makeAuthURL["test"]
makeAuthURL["https://www.test.wolframcloud.com/"] := makeAuthURL["test"]
makeAuthURL["http://www.test.wolframcloud.com"] := makeAuthURL["test"]
makeAuthURL["https://www.test.wolframcloud.com"] := makeAuthURL["test"]
makeAuthURL["Automatic"] := "https://user.wolfram.com/oauth/access_token"
makeAuthURL["http://www.wolframcloud.com/"] := makeAuthURL["Automatic"]
makeAuthURL["https://www.wolframcloud.com/"] := makeAuthURL["Automatic"]
makeAuthURL["http://www.wolframcloud.com"] := makeAuthURL["Automatic"]
makeAuthURL["https://www.wolframcloud.com"] := makeAuthURL["Automatic"]
makeAuthURL[___] := $Failed

makeSignatureURL["localhost"] := "http://localhost"
makeSignatureURL["localhost/"] := makeSignatureURL["localhost"]
makeSignatureURL["http://localhost"] := makeSignatureURL["localhost"]
makeSignatureURL["https://localhost"] := makeSignatureURL["localhost"]
makeSignatureURL["devel"] := "http://devel.wolframcloud.com"
makeSignatureURL["http://www.devel.wolframcloud.com/"] := makeSignatureURL["devel"]
makeSignatureURL["https://www.devel.wolframcloud.com/"] := makeSignatureURL["devel"]
makeSignatureURL["http://www.devel.wolframcloud.com"] := makeSignatureURL["devel"]
makeSignatureURL["https://www.devel.wolframcloud.com"] := makeSignatureURL["devel"]
makeSignatureURL["test"] := "http://test.wolframcloud.com"
makeSignatureURL["http://www.test.wolframcloud.com/"] := makeSignatureURL["test"]
makeSignatureURL["https://www.test.wolframcloud.com/"] := makeSignatureURL["test"]
makeSignatureURL["http://www.test.wolframcloud.com"] := makeSignatureURL["test"]
makeSignatureURL["https://www.test.wolframcloud.com"] := makeSignatureURL["test"]
makeSignatureURL["Automatic"] := "http://wolframcloud.com"
makeSignatureURL["http://www.wolframcloud.com/"] := makeSignatureURL["Automatic"]
makeSignatureURL["https://www.wolframcloud.com/"] := makeSignatureURL["Automatic"]
makeSignatureURL["http://www.wolframcloud.com"] := makeSignatureURL["Automatic"]
makeSignatureURL["https://www.wolframcloud.com"] := makeSignatureURL["Automatic"]
makeSignatureURL[___] := $Failed

$authUrl := makeAuthURL[System`$CloudBase](*"https://user.test.wolfram.com/oauth/access_token"*)
$signatureURL := makeSignatureURL[System`$CloudBase](*"http://test.wolframcloud.com"*)
$oauthVersion = "1.0";
$unixtimebase = AbsoluteTime[{1970, 1, 1, 0, 0, 0}];
unixtime[] :=  Round[AbsoluteTime[TimeZone -> 0] - $unixtimebase];
nonce[] := StringJoin[RandomChoice[$randomCharacters, 20]]
$sigMethod = "HMAC-SHA1";

If[Not[ValueQ[$DisplayUsername]],$DisplayUserName=""];

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
setWolframUUID[uuid:(_String)]:=(Unprotect[$WolframUUID];Set[$WolframUUID,uuid];Protect[$WolframUUID])
setCloudConnected[value:True|False] := (Unprotect[$CloudConnected];Set[$CloudConnected,value];Protect[$CloudConnected];value)

authenticateWithServer[{username_String, password_String},other_] := Catch[URLFetch;Block[{HTTPClient`Private`passwordDialog},(*prevent https dialog*)
 Module[{status,content}, 
  {status,content}=Check[URLFetch[$authUrl, {"StatusCode", "Content"}, "Method" -> "POST", 
 "Headers" -> {"Authorization" -> makeOAuthHeader[{username,password}]}, 
 "Parameters" -> {"x_auth_mode" -> "client_auth", 
   "x_auth_password" -> password, "x_auth_username" -> username}, 
 "VerifyPeer" -> False(*TODO: remove for production*)],Throw[$Failed,$tag]];
 If[status === 200,
 content=ImportString[content, "JSON"];
 If[Not[MatchQ[content,{_Rule..}]],Return[$Failed]];
 If[TrueQ[$CloudDebug],Print["response contents: ",content]];
 setAuthentication[username, "uuid" /. content,
 	StringJoin[{"firstname"," ","lastname"}/. content], "oauth_token_secret" /. content, "oauth_token" /. content],
 If[TrueQ[$CloudDebug],Print["authentication failed: ",{status,content}]];$Failed]]],
     $tag]

authenticatedURLFetch[url_String,elements_:"Content",opts___?OptionQ] := Catch[
	If[Not[TrueQ[$CloudConnected]],Throw[$Failed["MustAuthenticate"],$tag]];
	With[{method=("Method"/.{opts})/."Method"->"GET",
		headers=("Headers"/.{opts})/."Headers"->{},
		options=Sequence@@Prepend[DeleteCases[{opts},HoldPattern[Rule["Method"|"Headers",_]]],"VerifyPeer"->False](*TODO: remove for production*)},
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
	Set[$DisplayUserName,userdisplayname];setAccessData[accesstoken,accesssecret];
	setCloudConnected[True]]
setAuthentication[___] := $Failed

getAuthentication[] := If[ TrueQ[$CloudConnected],
	{$WolframID,$WolframUUID,$DisplayUserName,getAccessSecret[],getAccessSecret[]},
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
$storageKey:=Internal`HouseKeep[$signatureURL, {
	"machine_ID" -> $MachineID, 
	"version" -> $Version, 
	"system_ID" -> $SystemID,
	"user_name" -> $UserName
}]

storeCredentials[]:=storeCredentials[$credsDir,$credsFile,$storageKey]
storeCredentials[directory_String,filename_String,key_String]/;authenticatedQ[]:= Catch[
	SocialMediaData[];(*load AES.m from social media data*)
Block[{$authenticationCredentials=DataPaclets`SocialMediaDataDump`rijndaelEncryption[
		StringJoin["token=",getAccessToken[],"secret=",getAccessSecret[],"username=",$DisplayUserName,"uuid=",$WolframUUID,"wolframid=",$WolframID],
		key,"CiphertextFormat" -> "ByteList"]},
With[{CreateDirectorymessages:={CreateDirectory::filex,CreateDirectory::privv},
	Savemessages:={Save::wtype,Save::argm,Save::argmu,General::stream,Save::sym,General::privv,General::noopen},
	file=FileNameJoin[{directory,filename}]},
	If[Not[DirectoryQ[directory]],(*make directory if it doesn't exist*)
		Quiet[Check[CreateDirectory[directory],Throw[$Failed["NoMakeDir"],$tag],CreateDirectorymessages],CreateDirectorymessages]];
	Quiet[Check[DumpSave[file,$authenticationCredentials],Throw[$Failed["NoDump"],$tag],Savemessages],Savemessages];
	True
]],$tag]
storeCredentials[{username_String,password_String}] /; authenticatedQ[] :=  Catch[
	SocialMediaData[];(*load AES.m from social media data*)
Block[{$authenticationCredentials=DataPaclets`SocialMediaDataDump`rijndaelEncryption[
		StringJoin["username=",username,"password=",password],
		$storageKey,"CiphertextFormat" -> "ByteList"]},
With[{CreateDirectorymessages:={CreateDirectory::filex,CreateDirectory::privv},
	Savemessages:={Save::wtype,Save::argm,Save::argmu,General::stream,Save::sym,General::privv,General::noopen},
	file=FileNameJoin[{$credsDir,$credsFile}]},
	If[Not[DirectoryQ[$credsDir]],(*make directory if it doesn't exist*)
		Quiet[Check[CreateDirectory[$credsDir],Throw[$Failed["NoMakeDir"],$tag],CreateDirectorymessages],CreateDirectorymessages]];
	Quiet[Check[DumpSave[file,$authenticationCredentials],Throw[$Failed["NoDump"],$tag],Savemessages],Savemessages];
	True
]],$tag]
storeCredentials[___]:=$Failed["NotAuthenticated"]

getCredentials[]:=getCredentials[$credsDir,$credsFile,$storageKey]
getCredentials[directory_String,filename_String,key_String]:=Catch[
	SocialMediaData[];(*load AES.m from social media data*)
	Block[{$authenticationCredentials,DataPaclets`SocialMediaDataDump`Private`flagQ = True},With[
	{Getmessages:={Get::enkey,Get::notencode,General::privv,General::noopen},file=FileNameJoin[{directory,filename}]},
	If[Not[DirectoryQ[directory]],Throw[$Failed["NotDir"],$tag]];
	Quiet[Check[Get[file],Throw[$Failed["NoGet"],$tag],Getmessages],Getmessages];
	If[Not[MatchQ[$authenticationCredentials,{_Integer..}]],Throw[$Failed["Bytes"],$tag]];
	$authenticationCredentials=DataPaclets`SocialMediaDataDump`RijndaelDecryption[$authenticationCredentials, key];
	If[Not[StringQ[$authenticationCredentials]],$authenticationCredentials=ExportString[$authenticationCredentials,"Byte"]];
	$authenticationCredentials=StringSplit[$authenticationCredentials, {"token=", "secret=","username=","uuid=","wolframid="}];
	If[MatchQ[$authenticationCredentials,{_String,_String,_String,_String,_String}],
		setAuthentication[Sequence@@Reverse[$authenticationCredentials]],
		Throw[$Failed["NotStringPair"],$tag]
	]
]],$tag]
getCredentials["login"]:=Catch[
	SocialMediaData[];(*load AES.m from social media data*)
	Block[{$authenticationCredentials,DataPaclets`SocialMediaDataDump`Private`flagQ = True},With[
	{Getmessages:={Get::enkey,Get::notencode,General::privv,General::noopen},file=FileNameJoin[{$credsDir,$credsFile}]},
	If[Not[DirectoryQ[$credsDir]],Throw[$Failed["NotDir"],$tag]];
	Quiet[Check[Get[file],Throw[$Failed["NoGet"],$tag],Getmessages],Getmessages];
	If[Not[MatchQ[$authenticationCredentials,{_Integer..}]],Throw[Print[$authenticationCredentials];$Failed["Bytes"],$tag]];
	$authenticationCredentials=DataPaclets`SocialMediaDataDump`RijndaelDecryption[$authenticationCredentials, $storageKey];
	If[Not[StringQ[$authenticationCredentials]],$authenticationCredentials=ExportString[$authenticationCredentials,"Byte"]];
	$authenticationCredentials=StringSplit[$authenticationCredentials, {"username=", "password="}];
	If[MatchQ[$authenticationCredentials,{_String,_String}],
		$authenticationCredentials,
		Throw[$Failed["NotStringPair"],$tag]
	]
]],$tag]
getCredentials[__]:=$Failed["BadParameters"]

SetAttributes[CloudConnect,ReadProtected];
Protect[CloudConnect];
SetAttributes[CloudDisconnect,ReadProtected];
Protect[CloudDisconnect];
Protect[$CloudConnected];

End[]

EndPackage[]
