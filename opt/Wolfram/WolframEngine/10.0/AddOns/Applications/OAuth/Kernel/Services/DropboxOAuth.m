
System`Private`NewContextPath[{"OAuthClient`","System`"}];

(* declare functions that will be used by the OAuth paclet *)
OAuthClient`dropboxdata;
OAuthClient`dropboxcookeddata;
OAuthClient`dropboxsendmessage;


Begin["DropboxOAuthDump`"] (* Begin Private Context *) 

(******************************* Dropbox *************************************)

ServiceExecute::ndir="The specified path `1` is not a directory in the connected Dropbox account."
ServiceExecute::nfile="The specified path `1` is not a file in the connected Dropbox account."
ServiceExecute::grext="The graphic could not be exported as the file type, `1`, given in the path."

(* Authentication information *)

dropboxdata[]={
		"OAuthVersion"		->"2.0",
		"ServiceName" 		-> "Dropbox", 
		"AuthorizeEndpoint" -> "https://www.dropbox.com/1/oauth2/authorize", 
		"AccessEndpoint" 	-> "https://api.dropbox.com/1/oauth2/token", 
		"RedirectURI" 		-> "https://user.wolfram.com/oauth/facebook_catch.php",
	 	"ClientInfo"		-> {40, 42, 33, 49, 78, 33, 42, 41, 53, 52, 42, 13, 10, 102, 118, 122, 
			49, 82, 97, 109, 88, 33, 194, 160, 70, 94, 50, 93, 82, 61, 115, 35, 
			55, 83, 90, 92, 95, 35, 86, 71, 109, 54, 102, 60, 103, 52, 116, 51, 
			84, 83, 194, 173, 98, 43, 109, 70, 95, 13, 10},
	 	"AuthenticationDialog" -> "TokenDialog",
	 	"Gets"				-> {"DirectoryTreePlot","FileSearch","FileData","FileNames","FileContents","DirectoryData","UserData","ImportFile"},
	 	"Posts"				-> {"DataUpload","GraphicsUpload"},
	 	"RawGets"			-> {"RawUserData","RawFileDownload","RawPathData","RawFileRevisions",
	 		"RawFileSearch","RawCopyFileReference",  "RawThumbnail"},
	 	"RawPosts"			-> {"RawFileUpload","RawFileRestore","RawFileChange","RawFilePreviewLink","RawFileLink",
	 		"RawChunkUpload","RawChunkedUploadCommit","RawFileCopy","RawCreateFolder","RawFileDelete","RawFileMove"},
	 	"LogoutURL"			-> "https://www.dropbox.com/logout",
 		"Information"		-> "Connect Mathematica with your dropbox account"
}

(* a function for importing the raw data - usually json or xml - from the service *)
dropboximport[$Failed]:=Throw[$Failed]
dropboximport[raw_String]:=If[StringFreeQ[raw,"error"],raw,Message[ServiceExecute::apierr,raw]
]
dropboximport[raw_]:=raw


dropboximportjson[$Failed]:=Throw[$Failed]
dropboximportjson[json_]:=With[{res=ImportString[json,"JSON"]},
	If[FreeQ[res,_["errors",_]],
		Switch[res,
			_Rule|{_Rule...},assoc@res,
			{{_Rule...}...},assoc/@res,
			_,res
		],
		Message[ServiceExecute::apierr,("errors"/.res)];
		Throw[$Failed]
	]
]


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

dropboxdata["RawUserData"] = {
        "URL"				-> "https://api.dropbox.com/1/account/info",
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> dropboximportjson
    }   

dropboxdata["RawFileDownload"] = {
        "URL"				-> (ToString@StringForm["https://api-content.dropbox.com/1/files/`1`", formatrootpath[##]]&),
        "PathParameters"	-> {"Root","Path"},
        "Parameters"		-> {"rev"},
        "RequiredParameters"-> {"Root","Path"}, 
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> dropboximport
    }   
    
dropboxdata["RawPathData"] = {
        "URL"				-> (ToString@StringForm["https://api.dropbox.com/1/metadata/`1`", formatrootpath[##]]&),
        "PathParameters"	-> {"Root","Path"},
        "Parameters"		-> {"file_limit","list","hash","include_deleted","rev","locale"},
        "RequiredParameters"-> {"Root","Path"}, 
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> dropboximportjson
    }  
      
dropboxdata["RawFileChange"] = {
        "URL"				-> "https://api.dropbox.com/1/delta",
        "Parameters"		-> {"cursor","locale"},
        "HTTPSMethod"		-> "POST",
        "ResultsFunction"	-> dropboximportjson
    }  
      
dropboxdata["RawFileRevisions"] = {
        "URL"				-> (ToString@StringForm["https://api.dropbox.com/1/revisions/`1`", formatrootpath[##]]&),
        "PathParameters"	-> {"Root","Path"},
        "Parameters"		-> {"rev_limit","locale"},
        "RequiredParameters"-> {"Root","Path"}, 
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> dropboximportjson
    }  
    
dropboxdata["RawFileSearch"] = {
        "URL"				-> (ToString@StringForm["https://api.dropbox.com/1/search/`1`", formatrootpath[##]]&),
        "PathParameters"	-> {"Root","Path"},
        "Parameters"		-> {"query","file_limit","include_deleted","locale"},
        "RequiredParameters"-> {"Root","Path","query"}, 
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> dropboximportjson
    }  
             
dropboxdata["RawFileUpload"] = {
        "URL"				-> (ToString@StringForm["https://api-content.dropbox.com/1/files_put/`1`", formatrootpath[##]]&),
        "PathParameters"	-> {"Root","Path"},
        "Parameters"		-> {"rev","locale","overwrite","parent_rev"},
        "RequiredParameters"-> {"Root","Path"}, 
        "BodyData"			-> {"ParameterlessBodyData"},
      (*   "MultipartData"		-> {{"FileData","text/plain"}}, *)
      	"Headers" 			-> {"Content-Type" -> "text/plain"},
        "HTTPSMethod"		-> "POST",
        "ResultsFunction"	-> dropboximportjson
    }   
            
dropboxdata["RawFileRestore"] = {
        "URL"				-> (ToString@StringForm["https://api.dropbox.com/1/restore/`1`", formatrootpath[##]]&),
        "PathParameters"	-> {"Root","Path"},
        "Parameters"		-> {"rev","locale"},
        "RequiredParameters"-> {"Root","Path","rev"}, 
        "HTTPSMethod"		-> "POST",
        "ResultsFunction"	-> dropboximportjson
    }   
            
dropboxdata["RawFilePreviewLink"] = {
        "URL"				-> (ToString@StringForm["https://api.dropbox.com/1/shares/`1`", formatrootpath[##]]&),
        "PathParameters"	-> {"Root","Path"},
        "Parameters"		-> {"short_url","locale"},
        "RequiredParameters"-> {"Root","Path"}, 
        "HTTPSMethod"		-> "POST",
        "ResultsFunction"	-> dropboximportjson
    }  

dropboxdata["RawFileLink"] = {
        "URL"				-> (ToString@StringForm["https://api.dropbox.com/1/media/`1`", formatrootpath[##]]&),
        "PathParameters"	-> {"Root","Path"},
        "Parameters"		-> {"locale"},
        "RequiredParameters"-> {"Root","Path"}, 
        "HTTPSMethod"		-> "POST",
        "ResultsFunction"	-> dropboximportjson
    }     
    
dropboxdata["RawCopyFileReference"] = {
        "URL"				-> (ToString@StringForm["https://api.dropbox.com/1/search/`1`", formatrootpath[##]]&),
        "PathParameters"	-> {"Root","Path"},
        "RequiredParameters"-> {"Root","Path"}, 
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> dropboximportjson
    } 
    
dropboxdata["RawThumbnail"] = {
        "URL"				-> (ToString@StringForm["https://api-content.dropbox.com/1/thumbnails/`1`", formatrootpath[##]]&),
        "PathParameters"	-> {"Root","Path"},
        "Parameters"		-> {"format","size"},
        "RequiredParameters"-> {"Root","Path"}, 
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> dropboximportjson
    }  
    
dropboxdata["RawChunkUpload"] = {
        "URL"				-> "https://api-content.dropbox.com/1/chunked_upload",
        "BodyData"			-> {"ParameterlessBodyData"},
        "Parameters"		-> {"upload_id","offset"},
        "RequiredParameters"-> {"ParameterlessBodyData"}, 
        "HTTPSMethod"		-> "POST",
        "ResultsFunction"	-> dropboximportjson
    }  
       
dropboxdata["RawChunkedUploadCommit"] = {
        "URL"				-> (ToString@StringForm["https://api-content.dropbox.com/1/commit_chunked_upload/`1`", formatrootpath[##]]&),
        "PathParameters"	-> {"Root","Path"},
        "Parameters"		-> {"locale","overwrite","parent_rev","upload_id"},
        "RequiredParameters"-> {"Root","Path"}, 
        "HTTPSMethod"		-> "POST",
        "ResultsFunction"	-> dropboximportjson
    } 

(** File Operations **)
dropboxdata["RawFileCopy"] = {
        "URL"				-> "https://api.dropbox.com/1/fileops/copy",
        "Parameters"		-> {"root","from_path","to_path","locale","from_copy_ref"},
        "RequiredParameters"-> {"root","to_path","from_path"|"from_copy_ref"}, 
        "HTTPSMethod"		-> "POST",
        "ResultsFunction"	-> dropboximportjson
    } 

dropboxdata["RawCreateFolder"] = {
        "URL"				-> "https://api.dropbox.com/1/fileops/create_folder",
        "Parameters"		-> {"root","path","locale"},
        "RequiredParameters"-> {"root","path"}, 
        "HTTPSMethod"		-> "POST",
        "ResultsFunction"	-> dropboximportjson
    } 
    
dropboxdata["RawFileDelete"] = {
        "URL"				-> "https://api.dropbox.com/1/fileops/delete",
        "Parameters"		-> {"root","path","locale"},
        "RequiredParameters"-> {"root","path"}, 
        "HTTPSMethod"		-> "POST",
        "ResultsFunction"	-> dropboximportjson
    } 
     
dropboxdata["RawFileMove"] = {
        "URL"				-> "https://api.dropbox.com/1/fileops/move",
        "Parameters"		-> {"root","from_path","to_path","locale"},
        "RequiredParameters"-> {"root","from_path","to_path"}, 
        "HTTPSMethod"		-> "POST",
        "ResultsFunction"	-> dropboximportjson
    } 
    
    
dropboxdata[___]:=$Failed
(****** Cooked Properties ******)
  
(* cooked data queries 
	Cooked queries call at least one of the raw queried underneath but add pre and post processing to make the function easier to use and the result clearer.
*)  

dropboxcookeddata[prop_,id_,rule_Rule, rest___]:=dropboxcookeddata[prop,id,{rule}, rest]
dropboxcookeddata[prop_,id_]:=dropboxcookeddata[prop,id,{}]

dropboxcookeddata["FileContents",id_,args_]:=Block[
	{params,rawdata, root,data},
	params=filterparameters[Join[args,{"Root"->"dropbox"}],getallparameters["RawFileDownload"]];
	If[FreeQ[params,#],Message[ServiceExecute::nparam,#1];Throw[$Failed]]&/@{"Root","Path"};
	rawdata=OAuthClient`rawoauthdata[id,"RawFileDownload",params];
	debugPrint["rawdata"->rawdata];
	data=dropboximport[rawdata];
	debugPrint["data"->data];
	data/;data=!=$Failed
]

dropboxcookeddata["ImportFile",id_,args_]:=Block[
	{params,rawdata, root,data, ext,res,url},
	params=filterparameters[Join[args,{"Root"->"dropbox"}],getallparameters["RawFileLink"]];
	If[FreeQ[params,#],Message[ServiceExecute::nparam,#1];Throw[$Failed]]&/@{"Root","Path"};
	rawdata=OAuthClient`rawoauthdata[id,"RawFileLink",params];
	debugPrint["rawdata"->rawdata];
	data=dropboximportjson[rawdata];
	(url=Lookup[data,"url",$Failed];
		(	
			res=Import[url];
			res/;res=!=$Failed
		)/;url=!=$Failed
	)/;data=!=$Failed
]

dropboxcookeddata[prop:("UserData"),id_,args_]:=Block[
	{params,rawdata, root,data},
	params=filterparameters[args,getallparameters["RawUserData"]];
	rawdata=OAuthClient`rawoauthdata[id,"RawUserData",params];
	data=dropboximportjson[rawdata];
	assoc[Replace[Normal[data],HoldPattern[Rule][a_,b_]:>Rule[camelcase[a],b],Infinity]/."Uid"->"UserID"]
]


dropboxcookeddata[prop:("FileData"|"DirectoryData"),id_,args_]:=Block[
	{params,rawdata, root,data},
	params=filterparameters[Join[args,{"Root"->"dropbox"}]/."File"->"Path",getallparameters["RawPathData"]];
	If[FreeQ[params,#],Message[ServiceExecute::nparam,#1];Throw[$Failed]]&/@{"Root","Path"};
	rawdata=OAuthClient`rawoauthdata[id,"RawPathData",params];
	debugPrint["rawdata 1"->rawdata];
	data=dropboximportjson[rawdata];
	debugPrint["data 1"->data];
	If[Lookup[data,"is_dir",False],
		If[prop==="FileData",Message[ServiceExecute::nfile,"Path"/.params]];
		assoc[Replace[Normal[data],HoldPattern[Rule][a_,b_]:>Rule[camelcase[a],b],Infinity]]
		,
		If[prop==="PathData",Message[ServiceExecute::ndir,"Path"/.params]];
		assoc[Replace[Normal[data],HoldPattern[Rule][a_,b_]:>Rule[camelcase[a],b],Infinity]]
	]
]

dropboxcookeddata["FileNames",id_,args_]:=Block[
	{data},
	data=dropboxcookeddata["DirectoryData",id,args];
	debugPrint["data 2"->data];
	data=Lookup[data,"Contents",{}];
	"Path"/.data

]

dropboxcookeddata["DataUpload",id_,args_]:=Block[
	{params,rawdata, root,data},
	If[FreeQ[args,#],Message[ServiceExecute::nparam,#1];Throw[$Failed]]&/@{"Data"};
	params=filterparameters[Join[args,{"Root"->"dropbox", "overwrite" -> "true"}]/.{
		"Data"->"ParameterlessBodyData",True->"true",False->"false"},getallparameters["RawFileUpload"]];
	If[FreeQ[params,#],Message[ServiceExecute::nparam,#1];Throw[$Failed]]&/@{"Root","Path"};
	debugPrint["params"->params];
	params=params/.HoldPattern[Rule]["ParameterlessBodyData",d:Except[_String]]:>Rule["ParameterlessBodyData",ToString[InputForm[d]]];
	rawdata=OAuthClient`rawoauthdata[id,"RawFileUpload",params];
	debugPrint["rawdata"->rawdata];
	data=dropboximportjson[rawdata];
	debugPrint["data"->data];
	Map[(camelcase[#]->data[#])&,{"modified", "revision", "rev","path", "root", "mime_type"}]
]

dropboxcookeddata["GraphicsUpload",id_,args_]:=Block[
	{params,rawdata, root,data, ext},
	debugPrint["args"->args];
	If[FreeQ[args,#],Message[ServiceExecute::nparam,#1];Throw[$Failed]]&/@{"Graphics"};
	params=filterparameters[Join[args,{"Root"->"dropbox", "overwrite" -> "true"}]/.{
		"Graphics"->"ParameterlessBodyData"},getallparameters["RawFileUpload"]];
	debugPrint["params"->params];
	If[FreeQ[params,#],Message[ServiceExecute::nparam,#1];Throw[$Failed]]&/@{"Root","Path"};
	ext=FileExtension["Path"/.params];
	data="ParameterlessBodyData"/.params;
	debugPrint[Hold[ImportString][Hold[ExportString][data, ext], "Byte"]];
	data=Check[ImportString[ExportString[data, ext], "Byte"],
		Message[ServiceExecute::grext, ext];Throw[$Failed]
	];
	params=params/.HoldPattern[Rule]["ParameterlessBodyData",g_]:>Rule["ParameterlessBodyData",data];
	debugPrint["params"->params];
	rawdata=OAuthClient`rawoauthdata[id,"RawFileUpload",params];
	debugPrint["rawdata"->rawdata];
	data=dropboximportjson[rawdata];
	debugPrint["data"->data];
	Map[(camelcase[#]->data[#])&,{"modified", "revision", "rev","path", "root", "mime_type"}]
]

dropboxcookeddata["FileSearch",id_,args_]:=Block[
	{params,rawdata, root,data},
	params=filterparameters[Join[args,{"Root"->"dropbox","Path"->""}],getallparameters["RawFileSearch"]];
	If[FreeQ[params,#],Message[ServiceExecute::nparam,#1];Throw[$Failed]]&/@{"Root","Path","query"};
	rawdata=OAuthClient`rawoauthdata[id,"RawFileSearch",params];
	debugPrint["rawdata"->rawdata];
	data=dropboximportjson[rawdata];
	debugPrint["data"->data];
	Lookup[#,"path",Sequence@@{}]&/@data
]

$drobpoxdirectoryplotsizelimit=12;

dropboxcookeddata["DirectoryTreePlot",id_,args_]:=Block[
	{params,rawdata,OAuthClient`$CacheResults=True, path, root,data,flatdata},
	params=filterparameters[Join[args,{"Root"->"dropbox","Path"->""}],getallparameters["RawPathData"]];
	If[FreeQ[params,#],Message[ServiceExecute::nparam,#1];Throw[$Failed]]&/@{"Root","Path"};
	{root,path}={"Root","Path"}/.params;
	data=getcontents[id,root,path];
	flatdata=Flatten[Cases[data, HoldPattern[Rule[__]], {0, Infinity}] //. (Rule[a_, b_List]) :> (Thread[Rule[a, b]])];
	TreePlot[Union[flatdata /. (Rule[a_, Rule[b_, _]]) :> Rule[a, b]]]
]

getcontents[id_,root_,path_]:=Module[
	{rawdata = OAuthClient`rawoauthdata[id,"RawPathData",{"Root"->root,"Path"->path}], subpaths, contents, dirQ, headdata, res},
	headdata=dropboximportjson[rawdata];
	res=If[TrueQ[headdata["is_dir"]],
		contents = headdata["contents"];
		If[Length[contents]>$drobpoxdirectoryplotsizelimit,
			contents=Join[Take[contents,$drobpoxdirectoryplotsizelimit],{{"path"->(path<>"/..."),"is_dir"->False}}];
		];
		subpaths = "path" /. contents;
		dirQ = "is_dir" /. contents;
		debugPrint["subpaths"->subpaths];
		debugPrint["dirQ"->dirQ];
		path -> MapThread[If[TrueQ[#2]&&!MatchQ[#1,"..."], getcontents[id, root,#1], #1 -> {}]&, {subpaths, dirQ}]
		,
    	path -> {}
    ];
    res/.HoldPattern[ Rule[x_, {}]] :> x
]
    
dropboxcookeddata[___]:=$Failed 
(* Send Message *)

dropboxsendmessage[___]:=$Failed

(*** Service specific utilites ****)
(* "Sat, 21 Aug 2010 22:31:20 +0000" *)
readDate[date_, form_: DateString] := 
 form@DateList[{StringSplit[date, {" +"," -"}][[1]], 
 	{"DayName", "Day", "MonthNameShort","Year", "Hour", ":", "Minute", ":",
      "Second"}}]
    
getallparameters[str_]:=DeleteCases[Flatten[{"Parameters","PathParameters","BodyData","MultipartData"}/.dropboxdata[str]],
	("Parameters"|"PathParameters"|"BodyData"|"MultipartData")]

formatrootpath[root_,path_]:=StringJoin[stripslash[root],"/",stripslash[path]]
stripslash[""]=""
stripslash[str_]:=If[StringTake[#,1]==="/",StringDrop[#,1],#]&@If[StringTake[str,-1]==="/",StringDrop[str,-1],str]

End[] (* End Private Context *)

SetAttributes[{},{ReadProtected, Protected}];

System`Private`RestoreContextPath[];

(* Return three functions to define oauthservicedata, oauthcookeddata, oauthsendmessage  *)
{OAuthClient`dropboxdata,OAuthClient`dropboxcookeddata,OAuthClient`dropboxsendmessage}
