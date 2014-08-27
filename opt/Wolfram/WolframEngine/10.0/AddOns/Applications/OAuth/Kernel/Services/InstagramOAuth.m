
System`Private`NewContextPath[{"OAuthClient`","System`"}];

(* declare functions that will be used by the OAuth paclet *)
OAuthClient`instagramdata;
OAuthClient`instagramcookeddata;
OAuthClient`instagramsendmessage;


Begin["InstagramOAuthDump`"] (* Begin Private Context *) 

(******************************* Instagram *************************************)

(* Authentication information *)

instagramdata[]={
		"OAuthVersion"		->"2.0",
		"ServiceName" 		-> "Instagram", 
	 	"AuthorizeEndpoint" -> "https://api.instagram.com/oauth/authorize/", 
     	"AccessEndpoint"    -> "https://api.instagram.com/oauth/access_token",
     	"RedirectURI"       -> "https://user.wolfram.com/oauth/facebook_catch.php",
     	"VerifierLabel"     -> "code", (* specified how the authentication is returned in the callback url *)
	 	"ClientInfo"->{40, 42, 33, 49, 78, 33, 42, 41, 53, 52, 42, 13, 10, 102, 118, 194, 
			160, 61, 124, 47, 67, 194, 171, 78, 92, 96, 40, 49, 120, 86, 90, 36, 
			77, 82, 95, 119, 126, 111, 102, 104, 99, 114, 116, 99, 99, 108, 72, 
			34, 123, 56, 111, 121, 106, 90, 114, 122, 101, 53, 35, 39, 108, 66, 
			111, 100, 99, 113, 59, 123, 79, 194, 161, 35, 113, 103, 122, 42, 82, 
			194, 177, 107, 121, 81, 64, 53, 82, 36, 105, 13, 10, 40, 93, 98, 106, 
			74, 77, 13, 10},
	 	"AuthenticationDialog" -> "TokenDialog", (* specifies the interface for receiving the authentication information *)
	 	"Scope"				-> {"basic+relationships+comments+likes"}, (* a common field in OAuth 2.0 that specifies the permissions an app will receive during authentication *)
	 	"Gets"				-> {"UserID","LatestPhotos","CaptionedLatestPhotos",
	 								"Caption","CommentCount","CommentIDs","Comments","CommentAuthors","CreationDate",
	 								"FollowerIDs","Followers","FollowerCount","Followings","FollowingIDs","FollowingCount",
	 								"LatestMedia", "LatestMediaIDs","LikeCount", "Liked", "LikedIDs", "LikeIDs", "Likes","Link","LatestVideos",
	 								"Location","Media","MediaID", "MediaIDs","Owner","OwnerID","Picture","PopularPhotos","PopularVideos","CaptionedPopularPhotos",
	 								"PopularMedia","PopularMediaURLs","PopularMediaIDs","Type","UserData","UserSearch","TagSearch","TagSearchGrid",
	 								"TaggedMedia","TaggedMediaURLs","TaggedMediaIDs"}, (* cooked HTTP Get requests *)
	 	"Posts"				-> {},(* cooked HTTP Post requests *)
	 	"RawGets"			-> {"RawPopularPhotos","RawUserData","RawUserFeed","RawRecentMedia","RawRecentLikedMedia",
	 								"RawFollowings","RawFollowers","RawRelationship","RawMediaInfo","RawMediaComments",
	 								"RawPostMediaComments","RawMediaLikes","RawLikeMedia","RawTagData",
	 								"RawRecentTaggedMedia","RawLocationData","RawLatLongLocations","RawRecentLocationMedia","RawUserSearch","RawTagSearch"},(* raw HTTP Get requests *)
	 	"RawPosts"			-> {},(* raw HTTP Post requests *)
 		"Information"		-> "A service for exchanging data with an Instagram account"
}

(* a function for importing the raw data - usually json or xml - from the service *)
instagramimport[$Failed]:=Throw[$Failed]
instagramimport[json_]:=With[{res=ImportString[json,"JSON"]},
	If[FreeQ[res,_["errors",_]],
		Switch[res,
			_Rule|{_Rule...},assoc@res,
			{{_Rule...}...},assoc/@res,
			_,res
		],
		Message[ServiceExecute::apierr,"message"/.("errors"/.res)];
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
 
instagramdata["RawPopularPhotos"] = {
        "URL"				->  "https://api.instagram.com/v1/media/popular",
        "Parameters" 		-> {},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> instagramimport,
        "RequiredPermissions"-> {}
    }
 
(** User **)   
instagramdata["RawUserData"] = {
        "URL"				->  (ToString[StringForm["https://api.instagram.com/v1/users/`1`",pparam[##]]]&),
        "PathParameters" 		-> {"UserID"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> instagramimport
    }
    
instagramdata["RawUserFeed"] = {
        "URL"				->  "https://api.instagram.com/v1/users/self/feed",
        "HTTPSMethod"		-> "GET",
        "Parameters"		-> {"count","MIN_ID","MAX_ID"},
        "ResultsFunction"	-> instagramimport,
        "RequiredPermissions"-> {}
    }
    
instagramdata["RawUserSearch"] = {
        "URL"				->  "https://api.instagram.com/v1/users/search",
        "HTTPSMethod"		-> "GET",
        "Parameters"		-> {"q","count"},
        "RequiredParameters"-> {"q"},
        "ResultsFunction"	-> instagramimport,
        "RequiredPermissions"-> {}
    }
      
instagramdata["RawRecentMedia"] = {
        "URL"				->  (ToString[StringForm["https://api.instagram.com/v1/users/`1`/media/recent",pparam[##]]]&),
        "PathParameters" 		-> {"UserID"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> instagramimport
    } 

instagramdata["RawRecentLikedMedia"] = {
        "URL"				->  "https://api.instagram.com/v1/users/self/media/liked",
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> instagramimport
    }    
    
(** Relationships **)  

instagramdata["RawFollowings"] = {
        "URL"				->  (ToString[StringForm["https://api.instagram.com/v1/users/`1`/follows",pparam[##]]]&),
        "PathParameters" 		-> {"UserID"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> instagramimport
    }    
        
instagramdata["RawFollowers"] = {
        "URL"				-> (ToString[StringForm["https://api.instagram.com/v1/users/`1`/followed-by",pparam[##]]]&),
        "PathParameters" 		-> {"UserID"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> instagramimport
    }    
    
instagramdata["RawRelationship"] = {
        "URL"				-> (ToString[StringForm["https://api.instagram.com/v1/users/`1`/relationship",pparam[##]]]&),
        "PathParameters" 		-> {"UserID"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> instagramimport
    }    
      
(** Media **)  
    
instagramdata["RawMediaInfo"] = {
        "URL"				->  (ToString[StringForm["https://api.instagram.com/v1/media/`1`",pparam[##]]]&),
        "PathParameters" 	-> {"MediaID"},
        "RequiredParameters"-> {"MediaID"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> instagramimport
    }   
    
instagramdata["RawMediaComments"] = {
        "URL"				->  (ToString[StringForm["https://api.instagram.com/v1/media/`1`/comments",pparam[##]]]&),
        "PathParameters" 		-> {"MediaID"},
        "RequiredParameters"-> {"MediaID"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> instagramimport
    }     
       
instagramdata["RawPostMediaComments"] = {
        "URL"				->  (ToString[StringForm["https://api.instagram.com/v1/media/`1`/comments",pparam[##]]]&),
        "PathParameters" 		-> {"MediaID"},
        "Parameters"		-> {"text"},
        "RequiredParameters"-> {"MediaID","text"},
        "HTTPSMethod"		-> "POST",
        "ResultsFunction"	-> instagramimport
    } 
    
instagramdata["RawMediaLikes"] = {
        "URL"				->  (ToString[StringForm["https://api.instagram.com/v1/media/`1`/likes",pparam[##]]]&),
        "PathParameters" 		-> {"MediaID"},
        "RequiredParameters"-> {"MediaID"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> instagramimport
    }     
       
instagramdata["RawLikeMedia"] = {
        "URL"				->  (ToString[StringForm["https://api.instagram.com/v1/media/`1`/likes",pparam[##]]]&),
        "PathParameters" 		-> {"MediaID"},
        "RequiredParameters"-> {"MediaID"},
        "HTTPSMethod"		-> "POST",
        "ResultsFunction"	-> instagramimport
    }     
      
(** Tags **)  
instagramdata["RawTagData"] = {
        "URL"				->  (ToString[StringForm["https://api.instagram.com/v1/tags/`1`",pparam[##]]]&),
        "PathParameters" 		-> {"Tag"},
        "RequiredParameters"-> {"Tag"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> instagramimport
    }   

instagramdata["RawRecentTaggedMedia"] = {
        "URL"				->  (ToString[StringForm["https://api.instagram.com/v1/tags/`1`/media/recent",pparam[##]]]&),
        "PathParameters" 		-> {"Tag"},
        "RequiredParameters"-> {"Tag"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> instagramimport
    }   

instagramdata["RawTagSearch"] = {
        "URL"				->  "https://api.instagram.com/v1/tags/search",
        "Parameters" 		-> {"q"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> instagramimport
    }  
      
(** Locations **)  
instagramdata["RawLocationData"] = {
        "URL"				->  (ToString[StringForm["https://api.instagram.com/v1/locations/`1`",pparam[##]]]&),
        "PathParameters" 		-> {"Location"},
        "RequiredParameters"-> {"Location"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> instagramimport
    } 
            
instagramdata["RawLatLongLocations"] = {
        "URL"				->  "https://api.instagram.com/v1/locations/search",
        "Parameters" 		-> {"lat","lng","distance"},
        "RequiredParameters"-> {"lat","lng"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> instagramimport
    }   
    
instagramdata["RawRecentLocationMedia"] = {
        "URL"				->  (ToString[StringForm["https://api.instagram.com/v1/locations/`1`/media/recent",pparam[##]]]&),
        "PathParameters" 		-> {"Location"},
        "RequiredParameters"-> {"Location"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> instagramimport
    }   
    (*
    Requires setting up predefined geographies associated with our app
instagramdata["RawRecentGeographyMedia"] = {
        "URL"				->  (ToString[StringForm["https://api.instagram.com/v1/geographies/`1`/media/recent",pparam[##]]]&),
        "PathParameters" 		-> {"Location"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> instagramimport
    } 
    *)
instagramdata[___]:=$Failed
(****** Cooked Properties ******)
  
(* cooked data queries 
	Cooked queries call at least one of the raw queried underneath but add pre and post processing to make the function easier to use and the result clearer.
*)  


(* SocialMediaData["Instagram", "Properties"]

{"Caption", "CommentAuthors", "CommentCount", "CommentIDs", 
"Comments", "CreationDate", "FollowerIDs", "Followers", 
"FollowingIDs", "Followings", "Image", "LatestMedia", 
"LatestMediaIDs", "LikeCount", "Liked", "LikedIDs", "LikeIDs", 
"Likes", "Link", "Location", "LowResolutionImage", "Media", 
"MediaID", "MediaIDs", "Owner", "OwnerID", "Picture", "PopularMedia", 
"PopularMediaIDs", "Thumbnail", "Type", "UserData"}

{"Type", "UserData"}

*)
(* User *)

$InstagramPhotoImportCount=10;
$InstagramVideoImportDefault="ImageLink"

instagramcookeddata[prop_,id_,rule_Rule, rest___]:=instagramcookeddata[prop,id,{rule}, rest]
instagramcookeddata[prop_,id_]:=instagramcookeddata[prop,id,{}]
  

instagramcookeddata[prop:("UserID"|"Picture"|"UserData"),id_,args_]:=Module[
	{rawdata,as, keys, postproc,data,resolution},
	rawdata=OAuthClient`rawoauthdata[id,"RawUserData",args];
	resolution=getquality[{args},"Low"];
	debugPrint["resolution"->resolution];
	as=instagramimport[rawdata];
	keys=Switch[prop,
		"UserID",{"id"},
		"Picture",{"profile_picture"},
		"UserData",{}
	];
	postproc=Switch[prop,
		"UserID",ToExpression,
		"Picture",importphoto,
		"UserData",
			(Association[Cases[#,HoldPattern[Rule["id"|"username"|"full_name"|"website"|"media"|"follows"|"followed_by",Except[""]]],Infinity]]&),
		
		(*(DeleteCases[
			Rule @@@ Transpose[{{"id", "username", "full_name", "website", "media", "follows", "followed_by"}, 
   				Join[{"id", "username", "full_name", "website"} /. #, {"media", "follows", "followed_by"} /. (
   					"counts" /. {"username" -> "bob", "counts" -> #})]}],
				Rule[_,""]]&),*)
				
		_,Identity
	];
	If[KeyExistsQ[as,"data"],
		data=as["data"];
	debugPrint["data"->data];
		postproc@getdata[data,keys]
		,
		{}
	]
]

  
instagramcookeddata["UserSearch",id_,args_]:=Module[
	{rawdata,as, data,params, names, ids},
	params=filterparameters[(args/."Query"->"q"),getallparameters["RawUserSearch"]];
	rawdata=OAuthClient`rawoauthdata[id,"RawUserSearch",params];
	(***** workaround for JSON Import Bug  255746 ********
	as=instagramimport[rawdata];
	
	as/;as=!=$Failed
	
	*)
	data=StringCases[rawdata, "\"data\":[" ~~ Longest[x__] ~~ "]" :> x];
	If[Length[data]=!=1,Return[{}]];
	names=StringCases[data[[1]], "\"username\":\"" ~~ Shortest[x__] ~~ "\"" :> x];
	ids = StringCases[data[[1]], "\"id\":\"" ~~ Shortest[x__] ~~ "\"" :> x];
	
	If[Length[names]===Length[ids],
		MapThread[Association[{"Username"->#1,"UserID"->#2}]&,{names, ids}]
		,
		{}
	]
	
	 (* end workaround *)
]
  
instagramcookeddata[prop:("TagSearch"|"TagSearchGrid"),id_,args_]:=Module[
	{rawdata,as, data,params, names, counts},
	params=filterparameters[(args/."Query"->"q"),getallparameters["RawTagSearch"]];
	rawdata=OAuthClient`rawoauthdata[id,"RawTagSearch",params];
	(***** workaround for JSON Import Bug  255746 ********
	as=instagramimport[rawdata];
	
	as/;as=!=$Failed
	
	*)
	data=StringCases[rawdata, "\"data\":[" ~~ Longest[x__] ~~ "]" :> x];
	
	(*If[Length[data]=!=1,Return[{}]];
	names=StringCases[data[[1]], "\"username\":\"" ~~ Shortest[x__] ~~ "\"" :> x];
	ids = StringCases[data[[1]], "\"id\":\"" ~~ Shortest[x__] ~~ "\"" :> x];
	
	If[Length[names]===Length[ids],
		MapThread[Association[{"Username"->#1,"UserID"->#2}]&,{names, ids}]
		,
		{}
	]
	*)
	If[Length[data]=!=1,Return[{}]];
	counts=StringCases[data[[1]], "\"media_count\":" ~~ Shortest[x__] ~~ "," :> ToExpression[x]];
	names = StringCases[data[[1]], "\"name\":\"" ~~ Shortest[x__] ~~ "\"" :> x];
	debugPrint["names"->Length[names]->names];
	debugPrint["counts"->Length[counts]->counts];
	If[Length[counts]=!=Length[names],Return[{}]];
	Switch[prop,
		"TagSearch",
		MapThread[Association[{"Name"->#1,"MediaCount"->#2}]&,{names, counts}],
		"TagSearchGrid",
		addtitles[prettygrid[Transpose[{names,counts}]],{"Name","MediaCount"}]
		
	]
	 (* end workaround *)
]

(* the user's likes *)
instagramcookeddata[prop:("Liked"|"LikedIDs"),id_,args_]:=Module[
	{rawdata,as, data,keys,postproc,resolution},
	resolution=getquality[{args},"Low"];
	rawdata=OAuthClient`rawoauthdata[id,"RawRecentLikedMedia",args];

	as=instagramimport[rawdata];
	keys=Switch[prop,
		"LikedIDs",{"id"},
		"Liked",{}
	];
	postproc=Switch[prop,
		"Liked",(getphotos[#,resolution]&),
		_,Identity
	];
	If[KeyExistsQ[as,"data"],
		data=as["data"];
	debugPrint["data"->data];
		postproc@getdata[data,keys]
		,{}
	]
]

(* Media *)
instagramcookeddata[prop:("PopularMedia"|"PopularMediaURLs"|"PopularMediaIDs"),id_,args_]:=Module[
	{rawdata,as, resolution,keys,postproc,data, videoform,urls,allids},
	
	rawdata=OAuthClient`rawoauthdata[id,"RawPopularPhotos",args];
	debugPrint["Popular1"];
	resolution=getquality[{args},"Low"];
	
	debugPrint["Popular2"];
		
	(***** workaround for JSON Import Bug  255746 ********
 
	as=instagramimport[rawdata];
	keys=Switch[prop,
		"PopularMedia",{},
		"PopularMediaURLs",{"images",resolution,"url"},
		"PopularMediaIDs",{"id"}
	];
	postproc=Switch[prop,
		"PopularMedia",(getphotos[#,resolution]&),
		"PopularMediaURLs",(Hyperlink/@#&),
		"PopularMediaIDs",ToExpression,
		_,Identity
	];
	If[KeyExistsQ[as,"data"],
		data=as["data"];
	debugPrint["data"->data->getdata[data,keys]];
		postproc@getdata[data,keys]
		,{}
	]
	*)
	videoform=("VideoImport"/.args)/."VideoImport"->$InstagramVideoImportDefault;
	
	debugPrint["Popular3"];
	data=StringCases[rawdata, "\"data\":[" ~~ Longest[x__] ~~ "]" :> x];
	If[Length[data]=!=1,Return[{}]];
	data=First[data];
	debugPrint["Popular prop "->prop];
	Switch[prop,
		"PopularMedia",getmediajson[data,resolution, videoform],
		"PopularMediaURLs",urls=Flatten[StringCases[data,  "\"" <> resolution <> "\":" ~~ x : ("{" ~~ Shortest[__] ~~ "}") :> x]];
			If[!MatchQ[urls,{_String..}],Return[{}]];
			urls="url" /. (ImportString[#, "JSON"] & /@urls);
			If[!MatchQ[urls,{_String..}],Return[{}]];
			Hyperlink/@urls,
		"PopularMediaIDs",
			ToExpression@getids[data,"Media"]
	]
	
	
]
 
 
instagramcookeddata[prop:("TaggedMedia"|"TaggedMediaURLs"|"TaggedMediaIDs"),id_,args_]:=Module[
	{rawdata, resolution,data, videoform,urls},
	
	rawdata=OAuthClient`rawoauthdata[id,"RawRecentTaggedMedia",args];
	resolution=getquality[{args},"Low"];
		
	videoform=("VideoImport"/.args)/."VideoImport"->$InstagramVideoImportDefault;
	
	data=StringCases[rawdata, "\"data\":[" ~~ Longest[x__] ~~ "]" :> x];
	If[Length[data]=!=1,Return[{}]];
	data=First[data];
	Switch[prop,
		"TaggedMedia",getmediajson[data,resolution, videoform],
		"TaggedMediaURLs",urls=Flatten[StringCases[data,  "\"" <> resolution <> "\":" ~~ x : ("{" ~~ Shortest[__] ~~ "}") :> x]];
			If[!MatchQ[urls,{_String..}],Return[{}]];
			urls="url" /. (ImportString[#, "JSON"] & /@urls);
			If[!MatchQ[urls,{_String..}],Return[{}]];
			Hyperlink/@urls,
		"TaggedMediaIDs",
			ToExpression@getids[data,"Media"]
	]
	
	
]
 
 
instagramcookeddata[prop:("PopularPhotos"|"CaptionedPopularPhotos"|"PopularVideos"),id_,args_]:=Module[
	{rawdata,as, media, captions, data,resolution, videoform},
	videoform=If[prop==="PopularVideos",{"OnlyVideo",("VideoImport"/.args)}/."VideoImport"->$InstagramVideoImportDefault,None];
	rawdata=OAuthClient`rawoauthdata[id,"RawPopularPhotos",args];
	resolution=getquality[{args},"Low"];
	
	(***** workaround for JSON Import Bug  255746 ********
 
	as=instagramimport[rawdata];
	If[KeyExistsQ[as,"data"],
		data=as["data"];
		If[Length[data]>0,
			images=getphotos[data,resolution,"image"];
			If[prop==="CaptionedLatestPhotos",
				captions="text"/.("caption"/.data);
				MapThread[Labeled[#1,#2]&,{images,captions}],
				images
			],{}
		],{}
	]
	*)
	
	data=StringCases[rawdata, "\"data\":[" ~~ Longest[x__] ~~ "]" :> x];
	If[Length[data]=!=1,Return[{}]];
	media=getmediajson[data[[1]],resolution, videoform];
	
	If[prop==="CaptionedPopularPhotos",
		captions = StringCases[data[[1]], "\"caption\":" ~~ x : ("{" ~~ Shortest[__] ~~ "}") :> x];
		captions = Flatten@StringCases[captions, "\"text\":\"" ~~ x : Shortest[__] ~~ "\"" :> OAuthClient`fromunicode[x]];
		MapThread[Labeled[#1,#2]&,{media,captions}],
		media
	]
	 (* end workaround *)
]

instagramcookeddata[prop:("LatestPhotos"|"CaptionedLatestPhotos"|"LatestVideos"),id_,args_]:=Module[
	{rawdata,as, media, captions, data,resolution, videoform},
	videoform=If[prop==="LatestVideos",{"OnlyVideo",("VideoImport"/.args)}/."VideoImport"->$InstagramVideoImportDefault,None];
	rawdata=OAuthClient`rawoauthdata[id,"RawRecentMedia",args];
	resolution=getquality[{args},"Low"];
	
	(***** workaround for JSON Import Bug  255746 ********
 
	as=instagramimport[rawdata];
	If[KeyExistsQ[as,"data"],
		data=as["data"];
		If[Length[data]>0,
			images=getphotos[data,resolution,"image"];
			If[prop==="CaptionedLatestPhotos",
				captions="text"/.("caption"/.data);
				MapThread[Labeled[#1,#2]&,{images,captions}],
				images
			],{}
		],{}
	]
	*)
	
	data=StringCases[rawdata, "\"data\":[" ~~ Longest[x__] ~~ "]" :> x];
	If[Length[data]=!=1,Return[{}]];
	media=getmediajson[data[[1]],resolution, videoform];
	
	If[prop==="CaptionedLatestPhotos",
		captions = StringCases[data[[1]], "\"caption\":" ~~ x : ("{" ~~ Shortest[__] ~~ "}") :> x];
		captions = Flatten@StringCases[captions, "\"text\":\"" ~~ x : Shortest[__] ~~ "\"" :> OAuthClient`fromunicode[x]];
		MapThread[Labeled[#1,#2]&,{media,captions}],
		media
	]
	 (* end workaround *)
]

instagramcookeddata["Media",id_,args_]:=Module[
	{rawdata,as, images, data,resolution,videoform},
	rawdata=OAuthClient`rawoauthdata[id,"RawMediaInfo",args];
	videoform=("VideoImport"/.args)/."VideoImport"->$InstagramVideoImportDefault;
	resolution=getquality[{args},"Low"];
		(***** workaround for JSON Import Bug  255746 ********
 as=instagramimport[rawdata];
	If[KeyExistsQ[as,"data"],
		data=as["data"];
		If[Length[data]>0,
			images=getmedia[data,resolution,videoform];
			,{}
		],{}
	]
	*)
	data=StringCases[rawdata, "\"data\":[" ~~ Longest[x__] ~~ "]" :> x];
	If[Length[data]=!=1,Return[{}]];
	getmediajson[data[[1]],resolution, videoform];
	
]

mediainfokeys=("Caption"|"CommentCount"|"CommentIDs"|"Comments"|"CommentAuthors"|"CreationDate"|"Link"|"Location"|"MediaID"|"Owner"|"OwnerID"|"Type")
instagramcookeddata[prop:mediainfokeys,id_,args_]:=Module[
	{rawdata,as, data,resolution,keys,postproc},
	rawdata=OAuthClient`rawoauthdata[id,"RawMediaInfo",args];
	resolution=getquality[{args},"Low"];
	as=instagramimport[rawdata];
	keys=Switch[prop,
		"Caption",{"caption","text"},
		"CommentCount",{"comments","count"},
		"CommentIDs",{"comments","data","id"},
		"Comments",{"comments","data","text"},
		"CommentAuthors",{"comments","data","from","full_name"},
		"CreationDate",{"created_time"},
		"Link",{"link"},
		"Location",{"location"},
		"MediaID",{"id"},
		"Owner",{"user","full_name"},
		"OwnerID",{"user","id"},
		"Type",{"type"}
	];
	postproc=Switch[prop,
		"CommentCount"|"CommentIDs"|"OwnerID",ToExpression,
		"CreationDate",readinstagramdate,
		"Link",Hyperlink,
		"Location",(#/.Null->{}&),
		_,Identity
	];
	If[KeyExistsQ[as,"data"],
		data=as["data"];
	debugPrint["data"->data->getdata[data,keys]];
		postproc@getdata[data,keys]
		,{}
	]
]

instagramcookeddata[prop:("FollowerIDs"|"Followers"|"FollowerCount"),id_,args_]:=Module[
	{rawdata,as, data,keys,postproc},
	rawdata=OAuthClient`rawoauthdata[id,"RawFollowers",args];
	(***** workaround for JSON Import Bug  255746 ********
	as=instagramimport[rawdata];
	keys=Switch[prop,
		"FollowerIDs",{"id"},
		"Followers",{"full_name"}
	];
	postproc=Switch[prop,
		"FollowerIDs",ToExpression,
		_,Identity
	];
	If[KeyExistsQ[as,"data"],
		data=as["data"];
	debugPrint["data"->data->getdata[data,keys]];
		postproc@getdata[data,keys]
		,{}
	]
	*)
	
	data=StringCases[rawdata, "\"data\":[" ~~ x__ ~~ "]" :> x];
	If[Length[data]=!=1,Return[{}]];
	Switch[prop,
		"FollowerCount",StringCount[data[[1]], "\"username\""],
		"FollowerIDs",StringCases[data[[1]], "\"id\":\"" ~~ x : Shortest[__] ~~ "\"" :> x],
		"Followers",OAuthClient`fromunicode[StringCases[data[[1]], "\"full_name\":\"" ~~ x : Shortest[__] ~~ "\"" :> x]]
	]
	 (* end workaround *)

]

instagramcookeddata[prop:("FollowingIDs"|"Followings"|"FollowingCount"),id_,args_]:=Module[
	{rawdata,as, data,keys,postproc},
	rawdata=OAuthClient`rawoauthdata[id,"RawFollowings",args];
	
	(***** workaround for JSON Import Bug  255746 ********
	as=instagramimport[rawdata];
	debugPrint["as"->as];
	keys=Switch[prop,
		"FollowingIDs",{"id"},
		"Followings",{"full_name"}
	];
	postproc=Switch[prop,
		"FollowingIDs",ToExpression,
		_,Identity
	];
	If[KeyExistsQ[as,"data"],
		data=as["data"];
	debugPrint["data"->data];
		postproc@getdata[data,keys]
		,{}
	]
	*)
	
	data=StringCases[rawdata, "\"data\":[" ~~ x__ ~~ "]" :> x];
	If[Length[data]=!=1,Return[{}]];
	Switch[prop,
		"FollowingCount",StringCount[data[[1]], "\"username\""],
		"FollowingIDs",StringCases[data[[1]], "\"id\":\"" ~~ x : Shortest[__] ~~ "\"" :> x],
		"Followings",OAuthClient`fromunicode[StringCases[data[[1]], "\"full_name\":\"" ~~ x : Shortest[__] ~~ "\"" :> x]]
	]
	 (* end workaround *)
	
]

instagramcookeddata[prop:("LatestMedia"|"LatestMediaIDs"|"MediaIDs"),id_,args_]:=Module[
	{rawdata,as, data,keys,postproc,resolution,videoform},
	resolution=getquality[{args},"Low"];
	debugPrint["resolution"->resolution];
	rawdata=OAuthClient`rawoauthdata[id,"RawRecentMedia",args];
	videoform=("VideoImport"/.args)/."VideoImport"->$InstagramVideoImportDefault;
	
	
	(***** workaround for JSON Import Bug  255746 ********
	as=instagramimport[rawdata];
	keys=Switch[prop,
		"LatestMediaIDs"|"MediaIDs",{"id"},
		"LatestMedia",{}
	];
		
	postproc=Switch[prop,
		"LatestMedia",(getphotos[#,resolution]&),
		_,Identity
	];
	If[KeyExistsQ[as,"data"],
		data=as["data"];
	debugPrint["data"->data];
		postproc@getdata[data,keys]
		,{}
	]
	*)
	data=StringCases[rawdata, "\"data\":[" ~~ Longest[x__] ~~ "]" :> x];
	If[Length[data]=!=1,Return[{}]];
	Switch[prop,
		"LatestMediaIDs"|"MediaIDs",StringCases[data[[1]], "\"id\":\"" ~~ x : Shortest[__] ~~ "\"" :> x],
		"LatestMedia",getmediajson[data[[1]],resolution,videoform]
	]
	 (* end workaround *)
	
]

(* Likes on a media *)
instagramcookeddata[prop:("LikeCount"|"LikeIDs"|"Likes"),id_,args_]:=Module[
	{rawdata,as, data,keys,postproc},
	rawdata=OAuthClient`rawoauthdata[id,"RawMediaLikes",args];

	
	(***** workaround for JSON Import Bug  255746 ********
	keys=Switch[prop,
		"LikeCount",{"full_name"},
		"LikeIDs",{"id"},
		"Likes",{}
	];
	postproc=Switch[prop,
		"LikeCount",Length,
		_,Identity
	];
	
	as=instagramimport[rawdata];
	If[KeyExistsQ[as,"data"],
		data=as["data"];
	debugPrint["data"->data];
		postproc@getdata[data,keys]
		,{}
	]
	*)
	
	data=StringCases[rawdata, "\"data\":[" ~~ x__ ~~ "]" :> x];
	If[Length[data]=!=1,Return[{}]];
	Switch[prop,
		"LikeCount",StringCount[data[[1]], "\"username\""],
		"LikeIDs",StringCases[data[[1]], "\"id\":\"" ~~ x : Shortest[__] ~~ "\"" :> x],
		"Likes",OAuthClient`fromunicode[StringCases[data[[1]], "\"full_name\":\"" ~~ x : Shortest[__] ~~ "\"" :> x]]
	]
	 (* end workaround *)
	
]


instagramcookeddata[___]:=$Failed

(******** Send message ****************)
OAuthClient`instagramsendmessage[___]:=$Failed

(******** Service specific utilities ***********)

getallparameters[str_]:=DeleteCases[Flatten[{"Parameters","PathParameters","BodyData","MultipartData"}/.instagramdata[str]],
	("Parameters"|"PathParameters"|"BodyData"|"MultipartData")]
	
getphotos[data_List,resolution_]:=With[{urls="url"/.(resolution/.("images"/.data))},
	importphoto/@urls
]

getvideos[data_List,resolution_]:=With[{urls="url"/.(resolution/.("images"/.data))},
	importvideo/@urls
]

getmediajson[data_String,resolution_,videoform_:$InstagramVideoImportDefault]:=Block[
	{splitdata,imageurls,videourls, types},
	splitdata = Rest[StringSplit[data, "attribution"]];
	types = Flatten[StringCases[splitdata, "\"type\":\"" ~~ Shortest[x__] ~~ "\"" :> x]];
	
	If[Length[types]=!=Length[splitdata],Return[{}]];
	
	imageurls=StringCases[splitdata, "\"images\":" ~~ x : ("{" ~~ Shortest[__] ~~ "}}") :> x];
	videourls=StringCases[splitdata, "\"videos\":" ~~ x : ("{" ~~ Shortest[__] ~~ "}}") :> x];
	
	imageurls=Flatten[StringCases[#,  "\"" <> resolution <> "\":" ~~ x : ("{" ~~ Shortest[__] ~~ "}") :> x]]&/@imageurls;
	videourls=Flatten[StringCases[#,  "\"" <> resolution <> "\":" ~~ x : ("{" ~~ Shortest[__] ~~ "}") :> x]]&/@videourls;
	If[MatchQ[videourls,{({_String}|{})...}]&&MatchQ[imageurls,{{_String}...}],
		imageurls="url"/.Map[First[ImportString[#, "JSON"]] &, imageurls, {2}];
		videourls="url"/.Map[First[ImportString[#, "JSON"]] &, videourls, {2}];
		If[MatchQ[videourls,{_String...}]&&MatchQ[imageurls,{_String...}],
			MapThread[importmedia[#1,#2,#3,videoform]&,{imageurls, videourls,types}],
			{}
		]
		,{}
	]
	
]

importmedia[_,_,"image",{"OnlyVideo",_}]:=Sequence@@{}
importmedia[iurl_,_,"image",_]:=importphoto[iurl]

importmedia[iurl_,vurl_,"video",{"OnlyVideo",elem_}]:=importmedia[iurl,vurl,"video",elem]
importmedia[iurl_,vurl_,"video",Automatic]:=importmedia[iurl,vurl,"video",$InstagramVideoImportDefault]
importmedia[iurl_,vurl_,"video",True]:=importmedia[iurl,vurl,"video","Animation"]
importmedia[iurl_,_,"video","Image"]:=importmedia[iurl,"","image","Image"]
importmedia[_,_,"video",None]:=Sequence@@{}
importmedia[iurl_,vurl_,"video","ImageLink"]:=Hyperlink[importmedia[iurl,"","image","Image"],vurl]
importmedia[_,vurl_,"video",elem_]:=importvideo[vurl,elem]

importmedia[__]:=Sequence@@{}

importphoto[{str_String}]:=importphoto[str]
importphoto[str_String]:=Import[str]
importphoto[hy_Hyperlink]:=Import[Last[hy]]
importphoto[___]:=Sequence@@{}

importvideo[{str_String},elem_]:=importvideo[str,elem]
importvideo[str_String,elem_]:=Import[str,{"QuickTime",elem}]
importvideo[hy_Hyperlink,rest___]:=importvideo[Last[hy],rest]
importvideo[___]:=Sequence@@{}

readinstagramdate[date_, form_:DateObject]:=form[ToExpression[date] + AbsoluteTime[{1970, 1, 1, 0, 0, 0}, TimeZone -> 0]]

pparam[s_String]:=s
pparam[s_?IntegerQ]:=ToString[s]
pparam[___]:="self"

qualityrules={"High"|"Standard"|"Full"|"StandardResolution"->"standard_resolution","Low"|"LowResolution"->"low_resolution","Thumb"|"thumb"|"Thumbnail"->"thumbnail"};
getquality[args_,default_]:=(("MediaResolution"/.Cases[{args},_Rule,Infinity])/."MediaResolution"->default)/.qualityrules

(* TODO: remove when JSON bug is fixed *)
getids[data_,type_]:=Block[{allids},
	debugPrint["getids"];
	allids=Flatten[StringCases[data, "\"" <> "id" <> "\":\"" ~~ x : Shortest[__] ~~ "\"" :> x]];
	Switch[type,
		"User",
		Flatten[StringCases[allids, 
			(DigitCharacter ..) ~~ "_" ~~ (x : (DigitCharacter ..)) :> x]],
		"Media",
		Flatten[StringCases[allids, 
			(x : (DigitCharacter ..)) ~~ "_" ~~ (DigitCharacter ..) :> x]],
		"FullMedia",
		Flatten[StringCases[allids, 
			(DigitCharacter ..) ~~ "_" ~~ (DigitCharacter ..)]]
	]
	
];

End[] (* End Private Context *)

SetAttributes[{},{ReadProtected, Protected}];

System`Private`RestoreContextPath[];

(* Return three functions to define oauthservicedata, oauthcookeddata, oauthsendmessage  *)
{OAuthClient`instagramdata,OAuthClient`instagramcookeddata,OAuthClient`instagramsendmessage}
