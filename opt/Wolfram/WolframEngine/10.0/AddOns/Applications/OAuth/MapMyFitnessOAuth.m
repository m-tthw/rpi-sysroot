
System`Private`NewContextPath[{"OAuthClient`","OAuthClientDump`","System`"}];

(* declare functions that will be used by the OAuth paclet *)
OAuthClient`mapmyfitnessdata;
OAuthClient`mapmyfitnesscookeddata;
OAuthClient`mapmyfitnesssendmessage;


Begin["InstagramOAuthDump`"] (* Begin Private Context *) 

(******************************* Instagram *************************************)

(* Authentication information *)
mapmyfitnessdata[]={
		"ServiceName"       -> "MapMyFitness",
        "OAuthVersion"		-> "1.0a",
        "RequestEndpoint"   -> "http://api.mapmyfitness.com/3.1/oauth/request_token",
        "AccessEndpoint"    -> "http://api.mapmyfitness.com/3.1/oauth/access_token",
        "AuthorizeEndpoint" -> "http://www.mapmyfitness.com/oauth/authorize",
        "VerifierLabel"		->	"security code",
	 	"ClientInfo"->{40, 42, 33, 49, 78, 33, 42, 41, 53, 52, 42, 13, 10, 102, 118, 118, 
			39, 34, 53, 79, 88, 43, 94, 59, 120, 40, 121, 99, 84, 99, 66, 95, 
			194, 163, 45, 37, 117, 122, 107, 121, 91, 81, 59, 194, 168, 126, 99, 
			72, 55, 103, 87, 47, 115, 41, 54, 102, 111, 114, 39, 41, 113, 34, 50, 
			46, 85, 35, 90, 119, 103, 58, 90, 95, 73, 82, 106, 123, 194, 164, 
			116, 39, 91, 91, 36, 123, 63, 75, 13, 10, 101, 119, 59, 121, 101, 98, 
			194, 171, 59, 62, 194, 171, 53, 60, 47, 194, 168, 82, 49, 95, 125, 
			43, 68, 194, 168, 87, 55, 114, 51, 59, 103, 36, 194, 173, 117, 46, 
			101, 113, 36, 108, 112, 52, 51, 13, 10},
	 	"AuthenticationDialog" -> "TokenDialog", (* specifies the interface for receiving the authentication information *)
	 	"Gets"				-> {}, (* cooked HTTP Get requests *)
	 	"Posts"				-> {},(* cooked HTTP Post requests *)
	 	"RawGets"			-> {"RawUserData","RawFriends","RawRoute","RawRoutes","RawWorkout","RawWorkouts"},(* raw HTTP Get requests *)
	 	"RawPosts"			-> {},(* raw HTTP Post requests *)
 		"Information"		-> "A service for exchanging data with a MapMyFitness, MapMyRun, MapMyRide, MayMyHike or MapMyWalk account"
}

(* a function for importing the raw data - usually json or xml - from the service *)
mapmyfitnessimport[$Failed]:=Throw[$Failed]
mapmyfitnessimport[json_]:=Block[{res=ImportString[json,"JSON"], res2},
	debugPrint["mapmyfitnessimport res"->res];
	If[FreeQ[res,_["errors",{_}]],
		res2=("result"/.res);
		Switch[res2,
			_Rule|{_Rule...},assoc@res2,
			{{_Rule...}...},assoc/@res2,
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
 
baseurl="http://api.mapmyfitness.com/3.1/";

mapmyfitnessdata["RawUserData"] = {
        "URL"				-> baseurl<>"users/get_user_summary",
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> mapmyfitnessimport
    }
    
mapmyfitnessdata["RawFriends"] = {
        "URL"				-> baseurl<>"users/get_user_friends",
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> mapmyfitnessimport
    }
    
mapmyfitnessdata["RawRoute"] = {
        "URL"				-> (ToString@StringForm[baseurl<>"routes/get_route?&route_id=`1`", #]&) ,
        "PathParameters"	-> "Route",
        "RequiredParameters"	-> "Route",
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> mapmyfitnessimport
    }
    
mapmyfitnessdata["RawWorkout"] = {
        "URL"				-> (ToString@StringForm[baseurl<>"workouts/get_workout?&workout_id=`1`", #]&) ,
        "PathParameters"	-> "Workout",
        "RequiredParameters"-> "Workout",
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> mapmyfitnessimport
    }
    
mapmyfitnessdata["RawRoutes"] = {
        "URL"				-> baseurl<>"routes/get_routes",
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> mapmyfitnessimport
    }
    
mapmyfitnessdata["RawWorkouts"] = {
        "URL"				-> baseurl<>"workouts/get_workouts",
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> mapmyfitnessimport
    }
      
mapmyfitnessdata[___]:=$Failed
(****** Cooked Properties ******)
  
(* cooked data queries 
	Cooked queries call at least one of the raw queried underneath but add pre and post processing to make the function easier to use and the result clearer.
*)  

mapmyfitnesscookeddata[prop:("UserID"|"Picture"|"UserData"),id_,args___]:=Module[
	{rawdata,as, keys, postproc,data,resolution},
	rawdata=OAuthClientDump`rawoauthdata[id,"RawUserData",args];
	resolution=getquality[{args},"thumbnail"];
	debugPrint["resolution"->resolution];
	as=mapmyfitnessimport[rawdata];
	keys=Switch[prop,
		"UserID",{"id"},
		"Picture",{"profile_picture"},
		"UserData",{}
	];
	postproc=Switch[prop,
		"UserID",ToExpression,
		"Picture",importphoto,
		"UserData",
			(Cases[#,HoldPattern[Rule["id"|"username"|"full_name"|"website"|"media"|"follows"|"followed_by",Except[""]]],Infinity]&),
		
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

mapmyfitnesscookeddata[___]:=$Failed

(******** Send message ****************)
OAuthClient`mapmyfitnesssendmessage[___]:=$Failed

(******** Service specific utilities ***********)

End[] (* End Private Context *)

SetAttributes[{},{ReadProtected, Protected}];

System`Private`RestoreContextPath[];

(* Return three functions to define oauthservicedata, oauthcookeddata, oauthsendmessage  *)
{OAuthClient`mapmyfitnessdata,OAuthClient`mapmyfitnesscookeddata,OAuthClient`mapmyfitnesssendmessage}
