(* 
This package contains the information about individual OAuth services for which Wolfram provides support 
The values here use Wolfram's "app" to provide a link between Mathematica and the users account on a service.
*)

System`Private`NewContextPath[{"OAuthClient`","OAuthClientDump`","System`"}];
(*
BeginPackage["OAuthServicesData`"]
*)
(* Exported symbols added here with SymbolName::usage *)  
OAuthClient`predefinedservicelist;
OAuthClient`OAuthServicesData;
OAuthClient`oauthcookeddata;
OAuthClient`oauthsendmessage;

Begin["OAuthServicesDataDump`"] (* Begin Private Context *) 

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
predefinedservicelist:={"BodyMedia","Twitter","FitBit","Runkeeper","LinkedIn", "Facebook" (*,"Instagram" *)}

OAuthServicesData[args___]:=With[{res=oauthservices[args]},
	res/;res=!=$Failed&&Head[res]=!=oauthservicedata]

oauthservices[name_,prop___]:=Module[{data=oauthservicedata[name],availableproperties},
	availableproperties=First/@data;
	Switch[{prop},
		{},
		data,
		{"Properties"},availableproperties,
		{"Authentication"},
			Thread[defaultOAuthLabels->(defaultOAuthLabels/.Join[data,defaultOAuthParams])]
		,
		{Alternatives@@availableproperties},
		prop/.data,
		_,
		oauthservicedata[name,prop]		
	]
]


(*************************** Data for Services *********************************)
(* Storage of information about our apps and the services they work with *)


(* ------------- Instagram ------------------------ *)
(*
Get[FileNameJoin[{DirectoryName[System`Private`$InputFileName], "InstagramOAuth.m"}]];

oauthservicedata["Instagram",args___]:=OAuthClient`instagramdata[args]
oauthcookeddata["Instagram",args___]:=OAuthClient`instagramcookeddata[args]
oauthsendmessage["Instagram",args___]:=OAuthClient`instagramsendmessage[args]
*)
(* ------------- BodyMedia ------------------------ *)
oauthservicedata["BodyMedia"]={
		"OAuthVersion"		->"1.0a",
		"ServiceName" 		-> "BodyMedia", 
		"RequestEndpoint" 	-> "https://api.bodymedia.com/oauth/request_token",
	  	"AccessEndpoint" 	-> "https://api.bodymedia.com/oauth/access_token", 
	 	"AuthorizeEndpoint" -> "https://api.bodymedia.com/oauth/authorize", 
	 	"ClientInfo"		-> {40, 42, 33, 49, 78, 33, 42, 41, 53, 52, 42, 13, 10, 102, 118, 119, 
			109, 53, 109, 89, 194, 160, 82, 82, 119, 107, 90, 91, 96, 115, 96, 
			112, 71, 97, 96, 96, 88, 91, 89, 67, 114, 91, 79, 116, 94, 76, 43, 
			44, 92, 78, 194, 169, 54, 116, 68, 77, 122, 46, 123, 96, 55, 48, 110, 
			116, 83, 194, 173, 52, 49, 59, 50, 97, 113, 125, 126, 194, 162, 102, 
			111, 118, 89, 194, 163, 52, 56, 118, 120, 73, 13, 10, 51, 114, 84, 
			98, 105, 41, 61, 108, 122, 36, 41, 63, 91, 194, 161, 82, 118, 119, 
			68, 78, 49, 194, 168, 97, 109, 194, 165, 89, 50, 56, 194, 184, 63, 
			73, 59, 51, 75, 89, 48, 194, 174, 95, 33, 40, 38, 89, 73, 72, 48, 
			106, 126, 32, 63, 73, 45, 93, 110, 72, 36, 101, 53, 121, 91, 34, 35, 
			13, 10},
	 	"AdditionalOAuthParameter" -> ("api_key" -> "qt5cqdhbjxvmbbs6vnm9kcwpxwu77px5"),
	 	"AuthenticationDialog" -> "TokenlessDialog",
	 	"Gets"				-> {"UserDataGrid","StepsGrid"},
	 	"Posts"				-> {},
 		"RawGets"			-> {"RawUserData","RawSteps"},
 		"RawPosts"			-> {},
 		"Information"		-> "A service for accessing data from a BodyMedia account"
	
}

twitterimport[json_]:=With[{res=ImportString[json,"JSON"]},
	If[FreeQ[res,_["errors",_]],
		assoc@res,
		Message[ServiceExecute::apierr,"message"/.("errors"/.res)];
		Throw[$Failed]
	]
]

(* Raw *)
oauthservicedata["BodyMedia","RawUserData"]={
    "HTTPSMethod"		-> "GET",
	"URL"				-> ("http://api.bodymedia.com/v2/json/user/info?api_key=qt5cqdhbjxvmbbs6vnm9kcwpxwu77px5"&),
    "ResultsFunction"	-> twitterimport
    
}

oauthservicedata["BodyMedia","RawSteps"]={
    "HTTPSMethod"		-> "GET",
    "PathParameters"	-> {"Date"},
   	"URL"				-> (ToString@StringForm["http://api.bodymedia.com/v2/json/step/day/`1`?api_key=qt5cqdhbjxvmbbs6vnm9kcwpxwu77px5", formatDate[#1,"BodyMedia"]]&),
    "ResultsFunction"	-> twitterimport
}

(* Cooked *)
               
oauthcookeddata["BodyMedia","UserDataGrid",id_,args___]:=Module[
	{rawdata},
	rawdata=OAuthClientDump`rawoauthdata[id,"RawUserData",args];
	With[{data=(ImportString[rawdata,"JSON"]/.createquantity[{"height"->"Inches","weight"->"Pounds"}])},
    	prettygrid[Transpose[{{"Property","Armband Serial Number","Birthday","Gender","Height","Weight"},
    		{"Value","serialNumber"/.("armband"/.data),"birthday"/.(data/.formatvalue["birthday"->(readDate[#,"BodyMedia"]&)]),
    		"gender"/.data,"height"/.data,"weight"/.data
    	}}]]
    ]
]  

               
oauthcookeddata["BodyMedia","RawStepsGrid",id_,args___]:=Module[
	{rawdata},
	rawdata=OAuthClientDump`rawoauthdata[id,"Steps",args];
	prettygrid[Transpose[{{"Parameter","Date","Steps"},
    	{"Value","date","totalSteps"}/.First[
    		"days"/.(ImportString[rawdata,"JSON"]/.formatvalue["date"->(readDate[#,"BodyMedia"]&)])]}]]
]  

(* ------------- Twitter ------------------------ *)
oauthservicedata["Twitter"]={
		"OAuthVersion"		->"1.0a",
		"ServiceName" 		-> "Twitter", 
		"RequestEndpoint" 	-> "https://api.twitter.com/oauth/request_token",
	  	"AccessEndpoint" 	-> "https://api.twitter.com/oauth/access_token", 
	 	"AuthorizeEndpoint" -> "https://api.twitter.com/oauth/authorize", 
	 	"ClientInfo"		-> {40, 42, 33, 49, 78, 33, 42, 41, 53, 52, 42, 13, 10, 102, 118, 111, 
			101, 61, 92, 118, 107, 68, 63, 56, 70, 82, 115, 54, 49, 121, 55, 97, 
			194, 160, 49, 57, 61, 87, 54, 124, 109, 71, 45, 90, 103, 54, 88, 194, 
			166, 45, 39, 194, 167, 91, 89, 87, 60, 52, 194, 162, 120, 113, 46, 
			98, 54, 84, 54, 194, 174, 113, 109, 61, 53, 100, 79, 44, 87, 121, 88, 
			45, 68, 92, 119, 48, 68, 79, 120, 34, 13, 10, 194, 163, 126, 107, 34, 
			118, 194, 166, 64, 63, 194, 161, 194, 160, 72, 83, 121, 52, 73, 97, 
			68, 69, 109, 119, 111, 78, 93, 92, 13, 10},
	 	"AuthenticationDialog" -> "TokenDialog",
	 	"Gets"				->{"FollowerIDList","FollowerIDGrid","GetTweet","TimelineGrid","MyLastTweet"},
	 	"Posts"				->{"Tweet","ImageTweet"},
	 	"RawGets"			->	{"FollowerIDs","RawStatuses","StatusesUserTimeline"},
	 	"RawPosts"			->	{"Update","RawMediaUpload"},
	 	"LogoutURL"			-> "https://twitter.com/logout",
 		"Information"		-> "A service for sending and receiving tweets from a Twitter account"
}
   
twitterimport[json_]:=With[{res=ImportString[json,"JSON"]},
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

(* Raw *)
oauthservicedata["Twitter","FollowerIDs"] = {
        "URL"				-> "https://api.twitter.com/1.1/followers/ids.json",
        "Parameters" 		-> {},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> twitterimport
    }
        
oauthservicedata["Twitter","RawStatuses"] = {
     	"URL"				-> "https://api.twitter.com/1.1/statuses/show.json",
        "Parameters" 		-> {"id"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> twitterimport
    }  
    
        
oauthservicedata["Twitter","StatusesUserTimeline"] = {
        "URL"				-> "https://api.twitter.com/1.1/statuses/user_timeline.json",
        "Parameters" 		-> {},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> twitterimport
    }    
       
oauthservicedata["Twitter","Update"] = {
        "URL"				-> "https://api.twitter.com/1.1/statuses/update.json",
        "Parameters" 		-> {},
        "BodyData"			-> {"status"},
        "HTTPSMethod"		-> "POST",
        "ResultsFunction"	-> twitterimport
    }  

          
oauthservicedata["Twitter","RawMediaUpload"] = {
        "URL"				-> "https://api.twitter.com/1.1/statuses/update_with_media.json",
        "Parameters" 		-> {},
        "BodyData"			-> {},
        "MultipartData"		-> {{"media[]","image/jpeg"},{"status","text/plain"}},
        "HTTPSMethod"		-> "POST",
        "ResultsFunction"	-> twitterimport
    }  
 
(* Cooked *)        
oauthcookeddata["Twitter","FollowerIDList",id_,args___]:=Module[
	{rawdata},
	rawdata=OAuthClientDump`rawoauthdata[id,"FollowerIDs",args];
	twitterimport[rawdata]["ids"]
]         

oauthcookeddata["Twitter","FollowerIDGrid",id_,args___]:=Module[
	{rawdata,ids},
	rawdata=OAuthClientDump`rawoauthdata[id,"FollowerIDs",args];
	rawdata=twitterimport[rawdata];
	ids=If[KeyExistsQ[rawdata,"ids"],rawdata["ids"],{}];
	prettygrid[Join[{{"IDs"}},{ids}]]
]   
    
oauthcookeddata["Twitter","GetTweet",id_,args___]:=Module[
	{rawdata},
	rawdata=OAuthClientDump`rawoauthdata[id,"RawStatuses",Flatten[{args}/."TweetID"->"id"]];
	twitterimport[rawdata]["text"]
]         
     
oauthcookeddata["Twitter","TimelineGrid",id_,args___]:=Module[
	{rawdata},
	rawdata=OAuthClientDump`rawoauthdata[id,"StatusesUserTimeline",Flatten[{args}/."TweetID"->"id"]];
	prettygrid[Join[{{"Time","Message","Tweet Id"}},
		{"created_at", "text","id_str"}/.((Normal/@twitterimport[rawdata])/.formatvalue["created_at"->(readDate[#,"Twitter"]&)])],ItemStyle -> {Automatic, {Directive[Bold, 18]}}]
]   
        
oauthcookeddata["Twitter","MyLastTweet",id_,args___]:=Module[
	{rawdata},
	rawdata=OAuthClientDump`rawoauthdata[id,"StatusesUserTimeline",Flatten[{args}/."TweetID"->"id"]];
	twitterimport[rawdata]["text"]
]   

oauthcookeddata["Twitter","Tweet",id_,args___]:=Module[
	{rawdata},
	rawdata=OAuthClientDump`rawoauthdata[id,"Update",Flatten[{args}/."Tweet"->"status"]];
	twitterimport[rawdata]["text"]
]    
    
oauthcookeddata["Twitter","ImageTweet",id_,args___]:=Module[
	{rawdata, status,media,statusBytes,mediaBytes},
	{status,media}=Switch[{args},
		{_Rule..}|{{_Rule..}},
			{("Status"/.Flatten[{args}])/."Status"->"",
				("Image"/.Flatten[{args}])/."Image":>(Message[ServiceExecute::nparam,"Image"];Throw[$Failed])},
		{_},{"",args},
		_,(Message[ServiceExecute::nparam,"Image"];Throw[$Failed])
	];
	statusBytes=Switch[status,
		{_?IntegerQ...},status,
		_String,ImportString[status, "Byte"],
		_,ImportString[ToString[status], "Byte"]
	];
	
	mediaBytes=Which[
		MatchQ[media,{_?IntegerQ...}],media,
		TrueQ[Quiet[FileExistsQ[media],FileExistsQ::fstr]],Import[media, "Byte"],
		ImageQ[media],ImportString[ToString[media], "Byte"],
		True,
			Check[ImportString[ExportString[media, "JPG"], "Byte"],Throw[$Failed]]
	];
	
	rawdata=OAuthClientDump`rawoauthdata[id,"RawMediaUpload","media[]"->mediaBytes,"status"->statusBytes];
	debugPrint["rawdata"->rawdata];
	twitterimport[rawdata]["text"]
]   
     
(* Send Message *)
oauthsendmessage["Twitter",id_,message_]:=oauthcookeddata["Twitter","Tweet",id,"Tweet"->message]

(* ------------- FitBit ------------------------ *)
oauthservicedata["FitBit"] = {
        "ServiceName"       -> "FitBit",
        "OAuthVersion"		-> "1.0a",
        "RequestEndpoint"   -> "http://api.fitbit.com/oauth/request_token",
        "AccessEndpoint"    -> "http://api.fitbit.com/oauth/access_token",
        "AuthorizeEndpoint" -> "http://www.fitbit.com/oauth/authorize",
        "VerifierLabel"		->	"security code",
        "ClientInfo"		-> {40, 42, 33, 49, 78, 33, 42, 41, 53, 52, 42, 13, 10, 102, 118, 194, 
			163, 111, 37, 99, 67, 194, 180, 119, 61, 77, 112, 97, 59, 125, 82, 
			47, 95, 51, 110, 47, 93, 35, 89, 38, 111, 110, 107, 48, 63, 95, 79, 
			70, 125, 76, 34, 108, 42, 113, 77, 94, 94, 121, 46, 124, 36, 69, 105, 
			55, 110, 44, 123, 97, 117, 126, 32, 125, 124, 78, 51, 84, 59, 41, 87, 
			107, 104, 49, 56, 43, 78, 13, 10, 194, 164, 63, 83, 113, 72, 80, 13, 
			10},
        "AuthenticationDialog" -> "TokenDialog",
 		"Gets"				-> Join[{"UserDataGrid", "WeightGrid", "MeasurementsGrid", "FoodGrid", 
 			"ActivityGrid", "TimeSeriesActivityScores", "TimeSeriesActivityCalories", "SleepGrid", "SleepList","SleepCalendar","SleepDensityTimeline"},fitbittimeseriesproperties,fitbittimeseriesPlots],
 		"Posts"				-> {"RecordWeight"},
 		"RawGets"			->	Join[{"UserData", "Weight","BodyFat","Measurements","Food","Water",
 			"Activity","ActivityScores","AllActivityScores","AllActivityCalories","Sleep","FoodUnit"},
 			rawfitbittimeseriesproperties],
 		"RawPosts"			->	{"LogWeight","LogBodyFat","LogMeasurement","LogFood"},
    	"RequestFormat"		-> "Headers",
 		"Information"		-> "A service for sending and receiving data from a FitBit account"
    }
 
(* Raw *)   
   
fitbitimport[json_]:=With[{res=ImportString[json,"JSON"]},
	If[FreeQ[res,_["errors",_]],
		assoc@res,
		Message[ServiceExecute::apierr,"message"/.("errors"/.res)];
		Throw[$Failed]
	]
]
          
oauthservicedata["FitBit","UserData"] = {
        "URL"				->	"http://api.fitbit.com/1/user/-/profile.json",
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> fitbitimport
    }
 
oauthservicedata["FitBit","Measurements"] = {
        "URL"				->	(ToString@StringForm["http://api.fitbit.com/1/user/-/body/date/`1`.json", formatDate[#1,"FitBit"]]&),
        "PathParameters" 	-> {"Date"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> fitbitimport
    }
       
oauthservicedata["FitBit","Weight"] = {
       	"Headers" 			-> {"Accept-Language"->"en_US"},  (* Can be "en_US", "en_GB", or "" *)
        "URL"				->	(ToString@
        	StringForm["http://api.fitbit.com/1/user/-/body/log/weight/date/`1`.json", formatDate[##,"FitBit"]]&),
        "PathParameters" 	-> {"Date","StartDate","EndDate"},
        "OptionalParameters" 	-> {"Date","StartDate","EndDate"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> fitbitimport
    }

oauthservicedata["FitBit","BodyFat"] = {
        "URL"				->	(ToString@StringForm["http://api.fitbit.com/1/user/-/body/log/fat/date/`1`.json", formatDate[##,"FitBit"]]&),
        "PathParameters" 	-> {"Date","StartDate","EndDate"},
        "OptionalParameters" 	-> {"Date","StartDate","EndDate"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> fitbitimport
    }
           
oauthservicedata["FitBit","Food"] = {
        "URL"				->	(ToString@StringForm["http://api.fitbit.com/1/user/-/foods/log/date/`1`.json", formatDate[##,"FitBit"]]&),
        "HTTPSMethod"		-> "GET",
        "PathParameters" 	-> {"Date","StartDate","EndDate"},
        "OptionalParameters" 	-> {"Date","StartDate","EndDate"},
        "ResultsFunction"	-> fitbitimport
    }
           
oauthservicedata["FitBit","Water"] = {
        "URL"				->	(ToString@StringForm["http://api.fitbit.com/1/user/-/foods/log/water/date/`1`.json", formatDate[##,"FitBit"]]&),
        "HTTPSMethod"		-> "GET",
         "PathParameters" 	-> "Date",
         "OptionalParameters" 	-> "Date",
        "ResultsFunction"	-> fitbitimport
    }
    
oauthservicedata["FitBit","Activity"] = {
        "URL"				->	(ToString@StringForm["http://api.fitbit.com/1/user/-/activities/date/`1`.json", formatDate[#1,"FitBit"]]&),
        "PathParameters" 	-> {"Date"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> fitbitimport
    }
    
oauthservicedata["FitBit","AllActivityScores"] = {
        "URL"				->	(ToString@StringForm["http://api.fitbit.com/1/user/-/activities/activeScore/date/`1`/max.json", formatDate[#1,"FitBit"]]&),
        "PathParameters" 	-> {"Date"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> fitbitimport
    } 
     
oauthservicedata["FitBit","ActivityScores"] = {
        "URL"				->	(ToString@StringForm["http://api.fitbit.com/1/user/-/activities/activeScore/date/`1`/`2`.json", formatDate[#1,"FitBit"],formatDate[#2,"FitBit"]]&),
        "PathParameters" 	-> {"StartDate", "EndDate"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> fitbitimport
    } 
      
oauthservicedata["FitBit","AllActivityCalories"] = {
        "URL"				->	(ToString@StringForm["http://api.fitbit.com/1/user/-/activities/activityCalories/date/`1`/max.json", formatDate[#1,"FitBit"]]&),
        "PathParameters" 	-> {"Date"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> fitbitimport
    } 
        
oauthservicedata["FitBit","Sleep"] = {
        "URL"				->	(ToString@StringForm["http://api.fitbit.com/1/user/-/sleep/date/`1`.json", formatDate[#1,"FitBit"]]&),
        "PathParameters" 	-> {"Date"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	->fitbitimport
    } 
           
oauthservicedata["FitBit","FoodUnit"] = {
        "URL"				->	"http://api.fitbit.com/1/foods/units.json",
        "PathParameters" 	-> {},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	->fitbitimport
    } 
        
oauthservicedata["FitBit","LogFood"] = {
        "URL"				->	"http://api.fitbit.com/1/user/-/foods/log.json",
        "BodyData"			-> {"foodID","foodName","calories","brandName","mealTypeId","unitId","amount","date"},
        "OptionalParameters"	-> {"foodID","foodName","calories","brandName"},
        "HTTPSMethod"		-> "POST",
        "ResultsFunction"	->fitbitimport
    } 
                   
oauthservicedata["FitBit","LogWeight"] = {
        "URL"				->	"http://api.fitbit.com/1/user/-/body/log/weight.json",
       	"Headers" 			-> {"Accept-Language"->"en_US"},  (* Can be "en_US", "en_GB", or "" *)
        "BodyData"			-> {"weight","date"},
        "OptionalParameters"	-> {},
        "HTTPSMethod"		-> "POST",
        "ResultsFunction"	->fitbitimport
    } 
             
oauthservicedata["FitBit","LogBodyFat"] = {
        "URL"				->	"http://api.fitbit.com/1/user/-/body/log/fat.json",
        "BodyData"			-> {"fat","date"},
        "OptionalParameters"	-> {},
        "HTTPSMethod"		-> "POST",
        "ResultsFunction"	->fitbitimport
    } 
    
oauthservicedata["FitBit","LogMeasurement"] = {
        "URL"				->	"http://api.fitbit.com/1/user/-/body.json",
       	"Headers" 			-> {"Accept-Language"->"en_US"},
        "BodyData"			-> {"bicep","weight","date"},
        "OptionalParameters"	-> {"bicep","weight"},
        "HTTPSMethod"		-> "POST",
        "ResultsFunction"	->fitbitimport
    } 
    
oauthservicedata["FitBit","LogMeasurement"] = {
        "URL"				->	"http://api.fitbit.com/1/user/-/body.json",
       	"Headers" 			-> {"Accept-Language"->"en_US"},
        "BodyData"			-> {"bicep","weight","date"},
        "OptionalParameters"	-> {"bicep","weight"},
        "HTTPSMethod"		-> "POST",
        "ResultsFunction"	->fitbitimport
    } 
    
    
fitbittimeseriesproperties={"CaloriesInTimeSeries", "WaterTimeSeries", "CaloriesTimeSeries", 
   "CaloriesBMRTimeSeries", "StepsTimeSeries", "DistanceTimeSeries", 
   "FloorsTimeSeries", "ElevationTimeSeries", 
   "MinutesSedentaryTimeSeries", "MinutesLightlyActiveTimeSeries", 
   "MinutesFairlyActiveTimeSeries", "MinutesVeryActiveTimeSeries", 
   "ActivityCaloriesTimeSeries", "CaloriesTimeSeries", "StepsTimeSeries", 
   "DistanceTimeSeries", "MinutesSedentaryTimeSeries", 
   "MinutesLightlyActiveTimeSeries", "MinutesFairlyActiveTimeSeries", 
   "MinutesVeryActiveTimeSeries", "ActivityCaloriesTimeSeries", 
   "StartTimeTimeSeries", "TimeInBedTimeSeries", 
   "MinutesAsleepTimeSeries", "AwakeningsCountTimeSeries", 
   "MinutesAwakeTimeSeries", "MinutesToFallAsleepTimeSeries", 
   "MinutesAfterWakeupTimeSeries", "EfficiencyTimeSeries", 
   "WeightTimeSeries", "BmiTimeSeries", "FatTimeSeries"}
   
fitbittimeseriespaths={"foods/log/caloriesIn", "foods/log/water", "activities/calories", 
   "activities/caloriesBMR", "activities/steps", "activities/distance", 
   "activities/floors", "activities/elevation", 
   "activities/minutesSedentary", "activities/minutesLightlyActive", 
   "activities/minutesFairlyActive", "activities/minutesVeryActive", 
   "activities/activityCalories", "activities/tracker/calories", 
   "activities/tracker/steps", "activities/tracker/distance", 
  (*  "activities/tracker/floors", "activities/tracker/elevation",  *)
   "activities/tracker/minutesSedentary", 
   "activities/tracker/minutesLightlyActive", 
   "activities/tracker/minutesFairlyActive", 
   "activities/tracker/minutesVeryActive", 
   "activities/tracker/activityCalories", "sleep/startTime", 
   "sleep/timeInBed", "sleep/minutesAsleep", "sleep/awakeningsCount", 
   "sleep/minutesAwake", "sleep/minutesToFallAsleep", 
   "sleep/minutesAfterWakeup", "sleep/efficiency", "body/weight", 
   "body/bmi", "body/fat"};

rawfitbittimeseriesproperties=("Raw"<>#&/@fitbittimeseriesproperties)

fitbittimeseriesPlots=StringReplace[fitbittimeseriesproperties, "TimeSeries" -> "Plot"];

oauthservicedata["FitBit",prop:(Alternatives@@rawfitbittimeseriesproperties)] := {
        "URL"				->	(ToString@StringForm["http://api.fitbit.com/1/user/-/"<>
        	(prop/.Thread[rawfitbittimeseriesproperties->fitbittimeseriespaths])<>"/date/`1`/`2`.json", formatDate[#1,"FitBit"],formatDate[#2,"FitBit"]]&),
        "PathParameters" 	-> {"StartDate", "EndDate"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	->fitbitimport
    } 
(* Fit Bit TimeSeries endpoints 
foods/log/caloriesIn    
foods/log/water
activities/calories
activities/caloriesBMR
activities/steps
activities/distance
activities/floors
activities/elevation
activities/minutesSedentary
activities/minutesLightlyActive    
activities/minutesFairlyActive
activities/minutesVeryActive
activities/activityCalories
activities/tracker/calories
activities/tracker/steps
activities/tracker/distance
activities/tracker/floors
activities/tracker/elevation
activities/tracker/minutesSedentary
activities/tracker/minutesLightlyActive    
activities/tracker/minutesFairlyActive
activities/tracker/minutesVeryActive
activities/tracker/activityCalories   
sleep/startTime
sleep/timeInBed
sleep/minutesAsleep
sleep/awakeningsCount    
sleep/minutesAwake
sleep/minutesToFallAsleep
sleep/minutesAfterWakeup
sleep/efficiency
body/weight    
body/bmi
body/fat
*)

(* Cooked *)
oauthcookeddata["FitBit",prop:(Alternatives@@fitbittimeseriesproperties),id_,args___] :=Module[
	{rawdata, startDate, endDate, opts},
	{startDate, endDate} = Switch[{args},
		{_Rule..},opts=DeleteCases[{args},Rule[("StartDate"|"EndDate"),_]];{"StartDate", "EndDate"} /. Flatten[{args}],
		{{_,_},___},opts=Rest[{args}];First[{args}],
		{_,__},opts=Drop[{args},2];args[[1;;2]],
		_,Throw[$Failed]
	];
	If[DateDifference[startDate, endDate] < 0, Throw[$Failed]];
	rawdata=OAuthClientDump`rawoauthdata[id,"Raw"<>prop,
		"StartDate"->formatDate[startDate,"FitBit"],"EndDate"->formatDate[endDate,"FitBit"]];
	rawdata=Normal@fitbitimport[rawdata];
	rawdata={"dateTime","value"}/.((rawdata[[1,-1]]/.{("value"->val_):>("value"->ToExpression[val]),
		("dateTime"->date_):>("dateTime"->readDate[date,"FitBit",AbsoluteTime])}));
	TimeSeries[rawdata]
]

oauthcookeddata["FitBit",prop:(Alternatives@@fitbittimeseriesPlots),id_,args___] :=Module[
	{rawdata, startDate, endDate, opts},
	{startDate, endDate} = Switch[{args},
		{_Rule..},opts=DeleteCases[{args},Rule[("StartDate"|"EndDate"),_]];{"StartDate", "EndDate"} /. Flatten[{args}],
		{{_,_},___},opts=Rest[{args}];First[{args}],
		{_,__},opts=Drop[{args},2];args[[1;;2]],
		_,Throw[$Failed]
	];
	If[DateDifference[startDate, endDate] < 0, Throw[$Failed]];
	rawdata=OAuthClientDump`rawoauthdata[id,(prop/.Thread[fitbittimeseriesPlots->rawfitbittimeseriesproperties]),
		"StartDate"->formatDate[startDate,"FitBit"],"EndDate"->formatDate[endDate,"FitBit"]];
	rawdata=Normal@fitbitimport[rawdata];
	rawdata={"dateTime","value"}/.((rawdata[[1,-1]]/.{("value"->val_):>("value"->ToExpression[val]),
		("dateTime"->date_):>("dateTime"->readDate[date,"FitBit",AbsoluteTime])}));
	DateListPlot[rawdata, Sequence@@opts, Filling->Axis]
]

fitbitsleeplist[id_,args___]:=Module[
	{rawdata, startDate, endDate, ndays, n, day},
	{startDate, endDate} = {"StartDate", "EndDate"} /. Flatten[{args}];
	If[TrueQ[DateDifference[startDate, endDate] < 0], Throw[$Failed]];
	ndays = First[DateDifference[startDate, endDate, "Day"]];
	rawdata = Table[day = DatePlus[startDate, n];
 		OAuthClientDump`rawoauthdata[id, "Sleep", "Date" -> day], {n, 0, ndays}];
	If[rawdata === {}, Return[{}]];
	rawdata=DeleteCases[(Normal@fitbitimport[#]&)/@rawdata,FreeQ[#,"sleep"]&];
	"sleep" /. rawdata
]

oauthcookeddata["FitBit","SleepList",id_,args___]:=Module[
	{rawdata},
	rawdata=fitbitsleeplist[id, args];
	Map[Cases[#, _[
    "awakeningsCount" | "duration" | "efficiency" | 
     "minutesAfterWakeup" | "minutesAsleep" | "minutesAwake" | 
     "minutesToFallAsleep" | "startTime" | 
     "timeInBed", _], {1}] &, rawdata, {2}]/.{formatvalue["startTime"->(readDate[#,"FitBit"]&)],
     	createquantity["minutesAfterWakeup"->"Minutes"],createquantity["minutesAsleep"->"Minutes"],
     	createquantity["minutesAwake"->"Minutes"],createquantity["minutesToFallAsleep"->"Minutes"]
     	,createquantity["timeInBed"->"Minutes"],createquantity["duration"->"Seconds"]}
     
]/;parameterspresentQ[{args},{"StartDate","EndDate"}]
         
oauthcookeddata["FitBit","SleepCalendar",id_,args___]:=Module[
	{data, starts, durations, effs,minEfficiency,maxEfficiency,ndays, t0, i},
	data=fitbitsleeplist[id, args];
	debugPrint["SleepCalendar data"->data];
	If[Flatten[data]==={},Return[Graphics[]]];
	data=Cases[data,_?(! FreeQ[#, "startTime"] &)];
	starts = Check[readDate[#, "FitBit",Identity] & /@ 
   		Flatten[("startTime" /. data)],Throw[$Failed]];
	durations = Select[ToExpression[Flatten["timeInBed" /. data]], NumberQ];
	effs = Select[ToExpression[Flatten["efficiency" /. data]], NumberQ];
	minEfficiency = Min[effs];
	maxEfficiency = Max[effs];
	ndays = Ceiling[First[DateDifference["StartDate"/. Flatten[{args}], "EndDate"/. Flatten[{args}], "Day"]]];
	data=Transpose[{starts,durations, effs}];
	t0=Min[AbsoluteTime/@starts];
	Graphics[
		makeSleepBox[#, minEfficiency, maxEfficiency, t0]&/@data,
		AspectRatio -> 1/GoldenRatio,
		Axes -> True,
		AxesOrigin->{0,0},
		Ticks -> {Range[-2, 24], Table[{i+.5,DateString[DatePlus[t0,{i,"Day"}], {"MonthNameShort", " ", "Day", " ", "Year"}]},{i,0,ndays-1}]}
	]
]/;parameterspresentQ[{args},{"StartDate","EndDate"}]      

         
oauthcookeddata["FitBit","SleepDensityTimeline",id_,args___]:=Module[
	{data, starts, durations, effs,minEfficiency,maxEfficiency,ndays, t0},
	data=fitbitsleeplist[id, args];
	starts = readDate[#, "FitBit",Identity] & /@ 
   		Flatten[("startTime" /. data)];
	durations = Select[ToExpression[Flatten["timeInBed" /. data]], NumberQ];
	effs = Select[ToExpression[Flatten["efficiency" /. data]], NumberQ];
	minEfficiency = Min[effs];
	maxEfficiency = Max[effs];
	ndays = Ceiling[First[DateDifference["StartDate"/. Flatten[{args}], "EndDate"/. Flatten[{args}], "Day"]]];
	data=Transpose[{starts,durations, effs}];
	t0=Min[AbsoluteTime/@starts];
	Graphics[
		makeSleepBox[#, minEfficiency, maxEfficiency, #[[1,1;;3]]]&/@data,
		Axes -> True,
		AxesOrigin->{0,0},
		Ticks -> {Range[-2, 24], {1}}
	]
]/;parameterspresentQ[{args},{"StartDate","EndDate"}]      


oauthcookeddata["FitBit","UserDataGrid",id_,args___]:=Module[
	{rawdata},
	rawdata=OAuthClientDump`rawoauthdata[id,"UserData",args];
	FlatGrid[{{"user",{"city", "country", "dateOfBirth", "displayName", "gender", "height", "locale", 
        	"memberSince", "state", "strideLengthRunning","strideLengthWalking", "timezone", "weight"}}},rawdata]
]
   
oauthcookeddata["FitBit","WeightGrid",id_,args___]:=Module[
	{rawdata},
	rawdata=OAuthClientDump`rawoauthdata[id,"Weight",args];
	FlatGrid[{{"weight",{"bmi","date","time","weight"}}},rawdata]
]    

oauthcookeddata["FitBit","MeasurementsGrid",id_,args___]:=Module[
	{rawdata},
	rawdata=OAuthClientDump`rawoauthdata[id,"Measurements",args];
	FlatGrid[{{"body",{"bicep", "bmi", "calf", "chest", "fat", 
		"forearm", "hips", "neck", "thigh", "waist", "weight"}}},rawdata]
]    
    
oauthcookeddata["FitBit","FoodGrid",id_,args___]:=Module[
	{rawdata},
	rawdata=OAuthClientDump`rawoauthdata[id,"Food",args];
	GridList[{{"foods",{{"loggedFood",{"amount", "brand", "foodId", "mealTypeId", "name"}},
        	{"nutritionalValues",{"calories", "carbs", "fat", "fiber", "protein", "sodium"}}}}},rawdata]
]   


oauthcookeddata["FitBit","ActivityGrid",id_,args___]:=Module[
	{rawdata},
	rawdata=OAuthClientDump`rawoauthdata[id,"Activity",args];
	FlatGrid[{{"summary",{{"loggedFood",{"amount", "brand", "foodId", "mealTypeId", "name"}},
        	{"nutritionalValues",{"calories", "carbs", "fat", "fiber", "protein", "sodium"}}}}},rawdata]
]   

oauthcookeddata["FitBit","TimeSeriesActivityScores",id_,args___]:=Module[
	{rawdata},
	Switch[Length[{args}],
		1,
		rawdata=OAuthClientDump`rawoauthdata[id,"AllActivityScores",args],
		2,
		rawdata=OAuthClientDump`rawoauthdata[id,"ActivityScores",args]
	];
	TemporalData[{"dateTime", "value"} /. ("activities-activeScore" /. (
        	ImportString[rawdata, "JSON"]/.{formatvalue["dateTime"->DateList],formatvalue["value"->ToExpression]}))]
]   
    
oauthcookeddata["FitBit","TimeSeriesActivityCalories",id_,args___]:=Module[
	{rawdata},
	rawdata=OAuthClientDump`rawoauthdata[id,"AllActivityCalories",args];
	TemporalData[{"dateTime", "value"} /. ("activities-activityCalories" /. (
        	ImportString[rawdata, "JSON"]/.{formatvalue["dateTime"->DateList],formatvalue["value"->ToExpression]}))]
]   
 
oauthcookeddata["FitBit","SleepGrid",id_,args___]:=Module[
	{rawdata},
	rawdata=OAuthClientDump`rawoauthdata[id,"Sleep",args];
	addtitles[Replace[FlatGrid[{{"sleep",
        	{"efficiency","startTime","duration","minutesAsleep","minutesAwake","minutesAfterWakeup","awakeningsCount","timeInBed"}},
        	{"summary",{"totalSleepRecords","totalMinutesAsleep","totalTimeInBed"}}},rawdata,
        	{formatvalue["startTime"->(readDate[#,"FitBit"]&)],createquantity["minutesAsleep"->"Minutes"],
        	createquantity["minutesAwake"->"Minutes"],createquantity["minutesAfterWakeup"->"Minutes"],createquantity["timeInBed"->"Minutes"]}],{a_} -> a, {3}],
        	{"Parameter","Value"}]
]    
 
 (*
oauthcookeddata["FitBit","WeightPlot",id_,args___]:=Module[
	{rawdata, start, end, max},
	
	{start, end}=Switch[{args},
		{_Rule..},
			If[FreeQ[{args},"StartDate"],Message[ServiceExecute::nparam,"StartDate"];Throw[$Failed]];
			If[FreeQ[{args},"EndDate"],Message[ServiceExecute::nparam,"EndDate"];Throw[$Failed]];
			Flatten[{"StartDate","EndDate"}/.{args}],
		{_,_}|{{_,_}},
			Flatten[{args}],
		_,Throw[$Failed]
		
	];
	rawdata=OAuthClientDump`rawoauthdata[id,"Weight","StartDate"->formatDate[start,"FitBit"],"EndDate"->formatDate[end,"FitBit"]];
	rawdata={"date","weight"}/.(("weight"/.(fitbitimport[rawdata])/.{("weight"->weight_):>("weight"->ToExpression[weight]),
		("date"->date_):>("date"->readDate[date,"FitBit",AbsoluteTime])}));
	(* max=Max[rawdata[[All,2]]]; *)
	DateListPlot[rawdata, Filling->Axis (*,PlotRange->{Automatic, {0,1.1 max}}*)]
]  
	*)
oauthcookeddata["FitBit","RecordWeight",id_,args___]:=Module[
	{rawdata, args1},
	args1=Switch[{args},
		_?OptionQ,{args},
		{{_,_}},{"weight"->ToString@First[args],"date"->formatDate[args[[2]],"FitBit"]},
		{_},{"weight"->ToString[args],"date"->formatDate[DateString[],"FitBit"]},
		{_,_},{"weight"->ToString@First[{args}],"date"->formatDate[{args}[[2]],"FitBit"]}
	];
	rawdata=OAuthClientDump`rawoauthdata[id,"LogWeight",Sequence@@args1];
	"weight"/.("weightLog"/.fitbitimport[rawdata])
]    

makeSleepBox[data_, minEfficiency_, maxEfficiency_, t0_] :=
	Module[{day, duration, start, efficiency, xmin, xmax, end},
		day = data[[1,1;;3]];
		start = data[[1]];
		xmin = First@DateDifference[day, start, "Hour"];
		duration = data[[2]];
		end=DatePlus[start,{duration,"Minute"}];
		debugPrint["end"->end];
		xmax = First@DateDifference[day, end, "Hour"];
		efficiency = Rescale[data[[3]], {maxEfficiency, minEfficiency}];
		day=(AbsoluteTime[day]-t0)/86400;
		{
			Opacity[0.95],
			ColorData["TemperatureMap"][efficiency],
			Tooltip[
				If[xmax>24,
					If[xmax>48, Throw[$Failed]];
					{Rectangle[{0, day+1.95}, {xmax-24, day + 1.05}],
					Rectangle[{xmin, day+.95}, {24, day + 0.05}]}
					,
					Rectangle[{xmin, day+.95}, {xmax, day + 0.05}]
				],
				StringJoin[Riffle[ToString /@ DateDifference[start, end, "Hour"], " "], "s"]
			]
		}
	]
(* ------------- Runkeeper ------------------------ *)
     
oauthservicedata["Runkeeper"] = {
     	"ServiceName"       -> "Runkeeper",
     	"OAuthVersion"		 -> "2.0",
     	"AuthorizeEndpoint" -> "https://runkeeper.com/apps/authorize",
     	"AccessEndpoint"    -> "https://runkeeper.com/apps/token",
     	"RedirectURI"       -> "http://www.wolfram.com/socialmediadata/runkeeper",
     	"VerifierLabel"     -> "code",
     	"ClientInfo"		-> {40, 42, 33, 49, 78, 33, 42, 41, 53, 52, 42, 13, 10, 102, 118, 194, 
			161, 87, 55, 122, 94, 47, 80, 32, 54, 43, 39, 105, 124, 61, 99, 35, 
			94, 122, 194, 162, 120, 76, 57, 68, 75, 49, 114, 74, 33, 112, 121, 
			70, 54, 76, 34, 53, 81, 100, 34, 121, 94, 35, 97, 103, 95, 95, 109, 
			99, 94, 54, 72, 97, 94, 112, 59, 84, 68, 34, 51, 57, 117, 80, 54, 49, 
			39, 63, 126, 99, 53, 13, 10, 62, 112, 106, 98, 72, 122, 13, 10},
     	"AuthenticationDialog" -> "TokenDialog",
 		"Gets"				-> {"UserDataGrid","FitnessActivitiesGrid","FitnessActivityGrid","PathGrid","GoogleMap"},
 		"Posts"				-> {},
 		"RawGets"			-> {"UserData","FitnessActivities","FitnessActivity"},
 		"RawPosts"			-> {},
 		"Information"		-> "A service for accessing data from a Runkeeper account"
    }
    

runkeeperimport[json_]:=With[{res=ImportString[json,"JSON"]},
	If[FreeQ[res,_["errors",_]],
		assoc@res,
		Message[ServiceExecute::apierr,"message"/.("errors"/.res)];
		Throw[$Failed]
	]
]

(* Raw *)
oauthservicedata["Runkeeper","UserData"] = {
        "URL"				  	-> "https://api.runkeeper.com/user",
        "HTTPSMethod"			-> "GET",
        "ResultsFunction"		->	runkeeperimport
    }

oauthservicedata["Runkeeper","FitnessActivities"] = {
        "URL"				  	-> "https://api.runkeeper.com/fitnessActivities",
        "HTTPSMethod"			-> "GET",
        "ResultsFunction"		->	runkeeperimport
    }
    
oauthservicedata["Runkeeper","FitnessActivity"] = {
        "URL"				  	-> (ToString@StringForm["https://api.runkeeper.com/fitnessActivities/`1`", #1]&),
        "PathParameters" 		-> {"Activity"},
        "HTTPSMethod"			-> "GET",
        "ResultsFunction"		->	runkeeperimport
    }
    
(* cooked *)
oauthcookeddata["Runkeeper","UserDataGrid",id_,args___]:=Module[
	{rawdata},
	rawdata=OAuthClientDump`rawoauthdata[id,"UserData",args];
	addtitles[FlatGrid[{"userID","weight"},rawdata],{"Parameter","Value"}]
]    

oauthcookeddata["Runkeeper","FitnessActivitiesGrid",id_,args___]:=Module[
	{rawdata, data, ids},
	rawdata=OAuthClientDump`rawoauthdata[id,"FitnessActivities",args];
	data = Normal[runkeeperimport[rawdata]];
	data = "items" /. data;
	data=data/.formatvalue["start_time"->readDate];
 	ids = Last[StringSplit[#, "/"]] & /@ ("uri" /. data);
 	prettygrid[Join[{Flatten[{"id", {"duration", "has_path", "source", "start_time", "total_calories", "total_distance", "type"}}]},
   			Join[Transpose[{ids}], {"duration", "has_path", "source", "start_time", "total_calories", "total_distance", "type"} /. data, 2] 
   		]]
]    

oauthcookeddata["Runkeeper","FitnessActivityGrid",id_,args___]:=Module[
	{rawdata},
	rawdata=OAuthClientDump`rawoauthdata[id,"FitnessActivity",args];
	addtitles[FlatGrid[{"userID","type","start_time","total_distance","duration","total_calories"},rawdata,{formatvalue["start_time"->readDate]}],{"Parameter","Value"}]
]  

oauthcookeddata["Runkeeper","PathGrid",id_,args___]:=Module[
	{rawdata},
	rawdata=OAuthClientDump`rawoauthdata[id,"FitnessActivity",args];
	GridList[{{"path",{"altitude", "latitude", "longitude", "timestamp"}}},rawdata]
]  

oauthcookeddata["Runkeeper","GoogleMap",id_,args___]:=Module[
	{rawdata,latlongs},
	rawdata=OAuthClientDump`rawoauthdata[id,"FitnessActivity",args];
	latlongs={"latitude", "longitude"}/.(runkeeperimport[rawdata]["path"]);
	googleMapPath[latlongs]
]  

    
makepath[latlongs_] := 
 StringJoin @@ (ToString /@ 
    Flatten[Riffle[Insert[#, ",", 2] & /@ latlongs, "|"]])

googleMapPath[latlongs_] := Block[
  {baseurl = "http://maps.googleapis.com/maps/api/staticmap?", 
   midpoint, center, size, path, url, len, sample = 1},
  midpoint = Mean /@ Transpose[latlongs];
  center = 
   "center=" <> ToString[midpoint[[1]]] <> "," <> 
    ToString[midpoint[[2]]];
  size = "size=500x400";
  path = "path=color:0x0000ff|weight:5|" <> makepath[latlongs];
  len = StringLength[path];
  If[len > 1800,
   sample = Ceiling[len/1800];
   path = 
    "path=color:0x0000ff|weight:5|" <> 
     makepath[latlongs[[1 ;; -1 ;; sample]]];
   ];
  url = StringJoin[baseurl, size, "&", path, "&sensor=true"];
  Import[url]
  ]
  
(******************************* LinkedIN *************************************)

oauthservicedata["LinkedIn"]={
		"OAuthVersion"		->"2.0",
		"ServiceName" 		-> "LinkedIn", 
	 	"AuthorizeEndpoint" -> "https://www.linkedin.com/uas/oauth2/authorization", 
     	"AccessEndpoint"    -> "https://www.linkedin.com/uas/oauth2/accessToken",
     	"RedirectURI"       -> "http://www.wolfram.com/socialmediadata/linkedin",
     	"VerifierLabel"     -> "code",
	 	"ClientInfo"		-> {40, 42, 33, 49, 78, 33, 42, 41, 53, 52, 42, 13, 10, 102, 118, 194, 
			163, 194, 173, 39, 45, 43, 41, 120, 91, 67, 100, 36, 114, 70, 194, 
			166, 103, 116, 85, 39, 92, 126, 123, 37, 74, 69, 60, 97, 52, 39, 118, 
			93, 58, 85, 54, 38, 37, 98, 39, 62, 112, 194, 167, 42, 124, 76, 81, 
			13, 10},
	 	"AuthenticationDialog" -> "TokenDialog",
	 	"Gets"				-> {"Connections","ConnectionIDs","MyGroups"},
	 	"Posts"				-> {},
	 	"RawGets"			-> {"UserData","RawConnections","Search","RawGroups","GroupData"},
	 	"RawPosts"			-> {},
	 	"RequestFormat"		-> (With[{params=Cases[{##},("Parameters"->x_):>x,Infinity], url=DeleteCases[{##},"Parameters"->_,Infinity]},
	 		URLFetch@@({Sequence@@url, "Parameters"->Flatten[{params,"format"->"json"}]}/."access_token"->"oauth2_access_token")]&),
 		"Information"		-> "A service for receiving data from a LinkedIn account"
}
   
linkedinimport[json_]:=With[{res=ImportString[json,"JSON"]},
	If[FreeQ[res,_["errorCode",_]],
		assoc@res,
		Message[ServiceExecute::apierr,"message"/.res];
		Throw[$Failed]
	]
]

(* Raw *)
oauthservicedata["LinkedIn","UserData"] = {
        "URL"				-> "https://api.linkedin.com/v1/people/~:(id,formatted-name,headline,industry,distance,current-status,current-share,num-connections,summary,positions,picture-url,public-profile-url)",
        "Parameters" 		-> {},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> linkedinimport
    }
 
oauthservicedata["LinkedIn","RawConnections"] = {
        "URL"				-> "https://api.linkedin.com/v1/people/~/connections",
        "Parameters" 		-> {"start", "count", "modified", "modified-since"},
        "OptionalParameters" 		-> {"start", "count", "modified", "modified-since"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> linkedinimport
    }

oauthservicedata["LinkedIn","Search"] = {
        "URL"				-> "https://api.linkedin.com/v1/people-search",
        "Parameters" 		-> {"keywords", "first-name", "last-name", "school-name", "company-name","current-company","title","current-title","postal-code","distance","count","sort"},
        "OptionalParameters"-> {"keywords", "first-name", "last-name", "school-name", "company-name","current-company","title","current-title","postal-code","distance","count","sort"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> linkedinimport
    }
    
 oauthservicedata["LinkedIn","RawGroups"] = {
        "URL"				-> "https://api.linkedin.com/v1/people/~/group-memberships",
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> linkedinimport
    }
    
oauthservicedata["LinkedIn","GroupData"] = {
        "URL"				-> (ToString@StringForm["https://api.linkedin.com/v1/groups/`1`:(id,name,short-description,description,relation-to-viewer:(membership-state,available-actions),posts,counts-by-category,is-open-to-non-members,category,website-url,locale,location:(country,postal-code),allow-member-invites,site-group-url,small-logo-url,large-logo-url)", ToString[#]]&),
       	"PathParameters" 	-> {"Group"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> linkedinimport
    } 
    
(* Cooked *)
oauthcookeddata["LinkedIn","Connections",id_,args___]:=Module[
	{rawdata,names},
	rawdata=OAuthClientDump`rawoauthdata[id,"RawConnections",args];
	rawdata=Normal@linkedinimport[rawdata];
	names={"firstName", "lastName"}/.("values"/.rawdata);
	names=DeleteCases[names,"firstName"|"lastName",Infinity];
	If[Flatten[names]==={},Return[{}]];
	names = MapThread[StringJoin[#1, " ", #2] &, Transpose[names]];
    names = Replace[names, (a_ /; StringMatchQ[a, "private" ~~ __ ~~ "private" ~~ ___]) :> Sequence[], {1}];
	names /; (names =!= $Failed)
]

oauthcookeddata["LinkedIn","ConnectionIDs",id_,args___]:=Module[
	{rawdata,ids},
	rawdata=OAuthClientDump`rawoauthdata[id,"RawConnections",args];
	rawdata=Normal@linkedinimport[rawdata];
	ids="id"/.("values"/.rawdata);
	ids=DeleteCases[ids,"id",Infinity];
	If[Flatten[ids]==={},Return[{}]];
    ids = Replace[ids, (a_ /; StringMatchQ[a, "private" ~~ ___]) :> Sequence[], {1}];
	ids /; (ids =!= $Failed)
]

oauthcookeddata["LinkedIn","MyGroups",id_,args___]:=Module[
	{rawdata,groups},
	rawdata=OAuthClientDump`rawoauthdata[id,"RawGroups",args];
	rawdata=linkedinimport[rawdata];
	groups="group"/.rawdata["values"];
	groups=DeleteCases[groups,"group",Infinity];
	If[Flatten[groups]==={},Return[{}]];
	groups /; (groups =!= $Failed)
]

(******************************* Facebook *************************************)

oauthservicedata["Facebook"]={
		"OAuthVersion"		->"2.0",
		"ServiceName" 		-> "Facebook", 
	 	"AuthorizeEndpoint" -> "https://graph.facebook.com/oauth/authorize", 
     	"AccessEndpoint"    -> "https://graph.facebook.com/oauth/access_token",
     	"RedirectURI"       -> "https://www.wolfram.com/socialmediadata/facebook2",
     	"VerifierLabel"     -> "code",
	 	"ClientInfo"		-> {40, 42, 33, 49, 78, 33, 42, 41, 53, 52, 42, 13, 10, 102, 118, 106, 
			194, 166, 194, 161, 76, 75, 59, 62, 194, 169, 111, 107, 68, 46, 98, 
			96, 117, 79, 116, 62, 61, 67, 93, 107, 52, 116, 83, 92, 112, 115, 
			105, 75, 51, 45, 43, 85, 64, 37, 118, 88, 46, 100, 96, 126, 107, 194, 
			161, 99, 50, 100, 77, 53, 111, 104, 98, 111, 61, 102, 78, 67, 112, 
			13, 10},
	 	"AuthenticationDialog" -> "TokenDialog",
	 	"Gets"				-> Join[{"FriendsGrid","AllUserData","PermissionList"},facebookuserdataNames],
	 	"Posts"				-> {"PostLink","PostMessage"},
	 	"RawGets"			-> {"FriendsList","UserData",(* "Accounts", *)"Permissions","GetPosts"},
	 	"RawPosts"			-> {"SendPost"},
 		"Information"		-> "A service for sending and receiving data from a Facebook account",
 		"AccessTokenExtractor"	-> "Text/2.0"
}

OAuthClient`checkpermissions["Facebook",id_]:=With[{res=facebookimport[OAuthClientDump`rawoauthdata[id,"Permissions"]]},
	If[KeyExistsQ[res,"data"],
		Cases[res["data"],(p_->1):>p,Infinity],
		{}
	]
]

OAuthClient`addpermissions["Facebook",id_,permissions_]:=Module[{
	res,url
	},
	url="https://www.facebook.com/dialog/oauth?client_id="<>
        	("ConsumerKey"/.oauthservicedata["Facebook"])<>"&redirect_uri="<>
        	("RedirectURI"/.oauthservicedata["Facebook"])<>"&scope="<>
        	StringJoin[Riffle[permissions,","]];
	res=tokenOAuthDialog[url,"Permissions"];
	res
]

    

facebookimport[json_]:=assoc@ImportString[json,"JSON"]

(* Raw *)
oauthservicedata["Facebook","FriendsList"] = {
        "URL"				->  "https://graph.facebook.com/me/friends",
        "Parameters" 		-> {},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> facebookimport,
        "RequiredPermissions"-> {}
    }
 
$facebookuserdatapermissions ={};
oauthservicedata["Facebook","UserData"] = {
        "URL"				->  "https://graph.facebook.com/me",
        "Parameters" 		-> {"fields"},
        "OptionalParameters"-> {"fields"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> facebookimport,
        "RequiredPermissions":> $facebookuserdatapermissions
    }
    
oauthservicedata["Facebook","Accounts"] = {
        "URL"				->  "https://graph.facebook.com/me/accounts",
        "Parameters" 		-> {},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> facebookimport,
        "RequiredPermissions"-> {}
    }
    
oauthservicedata["Facebook","Permissions"] = {
        "URL"				->  "https://graph.facebook.com/me/permissions",
        "Parameters" 		-> {},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> facebookimport,
        "RequiredPermissions"-> {}
    } 
    
oauthservicedata["Facebook","GetPosts"] = {
        "URL"				->  "https://graph.facebook.com/me/posts",
        "Parameters" 		-> {"limit"},
        "OptionalParameters"-> {"limit"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> facebookimport,
        "RequiredPermissions"-> {}
    } 
       
oauthservicedata["Facebook","GetFeed"] = {
        "URL"				->  "https://graph.facebook.com/me/feed",
        "Parameters" 		-> {"limit"},
        "OptionalParameters"-> {"limit"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> facebookimport,
        "RequiredPermissions"-> {}
    } 
     
(* Raw Post *)
oauthservicedata["Facebook","SendPost"] = {
        "URL"				->  "https://graph.facebook.com/me/feed",
        "Parameters"		-> {"message","link"},
        "OptionalParameters"-> {"message","link"},
        "HTTPSMethod"		-> "Post",
        "ResultsFunction"	-> facebookimport,
        "RequiredPermissions"-> {"status_update"}
    }
    
(* Cooked *)
  
oauthcookeddata["Facebook","PermissionList",id_,args___]:=Module[
	{rawdata,as},
	rawdata=OAuthClientDump`rawoauthdata[id,"Permissions",args];
	as=facebookimport[rawdata];
	If[KeyExistsQ[as,"data"],
		Cases[as["data"],(p_->1):>p,Infinity]
		,
		{}
	]
]
 
oauthcookeddata["Facebook","FriendsGrid",id_,args___]:=Module[
	{rawdata,as},
	rawdata=OAuthClientDump`rawoauthdata[id,"FriendsList",args];
	as=facebookimport[rawdata];
	If[KeyExistsQ[as,"data"],
		prettygrid[Join[{{"Name","User ID"}},{"name", "id"} /. as[["data"]]]],
		{}
	]
]
 
oauthcookeddata["Facebook","PostMessage",id_,args0___]:=Module[
	{rawdata,as, args},
	args=Switch[{args0},
		{_["message"|"Message",_]},{"message"->args0[[2]]},
		_?(ListQ[#]&&!FreeQ[#,_["message"|"Message",_]]&),Flatten[{args0}/."Message"->"message"],
		{_String},{"message"->args0},
		_,Message[ServiceExecute::param,"message"];Throw[$Failed]
	];
	rawdata=OAuthClientDump`rawoauthdata[id,"SendPost",args];
	as=facebookimport[rawdata];
	If[FreeQ[Normal@as,"error"|"Error"],"message"/.args,errormessagehandler[as]]
]
 
oauthcookeddata["Facebook","PostLink",id_,args0___]:=Module[
	{rawdata,as, args},
	
	args=Switch[{args0},
		{_["link"|"Link",_]},{"link"->args0[[2]]},
		_?(ListQ[#]&&!FreeQ[#,_["link"|"Link",_]]&),Flatten[{args0}/."Link"->"link"],
		{_String},{"link"->args0},
		_,Message[ServiceExecute::paralinkage,"Link"];Throw[$Failed]
	];
	rawdata=OAuthClientDump`rawoauthdata[id,"SendPost",args];
	as=facebookimport[rawdata];
	If[FreeQ[Normal@as,"error"|"Error"],"link"/.args,errormessagehandler[as]]
]

facebookuserdatafields={"id", "name", "first_name", "middle_name", "last_name", "gender", "locale", "languages", "link",
                "username", "third_party_id", "installed", "timezone", "updated_time", "verified", "bio",
        		"birthday", "cover", "currency", "devices", "education", "email", "hometown", "interested_in",
                "location", "political", "favorite_athletes", "favorite_teams", "picture", "quotes", "relationship_status",
                "religion", "security_settings", "significant_other", "video_upload_limits", "website", "work" };
facebookuserdataNames=(StringReplace[#, {WordBoundary ~~ x_ :> ToUpperCase[x], "_" ~~ x_ :> ToUpperCase[x]}]&/@facebookuserdatafields)/.{"Name"->"Username","Id"->"UserId"};
facebookuserdatapermissionrules={"languages"->"user_likes","bio"->"user_about_me","education"->"user_education_history",
	"email"->"email","hometown"->"user_hometown","interested_in"->"user_relationship_details","location"->"user_location",
	"political"->"user_religion_politics","favorite_athletes"->"user_likes","favorite_teams"->"user_likes","quotes"->"user_about_me",
	"relationship_status"->"user_relationships","religion"->"user_religion_politics","significant_other"->"user_relationships",
	"website"->"user_website","work"->"user_work_history",_String:>Sequence@@{}};
                
oauthcookeddata["Facebook","AllUserData",id_,args0___]:=Block[
	{$facebookuserdatapermissions={"user_likes","user_about_me","user_birthday","user_education_history","email",
		"user_hometown","user_relationship_details","user_location","user_religion_politics","user_website","user_work_history","user_relationships"},
		rawdata},
		rawdata=OAuthClientDump`rawoauthdata[id,"UserData","fields"->StringJoin[Riffle[facebookuserdatafields,","]]];          
     	facebookimport[rawdata]            
]


oauthcookeddata["Facebook",prop:(Alternatives@@facebookuserdataNames),id_,args0___]:=Block[
	{field=prop/.Thread[facebookuserdataNames->facebookuserdatafields],
		$facebookuserdatapermissions,rawdata, res},
		$facebookuserdatapermissions={field/.facebookuserdatapermissionrules};
		debugPrint["Facebook user field"->field];
		debugPrint["$facebookuserdatapermissions"->$facebookuserdatapermissions];
		rawdata=OAuthClientDump`rawoauthdata[id,"UserData","fields"->field];          
     	res=facebookimport[rawdata];
     	If[KeyExistsQ[res,field],res[field],Missing["NotAvailable"]]           
]

facebookpostfields={"id", "from", "to", "message", "message_tags", "picture", "link", "name", "caption", "description",
                                "source", "properties", "icon", "actions", "privacy", "type", "likes", "place", "story", "story_tags",
                                "with_tags", "comments", "object_id", "application", "created_time", "updated_time","status_type"};
facebookpostNames=(("Post"<>StringReplace[#, {WordBoundary ~~ x_ :> ToUpperCase[x], "_" ~~ x_ :> ToUpperCase[x]}])&/@facebookpostfields);
facebookpostpermissionrules={"privacy"->"read_stream","place"->"read_stream","story"->"read_stream","story_tags"->"read_stream",
	"with_tags"->"read_stream","object_id"->"read_stream","comments"->"read_stream","application"->"read_stream",
	"created_time"->"read_stream","updated_time"->"read_stream","status_type"->"read_stream",
	_String:>Sequence@@{}};
                                
RawFacebookData["Posts"] = {
        "URL"				-> "https://graph.facebook.com/`id`/posts",
        "Parameters" 		-> {"id"},
        "Fields" 			-> {"id", "from", "to", "message", "message_tags", "picture", "link", "name", "caption", "description",
                                "source", "properties", "icon", "actions", "privacy", "type", "likes", "place", "story", "story_tags",
                                "with_tags", "comments", "object_id", "application", "created_time", "updated_time"},
        "Information" 		-> "https://developers.facebook.com/docs/reference/api/user/#posts"
    }


oauthcookeddata["Facebook",prop:(Alternatives@@facebookuserdataNames),id_,args0___]:=Block[
	{field=prop/.Thread[facebookuserdataNames->facebookuserdatafields],
		$facebookuserdatapermissions,rawdata, res},
		$facebookuserdatapermissions={field/.facebookuserdatapermissionrules};
		rawdata=OAuthClientDump`rawoauthdata[id,"UserData","fields"->field];          
     	res=facebookimport[rawdata];
     	If[KeyExistsQ[res,field],res[field],Missing["NotAvailable"]]           
]

(* Send Message *)
oauthsendmessage["Facebook",id_,message_String]:=oauthcookeddata["Facebook","Post",id,"message"->message]

(**** error handling ***)
  
oauthcookeddata[args___]:=Throw[$Failed]
oauthsendmessage[___]:=Throw[$Failed]

OAuthClient`checkpermissions[___]:=All
OAuthClient`addpermissions[___]:=Throw[$Failed]

(************************ Formating Utilities *********************************)

formatDate["FitBit"]:=formatDate[DateList[],"FitBit"]
formatDate[per:("1d"|"7d"|"30d"|"1w"|"1m"|"3m"|"6m"|"1y"|"max"|"today"),"FitBit"]:=per
formatDate[date_,"FitBit"]:=Quiet[DateString[DateList[date], {"Year", "-", "Month", "-", "Day"}],DateList::arg]
formatDate[dates__,"FitBit"]:=StringJoin[Riffle[formatDate[#,"FitBit"]&/@{dates},"/"]]
formatDate[date_,"BodyMedia"]:=DateString[date, {"Year", "Month", "Day"}]
readDate[date_]:=DateString[DateList[date]]
readDate[date_,"FitBit",form_:DateString]:=form[DateList[{date,{"Year", "-", "Month", "-", "Day"}}]]/;StringFreeQ[date,"T"]
readDate[date_,"FitBit",form_:DateString]:=form[DateList[{date,{"Year", "-", "Month", "-", "Day", "T", 
	"Hour", ":", "Minute", ":","Second", ".", "Millisecond"}}]]
readDate[date_,"Twitter",form_:DateString]:=form@DateList[{StringSplit[date, {" +", " -"}][[1]], {"DayName", 
    "MonthName", "Day", "Hour", ":", "Minute", ":", "Second"}}]
readDate[date_,"BodyMedia",form_:DateString]:=form[ToExpression /@ {StringTake[date, 4], StringTake[date, {5, 6}], StringTake[date, -2]}]
    
createquantity[l_List]:=createquantity/@l
createquantity[_[label_,unit_]]:=(Rule[label,value_]:>Rule[label,Quantity[value, unit]])

formatvalue[_[label_,fun_]]:=(Rule[label,value_]:>Rule[label,fun[value]])

GridList[params_,data_,rest___]:=gridlistfun[params,ImportString[data,"JSON"],rest]
gridlistfun[params_List, data_, gridopts___] := With[{tmp=Reap[flatgridfun0[#, data] & /@ params][[2, 1]]},
 prettygrid[Join[{tmp[[All, 1]]}, Transpose[tmp[[All, 2]]]], gridopts]]

prettygrid[args___]:=Grid[args, Background -> {None, {Lighter[Yellow, .9], {White, 
    Lighter[Blend[{Blue, Green}], .8]}}}, Dividers -> {{Darker[
    Gray, .6], {Lighter[Gray, .5]}, 
   Darker[Gray, .6]}, {Darker[Gray, .6], Darker[Gray, .6], {False}, 
   Darker[Gray, .6]}}, Alignment -> {{Left, Right, {Left}}}]


FlatGrid[params_,data_,formatrules_:{},rest___]:=flatgridfun[params,ImportString[data,"JSON"]/.formatrules,rest]
flatgridfun[params_List, data_, gridopts___] := 
 prettygrid[DeleteCases[Reap[flatgridfun0[#, data] & /@ params][[2, 1]],{a_,a_},{1}], gridopts]
 
flatgridfun0[{label_, params_List}, 
  data_] := (flatgridfun0[#, label /. data] & /@ params) /; !ListQ[label]
flatgridfun0[params_List, data_] := flatgridfun0[#, data] & /@ params
flatgridfun0[param_, data_] := Sow[{param, param /. data}]

addtitles[grid_Grid,title_]:=ReplacePart[grid,1->Prepend[grid[[1]],title]]
timeseries[params_, data_]:=TemporalData[Reap[flatgridfun0[#, data] & /@ params][[2, 1]]]

parameterspresentQ[args_, params_]:=And@@(!FreeQ[args,(Rule|RuleDelayed)[#,_]]&/@params)

(************ Error Messages ********************)
throwifpresent[as_, key_]:=If[KeyExistsQ[as,key],Throw[as[key]],Missing]

errormessagehandler[as_Association]:=(throwifpresent[as,#]&/@{"message","Message","ErrorMessage","errormessage","error_message","error","Error"};
	$Failed)

errormessagehandler[___]:=$Failed

End[] (* End Private Context *)

SetAttributes[{predefinedservicelist,OAuthServicesData,oauthcookeddata,oauthsendmessage},{ReadProtected, Protected}];

System`Private`RestoreContextPath[];

(*
{OAuthClient`predefinedservicelist,OAuthClient`OAuthServicesData,OAuthClient`oauthcookeddata,OAuthClient`oauthsendmessage}
*)