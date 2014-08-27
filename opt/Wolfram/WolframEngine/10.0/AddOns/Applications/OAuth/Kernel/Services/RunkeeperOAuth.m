System`Private`NewContextPath[{"OAuthClient`","System`"}];

(* declare functions that will be used by the OAuth paclet *)
OAuthClient`runkeeperdata;
OAuthClient`runkeepercookeddata;
OAuthClient`runkeepersendmessage;


Begin["RunkeeperOAuthDump`"] (* Begin Private Context *) 

(******************************* Fitbit *************************************)

(* Authentication information *)

runkeeperdata[]={
     	"ServiceName"       -> "Runkeeper",
     	"OAuthVersion"		 -> "2.0",
     	"AuthorizeEndpoint" -> "https://runkeeper.com/apps/authorize",
     	"AccessEndpoint"    -> "https://runkeeper.com/apps/token",
     	"RedirectURI"       -> "https://user.wolfram.com/oauth/facebook_catch.php",
     	"VerifierLabel"     -> "code",
     	"ClientInfo"		-> {40, 42, 33, 49, 78, 33, 42, 41, 53, 52, 42, 13, 10, 102, 118, 194, 
			161, 87, 55, 122, 94, 47, 80, 32, 54, 43, 39, 105, 124, 61, 99, 35, 
			94, 122, 194, 162, 120, 76, 57, 68, 75, 49, 114, 74, 33, 112, 121, 
			70, 54, 76, 34, 53, 81, 100, 34, 121, 94, 35, 97, 103, 95, 95, 109, 
			99, 94, 54, 72, 97, 94, 112, 59, 84, 68, 34, 51, 57, 117, 80, 54, 49, 
			39, 63, 126, 99, 53, 13, 10, 62, 112, 106, 98, 72, 122, 13, 10},
     	"AuthenticationDialog" -> "TokenDialog",
    	"RequestFormat"		-> {"Headers","Bearer"},
 		"Gets"				-> Join[{"UserID","UserData","FitnessActivitiesGrid","FitnessActivity","FitnessActivityGrid",
 			"AnimatedPathMap","Path","PathList","PathGrid","PathMap","PathsMap","PathGoogleMap"},
 									userendpointnames],
 		"Posts"				-> {},
 		"RawGets"			-> Join[{"RawUserData","RawFitnessActivity","RawBackgroundActivity"},
 									rawuserendpoints],
 		"RawPosts"			-> {},
 		"Information"		-> "A service for accessing data from a Runkeeper account"
    }
    
(* a function for importing the raw data - usually json or xml - from the service *)
runkeeperimport[$Failed,___]:=Throw[$Failed]
runkeeperimport[json_String,camelQ_:False,formatting_:{}]:=Block[{res=ImportString[json,"JSON"]},
	If[res===$Failed,Throw[$Failed]];
	If[FreeQ[res,_["errors",_]],
		debugPrint["runkeeperimport res1"->res];
		If[TrueQ[camelQ],res=Replace[(res/.formatting),HoldPattern[Rule[a_String,b_]]:>Rule[camelcase[a],b],Infinity]];
		debugPrint["runkeeperimport res2"->res];
		Switch[res,
			_Rule|{_Rule...},assoc@res,
			{{_Rule...}...},assoc/@res,
			_,res
		],
		Message[ServiceExecute::apierr,"message"/.("errors"/.res)];
		Throw[$Failed]
	]
]

runkeeperimport[raw_,___]:=raw

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

(* This is the only documented static url.
	All other urls should be requested from this url *)
baseurl="https://api.runkeeper.com"

(* current and default values, updated whenever possible *)
$updateduserendpoints=False;
{"RawProfile", "RawSettings", "RawFitnessActivities", 
"RawStrengthTrainingActivities", "RawBackgroundActivities", 
"RawSleep", "RawNutrition", "RawWeight", "RawGeneralMeasurements", 
"RawDiabetes", "RawRecords", "RawTeam"}
endpointurl["RawProfile"]="/profile";
endpointurl["RawSettings"]="/settings";
endpointurl["RawFitnessActivities"]="/fitnessActivities";
endpointurl["RawStrengthTrainingActivities"]="/strengthTrainingActivities";
endpointurl["RawBackgroundActivities"]="/backgroundActivities";
endpointurl["RawSleep"]="/sleep";
endpointurl["RawNutrition"]="/nutrition";
endpointurl["RawWeight"]="/weight";
endpointurl["RawGeneralMeasurements"]="/generalMeasurements";
endpointurl["RawDiabetes"]="/diabetes";
endpointurl["RawRecords"]="/records";
endpointurl["RawTeam"]="/team";
endpointurl["RawChangeLog"]="/changeLog";

userendpoints={"profile", "settings", "fitness_activities", "strength_training_activities", "background_activities", "sleep", 
"nutrition", "weight", "general_measurements", "diabetes", "records" (*,"change_log" *), "team"};
userendpointnames=camelcase[userendpoints];
rawuserendpoints="Raw"<>#&/@userendpointnames;

(* Raw *)
runkeeperdata["RawUserData"] = {
        "URL"				  	-> baseurl<>"/user",
        "HTTPSMethod"			-> "GET",
        "ResultsFunction"		->	runkeeperimport
    }

runkeeperdata[prop:(Alternatives@@rawuserendpoints)]:={
        "URL"				  	-> baseurl<>endpointurl[prop],
        "HTTPSMethod"			-> "GET",
        "Parameters"			-> {"noEarlierThan","noLaterThan","modifiedNoEarlierThan","modifiedNoLaterThan"},
        "ResultsFunction"		->	runkeeperimport
    }
    
runkeeperdata["RawFitnessActivity"] = {
        "URL"				  	-> (ToString@StringForm[baseurl<>endpointurl["RawFitnessActivities"]<>"/`1`", #1]&),
        "PathParameters" 		-> {"ActivityID"},
   		"RequiredParameters"	-> {"ActivityID"},
        "HTTPSMethod"			-> "GET",
        "ResultsFunction"		->	runkeeperimport
    }
runkeeperdata["RawBackgroundActivity"] = {
        "URL"				  	-> (ToString@StringForm[baseurl<>endpointurl["RawBackgroundActivities"]<>"/`1`", #1]&),
        "PathParameters" 		-> {"ActivityID"},
   		"RequiredParameters"	-> {"ActivityID"},
        "HTTPSMethod"			-> "GET",
        "ResultsFunction"		->	runkeeperimport
    }
     
       
runkeeperdata[___]:=$Failed
(****** Cooked Properties ******)
  
(* cooked data queries 
	Cooked queries call at least one of the raw queried underneath but add pre and post processing to make the function easier to use and the result clearer.
*)  

runkeepercookeddata[prop_,id_,rule_Rule, rest___]:=runkeepercookeddata[prop,id,{rule}, rest]
runkeepercookeddata[prop_,id_]:=runkeepercookeddata[prop,id,{}]

setpath[_,Missing|Missing[___]|$Failed]:=Null
setpath[prop:(Alternatives@@rawuserendpoints),url_]:=(endpointurl[prop]=url)

updateendpoints[data_]:=(MapThread[setpath[#1,Lookup[data,#2,Missing[]]]&,{rawuserendpoints,userendpoints}];
	$updateduserendpoints=True)

runkeepercookeddata["UserID",id_,args_]:=Module[
	{rawdata, params,data},
	params=filterparameters[args,getallparameters["RawUserData"]];
	rawdata=OAuthClient`rawoauthdata[id,"RawUserData",params];
	data=runkeeperimport[rawdata];
	If[!$updateduserendpoints,updateendpoints[data]];
	Lookup[data,"userID",Missing["NotAvailable"]]
]  

runkeepercookeddata["UserData",id_,args_]:=assoc@FilterRules[Normal[runkeepercookeddata["Profile",id, args]],{"Location","Profile","Birthday","Gender","Name","AthleteType"}]


runkeepercookeddata[prop:(Alternatives@@userendpointnames),id_,args_]:=Module[
	{rawdata,data, params},
	If[!$updateduserendpoints,runkeepercookeddata["UserID",id]];
	debugPrint["args"->args];
	params=filterparameters[args,getallparameters["Raw"<>prop]];
	debugPrint["params"->params];
	params=fixdates[params];
	rawdata=OAuthClient`rawoauthdata[id,"Raw"<>prop, params];
	debugPrint[Hold[runkeeperimport][rawdata,True]];
	data=runkeeperimport[rawdata,True];
	parseuserdata[data,prop]
]  

runkeepercookeddata["FitnessActivitiesGrid",id_,args_]:=Module[
	{data, fields,params},
	params=filterparameters[args,getallparameters["RawFitnessActivities"]];
	data=runkeepercookeddata["FitnessActivities",id,params];
	If[data==={},Return[{}]];
	fields={ "ActivityID", "StartTime","Type", "Source", "Duration", "TotalDistance","TotalCalories"};
 	OAuthClient`prettygrid[Join[{fields},
 			fields/.data
   		]]
]    

runkeepercookeddata["FitnessActivity",id_,args_]:=Module[
	{rawdata, params,fields={"Source","Uri","Climb","Type","Duration","TotalDistance","TotalCalories"}},
	params=filterparameters[args,getallparameters["RawFitnessActivity"]];
	If[FreeQ[params,"ActivityID"],Message[ServiceExecute::nparam,"ActivityID"];Throw[$Failed]];
	params=params/.HoldPattern[Rule[a_,b_]]:>Rule[a,ToString[b]];
	rawdata=OAuthClient`rawoauthdata[id,"RawFitnessActivity",params];
	assoc@(Thread[fields->(Lookup[
		runkeeperimport[rawdata,True,{formatvalue["start_time"->readDate],createquantity["duration"->"Seconds"],
			createquantity["climb"->"Meters"],
			createquantity["total_distance"->"Meters"],
			formatvalue["uri"->(getID[#]&)]}],
		#,
		Null
	]&/@fields)]/.{"Uri"->"ActivityID",HoldPattern[Rule[_,Null|Missing|Missing[___]]]->Sequence@@{}})
]  

runkeepercookeddata["FitnessActivityGrid",id_,args_]:=Module[
	{data, fields,params},
	params=filterparameters[args,getallparameters["RawFitnessActivity"]];
	data=runkeepercookeddata["FitnessActivity",id,params];
	If[data==={},Return[{}]];
	fields={"Source","ActivityID","Climb","Type","Duration","TotalDistance","TotalCalories"};
 	addtitles[OAuthClient`prettygrid[Transpose[{fields,
 			fields/.Normal[data]}]
   		],{"Activity Data",SpanFromLeft}]
]   
   
runkeepercookeddata[prop:("Path"|"PathGrid"|"PathList"),id_,args_]:=Module[
	{rawdata, params,data},
	params=filterparameters[args,getallparameters["RawFitnessActivity"]];
	If[FreeQ[params,"ActivityID"],Message[ServiceExecute::nparam,"ActivityID"];Throw[$Failed]];
	params=params/.HoldPattern[Rule[a_,b_]]:>Rule[a,ToString[b]];
	rawdata=OAuthClient`rawoauthdata[id,"RawFitnessActivity",params];
	data=runkeeperimport[rawdata,True,{formatvalue["timestamp"->readTime]}];
	data=getgeolocations[data["Path"]];
	Switch[prop,
		"Path",data={"Timestamp", "GeoPosition"}/.data;
			data[[All,1]]=DateList/@data[[All,1]];
			TimeSeries[data],
		"PathGrid",OAuthClient`GridList[{{"Timestamp","GeoPosition"}},data],
		"PathList",assoc/@data
		]
] 

runkeepercookeddata["PathMap",id_,args_]:=Module[
	{rawdata, params,data},
	debugPrint["PathMap 0"];
	params=filterparameters[args,getallparameters["RawFitnessActivity"]];
	If[FreeQ[params,"ActivityID"],Message[ServiceExecute::nparam,"ActivityID"];Throw[$Failed]];
	params=params/.HoldPattern[Rule[a_,b_]]:>Rule[a,ToString[b]];
	debugPrint["PathMap 3"];
	rawdata=OAuthClient`rawoauthdata[id,"RawFitnessActivity",params];
	data=runkeeperimport[rawdata];
	data=Cases[{"latitude","longitude"}/.data["path"],{_?NumberQ,_?NumberQ}];
	createMap[data]
]  

runkeepercookeddata["AnimatedPathMap",id_,args_]:=Module[
	{rawdata, params,data,t},
	debugPrint["PathMap 0"];
	params=filterparameters[args,getallparameters["RawFitnessActivity"]];
	If[FreeQ[params,"ActivityID"],Message[ServiceExecute::nparam,"ActivityID"];Throw[$Failed]];
	params=params/.HoldPattern[Rule[a_,b_]]:>Rule[a,ToString[b]];
	debugPrint["PathMap 3"];
	rawdata=OAuthClient`rawoauthdata[id,"RawFitnessActivity",params];
	data=runkeeperimport[rawdata];
	data=Cases[{"latitude","longitude","timestamp"}/.data["path"],{_?NumberQ..}];
	data=SortBy[data,Last];
	t=data[[All,3]];
	data=data[[All,1;;2]];
	createAnimatedMap[data,t]
]  

runkeepercookeddata["PathsMap",id_,args_]:=Block[
	{rawdata, params,data, urls,rawpathdata,pathdata, paths={},OAuthClient`$CacheResults=True},
	debugPrint["PathsMap 0"];
	params=filterparameters[args,getallparameters["RawFitnessActivities"]];
	params=params/.HoldPattern[Rule[a_,b_]]:>Rule[a,ToString[b]];
	rawdata=OAuthClient`rawoauthdata[id,"RawFitnessActivities",params];
	data=runkeeperimport[rawdata];
	urls={"uri","has_path"}/.data["items"];
	debugPrint["PathsMap urls"->urls];
	urls=First/@Cases[urls,{_,True},{1}];
	debugPrint["PathsMap urls 2"->urls];
	urls=getID/@urls;
	(	
		rawpathdata=OAuthClient`rawoauthdata[id,"RawFitnessActivity","ActivityID"->#];
		pathdata=runkeeperimport[rawpathdata];
		pathdata=Cases[{"latitude","longitude"}/.pathdata["path"],{_?NumberQ,_?NumberQ}];
		paths=Join[paths,{pathdata}];
	)&/@urls;
	createMap[paths]
] 

runkeepercookeddata["PathGoogleMap",id_,args_]:=Module[
	{rawdata,latlongs, params},
	params=filterparameters[args,getallparameters["RawFitnessActivity"]];
	rawdata=OAuthClient`rawoauthdata[id,"RawFitnessActivity",params];
	latlongs={"latitude", "longitude"}/.(runkeeperimport[rawdata]["path"]);
	googleMapPath[latlongs]
]  

runkeepercookeddata[args___]:=(debugPrint["runkeeper failed 1"->{args}];$Failed )

(* Send Message *)
runkeepersendmessage[___]:=$Failed

(*** Service specific utilites ****)
(* "Sat, 21 Aug 2010 22:31:20 +0000" *)
getallparameters[str_]:=DeleteCases[Flatten[{"Parameters","PathParameters","BodyData","MultipartData"}/.runkeeperdata[str]],
	("Parameters"|"PathParameters"|"BodyData"|"MultipartData")]

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

formatDate[date_]:=DateString[date, {"Year", "Month", "Day"}]
readDate[date_,form_:DateObject]:=form[date]
readTime[sec_,form_:TimeObject]:=form[{0,0,sec}]

toRunkeeperDate[date_]:=DateString[date, {"Year", "-", "Month", "-", "Day"}]
fixdates[params_]:=params/.HoldPattern[Rule][a:("noEarlierThan"|"noLaterThan"|"modifiedNoEarlierThan"|"modifiedNoLaterThan"),date_]:>Rule[a,toRunkeeperDate[date]]


getID[uri_]:=With[{split=StringSplit[uri,"/"]},
	ToExpression[Last[split]]
]

(******* parse user data ************)
parseuserdata[data_,"BackgroundActivities"]:=With[{items=data["Items"]},
	debugPrint["data"->data];
	(items/.{
		formatvalue["Uri"->getID],
		formatvalue["Timestamp"->readDate]}
		)/."Uri"->"ActivityID"
]
parseuserdata[data_,"FitnessActivities"]:=With[{items=data["Items"]},
	debugPrint["data"->data];
	assoc/@((items/.{
		formatvalue["Uri"->getID],
		createquantity["Duration"->"Seconds"],
		createquantity["TotalDistance"->"Meters"],
		formatvalue["StartTime"->readDate]}
		)/."Uri"->"ActivityID")
]
parseuserdata[data_,"StrengthTrainingActivities"]:=With[{items=data["Items"]},
	debugPrint["data"->data];
	assoc/@((items/.{
		formatvalue["Uri"->getID],
		createquantity["Duration"->"Seconds"],
		formatvalue["StartTime"->readDate]}
		)/."Uri"->"ActivityID")
]
parseuserdata[data_,"Sleep"]:=With[{items=data["Items"]},
	debugPrint["data"->data];
	(items/.{
		formatvalue["Uri"->getID],
		formatvalue["Timestamp"->readDate],
		createquantity["TotalSleep"->"Minutes"],
		createquantity["Rem"->"Minutes"],
		createquantity["Deep"->"Minutes"],
		createquantity["Light"->"Minutes"],
		createquantity["Awake"->"Minutes"]}
		)/.{"Uri"->"SleepID","Rem"->"REM"}
]
parseuserdata[data_,"Nutrition"]:=With[{items=data["Items"]},
	debugPrint["data"->data];
	(items/.{
		formatvalue["Uri"->getID],
		formatvalue["Timestamp"->readDate],
		createquantity["Fat"->"Gram"],
		createquantity["Calories"->"Gram"],
		createquantity["Carbohydrates"->"Gram"],
		createquantity["Fiber"->"Gram"],
		createquantity["Protein"->"Gram"],
		createquantity["Sodium"->"Milligram"],
		createquantity["Water"->"Fluid Ounces"],
		createquantity["Awake"->"Minutes"]}
		)/.{"Uri"->"NutritionID"}
]
parseuserdata[data_,"GeneralMeasurements"]:=With[{items=data["Items"]},
	debugPrint["data"->data];
	(items/.{
		formatvalue["Uri"->getID],
		formatvalue["Timestamp"->readDate],
		createquantity["Systolic"->"nnHG"],
		createquantity["Diastolic"->"Gram"],
		createquantity["TotalCholesterol"->"mg/dL"],
		createquantity["Hdl"->"mg/dL"],
		createquantity["Ldl"->"mg/dL"],
		createquantity["VitaminD"->"ng/dL"]}
		)/.{"Uri"->"MeasurementID","Hdl"->"HDL","Ldl"->"LDL"}
]
parseuserdata[data_,"Diabetes"]:=With[{items=data["Items"]},
	debugPrint["data"->data];
	(items/.{
		formatvalue["Uri"->getID],
		formatvalue["Timestamp"->readDate]}
		)/.{"Uri"->"DiabetesID"}
]
parseuserdata[data_,"Team"]:=With[{items=data["Items"]},
	debugPrint["data"->data];
	(items/.{formatvalue["Url"->getID]}
		)/."Url"->"TeamID"
]
parseuserdata[data_,"Weight"]:=With[{items=data["Items"]},
	debugPrint["data"->data];
	(items/.{
		formatvalue["Uri"->getID],
		createquantity["Weight"->"Kilograms"],
		formatvalue["Timestamp"->readDate]}
		)/."Uri"->"WeightID"
]
parseuserdata[data_,"Records"]:=Module[{},
	Cases[data,_?(Total["Value"/.Lookup[#,"Stats",{{"Value"->0}}]/."Value"->0]=!=0&)]
]
parseuserdata[data_,_]:=data

getgeolocation[data_]:=If[FreeQ[data,"Latitude"]||FreeQ[data,"Longitude"],data,
	Join[FilterRules[data,Except["Latitude"|"Longitude"|"Altitude"]],{"GeoPosition"->GeoPosition[{"Latitude","Longitude","Altitude"}/.data/."Altitude"->0]}]]

getgeolocations[data_]:=getgeolocation/@data

End[] (* End Private Context *)

SetAttributes[{},{ReadProtected, Protected}];

System`Private`RestoreContextPath[];

(* Return three functions to define oauthservicedata, oauthcookeddata, oauthsendmessage  *)
{OAuthClient`runkeeperdata,OAuthClient`runkeepercookeddata,OAuthClient`runkeepersendmessage}
