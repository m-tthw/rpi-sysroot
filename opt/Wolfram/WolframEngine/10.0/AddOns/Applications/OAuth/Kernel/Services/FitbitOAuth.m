System`Private`NewContextPath[{"OAuthClient`","System`"}];

(* declare functions that will be used by the OAuth paclet *)
OAuthClient`fitbitdata;
OAuthClient`fitbitcookeddata;
OAuthClient`fitbitsendmessage;


Begin["FitbitOAuthDump`"] (* Begin Private Context *) 

(******************************* Fitbit *************************************)

(* Authentication information *)

fitbitdata[]={
        "ServiceName"       -> "Fitbit",
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
 		"Gets"				-> Join[{"UserDataGrid", "WeightGrid", "MeasurementsGrid", "FoodGrid", "FoodList",
 			"ActivityData",
 			"ActivityGrid","SleepData", "SleepGrid", "SleepList","SleepCalendar","SleepDensityTimeline","UserData"},
 			fitbittimeseriesproperties,fitbittimeseriesPlots],
 		"Posts"				-> {"RecordWeight"},
 		"RawGets"			->	Join[{"RawUserData", "RawWeight", "RawBodyFat", "RawMeasurements", 
				"RawFood", "RawWater", "RawActivity", "RawSleep", "RawFoodUnit"},
 			rawfitbittimeseriesproperties],
 		"RawPosts"			->	{"RawLogWeight","RawLogBodyFat","RawLogMeasurement","RawLogFood"},
    	"RequestFormat"		-> "Headers",
 		"Information"		-> "A service for sending and receiving data from a Fitbit account"
    }
    
(* a function for importing the raw data - usually json or xml - from the service *)
fitbitimport[$Failed]:=Throw[$Failed]
fitbitimport[json_String]:=With[{res=ImportString[json,"JSON"]},
	If[res===$Failed,Throw[$Failed]];
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

fitbitimport[raw_]:=raw

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
 
     
fitbitdata["RawUserData"] = {
        "URL"				->	"http://api.fitbit.com/1/user/-/profile.json",
        "HTTPSMethod"		-> "GET",
       	"Headers" 			-> {"Accept-Language"->"en_US"},  (* Can be "en_US", "en_GB", or "" *)
        "ResultsFunction"	-> fitbitimport
    }
 
fitbitdata["RawMeasurements"] = {
        "URL"				->	(ToString@StringForm["http://api.fitbit.com/1/user/-/body/date/`1`.json", formatDate[##]]&),
       	"Headers" 			-> {"Accept-Language"->"en_US"},  (* Can be "en_US", "en_GB", or "" *)
        "PathParameters" 	-> {"Date"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> fitbitimport
    }
       
fitbitdata["RawWeight"] = {
        "URL"				->	(ToString@
        	StringForm["http://api.fitbit.com/1/user/-/body/log/weight/date/`1`.json", formatDate[##]]&),
       	"Headers" 			-> {"Accept-Language"->"en_US"},  (* Can be "en_US", "en_GB", or "" *)
        "PathParameters" 	-> {"Date"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> fitbitimport
    }

fitbitdata["RawBodyFat"] = {
        "URL"				->	(ToString@StringForm["http://api.fitbit.com/1/user/-/body/log/fat/date/`1`.json", formatDate[##]]&),
        "PathParameters" 	-> {"Date","StartDate","EndDate"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> fitbitimport
    }
           
fitbitdata["RawFood"] = {
        "URL"				->	(ToString@StringForm["http://api.fitbit.com/1/user/-/foods/log/date/`1`.json", formatDate[##]]&),
        "HTTPSMethod"		-> "GET",
        "PathParameters" 	-> {"Date","StartDate","EndDate"},
        "ResultsFunction"	-> fitbitimport
    }
           
fitbitdata["RawWater"] = {
        "URL"				->	(ToString@StringForm["http://api.fitbit.com/1/user/-/foods/log/water/date/`1`.json", formatDate[##]]&),
        "HTTPSMethod"		-> "GET",
        "PathParameters" 	-> {"Date"},
        "ResultsFunction"	-> fitbitimport
    }
    
fitbitdata["RawActivity"] = {
        "URL"				->	(ToString@StringForm["http://api.fitbit.com/1/user/-/activities/date/`1`.json", formatDate[##]]&),
        "PathParameters" 	-> {"Date"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	-> fitbitimport
    }
fitbitdata["RawSleep"] = {
        "URL"				->	(ToString@StringForm["http://api.fitbit.com/1/user/-/sleep/date/`1`.json", formatDate[##]]&),
        "PathParameters" 	-> {"Date"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	->fitbitimport
    } 
           
fitbitdata["RawFoodUnit"] = {
        "URL"				->	"http://api.fitbit.com/1/foods/units.json",
        "PathParameters" 	-> {},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	->fitbitimport
    } 
        
fitbitdata["RawLogFood"] = {
        "URL"				->	"http://api.fitbit.com/1/user/-/foods/log.json",
        "BodyData"			-> {"foodID","foodName","calories","brandName","mealTypeId","unitId","amount","date"},
   		"RequiredParameters"-> {"mealTypeId","unitId","amount","date"},
        "HTTPSMethod"		-> "POST",
        "ResultsFunction"	->fitbitimport
    } 
                   
fitbitdata["RawLogWeight"] = {
        "URL"				->	"http://api.fitbit.com/1/user/-/body/log/weight.json",
       	"Headers" 			-> {"Accept-Language"->"en_US"},  (* Can be "en_US", "en_GB", or "" *)
        "BodyData"			-> {"weight","date"},
   		"RequiredParameters"-> {"weight","date"},
        "HTTPSMethod"		-> "POST",
        "ResultsFunction"	->fitbitimport
    } 
             
fitbitdata["RawLogBodyFat"] = {
        "URL"				->	"http://api.fitbit.com/1/user/-/body/log/fat.json",
        "BodyData"			-> {"fat","date"},
   		"RequiredParameters"-> {"fat","date"},
        "HTTPSMethod"		-> "POST",
        "ResultsFunction"	->fitbitimport
    } 
    
fitbitdata["RawLogMeasurement"] = {
        "URL"				->	"http://api.fitbit.com/1/user/-/body.json",
       	"Headers" 			-> {"Accept-Language"->"en_US"},
        "BodyData"			-> {"bicep","calf","chest","fat","forearm","hips","neck","thigh","waist","weight","date"},
   		"RequiredParameters"-> {"bicep"|"calf"|"chest"|"fat"|"forearm"|"hips"|"neck"|"thigh"|"waist"|"weight"},
        "HTTPSMethod"		-> "POST",
        "ResultsFunction"	->fitbitimport
    } 
    
fitbittimeseriesproperties={"CaloriesInTimeSeries", "WaterTimeSeries", "CaloriesTimeSeries", 
   "CaloriesBMRTimeSeries", "StepsTimeSeries", "DistanceTimeSeries", 
   "FloorsTimeSeries", "ElevationTimeSeries", 
   "MinutesSedentaryTimeSeries", "MinutesLightlyActiveTimeSeries", 
   "MinutesFairlyActiveTimeSeries", "MinutesVeryActiveTimeSeries", 
   "ActivityCaloriesTimeSeries", (* "CaloriesTimeSeries", "StepsTimeSeries", 
   "DistanceTimeSeries", "MinutesSedentaryTimeSeries", 
   "MinutesLightlyActiveTimeSeries", "MinutesFairlyActiveTimeSeries", 
   "MinutesVeryActiveTimeSeries", "ActivityCaloriesTimeSeries",  *)
   "Bedtimes", "TimeInBedTimeSeries", 
   "MinutesAsleepTimeSeries", "AwakeningsCountTimeSeries", 
   "MinutesAwakeTimeSeries", "MinutesToFallAsleepTimeSeries", 
   "MinutesAfterWakeupTimeSeries", "SleepEfficiencyTimeSeries", 
   "WeightTimeSeries", "BMITimeSeries", "BodyFatTimeSeries"}
   
fitbittimeseriespaths={"foods/log/caloriesIn", "foods/log/water", "activities/calories", 
   "activities/caloriesBMR", "activities/steps", "activities/distance", 
   "activities/floors", "activities/elevation", 
   "activities/minutesSedentary", "activities/minutesLightlyActive", 
   "activities/minutesFairlyActive", "activities/minutesVeryActive", 
   "activities/activityCalories", (* "activities/tracker/calories" ,
   "activities/tracker/steps","activities/tracker/distance", 
   "activities/tracker/floors", "activities/tracker/elevation", 
    "activities/tracker/minutesSedentary", 
   "activities/tracker/minutesLightlyActive", 
  "activities/tracker/minutesFairlyActive", 
   "activities/tracker/minutesVeryActive", *)
 (*  "activities/tracker/activityCalories",*)"sleep/startTime", 
   "sleep/timeInBed", "sleep/minutesAsleep", "sleep/awakeningsCount", 
   "sleep/minutesAwake", "sleep/minutesToFallAsleep", 
   "sleep/minutesAfterWakeup", "sleep/efficiency", "body/weight", 
   "body/bmi", "body/fat"};

rawfitbittimeseriesproperties=("Raw"<>#&/@fitbittimeseriesproperties)

fitbittimeseriesPlots=DeleteCases[StringReplace[fitbittimeseriesproperties, "TimeSeries" -> "Plot"],"Bedtimes"]

fitbitdata[prop:(Alternatives@@rawfitbittimeseriesproperties)] := {
        "URL"				->	(ToString@StringForm["http://api.fitbit.com/1/user/-/"<>
        	(prop/.Thread[rawfitbittimeseriesproperties->fitbittimeseriespaths])<>"/date/`1`/`2`.json", formatDate[#1],formatDate[#2]]&),
        "PathParameters" 	-> {"StartDate", "EndDate"},
   		"RequiredParameters"-> {"StartDate", "EndDate"},
        "HTTPSMethod"		-> "GET",
        "ResultsFunction"	->fitbitimport
    } 
   
fitbitdata[___]:=$Failed
(****** Cooked Properties ******)
  
(* cooked data queries 
	Cooked queries call at least one of the raw queried underneath but add pre and post processing to make the function easier to use and the result clearer.
*)  

fitbitcookeddata[prop_,id_,rule_Rule, rest___]:=fitbitcookeddata[prop,id,{rule}, rest]
fitbitcookeddata[prop_,id_]:=fitbitcookeddata[prop,id,{}]

fitbittimeseriesdata[prop_,id_,args_]:=Block[{rawprop, params,start, end,rawdata},
	rawprop="Raw"<>prop;
	params=filterparameters[args,getallparameters[rawprop]];
	If[FreeQ[params,#],Message[ServiceExecute::nparam,#1];Throw[$Failed]]&/@{"StartDate"};
	{start, end} = {"StartDate", "EndDate"} /. Flatten[{args}]/."EndDate"->DateString[];
	If[!TrueQ[DateDifference[start, end] > 0], Throw[$Failed]];
	rawdata=OAuthClient`rawoauthdata[id,rawprop,Join[{"StartDate"->formatDate[start],"EndDate"->formatDate[end]},params]];
	rawdata=fitbitimport[rawdata];
	If[rawdata===$Failed,Return[$Failed]];
	rawdata
];

formattimeseriesdata[data_,_]:={"dateTime","value"}/.((data/.{("value"->val_):>("value"->ToExpression[val]),
		("dateTime"->date_):>("dateTime"->readDate[date,AbsoluteTime])}))

fitbitcookeddata["Bedtimes",id_,args_] :=Module[
	{data, dates, times},
	data=fitbittimeseriesdata["Bedtimes",id, args];
	data=Normal[data][[1,-1]];
	data=DeleteCases[data,{___,"value"->"",___}];
	dates="dateTime"/.data;
	times="value"/.data;
	MapThread[DateObject[#1<>" "<>#2]&,{dates,times}]
]

fitbitcookeddata[prop:(Alternatives@@fitbittimeseriesproperties),id_,args_] :=Module[
	{data},
	data=fitbittimeseriesdata[prop,id, args];
	data=formattimeseriesdata[Normal[data][[1,-1]],prop];
	TimeSeries[data]
]

fitbitcookeddata[prop:(Alternatives@@fitbittimeseriesPlots),id_,args_] :=Module[
	{data, opts},
	opts=FilterRules[args,Except["StartDate"|"EndDate"]];
	data=fitbittimeseriesdata[StringReplace[prop,"Plot"->"TimeSeries"],id, args];
	data=formattimeseriesdata[Normal[data][[1,-1]],prop];
	DateListPlot[data, Sequence@@opts, Filling->Axis]
]

fitbitsleeplist[id_,args_]:=Module[
	{rawdata, startDate, endDate, ndays, n, day},
	If[FreeQ[args,#],Message[ServiceExecute::nparam,#1];Throw[$Failed]]&/@{"StartDate"};
	{startDate, endDate} = {"StartDate", "EndDate"} /. Flatten[{args}]/."EndDate"->DateString[];
	If[TrueQ[DateDifference[startDate, endDate] < 0], Throw[$Failed]];
	ndays = First[DateDifference[startDate, endDate, "Day"]];
	rawdata = Table[day = DatePlus[startDate, n];
 		OAuthClient`rawoauthdata[id, "RawSleep", "Date" -> day], {n, 0, ndays}];
	If[rawdata === {}, Return[{}]];
	rawdata=DeleteCases[(Normal@fitbitimport[#]&)/@rawdata,FreeQ[#,"sleep"]&];
	"sleep" /. rawdata
]

fitbitcookeddata["SleepList",id_,args_]:=Module[
	{rawdata},
	rawdata=fitbitsleeplist[id, args];
	debugPrint["SleepList rawdata"->rawdata];
	Map[Cases[#, _[
    "awakeningsCount" | "duration" | "efficiency" | 
     "minutesAfterWakeup" | "minutesAsleep" | "minutesAwake" | 
     "minutesToFallAsleep" | "startTime" | 
     "timeInBed", _], {1}] &, rawdata, {2}]/.{formatvalue["startTime"->(readDate[#]&)],
     	createquantity["minutesAfterWakeup"->"Minutes"],createquantity["minutesAsleep"->"Minutes"],
     	createquantity["minutesAwake"->"Minutes"],createquantity["minutesToFallAsleep"->"Minutes"]
     	,createquantity["timeInBed"->"Minutes"],createquantity["duration"->"Seconds"]}/.HoldPattern[Rule[a_String,b_]]:>Rule[camelcase[a],b]
     
]
         
fitbitcookeddata["SleepCalendar",id_,args_]:=Module[
	{data, starts, durations, effs,minEfficiency,maxEfficiency,ndays, t0, i},
	debugPrint["SleepCalendar"];
	data=fitbitsleeplist[id, args];
	debugPrint["SleepCalendar data"->data];
	If[Flatten[data]==={},Return[Graphics[]]];
	data=Cases[data,_?(! FreeQ[#, "startTime"] &)];
	starts = Check[readDate[#, Identity] & /@ 
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
]      


fitbitcookeddata[prop:("SleepData"|"SleepGrid"),id_,args_]:=Module[
	{rawdata,params,data},
	params=filterparameters[args,getallparameters["RawSleep"]];
	rawdata=OAuthClient`rawoauthdata[id,"RawSleep",params];
	data=fitbitimport[rawdata];
	data=Normal[data];
	debugPrint["data"->data];
	If[data==={},Return[{}]];
	Switch[prop,
		"SleepGrid",
		addtitles[Replace[FlatGrid[{
	        	{"sleep",{"efficiency","startTime","duration","minutesAsleep","minutesAwake","minutesAfterWakeup","awakeningsCount","timeInBed"}},
	        	{"summary",{"totalSleepRecords","totalMinutesAsleep","totalTimeInBed"}}},
	        	data,
	        	{formatvalue["startTime"->(readDate[#]&)],createquantity["minutesAsleep"->"Minutes"],
	        	createquantity["minutesAwake"->"Minutes"],createquantity["minutesAfterWakeup"->"Minutes"],createquantity["timeInBed"->"Minutes"]}],{a_} -> a, {3}],
	        	{"Parameter","Value"}],
	   	"SleepData",
	   	If[data=!={},
	    		data=data/.{HoldPattern["sleep"->{sl___}]:>sl,HoldPattern["summary"->{su___}]:>su};
	    		data=FilterRules[data, {"efficiency","startTime","duration","minutesAsleep","minutesAwake","minutesAfterWakeup","awakeningsCount",
	    			"timeInBed","totalSleepRecords","totalMinutesAsleep","totalTimeInBed"}];
	    		assoc@Replace[data, HoldPattern[Rule[a_, b_]] :> Rule[OAuthClient`camelcase[a], b], Infinity]
	    		,
	    		{}
	    		
	    	]
	]
]    
 
         
fitbitcookeddata["SleepDensityTimeline",id_,args_]:=Module[
	{data, starts, durations, effs,minEfficiency,maxEfficiency,ndays, t0},
	data=fitbitsleeplist[id, args];
	debugPrint["data"->data];
	data=DeleteCases[data,{}];
	starts =readDate[#,Identity] & /@ 
   		 DeleteCases[Flatten[("startTime" /. data)],"startTime"];
	debugPrint["starts"->starts];
	durations = Select[ToExpression[Flatten["timeInBed" /. data]], NumberQ];
	debugPrint["durations"->durations];
	effs = Select[ToExpression[Flatten["efficiency" /. data]], NumberQ];
	debugPrint["effs"->effs];
	minEfficiency = Min[effs];
	maxEfficiency = Max[effs];
	ndays = Ceiling[First[DateDifference["StartDate"/. Flatten[{args}], "EndDate"/. Flatten[{args}], "Day"]]];
	data=Transpose[{starts,durations, effs}];
	debugPrint["data 10"->data];
	t0=Min[AbsoluteTime/@starts];
	Graphics[
		makeSleepBox[#, minEfficiency, maxEfficiency, AbsoluteTime[#[[1,1;;3]]],True]&/@data,
		Axes -> True,
		AxesOrigin->{0,0},
		Ticks -> {Range[-2, 24], {1}}
	]
]


fitbitcookeddata[prop:("UserDataGrid"|"UserData"),id_,args_]:=Module[
	{rawdata,params,data, grid},
	params=filterparameters[args,getallparameters["RawMeasurements"]];
	rawdata=OAuthClient`rawoauthdata[id,"RawUserData",params];
	data=Normal@fitbitimport[rawdata];
	data=data/.{formatvalue["dateOfBirth"->(readDate[#]&)],formatvalue["memberSince"->(readDate[#]&)],
        	createquantity["height"->"Inches"],createquantity["strideLengthRunning"->"Inches"],createquantity["strideLengthWalking"->"Inches"],
        	createquantity["weight"->"Pounds"]};
    grid=FlatGrid[{{"user",{"city", "country", "dateOfBirth", "displayName", "gender", "height", "locale", 
        	"memberSince", "state", "strideLengthRunning","strideLengthWalking", "timezone", "weight"}}},
        	data];
    If[prop==="UserData",
    	Association[Rule @@@ Normal[grid]]
    	,	
		addtitles[grid,{"User Data",SpanFromLeft}]
    ]
]
   
fitbitcookeddata["WeightGrid",id_,args_]:=Module[
	{rawdata,params,data,date},
	params=filterparameters[Join[args,{"Date"->DateString[]}],getallparameters["RawWeight"]];
	rawdata=OAuthClient`rawoauthdata[id,"RawWeight",params];
	data=fitbitimport[rawdata];
	data=data["weight"];
	If[data==={},Return[Missing["NotAvailable"]]];
	data=First[data];
	date=Check[DateObject[DateList@StringJoin[{"date"," ","time"}/.data]],""];
	data=FilterRules[data,Except["date"|"time"]];
	addtitles[FlatGrid[{{"date","bmi","weight"}},Join[{"date"->date},data]/.{createquantity["weight"->"Pounds"]}],{"Weight",SpanFromLeft}]
]    

fitbitcookeddata["MeasurementsGrid",id_,args_]:=Module[
	{rawdata,params,data},
	params=filterparameters[Join[args,{"Date"->DateString[]}],getallparameters["RawMeasurements"]];
	rawdata=OAuthClient`rawoauthdata[id,"RawMeasurements",params];
	data=fitbitimport[rawdata];
	addtitles[FlatGrid[{"bicep", "bmi", "calf", "chest", "fat", 
		"forearm", "hips", "neck", "thigh", "waist", "weight"},data["body"],
		Join[{createquantity["weight"->"Pounds"]},createquantity[#->"Inches"]&/@{"chest","calf","bicep","forearm","hips","neck","thigh","waist"}]],
			{"Measurements","Date"/.params/."Date"->""}]
]    
    
fitbitcookeddata[prop:("FoodGrid"|"FoodList"),id_,args_]:=Module[
	{rawdata,params,data},
	params=filterparameters[args,getallparameters["RawFood"]];
	rawdata=OAuthClient`rawoauthdata[id,"RawFood",Join[params,{"Date"->DateString[]}]];
	data=fitbitimport[rawdata];
	debugPrint["data"->data];
	data=Lookup[data,"foods",{}];
	debugPrint["data"->data];
	Switch[prop,
		"FoodGrid",
		OAuthClient`GridList[{{"loggedFood",{"amount", "brand", "foodId", "mealTypeId", "name"}},
	        	{"nutritionalValues",{"calories", "carbs", "fat", "fiber", "protein", "sodium"}}},data],
	    "FoodList",
	    	If[data=!={},
	    		data=data/.HoldPattern["loggedFood"->{lf___}]:>lf;
	    		data=FilterRules[#, {"logId", "logDate", "nutritionalValues", "amount","foodID","unit","brand","calories","mealTypeId","name","units"}]&/@data;
	    		assoc/@Replace[data, HoldPattern[Rule[a_, b_]] :> Rule[OAuthClient`camelcase[a], b], Infinity]
	    		,
	    		{}
	    		
	    	]
	    	
	        	
	]
]   


fitbitcookeddata[prop:("ActivityData"|"ActivityGrid"),id_,args_]:=Module[
	{rawdata,data,params,data1,data2,fields},
	fields={"activityCalories","caloriesBMR","caloriesOut","fairlyActiveMinutes", 
		"lightlyActiveMinutes", "marginalCalories", "sedentaryMinutes", "steps", "veryActiveMinutes"};
	params=filterparameters[args,getallparameters["RawActivity"]];
	rawdata=OAuthClient`rawoauthdata[id,"RawActivity",params];
	data=fitbitimport[rawdata];
	debugPrint["data"->data];
	data=data["summary"];
	data1=(camelcase[#]->getdata[data,#,Null])&/@fields;
	data2=parsedistances[getdata[data,"distances",{}]];
	data=Join[data1,data2];
	Switch[prop,
		"ActivityGrid",
		addtitles[prettygrid[List@@@data],{"Activity Data","Date"/.params/."Date"->DateString[{"Year","/","Month","/","Day"}]}],
		"ActivityData",
		data	
	]
]   

parsedistances[dists_]:=Cases[l:{___,HoldPattern[Rule["activity",act_]],___}:>(camelcase[act]->"distance"/.l),Infinity]

(* depricated 
fitbitcookeddata["TimeSeriesActivityScores",id_,args_]:=Module[
	{rawdata},
	Switch[Length[{args}],
		1,
		rawdata=OAuthClient`rawoauthdata[id,"AllActivityScores",args],
		2,
		rawdata=OAuthClient`rawoauthdata[id,"ActivityScores",args]
	];
	TemporalData[{"dateTime", "value"} /. ("activities-activeScore" /. (
        	ImportString[rawdata, "JSON"]/.{formatvalue["dateTime"->DateList],formatvalue["value"->ToExpression]}))]
]   
fitbitcookeddata["TimeSeriesActivityCalories",id_,args_]:=Module[
	{rawdata,params},
	params=filterparameters[args,getallparameters["RawFood"]];
	rawdata=OAuthClient`rawoauthdata[id,"AllActivityCalories",params];
	TemporalData[{"dateTime", "value"} /. ("activities-activityCalories" /. (
        	ImportString[rawdata, "JSON"]/.{formatvalue["dateTime"->DateList],formatvalue["value"->ToExpression]}))]
]   
 
  *)
fitbitcookeddata["RecordWeight",id_,args_]:=Module[
	{rawdata, params,data},
	params=filterparameters[Join[args,{"date"->DateString[]}],getallparameters["RawLogWeight"]];
	params=params/.HoldPattern[Rule[a_,b_?(!StringQ[#]&)]]:>Rule[a,ToString[b]];
	params=params/.HoldPattern[Rule["date",date_]]:>Rule["date",formatDate[date]];
	rawdata=OAuthClient`rawoauthdata[id,"RawLogWeight",params];
	debugPrint["rawdata"->rawdata];
	data=fitbitimport[rawdata];
	debugPrint["data"->data];
	getdata[data["weightLog"],"weight"]
]    

makeSleepBox[data_, minEfficiency_, maxEfficiency_, t0_, timelineQ_:False] :=
	Module[{day, duration, start, efficiency, xmin, xmax, end, tQ=Boole[timelineQ]},
		day = data[[1,1;;3]];
		start = data[[1]];
		xmin = First@DateDifference[day, start, "Hour"];
		duration = data[[2]];
		end=DatePlus[start,{duration,"Minute"}];
		xmax = First@DateDifference[day, end, "Hour"];
		efficiency = Rescale[data[[3]], {maxEfficiency, minEfficiency}];
		day=(AbsoluteTime[day]-t0)/86400;
	debugPrint["makeSleepBox day"->day];
		{
			Opacity[0.95-tQ/2],
			ColorData["TemperatureMap"][efficiency],
			Tooltip[
				If[xmax>24,
					If[xmax>48, Throw[$Failed]];
					{Rectangle[{0, day+1.95-tQ}, {xmax-24, day + 1.05-tQ}],
					Rectangle[{xmin, day+.95}, {24, day + 0.05}]}
					,
					Rectangle[{xmin, day+.95}, {xmax, day + 0.05}]
				],
				StringJoin[Riffle[ToString /@ DateDifference[start, end, "Hour"], " "], "s"]
			]
		}
	]






fitbitcookeddata[args___]:=(debugPrint["fitbit failed 1"->{args}];$Failed )

(* Send Message *)
fitbitsendmessage[___]:=$Failed

(*** Service specific utilites ****)
(* "Sat, 21 Aug 2010 22:31:20 +0000" *)
getallparameters[str_]:=DeleteCases[Flatten[{"Parameters","PathParameters","BodyData","MultipartData"}/.fitbitdata[str]],
	("Parameters"|"PathParameters"|"BodyData"|"MultipartData")]

readDate[date_,form_:DateObject]:=form[DateList[{date,{"Year", "-", "Month", "-", "Day"}}]]/;StringFreeQ[date,"T"]
readDate[date_,form_:DateObject]:=form[DateList[{date,{"Year", "-", "Month", "-", "Day", "T", "Hour", ":", "Minute", ":","Second", ".", "Millisecond"}}]]


formatDate[]:=formatDate[DateList[]]
formatDate[per:("1d"|"7d"|"30d"|"1w"|"1m"|"3m"|"6m"|"1y"|"max"|"today")]:=per
formatDate[date_]:=Quiet[DateString[DateList[date], {"Year", "-", "Month", "-", "Day"}],DateList::arg]
formatDate[dates__]:=StringJoin[Riffle[formatDate[#]&/@{dates},"/"]]

End[] (* End Private Context *)

SetAttributes[{},{ReadProtected, Protected}];

System`Private`RestoreContextPath[];

(* Return three functions to define oauthservicedata, oauthcookeddata, oauthsendmessage  *)
{OAuthClient`fitbitdata,OAuthClient`fitbitcookeddata,OAuthClient`fitbitsendmessage}
