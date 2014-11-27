(* Mathematica package *)
BeginPackage["CloudObject`"];

System`ScheduledTask;

Begin["`Private`"];

Needs["JLink`"];

ScheduledTask::notask = "Argument 1 in CloudDeploy is not a regonized ScheduledTask specification.";
ScheduledTask::nostart = "Unable to start ScheduledTask.";
ScheduledTask::nostop = "Unable to stop ScheduledTask `1`.";
ScheduledTask::sched = "`1` is not a recognized scheduling time specification.";
ScheduledTask::nouri = "Unrecognized uri specificiation `1`.";
ScheduledTask::noavil = "Scheduling tasks remotely is not yet available.";
ScheduledTask::norm = "Unable to remove ScheduledTask `1`.";
ScheduledTask::listing = "Unable to obtain ScheduledTask listing.";
ScheduledTask::nonext = "Unable to obtain next scheduled run time for ScheduledTask `1`.";
ScheduledTask::restr = "Use restricted under current subscription.";
ScheduledTask::argu = "Unrecognized scheduling specification.";
ScheduledTask::upda = "Unable to update scheduled task.";
ScheduledTask::crea = "Unable to create scheduled task.";

Unprotect[ScheduledTask,CloudObject];
SetAttributes[ScheduledTask,{HoldFirst,ReadProtected}];

ScheduledTask/:CloudDeploy[task_ScheduledTask,OptionsPattern[]] := With[
	{res=Catch[iCloudDeployScheduledTask[task],$tag]}, res]

ScheduledTask/:CloudDeploy[task_ScheduledTask, uri_String, OptionsPattern[]] := With[
	{res=Catch[iCloudDeployScheduledTask[task, CloudObject[uri]],$tag]}, res]

ScheduledTask/:CloudDeploy[task_ScheduledTask, co_CloudObject, OptionsPattern[]] := With[
	{res=Catch[iCloudDeployScheduledTask[task,co],$tag]}, res]


schedulingMetaInformation[ScheduledTask[task_, schedule_], uri_, uuid_, tz_] := Module[
	{json, sched = If[TrueQ[$CloudEvaluation], Identity, CloudEvaluate][
 CloudSystem`Scheduling`Private`standardizeSchedule[
  timespec2Cron[schedule]]]},
	json = {
		"name" -> uri,
    	"uuid" -> uuid,
    	"cronSchedule" -> First[sched[[2]]],
    	"repeatCount" -> ToString[Last[sched]],
    	"userID" -> ToString[$WolframUUID],
    	"taskData" -> ToString[Unevaluated[task], InputForm],
    	"taskType" -> "script",
    	"timeZone" -> tz
	};
	ExportString[json, "JSON", "Compact" -> True]
]
schedulingMetaInformation[___] := ""

handleSchedulingResponse[response_] := Switch[response,
	{$Failed,"restr"}, Message[ScheduledTask::restr];$Failed,
	{$Failed,"argu"}, Message[ScheduledTask::argu];$Failed,
	{$Failed,"upda"}, Message[ScheduledTask::upda];$Failed,
	{$Failed,"crea"}, Message[ScheduledTask::crea];$Failed,
	_, Message[ScheduledTask::nostart];$Failed
]

iCloudDeployScheduledTask[st:ScheduledTask[expr_, sched_]] := iCloudDeployScheduledTask[st, CloudObject[]]

iCloudDeployScheduledTask[st:ScheduledTask[expr_, sched_], obj:CloudObject[uri_String, ___]] := With[
	{schedule = timespec2Cron[sched], name = GetNameFromURI[uri],
		fun = If[TrueQ[$CloudEvaluation], Identity, CloudEvaluate], uuid = GetUUID[obj],
    
	(* Associate a time zone (not an offset) with the task. I default to system setting/cloud 
	 * preference, but this can easily be driven with an option so that tasks need not be scheduled
	 * in their owners' time zones. *)
		tz = If[TrueQ[$CloudEvaluation],
			If[ValueQ[CloudSystem`Private`$TimeZoneID], CloudSystem`Private`$TimeZoneID, Null],
			(* else *)fetchSystemTimeZone[]
			]
	},


	If[SameQ[schedule, $Failed],
		Message[ScheduledTask::sched, sched];
		Throw[$Failed, $tag]
	];
	Check[
		iCloudPut[st, obj, "application/vnd.wolfram.expression.task", SaveDefinitions -> True],
		Throw[$Failed,$tag]
	];
	Check[
		SetOptions[obj, MetaInformation -> "__Scheduling" -> schedulingMetaInformation[st, name, uuid, tz]],
		Throw[$Failed,$tag]
	];
	
	With[{res = fun[
			CloudSystem`Scheduling`StartScheduledCloudTask[
				CloudSystem`Scheduling`CreateScheduledCloudTask[ExecuteScheduledTask[obj], name, schedule, uuid]
			]
		]},
		
		If[SameQ[res, True],
			obj,
			(* else *)
			handleSchedulingResponse[res];
			(* DeleteFile[obj]? *)
			Throw[$Failed, $tag]
		]
	]
]

(*
 * Returns a string id (e.g. "America/Chicago"). 
 * TODO: test on multiple platforms for correspondence with $TimeZone.
 *)
fetchSystemTimeZone[] := Module[{},
	InstallJava[];
	LoadJavaClass["java.util.TimeZone"];
	java`util`TimeZone`getDefault[]@getID[]
];	

iCloudDeployScheduledTask[ScheduledTask[args___],___] := (ArgumentCountQ[ScheduledTask,Length[Hold[args]],2,2];$Failed)
iCloudDeployScheduledTask[___] := $Failed

toSchedule[n_String] := Module[{cron},
    cron = StringSplit[n];
    If[Length@cron < 3 || Length@cron > 7, Return[$Failed]];
    cron = cron /. {
        (*{s_, m_, h_, dom_, m_, dow_, y_}:>{s, m, h, dom, m, dow, y},
        {s_, m_, h_, dom_, m_, dow_}:>{s, m, h, dom, m, dow, "*"},
        {h_, dom_, m_, dow_, y_}:>{"*", "*", h, dom, m, dow, y},
        {h_, dom_, m_, dow_}:>{"*", "*", h, dom, m, dow, "*"}*)
        
        
        {s_, m_, h_, dom_, mo_, dow_, y_}:>{s, m, h, dom, mo, dow, y}, (* quartz expression *)
        {m_, h_, dom_, mo_, dow_, y_}:>{"*", m, h, dom, mo, dow, y}, (* classic cron with optional year *)
        {m_, h_, dom_, mo_, dow_}:>{"*", m, h, dom, mo, dow, "*"}, (* classic cron *)
        {h_, dom_, mo_, dow_}:>{"*", "*", h, dom, mo, dow, "*"}
        
        
    };
    StringJoin[Riffle[ToUpperCase@cron, " "]]
]

toSchedule[___] := $Failed

current[spec_] := DateString[DateList[], spec]
(* We can remove this and instead use dowToCron *)
currentDOW[] := With[{date = DateList[]},(*TODO: figure out if these are actually accurate*)
  Which[
   DayMatchQ[date, Sunday], "1",
   DayMatchQ[date, Monday], "2",
   DayMatchQ[date, Tuesday], "3",
   DayMatchQ[date, Wednesday], "4",
   DayMatchQ[date, Thursday], "5",
   DayMatchQ[date, Friday], "6",
   DayMatchQ[date, Saturday], "7",
   True, "*"
   ]]

$TimeSpecs = {
   "Hourly" :> "* * * * ? *",
   "Daily" :> StringJoin["* ", current[{"HourShort"}], " * * ? *"],
   "Weekly" :> StringJoin["* ", current[{"HourShort"}], " ? * ", currentDOW[], " *"],
   "Monthly" :> StringJoin["* ", current[{"HourShort", " ", "DayShort"}], " * ? *"],
   "Yearly" :> StringJoin["* ", current[{"HourShort", " ", "DayShort", " ", "MonthShort"}], " ? *"]
   };

(*validateTimeSpec[{n_Integer?Positive, di:(_Integer|_DirectedInfinity)}] := {n, di}
validateTimeSpec[__] := $Failed
validateCronSpec[{cron_, di_DirectedInfinity}] := toSchedule[cron]*)

$AvailableTimeSpecs = First /@ $TimeSpecs;

resolveSecs = {
    {a_?NumberQ, "Second"} :> Round[a],
    {a_?NumberQ, "Minute"} :> Round[a*60],
    {a_?NumberQ, "Hour"} :> Round[a*3600],
    {a_?NumberQ, "Day"} :> Round[a*3600*24],
    {a_?NumberQ, "Week"} :> Round[a*3600*24*7],
    {a_?NumberQ, "Month"} :> Round[a*3600*24*30], (* 30 days *)
    {a_?NumberQ, "Quarter"} :> Round[a*3600*24*90], (* 90 days *)
    {a_?NumberQ, "Year"} :> Round[a*3600*24*365] (* 365 days *)
};


(* CRON Output *)
timespec2Cron[string_String] /;  MemberQ[$AvailableTimeSpecs, string] := {string /. $TimeSpecs, Infinity}
timespec2Cron[d_DateObject] := With[{spec=StringJoin["* ", System`Utilities`DateObjectToCronSpecification[d]]},If[StringQ[spec],{spec, Infinity},$Failed]]
timespec2Cron[cron_String] := With[{spec=toSchedule[cron]},If[StringQ[spec],{spec, Infinity},$Failed]]
timespec2Cron[{spec_String, di_DirectedInfinity}] := Module[{tmp}, 
    tmp = timespec2Cron[spec];
    tmp[[2]] = di;
    tmp]
    
(* List[Integer, DirectedInfinity] Output *)
timespec2Cron[HoldPattern[q_Quantity]]:= If[CompatibleUnitQ[q, "Seconds"] ,{QuantityMagnitude[UnitConvert[q, "Seconds"]] /. resolveSecs, Infinity},$Failed] 
timespec2Cron[n_Integer?Positive] := {n,Infinity}
timespec2Cron[{spec_}] := timespec2Cron[{spec, 1}]
timespec2Cron[{n_Integer?Positive, di_DirectedInfinity}] := Module[{tmp}, 
    tmp = timespec2Cron[n];
    tmp[[2]] = di;
    tmp]

(* ambiguous *)
timespec2Cron[{spec : Except[_List], count_Integer}] := With[{s = timespec2Cron[spec]}, {First[s], count} /; s =!= $Failed]
timespec2Cron[{start_DateObject, timespec_, end_DateObject}] := {start, timespec2Cron[timespec], end}
timespec2Cron[{start_DateObject, timespec_}] := {start, timespec2Cron[timespec]}
timespec2Cron[{timespec_, end_DateObject}] := {timespec2Cron[timespec], end}

(* failures *)
timespec2Cron[_DateObject, _DateObject] := Message[ScheduledTask::sched, "2 argument timespec is ambiguous. Please use a different form."]
timespec2Cron[__] := $Failed

CloudObject /: StopScheduledTask[task_CloudObject, OptionsPattern[]] := With[
	{res = Catch[iCloudStopScheduledTask[task], $tag]},
	res
]

iCloudStopScheduledTask[co:CloudObject[uri_String, ___]] := With[
	{uuid = GetUUID[uri]},
	(*If[MatchQ[#, $Failed | _CloudSystem`Scheduling`StopScheduledCloudTask],
		Message[ScheduledTask::nostop, co];
		$Failed,
		(* else *)
		co
	] & [*)
		If[TrueQ[$CloudEvaluation], Identity, CloudEvaluate][
			CloudSystem`Scheduling`StopScheduledCloudTask[uuid]
		]

]

iCloudStopScheduledTask[st_,OptionsPattern[]] := (Message[ScheduledTask::nostop,st];$Failed)

CloudObject /: StartScheduledTask[task_CloudObject, OptionsPattern[]] := With[
	{res = Catch[iCloudResumeScheduledTask[task], $tag]}, res
]

iCloudResumeScheduledTask[co:CloudObject[uri_String, ___]] := With[
	{uuid = GetUUID[uri]},
	If[MatchQ[#, $Failed | _CloudSystem`Scheduling`ResumeScheduledCloudTask],
		handleSchedulingResponse[#],
		(* else *)
		co
	] & [
		If[TrueQ[$CloudEvaluation], Identity, CloudEvaluate][
			CloudSystem`Scheduling`ResumeScheduledCloudTask[uuid]
		]
	]
]

iCloudResumeScheduledTask[st_, OptionsPattern[]] := handleSchedulingResponse[$Failed]

(*
 * Equivalent to "Run now" in web interface.
 *)
CloudObject /: RunScheduledTask[task_CloudObject,OptionsPattern[]] := With[
	{res = Catch[iCloudRunScheduledTask[task], $tag]},
	res
]

iCloudRunScheduledTask[co:CloudObject[uri_String, ___]] := With[
	{uuid = GetUUID[uri]},
	If[MatchQ[#, $Failed | _CloudSystem`Scheduling`ExecuteScheduledCloudTask],
		handleSchedulingResponse[#],
		(* else *)
		co
	] & [
		If[TrueQ[$CloudEvaluation], Identity, CloudEvaluate][
			CloudSystem`Scheduling`ExecuteScheduledCloudTask[uuid]
		]
	]
]

iCloudRunScheduledTask[st_,OptionsPattern[]] := handleSchedulingResponse[$Failed]

CloudObject /: RemoveScheduledTask[task_CloudObject] := With[
	{res = Catch[iCloudRemoveScheduledTask[task], $tag]},
	res
]

iCloudRemoveScheduledTask[co:CloudObject[uri_String, ___]] := With[
	{uuid = GetUUID[uri]},
	If[MatchQ[#, $Failed | _CloudSystem`Scheduling`RemoveScheduledCloudTask], Message[ScheduledTask::norm, co]; $Failed, co] & [
		If[TrueQ[$CloudEvaluation], Identity, CloudEvaluate][
			CloudSystem`Scheduling`RemoveScheduledCloudTask[uuid]
		]
	]
]

iCloudRemoveScheduledTask[st_, OptionsPattern[]] := (Message[ScheduledTask::norm, st]; $Failed)

(*not documented; gets ScheduledTask out of CloudObject and runs it*)
CloudObject /: ExecuteScheduledTask[task_CloudObject] := With[
	{res = Catch[iCloudExecuteScheduledTask[task],$tag]}, res]

iCloudExecuteScheduledTask[co:CloudObject[uri_String, ___]]:= With[
	{task=Quiet[If[MatchQ[#,_ScheduledTask],#,$Failed]&[CloudGet[co]]]},
	If[UnsameQ[task,$Failed],First[task],handleSchedulingResponse[task]]
]

iCloudExecuteScheduledTask[st_, OptionsPattern[]] := handleSchedulingResponse[$Failed]
(*
 * Hybrid task listing
 *)
Unprotect[ScheduledTasks];
$cloudScheduledTasksFlag = True;
ScheduledTasks[] /; TrueQ[And[$CloudConnected, $cloudScheduledTasksFlag]] := Block[
	{$cloudScheduledTasksFlag = False},
	Join[ScheduledTasks[], iCloudScheduledTasks[]]
]

iCloudScheduledTasks[] := Module[
	{raw, med},
	raw = If[TrueQ[$CloudEvaluation], Identity, CloudEvaluate][
		CloudSystem`Scheduling`ScheduledCloudTasks[]
	];

	If[MatchQ[raw, $Failed | _CloudSystem`Scheduling`ScheduledCloudTasks],
		Message[ScheduledTask::listing]; {$Failed},
		(* else *)
		(* This sorts by next run date, soonest first. *)
		med = Reverse @ SortBy[raw, #[[4]] &];
		CloudObject[JoinURL[$CloudBase, "objects", First[#]]] & /@ med
	]
]

(* Not presently documented *)
CloudObject /: NextScheduledTaskTime[task_CloudObject] := With[
	{res = Catch[iCloudNextScheduledTaskTime[task], $tag]},
	res
]

iCloudNextScheduledTaskTime[co:CloudObject[uri_String, ___]] := Module[
	{uuid = GetUUID[uri]},
	If[MatchQ[#, $Failed | _CloudSystem`Scheduling`Private`scheduledJob],
		Message[ScheduledTask::nonext, co];
		$Failed,
		(* else *)
		If[TrueQ@#[[5]],
			(* Active *)
			#[[4]] /. {Null -> None, d_?NumericQ :> DateObject[d]},
			(* else *)
			None
		]
	] & [
		If[TrueQ[$CloudEvaluation], Identity, CloudEvaluate][
			CloudSystem`Scheduling`Private`scheduledJob[uuid]
		]
	]
]

iCloudNextScheduledTaskTime[st_, OptionsPattern[]] := (Message[ScheduledTask::nonext, st]; $Failed)

GetNameFromURI[uri_String] := With[{split=StringSplit[uri,"/"]},
	If[Length[split]<2,Message[ScheduledTask::nouri,uri];Throw[$Failed,$tag],Last[split]]
]

GetUUID[obj_CloudObject] := Module[{res},If[MatchQ[res=getCloudAndUUID[obj],{_,id_String}],Last[res],Throw[$Failed,$tag]]]
GetUUID[obj_String] := GetUUID[CloudObject[obj]]
GetUUID[___] := Throw[$Failed,$tag]


Protect[ScheduledTask,CloudObject, ScheduledTasks];

$Flag = False;

End[]

EndPackage[]
