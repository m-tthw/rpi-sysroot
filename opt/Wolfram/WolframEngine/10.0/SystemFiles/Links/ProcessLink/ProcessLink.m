BeginPackage["ProcessLink`"];

System`StartProcess;
System`RunProcess;
System`KillProcess;
System`ProcessConnection;
System`ProcessInformation;
System`ProcessStatus;
System`ProcessObject;

Begin["`Private`"];

(*
(*uncomment the following to test in older environments where
associations are still named collections:*)
Association[x_][y_] := Collection[x][y];
Unprotect[Normal];
Normal[Association[x_]] := x;
*) 

ProcessLink::cantSetup = "Can't setup ProcessLink.";

If[!StringQ[$LibraryName], $LibraryName = "libProcessLink"];

If[LibraryLoad[$LibraryName] === $Failed, Message[ProcessLink::cantSetup]];

maxRunArgs = 20;
runs = Quiet[
	Map[
		LibraryFunctionLoad[$LibraryName, "run", Join[{Integer, "UTF8String"}, "UTF8String" & /@ Range[#]], Integer] &,
		Range[maxRunArgs + 1]
	],
	LibraryFunction::overload];
killProcess = LibraryFunctionLoad[$LibraryName, "killProcess", {Integer, "Boolean"}, "Boolean"];
freeProcess = LibraryFunctionLoad[$LibraryName, "freeProcess", {Integer}, "Void"];
hasFinishedQ = LibraryFunctionLoad[$LibraryName, "hasFinishedQ", {Integer}, "Boolean"];
getExitValue = LibraryFunctionLoad[$LibraryName, "getExitValue", {Integer}, Integer];
hasExitValue = LibraryFunctionLoad[$LibraryName, "hasExitValue", {Integer}, "Boolean"];
waitFor = LibraryFunctionLoad[$LibraryName, "waitFor", {Integer}, "Void"];

environmentToList[environment_Association] :=
	StringJoin[ToString[StringTrim[#]] & /@ {#[[1]], "=", #[[2]]}] & /@ Normal[environment]
	

Clear[startProcess];

startProcess[commands_List, environment_Association, currentDirectory:(_String|None)] :=
Module[{runid, args, retid, currDir, envList},
	envList = environmentToList[environment];
	(* we find the function in runs which has the right number of arguments, and we call it *)
	runid = Length[Join[commands, envList]];
	currDir = If[currentDirectory === None, "", currentDirectory];
	If[runid <= maxRunArgs + 1,
		args = Flatten[{Length[envList], currDir, commands, envList}];
		retid = runs[[runid]] @@ args;
		If[!NumberQ[retid], $Failed, ProcessObject[retid]]
		,
		Message[Run2::tooManyArgs]; $Failed
	]
]

Clear[StartProcess];

Options[StartProcess] = {
	"ProcessEnvironment" -> Association[{}],
	"CurrentDirectory" -> None
};

StartProcess[command_String, opts:OptionsPattern[]] :=
	startProcess[{command}, OptionValue["ProcessEnvironment"], OptionValue["CurrentDirectory"]]

StartProcess[{command_String, args: (_String | (_String -> _String))...}, opts:OptionsPattern[]] := With[
	{cmdlist = ToString /@ Prepend[Flatten[List @@@ List[args]], command]},
	startProcess[cmdlist, OptionValue["ProcessEnvironment"], OptionValue["CurrentDirectory"]]
]

Clear[StreamCache];
StreamCache[___] := None;
getCachedStream[p_, type_] := If[StreamCache[p, type] =!= None,
	StreamCache[p, type]
	,
	With[{fun = Switch[
		type,
		"Input", iGetInputStream,
		"Output", iGetOutputStream,
		"Error", iGetErrorStream
	]},
		StreamCache[p, type] = fun[p];
		StreamCache[p, type]
	]
]

iGetOutputStream::cantGetStream = "Can't get output stream."; 
iGetInputStream::cantGetStream = iGetOutputStream::cantGetStream; 
iGetErrorStream::cantGetStream = iGetOutputStream::cantGetStream; 

iGetOutputStream[ProcessObject[p_]] := If[NumberQ[p], 
	OpenWrite["in:" <> ToString[p], Method -> "RunPipe", BinaryFormat -> True], 
	Message[iGetOutputStream::cantGetStream]; $Failed]

iGetInputStream[ProcessObject[p_]] := If[NumberQ[p], 
	OpenRead["out:" <> ToString[p], Method -> "RunPipe", BinaryFormat -> True], 
	Message[iGetInputStream::cantGetStream]; $Failed]

iGetErrorStream[ProcessObject[p_]] := If[NumberQ[p], 
	OpenRead["err:" <> ToString[p], Method -> "RunPipe", BinaryFormat -> True], 
	Message[iGetErrorStream::cantGetStream]; $Failed]

ProcessObject /: BinaryReadList[p : ProcessObject[_], args___] := BinaryReadList[getCachedStream[p, "Input"], args]

ProcessObject /: BinaryRead[p : ProcessObject[_], args___] := BinaryRead[getCachedStream[p, "Input"], args]

ProcessObject /: BinaryWrite[p : ProcessObject[_], args___] := BinaryWrite[getCachedStream[p, "Output"], args]

iGetExitValue[ProcessObject[id_]] := If[hasExitValue[id], getExitValue[id], None]

iWaitFor[pr : ProcessObject[id_]] := (waitFor[id]; iGetExitValue[pr])

runProcess[commands_List, environment_Association, currentDirectory_, useInputExpr_, inputexpr_, return_: All] := Module[
	{process, read, all},
	
	process = StartProcess[commands, "ProcessEnvironment" -> environment, "CurrentDirectory" -> currentDirectory];
	If[!MatchQ[process, ProcessObject[___]], Return[$Failed]];
	If[useInputExpr =!= None, BinaryWrite[process, ToString[inputexpr]]];
	(* todo: this shouldn't wait forever; just a little bit and iterate, to make it interruptable; also, read should read
	unlimited chars *)
	iWaitFor[process];
	read = BinaryReadList[process, "Character8", Infinity, "AllowIncomplete" -> True];
	If[ListQ[read], read = StringJoin @@ read, read = ""];

	(*Module[{sterr},(*todo, comment out*)
		sterr = BinaryReadList[ProcessConnection[process, "StandardError"], "Character8", Infinity, "AllowIncomplete" -> True];
		If[ListQ[sterr] && sterr =!= {},
			Print[Style[StringJoin @@ sterr, Blue]];
		];
	];*)
	
	all = Association[{"ExitCode" -> iGetExitValue[process], "Output" -> read}];
	If[return === All, all, all[return]]
]

Clear[RunProcess];

Options[RunProcess] = {
	"ProcessEnvironment" -> Association[{}],
	"CurrentDirectory" -> None
};

RunProcess[command_, opts:OptionsPattern[]] := RunProcess[{command}, opts]

RunProcess[commands_List, opts:OptionsPattern[]] :=
	runProcess[commands, OptionValue["ProcessEnvironment"], OptionValue["CurrentDirectory"], False, None, All]

RunProcess[command_, inputexpr_, ret: (All | _String) : All, opts:OptionsPattern[]] :=
	RunProcess[{command}, inputexpr, ret, opts]

RunProcess[commands_List, inputexpr_, ret: (All | _String) : All, opts:OptionsPattern[]] :=
	runProcess[commands, OptionValue["ProcessEnvironment"], OptionValue["CurrentDirectory"], True, inputexpr, ret]
 

KillProcess[ProcessObject[p_]] := (killProcess[p, False];)

ProcessConnection[pr : ProcessObject[p_], "StandardInput"] := getCachedStream[pr, "Input"]

ProcessConnection[pr : ProcessObject[p_], "StandardOutput"] := getCachedStream[pr, "Output"]

ProcessConnection[pr : ProcessObject[p_], "StandardError"] := getCachedStream[pr, "Error"]

ProcessInformation[pr : ProcessObject[p_], "ExitCode"] := iGetExitValue[pr]

ProcessObject /: Import[p : ProcessObject[_], args___] := Import[getCachedStream[p, "Input"], args]
 
ProcessObject /: Read[p : ProcessObject[_], args___] := Read[getCachedStream[p, "Input"], args]

ProcessObject /: Write[p : ProcessObject[_], args___] := Write[getCachedStream[p, "Output"], args]
 
ProcessStatus[ProcessObject[id_]] := If[hasFinishedQ[id], "Finished", "Running"]
 
ProcessStatus[pr : ProcessObject[_], r_] := (ProcessStatus[pr] === r)


End[];

EndPackage[];
