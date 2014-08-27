BeginPackage["ProcessLink`"];

System`StartProcess;
System`RunProcess;
System`KillProcess;
System`ProcessConnection;
System`ProcessInformation;
System`ProcessStatus;
System`ProcessObject;
System`WriteLine;
System`Processes;
System`$SystemShell;

System`EndOfBuffer;
System`ReadString;
System`ReadLine;

Begin["`Private`"];

Clear[ReadString, startProcess, StartProcess, RunProcess, ProcessReadBuffer, GetString, ReadString, GetLine, ReadLine];


(*
(*uncomment the following to test in older environments where
associations are still named collections:*)
Association[x_][y_] := Collection[x][y];
Unprotect[Normal];
Normal[Association[x_]] := x;
*) 

safeStringTake[l_, n_] := If[Abs[n] > StringLength[l], l, StringTake[l, n]]
safeStringDrop[l_, n_] := If[Abs[n] > StringLength[l], "", StringDrop[l, n]]

$SystemShell = If[StringFreeQ[$System, "Windows"],
	"/bin/sh",
	"cmd"
];

$RunningProcesses = {};
addRunningProcess[rp_] := ($RunningProcesses = Flatten[{$RunningProcesses, rp}];);
removeRunningProcesses[rps_] := ($RunningProcesses = Complement[$RunningProcesses, rps]);  

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
setupLogFile = LibraryFunctionLoad[$LibraryName, "setupLogFile", {"UTF8String"}, "Void"];

environmentToList[environment_Association] :=
	StringJoin[ToString[StringTrim[#]] & /@ {#[[1]], "=", #[[2]]}] & /@ Normal[environment]

StartProcess::tooManyArgs = "Too many arguments.";

startProcess[commands_List, environment_Association, currentDirectory:(_String|None)] :=
Module[{commands2, runid, args, retid, currDir, envList, process},
	envList = environmentToList[environment];
	(*this is hacky; we remove the quotes on windows, the arguments have to be parsed by the OS, so quotes on commands aren't harmless *)
	commands2 = If[!StringFreeQ[$System, "Windows"], StringReplace[#, "\"" -> ""]& /@ commands, commands];
	(* we find the function in "runs" which has the right number of arguments, and we call it *)
	runid = Length[Join[commands2, envList]];
	currDir = If[currentDirectory === None, "", currentDirectory];
	If[runid <= maxRunArgs + 1,
		args = Flatten[{Length[envList], currDir, commands2, envList}];
		retid = runs[[runid]] @@ args;
		If[!NumberQ[retid],
			$Failed,
			process = ProcessObject[retid];
			addRunningProcess[process];
			process
		]
		,
		Message[StartProcess::tooManyArgs]; $Failed
	]
]

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

StartProcess[args__] /; (Message[RunProcess::argt, RunProcess, Length[{args}], 1, 2]; False) := None 

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

	all = Association[{"ExitCode" -> iGetExitValue[process], "Output" -> read}];
	If[return === All, all, all[return]]
]

Options[RunProcess] = {
	"ProcessEnvironment" -> Association[{}],
	"CurrentDirectory" -> None
};

RunProcess[command_, opts:OptionsPattern[]] := RunProcess[{command}, opts]

RunProcess[commands_List, opts:OptionsPattern[]] :=
	runProcess[commands, OptionValue["ProcessEnvironment"], OptionValue["CurrentDirectory"], False, None, All]

RunProcess[command_, inputexpr_, ret: (All | _String) : All, opts:OptionsPattern[]] :=
	RunProcess @@ {{command}, inputexpr, ret, opts} (* using @@ to get rid of a pointless Workbench warning *)

RunProcess[commands_List, inputexpr_, ret: (All | _String) : All, opts:OptionsPattern[]] :=
	runProcess[commands, OptionValue["ProcessEnvironment"], OptionValue["CurrentDirectory"], True, inputexpr, ret]

RunProcess[args__] /; (Message[RunProcess::argb, KillProcess, Length[{args}], 1, 3]; False) := None 

KillProcess[ProcessObject[p_]] := killProcess[p, False];

KillProcess[args__] /; (Message[KillProcess::argx, KillProcess, Length[{args}]]; False) := None

ProcessConnection[pr : ProcessObject[p_], "StandardInput"] := getCachedStream[pr, "Input"]

ProcessConnection[pr : ProcessObject[p_], "StandardOutput"] := getCachedStream[pr, "Output"]

ProcessConnection[pr : ProcessObject[p_], "StandardError"] := getCachedStream[pr, "Error"]

ProcessConnection[args__] /; (Message[ProcessConnection::argr, ProcessConnection, Length[{args}], 2]; False) := None

ProcessInformation[pr : ProcessObject[p_]] := Association[{"ExitCode" -> ProcessInformation[pr, "ExitCode"]}]

ProcessInformation[pr : ProcessObject[p_], "ExitCode"] := iGetExitValue[pr]

ProcessInformation[args__] /; (Message[ProcessInformation::argt, ProcessInformation, Length[{args}], 1, 2]; False) := None

ProcessObject /: Import[p : ProcessObject[_], args___] := Import[getCachedStream[p, "Input"], args]
 
ProcessObject /: Read[p : ProcessObject[_], args___] := Read[getCachedStream[p, "Input"], args]

ProcessObject /: Write[p : ProcessObject[_], args___] := Write[getCachedStream[p, "Output"], args]
 
ProcessStatus[ProcessObject[id_]] := If[hasFinishedQ[id], "Finished", "Running"]
 
ProcessStatus[pr : ProcessObject[_], r_] := (ProcessStatus[pr] === r)

ProcessStatus[args__] /; (Message[ProcessStatus::argt, ProcessStatus, Length[{args}], 1, 2]; False) := None

ProcessObject /: WriteString[pr : ProcessObject[_], args___] := BinaryWrite[pr, args]

WriteLine[st_, str_String] := WriteString[st, str <> "\n"] 

WriteLine[args__] /; (Message[WriteLine::argrx, WriteLine, Length[{args}], 2]; False) := None

Processes[] := (
	removeRunningProcesses[Select[$RunningProcesses, ProcessStatus[#] === "Finished" &]];
	$RunningProcesses
)

Processes[args__] /; (Message[Processes::argrx, Processes, Length[{args}], 0]; False) := None

(* non-blocking binary read list, but can return EOF (output is _String | EOF)  *)
BinaryReadListEOF[st_] := Module[{l},
	l = BinaryReadList[st, "Character8", Infinity, "AllowIncomplete" -> True];
	If[l =!= {},
		l,
		l = BinaryRead[st, "Character8", "AllowIncomplete" -> True];
		Switch[l,
			Block, {},
			EndOfFile, l,
			_String, {l},
			_, {}
		]
	]
]

(* the cache always contains a string *)
Clear[$StreamCache];
$StreamCache[_] := "";

popStreamCache[st_InputStream] := With[{c = $StreamCache[st]},
	Quiet[Unset[$StreamCache[st]], Unset::norep];
	c
]

popStreamCache[pr_ProcessObject] := popStreamCache[getCachedStream[pr, "Input"]]

popStreamCache[___] := $Failed

pushStreamCache[st_InputStream, value_String] := With[{c = $StreamCache[st]},
	$StreamCache[st] = c <> value;
]

pushStreamCache[pr_ProcessObject] := pushStreamCache[getCachedStream[pr, "Input"]]

pushStreamCache[___] := $Failed

getStreamCache[st_InputStream] := $StreamCache[st]

getStreamCache[pr_ProcessObject] := getStreamCache[getCachedStream[pr, "Input"]]

getStreamCache[___] := $Failed

setStreamCache[st_InputStream, value_String] := (
	$StreamCache[st] = value;
)

setStreamCache[pr_ProcessObject, value_String] := setStreamCache[getCachedStream[pr, "Input"], value]

setStreamCache[___] := $Failed

(* read all from a stream (non-blocking), including its cache, and empty the cache
returns _String | EndOfFile | $Failed *)
cachedReadString[st_InputStream] := With[{l = BinaryReadListEOF[st]},
	If[l === EndOfFile,
		With[{c = popStreamCache[st]}, If[c =!= "", c, EndOfFile]],
		popStreamCache[st] <> StringJoin@@l
	]
]

cachedReadString[pr_ProcessObject] := cachedReadString[getCachedStream[pr, "Input"]]

cachedReadString[_] := $Failed

(* quickGetString, implementation for strings and alternatives that is quicker than the generic
case
genericGetString reads the stream every time, storing the result in a buffer, and then
uses Position in the full buffer to look for terminator. That is very wasteful, when the buffer
grows too big, Position gets slower and slower. quickGetString only looks for the terminator in
the last n bytes of the buffer + the most recently read characters, where n is the string length
of the terminator.
 *)
getMaxStringLength[_] := $Failed
getMaxStringLength[str_String] := StringLength[str]
getMaxStringLength[alt_Alternatives] := With[{lens = getMaxStringLength /@ List@@alt},
	If[Cases[lens, $Failed, {1}, 1] === {},
		Max[lens],
		$Failed
	]
]

ReadString::terminatorNotFound = "Specified terminator not found.";

(* higher performance implementation of ReadString, only works on terminators that are strings or string alternatives *)
quickGetString[st_, terminator_] := Module[
	{termlen, oldbuf = "", lastbuf = "", str, pos = {}, first = True},
	
	(* oldbuf = buffer of all text that was already checked
	lastbuf = some extra remaining chars that can still contain parts of the terminator
	str = most recently read buffer *)
	
	termlen = getMaxStringLength[terminator];
	
	While[pos === {},
		If[first,
			str = popStreamCache[st];
			first = False;
			,
			str = With[{l = BinaryReadListEOF[st]}, If[l === EndOfFile, l, StringJoin@@l]]
		];
		If[!StringQ[str],
			lastbuf = oldbuf <> lastbuf;
			Return[If[lastbuf === "", str, Message[ReadString::terminatorNotFound]; lastbuf]]
		];
		lastbuf = lastbuf <> str;
		str = "";
		
		pos = StringPosition[lastbuf, terminator];
		If[pos === {},
			oldbuf = oldbuf <> safeStringDrop[lastbuf, -(termlen - 1)];
			lastbuf = safeStringTake[lastbuf, -(termlen - 1)];
		];
	];

	setStreamCache[st, StringDrop[lastbuf, pos[[1, 2]]]];
	oldbuf <> StringTake[lastbuf, pos[[1, 1]] - 1]
]

(* simple generic version of ReadString, can work for any delimiter, but it is most likely super
inefficient, except for simple terminators like EndOfFile and EndOfBuffer *)
genericGetString[st_, terminator_] := Module[{str, buff = "", pos = {}},
	While[pos === {},
		str = cachedReadString[st];
		If[!StringQ[str],
			Return[If[buff === "", str, Message[ReadString::terminatorNotFound]; buff]]
		];
		buff = buff <> str;
		pos = StringPosition[buff, terminator];
	];

	setStreamCache[st, StringDrop[buff, pos[[1, 2]]]];
	StringTake[buff, pos[[1, 1]] - 1]
]

genericGetString[st_, EndOfBuffer] := cachedReadString[st]

genericGetString[st_, EndOfFile] := Module[{all, res = ""},
	all = cachedReadString[st];
	
	While[res =!= EndOfFile,
		res = BinaryReadListEOF[st];
		If[res =!= EndOfFile, all = all <> StringJoin@@res];
	];
	all
]

(*
cases are:
- single character: read, look for character, store on cache
- more than one character: read, check for all those characters in last elems of cache + all new elems
	(maybe these two last ones can be combined)
- new line
- alternatives?
- ensure the actual performance of the case by case code is better than of a single monolithic function
- careful, if the buffer is not empty, some of the specialized functions cannot be called
	(eg. processreadbuffer)
*)
(* reads full contents until terminator is found; by default term is EndOfFile so it reads the stream fully *)
ReadString[st:(_ProcessObject | _InputStream), terminator_:EndOfFile] := With[{termlen = getMaxStringLength[terminator]},
	If[NumberQ[termlen],
		quickGetString[st, terminator],
		genericGetString[st, terminator]
	]
]

ReadString[args___] /; (Message[ReadString::argrx, ReadString, Length[{args}], 2]; False) := None

(*  reads line until completion, blocks; can return EndOfFile if no more to read *)
ReadLine[st:(_ProcessObject | _InputStream)] := Quiet[ReadString[st, ("\n" | "\r\n")], ReadString::terminatorNotFound]

ReadLine[args___] /; (Message[ReadLine::argx, ReadLine, Length[{args}]]; False) := None

SetupLogFile[path_] := setupLogFile[path] 

End[];

EndPackage[];
