(* Autoloader of Parallel Computing Toolkit *)

(* :Package Version: 1.0 ($Id: sysload.m,v 1.50 2012/11/19 16:40:15 maeder Exp $) *)

(* :Summary:
   this is invoked from sysinit.m to integrate PCT into Mathematica
*)

(* :Discussion:
   this sets up autoloading of PCT and initializes the user's configuration.
   Packages are read only later when the declared symbols are first evaluated.
   This happens through Parallel/Kernel/autoload.m
*)

Begin["Parallel`Private`"]

`mainCtx= "System`";
`devCtx = "Parallel`Developer`";

`mainNames = Flatten[{
	mainCtx<>#&/@{"LaunchKernels", "Kernels", "AbortKernels", "CloseKernels", "$KernelCount", "$KernelID"},
	mainCtx<>#&/@{"ParallelSubmit","WaitAll","WaitNext", "CriticalSection"},
	mainCtx<>#&/@{"ParallelEvaluate","ParallelNeeds","DistributeDefinitions","DistributedContexts"},
	mainCtx<>#&/@{"Parallelize", "ParallelTry"},
	mainCtx<>#&/@{"SetSharedVariable", "SetSharedFunction", "$SharedVariables", "$SharedFunctions", "UnsetShared"},
	mainCtx<>#&/@{"EvaluationObject", "KernelObject"},
	mainCtx<>"Parallel"<>#&/@{"Combine","Map","Table","Sum","Product","Do","Array"},
	{}
}];

(* when changing the symbol set above, also have a look at autoload.m *)

(* symbols that should exist, but do not cause autoloading *)

`nonLoadNames = mainCtx<>#&/@ {"$ConfiguredKernels", "$DistributedContexts"};

(* developer context main functions *)

`devNames = devCtx<>#& /@ {
    "ClearKernels", "CloseError", "ConcurrentEvaluate", "ConnectKernel", "created",
    "dequeued", "DoneQ", "EvaluationCount", "finished", "KernelFromID",
    "KernelID", "KernelName", "KernelStatus", "LaunchDefaultKernels", "LaunchKernel",
    "Load", "ParallelDispatch", "ParallelPreferences", "Process",
    "ProcessID", "ProcessResult", "ProcessState", "queued", "QueueRun", "Receive",
    "ReceiveIfReady", "ResetQueues", "running", "Scheduling", "Send",
    "SendBack", "SetQueueType", "SubKernel", "$DistributedDefinitions", "ClearDistributedDefinitions",
    "$InitCode", "$LoadFactor", "$NotReady", "$ParallelCommands",
    "$Queue", "$QueueLength", "$QueueType"
};

(* user interface code *)

`uiNames = "Parallel`Palette`"<>#& /@ {
	"menuStatus", "tabPreferences", "buttonConfigure"
};

(* language version number, to detect master/subkernel version mismatch *)
`$ParallelLanguageVersion = 8.0;

(* remember path for later autoloading *)
Parallel`Private`tooldir = ParentDirectory[ParentDirectory[DirectoryName[$InputFileName]]]

Which[ (* what is our role? *)
	$LicenseType === "Player" || TrueQ[Parallel`Static`$player], (* Player product *)
		Parallel`Static`$player = True; SetAttributes[Parallel`Static`$player,{Protected,Locked}];
		Parallel`Static`$silent = True;
		Parallel`Static`$persistentPrefs = False;
		Get["Parallel`Kernel`noparinit`"];
		,
	!TrueQ[System`Parallel`$SubKernel], (* master kernel, Mathematica or PlayerPro *)
		If[System`Parallel`$SubKernel=!=False, System`Parallel`$SubKernel=False]; (* we are NOT a subkernel *)
		If[$LicenseType === "PlayerPro" || TrueQ[Parallel`Static`$playerPro], (* PlayerPro product *)
			Parallel`Static`$playerPro = True; SetAttributes[Parallel`Static`$playerPro,{Protected,Locked}];
			Parallel`Static`$persistentPrefs = False;
			Parallel`Static`$Profile = "PlayerPro";
		  , (* else full Mathematica *)
		  	If[!ValueQ[Parallel`Static`$persistentPrefs], Parallel`Static`$persistentPrefs = True ];
		];
		Package`DeclareLoad[ Join[Symbol/@mainNames, Symbol/@devNames, Symbol/@uiNames],
			"Parallel`Kernel`autoload`", Package`ExportedContexts -> {}
		];
		(* create nonload symbols *)
		Symbol/@nonLoadNames;
		(* to trigger conditional code inside Parallel/Kernel/init.m, whenever this is read later *)
		Parallel`Static`$sysload = True;
		Parallel`Static`$silent = True;
		Parallel`Static`$loaded = False;
		(* Set up SystemInformation[] *)
		Get["Parallel`SysInfo`"];
		,
	True, (* subkernel, by default *)
		Get["Parallel`Kernel`subinit`"];
]

(* $ConfiguredKernels hack; must be outside the previous Which[] *)
If[!System`Parallel`$SubKernel && !ValueQ[$ConfiguredKernels] && !TrueQ[Parallel`Static`$player] && !TrueQ[Parallel`Static`$playerPro],
	$ConfiguredKernels:=(Clear[$ConfiguredKernels];$KernelID;$ConfiguredKernels)
];

End[]

Null
