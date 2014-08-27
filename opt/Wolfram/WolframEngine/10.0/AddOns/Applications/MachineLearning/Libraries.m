Begin["MachineLearning`Libraries`"];

MachineLearning`Libraries`Function::usage = "MachineLearning`Libraries`Function[file, name, signature] loads name from a library called file";
MachineLearning`Libraries`Compile::usage = "MachineLearning`Libraries`Compile[args, body] is like Compile";

MachineLearning`Libraries`Function[file_, args___] := 
	Block[{$LibraryPath = MachineLearning`Libraries`Directory[]},
		LibraryFunctionLoad[file <> ToString[Round @ $VersionNumber], args]
	];

MachineLearning`Libraries`$Name = System`Private`$InputFileName;
		
MachineLearning`Libraries`Directory[] := Module[
	{file},
	file = FindFile["MachineLearning`LibraryResources`Dummy`"];
	If[StringQ[file],
		file = ParentDirectory @ DirectoryName @ file;
	,
		file = FindFile["MachineLearning`"];
		If[StringQ[file],
			file = ParentDirectory @ DirectoryName @ file;
		,
			file = MachineLearning`Libraries`$Name;
			If[StringQ[file] && file =!= "",
				file = DirectoryName @ file,
				Throw[Missing["DataScience/LibraryResources is missing"]]
			]
		]
	];
	FileNameJoin[{file, "LibraryResources", ToLowerCase[$SystemID]}]
];

	
SetAttributes[MachineLearning`Libraries`Compile, HoldAll];

MachineLearning`Libraries`Compile[args_, body_, opts___Rule] := 
	If[TrueQ[True],
		With[{opts2 = Sequence @@ DeleteCases[{opts}, CompilationTarget -> "C"]},
			Compile[args, body, opts2]
		],
		Compile[args, body, opts]
	];

End[]
