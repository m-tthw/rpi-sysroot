Package["MachineLearning`"]


PackageScope["RandomForestClassifierFunction"]

RandomForestClassifierFunction::usage = "RandomForest[{X, Y}].
X feature matrix
Y is a list of integer in {1, 2, ..., nclass}"


PackageScope["RandomForestPredictorFunction"]

RandomForestPredictorFunction::usage = "RandomForest[{X, Y}].
X feature matrix
Y is a list of integer in {1, 2, ..., nclass}"


If[$SystemWordLength =!= 32,
	
giveData = MachineLearning`Libraries`Function["libRandomForest", "giveData", {{Real, 1, Automatic}, {Real, 1, Automatic}, Integer, Integer, {Integer, 1, Automatic}, Integer, Integer}, "Void"];
makeTree = MachineLearning`Libraries`Function["libRandomForest", "makeTree", {Integer}, {Integer, 2, Automatic}];
(*makeParallelTree = MachineLearning`Libraries`Function["libRandomForest", "makeParallelTree", {Integer}, "Void"];
getTree = MachineLearning`Libraries`Function["libRandomForest", "getTree", {Integer}, {Integer, 2, Automatic}];*)
releaseData = MachineLearning`Libraries`Function["libRandomForest", "releaseData", {Integer}, "Void"];
returnOOB = MachineLearning`Libraries`Function["libRandomForest", "returnOOB", {Integer}, {Real, 1, Automatic}];

];

viewTree[{features_, splits_, lidx_, ridx_}]:=Grid[Join[{features}, Map[If[#<-63, IntegerToFloat[-#], #]&, {splits, lidx, ridx}, {2}]]]

LibraryFunction::dataexist = "Data is already loaded. Release and load again.";
LibraryFunction::datanoexist = "There is no data loaded.";

FloatToInteger = MachineLearning`Libraries`Compile[{{float, _Real}}, Module[
	{mantissa, exponent, realmantissa},
	If[float == 0, Return[0]];
	exponent = IntegerPart[Log2[Abs[float]]]-1;
	realmantissa = float*2.^-exponent;
	mantissa = IntegerPart[2.^49*realmantissa];
	If[exponent < 0, exponent = - exponent + 2^10];
	If[mantissa < 0, mantissa = - mantissa + 2^51];
	mantissa*2^11+exponent
	],
	RuntimeAttributes -> {Listable}, 
	RuntimeOptions -> "Speed", 
	Parallelization -> True, 
	CompilationTarget -> "C"
];

IntegerToFloat = MachineLearning`Libraries`Compile[{{integer, _Integer}}, Module[
	{mantissa, exponent, realmantissa},
	mantissa = Quotient[integer, 2^11];
	exponent = integer - mantissa*2^11;
	If[mantissa > 2^51-1, mantissa = 2^51 - mantissa];
	If[exponent > 2^10-1, exponent = 2^10 - exponent];
	realmantissa = mantissa * 2.^-49;
	realmantissa*2.^exponent
	],
	RuntimeAttributes -> {Listable},
	RuntimeOptions -> "Speed",
	Parallelization -> True,
	CompilationTarget -> "C"
];

decodeCategories[fc_, arg_] := Block[
	{nums = (First/@Position[Reverse[ arg[[3]] ], 1]), pos},
	pos = Position[fc[[1, 1]], arg[[2]] ][[1, 1]];
	fc[[2, pos, 1]][[ nums ]]
]

(**********************   PREDICTOR      *******************************)

Predict::maxcat = Classify::maxcat = "With Method -> \"RandomForest\", nominal features should have less that 64 classes.";

Options[RandomForestPredictorFunction] = 
{
	"VariableSampleSize" -> Automatic,
	"LeafSize" -> Automatic,
	"TreeNumber" -> 100,
	(*"Parallel" -> False,*)
	PerformanceGoal -> Automatic,
	"LabelNames" -> Automatic,
	NominalVariables -> {},
	"CrossValidation"->Automatic,
	"BinaryEncoder" -> Preprocessor["Identity"]
};

RandomForestPredictorFunction[{X_, Y_}, opts:OptionsPattern[]] := Block[
	{variableTypes, trees, m, n, fullX, variableSampleSize, leafSize, sigma, evaluationdata, metadata, options},
		{m, n} = Dimensions[X];
		variableTypes = ConstantArray[1, n];
		Scan[Set[variableTypes[[#]], Max[X[[All, #]]]] &, OptionValue[NominalVariables]];
		If[Max[variableTypes] > 63, AbortMachineLearning["maxcat"];];
		
		If[OptionValue["VariableSampleSize"] === Automatic,
			variableSampleSize = Max[1, Floor[n/3]];,
			variableSampleSize = OptionValue["VariableSampleSize"];
		];
		If[!(1<=variableSampleSize<=n), AbortMachineLearning[]];

		leafSize = If[OptionValue["LeafSize"] === Automatic,
			Which[
				m > 20, 10
				,
				m > 10, m - 10
				,
				True, 1
			]
			,
			OptionValue["LeafSize"]
		];
		Quiet[releaseData[0]];
		(*If[OptionValue["Parallel"],
			giveData[fullX, Y, variableSampleSize, leafSize, variableTypes, 0, OptionValue["TreeNumber"]];
			makeParallelTree[0];
			trees = Table[getTree[i], {i, 0, OptionValue["TreeNumber"] - 1}];
			sigma = Mean[(Y - returnOOB[0])^2];
			releaseData[0];
			,*)
			Internal`WithLocalSettings[
				giveData[Flatten[Transpose[X]], Y, variableSampleSize, leafSize, variableTypes, 0, 1];
				,
				trees = Table[makeTree[0],{OptionValue["TreeNumber"]}];
				trees = Select[trees, # =!= {{},{},{},{}} &]; (*remove tree without nodes for now...*)
				If[Length[trees]<1, AbortMachineLearning[]];
				sigma = Sqrt[Mean[(Y - returnOOB[0])^2]];
				,
				releaseData[0];
			];
	(*
		];
*)

	options = Options[RandomForestPredictorFunction];
	options = DeleteDuplicates[Join[{opts}, options], First[#1] === First[#2] &];
	options = Replace[options,
		{
			("LeafSize"-> _) -> ("LeafSize" -> leafSize)
			,
			("VariableSampleSize"-> _) -> ("VariableSampleSize" -> variableSampleSize)
		},
		{1}
	];
	
	evaluationdata = {{Preprocessor["Identity"], Preprocessor["Identity"], Table[Except[_List], {n}]}, {trees}};
	metadata = {{{m, n}, {Table[{}, {n}]}, {"value", {Min[Y], Max[Y]}}}, {sigma, leafSize}};
	PredictorFunction["RandomForest", evaluationdata, metadata, options]

];

iPredictorFunction["RandomForest"][___][_, prop_] := AbortMachineLearning["naprop", prop];
iNonePredictorFunction["RandomForest"][___][prop_] := AbortMachineLearning["napropn", prop];
iNonePredictorFunction["RandomForest"][___][] := AbortMachineLearning["napropnn"];

iPredictorFunction["RandomForest"][eval_, _, _][x_] := Module[
	{preprocessory, trees},
	preprocessory = eval[[1, 2]];
	trees = eval[[2, 1]];
	preprocessory[Mean[leafValue[x, ##] & @@@ trees], "Inverse"]
];

iPredictorFunction["RandomForest"][eval_, _, _][x_, "Votes"] := Module[
	{preprocessory, trees},
	preprocessory = eval[[1, 2]];
	trees = eval[[2, 1]];
	preprocessory[#, "Inverse"] & /@ Transpose[leafValue[x, ##] & @@@ trees]
];

iPredictorFunction["RandomForest"][eval_, meta_, _][x_, "Distribution"] := Module[
	{preprocessory, trees, sigma, mus},
	preprocessory = eval[[1, 2]];
	trees = eval[[2, 1]];
	sigma = meta[[2, 1]];
	mus = preprocessory[Mean[leafValue[x, ##] & @@@ trees], "Inverse"];
	NormalDistribution[#, sigma] & /@ mus
];

iNonePredictorFunction["RandomForest"][_, _, options_]["Options"] := Sequence @@ options;

iNonePredictorFunction["RandomForest"][arg___]["PredictionPlot"] := PredictionPlot[PredictorFunction["RandomForest", arg]];
iNonePredictorFunction["RandomForest"][arg___][{"PredictionPlot", arg2___}] := PredictionPlot[PredictorFunction["RandomForest", arg], arg2];

iNonePredictorFunction["RandomForest"][{{preprocessorx_, preprocessory_, __}, {trees_, ___}}, _, _]["TreePlot", j_] := Block[
	{tree, nNodes, vertices},
	If[j > Length[trees], Print["Argument greater than TreeNumber. Tree doesn't exist."]; Return[]];
	tree = trees[[j]];
	nNodes = Length[tree[[1]]];
	vertices = Reap[
		MapIndexed[
			With[
				{t = #1, i = First[#2]},
				If[t[[3]] > 0,
					Sow[{i -> t[[3]]}, "nodes"];
					,
					Sow[{i -> t[[3]]}, "leaf"];
				];
				If[t[[4]] > 0,
					Sow[{i -> t[[4]]}, "nodes"];
					,
					Sow[{i -> t[[4]]}, "leaf"];
				];
			] &
			,
			Transpose[tree]
		]
	][[2]];

	vertices = Flatten[vertices];
	vertices = vertices /. {i_ :> {"leaf", i, N@Round[IntegerToFloat[-i], 1/1000]} /; i < 0,    	
			i_ :> If[tree[[2, i]] < 0,
			{"real", tree[[1, i]], N@Round[IntegerToFloat[-tree[[2, i]]], 1/1000], i}
			,
			{"category", tree[[1, i]], IntegerDigits[tree[[2, i]], 2], i}
		] /; i > 0
	};

	TreePlot[vertices, Top, vertices[[1, 1]], 
		VertexRenderingFunction -> (Inset[Framed[Switch[#2[[1]],
			"leaf", ToString@#2[[3]],
			"real",	StringForm["\!\(\*SubscriptBox[\(X\), \(``\)]\) < ``", #2[[2]], #2[[3]]],
			"category",
			StringForm["\!\(\*SubscriptBox[\(X\), \(``\)]\) \[Element] ``",
			(*#2[[2]], decodeCategories[preprocessorx, #2]]*)
			#2[[2]], decodeCategories2[preprocessorx, #2]]
		], Background -> White], #1] &)
	]
]

decodeCategories2[fc_, arg_] := Block[
	{nums = (First/@Position[Reverse[ arg[[3]] ], 1]), pos},
	pos = Position[fc[[1, 1]], arg[[2]] ][[1, 1]];
	fc[[2, pos, 1]][[ nums ]]
]


(****************  CLASSIFIER     *************************)

Options[RandomForestClassifierFunction] =
{
	"VariableSampleSize" -> Automatic,
	"LeafSize" -> Automatic,
	"TreeNumber" -> 100,
	"ClassNumber"-> Automatic,
(*	"Parallel" -> False,*)
	PerformanceGoal -> Automatic,
	"CrossValidation"->Automatic,
	NominalVariables -> {},
	"BinaryEncoder" -> Preprocessor["Identity"],
	"DistributionSmoothing" -> .5 (*smooth the confusion matrix used for distribution*)
};

RandomForestClassifierFunction[{X_, Y_}, opts:OptionsPattern[]] := Block[
	{variableTypes, trees, m, n, preprocessx, variableSampleSize, leafSize, nclass, 
		dist, confMat, oob, varClasses, evaluationdata, metadata, options},
	{m, n} = Dimensions[X];
	nclass = OptionValue["ClassNumber"];
	If[nclass === Automatic, nclass = Max[Y]];
	If[nclass < 2 || Min[Y]<1, AbortMachineLearning[]];
	variableTypes = ConstantArray[1, n];
	Scan[Set[variableTypes[[#]], Max[X[[All, #]]]] &, OptionValue[NominalVariables]];
	If[Max[variableTypes] > 63, AbortMachineLearning["maxcat"];];

	If[OptionValue["VariableSampleSize"] === Automatic,
		variableSampleSize = Max[1, Floor[n/3]];,
		variableSampleSize = OptionValue["VariableSampleSize"];
	];

	If[!1<=variableSampleSize<=n, AbortMachineLearning[]];

	leafSize = If[OptionValue["LeafSize"] === Automatic,
		Which[
			m > 20, 10
			,
			m > 10, m - 10
			,
			True, 1
		]
		,
		OptionValue["LeafSize"]
	];
	
	Quiet[releaseData[0]];
	(*If[OptionValue["Parallel"],
		giveData[Flatten[Transpose[X]], Y, variableSampleSize, leafSize, variableTypes, nclass, OptionValue["TreeNumber"]];
		makeParallelTree[0];
		trees = Table[getTree[i], {i, 0, OptionValue["TreeNumber"] - 1}];
		(*sigma = Mean[(Y - returnOOB[0])^2];*)
		releaseData[0];
		,*)
		Internal`WithLocalSettings[
			giveData[Flatten[Transpose[X]], Y, variableSampleSize, leafSize, variableTypes, nclass, 1];
			,
			trees = Table[makeTree[0],{OptionValue["TreeNumber"]}];
			trees = Select[trees, # =!= {{},{},{},{}} &]; (*remove tree without nodes for now...*)
			If[Length[trees]<1, AbortMachineLearning[]];
			oob = returnOOB[0];
			If[nclass > 2, 
				oob = MaxLoc /@ Partition[IntegerPart[oob], nclass];
				,
				oob = Round@oob;
				];
			,
			releaseData[0];
		];
(*
	];*)
	
	confMat = ConstantArray[0, {nclass, nclass}];
	confMat[[#1, #2]] ++ & @@@ Transpose[{Y, oob}];
	confMat = confMat + OptionValue["DistributionSmoothing"]*(1.+0.01*IdentityMatrix[nclass]);(*smoothing + tie breaker*)
	confMat = Transpose[confMat] / Total[confMat];
	options = Options[RandomForestClassifierFunction];
	options = DeleteDuplicates[Join[{opts}, options], First[#1] === First[#2] &];
	options = Replace[options,
		{
			("LeafSize"-> _) -> ("LeafSize" -> leafSize)
			,
			("VariableSampleSize"-> _) -> ("VariableSampleSize" -> variableSampleSize)
		},
		{1}
	];
	evaluationdata = {{Preprocessor["Identity"], Preprocessor["Identity"], Table[Except[_List], {n}]}, {trees}};
	metadata = {{{m, n, nclass}, {Table[{}, {n}]}, {"class", Range[nclass]}}, {leafSize, confMat}};
	ClassifierFunction["RandomForest", evaluationdata, metadata, options]
]

iClassifierFunction["RandomForest"][___][_, prop_] := AbortMachineLearning["naprop", prop];
iNoneClassifierFunction["RandomForest"][___][prop_] := AbortMachineLearning["napropn", prop];
iNoneClassifierFunction["RandomForest"][___][] := AbortMachineLearning["napropnn"];

distributionvector[x_, trees_, confMat_] := Module[
	{classProb},
	classProb = ClassProbabilities[
		Transpose[leafClass[x, ##] & @@@ trees],
		"ClassNumber"-> Length[confMat]
	];
	classProb = N@confMat[[MaxLoc/@classProb]]
];

iClassifierFunction["RandomForest"][eval_, meta_, _][x_] := Module[
	{preprocessory, trees, confMat},
	preprocessory = eval[[1, 2]];
	trees = eval[[2, 1]];
	confMat = meta[[2, 2]];
	preprocessory[MaxLoc[distributionvector[x, trees, confMat]], "Inverse"]
];

iClassifierFunction["RandomForest"][eval_, meta_, _][x_, "DistributionList"] := Module[
	{trees, classes, confMat},
	trees = eval[[2, 1]];
	classes = meta[[1, 3, 2]];
	confMat = meta[[2, 2]];
	Thread[classes -> #] & /@ distributionvector[x, trees, confMat]
];
iClassifierFunction["RandomForest"][eval_, meta_, _][x_, "FullProbabilities"] := Module[
	{trees, classes, confMat},
	trees = eval[[2, 1]];
	classes = meta[[1, 3, 2]];
	confMat = meta[[2, 2]];
	Association[Thread[classes -> #]] & /@ distributionvector[x, trees, confMat]
];
iClassifierFunction["RandomForest"][arg__][x_, "Probabilities"] := Module[
	{distlist},
	distlist = iClassifierFunction["RandomForest"][arg][x, "DistributionList"];
	MostProbableClasses[distlist, 0.1]
];

iClassifierFunction["RandomForest"][eval_, meta_, _][x_, {"Probability", class_}] := Module[
	{preprocessory, trees, confMat, distvec, classesindex},
	preprocessory = eval[[1, 2]];
	trees = eval[[2, 1]];
	confMat = meta[[2, 2]];
	distvec = distributionvector[x, trees, confMat];
	classesindex = preprocessory[class];
	#[[classesindex]] & /@ distvec
];

iClassifierFunction["RandomForest"][eval_, _, _][x_, "Votes"] := Module[
	{preprocessory, trees},
	preprocessory = eval[[1, 2]];
	trees = eval[[2, 1]];
	preprocessory[#, "Inverse"] & /@ Transpose[leafClass[x, ##] & @@@ trees]	
];

iNoneClassifierFunction["RandomForest"][_, _, options_]["Options"] := Sequence @@ options;

iNoneClassifierFunction["RandomForest"][arg___]["ClassificationPlot"] := ClassificationPlot[ClassifierFunction["RandomForest", arg]];
iNoneClassifierFunction["RandomForest"][arg___]["ProbabilityPlot"] := DistributionClassifierPlot[ClassifierFunction["RandomForest", arg]];
iNoneClassifierFunction["RandomForest"][arg___][{"ClassificationPlot", arg2___}] := ClassificationPlot[ClassifierFunction["RandomForest", arg], arg2];
iNoneClassifierFunction["RandomForest"][arg___][{"ProbabilityPlot", arg2___}] := DistributionClassifierPlot[ClassifierFunction["RandomForest", arg], arg2];

iNoneClassifierFunction["RandomForest"][{{preprocessorx_, preprocessory_, __}, {trees_}}, _, _]["TreePlot", j_] := Block[
	{tree, nNodes, vertices},
	If[j > Length[trees], Print["Argument greater than TreeNumber. Tree doesn't exist."]; Return[]];
	tree = trees[[j]];
	nNodes = Length[tree[[1]]];
	vertices = Reap[
		MapIndexed[
			With[
				{t = #1, i = First[#2]},
				If[t[[3]] > 0,
					Sow[{i -> t[[3]]}, "nodes"];
				,
				Sow[{i -> -i}, "leaf"];
				];
				If[t[[4]] > 0,
					Sow[{i -> t[[4]]}, "nodes"];
					,
					Sow[{i -> -nNodes - i}, "leaf"];
				];
			] &
			,
			Transpose[tree]
		]
	][[2]];
	vertices = Flatten[vertices];
	vertices = vertices /. {i_ :> If[tree[[2, i]] < 0,
		{"real", tree[[1, i]], N@Round[IntegerToFloat[-tree[[2, i]]], 1/1000], i}
		,
		{"category", tree[[1, i]], IntegerDigits[tree[[2, i]], 2], i}
		] /; i > 0,		
		i_ :> {"leaf", i, -tree[[3, -i]]} /; -nNodes <= i < 0,
		i_ :> {"leaf", i, -tree[[4, -i - nNodes]]} /; i < -nNodes
	};
	TreePlot[vertices, Top, vertices[[1, 1]], 
		VertexRenderingFunction -> (Inset[Framed[Switch[#2[[1]],
		"leaf", ToString@preprocessory[#2[[3]], "Inverse"],
		"real",	StringForm["\!\(\*SubscriptBox[\(X\), \(``\)]\) < ``", #2[[2]], #2[[3]]],
		"category",
		StringForm["\!\(\*SubscriptBox[\(X\), \(``\)]\) \[Element] ``",
		#2[[2]], decodeCategories[preprocessorx, #2]]
		], Background -> White], #1] &)
	]

];



(************************   PREDICTION     *****************************)

leafClass = MachineLearning`Libraries`Compile[
	{{x, _Real, 1}, {features, _Integer, 1}, {splits, _Integer, 1},
	{indicesleft, _Integer, 1}, {indicesright, _Integer, 1}}, 
	Module[
		{pos, feat, splt, threshold, mantissa, exponent, realmantissa},
		pos = 1;
		While[pos > 0,
			feat = features[[pos]];
			splt = splits[[pos]];
			If[splt <= 0,
				(*conversion to float*)
				mantissa = Quotient[- splt, 2^11];
				exponent = - splt - mantissa*2^11;
				If[mantissa > 2^51-1, mantissa = 2^51 - mantissa];
				If[exponent > 2^10-1, exponent = 2^10 - exponent];
				realmantissa = mantissa * 2.^-49;
				threshold = realmantissa*2.^exponent;
				(* end conversion to float*)
				If[x[[feat]] < threshold,
					pos = indicesleft[[pos]],
					pos = indicesright[[pos]]
				]
				,
				If[Floor[2.*FractionalPart[splt/2.^x[[feat]]]] == 1,
					pos = indicesleft[[pos]],
					pos = indicesright[[pos]]
				]
			];
		];
		-pos
	],
	RuntimeAttributes -> {Listable}, 
	RuntimeOptions -> "Speed", 
	Parallelization -> True, 
	CompilationTarget -> "C"
];

leafValue = MachineLearning`Libraries`Compile[
	{{x, _Real, 1}, {features, _Integer, 1}, {splits, _Integer, 1}, 
		{indicesleft, _Integer, 1}, {indicesright, _Integer, 1}}, 
	Module[
		{pos, feat, splt, threshold, mantissa, exponent, realmantissa},
		pos = 1;
		While[pos > 0,
			feat = features[[pos]];
			splt = splits[[pos]];
			If[splt <= 0,
				(*conversion to float*)
				mantissa = Quotient[- splt, 2^11];
				exponent = - splt - mantissa*2^11;
				If[mantissa > 2^51-1, mantissa = 2^51 - mantissa];
				If[exponent > 2^10-1, exponent = 2^10 - exponent];
				realmantissa = mantissa * 2.^-49;
				threshold = realmantissa*2.^exponent;
				(* end conversion to float*)
				If[x[[feat]] < threshold,
					pos = indicesleft[[pos]],
					pos = indicesright[[pos]]
				]
				,
				If[Floor[2.*FractionalPart[splt/2.^x[[feat]]]] == 1,
					pos = indicesleft[[pos]],
					pos = indicesright[[pos]]
				]
			];
		];
		mantissa = Quotient[- pos, 2^11];
		exponent = - pos - mantissa*2^11;
		If[mantissa > 2^51-1, mantissa = 2^51 - mantissa];
		If[exponent > 2^10-1, exponent = 2^10 - exponent];
		realmantissa = mantissa * 2.^-49;
		realmantissa*2.^exponent
	],
	RuntimeAttributes -> {Listable}, 
	RuntimeOptions -> "Speed", 
	Parallelization -> True, 
	CompilationTarget -> "C"
];


(********************* ADDITION TO NOMINAL ENCODING ******************************)

NominalEncoder[{nominalindices_, numericalindices_}, labelers_][features_List, "NoLabel"] :=
	First[NominalEncoder[{nominalindices, numericalindices}, labelers][{features}, "NoLabel"]];
NominalEncoder[{nominalindices_, numericalindices_}, labelers_][encodedfeatures:{__List}, "NoLabel"] := Module[
	{encodedfeaturesT, nominalfeatures, numericalfeatures, nominalspans , lnominal, lnumerical},
	lnominal = Length[nominalindices];
	lnumerical = Length[numericalindices];
	encodedfeaturesT = Transpose[encodedfeatures];
	numericalfeatures = encodedfeaturesT[[;; lnumerical]];
	nominalspans = Accumulate[Length[#[[1]]] - 1 & /@ labelers];
	nominalspans = MapThread[Span, lnumerical + {Prepend[Most[nominalspans+1], 1], nominalspans}];
	nominalfeatures = Transpose[encodedfeaturesT[[#]]] & /@ nominalspans;
	nominalfeatures = With[
   {nclass = Length /@ (labelers[[All, 1]]) - 1},
           MapThread[
    Plus, {nclass + 1,
     MapThread[Dot, {nominalfeatures, -Reverse@Range[#] & /@ nclass}]}]
   ];
	encodedfeaturesT[[numericalindices]] = numericalfeatures;	
	encodedfeaturesT[[nominalindices]] = nominalfeatures;
	Transpose[encodedfeaturesT[[;; lnominal + lnumerical]]]
];
