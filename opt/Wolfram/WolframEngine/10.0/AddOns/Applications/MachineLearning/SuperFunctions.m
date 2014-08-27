(* ::Package:: *)

Package["MachineLearning`"]


PackageExport["System`Classify"]
PackageExport["System`Predict"]

Classify::usage = "Classify[X -> Y] trains a model to predict categories Y from input data X."
Predict::usage = "Predict[X -> Y] trains a model to continuous values Y from input data X."

(* these exist purely to wrap the inner functions. they're a hack for now. point is we need just two symbols to stubbify in sysinit.m *)

(* We should do all the preprocesing together to avoid having to duplicate the dataset.. 
Or use HoldFirst... check that later. *)

(******************** Classify ************************)

Unprotect[Classify];
Unprotect[Predict];

Classify::bdmtd = "Value of option Method -> `` is not Automatic, \"LogisticRegression\", \"NearestNeighbors\", or \"RandomForest\".";
Classify::bdmtdtxt = "Value of option Method -> `` is not Automatic, or \"NaiveBayesText\".";
Classify::bdfmt = "Argument `` should be a rule, a list of rules, or an association."
Classify::bdvfmt = "Value of option \"ValidationSet\" -> `` should be a rule, a list of rules, or an association."
Classify::smdat = "The dataset should contain at least two examples."
Classify::unbal = "The number of examples (`1`), and the number of corresponding classes (`2`) should be identical."
Classify::mincnb = "The dataset should contain at least two different classes."
Classify::bftlgth = "Examples should have the same number of features."
Classify::bfttype = "Features cannot be lists."
Classify::manyval = "Only one variable can be predicted."
Classify::bdval = "Classes to be predicted cannot be lists."
Classify::sglex = "Class `1` contains only one example. Each class should contain at least two examples."
Classify::uknwncl = "Class `1` in \"ValidationSet\" -> `` is not in the training set."
Classify::bdpg = "Value of option PerformanceGoal -> `` is not Automatic, \"Speed\", \"Memory\", or \"Quality\"."
Classify::bdnv = "Value of option NominalVariables -> `` is not Automatic, All, None, or a list of integers."
Classify::bdnvs = "Index `1` in value of option NominalVariables -> `2` is higher than the number of features."
Classify::bdfeatn = "Value of option \"FeatureNames\" -> `1` is not Automatic, or a list whose length is the number of features."

Options[Classify] = Options[iClassify] = {
	Method -> Automatic, (*can be "LogisticRegression" or "NearestNeighbors"*)
	PerformanceGoal -> Automatic,
	NominalVariables -> Automatic,
	"ValidationSet" -> None,
	"ClassName" -> "class",
	"FeatureNames" -> Automatic
};
Classify[dataset_, opts:OptionsPattern[]] := CatchAbortMachineLearning[
	getclassifier[dataset, opts]
	,
	Defer[Classify[Short[dataset, 10], opts]]
];
Classify[dataset_, new_, opts:OptionsPattern[]] := CatchAbortMachineLearning[
	getclassifier[dataset, opts][new]
	,
	Defer[Classify[Short[dataset, 10], new, opts]]
];
Classify[arguments___, opts:OptionsPattern[]] := (
	Message[Classify::argt, Classify, Length[{arguments}], 1, 2];
	Defer[Short[Classify[arguments, opts], 10]]
);

getclassifier[dataset_, opts___] := Module[
	{parsed, classifier},
	parsed = checknParseClassifyData[dataset, opts];
	classifier = iClassify[parsed, checknParseClassifyValidationSet[parsed, opts]];
	AppendTo[classifier[[3, 1]], {$VersionNumber, $ReleaseNumber}];
	classifier
]
Options[checknParseClassifyData ] = Options[Classify];
checknParseClassifyData[dataset_, opts:OptionsPattern[]] := Module[
	{parsed, perfg, nomvar},
	If[!ClassifyFormatQ[dataset], AbortMachineLearning["bdfmt", dataset]];
	parsed = ParseClassifierDataset[dataset];
	CheckClassifyData[parsed];
	CheckOptions[opts, Classify];
	nomvar = OptionValue[NominalVariables];
	If[!MatchQ[nomvar, ({___?(FractionalPart[#] ==0 &)} | Automatic | All | None)], AbortMachineLearning["bdnv", nomvar]];
	perfg = OptionValue[PerformanceGoal];
	If[FreeQ[{Automatic, "Speed", "Memory", "Quality"}, perfg], AbortMachineLearning["bdpg", perfg]];
	parsed
];
checknParseClassifyValidationSet[parsed_, opts___] := Replace[{opts},
	("ValidationSet" -> validationset:Except[Automatic|None]) :>
	("ValidationSet" -> Module[
		{parsedval, complement},
		If[!ClassifyFormatQ[validationset], AbortMachineLearning["bdvfmt", validationset]];
		parsedval = ParseClassifierDataset[validationset];
		CheckParsedData[parsedval];
		complement = Complement[DeleteDuplicates[Last@parsedval], DeleteDuplicates[Last@parsed]];
		If[Length[complement]>0, AbortMachineLearning["uknwncl", First[complement], validationset]];
		parsedval
	]), {1}
];

iClassify[{featurevectors:{__List}, labels_}, opts:OptionsPattern[]] := Module[
	{m, n, X, Y, preprocessorx, preprocessory, classifier, oldnominalindices, nominalindices, nclass, 
		validationset, binaryencoderx, options, classlabels, commonmetadata, nv, featnames
	},
	{m, n} = Dimensions[featurevectors];
	nv = OptionValue[NominalVariables];
	If[MatchQ[nv, _List],
		If[#<1 || #>n, AbortMachineLearning["bdnvs", #, nv]] & /@ nv
	];
	featnames = OptionValue["FeatureNames"];
	If[!MatchQ[featnames, ({Repeated[_, {n}]} | Automatic)], AbortMachineLearning["bdfeatn", featnames]];
	{oldnominalindices, binaryencoderx, preprocessorx, preprocessory} = PreprocessClassifierData[{featurevectors, labels}, FilterOptions[PreprocessClassifierData, opts]];
	nominalindices = Sort[n + 1 - Range[Length[oldnominalindices]]];
	nclass = Length[preprocessory[[2]]];
	If[nclass < 2, AbortMachineLearning["mincnb"]];
	classlabels = preprocessory[Range[nclass], "Inverse"];
	commonmetadata = ClassifierMetaData[
		{featurevectors, labels},
		"ClassLabels" -> classlabels,
		"ClassName" -> OptionValue["ClassName"],
		"FeatureNames" -> OptionValue["FeatureNames"],
		NominalVariables -> oldnominalindices
	];
	X = preprocessorx[featurevectors];
	Y = preprocessory[labels];
	{X, Y} = ShuffleSet[{X, Y}];
	validationset = OptionValue["ValidationSet"];
	If[!MemberQ[{Automatic, None}, validationset],
		validationset = {preprocessorx[First[validationset]], preprocessory[Last[validationset]]}
		(* could do check on the validation set *)
	];
	classifier = internalClassify[{X, Y}, 
		NominalVariables -> nominalindices, 
		"BinaryEncoder"-> binaryencoderx,
		"Type" -> "Data", 
		"ClassNumber" -> nclass, 
		"ValidationSet" -> validationset,
		FilterOptions[internalClassify, opts]
	];
	preprocessorx = JoinPreprocessors[preprocessorx, classifier[[2, 1, 1]]];
	preprocessory = JoinPreprocessors[preprocessory, classifier[[2, 1, 2]]];
	classifier[[3, 1]] = commonmetadata;
	classifier[[2, 1]] = {preprocessorx, preprocessory, Replace[commonmetadata[[2, All, 2]], $inputPatternRule, {1}]};
	options = {FilterOptions[Classify, classifier[[4]]]};
	options = Join[options, {
		NominalVariables -> oldnominalindices
	}];
	classifier[[4]] = options;
	classifier
];

$inputPatternRule = {
	"image" -> _Image,
	"image3D" -> _Image3D,
	"nominal" -> Except[_List],
	"numerical" -> (_Real | _Integer | _Slot | _Missing | _?(MatchQ[N[#], _Real] &)),
	"text" -> _String,
	"sound" -> _Sound,
	"temporal" -> _TemporalData
};

iClassify[{featurevectors_List, labels_}, opts:OptionsPattern[]] := Module[
	{X, preprocessorx, classifier, validationset},
	preprocessorx = Preprocessor["ToMatrix"];
	X = preprocessorx[featurevectors];
	validationset = OptionValue["ValidationSet"];
	If[!MemberQ[{Automatic, None}, validationset],
		validationset[[1]] = preprocessorx[First[validationset]]
		(* could do check on the validation set *)
	];
	classifier = iClassify[{X, labels}, "ValidationSet" -> validationset, opts];
	classifier[[2, 1, 1]] = JoinPreprocessors[preprocessorx, classifier[[2, 1, 1]]];
	classifier[[2, 1, 3]] = First[classifier[[2, 1, 3]]];
	classifier
];

iClassify[{images:{__Image}, labels_}, opts:OptionsPattern[]] := Module[
	{X, Y, preprocessorx, preprocessory, classifier, nclass, validationset, options, classlabels, commonmetadata},
	{preprocessorx, preprocessory} = PreprocessClassifierImage[{images, labels}, FilterOptions[PreprocessClassifierImage, opts]];
	X = preprocessorx[images];
	Y = preprocessory[labels];
	{X, Y} = ShuffleSet[{X, Y}];
	nclass = Length[preprocessory[[2]]];
	If[nclass < 2, AbortMachineLearning["mincnb"]];
	classlabels = preprocessory[Range[nclass], "Inverse"];
	commonmetadata = ClassifierMetaData[
		{images, labels},
		"ClassLabels" -> classlabels,
		"ClassName" -> OptionValue["ClassName"],
		"FeatureNames" -> OptionValue["FeatureNames"],
		NominalVariables -> {}
	];
	validationset = OptionValue["ValidationSet"];
	If[!MemberQ[{Automatic, None}, validationset],
		validationset = {preprocessorx[First[validationset]], preprocessory[Last[validationset]]}
		(* could do check on the validation set *)
	];
	classifier = internalClassify[{X, Y}, 
		NominalVariables -> None, 
		"Type" -> "Image", 
		"ClassNumber" -> nclass, 
		"ValidationSet" -> validationset,
		FilterOptions[internalClassify, opts]
	];
	preprocessorx = JoinPreprocessors[preprocessorx, classifier[[2, 1, 1]]];
	preprocessory = JoinPreprocessors[preprocessory, classifier[[2, 1, 2]]];
	classifier[[2, 1]] = {preprocessorx, preprocessory, _Image};
	classifier[[3, 1]] = commonmetadata;
	options = {FilterOptions[Classify, classifier[[4]]]};
	options = Join[options, {
		NominalVariables -> None
	}];
	classifier[[4]] = options;
	classifier
];

iClassify[{strings:{__String}, labels_}, opts:OptionsPattern[]] := Module[
	{X, Y, classifier, preprocessorx, preprocessory, options, nclass, classlabels, commonmetadata, type, method, methodoptions},
	type = DetectFeatureType[strings];
	If[type === "nominal",
		preprocessorx = Preprocessor["ToMatrix"];
		X = preprocessorx[strings];
		classifier = iClassify[{X, labels}, opts];
		classifier[[2, 1, 1]] = JoinPreprocessors[preprocessorx, classifier[[2, 1, 1]]];
		classifier[[2, 1, 3]] = First[classifier[[2, 1, 3]]];
		classifier
		,
		{preprocessorx, preprocessory} = PreprocessClassifierText[{strings, labels}, FilterOptions[PreprocessClassifierText, opts]];
		nclass = Length[preprocessory[[2]]];
		If[nclass < 2, AbortMachineLearning["mincnb"]];
		classlabels = preprocessory[Range[nclass], "Inverse"];
		commonmetadata = ClassifierMetaData[
			{strings, labels},
			"ClassLabels" -> classlabels,
			"ClassName" -> OptionValue["ClassName"],
			"FeatureNames" -> OptionValue["FeatureNames"],
			NominalVariables -> {}
		];
		Y = preprocessory[labels];
		X = preprocessorx[strings];
		(*{X, Y} = ShuffleSet[{X, Y}];*) (*no need at this point*)
		method = OptionValue[Method];
		methodoptions = Sequence[
			"ClassNumber"-> nclass,
			PerformanceGoal -> OptionValue[PerformanceGoal]
		];
		classifier = Switch[method,
			Automatic,
				NaiveBayesText[{X, Y}, 
					methodoptions
				]
			,
			"NaiveBayesText", NaiveBayesText[{X, Y}, methodoptions]
			,
			{"NaiveBayesText", ___}, NaiveBayesText[{X, Y}, Sequence @@ Rest[method], methodoptions]
			,
			_,
				AbortMachineLearning["bdmtdtxt", method]
		];
		preprocessorx = JoinPreprocessors[preprocessorx, classifier[[2, 1, 1]]];
		preprocessory = JoinPreprocessors[preprocessory, classifier[[2, 1, 2]]];
		classifier[[2, 1]] = {preprocessorx, preprocessory, _String};
		classifier[[3, 1]] = commonmetadata;
		options = {FilterOptions[Classify, classifier[[4]]]};
		options = Join[options, {
			NominalVariables -> None
		}];
		classifier[[4]] = options;
		classifier
	]
];

(*type can be: "numerical", "nominal", "image", "text", "sound", "temporal"*)

DetectFeatureType[features:{__}] := Switch[N[features],
	{__Image}, "image"
	,
	{__Sound}, "sound"
	,
	{__TemporalData}, "temporal"
	,
	{__String}, textornominal[features]
	,
	{__Real}, numericalornominal[features]
	,
	{__}, "nominal"
	,
	_, AbortMachineLearning[]
];

textornominal[features_] := Module[
	{feat, nfeat, m, lfeat},
	feat = DeleteCases[DeleteDuplicates[features], _Missing];
	nfeat = Length[feat];
	lfeat = Max[StringLength /@ feat];
	m = Length[features];
	If[lfeat < 4, Return["nominal"]];
	If[lfeat > 30, Return["text"]];
	If[nfeat < 4, Return["nominal"]];
	If[nfeat > 100, Return["text"]];
	If[nfeat/m < 0.25, "nominal", "text"]
];

numericalornominal[features_] := Module[
	{feat, m, numeric},
	feat = DeleteCases[DeleteDuplicates[features], _Missing];
	m = Length[features];
	(*is nominal if it has non-numeric values, or less than 10 integer values..*)
	numeric = Apply[And, NumericQ /@ feat];
	If[!numeric, Return["nominal"]];
	If[
		And[Length[feat]<10, Apply[And, # == 0 & /@ FractionalPart[feat]], m>20],
		"nominal",
		"numerical"
	]
];

Options[internalClassify] = {
	Method -> Automatic,
	"Type" -> "Data",
	"ValidationSet" -> Automatic,
	PerformanceGoal -> Automatic,
	NominalVariables -> {},
	"ClassNumber" -> Automatic,
	"BinaryEncoder"-> Preprocessor["Identity"]
};

(*careful, NominalVariables have changed!*)

internalClassify[{X_, Y_}, opts:OptionsPattern[]] := Module[
	{method, cvopts, models, options, methodoptions, classifier, validationset},
	method = OptionValue[Method];
	methodoptions = Sequence[
		"ClassNumber"-> OptionValue["ClassNumber"],
		NominalVariables -> OptionValue[NominalVariables],
		PerformanceGoal -> OptionValue[PerformanceGoal],
		"BinaryEncoder"-> OptionValue["BinaryEncoder"]
	];
	classifier = Switch[method,
		Automatic,
			models = classifierCandidates[{X, Y}, FilterOptions[classifierCandidates, opts]];
			If[$SystemWordLength === 32,
				models = DeleteCases[models, "RandomForest"]];
			validationset = OptionValue["ValidationSet"];
			If[MemberQ[{Automatic, None}, validationset],
				cvopts = choosecvopt[{X, Y}, FilterOptions[choosecvopt, opts]];
				,
				cvopts = {Method -> "External", "ValidationSet"-> validationset};
			];
			CrossValidationModelSelect[{X, Y}, models,
				"CrossValidation" -> cvopts,
				"ModelOptions"-> {methodoptions}
			]
		,
		"NaiveBayes", NaiveBayes[{X, Y}, methodoptions]
		,
		{"NaiveBayes", ___}, NaiveBayes[{X, Y}, Sequence @@ Rest[method], methodoptions]
		,
		"LogisticRegression", LogisticClassifierFunction[{X, Y}, methodoptions]
		,
		{"LogisticRegression", ___}, LogisticClassifierFunction[{X, Y}, Sequence @@ Rest[method], methodoptions]
		,
		"NearestNeighbors", NearestNeighborsClassifierFunction[{X, Y}, methodoptions]
		,
		{"NearestNeighbors", ___}, NearestNeighborsClassifierFunction[{X, Y}, Sequence @@ Rest[method], methodoptions]
		,
		"RandomForest", RandomForestClassifierFunction[{X, Y}, methodoptions]
		,
		{"RandomForest", ___}, RandomForestClassifierFunction[{X, Y}, Sequence @@ Rest[method], methodoptions]
		,		
		_,
			AbortMachineLearning["bdmtd", method]
	];
	method = classifier[[1]];
	If[method === "MultiLogistic", method = "LogisticRegression"];
	methodoptions = classifier[[4]];
	methodoptions = DeleteCases[methodoptions,
		(
			("BinaryEncoder"-> _)|
			(NominalVariables -> _)|
			("ClassNumber" -> _) |
			(PerformanceGoal -> _)|
			("CrossValidation"-> {___, Method -> "External", ___ })
		)
		, {1}
	];
	methodoptions = Method -> {method, Sequence @@ methodoptions};
	options = Options[internalClassify];
	options = DeleteDuplicates[Join[{opts}, options], First[#1] === First[#2] &];
	options = DeleteCases[options,
		(
			("BinaryEncoder"-> _)|
			(NominalVariables -> _)|
			("Type" -> _)|
			(Method-> _)|
			("ValidationSet"->_)
		)
		, {1}
	];
	options = Prepend[options, methodoptions];
	classifier[[4]] = options;
	classifier
];

Options[classifierCandidates] := {
	"Type" -> "Data",
	PerformanceGoal -> Automatic
};
(*for now, do not use "Type"*)
classifierCandidates[{X_, Y_}, opts:OptionsPattern[]] := Module[
	{m, n, nclass},
	{m, n} = Dimensions[X];
	nclass = Max[Y];
	If[m<3, Return[{LogisticClassifierFunction}]];
	Switch[OptionValue[PerformanceGoal],
		"Speed",
			{NearestNeighborsClassifierFunction, LogisticClassifierFunction}
		,
		"Quality",
			{NearestNeighborsClassifierFunction, LogisticClassifierFunction, RandomForestClassifierFunction}
		,
		"Memory",
			{LogisticClassifierFunction}
		,
		_,
			{NearestNeighborsClassifierFunction, LogisticClassifierFunction}
		(*Which[
			m*n >= 10000,
				{NearestNeighborsClassifierFunction, LogisticClassifierFunction}
			,
			m*n < 10000,
				{NearestNeighborsClassifierFunction, LogisticClassifierFunction, RandomForestClassifierFunction}
			,
			True, AbortMachineLearning[];
		]*)
	]
];

Options[choosecvopt] := {
	"Type" -> "Data",
	PerformanceGoal -> Automatic
};
choosecvopt[{X_, Y_}, opts:OptionsPattern[]] := Module[
	{m, n, nclass},
	{m, n} = Dimensions[X];
	nclass = Max[Y];
	Switch[OptionValue[PerformanceGoal],
		"Speed",
			Which[
				m >= 100,
					{Method -> "RandomSplit", "SplitNumber" -> 1, "SplitSize" -> .5}
				,
				m < 100,
					{Method -> "KFoldSplit", "SplitNumber" -> 2}
				,
				True, AbortMachineLearning[];
			]
		,
		"Quality",
			Which[
				m >= 1000000,
					{Method -> "RandomSplit", "SplitNumber" -> 1, "SplitSize" -> .5}
				,
				m >= 100000,
					{Method -> "RandomSplit", "SplitNumber" -> 1, "SplitSize" -> .2}
				,
				m >= 10000,
					{Method -> "KFoldSplit", "SplitNumber" -> 2}
				,
				m >= 1000,
					{Method -> "KFoldSplit", "SplitNumber" -> 4}
				,
				m >= 10,
					{Method -> "KFoldSplit", "SplitNumber" -> 10}
				,
				m < 10,
					{Method -> "KFoldSplit", "SplitNumber" -> m}
				,
				True, AbortMachineLearning[];
			]
		,
		_,
			Which[
				m >= 10000,
					{Method -> "RandomSplit", "SplitNumber" -> 1, "SplitSize" -> .5}
				,
				m >= 1000,
					{Method -> "RandomSplit", "SplitNumber" -> 1, "SplitSize" -> .2}
				,
				m >= 100,
					{Method -> "KFoldSplit", "SplitNumber" -> 2}
				,
				m >= 10,
					{Method -> "KFoldSplit", "SplitNumber" -> 4}
				,
				m < 10,
					{Method -> "KFoldSplit", "SplitNumber" -> m}
				,
				True, AbortMachineLearning[];
			]
	]
];

(******************** End Classify ************************)


(******************** Predict ************************)

Predict::bdmtd = "Value of option Method -> `` is not Automatic, \"LinearRegression\", \"NearestNeighbors\", or \"RandomForest\"";
Predict::bdfmt = "Argument `` should be a Rule or a list of Rule."
Predict::bdvfmt = "Value of option \"ValidationSet\" -> `` should be a Rule or a list of Rule."
Predict::smdat = "The dataset should contain at least two examples."
Predict::unbal = "The number of examples (`1`), and the number of corresponding values (`2`) should be identical."
Predict::bdval = "Values to be predicted should be (non-complex) numerical."
Predict::bftlgth = "Examples should have the same number of features."
Predict::bfttype = "Features cannot be lists."
Predict::manyval = "Only one variable can be predicted."
Predict::bdpg = "Value of option PerformanceGoal -> `` is not Automatic, \"Speed\", \"Memory\", or \"Quality\"."
Predict::bdnv = "Value of option NominalVariables -> `` is not Automatic, All, None, or a list of integers."
Predict::bdnvs = "Index `1` in value of option NominalVariables -> `2` is higher than the number of features."
Predict::bdfeatn = "Value of option \"FeatureNames\" -> `1` is not Automatic, or a list whose length is the number of features."

Options[Predict] = Options[iPredict] = {
	Method -> Automatic,
	PerformanceGoal -> Automatic,
	NominalVariables -> Automatic,
	"ValueName" -> "value",
	"FeatureNames" -> Automatic,
	"ValidationSet" -> None
};

Predict[dataset_, opts:OptionsPattern[]] := CatchAbortMachineLearning[
	getpredictor[dataset, opts]
	,
	Defer[Predict[Short[dataset, 10], opts]]
];
Predict[dataset_, new_, opts:OptionsPattern[]] := CatchAbortMachineLearning[
	getpredictor[dataset, opts][new]
	,
	Defer[Predict[Short[dataset, 10], new, opts]]
];
Predict[arguments___, opts:OptionsPattern[]] := (
	Message[Predict::argt, Predict, Length[{arguments}], 1, 2];
	Defer[Predict[arguments, opts]]
);

getpredictor[dataset_, opts___] := Module[
	{parsed, predictor},
	parsed = checknParsePredictData[dataset, opts];
	predictor = iPredict[parsed, checknParsePredictValidationSet[parsed, opts]];
	AppendTo[predictor[[3, 1]], {$VersionNumber, $ReleaseNumber}];
	predictor
]
Options[checknParsePredictData ] = Options[Predict];
checknParsePredictData[dataset_, opts:OptionsPattern[]] := Module[
	{parsed, perfg, nv},
	If[!PredictFormatQ[dataset], AbortMachineLearning["bdfmt", dataset]];
	parsed = ParsePredictorDataset[dataset];
	CheckPredictData[parsed];
	CheckOptions[opts, Predict];
	nv = OptionValue[NominalVariables];
	If[!MatchQ[nv, ({__?(FractionalPart[#] ==0 &)} | Automatic | All | None)], AbortMachineLearning["bdnv", nv]];
	perfg = OptionValue[PerformanceGoal];
	If[FreeQ[{Automatic, "Speed", "Memory", "Quality"}, perfg], AbortMachineLearning["bdpg", perfg]];
	parsed
];
checknParsePredictValidationSet[parsed_, opts___] := Replace[{opts},
	("ValidationSet" -> validationset:Except[Automatic|None]) :>
	("ValidationSet" -> Module[
		{parsedval, complement},
		If[!PredictFormatQ[validationset], AbortMachineLearning["bdvfmt", validationset]];
		parsedval = ParsePredictorDataset[validationset];
		CheckParsedData[parsedval];
		complement = Complement[DeleteDuplicates[Last@parsedval], DeleteDuplicates[Last@parsed]];
		If[Length[complement]>0, AbortMachineLearning["uknwncl", First[complement], validationset]];
		parsedval
	]), {1}
];


iPredict[{featurevectors:{__List}, values_}, opts:OptionsPattern[]] := Module[
	{m, n, X, Y, preprocessorx, nominalindices, oldnominalindices, predictor, binaryencoderx, options,
		validationset, commonmetadata, nv, featnames
	},
	{m, n} = Dimensions[featurevectors];
	nv = OptionValue[NominalVariables];
	If[MatchQ[nv, _List],
		If[#<1 || #>1, AbortMachineLearning["bdnvs", #, nv]] & /@ nv
	];
	featnames = OptionValue["FeatureNames"];
	If[!MatchQ[featnames, ({Repeated[_, {n}]} | Automatic)], AbortMachineLearning["bdfeatn", featnames]];
	{oldnominalindices, binaryencoderx, preprocessorx} = PreprocessPredictorData[{featurevectors, values}, 
		FilterOptions[PreprocessPredictorData, opts]
	];
	nominalindices = Sort[n + 1 - Range[Length[oldnominalindices]]];
	commonmetadata = PredictorMetaData[
		{featurevectors, values},
		"ValueName" -> OptionValue["ValueName"],
		"FeatureNames" -> OptionValue["FeatureNames"],
		NominalVariables -> oldnominalindices
	];
	X = preprocessorx[featurevectors];
	Y = values;
	{X, Y} = ShuffleSet[{X, Y}];
	validationset = OptionValue["ValidationSet"];
	If[!MemberQ[{Automatic, None}, validationset],
		validationset = {preprocessorx[First[validationset]], Last[validationset]}
		(* could do check on the validation set *)
	];
	predictor = internalPredict[{X, Y},
		NominalVariables -> nominalindices,
		"BinaryEncoder"-> binaryencoderx,
		"Type" -> "Data",
		"ValidationSet" -> validationset,
		FilterOptions[internalPredict, opts]
	];
	preprocessorx = JoinPreprocessors[preprocessorx, predictor[[2, 1, 1]]];
	(*preprocessory = JoinPreprocessors[preprocessory, predictor[[2, 1, 2]]];*)
	predictor[[2, 1, 1]] = preprocessorx;
	predictor[[2, 1, 3]] = Replace[commonmetadata[[2, All, 2]], $inputPatternRule, {1}];
	predictor[[3, 1]] = commonmetadata;
	options = {FilterOptions[Predict, predictor[[4]]]};
	options = Join[options, {
		NominalVariables -> oldnominalindices
	}];
	predictor[[4]] = options;
	predictor
];

iPredict[{featurevectors_List, values_}, opts:OptionsPattern[]] := Module[
	{X, preprocessorx, predictor, validationset},
	preprocessorx = Preprocessor["ToMatrix"];
	X = preprocessorx[featurevectors];
	validationset = OptionValue["ValidationSet"];
	If[!MemberQ[{Automatic, None}, validationset],
		validationset[[1]] = preprocessorx[First[validationset]]
		(* could do check on the validation set *)
	];
	predictor = iPredict[{X, values}, "ValidationSet" -> validationset, opts];
	predictor[[2, 1, 1]] = JoinPreprocessors[preprocessorx, predictor[[2, 1, 1]]];
	predictor[[2, 1, 3]] = First[predictor[[2, 1, 3]]];
	predictor
];

iPredict[{images:{__Image}, values_}, opts:OptionsPattern[]] := Module[
	{X, Y, preprocessorx, predictor, validationset, commonmetadata},
	preprocessorx = PreprocessPredictorImage[{images, values}, FilterOptions[PreprocessPredictorImage, opts]];
	commonmetadata = PredictorMetaData[
		{images, values},
		"ValueName" -> OptionValue["ValueName"],
		"FeatureNames" -> OptionValue["FeatureNames"],
		NominalVariables -> {}
	];
	X = preprocessorx[images];
	Y = values;
	{X, Y} = ShuffleSet[{X, Y}];
	validationset = OptionValue["ValidationSet"];
	If[!MemberQ[{Automatic, None}, validationset],
		validationset = {preprocessorx[First[validationset]], Last[validationset]}
		(* could do check on the validation set *)
	];
	predictor = internalPredict[{X, Y},
		NominalVariables -> None,
		"ValidationSet" -> validationset,
		"Type" -> "Image",
		FilterOptions[internalPredict, opts]
	];
	preprocessorx = JoinPreprocessors[preprocessorx, predictor[[2, 1, 1]]];
	(*preprocessory = JoinPreprocessors[preprocessory, predictor[[2, 1, 2]]];*)
	predictor[[2, 1, 1]] = preprocessorx;
	predictor[[2, 1, 3]] = _Image;
	predictor[[3, 1]] = commonmetadata;
	predictor[[4]] = {FilterOptions[Predict, predictor[[4]]]};
	predictor
];

Options[internalPredict] = {
	Method -> Automatic, 
	"Type" -> "Data",
	"ValidationSet" -> Automatic,
	PerformanceGoal -> Automatic,
	NominalVariables -> Automatic,
	"BinaryEncoder" -> Preprocessor["Identity"]
};
internalPredict[{X_, Y_}, opts:OptionsPattern[]] := Module[
	{method, cvopts, models, options, predictor, methodoptions, validationset},
	method = OptionValue[Method];
	methodoptions = Sequence[
		NominalVariables -> OptionValue[NominalVariables],
		PerformanceGoal -> OptionValue[PerformanceGoal],
		"BinaryEncoder"-> OptionValue["BinaryEncoder"]
	];
	predictor = Switch[method,
		Automatic,
			models = predictorCandidates[{X, Y}, FilterOptions[predictorCandidates, opts]];
			If[$SystemWordLength === 32,
			models = DeleteCases[models, "RandomForest"]];
			validationset = OptionValue["ValidationSet"];
			If[MemberQ[{Automatic, None}, validationset],
				cvopts = choosecvopt[{X, Y}, FilterOptions[choosecvopt, opts]];
				,
				cvopts = {Method -> "External", "ValidationSet"-> validationset};
			];
			CrossValidationModelSelect[{X, Y}, models, 
				"CrossValidation" -> cvopts, 
				"ModelOptions"-> {methodoptions}
			]
		,
		"LinearRegression", LinearPredictorFunction[{X, Y}, methodoptions]
		,
		{"LinearRegression", ___}, LinearPredictorFunction[{X, Y}, Sequence @@ Rest[method], methodoptions]
		,
		"NearestNeighbors", NearestNeighborsPredictorFunction[{X, Y}, methodoptions]
		,
		{"NearestNeighbors", ___}, NearestNeighborsPredictorFunction[{X, Y}, Sequence @@ Rest[method], methodoptions]
		,
		"RandomForest", RandomForestPredictorFunction[{X, Y}, methodoptions]
		,
		{"RandomForest", ___}, RandomForestPredictorFunction[{X, Y}, Sequence @@ Rest[method], methodoptions]
		,		
		_,
			AbortMachineLearning["bdmtd", method]
	];
	method = predictor[[1]];
	methodoptions = predictor[[4]];
	methodoptions = DeleteCases[methodoptions,
		(
			("BinaryEncoder"-> _)|
			(NominalVariables -> _)|
			(PerformanceGoal -> _)|
			("CrossValidation"-> {___, Method -> "External", ___})
		)
		, {1}
	];
	methodoptions = Method -> {method, Sequence @@ methodoptions};
	If[MatchQ[cvopts, {___, Method -> "External", ___ }], cvopts = {Method -> Automatic}];
	options = Options[internalPredict];
	options = DeleteDuplicates[Join[{opts}, options], First[#1] === First[#2] &];
	options = DeleteCases[options,
		(
			("BinaryEncoder"-> _)|
			(NominalVariables -> _)|
			("Type" -> _)|
			(Method-> _)|
			("ValidationSet" -> _)
		)
		, {1}
	];
	options = Prepend[options, methodoptions];
	predictor[[4]] = options;
	predictor
];

Options[predictorCandidates] := {
	"Type" -> "Data",
	PerformanceGoal -> Automatic
};
(*for now, do not use "Type"*)
predictorCandidates[{X_, Y_}, opts:OptionsPattern[]] := Module[
	{m, n},
	{m, n} = Dimensions[X];
	If[m < 3, Return[{LinearPredictorFunction}]];
	Switch[OptionValue[PerformanceGoal],
		"Speed",
			{NearestNeighborsPredictorFunction, LinearPredictorFunction}
		,
		"Quality",
			{NearestNeighborsPredictorFunction, LinearPredictorFunction, RandomForestPredictorFunction}
		,
		"Memory",
			{LinearPredictorFunction}
		,
		_,
			{NearestNeighborsPredictorFunction, LinearPredictorFunction}
		(*Which[
			m*n >= 10000,
				{NearestNeighborsPredictorFunction, LinearPredictorFunction}
			,
			m*n < 10000,
				{NearestNeighborsPredictorFunction, LinearPredictorFunction, RandomForestPredictorFunction}
			,
			True, AbortMachineLearning[];
		]*)
	]
];
	
(******************** End Predict ************************)

Protect[Classify];
Protect[Predict];