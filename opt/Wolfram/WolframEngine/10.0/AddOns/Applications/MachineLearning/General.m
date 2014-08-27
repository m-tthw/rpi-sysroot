(* ::Package:: *)

Package["MachineLearning`"]

PackageImport["Developer`"]

PackageExport["System`ClassifierFunction"]

ClassifierFunction::usage = "ClassifierFunction[type, ...] represents a function that can predict categories from input data.";

PackageExport["System`PredictorFunction"]

PredictorFunction::usage = "PredictorFunction[type, ...] represents a function that can predict continuous values from input data.";

PackageScope["FilterOptions"]
PackageScope["FastMeanColumn"]
PackageScope["FastMeanRow"]
PackageScope["FastTotalRow"]
PackageScope["FastStandardDeviation"]
PackageScope["WeightedMean"]
PackageScope["WeightedStandardDeviation"]
PackageScope["MinLoc"]
PackageScope["MaxLoc"]
PackageScope["ClassProbabilities"]
PackageScope["NominalEncoder"]
PackageScope["NominalEncoder"]
PackageScope["Labeler"]
PackageScope["BinaryLabeler"]
PackageScope["AbortMachineLearning"]
PackageScope["CatchAbortMachineLearning"]

FilterOptions::usage = "FilterOptions[head, opts...] more directly useful than FilterRules.";
FastMeanColumn::usage = "Computes column mean of a matrix. 10x faster than Mean."
FastMeanRow::usage = "Computes in parallel the Mean of each element of a list."
FastTotalRow::usage = "Computes in parallel the Total of each element of a list."
FastStandardDeviation::usage = "Computes column standard deviation of a matrix. 5x faster than StandardDeviation"
WeightedMean::usage = "WeightedMean[list, weights] compute Mean of list weighted by weights"
WeightedStandardDeviation::usage = "WeightedStandardDeviation[list, weights] compute StandardDeviation of list weighted by weights"
MinLoc::usage = "Outputs the location of the smallest element in a list of real "
MaxLoc::usage = "Outputs the location of the largest element in a list of real "
ClassProbabilities::usage = "ClassProbabilities[classes] computes the probability vector of each class {p1, p2, ... pn} given a list of classes (striclty positive integers).
ClassProbabilities[classes, weigths] weigths each element of classes."
NominalEncoder::usage = "NominalEncoder[{nominalindices_, numericalindices_}, labelers_][feature_List]
encodes nominal variables of feature witha binary coding."
Labeler::usage = "Labeler[labelnames, rule_][indices] labels indices. Labeler[labelnames, rule_][labels, \"Inverse\"] unlabel them."
BinaryLabeler::usage = "BinaryLabeler[labelnames, rule_][binarycode] labels binarycode. Labeler[labelnames, rule_][labels, \"Inverse\"] unlabel them."

PackageScope["ParsePredictorDataset"]

PackageScope["ParseClassifierDataset"]

ParsePredictorDataset::usage = "ParsePredictorDataset[dataset] outputs the dataset of a predictor in the right internal format {X, Y}"
ParseClassifierDataset::usage = "ParseClassifierDataset[dataset] outputs the dataset of a classifier in the right internal format {X, Y}"
(* General functions that are used in the Stats` files. *)

AbortMachineLearning::usage = "abort process and returns $Failed";
CatchAbortMachineLearning::usage = "Catch AbortMachineLearning";

PackageScope["ShuffleSet"]

ShuffleSet::usage = "ShuffleSet[{X, Y}] shuffles X and Y with the same indices"

PackageScope["iClassifierFunction"]
PackageScope["iNoneClassifierFunction"]
PackageScope["iPredictorFunction"]
PackageScope["iNonePredictorFunction"]

ClassifierFunction::winpt = "Argument `` does not have the right format.";
PredictorFunction::winpt = "Argument `` does not have the right format.";
ClassifierFunction::naprop = "Property `` is not available for this ClassifierFunction[\[Ellipsis]]."
ClassifierFunction::napropn = "Property `` with argument None is not available for this ClassifierFunction[\[Ellipsis]]."
ClassifierFunction::napropnn = "Argument None is not available for this ClassifierFunction[\[Ellipsis]]."
PredictorFunction::naprop = "Property `` is not available for this PredictorFunction[\[Ellipsis]]."
PredictorFunction::napropn = "Property `` with argument None is not available for this PredictorFunction[\[Ellipsis]]."
PredictorFunction::napropnn = "Argument None is not available for this PredictorFunction[\[Ellipsis]]."


ClassifierFunction[name_, evaluationdata_, metadata_, options_][x:Except[None], prop___] := CatchAbortMachineLearning[
	Module[
		{pattern, preprocessorx},
		pattern = evaluationdata[[1, 3]];
		preprocessorx = evaluationdata[[1, 1]];
		If[FreeQ[{0, 1}, Length[{prop}]], AbortMachineLearning["argt", ClassifierFunction["\[Ellipsis]"], Length[{prop}]+1, 1, 2]];
		Switch[x,
			pattern,
				First @ iClassifierFunction[name][evaluationdata, metadata, options][preprocessorx[{x}], prop],
			{pattern..},
				iClassifierFunction[name][evaluationdata, metadata, options][preprocessorx[x], prop],
			{pattern, __},
				If[!MatchQ[#, pattern], AbortMachineLearning["winpt", #]] & /@ x,
			_,
				AbortMachineLearning["winpt", x]
		]
	]
	,
	Defer[ClassifierFunction[name, evaluationdata, metadata, options][Short[x, 10], prop]]
];
ClassifierFunction[name_, evaluationdata_, metadata_, options_][None, prop___] := CatchAbortMachineLearning[
	iNoneClassifierFunction[name][evaluationdata, metadata, options][prop]
	,
	Defer[ClassifierFunction[name, evaluationdata, metadata, options][None, prop]]
];

PredictorFunction[name_, evaluationdata_, metadata_, options_][x:Except[None], prop___] := CatchAbortMachineLearning[ 
	Module[
		{pattern, preprocessorx},
		pattern = evaluationdata[[1, 3]];
		preprocessorx = evaluationdata[[1, 1]];
		If[FreeQ[{0, 1}, Length[{prop}]], AbortMachineLearning["argt", PredictorFunction["\[Ellipsis]"], Length[{prop}]+1, 1, 2]];
		Switch[x,
			pattern,
				First @ iPredictorFunction[name][evaluationdata, metadata, options][preprocessorx[{x}], prop],
			{pattern..},
				iPredictorFunction[name][evaluationdata, metadata, options][preprocessorx[x], prop],
			{pattern, __},
				If[!MatchQ[#, pattern], AbortMachineLearning["winpt", #]] & /@ x,
			_,
				AbortMachineLearning["winpt", x]
		]
	]
	,
	Defer[PredictorFunction[name, evaluationdata, metadata, options][Short[x, 10], prop]]
];

PredictorFunction[name_, evaluationdata_, metadata_, options_][None, prop___] := CatchAbortMachineLearning[
	iNonePredictorFunction[name][evaluationdata, metadata, options][prop]
	,
	Defer[PredictorFunction[name, evaluationdata, metadata, options][None, prop]]
];

AbortMachineLearning[] := Throw[$Failed, "AbortMachineLearning"];
AbortMachineLearning[value_] := Throw[value, "AbortMachineLearning"];
AbortMachineLearning[values__] := Throw[{values}, "AbortMachineLearning"];

SetAttributes[CatchAbortMachineLearning, HoldAll];
CatchAbortMachineLearning[body_, input_] := Catch[body, "AbortMachineLearning", abortfunction[#1, #2, input] &];

abortfunction[$Failed, tag_, input_] := input;
abortfunction[messagetag_, tag_, input_] := With[{symbol = getsymbol[input]},
	Message[MessageName[symbol, messagetag]];
	input
];
abortfunction[{messagetag_, extravalues__}, tag_, input_] := Module[
	{values},
	values = Sequence @@ Replace[{extravalues}, "Input"-> input, {1}];
	With[{symbol = getsymbol[input]}, Message[MessageName[symbol, messagetag], values]];
	input
];

getsymbol[expr_] := With[{pos = Position[expr, _Symbol]}, expr[[Sequence @@ pos[[2]]]]];

FilterOptions[head_Symbol, opts___] := Sequence @@ FilterRules[{opts}, Options[head]];
FilterOptions /: head_[args___, FilterOptions[opts___Rule], rest___] := head[args, FilterOptions[head, opts], rest];

PackageScope["MostProbableClasses"]
MostProbableClasses[distlist_, threshold_] := With[
	{sorted = Reverse[SortBy[#, Last]] & /@ distlist},
	Function[s, Select[s, Last[#] > threshold*s[[1, 2]] &]] /@ sorted
];

PackageScope["ClassifyFormatQ"]
PackageScope["PredictFormatQ"]
PackageScope["CheckOptions"]
PackageScope["CheckClassifyData"]
PackageScope["CheckPredictData"]
PackageScope["CheckParsedData"]

ClassifyFormatQ[dataset_] := MatchQ[dataset, (_Rule | {__Rule} | _Association)];
PredictFormatQ[dataset_] := MatchQ[dataset, (_Rule | {__Rule})];

CheckOptions[opts:OptionsPattern[], function_] := If[!MemberQ[First /@ Options[function], #],
	AbortMachineLearning["optx", #, "Input"];
] & /@ {opts}[[All, 1]];

CheckClassifyData[___] := AbortMachineLearning[];
CheckClassifyData[{x_List, y_List}] := Module[
	{lx, ly},
	lx = Length[x];
	ly = Length[y];
	CheckParsedData[{x, y}];
	If[lx<2, AbortMachineLearning["smdat"]];
	If[Length[DeleteDuplicates[y]]<2, AbortMachineLearning["mincnb"]];
	If[#2 == 1, AbortMachineLearning["sglex", #1]] & @@@ Tally[y];
];

CheckParsedData[___] :=AbortMachineLearning[];
CheckParsedData[{x_List, y_List}] := Module[
	{lx, ly},
	lx = Length[x];
	ly = Length[y];
	If[lx != ly, AbortMachineLearning["unbal", lx, ly]];
	If[ArrayDepth[y]>1, AbortMachineLearning["manyval"]];
	If[!MatchQ[y, {Except[_List]..}], AbortMachineLearning["bdval"]];
	Switch[x,
		{__List},
			If[ArrayDepth[x] < 2, AbortMachineLearning["bftlgth"]];
			If[!MatchQ[x, {{Except[_List]..}..}], AbortMachineLearning["bfttype"]];
		,
		_List,
			If[!MatchQ[x, {Except[_List]..}], AbortMachineLearning["bfttype"]];
	];
];

CheckPredictData[___] := AbortMachineLearning[];
CheckPredictData[{x_List, y_List}] := Module[
	{lx, ly},
	lx = Length[x];
	ly = Length[y];
	CheckParsedData[{x, y}];	
	If[lx<2, AbortMachineLearning["smdat"]];
	If[!MatchQ[N[y], {__Real}], AbortMachineLearning["bdval"]];
];
(*should check that there is no list, in x and y.. How to do that fast?*)

ParsePredictorDataset[examples:{__Rule}] := Transpose[List @@@ examples]; (* copy the dataset... *)
ParsePredictorDataset[X_ -> Y_] := {X, Y};
ParsePredictorDataset[___] := AbortMachineLearning[];

ParseClassifierDataset[examples:{__Rule}] := Transpose[List @@@ examples];
ParseClassifierDataset[X_ -> Y_] := {X, Y};
ParseClassifierDataset[dataset_Association] := Module[
	{X, Y, classes},
	classes = Keys[dataset];
	X = dataset /@ classes;
	Y = Table[#1, {#2}] & @@@ Transpose[{classes, Length /@ X}];
	{Flatten[X, 1], Flatten[Y, 1]}	
];
ParseClassifierDataset[___] := AbortMachineLearning[];

ShuffleSet[{X_, Y_}] := Module[
	{indices},
	indices = RandomSample[Range[Length[Y]]];
	{X[[indices]], Y[[indices]]}
];

FastMeanColumn = MachineLearning`Libraries`Compile[{{x, _Real, 2}}, 
	Total[x]/Length[x],
	CompilationTarget -> "C"
];

FastMeanRow = MachineLearning`Libraries`Compile[{{x, _Real, 1}}, 
	Total[x]/Length[x], 
	RuntimeAttributes -> {Listable}, 
	Parallelization->True,
	CompilationTarget -> "C"
];

FastTotalRow = MachineLearning`Libraries`Compile[{{x, _Real, 1}}, 
	Total[x], 
	RuntimeAttributes -> {Listable}, 
	Parallelization->True,
	CompilationTarget -> "C"
];

FastStandardDeviation = MachineLearning`Libraries`Compile[{{x, _Real, 2}},
	Module[
		{xmod, n, mean},
		n = Length[x];
		mean = Total[x]/n;
		xmod = # - mean & /@ x;
		Sqrt[Total[xmod^2]/(n-1.)]
	],
	CompilationTarget -> "C"
];

WeightedMean = MachineLearning`Libraries`Compile[
	{{values, _Real, 1}, {weights, _Real, 1}},
	Total[values*weights]/Total[weights],
	RuntimeAttributes->{Listable},
	Parallelization->True,
	CompilationTarget -> "C"
];

WeightedStandardDeviation = MachineLearning`Libraries`Compile[
	{{values, _Real, 1}, {weights, _Real, 1}},
	Module[{mu, mu2, var},
		mu = Total[values*weights]/Total[weights];
		mu2 = Total[values^2*weights]/Total[weights];
		var = (mu2 - mu^2)/Max[(1.-Total[weights^2]/Total[weights]^2), 1.];
		Sqrt[var]
	], 
	RuntimeAttributes -> {Listable},
	Parallelization -> True,
	CompilationTarget -> "C"
];

(*MaxLoc = MachineLearning`Libraries`Compile[{{list, _Real, 1}},
	Module[
		{extr, newval, loc},
		extr = First[list];
		loc = 1;
		Do[
			newval = list[[iter]];
			If[newval > extr, 
				extr = newval;
				loc = iter
			];
			, {iter, 2, Length[list]}
		];
		loc
	]
	, RuntimeAttributes-> {Listable}, Parallelization->True, CompilationTarget -> "C"
];*)
MaxLoc[x_List] := First[RandomChoice[Position[x, Max[x]]]];
MaxLoc[x:{__List}] := First[RandomChoice[Position[#, Max[#]]]] & /@ x;
MinLoc[x_List] := First[RandomChoice[Position[x, Min[x]]]];
MinLoc[x:{__List}] := First[RandomChoice[Position[#, Min[#]]]] & /@ x;

Options[ClassProbabilities] = {	
	"ClassNumber" -> Automatic,
	"AdditiveSmoothing" -> 0.
};
ClassProbabilities[classes_, opts:OptionsPattern[]] := Module[
	{nclass, smooth},
	nclass = OptionValue["ClassNumber"];
	If[nclass === Automatic, nclass = Max[classes]];
	smooth = OptionValue["AdditiveSmoothing"];
	If[Length[smooth] == 0, smooth = Table[smooth, {nclass}]];
	iClassProbabilities[classes, nclass, smooth]
];
ClassProbabilities[classes_, weigths_, opts:OptionsPattern[]] := Module[
	{nclass, smooth},
	nclass = OptionValue["ClassNumber"];
	If[nclass === Automatic, nclass = Max[classes]];
	smooth = OptionValue["AdditiveSmoothing"];
	If[Length[smooth] == 0, smooth = Table[smooth, {nclass}]];
	iClassProbabilitiesWeight[classes, nclass, smooth, weigths]
];
iClassProbabilities = MachineLearning`Libraries`Compile[
	{{class, _Integer, 1}, {nclass, _Integer}, {smooth, _Real, 1}}, 
	Module[{proba},
		proba = Table[0., {nclass}];
		Do[
			proba[[class[[iter]]]] ++;
			,
			{iter, Length[class]}
		];
		proba = proba + smooth;
		proba/Total[proba]
	],
	RuntimeAttributes -> {Listable}, Parallelization -> True, CompilationTarget -> "C"
];
iClassProbabilitiesWeight = MachineLearning`Libraries`Compile[
	{{class, _Integer, 1}, {nclass, _Integer}, {smooth, _Real, 1}, {weights, _Real, 1}}, 
	Module[{proba},
		proba = Table[0., {nclass}];
		Do[
			proba[[class[[iter]]]] += weights[[iter]]
			,
			{iter, Length[class]}
		];
		proba = proba + smooth;
		proba/Total[proba]
	],
	RuntimeAttributes -> {Listable}, Parallelization -> True, CompilationTarget -> "C"
];

