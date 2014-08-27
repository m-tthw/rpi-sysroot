(* ::Package:: *)

Package["MachineLearning`"]

PackageImport["Developer`"]

PackageScope["CrossValidationHyperparameterSelect"]

CrossValidationHyperparameterSelect::usage = "Performs a search for the optimal hyperparameter with cross validation.
CrossValidationHyperparameterSelect[data, hyperparameters,
optimizer, costfunction, theta0] outputs the value in hyperparameters that minimizes the function 
costfunction[validationset, theta] with theta = optimizer[trainingset, hyperparameter, theta].
theta0 is the initial value for theta before each optimization. 
If \"WarmRestart\"-> True, it does not reset the value of theta between optimization.
To be used internaly for now."

PackageScope["KFoldSplit"]

KFoldSplit::usage = " KFoldSplit[data, nsplit] outputs nsplit pairs {trainingset, validationset} according to the K-Fold cross validation procedure.
KFoldSplit[data, nsplit, i] outputs the i-th split.
data can be X or {X, Y} where X is the list of feature vectors, and Y is the response vector."

PackageScope["RandomSplit"]

RandomSplit::usage = "RandomSplit[data, splitsize] splits data into {trainingset, validationset} where validationset is a random sample of length splitsize.
RandomSplit[data, splitsize, nsplit] performs this procedure nplit times.
data can be X or {X, Y} where X is the list of feature vectors, and Y is the response vector."


PackageScope["ParseCrossValidationOptions"]

ParseCrossValidationOptions::usage = "ParseCrossValidationOptions[opts:OptionsPattern[]] outputs {SplitNumber, SplitFunction} from cross validation options"


PackageScope["CrossValidationModelSelect"]

CrossValidationModelSelect::usage = "CrossValidationModelSelect[data, {model1, model2, ...}] outputs the best model acording to a cross validation analysis."


PackageScope["CrossEstimate"]

CrossEstimate::usage = "CrossEstimate[data, optimizer_Function, function_Function, theta0] is used to obtain an estimate of function[data, theta] 
by the cross-validation. theta is obtain with optimizer[trainingset, thetainitial]. theta0 is the initial parameter for the optimizer.";


Options[CrossValidationModelSelect] = {
	"CrossValidation" -> Automatic,
	"Stratified" -> Automatic,
	"ObjectiveFunction"-> Automatic, (*Automatic, "LeastSquare", "Likelihood", "Accuracy"*)
	"ModelOptions" -> None 
};
CrossValidationModelSelect::wtype = "Models must be of the same type, either a PredictorFunction or ClassifierFunction object.";
CrossValidationModelSelect::wobj = "Unknown value for option \"ObjectiveFunction\".";
CrossValidationModelSelect[{X_, Y_}, models_, opts:OptionsPattern[]]:= Module[
	{
		finalpredictors, finalpredictorsoptions, nsplit, splitfunction, type, obj,
		trainingset, xv, yv, predictors, predictions, costs, modeloptions
	},
	modeloptions = OptionValue["ModelOptions"];
	If[modeloptions === None,
		finalpredictors = #[{X, Y}] & /@ models;
		,
		finalpredictors = #[{X, Y}, Sequence @@ modeloptions] & /@ models;
	];
	If[Length[finalpredictors] == 1, Return[First@finalpredictors]];
	finalpredictorsoptions = {#[None, "Options"]} & /@ finalpredictors;
	{nsplit, splitfunction} = ParseCrossValidationOptions[
		Sequence @@ OptionValue["CrossValidation"]
	];
	type = DeleteDuplicates[Head /@ finalpredictors];
	obj = OptionValue["ObjectiveFunction"];
	obj = Switch[type,
		{PredictorFunction},
			"LeastSquare"
		,
		{ClassifierFunction},
			Switch[obj,
				Automatic, "Likelihood",
				_, obj
			]
		,
		_,
			Message[CrossValidationModelSelect::wtype];
			AbortMachineLearning[];
	];
	costs = Mean @ Table[
		{trainingset, {xv, yv}} = splitfunction[{X, Y}, iter];
		predictors = #1[trainingset, Sequence @@ #2] & @@@ Transpose[{models, finalpredictorsoptions}];
		Switch[obj,
			"LeastSquare",
				predictions = #[xv] & /@ predictors;
				Total[(yv - #)^2]/Length[#] & /@ predictions
			,
			"Likelihood",
				predictions = #[xv, "DistributionList"][[All, All, 2]] & /@ predictors;
				- Total[Log[#1[[#2]]] & @@@ Transpose[{#, yv}]]/Length[#] & /@ predictions
			,
			"Accuracy",
				predictions = #[xv] & /@ predictors;
				1. - Count[yv - #, 0]/Length[#] & /@ predictions
			,
			_,
				Message[CrossValidationModelSelect::wobj];
				AbortMachineLearning[];
		]
		,
		{iter, nsplit}
	];
	finalpredictors[[MinLoc[costs]]]
];

(* hyperparameter search. Not suited for selecting from two models. *)

Options[CrossValidationHyperparameterSelect] = {
	"CrossValidation" -> Automatic,
	Method -> "LineSearch",
	"WarmRestart" -> False,
	"Stratified" -> Automatic,
	"InitialHyperparameter" -> 1., (* for "LineSearch" Method *)
	"Order" -> "ConstantData" (* for "ListSearch" Method *)
};
(* "CrossValidation" options can be :
		{Method -> "KFoldSplit", "SplitNumber" -> 4},
		{Method -> "RandomSplit", "SplitNumber" -> 2, "SplitSize" -> 10},
		{Method -> customfunction_Function, "SplitNumber" -> 3},
		{Method -> "External", "ValidationSet" -> xxx}
	order does not matter
	
	Specific to hyperparameter search:
	
	Method can be (for now...):
		"ListSearch"
		"LineSearch"
	"Order" can be:
		"ConstantData"
		"ConstantParameter"
*)
CrossValidationHyperparameterSelect[arguments__, opts:OptionsPattern[]] := Module[
	{function},
	function = Switch[OptionValue[Method]
		,
		"ListSearch", crossValidationHyperparameterListSelect
		,
		"LineSearch", crossValidationHyperparameterLineSelect
		,
		_, AbortMachineLearning[];
	];
	function[arguments, FilterOptions[function, opts]]
];

ParseCrossValidationOptions::noset = "The validation set is empty."
Options[ParseCrossValidationOptions] = {
	Method -> "KFoldSplit",
	"SplitNumber" -> 4,
	"SplitSize" -> .2,
	"ValidationSet"-> {}
};
ParseCrossValidationOptions[opts:OptionsPattern[]] := Module[
	{nsplit, splitfunction, val},
	Switch[OptionValue[Method],
		"KFoldSplit",
			nsplit = OptionValue["SplitNumber"];
			splitfunction = KFoldSplit[#1, nsplit, #2] &;
		,
		"RandomSplit",
			nsplit = OptionValue["SplitNumber"];
			splitfunction = RandomSplit[#1, OptionValue["SplitSize"]] &;
		,
		"External",
			nsplit = 1;
			val = OptionValue["ValidationSet"];
			If[val === {}, Message[ParseCrossValidationOptions::noset]; AbortMachineLearning[];];
			splitfunction = Function[{#1, val}];
		,
		_,
			nsplit = OptionValue["SplitNumber"];
			splitfunction = OptionValue[Method];
	];
	{nsplit, splitfunction}
];

RandomSplit::wsplt = "Wrong split size."
RandomSplit[X_, splitsize_] := Module[
	{sample, splitsizemod},
	splitsizemod = Flatten[{splitsize}];
	splitsizemod = If[# < 1, Ceiling[#*Length[X]], Round[#]] & /@ splitsizemod;
	splitsizemod = Min[splitsizemod];
	If[splitsizemod >= Length[X], Message[RandomSplit::wsplt]; AbortMachineLearning[];];
	sample = RandomSample[Range[Length[X]]];
	{X[[sample[[;; splitsizemod]]]], X[[sample[[splitsizemod+1 ;;]]]]}
];
RandomSplit[{X:{{__}..}, Y_}, splitsize_] := Module[
	{sample, s1, s2, splitsizemod},
	splitsizemod = Flatten[{splitsize}];
	splitsizemod = If[# < 1, Ceiling[#*Length[X]], Round[#]] & /@ splitsizemod;
	splitsizemod = Min[splitsizemod];
	If[splitsizemod >= Length[X], Message[RandomSplit::wsplt]; AbortMachineLearning[];];
	sample = RandomSample[Range[Length[X]]];
	s1 = sample[[splitsizemod + 1 ;;]];
	s2 = sample[[;; splitsizemod]];
	{{X[[s1]], Y[[s1]]}, {X[[s2]], Y[[s2]]}}
];
RandomSplit[X_, splitsize_, splitnumber_] := Table[RandomSplit[X, splitsize], {splitnumber}];

KFoldSplit::wsplt = "Wrong split number."
KFoldSplit::witer = "Wrong split index."
KFoldSplit[X_, nsplit_Integer, iter_Integer] := Module[
	{m, p1, m1, m2, lb, rb},
	m = Length[X];
	If[nsplit < 2 || nsplit > m, Message[KFoldSplit::wsplt]; AbortMachineLearning[];];
	If[iter < 1 || iter > nsplit, Message[KFoldSplit::witer]; AbortMachineLearning[];];
	m1 = Floor[m/nsplit];
	m2 = m1+1;
	p1 = m2*nsplit - m;
	{lb, rb} = If[
		iter <= p1
		,
		(iter-1)*m1 + {1, m1}
		, 
		(iter-1)*m2-p1 + {1, m2}
	];
	{Join[X[[;; lb-1]], X[[rb+1 ;;]]], X[[lb ;; rb]]}
];
KFoldSplit[{X:{{__}..}, Y_}, nsplit_Integer, iter_Integer] := Transpose[
	{KFoldSplit[X, nsplit, iter], KFoldSplit[Y, nsplit, iter]}
];
KFoldSplit[data_, nsplit_Integer] := KFoldSplit[data, nsplit, #] & /@ Range[nsplit];

(* compute the cross validated cost and outputs the modified parameter for warm restart use

	optimizer[data, theta0] optimize with data data, and initial parameter theta0.
	cost[data, theta] compute cost of data with parameter theta.
*)

Options[CrossEstimate] = {
	"CrossValidation" -> Automatic,
	"WarmRestart" -> False,
	"Stratified" -> Automatic,
	"Seed" -> None
};
CrossEstimate[data_, optimizer_Function, costfunction_Function, theta0_, opts:OptionsPattern[]] := Module[
	{trainingset, validationset, theta, warm, nsplit, splitfunction, cost, cvopts, seed},
	cvopts = OptionValue["CrossValidation"];
	If[cvopts === Automatic, (*Should do something smarter.*)
		cvopts = {Method -> "KFoldSplit", "SplitNumber" -> 2}
	];
	seed = OptionValue["Seed"];
	If[seed =!= None, SeedRandom[seed]];
	theta = theta0;
	{nsplit, splitfunction} = ParseCrossValidationOptions[Sequence @@ cvopts];
	warm = OptionValue["WarmRestart"];
	cost = Mean @ Table[
		{trainingset, validationset} = splitfunction[data, iter];
		If[!warm, theta = theta0];
		theta = optimizer[trainingset, theta];
		costfunction[validationset, theta]
		,
		{iter, nsplit}
	];
	{cost, theta}
];

Options[crossValidationHyperparameterListSelect] = {
	"CrossValidation" -> Automatic,
	"WarmRestart" -> False,
	"Stratified" -> Automatic,
	"Order" -> "ConstantData" (*can be either "ConstantData" or "ConstantParameter", works only for the full search.*)
};
crossValidationHyperparameterListSelect[data_, hyperparameters_List, arguments__, opts:OptionsPattern[]] := Module[
	{costs, function},
	function = Switch[OptionValue["Order"]
		,
		"ConstantParameter", crossValidatedCostsConstantParameter
		,
		"ConstantData", crossValidatedCostsConstantData
		,
		_, AbortMachineLearning[];
	];
	costs = function[data, hyperparameters, arguments, FilterOptions[function, opts]];
	First@hyperparameters[[Ordering[costs, 1]]]
];

Options[crossValidatedCostsConstantParameter] = {
	"CrossValidation" -> Automatic,
	"WarmRestart" -> False,
	"Stratified" -> Automatic
};
crossValidatedCostsConstantParameter[data_, hyperparameters_List, optimizer_Function, costfunction_Function, theta0_, opts:OptionsPattern[]]:= Module[
	{cost, theta, seed, warm},
	seed = RandomInteger[$MaxMachineInteger];
	warm = OptionValue["WarmRestart"];
	theta = theta0;
	Table[
		{cost, theta} = CrossEstimate[data, optimizer[#1, hyper, #2] &, 
		costfunction, theta, FilterOptions[CrossEstimate, opts], "Seed" -> seed];
		If[!warm, theta = theta0];
		cost
		,
		{hyper, hyperparameters}
	]
];

Options[crossValidatedCostsConstantData] = {
	"CrossValidation" -> Automatic,
	"WarmRestart" -> False,
	"Stratified" -> Automatic
};
crossValidatedCostsConstantData[data_, hyperparameters_List, optimizer_Function, costfunction_Function, theta0_, opts:OptionsPattern[]]:= Module[
	{theta, warm, nsplit, splitfunction, trainingset, validationset},
	warm = OptionValue["WarmRestart"];
	theta = theta0;
	{nsplit, splitfunction} = ParseCrossValidationOptions[Sequence @@ OptionValue["CrossValidation"]];
	Mean @ Table[
		{trainingset, validationset} = splitfunction[data, iter];
		(
			If[!warm, theta = theta0];
			theta = optimizer[trainingset, #, theta];
			costfunction[validationset, theta]
		) & /@ hyperparameters
		,
		{iter, nsplit}
	]
];

(* 
	performs a line search in the hyperparameter list:
	explores the descending direction until local minimum is found.

	optimizer[trainingset, hyper, theta0] optimizes with data trainingset, hyperparmeter hyper, and initial parameter theta0.
	cost[validationset, theta] computes cost of data validationset with parameter theta.
	when custom, "SplitFunction" should thake as input [data, iter], and should outputs {trainingset, validationset}
*)

Options[crossValidationHyperparameterLineSelect] = {
	"CrossValidation" -> Automatic,
	"WarmRestart" -> False,
	"InitialHyperparameter" -> Automatic,
	"Stratified" -> Automatic
};
crossValidationHyperparameterLineSelect[data_, hyperparameters_List,
optimizer_Function, costfunction_Function, theta0_, opts:OptionsPattern[]] := Module[
	{sortedhyperparameters, currenthyperindex, newhyperindex, currentcost, newcost, nhyper, direction, seed, theta, warm},
	warm = OptionValue["WarmRestart"];
	sortedhyperparameters = Sort[hyperparameters];
	nhyper = Length[sortedhyperparameters];
	currenthyperindex = OptionValue["InitialHyperparameter"];
	If[currenthyperindex === Automatic,
		currenthyperindex = Ceiling[nhyper/2];
		,
		currenthyperindex = First[Nearest[sortedhyperparameters -> Automatic, currenthyperindex, 1]];
	];
	seed = RandomInteger[$MaxMachineInteger];
	theta = theta0;
	{currentcost, theta} = CrossEstimate[data, optimizer[#1, sortedhyperparameters[[currenthyperindex]], #2] &, 
		costfunction, theta, FilterOptions[CrossEstimate, opts], "Seed" -> seed];
	If[!warm, theta = theta0];
	direction = 1;
	newhyperindex = currenthyperindex + direction;
	If[newhyperindex > nhyper,
		direction = -1
		,
		{newcost, theta} = CrossEstimate[data, optimizer[#1, sortedhyperparameters[[newhyperindex]], #2] &, 
			costfunction, theta, FilterOptions[CrossEstimate, opts], "Seed" -> seed];
		If[!warm, theta = theta0];
		If[newcost < currentcost,
			direction = 1;
			currenthyperindex = newhyperindex;
			currentcost = newcost;
			,
			direction = -1;
		];
	];
	newhyperindex = currenthyperindex + direction;
	While[newhyperindex <= nhyper && newhyperindex >= 1,
		{newcost, theta} = CrossEstimate[data, optimizer[#1, sortedhyperparameters[[newhyperindex]], #2] &, 
			costfunction, theta, FilterOptions[CrossEstimate, opts], "Seed" -> seed];
		If[!warm, theta = theta0];
		If[newcost > currentcost, Break[]];
		currenthyperindex = newhyperindex;
		currentcost = newcost;
		newhyperindex = currenthyperindex + direction;
	];
	sortedhyperparameters[[currenthyperindex]]
];
