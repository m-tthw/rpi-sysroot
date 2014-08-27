(* ::Package:: *)

Package["MachineLearning`"]

PackageImport["Developer`"]

PackageScope["LinearPredictorFunction"]

LinearPredictorFunction::usage = "LinearPredictorFunction[{X, Y}] outputs a PredictorFunction using the linear regression model trained on the feature matrix X and the reponse vector Y.
PredictorFunction[newfeatures] outputs the most likely response value given newfeatures."

Options[LinearPredictorFunction] = {
	"L2Regularization" -> Automatic,
	Weights -> Automatic, (*not fully implemented yet*)
	"CrossValidation" -> Automatic,
	PerformanceGoal-> Automatic,
	NominalVariables -> Automatic,
	"BinaryEncoder" -> Preprocessor["Identity"]
	(*
	Method that should be implemented:
		"L1Regularization"
		"ErrorDistribution" ( -> NormalDistribution, LaplaceDistribution, ... )
	*)
};

LinearPredictorFunction[{X_, Y_}, opts:OptionsPattern[]] := Module[
	{m, n, Xmod, Ymod, preprocessorx, preprocessory, theta, lambdaopt, stddev, cvopts, evaluationdata, metadata, options, sigmay, muy},
	{m, n} = Dimensions[X];
	preprocessorx = JoinPreprocessors[OptionValue["BinaryEncoder"], Preprocessor["PrependOne"]];
	Xmod = preprocessorx[X];
	muy = Mean[Y];
	sigmay = StandardDeviation[Y];
	preprocessory = Preprocessor["Sequence", {
		Preprocessor["ToMatrix"], 
		Preprocessor["Standardize", {muy, sigmay}]
	}];
	Ymod = preprocessory[Y];
	cvopts = OptionValue["CrossValidation"];
	If[MatchQ[cvopts, {___, Method -> "External", ___}],
		cvopts = Replace[cvopts, ("ValidationSet" -> {xv_, yv_}) :> "ValidationSet" -> {preprocessorx[xv], preprocessory[yv]}, {1}];
	];
	{theta, lambdaopt, stddev, cvopts} = iLinearPredictorFunction[{Xmod, Ymod},
		"CrossValidation" -> cvopts,
		FilterOptions[iLinearPredictorFunction, opts]
	];
	options = Options[LinearPredictorFunction];
	options = DeleteDuplicates[Join[{opts}, options], First[#1] === First[#2] &];
	options = Replace[options,
		{
			("L2Regularization"-> _) -> ("L2Regularization" -> lambdaopt),
			("CrossValidation"-> _) -> ("CrossValidation" -> cvopts)
		},
		{1}
	];
	evaluationdata = {{preprocessorx, preprocessory, Table[Except[_List], {n}]}, {theta}};
	metadata = {{{m, n}, {Table[{}, {n}]}, {"value", {Min[Y], Max[Y]}}}, {stddev*sigmay}};
	PredictorFunction["LinearRegression", evaluationdata, metadata, options]
];

iPredictorFunction["LinearRegression"][___][data_, prop_] := AbortMachineLearning["naprop", prop];
iNonePredictorFunction["LinearRegression"][___][prop_] := AbortMachineLearning["napropn", prop];

iPredictorFunction["LinearRegression"][eval_, _, _][x_] := Module[
	{preprocessory, theta},
	preprocessory = eval[[1, 2]];
	theta = eval[[2, 1]];
	preprocessory[x.theta, "Inverse"]
];

iPredictorFunction["LinearRegression"][eval_, meta_, _][x_, "Distribution"] := Module[
	{preprocessory, theta, stddev, y},
	preprocessory = eval[[1, 2]];
	theta = eval[[2, 1]];
	stddev = meta[[2, 1]];
	y = preprocessory[x.theta, "Inverse"];
	NormalDistribution[#, First[stddev]] & /@ y
];

iNonePredictorFunction["LinearRegression"][_, _, options_]["Options"] := Sequence @@ options;

iNonePredictorFunction["LinearRegression"][arg___]["PredictionPlot"] := PredictionPlot[PredictorFunction["LinearRegression", arg]];
iNonePredictorFunction["LinearRegression"][arg___][{"PredictionPlot", arg2___}] := PredictionPlot[PredictorFunction["LinearRegression", arg], arg2];


iNonePredictorFunction["LinearRegression"][eval_, _, _]["InternalParameters"] := eval[[2, 1]];

(* beware bug if n=1 without {}, and image or text... *)
iNonePredictorFunction["LinearRegression"][eval_, meta_, opts_][] := Module[
	{var, function, n, pattern},
	If[!MemberQ[{"numerical", "nominal"}, #2], AbortMachineLearning["naprop", "None, \"DistributionList\""]] & @@@ meta[[1, 2]];
	n = meta[[1, 1, 2]];
	pattern = eval[[1, 3]];
	var = If[MatchQ[pattern, _List], 
		Table[Slot[i], {i, 1, n}]
		,
		#
	];
	function = Function[Evaluate[
		ExpandAll @ PredictorFunction["LinearRegression", eval, meta, opts][var]
	]];
	function = ReplaceAll[function, Boole[Equal[e1_, e2_]] :> Boole[SameQ[e1, e2]]];
	function
];
iNonePredictorFunction["LinearRegression"][eval_, meta_, opts_]["Distribution"] := Module[
	{var, function, n, pattern},
	If[!MemberQ[{"numerical", "nominal"}, #2], AbortMachineLearning["naprop", None]] & @@@ meta[[1, 2]];
	n = meta[[1, 1, 2]];
	pattern = eval[[1, 3]];
	var = If[MatchQ[pattern, _List], 
		Table[Slot[i], {i, 1, n}]
		,
		#
	];
	function = Function[Evaluate[
		ExpandAll @ PredictorFunction["LinearRegression", eval, meta, opts][var, "Distribution"]
	]];
	function = ReplaceAll[function, Boole[Equal[e1_, e2_]] :> Boole[SameQ[e1, e2]]];
	function
];

Options[iLinearPredictorFunction] = {
	"L2Regularization" -> OptionValue[LinearPredictorFunction, "L2Regularization"],
	"CrossValidation" -> OptionValue[LinearPredictorFunction, "CrossValidation"],
	PerformanceGoal-> Automatic
};
iLinearPredictorFunction[{X_, Y_}, opts:OptionsPattern[]] := Module[
	{m, n, l2reg, lambdaopt, theta, cvopts, stddev, xe, ye},
	l2reg = OptionValue["L2Regularization"];
	If[Length[l2reg]>1 || l2reg === Automatic,
		{lambdaopt, cvopts} = crossvalidateLinearPredictorFunction[{X, Y}, l2reg, 
			FilterOptions[crossvalidateLinearPredictorFunction, opts]
		];
		,
		lambdaopt = First@Flatten[{l2reg}];
		cvopts = OptionValue["CrossValidation"];
	];
	theta = linearOptimize[convenientdata[{X, Y}], lambdaopt];
	If[MatchQ[cvopts, {___, "ValidationSet" -> {_, _}, ___}], (*use a validation set if possible*)
		{xe, ye} = First@Cases[cvopts, ("ValidationSet" -> v_) :> v];
		stddev = Sqrt[Total[(ye-xe.theta)^2.]/Length[ye]];
		,
		{m, n} = Dimensions[X];
		stddev = Sqrt[Total[(Y - X.theta)^2.]/Max[m - n, 1.]];
		(*future: use the cross validation instead.*)
	];
	{theta, lambdaopt, stddev, cvopts}
];

Options[crossvalidateLinearPredictorFunction] = {
	"CrossValidation" -> OptionValue[LinearPredictorFunction, "CrossValidation"],
	PerformanceGoal-> Automatic
};

crossvalidateLinearPredictorFunction[{X_, Y_}, l2reg_, opts:OptionsPattern[]] := Module[
	{m, n, lambda, lambdaopt, nsplit, splitfunction, theta0, cvopts, method},
	{m, n} = Dimensions[X];
	cvopts = OptionValue["CrossValidation"];
	If[cvopts === Automatic,
		Switch[OptionValue[PerformanceGoal],
			"Speed", Return[{1., cvopts}] (*Drastic choice*)
			,
			"Quality", 
				Which[
					m >= 1000000, Return[{1., cvopts}]
					,
					m >= 100000, cvopts = {Method -> "RandomSplit", "SplitNumber" -> 1, "SplitSize" -> .5}
					,
					m >= 10000, cvopts = {Method -> "RandomSplit", "SplitNumber" -> 1, "SplitSize" -> .2}
					,
					m >= 1000, cvopts = {Method -> "KFoldSplit", "SplitNumber" -> 2}
					,
					m >= 100, cvopts = {Method -> "KFoldSplit", "SplitNumber" -> 4}
					,
					m >= 10, cvopts = {Method -> "KFoldSplit", "SplitNumber" -> 10}
					,
					m < 10, cvopts = {Method -> "KFoldSplit", "SplitNumber" -> m}
					,
					m == 1, Return[{1., cvopts}]
					,
					True, AbortMachineLearning[]
				];
			,
			_,
				Which[
					m >= 10000, Return[{1., cvopts}]
					,
					m >= 1000, cvopts = {Method -> "RandomSplit", "SplitNumber" -> 1, "SplitSize" -> .5}
					,
					m >= 100, cvopts = {Method -> "KFoldSplit", "SplitNumber" -> 2}
					,
					m >= 4, cvopts = {Method -> "KFoldSplit", "SplitNumber" -> 4}
					,
					m < 4, cvopts = {Method -> "KFoldSplit", "SplitNumber" -> m}
					,
					m == 1, Return[{1., cvopts}]
					,
					True, AbortMachineLearning[]
				];
		];
	];
	{nsplit, splitfunction} = ParseCrossValidationOptions[Sequence @@ cvopts];
	splitfunction = Composition[convenientdata /@ # &, splitfunction];
	If[l2reg === Automatic,
		method = "ListSearch"; (*one can be a bit smarter using the value of m and n.*)
		lambda = Table[10.^i, {i, -5, 5}];
		,
		method = "ListSearch";
		lambda = N[l2reg]
	];
	theta0 = ConstantArray[0., Last[Dimensions[#]] & /@ {X, Y}];
	lambdaopt = CrossValidationHyperparameterSelect[
		{X, Y},
		lambda,
		linearOptimize[#1, #2] &,
		linearCost[#1, #2] &,
		theta0,
		"CrossValidation" -> {Method -> splitfunction, "SplitNumber" -> nsplit},
		Method -> method,
		"Order" -> "ConstantData",
		"InitialHyperparameter" -> 1.
	];
	{lambdaopt, cvopts}
];

convenientdata[{X_, Y_}] := With[{Xt = Transpose[X]}, {Xt.X, Xt.Y}];

(*(*computes covariance matrix with addition of 1s.*)

covmatrix = Compile[{{X, _Real, 2}, {Xt, _Real, 2}}, Module[
	{m, n, M, tot},
	{m, n} = Dimensions[X];
	M = Table[0., {n+1}, {n+1}];
	M[[2;;n+1, 2;;n+1]] = Xt.X;
	tot = Total[X];
	M[[1, 2;; n+1]] = tot;
	M[[2;; n+1, 1]] = tot;
	M[[1, 1]] = m;
	M
]];*)

linearCost[{XtX_, XtY_}, theta_] := Total[Diagonal[Transpose[theta].(XtX.theta-2*XtY)]];

linearOptimize[{XtX_, XtY_}, lambda_] := Module[
	{M, reg},
	reg = lambda*IdentityMatrix[Length[XtX]];
	reg[[1, 1]] = 0.;
	M = XtX + reg;
	If[!SymmetricMatrixQ[M], M = (M + Transpose[M])/2.];
	If[
		PositiveDefiniteMatrixQ[M]
		,
		LinearSolve[M, XtY, Method -> "Cholesky"]
		,
		PseudoInverse[M].XtY
	]
];

(*restoreLinearCoeff[thetastd_, mux_, sigmax_, muy_, sigmay_] := Module[
	{theta, theta0},
	theta = #*sigmay & /@ (thetastd/sigmax);
	theta0 = muy - Total[#*sigmay & /@ (thetastd*mux/sigmax)];
	Join[{theta0}, theta]
];
*)
