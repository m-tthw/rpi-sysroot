(* ::Package:: *)

Package["MachineLearning`"]

PackageImport["Developer`"]


PackageScope["LogisticClassifierFunction"]

LogisticClassifierFunction::usage = 
"LogisticClassifierFunction[{X, Y}] outputs a ClassifierFunction using the logistic regression model trained on the feature matrix X and the reponse vector Y.
Y values must be in {1, 2, ..., nclass}.
ClassifierFunction[newfeatures] outputs the most likely class given newfeatures."


Options[LogisticClassifierFunction] = 
{
	"L2Regularization" -> Automatic,
	MaxIterations -> 10,
	"CrossValidation" -> Automatic,
	"ClassNumber" -> Automatic,
	PerformanceGoal -> Automatic,
	NominalVariables -> Automatic,
	"BinaryEncoder" -> Preprocessor["Identity"]
};

LogisticClassifierFunction[{X_, Y_}, opts:OptionsPattern[]]:= Module[
	{nclass},
	nclass = OptionValue["ClassNumber"];
	If[nclass === Automatic, nclass = Max[Y]];
	If[nclass == 2, 
		BinomialLogisticClassifierFunction[{X, Y}, opts]
		,
		MultinomialLogisticClassifierFunction[{X, Y}, opts]
	]
];

Options[BinomialLogisticClassifierFunction] = Options[LogisticClassifierFunction];
BinomialLogisticClassifierFunction[{X_, Y_}, opts:OptionsPattern[]]:= Module[
	{m, n, theta, Xmod, preprocessorx, lambdaopt, cvopts, evaluationdata, metadata, options},
	{m, n} = Dimensions[X];
	preprocessorx = JoinPreprocessors[OptionValue["BinaryEncoder"], Preprocessor["PrependOne"]];
	Xmod = preprocessorx[X];
	Xmod = Xmod*(3-2*Y); (* Trick to speed-up optimization. *)
	cvopts = OptionValue["CrossValidation"];
	If[MatchQ[cvopts, {___, Method -> "External", ___}],
		cvopts = Replace[cvopts, ("ValidationSet" -> {xv_, yv_}) :> "ValidationSet" -> preprocessorx[xv]*(3-2*yv), {1}];
	];
	{theta, lambdaopt, cvopts} = iBinomialLogisticClassifierFunction[Xmod, 
		"CrossValidation" -> cvopts,
		FilterOptions[iBinomialLogisticClassifierFunction, opts]
	];
	options = Options[BinomialLogisticClassifierFunction];
	options = DeleteDuplicates[Join[{opts}, options], First[#1] === First[#2] &];
	options = Replace[options,
		{
			("L2Regularization"-> _) -> ("L2Regularization" -> lambdaopt),
			("CrossValidation"-> _) -> ("CrossValidation" -> cvopts)
		}
		,{1}
	];
	evaluationdata = {{preprocessorx, Preprocessor["Identity"], Table[Except[_List], {n}]}, {theta}};
	metadata = {{{m, n, 2}, {Table[{}, {n}]}, {"class", {1, 2}}}, {}};
	ClassifierFunction["LogisticRegression", evaluationdata, metadata, options]
];

distributionvector[x_, theta_] := With[{p = 1./(1. + Exp[x.theta])}, Transpose[{1. - p, p}]];

iClassifierFunction["LogisticRegression"][___][_, prop_] := AbortMachineLearning["naprop", prop];
iNoneClassifierFunction["LogisticRegression"][___][prop_] := AbortMachineLearning["napropn", prop];

iClassifierFunction["LogisticRegression"][eval_, _, _][x_] := Module[
	{preprocessory, theta},
	preprocessory = eval[[1, 2]];
	theta = eval[[2, 1]];
	preprocessory[2 - UnitStep[x.theta], "Inverse"]
];

iClassifierFunction["LogisticRegression"][eval_, meta_, _][x_, "DistributionList"] := Module[
	{theta, classes},
	theta = eval[[2, 1]];
	classes = meta[[1, 3, 2]];
	Thread[classes -> #] & /@ distributionvector[x, theta]
];
iClassifierFunction["LogisticRegression"][eval_, meta_, _][x_, "FullProbabilities"] := Module[
	{theta, classes},
	theta = eval[[2, 1]];
	classes = meta[[1, 3, 2]];
	Association[Thread[classes -> #]] & /@ distributionvector[x, theta]
];

iClassifierFunction["LogisticRegression"][eval_, meta_, _][x_, "Probabilities"] := Module[
	{theta, classes, distlist},
	classes = meta[[1, 3, 2]];
	theta = eval[[2, 1]];
	distlist = Thread[classes -> #] & /@ distributionvector[x, theta];
	MostProbableClasses[distlist, 0.1]
];

iClassifierFunction["LogisticRegression"][eval_, _, _][x_, {"Probability", class_}] := Module[
	{preprocessory, theta, distvec, classesindex},
	preprocessory = eval[[1, 2]];
	theta = eval[[2, 1]];
	distvec = distributionvector[x, theta];
	classesindex = preprocessory[class];
	#[[classesindex]] & /@ distvec
];

iNoneClassifierFunction["LogisticRegression"][_, _, options_]["Options"] := Sequence @@ options;

iNoneClassifierFunction["LogisticRegression"][eval_, meta_, opts_]["DistributionList"] := Module[
	{var, expr, function, n, pattern},
	If[!MemberQ[{"numerical", "nominal"}, #2], AbortMachineLearning["naprop", "None, \"DistributionList\""]] & @@@ meta[[1, 2]];
	n = meta[[1, 1, 2]];
	pattern = eval[[1, 3]];
	var = If[MatchQ[pattern, _List], 
		Table[Slot[i], {i, 1, n}]
		,
		#
	];
	expr = ExpandAll @ ClassifierFunction["LogisticRegression", eval, meta, opts][var, "DistributionList"];
	function = Function[Evaluate[expr]];
	function = ReplaceAll[function, Boole[Equal[e1_, e2_]] :> Boole[SameQ[e1, e2]]];
	function
];

iNoneClassifierFunction["LogisticRegression"][eval_, meta_, _][] := Module[
	{preprocessorx, pattern, theta, var, xmod, y, function, n, classes},
	If[!MemberQ[{"numerical", "nominal"}, #2], AbortMachineLearning["naprop", None]] & @@@ meta[[1, 2]];
	preprocessorx = eval[[1, 1]];
	n = meta[[1, 1, 2]];
	pattern = eval[[1, 3]];
	theta = eval[[2, 1]];
	classes = meta[[1, 3, 2]];
	var = If[MatchQ[pattern, _List], 
		Table[Slot[i], {i, 1, n}]
		,
		#
	];
	xmod = preprocessorx[var];
	y = ExpandAll[xmod.theta];
	function = With[{expr = y, c1 = First[classes], c2 = Last[classes]},
		Function[If[expr > 0., c1, c2]]
	];
	function = ReplaceAll[function, Boole[Equal[e1_, e2_]] :> Boole[SameQ[e1, e2]]];
	function
];

iNoneClassifierFunction["LogisticRegression"][arg___]["ClassificationPlot"] := ClassificationPlot[ClassifierFunction["LogisticRegression", arg]];
iNoneClassifierFunction["LogisticRegression"][arg___]["ProbabilityPlot"] := DistributionClassifierPlot[ClassifierFunction["LogisticRegression", arg]];

iNoneClassifierFunction["LogisticRegression"][arg___][{"ClassificationPlot", arg2___}] := ClassificationPlot[ClassifierFunction["LogisticRegression", arg], arg2];
iNoneClassifierFunction["LogisticRegression"][arg___][{"ProbabilityPlot", arg2___}] := DistributionClassifierPlot[ClassifierFunction["LogisticRegression", arg], arg2];

(*TODO: add [None, {"Probability", class}]*)

Options[iBinomialLogisticClassifierFunction] = {
	"L2Regularization" -> OptionValue[BinomialLogisticClassifierFunction, "L2Regularization"],
	"CrossValidation" -> OptionValue[BinomialLogisticClassifierFunction, "CrossValidation"],
	MaxIterations -> OptionValue[BinomialLogisticClassifierFunction, MaxIterations],
	PerformanceGoal-> Automatic
};

iBinomialLogisticClassifierFunction[Xmod_, opts:OptionsPattern[]]:= Module[
	{theta, theta0, lambdaopt, l2reg, maxiter, cvopts},
	l2reg = OptionValue["L2Regularization"];
	If[Length[l2reg]>1 || l2reg === Automatic,
		{lambdaopt, cvopts} = crossvalidateBinomialLogisticClassifierFunction[Xmod, l2reg, 
			FilterOptions[crossvalidateBinomialLogisticClassifierFunction, opts]
		];
		,
		lambdaopt = First@Flatten[{l2reg}];
		cvopts = OptionValue["CrossValidation"];
	];
	maxiter = OptionValue[MaxIterations];
	theta0 = ConstantArray[0., {Last @ Dimensions[Xmod]}];
	theta = logisticOptimize[Xmod, lambdaopt, theta0, maxiter];
	{theta, lambdaopt, cvopts}
];

Options[crossvalidateBinomialLogisticClassifierFunction] = {
	"CrossValidation" -> OptionValue[BinomialLogisticClassifierFunction, "CrossValidation"],
	MaxIterations -> OptionValue[BinomialLogisticClassifierFunction, MaxIterations],
	PerformanceGoal-> Automatic
};

crossvalidateBinomialLogisticClassifierFunction[Xmod_, l2reg_, opts:OptionsPattern[]] := Module[
	{m, n, lambda, lambdaopt, theta0, cvopts, method, maxiter, warmrestart},
	{m, n} = Dimensions[Xmod];
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
	maxiter = OptionValue[MaxIterations];
	If[l2reg === Automatic,
		method = "LineSearch";
		lambda = Table[10.^i, {i, -10, 10}]; (* Could use PerformanceGoal as well *)
		warmrestart = False;
		,
		method = "ListSearch";
		lambda = N[l2reg];
		warmrestart = False;
	];
	theta0 = ConstantArray[0., Last[Dimensions[Xmod]]];
	lambdaopt = CrossValidationHyperparameterSelect[
		Xmod,
		lambda,
		logisticOptimize[#1, #2, #3, maxiter] &,
		logisticCost[#1, 0., #2] &,
		theta0,
		"CrossValidation" -> cvopts,
		Method -> method,
		"Order" -> "ConstantData",
		"InitialHyperparameter" -> 1.,
		"WarmRestart" -> warmrestart
	];
	{lambdaopt, cvopts}
];

logisticOptimize[Xmod_, lambda_, theta0_, maxiter_] := Module[
	{theta},
	theta = NewtonDescent[
		Xmod, 
		logisticCost[#1, lambda, #2] &,
		logisticGradient[#1, lambda, #2] &,
		logisticHessian[#1, lambda, #2] &,
		theta0,
		MaxIterations -> maxiter
	];
	theta
];

logisticCost[Xmod_, lambda_, theta_] := Module[
	{m, p, cost, reg},
	m = Length[Xmod];
	p = Exp[-Xmod.theta];
	cost = Total[Log[1.+p]];
	reg = Total[0.5*lambda*Rest[theta]^2];
	(cost + reg)/m
]; (* O(mn)*)

logisticGradient[Xmod_, lambda_, theta_] := Module[
	{m, p, gradient, reg},
	m = Length[Xmod];
	p = Exp[Xmod.theta];
	p = -1./(1.+p);
	gradient = p.Xmod;
	reg = lambda*theta;
	reg[[1]] = 0;
	(gradient + reg)/m
];

logisticHessian[X_, lambda_, theta_] := Module[
	{m, n, clambda, p, hessian},
	{m, n} = Dimensions[X];
	clambda = lambda*IdentityMatrix[n];
	clambda[[1, 1]] = 0.;
	p = 0.5/(1.+Cosh[X.theta]);
	hessian = Transpose[X].(p*X);
	hessian = (hessian + Transpose[hessian])/2.;
	(hessian + clambda)/m
];

