Package["MachineLearning`"]

PackageImport["Developer`"]


PackageScope["MultinomialLogisticClassifierFunction"]

MultinomialLogisticClassifierFunction::usage = "MultinomialLogisticClassifierFunction[{X, Y}] outputs a ClassifierFunction using the multinomial logistic regression model trained on the feature matrix X and the reponse vector Y.
Y values must be in {1, 2, ..., nclass}.
ClassifierFunction[newfeatures] outputs the most likely class given newfeatures.
Can be used through LogisticClassifier[{X, Y}]"

Options[MultinomialLogisticClassifierFunction] = 
	{
	"L2Regularization" -> Automatic,
	(*Weights -> Automatic,*)
	MaxIterations -> 10,
	"CrossValidation" -> Automatic,
	"ClassNumber" -> Automatic,
	PerformanceGoal -> Automatic,
	NominalVariables -> Automatic,
	"BinaryEncoder" -> Preprocessor["Identity"]
};

MultinomialLogisticClassifierFunction[{X_, Y_}, opts:OptionsPattern[]]:= Module[
	{n, m, Xmod, preprocessorx, Yvec, theta, lambdaopt, nclass, cvopts, 
		evaluationdata, metadata, options},
	{m, n} = Dimensions[X];
	preprocessorx = JoinPreprocessors[OptionValue["BinaryEncoder"], Preprocessor["PrependOne"]];
	Xmod = preprocessorx[X];
	nclass = OptionValue["ClassNumber"];
	Yvec = ConstantArray[0., {m, nclass}];
	MapIndexed[Set[Yvec[[#2, #1]], 1] &, Y];
	cvopts = OptionValue["CrossValidation"];
	If[MatchQ[cvopts, {___, Method -> "External", ___}],
		cvopts = Replace[cvopts, ("ValidationSet" -> {xv_, yv_}) :> "ValidationSet" -> {preprocessorx[xv], yv}, {1}];
	];
(*	yvvec = ConstantArray[0., {Length[yv], k}];
				MapIndexed[yvvec[[#2, #1]]++ &, yv];
				yvvec*)
	{theta, lambdaopt, cvopts} = iMultinomialLogisticClassifierFunction[{Xmod, Yvec},
		"CrossValidation" -> cvopts,
		FilterOptions[iMultinomialLogisticClassifierFunction, opts]
	];
	options = Options[MultinomialLogisticClassifierFunction];
	options = DeleteDuplicates[Join[{opts}, options], First[#1] === First[#2] &];
	options = Replace[options, 
		{
			("L2Regularization"-> _) -> ("L2Regularization" -> lambdaopt),
			("CrossValidation"-> _) -> ("CrossValidation" -> cvopts)
		},
		{1}
	];
	evaluationdata = {{preprocessorx, Preprocessor["Identity"], Table[Except[_List], {n}]}, {theta}};
	metadata = {{{m, n, nclass}, {Table[{}, {n}]}, {"class", Range[nclass]}}, {}};
	ClassifierFunction["MultiLogistic", evaluationdata, metadata, options]
];

iClassifierFunction["MultiLogistic"][___][_, prop_] := AbortMachineLearning["naprop", prop];
iNoneClassifierFunction["MultiLogistic"][___][prop_] := AbortMachineLearning["napropn", prop];

iClassifierFunction["MultiLogistic"][eval_, _, _][x_] := Module[
	{preprocessory, theta},
	preprocessory = eval[[1, 2]];
	theta = eval[[2, 1]];
	preprocessory[MaxLoc[x.Transpose[theta]], "Inverse"]
];

distributionvector[x_, theta_] := Module[
	{proba},
	proba = Exp[x.Transpose[theta]];
	proba /= (Total /@ proba)
];

iClassifierFunction["MultiLogistic"][eval_, meta_, _][x_, "DistributionList"] := Module[
	{classes, theta},
	classes = meta[[1, 3, 2]];
	theta = eval[[2, 1]];
	Thread[classes -> #] & /@ distributionvector[x, theta]
];
iClassifierFunction["MultiLogistic"][eval_, meta_, _][x_, "FullProbabilities"] := Module[
	{classes, theta},
	classes = meta[[1, 3, 2]];
	theta = eval[[2, 1]];
	Association[Thread[classes -> #]] & /@ distributionvector[x, theta]
];

iClassifierFunction["MultiLogistic"][eval_, meta_, _][x_, "Probabilities"] := Module[
	{theta, classes, distlist},
	classes = meta[[1, 3, 2]];
	theta = eval[[2, 1]];
	distlist = Thread[classes -> #] & /@ distributionvector[x, theta];
	MostProbableClasses[distlist, 0.1]
];

iClassifierFunction["MultiLogistic"][eval_, _, _][x_, {"Probability", class_}] := Module[
	{preprocessory, theta, distvec, classesindex},
	preprocessory = eval[[1, 2]];
	theta = eval[[2, 1]];
	distvec = distributionvector[x, theta];
	classesindex = preprocessory[class];
	#[[classesindex]] & /@ distvec
];

iNoneClassifierFunction["MultiLogistic"][_, _, options_]["Options"] := Sequence @@ options;

iNoneClassifierFunction["MultiLogistic"][eval_, meta_, opts_]["DistributionList"] := Module[
	{var, expr, function, n, pattern},
	If[!MemberQ[{"numerical", "nominal"}, #2], AbortMachineLearning["naprop", "None, \"DistributionList\""]] & @@@ meta[[1, 2]];
	n = meta[[1, 1, 2]];
	pattern = eval[[1, 3]];
	var = If[MatchQ[pattern, _List], 
		Table[Slot[i], {i, 1, n}]
		,
		#
	];
	expr = ExpandAll @ ClassifierFunction["MultiLogistic", eval, meta, opts][var, "DistributionList"];
	function = Function[Evaluate[expr]];
	function = ReplaceAll[function, Boole[Equal[e1_, e2_]] :> Boole[SameQ[e1, e2]]];
	function
];

iNoneClassifierFunction["MultiLogistic"][eval_, meta_, _][] := Module[
	{theta, preprocessorx, pattern, var, xmod, proba, function, n, classes},
	If[!MemberQ[{"numerical", "nominal"}, #2], AbortMachineLearning["naprop", None]] & @@@ meta[[1, 2]];
	theta = eval[[2, 1]];
	preprocessorx = eval[[1, 1]];
	n = meta[[1, 1, 2]];
	pattern = eval[[1, 3]];
	classes = meta[[1, 3, 2]];
	var = If[MatchQ[pattern, _List],
		Table[Slot[i], {i, 1, n}]
		,
		#
	];
	xmod = preprocessorx[var];
	proba = xmod.Transpose[theta];
	proba = ExpandAll@proba;
	function = With[{expr = proba, classes2 = classes},
		Function[classes2[[First[Ordering[expr]]]]]
	];
	function = ReplaceAll[function, Boole[Equal[e1_, e2_]] :> Boole[SameQ[e1, e2]]];
	function
];

iNoneClassifierFunction["MultiLogistic"][arg___]["ClassificationPlot"] := ClassificationPlot[ClassifierFunction["MultiLogistic", arg]];
iNoneClassifierFunction["MultiLogistic"][arg___]["ProbabilityPlot"] := DistributionClassifierPlot[ClassifierFunction["MultiLogistic", arg]];

iNoneClassifierFunction["MultiLogistic"][arg___][{"ClassificationPlot", arg2___}] := ClassificationPlot[ClassifierFunction["MultiLogistic", arg], arg2];
iNoneClassifierFunction["MultiLogistic"][arg___][{"ProbabilityPlot", arg2___}] := DistributionClassifierPlot[ClassifierFunction["MultiLogistic", arg], arg2];


Options[iMultinomialLogisticClassifierFunction] = {
	"L2Regularization" -> OptionValue[MultinomialLogisticClassifierFunction, "L2Regularization"],
	"CrossValidation" -> OptionValue[MultinomialLogisticClassifierFunction, "CrossValidation"],
	MaxIterations -> OptionValue[MultinomialLogisticClassifierFunction, MaxIterations],
	PerformanceGoal -> Automatic
};
iMultinomialLogisticClassifierFunction[{Xmod_, Yvec_}, opts:OptionsPattern[]]:= Module[
	{n, k, theta, theta0, lambdaopt, l2reg, maxiter, cvopts},
	l2reg = OptionValue["L2Regularization"];
	If[Length[l2reg]>1 || l2reg === Automatic,
		{lambdaopt, cvopts} = crossvalidateMultinomialLogisticClassifierFunction[{Xmod, Yvec}, l2reg, 
			FilterOptions[crossvalidateMultinomialLogisticClassifierFunction, opts]
		];
		,
		cvopts = OptionValue["CrossValidation"];
		lambdaopt = First@Flatten[{l2reg}];
	];
	maxiter = OptionValue[MaxIterations];
	n = Last @ Dimensions[Xmod];
	k = Last @ Dimensions[Yvec];
	theta0 = ConstantArray[0., {k, n}];
	theta = multinomialLogisticOptimize[{Xmod, Yvec}, lambdaopt, theta0, maxiter];
	{theta, lambdaopt, cvopts}
];

Options[crossvalidateMultinomialLogisticClassifierFunction] = {
	"CrossValidation" -> OptionValue[MultinomialLogisticClassifierFunction, "CrossValidation"],
	MaxIterations -> OptionValue[MultinomialLogisticClassifierFunction, MaxIterations],
	PerformanceGoal -> Automatic
};

crossvalidateMultinomialLogisticClassifierFunction[{Xmod_, Yvec_}, l2reg_, opts:OptionsPattern[]] := Module[
	{m, n, k, lambda, lambdaopt, theta0, cvopts, method, maxiter, warmrestart},
	{m, n} = Dimensions[Xmod];
	k = Last @ Dimensions[Yvec];
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
	If[l2reg === Automatic,
		method = "LineSearch";
		lambda = Table[10.^i, {i, -10, 10}];
		warmrestart = False; (*Check if it is secure to put True.*)
		,
		method = "ListSearch";
		lambda = N[l2reg];
		warmrestart = False;
	];
	maxiter = OptionValue[MaxIterations];
	theta0 = ConstantArray[0., {k, n}];
	lambdaopt = CrossValidationHyperparameterSelect[
		{Xmod, Yvec},
		lambda,
		multinomialLogisticOptimize[#1, #2, #3, maxiter] &,
		multinomialLogisticCost[#1, 0., #2] &,
		theta0,
		"CrossValidation" -> cvopts,
		Method -> method,
		"Order" -> "ConstantData",
		"InitialHyperparameter" -> 1.,
		"WarmRestart" -> warmrestart
	];
	{lambdaopt, cvopts}
];


Options[multinomialLogisticOptimize] = {
	Method -> "LBFGS"
};

multinomialLogisticOptimize[{X_, Yvec_}, lambda_, theta0_, maxiter_, opts:OptionsPattern[]] := Module[
	{m, n, theta, cost, gradient, hessian},
	{m, n} = Dimensions[X];
	cost = multinomialLogisticCost[#1, lambda, Partition[#2, n]] &;
	gradient = Flatten[multinomialLogisticGradient[#1, lambda, Partition[#2, n]]] &;
	hessian = ArrayFlatten[multinomialLogisticHessian[First @ #1, lambda, Partition[#2, n]]] &;
	theta = Switch[OptionValue[Method],
		"Newton",
			NewtonDescent[{X, Yvec}, cost, gradient, hessian, Flatten[theta0], MaxIterations -> maxiter]
		,
		"LBFGS",
			LBFGSDescent[{X, Yvec}, cost, gradient, Flatten[theta0], MaxIterations -> maxiter]
	];
	Partition[theta, n]
];

multinomialLogisticCost[{X_, Yvec_}, lambda_, theta_] := Module[
	{m, dummy1, dummy2, reg, cost},
	m = Length[X];
	(*thetam = Append[theta, ConstantArray[0., n]];*)
	dummy1 = X.Transpose[theta];
	dummy2 = dummy1*Yvec; (*should be faster*)
	dummy2 = -Total[dummy2, 2];
	dummy1 = Total[Log[Total /@ Exp[dummy1]]];
	cost = dummy1 + dummy2;
	(*reg = 0.5*lambda*Total[theta[[All, 2;;]], 2];*)
	reg = 0.5*lambda*Total[theta, 2];
	(cost+reg)/m
];

multinomialLogisticGradient[{X_, Yvec_}, lambda_, theta_] := Module[
	{m, proba, Z, reg, gradient},
	m = Length[X];
	(*thetam = Append[theta, ConstantArray[0., n]];*)
	proba = Exp[X.Transpose[theta]];
	Z = Total /@ proba;
	proba /= Z;
	proba -= Yvec;
	(*proba = proba[[All, ;;-2]];*)
	gradient = Transpose[proba].X;
	reg = lambda*theta;
	(*reg[[All, 1]] = 0;*)
	(gradient + reg)/m
];

multinomialLogisticHessian[X_, lambda_, theta_] := Module[
	{n, m, k, proba, Z, clambda, hessian, i, j, pi, pj, block, Xt},
	{m, n} = Dimensions[X];
	k = Length[theta];
	clambda = lambda*IdentityMatrix[n];
	(*clambda[[1, 1]] = 0.;*)
	(*thetam = Append[theta, ConstantArray[0., n]];*)
	proba = Exp[X.Transpose[theta]];
	Z = Total /@ proba;
	proba /= Z;
	(*proba = Transpose[proba[[All, ;;-2]]];*)
	proba = Transpose[proba];
	hessian = Table[{}, {k}, {k}];
	(*hessian = Table[{}, {k-1}, {k-1}];*)
	Xt = Transpose[X];
	(* 
		Computes the hessian as a tensor of dim {k-1, k-1, n, n},
		if k-1 > n, could be faster to compute {n, n, k-1, k-1} instead.
	 *)
	Do[
		Do[
			pi = proba[[i]];
			pj = proba[[j]];
			block = - Xt.((pi*pj)*X);
			hessian[[i, j]] = block;
			hessian[[j, i]] = block;
			,
			{j, i+1, k}
		];
		,
		{i, k}
	];
	Do[
		pi = proba[[i]];
		block = - Xt.((pi*(pi-1.))*X);
		hessian[[i, i]] = block + clambda;
		,
		{i, k}
	];
	hessian = ToPackedArray[hessian, Real];
	hessian = hessian + Transpose[hessian, {2, 1, 4, 3}]; (*restore symetry loss due to round off error*)
	hessian /= (2*m)
];

(*restoreLogisticCoeff[thetastd_, mux_, sigmax_] := Module[
	{theta, theta0},
	theta = Rest[thetastd]/sigmax;
	theta0 = thetastd[[1]] - Total[Rest[thetastd]*mux/sigmax];
	Join[{theta0}, theta]
];*)
