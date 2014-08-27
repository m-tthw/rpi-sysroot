Package["MachineLearning`"]

PackageImport["Developer`"]

PackageScope["LowRankFactorize"]

LowRankFactorize::usage = "Factorizes a Sparse Matrix under the assumption that the completed matrices has a given maximum rank.
LowRankFactorize[sparsearray, \"L2Regularization\" -> 10., \"Rank\" -> 10] put a L2 penalty weight on the factor matrices, and assume a rank 10.
More to come.. "

(*should have a version for a dens matrix with missing.*)


Options[LowRankFactorize] = {
	PrecisionGoal -> 3,
	"L2Regularization" -> 0.,
	MaxIterations -> 100,
	"CrossValidation" -> {"k-split", "k" -> 1, "SetRatio" -> 0.2},
	"TestSetRatio" -> 0.,
	"Monitor" -> False,
	"Rank" -> 10
};

LowRankFactorize[sparseratings_SparseArray, opts:OptionsPattern[]]:= Module[
	{userlist, itemlist, itemaverage, validationitemlist, testitemlist,
		validationratio, testratio, cvopts, theta, beta, costhistory, lambda, n, k, p},
	cvopts = OptionValue["CrossValidation"];
	If[!SameQ[First[cvopts], "k-split"] || !SameQ[cvopts[[2, 1]], "k"], Print["Cross-Validation method not supported yet."]; abortLearning[]];
	validationratio = cvopts[[3, 2]];
	testratio = OptionValue["TestSetRatio"];
	Print["Prepare data structure"];
	{
		{userlist, itemlist, itemaverage},
		validationitemlist,
		testitemlist
	} = prepareDataSets[sparseratings, validationratio, testratio];
	lambda = Flatten[{OptionValue["L2Regularization"]}];
	n = Length[userlist];
	k = Length[itemlist];
	p = OptionValue["Rank"];
	theta = RandomReal[{-0.01, 0.01}, {n, p}];
	beta = RandomReal[{-0.01, 0.01}, {k, p}];
	Print["Start optimization"];
	If[Length[lambda] == 1,
		{{theta, beta}, costhistory} = iLowRankFactorize[{userlist, itemlist}, {theta, beta}, First[lambda],
			"ValidationCostFunction" ->  (lowRankFactorizeCost[validationitemlist, Reverse @ # , 0.] &),
			"Monitor" -> True,
			FilterOptions[iLowRankFactorize, opts]];,
		Print["not supported yet"];
	];
	{theta, beta}
]; 


Options[iLowRankFactorize] = {
	PrecisionGoal -> 2,
	MaxIterations -> 100,
	"ValidationCostFunction" -> None, (*lowRankFactorizeCost[validationitemlist, Reverse@# , 0.] &*)
	"Monitor" -> False
};

iLowRankFactorize[{userlist_, itemlist_}, {theta0_, beta0_}, lambda_, opts:OptionsPattern[]] :=  Module[
	{theta = theta0, beta = beta0, history, monitor, eps, validationcost},
	monitor = OptionValue["Monitor"];
	validationcost = OptionValue["ValidationCostFunction"];
	history = {costs[{itemlist, userlist}, {theta, beta}, lambda, validationcost]};
	Print["Initial Costs = ", Last @ history];
	eps = 10.^-(OptionValue[PrecisionGoal]+1.);
	Do[
		theta = lowRankFactorizeLineSearch[userlist, {theta, beta}, lambda];
		beta = lowRankFactorizeLineSearch[itemlist, {beta, theta}, lambda];
		AppendTo[history, costs[{itemlist, userlist}, {theta, beta}, lambda, validationcost]];
		If[iLowRankFactorizeStopQ[history, eps], Break[]];
		,
		{OptionValue[MaxIterations]}
	];
	Print[Length @ history - 1, " iterations"];
	Print["Final Costs = ", Last @ history];
	If[monitor, plothistory[history]];
	{{theta, beta}, history}
];

iLowRankFactorizeStopQ[history_, eps_] := Or[
	(history[[-2, 1]] - history[[-1, 1]] < 0.)
	,
	And[
		Length[history] > 3,
		(history[[-2, 1]] - history[[-1, 1]] < Abs[history[[-1, 1]]]*eps),
		(history[[-3, 1]] - 2.*history[[-2, 1]] + history[[-1, 1]] > 0.)
	]
];

costs[{itemlist_, userlist_}, {theta_, beta_}, lambda_, validationcost_]:= Module[
	{m, cost, costnoreg, costcv}, 
	m = Total[Length[First@#] & /@ itemlist];
	costnoreg = lowRankFactorizeCost[itemlist, {beta, theta}, 0.];
	cost = costnoreg + regularizationCost[{beta, theta}, lambda, m];
	If[SameQ[validationcost, None],
		{cost, costnoreg}
		,
		costcv = validationcost[{theta, beta}];
		{cost, costnoreg, costcv}
	]
];

lowRankFactorizeLineSearch[list_, {param1_, param2_}, lambda_]:= Module[
	{matrix, A, B, p, zeros, param1new, reg},
	p = Length[First@param1];
	zeros = ConstantArray[0., p];
	reg = lambda*IdentityMatrix[p];
	param1new = MapIndexed[
		(
		matrix = param2[[ First@#1 ]];
		If[SameQ[matrix, {}],
			zeros
			,
			A = Transpose[matrix].matrix + reg;
			If[!SymmetricMatrixQ[A], A = (A + Transpose[A])/2.];
			B = #1[[2]].matrix;
			If[PositiveDefiniteMatrixQ[A],
				LinearSolve[A, B, Method -> "Cholesky"] (*probably compute PositiveDefiniteMatrixQ twice...*)
				,
				PseudoInverse[A].B
			]
		]
		) &
		,
		list
	];
	ToPackedArray[param1new, Real]
];

lowRankFactorizeCost[list_, {param1_, param2_}, lambda_] :=  Module[
	{matrix, iter, m, cost},
	m = Total[Length[#[[1]]] & /@ list];
	cost = Total[MapIndexed[
		(
		iter = First@#2;
		matrix = Part[param2, list[[iter, 1]]];
		If[SameQ[matrix, {}],
			0.,
			Total[(matrix.#1 - list[[iter, 2]])^2]
		]
		) &
		,
		param1
	]];
	cost/(2*m) + regularizationCost[{param1, param2}, lambda, m]
];

regularizationCost[{param1_, param2_}, lambda_, m_] := lambda*(Total[param1^2, 2] + Total[param2^2, 2])/(2*m);

lowRankFactorizeGradient[list_, {param1_, param2_}, lambda_] :=  Module[
	{matrix, iter, m, grad, reg},
	m = Total[Length[#[[1]]] & /@ list];
	grad = MapIndexed[
		(
		iter = First@#2;
		matrix = Part[param2, list[[iter, 1]]];
		If[SameQ[matrix, {}], 
			0.*#1,
			(matrix.#1 - list[[iter, 2]]).matrix
		]
		) &
		,
		param1
	];
	reg = lambda*param1;
	(grad+reg)/m
];

plothistory[history_] := Module[
	{historymod},
	historymod = Transpose[{Range[0, Length[history] - 1], #}] & /@ Transpose[history];
	Print[
		If[Length[historymod] > 100,
			ListLogLinearPlot[Rest @ historymod, Joined -> True, 
			PlotLegends -> {"Cost", "Cost without regularization","Cost on validation set"}]
			,
			ListPlot[historymod, Joined -> True, PlotMarkers -> Automatic, 
			PlotLegends -> {"Cost", "Cost without regularization","Cost on validation set"}]
		]
	];
];



userlistToTriplets[userlist_] := Module[
	{triplets},
	triplets = MapIndexed[Function[{x}, Prepend[x, First@#2]] /@ Transpose[#1] &, userlist];
	ToPackedArray[Flatten[triplets, 1], Real]
];


itemlistToTriplets[userlist_] := Module[
	{triplets},
	triplets = MapIndexed[Function[{x, y}, {x, First@#2, y}] @@@ Transpose[#1] &, userlist];
	ToPackedArray[Flatten[triplets, 1], Real]
];


tripletsToUserlist[triplets_, nuser_] := Module[
	{list, userlist},
	list = Transpose /@ SortBy[GatherBy[triplets, #[[1]] &], #[[1, 1]] &];
	userlist = Table[{{}, {}}, {nuser}];
	ToPackedArray[userlist[[First@#1]] = {#2, #3}, Real] & @@@ list;
	userlist
];


tripletsToItemlist[triplets_, nitem_] := Module[
	{list, itemlist},
	list = Transpose /@ SortBy[GatherBy[triplets, #[[2]] &], #[[1, 2]] &];
	itemlist = Table[{{}, {}}, {nitem}];
	ToPackedArray[itemlist[[First@#2]] = {#1, #3}, Real] & @@@ list;
	itemlist
];

tripletsToSparseArray[triplets_] := SparseArray[Round[Transpose[{#1, #2}]] -> #3 & @@ Transpose[triplets]];

sparseArrayToTriplets[sa_] := ToPackedArray[Flatten /@ List @@@ Most[ArrayRules[sa]], Real];


prepareDataSets[sa_SparseArray, validationratio_, testratio_] := iprepareDataSets[RandomSample[sparseArrayToTriplets[sa]], validationratio, testratio];
prepareDataSets[userlist_, validationratio_, testratio_] := iprepareDataSets[RandomSample[userlistToTriplets[userlist]], validationratio, testratio];
prepareDataSets[triplets:{{_, _, _}..}, validationratio_, testratio_] := iprepareDataSets[RandomSample[triplets], validationratio, testratio];

Clear[iprepareDataSets]
iprepareDataSets[triplets_, validationratio_, testratio_] := Module[
	{nuser, nitem, trainingtriplets, validationtriplets, testtriplets, tuserlist, titemlist, itemaverage},
	nuser = Max[triplets[[All, 1]]];
	nitem = Max[triplets[[All, 2]]];
	{trainingtriplets, validationtriplets, testtriplets} = splitDataSets[triplets, validationratio, testratio];
	{tuserlist, titemlist, itemaverage} = prepareTrainingSet[trainingtriplets, nitem, nuser];
	{
	{tuserlist, titemlist, itemaverage},
	MapIndexed[{#1[[1]], #1[[2]] - itemaverage[[First@#2]]} &, tripletsToItemlist[validationtriplets, nitem]],
	tripletsToItemlist[testtriplets, nitem]
	}
];


splitDataSets[triplets_, ratiocv_, ratiotest_] := Module[
	{m, mcv, mtest},
	m = Length[triplets];
	mcv = Floor[ratiocv*m];
	mtest = Floor[ratiotest*m];
	{
		triplets[[mcv + mtest + 1;;]],
		triplets[[;; mcv]],
		triplets[[mcv + 1 ;; mcv + mtest]]
	}
];


prepareTrainingSet[triplets_, nitem_, nuser_] := Module[
	{userlist, itemlist, itemaverage, globalmean},
	itemlist = tripletsToItemlist[triplets, nitem];
	globalmean = Mean[triplets[[All, 3]]];
	itemaverage = If[Length[#2] == 0, globalmean, Mean[#2]] & @@@ itemlist;
	itemlist = {#1, #2 - Mean[#2]} &  @@@ itemlist;
	userlist = tripletsToUserlist[triplets, nuser];
	userlist = {#1, #2 - itemaverage[[#1]]} & @@@ userlist;
	{userlist, itemlist, itemaverage}

];
