(* ::Package:: *)

Package["MachineLearning`"]


PackageScope["NearestNeighborsClassifierFunction"]

NearestNeighborsClassifierFunction::usage = "NearestNeighbors[{X, Y}]. 
X feature matrix
Y is a list of integer in {1, 2, ..., nclass}"

PackageScope["NearestNeighborsPredictorFunction"]


NearestNeighborsPredictorFunction::usage = "NearestNeighbors[{X, Y}]. 
X feature matrix
Y is a list of integer in {1, 2, ..., nclass}"



Options[NearestNeighborsClassifierFunction] = 
{
	"NeighborsNumber" -> Automatic,
	Method -> Automatic, (* Method can be "Lazy" or "KDTree" *)
	"CrossValidation" -> Automatic,
	"ClassNumber" -> Automatic,
	PerformanceGoal -> Automatic,
	NominalVariables -> Automatic,
	"DistanceWeight" -> "Uniform",
	"DistributionSmoothing" -> .5,
	"BinaryEncoder" -> Preprocessor["Identity"]
};

Options[NearestNeighborsPredictorFunction] = 
{
	"NeighborsNumber" -> Automatic,
	Method -> Automatic, (* Method can be "Lazy" or "KDTree" *)
	"CrossValidation" -> Automatic,
	"ClassNumber" -> Automatic,
	PerformanceGoal -> Automatic,
	NominalVariables -> Automatic,
	"DistanceWeight" -> "Uniform",
	"DistributionSmoothing" -> .5,
	"BinaryEncoder" -> Preprocessor["Identity"]
};

NearestNeighborsClassifierFunction[{X_, Y_}, opts:OptionsPattern[]] := Module[
	{Xmod, m, n, preprocessorx, neighborhoodfunction, weightfunction, kopt, cvopts, 
		options, method, classprior, smooth, evaluationdata, metadata, nclass},
	{m, n} = Dimensions[X];
	nclass = OptionValue["ClassNumber"];
	If[nclass === Automatic, nclass = Max[Y]];
	If[nclass < 2 || Min[Y]<1, AbortMachineLearning[]];
	preprocessorx = OptionValue["BinaryEncoder"];
	Xmod = preprocessorx[X];
	cvopts = OptionValue["CrossValidation"];
	If[MatchQ[cvopts, {___, Method -> "External", ___}],
		cvopts = Replace[cvopts, ("ValidationSet" -> {xv_, yv_}) :> "ValidationSet" -> {preprocessorx[xv], yv}, {1}];
	];
	{neighborhoodfunction, weightfunction, kopt, classprior, cvopts, method} = 
		iNearestNeighborsClassifierFunction[
			{Xmod, Y}, 
			"CrossValidation" -> cvopts,
			FilterOptions[iNearestNeighborsClassifierFunction, opts]
		];
	options = Options[NearestNeighborsClassifierFunction];
	options = DeleteDuplicates[Join[{opts}, options], First[#1] === First[#2] &];
	options = Replace[options,
		{
			("NeighborsNumber"-> _) -> ("NeighborsNumber" -> kopt),
			(Method -> _) -> (Method -> method),
			("CrossValidation"-> _) -> ("CrossValidation" -> cvopts)
		}
		,
		{1}
	];
	smooth = OptionValue["DistributionSmoothing"];
	evaluationdata = {
		{preprocessorx, Preprocessor["Identity"], Table[Except[_List], {n}]},
		{kopt, Y, neighborhoodfunction, weightfunction, smooth}
	};
	metadata = {{{m, n, nclass}, {Table[{}, {n}]}, {"class", Range[nclass]}}, {classprior}};
	ClassifierFunction["NearestNeighbors", evaluationdata, metadata, options]
];

NearestNeighborsPredictorFunction[{X_, Y_}, opts:OptionsPattern[]] := Module[
	{m, n, Xmod, preprocessorx, neighborhoodfunction, weightfunction, kopt,
		options, method, cvopts, evaluationdata, metadata, smooth, stddev},
	{m, n} = Dimensions[X];
	preprocessorx = OptionValue["BinaryEncoder"];
	Xmod = preprocessorx[X];
	cvopts = OptionValue["CrossValidation"];
	If[MatchQ[cvopts, {___, Method -> "External", ___}],
		cvopts = Replace[cvopts, ("ValidationSet" -> {xv_, yv_}) :> "ValidationSet" -> {preprocessorx[xv], yv}, {1}];
	];
	{neighborhoodfunction, weightfunction, kopt, stddev, cvopts, method} = iNearestNeighborsPredictorFunction[
		{Xmod, Y},
		"CrossValidation" -> cvopts,
		FilterOptions[iNearestNeighborsPredictorFunction, opts]
	];
	options = Options[NearestNeighborsPredictorFunction];
	options = DeleteDuplicates[Join[{opts}, options], First[#1] === First[#2] &];
	options = Replace[options, 
		{
			("NeighborsNumber"-> _) -> ("NeighborsNumber" -> kopt),
			(Method -> _) -> (Method -> method),
			("CrossValidation"-> _) -> ("CrossValidation" -> cvopts)
		}
		,
		{1}
	];
	smooth = OptionValue["DistributionSmoothing"];
	evaluationdata = {{preprocessorx, Preprocessor["Identity"], Table[Except[_List], {n}]}, {kopt, Y, neighborhoodfunction, weightfunction, smooth}};
	metadata = {{{m, n}, {Table[{}, {n}]}, {"value", {Min[Y], Max[Y]}}}, {stddev}};
	PredictorFunction["NearestNeighbors", evaluationdata, metadata, options]
];

iClassifierFunction["NearestNeighbors"][___][_, prop_] := AbortMachineLearning["naprop", prop];
iNoneClassifierFunction["NearestNeighbors"][___][prop_] := AbortMachineLearning["napropn", prop];
iNoneClassifierFunction["NearestNeighbors"][___][] := AbortMachineLearning["napropnn"];
iPredictorFunction["NearestNeighbors"][___][_, prop_] := AbortMachineLearning["naprop", prop];
iNonePredictorFunction["NearestNeighbors"][___][prop_] := AbortMachineLearning["napropn", prop];
iNonePredictorFunction["NearestNeighbors"][___][] := AbortMachineLearning["napropnn"];

getneighbors[neighborhoodfunction_, x_, k_] := Switch[SymbolName[Head[neighborhoodfunction]],
	"lazyNearestFunction", neighborhoodfunction[x, k]
	,
	"NearestFunction", neighborhoodfunction[#, k] & /@ x
];

(* outputs neighbors in the original data space *)
iClassifierFunction["NearestNeighbors"][eval_, _, _][x_, "Neighbors"] := Module[
	{preprocessorx, preprocessory, k, Y, neighborhoodfunction, neighbors, xs, ys},
	{preprocessorx, preprocessory} = eval[[1, 1;;2]];
	{k, Y, neighborhoodfunction} = eval[[2, 1;;3]];
	neighbors = getneighbors[neighborhoodfunction, x, k];
	xs = neighborhoodfunction[[-4, #]] & /@ neighbors;
	ys = preprocessory[#, "Inverse"] & /@ (Y[[#]] & /@ neighbors);
	xs = preprocessorx[#, "Inverse"] & /@ xs;
	Apply[Rule, Transpose[{xs, ys}, {3, 1, 2}], {2}]
];
iPredictorFunction["NearestNeighbors"][arg__][x_, "Neighbors"] := iClassifierFunction["NearestNeighbors"][arg][x, "Neighbors"];

iNNeval[k_, Y_, neighborhoodfunction_, weightfunction_, x_] := Module[
	{neighbors, distances, xs, ys, weights},
	neighbors = getneighbors[neighborhoodfunction, x, k];
	xs = neighborhoodfunction[[-4, #]] & /@ neighbors;
	ys = Y[[#]] & /@ neighbors;
	distances = euclideanDistanceList @@@ Transpose[{x, xs}];
	weights = Map[weightfunction, distances, {2}];
	{ys, weights}
];
distributionvector[x_, {k_, Y_, neighborhoodfunction_, weightfunction_, smooth_}, classprior_] := Module[
	{ys, weights},
	{ys, weights} = iNNeval[k, Y, neighborhoodfunction, weightfunction, x];
	ClassProbabilities[ys, weights, "ClassNumber"-> Length[classprior], "AdditiveSmoothing"-> smooth*classprior]
];

distribution[x_, {k_, Y_, neighborhoodfunction_, weightfunction_, smooth_}, stddev_]:= Module[
	{ys, weights, muy, sigmay},
	{ys, weights} = iNNeval[k, Y, neighborhoodfunction, weightfunction, x];
	muy = WeightedMean[ys, weights];
	weights = #*k/Total[#] & /@ weights;
	sigmay = Sqrt[smooth*stddev^2+Total /@ (weights*(ys-muy)^2)]/Sqrt[Max[k-1+smooth, $MinMachineNumber]];
	(*sigmay = WeightedStandardDeviation[ys, weights];*)
	NormalDistribution @@@ Transpose[{muy, sigmay}]
];

iPredictorFunction["NearestNeighbors"][eval_, meta_, _][x_, "Distribution"] := Module[
	{stddev},
	stddev = meta[[2, 1]];
	distribution[x, eval[[2, ;;5]], stddev]
];

iClassifierFunction["NearestNeighbors"][eval_, meta_, _][x_, "DistributionList"] := Module[
	{classes, classprior},
	classes = meta[[1, 3, 2]];
	classprior = meta[[2, 1]];
	Thread[classes -> #] & /@ distributionvector[x, eval[[2, ;; 5]], classprior]
];
iClassifierFunction["NearestNeighbors"][eval_, meta_, _][x_, "FullProbabilities"] := Module[
	{classes, classprior},
	classes = meta[[1, 3, 2]];
	classprior = meta[[2, 1]];
	Association[Thread[classes -> #]] & /@ distributionvector[x, eval[[2, ;; 5]], classprior]
];

iClassifierFunction["NearestNeighbors"][eval_, meta_, opts_][x_, "Probabilities"] := Module[
	{distlist},
	distlist = iClassifierFunction["NearestNeighbors"][eval, meta, opts][x, "DistributionList"];
	MostProbableClasses[distlist, 0.1]
];

iClassifierFunction["NearestNeighbors"][eval_, meta_, _][x_, {"Probability", class_}] := Module[
	{preprocessory, distvec, classesindex, classprior},
	preprocessory = eval[[1, 2]];
	classprior = meta[[2, 1]];
	distvec = distributionvector[x, eval[[2, ;;5]], classprior];
	classesindex = preprocessory[class];
	#[[classesindex]] & /@ distvec
];

iClassifierFunction["NearestNeighbors"][eval_, meta_, _][x_] := Module[
	{distvec, preprocessory, classprior},
	preprocessory = eval[[1, 2]];
	classprior = meta[[2, 1]];
	distvec = distributionvector[x, eval[[2, ;;5]], classprior];
	preprocessory[MaxLoc[distvec], "Inverse"]
];

iPredictorFunction["NearestNeighbors"][eval_, meta_, _][x_] := Module[
	{preprocessory, distributions, stddev},
	preprocessory = eval[[1, 2]];
	stddev = meta[[2, 1]];
	distributions = distribution[x, eval[[2, ;;5]], stddev];
	preprocessory[First /@ distributions, "Inverse"]
];

iNoneClassifierFunction["NearestNeighbors"][_, _, options_]["Options"] := Sequence @@ options;
iNonePredictorFunction["NearestNeighbors"][_, _, options_][ "Options"] := Sequence @@ options;

iNoneClassifierFunction["NearestNeighbors"][arg___]["ClassificationPlot"] := ClassificationPlot[ClassifierFunction["NearestNeighbors", arg]];
iNoneClassifierFunction["NearestNeighbors"][arg___]["ProbabilityPlot"] := DistributionClassifierPlot[ClassifierFunction["NearestNeighbors", arg]];
iNoneClassifierFunction["NearestNeighbors"][arg___][{"ClassificationPlot", arg2___}] := ClassificationPlot[ClassifierFunction["NearestNeighbors", arg], arg2];
iNoneClassifierFunction["NearestNeighbors"][arg___][{"ProbabilityPlot", arg2___}] := DistributionClassifierPlot[ClassifierFunction["NearestNeighbors", arg], arg2];

iNonePredictorFunction["NearestNeighbors"][arg___]["PredictionPlot"] := PredictionPlot[PredictorFunction["NearestNeighbors", arg]];
iNonePredictorFunction["NearestNeighbors"][arg___][{"PredictionPlot", arg2___}] := PredictionPlot[PredictorFunction["NearestNeighbors", arg], arg2];


Options[iNearestNeighborsClassifierFunction] = {
	"NeighborsNumber" -> OptionValue[NearestNeighborsClassifierFunction, "NeighborsNumber"],
	Method -> OptionValue[NearestNeighborsClassifierFunction, Method],
	"DistanceWeight" -> OptionValue[NearestNeighborsClassifierFunction, "DistanceWeight"],
	"CrossValidation" -> OptionValue[NearestNeighborsClassifierFunction, "CrossValidation"],
	PerformanceGoal -> OptionValue[NearestNeighborsClassifierFunction, PerformanceGoal],
	"DistributionSmoothing" -> OptionValue[NearestNeighborsClassifierFunction, "DistributionSmoothing"],
	"ClassNumber" -> OptionValue[NearestNeighborsClassifierFunction, "ClassNumber"]
};
iNearestNeighborsClassifierFunction[{X_, Y_}, opts:OptionsPattern[]] := Module[
	{m, n, nclass, method, distweightopt, weightfunction, neighborhoodfunction, k, hyper, cvopts, classprior, yv, ssize},
	{m, n} = Dimensions[X];
	Switch[OptionValue[Method],
		"Lazy", method = "Lazy",
		"KDTree", method = "KDTree",
		Automatic, method = If[n >= 5, "Lazy", "KDTree"],
		_, AbortMachineLearning[];
	];
	Switch[method,
		"Lazy",
			neighborhoodfunction = lazyNearestFunction[X, EuclideanDistance, Null, Null],
		"KDTree",
			neighborhoodfunction = Nearest[X -> Automatic]
	];
	distweightopt = OptionValue["DistanceWeight"];
	Switch[distweightopt,
		"Uniform", weightfunction = 1. &,
		"Inverse", weightfunction = 1./#1 &,
		_Function, weightfunction = distweightopt,
		_, AbortMachineLearning[]
	];
	hyper = OptionValue["NeighborsNumber"];
	cvopts = OptionValue["CrossValidation"];
	If[cvopts === Automatic, (*Could do something smarter.*)
		Switch[OptionValue[PerformanceGoal],
			"Speed", ssize = Ceiling[10^7/(m*n)]
			,
			"Quality", ssize = Ceiling[10^10/(m*n)]
			,
			_, ssize = Ceiling[10^9/(m*n)]
		];
		Which[
			ssize >= 10^5, ssize = 5680 + .01*(ssize-10^5)
			,
			ssize >= 10^4, ssize = 1080 + .05*(ssize-10^4)
			,
			ssize >= 10^3, ssize = 280 + .1*(ssize-10^3)
			,
			ssize >= 100, ssize = 100 + .2*(ssize-10^2)
			,
			ssize >= 10, ssize = 55 + .5*(ssize-10)
			,
			ssize < 10, ssize = 10 + 4.5*ssize
			,
			True, AbortMachineLearning[];
		];
		ssize = Min[Ceiling[ssize], m];
		cvopts = {Method -> "NearestNeighborsValidation", "SplitSize" -> ssize};
	];
	nclass = OptionValue["ClassNumber"];
	If[nclass === Automatic, nclass = Max[Y]];
	If[MatchQ[cvopts, {___, "ValidationSet" -> {_, _}, ___}], (*use a validation set if possible*)
		yv = First@Cases[cvopts, ("ValidationSet" -> {_, v_}) :> v];
		nclass = Max[nclass, Max[yv]]; (*be careful*)
		classprior = ClassProbabilities[yv, "ClassNumber"-> nclass, "AdditiveSmoothing" -> 1];
		,
		classprior = ClassProbabilities[Y, "ClassNumber"-> nclass, "AdditiveSmoothing" -> 1];
	];
	If[Length[hyper]>1 || hyper === Automatic,
		k = crossvalidateKNearestNeighborsClassifierFunction[{X, Y}, hyper,
			neighborhoodfunction, weightfunction, nclass, classprior, OptionValue["DistributionSmoothing"],
			Sequence @@ cvopts
		];
		,
		k = First@Flatten[{hyper}];
	];
	{neighborhoodfunction, weightfunction, k, classprior, cvopts, method}
];

Options[iNearestNeighborsPredictorFunction] = {
	"NeighborsNumber" -> OptionValue[NearestNeighborsPredictorFunction, "NeighborsNumber"],
	Method -> OptionValue[NearestNeighborsPredictorFunction, Method],
	"DistanceWeight" -> OptionValue[NearestNeighborsPredictorFunction, "DistanceWeight"],
	"CrossValidation" -> OptionValue[NearestNeighborsPredictorFunction, "CrossValidation"],
	PerformanceGoal -> OptionValue[NearestNeighborsPredictorFunction, PerformanceGoal],
	"DistributionSmoothing" -> OptionValue[NearestNeighborsPredictorFunction, "DistributionSmoothing"]
};
iNearestNeighborsPredictorFunction[{X_, Y_}, opts:OptionsPattern[]] := Module[
	{m, n, method, distweightopt, weightfunction, neighborhoodfunction, k, hyper, cvopts, stddev, xv, yv, yp, ind, ssize},
	{m, n} = Dimensions[X];
	Switch[OptionValue[Method],
		"Lazy", method = "Lazy",
		"KDTree", method = "KDTree",
		Automatic, method = If[n >= 5, "Lazy", "KDTree"],
		_, AbortMachineLearning[];
	];
	Switch[method,
		"Lazy",
			neighborhoodfunction = lazyNearestFunction[X, EuclideanDistance, Null, Null],
		"KDTree",
			neighborhoodfunction = Nearest[X -> Automatic]
	];
	distweightopt = OptionValue["DistanceWeight"];
	Switch[distweightopt,
		"Uniform", weightfunction = 1. &,
		"Inverse", weightfunction = 1./#1 &,
		_Function, weightfunction = distweightopt,
		_, AbortMachineLearning[]
	];
	hyper = OptionValue["NeighborsNumber"];
	cvopts = OptionValue["CrossValidation"];
	If[cvopts === Automatic, (*Should do something smarter, sufficient statistics...*)
		Switch[OptionValue[PerformanceGoal],
			"Speed", ssize = Ceiling[10^7/(m*n)]
			,
			"Quality", ssize = Ceiling[10^10/(m*n)]
			,
			_, ssize = Ceiling[10^9/(m*n)]
		];
		Which[
			ssize >= 10^5, ssize = 5680 + .01*(ssize-10^5)
			,
			ssize >= 10^4, ssize = 1080 + .05*(ssize-10^4)
			,
			ssize >= 10^3, ssize = 280 + .1*(ssize-10^3)
			,
			ssize >= 100, ssize = 100 + .2*(ssize-10^2)
			,
			ssize >= 10, ssize = 55 + .5*(ssize-10)
			,
			ssize < 10, ssize = 10 + 4.5*ssize
			,
			True, AbortMachineLearning[];
		];
		ssize = Min[Ceiling[ssize], m];
		cvopts = {Method -> "NearestNeighborsValidation", "SplitSize" -> ssize};
	];
	If[Length[hyper]>1 || hyper === Automatic,
		k = crossvalidateKNearestNeighborsPredictorFunction[{X, Y}, hyper,
			neighborhoodfunction, weightfunction,
			Sequence @@ cvopts
		];
		,
		k = First@Flatten[{hyper}];
	];
	If[MatchQ[cvopts, {___, "ValidationSet" -> {_, _}, ___}], (*use a validation set if possible*)
		{xv, yv} = First@Cases[cvopts, ("ValidationSet" -> v_) :> v];
		,
		ind = RandomSample[Range[m], Min[100, Length[Y]]];
		{xv, yv} = {X[[ind]], Y[[ind]]};
	];
	yp = First@cvPredictorFunction[{X, Y}, neighborhoodfunction, weightfunction, {k}, True, xv];
	stddev = Sqrt[Mean[(yp-yv)^2]];
	{neighborhoodfunction, weightfunction, k, stddev, cvopts, method}
];

Options[crossvalidateKNearestNeighborsPredictorFunction] = {
	Method -> "NearestNeighborsValidation",
	"SplitSize" -> {1., 1000},
	"ValidationSet"-> {}
};

NearestNeighborsPredictorFunction::nocv = "This Cross-Validation method is not suported by this algorithm.";
NearestNeighborsClassifierFunction::nocv = "This Cross-Validation method is not suported by this algorithm.";
NearestNeighborsPredictorFunction::noset = "The validation set is empty.";
NearestNeighborsClassifierFunction::noset = "The validation set is empty.";

crossvalidateKNearestNeighborsPredictorFunction[{X_, Y_}, hyper_, neighborhoodfunction_, weightfunction_, opts:OptionsPattern[]] := Module[
	{xv, yv, yvp, ks, validationindices, costs, val},
	{ks, validationindices} = getks[X, hyper, OptionValue["SplitSize"]];
	Switch[OptionValue[Method],
		"NearestNeighborsValidation",
			xv = X[[validationindices]];
			yv = Y[[validationindices]];
			yvp = cvPredictorFunction[{X, Y}, neighborhoodfunction, weightfunction, ks, True, xv];
		,
		"External",
			val = OptionValue["ValidationSet"];
			If[val === {}, Message[NearestNeighborsPredictorFunction::noset]; AbortMachineLearning[];];
			{xv, yv} = val;
			yvp = cvPredictorFunction[{X, Y}, neighborhoodfunction, weightfunction, ks, False, xv];
		,
		_, Message[NearestNeighborsPredictorFunction::nocv]; AbortMachineLearning[];
	];
	costs = Total[(yv - #)^2]/Length[#] & /@ yvp;
	ks[[MinLoc[costs]]]
];

Options[crossvalidateKNearestNeighborsClassifierFunction] = {
	Method -> "NearestNeighborsValidation",
	"SplitSize" -> {1., 1000},
	"ValidationSet"-> {}
};
crossvalidateKNearestNeighborsClassifierFunction[{X_, Y_}, hyper_, neighborhoodfunction_, weightfunction_, nclass_, smooth_, classprior_, opts:OptionsPattern[]] := Module[
	{xv, yv, yvp, ks, validationindices, costs, val},
	{ks, validationindices} = getks[X, hyper, OptionValue["SplitSize"]];
	Switch[OptionValue[Method],
		"NearestNeighborsValidation",
			xv = X[[validationindices]];
			yv = Y[[validationindices]];
			yvp = cvClassifierFunction[{X, Y}, neighborhoodfunction, weightfunction, ks, True, nclass, smooth, classprior, xv];
		,
		"External",
			val = OptionValue["ValidationSet"];
			If[val === {}, Message[NearestNeighborsClassifierFunction::noset]; AbortMachineLearning[];];
			{xv, yv} = val;
			yvp = cvClassifierFunction[{X, Y}, neighborhoodfunction, weightfunction, ks, False, nclass, smooth, classprior, xv];
		,
		_, Message[NearestNeighborsClassifierFunction::nocv]; AbortMachineLearning[];
	];
	costs = - Total[Log[#1[[#2]]] & @@@ Transpose[{#, yv}]]/Length[#] & /@ yvp;
	ks[[MinLoc[costs]]]
];
getks[X_, hyper_, splitsize_]:= Module[
	{m, ks, nvalid, validationindices},
	m = Length[X];
	If[hyper === Automatic,
		(*ok for lazy evaluation. Should be smarter fo KDTree.*)
		ks = Flatten @ Table[{1, 2, 5}*10^i, {i, 0, Round[Log10[m]]}];
		ks = Select[ks, # < m &];
		ks = Which[
			m<=5,
				{1},
			m<=10,
				{1, 2},
			m<=50,
				Select[ks, # < m/2 &],
			m<=100,
				Select[ks, # < m/5 &],
			m<=10^3,
				Select[ks, # < m/10 &],
			m<=10^4,
				Select[ks, # < m/20 &],
			True,
				Select[ks, # < m/50 &]
		]; (*use n as well?*)
		,
		ks = hyper
	];
	ks = Sort[ks];
	ks = Select[ks, # < m &];
	nvalid = Flatten[{splitsize}];
	nvalid = If[# < 1 || # === 1., Ceiling[#*m], #] & /@ nvalid;
	nvalid = Min[nvalid];
	validationindices = RandomSample[Range[m], nvalid];
	{ks, validationindices}
];

(*special function for cross validation*)
cvPredictorFunction[{X_, Y_}, neighborhoodfunction_, weightfunction_, ks_List, removefirst_, x:{{__}..}] := Module[
	{ys, weights},
	{ys, weights} = icvknn[{X, Y}, neighborhoodfunction, weightfunction, ks, removefirst, x];
	WeightedMean[ys[[All, ;;#]], weights[[All, ;;#]]] & /@ ks
];

cvClassifierFunction[{X_, Y_}, neighborhoodfunction_, weightfunction_, ks_List, removefirst_, nclass_, smooth_, classprior_, x:{{__}..}] := Module[
	{ys, weights},
	{ys, weights} = icvknn[{X, Y}, neighborhoodfunction, weightfunction, ks, removefirst, x];
	ClassProbabilities[ys[[All, ;;#]], weights[[All, ;;#]], "ClassNumber"-> nclass, "AdditiveSmoothing"-> smooth*classprior]  & /@ ks
];

icvknn[{X_, Y_}, neighborhoodfunction_, weightfunction_, ks_List, removefirst_, x_] := Module[
	{neighbors, xs, ys, distances, weights, kmax},
	kmax = Max[ks];
	neighbors = getneighbors[neighborhoodfunction, x, kmax+1];
	neighbors = If[removefirst, 
		Rest /@ neighbors,
		Most /@ neighbors
	];
	xs = X[[#]] & /@ neighbors;
	distances = euclideanDistanceList @@@ Transpose[{x, xs}];
	weights = Map[weightfunction, distances, {2}];
	ys = Y[[#]] & /@ neighbors;
	{ys, weights}
];

lazyNearestFunction[X_, EuclideanDistance, args__][x_, k_] := First[
	lazyNearestFunction[X, EuclideanDistance, args][{x}, k]
];
lazyNearestFunction[X_, EuclideanDistance, __][x:{{__}..}, k_] := Module[
	{d, i1, i2},
	{i2, d} = N@Log2[Dimensions[X]];
	i1 = N@Log2[Length[x]];
	(* machine-learned coefficients for 8 cores of 2013*)
	If[3.39332 - 0.435474 d + 0.046742 i1 - 0.051543 i2 > 0, 
		ilazyNearestFunction1[x, Transpose[X], k]
		,
		ilazyNearestFunction2[x, X, k]
	]
];
ilazyNearestFunction1 = MachineLearning`Libraries`Compile[{{ref, _Real, 1}, {list, _Real, 2}, {k, _Integer}},
	Ordering[Total[(list - ref)^2], k],
	RuntimeAttributes -> {Listable},
	Parallelization -> True,
	CompilationTarget -> "C"
];
ilazyNearestFunction2[x_, X_, k_] := Ordering[ieuclideanDistanceList2[#, X], k] & /@ x;

(*fast computation of distance between reference vector(s) and a list of vector.*)

euclideanDistanceList[ref_, matrix_] := First[euclideanDistanceList[{ref}, matrix]];
euclideanDistanceList[ref:{{__}..}, matrix_] := Module[
	{d, i1, i2},
	{i2, d} = N@Log2[Dimensions[matrix]];
	i1 = N@Log2[Length[ref]];
	(* machine-learned coefficients for 8 cores of 2013*)
	If[2.26393 - 0.320407 d + 0.0825958 i1 - 0.058974 i2 > 0,
		ieuclideanDistanceList1[ref, Transpose[matrix]]
		,
		ieuclideanDistanceList2[#, matrix] & /@ ref
	]
];
ieuclideanDistanceList1 = MachineLearning`Libraries`Compile[{{ref, _Real, 1}, {matrix, _Real, 2}}, 
	Sqrt[Total[(matrix - ref)^2]],
	RuntimeAttributes -> {Listable},
	Parallelization -> True,
	CompilationTarget -> "C"
];
ieuclideanDistanceList2 = MachineLearning`Libraries`Compile[{{ref, _Real, 1}, {vector, _Real, 1}}, 
	Sqrt[Total[(vector - ref)^2]],
	RuntimeAttributes -> {Listable},
	Parallelization -> True,
	CompilationTarget -> "C"
];
