(* ::Package:: *)

Package["MachineLearning`"]

PackageExport["System`PredictorMeasurements"]
PredictorMeasurements::usage = "PredictorMeasurements[PredictorFunction[...], testset, prop] measure prop when PredictorFunction[...] is used on testset.
PredictorMeasurements[PredictorFunction[...], testset] outputs a function that can be used on any prop.
prop can be: \"MeanSquare\", \"StandardDeviation\", \"MeanDeviation\"
"

PackageExport["System`ClassifierMeasurements"]
ClassifierMeasurements::usage = "ClassifierMeasurements[ClassifierFunction[...], testset, prop] measure prop when ClassifierFunction[...] is used on testset.
ClassifierMeasurements[ClassifierFunction[...], testset] outputs a function that can be used on any prop.
prop can be: \"Likelihood\", \"LogLikelihood\", \"Accuracy\", \"ConfusionMatrixPlot\""

ClassifierMeasurements::bdfmt = "Argument `` should be a rule, a list of rules, or an association."
PredictorMeasurements::bdfmt = "Argument `` should be a rule or a list of rules."

ClassifierMeasurements::unbal = "The number of examples (`1`), and the number of corresponding classes (`2`) should be identical."
PredictorMeasurements::unbal = "The number of examples (`1`), and the number of corresponding classes (`2`) should be identical."

ClassifierMeasurements::naprop = "Property `` is not available for this ClassifierMeasurements[\[Ellipsis]] object."
PredictorMeasurements::naprop = "Property `` is not available for this PredictorMeasurements[\[Ellipsis]] object."

PredictorMeasurements::unbal = ClassifierMeasurements::unbal = "The number of examples (`1`), and the number of corresponding values (`2`) should be identical."
PredictorMeasurements::bdval = ClassifierMeasurements::bdval = "Values to be predicted should be (non-complex) numerical."
PredictorMeasurements::bftlgth = ClassifierMeasurements::bftlgth = "Examples should have the same number of features."
PredictorMeasurements::bfttype = ClassifierMeasurements::bfttype = "Features cannot be lists."
PredictorMeasurements::manyval = ClassifierMeasurements::manyval = "Only one variable can be predicted."


PredictorMeasurements[model_, dataset_] := CatchAbortMachineLearning[
	Module[
		{parsed},
		If[!PredictFormatQ[dataset], AbortMachineLearning["bdfmt", dataset]];
		parsed = ParsePredictorDataset[dataset];
		CheckParsedData[parsed];
		iPredictorMeasurements[model, parsed]
	]
	,
	Defer[PredictorMeasurements[model, Short[dataset, 10]]]
];

PredictorMeasurements[model_, dataset_, arg_] := CatchAbortMachineLearning[
	Module[
		{parsed, predictormeasurer},
		If[!PredictFormatQ[dataset], AbortMachineLearning["bdfmt", dataset]];
		parsed = ParsePredictorDataset[dataset];
		CheckParsedData[parsed];
		predictormeasurer = iPredictorMeasurements[model, parsed];
		predictormeasurer[arg]
	]
	,
	Defer[PredictorMeasurements[model, Short[dataset, 10], arg]]
];


ClassifierMeasurements[model_, dataset_] := CatchAbortMachineLearning[
	Module[
		{parsed},
		If[!ClassifyFormatQ[dataset], AbortMachineLearning["bdfmt", dataset]];
		parsed = ParseClassifierDataset[dataset];
		CheckParsedData[parsed];
		iClassifierMeasurements[model, parsed]
	]
	,
	Defer[ClassifierMeasurements[model, Short[dataset, 10]]]
];

ClassifierMeasurements[model_, dataset_, arg_] := CatchAbortMachineLearning[
	Module[
		{parsed, predictormeasurer},
		If[!ClassifyFormatQ[dataset], AbortMachineLearning["bdfmt", dataset]];
		parsed = ParseClassifierDataset[dataset];
		CheckParsedData[parsed];
		predictormeasurer = iClassifierMeasurements[model, parsed];
		predictormeasurer[arg]
	]
	,
	Defer[ClassifierMeasurements[model, Short[dataset, 10], arg]]
];

ClassifierMeasurements[arguments___] := (
	Message[ClassifierMeasurements::argt, ClassifierMeasurements, Length[{arguments}], 1, 2];
	Defer[Short[ClassifierMeasurements[arguments], 10]]
);
PredictorMeasurements[arguments___] := (
	Message[PredictorMeasurements::argt, PredictorMeasurements, Length[{arguments}], 1, 2];
	Defer[Short[PredictorMeasurements[arguments], 10]]
);

(************ predictor ************)

iPredictorMeasurements[model_, {features_, values_}] := Module[
	{distributions, predictions, meansq, meandev, loglike},
	distributions = model[features, "Distribution"];
	predictions = First /@ distributions;
	meandev = Mean[Abs[predictions - values]];
	meansq = Mean[(predictions - values)^2];
	loglike = - Mean @ Log[PDF @@@ Transpose[{distributions, values}]];
	PredictorMeasurerFunction[model, {features, values}, predictions, {meandev, meansq, loglike}]
];

PredictorMeasurerFunction[arg1___][arguments___] := (
	Message[PredictorMeasurements::argx, "PredictorMeasurements[\[Ellipsis]]", Length[{arguments}], 1];
	Defer[Short[PredictorMeasurerFunction[arg1][arguments], 10]]
);

PredictorMeasurerFunction[arg1___][prop_] := (
	Message[PredictorMeasurements::naprop, prop];
	Defer[Short[PredictorMeasurerFunction[arg1][prop], 10]]
);

PredictorMeasurerFunction[_, _, _, {_, meansq_, _}]["MeanSquare"] := meansq;
PredictorMeasurerFunction[_, _, _, {_, meansq_, _}]["StandardDeviation"] := Sqrt[meansq];
(*PredictorMeasurerFunction[_, _, _, {meandev_, _, _}]["MeanDeviation"] := meandev;*)
PredictorMeasurerFunction[_, _, _, {_, _, loglike_}]["LogLikelihoodRate"] := loglike;
PredictorMeasurerFunction[model_, _, _, {_, _, loglike_}]["LogLikelihood"] := loglike*model[[3, 1, 1, 1]];
PredictorMeasurerFunction[model_, _, _, {_, _, loglike_}]["Likelihood"] := Exp[loglike*model[[3, 1, 1, 1]]];
PredictorMeasurerFunction[arg___]["ComparisonPlot"] := PredictorMeasurerFunction[arg][{"ComparisonPlot"}];
PredictorMeasurerFunction[model_, {_, Y_}, predictions_, _][{"ComparisonPlot", opts___}] := Module[
	{xmin, xmax, valuename, plot},
	valuename = model[[3, 1, 3, 1]];
	(*should add scale later*)
	{xmin, xmax} = FindPlotRange[{predictions, Y}];
	(*Add information about standarddeviation, loglikelihood...*)
	plot = ListPlot[
			Transpose[{predictions, Y}],
			FilterOptions[ListPlot, opts],
			FrameLabel -> {"predicted " <> valuename, "test " <> valuename},
			Frame -> {{True, False},{True, False}},
			PlotRange -> {{xmin, xmax}, {xmin, xmax}},
			AspectRatio -> Automatic,
			PlotLegends -> {"predictions"},
			Axes -> None
	];
	If[FreeQ[{opts}, "PerfectPrediction"-> False],
		Show[
			plot
			,
			Plot[x, {x, xmin, xmax}, PlotStyle -> {Gray, Dashed, Thickness[.002]}, PlotLegends -> {"perfect prediction line"}]
		]
		,
		plot
	]
];

PredictorMeasurerFunction[_, {_, Y_}, predictions_, _]["Residuals"] := Y - predictions;

PredictorMeasurerFunction[arg___]["ResidualPlot"] := PredictorMeasurerFunction[arg][{"ResidualPlot"}]
PredictorMeasurerFunction[model_, {_, Y_}, predictions_, _][{"ResidualPlot", opts___}] := Module[
	{xmin, xmax, rmax, valuename},
	valuename = model[[3, 1, 3, 1]];
	{xmin, xmax} = FindPlotRange[{Y, predictions}];
	rmax = 1.05*Max[Abs[Y- predictions]];
	Show[
		ListPlot[
			Transpose[{predictions, Y - predictions}],
			opts,
			FrameLabel -> {"predicted " <> valuename, "test " <> valuename <>" - predicted " <> valuename},
			Frame -> {{True, False},{True, False}},
			PlotRange -> {Full, {-rmax, rmax}},
			AxesOrigin -> {Automatic, 0},
			Filling -> Axis,
			Axes -> {True, False}
		]
	]
];

PredictorMeasurerFunction[arg___]["ResidualHistogram"] := PredictorMeasurerFunction[arg][{"ResidualHistogram"}];
PredictorMeasurerFunction[model_, {_, Y_}, predictions_, _][{"ResidualHistogram", opts___}] := Module[
	{valuename},
	valuename = model[[3, 1, 3, 1]];
	Histogram[
		Y - predictions,
		opts,
		FrameLabel -> {"test " <> valuename <>" - predicted " <> valuename, "counts"},
		Frame -> {{True, False},{True, False}},
		Axes -> False
	]
];

(*PredictorMeasurerFunction[_, {_, Y_}, predictions_, _]["StandardizedResidualPlot"] := "Not Yet";
PredictorMeasurerFunction[_, {_, Y_}, predictions_, _]["NormalResidualPlot"] := "Not Yet";*)

(*include a way to input training data?*)

PredictorMeasurerFunction[model_, {X_, Y_}, predictions_, _]["PredictionPlot"] := 
	PredictionPlot[model, "TestSet" -> {X, Y}];

PredictorMeasurerFunction[model_, {X_, Y_}, predictions_, _][{"PredictionPlot", arg___}] := 
	PredictionPlot[model, "TestSet" -> {X, Y}, arg];

Format[PredictorMeasurerFunction[model_, {X_, Y_}, _, _], StandardForm] := "PredictorMeasurements"[
	"ModelMethod" -> model[[1]],
	"TestExampleNumber" -> Length[Y]
];


(************ end predictor ************)

(************ classifier ************)

iClassifierMeasurements[model_, {X_, Y_}] := Module[
	{preprocessory, classname, classes, nclass, predictions, distributions, indicesmatrix, countmatrix, accuracy, loglike},
	preprocessory = model[[2, 1, 2]];
	{classname, classes} = model[[3, 1, 3]];
	nclass = Length[classes];
	predictions = model[X]; (* model has to be listable *)
	distributions = model[X, "DistributionList"];
	indicesmatrix = ConstantArray[{}, {nclass, nclass}];
	MapIndexed[
		AppendTo[indicesmatrix[[#1[[1]], #1[[2]]]], First@#2]&, 
		Transpose[{preprocessory[Y], preprocessory[predictions]}]
	];
	countmatrix = Map[Length, indicesmatrix, {2}];
	accuracy = N@Total[Diagonal[countmatrix]]/Total[countmatrix, 2];
	loglike = Mean @ Log[Replace @@@ Transpose[{Y, distributions}]];
	ClassifierMeasurerFunction[{model, {X, Y}, classes}, {predictions}, {countmatrix, indicesmatrix, accuracy, loglike}]
	
	(*
	
	countmatrix = Map[Length, indicesmatrix, {2}];
	rowfraction = N@With[{t = Total[#]}, If[t>0, #/t, #]] & /@ countmatrix;
	columnfraction = Transpose[N@With[{t = Total[#]}, If[t>0, #/t, #]] & /@ Transpose[countmatrix]];
	accuracy = N@Total[Diagonal[countmatrix]]/Total[countmatrix, 2];
	ConfusionMatrix[preprocessory, {X, Y}, {countmatrix, indicesmatrix}, {rowfraction, columnfraction}, accuracy]*)
];

Format[ClassifierMeasurerFunction[{model_, {_, Y_}, _}, _, _], StandardForm] := "ClassifierMeasurements"[
	"ModelMethod" -> model[[1]],
	"TestExampleNumber" -> Length[Y]
];

ClassifierMeasurerFunction[arg1___][arguments___] := (
	Message[ClassifierMeasurements::argx, "ClassifierMeasurements[\[Ellipsis]]", Length[{arguments}], 1];
	Defer[ClassifierMeasurerFunction[arg1][arguments]]
);

ClassifierMeasurerFunction[arg1___][prop_] := (
	Message[ClassifierMeasurements::naprop, prop];
	Defer[ClassifierMeasurerFunction[arg1][prop]]
);

ClassifierMeasurerFunction[_, _, {_, _, accuracy_, _}]["Accuracy"] := accuracy;
ClassifierMeasurerFunction[_, _, {_, _, accuracy_, _}]["Error"] := 1. - accuracy;
ClassifierMeasurerFunction[_, _, {_, _, _, loglike_}]["LogLikelihoodRate"] := loglike;
ClassifierMeasurerFunction[{model_, _, _}, _, {_, _, _, loglike_}]["LogLikelihood"] := loglike*model[[3, 1, 1, 1]];
ClassifierMeasurerFunction[{model_, _, _}, _, {_, _, _, loglike_}]["Likelihood"] := Exp[loglike*model[[3, 1, 1, 1]]];

ClassifierMeasurerFunction[{_, _, classes_}, _, {countmatrix_, _, _, _}]["Precision"] := Module[
	{rowfraction},
	rowfraction = N@With[{t = Total[#]}, If[t>0, #/t, #]] & /@ countmatrix;
	Association @ Thread[Rule[classes, Diagonal[rowfraction]]]
];
(*ClassifierMeasurerFunction[arg___][{"Precision", class_}] := Replace[class, ClassifierMeasurerFunction[arg]["Precision"]];
ClassifierMeasurerFunction[arg___][{"Precision", classes_List}] := Replace[classes, ClassifierMeasurerFunction[arg]["Precision"], {1}];
*)
ClassifierMeasurerFunction[{_, _, classes_}, _, {countmatrix_, _, _, _}]["Recall"] := Module[ (*better name?*)
	{columnfraction},
	columnfraction = Transpose[N@With[{t = Total[#]}, If[t>0, #/t, #]] & /@ Transpose[countmatrix]];
	Association @ Thread[Rule[classes, Diagonal[columnfraction]]]
];
(*ClassifierMeasurerFunction[arg___][{"Recall", class_}] := Replace[class, ClassifierMeasurerFunction[arg]["Recall"]];
ClassifierMeasurerFunction[arg___][{"Recall", classes_List}] := Replace[classes, ClassifierMeasurerFunction[arg]["Recall"], {1}];
*)
ClassifierMeasurerFunction[arg___]["FScore"] := ClassifierMeasurerFunction[arg][{"FScore", 1}]
ClassifierMeasurerFunction[{_, _, classes_}, _, {countmatrix_, _, _, _}][{"FScore", beta_}] := Module[
	{diagrowfraction, diagcolumnfraction, sum, product},
	diagrowfraction = Diagonal[N@With[{t = Total[#]}, If[t>0, #/t, #]] & /@ countmatrix];
	diagcolumnfraction = Diagonal[Transpose[N@With[{t = Total[#]}, If[t>0, #/t, #]] & /@ Transpose[countmatrix]]];
	sum = beta^2*diagrowfraction + diagcolumnfraction;
	sum = If[# == 0, 1, #] & /@ sum;
	product = (1+beta^2)*diagrowfraction*diagcolumnfraction;
	Association @ Thread[classes -> product/sum]
];
(*ClassifierMeasurerFunction[arg___][{"FScore", class_}] := Replace[class, ClassifierMeasurerFunction[arg]["FScore"]];
ClassifierMeasurerFunction[arg___][{"FScore", classes_List}] := Replace[classes, ClassifierMeasurerFunction[arg]["FScore"], {1}];
*)
ClassifierMeasurerFunction[{_, _, classes_}, _, {countmatrix_, _, _, _}]["ConfusionMatrix"] := Module[
	{rules},
	rules = Thread[classes -> (Thread[classes -> #] & /@ countmatrix)];
	If[$VersionNumber <10,
		rules,
		Association[MapAt[Association, rules, {All, 2}]]
	]
];

ClassifierMeasurerFunction[arg___]["ConfusionMatrixPlot"] := ClassifierMeasurerFunction[arg][{"ConfusionMatrixPlot"}];

ClassifierMeasurerFunction[{_, {X_, Y_}, classes_}, _, {countmatrix_, indicesmatrix_, accuracy_, _}][{"ConfusionMatrixPlot", opts___}] := Module[
	{rowfraction, columnfraction, nclass},
	rowfraction = N@With[{t = Total[#]}, If[t>0, #/t, #]] & /@ countmatrix;
	columnfraction = Transpose[N@With[{t = Total[#]}, If[t>0, #/t, #]] & /@ Transpose[countmatrix]];
	nclass = Length[classes];
	MatrixPlot[
		countmatrix,
		opts,
		(*ColorFunction -> $heatGradient,*)
		FrameTicks -> {
			Transpose[{Range[nclass], Rotate[#, 0.001] & /@ classes}], (*should rotate automaticaly*)
			Transpose[{Range[nclass], Total[countmatrix]}],
			Transpose[{Range[nclass], Total /@ countmatrix}],
			Transpose[{Range[nclass], Rotate[#, 0.001] & /@ classes}]}
		,
		FrameLabel -> {"test class", "predicted class"}
		,
		Epilog -> Join[
			Table[Tooltip[
				Inset[countmatrix[[j, i]], {i - .5, nclass - j + .5}],
				"Row fraction = " <> ToString[rowfraction[[j, i]]]
				<> "\n"
				<> "Column fraction = " <> ToString[columnfraction[[j, i]]]
				], {i, 1, nclass}, {j, 1, nclass}
			]
		]
	]
];

ClassifierMeasurerFunction[{model_, {X_, Y_}, _}, _, {_, indicesmatrix_, _, _}][{"Examples", trueclass_ -> predictedclass_}] := Module[
	{preprocessory, examples},
	preprocessory = model[[2, 1, 2]];
	examples = X[[indicesmatrix[[preprocessory[trueclass], preprocessory[predictedclass]]]]]
];

$niceGradient = RGBColor /@ {
	{1.000000`, 1.000000`, 1.000000`}, 
	{0.992157`, 0.898039`, 0.843137`},
	{0.976471`, 0.733333`, 0.619608`},
	{0.972549`, 0.576471`, 0.423529`},
	{0.964706`, 0.419608`, 0.258824`},
	{0.850980`, 0.188235`, 0.113725`},
	{0.631373`, 0.074509`, 0.058823`}};
	
$heatGradient := Function[Blend[$niceGradient, #]];

ClassifierMeasurerFunction[{model_, {X_, Y_}, classes_}, predictions_, {_, indicesmatrix_, _, _}][{"ProbabilityPlot", opts___}] := 
	DistributionClassifierPlot[model, "TestSet" -> {X, Y}, opts];
	
ClassifierMeasurerFunction[{model_, {X_, Y_}, classes_}, predictions_, {_, indicesmatrix_, _, _}]["ProbabilityPlot"] := 
	DistributionClassifierPlot[model, "TestSet" -> {X, Y}];

ClassifierMeasurerFunction[{model_, {X_, Y_}, classes_}, predictions_, {_, indicesmatrix_, _, _}][{"ClassificationPlot", opts___}] := 
	ClassificationPlot[model, "TestSet" -> {X, Y}, opts];

ClassifierMeasurerFunction[{model_, {X_, Y_}, classes_}, predictions_, {_, indicesmatrix_, _, _}]["ClassificationPlot"] := 
	ClassificationPlot[model, "TestSet" -> {X, Y}];




