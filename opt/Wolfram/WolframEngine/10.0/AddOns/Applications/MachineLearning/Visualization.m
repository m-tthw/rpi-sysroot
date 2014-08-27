(* ::Package:: *)

Package["MachineLearning`"]

(*here are tools to visualize models*)

PackageScope[PredictionPlot]
PackageScope[ClassificationPlot]
PackageScope[DistributionClassifierPlot]
PackageScope[FindPlotRange]

PredictionPlot::usage = "PredictionPlot[predictor] plots the predictions of predictor as function of the features"
ClassificationPlot::usage = "ClassificationPlot[classifier] plots the classification boundaries of classifier"
DistributionClassifierPlot::usage = "DistributionClassifierPlot[classifier] plots probabilities of classes conditioned on the features."

FindPlotRange::usage = "finds plot range"

abortVisualization::usage = "abort Visualization and returns $Failed from PredictionPlot and ClassificationPlot";
catchabortVisualization::usage = "Catch abortVisualization";

abortVisualization[] := Throw[$Failed, abortVisualization];

SetAttributes[catchabortVisualization, HoldAll];
catchabortVisualization[body_] := Catch[body, abortVisualization];


Options[PredictionPlot] = {
	"TestSet" -> None,
	"ConfidenceProbability" -> 0.95,
	"SinglePlot" -> True
};

PredictionPlot[model_, opts:OptionsPattern[]] := catchabortVisualization @ Module[
	{n, featuretypes, nominalfeatures, numericalfeatures,
		nnominal, nnumerical},
	featuretypes = model[[3, 1, 2, All, 2]];
	numericalfeatures = First /@ Position[featuretypes, "numerical"];
	nominalfeatures = First /@ Position[featuretypes, "nominal"];
	nnominal = Length[nominalfeatures];
	nnumerical = Length[numericalfeatures];
	n = model[[3, 1, 1, 2]];
	If[nnominal + nnumerical != n, abortVisualization[]];	
	Which[
		nnumerical == 0, predictionPlot0f[model, opts],
		nnumerical == 1, predictionPlot1f[model, 
			"NominalFeatures" -> nominalfeatures, 
			"NumericalFeatures" -> numericalfeatures, 
			opts],
		nnumerical == 2, predictionPlot2f[model,
			"NominalFeatures" -> nominalfeatures, 
			"NumericalFeatures" -> numericalfeatures, 
			opts],
		True, abortVisualization[];
	]
];

predictionPlot0f[model_, opts___] := abortVisualization[];

Options[predictionPlot1f] = {
	"TestSet" -> None,
	"ConfidenceProbability" -> 0.95,
	PlotLabel -> "PredictionPlot",
	"SinglePlot" -> True,
	ImageSize -> Automatic,
	"NominalFeatures" -> {},
	"NumericalFeatures" -> {}
};
predictionPlot1f[model_, opts:OptionsPattern[]] := Module[
	{X, Y, nominalfeatures, numericalfeatures, testset, testsetindices,
		numericalfeaturename, numericalfeaturerange, valuename, valuerange, singleplot,
		nominalfeaturenames, nominallabels, nominallabelscombinations, inputpattern, plots
	},
	inputpattern = model[[2, 1, 3]];
	singleplot = OptionValue["SinglePlot"];
	nominalfeatures = OptionValue["NominalFeatures"];
	numericalfeatures = OptionValue["NumericalFeatures"];
	nominallabels = model[[3, 1, 2, nominalfeatures, 3]];
	nominalfeaturenames = model[[3, 1, 2, nominalfeatures, 1]];
	nominallabelscombinations = Tuples[nominallabels];
	{numericalfeaturename, numericalfeaturerange} = model[[3, 1, 2, First[numericalfeatures], {1, 3}]];
	{valuename, valuerange} = model[[3, 1, 3]];
	testset = OptionValue["TestSet"];
	If[testset =!= None, 
		{X, Y} = ParsePredictorDataset[testset];
		If[!MatchQ[inputpattern, _List], X = List /@ X];
		valuerange = FindPlotRange[{valuerange, Y}];
		numericalfeaturerange = FindPlotRange[{numericalfeaturerange, X[[All, numericalfeatures]]}];
		,
		valuerange = FindPlotRange[valuerange];
		numericalfeaturerange = FindPlotRange[numericalfeaturerange];
	];
	plots = Function[{combination, style}, ipredictionPlot1f[model, 
		If[!MatchQ[inputpattern, _List],
			# &,
			Insert[combination, #, numericalfeatures] &],
		PlotRange -> {numericalfeaturerange, valuerange},
		"TestSetPlotStyle" -> style,
		"PredictionPlotStyle" -> style,
		"FeatureName" -> numericalfeaturename,
		"ValueName" -> valuename,
		"ConfidenceProbability" -> OptionValue["ConfidenceProbability"],
		"TestSet" -> If[testset =!= None,
			testsetindices = First /@ Position[X, _?(#[[nominalfeatures]] === combination &), {1}, Heads -> False];
			{First /@ X[[testsetindices]][[All, numericalfeatures]], Y[[testsetindices]]}
			,
			None],
		PlotLabel -> If[singleplot,
			None,
			Row[Riffle[Row[{#1 <> "=", #2}] & @@@ Transpose[{nominalfeaturenames, combination}], ", "]]],
		"PredictionLegend" -> If[singleplot && Length[combination]>0,
			{Row[{
				"prediction for ",
				Row[Riffle[Row[{#1 <> "=", #2}] & @@@ Transpose[{nominalfeaturenames, combination}], " and "]]
			}]}
			,
			{"prediction"}],
		If[OptionValue["SinglePlot"],
			ImageSize -> Automatic,
			ImageSize -> 200]
	]] @@@ Transpose[{
		nominallabelscombinations,
		ColorData[1, "ColorList"][[;;Length[nominallabelscombinations]]]
	}];
	If[OptionValue["SinglePlot"],
		Show[plots],
		Row[plots]
	]
];

Options[ipredictionPlot1f] = {
	"ConfidenceProbability" -> 0.95,
	PlotRange -> Automatic,
	"TestSetPlotStyle" -> RGBColor[0.247`, 0.24`, 0.6`],
	"PredictionPlotStyle" -> RGBColor[0.247`, 0.24`, 0.6`],
	"PredictionLegend" -> {"prediction"},
	"TestSetLegend" -> {"test data"},
	"FeatureName" -> "feature",
	"ValueName" -> "value",
	"TestSet" -> None,
	ImageSize -> Automatic,
	PlotLabel -> None
};

ipredictionPlot1f[model_, process_, opts:OptionsPattern[]] := Module[
	{plots, confidence, range, style, featurename, valuename, testset},
	{confidence, range, style, featurename, valuename, testset} = OptionValue[
		{"ConfidenceProbability", PlotRange, "PredictionPlotStyle", "FeatureName", "ValueName", "TestSet"}
	];
	plots = {};
	If[confidence =!= None, AppendTo[plots,
		Legended[ContourPlot[
			CDF[model[process[x], "Distribution"]][y],
			{x, range[[1, 1]], range[[1, 2]]},
			{y, range[[2, 1]], range[[2, 2]]},
			PlotRange -> range,
			ContourShading -> {{White, Opacity[0.]}, {Lighter[style, 0.9], Opacity[0.5]}, {White, Opacity[0.]}},
			Contours -> ({(1. + #)/2, (1. - #)/2} & @ confidence),
			ContourLabels -> None,
			ContourStyle -> Lighter[style, 0.7],
			FrameLabel -> {featurename, valuename},
			Frame -> {{True, False}, {True, False}}
			]
			,
			SwatchLegend[{Lighter[style, 0.9]}, {ToString[Round[confidence*100]] <> "% confidence region"}]
		]
	]];
	If[testset =!= None,
		AppendTo[plots,
			ListPlot[
				Transpose[testset],
				PlotLegends -> OptionValue["TestSetLegend"],
				PlotStyle -> OptionValue["TestSetPlotStyle"],
				FrameLabel -> {featurename, valuename},
				PlotRange -> range,
				Frame -> {{True, False}, {True, False}},
				Axes -> None
			]
		];
	];
	AppendTo[plots,
		Plot[model[process[x]], {x, range[[1, 1]], range[[1, 2]]}, 
			PlotStyle -> style, 
			PlotLegends-> OptionValue["PredictionLegend"],
			FrameLabel -> {featurename, valuename},
			PlotRange -> range,
			GridLines -> None,
			Frame -> {{True, False}, {True, False}},
			AspectRatio -> 1,
			Axes -> None
		]
	];
	Show[plots, ImageSize -> OptionValue[ImageSize], PlotLabel -> OptionValue[PlotLabel]]
];


Options[predictionPlot2f] = {
	"TestSet" -> None,
	"ConfidenceProbability" -> 0.95,
	PlotLabel -> "PredictionPlot",
	"SinglePlot" -> False,
	ImageSize -> Automatic,
	"NominalFeatures" -> {},
	"NumericalFeatures" -> {}
};
(*could be merged with predictionPlot1f*)
predictionPlot2f[model_, opts:OptionsPattern[]] := Module[
	{n, X, Y, nominalfeatures, numericalfeatures, testset, testsetindices,
		numericalfeaturename, numericalfeaturerange, valuename, valuerange, singleplot,
		nominalfeaturenames, nominallabels, nominallabelscombinations, plots
	},
	singleplot = OptionValue["SinglePlot"];
	nominalfeatures = OptionValue["NominalFeatures"];
	numericalfeatures = OptionValue["NumericalFeatures"];
	nominallabels = model[[3, 1, 2, nominalfeatures, 3]];
	nominalfeaturenames = model[[3, 1, 2, nominalfeatures, 1]];
	nominallabelscombinations = Tuples[nominallabels];
	{numericalfeaturename, numericalfeaturerange} = Transpose[model[[3, 1, 2, numericalfeatures, {1, 3}]]];	
	{valuename, valuerange} = model[[3, 1, 3]];
	testset = OptionValue["TestSet"];
	n = model[[3, 1, 1, 2]];
	If[testset =!= None, 
		{X, Y} = ParsePredictorDataset[testset];
		valuerange = FindPlotRange[{valuerange, Y}];
		numericalfeaturerange = FindPlotRange /@ Transpose[{numericalfeaturerange, Transpose[X][[numericalfeatures]]}];
		,
		valuerange = FindPlotRange[valuerange];
		numericalfeaturerange = FindPlotRange /@ numericalfeaturerange;
	];
	plots = Function[{combination, style}, ipredictionPlot2f[model, 
		Module[{vector}, 
			vector = ConstantArray[0., {n}];
			vector[[numericalfeatures]] = #;
			vector[[nominalfeatures]] = combination;
			vector
		] &,
		PlotRange -> {Sequence @@ numericalfeaturerange, valuerange},
		"TestSetPlotStyle" -> style,
		"PredictionPlotStyle" -> style,
		"FeatureName" -> numericalfeaturename,
		"ValueName" -> valuename,
		"ConfidenceProbability" -> OptionValue["ConfidenceProbability"],
		"TestSet" -> If[testset =!= None,
			testsetindices = First /@ Position[X, _?(#[[nominalfeatures]] === combination &), {1}, Heads -> False];
			{X[[testsetindices]][[All, numericalfeatures]], Y[[testsetindices]]}
			,
			None],
		PlotLabel -> If[singleplot,
			None,
			Row[Riffle[Row[{#1 <> "=", #2}] & @@@ Transpose[{nominalfeaturenames, combination}], ", "]]],
		"PredictionLegend" -> If[singleplot && Length[combination]>0,
			{Row[{
				"prediction for ",
				Row[Riffle[Row[{#1 <> "=", #2}] & @@@ Transpose[{nominalfeaturenames, combination}], " and "]]
			}]}
			,
			{"prediction"}],
		If[OptionValue["SinglePlot"],
			ImageSize -> Automatic,
			ImageSize -> 200]
	]] @@@ Transpose[{
		nominallabelscombinations,
		ColorData[60, "ColorList"][[;;Length[nominallabelscombinations]]]
	}];
	If[OptionValue["SinglePlot"],
		Show[plots],
		Row[plots]
	]
];

Options[ipredictionPlot2f] = {
	"ConfidenceProbability" -> 0.95,
	PlotRange -> Automatic,
	"TestSetPlotStyle" -> RGBColor[0.247`, 0.24`, 0.6`],
	"PredictionPlotStyle" -> RGBColor[0.247`, 0.24`, 0.6`],
	"PredictionLegend" -> {"prediction"},
	"TestSetLegend" -> {"test data"},
	"FeatureName" -> "feature",
	"ValueName" -> "value",
	"TestSet" -> None,
	ImageSize -> Automatic,
	PlotLabel -> None
};

ipredictionPlot2f[model_, process_, opts:OptionsPattern[]] := Module[
	{plots, confidence, range, style, featurename, valuename, testset},
	{confidence, range, style, featurename, valuename, testset} = OptionValue[
		{"ConfidenceProbability", PlotRange, "PredictionPlotStyle", "FeatureName", "ValueName", "TestSet"}
	];
	plots = {};
		AppendTo[plots,
		Plot3D[model[process[{x, y}]], 
			{x, range[[1, 1]], range[[1, 2]]},
			{y, range[[2, 1]], range[[2, 2]]},
			PlotStyle -> {style, Opacity[0.2]}, 
			PlotLegends-> OptionValue["PredictionLegend"],
			AxesLabel -> Flatten[{featurename, valuename}],
			PlotRange -> range,
			AspectRatio -> 1
		]
	];
	If[confidence =!= None, AppendTo[plots,
		Legended[ContourPlot3D[
			Hold[CDF[model[process[{x, y}], "Distribution"]][z]],
			{x, range[[1, 1]], range[[1, 2]]},
			{y, range[[2, 1]], range[[2, 2]]},
			{z, range[[3, 1]], range[[3, 2]]},
			PlotRange -> range,
			Contours -> ({(1. + #)/2, (1. - #)/2} & @ confidence),
			ContourStyle -> Directive[Lighter[style, 0.9], Opacity[0.2]],
			PerformanceGoal -> "Speed",
		(*	PlotLegends -> {ToString[Round[confidence*100]] <> "% confidence boundaries"},*)
			AxesLabel -> Flatten[{featurename, valuename}],
			AspectRatio -> 1
		],
			SwatchLegend[{Lighter[style, 0.9]}, {ToString[Round[confidence*100]] <> "% confidence boundaries"}]
		]
	]];
	If[testset =!= None, AppendTo[plots,
		ListPointPlot3D[
			Flatten /@ Transpose[testset],
			PlotLegends -> OptionValue["TestSetLegend"],
			PlotStyle -> OptionValue["TestSetPlotStyle"],
			AxesLabel -> Flatten[{featurename, valuename}],
			PlotRange -> range,
			AspectRatio -> 1
		]
	]];
	Show[plots, ImageSize -> OptionValue[ImageSize], PlotLabel -> OptionValue[PlotLabel]]
];

Options[ClassificationPlot] = {
	"TestSet" -> None
};

ClassificationPlot[model_, opts:OptionsPattern[]] := catchabortVisualization @ Module[
	{n, featuretypes, nominalfeatures, numericalfeatures,
		nnominal, nnumerical},
	featuretypes = model[[3, 1, 2, All, 2]];
	numericalfeatures = First /@ Position[featuretypes, "numerical"];
	nominalfeatures = First /@ Position[featuretypes, "nominal"];
	nnominal = Length[nominalfeatures];
	nnumerical = Length[numericalfeatures];
	n = model[[3, 1, 1, 2]];
	If[nnominal + nnumerical != n, abortVisualization[]];
	Which[
		nnumerical == 0, classificationPlot0f[model, opts],
		nnumerical == 1, classificationPlot1f[model,
			"NominalFeatures" -> nominalfeatures,
			"NumericalFeatures" -> numericalfeatures,
			opts],
		nnumerical == 2, classificationPlot2f[model,
			"NominalFeatures" -> nominalfeatures,
			"NumericalFeatures" -> numericalfeatures,
			opts],
		nnumerical == 3, classificationPlot3f[model,
			"NominalFeatures" -> nominalfeatures,
			"NumericalFeatures" -> numericalfeatures,
			opts],
		True, abortVisualization[];
	]
];


classificationPlot0f[model_, opts___] := abortVisualization[];
classificationPlot1f[model_, opts___] := abortVisualization[];

Options[classificationPlot2f] = {
	"TestSet" -> None,
	PlotLabel -> "ClassificationPlot",
	ImageSize -> Automatic,
	"NominalFeatures" -> {},
	"NumericalFeatures" -> {}
};

classificationPlot2f[model_, opts:OptionsPattern[]] := Module[
	{n, X, Y, nominalfeatures, numericalfeatures, testset, testsetindices,
		numericalfeaturename, numericalfeaturerange, classname, classlabels,
		nominalfeaturenames, nominallabels, nominallabelscombinations, plots, imagesize
	},
	nominalfeatures = OptionValue["NominalFeatures"];
	numericalfeatures = OptionValue["NumericalFeatures"];
	nominallabels = model[[3, 1, 2, nominalfeatures, 3]];
	nominalfeaturenames = model[[3, 1, 2, nominalfeatures, 1]];
	nominallabelscombinations = Tuples[nominallabels];
	{numericalfeaturename, numericalfeaturerange} = Transpose[model[[3, 1, 2, numericalfeatures, {1, 3}]]];	
	{classname, classlabels} = model[[3, 1, 3]];
	testset = OptionValue["TestSet"];
	n = model[[3, 1, 1, 2]];
	imagesize = OptionValue[ImageSize];
	If[testset =!= None, 
		{X, Y} = ParseClassifierDataset[testset];
		numericalfeaturerange = FindPlotRange /@ Transpose[{numericalfeaturerange, Transpose[X][[numericalfeatures]]}];
		,
		numericalfeaturerange = FindPlotRange /@ numericalfeaturerange;
	];
	plots = Function[{combination}, iclassificationPlot2f[model, 
		Module[{vector}, 
			vector = ConstantArray[0., {n}];
			vector[[numericalfeatures]] = #;
			vector[[nominalfeatures]] = combination;
			vector
		] &,
		PlotRange -> numericalfeaturerange,
		"FeatureName" -> numericalfeaturename,
		"ClassName" -> classname,
		"ClassLabels" -> classlabels,
		"TestSet" -> If[testset =!= None,
			testsetindices = First /@ Position[X, _?(#[[nominalfeatures]] === combination &), {1}, Heads -> False];
			{X[[testsetindices]][[All, numericalfeatures]], Y[[testsetindices]]}
			,
			None],
		PlotLabel -> Row[Riffle[Row[{#1 <> "=", #2}] & @@@ Transpose[{nominalfeaturenames, combination}], ", "]],
		ImageSize -> If[imagesize === Automatic, 
			If[Length[nominallabelscombinations]>1,
				200,
				300
			],
			imagesize
		]
	]] /@ nominallabelscombinations;
	Row[plots]
];


Options[iclassificationPlot2f] = {
	PlotRange -> Automatic,
	"TestSetLegend" -> {"test data"},
	"FeatureName" -> {"feature1", "feature2"},
	"ClassName" -> "class",
	"ClassLabels" -> {},
	"TestSet" -> None,
	ImageSize -> Automatic,
	PlotLabel -> None
};

iclassificationPlot2f[model_, process_, opts:OptionsPattern[]] := Module[
	{plots, points, plotstyle, legend, range, testset, classlabels, featurename},
	plotstyle = ColorData[1, "ColorList"];
	testset = OptionValue["TestSet"];
	classlabels = OptionValue["ClassLabels"];
	featurename = OptionValue["FeatureName"];
	range = OptionValue[PlotRange];
	legend = MapAt[Row[{"predicted region for ", OptionValue["ClassName"]," = ", #}] &, classlabels, 1];
	plots = Reverse[RegionPlot[
		{Hold[SameQ[model[process[{x, y}]], #1]]},
		{x, range[[1, 1]], range[[1, 2]]},
		{y, range[[2, 1]], range[[2, 2]]},
		PlotStyle -> #2,
		PlotLegends -> {#3},
		FrameLabel -> featurename
	] & @@@ Transpose[{
		classlabels,
		(Lighter[#, 0.5] & /@ plotstyle[[;;Length[classlabels]]]),
		legend
	}]];
	If[testset =!= None, 
		points = Pick[testset[[1]], testset[[2]], #] & /@ classlabels;
		AppendTo[plots,
			ListPlot[points,
				PlotRange-> range, 
				PlotStyle -> (Darker[#, 0.2] & /@ plotstyle[[;;Length[classlabels]]]),
				AxesLabel -> {"feature1", "feature2"},
				PlotLegends -> MapAt[Row[{#, ", test examples"}] &, classlabels, 1]
			]
		];
	];
	Show[plots, ImageSize -> OptionValue[ImageSize], PlotLabel-> OptionValue[PlotLabel]]
];

Options[classificationPlot3f] = {
	"TestSet" -> None,
	PlotLabel -> "ClassificationPlot",
	ImageSize -> Automatic,
	"NominalFeatures" -> {},
	"NumericalFeatures" -> {}
};

classificationPlot3f[model_, opts:OptionsPattern[]] := Module[
	{n, X, Y, nominalfeatures, numericalfeatures, testset, testsetindices,
		numericalfeaturename, numericalfeaturerange, classname, classlabels,
		nominalfeaturenames, nominallabels, nominallabelscombinations, plots, imagesize
	},
	nominalfeatures = OptionValue["NominalFeatures"];
	numericalfeatures = OptionValue["NumericalFeatures"];
	nominallabels = model[[3, 1, 2, nominalfeatures, 3]];
	nominalfeaturenames = model[[3, 1, 2, nominalfeatures, 1]];
	nominallabelscombinations = Tuples[nominallabels];
	{numericalfeaturename, numericalfeaturerange} = Transpose[model[[3, 1, 2, numericalfeatures, {1, 3}]]];	
	{classname, classlabels} = model[[3, 1, 3]];
	testset = OptionValue["TestSet"];
	n = model[[3, 1, 1, 2]];
	imagesize = OptionValue[ImageSize];
	If[testset =!= None, 
		{X, Y} = ParseClassifierDataset[testset];
		numericalfeaturerange = FindPlotRange /@ Transpose[{numericalfeaturerange, Transpose[X][[numericalfeatures]]}];
		,
		numericalfeaturerange = FindPlotRange /@ numericalfeaturerange;
	];
	plots = Function[{combination}, iclassificationPlot3f[model, 
		Module[{vector}, 
			vector = ConstantArray[0., {n}];
			vector[[numericalfeatures]] = #;
			vector[[nominalfeatures]] = combination;
			vector
		] &,
		PlotRange -> numericalfeaturerange,
		"FeatureName" -> numericalfeaturename,
		"ClassName" -> classname,
		"ClassLabels" -> classlabels,
		"TestSet" -> If[testset =!= None,
			testsetindices = First /@ Position[X, _?(#[[nominalfeatures]] === combination &), {1}, Heads -> False];
			{X[[testsetindices]][[All, numericalfeatures]], Y[[testsetindices]]}
			,
			None],
		PlotLabel -> Row[Riffle[Row[{#1 <> "=", #2}] & @@@ Transpose[{nominalfeaturenames, combination}], ", "]],
		ImageSize -> If[imagesize === Automatic, 
			If[Length[nominallabelscombinations]>1,
				200,
				300
			],
			imagesize
		]
	]] /@ nominallabelscombinations;
	Row[plots]
];

Options[iclassificationPlot3f] = {
	PlotRange -> Automatic,
	"TestSetLegend" -> {"test data"},
	"FeatureName" -> {"feature1", "feature2", "feature3"},
	"ClassName" -> "class",
	"ClassLabels" -> {},
	"TestSet" -> None,
	ImageSize -> Automatic,
	PlotLabel -> None
};

iclassificationPlot3f[model_, process_, opts:OptionsPattern[]] := Module[
	{plots, points, plotstyle, legend, range, testset, classlabels, featurename},
	plotstyle = ColorData[60, "ColorList"];
	testset = OptionValue["TestSet"];
	classlabels = OptionValue["ClassLabels"];
	featurename = OptionValue["FeatureName"];
	range = OptionValue[PlotRange];
	legend = MapAt[Row[{OptionValue["ClassName"]," = ", #}] &, classlabels, 1];
	plots = Reverse[RegionPlot3D[
		SameQ[model[process[{x, y, z}]], #1],
		{x, range[[1, 1]], range[[1, 2]]},
		{y, range[[2, 1]], range[[2, 2]]},
		{z, range[[3, 1]], range[[3, 2]]},
		PlotStyle -> #2,
		PlotLegends -> {#3},
		AxesLabel -> featurename,
		Mesh -> None
	] & @@@ Transpose[{
		classlabels,
		Directive[#, Opacity[0.3]] & /@ plotstyle[[;;Length[classlabels]]],
		legend
	}]];
	If[testset =!= None, 
		points = Pick[testset[[1]], testset[[2]], #] & /@ classlabels;
		AppendTo[plots,
			ListPointPlot3D[points,
				PlotRange-> range, 
				PlotStyle -> plotstyle[[;;Length[classlabels]]],
				AxesLabel -> featurename,
				PlotLegends -> MapAt[Row[{#, ", test examples"}] &, classlabels, 1]
			]
		];
	];
	Show[plots, ImageSize -> OptionValue[ImageSize], PlotLabel-> OptionValue[PlotLabel]]
];

Options[DistributionClassifierPlot] = {
	"TestSet" -> None,
	"SinglePlot" -> True,
	"Class" -> All
};

DistributionClassifierPlot[model_, opts:OptionsPattern[]] := catchabortVisualization @ Module[
	{n, featuretypes, nominalfeatures, numericalfeatures,
		nnominal, nnumerical},
	featuretypes = model[[3, 1, 2, All, 2]];
	numericalfeatures = First /@ Position[featuretypes, "numerical"];
	nominalfeatures = First /@ Position[featuretypes, "nominal"];
	nnominal = Length[nominalfeatures];
	nnumerical = Length[numericalfeatures];
	n = model[[3, 1, 1, 2]];
	If[nnominal + nnumerical != n, abortVisualization[]];
	Which[
		nnumerical == 0, distributionPlot0f[model, opts],
		nnumerical == 1, distributionPlot1f[model,
			"NominalFeatures" -> nominalfeatures,
			"NumericalFeatures" -> numericalfeatures,
			opts],
		nnumerical == 2, distributionPlot2f[model,
			"NominalFeatures" -> nominalfeatures,
			"NumericalFeatures" -> numericalfeatures,
			opts],
		nnumerical == 3, distributionPlot3f[model,
			"NominalFeatures" -> nominalfeatures,
			"NumericalFeatures" -> numericalfeatures,
			opts],
		True, abortVisualization[];
	]
];

distributionPlot0f[model_, opts___] := abortVisualization[];

Options[distributionPlot1f] = {
	"TestSet" -> None,
	"SinglePlot" -> True,
	ImageSize -> Automatic,
	"NominalFeatures" -> {},
	"NumericalFeatures" -> {},
	"Class" -> All
};
distributionPlot1f[model_, opts:OptionsPattern[]] := Module[
	{n, X, Y, nominalfeatures, numericalfeatures, testset, testsetindices,
		numericalfeaturename, numericalfeaturerange, classname, classlabels, singleplot,
		nominalfeaturenames, nominallabels, nominallabelscombinations, inputpattern, plots,
		plottedclass
	},
	inputpattern = model[[2, 1, 3]];
	singleplot = OptionValue["SinglePlot"];
	nominalfeatures = OptionValue["NominalFeatures"];
	numericalfeatures = OptionValue["NumericalFeatures"];
	nominallabels = model[[3, 1, 2, nominalfeatures, 3]];
	nominalfeaturenames = model[[3, 1, 2, nominalfeatures, 1]];
	nominallabelscombinations = Tuples[nominallabels];
	{numericalfeaturename, numericalfeaturerange} = Transpose[model[[3, 1, 2, numericalfeatures, {1, 3}]]];
	{classname, classlabels} = model[[3, 1, 3]];
	plottedclass = OptionValue["Class"];
	If[plottedclass =!= All, classlabels = {plottedclass}];
	testset = OptionValue["TestSet"];
	If[testset =!= None, 
		{X, Y} = ParseClassifierDataset[testset];
		numericalfeaturerange = FindPlotRange /@ Transpose[{numericalfeaturerange, Transpose[X][[numericalfeatures]]}];
		,
		numericalfeaturerange = FindPlotRange /@ numericalfeaturerange;
	];
	
	n = model[[3, 1, 1, 2]];

	plots = Function[{combination, style}, idistributionPlot1f[model,
		If[!MatchQ[inputpattern, _List],
			# &,
			Module[{vector}, 
				vector = ConstantArray[0., {n}];
				vector[[numericalfeatures]] = #;
				vector[[nominalfeatures]] = combination;
				vector
			] &
		],
		PlotRange -> {Sequence @@ numericalfeaturerange, {0., 1.01}},
		"TestSetPlotStyle" -> style,
		"ClassificationPlotStyle" -> style,
		"FeatureName" -> numericalfeaturename,
		"ClassName" -> classname,
		"ClassLabels" -> classlabels,
		"TestSet" -> If[testset =!= None,
			testsetindices = First /@ Position[X, _?(#[[nominalfeatures]] === combination &), {1}, Heads -> False];
			{X[[testsetindices]][[All, numericalfeatures]], Y[[testsetindices]]}
			,
			None],
		PlotLabel -> Row[Riffle[Row[{#1 <> "=", #2}] & @@@ Transpose[{nominalfeaturenames, combination}], ", "]],
		"ClassificationLegend" -> If[singleplot && Length[combination]>0,
			{Row[{
				"probability for ",
				Row[Riffle[Row[{#1 <> "=", #2}] & @@@ Transpose[{nominalfeaturenames, combination}], " and "]]
			}]}
			,
			{"probability"}],
		ImageSize -> If[OptionValue[ImageSize] === Automatic,
			If[Length[combination]>1 && OptionValue["Class"] === All,
				200,
				Automatic
			]
			,
			OptionValue[ImageSize]
		]
	]] @@@ Transpose[{
		nominallabelscombinations,
		ColorData[1, "ColorList"][[;;Length[nominallabelscombinations]]]
	}];
	If[OptionValue["Class"] === All, 
		Row[plots],
		Show[plots]
	]
];

Options[idistributionPlot1f] = {
	PlotRange -> Automatic,
	"TestSetPlotStyle" -> RGBColor[0.247`, 0.24`, 0.6`],
	"ClassificationPlotStyle" -> RGBColor[0.247`, 0.24`, 0.6`],
	"ClassificationLegend" -> {"prediction"},
	"TestSetLegend" -> {"test data"},
	"FeatureName" -> "feature",
	"ClassName" -> "class",
	"ClassLabels" -> {},
	"TestSet" -> None,
	ImageSize -> Automatic,
	PlotLabel -> None
};

idistributionPlot1f[model_, process_, opts:OptionsPattern[]] := Module[
	{nclass, plots, points, ypoint, range, style, featurename, classname, classlabels, testset, curves},
	{range, style, featurename, classname, classlabels, testset} = OptionValue[
		{PlotRange, "ClassificationPlotStyle", "FeatureName", "ClassName", "ClassLabels", "TestSet"}
	];
	nclass = Length[classlabels];
	ypoint = 1.;
	plots = {};
	curves = Hold[model[process[x], {"Probability", #}] ] & /@ classlabels;
	AppendTo[plots,
		Plot[
			curves,
			{x, range[[1, 1]], range[[1, 2]]},
			FrameLabel -> {featurename[[1]], "probability"},
			Frame -> {{True, False}, {True, False}},
			PlotRange -> range,
			PlotLegends -> If[nclass == 1, 
				{OptionValue[PlotLabel]}, 
				Map[Row[{"P(", classname, " = ", #, " | ", featurename[[1]], ")"}] &, classlabels]
			]
			,
			Axes -> None,
			PlotStyle -> If[nclass == 1, style, Automatic]
		]
	];
	If[testset =!= None,
		points = ({First[#], ypoint} & /@ Pick[testset[[1]], testset[[2]], #]) & /@ classlabels;
		AppendTo[plots,
			ListPlot[points, 
				PlotRange -> range,
				PlotLegends -> If[nclass == 1, 
					{"test examples"}, 
					MapAt[Row[{#, ", test examples"}] &, classlabels, 1]
				],
				FrameLabel -> {featurename[[1]], "probability"},
				Frame -> {{True, False}, {True, False}},
				Axes -> None,
				PlotStyle -> If[nclass == 1, style, Automatic]
			]
		]
	];
	Show[plots, PlotLabel -> If[nclass == 1, First[Map[Row[{"P(", classname, " = ", #, " | ", featurename[[1]], ")"}] &, classlabels]], OptionValue[PlotLabel]], ImageSize -> OptionValue[ImageSize]]
];

Options[distributionPlot2f] = {
	"TestSet" -> None,
	"SinglePlot" -> True,
	ImageSize -> Automatic,
	"NominalFeatures" -> {},
	"NumericalFeatures" -> {},
	"Class" -> All
};
distributionPlot2f[model_, opts:OptionsPattern[]] := Module[
	{n, X, Y, nominalfeatures, numericalfeatures, testset, testsetindices,
		numericalfeaturename, numericalfeaturerange, classname, classlabels, singleplot,
		nominalfeaturenames, nominallabels, nominallabelscombinations, inputpattern, plots,
		plottedclass
	},
	inputpattern = model[[2, 1, 3]];
	singleplot = OptionValue["SinglePlot"];
	nominalfeatures = OptionValue["NominalFeatures"];
	numericalfeatures = OptionValue["NumericalFeatures"];
	nominallabels = model[[3, 1, 2, nominalfeatures, 3]];
	nominalfeaturenames = model[[3, 1, 2, nominalfeatures, 1]];
	nominallabelscombinations = Tuples[nominallabels];
	{numericalfeaturename, numericalfeaturerange} = Transpose[model[[3, 1, 2, numericalfeatures, {1, 3}]]];
	{classname, classlabels} = model[[3, 1, 3]];
	
	plottedclass = OptionValue["Class"];
	If[plottedclass =!= All, classlabels = {plottedclass}];
	
	testset = OptionValue["TestSet"];
	If[testset =!= None, 
		{X, Y} = ParseClassifierDataset[testset];
		numericalfeaturerange = FindPlotRange /@ Transpose[{numericalfeaturerange, Transpose[X][[numericalfeatures]]}];
		,
		numericalfeaturerange = FindPlotRange /@ numericalfeaturerange;
	];
	
	n = model[[3, 1, 1, 2]];

	plots = Function[{combination, style}, idistributionPlot2f[model,
		If[!MatchQ[inputpattern, _List],
			# &,
			Module[{vector}, 
				vector = ConstantArray[0., {n}];
				vector[[numericalfeatures]] = #;
				vector[[nominalfeatures]] = combination;
				vector
			] &
		],
		PlotRange -> {Sequence @@ numericalfeaturerange, {0., 1.01}},
		"TestSetPlotStyle" -> style,
		"ClassificationPlotStyle" -> style,
		"FeatureName" -> numericalfeaturename,
		"ClassName" -> classname,
		"ClassLabels" -> classlabels,
		"TestSet" -> If[testset =!= None,
			testsetindices = First /@ Position[X, _?(#[[nominalfeatures]] === combination &), {1}, Heads -> False];
			{X[[testsetindices]][[All, numericalfeatures]], Y[[testsetindices]]}
			,
			None],
		PlotLabel -> If[Length[nominallabelscombinations]>1,
			Row[Riffle[Row[{#1 <> "=", #2}] & @@@ Transpose[{nominalfeaturenames, combination}], ", "]]
			,
			"probability"
		],
		"ClassificationLegend" -> If[singleplot && Length[combination]>0,
			{Row[{
				"probability for ",
				Row[Riffle[Row[{#1 <> "=", #2}] & @@@ Transpose[{nominalfeaturenames, combination}], " and "]]
			}]}
			,
			{"probability"}],
		ImageSize -> If[OptionValue[ImageSize] === Automatic,
			300,
			Automatic
		]
	]] @@@ Transpose[{
		nominallabelscombinations,
		Directive[#, Opacity[0.8]] & /@ ColorData[60, "ColorList"][[;;Length[nominallabelscombinations]]]
	}];
	If[OptionValue["Class"] === All, 
		Row[plots],
		Show[plots]
	]
];

Options[idistributionPlot2f] = {
	PlotRange -> Automatic,
	"TestSetPlotStyle" -> RGBColor[0.247`, 0.24`, 0.6`],
	"ClassificationPlotStyle" -> RGBColor[0.247`, 0.24`, 0.6`],
	"ClassificationLegend" -> {"prediction"},
	"TestSetLegend" -> {"test data"},
	"FeatureName" -> "feature",
	"ClassName" -> "class",
	"ClassLabels" -> {},
	"TestSet" -> None,
	ImageSize -> Automatic,
	PlotLabel -> None
};

idistributionPlot2f[model_, process_, opts:OptionsPattern[]] := Module[
	{nclass, plots, points, ypoint, range, style, featurename, classname, classlabels, testset},
	{range, style, featurename, classname, classlabels, testset} = OptionValue[
		{PlotRange, "ClassificationPlotStyle", "FeatureName", "ClassName", "ClassLabels", "TestSet"}
	];
	nclass = Length[classlabels];
	ypoint = 1.;
	plots = {};
	AppendTo[plots,
		With[{
			curves = Hold[model[process[{x, y}], {"Probability", #}] ] & /@ classlabels,
			legend = Map[Row[{"P(", classname, " = ", #, " | ", featurename[[1]], ", ", featurename[[2]], ")"}] &, classlabels]
			},
			Plot3D[
				curves,
				{x, range[[1, 1]], range[[1, 2]]},
				{y, range[[2, 1]], range[[2, 2]]},
				AxesLabel -> {featurename[[1]], featurename[[2]], "probability"},
				PlotRange -> range,
				PlotStyle -> If[nclass == 1, style,  Directive[#, Opacity[1.]] & /@  ColorData[60, "ColorList"]],
				PlotLegends -> If[nclass == 1, 
					{OptionValue[PlotLabel]}, 
					legend]
			]
		]
	];
	If[testset =!= None,
		points = ({#[[1]], #[[2]], ypoint} & /@ Pick[testset[[1]], testset[[2]], #]) & /@ classlabels;
		AppendTo[plots,
			ListPointPlot3D[points, 
				PlotRange -> range[[3]],
				PlotLegends -> If[nclass == 1, 
					{"test examples"}, 
					MapAt[Row[{#, ", test examples"}] &, classlabels, 1]
				],
				AxesLabel -> {featurename[[1]], featurename[[2]], "probability"},
				PlotStyle -> If[nclass == 1, style, ColorData[60, "ColorList"]]
			]
		]
	];
	Show[plots, PlotLabel -> If[nclass == 1, First[Map[Row[{"P(", classname, " = ", #, " | ", featurename[[1]], ", ", featurename[[2]], ")"}] &, classlabels]], OptionValue[PlotLabel]], ImageSize -> OptionValue[ImageSize]]
];


FindPlotRange[values_] := Module[{xmin, xmax},
	xmin = DeleteCases[Min[values], Missing];
	xmax = DeleteCases[Max[values], Missing];
	xmin = xmin - 0.05*(xmax - xmin);
	xmax = xmax + 0.05*(xmax - xmin);
	If[0 < xmin < xmax/4, xmin = 0];
	If[xmin/2 < xmax < 0, xmax = 0];
	{xmin, xmax}
];
