Package["MachineLearning`"]

If[$VersionNumber >= 10,
	
	commonGraphicsOptions = ToExpression["ElisionsDump`commonGraphicsOptions"];
	arrangeBox = ToExpression["BoxForm`ArrangeSummaryBox"];
	makeItem = ToExpression["BoxForm`MakeSummaryItem"];
	defcolor = ToExpression["ElisionsDump`defcolor"];
	
	getBasicInfo[type_, {{m_, n_}, ___}] := {{Method -> type, "Features" -> n}, {"Examples" -> m}};
	getBasicInfo[type_, {{m_, n_, c_}, ___}] := {{Method -> If[type === "MultiLogistic", "LogisticRegression", type], "Classes" -> c}, {"Features" -> n, "Examples" -> m}};
	
	makeGrid[list_List] := Sequence @@ Map[
		makeItem[{ToString[#1] <> ": ", #2}, StandardForm]& @@@ #&,
		list
	];

	ClassifierFunction /: MakeBoxes[cf:ClassifierFunction[type_, _, {meta_, _}, opts_], StandardForm] := 
		arrangeBox[ClassifierFunction, cf, classifierIcon, makeGrid @ getBasicInfo[type, meta], StandardForm];
		
	PredictorFunction /: MakeBoxes[cf:PredictorFunction[type_, pre_, {meta_, _}, opts_], StandardForm] := 
		arrangeBox[PredictorFunction, cf, predictorIcon, makeGrid @ getBasicInfo[type, meta], StandardForm];
	
	classifierIcon = Graphics[{{AbsolutePointSize[1], Red, 
	   Point[{{1.2`, 0.7`}, {0.2`, 1.7`}, {0.45`, 0.95`}, {0.7`, 
	      0.2`}, {1.45`, 0.7`}, {0.95`, 0.45`}, {0.95`, 1.2`}, {0.7`, 
	      0.7`}, {1.45`, 0.2`}, {1.2`, 0.45`}, {0.95`, 1.2`}, {0.7`, 
	      0.45`}, {0.95`, 1.2`}, {0.45`, 1.2`}, {0.7`, 1.7`}}], Blue, 
	   Point[{{-0.2`, -0.45`}, {-0.7`, -0.7`}, {-1.2`, -0.7`}, {-0.95`, \
	-1.2`}, {-0.2`, -0.7`}, {-0.95`, -1.7`}, {-0.7`, -1.45`}, {-0.7`, \
	-0.95`}, {-0.7`, -1.7`}, {-1.45`, -0.7`}, {-0.95`, -0.7`}, {-1.2`, \
	-0.95`}, {-0.2`, -1.45`}, {-1.45`, -0.7`}, {-1.7`, -1.2`}}]}, \
	{defcolor, AbsoluteThickness[1], 
	   Line[{{1, -1.3}, {-1, 1.3}}]}}, {commonGraphicsOptions, 
	  Axes -> {False, False}, AxesLabel -> {None, None}, 
	  AxesOrigin -> {0, 0}, 
	  BaseStyle -> {FontFamily -> "Arial", AbsoluteThickness[1.5]}, 
	  DisplayFunction -> Identity, Frame -> {{True, True}, {True, True}}, 
	  FrameLabel -> {{None, None}, {None, None}}, 
	  FrameTicks -> {{None, None}, {None, None}}, 
	  GridLines -> {None, None}, ImageSize -> 35, 
	  LabelStyle -> {FontFamily -> "Arial"}, 
	  Method -> {"ScalingFunctions" -> None}, 
	  PlotRange -> {{-1.2, 1}, {-1.5, 1}}, PlotRangeClipping -> True, 
	  PlotRangePadding -> 0.7*{{1, 1}, {1, 1}}, Ticks -> {None, None}}];
	
	predictorIcon = Graphics[{{}, {{}, {AbsolutePointSize[1], Red, 
	    Point[{{-1, 1.5}, {0, 2.3}, {1, 0.8}, {2, 2}, {3, 1.3}, {4, 
	       4.0}}]}, {defcolor, AbsoluteThickness[1], 
	    BSplineCurve[{{-1, 2}, {0, 2}, {1, 1}, {2, 1}, {4, 2}, {5, 
	       4}}]}}, {}}, {commonGraphicsOptions, Axes -> {False, False}, 
	  AxesLabel -> {None, None}, AxesOrigin -> {0, 0}, 
	  BaseStyle -> {FontFamily -> "Arial", AbsoluteThickness[1.5]}, 
	  DisplayFunction -> Identity, Frame -> {{True, True}, {True, True}}, 
	  FrameLabel -> {{None, None}, {None, None}}, 
	  FrameTicks -> {{None, None}, {None, None}}, 
	  GridLines -> {None, None}, ImageSize -> 35, 
	  LabelStyle -> {FontFamily -> "Arial"}, 
	  Method -> {"ScalingFunctions" -> None}, PlotRange -> {All, All}, 
	  PlotRangeClipping -> True, PlotRangePadding -> {{1, 1}, {1, 1}}, 
	  Ticks -> {None, None}}];
	 
	,
	Format[ClassifierFunction[name_, _, {{{m_, n_, nclass_}, ___}, _}, _], StandardForm] := 
		"ClassifierFunction"[If[name === "MultiLogistic", "LogisticRegression", name], 
			"FeatureNumber" -> n,
			"ClassNumber" -> nclass
		];
	Format[PredictorFunction[name_, _, {{{m_, n_}, ___}, _}, _], StandardForm] := 
		"PredictorFunction"[name, 
			"FeatureNumber" -> n
		];
	
	
];