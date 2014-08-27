Package["MachineLearning`"]


PackageScope["NaiveBayes"]


Options[NaiveBayes] = 
{
	"ClassNumber"-> Automatic,
	"UniformPrior" -> False,
	"SmoothingParameter" -> 2.,
	PerformanceGoal -> Automatic,
	NominalVariables -> Automatic,
	"DiscretizationMethod"-> {"Supervised","MinimumDescriptionLength"},
	"BinaryEncoder"-> Preprocessor["Identity"]
};

NaiveProbabilityFactor[classNum_Integer, feature_List, classes_List, m_]:=Module[
	{
	featureDim
	,p
	,featureVals
	,positions
	,nci
	},
	featureDim=Max[feature];
	p=1/Max[feature];
	featureVals=Range[Max[feature]];
	positions=Flatten[Position[classes, classNum]];
	nci=Count[feature[[positions]], #]&/@featureVals;
	(nci+m*p)/(classNum+m)
]

NaiveBayes[{X_, Y_}, opts:OptionsPattern[]]:=Module[
	{nClass, nExample, kSmoothing, discMethod
	,discretizor, numericalIndices, mSmoothing, nc, catproba
	,probs,evaluationdata, metadata, options,nFeatures,Xdisc},
	(* input values and metadata *)
	If[
		OptionValue["ClassNumber"]===Automatic
			,nClass=Max[Y]
			,nClass=OptionValue["ClassNumber"]
		];
	If[nClass < 2 || Min[Y]<1, Message[Classify::wcnb]; abortLearning[]];
	mSmoothing=OptionValue["SmoothingParameter"];
	kSmoothing=1;
	nFeatures=Length[First@X];
	(*Create discretization preprocessor*)
	numericalIndices=Complement[Range[nFeatures], OptionValue[NominalVariables]];
	discMethod=OptionValue["DiscretizationMethod"];
	If[First@discMethod==="Supervised",
		discretizor=CreateDiscretizer[{X, Y}, discMethod[[2]], NominalVariables -> OptionValue[NominalVariables]]
		,discretizor=CreateDiscretizer[X, discMethod[[2]], NominalVariables -> OptionValue[NominalVariables]]
		];
	Xdisc=discretizor[X];
	(* calculate probability tensor *)
	If[OptionValue["UniformPrior"]
		,catproba=1/nClass
		,catproba=(nc+kSmoothing)/(nExample+nClass*kSmoothing)
		];
	nc=Count[Y,#]&/@Range[nClass];
	nExample=Length@Y;
	probs=Outer[NaiveProbabilityFactor[#1, #2, Y, mSmoothing]&,Range[nClass],Transpose@Xdisc, 1];
	probs=Outer[Times, Sequence@@probs[[#]]]&/@Range[nClass];
	probs=catproba*probs;
	evaluationdata={{discretizor, Preprocessor["Identity"], Table[Except[_List], {nFeatures}]},{probs}};
	metadata={{{nExample, nFeatures, nClass}, {Table[{}, {nFeatures}]}, {"class", Range[nClass]}},{}};
	options = Options[NaiveBayes];
	options = DeleteDuplicates[Join[{opts}, options], First[#1] === First[#2] &];
	ClassifierFunction["NaiveBayes", evaluationdata, metadata, options]
]

(* If feature vector has features greater than seen during the training phase, marginalize over that feature
so it give no information for the class probability *)
distributionMarginalize[X_, probs_, featureDim_]:=Module[
	{compare, trues, falses, marginalized, classProbs},
	compare=Thread[#1>#2&[X, featureDim]];
	trues=Flatten@Position[compare, True];
	If[Length@trues==0
		,classProbs=Normalize[Flatten@probs[[;;, Sequence@@X]]]
		,falses=Complement[Range[Length@X], trues];
			marginalized=Total[probs, trues+1];
			classProbs=Normalize[
								Flatten@marginalized[[;;, Sequence@@X[[falses]]]]]
		];
	classProbs
]

iClassifierFunction["NaiveBayes"][eval_, _, _][X_]:=Module[
	{probDist, featureDim},
	probDist=eval[[2, 1]];
	featureDim=Dimensions[probDist][[2;;]];
	probDist=distributionMarginalize[#, probDist, featureDim]&/@X;
	MaxLoc[probDist]
	]	

iClassifierFunction["NaiveBayes"][eval_, meta_, _][X_, "DistributionList"]:=Module[
	{classes,probDist,featureDim},
	classes=meta[[1, 3, 2]];
	probDist=eval[[2, 1]];
	featureDim=Dimensions[probDist][[2;;]];
	probDist=distributionMarginalize[#, probDist, featureDim]&/@X;
	Thread[#1 -> #2 &[classes, #]]&/@probDist
	]
	
iClassifierFunction["NaiveBayes"][eval_, meta_, _][X_, "FullProbabilities"]:=Module[
	{classes, probDist, featureDim},
	classes=meta[[1, 3, 2]];
	probDist=eval[[2, 1]];
	featureDim=Dimensions[probDist][[2;;]];
	probDist=distributionMarginalize[#,probDist, featureDim]&/@X;
	Association[
				Thread[#1 -> #2 &[classes, #]]
				]&/@probDist
	]

iClassifierFunction["NaiveBayes"][eval_, meta_, _][X_, "Probabilities"]:=Module[
	{classes, probDist, featureDim},
	classes=meta[[1, 3, 2]];
	probDist=eval[[2, 1]];
	featureDim=Dimensions[probDist][[2;;]];
	probDist=distributionMarginalize[#, probDist, featureDim]&/@X;
	MostProbableClasses[
		Thread[#1 -> #2 &[classes, #]]&/@probDist
		, 0.1	
	]
	]
	
iClassifierFunction["NaiveBayesText"][eval_, _, _][X_, {"Probability", class_}] := Module[
	{distvec, probDist, classesindex, preprocessory, featureDim},
	Print@1;
	preprocessory = eval[[1, 2]];
	probDist=eval[[2, 1]];
	featureDim=Dimensions[probDist][[2;;]];
	probDist=distributionMarginalize[#, probDist, featureDim]&/@X;
	classesindex = preprocessory[class];
	#[[classesindex]] & /@ distvec
]

iNoneClassifierFunction["NaiveBayes"][_, _, options_]["Options"] := Sequence @@ options;


(* Error Handling *)
(*
iClassifierFunction["NaiveBayes"][___][_, prop_] := AbortMachineLearning["naprop", prop];
iNoneClassifierFunction["NaiveBayes"][___][prop_] := AbortMachineLearning["napropn", prop];
iNonePredictorFunction["NaiveBayes"][___][] := AbortMachineLearning["napropnn"];
*)
