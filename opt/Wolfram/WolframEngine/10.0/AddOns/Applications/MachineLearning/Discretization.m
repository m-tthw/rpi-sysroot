Package["MachineLearning`"]
PackageImport["Developer`"]

PackageScope["CreateDiscretizer"] (* used internally, but not user *)

					
CreateDiscretizer::usage="CreateDiscretizer[data, method, NominalVariables -> {1,2,4} \"PredictedVariable\"-> {Numerical, Nominal}]:
					data of form {features,classes} for supervised, data=features for unsupervised. 
					'features' is a list of examples.'classes' is a list of class-values. 'Method' options available are 'MinimumDescriptionLength'
					Returns Discretizor object.";

(*********Section: Supervised Discretization ********)

automaticBinSize["FreedmanDiaconis", list_List]:=
	2*InterquartileRange[list]/CubeRoot[Length@list]

automaticBinSize["Scott", list_]:=With[{sigmaEst=Min[StandardDeviation[list],InterquartileRange[list]/1.349]}
	,(3.49*sigmaEst)/CubeRoot[Length@list]
	]

automaticBinSize["Sturges", list_]:=
	(Max[list]-Min[list])/(1.+Log2[Length@list])

CutPoints[{"Unsupervised", "EqualWidth"}, list_List, binNumber_]:=Module[
	{min, max, range, binSize},
	max=Max@list;
	min=Min@list;
	range=max-min;
	If[IntegerQ@binNumber
		,binSize=range/binNumber
		,binSize=automaticBinSize[binNumber,list]
			;binSize=range/Ceiling[range/binSize]
		];
	Range[min, max, binSize][[2;;-2]]
	]
	
CutPoints[{"Unsupervised", "EqualFrequency"}, list_List, binNumber_]:=Module[
	{min, max, range, n, binNum, partitions, binSize},
	binNum=binNumber;
	n=Length@list;
	max=Max@list;
	min=Min@list;
	range=max-min;
	If[IntegerQ@binNumber==False
		,binSize=automaticBinSize[binNumber, list]
			;binNum=range/binSize
		];
	partitions=Partition[Sort@list,Ceiling[n/binNum]];
	If[Length@partitions==0
		,Return[{}]
		,Return[(partitions[[2;;, 1]]+partitions[[;;-2, -1]])/2]
		]
	]

(*********Section: Supervised Discretization ********)

		(* MinimumDescriptionLength: Fayyad and Irani (1993), Multi-Interval discretization of continuous-valued
									attributes for classification learning, Artificial Intelligence, 13, 1022-1027*)
									
MDLentCompile=MachineLearning`Libraries`Compile[{{categories, _Integer, 1},{cutIndex, _Integer}},
	Module[
		{n1, n2, subset1, subset2, ent1, ent2, prob1, prob2},
		subset1=categories[[1;;cutIndex]];
		subset2=categories[[cutIndex+1;;-1]];
		n1=N[Length[subset1]];
		n2=N[Length[subset2]];
		prob1=(#[[2]]/n1)&/@Tally[subset1];
		prob2=(#[[2]]/n2)&/@Tally[subset2];
		ent1=-prob1.Log2[prob1];
		ent2=-prob2.Log2[prob2];
		(n1*ent1+n2*ent2)
		],
	RuntimeAttributes->{Listable},Parallelization->True,CompilationTarget->"C"
	];
	
	MDLcutPoint[{features_, categories_}]:=Module[
	{
	n
	,entropy
	,entropy1
	,entropy2
	,partitionEntropy
	,cutIndex
	,subset1
	,subset2
	,k
	,k1
	,k2
	,gain
	,mdl
	,indices
	,maxIndex
	,cutPoint
	},
	n=Length[features];
	entropy=Statistics`Library`NEntropy[categories]; (* entropy base 2 *)
	indices=Range[1, n-1]; (* the last point can't be a cut point *)
	indices=Select[indices,(features[[#]]!=features[[#+1]])&]; 
	If[Length[indices]==0,
		Return[{features,categories}]
		];
	partitionEntropy=MDLentCompile[categories,indices]; 
	maxIndex=Ordering[partitionEntropy][[1]];
	cutIndex=indices[[maxIndex]]; (* find the lowest entropy cut index point*)
	(* we have found the best possible cut-point. Now we need to check whether it is good enough to split here *)
	subset1=categories[[;;cutIndex]];
	subset2=categories[[cutIndex+1;;]];
	entropy1=Statistics`Library`NEntropy[subset1];
	entropy2=Statistics`Library`NEntropy[subset2];
	gain=entropy-(Length[subset1]*entropy1+Length[subset2]*entropy2)/n;
	{k,k1,k2}=Length[Union[#]]&/@{categories,subset1,subset2}; (* the number of categories *)
	mdl=Log[2, n-1]/n+Log[2, 3^k-2]/n-(k*entropy-k1*entropy1-k2*entropy2)/n; (* min description length *)
	cutPoint=(features[[cutIndex]]+features[[cutIndex+1]])/2;
	If[gain>mdl,
		Return[{cutPoint, {features[[;;cutIndex]], subset1}, {features[[cutIndex+1;;]], subset2}}]
		,Return[{features, categories}]]
	];

CutPoints[{"Supervised", "MinimumDescriptionLength"},{features_, categories_}]:=Module[
	{order, featuresSort, categoriesSort, cutPoints, splitSet, setsTemp, setsplit},
	order=Ordering[features];
	featuresSort=features[[order]];
	categoriesSort=categories[[order]];
	cutPoints={};
	splitSet={{featuresSort, categoriesSort}}; (* the sets that will be attempted to be split *)
	While[Length[splitSet]>0,
		setsTemp=MDLcutPoint[#]&/@splitSet;
		setsplit=Select[setsTemp, Length[#]>2&];
		AppendTo[cutPoints, #[[1]]]&/@setsplit;
		splitSet=Sequence@@{#[[2]], #[[3]]}&/@setsplit;
		];
	Sort@cutPoints
	]
		
		
(*********Section:  Discretizor ********)

Options[CreateDiscretizer] = 
{
	"NumberBins"-> "FreedmanDiaconis",
	NominalVariables-> {}
};

CreateDiscretizer[X_, method_, opts:OptionsPattern[]]:=Module[
	{Xtrans, numericalIndices, cutpoints},
	Xtrans=Transpose@X;
	numericalIndices=Complement[Range[Length@Xtrans],OptionValue[NominalVariables]];
	cutpoints=CutPoints[{"Unsupervised", method}, #, OptionValue["NumberBins"]]&/@Xtrans[[numericalIndices]];
	Preprocessor["Discretizer", cutpoints, numericalIndices]
	]

CreateDiscretizer[{X_, classes_}, method_, opts:OptionsPattern[]]:=Module[
	{Xtrans, numericalIndices, cutpoints},
	Xtrans=Transpose@X;
	numericalIndices=Complement[Range[Length@Xtrans], OptionValue[NominalVariables]];
	cutpoints=CutPoints[{"Supervised", "MinimumDescriptionLength"}, {#, classes}]&/@Xtrans[[numericalIndices]];
	Preprocessor["Discretizer", cutpoints, numericalIndices]
	]

Preprocessor["Discretizer", cutpoints_, numericalIndices_][X:{__List}]:=Module[{Xtrans},
	Xtrans=Transpose@X;
	Xtrans[[numericalIndices]]=MapThread[featureDiscretize, {Xtrans[[numericalIndices]], cutpoints}, 1];
	Transpose@Xtrans
]

Preprocessor["Discretizer", cutpoints_, numericalIndices_][X_List]:=
	Flatten@Preprocessor["Discretizer", cutpoints, numericalIndices][{X}]

binarySearch=MachineLearning`Libraries`Compile[{{list, _Real, 1},{value, _Real}},Module[
	{low, high, mid, midVal},
	low = 1;
	high = Length[list];
	While[low <= high,
		mid = Floor[(low + high) / 2];
		midVal = list[[mid]];
		If[midVal < value,
			low = mid + 1,
			If[midVal > value,
				high = mid - 1,
				Return[mid]
				]
			]
		];
	low
	],
RuntimeAttributes->{Listable},Parallelization->True
];

featureDiscretize[feature_, cutpoints_]:=Module[{},
	binarySearch[cutpoints, #]&/@feature
]
