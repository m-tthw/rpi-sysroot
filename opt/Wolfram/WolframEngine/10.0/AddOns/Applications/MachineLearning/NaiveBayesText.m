Package["MachineLearning`"]

PackageImport["Developer`"]

PackageImport["MachineLearning`Structs`SortedHashArray`"]

PackageScope["NaiveBayesText"]

Options[NaiveBayesText] = 
{
	"Preprocessing" -> "ToLowerCase",
	"ClassNumber"-> Automatic,
	"Token" -> "Word",
	"Emoticons"-> True,
	"Punctuation"-> False,
	"CapitalizedPhrases" -> False,
	"URLStem" -> False,
	(*"Stem" -> False,*)
	"UniformPrior" -> False,
	"MinimumTokenCount" -> Automatic,
	"ExcludedTokens"-> {},
	"SpecialTokens"-> {},
	"AdditiveSmoothing" -> 1.,
	PerformanceGoal -> Automatic
};

NaiveBayesText[{X_, Y_}, opts:OptionsPattern[]] := Module[
	{nclass, docbycat, cat, catproba, wordproba, vocabulary, vocabularyPosition, 
		alpha, evaluationdata, metadata, options, mincount, ntoken, preprocessorx},
	alpha = OptionValue["AdditiveSmoothing"];
	nclass = OptionValue["ClassNumber"];
	If[nclass === Automatic, nclass = Max[Y]];
	cat = N@Range[nclass];
	docbycat = Pick[X, Y, #] & /@ cat;
	catproba = Length[#] + 1. & /@ docbycat;
	catproba /= Total[catproba];
	docbycat = StringJoin[Riffle[#, " "]] & /@ docbycat;
	preprocessorx = Preprocess[X,
		"InputMissing" -> None,
		"Preprocessing" -> OptionValue["Preprocessing"],
		NominalVariables -> None
	];
	preprocessorx = JoinPreprocessors[
		preprocessorx, 
		CreatePreprocessor[X, "WordTokenize", FilterOptions[CreatePreprocessor, opts]]
	];
	docbycat = preprocessorx[docbycat];
	vocabulary = Tally[Flatten[docbycat]];
	ntoken = Length[vocabulary];
	mincount = OptionValue["MinimumTokenCount"];
	If[mincount === Automatic,
		Which[
			ntoken > 1000000, mincount = 4,
			ntoken > 100000, mincount = 3,
			ntoken > 10000, mincount = 2,
			True, mincount = 1
		]
	];
	vocabulary = Select[vocabulary, #[[2]] >= mincount &];
	vocabulary = DeleteCases[vocabulary[[All, 1]], Alternatives @@ OptionValue["ExcludedTokens"]];
	ntoken = Length[vocabulary];
	vocabularyPosition = SortedHashArray[vocabulary, Range[ntoken]];
	wordproba = Transpose[wordLogProbability[#, vocabulary, alpha] & /@ docbycat];
	wordproba = Append[wordproba, Table[0., {Length @ cat}]];
	wordproba = ToPackedArray[wordproba, Real];
	If[OptionValue["UniformPrior"], catproba = 1./ Length[catproba]];
	evaluationdata = {{preprocessorx, Preprocessor["Identity"], {__String}}, {catproba, wordproba, vocabularyPosition}};
	metadata = {{Length[wordproba]-1, Length[Y], Length[catproba]}, {}};
	options = {FilterOptions[StringWords, Options[NaiveBayesText]]};
	options = DeleteDuplicates[Join[{opts}, options], First[#1] === First[#2] &];
	ClassifierFunction["NaiveBayesText", evaluationdata, metadata, options]
];

(*listableassociation[association_][key_] := association[key];
listableassociation[association_][keys_List] := association /@ keys;
*)
wordLogProbability[words_, vocabulary_, alpha_] := Module[
	{proba},
	proba = Tally[Join[vocabulary, words]][[;;Length[vocabulary], 2]];
	proba = proba + alpha - 1;
	proba /= N @ Total[proba];
	Log[proba]
];

iClassifierFunction["NaiveBayesText"][___][_, prop_] := AbortMachineLearning["naprop", prop];
iNoneClassifierFunction["NaiveBayesText"][___][prop_] := AbortMachineLearning["napropn", prop];
iNonePredictorFunction["NaiveBayesText"][___][] := AbortMachineLearning["napropnn"];

distributionvector[x_, {catproba_, wordproba_, vocabularyPosition_}]:=Module[
	{positions, proba},
	positions = vocabularyPosition /@ x;
	positions = Replace[positions, {{} -> -1, _Missing -> -1}, {2}];
	proba = Exp[Total[wordproba[[#]]] & /@ positions];
	proba = #*catproba & /@ proba;
	#/Total[#] & /@ proba
];

iClassifierFunction["NaiveBayesText"][eval_, meta_, _][x_, "DistributionList"] := With[
	{classes = meta[[1, 3, 2]]},
	Thread[classes -> #] & /@ distributionvector[x, eval[[2, ;;3]]]
];
iClassifierFunction["NaiveBayesText"][eval_, meta_, _][x_, "FullProbabilities"] := With[
	{classes = meta[[1, 3, 2]]},
	Association[Thread[classes -> #]] & /@ distributionvector[x, eval[[2, ;;3]]]
];

iClassifierFunction["NaiveBayesText"][eval_, _, _][x_] := Module[
	{preprocessory},
	preprocessory = eval[[1, 2]];
	preprocessory[
		MaxLoc[distributionvector[x, eval[[2, ;;3]]]],
		"Inverse"
	]
];

iClassifierFunction["NaiveBayesText"][eval_, meta_, _][x_, "Probabilities"] := With[
	{classes = meta[[1, 3, 2]]},
	MostProbableClasses[
		Thread[classes -> #] & /@ distributionvector[x, eval[[2, ;;3]]]
		, 0.1
	]
];

iClassifierFunction["NaiveBayesText"][eval_, _, _][x_, {"Probability", class_}] := Module[
	{distvec, classesindex, preprocessory},
	preprocessory = eval[[1, 2]];
	distvec = distributionvector[x, eval[[2, ;;3]]];
	classesindex = preprocessory[class];
	#[[classesindex]] & /@ distvec
];

iNoneClassifierFunction["NaiveBayesText"][_, _, options_]["Options"] := Sequence @@ options;