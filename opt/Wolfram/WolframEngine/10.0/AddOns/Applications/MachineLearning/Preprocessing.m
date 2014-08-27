(* ::Package:: *)

Package["MachineLearning`"]

PackageImport["Developer`"]

PackageScope["Preprocess"]

Preprocess::usage = "Performs classic data preprocessing to a data matrix (list of feature vectors).
Preprocess[data] outputs {datamod, preprocessor} where datamod is the modified data. 
preprocessor[newdata] performs the transformation. preprocessor[newdata, \"Inverse\"] performs the inverse transformation.
Elements of data have to be either Real values or Missing.
By default, it replaces Missing by the average of the corresponding feature, then it standardizes data.
Can also perform dimensionsality reduction."


PackageScope["Preprocessor"]

Preprocessor::usage = "General preprocessor object. 
	f = Preprocessor[\"type\", data] is a preprocessing function.
	f[data] preprocess. f[data, \"Inverse\"] does the inverse.
	Preprocessor[\"Sequence\", {preprocessors}] allows to compose preprocesses.
	Preprocessor[\"Threads\", {preprocessors}] allows to threads preprocesses.)"
	
	
PackageScope["DetectNominals"]

DetectNominals::usage = "DetectNominals[data] outputs the indices of columns containing nominal variables."


PackageScope["SimplifyPreprocessor"]

SimplifyPreprocessor::usage = "SimplifyPreprocessor[p] transforms p into a simpler equivalent Preprocessor. "


PackageScope["JoinPreprocessors"]

JoinPreprocessors::usage = "JoinPreprocessor[p1, p2] forms a \"Sequence\" preprocessor with p1 and p2."


PackageScope["PreprocessClassifierData"]

PreprocessClassifierData::usage = "PreprocessClassifierData[{featurevectors, labels}] performs automatic preprocessing of numerical and nominal data that will be used by a classifier.
Used by Classify and related functions."


PackageScope["PreprocessPredictorData"]

PreprocessPredictorData::usage = "PreprocessPredictorData[{featurevectors, values}] performs automatic preprocessing of numerical and nominal data that will be used by a predictor.
Used by Predict and related functions."


PackageScope["PreprocessClassifierImage"]

PreprocessClassifierImage::usage = "PreprocessClassifierImage[{images, labels}] performs automatic preprocessing of images that will be used by a classifier.
Used by Classify and related functions."


PackageScope["PreprocessPredictorImage"]

PreprocessPredictorImage::usage = "PreprocessPredictorImage[{images, value}] performs automatic preprocessing of images that will be used by a predictors.
Used by Predict and related functions."

PackageScope["ClassifierMetaData"]

ClassifierMetaData::usage = "ClassifierMetaData[{features, labels}] generates classifier metadata"

PackageScope["PredictorMetaData"]

PredictorMetaData::usage = "PredictorMetaData[{features, values}] generates predictor metadata"

PackageScope["CreatePreprocessor"]
CreatePreprocessor::usage = "CreatePreprocessor[data, name] generates Preprocessor[name, ...] for data."

(*JoinPreprocessors::usage = "JoinPreprocessors[preprocessor1, preprocessor2, ...] merge preprocessors into one preprocessor."*)

(***** Preprocess functions *****)

Options[Preprocess] = {
	"InputMissing" -> {"Numerical" -> "Mean", "Nominal" -> "Commonest"},
	"Preprocessing" -> "Standardize", (*only for numerical features.*)
	"NominalPreprocess" -> "IntegerEncode", (*IntegerEncode, BinaryEncode, ...*)
	NominalVariables -> Automatic
};
Options[iPreprocessMulti] = {
	"InputMissing" -> {"Numerical" -> "Mean", "Nominal" -> "Commonest"},
	"Preprocessing" -> "Standardize", (*only for numerical features.*)
	"NominalPreprocess" -> "IntegerEncode"
};

Preprocess[featurevectors:{__List}, opts:OptionsPattern[]] := Module[
	{m, n, nominalindices, numericalindices, preprocessor, optnum, optnom},
	{m, n} = Dimensions[featurevectors];
	nominalindices = OptionValue[NominalVariables];
	nominalindices = Switch[nominalindices,
		All, Range[n]
		,
		None, {}
		,
		{___Integer}, nominalindices
		,
		Automatic, DetectNominals[featurevectors]
		,
		_, AbortMachineLearning[]
	];
	numericalindices = Complement[Range[n], nominalindices];
	nominalindices = Sort[nominalindices];
	numericalindices = Sort[numericalindices];
	{optnum, optnom} = parseinputmissingopts[Sequence @@ OptionValue["InputMissing"]];
	preprocessor = Switch[Length[nominalindices],
		0,
			iPreprocessSingle[featurevectors, optnum, OptionValue["Preprocessing"]],
		n,
			iPreprocessSingle[featurevectors, optnom, OptionValue["NominalPreprocess"]],
		_,
			iPreprocessMulti[featurevectors, {nominalindices, numericalindices},
				FilterOptions[iPreprocessMulti, opts]
			]
	];
	SimplifyPreprocessor[preprocessor]
];

Preprocess[featurevectors:{__Image}, opts:OptionsPattern[]] := Module[
	{X, preprocessor, p},
	X = featurevectors;
	preprocessor = Map[
		(p = CreatePreprocessor[X, #];
		X = p[X];
		p) &
		,
		parsePreprocessingOptions[OptionValue["Preprocessing"]]
	];
	preprocessor = Preprocessor["Sequence", preprocessor];
	SimplifyPreprocessor[preprocessor]
];

Preprocess[featurevectors:{__String}, opts:OptionsPattern[]] := Module[
	{X, preprocessor, p},
	X = featurevectors;
	preprocessor = Map[
		(p = CreatePreprocessor[X, #];
		X = p[X];
		p) &
		,
		parsePreprocessingOptions[OptionValue["Preprocessing"]]
	];
	preprocessor = Preprocessor["Sequence", preprocessor];
	SimplifyPreprocessor[preprocessor]
];

parsePreprocessingOptions[preprocesses_List] := preprocesses;
parsePreprocessingOptions[{preprocess_, opts:__Rule}] := {{preprocess, opts}};
parsePreprocessingOptions[preprocess_] := {preprocess};

iPreprocessSingle[featurevectors_, inputopt_, preprocessopt_] := Module[
	{X, inputer, preprocessor, p},
	X = featurevectors;
	inputer = createinputer[X, inputopt];
	X = inputer[X];
	preprocessor = Map[
		(p = CreatePreprocessor[X, #];
		X = p[X];
		p) &
		,
		parsePreprocessingOptions[preprocessopt]
	];
	preprocessor = Preprocessor["Sequence", preprocessor];
	Preprocessor["Sequence", {inputer, preprocessor}]
];

iPreprocessMulti[featurevectors_, {nominalindices_, numericalindices_}, opts:OptionsPattern[]] := Module[
	{Xnum, Xnom, separator, inputer, preprocessor, opt, optnum, optnom,
		numericalprocessor, nominalprocessor, p, merger},
	separator = Preprocessor["SeparateNumerical&Nominals", numericalindices, nominalindices];
	{Xnum, Xnom} = separator[featurevectors];
	opt = OptionValue["InputMissing"];
	{optnum, optnom} = parseinputmissingopts[Sequence @@ opt];
	inputer = Preprocessor["Threads", {createinputer[Xnum, optnum], createinputer[Xnom, optnom]}];
	{Xnum, Xnom} = inputer[{Xnum, Xnom}]; (*Can be streamed to save memory...*)
	numericalprocessor = Map[
		(p = CreatePreprocessor[Xnum, #];
		Xnum = p[Xnum];
		p) &
		,
		parsePreprocessingOptions[OptionValue["Preprocessing"]]
	];
	numericalprocessor = Preprocessor["Sequence", numericalprocessor];
	nominalprocessor = Map[
		(p = CreatePreprocessor[Xnom, #];
		Xnom = p[Xnom];
		p) &
		,
		parsePreprocessingOptions[OptionValue["NominalPreprocess"]]
	];
	nominalprocessor = Preprocessor["Sequence", nominalprocessor];
	preprocessor = Preprocessor["Threads", {numericalprocessor, nominalprocessor}];
	merger = Preprocessor["MergeNumerical&Nominals", Length[numericalindices]];
	Preprocessor["Sequence", {separator, inputer, preprocessor, merger}]
];

DetectNominals[featurevectors_] := Module[
	{m, n, nominalq},
	{m, n} = Dimensions[featurevectors];
	nominalq = nominalQ[featurevectors[[All, #]]] & /@ Range[n];
	First /@ Position[nominalq, True]
];
nominalQ[features_] := Module[
	{nominal, feat, m, num},
	feat = DeleteCases[DeleteDuplicates[features], _Missing];
	m = Length[features];
	num = MatchQ[N[#], _Real] & /@ feat; (*should we handle complex numbers?*)
	(*is nominal if it has non-numeric values, or less than 10 only integer values. Change?*)
	nominal = Or[
		Not[Apply[And, num]],
		And[Length[feat]<10, Apply[And, # == 0 & /@ FractionalPart[feat]], m>20]
	];
(*	If[nominal && Length[feat]>63, 
		If[Count[num, True]/Length[feat]>0.9, AbortMachineLearning["bdnumfeat"]]; (*Probably an error in the input.*)
	];*)
	(*should set to nominal, even if there is an error...*)
	nominal
];

Options[parseinputmissingopts] = {
	"Numerical" -> "Mean",
	"Nominal" -> "Commonest"
};
parseinputmissingopts[opts:OptionsPattern[]] := {OptionValue["Numerical"], OptionValue["Nominal"]};
parseinputmissingopts[None] := {None, None};


(*** numerical and nominal data preprocessing ***)

Options[PreprocessClassifierData] = {
	"Preprocessing" -> Automatic,
	"InputMissing" -> Automatic,
	NominalVariables -> Automatic,
	"ClassLabels" -> Automatic
};
PreprocessClassifierData[{featurevectors_, labels_}, opts:OptionsPattern[]] := Module[
	{nominalindices, preprocessorx, preprocessory, datalabelnames, labelnames, binaryencoderx},
	{nominalindices, binaryencoderx, preprocessorx} = 
		iPreprocessData[featurevectors, FilterOptions[iPreprocessData, opts]];
	labelnames = OptionValue["ClassLabels"];
	datalabelnames = Sort[DeleteDuplicates[labels]];
	Switch[labelnames,
		Automatic, labelnames = datalabelnames
		,
		{__}, If[!And @@ (MemberQ[labelnames, #] & /@ datalabelnames), AbortMachineLearning[]]
		,
		_, AbortMachineLearning[]
	];
	preprocessory = CreatePreprocessor[labelnames, "IntegerEncodeList"];
	{nominalindices, binaryencoderx, preprocessorx, preprocessory}
];

Options[PreprocessPredictorData] = {
	"Preprocessing" -> Automatic,
	"InputMissing" -> Automatic,
	NominalVariables -> Automatic
};
PreprocessPredictorData[{featurevectors_, values_}, opts:OptionsPattern[]] := Module[
	{nominalindices, preprocessorx, binaryencoderx},
	{nominalindices, binaryencoderx, preprocessorx} = 
		iPreprocessData[featurevectors, FilterOptions[iPreprocessData, opts]];
(*	preprocessory = Preprocess[values,
		"InputMissing" -> None,
		"Preprocessing" -> "Standardize",
		NominalVariables -> None
	];*)
	{nominalindices, binaryencoderx, preprocessorx}
];

Options[iPreprocessData] = {
	"Preprocessing" -> Automatic,
	"InputMissing" -> Automatic,
	NominalVariables -> Automatic
};
iPreprocessData[featurevectors_, opts:OptionsPattern[]] := Module[
	{m, n, nominalindices, inputopt, preprocessopt, preprocessorx, X, binaryencoderx},
	{m, n} = Dimensions[featurevectors];
	nominalindices = OptionValue[NominalVariables];
	nominalindices = Switch[nominalindices,
		All, Range[n]
		,
		None, {}
		,
		{___Integer}, nominalindices
		,
		Automatic, DetectNominals[featurevectors]
		,
		_, AbortMachineLearning[]
	];
	inputopt = OptionValue["InputMissing"];
	If[inputopt === Automatic, inputopt = {"Numerical" -> "Mean", "Nominal" -> "Commonest"}];
	preprocessopt = OptionValue["Preprocessing"];
	If[preprocessopt === Automatic, preprocessopt = "Standardize"];
	preprocessorx = Preprocess[featurevectors,
		"InputMissing" -> inputopt,
		"Preprocessing" -> preprocessopt,
		"NominalPreprocess" -> "IntegerEncode",
		NominalVariables -> nominalindices
	];	
	X = preprocessorx[featurevectors];
	binaryencoderx = Preprocess[X,
		"InputMissing" -> None,
		"Preprocessing" -> None,
		"NominalPreprocess" -> "BinaryEncode",
		NominalVariables -> Sort[n + 1 - Range[Length[nominalindices]]]
	];
	{nominalindices, binaryencoderx, preprocessorx}
];
(*** End - numerical and nominal data preprocessing ***)

Options[ClassifierMetaData] = {
	"ClassLabels" -> Automatic,
	"ClassName" -> "class",
	"FeatureNames" -> Automatic,
	NominalVariables -> Automatic
};
ClassifierMetaData[{features_, labels_}, opts:OptionsPattern[]] := Module[
	{m, n, nclass, featuresmetadata, classmetadata},
	{m, n} = Dimensions[features];
	featuresmetadata = featureMetaData[features, FilterOptions[featureMetaData, opts]];
	classmetadata = {OptionValue["ClassName"], OptionValue["ClassLabels"]};
	nclass = Length[classmetadata[[2]]];
	{{m, n, nclass}, featuresmetadata, classmetadata}
];

ClassifierMetaData[{features:{__Image}, labels_}, opts:OptionsPattern[]] := Module[
	{m, n, nclass, featuresmetadata, classmetadata, featurenames},
	{m, n} = {Length[features], 1}; (*could use n = ImageChannels[#] * Times @@ ImageDimensions[#] & @ First[images]*)
	featurenames = OptionValue["FeatureNames"];
	If[featurenames === Automatic, featurenames = {"image1"}];
	featuresmetadata = {Flatten[{featurenames, "image"}]};
	classmetadata = {OptionValue["ClassName"], OptionValue["ClassLabels"]};
	nclass = Length[classmetadata[[2]]];
	{{m, n, nclass}, featuresmetadata, classmetadata}
];

ClassifierMetaData[{features:{__String}, labels_}, opts:OptionsPattern[]] := Module[
	{m, n, nclass, featuresmetadata, classmetadata, featurenames},
	{m, n} = {Length[features], 1}; 
	featurenames = OptionValue["FeatureNames"];
	If[featurenames === Automatic, featurenames = {"text1"}];
	featuresmetadata = {Flatten[{featurenames, "text"}]};
	classmetadata = {OptionValue["ClassName"], OptionValue["ClassLabels"]};
	nclass = Length[classmetadata[[2]]];
	{{m, n, nclass}, featuresmetadata, classmetadata}
];

Options[PredictorMetaData] = {
	"ValueName" -> "value",
	"FeatureNames" -> Automatic,
	NominalVariables -> Automatic
};
PredictorMetaData[{features_, values_}, opts:OptionsPattern[]] := Module[
	{dim, featuresmetadata, valuemetadata},
	dim = Dimensions[features];
	featuresmetadata = featureMetaData[features, FilterOptions[featureMetaData, opts]];
	valuemetadata = {OptionValue["ValueName"], {Min[values], Max[values]}};
	{dim, featuresmetadata, valuemetadata}
];

PredictorMetaData[{images:{__Image}, values_}, opts:OptionsPattern[]] := Module[
	{dim, featuresmetadata, valuemetadata, featurenames},
	dim = {Length[images], 1}; (*could use n = ImageChannels[#] * Times @@ ImageDimensions[#] & @ First[images]*)
	featurenames = OptionValue["FeatureNames"];
	If[featurenames === Automatic, featurenames = {"image1"}];
	featuresmetadata = {Flatten[{featurenames, "image"}]};
	valuemetadata = {OptionValue["ValueName"], {Min[values], Max[values]}};
	{dim, featuresmetadata, valuemetadata}
];

PredictorMetaData[{string:{__String}, values_}, opts:OptionsPattern[]] := Module[
	{dim, featuresmetadata, valuemetadata, featurenames},
	dim = {Length[string], 1}; (*number of features?*)
	featurenames = OptionValue["FeatureNames"];
	If[featurenames === Automatic, featurenames = {"text1"}];
	featuresmetadata = {Flatten[{featurenames, "text"}]};
	valuemetadata = {OptionValue["ValueName"], {Min[values], Max[values]}};
	{dim, featuresmetadata, valuemetadata}
];

Options[featureMetaData] = {
	"FeatureNames" -> Automatic,
	NominalVariables -> Automatic
};
featureMetaData[features_, opts:OptionsPattern[]] := Module[
	{m, n, Xt, nominalindices, numericalindices, labels, ranges, type, extra, featuresname},
	nominalindices = OptionValue[NominalVariables];
	{m, n} = Dimensions[features];
	Xt = Transpose[features];
	numericalindices = Complement[Range[n], nominalindices];
	labels = Sort[DeleteDuplicates[#]] & /@ Xt[[nominalindices]];
	ranges = {Min[#], Max[#]} & /@ (DeleteCases[#, _Missing] & /@ Xt[[numericalindices]]);
	type = Table["unknown", {n}];
	type[[numericalindices]] = "numerical";
	type[[nominalindices]] = "nominal";
	extra = Table[{}, {n}];
	extra[[numericalindices]] = ranges;
	extra[[nominalindices]] = labels;
	featuresname = OptionValue["FeatureNames"];
	If[featuresname === Automatic, featuresname = ("feature" <> ToString[#]) & /@ Range[n]];
	Transpose[{featuresname, type, extra}]
];
	
(*** Image preprocessing ***)

Options[PreprocessClassifierImage] = {
	"Preprocessing" -> Automatic,
	"ClassLabels" -> Automatic
};
PreprocessClassifierImage[{images_, labels_}, opts:OptionsPattern[]] := Module[
	{preprocessorx, preprocessory, labelnames},
	preprocessorx = iPreprocessImage[images, FilterOptions[iPreprocessImage, opts]];
	labelnames = OptionValue["ClassLabels"];
	If[labelnames === Automatic, labelnames = Sort[DeleteDuplicates[labels]]];	
	preprocessory = CreatePreprocessor[labelnames, "IntegerEncodeList"];
	{preprocessorx, preprocessory}
];

Options[PreprocessPredictorImage] = {
	"Preprocessing" -> Automatic
};
PreprocessPredictorImage[{images_, values_}, opts:OptionsPattern[]] := Module[
	{preprocessorx},
	preprocessorx = iPreprocessImage[images, FilterOptions[iPreprocessImage, opts]];
	preprocessorx
];
Options[iPreprocessImage] = {
	"Preprocessing" -> Automatic
};
iPreprocessImage[images_, opts:OptionsPattern[]] := Module[
	{preprocessorx},
	preprocessorx = Preprocess[images,
		"InputMissing" -> None,
		"Preprocessing" -> {"ImageConform", "ImagePixels", "GlobalStandardize", "ReduceDimension"},
		NominalVariables -> None
	];
	preprocessorx
];

CreatePreprocessor[images_, "ImagePixels"] := Module[
	{dim, chan, opts},
	dim = DeleteDuplicates[ImageDimensions /@ images];
	If[Length[dim]>1, AbortMachineLearning[];, dim = First[dim]];
	chan = DeleteDuplicates[ImageChannels /@ images];
	If[Length[chan]>1, AbortMachineLearning[];, chan = First[chan]];
	opts = DeleteDuplicates[Options /@ images];
	(*If[Length[opts]>1, AbortMachineLearning[];, opts = First[opts]];*)
	Preprocessor["ImagePixels", {dim, chan, opts}]
];
Preprocessor["ImagePixels", _][images:{__Image}] := Flatten[ImageData[#]] & /@ images;
Preprocessor["ImagePixels", _][image_Image] := Flatten[ImageData[image]];
Preprocessor["ImagePixels", {dim_, chan_, opts_}][data:{__List}, "Inverse"] :=
	Image[
		Partition[
			If[chan>1, Partition[#, chan], #], 
			First[dim]
		], Sequence @@ opts
	] & /@ data;

CreatePreprocessor[images_, "ImageConform"] := Module[
	{dimensions,colors,newDimension,noGrays,newColor},
	dimensions=Transpose[ImageDimensions[#]&/@images];
	colors=ImageChannels[#]&/@images;
	newDimension={Median[First[dimensions]],Median[Last[dimensions]]};	
	noGrays=Count[colors,1];
	newColor=If[noGrays>Length[images]-noGrays,"GrayScale","RGB"];
	Preprocessor["ImageConform", {newColor, newDimension}]
];
Preprocessor["ImageConform", arg_][image_, "Inverse"] := image;
Preprocessor["ImageConform", arg_][image_List] := Preprocessor["ImageConform", arg] /@ image;
Preprocessor["ImageConform", {newColor_, newDimension_}][image_] := Module[
	{newImage, imageColor, imageDim, newColorNum},
	If[TrueQ[newColor=="RGB"]
		,newColorNum=3
		,newColorNum=1
	];
	imageColor=ImageColorSpace[image];
	imageDim=ImageDimensions[image];
	If[TrueQ[imageColor==newColor]
		,newImage=image
		,newImage=ColorConvert[image,newColor]
	];
	If[ImageChannels[newImage]!=newColorNum
		,newImage=Image[ImageData[newImage][[;;,;;,;;newColorNum]],ColorSpace->newColor]
	];
	If[TrueQ[imageDim!=newDimension]
		,newImage=ImageResize[newImage, newDimension]
	];
	newImage
];


(*** End - image preprocessing ***)

(***** End - Preprocess function *****)

(**** general ****)

Format[Preprocessor[type_, ___], StandardForm] := type;

Preprocessor["Sequence", preprocessors_List][data_] := Composition[Sequence @@ Reverse[preprocessors]][data];
Preprocessor["Sequence", preprocessors_List][data_, "Inverse"] := Module[
	{functions},
	functions = Function[x, #[x, "Inverse"]] & /@ preprocessors;
	Composition[Sequence @@ functions][data]
];
Format[Preprocessor["Sequence", preprocessors_List], StandardForm] := 
	Framed[Row[preprocessors, " \[Rule] "], RoundingRadius -> 10, FrameStyle -> LightGray];

Preprocessor["Threads", preprocessors_List][data_] := MapThread[#1[#2] &, {preprocessors, data}];
Preprocessor["Threads", preprocessors_List][data_, "Inverse"] := MapThread[#1[#2, "Inverse"] &, {preprocessors, data}];
Format[Preprocessor["Threads", preprocessors_List], StandardForm] := 
	Framed[Column[preprocessors, Center], RoundingRadius -> 10, FrameStyle -> Gray];


JoinPreprocessors[preprocessors__] := SimplifyPreprocessor @ Preprocessor["Sequence", 
	{preprocessors}
];

(** Simplify Preprocessor**)

SimplifyPreprocessor[preprocessor_] := ReplaceRepeated[preprocessor, 
	{$deleteRule , $emptyRule, $mergeThreadsRule, $singleRule, $unnestRule, $mergeEncodersRule}
];

$inversepairs = {
	{"MergeNumerical&Nominals", "SeparateNumerical&Nominals"}, 
	{"ToMatrix", "ToVector"}
};

$deletePatterns = Alternatives[
	Preprocessor["Identity"]
	,
	Preprocessor["Pack"]
	,
	Preprocessor["Threads", {Preprocessor["Identity"]..}]
	,
	Apply[Sequence,
		PatternSequence[Preprocessor[#1, ___],  Preprocessor[#2, ___]] & @@@ 
			Join[$inversepairs, Reverse /@ $inversepairs]
	]
];

$deleteRule = Preprocessor["Sequence", {pa___, $deletePatterns, pb___}] :> 
	Preprocessor["Sequence", {pa, pb}];

$unnestRule = Preprocessor["Sequence", {pa___, Preprocessor["Sequence", p_], pb___}] :> 
	Preprocessor["Sequence", {pa, Sequence @@ p, pb}];

$singleRule = Preprocessor["Sequence", {p_}] :> p;

$emptyRule = Preprocessor["Sequence", {}] :> Preprocessor["Identity"];

$mergeThreadsRule = Preprocessor["Sequence", 
	{pa___, Preprocessor["Threads", p1_], Preprocessor["Threads", p2_], pb___}
] :> Preprocessor["Sequence", 
	{pa, Preprocessor["Threads", Preprocessor["Sequence", #] & /@ Transpose[{p1, p2}]], pb}];

$mergeEncodersRule = Preprocessor["Sequence", {
	pa___,
	PatternSequence[Preprocessor["IntegerEncode", pint_], Preprocessor["BinaryEncode", pbin_]],
	pb___
}] :> 
	Preprocessor["Sequence", {
	pa
	,
	Preprocessor["BinaryEncode",
		MapThread[Function[{integer, binary}, Preprocessor[
			"BinaryEncodeList",
			integer[binary[[2]], "Inverse"], 
			integer[#1, "Inverse"]-> #2 & @@@ binary[[3]]]], 
			{pint, pbin}
		]
	]
	,
	pb
	}
];

(** End - Simplify Preprocessor**)

(* Chop small numbers when array cannot be packed*)
ToPackedArrayForced[array_] := Module[
	{packedarray},
	packedarray = ToPackedArray[array];
	If[!PackedArrayQ[packedarray],
		packedarray = ToPackedArray[Chop[packedarray, $MinMachineNumber], Real]
	];
	packedarray
];
ToPackedArrayForced[array_, Real] := Module[
	{packedarray},
	packedarray = ToPackedArray[array, Real];
	If[!PackedArrayQ[packedarray],
		packedarray = ToPackedArray[Chop[packedarray, $MinMachineNumber], Real]
	];
	packedarray
];
ToPackedArrayForced[array_, Integer] := ToPackedArray[array, Integer];

Preprocessor["Pack"][data_] := ToPackedArrayForced[data, Real];
Preprocessor["Pack"][data_, "Inverse"] := data;

Preprocessor["Identity"][data_] := data;
Preprocessor["Identity"][data_, "Inverse"] := data;

Preprocessor["ToMatrix"][data_List] := Transpose[{data}];
Preprocessor["ToMatrix"][data:{__List}, "Inverse"] := First[Transpose[data]];
Preprocessor["ToMatrix"][data_] := {data};
Preprocessor["ToMatrix"][data_List, "Inverse"] := First[data];

Preprocessor["ToVector"][data_List] := First[data];
Preprocessor["ToVector"][data_, "Inverse"] := {data};
Preprocessor["ToVector"][data:{__List}] := First[Transpose[data]];
Preprocessor["ToVector"][data:_List, "Inverse"] := Transpose[{data}];

Preprocessor["PrependOne"][data_List] := Prepend[data, 1.];
Preprocessor["PrependOne"][data_List, "Inverse"] := Rest[data];
Preprocessor["PrependOne"][data:{__List}] := Module[
	{Xmod},
	Xmod = ConstantArray[1., Dimensions[data] + {0, 1}];
	Xmod[[All, 2 ;;]] = data;
	Xmod
];
Preprocessor["PrependOne"][data:{__List}, "Inverse"] := data[[All, 2;;]];

(**** End - general ****)

(**** separation & recombining ****)

Preprocessor["SeparateNumerical&Nominals", numericalindices_, nominalindices_][data_List] := {data[[numericalindices]], data[[nominalindices]]};
Preprocessor["SeparateNumerical&Nominals", numericalindices_, nominalindices_][data:{_List, _List}, "Inverse"] := Module[
	{data2},
	data2 = Table[0, {Length[numericalindices] + Length[nominalindices]}];
	data2[[numericalindices]] = First[data];
	data2[[nominalindices]] = Last[data];
	data2
];
Preprocessor["SeparateNumerical&Nominals", numericalindices_, nominalindices_][data:{__List}] := {data[[All, numericalindices]], data[[All, nominalindices]]};
Preprocessor["SeparateNumerical&Nominals", numericalindices_, nominalindices_][data:{{__List}, {__List}}, "Inverse"] := Module[
	{data2},
	data2 = Table[0, {Length[First[data]]}, {Length[numericalindices]+Length[nominalindices]}];
	data2[[All, numericalindices]] = First[data];
	data2[[All, nominalindices]] = Last[data];
	data2
];
Preprocessor["MergeNumerical&Nominals", nnum_][data:{_List, _List}] := Join @@ data;
Preprocessor["MergeNumerical&Nominals", nnum_][data:_List, "Inverse"] := {data[[;;nnum]], data[[nnum+1;;]]};
Preprocessor["MergeNumerical&Nominals", nnum_][data:{{__List}, {__List}}] := MapThread[Join, data];
Preprocessor["MergeNumerical&Nominals", nnum_][data:{__List}, "Inverse"] := {data[[All, ;;nnum]], data[[All, nnum+1;;]]};
(*check if it keeps packing*)

(**** End - separation & recombining ****)


(******** Input Missing ********)

Preprocessor["InputMissing", generators_][data_List] := Module[
	{pos, listcompleted},
	If[Or[PackedArrayQ[data], !MemberQ[data, _Missing]], Return[data]];
	pos = Position[data, _Missing, {1}][[All, 1]];
	listcompleted = data;
	listcompleted[[pos]] = generators[[pos, 1]];
	listcompleted
];
(*can do more efficient... (to speed up evaluation, see later if it is necessary).*)
Preprocessor["InputMissing", generators_][data:{__List}] := If[
	And[!PackedArrayQ[data], MemberQ[data, _Missing, {2}]]
	,
	Preprocessor["Pack"][
		Preprocessor["InputMissing", generators] /@ data
	]	
	,
	data
];
Preprocessor["InputMissing", _][data_, "Inverse"] := data;


createinputer[X_, None] := Preprocessor["Identity"];
createinputer[X_, "Discard"] := Preprocessor["DiscardMissing"];
createinputer[X_, indices_, "Discard"] := Preprocessor["DiscardMissing", indices];
Preprocessor["DiscardMissing"][data:{__List}] := If[
	And[!PackedArrayQ[data], MemberQ[data, _Missing, {2}]],
	Select[data, !MemberQ[#, _Missing, {1}] &]
];
Preprocessor["DiscardMissing"][data_List] := If[MemberQ[data, _Missing, {1}], Null, data];
Preprocessor["DiscardMissing"][data_, "Inverse"] := data;
Preprocessor["DiscardMissing", indices_][data:{__List}] := If[
	And[!PackedArrayQ[data], MemberQ[data, _Missing, {2}]],
	Select[data, !MemberQ[#[[indices]], _Missing, {1}] &]
];
Preprocessor["DiscardMissing", indices_][data_List] := If[MemberQ[data[[indices]], _Missing, {1}], Null, data];
Preprocessor["DiscardMissing", indices_][data_, "Inverse"] := data;

createinputer[X_, "Mean"] := Module[
	{means},
	means = Mean[DeleteCases[X[[All, #]], _Missing]] & /@ Range[Last[Dimensions[X]]];
	Preprocessor["InputMissing", List /@ means]
];
createinputer[X_, "Median"] := Module[
	{medians},
	medians = Median[DeleteCases[X[[All, #]], _Missing]] & /@ Range[Last[Dimensions[X]]];
	Preprocessor["InputMissing", List /@ medians]
];
createinputer[X_, "Commonest"] := Module[
	{mostf},
	mostf = SortBy[Tally[DeleteCases[X[[All, #]], _Missing]], Last][[-1, 1]] & /@ Range[Last[Dimensions[X]]];
	Preprocessor["InputMissing", List /@ mostf]
];
createinputer[X_, "NormalSample"] := Module[
	{nonmissing, means, stddevs, distributions},
	nonmissing = DeleteCases[X[[All, #]], _Missing] & /@ Range[Last[Dimensions[X]]];
	means = Mean /@ nonmissing;
	stddevs = StandardDeviation /@ nonmissing;
	stddevs = stddevs + $MinMachineNumber;
	distributions = NormalDistribution @@@ Transpose[{means, stddevs}];
	Preprocessor["InputMissing", Hold[RandomVariate[#]] & /@ distributions]
];
createinputer[X_, "Sample"] := Module[
	{nonmissing, counts},
	nonmissing = DeleteCases[X[[All, #]], _Missing] & /@ Range[Last[Dimensions[X]]];
	counts = Reverse[Rule @@ Transpose[Tally[#]]] & /@ nonmissing;
	Preprocessor["InputMissing", Hold[RandomChoice[#]] & /@ counts]
];
(********** End - Input Missing **********)


(********** Preprocessing **********)

(***** Numerical *****)

CreatePreprocessor[X_, None] := Preprocessor["Identity"];

CreatePreprocessor[X_, "Pack"] := Preprocessor["Pack"];

CreatePreprocessor[X_, "MeanSubstract"] := Preprocessor["MeanSubstract", FastMeanColumn[X]]
Preprocessor["MeanSubstract", means_][data_] := Quiet[
	iMeanSubstract[means, data],
	{CompiledFunction::cfta} (*In case of non-machine numbers.*)
];
Preprocessor["MeanSubstract", means_][data_, "Inverse"] := Preprocessor["MeanSubstract", -means][data];

iMeanSubstract = MachineLearning`Libraries`Compile[{{means, _Real, 1}, {data, _Real, 1}},
	data - means,
	RuntimeAttributes -> {Listable},
	Parallelization -> True
];

CreatePreprocessor[X_, "Standardize"] := Module[
	{means, stddevs},
	means = FastMeanColumn[X];
	stddevs = FastStandardDeviation[X];
	stddevs = If[#1>#2, #1, 1.] & @@@ 
		Transpose[{stddevs, Length[X]*10.^(Last /@ MantissaExponent[means] - $MachinePrecision)}];
		(*set stddev to 1 for column of constant elements.*)
	(*stddevs = stddevs + $MinMachineNumber*10.^10; (* very mild regularization. *)*)
	Preprocessor["Standardize", {means, stddevs}]
];
Preprocessor["Standardize", {means_, stddevs_}][data_] := Quiet[
	iStandardize[means, stddevs, data], 
	{CompiledFunction::cfta}
];
iStandardize = MachineLearning`Libraries`Compile[{{means, _Real, 1}, {stddevs, _Real, 1}, {data, _Real, 1}},
	(data - means)/stddevs,
	RuntimeAttributes -> {Listable},
	Parallelization -> True
];
Preprocessor["Standardize", {means_, stddevs_}][data_, "Inverse"] := Quiet[
	iStandardizeInv[means, stddevs, data],
	{CompiledFunction::cfta}
];
iStandardizeInv = MachineLearning`Libraries`Compile[{{means, _Real, 1}, {stddevs, _Real, 1}, {data, _Real, 1}},
	data*stddevs + means,
	RuntimeAttributes->{Listable},
	Parallelization->True
];

CreatePreprocessor[X_, "GlobalStandardize"] := Module[
	{mean, stddev},
	mean = Mean[FastMeanColumn[X]];
	stddev = Sqrt[Mean[FastStandardDeviation[X]^2]];
	Preprocessor["GlobalStandardize", {mean, stddev}]
];
Preprocessor["GlobalStandardize", {mean_, stddev_}][data_] := Quiet[
	iGlobalStandardize[mean, stddev, data], 
	{CompiledFunction::cfta}
];
iGlobalStandardize = MachineLearning`Libraries`Compile[{{mean, _Real}, {stddev, _Real}, {data, _Real, 1}},
	(data - mean)/stddev,
	RuntimeAttributes -> {Listable},
	Parallelization -> True
];
Preprocessor["GlobalStandardize", {mean_, stddev_}][data_, "Inverse"] := Quiet[
	iGlobalStandardizeInv[mean, stddev, data], 
	{CompiledFunction::cfta}
];
iGlobalStandardizeInv = MachineLearning`Libraries`Compile[{{mean, _Real}, {stddev, _Real}, {data, _Real, 1}},
	data*stddev + mean,
	RuntimeAttributes->{Listable},
	Parallelization->True
];

CreatePreprocessor[X_, "RangeScale"] := CreatePreprocessor[X, "RangeScale" -> {0, 1}];
CreatePreprocessor[X_, "RangeScale" -> {xmin_, xmax_}] := Module[
	{mins, maxs, Xt},
	Xt = Transpose[X];
	mins = Min /@ Xt;
	maxs = Max /@ Xt;
	Preprocessor["RangeScale", Transpose[{mins, maxs}], {xmin, xmax}]
];
Preprocessor["RangeScale", ranges_, targetrange_][data_List] := Rescale[##, targetrange] & @@@ Transpose[{data, ranges}];
Preprocessor["RangeScale", ranges_, targetrange_][data:{__List}] := Transpose[Rescale[##, targetrange] & @@@ Transpose[{Transpose[data], ranges}]];
Preprocessor["RangeScale", ranges_, targetrange_][data_, "Inverse"] := Rescale[#1, targetrange, #2] & @@@ Transpose[{data, ranges}];
Preprocessor["RangeScale", ranges_, targetrange_][data:{__List}, "Inverse"] := Transpose[Rescale[#1, targetrange, #2] & @@@ Transpose[{Transpose[data], ranges}]];

CreatePreprocessor[X_, "Decorrelate"] := Module[
	{means, w, stddevs, transformation, inverse},
	{means, w, stddevs} = idecorrelation[X];
	transformation = w;
	inverse = Transpose[w];
	Preprocessor["LinearTransformation", means, transformation, inverse]
];
CreatePreprocessor[X_, "Whiten"] := Module[
	{means, w, stddevs, transformation, inverse},
	{means, w, stddevs} = idecorrelation[X];
	transformation = Transpose[Transpose[w]/stddevs];
	inverse = Inverse[transformation];
	Preprocessor["LinearTransformation", means, transformation, inverse]
];
CreatePreprocessor[X_, "ZCAWhiten"] := Module[
	{means, w, stddevs, transformation, inverse},
	{means, w, stddevs} = idecorrelation[X];
	transformation = w.(Transpose[w]/stddevs);
	inverse = Inverse[transformation];
	Preprocessor["LinearTransformation", means, transformation, inverse]
];
idecorrelation[X_] := Module[
	{Xmod, means, v, w, stddevs},
	means = FastMeanColumn[X];
	Xmod = # - means & /@ X;
	{v, w} = Rest @ SingularValueDecomposition[Xmod, Min[Dimensions[Xmod]]];
	stddevs = Diagonal[v]/Sqrt[Length[X]-1.];
	(*stddevs = stddevs + $MinMachineNumber*10.^10; (* very mild regularization? *)*)
	stddevs = Replace[stddevs, 0. -> 1., {1}];
	{means, w, stddevs}
];

Preprocessor["LinearTransformation", means_, transformation_, inverse_][data_] := iMeanSubstract[means, data].transformation;
Preprocessor["LinearTransformation", means_, transformation_, inverse_][data_, "Inverse"] := iMeanSubstract[-means, data.inverse];

CreatePreprocessor[X_, "ReduceDimension"] := Last[DimensionReduce[X, Tolerance -> 0.1]];
CreatePreprocessor[X_, "ReduceDimension"-> {opts__}] := Last[DimensionReduce[X, opts]];

(***** End - Numerical *****)

(***** Nominal *****)

CreatePreprocessor[Xnom_, "BinaryEncode"] := Module[
	{encoders, labelnames, rules},
	labelnames = DeleteDuplicates /@ Transpose[Xnom];
	labelnames = Sort /@ labelnames;
	rules = Thread[Rule[#, N@Range[Length[#]]]] & /@ labelnames;
	encoders = Preprocessor["BinaryEncodeList", #1, #2] & @@@ Transpose[{labelnames, rules}];
	Preprocessor["BinaryEncode", encoders]
];

CreatePreprocessor[Xnom_, "IntegerEncode"] := Module[
	{encoders, labelnames, rules},
	labelnames = DeleteDuplicates /@ Transpose[Xnom];
	labelnames = Sort /@ labelnames;
	rules = Thread[Rule[#, N@Range[Length[#]]]] & /@ labelnames;
	encoders = Preprocessor["IntegerEncodeList", #1, #2] & @@@ Transpose[{labelnames, rules}];
	Preprocessor["IntegerEncode", encoders]
];
CreatePreprocessor[list_, "IntegerEncodeList"] := Module[
	{labelnames, rules},
	labelnames = DeleteDuplicates[list];
	rules = Thread[Rule[#, N@Range[Length[#]]]] & @ labelnames;
	Preprocessor["IntegerEncodeList", labelnames, rules]
];

Preprocessor["BinaryEncodeList", labelnames_, rule_][label_] := Module[
	{code, index},
	code = ConstantArray[0., Length[labelnames]];
	index = Replace[label, Append[rule, _ -> 0]];
	If[index != 0, code[[index]] = 1.];
	code
];

Preprocessor["BinaryEncodeList", labelnames_, rule_][labels_List] := Module[
	{indices, nlabel, identity},
	indices = Replace[labels, Append[rule, _ -> 0], {1}] + 1;
	nlabel = Length[labelnames];
	identity = Prepend[IdentityMatrix[nlabel], ConstantArray[0., nlabel]];
	N@identity[[indices]]
];

Preprocessor["BinaryEncodeList", labelnames_, rule_][labels:{__Slot}] := (
	(Function[x, Boole[# == x]] /@ labelnames) & /@ labels);

Preprocessor["BinaryEncodeList", labelnames_, rule_][label_Slot] :=
	(Function[x, Boole[label == x]] /@ labelnames);

Preprocessor["BinaryEncodeList", arg__][code_List, "Inverse"] := First[Preprocessor["BinaryEncodeList", arg][{code}, "Inverse"]];
Preprocessor["BinaryEncodeList", labelnames_, rule_][codes:{__List}, "Inverse"] := Module[
	{indices, nlabel},
	nlabel = Length[labelnames];
	indices = codes.Range[nlabel];
	labelnames[[indices]]
];

Preprocessor["BinaryEncode", encoders_][data:{__List}] := Preprocessor["Pack"][
	Join @@@ Transpose[MapThread[#1[#2] &, {encoders, Transpose[data]}]]
];
Preprocessor["BinaryEncode", encoders_][data_List] := 
	Join @@ MapThread[#1[#2] &, {encoders, data}];
Preprocessor["BinaryEncode", encoders_][data:{__List}, "Inverse"] := Module[
	{nominalspans},
	nominalspans = Accumulate[Length[#[[2]]] & /@ encoders];
	nominalspans = MapThread[Span, {Prepend[Most[nominalspans+1], 1], nominalspans}];
	Transpose @ MapThread[#1[data[[All, #2]], "Inverse"] &, {encoders, nominalspans}]
];
Preprocessor["BinaryEncode", encoders_][data_List, "Inverse"] := Module[
	{nominalspans},
	nominalspans = Accumulate[Length[#[[2]]] & /@ encoders];
	nominalspans = MapThread[Span, {Prepend[Most[nominalspans+1], 1], nominalspans}];
	MapThread[#1[data[[#2]], "Inverse"] &, {encoders, nominalspans}]
];

Preprocessor["IntegerEncodeList", labelnames_, _][indices_, "Inverse"]:= labelnames[[indices]];
Preprocessor["IntegerEncodeList", _, rule_][labels_]:= Replace[labels, Append[rule, _ -> 0]];
Preprocessor["IntegerEncodeList", _, rule_][labels_List]:= Replace[labels, Append[rule, _ -> 0], {1}];

(*Preprocessor["IntegerEncodeList", _, rule_][label_Slot]:= Total[Function[{x, y},
	y*Boole[label == x]] @@@ rule];*)

Preprocessor["IntegerEncode", encoders_][data:{__List}] := Preprocessor["Pack"][
	Transpose[MapThread[#1[#2] &, {encoders, Transpose[data]}]]
];
Preprocessor["IntegerEncode", encoders_][data_List] := 
	MapThread[#1[#2] &, {encoders, data}];
Preprocessor["IntegerEncode", encoders_][data:{__List}, "Inverse"] := 
	Transpose[MapThread[#1[#2, "Inverse"] &, {encoders, Transpose[data]}]];
Preprocessor["IntegerEncode", encoders_][data_List, "Inverse"] := 
	MapThread[#1[#2, "Inverse"] &, {encoders, data}];

(***** End - Nominal *****)

(********** End - Preprocessing **********)