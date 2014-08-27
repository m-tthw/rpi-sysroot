(* ::Package:: *)

Package["MachineLearning`BuiltInModels`Language`"]

PackageExport["System`languageDetect"]

languageDetect::usage = "detect language"


$dir = "~/Checkout/Alpha/Source/DataScience/Stats/BuiltInModels/LanguageResources/"

(* tree creation *)
$subLanguages = Association[
	"Natural" -> {
		"LatinScript", "Cyrillic", "ArabicScript", "CJK", "Devanagari",
		"Amharic", "Armenian", "Georgian", "Greek", "Hebrew", "Khmer", "Thai", "Bengali",
		"Punjabi", "Gujarati", "Malayalam", "Kannada", "Tamil", "Telugu", "Sinhala"
	},
	"LatinScript" -> {
		"Afrikaans", "Albanian", "Azeri", "Catalan", "Croatian", "Czech",
		"Danish", "Dutch", "English", "Estonian", "Finnish", "French", "German", "Greenlandic",
		"Hungarian", "Icelandic", "Malay", "Italian", "Latvian", "Lithuanian", "NorwegianBokmal",
		"Polish", "Portuguese", "Romanian", "SerbianLatin", "Bosnian", "Slovak", "Slovenian",
		"Swahili", "Swedish", "Tagalog", "Turkish", "UzbekLatin", "Vietnamese", "Welsh",
		"Spanish", "Basque", "Esperanto"
	},
	"Cyrillic" -> {"Kazakh", "Macedonian", "Mongolian", "Russian", "Ukrainian", "SerbianCyrillic", "Bulgarian"},
	"ArabicScript" -> {"Arabic", "Persian", "Urdu"},
	"CJK" -> {"Chinese", "Japanese", "Korean"},
	"Devanagari" -> {"Hindi", "Nepali"}(*,
	"Chinese" -> {"ChineseSimplified", "ChineseTraditional"},
	"Malay" -> {"Indonesian", "Malaysian"},
	"Portuguese" -> {"PortugueseBrazil", "PortuguesePortugal"},
	"Spanish" -> {"SpanishSpain", "SpanishSouthAmerica"}*)
];
$leaves = {"Afrikaans", "Albanian", "Amharic", "Arabic", "Armenian", 
	"Azeri", "Basque", "Bengali", "Bosnian", "Bulgarian", "Catalan", 
	"Chinese", "Croatian", "Czech", "Danish", "Dutch", "English", 
	"Esperanto", "Estonian", "Finnish", "French", "Georgian", "German", 
	"Greek", "Greenlandic", "Gujarati", "Hebrew", "Hindi", "Hungarian", 
	"Icelandic", "Italian", "Japanese", "Kannada", "Kazakh", "Khmer", 
	"Korean", "Latvian", "Lithuanian", "Macedonian", "Malay", 
	"Malayalam", "Mongolian", "Nepali", "NorwegianBokmal", "Persian", 
	"Polish", "Portuguese", "Punjabi", "Romanian", "Russian", 
	"SerbianCyrillic", "SerbianLatin", "Sinhala", "Slovak", "Slovenian",
	"Spanish", "Swahili", "Swedish", 
	"Tagalog", "Tamil", "Telugu", "Thai", "Turkish", "Ukrainian", 
	"Urdu", "UzbekLatin", "Vietnamese", "Welsh"
};

buildlanguagetree[leaves_] := Module[
	{lmin, ncomon, ancesters, nearestancesters, root},
	ancesters = Flatten[getparents[#]][[2 ;; -2]] & /@ leaves;
	If[Or@@(MemberQ[Flatten[ancesters], #] & /@ leaves), Return[$Failed]];
	lmin = Min[Length /@ ancesters];
	ncomon = Count[SameQ @@@ Transpose[ancesters[[All, -lmin;; ]]], True];
	root = ancesters[[1, -ncomon]];
	nearestancesters = DeleteDuplicates[Flatten[ancesters[[All, ;;-ncomon]]]];
	addbranch[root, nearestancesters]
];
addbranch[name_, p_] :=  With[
	{children = $subLanguages[name]},
	{name, Length[children], addbranch[#, p] & /@ Select[children, MemberQ[p, #] &]}
];
$hypLanguages = Association[Flatten[Thread /@ Reverse /@ Normal[$subLanguages]]];
getparents[child_] := {child, getparents[$hypLanguages[child]]};
getparents[child_Missing] := Null;
$languagetree = buildlanguagetree[$leaves];

findlanguages[name_, leaves_] :=  If[MemberQ[leaves, #], #, findlanguages[#, leaves]] & /@ $subLanguages[name];
$languages = Flatten[findlanguages[First[$languagetree], $leaves]];

$priors = Association[{"English" -> 7.60077, "Spanish" -> 4.87923, "Chinese" -> 4.20279, 
"Portuguese" -> 2.48591, "Russian" -> 2.25549, "German" -> 1.93381, 
"Arabic" -> 1.89909, "Japanese" -> 1.7491, "French" -> 1.72681, 
"Italian" -> 1.43399, "Turkish" -> 1.16381, "Vietnamese" -> 1.11823, 
"Polish" -> 1.09598, "Persian" -> 1.09368, "Korean" -> 0.978244, 
"Dutch" -> 0.897532, "Ukrainian" -> 0.833055, "Romanian" -> 0.820846, 
"Thai" -> 0.783899, "Malay" -> 0.775828, "Hindi" -> 0.769916, 
"Hungarian" -> 0.709044, "Czech" -> 0.67116, "Greek" -> 0.670812, 
"Swedish" -> 0.667217, "Tamil" -> 0.647253, "Bengali" -> 0.644361, 
"Bulgarian" -> 0.642237, "Azeri" -> 0.631578, 
"Malayalam" -> 0.624349, "Bosnian" -> 0.615532, "Danish" -> 0.609066, 
"Tagalog" -> 0.608353, "Hebrew" -> 0.605876, 
"NorwegianBokmal" -> 0.605217, "Finnish" -> 0.603008, 
"Catalan" -> 0.594933, "SerbianLatin" -> 0.591046, 
"SerbianCyrillic" -> 0.591046, "Croatian" -> 0.588594, 
"Slovenian" -> 0.587811, "Telugu" -> 0.587104, 
"Albanian" -> 0.584212, "Slovak" -> 0.582832, 
"Esperanto" -> 0.581137, "Afrikaans" -> 0.580884, 
"Lithuanian" -> 0.576121, "Urdu" -> 0.573945, "Estonian" -> 0.571867,
"Georgian" -> 0.567695, "Basque" -> 0.563132, 
"Macedonian" -> 0.562509, "Latvian" -> 0.559376, 
"Kazakh" -> 0.557691, "Icelandic" -> 0.55373, "Armenian" -> 0.549381,
 "Mongolian" -> 0.547423, "Gujarati" -> 0.54659, 
"Kannada" -> 0.546041, "Welsh" -> 0.545968, "Swahili" -> 0.542369, 
"Sinhala" -> 0.539142, "Amharic" -> 0.537981, 
"UzbekLatin" -> 0.537571, "Nepali" -> 0.535367, "Khmer" -> 0.531255, 
"Punjabi" -> 0.518708, "Greenlandic" -> 0.513472}];
$orderedpriors = $priors /@ $languages;

(* end - tree creation *)

(* common functions *)
loadNGramData[directory_, name_, n_] := Module[
	{table, array},
	table = BinaryReadList[directory <> name <> "_Table.bin", "UnsignedInteger16"];
	array = BinaryReadList[directory <> name <> "_Array.bin", "Real32"];
	array = Nest[Partition[#, Max[table]] &, array, n];
	array[[All, Sequence @@ Table[1, {n}] ]] = 0.;
	array = Transpose[array, RotateRight[Range[ArrayDepth[array]]]];
	{table, n, array}
];
fasttotal = Compile[{{x, _Real, 2}}, Total[x]];
inferNGramProbabilities[text_, {table_, n_, arrays_}] := Module[
	{code, p},
	code = Append[ngrams[text, table, n], Table[1, {n}]];
	p = Exp[fasttotal[Extract[arrays, code]]];
	p /= Total[p];
	Chop[p, $MinMachineNumber]
];
ngrams[text_, table_, n_] := Module[
	{code},
	code = ToCharacterCode[text]+1;
	code = table[[code]];
	Partition[code, n, 1]
];
MaxLoc = Compile[{{list, _Real, 1}},
	Module[
		{extr, newval, loc},
		extr = First[list];
		loc = 1;
		Do[
			newval = list[[iter]];
			If[newval > extr, 
				extr = newval;
				loc = iter
			];
			, {iter, 2, Length[list]}
		];
		loc
	]
	, RuntimeAttributes -> {Listable}, Parallelization -> True, CompilationTarget -> "C"
];
(* end - comon functions*)

(* internal classifiers *)
$Natural = loadNGramData[$dir, "Natural", 1];
ilanguageClassifier["Natural"][text_] := inferNGramProbabilities[text, $Natural];

$LatinScript1 = loadNGramData[$dir, "LatinScript1", 1];
$LatinScript3 = loadNGramData[$dir, "LatinScript3", 3];
ilanguageClassifier["LatinScript"][text_] := (
	0.5 * inferNGramProbabilities[text, $LatinScript1]
	+ 0.5 * inferNGramProbabilities[text, $LatinScript3]);

$Cyrillic2 = loadNGramData[$dir, "Cyrillic2", 2];
$Cyrillic3 = loadNGramData[$dir, "Cyrillic3", 3];
ilanguageClassifier["Cyrillic"][text_] := (
	0.5*inferNGramProbabilities[text, $Cyrillic2]
	+ 0.5* inferNGramProbabilities[text, $Cyrillic3]);

$ArabicScript = loadNGramData[$dir, "ArabicScript", 1];
ilanguageClassifier["ArabicScript"][text_] := inferNGramProbabilities[text, $ArabicScript];

$CJK = loadNGramData[$dir, "CJK", 1];
ilanguageClassifier["CJK"][text_] := inferNGramProbabilities[text, $CJK];

$Devanagari = loadNGramData[$dir, "Devanagari2", 2];
ilanguageClassifier["Devanagari"][text_] := inferNGramProbabilities[text, $Devanagari];
(* end - internal classifiers *)

(* classifier *)
textsample[text_, samplenumber_, samplelength_] := Module[
	{code, ranges, samples},
	code = ToCharacterCode[text];
	ranges = {1, samplelength} + # & /@ RandomInteger[Length[code]- samplelength, samplenumber];
	samples = FromCharacterCode[Take[code, #] & /@ ranges];
	StringJoin[Riffle[samples, "\n"]]
];

lucky[text_, {name_, n_, sub_}, prior_] := Module[
	{p, loc, pmax},
	p = ilanguageClassifier[name][text];
	loc = MaxLoc[p];
	pmax = p[[loc]];
	If[loc <= Length[sub],
		lucky[text, sub[[loc]], pmax],
		{name, loc, pmax*prior}
	]
];
lucky[text_, {name_, n_, {}}, prior_] := Module[
	{p, loc, pmax},
	p = ilanguageClassifier[name][text];
	loc = MaxLoc[p];
	pmax = p[[loc]];
	{name, loc, pmax*prior}
];

proba[text_, {name_, n_Integer, sub_}, True] := Module[
	{ps},
	ps = ilanguageClassifier[name][text];
	MapIndexed[
		Set[ps[[Sequence @@ #2]], ps[[Sequence @@ #2]] * proba[text, #1, If[ps[[Sequence @@ #2]] > 1./Length[$languages], True, False]]]
		&, sub
	];
	ps
];
proba[text_, {name_, n_Integer, {}}, True] := ilanguageClassifier[name][text];
proba[text_, {name_, n_Integer, sub_}, False] := (proba[text, #, False] & /@ sub)/n;
proba[text_, {name_, n_Integer, {}}, False] := ConstantArray[1./n, {n}];

(*languageDetect[text_] := Module[
	{g, loc, p},
	{g, loc, p} = If[StringLength[text]>2000,
		lucky[textsample[text, 10, 100], $languagetree, 1]
		,
		lucky[text, $languagetree, 1.]
	];
	$subLanguages[[g, loc]]
];
*)
languageDetect[text_] := Module[
	{p, maxloc},
	p = Flatten[If[StringLength[text]>2000,
		proba[textsample[text, 10, 100], $languagetree, True]
		,
		proba[text, $languagetree, True]
	]];
	p *= $orderedpriors;
	maxloc = MaxLoc[Chop[p, $MinMachineNumber]];
	$languages[[maxloc]]
];

languageDetect[text_, "DistributionList"] := Module[
	{p},
	p = Flatten[If[StringLength[text]>2000,
		proba[textsample[text, 10, 100], $languagetree, True]
		,
		proba[text, $languagetree, True]
	]];
	p *= $orderedpriors;
	p /= Total[p];
	Association @@ Thread[$languages -> p]
];

languageDetect[text_, "Probabilities"] := Module[
	{p, dist, max},
	p = Flatten[If[StringLength[text]>2000,
		proba[textsample[text, 10, 100], $languagetree, True]
		,
		proba[text, $languagetree, True]
	]];
	p *= $orderedpriors;
	p /= Total[p];
	max = Max[p];
	dist = Select[Thread[$languages -> p], #[[2]] > 0.1*max &];
	Reverse[SortBy[dist, Last]]
];

(* end - classifier *)



















