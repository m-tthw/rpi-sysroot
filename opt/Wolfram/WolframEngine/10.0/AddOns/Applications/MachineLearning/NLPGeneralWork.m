Package["MachineLearning`NLPGeneralWork`"]

PackageImport["Developer`"]

StringWords::usage = "StringWords[string] splits a string up into words";
StringTokens::usage = "StringTokens[string] tokenize string into characters, words, or sentences.";
RemoveDiacritic::usage = "RemoveDiacritic[string] converts Latin characters of string into the closest characters of the classic alphabet {a, ..., z, A, ..., Z}. To be extended to Cyrillic."
ToLowerCaseUnicode::usage = "Extends ToLowerCase to all Unicode Characters";
ToUpperCaseUnicode::usage = "Extends ToUpperCase to all Unicode Characters";


PackageExport["System`StringWords"]
PackageExport["System`StringTokens"]
PackageExport["System`StringTokenCounts"]
PackageExport["System`RemoveDiacritic"]
PackageExport["System`ToLowerCaseUnicode"]
PackageExport["System`ToUpperCaseUnicode"]

Options[StringWords] := {
	"LowerCase" -> False,
	(*"Stem" -> False,*)
	"RemoveDiacritic" -> False,
	"Punctuation" -> False,
	"Emoticons" -> False,
	"CapitalizedPhrases" -> False,
	"URLStem"-> False,
	"SpecialTokens"-> {}
};

$shortpunctuations = Join[
	Characters[",@#$%^&*()_-+={[}]:;\"\|\\<>/~`"],
	Characters[".!?"]
];

$shortcapitals = CharacterRange["A","Z"];

$positiveEmoticons = {":)",":-)",":D",":-D",";)",";-)","<3",":p",":-p",":P",":-P"};
$negativeEmoticons = {":(", ":-(", "=(",":/", ":-/", "</3"};
$emoticons = Join[$positiveEmoticons, $negativeEmoticons];

StringWords[text_String, opts:OptionsPattern[StringWords]] := Module[
	{words = text, separators},
	If[OptionValue["RemoveDiacritic"], words = RemoveDiacritic[words]];
	separators = {" ", "\n"};
	If[OptionValue["Emoticons"], AppendTo[separators, x:$emoticons :> x]];
	If[OptionValue["Punctuation"], AppendTo[separators, x:$shortpunctuations :> x], AppendTo[separators, $shortpunctuations]];
	If[OptionValue["CapitalizedPhrases"], 
		AppendTo[separators, x:(Repeated[$shortcapitals~~Except[{" ", ".", "!", "?"}]..~~" "]~~$shortcapitals~~Except[{" ", ".", "!", "?"}]..) :> x]
	];
	If[OptionValue["URLStem"],
		AppendTo[separators, {"http://www.", "www.", "http://" }~~x:(Except[{" ", "\n", "/"}])..~~Repeated[Except[{" ", "\n"}], {0, Infinity}] :> x];
		AppendTo[separators, x:(Except[{" ","\n"}]..~~".com")~~Repeated[Except[{" ", "\n"}], {0, Infinity}] :> x];
	];
	words = StringSplit[words, separators];
	If[OptionValue["LowerCase"], words = ToLowerCaseUnicode /@ words];
	(*If[OptionValue["LowerCase"], words = ToLowerCase /@ words];*)
	(*If[OptionValue["Stem"], words = StringStem /@ words];*)
	DeleteCases[words, ""]
];

Options[StringTokens] = {
	"NGram" -> 1,
	"Token" -> "Word",
	"LowerCase" -> False,
(*	"Stem" -> False,*)
	"RemoveDiacritic" -> False,
	"SpecialTokens"-> {}
};

StringTokens[doc_, opts:OptionsPattern[]]:= Module[
	{tokens = doc, type, ngram = OptionValue["NGram"]},
	If[OptionValue["LowerCase"], tokens = ToLowerCaseUnicode[tokens]]; (*might be important for String Sentences.*)
	If[OptionValue["RemoveDiacritic"], tokens = RemoveDiacritic[tokens]];
	type = OptionValue["Token"];
	tokens = Switch[type,
		"Word", StringWords[tokens, (*"Stem" -> OptionValue["Stem"], *)"SpecialTokens"-> OptionValue["SpecialTokens"]],
		"Character", Characters[tokens], 
		"Sentence", StringSentences[tokens],
		"NamedEntities", StringNamedEntities[tokens]
	];
	Partition[tokens, ngram, 1]
];


Options[StringTokenCounts] = {
	"Token" -> "Word",
	"NGram" -> 1,
	"ExcludeSingleElements" -> False,
	"ExcludedElements" -> {},
	"LowerCase" -> False,
(*	"Stem" -> False,*)
	"RemoveDiacritic" -> False,
	"SpecialTokens"-> {}
};
StringTokenCounts[doc_List, opts:OptionsPattern[]] := StringTokenCounts[StringJoin[Riffle[doc, " "]], opts];

FilterOptions[head_Symbol, opts___] := Sequence @@ FilterRules[{opts}, Options[head]];

StringTokenCounts[doc_String, opts:OptionsPattern[]] := Module[
	{tokens, counts},
	tokens = StringTokens[doc, FilterOptions[StringTokens, opts]];
	counts = Reverse @ SortBy[Tally[tokens], Last];
	If[OptionValue["ExcludeSingleElements"], counts = Select[counts, #[[2]]>1 &]];
	Select[counts, !MemberQ[OptionValue["ExcludedElements"], #[[1]]] &]
];

$toLowerCaseTable = BinaryReadList["~/Checkout/Alpha/Source/DataScience/Stats/Resources/ToLowerCaseTable.bin", "UnsignedInteger16"];
$toUpperCaseTable = Get["~/Checkout/Alpha/Source/DataScience/Stats/Resources/ToUpperCaseTable.m"];

$removeDiacriticTable = Get["~/Checkout/Alpha/Source/DataScience/Stats/Resources/RemoveDiacriticTable.m"];

ToLowerCaseUnicode[text_String] := FromCharacterCode[$toLowerCaseTable[[ToCharacterCode[text]+1]]];
ToLowerCaseUnicode[text_List] :=ToLowerCaseUnicode /@ text;

ToUpperCaseUnicode[text_String] := FromCharacterCode[Flatten[$toUpperCaseTable[[ToCharacterCode[text]+1]]]];
ToUpperCaseUnicode[text_List] := ToUpperCaseUnicode /@ text;


RemoveDiacritic[text_String] := FromCharacterCode[DeleteCases[Flatten[$removeDiacriticTable[[ToCharacterCode[text]+1]]], Null]];


