(* ::Package:: *)

Package["MachineLearning`"]

PackageScope["PreprocessClassifierText"]

PreprocessClassifierText::usage = "PreprocessClassifierText[{strings, labels}] performs automatic preprocessing of strings that will be used by a classifier.
used by Classify and related functions."

(*** String preprocessing ***)

Options[PreprocessClassifierText] = {
	"ClassLabels" -> Automatic
};
PreprocessClassifierText[{strings_, labels_}, opts:OptionsPattern[]] := Module[
	{preprocessorx, preprocessory, labelnames},
	If[MemberQ[labels, _Missing], abortLearning[]];
	labelnames = OptionValue["ClassLabels"];
	If[labelnames === Automatic, labelnames = Sort[DeleteDuplicates[labels]]];
	preprocessory = CreatePreprocessor[labelnames, "IntegerEncodeList"];
	preprocessorx = Preprocessor["Identity"];
	{preprocessorx, preprocessory}
];

$shortpunctuations = Join[
	Characters[",@#$%^&*()_-+={[}]:;\"\|\\<>/~`"],
	Characters[".!?"]
];
$shortcapitals = CharacterRange["A","Z"];
$positiveEmoticons = {":)",":-)",":D",":-D",";)",";-)","<3",":p",":-p",":P",":-P"};
$negativeEmoticons = {":(", ":-(", "=(",":/", ":-/", "</3"};
$emoticons = Join[$positiveEmoticons, $negativeEmoticons];

Options[CreatePreprocessor] = Join[Options[CreatePreprocessor], {
	"SpecialTokens" -> {},
	"Emoticons" -> False,
	"Punctuation" -> False,
	"CapitalizedPhrases" -> False
}];

CreatePreprocessor[strings_, "WordTokenize", opts:OptionsPattern[]] := Module[
	{extraseparators, specialtokens},
	extraseparators = {};
	specialtokens = OptionValue["SpecialTokens"];
	If[OptionValue["Emoticons"], AppendTo[specialtokens, $emoticons]];
	If[OptionValue["Punctuation"], AppendTo[specialtokens, $shortpunctuations], AppendTo[extraseparators, $shortpunctuations]];
	If[OptionValue["CapitalizedPhrases"], 
		AppendTo[specialtokens, Repeated[$shortcapitals~~Except[{" ", ".", "!", "?"}]..~~" "]~~$shortcapitals~~Except[{" ", ".", "!", "?"}]..]
	];
	Preprocessor["WordTokenize", extraseparators, specialtokens]
];

Preprocessor["WordTokenize", extraseparators_, specialtokens_][strings_] := DeleteCases[
	StringSplit[strings, Join[{" ", "\n", "\t"}, {x:specialtokens:>x}, extraseparators]],
	"", -1
];
Preprocessor["WordTokenize", extraseparators_, specialtokens_][strings_, "Inverse"] := StringJoin[Riffle[strings, " "]];

CreatePreprocessor[strings_, "ToLowerCase"] := Preprocessor["ToLowerCase"];
Preprocessor["ToLowerCase"][strings_] := ToLowerCase[strings];
Preprocessor["ToLowerCase"][strings_, "Inverse"] := strings;

CreatePreprocessor[strings_, "RemoveDiacritic"] := Preprocessor["RemoveDiacritic"];
Preprocessor["RemoveDiacritic"][strings_] := RemoveDiacritic[strings];
Preprocessor["RemoveDiacritic"][strings_, "Inverse"] := strings;


(*CreatePreprocessor[strings_, "URLStem"] := Preprocessor["URLStem"];
Preprocessor["URLStem"][strings_] := RemoveDiacritic[strings];
	If[OptionValue["URLStem"],
		AppendTo[separators, {"http://www.", "www.", "http://" }~~x:(Except[{" ", "\n", "/"}])..~~Repeated[Except[{" ", "\n"}], {0, Infinity}] :> x];
		AppendTo[separators, x:(Except[{" ","\n"}]..~~".com")~~Repeated[Except[{" ", "\n"}], {0, Infinity}] :> x];
	];
*)
(*
CreatePreprocessor[strings_, "Stem"] := Preprocessor["Stem"];
Preprocessor["Stem"][strings_] := Stem[strings];*)

(*Options[StringTokens] = {
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
		"Word", StringWords[tokens, "Stem" -> OptionValue["Stem"], "SpecialTokens"-> OptionValue["SpecialTokens"]],
		"Character", Characters[tokens], 
		"Sentence", StringSentences[tokens],
		"NamedEntities", StringNamedEntities[tokens]
	];
	Partition[tokens, ngram, 1]
];
*)


(*** End - String preprocessing ***)