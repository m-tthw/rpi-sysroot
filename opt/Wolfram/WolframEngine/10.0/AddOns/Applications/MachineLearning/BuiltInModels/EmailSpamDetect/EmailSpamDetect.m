(* Mathematica Package *)

BeginPackage["MachineLearning`BuiltInModels`EmailSpamDetect`EmailSpamDetect`"]
(* Exported symbols added here with SymbolName::usage *)  

emailSpamDetect::usage = "Email spam detector";

Begin["`Private`"] (* Begin Private Context *) 

(*Training data from CSDMC2010 and Spam Assassin*)
(*http://csmining.org/index.php/spam-email-datasets-.html*)
(*https://spamassassin.apache.org/publiccorpus/*)
emailSpamDetect = Uncompress[Get["MachineLearning`BuiltInModels`EmailSpamDetect`Resources`EmailSpamDetectData`"]]
End[] (* End Private Context *)

EndPackage[]