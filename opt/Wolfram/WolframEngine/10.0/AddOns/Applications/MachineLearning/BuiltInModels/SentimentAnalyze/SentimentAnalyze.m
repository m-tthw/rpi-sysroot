(* Mathematica Package *)

BeginPackage["MachineLearning`BuiltInModels`SentimentAnalyze`SentimentAnalyze`"]
(* Exported symbols added here with SymbolName::usage *)  

sentimentAnalyze::usage = "Classifier for sentiment analysis."

Begin["`Private`"] (* Begin Private Context *) 
(*Training data from Cornell Movie Review Datset 
http://www.cs.cornell.edu/people/pabo/movie-review-data/, 

Cornell Sentence Polarity dataset (same URL)

UMichigan Sentiment Analysis Dataset (Kaggle)
http://inclass.kaggle.com/c/si650winter11

and JHU Sentiment Corpus
http://www.cs.jhu.edu/~mdredze/datasets/sentiment/
*)

sentimentAnalyze = Uncompress[Get["MachineLearning`BuiltInModels`SentimentAnalyze`Resources`SentimentAnalyzeData`"]];
End[] (* End Private Context *)

EndPackage[]