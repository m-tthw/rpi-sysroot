(* Mathematica Package *)

BeginPackage["MachineLearning`BuiltInModels`FlagClassify`FlagClassify`"]
(* Exported symbols added here with SymbolName::usage *)  

flagClassify::usage = "Flag classifier";

Begin["`Private`"] (* Begin Private Context *) 

(*Code for creating training and test data*)
(*builtInFlags = {CountryData[#, "Flag"], #} & /@ CountryData[];
builtInFlags1 = {ImageResize[#[[1]], 80], #[[2]]} & /@ DeleteDuplicates[builtInFlags, SameQ[#1[[1]], #2[[1]]] &];

(*Create larger training set with adjusted images and added Poisson noise*)
flagAll = Table[{ImageAdjust[#[[1]], {i, j, k}], #[[2]]}, {i, 0, 1, .25}, {j, 0, 1, .25}, {k, 1, 3, 1}] & /@ builtInFlags1;
flagMod = Table[{ImageEffect[#[[1]], {"PoissonNoise", i}], #[[2]]}, {i, .5, .9, .1}] & /@ builtInFlags1;

flagAll0 = Union[Partition[Flatten@flagAll, 2], Flatten[flagMod, 1]];

flagAll1 = Transpose[{ConformImages[First /@ flagAll0], Last /@ flagAll0}];

flagTraining = RandomSample[flagAll1, 10000];
flagTest = RandomSample[Complement[flagAll1, flagTraining], 1000];

flagClassify = Classify[Rule @@@ flagLDTraining, Method -> "LogisticRegression"]*)

flagClassify = Uncompress[Get["MachineLearning`BuiltInModels`FlagClassify`Resources`FlagClassifyData`"]];

End[] (* End Private Context *)

EndPackage[]