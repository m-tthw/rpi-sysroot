(* All loading of the paclet's Wolfram Language code should go through this file. *)

(* Developer maintains this list of symbols.
   autoloadSymbols must agree with the symbols listed in the Kernel extension in the PacletInfo.m file.
*)
MachineLearning`Private`autoloadSymbols = {
         "System`Classify", 
         "System`ClassifierFunction", 
         "System`ClassifierMeasurements",
         "System`ClassifierInformation",
         "System`ClassPriors",
         "System`IndeterminateThreshold",
            
         "System`Predict",
         "System`PredictorFunction",
         "System`PredictorMeasurements",
         "System`PredictorInformation",
            
         "System`UtilityFunction",
         "System`ValidationSet"
         }


MachineLearning`Private`symsToProtect =
    Hold[Complement[
            Select[Names["MachineLearning`*"] ~Join~ Names["MachineLearning`PackageScope`*"],
                ToExpression[#, InputForm, Function[{sym}, Length[DownValues[sym]] > 0 || Length[SubValues[sym]] > 0, HoldFirst]] &
            ] ~Join~ MachineLearning`Private`autoloadSymbols
            ,{
               "MachineLearning`PackageScope`PredictorEvaluation",
               "MachineLearning`PackageScope`ClassifierEvaluation",
               "MachineLearning`PackageScope`PrepackagedClassify",
               "MachineLearning`PackageScope`PrepackagedPredict"
           }
        ]
    ]


PacletManager`Package`loadWolframLanguageCode["MachineLearning", "MachineLearning`", DirectoryName[$InputFileName], "SuperFunctions.m",
         "AutoUpdate" -> True,
         "ForceMX" -> TrueQ[MachineLearning`$ForceMX], "Lock" -> True,
         "AutoloadSymbols" -> MachineLearning`Private`autoloadSymbols,
         "HiddenImports" -> {"PacletManager`", "Developer`", "GeneralUtilities`"},
         "SymbolsToProtect" -> MachineLearning`Private`symsToProtect
]